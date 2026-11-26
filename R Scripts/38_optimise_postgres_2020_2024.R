# ============================================================
# BPJS 2020-2024: PostgreSQL Validation and Optimisation
#
# Validate imported BPJS tables, create database indexes,
# establish encounter views and update query planner statistics.
#
# Database: bpjs_cohort_2020_2024
# Source schema: bpjs_data
# Encounter view schema: bpjs_app
#
# Source table records are preserved without modification.
# Audit reports are exported as CSV files.
#
# Required packages: DBI, RPostgres
# Optional package: rstudioapi
# ============================================================

library(DBI)
library(RPostgres)

# PostgreSQL connection and output settings.
PG_HOST <- Sys.getenv("PGHOST", "localhost")
PG_PORT <- as.integer(Sys.getenv("PGPORT", "5432"))
PG_DB <- "bpjs_cohort_2020_2024"
PG_USER <- Sys.getenv("PGUSER", "postgres")
PG_PASS <- Sys.getenv("PGPASSWORD", "")

DATA_SCHEMA <- "bpjs_data"
VIEW_SCHEMA <- "bpjs_app"
OUTPUT_DIR <- getwd()

# Configure index and view creation.
RUN_INDEXES <- TRUE
RUN_VIEWS <- TRUE

# Preserve existing views unless replacement is explicitly enabled.
REPLACE_EXISTING_VIEWS <- FALSE

# Expected record counts from the final harmonisation audit.
expected <- c(
  participant_backbone = 12005697,
  primary_care_capitated = 18515654,
  primary_care_noncapitated = 862840,
  secondary_care = 6998575,
  secondary_diagnosis = 7449544
)
stopifnot(is.finite(PG_PORT), dir.exists(OUTPUT_DIR))
if (!nzchar(PG_PASS)) {
  if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    PG_PASS <- rstudioapi::askForPassword("PostgreSQL password")
  } else {
    stop("Set PGPASSWORD before running the script")
  }
}
con <- dbConnect(RPostgres::Postgres(), host = PG_HOST, port = PG_PORT,
                 dbname = PG_DB, user = PG_USER, password = PG_PASS)
on.exit(dbDisconnect(con), add = TRUE)
cat("Connected to:", dbGetQuery(con, "SELECT current_database() AS db")$db, "\n")
stopifnot(identical(dbGetQuery(con, "SELECT current_database() AS db")$db[[1]], PG_DB))

qi <- function(x) as.character(dbQuoteIdentifier(con, x))
qt <- function(table, schema = DATA_SCHEMA) as.character(dbQuoteIdentifier(con, Id(schema = schema, table = table)))
write_report <- function(x, name) {
  path <- file.path(OUTPUT_DIR, name)
  write.csv(x, path, row.names = FALSE, na = "")
  cat("Saved:", normalizePath(path, winslash = "/"), "\n")
}

# ---- 1. Validate source tables and record counts --------
missing_tables <- names(expected)[!vapply(names(expected), function(t) {
  dbExistsTable(con, Id(schema = DATA_SCHEMA, table = t))
}, logical(1))]
if (length(missing_tables)) stop("Required table(s) absent in ", DATA_SCHEMA, ": ", paste(missing_tables, collapse = ", "))

counts <- do.call(rbind, lapply(names(expected), function(t) {
  n <- as.numeric(dbGetQuery(con, paste0("SELECT count(*) AS n FROM ", qt(t)))$n[[1]])
  data.frame(table = t, expected = unname(expected[[t]]), imported = n,
             status = if (n == expected[[t]]) "PASS" else "FAIL")
}))
print(counts, row.names = FALSE)
write_report(counts, "postgres_2020_2024_stage38_row_counts.csv")
if (any(counts$status != "PASS")) stop("Row count validation failed. No indexes or views created.")

# ---- 2. Inspect PostgreSQL table schemas ----

# Retrieve column definitions using quoted schema
# and table names.
schema_sql <- paste(
  "SELECT table_name, ordinal_position, column_name, data_type, udt_name,",
  "is_nullable FROM information_schema.columns",
  "WHERE table_schema =", as.character(dbQuoteString(con, DATA_SCHEMA)),
  "AND table_name IN (", paste(as.character(dbQuoteString(con, names(expected))), collapse = ", "), ")",
  "ORDER BY table_name, ordinal_position"
)
schema_audit <- dbGetQuery(con, schema_sql)
if (nrow(schema_audit) == 0) stop("Schema query returned no columns. Check DATA_SCHEMA and imported table names.")
write_report(schema_audit, "postgres_2020_2024_stage38_schema.csv")
has_col <- function(t, col) any(schema_audit$table_name == t & schema_audit$column_name == col)

# ---- 3. Create database indexes ----
#
# Index patient identifiers, encounter dates, diagnosis codes
# and visit identifiers to support filtering and linkage.
# Retain existing indexes during subsequent executions.
# Index creation requires additional processing time and disk space.
index_plan <- data.frame(
  table = c("participant_backbone", "primary_care_capitated", "primary_care_capitated",
            "primary_care_noncapitated", "primary_care_noncapitated", "secondary_care",
            "secondary_care", "secondary_care", "secondary_diagnosis"),
  index = c("idx_2024_pb_patient", "idx_2024_pc_patient_date", "idx_2024_pc_diagnosis_date",
            "idx_2024_pn_patient_date", "idx_2024_pn_diagnosis_date", "idx_2024_sc_patient_date",
            "idx_2024_sc_diagnosis_date", "idx_2024_sc_visit_id", "idx_2024_sd_visit_id"),
  columns = c("patient_id", "patient_id,visit_date", "diagnosis_code,visit_date",
              "patient_id,visit_date", "diagnosis_code,visit_date", "patient_id,visit_date",
              "diagnosis_code,visit_date", "visit_id", "visit_id"),
  stringsAsFactors = FALSE
)
index_plan$status <- "NOT_RUN"
index_plan$notes <- ""
for (i in seq_len(nrow(index_plan))) {
  t <- index_plan$table[i]
  cols <- strsplit(index_plan$columns[i], ",", fixed = TRUE)[[1]]
  if (!all(vapply(cols, function(col) has_col(t, col), logical(1)))) {
    index_plan$status[i] <- "SKIPPED_MISSING_COLUMN"
    next
  }
  if (!RUN_INDEXES) next
  sql <- paste0("CREATE INDEX IF NOT EXISTS ", qi(index_plan$index[i]),
                " ON ", qt(t), " (", paste(qi(cols), collapse = ", "), ")")
  cat("Index:", index_plan$index[i], "on", t, "\n")
  tryCatch({
    dbExecute(con, sql)
    index_plan$status[i] <- "OK"
  }, error = function(e) {
    index_plan$status[i] <<- "ERROR"
    index_plan$notes[i] <<- conditionMessage(e)
    message("Index failed (continuing): ", conditionMessage(e))
  })
}
write_report(index_plan, "postgres_2020_2024_stage38_indexes.csv")

# ---- 4. Create encounter views ----
#
# Select common variables across the three encounter tables.
# Combine records using UNION ALL without materialising
# additional encounter tables.
# Retain source year and encounter year as separate variables.
view_results <- data.frame(view = character(), status = character(), note = character())
if (RUN_VIEWS) {
  dbExecute(con, paste("CREATE SCHEMA IF NOT EXISTS", qi(VIEW_SCHEMA)))
  common_cols <- c("patient_id", "visit_id", "visit_date", "source_year",
                   "encounter_year", "diagnosis_code", "diagnosis_name",
                   "care_setting", "facility_province_code_preferred")
  encounter_tables <- c("primary_care_capitated", "primary_care_noncapitated", "secondary_care")
  if (!all(vapply(encounter_tables, function(t) all(vapply(common_cols, function(c) has_col(t, c), logical(1))), logical(1)))) {
    stop("Encounter view needs columns not present in all 3 source tables. Check stage38_schema.csv.")
  }
  sql_parts <- vapply(encounter_tables, function(t) {
    type <- if (t == "secondary_care") "Secondary care" else "Primary care"
    paste0("SELECT patient_id, visit_id, visit_date, source_year, encounter_year, ",
           "diagnosis_code, diagnosis_name, care_setting, facility_province_code_preferred, ",
           "'", t, "'::text AS record_source, '", type, "'::text AS care_group ",
           "FROM ", qt(t))
  }, character(1))
  # Define unified and care-setting-specific encounter views.
  view_defs <- list(
    all_encounters = paste(sql_parts, collapse = " UNION ALL "),
    primary_encounters = paste(sql_parts[1:2], collapse = " UNION ALL "),
    secondary_encounters = unname(sql_parts[[3]])
  )
  for (v in names(view_defs)) {
    view_id <- Id(schema = VIEW_SCHEMA, table = v)
    exists <- dbExistsTable(con, view_id)
    if (exists && !REPLACE_EXISTING_VIEWS) {
      view_results <- rbind(view_results, data.frame(view = v, status = "SKIPPED_EXISTING", note = "View/table already exists"))
      next
    }
    cmd <- if (exists) "CREATE OR REPLACE VIEW " else "CREATE VIEW "
    sql <- paste0(cmd, qt(v, VIEW_SCHEMA), " AS ", view_defs[[v]])
    tryCatch({
      dbExecute(con, sql)
      view_results <- rbind(view_results, data.frame(view = v, status = "OK", note = ""))
      cat("View ready:", VIEW_SCHEMA, ".", v, "\n", sep = "")
    }, error = function(e) {
      view_results <<- rbind(view_results, data.frame(view = v, status = "ERROR", note = conditionMessage(e)))
      message("View failed: ", v, " -- ", conditionMessage(e))
    })
  }
}
write_report(view_results, "postgres_2020_2024_stage38_views.csv")

# ---- 5. Update query planner statistics ----
#
# Run ANALYZE to update table statistics and support
# PostgreSQL query planning.
for (t in names(expected)) {
  cat("ANALYZE", t, "\n")
  tryCatch(dbExecute(con, paste("ANALYZE", qt(t))),
           error = function(e) message("ANALYZE failed for ", t, ": ", conditionMessage(e)))
}

cat("\nBPJS POSTGRESQL VALIDATION AND OPTIMISATION COMPLETE\n")
cat("Review the generated audit reports for validation results.\n")
cat("Database:", PG_DB, "| Data schema:", DATA_SCHEMA,
    "| View schema:", VIEW_SCHEMA, "\n")
