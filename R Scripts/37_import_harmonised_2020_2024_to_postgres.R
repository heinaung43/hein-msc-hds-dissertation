# ============================================================
# BPJS 2020-2024: PostgreSQL Database Import
#
# Import harmonised BPJS datasets into a dedicated PostgreSQL
# database without overwriting existing tables.
#
# Configure the input directory and database connection
# before execution.
#
# Required packages: DBI, RPostgres
# ============================================================

library(DBI)
library(RPostgres)

# Directory containing harmonised BPJS datasets.
DATA_DIR <- "harmonised_2020_2024"

# PostgreSQL connection settings.
# Database credentials are obtained from environment variables.
PG_HOST <- Sys.getenv("PGHOST", "localhost")
PG_PORT <- as.integer(Sys.getenv("PGPORT", "5432"))
PG_DB <- "bpjs_cohort_2020_2024"
MAINTENANCE_DB <- "postgres"
PG_USER <- Sys.getenv("PGUSER", "postgres")
PG_PASS <- Sys.getenv("PGPASSWORD", "")

# Target schema and import batch size.
TARGET_SCHEMA <- "bpjs_data"
# Adjust batch size according to available system memory.
CHUNK_SIZE <- 100000L

# Input files and expected row counts from 2020_2024_final_dataset_summary.csv
manifest <- data.frame(
  table = c("participant_backbone", "primary_care_capitated",
            "primary_care_noncapitated", "secondary_care", "secondary_diagnosis"),
  file = c("participant_backbone_2020_2024.rds",
           "primary_care_capitated_2020_2024.rds",
           "primary_care_noncapitated_2020_2024.rds",
           "secondary_care_2020_2024.rds",
           "secondary_diagnosis_2020_2024.rds"),
  expected_rows = c(12005697, 18515654, 862840, 6998575, 7449544),
  stringsAsFactors = FALSE
)

stopifnot(dir.exists(DATA_DIR), is.finite(PG_PORT), CHUNK_SIZE > 0)
manifest$path <- file.path(DATA_DIR, manifest$file)
if (any(!file.exists(manifest$path))) {
  stop("Missing RDS file(s):\n", paste(manifest$path[!file.exists(manifest$path)], collapse = "\n"))
}

if (!nzchar(PG_PASS)) {
  if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    PG_PASS <- rstudioapi::askForPassword("PostgreSQL password")
  } else {
    stop("Set the PGPASSWORD environment variable before running this script.")
  }
}

# Connect to the maintenance database and create the
# target database if it does not already exist.
admin_con <- dbConnect(RPostgres::Postgres(), host = PG_HOST, port = PG_PORT,
                       dbname = MAINTENANCE_DB, user = PG_USER, password = PG_PASS)
db_exists <- dbGetQuery(admin_con,
  "SELECT 1 FROM pg_database WHERE datname = $1", params = list(PG_DB))
if (nrow(db_exists) == 0L) {
  dbExecute(admin_con, paste("CREATE DATABASE",
                              as.character(dbQuoteIdentifier(admin_con, PG_DB))))
  cat("Created database:", PG_DB, "\n")
} else {
  cat("Database already exists:", PG_DB, "(no changes to existing tables)\n")
}
dbDisconnect(admin_con)

con <- dbConnect(RPostgres::Postgres(), host = PG_HOST, port = PG_PORT,
                 dbname = PG_DB, user = PG_USER, password = PG_PASS)
cat("Import target:", PG_DB, "schema:", TARGET_SCHEMA, "\n")

# Quoted schema identifier and safe table lookup
schema_ident <- as.character(dbQuoteIdentifier(con, TARGET_SCHEMA))
dbExecute(con, paste("CREATE SCHEMA IF NOT EXISTS", schema_ident))

# Convert R-specific column classes for PostgreSQL compatibility.
prepare_for_postgres <- function(d) {
  if (!is.data.frame(d)) stop("RDS is not a data.frame/tibble.")
  if (anyDuplicated(names(d))) stop("Duplicate column names found.")
  for (nm in names(d)) {
    v <- d[[nm]]
    if (inherits(v, "haven_labelled") || inherits(v, "labelled")) {
      # Retain underlying values and remove variable attributes.
      d[[nm]] <- unclass(v)
      attributes(d[[nm]]) <- NULL
    }
    if (is.factor(d[[nm]])) d[[nm]] <- as.character(d[[nm]])
    if (is.list(d[[nm]]) && !inherits(d[[nm]], "POSIXlt")) {
      stop("List column not supported in this import: ", nm)
    }
  }
  d
}

# Prevent duplicate imports by rejecting existing target tables.
results <- data.frame(table = character(), expected = numeric(), imported = numeric(),
                      status = character(), stringsAsFactors = FALSE)

for (i in seq_len(nrow(manifest))) {
  tbl <- manifest$table[i]
  filepath <- manifest$path[i]
  expected <- manifest$expected_rows[i]
  table_id <- Id(schema = TARGET_SCHEMA, table = tbl)
  cat("\n=====", tbl, "=====\n")

  if (dbExistsTable(con, table_id)) {
    stop("Target table already exists: ", TARGET_SCHEMA, ".", tbl,
         ". Check it manually before rerunning. No overwrite performed.")
  }

  # One full RDS object is loaded into memory at a time.
  d <- readRDS(filepath)
  d <- prepare_for_postgres(d)
  actual <- nrow(d)
  if (!identical(as.numeric(actual), as.numeric(expected))) {
    stop("Input row-count mismatch in ", tbl, ": ", actual, " vs audit ", expected)
  }
  cat("Input rows:", format(actual, big.mark = ","),
      "| columns:", ncol(d), "\n")

  # Create an empty table using inferred PostgreSQL column types.
  dbWriteTable(con, table_id, d[0, , drop = FALSE], overwrite = FALSE)

  for (start in seq.int(1L, actual, by = CHUNK_SIZE)) {
    finish <- min(start + CHUNK_SIZE - 1L, actual)
    dbAppendTable(con, table_id, d[start:finish, , drop = FALSE])
    if (finish == actual || (finish %% (CHUNK_SIZE * 10L)) == 0L) {
      cat("  Imported", format(finish, big.mark = ","), "of",
          format(actual, big.mark = ","), "rows\n")
    }
  }

  count_query <- paste0("SELECT COUNT(*) AS n FROM ",
                        as.character(dbQuoteIdentifier(con, table_id)))
  imported <- as.numeric(dbGetQuery(con, count_query)$n[1])
  if (imported != expected) {
    stop("FAILED row-count check for ", tbl, ": PostgreSQL has ", imported,
         " rows; expected ", expected, ". Preserve table for investigation.")
  }

  # Summarise imported record counts by source year.
  if ("source_year" %in% names(d)) {
    yr_sql <- paste0("SELECT source_year, COUNT(*) AS n FROM ",
                     as.character(dbQuoteIdentifier(con, table_id)),
                     " GROUP BY source_year ORDER BY source_year")
    print(dbGetQuery(con, yr_sql))
  }

  results <- rbind(results, data.frame(table = tbl, expected = expected,
                                      imported = imported, status = "PASS"))
  rm(d)
  invisible(gc())
}

out_file <- file.path(DATA_DIR, "postgres_2020_2024_import_results.csv")
write.csv(results, out_file, row.names = FALSE)
cat("\nALL FIVE TABLES PASSED ROW-COUNT VALIDATION\n")
print(results)
cat("Saved audit:", out_file, "\n")
dbDisconnect(con)
