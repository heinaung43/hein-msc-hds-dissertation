# =====================================================================
# Interactive Cohort Construction and Analysis System
# BPJS Indonesian Health Insurance Claims Data
# Version: 7.3.1
#
# Supports cohort construction, descriptive analysis and longitudinal
# exploration of recorded healthcare claims.
#
# Modules: dataset overview, primary and combined cohorts, secondary
# care and hospital outcomes, longitudinal analysis and aggregate export.
#
# Database: bpjs_cohort_2020_2024
# Source schema: bpjs_data
# Encounter view: bpjs_app.all_encounters (Script 38)
# Application schema: bpjs_v7_adapter
#
# Source releases: 2020-2024; encounter dates may precede source years.
# Requires permission to create application views, a materialised
# demographic view and its index. Initial aggregation may take time.
# Disclosure controls include cohort-level and selected cell suppression.
# =====================================================================

# ---------------------------------------------------------------------
# 0. Load required packages
# ---------------------------------------------------------------------
required_packages <- c(
  "shiny", "bslib", "ggplot2", "dplyr", "tidyr", "tibble",
  "DBI", "RPostgres", "pool", "scales", "stringr", "bsicons", "zip"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(shiny)
library(bslib)
library(bsicons)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(DBI)
library(RPostgres)
library(pool) 
library(scales)
library(stringr)

# ---------------------------------------------------------------------
# 1. Configure PostgreSQL connection
# ---------------------------------------------------------------------
pg_host <- Sys.getenv("PGHOST", unset = "localhost")
pg_port <- as.integer(Sys.getenv("PGPORT", unset = "5432"))
pg_dbname <- Sys.getenv("PGDATABASE", unset = "bpjs_cohort_2020_2024")
pg_user <- Sys.getenv("PGUSER", unset = "postgres")
pg_password <- Sys.getenv("PGPASSWORD", unset = "")
if (!nzchar(pg_password)) {
  if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    pg_password <- rstudioapi::askForPassword("PostgreSQL password")
  } else stop("Set PGPASSWORD before starting the app")
}


# Unified encounter view for primary and combined cohort construction.
# Includes capitated, non-capitated and secondary care records.
primary_table <- "cohort_encounters"
secondary_events_table <- "secondary_care_patient_events"
secondary_diagnoses_view <- "secondary_care_diagnoses"
icd_dictionary_table <- "icd10_diagnosis_dictionary"

db_pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = pg_dbname,
  host = pg_host,
  port = pg_port,
  user = pg_user,
  password = pg_password
)

onStop(function() {
  poolClose(db_pool)
})


# ---------------------------------------------------------------------
# Application database views
# Create views of harmonised source tables without altering source records.
# Application objects are maintained in a separate schema.
# ---------------------------------------------------------------------
adapter_schema <- "bpjs_v7_adapter"
source_schema <- "bpjs_data"
quote_id <- function(x) as.character(DBI::dbQuoteIdentifier(db_pool, x))
source_ref <- function(t) paste0(quote_id(source_schema), ".", quote_id(t))
adapter_ref <- function(t) paste0(quote_id(adapter_schema), ".", quote_id(t))

if (!identical(pg_dbname, "bpjs_cohort_2020_2024")) {
  stop("The application requires the bpjs_cohort_2020_2024 database.")
}
needed_sources <- c("participant_backbone", "primary_care_capitated",
                    "primary_care_noncapitated", "secondary_care", "secondary_diagnosis")
for (tbl in needed_sources) {
  if (!DBI::dbExistsTable(db_pool, DBI::Id(schema = source_schema, table = tbl))) {
    stop("Missing source table: ", source_schema, ".", tbl)
  }
}
DBI::dbExecute(db_pool, paste0("CREATE SCHEMA IF NOT EXISTS ", quote_id(adapter_schema)))
create_adapter <- function(name, sql) {
  DBI::dbExecute(db_pool, paste0("CREATE OR REPLACE VIEW ", adapter_ref(name), " AS ", sql))
}

# Create a materialised demographic view with one record per patient.
# Select the most recent membership record by source and sample year
# to avoid multiplying encounters during demographic linkage.
# Refresh bpjs_v7_adapter.demographics_cache when membership data change.
DBI::dbExecute(db_pool, paste0(
  "CREATE MATERIALIZED VIEW IF NOT EXISTS ", adapter_ref("demographics_cache"), " AS ",
  "SELECT DISTINCT ON (patient_id) patient_id, date_of_birth, sex, ",
  "residence_province_code_preferred::text AS province, ",
  "residence_province_code_system::text AS province_system ",
  "FROM ", source_ref("participant_backbone"), " ",
  "WHERE patient_id IS NOT NULL ",
  "ORDER BY patient_id, source_year DESC NULLS LAST, sample_year DESC NULLS LAST"
))
DBI::dbExecute(db_pool, paste0(
  "CREATE UNIQUE INDEX IF NOT EXISTS idx_v7_demographics_patient ",
  "ON ", adapter_ref("demographics_cache"), " (patient_id)"
))

# Distinguish patient residence province from facility province.
# Map administrative province codes and Indonesian names using the
# BPJS geographical codebook. Retain unverified sequential codes
# as separate categories; do not infer uncertain geographical mappings.
# Administrative codes apply to 2020/2022/2023 capitated and hospital
# records; 2021-2023 non-capitated sequential codes remain unverified.
province_indonesian_codes <- c(
  "ACEH"="11", "SUMATERA UTARA"="12", "SUMATERA BARAT"="13",
  "RIAU"="14", "JAMBI"="15", "SUMATERA SELATAN"="16",
  "BENGKULU"="17", "LAMPUNG"="18", "KEPULAUAN BANGKA BELITUNG"="19",
  "KEPULAUAN RIAU"="21", "DKI JAKARTA"="31", "JAKARTA"="31",
  "JAWA BARAT"="32", "JAWA TENGAH"="33", "DAERAH ISTIMEWA YOGYAKARTA"="34",
  "DI YOGYAKARTA"="34", "JAWA TIMUR"="35", "BANTEN"="36",
  "BALI"="51", "NUSA TENGGARA BARAT"="52", "NUSA TENGGARA TIMUR"="53",
  "KALIMANTAN BARAT"="61", "KALIMANTAN TENGAH"="62",
  "KALIMANTAN SELATAN"="63", "KALIMANTAN TIMUR"="64",
  "KALIMANTAN UTARA"="65", "SULAWESI UTARA"="71",
  "SULAWESI TENGAH"="72", "SULAWESI SELATAN"="73",
  "SULAWESI TENGGARA"="74", "GORONTALO"="75", "SULAWESI BARAT"="76",
  "MALUKU"="81", "MALUKU UTARA"="82", "PAPUA BARAT"="91",
  "PAPUA BARAT DAYA"="92", "PAPUA"="94", "PAPUA SELATAN"="95",
  "PAPUA TENGAH"="96", "PAPUA PEGUNUNGAN"="97"
)
province_name_case <- function(value) {
  clauses <- vapply(seq_along(province_indonesian_codes), function(i) {
    paste0(" WHEN ", as.character(DBI::dbQuoteString(db_pool, names(province_indonesian_codes)[i])),
           " THEN ", as.character(DBI::dbQuoteString(db_pool, unname(province_indonesian_codes[i]))))
  }, character(1))
  paste0("CASE UPPER(TRIM((", value, ")::text)) ",
         paste(clauses, collapse = " "), " ELSE NULL::text END")
}
province_sql <- function(value, system) {
  valid_admin_codes <- c("11","12","13","14","15","16","17","18","19",
                         "21","31","32","33","34","35","36","51","52","53",
                         "61","62","63","64","65","71","72","73","74","75",
                         "76","81","82","91","92","94","95","96","97")
  val <- paste0("TRIM((", value, ")::text)")
  allowed <- paste(vapply(valid_admin_codes, function(z)
    as.character(DBI::dbQuoteString(db_pool, z)), character(1)), collapse = ",")
  mapped_name <- province_name_case(value)
  paste0(
    "CASE WHEN NULLIF(", val, ", '') IS NULL THEN NULL::text ",
    "WHEN ", val, " ~ '^[0-9]+$' AND ",
    "(", system, ")::text IN ('34-province','38-province') AND ",
    val, " IN (", allowed, ") THEN 'standard:' || ", val, " ",
    "WHEN ", val, " ~ '^[0-9]+$' THEN 'legacy:' || ", val, " ",
    "WHEN ", mapped_name, " IS NOT NULL THEN 'standard:' || ", mapped_name, " ",
    "ELSE 'name:' || ", val, " END"
  )
}
# Prefer recorded encounter-level residence province.
# Use membership residence as a fallback; do not substitute facility province.
enc_province <- function(alias) paste0(
  "COALESCE(", province_sql(paste0(alias, ".residence_province"), paste0(alias, ".source_year_system")),
  ", ", province_sql("p.province", "p.province_system"), ")"
)
# Retain plausible recorded ages or derive completed years at encounter.
age_sql <- function(alias) paste0(
  "CASE WHEN ", alias, ".age BETWEEN 0 AND 110 THEN ", alias, ".age::double precision ",
  "WHEN p.date_of_birth IS NOT NULL AND p.date_of_birth <= ", alias, ".visit_date ",
  "AND p.date_of_birth >= DATE '1900-01-01' THEN ",
  "CASE WHEN EXTRACT(YEAR FROM AGE(", alias, ".visit_date, p.date_of_birth)) BETWEEN 0 AND 110 ",
  "THEN EXTRACT(YEAR FROM AGE(", alias, ".visit_date, p.date_of_birth))::double precision ",
  "ELSE NULL::double precision END ELSE NULL::double precision END"
)
# Link encounters to a patient-level demographic view to avoid
# record multiplication from repeated membership records.
create_adapter("cohort_encounters", paste0(
  "SELECT e.patient_id, e.visit_id, e.visit_date, e.diagnosis_code, e.diagnosis_name, ",
  "'Primary care'::text AS care_setting, ",
  "'capitated:' || COALESCE(e.facility_type_code::text, '[Missing]') AS facility_type, ",
  enc_province("e"), "::text AS province, ",
  "COALESCE(NULLIF(e.sex::text, ''), p.sex::text) AS sex, ",
  age_sql("e"), " AS age, e.source_year, ",
  "'capitated'::text AS record_source ",
  "FROM (SELECT *, CASE WHEN source_year = 2024 THEN '38-province' ",
  "WHEN source_year IN (2020, 2022, 2023) THEN '34-province' ELSE 'named' END AS source_year_system FROM ",
  source_ref("primary_care_capitated"), ") e ",
  "LEFT JOIN ", adapter_ref("demographics_cache"), " p ON e.patient_id = p.patient_id ",
  "UNION ALL ",
  "SELECT e.patient_id, e.visit_id, e.visit_date, e.diagnosis_code, e.diagnosis_name, ",
  "'Primary care'::text, ",
  "'noncapitated:' || COALESCE(e.facility_type_code::text, '[Missing]'), ",
  enc_province("e"), "::text, p.sex::text, ",
  age_sql("e"), ", e.source_year, 'noncapitated'::text ",
  "FROM (SELECT *, CASE WHEN source_year = 2024 THEN '38-province' ",
  "WHEN source_year = 2020 THEN '34-province' ELSE 'sequential-unverified' END AS source_year_system FROM ",
  source_ref("primary_care_noncapitated"), ") e ",
  "LEFT JOIN ", adapter_ref("demographics_cache"), " p ON e.patient_id = p.patient_id ",
  "UNION ALL ",
  "SELECT e.patient_id, e.visit_id, e.visit_date, e.diagnosis_code, e.diagnosis_name, ",
  "'Secondary care'::text, ",
  "'hospital:' || COALESCE(e.facility_type_code::text, '[Missing]'), ",
  enc_province("e"), "::text, ",
  "COALESCE(NULLIF(e.sex::text, ''), p.sex::text), ",
  age_sql("e"), ", e.source_year, 'hospital'::text ",
  "FROM (SELECT *, CASE WHEN source_year = 2024 THEN '38-province' ",
  "WHEN source_year IN (2020, 2022, 2023) THEN '34-province' ELSE 'named' END AS source_year_system FROM ",
  source_ref("secondary_care"), ") e ",
  "LEFT JOIN ", adapter_ref("demographics_cache"), " p ON e.patient_id = p.patient_id"
))

create_adapter("fktp_encounters", paste0(
  "SELECT patient_id, visit_id, visit_date FROM ", source_ref("primary_care_capitated"),
  " UNION ALL SELECT patient_id, visit_id, visit_date FROM ", source_ref("primary_care_noncapitated")
))

# Provide a patient-level demographic view for descriptive summaries.
# Membership records do not establish continuous observation.
create_adapter("patient_demographics", paste0(
  "SELECT patient_id, sex, date_of_birth, province FROM ", adapter_ref("demographics_cache")
))

# Retain the original hospital visit identifier.
# Represent unavailable 30-day readmission and follow-up indicators as NULL.
create_adapter("secondary_care_patient_events", paste0(
  "SELECT e.patient_id, e.visit_id AS secondary_visit_id, e.admission_date, e.discharge_date, ",
  province_sql("facility_province_code_preferred",
               paste0("CASE WHEN source_year = 2024 THEN '38-province' ",
                      "WHEN source_year IN (2020, 2022, 2023) THEN '34-province' ",
                      "ELSE 'named' END")),
  " AS secondary_care_province, ",
  "facility_type_code::text AS facility_type, hospital_class_code::text AS facility_class, ",
  "service_level_code::text AS service_level, discharge_status_code::text AS discharge_status, ",
  "length_of_stay_days::double precision AS length_of_stay_days, ",
  "verified_cost::double precision AS bpjs_verified_cost, ",
  "referring_primary_visit_id AS primary_care_referral_visit_id, ",
  "primary_diagnosis_name::text AS primary_diagnosis_name, ",
  "NULL::boolean AS readmission_30d, NULL::boolean AS follow_up_30d_available, ",
  "source_year, ",
  "COALESCE(NULLIF(e.sex::text, ''), p.sex::text) AS sex, ",
  "CASE WHEN e.age BETWEEN 0 AND 110 THEN e.age::double precision ",
  "WHEN COALESCE(e.date_of_birth, p.date_of_birth) IS NOT NULL ",
  "AND COALESCE(e.date_of_birth, p.date_of_birth) >= DATE '1900-01-01' ",
  "AND COALESCE(e.date_of_birth, p.date_of_birth) <= e.admission_date ",
  "AND EXTRACT(YEAR FROM AGE(e.admission_date, COALESCE(e.date_of_birth, p.date_of_birth))) BETWEEN 0 AND 110 ",
  "THEN EXTRACT(YEAR FROM AGE(e.admission_date, COALESCE(e.date_of_birth, p.date_of_birth)))::double precision ",
  "ELSE NULL::double precision END AS age ",
  "FROM ", source_ref("secondary_care"), " e ",
  "LEFT JOIN ", adapter_ref("demographics_cache"), " p ON e.patient_id = p.patient_id"
))

# Combine primary and secondary hospital diagnoses, preserving position.
# Link using hospital visit IDs; global uniqueness is an audit prerequisite.
create_adapter("secondary_care_diagnoses", paste0(
  "SELECT visit_id AS secondary_visit_id, primary_diagnosis_code::text AS diagnosis_code, ",
  "primary_diagnosis_name::text AS diagnosis_name, 'Primary'::text AS diagnosis_position ",
  "FROM ", source_ref("secondary_care"),
  " UNION ALL SELECT visit_id, secondary_diagnosis_code::text, ",
  "secondary_diagnosis_name::text, 'Secondary'::text FROM ", source_ref("secondary_diagnosis")
))


# ---------------------------------------------------------------------
# 2. Database query and utility functions
# ---------------------------------------------------------------------
qi <- function(x) {
  if (length(x) == 1L && x %in% c(
    "cohort_encounters", "fktp_encounters", "patient_demographics",
    "secondary_care_patient_events", "secondary_care_diagnoses")) {
    return(adapter_ref(x))
  }
  as.character(DBI::dbQuoteIdentifier(db_pool, x))
}

qs <- function(x) {
  as.character(DBI::dbQuoteString(db_pool, as.character(x)))
}

sql_in <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NULL)
  paste(vapply(x, qs, character(1)), collapse = ", ")
}

db_scalar <- function(sql) {
  out <- DBI::dbGetQuery(db_pool, sql)
  if (nrow(out) == 0 || ncol(out) == 0) return(NA)
  out[[1]][1]
}

db_distinct_values <- function(table_name, column_name) {
  sql <- paste0(
    "SELECT DISTINCT ", qi(column_name), " AS value ",
    "FROM ", qi(table_name), " ",
    "WHERE ", qi(column_name), " IS NOT NULL ",
    "AND TRIM(CAST(", qi(column_name), " AS TEXT)) <> '' ",
    "ORDER BY value"
  )
  
  as.character(DBI::dbGetQuery(db_pool, sql)$value)
}

db_range <- function(table_name, column_name) {
  sql <- paste0(
    "SELECT MIN(", qi(column_name), ") AS min_value, ",
    "MAX(", qi(column_name), ") AS max_value ",
    "FROM ", qi(table_name)
  )
  DBI::dbGetQuery(db_pool, sql)
}

format_count <- function(x) {
  if (length(x) == 0 || is.na(x)) return("NA")
  format(as.numeric(x), big.mark = ",", scientific = FALSE, trim = TRUE)
}

format_currency_idr <- function(x) {
  if (length(x) == 0 || is.na(x)) return("NA")
  paste0("IDR ", format(round(as.numeric(x), 0), big.mark = ",", scientific = FALSE))
}

safe_percent <- function(numerator, denominator) {
  if (is.na(denominator) || denominator == 0) return(NA_real_)
  round(100 * numerator / denominator, 1)
}

download_plot_png <- function(plot_object, file, width = 11, height = 7) {
  ggplot2::ggsave(
    filename = file,
    plot = plot_object,
    width = width,
    height = height,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}

small_n_threshold <- 5L

help_label <- function(label, help_text) {
  tagList(
    label,
    bslib::tooltip(
      bsicons::bs_icon("question-circle", size = "1em"),
      help_text,
      placement = "right"
    )
  )
}

disclosure_note <- function() {
  div(
    class = "disclosure-note",
    tags$b("Disclosure rule: "),
    "Graphs and detailed downloads are only shown when the selected cohort contains at least ",
    small_n_threshold,
    " unique patients. Smaller cohorts are suppressed to reduce small-cell disclosure risk."
  )
}

# ---------------------------------------------------------------------
# 3. Display labels and filter mapping
# ---------------------------------------------------------------------
normalise_facility_type <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  toupper(x)
}

facility_type_english_lookup <- c(
  "PUSKESMAS" = "Public health centre",
  "KLINIK PRATAMA" = "Primary care clinic",
  "KLINIK UTAMA" = "Main clinic",
  "DOKTER UMUM" = "General practitioner",
  "DOKTER GIGI" = "Dentist",
  "LABORATORIUM" = "Laboratory",
  "LAIN-LAIN" = "Other facility",
  "LAIN LAIN" = "Other facility",
  "JEJARING" = "Network provider",
  "RUMAH SAKIT" = "Hospital",
  "APOTEK" = "Pharmacy",
  "OPTIK" = "Optical provider",
  "RS KELAS A" = "Class A hospital",
  "RS KELAS B" = "Class B hospital",
  "RS KELAS C" = "Class C hospital",
  "RS KELAS D" = "Class D hospital",
  "RS KELAS D PRATAMA" = "Primary Class D hospital",
  "RS SWASTA SETARA TYPE A" = "Private hospital equivalent to Class A",
  "RS SWASTA SETARA TIPE A" = "Private hospital equivalent to Class A",
  "RS SWASTA SETARA TYPE B" = "Private hospital equivalent to Class B",
  "RS SWASTA SETARA TIPE B" = "Private hospital equivalent to Class B",
  "RS SWASTA SETARA TYPE C" = "Private hospital equivalent to Class C",
  "RS SWASTA SETARA TIPE C" = "Private hospital equivalent to Class C",
  "RS SWASTA SETARA TYPE D" = "Private hospital equivalent to Class D",
  "RS SWASTA SETARA TIPE D" = "Private hospital equivalent to Class D",
  "RS TNI POLRI KELAS I" = "Military/police Class I hospital",
  "RS TNI POLRI KELAS II" = "Military/police Class II hospital",
  "RS TNI POLRI KELAS III" = "Military/police Class III hospital",
  "RS TNI POLRI KELAS IV" = "Military/police Class IV hospital",
  "KHUSUS BEDAH" = "Specialist surgical hospital",
  "KHUSUS GIGI DAN MULUT" = "Specialist dental and oral hospital",
  "KHUSUS HEMODIALISA" = "Specialist haemodialysis hospital",
  "KHUSUS IBU DAN ANAK" = "Specialist maternal and child hospital",
  "KHUSUS JANTUNG" = "Specialist cardiac hospital",
  "KHUSUS JIWA" = "Specialist psychiatric hospital",
  "KHUSUS KANKER ONKOLOGI" = "Specialist oncology hospital",
  "KHUSUS KUSTA" = "Specialist leprosy hospital",
  "KHUSUS MATA" = "Specialist eye hospital",
  "KHUSUS PARU" = "Specialist pulmonary hospital"
)

facility_numeric_map <- list(
  capitated = c("1"="Public health centre (Puskesmas)",
                "2"="Primary care clinic (Klinik Pratama)",
                "3"="General practitioner (Dokter Umum)",
                "4"="Dentist (Dokter Gigi)",
                "5"="Laboratory (Laboratorium)",
                "6"="Other facility (Lain-lain)",
                "7"="Network provider (Jejaring)"),
  noncapitated = c("1"="Public health centre (Puskesmas)",
                   "2"="Primary care clinic (Klinik Pratama)",
                   "3"="General practitioner (Dokter Umum)",
                   "4"="Laboratory (Laboratorium)"),
  hospital = c("1"="Hospital (Rumah Sakit)",
               "2"="Main clinic (Klinik Utama)",
               "3"="Primary care clinic (Klinik Pratama)")
)
facility_label_vector <- function(values) {
  vapply(as.character(values), function(v) {
    parts <- strsplit(v, ":", fixed = TRUE)[[1]]
    if (length(parts) == 2L && parts[1] %in% names(facility_numeric_map)) {
      source <- parts[1]; raw <- parts[2]
      readable <- unname(facility_numeric_map[[source]][raw])
      if (is.na(readable) || !nzchar(readable)) {
        readable <- unname(facility_type_english_lookup[normalise_facility_type(raw)])
      }
      if (length(readable) == 0 || is.na(readable) || !nzchar(readable))
        readable <- paste0("Unmapped facility code: ", raw)
      return(paste0(readable, " [", source, "]"))
    }
    readable <- unname(facility_type_english_lookup[normalise_facility_type(v)])
    if (length(readable) == 0 || is.na(readable) || !nzchar(readable))
      paste0("Unmapped facility code: ", v)
    else paste0(readable, " (", v, ")")
  }, character(1))
}
facility_choices <- function(values) {
  stats::setNames(as.character(values), facility_label_vector(values))
}
format_selected_facilities <- function(values) {
  if (is.null(values) || length(values) == 0) return("All")
  paste(facility_label_vector(values), collapse = ", ")
}

# ---------------------------------------------------------------------
# BPJS administrative code mappings
# Values are retained as their original database codes for filtering.
# The interface displays clear English labels.
# ---------------------------------------------------------------------
normalise_code <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("\\.0+$", "", x)
  x
}

province_lookup <- c(
  "11" = "Aceh",
  "12" = "North Sumatra",
  "13" = "West Sumatra",
  "14" = "Riau",
  "15" = "Jambi",
  "16" = "South Sumatra",
  "17" = "Bengkulu",
  "18" = "Lampung",
  "19" = "Bangka Belitung Islands",
  "21" = "Riau Islands",
  "31" = "Jakarta",
  "32" = "West Java",
  "33" = "Central Java",
  "34" = "Yogyakarta",
  "35" = "East Java",
  "36" = "Banten",
  "51" = "Bali",
  "52" = "West Nusa Tenggara",
  "53" = "East Nusa Tenggara",
  "61" = "West Kalimantan",
  "62" = "Central Kalimantan",
  "63" = "South Kalimantan",
  "64" = "East Kalimantan",
  "65" = "North Kalimantan",
  "71" = "North Sulawesi",
  "72" = "Central Sulawesi",
  "73" = "South Sulawesi",
  "74" = "Southeast Sulawesi",
  "75" = "Gorontalo",
  "76" = "West Sulawesi",
  "81" = "Maluku",
  "82" = "North Maluku",
  "91" = "West Papua",
  "94" = "Papua",
  "92" = "Southwest Papua",
  "95" = "South Papua",
  "96" = "Central Papua",
  "97" = "Highland Papua"
)

hospital_class_lookup <- c(
  "1" = "Class A hospital",
  "2" = "Class B hospital",
  "3" = "Class C hospital",
  "4" = "Class D hospital",
  "5" = "Private hospital equivalent to Class A",
  "6" = "Private hospital equivalent to Class B",
  "7" = "Private hospital equivalent to Class C",
  "8" = "Private hospital equivalent to Class D",
  "9" = "Military/police Class I hospital",
  "10" = "Military/police Class II hospital",
  "11" = "Military/police Class III hospital",
  "12" = "Military/police Class IV hospital",
  "13" = "Specialist surgical hospital",
  "14" = "Specialist dental and oral hospital",
  "15" = "Specialist haemodialysis hospital",
  "16" = "Specialist maternal and child hospital",
  "17" = "Specialist cardiac hospital",
  "18" = "Specialist psychiatric hospital",
  "19" = "Specialist oncology hospital",
  "20" = "Specialist leprosy hospital",
  "21" = "Specialist eye hospital",
  "22" = "Specialist pulmonary hospital",
  "23" = "Specialist stroke hospital",
  "24" = "Specialist orthopaedic hospital",
  "25" = "Non-inpatient clinic",
  "26" = "Other specialist hospital",
  "27" = "Non-provider emergency hospital"
)

service_level_lookup <- c(
  "1" = "Outpatient secondary care (RJTL)",
  "2" = "Inpatient secondary care (RITL)"
)

label_from_lookup <- function(values, lookup, fallback_prefix = "Code") {
  values <- as.character(values)
  keys <- normalise_code(values)
  labels <- unname(lookup[keys])
  text_matches <- match(toupper(trimws(values)), toupper(unname(lookup)))
  missing <- is.na(labels) | labels == ""
  labels[missing & !is.na(text_matches)] <- unname(lookup)[text_matches[missing & !is.na(text_matches)]]
  missing <- is.na(labels) | labels == ""
  labels[missing] <- paste0("Unmapped ", fallback_prefix, ": ", values[missing])
  labels
}

named_choices_from_lookup <- function(values, lookup, fallback_prefix = "Code") {
  values <- as.character(values)
  labels <- label_from_lookup(values, lookup, fallback_prefix)
  stats::setNames(values, labels)
}

# ---------------------------------------------------------------------
# Standardised category labels
# Map display categories to their corresponding database values.
# Retain unmapped values as separate selectable categories.
# ---------------------------------------------------------------------
primary_facility_label <- function(v) {
  parts <- strsplit(as.character(v), ":", fixed = TRUE)[[1]]
  if (length(parts) != 2L || !parts[1] %in% names(facility_numeric_map))
    return(facility_label_vector(v))
  source <- parts[1]; raw <- trimws(parts[2])
  mapped <- unname(facility_numeric_map[[source]][raw])
  if (!length(mapped) || is.na(mapped) || !nzchar(mapped)) {
    lookup <- unname(facility_type_english_lookup[normalise_facility_type(raw)])
    if (length(lookup) && !is.na(lookup) && nzchar(lookup)) mapped <- lookup
  }
  if (!length(mapped) || is.na(mapped) || !nzchar(mapped))
    mapped <- paste0("Unmapped facility code: ", raw)
  paste0(mapped, " [", source, "]")
}

hospital_facility_label <- function(v) {
  raw <- trimws(as.character(v))
  mapped <- unname(facility_numeric_map$hospital[normalise_code(raw)])
  if (!length(mapped) || is.na(mapped) || !nzchar(mapped)) {
    english <- unname(facility_type_english_lookup[normalise_facility_type(raw)])
    if (length(english) && !is.na(english) && nzchar(english)) {
      mapped <- switch(english,
                       "Hospital" = "Hospital (Rumah Sakit)",
                       "Main clinic" = "Main clinic (Klinik Utama)",
                       "Primary care clinic" = "Primary care clinic (Klinik Pratama)",
                       paste0(english, " (", raw, ")"))
    }
  }
  if (!length(mapped) || is.na(mapped) || !nzchar(mapped))
    mapped <- paste0("Unmapped hospital facility code: ", raw)
  mapped
}

hospital_class_text_codes <- c(
  "RS KELAS A"="1", "RS KELAS B"="2", "RS KELAS C"="3", "RS KELAS D"="4",
  "KHUSUS BEDAH"="13", "KHUSUS GIGI DAN MULUT"="14",
  "KHUSUS HEMODIALISA"="15", "KHUSUS IBU DAN ANAK"="16",
  "KHUSUS JANTUNG"="17", "KHUSUS JIWA"="18",
  "KHUSUS KANKER ONKOLOGI"="19", "KHUSUS KUSTA"="20",
  "KHUSUS MATA"="21", "KHUSUS PARU"="22",
  "KHUSUS STROKE"="23", "KHUSUS ORTHOPEDI"="24",
  "KHUSUS ORTOPEDI"="24", "KHUSUS LAIN"="26"
)
hospital_class_label <- function(v) {
  raw <- trimws(as.character(v))
  key <- normalise_code(raw)
  text_code <- unname(hospital_class_text_codes[normalise_facility_type(raw)])
  if (length(text_code) && !is.na(text_code)) key <- text_code
  readable <- unname(hospital_class_lookup[key])
  if (!length(readable) || is.na(readable) || !nzchar(readable)) {
    # Translate further Indonesian hospital-class text variants via the
    # existing bilingual facility dictionary where definitions coincide.
    english <- unname(facility_type_english_lookup[normalise_facility_type(raw)])
    if (length(english) && !is.na(english) && nzchar(english)) {
      match_idx <- match(english, unname(hospital_class_lookup))
      if (!is.na(match_idx)) readable <- unname(hospital_class_lookup)[match_idx]
    }
  }
  if (!length(readable) || is.na(readable) || !nzchar(readable)) {
    # Existing canonical English descriptions can also appear verbatim.
    found <- match(normalise_facility_type(raw), normalise_facility_type(unname(hospital_class_lookup)))
    if (!is.na(found)) readable <- unname(hospital_class_lookup)[found]
  }
  if (!length(readable) || is.na(readable) || !nzchar(readable))
    readable <- paste0("Unmapped hospital class: ", raw)
  readable
}

service_level_label <- function(v) {
  raw <- normalise_facility_type(v)
  if (raw %in% c("1", "RJTL", "OUTPATIENT SECONDARY CARE (RJTL)"))
    return("Outpatient secondary care (RJTL)")
  if (raw %in% c("2", "RITL", "INPATIENT SECONDARY CARE (RITL)"))
    return("Inpatient secondary care (RITL)")
  paste0("Unmapped service level: ", as.character(v))
}

# Named list: category label -> vector of raw values, used by SQL filtering.
category_map <- function(raw_values, label_fn) {
  vals <- unique(as.character(raw_values))
  labels <- vapply(vals, label_fn, character(1))
  split(vals, factor(labels, levels = unique(labels)))
}
category_choices <- function(mapping) {
  labels <- names(mapping)
  stats::setNames(labels, labels)
}
category_filter_sql <- function(field, selections, mapping) {
  if (!length(selections)) return(NULL)
  raw <- unique(unlist(mapping[intersect(selections, names(mapping))], use.names = FALSE))
  if (!length(raw)) stop("Selected filter category is not mapped to database values")
  paste0(field, " IN (", sql_in(raw), ")")
}
format_categories <- function(selections) {
  if (!length(selections)) "All" else paste(selections, collapse = ", ")
}

format_selected_with_lookup <- function(values, lookup, fallback_prefix = "Code") {
  if (is.null(values) || length(values) == 0) return("All")
  paste(label_from_lookup(values, lookup, fallback_prefix), collapse = ", ")
}

canonical_province_label <- function(values) {
  vapply(as.character(values), function(v) {
    if (is.na(v) || !nzchar(trimws(v))) return("Unknown province")
    if (startsWith(v, "legacy:")) {
      return(paste0("Legacy province code ", substring(v, 8), " (unverified)"))
    }
    if (startsWith(v, "standard:")) {
      k <- normalise_code(substring(v, 10))
      lbl <- unname(province_lookup[k])
      if (length(lbl) && !is.na(lbl) && nzchar(lbl)) return(lbl)
      return(paste0("Unmapped province code ", k))
    }
    if (startsWith(v, "name:")) return(stringr::str_to_title(substring(v, 6)))
    # Unqualified digits have an unknown coding system; never guess the province.
    if (grepl("^[0-9]+$", v)) return(paste0("Unverified province code ", v))
    stringr::str_to_title(v)
  }, character(1))
}

make_province_map <- function(values) {
  tibble(
    raw_value = as.character(values),
    province_label = canonical_province_label(values)
  ) %>%
    filter(!is.na(raw_value), raw_value != "", !is.na(province_label), province_label != "") %>%
    distinct(raw_value, province_label)
}

province_choice_labels <- function(province_map) {
  labels <- sort(unique(province_map$province_label))
  stats::setNames(labels, labels)
}

province_raw_values <- function(selected_labels, province_map) {
  if (is.null(selected_labels) || length(selected_labels) == 0) return(character(0))
  province_map %>%
    filter(.data$province_label %in% selected_labels) %>%
    pull(.data$raw_value) %>%
    unique()
}

format_selected_provinces <- function(selected_labels) {
  if (is.null(selected_labels) || length(selected_labels) == 0) return("All")
  paste(selected_labels, collapse = ", ")
}

parse_custom_age_ranges <- function(range_text) {
  if (is.null(range_text) || trimws(range_text) == "") {
    return(tibble(label = character(0), lower = numeric(0), upper = numeric(0)))
  }
  
  tokens <- trimws(unlist(strsplit(range_text, ",")))
  tokens <- tokens[tokens != ""]
  
  parsed <- lapply(tokens, function(token) {
    x <- gsub("\\s+", "", token)
    
    if (grepl("^\\d+$", x)) {
      tibble(label = x, lower = as.numeric(x), upper = as.numeric(x))
    } else if (grepl("^\\d+-\\d+$", x)) {
      bounds <- as.numeric(unlist(strsplit(x, "-")))
      if (bounds[1] > bounds[2]) {
        stop("An age-range lower limit cannot be greater than its upper limit.")
      }
      tibble(label = x, lower = bounds[1], upper = bounds[2])
    } else if (grepl("^\\d+\\+$", x)) {
      tibble(label = x, lower = as.numeric(gsub("\\+", "", x)), upper = Inf)
    } else {
      stop(
        paste0(
          "Invalid custom age input: '", token,
          "'. Use examples such as 0-4, 18-39, 65+, or 25."
        )
      )
    }
  })
  
  bind_rows(parsed)
}

age_condition_sql <- function(age_ranges, field = "age") {
  if (nrow(age_ranges) == 0) return(NULL)
  
  clauses <- vapply(seq_len(nrow(age_ranges)), function(i) {
    lower <- age_ranges$lower[i]
    upper <- age_ranges$upper[i]
    
    if (is.infinite(upper)) {
      paste0(field, " >= ", lower)
    } else {
      paste0(field, " BETWEEN ", lower, " AND ", upper)
    }
  }, character(1))
  
  paste0("(", paste(clauses, collapse = " OR "), ")")
}

collapse_or_all <- function(x) {
  if (is.null(x) || length(x) == 0) "All" else paste(x, collapse = ", ")
}

# ---------------------------------------------------------------------
# 4. Initialise database objects and filter choices
# ---------------------------------------------------------------------
postgres_object_exists <- function(object_name) {
  if (object_name == icd_dictionary_table) return(FALSE)
  DBI::dbExistsTable(db_pool, DBI::Id(schema = adapter_schema, table = object_name))
}

required_db_objects <- c(
  primary_table,
  secondary_events_table,
  secondary_diagnoses_view
)

missing_db_objects <- required_db_objects[
  !vapply(required_db_objects, postgres_object_exists, logical(1))
]

if (length(missing_db_objects) > 0) {
  stop(
    "The following required PostgreSQL database objects are missing:\n",
    paste(missing_db_objects, collapse = ", "),
    "\n\nVerify the database setup and application views."
  )
}

icd_dictionary_available <- postgres_object_exists(icd_dictionary_table)

db_has_column <- function(table_name, column_name) {
  sql <- paste0(
    "SELECT EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema = ",
    qs(adapter_schema), " AND table_name = ", qs(table_name),
    " AND column_name = ", qs(column_name), ") AS exists")
  isTRUE(DBI::dbGetQuery(db_pool, sql)$exists[1])
}

# Shorten diagnosis labels for display while retaining original
# diagnosis values for SQL filtering.
short_diagnosis_label <- function(name, code = NA_character_, max_chars = 54L) {
  name <- stringr::str_squish(as.character(name))
  # Remove a trailing code + verbose title in parentheses, preserving any
  # genuine clinical wording before it.
  name <- sub("\\s*\\(([A-Z][0-9]{2}(?:\\.[0-9A-Z]+)?)[[:space:]]+[^)]{8,}\\)\\s*$", "", name, perl = TRUE)
  if (!nzchar(name)) name <- "Unlabelled diagnosis"
  if (nchar(name) > max_chars) name <- paste0(substr(name, 1L, max_chars - 1L), "…")
  code <- trimws(as.character(code)[1L])
  # Use only a concise ICD code, not the entire title from the source field.
  code_match <- regmatches(code, regexpr("^[A-Z][0-9]{2}(?:\\.[0-9A-Z]+)?", code, perl = TRUE))
  if (length(code_match) && nzchar(code_match)) paste0(name, " (", code_match, ")") else name
}

# Disambiguate duplicate display labels without changing database values.
unique_dx_labels <- function(short_labels, raw_names) {
  duplicates <- duplicated(short_labels) | duplicated(short_labels, fromLast = TRUE)
  short_labels[duplicates] <- paste0(short_labels[duplicates], " · ",
                                     vapply(raw_names[duplicates], function(v) {
                                       bytes <- utf8ToInt(enc2utf8(v))
                                       # deterministic small suffix for display, without extra packages
                                       sprintf("%05d", sum((seq_along(bytes) %% 97 + 1) * bytes) %% 100000)
                                     }, character(1)))
  make.unique(short_labels, sep = " #")
}

get_diagnosis_choices <- function(table_name, limit_n = 500,
                                  diagnosis_col = "diagnosis_name", code_col = "diagnosis_code") {
  # Retrieve the most frequently recorded diagnosis names using
  # server-side aggregation.
  has_code <- db_has_column(table_name, code_col)
  code_sql <- if (has_code) paste0("MIN(NULLIF(TRIM(", qi(code_col), "), '')) AS icd10_code, ")
  else "NULL::text AS icd10_code, "
  sql <- paste0(
    "SELECT TRIM(", qi(diagnosis_col), ") AS diagnosis_name, ", code_sql,
    "COUNT(*) AS n FROM ", qi(table_name),
    " WHERE ", qi(diagnosis_col), " IS NOT NULL AND TRIM(", qi(diagnosis_col), ") <> '' ",
    "AND TRIM(", qi(diagnosis_col), ") NOT IN ('9999', 'Unknown', 'UNKNOWN') ",
    "GROUP BY TRIM(", qi(diagnosis_col), ") ORDER BY n DESC LIMIT ", as.integer(limit_n))
  df <- DBI::dbGetQuery(db_pool, sql)
  if (!nrow(df)) return(list(disease_first = character(), code_first = character(), values = character()))
  has <- !is.na(df$icd10_code) & nzchar(df$icd10_code)
  disease <- mapply(short_diagnosis_label, df$diagnosis_name,
                    df$icd10_code, USE.NAMES = FALSE)
  disease <- unique_dx_labels(disease, df$diagnosis_name)
  code <- ifelse(has, paste0(df$icd10_code, " · ", disease), disease)
  list(disease_first = stats::setNames(df$diagnosis_name, disease),
       code_first = stats::setNames(df$diagnosis_name, code), values = as.character(df$diagnosis_name))
}

format_selected_diagnoses <- function(values, choices) {
  if (is.null(values) || length(values) == 0) return("All")
  labels <- names(choices)[match(values, unname(choices))]
  labels[is.na(labels) | labels == ""] <- values[is.na(labels) | labels == ""]
  paste(labels, collapse = ", ")
}

# Primary-care and combined encounter choices
primary_date_range <- db_range(primary_table, "visit_date")

primary_age_min <- 0L
primary_age_max <- 110L
primary_date_min <- as.Date(primary_date_range$min_value[1])
primary_date_max <- as.Date(primary_date_range$max_value[1])

primary_sex_choices <- db_distinct_values(primary_table, "sex")
primary_province_choices_raw <- db_distinct_values(primary_table, "province")
primary_province_map <- make_province_map(primary_province_choices_raw)
primary_province_choices <- province_choice_labels(primary_province_map)
primary_care_setting_choices <- db_distinct_values(primary_table, "care_setting")
primary_facility_choices_raw <- db_distinct_values(primary_table, "facility_type")

primary_dx_choice_objects <- get_diagnosis_choices(
  primary_table,
  limit_n = 700,
  diagnosis_col = "diagnosis_name",
  code_col = "diagnosis_code"
)
primary_dx_choices <- primary_dx_choice_objects$disease_first
primary_top_dx <- primary_dx_choice_objects$values

# Secondary-care choices
secondary_date_range <- db_range(secondary_events_table, "admission_date")
secondary_age_min <- 0L
secondary_age_max <- 110L
secondary_sex_choices <- c("Male", "Female")
secondary_date_min <- as.Date(secondary_date_range$min_value[1])
secondary_date_max <- as.Date(secondary_date_range$max_value[1])

secondary_province_choices_raw <- db_distinct_values(secondary_events_table, "secondary_care_province")
secondary_province_map <- make_province_map(secondary_province_choices_raw)
secondary_province_choices <- province_choice_labels(secondary_province_map)
secondary_facility_choices <- db_distinct_values(secondary_events_table, "facility_type")
secondary_facility_display_choices <- stats::setNames(
  secondary_facility_choices,
  vapply(secondary_facility_choices, function(v) {
    mapped <- unname(facility_numeric_map$hospital[v])
    if (length(mapped) == 0 || is.na(mapped) || !nzchar(mapped)) {
      mapped <- unname(facility_type_english_lookup[normalise_facility_type(v)])
    }
    if (length(mapped) == 0 || is.na(mapped) || !nzchar(mapped))
      paste0("Unmapped hospital facility code: ", v)
    else paste0(mapped, " [", v, "]")
  }, character(1))
)
secondary_facility_class_choices <- db_distinct_values(secondary_events_table, "facility_class")
secondary_service_level_choices <- db_distinct_values(secondary_events_table, "service_level")
secondary_discharge_choices <- db_distinct_values(secondary_events_table, "discharge_status")
primary_facility_categories <- category_map(primary_facility_choices_raw, primary_facility_label)
secondary_facility_categories <- category_map(secondary_facility_choices, hospital_facility_label)
secondary_hospital_class_categories <- category_map(secondary_facility_class_choices, hospital_class_label)
secondary_service_categories <- category_map(secondary_service_level_choices, service_level_label)

secondary_dx_choice_objects <- get_diagnosis_choices(
  secondary_diagnoses_view,
  limit_n = 900,
  diagnosis_col = "diagnosis_name",
  code_col = "diagnosis_code"
)
secondary_dx_choices <- secondary_dx_choice_objects$disease_first
secondary_top_dx <- secondary_dx_choice_objects$values

# ---------------------------------------------------------------------
# 5. Primary and combined cohort construction
# ---------------------------------------------------------------------
build_primary_cohort_sql <- function(input) {
  custom_ranges <- parse_custom_age_ranges(input$primary_custom_age_ranges)
  primary_index_dx <- input$primary_index_diagnosis
  filters <- c()
  
  if (nrow(custom_ranges) > 0) {
    filters <- c(filters, age_condition_sql(custom_ranges, "e.age"))
  } else {
    filters <- c(
      filters,
      paste0(
        "e.age BETWEEN ", as.numeric(input$primary_age_slider[1]),
        " AND ", as.numeric(input$primary_age_slider[2])
      )
    )
  }
  
  if (!identical(input$primary_sex, "All")) {
    filters <- c(filters, paste0("e.sex = ", qs(input$primary_sex)))
  }
  
  if (length(input$primary_province) > 0) {
    selected_province_values <- province_raw_values(input$primary_province, primary_province_map)
    if (length(selected_province_values) > 0) {
      filters <- c(
        filters,
        paste0("CAST(e.province AS TEXT) IN (", sql_in(selected_province_values), ")")
      )
    }
  }
  
  if (!identical(input$primary_care_setting, "All")) {
    filters <- c(
      filters,
      paste0("e.care_setting = ", qs(input$primary_care_setting))
    )
  }
  
  if (length(input$primary_facility_type) > 0) {
    filters <- c(
      filters,
      category_filter_sql("CAST(e.facility_type AS TEXT)", input$primary_facility_type, primary_facility_categories)
    )
  }
  
  general_where <- paste(filters, collapse = " AND ")
  
  index_filters <- c(
    general_where,
    paste0("e.visit_date BETWEEN ", qs(input$primary_date_range[1]), "::date AND ", qs(input$primary_date_range[2]), "::date")
  )
  
  if (length(primary_index_dx) > 0) {
    index_filters <- c(
      index_filters,
      paste0("e.diagnosis_name IN (", sql_in(primary_index_dx), ")")
    )
  }
  
  index_where <- paste(index_filters, collapse = " AND ")
  
  lookback_years <- as.integer(input$primary_lookback_years)
  history_conditions <- c()
  
  if (length(input$primary_required_prior_dx) > 0) {
    history_conditions <- c(
      history_conditions,
      paste0(
        "EXISTS (",
        "SELECT 1 FROM ", qi(primary_table), " h ",
        "WHERE h.patient_id = pi.patient_id ",
        "AND h.visit_date >= (pi.index_date - INTERVAL '", lookback_years, " years') ",
        "AND h.visit_date < pi.index_date ",
        "AND h.diagnosis_name IN (", sql_in(input$primary_required_prior_dx), ")",
        ")"
      )
    )
  }
  
  if (length(input$primary_excluded_prior_dx) > 0) {
    history_conditions <- c(
      history_conditions,
      paste0(
        "NOT EXISTS (",
        "SELECT 1 FROM ", qi(primary_table), " h ",
        "WHERE h.patient_id = pi.patient_id ",
        "AND h.visit_date >= (pi.index_date - INTERVAL '", lookback_years, " years') ",
        "AND h.visit_date < pi.index_date ",
        "AND h.diagnosis_name IN (", sql_in(input$primary_excluded_prior_dx), ")",
        ")"
      )
    )
  }
  
  if (isTRUE(input$primary_incident_only) && length(primary_index_dx) > 0) {
    history_conditions <- c(
      history_conditions,
      paste0(
        "NOT EXISTS (",
        "SELECT 1 FROM ", qi(primary_table), " h ",
        "WHERE h.patient_id = pi.patient_id ",
        "AND h.visit_date >= (pi.index_date - INTERVAL '", lookback_years, " years') ",
        "AND h.visit_date < pi.index_date ",
        "AND h.diagnosis_name IN (", sql_in(primary_index_dx), ")",
        ")"
      )
    )
  }
  
  if (isTRUE(input$primary_require_prior_encounter)) {
    history_conditions <- c(
      history_conditions,
      paste0(
        "EXISTS (",
        "SELECT 1 FROM ", qi(primary_table), " h ",
        "WHERE h.patient_id = pi.patient_id ",
        "AND h.visit_date >= (pi.index_date - INTERVAL '", lookback_years, " years') ",
        "AND h.visit_date < pi.index_date",
        ")"
      )
    )
  }
  
  history_where <- if (length(history_conditions) == 0) {
    "TRUE"
  } else {
    paste(history_conditions, collapse = " AND ")
  }
  
  final_source <- if (identical(input$primary_analysis_mode, "index_records")) {
    paste0(
      "SELECT DISTINCT ON (e.patient_id) e.* FROM ", qi(primary_table), " e ",
      "INNER JOIN final_index_events fi ",
      "ON e.patient_id = fi.patient_id AND e.visit_date = fi.index_date ",
      "AND e.visit_id IS NOT DISTINCT FROM fi.visit_id ",
      "AND e.record_source = fi.record_source ",
      "ORDER BY e.patient_id, e.visit_date, e.visit_id, e.record_source"
    )
  } else {
    paste0(
      "SELECT e.* FROM ", qi(primary_table), " e ",
      "INNER JOIN (SELECT DISTINCT patient_id FROM final_index_events) p ",
      "ON e.patient_id = p.patient_id"
    )
  }
  
  paste0(
    "WITH index_candidates AS (",
    "SELECT DISTINCT e.patient_id, e.visit_date, e.visit_id, e.record_source ",
    "FROM ", qi(primary_table), " e ",
    "WHERE ", index_where,
    "), patient_index AS (",
    "SELECT DISTINCT ON (patient_id) patient_id, visit_date AS index_date, visit_id, record_source ",
    "FROM index_candidates ORDER BY patient_id, visit_date, visit_id NULLS LAST, record_source",
    "), final_index_events AS (",
    "SELECT pi.* FROM patient_index pi WHERE ", history_where,
    "), cohort_events AS (", final_source, ") "
  )
}

# ---------------------------------------------------------------------
# 6. Secondary care cohort construction
# ---------------------------------------------------------------------
build_secondary_cohort_sql <- function(input) {
  secondary_index_dx <- input$secondary_index_diagnosis
  event_filters <- c(
    paste0(
      "e.admission_date BETWEEN ",
      qs(input$secondary_date_range[1]), "::date AND ",
      qs(input$secondary_date_range[2]), "::date"
    )
  )
  
  custom_ages <- parse_custom_age_ranges(input$secondary_custom_age_ranges)
  if (nrow(custom_ages)) {
    event_filters <- c(event_filters, age_condition_sql(custom_ages, "e.age"))
  } else if (!is.null(input$secondary_age_slider) &&
             (input$secondary_age_slider[1] > secondary_age_min ||
              input$secondary_age_slider[2] < secondary_age_max)) {
    event_filters <- c(event_filters, paste0("e.age BETWEEN ",
                                             as.integer(input$secondary_age_slider[1]), " AND ",
                                             as.integer(input$secondary_age_slider[2])))
  }
  if (!is.null(input$secondary_sex) && !identical(input$secondary_sex, "All")) {
    event_filters <- c(event_filters, paste0("e.sex = ", qs(input$secondary_sex)))
  }
  
  if (length(input$secondary_province) > 0) {
    selected_secondary_province_values <- province_raw_values(input$secondary_province, secondary_province_map)
    if (length(selected_secondary_province_values) > 0) {
      event_filters <- c(
        event_filters,
        paste0("e.secondary_care_province IN (", sql_in(selected_secondary_province_values), ")")
      )
    }
  }
  
  if (length(input$secondary_facility_type) > 0) {
    event_filters <- c(
      event_filters,
      category_filter_sql("e.facility_type", input$secondary_facility_type, secondary_facility_categories)
    )
  }
  
  if (length(input$secondary_facility_class) > 0) {
    event_filters <- c(
      event_filters,
      category_filter_sql("e.facility_class", input$secondary_facility_class, secondary_hospital_class_categories)
    )
  }
  
  if (length(input$secondary_service_level) > 0) {
    event_filters <- c(
      event_filters,
      category_filter_sql("e.service_level", input$secondary_service_level, secondary_service_categories)
    )
  }
  
  if (isTRUE(input$secondary_min_los_enabled)) {
    event_filters <- c(
      event_filters,
      paste0("COALESCE(e.length_of_stay_days, 0) >= ", as.numeric(input$secondary_min_los))
    )
  }
  
  joins <- ""
  if (length(secondary_index_dx) > 0) {
    joins <- paste0(
      "INNER JOIN ", qi(secondary_diagnoses_view), " d ",
      "ON e.secondary_visit_id = d.secondary_visit_id "
    )
    
    event_filters <- c(
      event_filters,
      paste0("d.diagnosis_name IN (", sql_in(secondary_index_dx), ")")
    )
    
    if (!identical(input$secondary_diagnosis_position, "Any")) {
      event_filters <- c(
        event_filters,
        paste0("d.diagnosis_position = ", qs(input$secondary_diagnosis_position))
      )
    }
  }
  
  index_where <- paste(event_filters, collapse = " AND ")
  
  # Use the first qualifying hospital event per patient as the index event.
  all_events_or_index <- if (identical(input$secondary_analysis_mode, "index_events")) {
    paste0(
      "SELECT e.* FROM ", qi(secondary_events_table), " e ",
      "INNER JOIN patient_index pi ",
      "ON e.secondary_visit_id = pi.secondary_visit_id"
    )
  } else {
    paste0(
      "SELECT e.* FROM ", qi(secondary_events_table), " e ",
      "INNER JOIN (SELECT DISTINCT patient_id FROM patient_index) p ",
      "ON e.patient_id = p.patient_id"
    )
  }
  
  paste0(
    "WITH index_candidates AS (",
    "SELECT DISTINCT e.patient_id, e.secondary_visit_id, e.admission_date ",
    "FROM ", qi(secondary_events_table), " e ",
    joins,
    "WHERE ", index_where,
    "), patient_index AS (",
    "SELECT DISTINCT ON (patient_id) patient_id, secondary_visit_id, admission_date AS index_date ",
    "FROM index_candidates ",
    "ORDER BY patient_id, admission_date, secondary_visit_id",
    "), cohort_events AS (", all_events_or_index, ") "
  )
}

# ---------------------------------------------------------------------
# 7. Cohort summary and flow queries
# ---------------------------------------------------------------------
query_primary_summary <- function(sql_cte) {
  DBI::dbGetQuery(
    db_pool,
    paste0(
      sql_cte,
      "SELECT ",
      "COUNT(DISTINCT patient_id) AS cohort_patients, ",
      "COUNT(*) AS records, ",
      "COUNT(DISTINCT CASE WHEN care_setting = 'Primary care' THEN patient_id END) AS primary_care_patients, ",
      "ROUND(AVG(age)::numeric, 1) AS mean_age, ",
      "ROUND(100.0 * AVG(CASE WHEN sex = 'Female' THEN 1 ELSE 0 END), 1) AS female_pct, ",
      "ROUND(100.0 * AVG(CASE WHEN sex = 'Male' THEN 1 ELSE 0 END), 1) AS male_pct ",
      "FROM cohort_events"
    )
  )
}

query_primary_flow <- function(sql_cte) {
  DBI::dbGetQuery(
    db_pool,
    paste0(
      sql_cte,
      "SELECT ",
      "(SELECT COUNT(*) FROM index_candidates) AS index_records, ",
      "(SELECT COUNT(DISTINCT patient_id) FROM index_candidates) AS index_patients, ",
      "(SELECT COUNT(*) FROM final_index_events) AS final_index_patients, ",
      "(SELECT COUNT(*) FROM cohort_events) AS cohort_records, ",
      "(SELECT COUNT(DISTINCT patient_id) FROM cohort_events) AS cohort_patients"
    )
  )
}

query_secondary_summary <- function(sql_cte) {
  DBI::dbGetQuery(
    db_pool,
    paste0(
      sql_cte,
      "SELECT ",
      "COUNT(DISTINCT patient_id) AS cohort_patients, ",
      "COUNT(*) AS hospital_events, ",
      "ROUND(AVG(length_of_stay_days)::numeric, 1) AS mean_los_days, ",
      "ROUND(PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY length_of_stay_days)::numeric, 1) AS median_los_days, ",
      "ROUND(AVG(bpjs_verified_cost)::numeric, 0) AS mean_verified_cost, ",
      "COUNT(*) FILTER (WHERE primary_care_referral_visit_id IS NOT NULL) AS events_with_primary_referral ",
      "FROM cohort_events"
    )
  )
}

query_secondary_flow <- function(sql_cte) {
  DBI::dbGetQuery(
    db_pool,
    paste0(
      sql_cte,
      "SELECT ",
      "(SELECT COUNT(*) FROM index_candidates) AS qualifying_index_events, ",
      "(SELECT COUNT(DISTINCT patient_id) FROM index_candidates) AS qualifying_index_patients, ",
      "(SELECT COUNT(*) FROM patient_index) AS final_index_patients, ",
      "(SELECT COUNT(*) FROM cohort_events) AS cohort_events, ",
      "(SELECT COUNT(DISTINCT patient_id) FROM cohort_events) AS cohort_patients"
    )
  )
}

# ---------------------------------------------------------------------
# 8. User interface
# ---------------------------------------------------------------------
ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Inter")
  ),
  
  tags$head(
    tags$script(HTML("$(document).on('shiny:busy', function(){ $('#v7-busy').show(); }); $(document).on('shiny:idle', function(){ $('#v7-busy').hide(); });")),
    tags$style(HTML("
      #v7-busy { display:none; position:fixed; inset:0; z-index:9999;
        background:rgba(248,250,253,.73); backdrop-filter:blur(2px); }
      #v7-busy .busy-content { position:absolute; top:44%; left:50%;
        transform:translate(-50%,-50%); background:white; padding:28px 36px;
        border-radius:12px; border:1px solid #d9dee3; text-align:center;
        box-shadow:0 10px 36px rgba(0,0,0,.10); font-weight:600; }
      #v7-busy .busy-wheel { margin:0 auto 15px; width:38px; height:38px;
        border:4px solid #dde6f1; border-top-color:#2c6cb0; border-radius:50%;
        animation:v7spin .85s linear infinite; }
      @keyframes v7spin { to { transform:rotate(360deg); } }
      .app-title {
        font-size: 32px;
        font-weight: 750;
        color: #1f2d3d;
        margin-bottom: 4px;
      }
      .app-subtitle {
        color: #5b6570;
        margin-bottom: 18px;
      }
      .sidebar-card, .summary-box, .plot-card, .metric-card-plain {
        background: #ffffff;
        border: 1px solid #d9dee3;
        border-radius: 10px;
      }
      .sidebar-card {
        background: #f8f9fa;
        padding: 16px;
      }
      .sidebar-section-title {
        font-size: 20px;
        font-weight: 700;
        margin-bottom: 12px;
      }
      .filter-group-title {
        color: #2c3e50;
        font-size: 16px;
        font-weight: 700;
        margin-top: 14px;
        margin-bottom: 8px;
      }
      .small-help {
        color: #5b6570;
        font-size: 13px;
        margin-top: 3px;
        margin-bottom: 8px;
      }
      .summary-box {
        padding: 14px;
        margin-bottom: 14px;
      }
      .selection-summary {
        background: #f8f9fa;
        border: 1px solid #d9dee3;
        border-radius: 8px;
        padding: 12px 14px;
      }
      .plot-card {
        padding: 14px;
        height: 100%;
      }
      .plot-title {
        color: #2c3e50;
        font-size: 17px;
        font-weight: 700;
        margin-bottom: 8px;
      }
      .metric-grid {
        display: grid;
        grid-template-columns: repeat(auto-fit, minmax(175px, 1fr));
        gap: 12px;
        margin-bottom: 14px;
      }
      .metric-card {
        border-radius: 10px;
        color: white;
        padding: 16px;
      }
      .metric-card.blue { background: #2c6cb0; }
      .metric-card.orange { background: #ef8a17; }
      .metric-card.green { background: #318a63; }
      .metric-card.purple { background: #7251a6; }
      .metric-label { font-size: 14px; font-weight: 600; }
      .metric-value { font-size: 27px; font-weight: 800; line-height: 1.18; }
      .metric-note { font-size: 12px; opacity: 0.92; margin-top: 5px; }
      .apply-btn .btn {
        width: 100%;
        font-weight: 700;
        background-color: #2c6cb0;
        border-color: #2c6cb0;
      }
      .download-row {
        display: flex;
        justify-content: flex-end;
        margin-top: 6px;
      }
      .warning-note {
        background: #fff8e8;
        border-left: 4px solid #ef8a17;
        padding: 10px 12px;
        border-radius: 4px;
        margin-bottom: 12px;
      }
      .section-intro {
        margin-bottom: 13px;
        color: #46515d;
      }
      .form-label .bi-question-circle,
      label .bi-question-circle {
        margin-left: 6px;
        color: #2c6cb0;
        cursor: help;
        vertical-align: -0.1em;
      }
      .disclosure-note {
        background: #eef7ff;
        border-left: 4px solid #2c6cb0;
        padding: 10px 12px;
        border-radius: 4px;
        margin: 8px 0 12px 0;
        color: #22313f;
      }
    "))
  ),
  
  div(id = "v7-busy", div(class = "busy-content",
                          div(class = "busy-wheel"),
                          div("Loading cohort results"),
                          div(class = "small-help", "PostgreSQL is processing your selection"))),
  
  div(class = "app-title", "Interactive Cohort Construction and Analysis System"),
  div(
    class = "app-subtitle",
    "Cohort construction and longitudinal exploration of Indonesian health insurance claims data"
  ),
  
  navset_tab(
    id = "main_tabs",
    
    nav_panel(
      "Dataset Overview",
      div(class = "section-intro",
          actionLink("overview_sources_info", "About the dataset and encounter years", icon = bsicons::bs_icon("info-circle"))),
      card(
        card_header("Data Architecture Overview"),
        card_body(
          disclosure_note(),
          tableOutput("dataset_overview_table"),
          actionButton("calculate_overview_patients", "Calculate unique patients", class = "btn-outline-primary"),
          actionLink("overview_unique_info", "About unique patient counts", icon = bsicons::bs_icon("info-circle")),
          verbatimTextOutput("overview_count_note")
        )
      ),
      br(),
      card(
        card_header("Care Coverage by Encounter Year"),
        card_body(
          p("See annual encounter volume for capitated and non-capitated primary care, hospital care and both settings combined."),
          actionButton("load_overview_coverage", "Load 2018–2024 coverage charts", class = "btn-primary"),
          
          layout_columns(
            col_widths = c(6, 6),
            div(class = "plot-card",
                div(class = "plot-title", "Primary-Care Encounter Coverage"),
                plotOutput("primary_date_coverage_plot", height = "320px")),
            div(class = "plot-card",
                div(class = "plot-title", "Secondary-Care Encounter Coverage"),
                plotOutput("secondary_date_coverage_plot", height = "320px"))
          ),
          div(class = "plot-card",
              div(class = "plot-title", "Primary vs Secondary Care: Annual Encounters"),
              plotOutput("combined_date_coverage_plot", height = "340px"))
        )
      )
    ),
    
    nav_panel(
      "Primary Cohort Builder",
      layout_columns(
        col_widths = c(4, 8),
        
        div(
          class = "sidebar-card",
          div(class = "sidebar-section-title", "Primary / Combined Cohort Filters"),
          disclosure_note(),
          
          div(class = "filter-group-title", "Demographics"),
          sliderInput(
            "primary_age_slider",
            help_label("Age range", "Restricts the cohort to patients whose age falls within this range at the encounter used for filtering."),
            min = primary_age_min, max = primary_age_max,
            value = c(primary_age_min, primary_age_max), step = 1
          ),
          textInput(
            "primary_custom_age_ranges",
            help_label("Custom age groups (optional)", "Enter custom age groups such as 0-4, 18-39 or 65+. When used, these replace the age slider."),
            placeholder = "Example: 0-4, 18-39, 65+"
          ),
          div(
            class = "small-help",
            "When entered, custom age groups replace the age slider. Single ages such as 25 are also supported."
          ),
          selectInput(
            "primary_sex", help_label("Sex", "Restricts the cohort by recorded sex in the demographics layer."),
            choices = c("All", primary_sex_choices), selected = "All"
          ),
          selectizeInput(
            "primary_province", help_label("Province", "Patient residence province (not facility location). Codes from unverified legacy systems are explicitly labelled and are not silently translated."),
            choices = primary_province_choices, multiple = TRUE,
            options = list(placeholder = "Select one or more provinces")
          ),
          
          tags$hr(),
          
          div(class = "filter-group-title", "Index Event Definition"),
          selectizeInput(
            "primary_index_diagnosis", help_label("Primary index diagnosis", "Select diagnosis names. Labels show disease name followed by ICD-10 code when available."),
            choices = primary_dx_choices, multiple = TRUE,
            options = list(
              placeholder = "Select one or more diagnoses",
              maxOptions = 2000
            )
          ),
          div(
            class = "small-help",
            "Patients enter the cohort at their earliest qualifying encounter in the selected date range. The diagnosis field can be searched by disease name or ICD-10 code."
          ),
          
          tags$hr(),
          
          div(class = "filter-group-title", "Healthcare Setting"),
          selectInput(
            "primary_care_setting", help_label("Care setting", "Choose whether to include primary care, secondary care, or both care settings in the cohort definition."),
            choices = c("All", primary_care_setting_choices), selected = "All"
          ),
          selectizeInput(
            "primary_facility_type", help_label("Facility type", "Restricts the cohort to selected facility types. Labels are translated where possible but database values are retained for filtering."),
            choices = category_choices(primary_facility_categories),
            multiple = TRUE,
            options = list(placeholder = "Select one or more facility types")
          ),
          
          tags$hr(),
          
          div(class = "filter-group-title", "Index Date Window"),
          dateRangeInput(
            "primary_date_range", help_label("Index date range", "The index event must occur within this date window."),
            start = primary_date_min, end = primary_date_max,
            min = primary_date_min, max = primary_date_max,
            format = "dd/mm/yyyy"
          ),
          
          tags$hr(),
          
          div(class = "filter-group-title", "Longitudinal History Criteria"),
          sliderInput(
            "primary_lookback_years", help_label("Lookback period within available data (years)", "Number of years before the index event used to check prior diagnoses or prior encounters. This is limited to available claims history."),
            min = 1, max = 10, value = 3, step = 1
          ),
          selectizeInput(
            "primary_required_prior_dx", help_label("Required prior recorded diagnosis", "Only include patients who had one of these diagnoses recorded before the index event within the lookback period."),
            choices = primary_dx_choices, multiple = TRUE,
            options = list(placeholder = "Optional")
          ),
          selectizeInput(
            "primary_excluded_prior_dx", help_label("Excluded prior recorded diagnosis", "Exclude patients who had one of these diagnoses recorded before the index event within the lookback period."),
            choices = primary_dx_choices, multiple = TRUE,
            options = list(placeholder = "Optional")
          ),
          checkboxInput(
            "primary_incident_only",
            help_label("Require no prior recorded index diagnosis", "Approximates incident cases by excluding patients with the same index diagnosis recorded earlier in the available claims history."),
            value = FALSE
          ),
          checkboxInput(
            "primary_require_prior_encounter",
            help_label("Require a prior encounter in the lookback period", "Requires evidence that the patient was observed in the dataset before the index event."),
            value = FALSE
          ),
          div(
            class = "small-help",
            "History criteria only use records available in this database. They do not prove the absence of disease outside the extract."
          ),
          
          tags$hr(),
          
          radioButtons(
            "primary_analysis_mode", help_label("Analysis records", "Index records only keeps the first qualifying record per patient. All available records keeps all records for selected cohort patients."),
            choices = c(
              "All available records for cohort patients" = "all_records",
              "Index records only" = "index_records"
            ),
            selected = "index_records"
          ),
          
          div(class = "apply-btn", actionButton("apply_primary", "Apply Primary Cohort Filters")),
          div(class = "reset-btn", actionButton("reset_primary_filters", "Reset Primary Filters"))
        ),
        
        div(
          div(class = "plot-title", "Primary / Combined Cohort Results"),
          div(class = "summary-box", uiOutput("primary_selection_summary")),
          uiOutput("primary_metric_boxes"),
          layout_columns(
            col_widths = c(6, 6),
            div(
              class = "plot-card",
              div(class = "plot-title", "Patient age and sex distribution"),
              plotOutput("primary_age_sex_plot", height = "340px"),
              div(class = "download-row", downloadButton("download_primary_age_sex", "Download PNG"))
            ),
            div(
              class = "plot-card",
              div(class = "plot-title", "Top diagnoses"),
              plotOutput("primary_top_dx_plot", height = "420px"),
              div(class = "download-row", downloadButton("download_primary_top_dx", "Download PNG"))
            ),
            div(
              class = "plot-card",
              div(class = "plot-title", "Monthly utilisation by care setting"),
              plotOutput("primary_utilisation_plot", height = "340px"),
              div(class = "download-row", downloadButton("download_primary_utilisation", "Download PNG"))
            ),
            div(
              class = "plot-card",
              div(class = "plot-title", "Top patient provinces"),
              plotOutput("primary_province_plot", height = "340px"),
              div(class = "download-row", downloadButton("download_primary_province", "Download PNG"))
            )
          ),
          br(),
          card(
            card_header("Primary Cohort Construction Flow"),
            card_body(tableOutput("primary_flow_table"))
          )
        )
      )
    ),
    
    nav_panel(
      "Secondary Care & Hospital Outcomes",
      layout_columns(
        col_widths = c(4, 8),
        
        div(
          class = "sidebar-card",
          div(class = "sidebar-section-title", "Secondary-Care Cohort Filters"),
          disclosure_note(),
          
          div(class = "filter-group-title", "Demographics"),
          sliderInput("secondary_age_slider",
                      help_label("Age range (years)", "Age at the index hospital encounter. The default includes all ages, including unknown ages."),
                      min = secondary_age_min, max = secondary_age_max,
                      value = c(secondary_age_min, secondary_age_max), step = 1),
          textInput("secondary_custom_age_ranges",
                    help_label("Custom age groups (optional)", "Enter multiple ranges such as 0-4, 18-39, 65+ or single ages. Replaces the age slider."),
                    placeholder = "Example: 0-4, 18-39, 65+"),
          selectInput("secondary_sex",
                      help_label("Sex", "Sex from the hospital record or the patient demographics lookup."),
                      choices = c("All", secondary_sex_choices), selected = "All"),
          div(class = "small-help", "The default age range does not exclude records with unknown age. Selecting a narrower range or custom ages requires a valid age."),
          tags$hr(),
          
          div(class = "filter-group-title", "Hospital Index Event"),
          dateRangeInput(
            "secondary_date_range", help_label("Admission date range", "The hospital index event must have an admission date within this range."),
            start = secondary_date_min, end = secondary_date_max,
            min = secondary_date_min, max = secondary_date_max,
            format = "dd/mm/yyyy"
          ),
          selectizeInput(
            "secondary_index_diagnosis", help_label("Index diagnosis", "Select hospital diagnoses. Labels show disease name followed by ICD-10 code when available."),
            choices = secondary_dx_choices, multiple = TRUE,
            options = list(
              placeholder = "Select one or more diagnoses",
              maxOptions = 2500
            )
          ),
          radioButtons(
            "secondary_diagnosis_position", help_label("Diagnosis position", "Choose whether the selected diagnosis can appear as any diagnosis, only the primary diagnosis, or only a secondary diagnosis."),
            choices = c(
              "Any diagnosis position" = "Any",
              "Primary diagnosis only" = "Primary",
              "Secondary diagnosis only" = "Secondary"
            ),
            selected = "Any"
          ),
          div(
            class = "small-help",
            "The first qualifying hospital admission for each patient is used as their index hospital event."
          ),
          
          tags$hr(),
          
          div(class = "filter-group-title", "Hospital and Service Characteristics"),
          selectizeInput(
            "secondary_province", help_label("Hospital province", "Hospital facility province. Verified 34- and 38-province administrative codes and 2021 province names share labels; unresolved codes remain separate."),
            choices = secondary_province_choices, multiple = TRUE,
            options = list(placeholder = "Select one or more provinces")
          ),
          selectizeInput(
            "secondary_facility_type", help_label("Hospital facility type", "Restricts hospital events to selected facility types."),
            choices = category_choices(secondary_facility_categories), multiple = TRUE,
            options = list(placeholder = "Select one or more facility types")
          ),
          selectizeInput(
            "secondary_facility_class", help_label("Hospital class", "Restricts hospital events to selected BPJS hospital class categories. Numeric codes are displayed as readable labels."),
            choices = category_choices(secondary_hospital_class_categories), multiple = TRUE,
            options = list(placeholder = "Select one or more facility classes")
          ),
          selectizeInput(
            "secondary_service_level", help_label("Service level", "Restricts hospital events to outpatient secondary care or inpatient secondary care."),
            choices = category_choices(secondary_service_categories), multiple = TRUE,
            options = list(placeholder = "Select one or more service levels")
          ),
          checkboxInput(
            "secondary_min_los_enabled",
            help_label("Apply a minimum length-of-stay filter", "When enabled, only hospital events with length of stay greater than or equal to the selected number of days are included."),
            value = FALSE
          ),
          conditionalPanel(
            condition = "input.secondary_min_los_enabled === true",
            numericInput(
              "secondary_min_los",
              help_label("Minimum length of stay (days)", "Minimum number of days between admission and discharge. Zero may represent same-day or outpatient secondary-care events."),
              value = 1, min = 0, step = 1
            )
          ),
          
          tags$hr(),
          
          radioButtons(
            "secondary_analysis_mode", help_label("Analysis records", "Index hospital events only keeps the first qualifying hospital event per patient. All available hospital events keeps all hospital events for selected cohort patients."),
            choices = c(
              "All available hospital events for cohort patients" = "all_events",
              "Index hospital events only" = "index_events"
            ),
            selected = "index_events"
          ),
          
          div(class = "apply-btn", actionButton("apply_secondary", "Apply Secondary-Care Filters")),
          div(class = "reset-btn", actionButton("reset_secondary_filters", "Reset Secondary-Care Filters"))
        ),
        
        div(
          div(class = "plot-title", "Secondary Care and Hospital Outcomes"),
          
          div(class = "summary-box", uiOutput("secondary_selection_summary")),
          uiOutput("secondary_metric_boxes"),
          
          layout_columns(
            col_widths = c(6, 6),
            
            div(
              class = "plot-card",
              div(class = "plot-title", "Hospital cohort age and sex distribution"),
              plotOutput("secondary_age_sex_plot", height = "350px"),
              div(class = "download-row", downloadButton("download_secondary_age_sex", "Download PNG"))
            ),
            div(
              class = "plot-card",
              div(class = "plot-title", "Length of stay distribution"),
              plotOutput("secondary_los_plot", height = "350px"),
              div(class = "download-row", downloadButton("download_secondary_los", "Download PNG"))
            ),
            
            div(
              class = "plot-card",
              div(class = "plot-title", "Discharge status"),
              plotOutput("secondary_discharge_plot", height = "350px"),
              div(class = "download-row", downloadButton("download_secondary_discharge", "Download PNG"))
            ),
            
            div(
              class = "plot-card",
              div(class = "plot-title", "Most common secondary diagnoses"),
              plotOutput("secondary_comorbidity_plot", height = "430px"),
              div(class = "download-row", downloadButton("download_secondary_comorbidity", "Download PNG"))
            ),
            
            div(
              class = "plot-card",
              div(class = "plot-title", "Monthly hospital admissions"),
              plotOutput("secondary_monthly_plot", height = "350px"),
              div(class = "download-row", downloadButton("download_secondary_monthly", "Download PNG"))
            ),
            
            div(
              class = "plot-card",
              div(class = "plot-title", "Verified BPJS cost by primary diagnosis"),
              plotOutput("secondary_cost_plot", height = "430px"),
              div(class = "download-row", downloadButton("download_secondary_cost", "Download PNG"))
            )
          ),
          
          br(),
          card(
            card_header("Secondary-Care Cohort Construction Flow"),
            card_body(tableOutput("secondary_flow_table"))
          )
        )
      )
    ),
    
    nav_panel(
      "Longitudinal Analysis",
      layout_columns(
        col_widths = c(4, 8),
        div(class = "sidebar-card",
            div(class = "sidebar-section-title", "Longitudinal Follow-up"),
            disclosure_note(),
            p("First select and apply a cohort in either existing cohort-builder tab."),
            radioButtons("long_index_source", help_label("Index cohort source", "Use the patient index dates created in the selected cohort-builder tab."),
                         choices = c("Primary / Combined cohort" = "primary",
                                     "Secondary-care cohort" = "secondary"), selected = "primary"),
            sliderInput("long_followup_years", help_label("Follow-up window (years)", "Recorded encounters after each index date, up to this many calendar years. Follow-up may be incomplete near the end of the data."),
                        min = 1, max = 5, value = 3, step = 1),
            checkboxInput("long_include_index_day", "Include encounters on index day", value = FALSE),
            checkboxInput("long_complete_only",
                          help_label("Require complete selected follow-up window",
                                     "Only include patients whose index date allows the full requested follow-up before 31 December 2024. This assesses database-calendar coverage, not continuous insurance enrolment."),
                          value = FALSE),
            div(class = "apply-btn", actionButton("apply_longitudinal", "Run Longitudinal Analysis"))
        ),
        div(
          div(class = "plot-title", "Longitudinal Cohort Results"),
          div(class = "summary-box", uiOutput("long_selection_summary")),
          uiOutput("long_metric_boxes"),
          layout_columns(col_widths = c(6, 6),
                         div(class = "plot-card", div(class = "plot-title", "Primary vs secondary care encounters by calendar year"),
                             plotOutput("long_care_plot", height = "370px"),
                             div(class = "download-row", downloadButton("download_long_plot", "Download PNG"))),
                         div(class = "plot-card", div(class = "plot-title", "Encounters by year since cohort entry"),
                             plotOutput("long_since_plot", height = "370px"),
                             div(class = "download-row", downloadButton("download_long_since_plot", "Download PNG")))
          ),
          br(), card(card_header("Follow-up summary"), card_body(tableOutput("long_summary_table"))),
          br(), card(
            card_header("Follow-up observation eligibility"),
            card_body(
              p("Counts of selected cohort patients who could enter or complete each follow-up year before the extract ends on 31 December 2024. Calendar coverage does not establish continuous insurance enrolment."),
              actionButton("calculate_long_eligibility", "Calculate follow-up eligibility"),
              div(class = "small-help", "Optional calculation: the index-cohort query may take time for a large cohort."),
              tableOutput("long_eligibility_table")
            )
          ),
          div(class = "download-row", downloadButton("download_long_csv", "Download longitudinal summary CSV")),
          div(class = "small-help", "These are recorded claims within the available dataset, not complete observation or validated incident disease and readmission rates. Source year is preserved independently of actual encounter dates.")
        )
      )
    ),
    
    nav_panel(
      "Export",
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          card_header("Primary Cohort Summary"),
          card_body(
            p("Download the current primary / combined cohort summary and cohort-construction flow."),
            downloadButton("download_primary_summary", "Download Primary Cohort Summary CSV")
          )
        ),
        
        card(
          card_header("Secondary-Care Cohort Summary"),
          card_body(
            p("Download aggregate hospital outcomes for the current secondary-care cohort."),
            downloadButton("download_secondary_summary", "Download Secondary-Care Summary CSV")
          )
        ),
        
        card(
          card_header("Secondary-Care Diagnosis Summary"),
          card_body(
            p("Download aggregated primary and secondary diagnosis frequencies for the selected hospital cohort."),
            downloadButton("download_secondary_dx_summary", "Download Secondary-Care Diagnosis Summary CSV")
          )
        ),
        
        card(
          card_header("Applied Cohort Characteristics"),
          card_body(
            p("Displays the settings saved when Apply Filters was last clicked, not any unsaved changes currently in the filter panels."),
            h5("Primary / Combined Cohort"),
            tableOutput("export_primary_characteristics"),
            downloadButton("download_primary_characteristics", "Download Primary Definition CSV"),
            tags$hr(),
            h5("Secondary-Care Cohort"),
            tableOutput("export_secondary_characteristics"),
            downloadButton("download_secondary_characteristics", "Download Secondary Definition CSV")
          )
        ),
        
        card(
          card_header("Longitudinal Analysis Settings"),
          card_body(
            tableOutput("export_longitudinal_characteristics"),
            downloadButton("download_longitudinal_characteristics", "Download Follow-up Definition CSV")
          )
        ),
        
        card(
          card_header("Download All Graphs (ZIP)"),
          card_body(
            p("Download all available overview, primary-care, secondary-care and longitudinal graphs as PNG files in one ZIP archive."),
            p("Apply the relevant cohort filters first. The longitudinal graphs are included after running Longitudinal Analysis. Small cohorts are excluded."),
            downloadButton("download_all_graphs_zip", "Download All Graphs ZIP")
          )
        ),
        
        card(
          card_header("Disclosure Control"),
          card_body(
            actionButton("show_export_disclosure", "View disclosure warning",
                         icon = bsicons::bs_icon("exclamation-triangle"),
                         class = "btn-outline-warning")
          )
        )
      )
    )
  )
)

# ---------------------------------------------------------------------
# 9. Application server logic
# ---------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Information opens only on request; no extra database queries.
  observeEvent(input$overview_sources_info, {
    showModal(modalDialog(
      title = "Dataset and encounter years",
      "Explore source layers and recorded encounter coverage. Source years and actual encounter years are different; graphs use encounter dates.",
      easyClose = TRUE, footer = modalButton("Close")))
  })
  observeEvent(input$overview_unique_info, {
    showModal(modalDialog(
      title = "Unique patient counts",
      "Exact distinct counts can take time on this dataset. Hospital secondary diagnoses have no direct patient_id; unique patients are not shown for that layer.",
      easyClose = TRUE, footer = modalButton("Close")))
  })
  show_disclosure_modal <- function() {
    showModal(modalDialog(
      title = "Disclosure control warning",
      paste0("Detailed downloadable outputs are blocked when fewer than ",
             small_n_threshold, " unique patients are selected. Small cells may also be suppressed in tables and plots. Do not attempt to identify suppressed counts from multiple exports."),
      easyClose = TRUE, footer = modalButton("I understand")))
  }
  # Warn once when Export is first opened; the button reopens the explanation.
  export_warning_seen <- reactiveVal(FALSE)
  observeEvent(input$main_tabs, {
    if (identical(input$main_tabs, "Export") && !isTRUE(export_warning_seen())) {
      export_warning_seen(TRUE)
      show_disclosure_modal()
    }
  }, ignoreInit = TRUE)
  observeEvent(input$show_export_disclosure, {
    show_disclosure_modal()
  })
  
  # Capture cohort definitions when filters are applied.
  # Subsequent unapplied changes are excluded from these definitions.
  primary_definition_keys <- c(
    "primary_age_slider", "primary_custom_age_ranges", "primary_sex",
    "primary_province", "primary_index_diagnosis", "primary_care_setting",
    "primary_facility_type", "primary_date_range", "primary_lookback_years",
    "primary_required_prior_dx", "primary_excluded_prior_dx",
    "primary_incident_only", "primary_require_prior_encounter",
    "primary_analysis_mode")
  secondary_definition_keys <- c(
    "secondary_age_slider", "secondary_custom_age_ranges", "secondary_sex",
    "secondary_date_range", "secondary_index_diagnosis", "secondary_diagnosis_position",
    "secondary_province", "secondary_facility_type", "secondary_facility_class",
    "secondary_service_level", "secondary_min_los_enabled", "secondary_min_los",
    "secondary_analysis_mode")
  long_definition_keys <- c("long_index_source", "long_followup_years",
                            "long_include_index_day", "long_complete_only")
  
  capture_definition <- function(keys) {
    stats::setNames(lapply(keys, function(key) isolate(input[[key]])), keys)
  }
  applied_primary_definition <- eventReactive(input$apply_primary,
                                              capture_definition(primary_definition_keys), ignoreNULL = TRUE)
  applied_secondary_definition <- eventReactive(input$apply_secondary,
                                                capture_definition(secondary_definition_keys), ignoreNULL = TRUE)
  applied_long_definition <- eventReactive(input$apply_longitudinal,
                                           capture_definition(long_definition_keys), ignoreNULL = TRUE)
  
  # Format applied cohort characteristics for display and export.
  # Retain input identifiers to support reproducibility.
  definition_table <- function(definition, cohort_type) {
    pretty <- c(
      primary_age_slider = "Age range (years)",
      primary_custom_age_ranges = "Custom age ranges",
      primary_sex = "Sex", primary_province = "Province",
      primary_index_diagnosis = "Index diagnosis", primary_care_setting = "Care setting",
      primary_facility_type = "Facility type", primary_date_range = "Index date range",
      primary_lookback_years = "Lookback (years)",
      primary_required_prior_dx = "Required prior diagnosis",
      primary_excluded_prior_dx = "Excluded prior diagnosis",
      primary_incident_only = "No previous index diagnosis",
      primary_require_prior_encounter = "Prior encounter required",
      primary_analysis_mode = "Analysis mode",
      secondary_age_slider = "Age range (years)",
      secondary_custom_age_ranges = "Custom age ranges",
      secondary_sex = "Sex", secondary_date_range = "Index date range",
      secondary_index_diagnosis = "Index diagnosis",
      secondary_diagnosis_position = "Diagnosis position",
      secondary_province = "Hospital province",
      secondary_facility_type = "Hospital facility type",
      secondary_facility_class = "Hospital class",
      secondary_service_level = "Service level",
      secondary_min_los_enabled = "Minimum length-of-stay filter enabled",
      secondary_min_los = "Minimum length of stay (days)",
      secondary_analysis_mode = "Analysis mode",
      long_index_source = "Index cohort source",
      long_followup_years = "Follow-up window (years)",
      long_include_index_day = "Include index day",
      long_complete_only = "Require complete calendar follow-up")
    values <- vapply(names(definition), function(key) {
      val <- definition[[key]]
      if (is.null(val) || length(val) == 0L ||
          (length(val) == 1L && is.na(val))) return("All / not specified")
      if (is.logical(val)) return(ifelse(isTRUE(val), "Yes", "No"))
      if (inherits(val, "Date")) return(paste(format(val, "%d/%m/%Y"), collapse = " to "))
      if (key %in% c("primary_age_slider", "secondary_age_slider"))
        return(paste(val, collapse = " to "))
      if (key %in% c("primary_index_diagnosis", "primary_required_prior_dx",
                     "primary_excluded_prior_dx"))
        return(format_selected_diagnoses(val, primary_dx_choices))
      if (key == "secondary_index_diagnosis")
        return(format_selected_diagnoses(val, secondary_dx_choices))
      if (key %in% c("primary_facility_type", "secondary_facility_type",
                     "secondary_facility_class", "secondary_service_level"))
        return(format_categories(val))
      if (key %in% c("primary_province", "secondary_province"))
        return(format_selected_provinces(val))
      if (length(val) == 1L && !nzchar(trimws(as.character(val)))) return("Not applied")
      paste(as.character(val), collapse = ", ")
    }, character(1))
    data.frame(`Cohort` = cohort_type, `Characteristic` = unname(pretty[names(definition)]),
               `Applied selection` = values, `Input ID` = names(definition),
               check.names = FALSE, stringsAsFactors = FALSE)
  }
  output$export_primary_characteristics <- renderTable({
    req(input$apply_primary > 0)
    definition_table(applied_primary_definition(), "Primary / Combined")[, 2:3]
  }, striped = TRUE, bordered = TRUE, spacing = "s", rownames = FALSE)
  output$export_secondary_characteristics <- renderTable({
    req(input$apply_secondary > 0)
    definition_table(applied_secondary_definition(), "Secondary care")[, 2:3]
  }, striped = TRUE, bordered = TRUE, spacing = "s", rownames = FALSE)
  output$export_longitudinal_characteristics <- renderTable({
    req(input$apply_longitudinal > 0)
    definition_table(applied_long_definition(), "Longitudinal follow-up")[, 2:3]
  }, striped = TRUE, bordered = TRUE, spacing = "s", rownames = FALSE)
  output$download_primary_characteristics <- downloadHandler(
    filename = function() paste0("primary_cohort_definition_", Sys.Date(), ".csv"),
    content = function(file) {
      req(input$apply_primary > 0, !primary_small_n())
      write.csv(definition_table(applied_primary_definition(), "Primary / Combined"), file, row.names = FALSE)
    })
  output$download_secondary_characteristics <- downloadHandler(
    filename = function() paste0("secondary_cohort_definition_", Sys.Date(), ".csv"),
    content = function(file) {
      req(input$apply_secondary > 0, !secondary_small_n())
      write.csv(definition_table(applied_secondary_definition(), "Secondary care"), file, row.names = FALSE)
    })
  output$download_longitudinal_characteristics <- downloadHandler(
    filename = function() paste0("longitudinal_definition_", Sys.Date(), ".csv"),
    content = function(file) {
      req(input$apply_longitudinal > 0, !longitudinal_small_n())
      write.csv(definition_table(applied_long_definition(), "Longitudinal follow-up"), file, row.names = FALSE)
    })
  
  
  # Reset buttons restore each cohort tab to its default state.
  observeEvent(input$reset_primary_filters, {
    updateSliderInput(session, "primary_age_slider", value = c(primary_age_min, primary_age_max))
    updateTextInput(session, "primary_custom_age_ranges", value = "")
    updateSelectInput(session, "primary_sex", selected = "All")
    updateSelectizeInput(session, "primary_province", selected = character(0), server = FALSE)
    updateSelectizeInput(session, "primary_index_diagnosis", selected = character(0), server = FALSE)
    updateSelectInput(session, "primary_care_setting", selected = "All")
    updateSelectizeInput(
      session,
      "primary_facility_type",
      choices = category_choices(primary_facility_categories),
      selected = character(0),
      server = FALSE
    )
    updateDateRangeInput(session, "primary_date_range", start = primary_date_min, end = primary_date_max)
    updateSliderInput(session, "primary_lookback_years", value = 3)
    updateSelectizeInput(session, "primary_required_prior_dx", selected = character(0), server = FALSE)
    updateSelectizeInput(session, "primary_excluded_prior_dx", selected = character(0), server = FALSE)
    updateCheckboxInput(session, "primary_incident_only", value = FALSE)
    updateCheckboxInput(session, "primary_require_prior_encounter", value = FALSE)
    updateRadioButtons(session, "primary_analysis_mode", selected = "index_records")
  })
  
  observeEvent(input$reset_secondary_filters, {
    updateSliderInput(session, "secondary_age_slider", value = c(secondary_age_min, secondary_age_max))
    updateTextInput(session, "secondary_custom_age_ranges", value = "")
    updateSelectInput(session, "secondary_sex", selected = "All")
    updateDateRangeInput(session, "secondary_date_range", start = secondary_date_min, end = secondary_date_max)
    updateSelectizeInput(session, "secondary_index_diagnosis", selected = character(0), server = FALSE)
    updateRadioButtons(session, "secondary_diagnosis_position", selected = "Any")
    updateSelectizeInput(session, "secondary_province", selected = character(0), server = FALSE)
    updateSelectizeInput(session, "secondary_facility_type", selected = character(0), server = FALSE)
    updateSelectizeInput(session, "secondary_facility_class", selected = character(0), server = FALSE)
    updateSelectizeInput(session, "secondary_service_level", selected = character(0), server = FALSE)
    updateCheckboxInput(session, "secondary_min_los_enabled", value = FALSE)
    updateNumericInput(session, "secondary_min_los", value = 1)
    updateRadioButtons(session, "secondary_analysis_mode", selected = "index_events")
  })
  
  # Keep primary facility choices aligned with care-setting selection,
  # without clearing a valid selection when the list updates.
  observeEvent(input$primary_care_setting, {
    available <- primary_facility_choices_raw
    
    if (!identical(input$primary_care_setting, "All")) {
      sql <- paste0(
        "SELECT DISTINCT facility_type AS value FROM ", qi(primary_table),
        " WHERE care_setting = ", qs(input$primary_care_setting),
        " AND facility_type IS NOT NULL ORDER BY value"
      )
      available <- as.character(DBI::dbGetQuery(db_pool, sql)$value)
    }
    
    available_map <- category_map(available, primary_facility_label)
    preserved_selection <- intersect(input$primary_facility_type, names(available_map))
    
    updateSelectizeInput(
      session,
      "primary_facility_type",
      choices = category_choices(category_map(available, primary_facility_label)),
      selected = preserved_selection,
      server = FALSE
    )
  }, ignoreInit = FALSE)
  
  # -------------------------------
  # Primary and combined cohort analysis
  # -------------------------------
  primary_sql_cte <- eventReactive(input$apply_primary, {
    tryCatch(
      build_primary_cohort_sql(input),
      error = function(e) {
        showNotification(e$message, type = "error", duration = 8)
        NULL
      }
    )
  }, ignoreNULL = TRUE)
  
  primary_summary <- reactive({
    req(primary_sql_cte())
    query_primary_summary(primary_sql_cte())
  })
  
  primary_flow <- reactive({
    req(primary_sql_cte())
    query_primary_flow(primary_sql_cte())
  })
  
  primary_small_n <- reactive({
    out <- primary_summary()
    is.na(out$cohort_patients[1]) || out$cohort_patients[1] < small_n_threshold
  })
  
  primary_plot_age_sex <- reactive({
    req(primary_sql_cte())
    data <- DBI::dbGetQuery(
      db_pool,
      paste0(
        primary_sql_cte(),
        "SELECT ",
        "CASE ",
        "WHEN age <= 4 THEN '0-4' ",
        "WHEN age <= 17 THEN '5-17' ",
        "WHEN age <= 44 THEN '18-44' ",
        "WHEN age <= 64 THEN '45-64' ",
        "WHEN age <= 74 THEN '65-74' ",
        "WHEN age <= 84 THEN '75-84' ",
        "WHEN age >= 85 THEN '85+' ",
        "ELSE 'Unknown' END AS age_group_display, ",
        "sex, COUNT(DISTINCT patient_id) AS n ",
        "FROM cohort_events ",
        "GROUP BY age_group_display, sex"
      )
    )
    
    data$age_group_display <- factor(
      data$age_group_display,
      levels = c("0-4", "5-17", "18-44", "45-64", "65-74", "75-84", "85+", "Unknown")
    )
    
    ggplot(data, aes(age_group_display, n, fill = sex)) +
      geom_col(position = "dodge") +
      labs(x = "Age group", y = "Patients", fill = "Sex") +
      scale_y_continuous(labels = comma) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })
  
  primary_plot_top_dx <- reactive({
    req(primary_sql_cte())
    data <- DBI::dbGetQuery(
      db_pool,
      paste0(
        primary_sql_cte(),
        "SELECT diagnosis_name, COUNT(*) AS n ",
        "FROM cohort_events ",
        "WHERE diagnosis_name IS NOT NULL AND diagnosis_name NOT IN ('Unknown', '9999') ",
        "GROUP BY diagnosis_name HAVING COUNT(DISTINCT patient_id) >= 5 ORDER BY n DESC LIMIT 15"
      )
    )
    
    data <- data %>%
      mutate(
        diagnosis_label = names(primary_dx_choices)[match(diagnosis_name, unname(primary_dx_choices))],
        diagnosis_label = ifelse(is.na(diagnosis_label) | diagnosis_label == "",
                                 vapply(diagnosis_name, short_diagnosis_label, character(1)), diagnosis_label)
      )
    
    ggplot(data, aes(reorder(diagnosis_label, n), n)) +
      geom_col() +
      coord_flip() +
      labs(x = NULL, y = "Records", caption = "Diagnosis labels abbreviated for display only") +
      scale_y_continuous(labels = comma) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())
  })
  
  primary_plot_utilisation <- reactive({
    req(primary_sql_cte())
    data <- DBI::dbGetQuery(
      db_pool,
      paste0(
        primary_sql_cte(),
        "SELECT DATE_TRUNC('month', visit_date)::date AS month, care_setting, COUNT(*) AS n ",
        "FROM cohort_events ",
        "GROUP BY month, care_setting HAVING COUNT(DISTINCT patient_id) >= 5 ORDER BY month"
      )
    )
    
    ggplot(data, aes(month, n, colour = care_setting)) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.8) +
      labs(x = "Month", y = "Records", colour = "Care setting") +
      scale_y_continuous(labels = comma) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })
  
  primary_plot_province <- reactive({
    req(primary_sql_cte())
    data <- DBI::dbGetQuery(
      db_pool,
      paste0(
        primary_sql_cte(),
        "SELECT CAST(province AS TEXT) AS province, COUNT(DISTINCT patient_id) AS n ",
        "FROM cohort_events ",
        "WHERE province IS NOT NULL ",
        "GROUP BY province HAVING COUNT(DISTINCT patient_id) >= 5 ORDER BY n DESC LIMIT 12"
      )
    )
    
    data <- data %>%
      mutate(province_label = canonical_province_label(province))
    
    ggplot(data, aes(reorder(province_label, n), n)) +
      geom_col() +
      coord_flip() +
      labs(x = NULL, y = "Patients") +
      scale_y_continuous(labels = comma) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())
  })
  
  output$primary_selection_summary <- renderUI({
    tagList(
      div(
        class = "selection-summary",
        HTML(paste0(
          "<b>Population filters:</b><br>",
          "Age = ",
          ifelse(
            trimws(input$primary_custom_age_ranges) == "",
            paste(input$primary_age_slider[1], "to", input$primary_age_slider[2]),
            htmltools::htmlEscape(input$primary_custom_age_ranges)
          ),
          "<br>Sex = ", htmltools::htmlEscape(input$primary_sex),
          "<br>Province = ", htmltools::htmlEscape(format_selected_provinces(input$primary_province)),
          "<br>Care setting = ", htmltools::htmlEscape(input$primary_care_setting),
          "<br>Facility type = ", htmltools::htmlEscape(format_categories(input$primary_facility_type)),
          "<br><br><b>Index event:</b><br>",
          "Diagnosis = ", htmltools::htmlEscape(format_selected_diagnoses(input$primary_index_diagnosis, primary_dx_choices)),
          "<br>Date range = ", format(input$primary_date_range[1], "%d/%m/%Y"),
          " to ", format(input$primary_date_range[2], "%d/%m/%Y"),
          "<br><br><b>Longitudinal history:</b><br>",
          "Lookback = ", input$primary_lookback_years, " year(s) within available data",
          "<br>Required prior diagnosis = ", htmltools::htmlEscape(format_selected_diagnoses(input$primary_required_prior_dx, primary_dx_choices)),
          "<br>Excluded prior diagnosis = ", htmltools::htmlEscape(format_selected_diagnoses(input$primary_excluded_prior_dx, primary_dx_choices)),
          "<br>Incident restriction = ", ifelse(input$primary_incident_only, "Yes", "No"),
          "<br>Prior encounter required = ", ifelse(input$primary_require_prior_encounter, "Yes", "No")
        ))
      )
    )
  })
  
  output$primary_metric_boxes <- renderUI({
    x <- primary_summary()
    if (primary_small_n()) return(disclosure_note())
    
    tagList(
      div(
        class = "metric-grid",
        div(
          class = "metric-card blue",
          div(class = "metric-label", "Cohort patients"),
          div(class = "metric-value", format_count(x$cohort_patients[1])),
          div(class = "metric-note", "Unique patients")
        ),
        div(
          class = "metric-card orange",
          div(class = "metric-label", "Analysis records"),
          div(class = "metric-value", format_count(x$records[1])),
          div(class = "metric-note", "Based on selected analysis mode")
        ),
        div(
          class = "metric-card green",
          div(class = "metric-label", "Mean age"),
          div(class = "metric-value", ifelse(is.na(x$mean_age[1]), "NA", x$mean_age[1])),
          div(class = "metric-note", "Years")
        ),
        div(
          class = "metric-card purple",
          div(class = "metric-label", "Female (%)"),
          div(class = "metric-value", ifelse(is.na(x$female_pct[1]), "NA", paste0(x$female_pct[1], "%"))),
          div(class = "metric-note", "Among analysis records")
        )
      )
    )
  })
  
  output$primary_age_sex_plot <- renderPlot({
    validate(
      need(!primary_small_n(), paste("At least", small_n_threshold, "patients are required to display cohort plots."))
    )
    primary_plot_age_sex()
  })
  
  output$primary_top_dx_plot <- renderPlot({
    validate(
      need(!primary_small_n(), paste("At least", small_n_threshold, "patients are required to display cohort plots."))
    )
    primary_plot_top_dx()
  })
  
  output$primary_utilisation_plot <- renderPlot({
    validate(
      need(!primary_small_n(), paste("At least", small_n_threshold, "patients are required to display cohort plots."))
    )
    primary_plot_utilisation()
  })
  
  output$primary_province_plot <- renderPlot({
    validate(
      need(!primary_small_n(), paste("At least", small_n_threshold, "patients are required to display cohort plots."))
    )
    primary_plot_province()
  })
  
  output$primary_flow_table <- renderTable({
    validate(need(!primary_small_n(), "Cohort fewer than five patients; suppressed."))
    x <- primary_flow()
    
    tibble(
      Step = c(
        "Qualifying index records",
        "Patients with a qualifying index record",
        "Patients after longitudinal-history criteria",
        "Analysis records",
        "Final cohort patients"
      ),
      Value = c(
        format_count(x$index_records[1]),
        format_count(x$index_patients[1]),
        format_count(x$final_index_patients[1]),
        format_count(x$cohort_records[1]),
        format_count(x$cohort_patients[1])
      )
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  # -------------------------------
  # Secondary care and hospital outcome analysis
  # -------------------------------
  secondary_sql_cte <- eventReactive(input$apply_secondary, {
    build_secondary_cohort_sql(input)
  }, ignoreNULL = TRUE)
  
  secondary_summary <- reactive({
    req(secondary_sql_cte())
    query_secondary_summary(secondary_sql_cte())
  })
  
  secondary_flow <- reactive({
    req(secondary_sql_cte())
    query_secondary_flow(secondary_sql_cte())
  })
  
  secondary_small_n <- reactive({
    x <- secondary_summary()
    is.na(x$cohort_patients[1]) || x$cohort_patients[1] < small_n_threshold
  })
  
  secondary_plot_age_sex <- reactive({
    req(secondary_sql_cte())
    dat <- DBI::dbGetQuery(db_pool, paste0(
      secondary_sql_cte(),
      "SELECT CASE ",
      "WHEN age BETWEEN 0 AND 4 THEN '0-4' ",
      "WHEN age BETWEEN 5 AND 17 THEN '5-17' ",
      "WHEN age BETWEEN 18 AND 44 THEN '18-44' ",
      "WHEN age BETWEEN 45 AND 64 THEN '45-64' ",
      "WHEN age BETWEEN 65 AND 74 THEN '65-74' ",
      "WHEN age BETWEEN 75 AND 84 THEN '75-84' ",
      "WHEN age BETWEEN 85 AND 110 THEN '85+' ",
      "ELSE 'Unknown' END AS age_group, ",
      "COALESCE(NULLIF(sex, ''), 'Unknown') AS sex, ",
      "COUNT(DISTINCT patient_id) AS n ",
      "FROM cohort_events GROUP BY age_group, sex"
    ))
    # Do not reveal categories contributed to by fewer than five patients.
    dat <- dat[!is.na(dat$n) & dat$n >= small_n_threshold, , drop = FALSE]
    validate(need(nrow(dat) > 0, "No age/sex groups meet the disclosure threshold."))
    dat$age_group <- factor(dat$age_group,
                            levels = c("0-4", "5-17", "18-44", "45-64", "65-74", "75-84", "85+", "Unknown"))
    ggplot(dat, aes(age_group, n, fill = sex)) +
      geom_col(position = "dodge") +
      labs(x = "Age at hospital encounter (years)", y = "Unique patients", fill = "Sex") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())
  })
  
  secondary_plot_los <- reactive({
    req(secondary_sql_cte())
    data <- DBI::dbGetQuery(
      db_pool,
      paste0(
        secondary_sql_cte(),
        "SELECT length_of_stay_days ",
        "FROM cohort_events ",
        "WHERE length_of_stay_days IS NOT NULL ",
        "AND length_of_stay_days >= 0 ",
        "AND length_of_stay_days <= 90"
      )
    )
    
    ggplot(data, aes(length_of_stay_days)) +
      geom_histogram(bins = 30) +
      labs(x = "Length of stay (days)", y = "Hospital events") +
      scale_y_continuous(labels = comma) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })
  
  secondary_plot_discharge <- reactive({
    req(secondary_sql_cte())
    data <- DBI::dbGetQuery(
      db_pool,
      paste0(
        secondary_sql_cte(),
        "SELECT COALESCE(discharge_status, 'Unknown') AS discharge_status, COUNT(*) AS n ",
        "FROM cohort_events ",
        "GROUP BY discharge_status HAVING COUNT(DISTINCT patient_id) >= 5 ORDER BY n DESC LIMIT 15"
      )
    )
    
    ggplot(data, aes(reorder(discharge_status, n), n)) +
      geom_col() +
      coord_flip() +
      labs(x = NULL, y = "Hospital events") +
      scale_y_continuous(labels = comma) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())
  })
  
  secondary_plot_comorbidity <- reactive({
    req(secondary_sql_cte())
    data <- DBI::dbGetQuery(
      db_pool,
      paste0(
        secondary_sql_cte(),
        "SELECT d.diagnosis_name, COUNT(*) AS n ",
        "FROM cohort_events e ",
        "INNER JOIN ", qi(secondary_diagnoses_view), " d ",
        "ON e.secondary_visit_id = d.secondary_visit_id ",
        "WHERE d.diagnosis_position = 'Secondary' ",
        "AND d.diagnosis_name IS NOT NULL ",
        "AND TRIM(d.diagnosis_name) <> '' ",
        "GROUP BY d.diagnosis_name HAVING COUNT(DISTINCT e.patient_id) >= 5 ORDER BY n DESC LIMIT 15"
      )
    )
    
    data <- data %>%
      mutate(
        diagnosis_label = names(secondary_dx_choices)[match(diagnosis_name, unname(secondary_dx_choices))],
        diagnosis_label = ifelse(is.na(diagnosis_label) | diagnosis_label == "",
                                 vapply(diagnosis_name, short_diagnosis_label, character(1)), diagnosis_label)
      )
    
    ggplot(data, aes(reorder(diagnosis_label, n), n)) +
      geom_col() +
      coord_flip() +
      labs(x = NULL, y = "Secondary-diagnosis records", caption = "Diagnosis labels abbreviated for display only") +
      scale_y_continuous(labels = comma) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())
  })
  
  secondary_plot_monthly <- reactive({
    req(secondary_sql_cte())
    data <- DBI::dbGetQuery(
      db_pool,
      paste0(
        secondary_sql_cte(),
        "SELECT DATE_TRUNC('month', admission_date)::date AS month, COUNT(*) AS n ",
        "FROM cohort_events GROUP BY month HAVING COUNT(DISTINCT patient_id) >= 5 ORDER BY month"
      )
    )
    
    ggplot(data, aes(month, n)) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.8) +
      labs(x = "Admission month", y = "Hospital events") +
      scale_y_continuous(labels = comma) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank())
  })
  
  secondary_plot_cost <- reactive({
    req(secondary_sql_cte())
    data <- DBI::dbGetQuery(
      db_pool,
      paste0(
        secondary_sql_cte(),
        "SELECT COALESCE(primary_diagnosis_name, 'Unknown') AS primary_diagnosis_name, ",
        "ROUND(AVG(bpjs_verified_cost)::numeric, 0) AS mean_cost, ",
        "COUNT(*) AS n ",
        "FROM cohort_events ",
        "WHERE bpjs_verified_cost IS NOT NULL ",
        "GROUP BY primary_diagnosis_name ",
        "HAVING COUNT(DISTINCT patient_id) >= 5 ",
        "ORDER BY mean_cost DESC LIMIT 15"
      )
    )
    
    data$diagnosis_short <- vapply(data$primary_diagnosis_name,
                                   short_diagnosis_label, character(1))
    data$diagnosis_short <- make.unique(data$diagnosis_short)
    ggplot(data, aes(reorder(diagnosis_short, mean_cost), mean_cost)) +
      geom_col() +
      coord_flip() +
      labs(x = NULL, y = "Mean verified BPJS cost (IDR)") +
      scale_y_continuous(labels = comma) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank())
  })
  
  output$secondary_selection_summary <- renderUI({
    tagList(
      div(
        class = "selection-summary",
        HTML(paste0(
          "<b>Demographics at index event:</b><br>",
          "Age = ", htmltools::htmlEscape(if (nzchar(trimws(input$secondary_custom_age_ranges)))
            input$secondary_custom_age_ranges else
              paste(input$secondary_age_slider[1], "to", input$secondary_age_slider[2])),
          "<br>Sex = ", htmltools::htmlEscape(input$secondary_sex),
          "<br><br><b>Index hospital event:</b><br>",
          "Admission range = ", format(input$secondary_date_range[1], "%d/%m/%Y"),
          " to ", format(input$secondary_date_range[2], "%d/%m/%Y"),
          "<br>Index diagnosis = ", htmltools::htmlEscape(format_selected_diagnoses(input$secondary_index_diagnosis, secondary_dx_choices)),
          "<br>Diagnosis position = ", htmltools::htmlEscape(input$secondary_diagnosis_position),
          "<br><br><b>Hospital characteristics:</b><br>",
          "Province = ", htmltools::htmlEscape(format_selected_provinces(input$secondary_province)),
          "<br>Facility type = ", htmltools::htmlEscape(format_categories(input$secondary_facility_type)),
          "<br>Hospital class = ", htmltools::htmlEscape(format_categories(input$secondary_facility_class)),
          "<br>Service level = ", htmltools::htmlEscape(format_categories(input$secondary_service_level)),
          "<br>Minimum length of stay = ",
          ifelse(input$secondary_min_los_enabled, paste0(input$secondary_min_los, " day(s)"), "Not applied"),
          "<br><br><b>Analysis mode:</b> ",
          ifelse(
            input$secondary_analysis_mode == "index_events",
            "Index hospital events only",
            "All available hospital events for cohort patients"
          )
        ))
      )
    )
  })
  
  output$secondary_metric_boxes <- renderUI({
    x <- secondary_summary()
    if (secondary_small_n()) return(disclosure_note())
    referral_rate <- safe_percent(
      x$events_with_primary_referral[1],
      x$hospital_events[1]
    )
    
    tagList(
      div(
        class = "metric-grid",
        div(
          class = "metric-card blue",
          div(class = "metric-label", "Cohort patients"),
          div(class = "metric-value", format_count(x$cohort_patients[1])),
          div(class = "metric-note", "Unique patients")
        ),
        div(
          class = "metric-card orange",
          div(class = "metric-label", "Hospital events"),
          div(class = "metric-value", format_count(x$hospital_events[1])),
          div(class = "metric-note", "Based on selected analysis mode")
        ),
        div(
          class = "metric-card green",
          div(class = "metric-label", "Median length of stay"),
          div(class = "metric-value", ifelse(is.na(x$median_los_days[1]), "NA", paste0(x$median_los_days[1], " d"))),
          div(class = "metric-note", "Among records with valid discharge dates")
        ),
        div(
          class = "metric-card purple",
          div(class = "metric-label", "Mean verified BPJS cost"),
          div(class = "metric-value", format_currency_idr(x$mean_verified_cost[1])),
          div(class = "metric-note", "Mean per hospital event")
        ),
        div(
          class = "metric-card blue",
          div(class = "metric-label", "Hospital records with FKTP referral ID"),
          div(class = "metric-value", ifelse(is.na(referral_rate), "NA", paste0(referral_rate, "%"))),
          div(class = "metric-note", "Recorded referral ID (not a verified FKTP linkage)")
        )
      )
    )
  })
  
  output$secondary_age_sex_plot <- renderPlot({
    validate(need(!secondary_small_n(),
                  paste("At least", small_n_threshold, "patients are required to display hospital cohort plots.")))
    secondary_plot_age_sex()
  })
  
  output$secondary_los_plot <- renderPlot({
    validate(
      need(!secondary_small_n(), paste("At least", small_n_threshold, "patients are required to display hospital outcome plots."))
    )
    secondary_plot_los()
  })
  
  output$secondary_discharge_plot <- renderPlot({
    validate(
      need(!secondary_small_n(), paste("At least", small_n_threshold, "patients are required to display hospital outcome plots."))
    )
    secondary_plot_discharge()
  })
  
  output$secondary_comorbidity_plot <- renderPlot({
    validate(
      need(!secondary_small_n(), paste("At least", small_n_threshold, "patients are required to display hospital outcome plots."))
    )
    secondary_plot_comorbidity()
  })
  
  output$secondary_monthly_plot <- renderPlot({
    validate(
      need(!secondary_small_n(), paste("At least", small_n_threshold, "patients are required to display hospital outcome plots."))
    )
    secondary_plot_monthly()
  })
  
  output$secondary_cost_plot <- renderPlot({
    validate(
      need(!secondary_small_n(), paste("At least", small_n_threshold, "patients are required to display hospital outcome plots."))
    )
    secondary_plot_cost()
  })
  
  output$secondary_flow_table <- renderTable({
    validate(need(!secondary_small_n(), "Cohort fewer than five patients; suppressed."))
    x <- secondary_flow()
    
    tibble(
      Step = c(
        "Qualifying hospital index events",
        "Patients with a qualifying hospital index event",
        "Final first-index hospital events",
        "Analysis hospital events",
        "Final cohort patients"
      ),
      Value = c(
        format_count(x$qualifying_index_events[1]),
        format_count(x$qualifying_index_patients[1]),
        format_count(x$final_index_patients[1]),
        format_count(x$cohort_events[1]),
        format_count(x$cohort_patients[1])
      )
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  
  # -------------------------------
  # Primary cohort visualisation export
  # -------------------------------
  output$download_primary_age_sex <- downloadHandler(
    filename = function() paste0("primary_age_sex_distribution_", Sys.Date(), ".png"),
    content = function(file) {
      req(!primary_small_n())
      download_plot_png(primary_plot_age_sex(), file)
    }
  )
  
  output$download_primary_top_dx <- downloadHandler(
    filename = function() paste0("primary_top_diagnoses_", Sys.Date(), ".png"),
    content = function(file) {
      req(!primary_small_n())
      download_plot_png(primary_plot_top_dx(), file)
    }
  )
  
  output$download_primary_utilisation <- downloadHandler(
    filename = function() paste0("primary_monthly_utilisation_", Sys.Date(), ".png"),
    content = function(file) {
      req(!primary_small_n())
      download_plot_png(primary_plot_utilisation(), file)
    }
  )
  
  output$download_primary_province <- downloadHandler(
    filename = function() paste0("primary_top_patient_provinces_", Sys.Date(), ".png"),
    content = function(file) {
      req(!primary_small_n())
      download_plot_png(primary_plot_province(), file)
    }
  )
  
  # -------------------------------
  # Secondary care visualisation export
  # -------------------------------
  output$download_secondary_age_sex <- downloadHandler(
    filename = function() paste0("secondary_age_sex_distribution_", Sys.Date(), ".png"),
    content = function(file) {
      req(!secondary_small_n())
      download_plot_png(secondary_plot_age_sex(), file)
    }
  )
  
  output$download_secondary_los <- downloadHandler(
    filename = function() paste0("secondary_length_of_stay_", Sys.Date(), ".png"),
    content = function(file) {
      req(!secondary_small_n())
      download_plot_png(secondary_plot_los(), file)
    }
  )
  
  output$download_secondary_discharge <- downloadHandler(
    filename = function() paste0("secondary_discharge_status_", Sys.Date(), ".png"),
    content = function(file) {
      req(!secondary_small_n())
      download_plot_png(secondary_plot_discharge(), file)
    }
  )
  
  output$download_secondary_comorbidity <- downloadHandler(
    filename = function() paste0("secondary_comorbidities_", Sys.Date(), ".png"),
    content = function(file) {
      req(!secondary_small_n())
      download_plot_png(secondary_plot_comorbidity(), file)
    }
  )
  
  output$download_secondary_monthly <- downloadHandler(
    filename = function() paste0("secondary_monthly_admissions_", Sys.Date(), ".png"),
    content = function(file) {
      req(!secondary_small_n())
      download_plot_png(secondary_plot_monthly(), file)
    }
  )
  
  output$download_secondary_cost <- downloadHandler(
    filename = function() paste0("secondary_verified_cost_", Sys.Date(), ".png"),
    content = function(file) {
      req(!secondary_small_n())
      download_plot_png(secondary_plot_cost(), file)
    }
  )
  
  # -------------------------------
  # Aggregate summary export
  # -------------------------------
  output$download_primary_summary <- downloadHandler(
    filename = function() paste0("primary_cohort_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      if (primary_small_n()) {
        stop(paste("Export blocked: fewer than", small_n_threshold, "unique patients in the cohort."))
      }
      
      x <- primary_summary()
      f <- primary_flow()
      
      summary_data <- tibble(
        section = c(
          rep("Cohort summary", 5),
          rep("Cohort flow", 5)
        ),
        metric = c(
          "Cohort patients",
          "Analysis records",
          "Mean age",
          "Female percentage",
          "Male percentage",
          "Qualifying index records",
          "Qualifying index patients",
          "Patients after longitudinal-history criteria",
          "Analysis records",
          "Final cohort patients"
        ),
        value = c(
          x$cohort_patients[1],
          x$records[1],
          x$mean_age[1],
          x$female_pct[1],
          x$male_pct[1],
          f$index_records[1],
          f$index_patients[1],
          f$final_index_patients[1],
          f$cohort_records[1],
          f$cohort_patients[1]
        )
      )
      
      write.csv(summary_data, file, row.names = FALSE)
    }
  )
  
  output$download_secondary_summary <- downloadHandler(
    filename = function() paste0("secondary_care_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      if (secondary_small_n()) {
        stop(paste("Export blocked: fewer than", small_n_threshold, "unique patients in the cohort."))
      }
      
      x <- secondary_summary()
      f <- secondary_flow()
      
      summary_data <- tibble(
        section = c(rep("Hospital outcomes", 6), rep("Cohort flow", 5)),
        metric = c(
          "Cohort patients",
          "Hospital events",
          "Mean length of stay (days)",
          "Median length of stay (days)",
          "Mean verified BPJS cost (IDR)",
          "Hospital events with recorded FKTP referral ID",
          "Qualifying hospital index events",
          "Qualifying index patients",
          "Final first-index hospital events",
          "Analysis hospital events",
          "Final cohort patients"
        ),
        value = c(
          x$cohort_patients[1],
          x$hospital_events[1],
          x$mean_los_days[1],
          x$median_los_days[1],
          x$mean_verified_cost[1],
          x$events_with_primary_referral[1],
          f$qualifying_index_events[1],
          f$qualifying_index_patients[1],
          f$final_index_patients[1],
          f$cohort_events[1],
          f$cohort_patients[1]
        )
      )
      
      write.csv(summary_data, file, row.names = FALSE)
    }
  )
  
  output$download_secondary_dx_summary <- downloadHandler(
    filename = function() paste0("secondary_care_diagnosis_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      if (secondary_small_n()) {
        stop(paste("Export blocked: fewer than", small_n_threshold, "unique patients in the cohort."))
      }
      
      out <- DBI::dbGetQuery(
        db_pool,
        paste0(
          secondary_sql_cte(),
          "SELECT d.diagnosis_position, d.diagnosis_code, d.diagnosis_name, COUNT(*) AS records ",
          "FROM cohort_events e ",
          "INNER JOIN ", qi(secondary_diagnoses_view), " d ",
          "ON e.secondary_visit_id = d.secondary_visit_id ",
          "WHERE d.diagnosis_name IS NOT NULL ",
          "GROUP BY d.diagnosis_position, d.diagnosis_code, d.diagnosis_name ",
          "HAVING COUNT(DISTINCT e.patient_id) >= 5 ",
          "ORDER BY d.diagnosis_position, records DESC"
        )
      )
      
      write.csv(out, file, row.names = FALSE)
    }
  )
  
  # -------------------------------
  # Dataset Overview
  # -------------------------------
  # Counts are not calculated at startup. This avoids multiple expensive
  # COUNT(DISTINCT ...) scans when opening the application.
  overview_patient_counts <- eventReactive(input$calculate_overview_patients, {
    withProgress(message = "Counting distinct patients by source layer", value = 0, {
      tables <- c("primary_care_capitated", "primary_care_noncapitated",
                  "secondary_care", "participant_backbone")
      counts <- numeric(length(tables))
      for (i in seq_along(tables)) {
        counts[i] <- as.numeric(db_scalar(paste0(
          "SELECT COUNT(DISTINCT patient_id) FROM ", source_ref(tables[i]))))
        incProgress(1 / 5, detail = paste("Checked", tables[i]))
      }
      # Only distinct patient IDs; do not sum distincts across tables.
      counts <- c(counts, as.numeric(db_scalar(paste0(
        "SELECT COUNT(DISTINCT patient_id) FROM ",
        quote_id("bpjs_app"), ".", quote_id("all_encounters")))))
      incProgress(1 / 5, detail = "Combined encounter view")
      counts
    })
  }, ignoreInit = TRUE)
  
  output$overview_count_note <- renderText({
    req(input$calculate_overview_patients > 0)
    "Exact distinct patient counts have been calculated for this application session."
  })
  
  output$dataset_overview_table <- renderTable({
    counts <- if (input$calculate_overview_patients > 0) overview_patient_counts() else NULL
    patient_values <- if (is.null(counts)) {
      c(rep("Click Calculate unique patients", 3),
        "Not applicable (diagnosis rows)",
        format_count(2590755), "Click Calculate unique patients")
    } else {
      c(format_count(counts[1]), format_count(counts[2]),
        format_count(counts[3]), "Not applicable (diagnosis rows)",
        format_count(counts[4]), format_count(counts[5]))
    }
    tibble(
      `Data layer` = c("Capitated primary care", "Non-capitated primary care",
                       "Hospital encounters", "Hospital secondary diagnoses",
                       "Membership records", "Unified encounter view"),
      Description = c("2020–2024 source extracts", "2020–2024 source extracts",
                      "FKRTL hospital claims", "Linked hospital secondary diagnoses",
                      "Longitudinal membership rows; not distinct people",
                      "Virtual union across three encounter sources"),
      Records = c(format_count(18515654), format_count(862840),
                  format_count(6998575), format_count(7449544),
                  format_count(12005697), format_count(26377069)),
      `Unique patients` = patient_values
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  overview_coverage <- eventReactive(input$load_overview_coverage, {
    withProgress(message = "Calculating recorded encounter coverage", value = 0, {
      # Aggregate annual encounter counts from the unified database view
      # and reuse the results across coverage plots.
      out <- DBI::dbGetQuery(db_pool, paste0(
        "SELECT EXTRACT(YEAR FROM visit_date)::int AS encounter_year, ",
        "care_group, record_source, COUNT(*)::bigint AS encounters ",
        "FROM ", quote_id("bpjs_app"), ".", quote_id("all_encounters"), " ",
        "WHERE visit_date IS NOT NULL ",
        "GROUP BY EXTRACT(YEAR FROM visit_date)::int, care_group, record_source ",
        "ORDER BY encounter_year, care_group, record_source"
      ))
      incProgress(1, detail = "Annual primary and secondary care totals ready")
      out$encounters <- as.numeric(out$encounters)
      out
    })
  }, ignoreInit = TRUE)
  
  output$primary_date_coverage_plot <- renderPlot({
    x <- overview_coverage()
    x <- x[x$care_group == "Primary care", , drop = FALSE]
    validate(need(nrow(x) > 0, "No primary care records available."))
    x$source_label <- ifelse(x$record_source == "primary_care_capitated", "Capitated",
                             ifelse(x$record_source == "primary_care_noncapitated", "Non-capitated", x$record_source))
    ggplot(x, aes(encounter_year, encounters, colour = source_label, group = source_label)) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      scale_x_continuous(breaks = sort(unique(x$encounter_year))) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Encounter year", y = "Encounter records", colour = "Primary care") +
      theme_minimal(base_size = 12)
  })
  
  output$secondary_date_coverage_plot <- renderPlot({
    x <- overview_coverage()
    x <- x[x$care_group == "Secondary care", , drop = FALSE]
    x <- aggregate(encounters ~ encounter_year, data = x, FUN = sum)
    validate(need(nrow(x) > 0, "No secondary care records available."))
    ggplot(x, aes(encounter_year, encounters)) +
      geom_col(fill = "#2c6cb0") +
      scale_x_continuous(breaks = sort(unique(x$encounter_year))) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Encounter year", y = "Hospital encounter records") +
      theme_minimal(base_size = 12)
  })
  
  output$combined_date_coverage_plot <- renderPlot({
    x <- overview_coverage()
    x <- stats::aggregate(encounters ~ encounter_year + care_group, data = x, FUN = sum)
    validate(need(nrow(x) > 0, "No encounter records available."))
    ggplot(x, aes(encounter_year, encounters, fill = care_group)) +
      geom_col(position = "dodge") +
      scale_x_continuous(breaks = sort(unique(x$encounter_year))) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Encounter year", y = "Encounter records", fill = "Care setting") +
      theme_minimal(base_size = 12)
  })
  
  # -------------------------------------------------------------------
  # Longitudinal analysis
  # Use the first qualifying index event per patient to define cohort entry.
  # Link subsequent encounters and aggregate results in PostgreSQL.
  # -------------------------------------------------------------------
  longitudinal_definition <- eventReactive(input$apply_longitudinal, {
    src <- isolate(input$long_index_source)
    if (identical(src, "primary")) {
      cte <- primary_sql_cte()
      idx <- "final_index_events"
    } else {
      cte <- secondary_sql_cte()
      idx <- "patient_index"
    }
    req(cte)
    yrs <- as.integer(input$long_followup_years)
    if (is.na(yrs) || yrs < 1L || yrs > 5L) stop("Invalid follow-up years")
    include_index <- isTRUE(input$long_include_index_day)
    complete_only <- isTRUE(input$long_complete_only)
    list(source = src, cte = cte, index_table = idx,
         years = yrs, include_index = include_index,
         complete_only = complete_only)
  }, ignoreInit = TRUE)
  
  longitudinal_data <- reactive({
    d <- longitudinal_definition()
    req(d)
    # Use index dates from the applied cohort definition.
    # Retrieve subsequent encounters from the unified encounter view.
    idx_sql <- paste0(
      "SELECT DISTINCT patient_id, index_date FROM ", d$index_table,
      if (d$complete_only) paste0(
        " WHERE index_date + INTERVAL '", d$years,
        " years' <= DATE '2025-01-01'") else ""
    )
    follow_condition <- if (d$include_index) ">=" else ">"
    sql <- paste0(
      d$cte,
      ", study_index AS (", idx_sql, "), ",
      "followup AS (",
      "SELECT e.patient_id, e.visit_date, e.care_group, i.index_date, ",
      "EXTRACT(YEAR FROM e.visit_date)::int AS calendar_year, ",
      "EXTRACT(YEAR FROM AGE(e.visit_date, i.index_date))::int AS year_since_index ",
      "FROM ", quote_id("bpjs_app"), ".", quote_id("all_encounters"), " e ",
      "JOIN study_index i ON e.patient_id = i.patient_id ",
      "WHERE e.visit_date ", follow_condition, " i.index_date ",
      "AND e.visit_date < i.index_date + INTERVAL '", d$years, " years'",
      ") ",
      "SELECT calendar_year, year_since_index, care_group, ",
      "COUNT(*)::bigint AS encounters, COUNT(DISTINCT patient_id)::bigint AS patients ",
      "FROM followup GROUP BY calendar_year, year_since_index, care_group ",
      "ORDER BY calendar_year, year_since_index, care_group")
    ans <- DBI::dbGetQuery(db_pool, sql)
    # Suppress aggregate cells contributed to by fewer than five patients.
    ans$encounters[ans$patients < small_n_threshold] <- NA_real_
    ans$patients[ans$patients < small_n_threshold] <- NA_real_
    ans
  })
  
  longitudinal_cohort_count <- reactive({
    d <- longitudinal_definition()
    req(d)
    sql <- paste0(d$cte, "SELECT COUNT(DISTINCT patient_id) AS patients FROM ", d$index_table,
                  if (d$complete_only) paste0(
                    " WHERE index_date + INTERVAL '", d$years,
                    " years' <= DATE '2025-01-01'") else "")
    as.numeric(DBI::dbGetQuery(db_pool, sql)$patients[1])
  })
  longitudinal_small_n <- reactive({
    n <- longitudinal_cohort_count()
    is.na(n) || n < small_n_threshold
  })
  output$long_selection_summary <- renderUI({
    d <- longitudinal_definition()
    req(d)
    div(class = "selection-summary", paste(
      "Index source:", if (d$source == "primary") "Primary / Combined" else "Secondary care",
      "| Follow-up:", d$years, "year(s)",
      "| Include index day:", if (d$include_index) "Yes" else "No",
      "| Complete calendar follow-up only:", if (d$complete_only) "Yes" else "No"))
  })
  output$long_metric_boxes <- renderUI({
    req(longitudinal_definition())
    n <- longitudinal_cohort_count()
    div(class = "metric-grid",
        div(class = "metric-card blue", div(class = "metric-label", "Index cohort patients"),
            div(class = "metric-value", if (n < small_n_threshold) "Suppressed (<5)" else format_count(n))),
        div(class = "metric-card green", div(class = "metric-label", "Observation"),
            div(class = "metric-value", "Recorded claims"),
            div(class = "metric-note", "Follow-up availability varies by entry date")))
  })
  longitudinal_care_data <- reactive({
    x <- longitudinal_data()
    req(!longitudinal_small_n())
    x %>% group_by(calendar_year, care_group) %>%
      summarise(encounters = if (any(is.na(encounters))) NA_real_ else sum(as.numeric(encounters)), .groups = "drop")
  })
  plot_long_care <- reactive({
    x <- longitudinal_care_data()
    ggplot(x, aes(calendar_year, encounters, colour = care_group)) +
      geom_line(linewidth = 1) + geom_point(size = 2) +
      scale_x_continuous(breaks = sort(unique(x$calendar_year))) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Encounter year", y = "Recorded encounters", colour = "Care setting") +
      theme_minimal(base_size = 13)
  })
  output$long_care_plot <- renderPlot({
    req(longitudinal_definition())
    validate(need(!longitudinal_small_n(), "Cohort fewer than five patients; suppressed."))
    plot_long_care()
  })
  plot_long_since <- reactive({
    x <- longitudinal_data() %>% group_by(year_since_index, care_group) %>%
      summarise(encounters = if (any(is.na(encounters))) NA_real_ else sum(as.numeric(encounters)), .groups = "drop")
    ggplot(x, aes(year_since_index, encounters, fill = care_group)) +
      geom_col(position = "dodge") + scale_y_continuous(labels = scales::comma) +
      labs(x = "Completed years since index", y = "Recorded encounters", fill = "Care setting") +
      theme_minimal(base_size = 13)
  })
  output$long_since_plot <- renderPlot({
    req(longitudinal_definition())
    validate(need(!longitudinal_small_n(), "Cohort fewer than five patients; suppressed."))
    plot_long_since()
  })
  output$long_summary_table <- renderTable({
    req(longitudinal_definition())
    validate(need(!longitudinal_small_n(), "Cohort fewer than five patients; suppressed."))
    x <- longitudinal_data()
    validate(need(nrow(x) > 0L,
                  "No follow-up encounters were found for the selected cohort and follow-up window."))
    
    # Display suppressed counts explicitly to distinguish them from zero.
    format_long_count <- function(values) {
      vapply(values, function(value) {
        if (is.na(value)) return("Suppressed (<5 patients)")
        format(round(as.numeric(value)), big.mark = ",", scientific = FALSE,
               trim = TRUE, nsmall = 0)
      }, character(1))
    }
    
    data.frame(
      "Calendar year" = as.integer(x$calendar_year),
      "Completed years since cohort entry" = as.integer(x$year_since_index),
      "Healthcare setting" = as.character(x$care_group),
      "Number of encounters" = format_long_count(x$encounters),
      "Unique patients" = format_long_count(x$patients),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, rownames = FALSE, sanitize.text.function = identity)
  # Calculate calendar-based follow-up eligibility on request.
  # Eligibility does not establish continuous insurance enrolment.
  # Reset eligibility results when a new analysis is applied.
  long_eligibility_result <- reactiveVal(NULL)
  observeEvent(input$apply_longitudinal, {
    long_eligibility_result(NULL)
  }, ignoreInit = TRUE)
  
  observeEvent(input$calculate_long_eligibility, {
    d <- longitudinal_definition()
    req(d)
    cutoff <- "DATE '2025-01-01'"  # exclusive end of extract
    components <- character(0)
    for (k in 0:(d$years - 1L)) {
      components <- c(components,
                      paste0("COUNT(*) FILTER (WHERE index_date + INTERVAL '", k,
                             " years' < ", cutoff, ")::bigint AS reached_", k),
                      paste0("COUNT(*) FILTER (WHERE index_date + INTERVAL '", k + 1L,
                             " years' <= ", cutoff, ")::bigint AS completed_", k))
    }
    index_sql <- paste0("SELECT DISTINCT patient_id, index_date FROM ", d$index_table,
                        if (d$complete_only) paste0(
                          " WHERE index_date + INTERVAL '", d$years,
                          " years' <= ", cutoff) else "")
    sql <- paste0(d$cte, ", eligibility_index AS (", index_sql, ") ",
                  "SELECT ", paste(components, collapse = ", "), " FROM eligibility_index")
    tryCatch({
      ans <- DBI::dbGetQuery(db_pool, sql)
      result <- data.frame(
        "Follow-up year" = 0:(d$years - 1L),
        "Reached start of year" = vapply(0:(d$years - 1L),
                                         function(k) as.numeric(ans[[paste0("reached_", k)]][1]), numeric(1)),
        "Completed full year" = vapply(0:(d$years - 1L),
                                       function(k) as.numeric(ans[[paste0("completed_", k)]][1]), numeric(1)),
        check.names = FALSE
      )
      long_eligibility_result(result)
    }, error = function(e) {
      message("Follow-up eligibility query failed: ", conditionMessage(e))
      long_eligibility_result(NULL)
    })
  })
  output$long_eligibility_table <- renderTable({
    x <- long_eligibility_result()
    req(x)
    protect <- function(v) ifelse(v > 0 & v < small_n_threshold,
                                  "Suppressed (<5)", format(v, big.mark = ",", scientific = FALSE, trim = TRUE))
    x[["Reached start of year"]] <- protect(x[["Reached start of year"]])
    x[["Completed full year"]] <- protect(x[["Completed full year"]])
    x
  }, striped = TRUE, bordered = TRUE, rownames = FALSE, sanitize.text.function = identity)
  
  output$download_long_csv <- downloadHandler(
    filename = function() paste0("longitudinal_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      req(!longitudinal_small_n())
      write.csv(longitudinal_data(), file, row.names = FALSE)
    }
  )
  output$download_long_plot <- downloadHandler(
    filename = function() paste0("longitudinal_care_", Sys.Date(), ".png"),
    content = function(file) {
      req(!longitudinal_small_n())
      download_plot_png(plot_long_care(), file)
    }
  )
  output$download_long_since_plot <- downloadHandler(
    filename = function() paste0("longitudinal_years_since_index_", Sys.Date(), ".png"),
    content = function(file) {
      req(input$apply_longitudinal > 0, !longitudinal_small_n())
      download_plot_png(plot_long_since(), file)
    }
  )
  
  
  # Export eligible graphs from applied analyses using the same
  # plot functions as individual PNG downloads.
  output$download_all_graphs_zip <- downloadHandler(
    filename = function() paste0("bpjs_cohort_graphs_", Sys.Date(), ".zip"),
    contentType = "application/zip",
    content = function(file) {
      folder <- tempfile(pattern = "bpjs_graphs_")
      dir.create(folder, recursive = TRUE)
      on.exit(unlink(folder, recursive = TRUE), add = TRUE)
      saved <- character(0)
      
      add_plot <- function(plot, name, width = 11, height = 7) {
        dest <- file.path(folder, name)
        download_plot_png(plot, dest, width = width, height = height)
        saved <<- c(saved, name)
      }
      
      # Dataset overview charts are included only if the user has already
      # requested the overview aggregation; do not trigger a new scan here.
      if (isTRUE(isolate(input$load_overview_coverage > 0))) {
        ov <- isolate(overview_coverage())
        pc <- ov[ov$care_group == "Primary care", , drop = FALSE]
        if (nrow(pc)) {
          pc$source_label <- ifelse(pc$record_source == "primary_care_capitated", "Capitated",
                                    ifelse(pc$record_source == "primary_care_noncapitated", "Non-capitated", pc$record_source))
          add_plot(ggplot(pc, aes(encounter_year, encounters, colour = source_label, group = source_label)) +
                     geom_line(linewidth = 1) + geom_point(size = 2) +
                     scale_x_continuous(breaks = sort(unique(pc$encounter_year))) +
                     scale_y_continuous(labels = scales::comma) +
                     labs(x = "Encounter year", y = "Encounter records", colour = "Primary care") +
                     theme_minimal(base_size = 12), "overview_primary_coverage.png")
        }
        sc <- ov[ov$care_group == "Secondary care", , drop = FALSE]
        if (nrow(sc)) {
          sc <- stats::aggregate(encounters ~ encounter_year, data = sc, FUN = sum)
          add_plot(ggplot(sc, aes(encounter_year, encounters)) +
                     geom_col(fill = "#2c6cb0") +
                     scale_x_continuous(breaks = sort(unique(sc$encounter_year))) +
                     scale_y_continuous(labels = scales::comma) +
                     labs(x = "Encounter year", y = "Hospital encounter records") +
                     theme_minimal(base_size = 12), "overview_secondary_coverage.png")
        }
        combined <- stats::aggregate(encounters ~ encounter_year + care_group, data = ov, FUN = sum)
        if (nrow(combined)) {
          add_plot(ggplot(combined, aes(encounter_year, encounters, fill = care_group)) +
                     geom_col(position = "dodge") +
                     scale_x_continuous(breaks = sort(unique(combined$encounter_year))) +
                     scale_y_continuous(labels = scales::comma) +
                     labs(x = "Encounter year", y = "Encounter records", fill = "Care setting") +
                     theme_minimal(base_size = 12), "overview_combined_coverage.png")
        }
      }
      
      # Do not query a cohort the user has not yet explicitly applied.
      if (isTRUE(isolate(input$apply_primary > 0)) && !isTRUE(isolate(primary_small_n()))) {
        add_plot(isolate(primary_plot_age_sex()), "primary_age_sex.png")
        add_plot(isolate(primary_plot_top_dx()), "primary_top_diagnoses.png")
        add_plot(isolate(primary_plot_utilisation()), "primary_monthly_utilisation.png")
        add_plot(isolate(primary_plot_province()), "primary_provinces.png")
      }
      if (isTRUE(isolate(input$apply_secondary > 0)) && !isTRUE(isolate(secondary_small_n()))) {
        add_plot(isolate(secondary_plot_age_sex()), "secondary_age_sex.png")
        add_plot(isolate(secondary_plot_los()), "secondary_length_of_stay.png")
        add_plot(isolate(secondary_plot_discharge()), "secondary_discharge_status.png")
        add_plot(isolate(secondary_plot_comorbidity()), "secondary_diagnoses.png")
        add_plot(isolate(secondary_plot_monthly()), "secondary_monthly_encounters.png")
        add_plot(isolate(secondary_plot_cost()), "secondary_verified_cost.png")
      }
      if (isTRUE(isolate(input$apply_longitudinal > 0)) &&
          !isTRUE(isolate(longitudinal_small_n()))) {
        add_plot(isolate(plot_long_care()), "longitudinal_by_calendar_year.png")
        add_plot(isolate(plot_long_since()), "longitudinal_years_since_index.png")
      }
      
      if (!length(saved)) {
        stop("No eligible graphs to export. Apply a cohort with at least five patients first.")
      }
      zip::zipr(zipfile = file, files = saved, root = folder)
    }
  )
  
  
}

shinyApp(ui, server)
