# ============================================================
# BPJS 2020-2024: Supplementary Harmonisation Audits
#
# Complete harmonisation audits using previously generated
# longitudinal datasets.
#
# Audit procedures:
#   - Cross-year visit identifier duplication
#   - Participant linkage
#   - Secondary diagnosis linkage
#   - Province coding systems
#   - Final dataset summaries
#
# Input directory: harmonised_2020_2024/
# Audit directory: harmonisation_audit_2020_2024/
#
# Memory management:
#   - Process large datasets sequentially.
#   - Retain reduced identifier tables for linkage.
#   - Release objects after processing.
#   - Run garbage collection between major operations.
# ============================================================


# =====================================================================
# 1. LOAD REQUIRED PACKAGES
# =====================================================================

required_packages <- c(
  "dplyr",
  "tibble",
  "readr"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(dplyr)
library(tibble)
library(readr)


# =====================================================================
# 2. CONFIGURE DIRECTORIES AND FILE PATHS
# =====================================================================

data_folder <- "harmonised_2020_2024"

audit_folder <- "harmonisation_audit_2020_2024"

if (!dir.exists(audit_folder)) {
  dir.create(
    audit_folder,
    recursive = TRUE
  )
}


participant_path <- file.path(
  data_folder,
  "participant_backbone_2020_2024.rds"
)

capitated_path <- file.path(
  data_folder,
  "primary_care_capitated_2020_2024.rds"
)

noncapitated_path <- file.path(
  data_folder,
  "primary_care_noncapitated_2020_2024.rds"
)

secondary_path <- file.path(
  data_folder,
  "secondary_care_2020_2024.rds"
)

secondary_dx_path <- file.path(
  data_folder,
  "secondary_diagnosis_2020_2024.rds"
)


# =====================================================================
# 3. VALIDATE INPUT FILES
# =====================================================================

required_files <- tibble(
  
  dataset = c(
    "participant_backbone",
    "primary_care_capitated",
    "primary_care_noncapitated",
    "secondary_care",
    "secondary_diagnosis"
  ),
  
  file_path = c(
    participant_path,
    capitated_path,
    noncapitated_path,
    secondary_path,
    secondary_dx_path
  )
  
) %>%
  
  mutate(
    exists =
      file.exists(file_path),
    
    file_size_mb =
      ifelse(
        exists,
        round(
          file.info(file_path)$size /
            1024^2,
          2
        ),
        NA_real_
      )
  )


print(
  required_files
)


if (any(!required_files$exists)) {
  
  stop(
    paste0(
      "\nOne or more harmonised RDS files are missing.\n\n",
      paste(
        required_files$file_path[
          !required_files$exists
        ],
        collapse = "\n"
      )
    )
  )
}


cat("\n")
cat("============================================================\n")
cat("ALL HARMONISED RDS FILES FOUND\n")
cat("============================================================\n\n")


# =====================================================================
# 4. CROSS-YEAR VISIT AUDIT FUNCTION
# =====================================================================
# Identify visit IDs appearing in multiple source years
# within each encounter dataset.
make_visit_duplicate_audit <- function(
    file_path,
    dataset_name
) {
  
  cat(
    "\nLoading ",
    dataset_name,
    "...\n",
    sep = ""
  )
  
  
  df <- readRDS(
    file_path
  )
  
  
  total_rows <- nrow(df)
  
  
  unique_visit_ids <- n_distinct(
    df$visit_id,
    na.rm = TRUE
  )
  
  
  cross_year <- df %>%
    
    filter(
      !is.na(
        visit_id
      )
    ) %>%
    
    distinct(
      visit_id,
      source_year
    ) %>%
    
    count(
      visit_id,
      name = "number_source_years"
    )
  
  
  result <- tibble(
    
    dataset =
      dataset_name,
    
    total_rows =
      total_rows,
    
    unique_visit_ids =
      unique_visit_ids,
    
    visit_ids_in_multiple_source_years =
      sum(
        cross_year$
          number_source_years > 1
      )
    
  )
  
  
  rm(
    df,
    cross_year
  )
  
  gc()
  
  
  result
}


# =====================================================================
# 5. ASSESS CROSS-YEAR VISIT IDENTIFIERS
# =====================================================================

cat("\n")
cat("============================================================\n")
cat("CROSS-YEAR VISIT-ID AUDIT\n")
cat("============================================================\n")


cap_visit_audit <-
  make_visit_duplicate_audit(
    capitated_path,
    "primary_care_capitated"
  )


noncap_visit_audit <-
  make_visit_duplicate_audit(
    noncapitated_path,
    "primary_care_noncapitated"
  )


secondary_visit_audit <-
  make_visit_duplicate_audit(
    secondary_path,
    "secondary_care"
  )


visit_id_duplicate_audit <- bind_rows(
  cap_visit_audit,
  noncap_visit_audit,
  secondary_visit_audit
)


write_csv(
  
  visit_id_duplicate_audit,
  
  file.path(
    audit_folder,
    "2020_2024_visit_id_duplicate_audit.csv"
  )
  
)


print(
  visit_id_duplicate_audit
)


rm(
  cap_visit_audit,
  noncap_visit_audit,
  secondary_visit_audit
)

gc()


# =====================================================================
# 6. PARTICIPANT LINKAGE AUDIT
# =====================================================================

cat("\n")
cat("============================================================\n")
cat("PARTICIPANT LINKAGE AUDIT\n")
cat("============================================================\n")


cat("\nLoading participant backbone IDs...\n")


participant <- readRDS(
  participant_path
)

# Extract distinct non-missing participant IDs across
# all source years for linkage assessment.
all_participant_ids <- participant %>%
  
  filter(
    !is.na(
      patient_id
    )
  ) %>%
  
  distinct(
    patient_id
  )


participant_total_rows <-
  nrow(participant)

participant_unique_patients <-
  n_distinct(
    participant$patient_id,
    na.rm = TRUE
  )


rm(
  participant
)

gc()


# ---------------------------------------------------------------------
# Assess encounter linkage against the longitudinal
# participant identifier table.
# ---------------------------------------------------------------------

make_participant_linkage <- function(
    file_path,
    dataset_name,
    participant_ids
) {
  
  cat(
    "\nChecking ",
    dataset_name,
    " linkage...\n",
    sep = ""
  )
  
  
  df <- readRDS(
    file_path
  )
  
  
  linked_n <- sum(
    df$patient_id %in%
      participant_ids$patient_id
  )
  
  
  total_n <- nrow(df)
  
  
  result <- tibble(
    
    dataset =
      dataset_name,
    
    records =
      total_n,
    
    linked =
      linked_n,
    
    not_linked =
      total_n -
      linked_n,
    
    linkage_percent =
      round(
        linked_n /
          total_n *
          100,
        4
      )
    
  )
  
  
  rm(
    df
  )
  
  gc()
  
  
  result
}


cap_linkage <-
  make_participant_linkage(
    capitated_path,
    "primary_care_capitated",
    all_participant_ids
  )


noncap_linkage <-
  make_participant_linkage(
    noncapitated_path,
    "primary_care_noncapitated",
    all_participant_ids
  )


secondary_linkage <-
  make_participant_linkage(
    secondary_path,
    "secondary_care",
    all_participant_ids
  )


participant_linkage_audit <- bind_rows(
  cap_linkage,
  noncap_linkage,
  secondary_linkage
)


write_csv(
  
  participant_linkage_audit,
  
  file.path(
    audit_folder,
    "2020_2024_participant_linkage_audit.csv"
  )
  
)


print(
  participant_linkage_audit
)


rm(
  cap_linkage,
  noncap_linkage,
  secondary_linkage,
  all_participant_ids
)

gc()


# =====================================================================
# 7. SECONDARY DIAGNOSIS LINKAGE AUDIT
# =====================================================================

cat("\n")
cat("============================================================\n")
cat("SECONDARY DIAGNOSIS LINKAGE AUDIT\n")
cat("============================================================\n")


cat("\nLoading secondary-care visit IDs...\n")


secondary <- readRDS(
  secondary_path
)

# Extract distinct non-missing secondary care visit IDs
# for secondary diagnosis linkage.
all_secondary_visit_ids <- secondary %>%
  
  filter(
    !is.na(
      visit_id
    )
  ) %>%
  
  distinct(
    visit_id
  )


secondary_total_rows <-
  nrow(secondary)

secondary_unique_patients <-
  n_distinct(
    secondary$patient_id,
    na.rm = TRUE
  )

secondary_unique_visits <-
  n_distinct(
    secondary$visit_id,
    na.rm = TRUE
  )


rm(
  secondary
)

gc()


cat(
  "\nLoading secondary diagnosis table...\n"
)


secondary_dx <- readRDS(
  secondary_dx_path
)


secondary_dx_total_rows <-
  nrow(
    secondary_dx
  )


secondary_dx_unique_visits <-
  n_distinct(
    secondary_dx$visit_id,
    na.rm = TRUE
  )


linked_secondary_dx <- sum(
  
  secondary_dx$visit_id %in%
    all_secondary_visit_ids$
    visit_id
  
)


secondary_diagnosis_linkage_audit <- tibble(
  
  records =
    secondary_dx_total_rows,
  
  unique_visits =
    secondary_dx_unique_visits,
  
  linked_records =
    linked_secondary_dx,
  
  unlinked_records =
    secondary_dx_total_rows -
    linked_secondary_dx,
  
  linkage_percent =
    round(
      linked_secondary_dx /
        secondary_dx_total_rows *
        100,
      4
    )
  
)


write_csv(
  
  secondary_diagnosis_linkage_audit,
  
  file.path(
    audit_folder,
    "2020_2024_secondary_diagnosis_linkage_audit.csv"
  )
  
)


print(
  secondary_diagnosis_linkage_audit
)


rm(
  secondary_dx,
  all_secondary_visit_ids
)

gc()


# =====================================================================
# 8. PROVINCE SYSTEM AUDIT
# =====================================================================

cat("\n")
cat("============================================================\n")
cat("PROVINCE SYSTEM AUDIT\n")
cat("============================================================\n")


# ---------------------------------------------------------------------
# Participant residence
# ---------------------------------------------------------------------

cat(
  "\nParticipant province audit...\n"
)


participant <- readRDS(
  participant_path
)


participant_province_audit <- participant %>%
  
  count(
    source_year,
    residence_province_code_system,
    name = "records"
  ) %>%
  
  transmute(
    
    dataset =
      "participant_residence",
    
    source_year,
    
    province_code_system =
      residence_province_code_system,
    
    records
    
  )


rm(
  participant
)

gc()


# ---------------------------------------------------------------------
# Capitated primary care facility
# ---------------------------------------------------------------------

cat(
  "\nCapitated province audit...\n"
)


capitated <- readRDS(
  capitated_path
)


capitated_total_rows <-
  nrow(
    capitated
  )

capitated_unique_patients <-
  n_distinct(
    capitated$patient_id,
    na.rm = TRUE
  )

capitated_unique_visits <-
  n_distinct(
    capitated$visit_id,
    na.rm = TRUE
  )


capitated_province_audit <- capitated %>%
  
  count(
    source_year,
    facility_province_code_system,
    name = "records"
  ) %>%
  
  transmute(
    
    dataset =
      "primary_capitated_facility",
    
    source_year,
    
    province_code_system =
      facility_province_code_system,
    
    records
    
  )


rm(
  capitated
)

gc()


# ---------------------------------------------------------------------
# Non-capitated primary care facility
# ---------------------------------------------------------------------

cat(
  "\nNon-capitated province audit...\n"
)


noncapitated <- readRDS(
  noncapitated_path
)


noncapitated_total_rows <-
  nrow(
    noncapitated
  )

noncapitated_unique_patients <-
  n_distinct(
    noncapitated$patient_id,
    na.rm = TRUE
  )

noncapitated_unique_visits <-
  n_distinct(
    noncapitated$visit_id,
    na.rm = TRUE
  )


noncapitated_province_audit <- noncapitated %>%
  
  count(
    source_year,
    facility_province_code_system,
    name = "records"
  ) %>%
  
  transmute(
    
    dataset =
      "primary_noncapitated_facility",
    
    source_year,
    
    province_code_system =
      facility_province_code_system,
    
    records
    
  )


rm(
  noncapitated
)

gc()


# ---------------------------------------------------------------------
# Secondary care facility
# ---------------------------------------------------------------------

cat(
  "\nSecondary-care province audit...\n"
)


secondary <- readRDS(
  secondary_path
)


secondary_province_audit <- secondary %>%
  
  count(
    source_year,
    facility_province_code_system,
    name = "records"
  ) %>%
  
  transmute(
    
    dataset =
      "secondary_facility",
    
    source_year,
    
    province_code_system =
      facility_province_code_system,
    
    records
    
  )


rm(
  secondary
)

gc()


province_system_audit <- bind_rows(
  
  participant_province_audit,
  capitated_province_audit,
  noncapitated_province_audit,
  secondary_province_audit
  
)


write_csv(
  
  province_system_audit,
  
  file.path(
    audit_folder,
    "2020_2024_province_system_audit.csv"
  )
  
)


print(
  province_system_audit
)


rm(
  participant_province_audit,
  capitated_province_audit,
  noncapitated_province_audit,
  secondary_province_audit
)

gc()


# =====================================================================
# 9. FINAL DATASET SUMMARY
# =====================================================================

cat("\n")
cat("============================================================\n")
cat("FINAL DATASET SUMMARY\n")
cat("============================================================\n")


# ---------------------------------------------------------------------
# Participant column count
# ---------------------------------------------------------------------

participant <- readRDS(
  participant_path
)

participant_columns <-
  ncol(participant)

rm(participant)

gc()


# ---------------------------------------------------------------------
# Capitated column count
# ---------------------------------------------------------------------

capitated <- readRDS(
  capitated_path
)

capitated_columns <-
  ncol(capitated)

rm(capitated)

gc()


# ---------------------------------------------------------------------
# Non-capitated column count
# ---------------------------------------------------------------------

noncapitated <- readRDS(
  noncapitated_path
)

noncapitated_columns <-
  ncol(noncapitated)

rm(noncapitated)

gc()


# ---------------------------------------------------------------------
# Secondary care column count
# ---------------------------------------------------------------------

secondary <- readRDS(
  secondary_path
)

secondary_columns <-
  ncol(secondary)

rm(secondary)

gc()


# ---------------------------------------------------------------------
# Secondary diagnosis summary
# ---------------------------------------------------------------------

secondary_dx <- readRDS(
  secondary_dx_path
)

secondary_dx_columns <-
  ncol(
    secondary_dx
  )

secondary_dx_total_rows <-
  nrow(
    secondary_dx
  )

secondary_dx_unique_visits <-
  n_distinct(
    secondary_dx$visit_id,
    na.rm = TRUE
  )

rm(
  secondary_dx
)

gc()


final_dataset_summary <- tibble(
  
  dataset = c(
    "participant_backbone",
    "primary_care_capitated",
    "primary_care_noncapitated",
    "secondary_care",
    "secondary_diagnosis"
  ),
  
  rows = c(
    participant_total_rows,
    capitated_total_rows,
    noncapitated_total_rows,
    secondary_total_rows,
    secondary_dx_total_rows
  ),
  
  columns = c(
    participant_columns,
    capitated_columns,
    noncapitated_columns,
    secondary_columns,
    secondary_dx_columns
  ),
  
  unique_patients = c(
    participant_unique_patients,
    capitated_unique_patients,
    noncapitated_unique_patients,
    secondary_unique_patients,
    NA_integer_
  ),
  
  unique_visits = c(
    NA_integer_,
    capitated_unique_visits,
    noncapitated_unique_visits,
    secondary_unique_visits,
    secondary_dx_unique_visits
  )
  
)


write_csv(
  
  final_dataset_summary,
  
  file.path(
    audit_folder,
    "2020_2024_final_dataset_summary.csv"
  )
  
)


print(
  final_dataset_summary
)


# =====================================================================
# 10. LOAD PREVIOUS HARMONISATION AUDITS
# =====================================================================
# Load previously generated audit files where available.
# Return NULL when an audit file is absent.
read_if_exists <- function(filename) {
  
  path <- file.path(
    audit_folder,
    filename
  )
  
  if (file.exists(path)) {
    
    return(
      read_csv(
        path,
        show_col_types = FALSE
      )
    )
  }
  
  NULL
}


input_file_check <-
  read_if_exists(
    "2020_2024_input_file_check.csv"
  )


schema_audit <-
  read_if_exists(
    "2020_2024_schema_audit.csv"
  )


row_count_audit <-
  read_if_exists(
    "2020_2024_row_count_audit.csv"
  )


patient_longitudinal_audit <-
  read_if_exists(
    "2020_2024_patient_longitudinal_audit.csv"
  )


encounter_year_audit <-
  read_if_exists(
    "2020_2024_encounter_year_audit.csv"
  )


# =====================================================================
# 11. SAVE COMBINED AUDIT OBJECT
# =====================================================================

saveRDS(
  
  list(
    
    input_file_check =
      input_file_check,
    
    schema_audit =
      schema_audit,
    
    row_count_audit =
      row_count_audit,
    
    patient_longitudinal_audit =
      patient_longitudinal_audit,
    
    encounter_year_audit =
      encounter_year_audit,
    
    visit_id_duplicate_audit =
      visit_id_duplicate_audit,
    
    participant_linkage_audit =
      participant_linkage_audit,
    
    secondary_diagnosis_linkage_audit =
      secondary_diagnosis_linkage_audit,
    
    province_system_audit =
      province_system_audit,
    
    final_dataset_summary =
      final_dataset_summary
    
  ),
  
  file.path(
    audit_folder,
    "2020_2024_harmonisation_audit_all.rds"
  )
  
)


# =====================================================================
# 12. FINAL SUMMARY
# =====================================================================

cat("\n\n")
cat("============================================================\n")
cat("REMAINING HARMONISATION AUDITS COMPLETE\n")
cat("============================================================\n\n")


cat("Created:\n\n")

cat(
  "1. 2020_2024_visit_id_duplicate_audit.csv\n"
)

cat(
  "2. 2020_2024_participant_linkage_audit.csv\n"
)

cat(
  "3. 2020_2024_secondary_diagnosis_linkage_audit.csv\n"
)

cat(
  "4. 2020_2024_province_system_audit.csv\n"
)

cat(
  "5. 2020_2024_final_dataset_summary.csv\n"
)

cat(
  "6. 2020_2024_harmonisation_audit_all.rds\n\n"
)