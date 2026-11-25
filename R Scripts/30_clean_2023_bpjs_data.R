# ============================================================
# BPJS 2023: Data Cleaning
#
# Clean and standardise the five BPJS data tables:
#   - Participant membership
#   - Capitated primary care (FKTP)
#   - Non-capitated primary care (FKTP)
#   - Secondary care (FKRTL)
#   - Secondary diagnoses
#
# Input directory: 2023/
# Cleaned data directory: 2023_cleaned/
# Audit directory: 2023_cleaning_audit/
#
# Structural changes in 2023:
#   - Introduction of PSTV09_NEW for the 38-province
#     coding system.
#   - Participant demographics are no longer embedded in
#     non-capitated primary care records and require linkage
#     to the participant backbone.
# ============================================================


# ---------------------------------------------------------------------
# 1. Load required packages
# ---------------------------------------------------------------------

required_packages <- c(
  "haven",
  "dplyr",
  "tidyr",
  "tibble",
  "readr",
  "stringr",
  "purrr",
  "lubridate"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(haven)
library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(stringr)
library(purrr)
library(lubridate)


# ---------------------------------------------------------------------
# 2. Configure directories
# ---------------------------------------------------------------------

target_year <- 2023L

raw_folder <- "2023"

clean_folder <- "2023_cleaned"

audit_folder <- "2023_cleaning_audit"

if (!dir.exists(clean_folder)) {
  dir.create(
    clean_folder,
    recursive = TRUE
  )
}

if (!dir.exists(audit_folder)) {
  dir.create(
    audit_folder,
    recursive = TRUE
  )
}


# ---------------------------------------------------------------------
# 3. Define source file paths
# ---------------------------------------------------------------------

participant_file <- file.path(
  raw_folder,
  "2015202301_kepesertaan.dta"
)

primary_cap_file <- file.path(
  raw_folder,
  "202302_fktpkapitasi.dta"
)

secondary_file <- file.path(
  raw_folder,
  "202303_fkrtl.dta"
)

primary_noncap_file <- file.path(
  raw_folder,
  "202304_nonkapitasi.dta"
)

secondary_dx_file <- file.path(
  raw_folder,
  "202305_diagnosissekunder.dta"
)


# ---------------------------------------------------------------------
# 4. Validate source file availability
# ---------------------------------------------------------------------

source_files <- c(
  participant_file,
  primary_cap_file,
  secondary_file,
  primary_noncap_file,
  secondary_dx_file
)

if (any(!file.exists(source_files))) {
  
  missing_files <- source_files[
    !file.exists(source_files)
  ]
  
  stop(
    paste0(
      "Missing source file(s):\n",
      paste(
        missing_files,
        collapse = "\n"
      )
    )
  )
}


# ---------------------------------------------------------------------
# 5. Helper functions
# ---------------------------------------------------------------------

# Convert labelled numeric variables to plain numeric codes
code_numeric <- function(x) {
  as.numeric(x)
}


# Standardise identifiers to character strings for linkage
# across datasets.
id_character <- function(x) {
  
  if (is.character(x)) {
    
    x <- stringr::str_trim(x)
    
    x[x == ""] <- NA_character_
    
    return(x)
  }
  
  output <- format(
    x,
    scientific = FALSE,
    trim = TRUE
  )
  
  output <- stringr::str_trim(output)
  
  output[
    output == ""
  ] <- NA_character_
  
  output
}


# Standardise whitespace and replace empty strings with NA.
clean_character <- function(x) {
  
  x <- as.character(x)
  
  x <- stringr::str_squish(x)
  
  x[
    x == ""
  ] <- NA_character_
  
  x
}


# Standardise ICD-10 codes by normalising whitespace,
# converting to uppercase and removing decimal points.
# Recode specified placeholder values as missing.
clean_icd <- function(x) {
  
  x <- clean_character(x)
  
  x <- toupper(x)
  
  x <- stringr::str_replace_all(
    x,
    "\\.",
    ""
  )
  
  x[
    x %in% c(
      "",
      "9999",
      "9998",
      "98",
      "99",
      "NONE",
      "NA"
    )
  ] <- NA_character_
  
  x
}


# Count repeated identifier occurrences beyond the first,
# excluding missing values.
duplicate_id_count <- function(x) {
  
  x <- x[
    !is.na(x)
  ]
  
  sum(
    duplicated(x)
  )
}


# Calculate minimum date, returning NA for empty or
# entirely missing vectors.
safe_min_date <- function(x) {
  
  if (length(x) == 0 || all(is.na(x))) {
    return(as.Date(NA))
  }
  
  min(
    x,
    na.rm = TRUE
  )
}


# Calculate maximum date, returning NA for empty or
# entirely missing vectors.
safe_max_date <- function(x) {
  
  if (length(x) == 0 || all(is.na(x))) {
    return(as.Date(NA))
  }
  
  max(
    x,
    na.rm = TRUE
  )
}


# =====================================================================
# CLEAN PARTICIPANT BACKBONE
# =====================================================================


# ---------------------------------------------------------------------
# 6. Read participant data
# ---------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("CLEANING 2023 PARTICIPANT BACKBONE\n")
cat("============================================================\n")

participant_raw <- haven::read_dta(
  participant_file
)


# ---------------------------------------------------------------------
# 7. Clean participant membership data
# ---------------------------------------------------------------------

participant_backbone_2023 <- participant_raw %>%
  
  transmute(
    
    patient_id =
      id_character(PSTV01),
    
    family_id =
      id_character(PSTV02),
    
    date_of_birth =
      as.Date(PSTV03),
    
    family_relationship_code =
      code_numeric(PSTV04),
    
    sex_code =
      code_numeric(PSTV05),
    
    marital_status_code =
      code_numeric(PSTV06),
    
    entitlement_class_code =
      code_numeric(PSTV07),
    
    participant_segment_code =
      code_numeric(PSTV08),
    
    # Retain both legacy (PSTV09) and updated 38-province
    # (PSTV09_NEW) residence province codes.
    
    residence_province_code_legacy =
      code_numeric(PSTV09),
    
    residence_province_code =
      code_numeric(PSTV09_NEW),
    
    residence_district_code =
      code_numeric(PSTV10),
    
    registered_facility_ownership_code =
      code_numeric(PSTV11),
    
    registered_facility_type_code =
      code_numeric(PSTV12),
    
    registered_facility_province_code =
      code_numeric(PSTV13),
    
    registered_facility_district_code =
      code_numeric(PSTV14),
    
    sample_weight =
      as.numeric(PSTV15),
    
    sample_year =
      as.integer(PSTV16),
    
    membership_status_code =
      code_numeric(PSTV17),
    
    death_year =
      as.integer(PSTV18)
    
  ) %>%
  
  mutate(
    
    sex = case_when(
      
      sex_code == 1 ~ "Male",
      
      sex_code == 2 ~ "Female",
      
      TRUE ~ "Unknown"
      
    ),
    
    source_year =
      target_year,
    
    source_dataset =
      "kepesertaan"
    
  )


saveRDS(
  participant_backbone_2023,
  file.path(
    clean_folder,
    "participant_backbone_2023.rds"
  )
)


cat(
  "Saved participant_backbone_2023.rds:",
  format(
    nrow(participant_backbone_2023),
    big.mark = ","
  ),
  "rows\n"
)


rm(
  participant_raw
)

gc()


# =====================================================================
# CLEAN PRIMARY CARE - CAPITATED
# =====================================================================


# ---------------------------------------------------------------------
# 8. Read capitated primary care
# ---------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("CLEANING 2023 CAPITATED PRIMARY CARE\n")
cat("============================================================\n")

primary_cap_raw <- haven::read_dta(
  primary_cap_file
)


# ---------------------------------------------------------------------
# 9. Clean capitated primary care data
# ---------------------------------------------------------------------

primary_care_capitated_2023 <- primary_cap_raw %>%
  
  transmute(
    
    patient_id =
      id_character(PSTV01),
    
    family_id =
      id_character(PSTV02),
    
    sample_weight =
      as.numeric(PSTV15),
    
    visit_id =
      id_character(FKP02),
    
    visit_date =
      as.Date(FKP03),
    
    discharge_date =
      as.Date(FKP04),
    
    facility_province_code =
      code_numeric(FKP05),
    
    facility_district_code =
      code_numeric(FKP06),
    
    facility_ownership_code =
      code_numeric(FKP07),
    
    facility_type_code =
      code_numeric(FKP08),
    
    facility_subtype_code =
      code_numeric(FKP09),
    
    service_level_code =
      code_numeric(FKP10),
    
    clinic_type_code =
      code_numeric(FKP11),
    
    participant_segment_code =
      code_numeric(FKP12),
    
    discharge_status_code =
      code_numeric(FKP13),
    
    diagnosis_3digit_code =
      clean_icd(FKP14A),
    
    diagnosis_code =
      clean_icd(FKP15),
    
    diagnosis_name =
      clean_character(FKP15A),
    
    referral_province_code =
      code_numeric(FKP16),
    
    referral_district_code =
      code_numeric(FKP17),
    
    referral_ownership_code =
      code_numeric(FKP18),
    
    referral_facility_type_code =
      code_numeric(FKP19),
    
    referral_facility_subtype_code =
      code_numeric(FKP20),
    
    referral_clinic_code =
      code_numeric(FKP21),
    
    visit_type_code =
      code_numeric(FKP22)
    
  ) %>%
  
  mutate(
    
    care_setting =
      "Primary care",
    
    primary_care_type =
      "Capitated",
    
    source_year =
      target_year,
    
    source_dataset =
      "fktp_kapitasi"
    
  )


saveRDS(
  primary_care_capitated_2023,
  file.path(
    clean_folder,
    "primary_care_capitated_2023.rds"
  )
)


cat(
  "Saved primary_care_capitated_2023.rds:",
  format(
    nrow(primary_care_capitated_2023),
    big.mark = ","
  ),
  "rows\n"
)


rm(
  primary_cap_raw
)

gc()


# =====================================================================
# CLEAN PRIMARY CARE - NON-CAPITATED
# =====================================================================


# ---------------------------------------------------------------------
# 10. Read non-capitated primary care
# ---------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("CLEANING 2023 NON-CAPITATED PRIMARY CARE\n")
cat("============================================================\n")

primary_noncap_raw <- haven::read_dta(
  primary_noncap_file
)


# ---------------------------------------------------------------------
# 11. Clean non-capitated primary care data
#
# The 2023 non-capitated dataset contains 21 source variables.
# Participant variables PSTV03-PSTV14 and PSTV16-PSTV18
# are absent and require linkage to the participant backbone
# using patient_id.
# ---------------------------------------------------------------------

primary_care_noncapitated_2023 <- primary_noncap_raw %>%
  
  transmute(
    
    patient_id =
      id_character(PSTV01),
    
    family_id =
      id_character(PSTV02),
    
    sample_weight =
      as.numeric(PSTV15),
    
    visit_id =
      id_character(PNK02),
    
    visit_date =
      as.Date(PNK03),
    
    procedure_date =
      as.Date(PNK04),
    
    discharge_date =
      as.Date(PNK05),
    
    facility_province_code =
      code_numeric(PNK06),
    
    facility_district_code =
      code_numeric(PNK07),
    
    facility_ownership_code =
      code_numeric(PNK08),
    
    facility_type_code =
      code_numeric(PNK09),
    
    facility_subtype_code =
      code_numeric(PNK10),
    
    service_level_code =
      code_numeric(PNK11),
    
    participant_segment_code =
      code_numeric(PNK12),
    
    diagnosis_3digit_code =
      clean_icd(PNK13A),
    
    diagnosis_code =
      clean_icd(PNK14),
    
    diagnosis_name =
      clean_character(PNK15),
    
    procedure_code =
      code_numeric(PNK16),
    
    billed_cost =
      as.numeric(PNK17),
    
    verified_cost =
      as.numeric(PNK18)
    
  ) %>%
  
  mutate(
    
    care_setting =
      "Primary care",
    
    primary_care_type =
      "Non-capitated",
    
    source_year =
      target_year,
    
    source_dataset =
      "fktp_nonkapitasi"
    
  )


saveRDS(
  primary_care_noncapitated_2023,
  file.path(
    clean_folder,
    "primary_care_noncapitated_2023.rds"
  )
)


cat(
  "Saved primary_care_noncapitated_2023.rds:",
  format(
    nrow(primary_care_noncapitated_2023),
    big.mark = ","
  ),
  "rows\n"
)


rm(
  primary_noncap_raw
)

gc()


# =====================================================================
# CLEAN SECONDARY CARE
# =====================================================================


# ---------------------------------------------------------------------
# 12. Read secondary care
# ---------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("CLEANING 2023 SECONDARY CARE\n")
cat("============================================================\n")

secondary_raw <- haven::read_dta(
  secondary_file
)


# ---------------------------------------------------------------------
# 13. Clean secondary care data
# ---------------------------------------------------------------------

secondary_care_2023 <- secondary_raw %>%
  
  transmute(
    
    patient_id =
      id_character(PSTV01),
    
    family_id =
      id_character(PSTV02),
    
    sample_weight =
      as.numeric(PSTV15),
    
    referring_primary_visit_id =
      id_character(FKP02),
    
    visit_id =
      id_character(FKL02),
    
    admission_date =
      as.Date(FKL03),
    
    discharge_date =
      as.Date(FKL04),
    
    facility_province_code =
      code_numeric(FKL05),
    
    facility_district_code =
      code_numeric(FKL06),
    
    facility_ownership_code =
      code_numeric(FKL07),
    
    facility_type_code =
      code_numeric(FKL08),
    
    hospital_class_code =
      code_numeric(FKL09),
    
    service_level_code =
      code_numeric(FKL10),
    
    clinic_type_code =
      code_numeric(FKL11),
    
    participant_segment_code =
      code_numeric(FKL12),
    
    treatment_class_code =
      code_numeric(FKL13),
    
    discharge_status_code =
      code_numeric(FKL14),
    
    admission_diagnosis_3digit_code =
      clean_icd(FKL15A),
    
    admission_diagnosis_code =
      clean_icd(FKL16),
    
    admission_diagnosis_name =
      clean_character(FKL16A),
    
    primary_diagnosis_3digit_code =
      clean_icd(FKL17A),
    
    primary_diagnosis_code =
      clean_icd(FKL18),
    
    primary_diagnosis_name =
      clean_character(FKL18A),
    
    inacbg_code =
      clean_character(FKL19),
    
    inacbg_description =
      clean_character(FKL19A),
    
    inacbg_main_group_code =
      code_numeric(FKL20),
    
    inacbg_case_group_code =
      code_numeric(FKL21),
    
    inacbg_case_specific_code =
      as.numeric(FKL22),
    
    inacbg_severity_code =
      code_numeric(FKL23),
    
    referring_facility_province_code =
      code_numeric(FKL25),
    
    referring_facility_district_code =
      code_numeric(FKL26),
    
    referring_facility_ownership_code =
      code_numeric(FKL27),
    
    referring_facility_type_code =
      code_numeric(FKL28),
    
    referring_facility_subtype_code =
      code_numeric(FKL29),
    
    procedure_description =
      clean_character(FKL30),
    
    inacbg_tariff_region_code =
      code_numeric(FKL31),
    
    inacbg_tariff =
      as.numeric(FKL32),
    
    special_subacute_code =
      clean_character(FKL33),
    
    special_subacute_tariff =
      as.numeric(FKL34),
    
    special_procedure_code =
      clean_character(FKL35),
    
    special_procedure_description =
      clean_character(FKL36),
    
    special_procedure_tariff =
      as.numeric(FKL37),
    
    special_prosthesis_code =
      clean_character(FKL38),
    
    special_prosthesis_description =
      clean_character(FKL39),
    
    special_prosthesis_tariff =
      as.numeric(FKL40),
    
    special_investigation_code =
      clean_character(FKL41),
    
    special_investigation_description =
      clean_character(FKL42),
    
    special_investigation_tariff =
      as.numeric(FKL43),
    
    special_drug_code =
      clean_character(FKL44),
    
    special_drug_description =
      clean_character(FKL45),
    
    special_drug_tariff =
      as.numeric(FKL46),
    
    billed_cost =
      as.numeric(FKL47),
    
    verified_cost =
      as.numeric(FKL48)
    
  ) %>%
  
  mutate(
    
    # Use admission date as the standard encounter date.
    visit_date =
      admission_date,
    
    # Map primary diagnosis fields to standard diagnosis variables.
    diagnosis_3digit_code =
      primary_diagnosis_3digit_code,
    
    diagnosis_code =
      primary_diagnosis_code,
    
    diagnosis_name =
      primary_diagnosis_name,
    
    care_setting =
      "Secondary care",
    
    source_year =
      target_year,
    
    source_dataset =
      "fkrtl"
    
  )


saveRDS(
  secondary_care_2023,
  file.path(
    clean_folder,
    "secondary_care_2023.rds"
  )
)


cat(
  "Saved secondary_care_2023.rds:",
  format(
    nrow(secondary_care_2023),
    big.mark = ","
  ),
  "rows\n"
)


rm(
  secondary_raw
)

gc()


# =====================================================================
# CLEAN SECONDARY DIAGNOSES
# =====================================================================


# ---------------------------------------------------------------------
# 14. Read secondary diagnosis data
# ---------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("CLEANING 2023 SECONDARY DIAGNOSES\n")
cat("============================================================\n")

secondary_dx_raw <- haven::read_dta(
  secondary_dx_file
)


# ---------------------------------------------------------------------
# 15. Clean secondary diagnosis data
#
# Source variable definitions:
# FKL02  = Secondary care visit ID
# FKL24  = Detailed secondary diagnosis ICD-10 code
# FKL24A = 3-character ICD-10 diagnosis category
# FKL24B = Diagnosis category code and description
# ---------------------------------------------------------------------

secondary_diagnosis_2023 <- secondary_dx_raw %>%
  
  transmute(
    
    visit_id =
      id_character(FKL02),
    
    secondary_diagnosis_code =
      clean_icd(FKL24),
    
    secondary_diagnosis_3digit_code =
      clean_icd(FKL24A),
    
    secondary_diagnosis_code_name_3digit =
      clean_character(FKL24B)
    
  ) %>%
  
  # Generate a diagnosis sequence using the original row order
  # within each visit. The sequence does not indicate clinical
  # priority or diagnosis position.
  group_by(
    visit_id
  ) %>%
  
  mutate(
    
    secondary_diagnosis_sequence =
      row_number()
    
  ) %>%
  
  ungroup() %>%
  
  mutate(
    
    secondary_diagnosis_record_id =
      paste0(
        visit_id,
        "_SDX",
        sprintf(
          "%03d",
          secondary_diagnosis_sequence
        )
      ),
    
    source_year =
      target_year,
    
    source_dataset =
      "diagnosis_sekunder"
    
  )


saveRDS(
  secondary_diagnosis_2023,
  file.path(
    clean_folder,
    "secondary_diagnosis_2023.rds"
  )
)


cat(
  "Saved secondary_diagnosis_2023.rds:",
  format(
    nrow(secondary_diagnosis_2023),
    big.mark = ","
  ),
  "rows\n"
)


rm(
  secondary_dx_raw
)

gc()


# =====================================================================
# AUDIT 1: CLEANING SUMMARY
# =====================================================================


# ---------------------------------------------------------------------
# 16. Compile cleaning summary
# ---------------------------------------------------------------------

cleaning_summary <- tibble(
  
  dataset = c(
    "participant_backbone",
    "primary_care_capitated",
    "primary_care_noncapitated",
    "secondary_care",
    "secondary_diagnosis"
  ),
  
  year =
    target_year,
  
  rows = c(
    nrow(
      participant_backbone_2023
    ),
    nrow(
      primary_care_capitated_2023
    ),
    nrow(
      primary_care_noncapitated_2023
    ),
    nrow(
      secondary_care_2023
    ),
    nrow(
      secondary_diagnosis_2023
    )
  ),
  
  columns = c(
    ncol(
      participant_backbone_2023
    ),
    ncol(
      primary_care_capitated_2023
    ),
    ncol(
      primary_care_noncapitated_2023
    ),
    ncol(
      secondary_care_2023
    ),
    ncol(
      secondary_diagnosis_2023
    )
  ),
  
  unique_patients = c(
    
    n_distinct(
      participant_backbone_2023$patient_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      primary_care_capitated_2023$patient_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      primary_care_noncapitated_2023$patient_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      secondary_care_2023$patient_id,
      na.rm = TRUE
    ),
    
    NA_integer_
  ),
  
  unique_visits = c(
    
    NA_integer_,
    
    n_distinct(
      primary_care_capitated_2023$visit_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      primary_care_noncapitated_2023$visit_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      secondary_care_2023$visit_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      secondary_diagnosis_2023$visit_id,
      na.rm = TRUE
    )
  )
  
)


write_csv(
  cleaning_summary,
  file.path(
    audit_folder,
    "2023_cleaning_summary.csv"
  )
)


# =====================================================================
# AUDIT 2: DUPLICATES
# =====================================================================


# ---------------------------------------------------------------------
# 17. Assess identifier duplication
# ---------------------------------------------------------------------

duplicate_audit <- tibble(
  
  dataset = c(
    "participant_backbone",
    "primary_care_capitated",
    "primary_care_noncapitated",
    "secondary_care",
    "secondary_diagnosis_record_id"
  ),
  
  identifier = c(
    "patient_id",
    "visit_id",
    "visit_id",
    "visit_id",
    "secondary_diagnosis_record_id"
  ),
  
  rows = c(
    nrow(
      participant_backbone_2023
    ),
    nrow(
      primary_care_capitated_2023
    ),
    nrow(
      primary_care_noncapitated_2023
    ),
    nrow(
      secondary_care_2023
    ),
    nrow(
      secondary_diagnosis_2023
    )
  ),
  
  unique_ids = c(
    
    n_distinct(
      participant_backbone_2023$patient_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      primary_care_capitated_2023$visit_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      primary_care_noncapitated_2023$visit_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      secondary_care_2023$visit_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      secondary_diagnosis_2023$
        secondary_diagnosis_record_id,
      na.rm = TRUE
    )
  ),
  
  duplicate_occurrences = c(
    
    duplicate_id_count(
      participant_backbone_2023$patient_id
    ),
    
    duplicate_id_count(
      primary_care_capitated_2023$visit_id
    ),
    
    duplicate_id_count(
      primary_care_noncapitated_2023$visit_id
    ),
    
    duplicate_id_count(
      secondary_care_2023$visit_id
    ),
    
    duplicate_id_count(
      secondary_diagnosis_2023$
        secondary_diagnosis_record_id
    )
  )
  
)


write_csv(
  duplicate_audit,
  file.path(
    audit_folder,
    "2023_duplicate_audit.csv"
  )
)


# =====================================================================
# AUDIT 3: PATIENT LINKAGE
# =====================================================================


# ---------------------------------------------------------------------
# 18. Assess participant linkage
# ---------------------------------------------------------------------

participant_ids <- participant_backbone_2023 %>%
  distinct(
    patient_id
  )


linkage_audit <- bind_rows(
  
  primary_care_capitated_2023 %>%
    
    summarise(
      
      dataset =
        "primary_care_capitated",
      
      records =
        n(),
      
      records_linked_to_participant =
        sum(
          patient_id %in%
            participant_ids$patient_id
        ),
      
      records_not_linked =
        sum(
          !(patient_id %in%
              participant_ids$patient_id)
        )
    ),
  
  primary_care_noncapitated_2023 %>%
    
    summarise(
      
      dataset =
        "primary_care_noncapitated",
      
      records =
        n(),
      
      records_linked_to_participant =
        sum(
          patient_id %in%
            participant_ids$patient_id
        ),
      
      records_not_linked =
        sum(
          !(patient_id %in%
              participant_ids$patient_id)
        )
    ),
  
  secondary_care_2023 %>%
    
    summarise(
      
      dataset =
        "secondary_care",
      
      records =
        n(),
      
      records_linked_to_participant =
        sum(
          patient_id %in%
            participant_ids$patient_id
        ),
      
      records_not_linked =
        sum(
          !(patient_id %in%
              participant_ids$patient_id)
        )
    )
  
) %>%
  
  mutate(
    
    linkage_percent =
      round(
        records_linked_to_participant /
          records *
          100,
        2
      )
    
  )


write_csv(
  linkage_audit,
  file.path(
    audit_folder,
    "2023_linkage_audit.csv"
  )
)


# =====================================================================
# AUDIT 4: SECONDARY DIAGNOSIS LINKAGE
# =====================================================================


# ---------------------------------------------------------------------
# 19. Assess secondary diagnosis linkage
# ---------------------------------------------------------------------

secondary_visit_ids <- secondary_care_2023 %>%
  distinct(
    visit_id
  )


secondary_dx_linkage <- secondary_diagnosis_2023 %>%
  
  summarise(
    
    secondary_diagnosis_records =
      n(),
    
    unique_secondary_diagnosis_visits =
      n_distinct(
        visit_id
      ),
    
    records_linked_to_fkrtl =
      sum(
        visit_id %in%
          secondary_visit_ids$visit_id
      ),
    
    records_not_linked_to_fkrtl =
      sum(
        !(visit_id %in%
            secondary_visit_ids$visit_id)
      )
    
  ) %>%
  
  mutate(
    
    linkage_percent =
      round(
        records_linked_to_fkrtl /
          secondary_diagnosis_records *
          100,
        2
      )
    
  )


# ---------------------------------------------------------------------
# 20. Summarise diagnosis records per visit
# ---------------------------------------------------------------------

secondary_dx_structure <- secondary_diagnosis_2023 %>%
  
  group_by(
    visit_id
  ) %>%
  
  summarise(
    
    number_secondary_diagnoses =
      n(),
    
    .groups =
      "drop"
    
  ) %>%
  
  summarise(
    
    unique_visits =
      n(),
    
    total_secondary_diagnosis_records =
      sum(
        number_secondary_diagnoses
      ),
    
    minimum_diagnoses_per_visit =
      min(
        number_secondary_diagnoses
      ),
    
    median_diagnoses_per_visit =
      median(
        number_secondary_diagnoses
      ),
    
    mean_diagnoses_per_visit =
      mean(
        number_secondary_diagnoses
      ),
    
    maximum_diagnoses_per_visit =
      max(
        number_secondary_diagnoses
      )
    
  )


secondary_diagnosis_audit <- bind_cols(
  
  secondary_dx_linkage,
  
  secondary_dx_structure
  
)


write_csv(
  secondary_diagnosis_audit,
  file.path(
    audit_folder,
    "2023_secondary_diagnosis_audit.csv"
  )
)


# =====================================================================
# AUDIT 5: DATES
# =====================================================================


# ---------------------------------------------------------------------
# 21. Assess encounter date distributions
# ---------------------------------------------------------------------

date_audit <- tibble(
  
  dataset = c(
    "primary_care_capitated",
    "primary_care_noncapitated",
    "secondary_care"
  ),
  
  date_variable = c(
    "visit_date",
    "visit_date",
    "admission_date"
  ),
  
  minimum_date = as.character(
    c(
      
      safe_min_date(
        primary_care_capitated_2023$
          visit_date
      ),
      
      safe_min_date(
        primary_care_noncapitated_2023$
          visit_date
      ),
      
      safe_min_date(
        secondary_care_2023$
          admission_date
      )
      
    )
  ),
  
  maximum_date = as.character(
    c(
      
      safe_max_date(
        primary_care_capitated_2023$
          visit_date
      ),
      
      safe_max_date(
        primary_care_noncapitated_2023$
          visit_date
      ),
      
      safe_max_date(
        secondary_care_2023$
          admission_date
      )
      
    )
  ),
  
  records_before_2023 = c(
    
    sum(
      primary_care_capitated_2023$
        visit_date <
        as.Date("2023-01-01"),
      na.rm = TRUE
    ),
    
    sum(
      primary_care_noncapitated_2023$
        visit_date <
        as.Date("2023-01-01"),
      na.rm = TRUE
    ),
    
    sum(
      secondary_care_2023$
        admission_date <
        as.Date("2023-01-01"),
      na.rm = TRUE
    )
    
  ),
  
  records_after_2023 = c(
    
    sum(
      primary_care_capitated_2023$
        visit_date >
        as.Date("2023-12-31"),
      na.rm = TRUE
    ),
    
    sum(
      primary_care_noncapitated_2023$
        visit_date >
        as.Date("2023-12-31"),
      na.rm = TRUE
    ),
    
    sum(
      secondary_care_2023$
        admission_date >
        as.Date("2023-12-31"),
      na.rm = TRUE
    )
    
  ),
  
  missing_date = c(
    
    sum(
      is.na(
        primary_care_capitated_2023$
          visit_date
      )
    ),
    
    sum(
      is.na(
        primary_care_noncapitated_2023$
          visit_date
      )
    ),
    
    sum(
      is.na(
        secondary_care_2023$
          admission_date
      )
    )
    
  )
  
)


write_csv(
  date_audit,
  file.path(
    audit_folder,
    "2023_date_audit.csv"
  )
)


# =====================================================================
# AUDIT 6: MISSING KEY VARIABLES
# =====================================================================


# ---------------------------------------------------------------------
# 22. Assess missing key variables
# ---------------------------------------------------------------------

missing_key_audit <- tibble(
  
  dataset = c(
    "participant_backbone",
    "primary_care_capitated",
    "primary_care_noncapitated",
    "secondary_care",
    "secondary_diagnosis"
  ),
  
  rows = c(
    nrow(
      participant_backbone_2023
    ),
    nrow(
      primary_care_capitated_2023
    ),
    nrow(
      primary_care_noncapitated_2023
    ),
    nrow(
      secondary_care_2023
    ),
    nrow(
      secondary_diagnosis_2023
    )
  ),
  
  missing_patient_id = c(
    
    sum(
      is.na(
        participant_backbone_2023$
          patient_id
      )
    ),
    
    sum(
      is.na(
        primary_care_capitated_2023$
          patient_id
      )
    ),
    
    sum(
      is.na(
        primary_care_noncapitated_2023$
          patient_id
      )
    ),
    
    sum(
      is.na(
        secondary_care_2023$
          patient_id
      )
    ),
    
    NA_integer_
    
  ),
  
  missing_visit_id = c(
    
    NA_integer_,
    
    sum(
      is.na(
        primary_care_capitated_2023$
          visit_id
      )
    ),
    
    sum(
      is.na(
        primary_care_noncapitated_2023$
          visit_id
      )
    ),
    
    sum(
      is.na(
        secondary_care_2023$
          visit_id
      )
    ),
    
    sum(
      is.na(
        secondary_diagnosis_2023$
          visit_id
      )
    )
    
  ),
  
  missing_diagnosis_3digit_code = c(
    
    NA_integer_,
    
    sum(
      is.na(
        primary_care_capitated_2023$
          diagnosis_3digit_code
      )
    ),
    
    sum(
      is.na(
        primary_care_noncapitated_2023$
          diagnosis_3digit_code
      )
    ),
    
    sum(
      is.na(
        secondary_care_2023$
          primary_diagnosis_3digit_code
      )
    ),
    
    sum(
      is.na(
        secondary_diagnosis_2023$
          secondary_diagnosis_3digit_code
      )
    )
    
  ),
  
  missing_diagnosis_code = c(
    
    NA_integer_,
    
    sum(
      is.na(
        primary_care_capitated_2023$
          diagnosis_code
      )
    ),
    
    sum(
      is.na(
        primary_care_noncapitated_2023$
          diagnosis_code
      )
    ),
    
    sum(
      is.na(
        secondary_care_2023$
          primary_diagnosis_code
      )
    ),
    
    sum(
      is.na(
        secondary_diagnosis_2023$
          secondary_diagnosis_code
      )
    )
    
  )
  
)


missing_key_audit <- missing_key_audit %>%
  
  mutate(
    
    missing_diagnosis_code_percent =
      round(
        missing_diagnosis_code /
          rows *
          100,
        2
      )
    
  )


write_csv(
  missing_key_audit,
  file.path(
    audit_folder,
    "2023_missing_key_audit.csv"
  )
)


# =====================================================================
# AUDIT 7: 2023 PROVINCE CODING CHANGE
# =====================================================================


# ---------------------------------------------------------------------
# 23. Compare legacy and updated residence province codes
# ---------------------------------------------------------------------

province_code_audit <- participant_backbone_2023 %>%
  
  summarise(
    
    total_participants =
      n(),
    
    unique_legacy_province_codes =
      n_distinct(
        residence_province_code_legacy,
        na.rm = TRUE
      ),
    
    unique_new_province_codes =
      n_distinct(
        residence_province_code,
        na.rm = TRUE
      ),
    
    missing_legacy_province_code =
      sum(
        is.na(
          residence_province_code_legacy
        )
      ),
    
    missing_new_province_code =
      sum(
        is.na(
          residence_province_code
        )
      ),
    
    participants_where_codes_differ =
      sum(
        residence_province_code_legacy !=
          residence_province_code,
        na.rm = TRUE
      ),
    
    participants_where_codes_match =
      sum(
        residence_province_code_legacy ==
          residence_province_code,
        na.rm = TRUE
      )
    
  ) %>%
  
  mutate(
    
    percent_where_codes_differ =
      round(
        participants_where_codes_differ /
          total_participants *
          100,
        2
      )
    
  )


write_csv(
  province_code_audit,
  file.path(
    audit_folder,
    "2023_province_code_audit.csv"
  )
)


# =====================================================================
# AUDIT 8: NON-CAPITATED DEMOGRAPHIC LINKABILITY
# =====================================================================


# ---------------------------------------------------------------------
# 24. Assess demographic completeness following participant linkage
# ---------------------------------------------------------------------

noncap_demographic_linkage <- primary_care_noncapitated_2023 %>%
  
  left_join(
    
    participant_backbone_2023 %>%
      select(
        patient_id,
        date_of_birth,
        sex,
        residence_province_code
      ),
    
    by =
      "patient_id"
    
  ) %>%
  
  summarise(
    
    total_noncapitated_records =
      n(),
    
    linked_date_of_birth =
      sum(
        !is.na(
          date_of_birth
        )
      ),
    
    linked_sex =
      sum(
        !is.na(
          sex
        ) &
          sex != "Unknown"
      ),
    
    linked_residence_province =
      sum(
        !is.na(
          residence_province_code
        )
      )
    
  ) %>%
  
  mutate(
    
    dob_linkage_percent =
      round(
        linked_date_of_birth /
          total_noncapitated_records *
          100,
        2
      ),
    
    sex_linkage_percent =
      round(
        linked_sex /
          total_noncapitated_records *
          100,
        2
      ),
    
    province_linkage_percent =
      round(
        linked_residence_province /
          total_noncapitated_records *
          100,
        2
      )
    
  )


write_csv(
  noncap_demographic_linkage,
  file.path(
    audit_folder,
    "2023_noncapitated_demographic_linkage.csv"
  )
)


# =====================================================================
# AUDIT 9: SAVE ALL AUDITS AS ONE R OBJECT
# =====================================================================


# ---------------------------------------------------------------------
# 25. Save combined audit object
# ---------------------------------------------------------------------

saveRDS(
  
  list(
    
    cleaning_summary =
      cleaning_summary,
    
    duplicate_audit =
      duplicate_audit,
    
    linkage_audit =
      linkage_audit,
    
    date_audit =
      date_audit,
    
    secondary_diagnosis_audit =
      secondary_diagnosis_audit,
    
    missing_key_audit =
      missing_key_audit,
    
    province_code_audit =
      province_code_audit,
    
    noncapitated_demographic_linkage =
      noncap_demographic_linkage
    
  ),
  
  file.path(
    audit_folder,
    "2023_cleaning_audit_all.rds"
  )
  
)


# =====================================================================
# FINAL SUMMARY
# =====================================================================


# ---------------------------------------------------------------------
# 26.	Final summary
# ---------------------------------------------------------------------

cat("\n\n")
cat("============================================================\n")
cat("2023 BPJS CLEANING COMPLETE\n")
cat("============================================================\n\n")


cat("Cleaned RDS files created:\n\n")

cat(
  "1. participant_backbone_2023.rds\n"
)

cat(
  "2. primary_care_capitated_2023.rds\n"
)

cat(
  "3. primary_care_noncapitated_2023.rds\n"
)

cat(
  "4. secondary_care_2023.rds\n"
)

cat(
  "5. secondary_diagnosis_2023.rds\n\n"
)


cat("Audit files created:\n\n")

cat(
  "1. 2023_cleaning_summary.csv\n"
)

cat(
  "2. 2023_duplicate_audit.csv\n"
)

cat(
  "3. 2023_linkage_audit.csv\n"
)

cat(
  "4. 2023_date_audit.csv\n"
)

cat(
  "5. 2023_secondary_diagnosis_audit.csv\n"
)

cat(
  "6. 2023_missing_key_audit.csv\n"
)

cat(
  "7. 2023_province_code_audit.csv\n"
)

cat(
  "8. 2023_noncapitated_demographic_linkage.csv\n"
)

cat(
  "9. 2023_cleaning_audit_all.rds\n\n"
)


cat(
  "Cleaned data folder:\n",
  normalizePath(
    clean_folder
  ),
  "\n\n"
)


cat(
  "Audit folder:\n",
  normalizePath(
    audit_folder
  ),
  "\n\n"
)


cat("Cleaning summary:\n\n")

print(
  cleaning_summary
)


cat("\nPatient linkage:\n\n")

print(
  linkage_audit
)


cat("\nProvince coding audit:\n\n")

print(
  province_code_audit
)


cat("\nNon-capitated demographic linkage:\n\n")

print(
  noncap_demographic_linkage
)