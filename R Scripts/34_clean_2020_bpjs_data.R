# ============================================================
# BPJS 2020: Data Cleaning
#
# Clean and standardise the five BPJS data tables:
#   - Participant membership
#   - Capitated primary care (FKTP)
#   - Non-capitated primary care (FKTP)
#   - Secondary care (FKRTL)
#   - Secondary diagnoses
#
# Input directory: 2020/
# Cleaned data directory: 2020_cleaned/
# Audit directory: 2020_cleaning_audit/
#
# Apply consistent cleaning and auditing procedures across
# annual BPJS datasets.
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
# 2. Configure directories and target year
# ---------------------------------------------------------------------
# Source year identifies the dataset release.
# Actual encounter dates are retained separately.
target_year <- 2020L

raw_folder <- "2020"

clean_folder <- "2020_cleaned"

audit_folder <- "2020_cleaning_audit"

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
# 3. Identify source files
# ---------------------------------------------------------------------

all_dta_files <- list.files(
  path = raw_folder,
  pattern = "\\.dta$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(all_dta_files) == 0) {
  stop(
    paste0(
      "No .dta files found in folder: ",
      raw_folder
    )
  )
}

# Identify source files by filename pattern and require
# exactly one matching file per dataset.
find_file <- function(pattern, dataset_name) {
  
  matches <- all_dta_files[
    str_detect(
      tolower(
        basename(all_dta_files)
      ),
      tolower(pattern)
    )
  ]
  
  if (length(matches) == 0) {
    
    stop(
      paste0(
        "Could not find file for ",
        dataset_name,
        ".\n\nFiles found:\n",
        paste(
          basename(all_dta_files),
          collapse = "\n"
        )
      )
    )
  }
  
  if (length(matches) > 1) {
    
    stop(
      paste0(
        "Multiple files matched ",
        dataset_name,
        ":\n",
        paste(
          basename(matches),
          collapse = "\n"
        )
      )
    )
  }
  
  matches
}


participant_file <- find_file(
  "kepesertaan",
  "kepesertaan"
)

primary_cap_file <- find_file(
  "fktpkapitasi",
  "fktp_kapitasi"
)

secondary_file <- find_file(
  "fkrtl",
  "fkrtl"
)

primary_noncap_file <- find_file(
  "nonkapitasi",
  "fktp_nonkapitasi"
)

secondary_dx_file <- find_file(
  "diagnosissekunder",
  "diagnosis_sekunder"
)


# ---------------------------------------------------------------------
# 4. Helper functions
# ---------------------------------------------------------------------
# Convert labelled numeric variables to numeric codes.
code_numeric <- function(x) {
  as.numeric(x)
}

# Standardise identifiers to character strings for linkage
# across datasets.
id_character <- function(x) {
  
  if (is.character(x)) {
    
    x <- str_trim(x)
    
    x[x == ""] <- NA_character_
    
    return(x)
  }
  
  output <- format(
    x,
    scientific = FALSE,
    trim = TRUE
  )
  
  output <- str_trim(output)
  
  output[
    output == ""
  ] <- NA_character_
  
  output
}

# Standardise whitespace and replace empty strings with NA.
clean_character <- function(x) {
  
  x <- as.character(x)
  
  x <- str_squish(x)
  
  x[x == ""] <- NA_character_
  
  x
}

# Standardise ICD-10 codes by normalising whitespace,
# converting to uppercase and removing decimal points.
# Recode specified placeholder values as missing.
clean_icd <- function(x) {
  
  x <- clean_character(x)
  
  x <- toupper(x)
  
  x <- str_replace_all(
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
# CLEAN PARTICIPANT MEMBERSHIP DATA
# =====================================================================

cat("\n")
cat("============================================================\n")
cat("CLEANING 2020 PARTICIPANT BACKBONE\n")
cat("============================================================\n")


participant_raw <- read_dta(
  participant_file
)


participant_backbone_2020 <- participant_raw %>%
  
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
    
    residence_province_code =
      code_numeric(PSTV09),
    
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
  participant_backbone_2020,
  file.path(
    clean_folder,
    "participant_backbone_2020.rds"
  )
)


cat(
  "Saved participant_backbone_2020.rds:",
  format(
    nrow(participant_backbone_2020),
    big.mark = ","
  ),
  "rows\n"
)


rm(participant_raw)

gc()


# =====================================================================
# CLEAN CAPITATED PRIMARY CARE
# =====================================================================

cat("\n")
cat("============================================================\n")
cat("CLEANING 2020 CAPITATED PRIMARY CARE\n")
cat("============================================================\n")


primary_cap_raw <- read_dta(
  primary_cap_file
)


primary_care_capitated_2020 <- primary_cap_raw %>%
  
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
  primary_care_capitated_2020,
  file.path(
    clean_folder,
    "primary_care_capitated_2020.rds"
  )
)


cat(
  "Saved primary_care_capitated_2020.rds:",
  format(
    nrow(primary_care_capitated_2020),
    big.mark = ","
  ),
  "rows\n"
)


rm(primary_cap_raw)

gc()


# =====================================================================
# CLEAN NON-CAPITATED PRIMARY CARE
# =====================================================================

cat("\n")
cat("============================================================\n")
cat("CLEANING 2020 NON-CAPITATED PRIMARY CARE\n")
cat("============================================================\n")


primary_noncap_raw <- read_dta(
  primary_noncap_file
)


primary_care_noncapitated_2020 <- primary_noncap_raw %>%
  
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
  primary_care_noncapitated_2020,
  file.path(
    clean_folder,
    "primary_care_noncapitated_2020.rds"
  )
)


cat(
  "Saved primary_care_noncapitated_2020.rds:",
  format(
    nrow(primary_care_noncapitated_2020),
    big.mark = ","
  ),
  "rows\n"
)


rm(primary_noncap_raw)

gc()


# =====================================================================
# CLEAN SECONDARY CARE
# =====================================================================

cat("\n")
cat("============================================================\n")
cat("CLEANING 2020 SECONDARY CARE\n")
cat("============================================================\n")


secondary_raw <- read_dta(
  secondary_file
)


secondary_care_2020 <- secondary_raw %>%
  
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
  secondary_care_2020,
  file.path(
    clean_folder,
    "secondary_care_2020.rds"
  )
)


cat(
  "Saved secondary_care_2020.rds:",
  format(
    nrow(secondary_care_2020),
    big.mark = ","
  ),
  "rows\n"
)


rm(secondary_raw)

gc()


# =====================================================================
# CLEAN SECONDARY DIAGNOSES
# =====================================================================

cat("\n")
cat("============================================================\n")
cat("CLEANING 2020 SECONDARY DIAGNOSES\n")
cat("============================================================\n")


secondary_dx_raw <- read_dta(
  secondary_dx_file
)


secondary_diagnosis_2020 <- secondary_dx_raw %>%
  
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
  secondary_diagnosis_2020,
  file.path(
    clean_folder,
    "secondary_diagnosis_2020.rds"
  )
)


cat(
  "Saved secondary_diagnosis_2020.rds:",
  format(
    nrow(secondary_diagnosis_2020),
    big.mark = ","
  ),
  "rows\n"
)


rm(secondary_dx_raw)

gc()


# =====================================================================
# AUDIT 1 - CLEANING SUMMARY
# =====================================================================

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
    nrow(participant_backbone_2020),
    nrow(primary_care_capitated_2020),
    nrow(primary_care_noncapitated_2020),
    nrow(secondary_care_2020),
    nrow(secondary_diagnosis_2020)
  ),
  
  columns = c(
    ncol(participant_backbone_2020),
    ncol(primary_care_capitated_2020),
    ncol(primary_care_noncapitated_2020),
    ncol(secondary_care_2020),
    ncol(secondary_diagnosis_2020)
  ),
  
  unique_patients = c(
    
    n_distinct(
      participant_backbone_2020$patient_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      primary_care_capitated_2020$patient_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      primary_care_noncapitated_2020$patient_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      secondary_care_2020$patient_id,
      na.rm = TRUE
    ),
    
    NA_integer_
  ),
  
  unique_visits = c(
    
    NA_integer_,
    
    n_distinct(
      primary_care_capitated_2020$visit_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      primary_care_noncapitated_2020$visit_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      secondary_care_2020$visit_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      secondary_diagnosis_2020$visit_id,
      na.rm = TRUE
    )
  )
  
)


write_csv(
  cleaning_summary,
  file.path(
    audit_folder,
    "2020_cleaning_summary.csv"
  )
)


# =====================================================================
# AUDIT 2 - IDENTIFIER DUPLICATION
# =====================================================================

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
    nrow(participant_backbone_2020),
    nrow(primary_care_capitated_2020),
    nrow(primary_care_noncapitated_2020),
    nrow(secondary_care_2020),
    nrow(secondary_diagnosis_2020)
  ),
  
  unique_ids = c(
    
    n_distinct(
      participant_backbone_2020$patient_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      primary_care_capitated_2020$visit_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      primary_care_noncapitated_2020$visit_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      secondary_care_2020$visit_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      secondary_diagnosis_2020$
        secondary_diagnosis_record_id,
      na.rm = TRUE
    )
  ),
  
  duplicate_occurrences = c(
    
    duplicate_id_count(
      participant_backbone_2020$patient_id
    ),
    
    duplicate_id_count(
      primary_care_capitated_2020$visit_id
    ),
    
    duplicate_id_count(
      primary_care_noncapitated_2020$visit_id
    ),
    
    duplicate_id_count(
      secondary_care_2020$visit_id
    ),
    
    duplicate_id_count(
      secondary_diagnosis_2020$
        secondary_diagnosis_record_id
    )
  )
  
)


write_csv(
  duplicate_audit,
  file.path(
    audit_folder,
    "2020_duplicate_audit.csv"
  )
)


# =====================================================================
# AUDIT 3 - PATIENT LINKAGE
# =====================================================================

participant_ids <- participant_backbone_2020 %>%
  distinct(
    patient_id
  )


linkage_audit <- bind_rows(
  
  primary_care_capitated_2020 %>%
    
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
  
  primary_care_noncapitated_2020 %>%
    
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
  
  secondary_care_2020 %>%
    
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
    "2020_linkage_audit.csv"
  )
)


# =====================================================================
# AUDIT 4 - SECONDARY DIAGNOSIS LINKAGE AND STRUCTURE
# =====================================================================

secondary_visit_ids <- secondary_care_2020 %>%
  distinct(
    visit_id
  )


secondary_dx_linkage <- secondary_diagnosis_2020 %>%
  
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


secondary_dx_structure <- secondary_diagnosis_2020 %>%
  
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
    "2020_secondary_diagnosis_audit.csv"
  )
)


# =====================================================================
# AUDIT 5 - ENCOUNTER DATES
# =====================================================================

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
        primary_care_capitated_2020$visit_date
      ),
      
      safe_min_date(
        primary_care_noncapitated_2020$visit_date
      ),
      
      safe_min_date(
        secondary_care_2020$admission_date
      )
      
    )
  ),
  
  maximum_date = as.character(
    c(
      
      safe_max_date(
        primary_care_capitated_2020$visit_date
      ),
      
      safe_max_date(
        primary_care_noncapitated_2020$visit_date
      ),
      
      safe_max_date(
        secondary_care_2020$admission_date
      )
      
    )
  ),
  
  records_before_2020 = c(
    
    sum(
      primary_care_capitated_2020$
        visit_date <
        as.Date("2020-01-01"),
      na.rm = TRUE
    ),
    
    sum(
      primary_care_noncapitated_2020$
        visit_date <
        as.Date("2020-01-01"),
      na.rm = TRUE
    ),
    
    sum(
      secondary_care_2020$
        admission_date <
        as.Date("2020-01-01"),
      na.rm = TRUE
    )
    
  ),
  
  records_after_2020 = c(
    
    sum(
      primary_care_capitated_2020$
        visit_date >
        as.Date("2020-12-31"),
      na.rm = TRUE
    ),
    
    sum(
      primary_care_noncapitated_2020$
        visit_date >
        as.Date("2020-12-31"),
      na.rm = TRUE
    ),
    
    sum(
      secondary_care_2020$
        admission_date >
        as.Date("2020-12-31"),
      na.rm = TRUE
    )
    
  ),
  
  missing_date = c(
    
    sum(
      is.na(
        primary_care_capitated_2020$visit_date
      )
    ),
    
    sum(
      is.na(
        primary_care_noncapitated_2020$visit_date
      )
    ),
    
    sum(
      is.na(
        secondary_care_2020$admission_date
      )
    )
    
  )
  
)


write_csv(
  date_audit,
  file.path(
    audit_folder,
    "2020_date_audit.csv"
  )
)


# =====================================================================
# AUDIT 6 - MISSING KEY VARIABLES
# =====================================================================

missing_key_audit <- tibble(
  
  dataset = c(
    "participant_backbone",
    "primary_care_capitated",
    "primary_care_noncapitated",
    "secondary_care",
    "secondary_diagnosis"
  ),
  
  rows = c(
    nrow(participant_backbone_2020),
    nrow(primary_care_capitated_2020),
    nrow(primary_care_noncapitated_2020),
    nrow(secondary_care_2020),
    nrow(secondary_diagnosis_2020)
  ),
  
  missing_patient_id = c(
    
    sum(
      is.na(
        participant_backbone_2020$patient_id
      )
    ),
    
    sum(
      is.na(
        primary_care_capitated_2020$patient_id
      )
    ),
    
    sum(
      is.na(
        primary_care_noncapitated_2020$patient_id
      )
    ),
    
    sum(
      is.na(
        secondary_care_2020$patient_id
      )
    ),
    
    NA_integer_
    
  ),
  
  missing_visit_id = c(
    
    NA_integer_,
    
    sum(
      is.na(
        primary_care_capitated_2020$visit_id
      )
    ),
    
    sum(
      is.na(
        primary_care_noncapitated_2020$visit_id
      )
    ),
    
    sum(
      is.na(
        secondary_care_2020$visit_id
      )
    ),
    
    sum(
      is.na(
        secondary_diagnosis_2020$visit_id
      )
    )
    
  ),
  
  missing_diagnosis_code = c(
    
    NA_integer_,
    
    sum(
      is.na(
        primary_care_capitated_2020$
          diagnosis_code
      )
    ),
    
    sum(
      is.na(
        primary_care_noncapitated_2020$
          diagnosis_code
      )
    ),
    
    sum(
      is.na(
        secondary_care_2020$
          primary_diagnosis_code
      )
    ),
    
    sum(
      is.na(
        secondary_diagnosis_2020$
          secondary_diagnosis_code
      )
    )
    
  )
  
) %>%
  
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
    "2020_missing_key_audit.csv"
  )
)


# =====================================================================
# AUDIT 7 - NON-CAPITATED DEMOGRAPHIC LINKAGE
# =====================================================================

noncap_demographic_linkage <- primary_care_noncapitated_2020 %>%
  
  left_join(
    
    participant_backbone_2020 %>%
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
        !is.na(sex) &
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
    "2020_noncapitated_demographic_linkage.csv"
  )
)


# =====================================================================
# AUDIT 8 - SAVE COMBINED AUDIT OBJECT
# =====================================================================

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
    
    noncapitated_demographic_linkage =
      noncap_demographic_linkage
    
  ),
  
  file.path(
    audit_folder,
    "2020_cleaning_audit_all.rds"
  )
  
)


# =====================================================================
# FINAL SUMMARY
# =====================================================================

cat("\n\n")
cat("============================================================\n")
cat("2020 BPJS CLEANING COMPLETE\n")
cat("============================================================\n\n")


cat("Cleaned RDS files created:\n\n")

cat("1. participant_backbone_2020.rds\n")
cat("2. primary_care_capitated_2020.rds\n")
cat("3. primary_care_noncapitated_2020.rds\n")
cat("4. secondary_care_2020.rds\n")
cat("5. secondary_diagnosis_2020.rds\n\n")


cat("Audit files created:\n\n")

cat("1. 2020_cleaning_summary.csv\n")
cat("2. 2020_duplicate_audit.csv\n")
cat("3. 2020_linkage_audit.csv\n")
cat("4. 2020_date_audit.csv\n")
cat("5. 2020_secondary_diagnosis_audit.csv\n")
cat("6. 2020_missing_key_audit.csv\n")
cat("7. 2020_noncapitated_demographic_linkage.csv\n")
cat("8. 2020_cleaning_audit_all.rds\n\n")


cat("Cleaning summary:\n\n")

print(
  cleaning_summary
)


cat("\nPatient linkage:\n\n")

print(
  linkage_audit
)