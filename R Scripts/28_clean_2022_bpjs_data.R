# ============================================================
# BPJS 2022: Data Cleaning
#
# Clean and standardise the five BPJS data tables:
#   - Participant membership
#   - Capitated primary care (FKTP)
#   - Non-capitated primary care (FKTP)
#   - Secondary care (FKRTL)
#   - Secondary diagnoses
#
# Input directory: 2022/
# Cleaned data directory: 2022_cleaned/
# Audit directory: 2022_cleaning_audit/
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

raw_folder <- "2022"

clean_folder <- "2022_cleaned"

audit_folder <- "2022_cleaning_audit"

if (!dir.exists(clean_folder)) {
  dir.create(clean_folder, recursive = TRUE)
}

if (!dir.exists(audit_folder)) {
  dir.create(audit_folder, recursive = TRUE)
}


# ---------------------------------------------------------------------
# 3. Define source file paths
# ---------------------------------------------------------------------

participant_file <- file.path(
  raw_folder,
  "2015202201_kepesertaan.dta"
)

primary_cap_file <- file.path(
  raw_folder,
  "202202_fktpkapitasi.dta"
)

secondary_file <- file.path(
  raw_folder,
  "202203_fkrtl.dta"
)

primary_noncap_file <- file.path(
  raw_folder,
  "202204_nonkapitasi.dta"
)

secondary_dx_file <- file.path(
  raw_folder,
  "202205_diagnosissekunder.dta"
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
      paste(missing_files, collapse = "\n")
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
    return(str_trim(x))
  }
  
  format(
    x,
    scientific = FALSE,
    trim = TRUE
  )
}


# Clean character fields
clean_character <- function(x) {
  
  x <- as.character(x)
  
  x <- stringr::str_squish(x)
  
  x[x == ""] <- NA_character_
  
  x
}


# Standardise ICD-10 codes by converting to uppercase,
# removing decimal points and recoding specified placeholders
# as missing values.
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
  
  x <- x[!is.na(x)]
  
  sum(duplicated(x))
}


# ---------------------------------------------------------------------
# 6. Clean participant membership data
# ---------------------------------------------------------------------

cat("\n========================================\n")
cat("Cleaning participant backbone\n")
cat("========================================\n")

participant_raw <- read_dta(
  participant_file
)


participant_backbone_2022 <- participant_raw %>%
  
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
    
    source_year = 2022L,
    
    source_dataset = "kepesertaan"
    
  )


saveRDS(
  participant_backbone_2022,
  file.path(
    clean_folder,
    "participant_backbone_2022.rds"
  )
)


cat(
  "Saved participant_backbone_2022.rds:",
  format(
    nrow(participant_backbone_2022),
    big.mark = ","
  ),
  "rows\n"
)


rm(participant_raw)

gc()


# ---------------------------------------------------------------------
# 7. Clean capitated primary care data
# ---------------------------------------------------------------------

cat("\n========================================\n")
cat("Cleaning capitated primary care\n")
cat("========================================\n")

primary_cap_raw <- read_dta(
  primary_cap_file
)


primary_care_capitated_2022 <- primary_cap_raw %>%
  
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
    
    care_setting = "Primary care",
    
    primary_care_type = "Capitated",
    
    source_year = 2022L,
    
    source_dataset = "fktp_kapitasi"
    
  )


saveRDS(
  primary_care_capitated_2022,
  file.path(
    clean_folder,
    "primary_care_capitated_2022.rds"
  )
)


cat(
  "Saved primary_care_capitated_2022.rds:",
  format(
    nrow(primary_care_capitated_2022),
    big.mark = ","
  ),
  "rows\n"
)


rm(primary_cap_raw)

gc()


# ---------------------------------------------------------------------
# 8. Clean non-capitated primary care data
# ---------------------------------------------------------------------

cat("\n========================================\n")
cat("Cleaning non-capitated primary care\n")
cat("========================================\n")

primary_noncap_raw <- read_dta(
  primary_noncap_file
)


primary_care_noncapitated_2022 <- primary_noncap_raw %>%
  
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
      as.numeric(PNK18),
    
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
    
    participant_segment_membership_code =
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
    
    care_setting = "Primary care",
    
    primary_care_type = "Non-capitated",
    
    source_year = 2022L,
    
    source_dataset = "fktp_nonkapitasi"
    
  )


saveRDS(
  primary_care_noncapitated_2022,
  file.path(
    clean_folder,
    "primary_care_noncapitated_2022.rds"
  )
)


cat(
  "Saved primary_care_noncapitated_2022.rds:",
  format(
    nrow(primary_care_noncapitated_2022),
    big.mark = ","
  ),
  "rows\n"
)


rm(primary_noncap_raw)

gc()


# ---------------------------------------------------------------------
# 9. Clean secondary care data
# ---------------------------------------------------------------------

cat("\n========================================\n")
cat("Cleaning secondary care\n")
cat("========================================\n")

secondary_raw <- read_dta(
  secondary_file
)


secondary_care_2022 <- secondary_raw %>%
  
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
    
    visit_date = admission_date,
    
    diagnosis_3digit_code =
      primary_diagnosis_3digit_code,
    
    diagnosis_code =
      primary_diagnosis_code,
    
    diagnosis_name =
      primary_diagnosis_name,
    
    care_setting = "Secondary care",
    
    source_year = 2022L,
    
    source_dataset = "fkrtl"
    
  )


saveRDS(
  secondary_care_2022,
  file.path(
    clean_folder,
    "secondary_care_2022.rds"
  )
)


cat(
  "Saved secondary_care_2022.rds:",
  format(
    nrow(secondary_care_2022),
    big.mark = ","
  ),
  "rows\n"
)


rm(secondary_raw)

gc()


# ---------------------------------------------------------------------
# 10. Clean secondary diagnosis data
# ---------------------------------------------------------------------

cat("\n========================================\n")
cat("Cleaning secondary diagnoses\n")
cat("========================================\n")

secondary_dx_raw <- read_dta(
  secondary_dx_file
)


secondary_diagnosis_2022 <- secondary_dx_raw %>%
  
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
  group_by(visit_id) %>%
  
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
    
    source_year = 2022L,
    
    source_dataset =
      "diagnosis_sekunder"
    
  )


saveRDS(
  secondary_diagnosis_2022,
  file.path(
    clean_folder,
    "secondary_diagnosis_2022.rds"
  )
)


cat(
  "Saved secondary_diagnosis_2022.rds:",
  format(
    nrow(secondary_diagnosis_2022),
    big.mark = ","
  ),
  "rows\n"
)


rm(secondary_dx_raw)

gc()


# =====================================================================
# AUDITS
# =====================================================================


# ---------------------------------------------------------------------
# 11. Compile cleaning summary
# ---------------------------------------------------------------------

cleaning_summary <- tibble(
  
  dataset = c(
    "participant_backbone",
    "primary_care_capitated",
    "primary_care_noncapitated",
    "secondary_care",
    "secondary_diagnosis"
  ),
  
  year = 2022L,
  
  rows = c(
    nrow(participant_backbone_2022),
    nrow(primary_care_capitated_2022),
    nrow(primary_care_noncapitated_2022),
    nrow(secondary_care_2022),
    nrow(secondary_diagnosis_2022)
  ),
  
  columns = c(
    ncol(participant_backbone_2022),
    ncol(primary_care_capitated_2022),
    ncol(primary_care_noncapitated_2022),
    ncol(secondary_care_2022),
    ncol(secondary_diagnosis_2022)
  ),
  
  unique_patients = c(
    n_distinct(
      participant_backbone_2022$patient_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      primary_care_capitated_2022$patient_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      primary_care_noncapitated_2022$patient_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      secondary_care_2022$patient_id,
      na.rm = TRUE
    ),
    
    NA_integer_
  ),
  
  unique_visits = c(
    NA_integer_,
    
    n_distinct(
      primary_care_capitated_2022$visit_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      primary_care_noncapitated_2022$visit_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      secondary_care_2022$visit_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      secondary_diagnosis_2022$visit_id,
      na.rm = TRUE
    )
  )
  
)


write_csv(
  cleaning_summary,
  file.path(
    audit_folder,
    "2022_cleaning_summary.csv"
  )
)


# ---------------------------------------------------------------------
# 12. Assess identifier duplication
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
    nrow(participant_backbone_2022),
    nrow(primary_care_capitated_2022),
    nrow(primary_care_noncapitated_2022),
    nrow(secondary_care_2022),
    nrow(secondary_diagnosis_2022)
  ),
  
  unique_ids = c(
    n_distinct(
      participant_backbone_2022$patient_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      primary_care_capitated_2022$visit_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      primary_care_noncapitated_2022$visit_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      secondary_care_2022$visit_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      secondary_diagnosis_2022$
        secondary_diagnosis_record_id,
      na.rm = TRUE
    )
  ),
  
  duplicate_occurrences = c(
    duplicate_id_count(
      participant_backbone_2022$patient_id
    ),
    
    duplicate_id_count(
      primary_care_capitated_2022$visit_id
    ),
    
    duplicate_id_count(
      primary_care_noncapitated_2022$visit_id
    ),
    
    duplicate_id_count(
      secondary_care_2022$visit_id
    ),
    
    duplicate_id_count(
      secondary_diagnosis_2022$
        secondary_diagnosis_record_id
    )
  )
  
)


write_csv(
  duplicate_audit,
  file.path(
    audit_folder,
    "2022_duplicate_audit.csv"
  )
)


# ---------------------------------------------------------------------
# 13. Assess participant linkage
# ---------------------------------------------------------------------

participant_ids <- participant_backbone_2022 %>%
  distinct(patient_id)


linkage_audit <- bind_rows(
  
  primary_care_capitated_2022 %>%
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
  
  primary_care_noncapitated_2022 %>%
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
  
  secondary_care_2022 %>%
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
          records * 100,
        2
      )
    
  )


write_csv(
  linkage_audit,
  file.path(
    audit_folder,
    "2022_linkage_audit.csv"
  )
)


# ---------------------------------------------------------------------
# 14. Assess secondary diagnosis linkage
# ---------------------------------------------------------------------

secondary_visit_ids <- secondary_care_2022 %>%
  distinct(visit_id)


secondary_dx_linkage <- secondary_diagnosis_2022 %>%
  
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
# 15. Summarise diagnosis records per visit
# ---------------------------------------------------------------------

secondary_dx_structure <- secondary_diagnosis_2022 %>%
  
  group_by(visit_id) %>%
  
  summarise(
    
    number_secondary_diagnoses =
      n(),
    
    .groups = "drop"
    
  ) %>%
  
  summarise(
    
    unique_visits =
      n(),
    
    total_secondary_diagnosis_records =
      sum(number_secondary_diagnoses),
    
    minimum_diagnoses_per_visit =
      min(number_secondary_diagnoses),
    
    median_diagnoses_per_visit =
      median(number_secondary_diagnoses),
    
    mean_diagnoses_per_visit =
      mean(number_secondary_diagnoses),
    
    maximum_diagnoses_per_visit =
      max(number_secondary_diagnoses)
    
  )


secondary_diagnosis_audit <- bind_cols(
  
  secondary_dx_linkage,
  
  secondary_dx_structure
  
)


write_csv(
  secondary_diagnosis_audit,
  file.path(
    audit_folder,
    "2022_secondary_diagnosis_audit.csv"
  )
)


# ---------------------------------------------------------------------
# 16. Assess date distributions
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
      min(
        primary_care_capitated_2022$visit_date,
        na.rm = TRUE
      ),
      
      min(
        primary_care_noncapitated_2022$visit_date,
        na.rm = TRUE
      ),
      
      min(
        secondary_care_2022$admission_date,
        na.rm = TRUE
      )
    )
  ),
  
  maximum_date = as.character(
    c(
      max(
        primary_care_capitated_2022$visit_date,
        na.rm = TRUE
      ),
      
      max(
        primary_care_noncapitated_2022$visit_date,
        na.rm = TRUE
      ),
      
      max(
        secondary_care_2022$admission_date,
        na.rm = TRUE
      )
    )
  ),
  
  records_before_2022 = c(
    
    sum(
      primary_care_capitated_2022$
        visit_date <
        as.Date("2022-01-01"),
      na.rm = TRUE
    ),
    
    sum(
      primary_care_noncapitated_2022$
        visit_date <
        as.Date("2022-01-01"),
      na.rm = TRUE
    ),
    
    sum(
      secondary_care_2022$
        admission_date <
        as.Date("2022-01-01"),
      na.rm = TRUE
    )
  ),
  
  records_after_2022 = c(
    
    sum(
      primary_care_capitated_2022$
        visit_date >
        as.Date("2022-12-31"),
      na.rm = TRUE
    ),
    
    sum(
      primary_care_noncapitated_2022$
        visit_date >
        as.Date("2022-12-31"),
      na.rm = TRUE
    ),
    
    sum(
      secondary_care_2022$
        admission_date >
        as.Date("2022-12-31"),
      na.rm = TRUE
    )
  )
  
)


write_csv(
  date_audit,
  file.path(
    audit_folder,
    "2022_date_audit.csv"
  )
)


# ---------------------------------------------------------------------
# 17. Assess missing key variables
# ---------------------------------------------------------------------

missing_key_audit <- tibble(
  
  dataset = c(
    "participant_backbone",
    "primary_care_capitated",
    "primary_care_noncapitated",
    "secondary_care",
    "secondary_diagnosis"
  ),
  
  missing_patient_id = c(
    
    sum(
      is.na(
        participant_backbone_2022$patient_id
      )
    ),
    
    sum(
      is.na(
        primary_care_capitated_2022$patient_id
      )
    ),
    
    sum(
      is.na(
        primary_care_noncapitated_2022$patient_id
      )
    ),
    
    sum(
      is.na(
        secondary_care_2022$patient_id
      )
    ),
    
    NA_integer_
  ),
  
  missing_visit_id = c(
    
    NA_integer_,
    
    sum(
      is.na(
        primary_care_capitated_2022$visit_id
      )
    ),
    
    sum(
      is.na(
        primary_care_noncapitated_2022$visit_id
      )
    ),
    
    sum(
      is.na(
        secondary_care_2022$visit_id
      )
    ),
    
    sum(
      is.na(
        secondary_diagnosis_2022$visit_id
      )
    )
  ),
  
  missing_diagnosis_code = c(
    
    NA_integer_,
    
    sum(
      is.na(
        primary_care_capitated_2022$
          diagnosis_code
      )
    ),
    
    sum(
      is.na(
        primary_care_noncapitated_2022$
          diagnosis_code
      )
    ),
    
    sum(
      is.na(
        secondary_care_2022$
          primary_diagnosis_code
      )
    ),
    
    sum(
      is.na(
        secondary_diagnosis_2022$
          secondary_diagnosis_code
      )
    )
  )
  
)


write_csv(
  missing_key_audit,
  file.path(
    audit_folder,
    "2022_missing_key_audit.csv"
  )
)


# ---------------------------------------------------------------------
# 18. Save combined audit object
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
      missing_key_audit
    
  ),
  
  file.path(
    audit_folder,
    "2022_cleaning_audit_all.rds"
  )
  
)


# ---------------------------------------------------------------------
# 19. Final summary
# ---------------------------------------------------------------------

cat("\n\n")
cat("============================================================\n")
cat("2022 BPJS CLEANING COMPLETE\n")
cat("============================================================\n\n")

cat("Cleaned RDS files created:\n\n")

cat(
  "1. participant_backbone_2022.rds\n"
)

cat(
  "2. primary_care_capitated_2022.rds\n"
)

cat(
  "3. primary_care_noncapitated_2022.rds\n"
)

cat(
  "4. secondary_care_2022.rds\n"
)

cat(
  "5. secondary_diagnosis_2022.rds\n\n"
)


cat("Audit files created:\n\n")

cat(
  "1. 2022_cleaning_summary.csv\n"
)

cat(
  "2. 2022_duplicate_audit.csv\n"
)

cat(
  "3. 2022_linkage_audit.csv\n"
)

cat(
  "4. 2022_date_audit.csv\n"
)

cat(
  "5. 2022_secondary_diagnosis_audit.csv\n"
)

cat(
  "6. 2022_missing_key_audit.csv\n"
)

cat(
  "7. 2022_cleaning_audit_all.rds\n\n"
)


cat(
  "Cleaned data folder:\n",
  normalizePath(clean_folder),
  "\n\n"
)

cat(
  "Audit folder:\n",
  normalizePath(audit_folder),
  "\n\n"
)

cat("============================================================\n")