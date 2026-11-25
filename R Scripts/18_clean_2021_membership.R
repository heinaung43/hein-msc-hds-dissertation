# ============================================================
# BPJS 2021: Participant Membership Data Cleaning
# ============================================================

library(haven)
library(dplyr)
library(readr)
library(tibble)

# ------------------------------------------------------------
# 1. File paths
# ------------------------------------------------------------

input_file <- file.path(
  "2021",
  "2015202101_kepesertaan.dta"
)

output_file <- "participant_backbone_2021.rds"
audit_file  <- "2021_kepesertaan_cleaning_audit.csv"


# ------------------------------------------------------------
# 2. Read raw membership data
# ------------------------------------------------------------

cat("\nReading 2021 membership dataset...\n")

kep_raw <- read_dta(input_file)

cat("Rows:", nrow(kep_raw), "\n")
cat("Columns:", ncol(kep_raw), "\n")


# ------------------------------------------------------------
# 3. Validate expected variables
# ------------------------------------------------------------

expected_vars <- paste0("PSTV", sprintf("%02d", 1:18))

missing_vars <- setdiff(expected_vars, names(kep_raw))

if (length(missing_vars) > 0) {
  stop(
    paste(
      "Expected variables missing:",
      paste(missing_vars, collapse = ", ")
    )
  )
}

cat("All expected PSTV01-PSTV18 variables found.\n")


# ------------------------------------------------------------
# 4. Record baseline counts for audit
# ------------------------------------------------------------

raw_rows <- nrow(kep_raw)
raw_patients <- n_distinct(kep_raw$PSTV01)


# ------------------------------------------------------------
# 5. Convert labelled categorical variables
# ------------------------------------------------------------

kep_clean <- kep_raw %>%
  mutate(
    PSTV04 = as_factor(PSTV04),
    PSTV05 = as_factor(PSTV05),
    PSTV06 = as_factor(PSTV06),
    PSTV07 = as_factor(PSTV07),
    PSTV08 = as_factor(PSTV08),
    PSTV09 = as_factor(PSTV09),
    PSTV10 = as_factor(PSTV10),
    PSTV11 = as_factor(PSTV11),
    PSTV12 = as_factor(PSTV12),
    PSTV13 = as_factor(PSTV13),
    PSTV14 = as_factor(PSTV14),
    PSTV17 = as_factor(PSTV17)
  )


# ------------------------------------------------------------
# 6. Rename variables
# ------------------------------------------------------------

kep_clean <- kep_clean %>%
  rename(
    patient_id = PSTV01,
    family_id = PSTV02,
    date_of_birth = PSTV03,
    
    family_relationship = PSTV04,
    sex = PSTV05,
    marital_status = PSTV06,
    entitlement_class = PSTV07,
    participant_segment = PSTV08,
    
    residence_province = PSTV09,
    residence_district = PSTV10,
    
    registered_facility_ownership = PSTV11,
    registered_facility_type = PSTV12,
    registered_facility_province = PSTV13,
    registered_facility_district = PSTV14,
    
    sample_weight = PSTV15,
    sample_year = PSTV16,
    membership_status = PSTV17,
    death_year = PSTV18
  )


# ------------------------------------------------------------
# 7. Standardise data types
# ------------------------------------------------------------

kep_clean <- kep_clean %>%
  mutate(
    patient_id = as.character(patient_id),
    family_id = as.character(family_id),
    
    date_of_birth = as.Date(date_of_birth),
    
    sample_year = as.integer(sample_year),
    death_year = as.integer(death_year),
    
    sample_weight = as.numeric(sample_weight),
    
    across(
      c(
        family_relationship,
        sex,
        marital_status,
        entitlement_class,
        participant_segment,
        residence_province,
        residence_district,
        registered_facility_ownership,
        registered_facility_type,
        registered_facility_province,
        registered_facility_district,
        membership_status
      ),
      as.character
    )
  )


# ------------------------------------------------------------
# 8. Standardise sex labels
# ------------------------------------------------------------

cat("\nSex values before recoding:\n")
print(table(kep_clean$sex, useNA = "ifany"))

kep_clean <- kep_clean %>%
  mutate(
    sex = case_when(
      sex %in% c(
        "Laki-laki",
        "Laki-Laki",
        "LAKI-LAKI",
        "Male"
      ) ~ "Male",
      
      sex %in% c(
        "Perempuan",
        "PEREMPUAN",
        "Female"
      ) ~ "Female",
      
      TRUE ~ sex
    )
  )

cat("\nSex values after recoding:\n")
print(table(kep_clean$sex, useNA = "ifany"))


# ------------------------------------------------------------
# 9. Check patient ID uniqueness
# ------------------------------------------------------------

duplicate_patient_ids <- kep_clean %>%
  count(patient_id) %>%
  filter(n > 1)

n_duplicate_patient_ids <- nrow(duplicate_patient_ids)

cat(
  "\nPatient IDs occurring more than once:",
  n_duplicate_patient_ids,
  "\n"
)


# ------------------------------------------------------------
# 10. Check missing patient IDs
# ------------------------------------------------------------

missing_patient_ids <- sum(
  is.na(kep_clean$patient_id) |
    kep_clean$patient_id == ""
)

cat(
  "Missing patient IDs:",
  missing_patient_ids,
  "\n"
)


# ------------------------------------------------------------
# 11. Validate dates of birth
# ------------------------------------------------------------

cat("\nDate of birth range:\n")

print(
  range(
    kep_clean$date_of_birth,
    na.rm = TRUE
  )
)

future_dob <- sum(
  kep_clean$date_of_birth > as.Date("2021-12-31"),
  na.rm = TRUE
)

cat(
  "DOB after 31 Dec 2021:",
  future_dob,
  "\n"
)


# ------------------------------------------------------------
# 12. Check sample year distribution
# ------------------------------------------------------------

cat("\nSample year distribution:\n")

print(
  table(
    kep_clean$sample_year,
    useNA = "ifany"
  )
)


# ------------------------------------------------------------
# 13. Check death year distribution
# ------------------------------------------------------------

cat("\nDeath year distribution:\n")

print(
  table(
    kep_clean$death_year,
    useNA = "ifany"
  )
)


# ------------------------------------------------------------
# 14. Missingness summary
# ------------------------------------------------------------

missingness <- tibble(
  variable = names(kep_clean),
  missing_n = sapply(
    kep_clean,
    function(x) sum(is.na(x))
  ),
  missing_percent = round(
    sapply(
      kep_clean,
      function(x) mean(is.na(x)) * 100
    ),
    2
  )
)

cat("\nMissingness summary:\n")

print(
  missingness,
  n = Inf
)


# ------------------------------------------------------------
# 15. Compile cleaning audit
# ------------------------------------------------------------

audit <- tibble(
  year = 2021,
  
  raw_rows = raw_rows,
  
  raw_unique_patients = raw_patients,
  
  cleaned_rows = nrow(kep_clean),
  
  cleaned_unique_patients =
    n_distinct(kep_clean$patient_id),
  
  duplicate_patient_ids =
    n_duplicate_patient_ids,
  
  missing_patient_ids =
    missing_patient_ids,
  
  future_date_of_birth =
    future_dob,
  
  min_date_of_birth =
    as.character(
      min(
        kep_clean$date_of_birth,
        na.rm = TRUE
      )
    ),
  
  max_date_of_birth =
    as.character(
      max(
        kep_clean$date_of_birth,
        na.rm = TRUE
      )
    )
)


# ------------------------------------------------------------
# 16. Save outputs
# ------------------------------------------------------------

saveRDS(
  kep_clean,
  output_file
)

write_csv(
  audit,
  audit_file
)

write_csv(
  missingness,
  "2021_kepesertaan_missingness.csv"
)


# ------------------------------------------------------------
# 17. Final summary
# ------------------------------------------------------------

cat("\n========================================\n")
cat("2021 MEMBERSHIP CLEANING COMPLETE\n")
cat("========================================\n")

cat(
  "\nRaw rows:",
  format(raw_rows, big.mark = ","),
  "\n"
)

cat(
  "Cleaned rows:",
  format(nrow(kep_clean), big.mark = ","),
  "\n"
)

cat(
  "Unique patients:",
  format(
    n_distinct(kep_clean$patient_id),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Duplicate patient IDs:",
  n_duplicate_patient_ids,
  "\n"
)

cat(
  "\nSaved:\n",
  " - participant_backbone_2021.rds\n",
  " - 2021_kepesertaan_cleaning_audit.csv\n",
  " - 2021_kepesertaan_missingness.csv\n"
)

# ------------------------------------------------------------
# 18. Validate saved output
# ------------------------------------------------------------

participant_backbone_2021 <- readRDS(
  "participant_backbone_2021.rds"
)

participant_backbone_2021 %>%
  count(sample_year) %>%
  arrange(sample_year)

# Check membership status distribution
participant_backbone_2021 %>%
  count(membership_status, sort = TRUE)

# Verify standardised sex categories
participant_backbone_2021 %>%
  count(sex)