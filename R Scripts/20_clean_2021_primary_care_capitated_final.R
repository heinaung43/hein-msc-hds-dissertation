# ============================================================
# BPJS 2021: Capitated Primary Care (FKTP) Data Cleaning
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
  "202102_fktpkapitasi.dta"
)

backbone_file <- "participant_backbone_2021.rds"

output_file <- "fktpkapitasi_2021_clean.rds"

audit_file <- "2021_fktpkapitasi_final_cleaning_audit.csv"

missingness_file <- "2021_fktpkapitasi_missingness.csv"


# ------------------------------------------------------------
# 2. Load raw data
# ------------------------------------------------------------

cat("\nReading 2021 FKTP capitated dataset...\n")

fkp_raw <- read_dta(input_file)

participant_backbone_2021 <- readRDS(
  backbone_file
)

raw_rows <- nrow(fkp_raw)

raw_unique_patients <- n_distinct(
  fkp_raw$PSTV01
)

raw_unique_visits <- n_distinct(
  fkp_raw$FKP02
)

cat(
  "Rows:",
  format(raw_rows, big.mark = ","),
  "\n"
)

cat(
  "Unique patients:",
  format(raw_unique_patients, big.mark = ","),
  "\n"
)

cat(
  "Unique visits:",
  format(raw_unique_visits, big.mark = ","),
  "\n"
)


# ------------------------------------------------------------
# 3. Convert labelled administrative variables
# ------------------------------------------------------------

# Convert administrative variables to character labels
# while preserving the original ICD diagnosis codes.

categorical_vars <- c(
  "FKP05",
  "FKP06",
  "FKP07",
  "FKP08",
  "FKP09",
  "FKP10",
  "FKP11",
  "FKP12",
  "FKP13",
  "FKP16",
  "FKP17",
  "FKP18",
  "FKP19",
  "FKP20",
  "FKP21",
  "FKP22"
)

categorical_vars <- categorical_vars[
  categorical_vars %in% names(fkp_raw)
]

fkp_clean <- fkp_raw %>%
  mutate(
    across(
      all_of(categorical_vars),
      ~ as.character(as_factor(.x))
    )
  )


# ------------------------------------------------------------
# 4. Rename variables
# ------------------------------------------------------------

fkp_clean <- fkp_clean %>%
  rename(
    patient_id = PSTV01,
    family_id = PSTV02,
    sample_weight = PSTV15,
    
    visit_id = FKP02,
    
    visit_start_date = FKP03,
    visit_end_date = FKP04,
    
    facility_province = FKP05,
    facility_district = FKP06,
    facility_ownership = FKP07,
    facility_type = FKP08,
    facility_subtype = FKP09,
    
    service_level = FKP10,
    clinic_type = FKP11,
    
    participant_segment = FKP12,
    discharge_status = FKP13,
    
    diagnosis_3digit_label = FKP14,
    diagnosis_3digit_code = FKP14A,
    
    diagnosis_code = FKP15,
    diagnosis_name = FKP15A,
    
    referral_province = FKP16,
    referral_district = FKP17,
    referral_ownership = FKP18,
    referral_facility_type = FKP19,
    referral_facility_subtype = FKP20,
    referral_clinic = FKP21,
    
    visit_type = FKP22
  )


# ------------------------------------------------------------
# 5. Standardise IDs and data types
# ------------------------------------------------------------

fkp_clean <- fkp_clean %>%
  mutate(
    patient_id = as.character(patient_id),
    family_id = as.character(family_id),
    visit_id = as.character(visit_id),
    
    visit_start_date = as.Date(visit_start_date),
    visit_end_date = as.Date(visit_end_date),
    
    sample_weight = as.numeric(sample_weight),
    
    diagnosis_3digit_label =
      as.character(diagnosis_3digit_label),
    
    diagnosis_3digit_code =
      as.character(diagnosis_3digit_code),
    
    diagnosis_code =
      as.character(diagnosis_code),
    
    diagnosis_name =
      as.character(diagnosis_name)
  )


# ------------------------------------------------------------
# 6. Clean diagnosis codes and descriptions
# ------------------------------------------------------------

fkp_clean <- fkp_clean %>%
  mutate(
    diagnosis_3digit_code =
      trimws(diagnosis_3digit_code),
    
    diagnosis_code =
      trimws(diagnosis_code),
    
    diagnosis_name =
      trimws(diagnosis_name),
    
    diagnosis_3digit_label =
      trimws(diagnosis_3digit_label)
  )


# ------------------------------------------------------------
# 7. Replace empty strings with missing values
# ------------------------------------------------------------

fkp_clean <- fkp_clean %>%
  mutate(
    across(
      where(is.character),
      ~ ifelse(
        trimws(.) == "",
        NA_character_,
        .
      )
    )
  )


# ------------------------------------------------------------
# 8. Add dataset identifiers
# ------------------------------------------------------------

fkp_clean <- fkp_clean %>%
  mutate(
    data_year = 2021L,
    care_setting = "Primary care",
    primary_care_type = "Capitated"
  )


# ------------------------------------------------------------
# 9. Add visit year
# ------------------------------------------------------------

fkp_clean <- fkp_clean %>%
  mutate(
    visit_year =
      as.integer(
        format(
          visit_start_date,
          "%Y"
        )
      )
  )


# ------------------------------------------------------------
# 10. Check visit year
# ------------------------------------------------------------

cat("\n========================================\n")
cat("VISIT YEAR CHECK\n")
cat("========================================\n")

visit_year_summary <- fkp_clean %>%
  count(
    visit_year,
    name = "n"
  ) %>%
  arrange(
    visit_year
  )

print(
  visit_year_summary,
  n = Inf
)


# ------------------------------------------------------------
# 11. Check date consistency
# ------------------------------------------------------------

end_before_start <- sum(
  fkp_clean$visit_end_date <
    fkp_clean$visit_start_date,
  na.rm = TRUE
)

cat(
  "\nVisits where end date is before start date:",
  format(end_before_start, big.mark = ","),
  "\n"
)


# ------------------------------------------------------------
# 12. Check duplicate visit IDs
# ------------------------------------------------------------

duplicate_visit_ids <- fkp_clean %>%
  count(
    visit_id,
    name = "n"
  ) %>%
  filter(
    n > 1
  )

cat(
  "Duplicated visit IDs:",
  format(
    nrow(duplicate_visit_ids),
    big.mark = ","
  ),
  "\n"
)


# ------------------------------------------------------------
# 13. Check exact duplicate rows
# ------------------------------------------------------------

exact_duplicate_rows <-
  nrow(fkp_clean) -
  nrow(distinct(fkp_clean))

cat(
  "Exact duplicate rows:",
  format(
    exact_duplicate_rows,
    big.mark = ","
  ),
  "\n"
)


# ------------------------------------------------------------
# 14. Assess participant linkage
# ------------------------------------------------------------

backbone_ids <- participant_backbone_2021 %>%
  transmute(
    patient_id =
      as.character(patient_id)
  ) %>%
  distinct(
    patient_id
  )

linked_rows <- fkp_clean %>%
  semi_join(
    backbone_ids,
    by = "patient_id"
  )

unlinked_rows <- fkp_clean %>%
  anti_join(
    backbone_ids,
    by = "patient_id"
  )

row_linkage_percent <- round(
  nrow(linked_rows) /
    nrow(fkp_clean) *
    100,
  4
)

cat(
  "\nRows linked to participant backbone:",
  format(
    nrow(linked_rows),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Rows NOT linked:",
  format(
    nrow(unlinked_rows),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Row linkage rate:",
  row_linkage_percent,
  "%\n"
)


# ------------------------------------------------------------
# 15. Join participant demographic and membership data
# ------------------------------------------------------------

# Select participant demographic and membership variables.
# Retain encounter-level facility information.

participant_fields <- participant_backbone_2021 %>%
  select(
    patient_id,
    date_of_birth,
    sex,
    residence_province,
    residence_district,
    sample_year,
    membership_status,
    death_year
  ) %>%
  mutate(
    patient_id =
      as.character(patient_id)
  )


fkp_clean <- fkp_clean %>%
  left_join(
    participant_fields,
    by = "patient_id"
  )


# ------------------------------------------------------------
# 16. Calculate age at encounter
# ------------------------------------------------------------

fkp_clean <- fkp_clean %>%
  mutate(
    age = floor(
      as.numeric(
        visit_start_date -
          date_of_birth
      ) / 365.25
    )
  )


# ------------------------------------------------------------
# 17. Check age values
# ------------------------------------------------------------

cat("\n========================================\n")
cat("AGE CHECK\n")
cat("========================================\n")

cat(
  "Minimum age:",
  min(
    fkp_clean$age,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Maximum age:",
  max(
    fkp_clean$age,
    na.rm = TRUE
  ),
  "\n"
)

negative_age <- sum(
  fkp_clean$age < 0,
  na.rm = TRUE
)

cat(
  "Negative ages:",
  negative_age,
  "\n"
)


# ------------------------------------------------------------
# 18. Derive age groups
# ------------------------------------------------------------

fkp_clean <- fkp_clean %>%
  mutate(
    age_group = case_when(
      is.na(age) ~ NA_character_,
      
      age < 0 ~ NA_character_,
      
      age <= 4 ~ "0-4",
      
      age <= 14 ~ "5-14",
      
      age <= 24 ~ "15-24",
      
      age <= 34 ~ "25-34",
      
      age <= 44 ~ "35-44",
      
      age <= 54 ~ "45-54",
      
      age <= 64 ~ "55-64",
      
      age <= 74 ~ "65-74",
      
      age >= 75 ~ "75+"
    )
  )


# ------------------------------------------------------------
# 19. Missingness summary
# ------------------------------------------------------------

missingness <- tibble(
  variable =
    names(fkp_clean),
  
  missing_n =
    sapply(
      fkp_clean,
      function(x) {
        sum(is.na(x))
      }
    ),
  
  missing_percent =
    round(
      sapply(
        fkp_clean,
        function(x) {
          mean(is.na(x)) * 100
        }
      ),
      2
    )
)


cat("\n========================================\n")
cat("MISSINGNESS SUMMARY\n")
cat("========================================\n")

print(
  missingness,
  n = Inf
)


# ------------------------------------------------------------
# 20. Diagnosis summary
# ------------------------------------------------------------

cat("\n========================================\n")
cat("DIAGNOSIS SUMMARY\n")
cat("========================================\n")

cat(
  "Missing 3-digit diagnosis codes:",
  sum(
    is.na(
      fkp_clean$diagnosis_3digit_code
    )
  ),
  "\n"
)

cat(
  "Missing full diagnosis codes:",
  sum(
    is.na(
      fkp_clean$diagnosis_code
    )
  ),
  "\n"
)

cat(
  "Missing diagnosis names:",
  sum(
    is.na(
      fkp_clean$diagnosis_name
    )
  ),
  "\n"
)

cat(
  "Unique 3-digit diagnosis codes:",
  n_distinct(
    fkp_clean$diagnosis_3digit_code,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Unique full diagnosis codes:",
  n_distinct(
    fkp_clean$diagnosis_code,
    na.rm = TRUE
  ),
  "\n"
)


# ------------------------------------------------------------
# 21. Compile cleaning audit
# ------------------------------------------------------------

audit <- tibble(
  year = 2021,
  
  dataset =
    "fktpkapitasi",
  
  raw_rows =
    raw_rows,
  
  cleaned_rows =
    nrow(fkp_clean),
  
  raw_unique_patients =
    raw_unique_patients,
  
  cleaned_unique_patients =
    n_distinct(
      fkp_clean$patient_id
    ),
  
  raw_unique_visits =
    raw_unique_visits,
  
  cleaned_unique_visits =
    n_distinct(
      fkp_clean$visit_id
    ),
  
  duplicated_visit_ids =
    nrow(
      duplicate_visit_ids
    ),
  
  exact_duplicate_rows =
    exact_duplicate_rows,
  
  unlinked_rows =
    nrow(
      unlinked_rows
    ),
  
  row_linkage_percent =
    row_linkage_percent,
  
  visit_end_before_start =
    end_before_start,
  
  negative_age =
    negative_age,
  
  missing_primary_diagnosis =
    sum(
      is.na(
        fkp_clean$diagnosis_code
      )
    )
)


# ------------------------------------------------------------
# 22. Save cleaned encounter data
# ------------------------------------------------------------

saveRDS(
  fkp_clean,
  output_file
)


# ------------------------------------------------------------
# 23. Save audit files
# ------------------------------------------------------------

write_csv(
  audit,
  audit_file
)

write_csv(
  missingness,
  missingness_file
)

write_csv(
  visit_year_summary,
  "2021_fktpkapitasi_visit_year_summary.csv"
)


# ------------------------------------------------------------
# 24. Export data quality exceptions
# ------------------------------------------------------------

if (nrow(duplicate_visit_ids) > 0) {
  
  write_csv(
    duplicate_visit_ids,
    "2021_fktpkapitasi_duplicate_visit_ids_final.csv"
  )
}

if (nrow(unlinked_rows) > 0) {
  
  write_csv(
    unlinked_rows,
    "2021_fktpkapitasi_unlinked_rows_final.csv"
  )
}

if (end_before_start > 0) {
  
  fkp_clean %>%
    filter(
      visit_end_date <
        visit_start_date
    ) %>%
    write_csv(
      "2021_fktpkapitasi_invalid_date_order.csv"
    )
}


# ------------------------------------------------------------
# 25. Final summary
# ------------------------------------------------------------

cat("\n========================================\n")
cat("2021 FKTP FINAL CLEANING COMPLETE\n")
cat("========================================\n")

cat(
  "\nRaw rows:",
  format(
    raw_rows,
    big.mark = ","
  ),
  "\n"
)

cat(
  "Cleaned rows:",
  format(
    nrow(fkp_clean),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Unique patients:",
  format(
    n_distinct(
      fkp_clean$patient_id
    ),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Unique visits:",
  format(
    n_distinct(
      fkp_clean$visit_id
    ),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Backbone linkage:",
  row_linkage_percent,
  "%\n"
)

cat("\nSaved:\n")

cat(
  " - fktpkapitasi_2021_clean.rds\n"
)

cat(
  " - 2021_fktpkapitasi_final_cleaning_audit.csv\n"
)

cat(
  " - 2021_fktpkapitasi_missingness.csv\n"
)

cat(
  " - 2021_fktpkapitasi_visit_year_summary.csv\n"
)