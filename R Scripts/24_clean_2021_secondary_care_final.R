# ============================================================
# BPJS 2021: Secondary Care (FKRTL) Data Cleaning
#
# Expected unit of observation: secondary care encounter.
# ============================================================

# ------------------------------------------------------------
# 1. Packages
# ------------------------------------------------------------

library(haven)
library(dplyr)
library(readr)
library(tibble)


# ------------------------------------------------------------
# 2. File paths
# ------------------------------------------------------------

input_file <- file.path(
  "2021",
  "202103_fkrtl.dta"
)

backbone_file <- "participant_backbone_2021.rds"

output_file <- "secondary_care_2021.rds"

audit_file <-
  "2021_secondary_care_final_cleaning_audit.csv"

missingness_file <-
  "2021_secondary_care_missingness.csv"


# ------------------------------------------------------------
# 3. Load raw data
# ------------------------------------------------------------

cat("\nReading 2021 secondary-care dataset...\n")

secondary_raw <- read_dta(input_file)

participant_backbone_2021 <- readRDS(
  backbone_file
)

raw_rows <- nrow(secondary_raw)

raw_unique_patients <- n_distinct(
  secondary_raw$PSTV01
)

raw_unique_visits <- n_distinct(
  secondary_raw$FKL02
)


cat(
  "Raw rows:",
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
# 4. Inspect source variable names
# ------------------------------------------------------------

cat("\n========================================\n")
cat("RAW VARIABLE NAMES\n")
cat("========================================\n")

print(
  names(secondary_raw)
)


# ------------------------------------------------------------
# 5. Validate required variables
# ------------------------------------------------------------

required_vars <- c(
  "PSTV01",
  "PSTV02",
  "PSTV15",
  "FKL02",
  "FKL03",
  "FKL04"
)

missing_required_vars <- setdiff(
  required_vars,
  names(secondary_raw)
)

if (length(missing_required_vars) > 0) {
  
  stop(
    paste(
      "Required variables missing:",
      paste(
        missing_required_vars,
        collapse = ", "
      )
    )
  )
}


# ------------------------------------------------------------
# 6. Inspect labelled variables
# ------------------------------------------------------------

labelled_vars <- names(secondary_raw)[
  sapply(
    secondary_raw,
    inherits,
    what = "haven_labelled"
  )
]

cat("\n========================================\n")
cat("LABELLED VARIABLES\n")
cat("========================================\n")

print(
  labelled_vars
)


# ------------------------------------------------------------
# 7. Convert selected labelled variables
# ------------------------------------------------------------

# Convert labelled variables within the selected FKL fields
# to character labels.

categorical_candidates <- c(
  "FKL05",
  "FKL06",
  "FKL07",
  "FKL08",
  "FKL09",
  "FKL10",
  "FKL11",
  "FKL12",
  "FKL13",
  "FKL14",
  "FKL15",
  "FKL16",
  "FKL17",
  "FKL18",
  "FKL19",
  "FKL20",
  "FKL21",
  "FKL22",
  "FKL23",
  "FKL24",
  "FKL25",
  "FKL26",
  "FKL27"
)

categorical_vars <- intersect(
  categorical_candidates,
  labelled_vars
)


secondary_clean <- secondary_raw %>%
  mutate(
    across(
      all_of(categorical_vars),
      ~ as.character(as_factor(.x))
    )
  )


# ------------------------------------------------------------
# 8. Rename core variables
# ------------------------------------------------------------

# Rename core variables and retain original names for
# remaining FKL variables.

secondary_clean <- secondary_clean %>%
  rename(
    patient_id = PSTV01,
    family_id = PSTV02,
    sample_weight = PSTV15,
    
    visit_id = FKL02,
    
    admission_date = FKL03,
    discharge_date = FKL04
  )


# ------------------------------------------------------------
# 9. Standardise data types
# ------------------------------------------------------------

secondary_clean <- secondary_clean %>%
  mutate(
    patient_id =
      as.character(patient_id),
    
    family_id =
      as.character(family_id),
    
    visit_id =
      as.character(visit_id),
    
    sample_weight =
      as.numeric(sample_weight),
    
    admission_date =
      as.Date(admission_date),
    
    discharge_date =
      as.Date(discharge_date)
  )


# ------------------------------------------------------------
# 10. Clean character fields
# ------------------------------------------------------------

secondary_clean <- secondary_clean %>%
  mutate(
    across(
      where(is.character),
      ~ trimws(.x)
    )
  ) %>%
  mutate(
    across(
      where(is.character),
      ~ ifelse(
        .x == "",
        NA_character_,
        .x
      )
    )
  )


# ------------------------------------------------------------
# 11. Add dataset identifiers
# ------------------------------------------------------------

secondary_clean <- secondary_clean %>%
  mutate(
    data_year = 2021L,
    care_setting = "Secondary care"
  )


# ------------------------------------------------------------
# 12. Derive visit year from discharge date
# ------------------------------------------------------------

# Derive visit year from the discharge date (FKL04).
# The date audit identified discharge dates within 2021.

secondary_clean <- secondary_clean %>%
  mutate(
    visit_year =
      as.integer(
        format(
          discharge_date,
          "%Y"
        )
      )
  )


# ------------------------------------------------------------
# 13. Summarise date distributions
# ------------------------------------------------------------

cat("\n========================================\n")
cat("DATE CHECKS\n")
cat("========================================\n")


date_summary <- bind_rows(
  
  secondary_clean %>%
    count(
      year =
        as.integer(
          format(
            admission_date,
            "%Y"
          )
        )
    ) %>%
    mutate(
      variable = "admission_date"
    ),
  
  secondary_clean %>%
    count(
      year =
        as.integer(
          format(
            discharge_date,
            "%Y"
          )
        )
    ) %>%
    mutate(
      variable = "discharge_date"
    )
  
) %>%
  select(
    variable,
    year,
    n
  ) %>%
  arrange(
    variable,
    year
  )


print(
  date_summary,
  n = Inf
)


# ------------------------------------------------------------
# 14. Check date consistency
# ------------------------------------------------------------

discharge_before_admission <- sum(
  secondary_clean$discharge_date <
    secondary_clean$admission_date,
  na.rm = TRUE
)


cat(
  "\nDischarge before admission:",
  format(
    discharge_before_admission,
    big.mark = ","
  ),
  "\n"
)


# ------------------------------------------------------------
# 15. Calculate length of stay
# ------------------------------------------------------------

secondary_clean <- secondary_clean %>%
  mutate(
    length_of_stay_days =
      as.numeric(
        discharge_date -
          admission_date
      )
  )


negative_length_of_stay <- sum(
  secondary_clean$length_of_stay_days < 0,
  na.rm = TRUE
)


cat(
  "Negative length of stay:",
  negative_length_of_stay,
  "\n"
)


cat(
  "Minimum length of stay:",
  min(
    secondary_clean$length_of_stay_days,
    na.rm = TRUE
  ),
  "\n"
)


cat(
  "Maximum length of stay:",
  max(
    secondary_clean$length_of_stay_days,
    na.rm = TRUE
  ),
  "\n"
)


# ------------------------------------------------------------
# 16. Check duplicate visit IDs
# ------------------------------------------------------------

duplicate_visit_ids <- secondary_clean %>%
  count(
    visit_id,
    name = "n"
  ) %>%
  filter(
    n > 1
  )


cat("\n========================================\n")
cat("VISIT UNIQUENESS\n")
cat("========================================\n")


cat(
  "Duplicate visit IDs:",
  format(
    nrow(duplicate_visit_ids),
    big.mark = ","
  ),
  "\n"
)


# ------------------------------------------------------------
# 17. Check exact duplicate rows
# ------------------------------------------------------------

exact_duplicate_rows <-
  nrow(secondary_clean) -
  nrow(distinct(secondary_clean))


cat(
  "Exact duplicate rows:",
  format(
    exact_duplicate_rows,
    big.mark = ","
  ),
  "\n"
)


# ------------------------------------------------------------
# 18. Assess participant linkage
# ------------------------------------------------------------

backbone_ids <- participant_backbone_2021 %>%
  transmute(
    patient_id =
      as.character(patient_id)
  ) %>%
  distinct(
    patient_id
  )


linked_rows <- secondary_clean %>%
  semi_join(
    backbone_ids,
    by = "patient_id"
  )


unlinked_rows <- secondary_clean %>%
  anti_join(
    backbone_ids,
    by = "patient_id"
  )


row_linkage_percent <- round(
  nrow(linked_rows) /
    nrow(secondary_clean) *
    100,
  4
)


cat("\n========================================\n")
cat("BACKBONE LINKAGE\n")
cat("========================================\n")


cat(
  "Linked rows:",
  format(
    nrow(linked_rows),
    big.mark = ","
  ),
  "\n"
)


cat(
  "Unlinked rows:",
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
# 19. Join participant demographic and membership data
# ------------------------------------------------------------

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


secondary_clean <- secondary_clean %>%
  left_join(
    participant_fields,
    by = "patient_id"
  )


# ------------------------------------------------------------
# 20. Calculate age at admission
# ------------------------------------------------------------

# Retain unadjusted age estimates in age_raw for auditing.

secondary_clean <- secondary_clean %>%
  mutate(
    age_raw = floor(
      as.numeric(
        admission_date -
          date_of_birth
      ) / 365.25
    )
  )


# ------------------------------------------------------------
# 21. Count invalid negative ages
# ------------------------------------------------------------

negative_age <- sum(
  secondary_clean$age_raw < 0,
  na.rm = TRUE
)


cat("\n========================================\n")
cat("AGE CHECK BEFORE CORRECTION\n")
cat("========================================\n")


cat(
  "Minimum raw age:",
  min(
    secondary_clean$age_raw,
    na.rm = TRUE
  ),
  "\n"
)


cat(
  "Maximum raw age:",
  max(
    secondary_clean$age_raw,
    na.rm = TRUE
  ),
  "\n"
)


cat(
  "Negative raw ages:",
  negative_age,
  "\n"
)


# ------------------------------------------------------------
# 22. Derive analytical age
# ------------------------------------------------------------

# Replace negative ages with NA while retaining
# the corresponding encounter records.

secondary_clean <- secondary_clean %>%
  mutate(
    age = if_else(
      age_raw < 0,
      NA_real_,
      as.numeric(age_raw)
    )
  )


cat("\n========================================\n")
cat("AGE CHECK AFTER CORRECTION\n")
cat("========================================\n")


cat(
  "Minimum valid age:",
  min(
    secondary_clean$age,
    na.rm = TRUE
  ),
  "\n"
)


cat(
  "Maximum valid age:",
  max(
    secondary_clean$age,
    na.rm = TRUE
  ),
  "\n"
)


cat(
  "Age values set to NA because age_raw < 0:",
  sum(
    is.na(secondary_clean$age) &
      !is.na(secondary_clean$age_raw) &
      secondary_clean$age_raw < 0
  ),
  "\n"
)


# ------------------------------------------------------------
# 23. Derive age groups
# ------------------------------------------------------------

secondary_clean <- secondary_clean %>%
  mutate(
    age_group = case_when(
      
      is.na(age) ~ NA_character_,
      
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
# 24. Missingness summary
# ------------------------------------------------------------

missingness <- tibble(
  
  variable =
    names(secondary_clean),
  
  missing_n =
    sapply(
      secondary_clean,
      function(x) {
        sum(is.na(x))
      }
    ),
  
  missing_percent =
    round(
      sapply(
        secondary_clean,
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
# 25. Compile cleaning audit
# ------------------------------------------------------------

audit <- tibble(
  
  year = 2021,
  
  dataset =
    "secondary_care",
  
  raw_rows =
    raw_rows,
  
  cleaned_rows =
    nrow(secondary_clean),
  
  raw_unique_patients =
    raw_unique_patients,
  
  cleaned_unique_patients =
    n_distinct(
      secondary_clean$patient_id
    ),
  
  raw_unique_visits =
    raw_unique_visits,
  
  cleaned_unique_visits =
    n_distinct(
      secondary_clean$visit_id
    ),
  
  duplicated_visit_ids =
    nrow(
      duplicate_visit_ids
    ),
  
  exact_duplicate_rows =
    exact_duplicate_rows,
  
  linked_rows =
    nrow(
      linked_rows
    ),
  
  unlinked_rows =
    nrow(
      unlinked_rows
    ),
  
  row_linkage_percent =
    row_linkage_percent,
  
  discharge_before_admission =
    discharge_before_admission,
  
  negative_length_of_stay =
    negative_length_of_stay,
  
  negative_age_raw =
    negative_age,
  
  negative_age_values_retained =
    sum(
      secondary_clean$age < 0,
      na.rm = TRUE
    ),
  
  invalid_age_values_set_to_na =
    sum(
      is.na(secondary_clean$age) &
        !is.na(secondary_clean$age_raw) &
        secondary_clean$age_raw < 0
    )
)


# ------------------------------------------------------------
# 26. Save cleaned encounter data
# ------------------------------------------------------------

saveRDS(
  secondary_clean,
  output_file
)


# ------------------------------------------------------------
# 27. Save audit outputs
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
  date_summary,
  "2021_secondary_care_date_summary.csv"
)


# ------------------------------------------------------------
# 28. Export data quality exceptions
# ------------------------------------------------------------

if (nrow(duplicate_visit_ids) > 0) {
  
  write_csv(
    duplicate_visit_ids,
    "2021_secondary_care_duplicate_visit_ids_final.csv"
  )
}


if (nrow(unlinked_rows) > 0) {
  
  write_csv(
    unlinked_rows,
    "2021_secondary_care_unlinked_rows_final.csv"
  )
}


if (discharge_before_admission > 0) {
  
  secondary_clean %>%
    filter(
      discharge_date <
        admission_date
    ) %>%
    write_csv(
      "2021_secondary_care_invalid_date_order.csv"
    )
}


# ------------------------------------------------------------
# 29. Export invalid age records
# ------------------------------------------------------------

if (negative_age > 0) {
  
  secondary_clean %>%
    filter(
      !is.na(age_raw),
      age_raw < 0
    ) %>%
    write_csv(
      "2021_secondary_care_invalid_age_records.csv"
    )
}


# ------------------------------------------------------------
# 30. Final summary
# ------------------------------------------------------------

cat("\n========================================\n")
cat("2021 SECONDARY CARE CLEANING COMPLETE\n")
cat("========================================\n")


cat(
  "\nRows:",
  format(
    nrow(secondary_clean),
    big.mark = ","
  ),
  "\n"
)


cat(
  "Unique patients:",
  format(
    n_distinct(
      secondary_clean$patient_id
    ),
    big.mark = ","
  ),
  "\n"
)


cat(
  "Unique visits:",
  format(
    n_distinct(
      secondary_clean$visit_id
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


cat(
  "Invalid raw ages detected:",
  negative_age,
  "\n"
)


cat(
  "Negative values remaining in analytical age:",
  sum(
    secondary_clean$age < 0,
    na.rm = TRUE
  ),
  "\n"
)


cat(
  "Invalid age values converted to NA:",
  sum(
    is.na(secondary_clean$age) &
      !is.na(secondary_clean$age_raw) &
      secondary_clean$age_raw < 0
  ),
  "\n"
)


cat("\nSaved:\n")

cat(
  " - secondary_care_2021.rds\n"
)

cat(
  " - 2021_secondary_care_final_cleaning_audit.csv\n"
)

cat(
  " - 2021_secondary_care_missingness.csv\n"
)

cat(
  " - 2021_secondary_care_date_summary.csv\n"
)


if (negative_age > 0) {
  
  cat(
    " - 2021_secondary_care_invalid_age_records.csv\n"
  )
}