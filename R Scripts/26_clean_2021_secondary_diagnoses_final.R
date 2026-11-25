# ============================================================
# BPJS 2021: Secondary Diagnosis Data Cleaning
#
# Unit of observation: secondary diagnosis record.
# Records are linked to secondary care encounters by visit ID.
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
  "202105_diagnosissekunder.dta"
)

secondary_care_file <- "secondary_care_2021.rds"

output_file <- "secondary_diagnoses_2021.rds"

audit_file <-
  "2021_secondary_diagnoses_final_cleaning_audit.csv"

missingness_file <-
  "2021_secondary_diagnoses_final_missingness.csv"


# ------------------------------------------------------------
# 3. Load raw data
# ------------------------------------------------------------

cat("\nReading 2021 secondary diagnosis dataset...\n")

secondary_dx_raw <- read_dta(
  input_file
)

secondary_care_2021 <- readRDS(
  secondary_care_file
)


raw_rows <- nrow(
  secondary_dx_raw
)

raw_unique_visits <- n_distinct(
  secondary_dx_raw$FKL02
)


cat(
  "Raw diagnosis rows:",
  format(
    raw_rows,
    big.mark = ","
  ),
  "\n"
)

cat(
  "Unique visits:",
  format(
    raw_unique_visits,
    big.mark = ","
  ),
  "\n"
)


# ------------------------------------------------------------
# 4. Validate expected variables
# ------------------------------------------------------------

expected_vars <- c(
  "FKL02",
  "FKL24",
  "FKL24A",
  "FKL24B"
)

missing_expected_vars <- setdiff(
  expected_vars,
  names(secondary_dx_raw)
)


if (length(missing_expected_vars) > 0) {
  
  stop(
    paste(
      "Expected variables missing:",
      paste(
        missing_expected_vars,
        collapse = ", "
      )
    )
  )
}


# ------------------------------------------------------------
# 5. Rename source variables
# ------------------------------------------------------------

# Source variable definitions:
# FKL02  = Secondary care visit ID
# FKL24  = Detailed secondary diagnosis ICD-10 code
# FKL24A = 3-digit secondary diagnosis ICD-10 code
# FKL24B = Secondary diagnosis name
#
# FKL24 represents a diagnosis code rather than diagnosis position.

secondary_dx_clean <- secondary_dx_raw %>%
  rename(
    
    visit_id =
      FKL02,
    
    secondary_diagnosis_code =
      FKL24,
    
    secondary_diagnosis_3digit_code =
      FKL24A,
    
    secondary_diagnosis_name =
      FKL24B
  )


# ------------------------------------------------------------
# 6. Standardise data types
# ------------------------------------------------------------

secondary_dx_clean <- secondary_dx_clean %>%
  mutate(
    
    visit_id =
      as.character(
        visit_id
      ),
    
    secondary_diagnosis_code =
      as.character(
        secondary_diagnosis_code
      ),
    
    secondary_diagnosis_3digit_code =
      as.character(
        secondary_diagnosis_3digit_code
      ),
    
    secondary_diagnosis_name =
      as.character(
        secondary_diagnosis_name
      )
  )


# ------------------------------------------------------------
# 7. Clean character strings
# ------------------------------------------------------------

secondary_dx_clean <- secondary_dx_clean %>%
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
# 8. Add dataset identifiers
# ------------------------------------------------------------

secondary_dx_clean <- secondary_dx_clean %>%
  mutate(
    
    data_year =
      2021L,
    
    care_setting =
      "Secondary care",
    
    diagnosis_type =
      "Secondary diagnosis"
  )


# ------------------------------------------------------------
# 9. Generate within-visit diagnosis sequence
# ------------------------------------------------------------

# Assign a diagnosis sequence based on the original row order
# within each visit.
# The sequence identifies records and does not indicate
# clinical priority or diagnosis position.

secondary_dx_clean <- secondary_dx_clean %>%
  group_by(
    visit_id
  ) %>%
  mutate(
    secondary_diagnosis_sequence =
      row_number()
  ) %>%
  ungroup()


# ------------------------------------------------------------
# 10. Generate secondary diagnosis record IDs
# ------------------------------------------------------------

secondary_dx_clean <- secondary_dx_clean %>%
  mutate(
    secondary_diagnosis_record_id =
      paste0(
        visit_id,
        "_SDX",
        secondary_diagnosis_sequence
      )
  )


# ------------------------------------------------------------
# 11. Verify record-ID uniqueness
# ------------------------------------------------------------

duplicate_record_ids <- secondary_dx_clean %>%
  count(
    secondary_diagnosis_record_id,
    name = "n"
  ) %>%
  filter(
    n > 1
  )


cat("\n========================================\n")
cat("DIAGNOSIS RECORD ID CHECK\n")
cat("========================================\n")


cat(
  "Duplicate secondary_diagnosis_record_id values:",
  format(
    nrow(
      duplicate_record_ids
    ),
    big.mark = ","
  ),
  "\n"
)


# ------------------------------------------------------------
# 12. Check exact duplicate rows
# ------------------------------------------------------------

exact_duplicate_rows <-
  nrow(
    secondary_dx_clean
  ) -
  nrow(
    distinct(
      secondary_dx_clean
    )
  )


cat(
  "Exact duplicate rows:",
  format(
    exact_duplicate_rows,
    big.mark = ","
  ),
  "\n"
)


# ------------------------------------------------------------
# 13. Extract unique secondary care visit IDs
# ------------------------------------------------------------

secondary_visit_ids <- secondary_care_2021 %>%
  transmute(
    visit_id =
      as.character(
        visit_id
      )
  ) %>%
  filter(
    !is.na(
      visit_id
    ),
    visit_id != ""
  ) %>%
  distinct(
    visit_id
  )


# ------------------------------------------------------------
# 14. Assess secondary diagnosis linkage
# ------------------------------------------------------------

linked_rows <- secondary_dx_clean %>%
  semi_join(
    secondary_visit_ids,
    by = "visit_id"
  )


unlinked_rows <- secondary_dx_clean %>%
  anti_join(
    secondary_visit_ids,
    by = "visit_id"
  )


row_linkage_percent <- round(
  nrow(
    linked_rows
  ) /
    nrow(
      secondary_dx_clean
    ) *
    100,
  4
)


cat("\n========================================\n")
cat("SECONDARY CARE LINKAGE\n")
cat("========================================\n")


cat(
  "Linked diagnosis rows:",
  format(
    nrow(
      linked_rows
    ),
    big.mark = ","
  ),
  "\n"
)


cat(
  "Unlinked diagnosis rows:",
  format(
    nrow(
      unlinked_rows
    ),
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
# 15. Summarise diagnosis records per visit
# ------------------------------------------------------------

diagnoses_per_visit <- secondary_dx_clean %>%
  count(
    visit_id,
    name =
      "secondary_diagnosis_count"
  )


multiple_diagnosis_visits <- diagnoses_per_visit %>%
  filter(
    secondary_diagnosis_count > 1
  )


max_diagnoses_per_visit <- max(
  diagnoses_per_visit$secondary_diagnosis_count,
  na.rm = TRUE
)


cat("\n========================================\n")
cat("SECONDARY DIAGNOSES PER VISIT\n")
cat("========================================\n")


cat(
  "Unique visits represented:",
  format(
    nrow(
      diagnoses_per_visit
    ),
    big.mark = ","
  ),
  "\n"
)


cat(
  "Visits with multiple secondary diagnoses:",
  format(
    nrow(
      multiple_diagnosis_visits
    ),
    big.mark = ","
  ),
  "\n"
)


cat(
  "Maximum secondary diagnoses in one visit:",
  max_diagnoses_per_visit,
  "\n"
)


# ------------------------------------------------------------
# 16. Assess secondary diagnosis coverage
# ------------------------------------------------------------

secondary_visits_with_dx <- secondary_visit_ids %>%
  semi_join(
    diagnoses_per_visit %>%
      select(
        visit_id
      ),
    by = "visit_id"
  )


secondary_visits_without_dx <- secondary_visit_ids %>%
  anti_join(
    diagnoses_per_visit %>%
      select(
        visit_id
      ),
    by = "visit_id"
  )


secondary_dx_coverage_percent <- round(
  nrow(
    secondary_visits_with_dx
  ) /
    nrow(
      secondary_visit_ids
    ) *
    100,
  4
)


cat("\n========================================\n")
cat("SECONDARY DIAGNOSIS COVERAGE\n")
cat("========================================\n")


cat(
  "Total secondary-care visits:",
  format(
    nrow(
      secondary_visit_ids
    ),
    big.mark = ","
  ),
  "\n"
)


cat(
  "Visits with secondary diagnoses:",
  format(
    nrow(
      secondary_visits_with_dx
    ),
    big.mark = ","
  ),
  "\n"
)


cat(
  "Visits without secondary diagnoses:",
  format(
    nrow(
      secondary_visits_without_dx
    ),
    big.mark = ","
  ),
  "\n"
)


cat(
  "Secondary diagnosis coverage:",
  secondary_dx_coverage_percent,
  "%\n"
)


# ------------------------------------------------------------
# 17. Detailed diagnosis-code summary
# ------------------------------------------------------------

cat("\n========================================\n")
cat("DETAILED DIAGNOSIS CODE SUMMARY\n")
cat("========================================\n")


cat(
  "Unique detailed diagnosis codes:",
  format(
    n_distinct(
      secondary_dx_clean$
        secondary_diagnosis_code,
      na.rm = TRUE
    ),
    big.mark = ","
  ),
  "\n"
)


cat(
  "Missing detailed diagnosis codes:",
  format(
    sum(
      is.na(
        secondary_dx_clean$
          secondary_diagnosis_code
      )
    ),
    big.mark = ","
  ),
  "\n"
)


# ------------------------------------------------------------
# 18. 3-digit diagnosis-code summary
# ------------------------------------------------------------

cat("\n========================================\n")
cat("3-DIGIT DIAGNOSIS CODE SUMMARY\n")
cat("========================================\n")


cat(
  "Unique 3-digit diagnosis codes:",
  format(
    n_distinct(
      secondary_dx_clean$
        secondary_diagnosis_3digit_code,
      na.rm = TRUE
    ),
    big.mark = ","
  ),
  "\n"
)


cat(
  "Missing 3-digit diagnosis codes:",
  format(
    sum(
      is.na(
        secondary_dx_clean$
          secondary_diagnosis_3digit_code
      )
    ),
    big.mark = ","
  ),
  "\n"
)


cat(
  "Missing diagnosis names:",
  format(
    sum(
      is.na(
        secondary_dx_clean$
          secondary_diagnosis_name
      )
    ),
    big.mark = ","
  ),
  "\n"
)


# ------------------------------------------------------------
# 19. Check repeated detailed diagnoses within visits
# ------------------------------------------------------------

# Identify repeated combinations of visit ID and detailed
# ICD-10 diagnosis code.

duplicate_visit_detailed_dx <-
  secondary_dx_clean %>%
  count(
    visit_id,
    secondary_diagnosis_code,
    name = "n"
  ) %>%
  filter(
    n > 1
  ) %>%
  arrange(
    desc(n)
  )


cat("\n========================================\n")
cat("REPEATED DETAILED DIAGNOSIS WITHIN VISIT\n")
cat("========================================\n")


cat(
  "Visit + detailed diagnosis combinations repeated:",
  format(
    nrow(
      duplicate_visit_detailed_dx
    ),
    big.mark = ","
  ),
  "\n"
)


if (nrow(duplicate_visit_detailed_dx) > 0) {
  
  cat("\nExamples:\n")
  
  print(
    duplicate_visit_detailed_dx %>%
      slice_head(n = 20),
    n = 20
  )
}


# ------------------------------------------------------------
# 20. Check repeated 3-digit categories within visits
# ------------------------------------------------------------

# Repeated 3-digit categories may represent distinct
# detailed ICD-10 diagnosis codes.

duplicate_visit_3digit_dx <-
  secondary_dx_clean %>%
  count(
    visit_id,
    secondary_diagnosis_3digit_code,
    name = "n"
  ) %>%
  filter(
    n > 1
  ) %>%
  arrange(
    desc(n)
  )


cat("\n========================================\n")
cat("REPEATED 3-DIGIT CATEGORY WITHIN VISIT\n")
cat("========================================\n")


cat(
  "Visit + 3-digit diagnosis combinations repeated:",
  format(
    nrow(
      duplicate_visit_3digit_dx
    ),
    big.mark = ","
  ),
  "\n"
)


# ------------------------------------------------------------
# 21. Check ICD-10 code consistency
# ------------------------------------------------------------

# Extract the first three characters of the detailed ICD-10
# code for comparison with the source 3-digit code.
# Retain the source code without modification in this step.

secondary_dx_clean <- secondary_dx_clean %>%
  mutate(
    derived_3digit_code =
      if_else(
        is.na(
          secondary_diagnosis_code
        ),
        NA_character_,
        substr(
          secondary_diagnosis_code,
          1,
          3
        )
      )
  )


three_digit_mismatch <- secondary_dx_clean %>%
  filter(
    !is.na(
      secondary_diagnosis_3digit_code
    ),
    !is.na(
      derived_3digit_code
    ),
    secondary_diagnosis_3digit_code !=
      derived_3digit_code
  )


cat("\n========================================\n")
cat("3-DIGIT CODE CONSISTENCY CHECK\n")
cat("========================================\n")


cat(
  "Rows where source 3-digit code differs from",
  "first 3 characters of detailed diagnosis code:",
  format(
    nrow(
      three_digit_mismatch
    ),
    big.mark = ","
  ),
  "\n"
)


# ------------------------------------------------------------
# 22. Missingness summary
# ------------------------------------------------------------

missingness <- tibble(
  
  variable =
    names(
      secondary_dx_clean
    ),
  
  missing_n =
    sapply(
      secondary_dx_clean,
      function(x) {
        sum(
          is.na(x)
        )
      }
    ),
  
  missing_percent =
    round(
      sapply(
        secondary_dx_clean,
        function(x) {
          mean(
            is.na(x)
          ) * 100
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
# 23. Rows-per-visit distribution
# ------------------------------------------------------------

rows_per_visit_summary <-
  diagnoses_per_visit %>%
  count(
    secondary_diagnosis_count,
    name =
      "number_of_visits"
  ) %>%
  arrange(
    secondary_diagnosis_count
  )


# ------------------------------------------------------------
# 24. Compile cleaning audit
# ------------------------------------------------------------

audit <- tibble(
  
  year =
    2021,
  
  dataset =
    "secondary_diagnoses",
  
  raw_rows =
    raw_rows,
  
  cleaned_rows =
    nrow(
      secondary_dx_clean
    ),
  
  raw_unique_visits =
    raw_unique_visits,
  
  cleaned_unique_visits =
    n_distinct(
      secondary_dx_clean$visit_id
    ),
  
  unique_diagnosis_records =
    n_distinct(
      secondary_dx_clean$
        secondary_diagnosis_record_id
    ),
  
  duplicate_diagnosis_record_ids =
    nrow(
      duplicate_record_ids
    ),
  
  exact_duplicate_rows =
    exact_duplicate_rows,
  
  visits_with_multiple_secondary_diagnoses =
    nrow(
      multiple_diagnosis_visits
    ),
  
  maximum_secondary_diagnoses_per_visit =
    max_diagnoses_per_visit,
  
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
  
  total_secondary_care_visits =
    nrow(
      secondary_visit_ids
    ),
  
  secondary_care_visits_with_secondary_dx =
    nrow(
      secondary_visits_with_dx
    ),
  
  secondary_care_visits_without_secondary_dx =
    nrow(
      secondary_visits_without_dx
    ),
  
  secondary_dx_coverage_percent =
    secondary_dx_coverage_percent,
  
  repeated_detailed_dx_within_visit =
    nrow(
      duplicate_visit_detailed_dx
    ),
  
  repeated_3digit_dx_within_visit =
    nrow(
      duplicate_visit_3digit_dx
    ),
  
  detailed_vs_3digit_mismatch_rows =
    nrow(
      three_digit_mismatch
    )
)


# ------------------------------------------------------------
# 25. Save cleaned secondary diagnosis data
# ------------------------------------------------------------

saveRDS(
  secondary_dx_clean,
  output_file
)


# ------------------------------------------------------------
# 26. Save audit outputs
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
  rows_per_visit_summary,
  "2021_secondary_diagnoses_rows_per_visit_final.csv"
)


write_csv(
  secondary_visits_without_dx,
  "2021_secondary_care_visits_without_secondary_diagnoses_final.csv"
)


# ------------------------------------------------------------
# 27. Export repeated detailed diagnoses
# ------------------------------------------------------------

if (nrow(duplicate_visit_detailed_dx) > 0) {
  
  write_csv(
    duplicate_visit_detailed_dx,
    "2021_secondary_diagnoses_repeated_detailed_code_within_visit.csv"
  )
}


# ------------------------------------------------------------
# 28. Export repeated 3-digit categories
# ------------------------------------------------------------

if (nrow(duplicate_visit_3digit_dx) > 0) {
  
  write_csv(
    duplicate_visit_3digit_dx,
    "2021_secondary_diagnoses_repeated_3digit_code_within_visit.csv"
  )
}


# ------------------------------------------------------------
# 29. Export unlinked diagnosis records
# ------------------------------------------------------------

if (nrow(unlinked_rows) > 0) {
  
  write_csv(
    unlinked_rows,
    "2021_secondary_diagnoses_unlinked_rows_final.csv"
  )
}


# ------------------------------------------------------------
# 30. Export duplicate record IDs
# ------------------------------------------------------------

if (nrow(duplicate_record_ids) > 0) {
  
  write_csv(
    duplicate_record_ids,
    "2021_secondary_diagnoses_duplicate_record_ids.csv"
  )
}


# ------------------------------------------------------------
# 31. Export ICD-10 code mismatches
# ------------------------------------------------------------

if (nrow(three_digit_mismatch) > 0) {
  
  write_csv(
    three_digit_mismatch,
    "2021_secondary_diagnoses_3digit_mismatch.csv"
  )
}


# ------------------------------------------------------------
# 32. Final summary
# ------------------------------------------------------------

cat("\n========================================\n")
cat("2021 SECONDARY DIAGNOSIS CLEANING COMPLETE\n")
cat("========================================\n")


cat(
  "\nDiagnosis rows:",
  format(
    nrow(
      secondary_dx_clean
    ),
    big.mark = ","
  ),
  "\n"
)


cat(
  "Unique secondary-care visits represented:",
  format(
    n_distinct(
      secondary_dx_clean$visit_id
    ),
    big.mark = ","
  ),
  "\n"
)


cat(
  "Unique secondary diagnosis records:",
  format(
    n_distinct(
      secondary_dx_clean$
        secondary_diagnosis_record_id
    ),
    big.mark = ","
  ),
  "\n"
)


cat(
  "Visits with multiple secondary diagnoses:",
  format(
    nrow(
      multiple_diagnosis_visits
    ),
    big.mark = ","
  ),
  "\n"
)


cat(
  "Maximum secondary diagnoses per visit:",
  max_diagnoses_per_visit,
  "\n"
)


cat(
  "Secondary-care linkage:",
  row_linkage_percent,
  "%\n"
)


cat(
  "Secondary diagnosis coverage:",
  secondary_dx_coverage_percent,
  "%\n"
)


cat(
  "Repeated detailed diagnosis codes within same visit:",
  format(
    nrow(
      duplicate_visit_detailed_dx
    ),
    big.mark = ","
  ),
  "\n"
)


cat(
  "Repeated 3-digit diagnosis categories within same visit:",
  format(
    nrow(
      duplicate_visit_3digit_dx
    ),
    big.mark = ","
  ),
  "\n"
)


cat(
  "Detailed vs source 3-digit mismatches:",
  format(
    nrow(
      three_digit_mismatch
    ),
    big.mark = ","
  ),
  "\n"
)


cat("\nSaved:\n")

cat(
  " - secondary_diagnoses_2021.rds\n"
)

cat(
  " - 2021_secondary_diagnoses_final_cleaning_audit.csv\n"
)

cat(
  " - 2021_secondary_diagnoses_final_missingness.csv\n"
)

cat(
  " - 2021_secondary_diagnoses_rows_per_visit_final.csv\n"
)

cat(
  " - 2021_secondary_care_visits_without_secondary_diagnoses_final.csv\n"
)