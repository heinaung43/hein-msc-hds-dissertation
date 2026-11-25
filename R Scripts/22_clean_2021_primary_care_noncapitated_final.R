# ============================================================
# BPJS 2021: Non-capitated Primary Care (FKTP) Data Cleaning
#
# Unit of observation: service or procedure record.
# Multiple service records may share the same visit ID.
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
  "202104_nonkapitasi.dta"
)

backbone_file <- "participant_backbone_2021.rds"

output_file <- "primary_care_noncapitated_2021.rds"

audit_file <-
  "2021_primary_care_noncapitated_final_cleaning_audit.csv"

missingness_file <-
  "2021_primary_care_noncapitated_missingness.csv"


# ------------------------------------------------------------
# 3. Load raw data
# ------------------------------------------------------------

cat("\nReading 2021 non-capitated primary care dataset...\n")

noncap_raw <- read_dta(input_file)

participant_backbone_2021 <- readRDS(
  backbone_file
)

raw_rows <- nrow(noncap_raw)

raw_unique_patients <- n_distinct(
  noncap_raw$PSTV01
)

raw_unique_visits <- n_distinct(
  noncap_raw$PNK02
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
# 4. Validate expected variables
# ------------------------------------------------------------

expected_vars <- c(
  "PSTV01",
  "PSTV02",
  "PSTV15",
  "PNK02",
  "PNK03",
  "PNK04",
  "PNK05",
  "PNK06",
  "PNK07",
  "PNK08",
  "PNK09",
  "PNK10",
  "PNK11",
  "PNK12",
  "PNK13",
  "PNK13A",
  "PNK14",
  "PNK15",
  "PNK16",
  "PNK17",
  "PNK18"
)

missing_expected_vars <- setdiff(
  expected_vars,
  names(noncap_raw)
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
# 5. Convert labelled categorical variables
# ------------------------------------------------------------

# Convert selected labelled variables to character labels.

categorical_vars <- c(
  "PNK06",
  "PNK07",
  "PNK08",
  "PNK09",
  "PNK10",
  "PNK11",
  "PNK12",
  "PNK14",
  "PNK15"
)

categorical_vars <- categorical_vars[
  categorical_vars %in% names(noncap_raw)
]


noncap_clean <- noncap_raw %>%
  mutate(
    across(
      all_of(categorical_vars),
      ~ as.character(as_factor(.x))
    )
  )


# ------------------------------------------------------------
# 6. Rename variables
# ------------------------------------------------------------

# Retain service code, description and amount at the record level
# to preserve variation within visits.

noncap_clean <- noncap_clean %>%
  rename(
    
    patient_id = PSTV01,
    family_id = PSTV02,
    sample_weight = PSTV15,
    
    visit_id = PNK02,
    
    service_start_date = PNK03,
    service_end_date = PNK04,
    claim_date = PNK05,
    
    facility_province = PNK06,
    facility_district = PNK07,
    facility_ownership = PNK08,
    facility_type = PNK09,
    
    participant_segment = PNK10,
    service_type = PNK11,
    discharge_status = PNK12,
    
    diagnosis_3digit_label = PNK13,
    diagnosis_3digit_code = PNK13A,
    diagnosis_code = PNK14,
    diagnosis_name = PNK15,
    
    service_code = PNK16,
    service_name = PNK17,
    service_amount = PNK18
  )


# ------------------------------------------------------------
# 7. Standardise data types
# ------------------------------------------------------------

noncap_clean <- noncap_clean %>%
  mutate(
    
    patient_id = as.character(patient_id),
    family_id = as.character(family_id),
    visit_id = as.character(visit_id),
    
    service_start_date =
      as.Date(service_start_date),
    
    service_end_date =
      as.Date(service_end_date),
    
    claim_date =
      as.Date(claim_date),
    
    sample_weight =
      as.numeric(sample_weight),
    
    diagnosis_3digit_label =
      as.character(diagnosis_3digit_label),
    
    diagnosis_3digit_code =
      as.character(diagnosis_3digit_code),
    
    diagnosis_code =
      as.character(diagnosis_code),
    
    diagnosis_name =
      as.character(diagnosis_name),
    
    service_code =
      as.character(service_code),
    
    service_name =
      as.character(service_name),
    
    service_amount =
      as.numeric(service_amount)
  )


# ------------------------------------------------------------
# 8. Standardise character strings
# ------------------------------------------------------------

noncap_clean <- noncap_clean %>%
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
# 9. Add dataset identifiers
# ------------------------------------------------------------

noncap_clean <- noncap_clean %>%
  mutate(
    
    data_year = 2021L,
    
    care_setting =
      "Primary care",
    
    primary_care_type =
      "Non-capitated"
  )


# ------------------------------------------------------------
# 10. Derive record year from claim date
# ------------------------------------------------------------

# Derive record year from the claim date (PNK05).
# Service dates may precede the claim year.

noncap_clean <- noncap_clean %>%
  mutate(
    record_year =
      as.integer(
        format(
          claim_date,
          "%Y"
        )
      )
  )


# ------------------------------------------------------------
# 11. Date summaries
# ------------------------------------------------------------

cat("\n========================================\n")
cat("DATE CHECKS\n")
cat("========================================\n")


date_summary <- bind_rows(
  
  noncap_clean %>%
    count(
      year =
        as.integer(
          format(
            service_start_date,
            "%Y"
          )
        )
    ) %>%
    mutate(
      variable = "service_start_date"
    ),
  
  noncap_clean %>%
    count(
      year =
        as.integer(
          format(
            service_end_date,
            "%Y"
          )
        )
    ) %>%
    mutate(
      variable = "service_end_date"
    ),
  
  noncap_clean %>%
    count(
      year =
        as.integer(
          format(
            claim_date,
            "%Y"
          )
        )
    ) %>%
    mutate(
      variable = "claim_date"
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
# 12. Check invalid date order
# ------------------------------------------------------------

end_before_start <- sum(
  noncap_clean$service_end_date <
    noncap_clean$service_start_date,
  na.rm = TRUE
)

cat(
  "\nRows where service end date is before start date:",
  format(
    end_before_start,
    big.mark = ","
  ),
  "\n"
)


# ------------------------------------------------------------
# 13. Check row and visit structure
# ------------------------------------------------------------

cat("\n========================================\n")
cat("VISIT STRUCTURE\n")
cat("========================================\n")


rows_per_visit <- noncap_clean %>%
  count(
    visit_id,
    name = "rows_per_visit"
  )


repeated_visit_ids <- rows_per_visit %>%
  filter(
    rows_per_visit > 1
  )


cat(
  "Total service/procedure rows:",
  format(
    nrow(noncap_clean),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Unique visits:",
  format(
    n_distinct(noncap_clean$visit_id),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Visits with multiple service rows:",
  format(
    nrow(repeated_visit_ids),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Maximum service rows per visit:",
  max(
    rows_per_visit$rows_per_visit,
    na.rm = TRUE
  ),
  "\n"
)


# ------------------------------------------------------------
# 14. Check exact duplicate rows
# ------------------------------------------------------------

exact_duplicate_rows <-
  nrow(noncap_clean) -
  nrow(distinct(noncap_clean))


cat(
  "Exact duplicate rows:",
  format(
    exact_duplicate_rows,
    big.mark = ","
  ),
  "\n"
)


# ------------------------------------------------------------
# 15. Assess participant linkage
# ------------------------------------------------------------

backbone_ids <- participant_backbone_2021 %>%
  transmute(
    patient_id =
      as.character(patient_id)
  ) %>%
  distinct(
    patient_id
  )


unlinked_rows <- noncap_clean %>%
  anti_join(
    backbone_ids,
    by = "patient_id"
  )


linked_rows <- noncap_clean %>%
  semi_join(
    backbone_ids,
    by = "patient_id"
  )


row_linkage_percent <- round(
  nrow(linked_rows) /
    nrow(noncap_clean) *
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
# 16. Join participant demographic and membership data
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


noncap_clean <- noncap_clean %>%
  left_join(
    participant_fields,
    by = "patient_id"
  )


# ------------------------------------------------------------
# 17. Calculate age at service
# ------------------------------------------------------------

# Calculate age at service start, using claim date when missing.

noncap_clean <- noncap_clean %>%
  mutate(
    
    age_reference_date =
      coalesce(
        service_start_date,
        claim_date
      ),
    
    age = floor(
      as.numeric(
        age_reference_date -
          date_of_birth
      ) / 365.25
    )
  )


# ------------------------------------------------------------
# 18. Check age values
# ------------------------------------------------------------

negative_age <- sum(
  noncap_clean$age < 0,
  na.rm = TRUE
)


cat("\n========================================\n")
cat("AGE CHECK\n")
cat("========================================\n")

cat(
  "Minimum age:",
  min(
    noncap_clean$age,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Maximum age:",
  max(
    noncap_clean$age,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Negative age records:",
  negative_age,
  "\n"
)


# ------------------------------------------------------------
# 19. Derive age groups
# ------------------------------------------------------------

noncap_clean <- noncap_clean %>%
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
# 20. Generate within-visit service identifiers
# ------------------------------------------------------------

# Generate service-record identifiers using the visit ID
# and within-visit row number.
# Retain the original visit ID for visit-level analysis.

noncap_clean <- noncap_clean %>%
  group_by(
    visit_id
  ) %>%
  mutate(
    service_row_number =
      row_number()
  ) %>%
  ungroup()


noncap_clean <- noncap_clean %>%
  mutate(
    service_record_id =
      paste0(
        visit_id,
        "_",
        service_row_number
      )
  )


# ------------------------------------------------------------
# 21. Verify service-record ID uniqueness
# ------------------------------------------------------------

duplicate_service_record_ids <-
  noncap_clean %>%
  count(
    service_record_id
  ) %>%
  filter(
    n > 1
  )


cat(
  "\nDuplicate service_record_id values:",
  nrow(
    duplicate_service_record_ids
  ),
  "\n"
)


# ------------------------------------------------------------
# 22. Missingness summary
# ------------------------------------------------------------

missingness <- tibble(
  
  variable =
    names(noncap_clean),
  
  missing_n =
    sapply(
      noncap_clean,
      function(x) {
        sum(is.na(x))
      }
    ),
  
  missing_percent =
    round(
      sapply(
        noncap_clean,
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
# 23. Compile cleaning audit
# ------------------------------------------------------------

audit <- tibble(
  
  year = 2021,
  
  dataset =
    "primary_care_noncapitated",
  
  raw_rows =
    raw_rows,
  
  cleaned_rows =
    nrow(noncap_clean),
  
  raw_unique_patients =
    raw_unique_patients,
  
  cleaned_unique_patients =
    n_distinct(
      noncap_clean$patient_id
    ),
  
  raw_unique_visits =
    raw_unique_visits,
  
  cleaned_unique_visits =
    n_distinct(
      noncap_clean$visit_id
    ),
  
  visits_with_multiple_service_rows =
    nrow(
      repeated_visit_ids
    ),
  
  maximum_rows_per_visit =
    max(
      rows_per_visit$rows_per_visit,
      na.rm = TRUE
    ),
  
  unique_service_records =
    n_distinct(
      noncap_clean$service_record_id
    ),
  
  duplicate_service_record_ids =
    nrow(
      duplicate_service_record_ids
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
  
  service_end_before_start =
    end_before_start,
  
  negative_age =
    negative_age
)


# ------------------------------------------------------------
# 24. Save cleaned service records
# ------------------------------------------------------------

saveRDS(
  noncap_clean,
  output_file
)


# ------------------------------------------------------------
# 25. Save audit outputs
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
  "2021_primary_care_noncapitated_date_summary.csv"
)

write_csv(
  rows_per_visit %>%
    count(
      rows_per_visit,
      name = "number_of_visits"
    ) %>%
    arrange(
      rows_per_visit
    ),
  "2021_primary_care_noncapitated_rows_per_visit_final.csv"
)


# ------------------------------------------------------------
# 26. Export data quality exceptions
# ------------------------------------------------------------

if (nrow(unlinked_rows) > 0) {
  
  write_csv(
    unlinked_rows,
    "2021_primary_care_noncapitated_unlinked_rows_final.csv"
  )
}


if (end_before_start > 0) {
  
  noncap_clean %>%
    filter(
      service_end_date <
        service_start_date
    ) %>%
    write_csv(
      "2021_primary_care_noncapitated_invalid_date_order.csv"
    )
}


if (negative_age > 0) {
  
  noncap_clean %>%
    filter(
      age < 0
    ) %>%
    write_csv(
      "2021_primary_care_noncapitated_negative_age.csv"
    )
}


# ------------------------------------------------------------
# 27. Final summary
# ------------------------------------------------------------

cat("\n========================================\n")
cat("2021 NON-CAPITATED CLEANING COMPLETE\n")
cat("========================================\n")

cat(
  "\nService/procedure rows:",
  format(
    nrow(noncap_clean),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Unique patients:",
  format(
    n_distinct(
      noncap_clean$patient_id
    ),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Unique visits:",
  format(
    n_distinct(
      noncap_clean$visit_id
    ),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Unique service records:",
  format(
    n_distinct(
      noncap_clean$service_record_id
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
  " - primary_care_noncapitated_2021.rds\n"
)

cat(
  " - 2021_primary_care_noncapitated_final_cleaning_audit.csv\n"
)

cat(
  " - 2021_primary_care_noncapitated_missingness.csv\n"
)

cat(
  " - 2021_primary_care_noncapitated_date_summary.csv\n"
)

cat(
  " - 2021_primary_care_noncapitated_rows_per_visit_final.csv\n"
)