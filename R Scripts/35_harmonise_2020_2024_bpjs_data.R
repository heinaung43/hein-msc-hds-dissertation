# ============================================================
# BPJS 2020-2024: Longitudinal Data Harmonisation
#
# Harmonise annual BPJS datasets into five longitudinal
# data layers:
#   - Participant membership
#   - Capitated primary care (FKTP)
#   - Non-capitated primary care (FKTP)
#   - Secondary care (FKRTL)
#   - Secondary diagnoses
#
# Harmonisation procedures:
#   - Standardise variable names and data types.
#   - Reconcile differences in annual dataset structures.
#   - Retain legacy and updated province coding systems.
#   - Distinguish source year from encounter year.
#   - Assess record counts, identifier duplication and linkage.
#
# Input directories: YYYY_cleaned/
# Output directory: harmonised_2020_2024/
# Audit directory: harmonisation_audit_2020_2024/
# ============================================================


# =====================================================================
# 1. LOAD REQUIRED PACKAGES
# =====================================================================

required_packages <- c(
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

library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(stringr)
library(purrr)
library(lubridate)


# =====================================================================
# 2. CONFIGURE YEARS AND DIRECTORIES
# =====================================================================

years <- 2020:2024

output_folder <- "harmonised_2020_2024"

audit_folder <- "harmonisation_audit_2020_2024"

if (!dir.exists(output_folder)) {
  dir.create(
    output_folder,
    recursive = TRUE
  )
}

if (!dir.exists(audit_folder)) {
  dir.create(
    audit_folder,
    recursive = TRUE
  )
}


# =====================================================================
# 3. GENERAL HELPER FUNCTIONS
# =====================================================================


# ---------------------------------------------------------------------
# Clean character values
# ---------------------------------------------------------------------

clean_chr <- function(x) {
  
  if (is.null(x)) {
    return(character(0))
  }
  
  # Convert numeric identifiers and codes without
  # scientific notation.
  if (is.numeric(x)) {
    
    x <- format(
      x,
      scientific = FALSE,
      trim = TRUE
    )
    
  } else {
    
    x <- as.character(x)
  }
  
  x <- stringr::str_trim(x)
  
  x[x == ""] <- NA_character_
  
  x
}


# ---------------------------------------------------------------------
# Standardise province codes to character strings.
# ---------------------------------------------------------------------

province_chr <- function(x) {
  
  clean_chr(x)
}


# ---------------------------------------------------------------------
# Standardise identifiers to character strings.
# ---------------------------------------------------------------------

id_chr <- function(x) {
  
  clean_chr(x)
}


# ---------------------------------------------------------------------
# Convert values to Date format where required.
# ---------------------------------------------------------------------

safe_date <- function(x) {
  
  if (inherits(x, "Date")) {
    return(x)
  }
  
  as.Date(x)
}


# ---------------------------------------------------------------------
# Extract year
# ---------------------------------------------------------------------

date_year <- function(x) {
  
  lubridate::year(
    safe_date(x)
  )
}


# ---------------------------------------------------------------------
# Rename the first available source variable to the target name
# when the target variable does not already exist.
# ---------------------------------------------------------------------

rename_first_available <- function(
    df,
    target,
    candidates
) {
  
  if (target %in% names(df)) {
    return(df)
  }
  
  available <- candidates[
    candidates %in% names(df)
  ]
  
  if (length(available) == 0) {
    return(df)
  }
  
  names(df)[
    names(df) == available[1]
  ] <- target
  
  df
}


# ---------------------------------------------------------------------
# Ensure a character column exists
# ---------------------------------------------------------------------

ensure_chr_column <- function(
    df,
    column_name
) {
  
  if (!column_name %in% names(df)) {
    
    df[[column_name]] <-
      rep(
        NA_character_,
        nrow(df)
      )
    
  } else {
    
    df[[column_name]] <-
      clean_chr(
        df[[column_name]]
      )
  }
  
  df
}


# ---------------------------------------------------------------------
# Convert existing column to numeric
# ---------------------------------------------------------------------

ensure_numeric_column <- function(
    df,
    column_name
) {
  
  if (column_name %in% names(df)) {
    
    df[[column_name]] <-
      suppressWarnings(
        as.numeric(
          df[[column_name]]
        )
      )
  }
  
  df
}


# ---------------------------------------------------------------------
# Convert existing column to Date
# ---------------------------------------------------------------------

ensure_date_column <- function(
    df,
    column_name
) {
  
  if (column_name %in% names(df)) {
    
    df[[column_name]] <-
      safe_date(
        df[[column_name]]
      )
  }
  
  df
}


# =====================================================================
# 4. IDENTIFY CLEANED SOURCE FILES
# =====================================================================


find_clean_file <- function(
    year,
    dataset
) {
  
  folder <- paste0(
    year,
    "_cleaned"
  )
  
  if (!dir.exists(folder)) {
    
    return(
      NA_character_
    )
  }
  
  
  standard_file <- file.path(
    
    folder,
    
    paste0(
      dataset,
      "_",
      year,
      ".rds"
    )
    
  )
  
  
  if (file.exists(standard_file)) {
    
    return(
      standard_file
    )
  }
  
  
  # ---------------------------------------------------------------
  # Accommodate the alternative secondary diagnosis filename
  # used in the 2021 cleaned dataset.
  # ---------------------------------------------------------------
  
  if (dataset == "secondary_diagnosis") {
    
    plural_file <- file.path(
      
      folder,
      
      paste0(
        "secondary_diagnoses_",
        year,
        ".rds"
      )
      
    )
    
    
    if (file.exists(plural_file)) {
      
      return(
        plural_file
      )
    }
  }
  
  
  NA_character_
}


# =====================================================================
# 5. INPUT FILE INVENTORY
# =====================================================================

expected_files <- tidyr::expand_grid(
  
  year = years,
  
  dataset = c(
    "participant_backbone",
    "primary_care_capitated",
    "primary_care_noncapitated",
    "secondary_care",
    "secondary_diagnosis"
  )
  
) %>%
  
  mutate(
    
    file_path =
      map2_chr(
        year,
        dataset,
        find_clean_file
      ),
    
    exists =
      !is.na(file_path) &
      file.exists(file_path),
    
    file_name =
      ifelse(
        exists,
        basename(file_path),
        NA_character_
      ),
    
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


write_csv(
  
  expected_files,
  
  file.path(
    audit_folder,
    "2020_2024_input_file_check.csv"
  )
  
)


if (any(!expected_files$exists)) {
  
  print(
    expected_files %>%
      filter(
        !exists
      )
  )
  
  stop(
    paste0(
      "\nOne or more cleaned input files are missing.\n",
      "See 2020_2024_input_file_check.csv."
    )
  )
}


cat("\n")
cat("============================================================\n")
cat("ALL REQUIRED CLEANED FILES FOUND\n")
cat("============================================================\n\n")

print(
  expected_files %>%
    select(
      year,
      dataset,
      file_name
    )
)


# =====================================================================
# 6. LOAD CLEANED DATA
# =====================================================================


load_cleaned <- function(
    target_year,
    target_dataset
) {
  
  path <- expected_files %>%
    
    filter(
      
      .data$year ==
        .env$target_year,
      
      .data$dataset ==
        .env$target_dataset
      
    ) %>%
    
    pull(
      file_path
    )
  
  
  if (length(path) != 1) {
    
    stop(
      paste0(
        "Could not uniquely identify ",
        target_dataset,
        " for ",
        target_year,
        ". Matches: ",
        length(path)
      )
    )
  }
  
  
  if (is.na(path) || !file.exists(path)) {
    
    stop(
      paste0(
        "Input file not found for ",
        target_dataset,
        " ",
        target_year
      )
    )
  }
  
  
  df <- readRDS(
    path
  )
  
  # Assign the source dataset year independently of
  # the actual encounter date.
  df$source_year <-
    as.integer(
      target_year
    )
  
  
  df
}


cat("\n")
cat("============================================================\n")
cat("LOADING CLEANED DATA\n")
cat("============================================================\n\n")


participant_list <- map(
  
  years,
  
  ~ load_cleaned(
    .x,
    "participant_backbone"
  )
  
)

names(participant_list) <-
  as.character(years)


primary_cap_list <- map(
  
  years,
  
  ~ load_cleaned(
    .x,
    "primary_care_capitated"
  )
  
)

names(primary_cap_list) <-
  as.character(years)


primary_noncap_list <- map(
  
  years,
  
  ~ load_cleaned(
    .x,
    "primary_care_noncapitated"
  )
  
)

names(primary_noncap_list) <-
  as.character(years)


secondary_list <- map(
  
  years,
  
  ~ load_cleaned(
    .x,
    "secondary_care"
  )
  
)

names(secondary_list) <-
  as.character(years)


secondary_dx_list <- map(
  
  years,
  
  ~ load_cleaned(
    .x,
    "secondary_diagnosis"
  )
  
)

names(secondary_dx_list) <-
  as.character(years)


# =====================================================================
# 7. STANDARDISE PARTICIPANT MEMBERSHIP DATA
# =====================================================================

cat(
  "Standardising participant backbone...\n"
)


participant_list <- imap(
  
  participant_list,
  
  function(df, year_name) {
    
    y <- as.integer(
      year_name
    )
    
    
    # ---------------------------------------------------------------
    # Harmonise participant variable names used in the
    # 2021 cleaned dataset.
    # ---------------------------------------------------------------
    
    df <- rename_first_available(
      df,
      "family_relationship_code",
      c("family_relationship")
    )
    
    
    # Identify numeric sex codes stored in the sex variable.
    if (
      !"sex_code" %in% names(df) &&
      "sex" %in% names(df) &&
      is.numeric(df$sex)
    ) {
      
      names(df)[
        names(df) == "sex"
      ] <- "sex_code"
    }
    
    
    df <- rename_first_available(
      df,
      "marital_status_code",
      c("marital_status")
    )
    
    
    df <- rename_first_available(
      df,
      "entitlement_class_code",
      c("entitlement_class")
    )
    
    
    df <- rename_first_available(
      df,
      "participant_segment_code",
      c("participant_segment")
    )
    
    
    df <- rename_first_available(
      df,
      "residence_province_code",
      c("residence_province")
    )
    
    
    df <- rename_first_available(
      df,
      "residence_district_code",
      c("residence_district")
    )
    
    
    df <- rename_first_available(
      df,
      "registered_facility_ownership_code",
      c("registered_facility_ownership")
    )
    
    
    df <- rename_first_available(
      df,
      "registered_facility_type_code",
      c("registered_facility_type")
    )
    
    
    df <- rename_first_available(
      df,
      "registered_facility_province_code",
      c("registered_facility_province")
    )
    
    
    df <- rename_first_available(
      df,
      "registered_facility_district_code",
      c("registered_facility_district")
    )
    
    
    df <- rename_first_available(
      df,
      "membership_status_code",
      c("membership_status")
    )
    
    
    # ---------------------------------------------------------------
    # Required fields
    # ---------------------------------------------------------------
    
    required <- c(
      "patient_id",
      "family_id",
      "residence_province_code",
      "registered_facility_province_code"
    )
    
    
    missing_required <- setdiff(
      required,
      names(df)
    )
    
    
    if (length(missing_required) > 0) {
      
      stop(
        paste0(
          "Participant schema problem in ",
          y,
          ". Missing: ",
          paste(
            missing_required,
            collapse = ", "
          )
        )
      )
    }
    
    
    # ---------------------------------------------------------------
    # IDs
    # ---------------------------------------------------------------
    
    df$patient_id <-
      id_chr(
        df$patient_id
      )
    
    df$family_id <-
      id_chr(
        df$family_id
      )
    
    
    # ---------------------------------------------------------------
    # Standardise province fields to character strings
    # before combining legacy and updated codes.
    # ---------------------------------------------------------------
    
    df$residence_province_code <-
      province_chr(
        df$residence_province_code
      )
    
    df$registered_facility_province_code <-
      province_chr(
        df$registered_facility_province_code
      )
    
    
    # ---------------------------------------------------------------
    # Residence province legacy/new
    # ---------------------------------------------------------------
    
    if (
      !"residence_province_code_legacy" %in%
      names(df)
    ) {
      
      if (y <= 2022) {
        
        df$residence_province_code_legacy <-
          df$residence_province_code
        
      } else {
        
        df$residence_province_code_legacy <-
          rep(
            NA_character_,
            nrow(df)
          )
      }
    }
    
    
    if (
      !"residence_province_code_new" %in%
      names(df)
    ) {
      
      if (y >= 2023) {
        
        df$residence_province_code_new <-
          df$residence_province_code
        
      } else {
        
        df$residence_province_code_new <-
          rep(
            NA_character_,
            nrow(df)
          )
      }
    }
    
    
    df$residence_province_code_legacy <-
      province_chr(
        df$residence_province_code_legacy
      )
    
    
    df$residence_province_code_new <-
      province_chr(
        df$residence_province_code_new
      )
    
    
    df$residence_province_code_preferred <-
      dplyr::coalesce(
        df$residence_province_code_new,
        df$residence_province_code_legacy
      )
    
    
    df$residence_province_code_system <-
      case_when(
        
        !is.na(
          df$residence_province_code_new
        ) ~
          "38-province",
        
        !is.na(
          df$residence_province_code_legacy
        ) ~
          "legacy",
        
        TRUE ~
          NA_character_
        
      )
    
    
    # ---------------------------------------------------------------
    # Registered facility province legacy/new
    # ---------------------------------------------------------------
    
    if (
      !"registered_facility_province_code_legacy" %in%
      names(df)
    ) {
      
      if (y <= 2023) {
        
        df$registered_facility_province_code_legacy <-
          df$registered_facility_province_code
        
      } else {
        
        df$registered_facility_province_code_legacy <-
          rep(
            NA_character_,
            nrow(df)
          )
      }
    }
    
    
    if (
      !"registered_facility_province_code_new" %in%
      names(df)
    ) {
      
      if (y >= 2024) {
        
        df$registered_facility_province_code_new <-
          df$registered_facility_province_code
        
      } else {
        
        df$registered_facility_province_code_new <-
          rep(
            NA_character_,
            nrow(df)
          )
      }
    }
    
    
    df$registered_facility_province_code_legacy <-
      province_chr(
        df$registered_facility_province_code_legacy
      )
    
    
    df$registered_facility_province_code_new <-
      province_chr(
        df$registered_facility_province_code_new
      )
    
    
    df$registered_facility_province_code_preferred <-
      dplyr::coalesce(
        
        df$registered_facility_province_code_new,
        
        df$registered_facility_province_code_legacy
        
      )
    
    
    df$registered_facility_province_code_system <-
      case_when(
        
        !is.na(
          df$registered_facility_province_code_new
        ) ~
          "38-province",
        
        !is.na(
          df$registered_facility_province_code_legacy
        ) ~
          "legacy",
        
        TRUE ~
          NA_character_
        
      )
    
    
    # ---------------------------------------------------------------
    # Sex label
    # ---------------------------------------------------------------
    
    if (!"sex" %in% names(df)) {
      
      if ("sex_code" %in% names(df)) {
        
        sex_temp <-
          suppressWarnings(
            as.numeric(
              df$sex_code
            )
          )
        
        
        df$sex <- case_when(
          
          sex_temp == 1 ~
            "Male",
          
          sex_temp == 2 ~
            "Female",
          
          TRUE ~
            "Unknown"
          
        )
      }
    }
    
    
    df$source_year <-
      y
    
    
    df
  }
  
)


# =====================================================================
# 8. STANDARDISE CAPITATED PRIMARY CARE
# =====================================================================

cat(
  "Standardising capitated primary care...\n"
)


primary_cap_list <- imap(
  
  primary_cap_list,
  
  function(df, year_name) {
    
    y <- as.integer(
      year_name
    )
    
    
    # ---------------------------------------------------------------
    # Harmonise variable names across annual datasets.
    # ---------------------------------------------------------------
    
    df <- rename_first_available(
      df,
      "visit_date",
      c("visit_start_date")
    )
    
    
    df <- rename_first_available(
      df,
      "discharge_date",
      c("visit_end_date")
    )
    
    
    df <- rename_first_available(
      df,
      "facility_province_code",
      c("facility_province")
    )
    
    
    df <- rename_first_available(
      df,
      "facility_district_code",
      c("facility_district")
    )
    
    
    df <- rename_first_available(
      df,
      "facility_ownership_code",
      c("facility_ownership")
    )
    
    
    df <- rename_first_available(
      df,
      "facility_type_code",
      c("facility_type")
    )
    
    
    df <- rename_first_available(
      df,
      "facility_subtype_code",
      c("facility_subtype")
    )
    
    
    df <- rename_first_available(
      df,
      "service_level_code",
      c("service_level")
    )
    
    
    df <- rename_first_available(
      df,
      "clinic_type_code",
      c("clinic_type")
    )
    
    
    df <- rename_first_available(
      df,
      "participant_segment_code",
      c("participant_segment")
    )
    
    
    df <- rename_first_available(
      df,
      "discharge_status_code",
      c("discharge_status")
    )
    
    
    df <- rename_first_available(
      df,
      "referral_province_code",
      c("referral_province")
    )
    
    
    df <- rename_first_available(
      df,
      "referral_district_code",
      c("referral_district")
    )
    
    
    df <- rename_first_available(
      df,
      "referral_ownership_code",
      c("referral_ownership")
    )
    
    
    df <- rename_first_available(
      df,
      "referral_facility_type_code",
      c("referral_facility_type")
    )
    
    
    df <- rename_first_available(
      df,
      "referral_facility_subtype_code",
      c("referral_facility_subtype")
    )
    
    
    df <- rename_first_available(
      df,
      "referral_clinic_code",
      c("referral_clinic")
    )
    
    
    df <- rename_first_available(
      df,
      "visit_type_code",
      c("visit_type")
    )
    
    
    # ---------------------------------------------------------------
    # Validate required variables
    # ---------------------------------------------------------------
    
    required <- c(
      "patient_id",
      "family_id",
      "visit_id",
      "visit_date",
      "facility_province_code"
    )
    
    
    missing_required <- setdiff(
      required,
      names(df)
    )
    
    
    if (length(missing_required) > 0) {
      
      stop(
        paste0(
          "Capitated schema problem in ",
          y,
          ". Missing: ",
          paste(
            missing_required,
            collapse = ", "
          )
        )
      )
    }
    
    
    # ---------------------------------------------------------------
    # IDs / dates
    # ---------------------------------------------------------------
    
    df$patient_id <-
      id_chr(
        df$patient_id
      )
    
    df$family_id <-
      id_chr(
        df$family_id
      )
    
    df$visit_id <-
      id_chr(
        df$visit_id
      )
    
    df$visit_date <-
      safe_date(
        df$visit_date
      )
    
    
    if ("discharge_date" %in% names(df)) {
      
      df$discharge_date <-
        safe_date(
          df$discharge_date
        )
    }
    
    
    # ---------------------------------------------------------------
    # Facility province
    # ---------------------------------------------------------------
    
    df$facility_province_code <-
      province_chr(
        df$facility_province_code
      )
    
    
    if (
      !"facility_province_code_legacy" %in%
      names(df)
    ) {
      
      if (y <= 2023) {
        
        df$facility_province_code_legacy <-
          df$facility_province_code
        
      } else {
        
        df$facility_province_code_legacy <-
          rep(
            NA_character_,
            nrow(df)
          )
      }
    }
    
    
    if (
      !"facility_province_code_new" %in%
      names(df)
    ) {
      
      if (y >= 2024) {
        
        df$facility_province_code_new <-
          df$facility_province_code
        
      } else {
        
        df$facility_province_code_new <-
          rep(
            NA_character_,
            nrow(df)
          )
      }
    }
    
    
    df$facility_province_code_legacy <-
      province_chr(
        df$facility_province_code_legacy
      )
    
    
    df$facility_province_code_new <-
      province_chr(
        df$facility_province_code_new
      )
    
    
    df$facility_province_code_preferred <-
      dplyr::coalesce(
        df$facility_province_code_new,
        df$facility_province_code_legacy
      )
    
    
    df$facility_province_code_system <-
      case_when(
        
        !is.na(
          df$facility_province_code_new
        ) ~ "38-province",
        
        !is.na(
          df$facility_province_code_legacy
        ) ~ "legacy",
        
        TRUE ~ NA_character_
        
      )
    
    
    # ---------------------------------------------------------------
    # Referral province
    # ---------------------------------------------------------------
    
    if (!"referral_province_code" %in% names(df)) {
      
      df$referral_province_code <-
        rep(
          NA_character_,
          nrow(df)
        )
    }
    
    
    df$referral_province_code <-
      province_chr(
        df$referral_province_code
      )
    
    
    if (
      !"referral_province_code_legacy" %in%
      names(df)
    ) {
      
      if (y <= 2023) {
        
        df$referral_province_code_legacy <-
          df$referral_province_code
        
      } else {
        
        df$referral_province_code_legacy <-
          rep(
            NA_character_,
            nrow(df)
          )
      }
    }
    
    
    if (
      !"referral_province_code_new" %in%
      names(df)
    ) {
      
      if (y >= 2024) {
        
        df$referral_province_code_new <-
          df$referral_province_code
        
      } else {
        
        df$referral_province_code_new <-
          rep(
            NA_character_,
            nrow(df)
          )
      }
    }
    
    
    df$referral_province_code_legacy <-
      province_chr(
        df$referral_province_code_legacy
      )
    
    
    df$referral_province_code_new <-
      province_chr(
        df$referral_province_code_new
      )
    
    
    df$referral_province_code_preferred <-
      dplyr::coalesce(
        df$referral_province_code_new,
        df$referral_province_code_legacy
      )
    
    
    df$referral_province_code_system <-
      case_when(
        
        !is.na(
          df$referral_province_code_new
        ) ~ "38-province",
        
        !is.na(
          df$referral_province_code_legacy
        ) ~ "legacy",
        
        TRUE ~ NA_character_
        
      )
    
    
    df$encounter_year <-
      date_year(
        df$visit_date
      )
    
    
    df$source_year <-
      y
    
    
    df
  }
  
)


# =====================================================================
# 9. STANDARDISE NON-CAPITATED PRIMARY CARE
# =====================================================================

cat(
  "Standardising non-capitated primary care...\n"
)


primary_noncap_list <- imap(
  
  primary_noncap_list,
  
  function(df, year_name) {
    
    y <- as.integer(
      year_name
    )
    
    
    # ---------------------------------------------------------------
    # Harmonise variable names across annual datasets.
    # ---------------------------------------------------------------
    
    df <- rename_first_available(
      df,
      "visit_date",
      c("service_start_date")
    )
    
    
    df <- rename_first_available(
      df,
      "procedure_date",
      c("service_end_date")
    )
    
    
    df <- rename_first_available(
      df,
      "facility_province_code",
      c("facility_province")
    )
    
    
    df <- rename_first_available(
      df,
      "facility_district_code",
      c("facility_district")
    )
    
    
    df <- rename_first_available(
      df,
      "facility_ownership_code",
      c("facility_ownership")
    )
    
    
    df <- rename_first_available(
      df,
      "facility_type_code",
      c("facility_type")
    )
    
    
    df <- rename_first_available(
      df,
      "facility_subtype_code",
      c("facility_subtype")
    )
    
    
    df <- rename_first_available(
      df,
      "service_level_code",
      c("service_level")
    )
    
    
    # ---------------------------------------------------------------
    # Remove redundant participant variables from service records.
    # Retain participant information in the participant backbone.
    # ---------------------------------------------------------------
    
    embedded_demographics <- c(
      
      "date_of_birth",
      "family_relationship_code",
      "sex_code",
      "sex",
      "marital_status_code",
      "entitlement_class_code",
      
      "residence_province_code",
      "residence_district_code",
      
      "registered_facility_ownership_code",
      "registered_facility_type_code",
      "registered_facility_province_code",
      "registered_facility_district_code",
      
      "sample_year",
      "membership_status_code",
      "death_year"
      
    )
    
    
    df <- df %>%
      
      select(
        -any_of(
          embedded_demographics
        )
      )
    
    
    # ---------------------------------------------------------------
    # Validate required variables
    # ---------------------------------------------------------------
    
    required <- c(
      "patient_id",
      "family_id",
      "visit_id",
      "visit_date",
      "facility_province_code"
    )
    
    
    missing_required <- setdiff(
      required,
      names(df)
    )
    
    
    if (length(missing_required) > 0) {
      
      stop(
        paste0(
          "Non-capitated schema problem in ",
          y,
          ". Missing: ",
          paste(
            missing_required,
            collapse = ", "
          )
        )
      )
    }
    
    
    # ---------------------------------------------------------------
    # IDs / dates
    # ---------------------------------------------------------------
    
    df$patient_id <-
      id_chr(
        df$patient_id
      )
    
    df$family_id <-
      id_chr(
        df$family_id
      )
    
    df$visit_id <-
      id_chr(
        df$visit_id
      )
    
    df$visit_date <-
      safe_date(
        df$visit_date
      )
    
    
    if ("procedure_date" %in% names(df)) {
      
      df$procedure_date <-
        safe_date(
          df$procedure_date
        )
    }
    
    
    if ("discharge_date" %in% names(df)) {
      
      df$discharge_date <-
        safe_date(
          df$discharge_date
        )
    }
    
    
    # ---------------------------------------------------------------
    # Province
    # ---------------------------------------------------------------
    
    df$facility_province_code <-
      province_chr(
        df$facility_province_code
      )
    
    
    if (
      !"facility_province_code_legacy" %in%
      names(df)
    ) {
      
      if (y <= 2023) {
        
        df$facility_province_code_legacy <-
          df$facility_province_code
        
      } else {
        
        df$facility_province_code_legacy <-
          rep(
            NA_character_,
            nrow(df)
          )
      }
    }
    
    
    if (
      !"facility_province_code_new" %in%
      names(df)
    ) {
      
      if (y >= 2024) {
        
        df$facility_province_code_new <-
          df$facility_province_code
        
      } else {
        
        df$facility_province_code_new <-
          rep(
            NA_character_,
            nrow(df)
          )
      }
    }
    
    
    df$facility_province_code_legacy <-
      province_chr(
        df$facility_province_code_legacy
      )
    
    
    df$facility_province_code_new <-
      province_chr(
        df$facility_province_code_new
      )
    
    
    df$facility_province_code_preferred <-
      dplyr::coalesce(
        df$facility_province_code_new,
        df$facility_province_code_legacy
      )
    
    
    df$facility_province_code_system <-
      case_when(
        
        !is.na(
          df$facility_province_code_new
        ) ~ "38-province",
        
        !is.na(
          df$facility_province_code_legacy
        ) ~ "legacy",
        
        TRUE ~ NA_character_
        
      )
    
    
    df$encounter_year <-
      date_year(
        df$visit_date
      )
    
    
    df$source_year <-
      y
    
    
    df
  }
  
)


# =====================================================================
# 10. STANDARDISE SECONDARY CARE
# =====================================================================

cat(
  "Standardising secondary care...\n"
)


secondary_list <- imap(
  
  secondary_list,
  
  function(df, year_name) {
    
    y <- as.integer(
      year_name
    )
    
    
    # ---------------------------------------------------------------
    # Harmonise secondary care variables retained under
    # alternative or original BPJS source names.
    # ---------------------------------------------------------------
    
    df <- rename_first_available(
      df,
      "facility_province_code",
      c(
        "facility_province",
        "FKL05"
      )
    )
    
    
    df <- rename_first_available(
      df,
      "facility_district_code",
      c(
        "facility_district",
        "FKL06"
      )
    )
    
    
    df <- rename_first_available(
      df,
      "facility_ownership_code",
      c(
        "facility_ownership",
        "FKL07"
      )
    )
    
    
    df <- rename_first_available(
      df,
      "facility_type_code",
      c(
        "facility_type",
        "FKL08"
      )
    )
    
    
    df <- rename_first_available(
      df,
      "hospital_class_code",
      c(
        "hospital_class",
        "FKL09"
      )
    )
    
    
    df <- rename_first_available(
      df,
      "service_level_code",
      c(
        "service_level",
        "FKL10"
      )
    )
    
    
    df <- rename_first_available(
      df,
      "clinic_type_code",
      c(
        "clinic_type",
        "FKL11"
      )
    )
    
    
    df <- rename_first_available(
      df,
      "participant_segment_code",
      c(
        "participant_segment",
        "FKL12"
      )
    )
    
    
    df <- rename_first_available(
      df,
      "treatment_class_code",
      c(
        "treatment_class",
        "FKL13"
      )
    )
    
    
    df <- rename_first_available(
      df,
      "discharge_status_code",
      c(
        "discharge_status",
        "FKL14"
      )
    )
    
    
    df <- rename_first_available(
      df,
      "admission_diagnosis_3digit_code",
      c("FKL15A")
    )
    
    
    df <- rename_first_available(
      df,
      "admission_diagnosis_code",
      c("FKL16")
    )
    
    
    df <- rename_first_available(
      df,
      "admission_diagnosis_name",
      c("FKL16A")
    )
    
    
    df <- rename_first_available(
      df,
      "primary_diagnosis_3digit_code",
      c("FKL17A")
    )
    
    
    df <- rename_first_available(
      df,
      "primary_diagnosis_code",
      c("FKL18")
    )
    
    
    df <- rename_first_available(
      df,
      "primary_diagnosis_name",
      c("FKL18A")
    )
    
    
    df <- rename_first_available(
      df,
      "referring_facility_province_code",
      c(
        "referring_facility_province",
        "FKL25"
      )
    )
    
    
    df <- rename_first_available(
      df,
      "referring_facility_district_code",
      c(
        "referring_facility_district",
        "FKL26"
      )
    )
    
    
    df <- rename_first_available(
      df,
      "referring_facility_ownership_code",
      c(
        "referring_facility_ownership",
        "FKL27"
      )
    )
    
    
    df <- rename_first_available(
      df,
      "referring_facility_type_code",
      c(
        "referring_facility_type",
        "FKL28"
      )
    )
    
    
    df <- rename_first_available(
      df,
      "referring_facility_subtype_code",
      c(
        "referring_facility_subtype",
        "FKL29"
      )
    )
    
    
    df <- rename_first_available(
      df,
      "billed_cost",
      c("FKL47")
    )
    
    
    df <- rename_first_available(
      df,
      "verified_cost",
      c("FKL48")
    )
    
    
    df <- rename_first_available(
      df,
      "admission_date",
      c(
        "admit_date"
      )
    )
    
    
    # ---------------------------------------------------------------
    # Required
    # ---------------------------------------------------------------
    
    required <- c(
      "patient_id",
      "family_id",
      "visit_id",
      "admission_date",
      "facility_province_code"
    )
    
    
    missing_required <- setdiff(
      required,
      names(df)
    )
    
    
    if (length(missing_required) > 0) {
      
      stop(
        paste0(
          "Secondary-care schema problem in ",
          y,
          ". Missing: ",
          paste(
            missing_required,
            collapse = ", "
          )
        )
      )
    }
    
    
    # ---------------------------------------------------------------
    # IDs / dates
    # ---------------------------------------------------------------
    
    df$patient_id <-
      id_chr(
        df$patient_id
      )
    
    df$family_id <-
      id_chr(
        df$family_id
      )
    
    df$visit_id <-
      id_chr(
        df$visit_id
      )
    
    df$admission_date <-
      safe_date(
        df$admission_date
      )
    
    
    if ("discharge_date" %in% names(df)) {
      
      df$discharge_date <-
        safe_date(
          df$discharge_date
        )
    }
    
    
    if (
      "referring_primary_visit_id" %in%
      names(df)
    ) {
      
      df$referring_primary_visit_id <-
        id_chr(
          df$referring_primary_visit_id
        )
    }
    
    
    # ---------------------------------------------------------------
    # Facility province
    # ---------------------------------------------------------------
    
    df$facility_province_code <-
      province_chr(
        df$facility_province_code
      )
    
    
    if (
      !"facility_province_code_legacy" %in%
      names(df)
    ) {
      
      if (y <= 2023) {
        
        df$facility_province_code_legacy <-
          df$facility_province_code
        
      } else {
        
        df$facility_province_code_legacy <-
          rep(
            NA_character_,
            nrow(df)
          )
      }
    }
    
    
    if (
      !"facility_province_code_new" %in%
      names(df)
    ) {
      
      if (y >= 2024) {
        
        df$facility_province_code_new <-
          df$facility_province_code
        
      } else {
        
        df$facility_province_code_new <-
          rep(
            NA_character_,
            nrow(df)
          )
      }
    }
    
    
    df$facility_province_code_legacy <-
      province_chr(
        df$facility_province_code_legacy
      )
    
    
    df$facility_province_code_new <-
      province_chr(
        df$facility_province_code_new
      )
    
    
    df$facility_province_code_preferred <-
      dplyr::coalesce(
        df$facility_province_code_new,
        df$facility_province_code_legacy
      )
    
    
    df$facility_province_code_system <-
      case_when(
        
        !is.na(
          df$facility_province_code_new
        ) ~ "38-province",
        
        !is.na(
          df$facility_province_code_legacy
        ) ~ "legacy",
        
        TRUE ~ NA_character_
        
      )
    
    
    # ---------------------------------------------------------------
    # Referring province
    # ---------------------------------------------------------------
    
    if (
      !"referring_facility_province_code" %in%
      names(df)
    ) {
      
      df$referring_facility_province_code <-
        rep(
          NA_character_,
          nrow(df)
        )
    }
    
    
    df$referring_facility_province_code <-
      province_chr(
        df$referring_facility_province_code
      )
    
    
    if (
      !"referring_facility_province_code_legacy" %in%
      names(df)
    ) {
      
      if (y <= 2023) {
        
        df$referring_facility_province_code_legacy <-
          df$referring_facility_province_code
        
      } else {
        
        df$referring_facility_province_code_legacy <-
          rep(
            NA_character_,
            nrow(df)
          )
      }
    }
    
    
    if (
      !"referring_facility_province_code_new" %in%
      names(df)
    ) {
      
      if (y >= 2024) {
        
        df$referring_facility_province_code_new <-
          df$referring_facility_province_code
        
      } else {
        
        df$referring_facility_province_code_new <-
          rep(
            NA_character_,
            nrow(df)
          )
      }
    }
    
    
    df$referring_facility_province_code_legacy <-
      province_chr(
        df$referring_facility_province_code_legacy
      )
    
    
    df$referring_facility_province_code_new <-
      province_chr(
        df$referring_facility_province_code_new
      )
    
    
    df$referring_facility_province_code_preferred <-
      dplyr::coalesce(
        
        df$referring_facility_province_code_new,
        
        df$referring_facility_province_code_legacy
        
      )
    
    
    df$referring_facility_province_code_system <-
      case_when(
        
        !is.na(
          df$referring_facility_province_code_new
        ) ~ "38-province",
        
        !is.na(
          df$referring_facility_province_code_legacy
        ) ~ "legacy",
        
        TRUE ~ NA_character_
        
      )
    
    
    # ---------------------------------------------------------------
    # Map primary diagnosis fields to standard diagnosis variables.
    # ---------------------------------------------------------------
    
    if (
      "primary_diagnosis_3digit_code" %in%
      names(df)
    ) {
      
      df$diagnosis_3digit_code <-
        clean_chr(
          df$primary_diagnosis_3digit_code
        )
    }
    
    
    if (
      "primary_diagnosis_code" %in%
      names(df)
    ) {
      
      df$diagnosis_code <-
        clean_chr(
          df$primary_diagnosis_code
        )
    }
    
    
    if (
      "primary_diagnosis_name" %in%
      names(df)
    ) {
      
      df$diagnosis_name <-
        as.character(
          df$primary_diagnosis_name
        )
    }
    
    
    df$visit_date <-
      df$admission_date
    
    
    df$care_setting <-
      "Secondary care"
    
    
    df$encounter_year <-
      date_year(
        df$admission_date
      )
    
    
    df$source_year <-
      y
    
    
    df
  }
  
)


# =====================================================================
# 11. STANDARDISE SECONDARY DIAGNOSES
# =====================================================================

cat(
  "Standardising secondary diagnoses...\n"
)


secondary_dx_list <- imap(
  
  secondary_dx_list,
  
  function(df, year_name) {
    
    y <- as.integer(
      year_name
    )
    
    
    df <- rename_first_available(
      df,
      "visit_id",
      c("FKL02")
    )
    
    
    df <- rename_first_available(
      df,
      "secondary_diagnosis_code",
      c("FKL24")
    )
    
    
    df <- rename_first_available(
      df,
      "secondary_diagnosis_3digit_code",
      c("FKL24A")
    )
    
    
    df <- rename_first_available(
      df,
      "secondary_diagnosis_code_name_3digit",
      c("FKL24B")
    )
    
    
    if (!"visit_id" %in% names(df)) {
      
      stop(
        paste0(
          "Secondary diagnosis visit_id missing in ",
          y
        )
      )
    }
    
    
    df$visit_id <-
      id_chr(
        df$visit_id
      )
    
    
    if (
      "secondary_diagnosis_code" %in%
      names(df)
    ) {
      
      df$secondary_diagnosis_code <-
        clean_chr(
          df$secondary_diagnosis_code
        )
    }
    
    
    if (
      "secondary_diagnosis_3digit_code" %in%
      names(df)
    ) {
      
      df$secondary_diagnosis_3digit_code <-
        clean_chr(
          df$secondary_diagnosis_3digit_code
        )
    }
    
    
    # ---------------------------------------------------------------
    # Generate within-visit diagnosis sequences where absent.
    # Sequence values do not indicate clinical priority.
    # ---------------------------------------------------------------
    
    if (
      !"secondary_diagnosis_sequence" %in%
      names(df)
    ) {
      
      df <- df %>%
        
        group_by(
          visit_id
        ) %>%
        
        mutate(
          
          secondary_diagnosis_sequence =
            row_number()
          
        ) %>%
        
        ungroup()
    }
    
    
    # ---------------------------------------------------------------
    # Generate secondary diagnosis record identifiers where absent.
    # ---------------------------------------------------------------
    
    if (
      !"secondary_diagnosis_record_id" %in%
      names(df)
    ) {
      
      df$secondary_diagnosis_record_id <-
        paste0(
          df$visit_id,
          "_SDX",
          sprintf(
            "%03d",
            df$secondary_diagnosis_sequence
          )
        )
    }
    
    
    df$secondary_diagnosis_record_id <-
      id_chr(
        df$secondary_diagnosis_record_id
      )
    
    
    df$source_year <-
      y
    
    
    df
  }
  
)


# =====================================================================
# 12. STANDARDISE COMMON CODE TYPES
#
# Convert code-related variables to character strings to
# maintain compatible data types across annual datasets.
# =====================================================================


standardise_code_fields <- function(df) {
  
  code_columns <- names(df)[
    stringr::str_detect(
      names(df),
      "_code($|_)"
    )
  ]
  
  
  for (column_name in code_columns) {
    
    df[[column_name]] <-
      clean_chr(
        df[[column_name]]
      )
  }
  
  
  df
}


participant_list <-
  map(
    participant_list,
    standardise_code_fields
  )


primary_cap_list <-
  map(
    primary_cap_list,
    standardise_code_fields
  )


primary_noncap_list <-
  map(
    primary_noncap_list,
    standardise_code_fields
  )


secondary_list <-
  map(
    secondary_list,
    standardise_code_fields
  )


secondary_dx_list <-
  map(
    secondary_dx_list,
    standardise_code_fields
  )


# =====================================================================
# 13. SCHEMA AUDIT BEFORE COMBINATION
# =====================================================================


make_schema_inventory <- function(
    data_list,
    dataset_name
) {
  
  map_dfr(
    
    names(data_list),
    
    function(y) {
      
      df <- data_list[[y]]
      
      
      tibble(
        
        source_year =
          as.integer(y),
        
        dataset =
          dataset_name,
        
        variable_position =
          seq_along(df),
        
        variable_name =
          names(df),
        
        variable_class =
          map_chr(
            
            df,
            
            ~ paste(
              class(.x),
              collapse = "/"
            )
            
          )
        
      )
    }
    
  )
}


schema_audit <- bind_rows(
  
  make_schema_inventory(
    participant_list,
    "participant_backbone"
  ),
  
  make_schema_inventory(
    primary_cap_list,
    "primary_care_capitated"
  ),
  
  make_schema_inventory(
    primary_noncap_list,
    "primary_care_noncapitated"
  ),
  
  make_schema_inventory(
    secondary_list,
    "secondary_care"
  ),
  
  make_schema_inventory(
    secondary_dx_list,
    "secondary_diagnosis"
  )
  
)


write_csv(
  
  schema_audit,
  
  file.path(
    audit_folder,
    "2020_2024_schema_audit.csv"
  )
  
)


# =====================================================================
# 14. COMBINE ANNUAL DATASETS
# =====================================================================

cat("\n")
cat("============================================================\n")
cat("COMBINING 2020-2024 DATA\n")
cat("============================================================\n\n")


participant_backbone_2020_2024 <-
  bind_rows(
    participant_list
  )


primary_care_capitated_2020_2024 <-
  bind_rows(
    primary_cap_list
  )


primary_care_noncapitated_2020_2024 <-
  bind_rows(
    primary_noncap_list
  )


secondary_care_2020_2024 <-
  bind_rows(
    secondary_list
  )


secondary_diagnosis_2020_2024 <-
  bind_rows(
    secondary_dx_list
  )


# =====================================================================
# 15. SAVE HARMONISED DATA
# =====================================================================

cat(
  "Saving harmonised datasets...\n"
)


saveRDS(
  
  participant_backbone_2020_2024,
  
  file.path(
    output_folder,
    "participant_backbone_2020_2024.rds"
  )
  
)


saveRDS(
  
  primary_care_capitated_2020_2024,
  
  file.path(
    output_folder,
    "primary_care_capitated_2020_2024.rds"
  )
  
)


saveRDS(
  
  primary_care_noncapitated_2020_2024,
  
  file.path(
    output_folder,
    "primary_care_noncapitated_2020_2024.rds"
  )
  
)


saveRDS(
  
  secondary_care_2020_2024,
  
  file.path(
    output_folder,
    "secondary_care_2020_2024.rds"
  )
  
)


saveRDS(
  
  secondary_diagnosis_2020_2024,
  
  file.path(
    output_folder,
    "secondary_diagnosis_2020_2024.rds"
  )
  
)


# =====================================================================
# 16. ROW COUNT AUDIT
# =====================================================================


row_count_audit <- bind_rows(
  
  participant_backbone_2020_2024 %>%
    
    count(
      source_year,
      name = "rows"
    ) %>%
    
    mutate(
      dataset =
        "participant_backbone"
    ),
  
  
  primary_care_capitated_2020_2024 %>%
    
    count(
      source_year,
      name = "rows"
    ) %>%
    
    mutate(
      dataset =
        "primary_care_capitated"
    ),
  
  
  primary_care_noncapitated_2020_2024 %>%
    
    count(
      source_year,
      name = "rows"
    ) %>%
    
    mutate(
      dataset =
        "primary_care_noncapitated"
    ),
  
  
  secondary_care_2020_2024 %>%
    
    count(
      source_year,
      name = "rows"
    ) %>%
    
    mutate(
      dataset =
        "secondary_care"
    ),
  
  
  secondary_diagnosis_2020_2024 %>%
    
    count(
      source_year,
      name = "rows"
    ) %>%
    
    mutate(
      dataset =
        "secondary_diagnosis"
    )
  
) %>%
  
  select(
    dataset,
    source_year,
    rows
  ) %>%
  
  arrange(
    dataset,
    source_year
  )


write_csv(
  
  row_count_audit,
  
  file.path(
    audit_folder,
    "2020_2024_row_count_audit.csv"
  )
  
)


# =====================================================================
# 17. LONGITUDINAL PARTICIPANT COVERAGE
# =====================================================================


patient_longitudinal_audit <-
  participant_backbone_2020_2024 %>%
  
  filter(
    !is.na(
      patient_id
    )
  ) %>%
  
  distinct(
    patient_id,
    source_year
  ) %>%
  
  count(
    patient_id,
    name = "years_present"
  ) %>%
  
  count(
    years_present,
    name = "patients"
  ) %>%
  
  arrange(
    years_present
  )


write_csv(
  
  patient_longitudinal_audit,
  
  file.path(
    audit_folder,
    "2020_2024_patient_longitudinal_audit.csv"
  )
  
)


# =====================================================================
# 18. ENCOUNTER YEAR AUDIT
# =====================================================================


encounter_year_audit <- bind_rows(
  
  primary_care_capitated_2020_2024 %>%
    
    count(
      source_year,
      encounter_year,
      name = "records"
    ) %>%
    
    mutate(
      dataset =
        "primary_care_capitated"
    ),
  
  
  primary_care_noncapitated_2020_2024 %>%
    
    count(
      source_year,
      encounter_year,
      name = "records"
    ) %>%
    
    mutate(
      dataset =
        "primary_care_noncapitated"
    ),
  
  
  secondary_care_2020_2024 %>%
    
    count(
      source_year,
      encounter_year,
      name = "records"
    ) %>%
    
    mutate(
      dataset =
        "secondary_care"
    )
  
) %>%
  
  select(
    dataset,
    source_year,
    encounter_year,
    records
  ) %>%
  
  arrange(
    dataset,
    source_year,
    encounter_year
  )


write_csv(
  
  encounter_year_audit,
  
  file.path(
    audit_folder,
    "2020_2024_encounter_year_audit.csv"
  )
  
)


# =====================================================================
# 19. ASSESS CROSS-YEAR VISIT IDENTIFIERS
# =====================================================================


make_visit_audit <- function(
    df,
    dataset_name
) {
  
  source_year_count <- df %>%
    
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
      name =
        "source_year_count"
    )
  
  
  tibble(
    
    dataset =
      dataset_name,
    
    total_rows =
      nrow(df),
    
    unique_visit_ids =
      n_distinct(
        df$visit_id,
        na.rm = TRUE
      ),
    
    visit_ids_in_multiple_source_years =
      sum(
        source_year_count$
          source_year_count > 1
      )
    
  )
}


visit_id_duplicate_audit <- bind_rows(
  
  make_visit_audit(
    primary_care_capitated_2020_2024,
    "primary_care_capitated"
  ),
  
  make_visit_audit(
    primary_care_noncapitated_2020_2024,
    "primary_care_noncapitated"
  ),
  
  make_visit_audit(
    secondary_care_2020_2024,
    "secondary_care"
  )
  
)


write_csv(
  
  visit_id_duplicate_audit,
  
  file.path(
    audit_folder,
    "2020_2024_visit_id_duplicate_audit.csv"
  )
  
)


# =====================================================================
# 20. PARTICIPANT LINKAGE AUDIT
# =====================================================================


all_participant_ids <-
  participant_backbone_2020_2024 %>%
  
  filter(
    !is.na(
      patient_id
    )
  ) %>%
  
  distinct(
    patient_id
  )


participant_linkage_audit <- bind_rows(
  
  primary_care_capitated_2020_2024 %>%
    
    summarise(
      
      dataset =
        "primary_care_capitated",
      
      records =
        n(),
      
      linked =
        sum(
          patient_id %in%
            all_participant_ids$
            patient_id
        ),
      
      not_linked =
        sum(
          !(patient_id %in%
              all_participant_ids$
              patient_id)
        )
      
    ),
  
  
  primary_care_noncapitated_2020_2024 %>%
    
    summarise(
      
      dataset =
        "primary_care_noncapitated",
      
      records =
        n(),
      
      linked =
        sum(
          patient_id %in%
            all_participant_ids$
            patient_id
        ),
      
      not_linked =
        sum(
          !(patient_id %in%
              all_participant_ids$
              patient_id)
        )
      
    ),
  
  
  secondary_care_2020_2024 %>%
    
    summarise(
      
      dataset =
        "secondary_care",
      
      records =
        n(),
      
      linked =
        sum(
          patient_id %in%
            all_participant_ids$
            patient_id
        ),
      
      not_linked =
        sum(
          !(patient_id %in%
              all_participant_ids$
              patient_id)
        )
      
    )
  
) %>%
  
  mutate(
    
    linkage_percent =
      round(
        linked /
          records *
          100,
        4
      )
    
  )


write_csv(
  
  participant_linkage_audit,
  
  file.path(
    audit_folder,
    "2020_2024_participant_linkage_audit.csv"
  )
  
)


# =====================================================================
# 21. SECONDARY DIAGNOSIS LINKAGE
# =====================================================================


all_secondary_visits <-
  secondary_care_2020_2024 %>%
  
  filter(
    !is.na(
      visit_id
    )
  ) %>%
  
  distinct(
    visit_id
  )


secondary_diagnosis_linkage_audit <-
  secondary_diagnosis_2020_2024 %>%
  
  summarise(
    
    records =
      n(),
    
    unique_visits =
      n_distinct(
        visit_id,
        na.rm = TRUE
      ),
    
    linked_records =
      sum(
        visit_id %in%
          all_secondary_visits$
          visit_id
      ),
    
    unlinked_records =
      sum(
        !(visit_id %in%
            all_secondary_visits$
            visit_id)
      )
    
  ) %>%
  
  mutate(
    
    linkage_percent =
      round(
        linked_records /
          records *
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


# =====================================================================
# 22. PROVINCE SYSTEM AUDIT
# =====================================================================


province_system_audit <- bind_rows(
  
  participant_backbone_2020_2024 %>%
    
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
      
    ),
  
  
  primary_care_capitated_2020_2024 %>%
    
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
      
    ),
  
  
  primary_care_noncapitated_2020_2024 %>%
    
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
      
    ),
  
  
  secondary_care_2020_2024 %>%
    
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
  
)


write_csv(
  
  province_system_audit,
  
  file.path(
    audit_folder,
    "2020_2024_province_system_audit.csv"
  )
  
)


# =====================================================================
# 23. FINAL DATASET SUMMARY
# =====================================================================


final_dataset_summary <- tibble(
  
  dataset = c(
    "participant_backbone",
    "primary_care_capitated",
    "primary_care_noncapitated",
    "secondary_care",
    "secondary_diagnosis"
  ),
  
  rows = c(
    
    nrow(
      participant_backbone_2020_2024
    ),
    
    nrow(
      primary_care_capitated_2020_2024
    ),
    
    nrow(
      primary_care_noncapitated_2020_2024
    ),
    
    nrow(
      secondary_care_2020_2024
    ),
    
    nrow(
      secondary_diagnosis_2020_2024
    )
    
  ),
  
  columns = c(
    
    ncol(
      participant_backbone_2020_2024
    ),
    
    ncol(
      primary_care_capitated_2020_2024
    ),
    
    ncol(
      primary_care_noncapitated_2020_2024
    ),
    
    ncol(
      secondary_care_2020_2024
    ),
    
    ncol(
      secondary_diagnosis_2020_2024
    )
    
  ),
  
  unique_patients = c(
    
    n_distinct(
      participant_backbone_2020_2024$
        patient_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      primary_care_capitated_2020_2024$
        patient_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      primary_care_noncapitated_2020_2024$
        patient_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      secondary_care_2020_2024$
        patient_id,
      na.rm = TRUE
    ),
    
    NA_integer_
    
  ),
  
  unique_visits = c(
    
    NA_integer_,
    
    n_distinct(
      primary_care_capitated_2020_2024$
        visit_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      primary_care_noncapitated_2020_2024$
        visit_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      secondary_care_2020_2024$
        visit_id,
      na.rm = TRUE
    ),
    
    n_distinct(
      secondary_diagnosis_2020_2024$
        visit_id,
      na.rm = TRUE
    )
    
  )
  
)


write_csv(
  
  final_dataset_summary,
  
  file.path(
    audit_folder,
    "2020_2024_final_dataset_summary.csv"
  )
  
)


# =====================================================================
# 24. SAVE COMBINED AUDIT OBJECT
# =====================================================================


saveRDS(
  
  list(
    
    input_file_check =
      expected_files,
    
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
# 25. FINAL SUMMARY
# =====================================================================


cat("\n\n")
cat("============================================================\n")
cat("2020-2024 BPJS HARMONISATION COMPLETE\n")
cat("============================================================\n\n")


cat("Harmonised datasets created:\n\n")

cat(
  "1. participant_backbone_2020_2024.rds\n"
)

cat(
  "2. primary_care_capitated_2020_2024.rds\n"
)

cat(
  "3. primary_care_noncapitated_2020_2024.rds\n"
)

cat(
  "4. secondary_care_2020_2024.rds\n"
)

cat(
  "5. secondary_diagnosis_2020_2024.rds\n\n"
)


cat("Final dataset summary:\n\n")

print(
  final_dataset_summary
)


cat("\nParticipant linkage:\n\n")

print(
  participant_linkage_audit
)


cat("\nSecondary diagnosis linkage:\n\n")

print(
  secondary_diagnosis_linkage_audit
)


cat("\nCross-year visit-ID audit:\n\n")

print(
  visit_id_duplicate_audit
)


cat("\nLongitudinal patient coverage:\n\n")

print(
  patient_longitudinal_audit
)


cat("\nProvince coding systems:\n\n")

print(
  province_system_audit
)


cat("\n")
cat("============================================================\n")
cat("============================================================\n\n")

cat(
  "2020_2024_input_file_check.csv\n"
)

cat(
  "2020_2024_schema_audit.csv\n"
)

cat(
  "2020_2024_row_count_audit.csv\n"
)

cat(
  "2020_2024_patient_longitudinal_audit.csv\n"
)

cat(
  "2020_2024_encounter_year_audit.csv\n"
)

cat(
  "2020_2024_visit_id_duplicate_audit.csv\n"
)

cat(
  "2020_2024_participant_linkage_audit.csv\n"
)

cat(
  "2020_2024_secondary_diagnosis_linkage_audit.csv\n"
)

cat(
  "2020_2024_province_system_audit.csv\n"
)

cat(
  "2020_2024_final_dataset_summary.csv\n\n"
)

cat("============================================================\n")