
# **************************************************************************************
#
# 		NAME:		helpers.R
#
#
# 		SUMMARY: 	This script provides helper functions for fetching and 
# 					harmonizing patient-level clinical data from TCGA studies 
# 					via the cBioPortal API. It ensures a consistent schema 
# 					across studies, even when column names differ or fields 
# 					are missing.
#
# 		
#		USER-DEFINED FUNCTIONS:
#
# 		- empty_cases(): returns an always-valid, empty schema.
# 		- pick_col(): selects the first available column from 
#   	candidate names.
# 		- map_vital(): standardizes vital status values.
# 		- cbio_cases_for_study(): main function to pull and 
#   	normalize patient-level clinical data for a given study.
# 		- fmt_pct0(): formats numeric values as whole-number 
#   	percentages.
#
#
#		BASE R FUNCTIONS:
#
# 		- if, else, return, tryCatch
# 		- NA, is.null, is.na, is.finite
# 		- as.character, as.numeric, tolower, trimws
# 		- round
# 		- gsub, grepl, sub
# 		- length, indexing [ ], %in%, all
#
#
#		PACKAGE FUNCTIONS:
#
# 		[tibble]
# 		- tibble::tibble(): creates tibble data frames.
#
# 		[dplyr]
# 		- dplyr::case_when(): vectorized conditional mapping.
# 		- dplyr::coalesce(): first non-missing value.
# 		- dplyr::distinct(): deduplicate rows.
# 		- dplyr::mutate(): add/transform columns.
# 		- dplyr::if_else(): strict typed if/else.
# 		- dplyr::bind_rows(): row-bind with schema alignment.
#
# 		[cBioPortalData]
# 		- cBioPortalData::cBioPortal(): connect to cBioPortal API.
# 		- cBioPortalData::clinicalData(): fetch clinical data.
#
#
#		REQUIRED LIBRARIES:
#
# 		- tibble
# 		- dplyr
# 		- cBioPortalData
#

# **************************************************************************************



DEFAULT_STUDY_ID <- "brca_tcga"  # change to "luad_tcga", "coadread_tcga", etc.

# Always-return schema (prevents missing-column errors)
empty_cases <- function() {
  tibble::tibble(
    case_id = character(),
    project_id = character(),
    primary_site = character(),
    disease_type = character(),
    gender = character(),
    vital_status = character(),
    age_days = numeric(),
    age_years = numeric(),
    tumor_stage = character(),
    primary_diagnosis = character()
  )
}

# Pick the first existing column from a list of candidate names
pick_col <- function(df, candidates, default = NA) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit)) df[[hit[1]]] else default
}

# Map OS_STATUS strings to GDC-like vital_status
map_vital <- function(x) {
  y <- tolower(trimws(as.character(x)))
  dplyr::case_when(
    y %in% c("1:deceased", "deceased", "dead") ~ "dead",
    y %in% c("0:living", "living", "alive")    ~ "alive",
    TRUE ~ NA_character_
  )
}

# Main fetcher: use cBioPortalData to get clinical data for a single TCGA study
cbio_cases_for_study <- function(study_id = DEFAULT_STUDY_ID) {
  # 1) pull clinical wide table (returns one row per sample or patient; study_id like "brca_tcga")
  api <- cBioPortalData::cBioPortal()
  clin <- tryCatch(cBioPortalData::clinicalData(api, study_id),
                   error = function(e) NULL)

  if (is.null(clin) || !is.data.frame(clin) || nrow(clin) == 0) {
    return(empty_cases())
  }

  # 2) prefer patient-level; if sample-level present, collapse to patient
  # Common id columns across studies:
  patient_id <- pick_col(clin, c("patientId", "PATIENT_ID", "PATIENT_IDEAL"))
  if (is.na(patient_id)[1]) {
    # no patient column found; fall back to sampleId then strip to patient prefix "TCGA-XX-XXXX"
    sample_id <- pick_col(clin, c("sampleId", "SAMPLE_ID", "SAMPLE_IDEAL"))
    if (all(is.na(sample_id))) return(empty_cases())
    pid <- sub("^([A-Za-z0-9]+-[A-Za-z0-9]+-[A-Za-z0-9]+).*$", "\\1", sample_id)
  } else {
    pid <- patient_id
  }

  # 3) harmonize commonly-needed fields with generous fallbacks
  #   Age: some studies have AGE, others AGE_AT_DIAGNOSIS (years already), sometimes in days
  age_years_raw <- suppressWarnings(as.numeric(pick_col(clin, c("AGE", "AGE_AT_DIAGNOSIS", "AGE_AT_INITIAL_PATHOLOGIC_DIAGNOSIS"))))
  age_days_raw  <- suppressWarnings(as.numeric(pick_col(clin, c("AGE_AT_DIAGNOSIS_DAYS", "age_at_diagnosis_days"))))
  age_years <- dplyr::coalesce(age_years_raw, ifelse(is.na(age_days_raw), NA_real_, round(age_days_raw/365.25, 1)))

  gender <- pick_col(clin, c("SEX", "gender", "Gender"))
  tumor_stage <- pick_col(clin, c("AJCC_PATHOLOGIC_TUMOR_STAGE", "PATHOLOGIC_STAGE", "TUMOR_STAGE"))
  primary_diag <- pick_col(clin, c("HISTOLOGICAL_TYPE", "PRIMARY_DIAGNOSIS", "DIAGNOSIS"))
  primary_site <- pick_col(clin, c("PRIMARY_SITE", "primary_site"))
  disease_type <- pick_col(clin, c("CANCER_TYPE_DETAILED", "CANCER_TYPE", "disease_type"))

  # vital status: map from OS_STATUS if present, else NA
  vital_status <- map_vital(pick_col(clin, c("OS_STATUS", "VITAL_STATUS", "vital_status")))

  # 4) build output (deduplicate to one row per patient)
  out <- tibble::tibble(
    case_id           = pid,
    project_id        = toupper(gsub("_tcga$", "", study_id)),
    primary_site      = primary_site,
    disease_type      = disease_type,
    gender            = gender,
    vital_status      = vital_status,
    age_days          = ifelse(is.na(age_years), age_days_raw, NA_real_), # keep if that’s all we have
    age_years         = age_years,
    tumor_stage       = tumor_stage,
    primary_diagnosis = primary_diag
  ) |>
    dplyr::distinct(case_id, .keep_all = TRUE)

  # 5) a cleanup
  out <- out |>
    dplyr::mutate(
      gender       = dplyr::if_else(is.na(gender) | gender == "", NA_character_, as.character(gender)),
      tumor_stage  = dplyr::if_else(is.na(tumor_stage) | tumor_stage == "", NA_character_, as.character(tumor_stage)),
      primary_site = dplyr::coalesce(primary_site, dplyr::if_else(grepl("^brca", study_id), "Breast", NA_character_)),
      disease_type = dplyr::coalesce(disease_type, dplyr::if_else(grepl("^brca", study_id), "Breast Invasive Carcinoma", NA_character_))
    )

  # guarantee schema & return
  dplyr::bind_rows(empty_cases(), out)
}

# Simple percent label
fmt_pct0 <- function(p) {
  ifelse(is.finite(p), paste0(round(p * 100, 0), "%"), "NA")
}