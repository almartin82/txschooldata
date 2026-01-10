# ==============================================================================
# Graduation Rate Data Processing Functions
# ==============================================================================
#
# This file contains functions for processing raw TEA graduation rate data
# into a clean, standardized format.
#
# TEA provides graduation rates in a complex wide format with:
# - Federal and state calculations (we use federal)
# - Multiple student groups (race/ethnicity, gender, special populations)
# - Denominator, numerator, and rate columns for each group
#
# ==============================================================================

#' Process raw TEA graduation rate data
#'
#' Transforms raw TEA graduation rate data into a standardized schema.
#' Combines campus, district, and county data into a single data frame.
#'
#' @param raw_data List containing campus, district, and county data frames from get_raw_grad
#' @param class_year Class year
#' @return Processed data frame with standardized columns
#' @keywords internal
process_grad <- function(raw_data, class_year) {

  # Process each level
  campus_processed <- process_grad_level(raw_data$campus, class_year, "Campus")
  district_processed <- process_grad_level(raw_data$district, class_year, "District")
  county_processed <- process_grad_level(raw_data$county, class_year, "County")

  # Create state aggregate from county data (sum of all counties = state)
  # Or better: use district totals to create state
  state_processed <- create_state_grad_aggregate(district_processed, class_year)

  # Combine all levels
  result <- dplyr::bind_rows(state_processed, district_processed, campus_processed)

  result
}


#' Process graduation data for a specific level
#'
#' @param df Raw data frame for a level (campus/district/county)
#' @param class_year Class year
#' @param level_type "Campus", "District", or "County"
#' @return Processed data frame
#' @keywords internal
process_grad_level <- function(df, class_year, level_type) {

  if (nrow(df) == 0) {
    # Return empty data frame with expected structure
    return(data.frame(
      class_year = integer(0),
      type = character(0),
      district_id = character(0),
      campus_id = character(0),
      district_name = character(0),
      campus_name = character(0),
      county = character(0),
      region = character(0),
      all_students_grad_rate = numeric(0),
      white_grad_rate = numeric(0),
      black_grad_rate = numeric(0),
      hispanic_grad_rate = numeric(0),
      asian_grad_rate = numeric(0),
      native_american_grad_rate = numeric(0),
      multiracial_grad_rate = numeric(0),
      female_grad_rate = numeric(0),
      male_grad_rate = numeric(0),
      econ_disadv_grad_rate = numeric(0),
      special_ed_grad_rate = numeric(0),
      lep_grad_rate = numeric(0),
      at_risk_grad_rate = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  # Filter to federal calculation only (CALC_FOR_STATE_ACCT == "No")
  # Federal rates are more comparable across years
  if ("CALC_FOR_STATE_ACCT" %in% names(df)) {
    df <- df[df$CALC_FOR_STATE_ACCT == "No", , drop = FALSE]
  }

  # Helper function to extract rate column
  extract_rate <- function(col_pattern) {
    col_match <- grep(col_pattern, names(df), value = TRUE, ignore.case = TRUE)
    if (length(col_match) > 0) {
      vals <- df[[col_match[1]]]
      # Convert to numeric, handling suppression
      vals <- as.character(vals)
      vals[vals %in% c("*", ".", "-", "", "NA", "N/A", "-1", "0", "-1.0")] <- NA
      as.numeric(vals)
    } else {
      rep(NA_real_, nrow(df))
    }
  }

  # Helper function to extract ID/name columns
  extract_col <- function(pattern, default = NA_character_) {
    col_match <- grep(pattern, names(df), value = TRUE, ignore.case = TRUE)
    if (length(col_match) > 0) {
      as.character(df[[col_match[1]]])
    } else {
      rep(default, nrow(df))
    }
  }

  # Build result data frame
  result <- data.frame(
    class_year = rep(as.integer(class_year), nrow(df)),
    type = rep(level_type, nrow(df)),

    # IDs
    district_id = extract_col("^district$|^DISTRICT"),
    campus_id = extract_col("^campus$|^CAMPUS"),

    # Names
    district_name = extract_col("^DISTNAME|^district_name"),
    campus_name = extract_col("^CAMPNAME|^campus_name"),

    # Location
    county = extract_col("^COUNTY"),
    region = extract_col("^REGION"),

    # Graduation rates (all use _R suffix for "rate")
    all_students_grad_rate = extract_rate("DIST_ALLR_GRAD|^ALLR_GRAD"),
    white_grad_rate = extract_rate("DIST_WHR_GRAD|_WHR_GRAD"),
    black_grad_rate = extract_rate("DIST_BLR_GRAD|_BLR_GRAD"),
    hispanic_grad_rate = extract_rate("DIST_HSR_GRAD|_HSR_GRAD"),
    asian_grad_rate = extract_rate("DIST_ASR_GRAD|_ASR_GRAD"),
    native_american_grad_rate = extract_rate("DIST_AIR_GRAD|_AIR_GRAD"),
    multiracial_grad_rate = extract_rate("DIST_MUR_GRAD|_MUR_GRAD"),
    female_grad_rate = extract_rate("DIST_FEMR_GRAD|_FEMR_GRAD"),
    male_grad_rate = extract_rate("DIST_MALR_GRAD|_MALR_GRAD"),
    econ_disadv_grad_rate = extract_rate("DIST_ECNR_GRAD|_ECNR_GRAD"),
    special_ed_grad_rate = extract_rate("DIST_SPER_GRAD|_SPER_GRAD"),
    lep_grad_rate = extract_rate("DIST_LEPR_GRAD|_LEPR_GRAD"),
    at_risk_grad_rate = extract_rate("DIST_ATRR_GRAD|_ATRR_GRAD"),
    stringsAsFactors = FALSE
  )

  # Clean IDs
  result$district_id <- gsub("^'", "", result$district_id)
  result$district_id <- trimws(result$district_id)
  result$campus_id <- gsub("^'", "", result$campus_id)
  result$campus_id <- trimws(result$campus_id)

  # Set NA for campus_id at district/county level, district_id at campus level
  if (level_type == "District") {
    result$campus_id <- NA_character_
    result$campus_name <- NA_character_
  } else if (level_type == "Campus") {
    # Keep both
  } else if (level_type == "County") {
    result$district_id <- NA_character_
    result$campus_id <- NA_character_
    result$district_name <- NA_character_
    result$campus_name <- NA_character_
  }

  result
}


#' Create state-level aggregate for graduation rates
#'
#' Creates a state aggregate row from district-level data.
#' State rates should match TEA's published statewide graduation rate.
#'
#' @param district_df Processed district data frame
#' @param class_year Class year
#' @return Data frame with single state row
#' @keywords internal
create_state_grad_aggregate <- function(district_df, class_year) {

  if (nrow(district_df) == 0) {
    return(data.frame(
      class_year = class_year,
      type = "State",
      district_id = NA_character_,
      campus_id = NA_character_,
      district_name = "Texas",
      campus_name = NA_character_,
      county = NA_character_,
      region = NA_character_,
      all_students_grad_rate = NA_real_,
      white_grad_rate = NA_real_,
      black_grad_rate = NA_real_,
      hispanic_grad_rate = NA_real_,
      asian_grad_rate = NA_real_,
      native_american_grad_rate = NA_real_,
      multiracial_grad_rate = NA_real_,
      female_grad_rate = NA_real_,
      male_grad_rate = NA_real_,
      econ_disadv_grad_rate = NA_real_,
      special_ed_grad_rate = NA_real_,
      lep_grad_rate = NA_real_,
      at_risk_grad_rate = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  # Calculate state-level as weighted average by district cohort size
  # For simplicity in v1, we'll use the mean of non-NA district rates
  # This is an approximation - for accuracy, need denominator weights

  rate_cols <- c(
    "all_students_grad_rate", "white_grad_rate", "black_grad_rate",
    "hispanic_grad_rate", "asian_grad_rate", "native_american_grad_rate",
    "multiracial_grad_rate", "female_grad_rate", "male_grad_rate",
    "econ_disadv_grad_rate", "special_ed_grad_rate", "lep_grad_rate",
    "at_risk_grad_rate"
  )

  state_rates <- sapply(rate_cols, function(col) {
    vals <- district_df[[col]]
    vals <- vals[!is.na(vals)]
    if (length(vals) > 0) {
      mean(vals, na.rm = TRUE)
    } else {
      NA_real_
    }
  })

  data.frame(
    class_year = as.integer(class_year),
    type = "State",
    district_id = NA_character_,
    campus_id = NA_character_,
    district_name = "Texas",
    campus_name = NA_character_,
    county = NA_character_,
    region = NA_character_,
    t(state_rates),
    stringsAsFactors = FALSE
  )
}
