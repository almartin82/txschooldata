# ==============================================================================
# Enrollment Data Processing Functions
# ==============================================================================
#
# This file contains functions for processing raw TEA enrollment data into a
# clean, standardized format.
#
# ==============================================================================

#' Convert to numeric, handling suppression markers
#'
#' TEA uses various markers for suppressed data (*, <5, -1, etc.)
#' and may use commas in large numbers.
#'
#' @param x Vector to convert
#' @return Numeric vector with NA for non-numeric values
#' @keywords internal
safe_numeric <- function(x) {
  # Remove commas and whitespace
  x <- gsub(",", "", x)
  x <- trimws(x)

  # Handle common suppression markers
  x[x %in% c("*", ".", "-", "-1", "<5", "N/A", "NA", "")] <- NA_character_

  suppressWarnings(as.numeric(x))
}


#' Process raw TEA enrollment data
#'
#' Transforms raw TAPR data into a standardized schema combining campus
#' and district data.
#'
#' @param raw_data List containing campus and district data frames from get_raw_enr
#' @param end_year School year end
#' @return Processed data frame with standardized columns
#' @keywords internal
process_enr <- function(raw_data, end_year) {

  # Process campus data
  campus_processed <- process_campus_enr(raw_data$campus, end_year)

  # Process district data
  district_processed <- process_district_enr(raw_data$district, end_year)

  # Create state aggregate
  state_processed <- create_state_aggregate(district_processed, end_year)

  # Combine all levels
  result <- dplyr::bind_rows(state_processed, district_processed, campus_processed)

  result
}


#' Process campus-level enrollment data
#'
#' @param df Raw campus data frame
#' @param end_year School year end
#' @return Processed campus data frame
#' @keywords internal
process_campus_enr <- function(df, end_year) {

  cols <- names(df)

  # Helper to find column by pattern (case-insensitive)
  find_col <- function(patterns) {
    for (pattern in patterns) {
      matched <- grep(paste0("^", pattern, "$"), cols, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) return(matched[1])
    }
    NULL
  }

  # Build result dataframe
  result <- data.frame(
    end_year = end_year,
    type = "Campus",
    stringsAsFactors = FALSE
  )

  # IDs
  campus_col <- find_col(c("CAMPUS"))
  if (!is.null(campus_col)) {
    result$campus_id <- trimws(df[[campus_col]])
    # District ID is first 6 digits of 9-digit campus ID
    result$district_id <- substr(result$campus_id, 1, 6)
  }

  district_col <- find_col(c("DISTRICT"))
  if (!is.null(district_col) && is.null(result$district_id)) {
    result$district_id <- trimws(df[[district_col]])
  }

  # Names
  campus_name_col <- find_col(c("CAMPNAME", "CAMPUSNAME"))
  if (!is.null(campus_name_col)) {
    result$campus_name <- trimws(df[[campus_name_col]])
  }

  district_name_col <- find_col(c("DISTNAME", "DISTRICTNAME"))
  if (!is.null(district_name_col)) {
    result$district_name <- trimws(df[[district_name_col]])
  }

  # County and region
  county_col <- find_col(c("CNTYNAME", "COUNTYNAME", "COUNTY"))
  if (!is.null(county_col)) {
    result$county <- trimws(df[[county_col]])
  }

  region_col <- find_col(c("REGION"))
  if (!is.null(region_col)) {
    result$region <- trimws(df[[region_col]])
  }

  # Charter flag
  charter_col <- find_col(c("CFLCHART", "CHARTER"))
  if (!is.null(charter_col)) {
    result$charter_flag <- trimws(df[[charter_col]])
  }

  # Total enrollment
  total_col <- find_col(c("CPETALLC", "PETALLC"))
  if (!is.null(total_col)) {
    result$row_total <- safe_numeric(df[[total_col]])
  }

  # Demographics
  demo_map <- list(
    white = c("CPETWHIC", "PETWHIC"),
    black = c("CPETBLAC", "PETBLAC"),
    hispanic = c("CPETHISC", "PETHISC"),
    asian = c("CPETASIC", "PETASIC"),
    pacific_islander = c("CPETPCIC", "PETPCIC"),
    native_american = c("CPETINDC", "PETINDC"),
    multiracial = c("CPETTWOC", "PETTWOC")
  )

  for (name in names(demo_map)) {
    col <- find_col(demo_map[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(df[[col]])
    }
  }

  # Special populations
  special_map <- list(
    econ_disadv = c("CPETECOC", "PETECOC"),
    lep = c("CPETLEPC", "PETLEPC"),
    special_ed = c("CPETSPEC", "PETSPEC")
  )

  for (name in names(special_map)) {
    col <- find_col(special_map[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(df[[col]])
    }
  }

  # Grade levels
  grade_map <- list(
    grade_ee = c("CPETEEC", "PETEEC"),
    grade_pk = c("CPETPKC", "PETPKC"),
    grade_k = c("CPETKGC", "PETKGC"),
    grade_01 = c("CPETG01C", "PETG01C"),
    grade_02 = c("CPETG02C", "PETG02C"),
    grade_03 = c("CPETG03C", "PETG03C"),
    grade_04 = c("CPETG04C", "PETG04C"),
    grade_05 = c("CPETG05C", "PETG05C"),
    grade_06 = c("CPETG06C", "PETG06C"),
    grade_07 = c("CPETG07C", "PETG07C"),
    grade_08 = c("CPETG08C", "PETG08C"),
    grade_09 = c("CPETG09C", "PETG09C"),
    grade_10 = c("CPETG10C", "PETG10C"),
    grade_11 = c("CPETG11C", "PETG11C"),
    grade_12 = c("CPETG12C", "PETG12C")
  )

  for (name in names(grade_map)) {
    col <- find_col(grade_map[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(df[[col]])
    }
  }

  result
}


#' Process district-level enrollment data
#'
#' @param df Raw district data frame
#' @param end_year School year end
#' @return Processed district data frame
#' @keywords internal
process_district_enr <- function(df, end_year) {

  cols <- names(df)

  # Helper to find column by pattern (case-insensitive)
  find_col <- function(patterns) {
    for (pattern in patterns) {
      matched <- grep(paste0("^", pattern, "$"), cols, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) return(matched[1])
    }
    NULL
  }

  # Build result dataframe
  result <- data.frame(
    end_year = end_year,
    type = "District",
    stringsAsFactors = FALSE
  )

  # IDs
  district_col <- find_col(c("DISTRICT"))
  if (!is.null(district_col)) {
    result$district_id <- trimws(df[[district_col]])
  }

  # Campus ID is NA for district rows
  result$campus_id <- NA_character_

  # Names
  district_name_col <- find_col(c("DISTNAME", "DISTRICTNAME"))
  if (!is.null(district_name_col)) {
    result$district_name <- trimws(df[[district_name_col]])
  }

  result$campus_name <- NA_character_

  # County and region
  county_col <- find_col(c("CNTYNAME", "COUNTYNAME", "COUNTY"))
  if (!is.null(county_col)) {
    result$county <- trimws(df[[county_col]])
  }

  region_col <- find_col(c("REGION"))
  if (!is.null(region_col)) {
    result$region <- trimws(df[[region_col]])
  }

  # Charter flag
  charter_col <- find_col(c("DFLCHART", "CHARTER"))
  if (!is.null(charter_col)) {
    result$charter_flag <- trimws(df[[charter_col]])
  }

  # Total enrollment
  total_col <- find_col(c("DPETALLC", "PETALLC"))
  if (!is.null(total_col)) {
    result$row_total <- safe_numeric(df[[total_col]])
  }

  # Demographics
  demo_map <- list(
    white = c("DPETWHIC", "PETWHIC"),
    black = c("DPETBLAC", "PETBLAC"),
    hispanic = c("DPETHISC", "PETHISC"),
    asian = c("DPETASIC", "PETASIC"),
    pacific_islander = c("DPETPCIC", "PETPCIC"),
    native_american = c("DPETINDC", "PETINDC"),
    multiracial = c("DPETTWOC", "PETTWOC")
  )

  for (name in names(demo_map)) {
    col <- find_col(demo_map[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(df[[col]])
    }
  }

  # Special populations
  special_map <- list(
    econ_disadv = c("DPETECOC", "PETECOC"),
    lep = c("DPETLEPC", "PETLEPC"),
    special_ed = c("DPETSPEC", "PETSPEC")
  )

  for (name in names(special_map)) {
    col <- find_col(special_map[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(df[[col]])
    }
  }

  # Grade levels
  grade_map <- list(
    grade_ee = c("DPETEEC", "PETEEC"),
    grade_pk = c("DPETPKC", "PETPKC"),
    grade_k = c("DPETKGC", "PETKGC"),
    grade_01 = c("DPETG01C", "PETG01C"),
    grade_02 = c("DPETG02C", "PETG02C"),
    grade_03 = c("DPETG03C", "PETG03C"),
    grade_04 = c("DPETG04C", "PETG04C"),
    grade_05 = c("DPETG05C", "PETG05C"),
    grade_06 = c("DPETG06C", "PETG06C"),
    grade_07 = c("DPETG07C", "PETG07C"),
    grade_08 = c("DPETG08C", "PETG08C"),
    grade_09 = c("DPETG09C", "PETG09C"),
    grade_10 = c("DPETG10C", "PETG10C"),
    grade_11 = c("DPETG11C", "PETG11C"),
    grade_12 = c("DPETG12C", "PETG12C")
  )

  for (name in names(grade_map)) {
    col <- find_col(grade_map[[name]])
    if (!is.null(col)) {
      result[[name]] <- safe_numeric(df[[col]])
    }
  }

  result
}


#' Create state-level aggregate from district data
#'
#' @param district_df Processed district data frame
#' @param end_year School year end
#' @return Single-row data frame with state totals
#' @keywords internal
create_state_aggregate <- function(district_df, end_year) {

  # Columns to sum
  sum_cols <- c(
    "row_total",
    "white", "black", "hispanic", "asian",
    "pacific_islander", "native_american", "multiracial",
    "econ_disadv", "lep", "special_ed",
    "grade_ee", "grade_pk", "grade_k",
    "grade_01", "grade_02", "grade_03", "grade_04",
    "grade_05", "grade_06", "grade_07", "grade_08",
    "grade_09", "grade_10", "grade_11", "grade_12"
  )

  # Filter to columns that exist
  sum_cols <- sum_cols[sum_cols %in% names(district_df)]

  # Create state row
  state_row <- data.frame(
    end_year = end_year,
    type = "State",
    district_id = NA_character_,
    campus_id = NA_character_,
    district_name = NA_character_,
    campus_name = NA_character_,
    county = NA_character_,
    region = NA_character_,
    charter_flag = NA_character_,
    stringsAsFactors = FALSE
  )

  # Sum each column
  for (col in sum_cols) {
    state_row[[col]] <- sum(district_df[[col]], na.rm = TRUE)
  }

  state_row
}
