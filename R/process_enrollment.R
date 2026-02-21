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


#' Clean TEA ID values
#'
#' TEA data often includes leading single quotes (') to force Excel to treat
#' IDs as text. This function removes those quotes and trims whitespace.
#'
#' @param x Character vector of IDs
#' @return Cleaned character vector
#' @keywords internal
clean_id <- function(x) {
  # Remove leading single quote (used by TEA to force text in Excel)
  x <- gsub("^'", "", x)
  # Trim whitespace
  trimws(x)
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

  # Backfill missing entity names from reference year
  # TAPR data for 2013-2020 doesn't include DISTNAME/CAMPNAME in the download
  district_processed <- backfill_district_names(district_processed)
  campus_processed <- backfill_campus_names(campus_processed)

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
  n_rows <- nrow(df)

  # Helper to find column by pattern (case-insensitive)
  find_col <- function(patterns) {
    for (pattern in patterns) {
      matched <- grep(paste0("^", pattern, "$"), cols, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) return(matched[1])
    }
    NULL
  }

  # Build result dataframe with same number of rows as input
  result <- data.frame(
    end_year = rep(end_year, n_rows),
    type = rep("Campus", n_rows),
    stringsAsFactors = FALSE
  )

  # IDs - use clean_id to remove leading quotes from TEA data
  campus_col <- find_col(c("CAMPUS"))
  if (!is.null(campus_col)) {
    result$campus_id <- clean_id(df[[campus_col]])
    # District ID is first 6 digits of 9-digit campus ID
    result$district_id <- substr(result$campus_id, 1, 6)
  }

  district_col <- find_col(c("DISTRICT"))
  if (!is.null(district_col) && is.null(result$district_id)) {
    result$district_id <- clean_id(df[[district_col]])
  }

  # Names - always create columns, even if NA (for backfill later)
  campus_name_col <- find_col(c("CAMPNAME", "CAMPUSNAME"))
  if (!is.null(campus_name_col)) {
    result$campus_name <- trimws(df[[campus_name_col]])
  } else {
    result$campus_name <- rep(NA_character_, n_rows)
  }

  district_name_col <- find_col(c("DISTNAME", "DISTRICTNAME"))
  if (!is.null(district_name_col)) {
    result$district_name <- trimws(df[[district_name_col]])
  } else {
    result$district_name <- rep(NA_character_, n_rows)
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
  # Note: Pre-2011 data uses PETPACC (Pacific/Asian combined) instead of separate
  # PETASIC (Asian) and PETPCIC (Pacific Islander) columns
  demo_map <- list(
    white = c("CPETWHIC", "PETWHIC"),
    black = c("CPETBLAC", "PETBLAC"),
    hispanic = c("CPETHISC", "PETHISC"),
    asian = c("CPETASIC", "PETASIC", "CPETPACC", "PETPACC"),  # PACC = Pacific/Asian combined
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

  # Grade levels - TEA uses CPETGEEC, CPETGPKC, CPETGKNC, CPETG01C etc
  grade_map <- list(
    grade_ee = c("CPETGEEC", "PETGEEC"),
    grade_pk = c("CPETGPKC", "PETGPKC"),
    grade_k = c("CPETGKNC", "PETGKNC"),
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
  n_rows <- nrow(df)

  # Helper to find column by pattern (case-insensitive)
  find_col <- function(patterns) {
    for (pattern in patterns) {
      matched <- grep(paste0("^", pattern, "$"), cols, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) return(matched[1])
    }
    NULL
  }

  # Build result dataframe with same number of rows as input
  result <- data.frame(
    end_year = rep(end_year, n_rows),
    type = rep("District", n_rows),
    stringsAsFactors = FALSE
  )

  # IDs - use clean_id to remove leading quotes from TEA data
  district_col <- find_col(c("DISTRICT"))
  if (!is.null(district_col)) {
    result$district_id <- clean_id(df[[district_col]])
  }

  # Campus ID is NA for district rows
  result$campus_id <- rep(NA_character_, n_rows)

  # Names - always create columns, even if NA (for backfill later)
  district_name_col <- find_col(c("DISTNAME", "DISTRICTNAME"))
  if (!is.null(district_name_col)) {
    result$district_name <- trimws(df[[district_name_col]])
  } else {
    result$district_name <- rep(NA_character_, n_rows)
  }

  result$campus_name <- rep(NA_character_, n_rows)

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
  # Note: Pre-2011 data uses PETPACC (Pacific/Asian combined) instead of separate
  # PETASIC (Asian) and PETPCIC (Pacific Islander) columns
  demo_map <- list(
    white = c("DPETWHIC", "PETWHIC"),
    black = c("DPETBLAC", "PETBLAC"),
    hispanic = c("DPETHISC", "PETHISC"),
    asian = c("DPETASIC", "PETASIC", "DPETPACC", "PETPACC"),  # PACC = Pacific/Asian combined
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

  # Grade levels - TEA uses DPETGEEC, DPETGPKC, DPETGKNC, DPETG01C etc
  grade_map <- list(
    grade_ee = c("DPETGEEC", "PETGEEC"),
    grade_pk = c("DPETGPKC", "PETGPKC"),
    grade_k = c("DPETGKNC", "PETGKNC"),
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


# ==============================================================================
# Name Backfill Functions
# ==============================================================================
#
# TAPR data for 2013-2020 does not include DISTNAME or CAMPNAME in the
# combined DSTUD/CSTUD download. These functions backfill missing names
# using a reference lookup from the nearest year that has them (2021+).
#
# The lookup is cached in a package-level environment to avoid redundant
# downloads when processing multiple years.
#
# ==============================================================================

# Package-level environment for caching name lookups
.name_lookup_cache <- new.env(parent = emptyenv())


#' Get district name lookup table
#'
#' Downloads district reference data from a year that includes DISTNAME
#' and returns a district_id -> district_name mapping. The result is cached
#' so subsequent calls don't re-download.
#'
#' @return Data frame with district_id and district_name columns, or NULL
#'   if the lookup cannot be built
#' @keywords internal
get_district_name_lookup <- function() {

  # Return cached lookup if available
  if (exists("district_lookup", envir = .name_lookup_cache)) {
    return(get("district_lookup", envir = .name_lookup_cache))
  }

  # Try to build lookup from 2021 (first TAPR year with names)
  lookup <- tryCatch({
    raw_ref <- download_tapr_combined(2021, "D")

    if (!"DISTNAME" %in% names(raw_ref) || !"DISTRICT" %in% names(raw_ref)) {
      warning("Reference year 2021 district data missing DISTNAME column")
      return(NULL)
    }

    df <- data.frame(
      district_id = clean_id(raw_ref[["DISTRICT"]]),
      district_name = trimws(raw_ref[["DISTNAME"]]),
      stringsAsFactors = FALSE
    )

    # Remove duplicates and NAs
    df <- df[!is.na(df$district_id) & !is.na(df$district_name), ]
    df <- df[!duplicated(df$district_id), ]

    df
  }, error = function(e) {
    warning("Could not build district name lookup: ", e$message)
    NULL
  })

  # Cache the result (even if NULL, to avoid retrying)
  assign("district_lookup", lookup, envir = .name_lookup_cache)

  lookup
}


#' Get campus name lookup table
#'
#' Downloads campus reference data from a year that includes CAMPNAME
#' and returns a campus_id -> (campus_name, district_name) mapping.
#' The result is cached so subsequent calls don't re-download.
#'
#' @return Data frame with campus_id, campus_name, and district_name columns,
#'   or NULL if the lookup cannot be built
#' @keywords internal
get_campus_name_lookup <- function() {

  # Return cached lookup if available
  if (exists("campus_lookup", envir = .name_lookup_cache)) {
    return(get("campus_lookup", envir = .name_lookup_cache))
  }

  # Try to build lookup from 2021 (first TAPR year with names)
  lookup <- tryCatch({
    raw_ref <- download_tapr_combined(2021, "C")

    if (!"CAMPNAME" %in% names(raw_ref) || !"CAMPUS" %in% names(raw_ref)) {
      warning("Reference year 2021 campus data missing CAMPNAME column")
      return(NULL)
    }

    df <- data.frame(
      campus_id = clean_id(raw_ref[["CAMPUS"]]),
      campus_name = trimws(raw_ref[["CAMPNAME"]]),
      stringsAsFactors = FALSE
    )

    # Add district_name if available
    if ("DISTNAME" %in% names(raw_ref)) {
      df$district_name <- trimws(raw_ref[["DISTNAME"]])
    }

    # Remove duplicates and NAs
    df <- df[!is.na(df$campus_id) & !is.na(df$campus_name), ]
    df <- df[!duplicated(df$campus_id), ]

    df
  }, error = function(e) {
    warning("Could not build campus name lookup: ", e$message)
    NULL
  })

  # Cache the result
  assign("campus_lookup", lookup, envir = .name_lookup_cache)

  lookup
}


#' Backfill missing district names
#'
#' If district_name is all NA in the processed data, joins a name lookup
#' from a reference year to populate district names.
#'
#' @param df Processed district data frame
#' @return Data frame with district_name populated (where possible)
#' @keywords internal
backfill_district_names <- function(df) {

  # Skip if district_name is already populated
  if (!"district_name" %in% names(df)) return(df)
  if (!all(is.na(df$district_name))) return(df)
  if (!"district_id" %in% names(df)) return(df)

  lookup <- get_district_name_lookup()
  if (is.null(lookup)) return(df)

  # Join lookup by district_id
  df$district_name <- lookup$district_name[
    match(df$district_id, lookup$district_id)
  ]

  df
}


#' Backfill missing campus names
#'
#' If campus_name is all NA in the processed data, joins a name lookup
#' from a reference year to populate campus and district names.
#'
#' @param df Processed campus data frame
#' @return Data frame with campus_name and district_name populated (where possible)
#' @keywords internal
backfill_campus_names <- function(df) {

  # Backfill campus_name if missing
  if ("campus_name" %in% names(df) && all(is.na(df$campus_name)) &&
      "campus_id" %in% names(df)) {

    lookup <- get_campus_name_lookup()
    if (!is.null(lookup)) {
      idx <- match(df$campus_id, lookup$campus_id)
      df$campus_name <- lookup$campus_name[idx]

      # Also backfill district_name from campus lookup if needed
      if ("district_name" %in% names(df) && all(is.na(df$district_name)) &&
          "district_name" %in% names(lookup)) {
        df$district_name <- lookup$district_name[idx]
      }
    }
  }

  # If campus district_name is still NA, try district lookup
  if ("district_name" %in% names(df) && all(is.na(df$district_name)) &&
      "district_id" %in% names(df)) {
    dist_lookup <- get_district_name_lookup()
    if (!is.null(dist_lookup)) {
      df$district_name <- dist_lookup$district_name[
        match(df$district_id, dist_lookup$district_id)
      ]
    }
  }

  df
}
