# ==============================================================================
# STAAR Data Fetch Wrapper
# ==============================================================================
#
# This file provides the main user-facing function for fetching STAAR data.
# Combines download, processing, and tidying with optional caching.
#
# ==============================================================================

#' Fetch STAAR assessment data
#'
#' Downloads, processes, and optionally tidies STAAR assessment data from TEA.
#' Supports legacy STAAR aggregate data for years 2018, 2021-2023.
#'
#' @param year Assessment year (2018, 2021, 2022, or 2023)
#' @param level "district" or "campus"
#' @param grade Grade level (3-8)
#' @param subject "english" or "spanish" (default: "english")
#' @param tidy Return tidy long format? (default: FALSE)
#' @param use_cache Use cached data if available? (default: TRUE)
#' @return Data frame with STAAR assessment data
#' @keywords internal
#' @examples
#' \dontrun{
#' # Fetch district-level Grade 3 RLA and Math results for 2023
#' staar_2023 <- fetch_staar(2023, "district", 3)
#'
#' # Fetch tidy format
#' staar_tidy <- fetch_staar(2023, "district", 3, tidy = TRUE)
#'
#' # Fetch campus-level data
#' staar_campus <- fetch_staar(2023, "campus", 5)
#' }
fetch_staar <- function(year,
                        level,
                        grade,
                        subject = "english",
                        tidy = FALSE,
                        use_cache = TRUE) {

  # Determine cache type
  cache_type <- ifelse(tidy, "tidy", "processed")

  # Check cache first
  if (use_cache && cache_exists_staar(year, level, grade, subject, cache_type)) {
    message(sprintf("Using cached STAAR data for %d Grade %d %s", year, grade, level))
    return(read_cache_staar(year, level, grade, subject, cache_type))
  }

  # Download raw data
  raw_data <- get_raw_staar(year, level, grade, subject)

  # Process to standardize columns
  processed_data <- process_staar(raw_data, year, level, grade)

  # Tidy if requested
  if (tidy) {
    result <- tidy_staar(processed_data)
  } else {
    result <- processed_data
  }

  # Write to cache
  write_cache_staar(result, year, level, grade, subject, cache_type)

  message(sprintf("Successfully fetched %d rows of STAAR data", nrow(result)))

  result
}

#' Get STAAR cache path
#'
#' Generates cache file path for STAAR data.
#'
#' @param year Assessment year
#' @param level "district" or "campus"
#' @param grade Grade level
#' @param subject "english" or "spanish"
#' @param cache_type "tidy" or "processed"
#' @return File path for cache
#' @keywords internal
get_staar_cache_path <- function(year, level, grade, subject, cache_type) {

  cache_dir <- get_cache_dir()

  filename <- sprintf("staar_%s_%d_%s_%d_%s.rds",
                      cache_type,
                      year,
                      level,
                      grade,
                      subject)

  file.path(cache_dir, filename)
}

#' Check if STAAR cache exists
#'
#' @param year Assessment year
#' @param level "district" or "campus"
#' @param grade Grade level
#' @param subject "english" or "spanish"
#' @param cache_type "tidy" or "processed"
#' @return Logical indicating if cache exists
#' @keywords internal
cache_exists_staar <- function(year, level, grade, subject, cache_type) {

  cache_path <- get_staar_cache_path(year, level, grade, subject, cache_type)
  file.exists(cache_path)
}

#' Read STAAR cache
#'
#' @param year Assessment year
#' @param level "district" or "campus"
#' @param grade Grade level
#' @param subject "english" or "spanish"
#' @param cache_type "tidy" or "processed"
#' @return Cached data frame or NULL if cache doesn't exist
#' @keywords internal
read_cache_staar <- function(year, level, grade, subject, cache_type) {

  cache_path <- get_staar_cache_path(year, level, grade, subject, cache_type)

  if (!file.exists(cache_path)) {
    return(NULL)
  }

  readr::read_rds(cache_path)
}

#' Write STAAR cache
#'
#' @param data Data frame to cache
#' @param year Assessment year
#' @param level "district" or "campus"
#' @param grade Grade level
#' @param subject "english" or "spanish"
#' @param cache_type "tidy" or "processed"
#' @keywords internal
write_cache_staar <- function(data, year, level, grade, subject, cache_type) {

  cache_path <- get_staar_cache_path(year, level, grade, subject, cache_type)

  # Create cache directory if needed
  cache_dir <- dirname(cache_path)
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  readr::write_rds(data, cache_path)
}

#' Clear STAAR cache
#'
#' Clears all cached STAAR data files.
#'
#' @keywords internal
clear_staar_cache <- function() {

  cache_dir <- get_cache_dir()

  # Remove all STAAR cache files
  staar_cache_files <- list.files(cache_dir, pattern = "^staar_", full.names = TRUE)

  if (length(staar_cache_files) > 0) {
    file.remove(staar_cache_files)
    message(sprintf("Cleared %d STAAR cache file(s)", length(staar_cache_files)))
  } else {
    message("No STAAR cache files found")
  }

  invisible(NULL)
}
