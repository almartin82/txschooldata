# ==============================================================================
# Raw Graduation Rate Data Download Functions
# ==============================================================================
#
# This file contains functions for downloading raw graduation rate data from
# the Texas Education Agency (TEA).
#
# Data comes from the Completion, Graduation, and Dropout data portal:
# https://tea.texas.gov/reports-and-data/school-performance/accountability-research/completion-graduation-and-dropout
#
# File naming pattern:
# - Campus: campus-data-download-4yr-{year}.xlsx
# - District: district-data-download-4yr-{year}.xlsx
# - County: county-data-download-4yr-{year}.xlsx
#
# Available years: Class of 2003-2024 (4-year longitudinal rates)
#
# ==============================================================================

#' Download raw graduation rate data from TEA
#'
#' Downloads campus, district, and state graduation rate data from TEA's
#' Completion, Graduation, and Dropout portal for a specified class year.
#'
#' @param class_year Class year (e.g., 2024 for Class of 2024). Valid values: 2003-2024.
#' @return List with campus, district, and state data frames
#' @keywords internal
get_raw_grad <- function(class_year) {

  # Validate year
  if (class_year < 2003 || class_year > 2024) {
    stop("class_year must be between 2003 and 2024")
  }

  message(paste("Downloading TEA graduation rate data for Class of", class_year, "..."))

  # Build URLs
  base_url <- "https://tea.texas.gov/reports-and-data/school-performance/accountability-research/completion-graduation-and-dropout"

  campus_url <- paste0(base_url, "/campus-data-download-4yr-", class_year, ".xlsx")
  district_url <- paste0(base_url, "/district-data-download-4yr-", class_year, ".xlsx")
  county_url <- paste0(base_url, "/county-data-download-4yr-", class_year, ".xlsx")

  # Download files
  message("  Downloading campus data...")
  campus_data <- download_grad_file(campus_url, class_year, "campus")

  message("  Downloading district data...")
  district_data <- download_grad_file(district_url, class_year, "district")

  message("  Downloading county data...")
  county_data <- download_grad_file(county_url, class_year, "county")

  list(
    campus = campus_data,
    district = district_data,
    county = county_data
  )
}


#' Download a single graduation rate Excel file
#'
#' @param url URL to download
#' @param class_year Class year for error messages
#' @param level Data level ("campus", "district", or "county")
#' @return Data frame with graduation rate data
#' @keywords internal
download_grad_file <- function(url, class_year, level) {

  # Create temp file
  temp_file <- tempfile(fileext = ".xlsx")

  # Download with httr
  tryCatch({
    response <- httr::GET(url, httr::write_disk(temp_file, overwrite = TRUE))

    if (httr::http_error(response)) {
      stop(paste("HTTP error:", httr::status_code(response)))
    }
  }, error = function(e) {
    stop(paste("Failed to download", level, "data for Class of", class_year, ":", e$message))
  })

  # Read Excel file - data is in "Comp_{year}_4yr" sheet
  sheet_name <- paste0("Comp_", class_year, "_4yr")

  tryCatch({
    data <- readxl::read_excel(temp_file, sheet = sheet_name, guess_max = 10000)
  }, error = function(e) {
    stop(paste("Failed to read", level, "Excel file for Class of", class_year, ":", e$message))
  })

  # Clean up temp file
  unlink(temp_file)

  # Add level indicator
  data$.data_level <- level

  data
}
