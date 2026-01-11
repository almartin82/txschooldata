# ==============================================================================
# Raw STAAR Data Download Functions
# ==============================================================================
#
# This file contains functions for downloading raw STAAR assessment data from TEA.
# Data comes from TEA's STAAR Aggregate Data files (legacy format 2018-2023).
#
# Data structure:
# - Comma-delimited text files (.dat)
# - District-level: dfy{year}e{grade}.dat (English), dfy{year}s{grade}.dat (Spanish)
# - Campus-level: cfy{year}e{grade}.dat (English), cfy{year}s{grade}.dat (Spanish)
#
# Years available:
# - 2018: 1,989 columns, 5 LEP codes, no 504 indicators
# - 2019: Not available
# - 2020: No assessments (COVID-19)
# - 2021-2022: 2,091 columns, 8 LEP codes, with 504 indicators
# - 2023: 2,065 columns, 8 LEP codes, with 504 indicators, RLA_ prefix
#
# ==============================================================================

STAAR_BASE_URL <- "https://tea.texas.gov/student-assessment/student-assessment-results/staar-aggregate-data/"

#' Download raw STAAR data from TEA
#'
#' Downloads STAAR aggregate data files from TEA's website. Legacy format
#' covers 2018-2023 (no data for 2019-2020).
#'
#' @param year Assessment year (2018, 2021, 2022, or 2023)
#' @param level "district" or "campus"
#' @param grade Grade level (3-8)
#' @param subject "english" or "spanish" (default: "english")
#' @return Data frame with raw STAAR data
#' @keywords internal
get_raw_staar <- function(year, level, grade, subject = "english") {

  # Validate year
  available_years <- c(2018, 2021, 2022, 2023)
  if (!year %in% available_years) {
    stop(sprintf("year must be one of: %s (no data available for 2019-2020 due to COVID)",
                paste(available_years, collapse = ", ")))
  }

  # Validate level
  if (!level %in% c("district", "campus")) {
    stop("level must be 'district' or 'campus'")
  }

  # Validate grade
  if (!grade %in% 3:8) {
    stop("grade must be between 3 and 8")
  }

  # Validate subject
  if (!subject %in% c("english", "spanish")) {
    stop("subject must be 'english' or 'spanish'")
  }

  # Build filename
  # Format: {level}fy{year}{subject_code}{grade}.dat
  level_code <- ifelse(level == "district", "d", "c")
  year_short <- substr(year, 3, 4)  # Get last 2 digits
  subject_code <- ifelse(subject == "english", "e", "s")

  filename <- sprintf("%sfy%s%s%d.dat",
                      level_code, year_short, subject_code, grade)

  url <- paste0(STAAR_BASE_URL, filename)

  message(sprintf("Downloading STAAR data for %d Grade %d %s %s...",
                  year, grade, level, subject))
  message(sprintf("  URL: %s", url))

  # Download with retry logic
  max_retries <- 3
  retry_delay <- 2

  for (attempt in 1:max_retries) {
    tryCatch({
      response <- httr::GET(url,
                            httr::timeout(30),
                            httr::user_agent("txschooldata R package"))

      # Check for successful response
      if (httr::http_error(response)) {
        stop(sprintf("HTTP error: %s", httr::status_code(response)))
      }

      # Get content
      content <- httr::content(response, as = "text", encoding = "UTF-8")

      # Parse as CSV
      # STAAR files are comma-delimited but may have quoted strings
      result <- readr::read_csv(content,
                                col_types = readr::cols(.default = "c"),
                                na = c("", "*", "-"),
                                show_col_types = FALSE)

      # Check for actual data (not HTML error page)
      if (ncol(result) < 100) {
        warning(sprintf("Downloaded file may not be valid (only %d columns). Check URL.",
                        ncol(result)))
      }

      message(sprintf("  Successfully downloaded %d rows, %d columns",
                      nrow(result), ncol(result)))

      return(result)

    }, error = function(e) {
      if (attempt < max_retries) {
        message(sprintf("  Download failed (attempt %d/%d): %s",
                       attempt, max_retries, e$message))
        message(sprintf("  Retrying in %d seconds...", retry_delay))
        Sys.sleep(retry_delay)
      } else {
        stop(sprintf("Failed to download STAAR data after %d attempts: %s",
                     max_retries, e$message))
      }
    })
  }
}

#' Get available STAAR years
#'
#' Returns a vector of years for which STAAR data is available.
#'
#' @return Integer vector of available years
#' @keywords internal
staar_available_years <- function() {
  c(2018, 2021, 2022, 2023)
}

#' Get STAAR grade levels
#'
#' Returns available grade levels for STAAR data.
#'
#' @return Integer vector of available grades (3-8)
#' @keywords internal
get_staar_grades <- function() {
  3:8
}
