# ==============================================================================
# Enrollment Data Fetching Functions
# ==============================================================================
#
# This file contains functions for downloading enrollment data from the
# Texas Education Agency (TEA) website.
#
# ==============================================================================

#' Fetch Texas enrollment data
#'
#' Downloads and processes enrollment data from the Texas Education Agency
#' PEIMS data files via the TAPR (Texas Academic Performance Reports) system.
#'
#' @param end_year A school year. Year is the end of the academic year - eg 2023-24
#'   school year is year '2024'. Valid values are 1997-2026 (AEIS CGI for 1997-2002,
#'   AEIS SAS for 2003-2012, TAPR for 2013-2026).
#' @param tidy If TRUE (default), returns data in long (tidy) format with subgroup
#'   column. If FALSE, returns wide format.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#'   Set to FALSE to force re-download from TEA.
#' @return Data frame with enrollment data. Wide format includes columns for
#'   district_id, campus_id, names, and enrollment counts by demographic/grade.
#'   Tidy format pivots these counts into subgroup and grade_level columns.
#' @export
#' @examples
#' \dontrun{
#' # Get 2024 enrollment data (2023-24 school year)
#' enr_2024 <- fetch_enr(2024)
#'
#' # Get wide format
#' enr_wide <- fetch_enr(2024, tidy = FALSE)
#'
#' # Force fresh download (ignore cache)
#' enr_fresh <- fetch_enr(2024, use_cache = FALSE)
#'
#' # Filter to specific district
#' austin_isd <- enr_2024 |>
#'   dplyr::filter(district_id == "101912")
#' }
fetch_enr <- function(end_year, tidy = TRUE, use_cache = TRUE) {

  # Validate year - AEIS CGI: 1997-2002, AEIS SAS: 2003-2012, TAPR: 2013-2026
  if (end_year < 1997 || end_year > 2026) {
    stop("end_year must be between 1997 and 2026")
  }

  # Determine cache type based on tidy parameter
  cache_type <- if (tidy) "tidy" else "wide"

  # Check cache first
  if (use_cache && cache_exists(end_year, cache_type)) {
    message(paste("Using cached data for", end_year))
    return(read_cache(end_year, cache_type))
  }

  # For years where old APIs are dead (2003-2023), use bundled data if available.

  # TEA decommissioned both the AEIS xplore/getdata.sas endpoint (2003-2012)

  # and the TAPR xplore/getdata.sas endpoint (2013-2023). Bundled data is the
  # only reliable source for these years.
  if (end_year >= 2003 && end_year <= 2023) {
    bundled <- tryCatch(
      read_bundled_enr(end_year, cache_type),
      error = function(e) NULL
    )
    if (!is.null(bundled)) {
      if (use_cache) {
        write_cache(bundled, end_year, cache_type)
      }
      return(bundled)
    }
    # No bundled data — fall through to live download attempt.
    # For AEIS years (2003-2012) this will likely fail since the API is dead.
  }

  # Get raw data from TEA
  raw <- get_raw_enr(end_year)

  # Process to standard schema
  processed <- process_enr(raw, end_year)

  # Optionally tidy
  if (tidy) {
    processed <- tidy_enr(processed) |>
      id_enr_aggs()
  }

  # Cache the result
  if (use_cache) {
    write_cache(processed, end_year, cache_type)
  }

  processed
}


#' Fetch enrollment data for multiple years
#'
#' Downloads and combines enrollment data for multiple school years.
#'
#' @param end_years Vector of school year ends (e.g., c(2022, 2023, 2024))
#' @param tidy If TRUE (default), returns data in long (tidy) format.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#' @return Combined data frame with enrollment data for all requested years
#' @export
#' @examples
#' \dontrun{
#' # Get 3 years of data
#' enr_multi <- fetch_enr_multi(2022:2024)
#'
#' # Track enrollment trends
#' enr_multi |>
#'   dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
#'   dplyr::select(end_year, n_students)
#' }
fetch_enr_multi <- function(end_years, tidy = TRUE, use_cache = TRUE) {

  # Validate years - AEIS CGI: 1997-2002, AEIS SAS: 2003-2012, TAPR: 2013-2026
  invalid_years <- end_years[end_years < 1997 | end_years > 2026]
  if (length(invalid_years) > 0) {
    stop(paste("Invalid years:", paste(invalid_years, collapse = ", "),
               "\nend_year must be between 1997 and 2026"))
  }

  # Fetch each year
  results <- purrr::map(
    end_years,
    function(yr) {
      message(paste("Fetching", yr, "..."))
      fetch_enr(yr, tidy = tidy, use_cache = use_cache)
    }
  )

  # Combine
  dplyr::bind_rows(results)
}


#' Read bundled enrollment data from inst/extdata
#'
#' For years where TEA's old xplore API is dead (2013-2023), reads
#' pre-processed data bundled with the package.
#'
#' @param end_year School year end (2013-2023)
#' @param type Data type: "tidy" or "wide"
#' @return Data frame with enrollment data
#' @keywords internal
read_bundled_enr <- function(end_year, type) {

  filename <- paste0("enr_", type, "_", end_year, ".rds")
  filepath <- system.file("extdata", filename, package = "txschooldata")

  if (filepath == "") {
    # Check if the other format exists
    other_type <- if (type == "wide") "tidy" else "wide"
    other_file <- system.file(
      "extdata",
      paste0("enr_", other_type, "_", end_year, ".rds"),
      package = "txschooldata"
    )

    if (other_file != "") {
      stop(
        "Bundled ", type, " format data is not available for ", end_year, ".\n",
        "The ", other_type, " format is available. Try: fetch_enr(", end_year,
        ", tidy = ", toupper(other_type == "tidy"), ")"
      )
    }

    stop(
      "No bundled data available for year ", end_year, ".\n",
      "TEA's old download API has been decommissioned for this year."
    )
  }

  message(paste("Reading bundled data for", end_year))
  readRDS(filepath)
}
