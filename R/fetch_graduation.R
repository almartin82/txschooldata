# ==============================================================================
# Graduation Rate Data Fetching Functions
# ==============================================================================
#
# This file contains functions for downloading and processing graduation rate
# data from the Texas Education Agency (TEA).
#
# ==============================================================================

#' Fetch Texas graduation rate data
#'
#' Downloads and processes graduation rate data from the Texas Education Agency
#' Completion, Graduation, and Dropout portal. Provides four-year longitudinal
#' graduation rates for the Class of 2003-2024.
#'
#' @param class_year A class year (e.g., 2024 for Class of 2024). Valid values: 2003-2024.
#' @param tidy If TRUE (default), returns data in long (tidy) format with subgroup
#'   column. If FALSE, returns wide format.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#'   Set to FALSE to force re-download from TEA.
#' @return Data frame with graduation rate data. Wide format includes columns for
#'   district/campus IDs and graduation rates by subgroup. Tidy format pivots
#'   these rates into a subgroup column.
#' @export
#' @examples
#' \dontrun{
#' # Get 2024 graduation rates (Class of 2024)
#' grad_2024 <- fetch_grad(2024)
#'
#' # Get wide format
#' grad_wide <- fetch_grad(2024, tidy = FALSE)
#'
#' # Force fresh download (ignore cache)
#' grad_fresh <- fetch_grad(2024, use_cache = FALSE)
#'
#' # Filter to specific district
#' austin_isd <- grad_2024 |>
#'   dplyr::filter(district_id == "101912")
#'
#' # Compare state and district rates
#' grad_2024 |>
#'   dplyr::filter(type %in% c("State", "District")) |>
#'   dplyr::select(district_name, subgroup, grad_rate)
#' }
fetch_grad <- function(class_year, tidy = TRUE, use_cache = TRUE) {

  # Validate year
  if (class_year < 2003 || class_year > 2024) {
    stop("class_year must be between 2003 and 2024")
  }

  # Determine cache type based on tidy parameter
  cache_type <- if (tidy) "grad_tidy" else "grad_wide"

  # Check cache first
  if (use_cache && cache_exists(class_year, cache_type)) {
    message(paste("Using cached data for Class of", class_year))
    return(read_cache(class_year, cache_type))
  }

  # Get raw data from TEA
  raw <- get_raw_grad(class_year)

  # Process to standard schema
  processed <- process_grad(raw, class_year)

  # Optionally tidy
  if (tidy) {
    processed <- tidy_grad(processed)
  }

  # Cache the result
  if (use_cache) {
    write_cache(processed, class_year, cache_type)
  }

  processed
}


#' Fetch graduation rate data for multiple class years
#'
#' Downloads and combines graduation rate data for multiple class years.
#'
#' @param class_years Vector of class years (e.g., c(2018, 2019, 2020))
#' @param tidy If TRUE (default), returns data in long (tidy) format with subgroup
#'   column. If FALSE, returns wide format.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#' @return Data frame with graduation rate data for all specified years
#' @export
#' @examples
#' \dontrun{
#' # Get graduation rates for Class of 2018-2024
#' grad_multi <- fetch_grad_multi(2018:2024)
#'
#' # Track state trends
#' grad_multi |>
#'   dplyr::filter(type == "State", subgroup == "all_students") |>
#'   dplyr::select(class_year, grad_rate)
#'
#' # Compare districts over time
#' grad_multi |>
#'   dplyr::filter(district_id %in% c("101912", "015902")) |>
#'   dplyr::group_by(district_name, class_year) |>
#'   dplyr::summarize(
#'     rate = mean(grad_rate, na.rm = TRUE),
#'     .groups = "drop"
#'   )
#' }
fetch_grad_multi <- function(class_years, tidy = TRUE, use_cache = TRUE) {

  # Validate years
  if (any(class_years < 2003) || any(class_years > 2024)) {
    stop("All class_years must be between 2003 and 2024")
  }

  # Download data for each year
  result <- purrr::map_df(class_years, function(year) {
    message(paste("Fetching Class of", year, "..."))
    fetch_grad(year, tidy = tidy, use_cache = use_cache)
  })

  result
}
