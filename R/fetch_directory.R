# ==============================================================================
# School Directory Data Fetching Functions
# ==============================================================================
#
# This file contains functions for downloading school directory data from the
# Texas Education Agency (TEA) via the Texas Open Data Portal (data.texas.gov).
#
# Data source: https://data.texas.gov/dataset/AskTED-Data/hzek-udky
# Original source: TEA's AskTED (Texas Education Directory) system
#
# ==============================================================================

#' Fetch Texas school directory data
#'
#' Downloads and processes school directory data from the Texas Education Agency
#' via the Texas Open Data Portal. This includes all public schools and districts
#' with contact information and administrator names.
#'
#' @param end_year Currently unused. The directory data represents current
#'   schools and is updated regularly. Included for API consistency with
#'   other fetch functions.
#' @param tidy If TRUE (default), returns data in a standardized format with
#'   consistent column names. If FALSE, returns raw column names from TEA.
#' @param use_cache If TRUE (default), uses locally cached data when available.
#'   Set to FALSE to force re-download from TEA.
#' @return A tibble with school directory data. Columns include:
#'   \itemize{
#'     \item \code{state_district_id}: 6-digit district identifier (zero-padded)
#'     \item \code{state_school_id}: 9-digit campus identifier (district + campus)
#'     \item \code{school_name}: School name
#'     \item \code{district_name}: District name
#'     \item \code{county_name}: County name
#'     \item \code{school_type}: Type of instruction (e.g., "REGULAR INSTRUCTIONAL")
#'     \item \code{grades_served}: Grade range (e.g., "09-12")
#'     \item \code{address}: Street address
#'     \item \code{city}: City
#'     \item \code{state}: State (always "TX")
#'     \item \code{zip}: ZIP code
#'     \item \code{phone}: Phone number
#'     \item \code{principal_name}: School principal name
#'     \item \code{principal_email}: School email address
#'     \item \code{superintendent_name}: District superintendent name
#'     \item \code{superintendent_email}: District email address
#'     \item \code{charter_type}: Charter type (empty if not charter)
#'     \item \code{status}: School status (e.g., "Active")
#'     \item \code{enrollment}: School enrollment as of October snapshot
#'   }
#' @details
#' The directory data is downloaded from the Texas Open Data Portal, which
#' publishes TEA's AskTED (Texas Education Directory) data. This data is
#' updated daily by TEA and represents the current state of Texas public
#' schools and districts.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get school directory data
#' dir_data <- fetch_directory()
#'
#' # Get raw format (original TEA column names)
#' dir_raw <- fetch_directory(tidy = FALSE)
#'
#' # Force fresh download (ignore cache)
#' dir_fresh <- fetch_directory(use_cache = FALSE)
#'
#' # Filter to active schools only
#' library(dplyr)
#' active_schools <- dir_data |>
#'   filter(status == "Active")
#'
#' # Find all schools in a district
#' austin_schools <- dir_data |>
#'   filter(state_district_id == "227901")
#' }
fetch_directory <- function(end_year = NULL, tidy = TRUE, use_cache = TRUE) {

  # Determine cache type based on tidy parameter
  cache_type <- if (tidy) "directory_tidy" else "directory_raw"

  # Check cache first
  if (use_cache && cache_exists_directory(cache_type)) {
    message("Using cached school directory data")
    return(read_cache_directory(cache_type))
  }

  # Get raw data from TEA
  raw <- get_raw_directory()

  # Process to standard schema
  if (tidy) {
    result <- process_directory(raw)
  } else {
    result <- raw
  }

  # Cache the result
  if (use_cache) {
    write_cache_directory(result, cache_type)
  }

  result
}


#' Get raw school directory data from TEA
#'
#' Downloads the raw school directory CSV file from the Texas Open Data Portal.
#' This is official TEA AskTED data published on data.texas.gov.
#'
#' @return Raw data frame as downloaded from TEA
#' @keywords internal
get_raw_directory <- function() {

  # Build download URL
  url <- build_directory_url()

  message("Downloading school directory data from TEA...")

  # Download file to temp location
  tname <- tempfile(pattern = "tea_directory", tmpdir = tempdir(), fileext = ".csv")

  # Set longer timeout for large files
  old_timeout <- getOption("timeout")
  options(timeout = 300)  # 5 minutes

  tryCatch({
    response <- httr::GET(
      url,
      httr::write_disk(tname, overwrite = TRUE),
      httr::timeout(300)
    )

    if (httr::http_error(response)) {
      options(timeout = old_timeout)
      stop(paste("HTTP error:", httr::status_code(response)))
    }
  }, error = function(e) {
    options(timeout = old_timeout)
    stop(paste("Failed to download school directory data from TEA:", e$message))
  })

  options(timeout = old_timeout)

  # Check if download was successful (file should be reasonably large)
  file_info <- file.info(tname)
  if (file_info$size < 10000) {
    stop("Download failed - file too small, may be error page")
  }

  message(paste("Downloaded", round(file_info$size / 1024 / 1024, 2), "MB file"))

  # Read CSV file - all as text to preserve leading zeros
  df <- readr::read_csv(
    tname,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )

  # Clean up temp file
  unlink(tname)

  message(paste("Loaded", nrow(df), "records"))

  # Convert to tibble for consistency
  dplyr::as_tibble(df)
}


#' Build TEA school directory download URL
#'
#' Constructs the download URL for the school directory CSV file from
#' the Texas Open Data Portal.
#'
#' @return URL string
#' @keywords internal
build_directory_url <- function() {
  # Texas Open Data Portal - AskTED dataset
  # This is official TEA data published on the state's open data portal
  # Updated regularly by TEA (daily in the AskTED system)
  "https://data.texas.gov/api/views/hzek-udky/rows.csv?accessType=DOWNLOAD"
}


#' Process raw school directory data to standard schema
#'
#' Takes raw school directory data from TEA and standardizes column names,
#' types, and adds derived columns.
#'
#' @param raw_data Raw data frame from get_raw_directory()
#' @return Processed data frame with standard schema
#' @keywords internal
process_directory <- function(raw_data) {

  cols <- names(raw_data)

  # Helper to find columns with flexible matching
  find_col <- function(patterns) {
    for (pattern in patterns) {
      matched <- grep(pattern, cols, value = TRUE, ignore.case = TRUE)
      if (length(matched) > 0) return(matched[1])
    }
    NULL
  }

  # Build the standardized result data frame
  n_rows <- nrow(raw_data)
  result <- dplyr::tibble(.rows = n_rows)

  # District ID - 6 characters, zero-padded
  # TEA uses 'District Number column with apostrophe prefix
  dist_col <- find_col(c("^District Number$", "^District.?Number$", "^DISTRICT"))
  if (!is.null(dist_col)) {
    # Remove leading apostrophe and ensure 6-digit format
    dist_vals <- gsub("^'", "", raw_data[[dist_col]])
    result$state_district_id <- sprintf("%06s", dist_vals)
    result$state_district_id <- gsub(" ", "0", result$state_district_id)
  }

  # School/Campus ID - 9 characters (6-digit district + 3-digit campus)
  school_col <- find_col(c("^School Number$", "^School.?Number$", "^SCHOOL"))
  if (!is.null(school_col)) {
    # Remove leading apostrophe and ensure 9-digit format
    school_vals <- gsub("^'", "", raw_data[[school_col]])
    result$state_school_id <- sprintf("%09s", school_vals)
    result$state_school_id <- gsub(" ", "0", result$state_school_id)
  }

  # County Name
  county_col <- find_col(c("^County Name$", "^County.?Name$", "^COUNTY"))
  if (!is.null(county_col)) {
    result$county_name <- trimws(raw_data[[county_col]])
  }

  # District Name
  district_name_col <- find_col(c("^District Name$", "^District.?Name$"))
  if (!is.null(district_name_col)) {
    result$district_name <- trimws(raw_data[[district_name_col]])
  }

  # School Name
  school_name_col <- find_col(c("^School Name$", "^School.?Name$"))
  if (!is.null(school_name_col)) {
    result$school_name <- trimws(raw_data[[school_name_col]])
  }

  # School Type / Instruction Type
  type_col <- find_col(c("^Instruction Type$", "^Instruction.?Type$", "^School.?Type$"))
  if (!is.null(type_col)) {
    result$school_type <- trimws(raw_data[[type_col]])
  }

  # District Type
  dist_type_col <- find_col(c("^District Type$", "^District.?Type$"))
  if (!is.null(dist_type_col)) {
    result$district_type <- trimws(raw_data[[dist_type_col]])
  }

  # Charter Type
  charter_col <- find_col(c("^Charter Type$", "^Charter.?Type$"))
  if (!is.null(charter_col)) {
    result$charter_type <- trimws(raw_data[[charter_col]])
  }

  # Alternative Education Accountability
  alt_ed_col <- find_col(c("^Alternative Education Accountability$"))
  if (!is.null(alt_ed_col)) {
    result$alternative_ed <- trimws(raw_data[[alt_ed_col]])
  }

  # Magnet Status
  magnet_col <- find_col(c("^Magnet Status$", "^Magnet$"))
  if (!is.null(magnet_col)) {
    result$magnet_status <- trimws(raw_data[[magnet_col]])
  }

  # Status (Active, Inactive, etc.)
  status_col <- find_col(c("^School Status$", "^Status$"))
  if (!is.null(status_col)) {
    result$status <- trimws(raw_data[[status_col]])
  }

  # Status Date
  status_date_col <- find_col(c("^School Status Date$", "^Status.?Date$"))
  if (!is.null(status_date_col)) {
    result$status_date <- trimws(raw_data[[status_date_col]])
  }

  # Grade Range
  grade_col <- find_col(c("^Grade Range$", "^Grade.?Range$", "^Grades$"))
  if (!is.null(grade_col)) {
    # Remove leading apostrophe that TEA uses to preserve formatting
    result$grades_served <- gsub("^'", "", raw_data[[grade_col]])
  }

  # School Address fields
  school_street_col <- find_col(c("^School Street Address$", "^School.?Street"))
  if (!is.null(school_street_col)) {
    result$address <- trimws(raw_data[[school_street_col]])
  }

  school_city_col <- find_col(c("^School City$", "^School.?City$"))
  if (!is.null(school_city_col)) {
    result$city <- trimws(raw_data[[school_city_col]])
  }

  school_state_col <- find_col(c("^School State$", "^School.?State$"))
  if (!is.null(school_state_col)) {
    result$state <- trimws(raw_data[[school_state_col]])
  } else {
    result$state <- "TX"
  }

  school_zip_col <- find_col(c("^School Zip$", "^School.?Zip$"))
  if (!is.null(school_zip_col)) {
    result$zip <- trimws(raw_data[[school_zip_col]])
  }

  # School Phone
  school_phone_col <- find_col(c("^School Phone$", "^School.?Phone$"))
  if (!is.null(school_phone_col)) {
    result$phone <- trimws(raw_data[[school_phone_col]])
  }

  # School Fax
  school_fax_col <- find_col(c("^School Fax$", "^School.?Fax$"))
  if (!is.null(school_fax_col)) {
    result$fax <- trimws(raw_data[[school_fax_col]])
  }

  # School Principal
  principal_col <- find_col(c("^School Principal$", "^Principal$"))
  if (!is.null(principal_col)) {
    result$principal_name <- trimws(raw_data[[principal_col]])
  }

  # School Email
  school_email_col <- find_col(c("^School Email Address$", "^School.?Email"))
  if (!is.null(school_email_col)) {
    result$principal_email <- trimws(raw_data[[school_email_col]])
  }

  # School Website
  school_web_col <- find_col(c("^School Web Page Address$", "^School.?Web"))
  if (!is.null(school_web_col)) {
    result$school_website <- trimws(raw_data[[school_web_col]])
  }

  # District Address fields
  dist_street_col <- find_col(c("^District Street Address$", "^District.?Street"))
  if (!is.null(dist_street_col)) {
    result$district_address <- trimws(raw_data[[dist_street_col]])
  }

  dist_city_col <- find_col(c("^District City$", "^District.?City$"))
  if (!is.null(dist_city_col)) {
    result$district_city <- trimws(raw_data[[dist_city_col]])
  }

  dist_state_col <- find_col(c("^District State$", "^District.?State$"))
  if (!is.null(dist_state_col)) {
    result$district_state <- trimws(raw_data[[dist_state_col]])
  }

  dist_zip_col <- find_col(c("^District Zip$", "^District.?Zip$"))
  if (!is.null(dist_zip_col)) {
    result$district_zip <- trimws(raw_data[[dist_zip_col]])
  }

  # District Phone
  dist_phone_col <- find_col(c("^District Phone$", "^District.?Phone$"))
  if (!is.null(dist_phone_col)) {
    result$district_phone <- trimws(raw_data[[dist_phone_col]])
  }

  # District Superintendent
  supt_col <- find_col(c("^District Superintendent$", "^Superintendent$"))
  if (!is.null(supt_col)) {
    result$superintendent_name <- trimws(raw_data[[supt_col]])
  }

  # District Email
  dist_email_col <- find_col(c("^District Email Address$", "^District.?Email"))
  if (!is.null(dist_email_col)) {
    result$superintendent_email <- trimws(raw_data[[dist_email_col]])
  }

  # District Website
  dist_web_col <- find_col(c("^District Web Page Address$", "^District.?Web"))
  if (!is.null(dist_web_col)) {
    result$district_website <- trimws(raw_data[[dist_web_col]])
  }

  # ESC Region
  region_col <- find_col(c("^ESC Region Served$", "^ESC.?Region", "^Region$"))
  if (!is.null(region_col)) {
    # Remove leading apostrophe
    result$esc_region <- gsub("^'", "", raw_data[[region_col]])
  }

  # School Enrollment
  school_enr_col <- find_col(c("^School Enrollment", "^Enrollment$"))
  if (!is.null(school_enr_col)) {
    result$enrollment <- as.integer(raw_data[[school_enr_col]])
  }

  # District Enrollment
  dist_enr_col <- find_col(c("^District Enrollment"))
  if (!is.null(dist_enr_col)) {
    result$district_enrollment <- as.integer(raw_data[[dist_enr_col]])
  }

  # NCES IDs (for reference, though we don't use federal data)
  nces_dist_col <- find_col(c("^NCES District ID$"))
  if (!is.null(nces_dist_col)) {
    result$nces_district_id <- gsub("^'", "", raw_data[[nces_dist_col]])
  }

  nces_school_col <- find_col(c("^NCES School ID$"))
  if (!is.null(nces_school_col)) {
    result$nces_school_id <- gsub("^'", "", raw_data[[nces_school_col]])
  }

  # Reorder columns for consistency
  preferred_order <- c(
    "state_district_id", "state_school_id",
    "county_name", "district_name", "school_name",
    "district_type", "school_type", "charter_type",
    "grades_served", "status", "status_date",
    "address", "city", "state", "zip", "phone", "fax",
    "principal_name", "principal_email", "school_website",
    "district_address", "district_city", "district_state", "district_zip",
    "district_phone", "superintendent_name", "superintendent_email", "district_website",
    "esc_region", "enrollment", "district_enrollment",
    "alternative_ed", "magnet_status",
    "nces_district_id", "nces_school_id"
  )

  existing_cols <- preferred_order[preferred_order %in% names(result)]
  other_cols <- setdiff(names(result), preferred_order)

  result <- result |>
    dplyr::select(dplyr::all_of(c(existing_cols, other_cols)))

  result
}


# ==============================================================================
# Directory-specific cache functions
# ==============================================================================

#' Build cache file path for directory data
#'
#' @param cache_type Type of cache ("directory_tidy" or "directory_raw")
#' @return File path string
#' @keywords internal
build_cache_path_directory <- function(cache_type) {
  cache_dir <- get_cache_dir()
  file.path(cache_dir, paste0(cache_type, ".rds"))
}


#' Check if cached directory data exists
#'
#' @param cache_type Type of cache ("directory_tidy" or "directory_raw")
#' @param max_age Maximum age in days (default 7). Set to Inf to ignore age.
#' @return Logical indicating if valid cache exists
#' @keywords internal
cache_exists_directory <- function(cache_type, max_age = 7) {
  cache_path <- build_cache_path_directory(cache_type)

  if (!file.exists(cache_path)) {
    return(FALSE)
  }

  # Check age
  file_info <- file.info(cache_path)
  age_days <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "days"))

  age_days <= max_age
}


#' Read directory data from cache
#'
#' @param cache_type Type of cache ("directory_tidy" or "directory_raw")
#' @return Cached data frame
#' @keywords internal
read_cache_directory <- function(cache_type) {
  cache_path <- build_cache_path_directory(cache_type)
  readRDS(cache_path)
}


#' Write directory data to cache
#'
#' @param data Data frame to cache
#' @param cache_type Type of cache ("directory_tidy" or "directory_raw")
#' @return Invisibly returns the cache path
#' @keywords internal
write_cache_directory <- function(data, cache_type) {
  cache_path <- build_cache_path_directory(cache_type)
  cache_dir <- dirname(cache_path)

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  saveRDS(data, cache_path)
  invisible(cache_path)
}


#' Clear school directory cache
#'
#' Removes cached school directory data files.
#'
#' @return Invisibly returns the number of files removed
#' @export
#' @examples
#' \dontrun{
#' # Clear cached directory data
#' clear_directory_cache()
#' }
clear_directory_cache <- function() {
  cache_dir <- get_cache_dir()

  if (!dir.exists(cache_dir)) {
    message("Cache directory does not exist")
    return(invisible(0))
  }

  files <- list.files(cache_dir, pattern = "^directory_", full.names = TRUE)

  if (length(files) > 0) {
    file.remove(files)
    message(paste("Removed", length(files), "cached directory file(s)"))
  } else {
    message("No cached directory files to remove")
  }

  invisible(length(files))
}
