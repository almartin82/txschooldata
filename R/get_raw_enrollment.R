# ==============================================================================
# Raw Enrollment Data Download Functions
# ==============================================================================
#
# This file contains functions for downloading raw enrollment data from TEA.
# Data comes from the Texas Academic Performance Reports (TAPR) system.
#
# TAPR data structure:
# - CSTUD: Campus Student Information (enrollment by campus)
# - DSTUD: District Student Information (enrollment by district)
# - CREF: Campus Reference (campus names, IDs, charter status)
# - DREF: District Reference (district names, IDs)
#
# ==============================================================================

#' Download raw enrollment data from TEA
#'
#' Downloads campus and district enrollment data from the Texas Academic
#' Performance Reports (TAPR) system.
#'
#' @param end_year School year end (2023-24 = 2024)
#' @return List with campus and district data frames
#' @keywords internal
get_raw_enr <- function(end_year) {

  # Validate year - TAPR data available from 2013 onwards
  if (end_year < 2013 || end_year > 2025) {
    stop("end_year must be between 2013 and 2025")
  }

  message(paste("Downloading TEA enrollment data for", end_year, "..."))

  # Download reference files (IDs, names, charter status)
  cref <- download_tapr_file(end_year, "cref")
  dref <- download_tapr_file(end_year, "dref")

  # Download student information files (enrollment counts)
  cstud <- download_tapr_file(end_year, "cstud")
  dstud <- download_tapr_file(end_year, "dstud")

  # Join reference data with student data
  campus_data <- merge_tapr_data(cref, cstud, "campus")
  district_data <- merge_tapr_data(dref, dstud, "district")

  # Add end_year column
  campus_data$end_year <- end_year
  district_data$end_year <- end_year

  list(
    campus = campus_data,
    district = district_data
  )
}


#' Build TAPR download URL
#'
#' Constructs the download URL for a TAPR data file based on year and file type.
#'
#' @param end_year School year end
#' @param file_type One of "cref", "dref", "cstud", "dstud", "sref", "sstud"
#' @return URL string
#' @keywords internal
build_tapr_url <- function(end_year, file_type) {


  # URL structure varies by year
  # 2024+: Basic Download folder structure
  # 2013-2023: xplore/download structure

  base_url <- "https://rptsvr1.tea.texas.gov/perfreport/tapr"

  if (end_year >= 2024) {
    # Modern URL pattern (2024+)
    # Example: https://rptsvr1.tea.texas.gov/perfreport/tapr/2024/download/state/cstud.dat
    url <- paste0(base_url, "/", end_year, "/download/state/", file_type, ".dat")
  } else {
    # Legacy URL pattern (2013-2023)
    # Example: https://rptsvr1.tea.texas.gov/perfreport/tapr/2023/download/state/cstud.dat
    url <- paste0(base_url, "/", end_year, "/download/state/", file_type, ".dat")
  }

  url
}


#' Download a TAPR data file
#'
#' Downloads and parses a single TAPR data file.
#'
#' @param end_year School year end
#' @param file_type One of "cref", "dref", "cstud", "dstud"
#' @return Data frame
#' @keywords internal
download_tapr_file <- function(end_year, file_type) {

  url <- build_tapr_url(end_year, file_type)

  # Download to temp file
  tname <- tempfile(
    pattern = paste0("tea_", file_type, "_"),
    tmpdir = tempdir(),
    fileext = ".dat"
  )

  # Try download with error handling
  tryCatch({
    downloader::download(url, dest = tname, mode = "wb", quiet = TRUE)
  }, error = function(e) {
    stop(paste("Failed to download", file_type, "data for year", end_year,
               "\nURL:", url,
               "\nError:", e$message))
  })

  # Check file size (small files likely error pages)
  file_info <- file.info(tname)
  if (file_info$size < 1000) {
    # Try to read and check if it's an error page
    content <- readLines(tname, n = 5, warn = FALSE)
    if (any(grepl("error|not found|404", content, ignore.case = TRUE))) {
      stop(paste("Data not available for year", end_year,
                 "- received error page instead of data"))
    }
  }

  # Read the comma-delimited file
  # TAPR .dat files are comma-delimited with headers
  df <- readr::read_csv(
    tname,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )

  df
}


#' Merge TAPR reference and student data
#'
#' Joins reference data (names, IDs) with student data (counts) based on
#' campus or district ID.
#'
#' @param ref_data Reference data frame (cref or dref)
#' @param stud_data Student data frame (cstud or dstud)
#' @param level Either "campus" or "district"
#' @return Merged data frame
#' @keywords internal
merge_tapr_data <- function(ref_data, stud_data, level) {

  # Determine join key based on level
  if (level == "campus") {
    join_key <- "CAMPUS"
  } else {
    join_key <- "DISTRICT"
  }

  # Check that join key exists in both datasets
  if (!(join_key %in% names(ref_data))) {
    # Try alternative column names
    alt_keys <- c("CAMPUS", "CAMPUSNUMBER", "DISTRICT", "DISTRICTNUMBER")
    found_key <- alt_keys[alt_keys %in% names(ref_data)][1]
    if (!is.na(found_key)) {
      names(ref_data)[names(ref_data) == found_key] <- join_key
    }
  }

  if (!(join_key %in% names(stud_data))) {
    alt_keys <- c("CAMPUS", "CAMPUSNUMBER", "DISTRICT", "DISTRICTNUMBER")
    found_key <- alt_keys[alt_keys %in% names(stud_data)][1]
    if (!is.na(found_key)) {
      names(stud_data)[names(stud_data) == found_key] <- join_key
    }
  }

  # Perform left join (keep all reference rows)
  merged <- dplyr::left_join(ref_data, stud_data, by = join_key)

  merged
}


#' Get column mappings for TAPR data
#'
#' Returns a list mapping TAPR column names to standardized names.
#' TAPR uses a specific naming convention:
#' - First char: C=Campus, D=District, R=Region, S=State
#' - PET: Student enrollment
#' - Suffix: demographic or program code
#'
#' @return Named list of column mappings
#' @keywords internal
get_tapr_column_map <- function() {
  list(
    # Reference columns
    campus_id = c("CAMPUS"),
    district_id = c("DISTRICT"),
    campus_name = c("CAMPNAME", "CAMPUSNAME"),
    district_name = c("DISTNAME", "DISTRICTNAME"),
    county_name = c("CNTYNAME", "COUNTYNAME"),
    region = c("REGION"),
    charter_flag = c("CFLCHART", "DFLCHART"),

    # Enrollment counts - use campus (C) or district (D) prefix
    # Pattern: [C|D]PET[GROUP][C|P] where C=count, P=percent
    total = c("CPETALLC", "DPETALLC"),
    white = c("CPETWHIC", "DPETWHIC"),
    black = c("CPETBLAC", "DPETBLAC"),
    hispanic = c("CPETHISC", "DPETHISC"),
    asian = c("CPETASIC", "DPETASIC"),
    pacific_islander = c("CPETPCIC", "DPETPCIC"),
    native_american = c("CPETINDC", "DPETINDC"),
    multiracial = c("CPETTWOC", "DPETTWOC"),

    # Special populations
    econ_disadv = c("CPETECOC", "DPETECOC"),
    lep = c("CPETLEPC", "DPETLEPC"),
    special_ed = c("CPETSPEC", "DPETSPEC"),

    # Grade levels
    grade_ee = c("CPETEEC", "DPETEEC"),
    grade_pk = c("CPETPKC", "DPETPKC"),
    grade_k = c("CPETKGC", "DPETKGC"),
    grade_01 = c("CPETG01C", "DPETG01C"),
    grade_02 = c("CPETG02C", "DPETG02C"),
    grade_03 = c("CPETG03C", "DPETG03C"),
    grade_04 = c("CPETG04C", "DPETG04C"),
    grade_05 = c("CPETG05C", "DPETG05C"),
    grade_06 = c("CPETG06C", "DPETG06C"),
    grade_07 = c("CPETG07C", "DPETG07C"),
    grade_08 = c("CPETG08C", "DPETG08C"),
    grade_09 = c("CPETG09C", "DPETG09C"),
    grade_10 = c("CPETG10C", "DPETG10C"),
    grade_11 = c("CPETG11C", "DPETG11C"),
    grade_12 = c("CPETG12C", "DPETG12C")
  )
}
