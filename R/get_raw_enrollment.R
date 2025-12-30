# ==============================================================================
# Raw Enrollment Data Download Functions
# ==============================================================================
#
# This file contains functions for downloading raw enrollment data from TEA.
# Data comes from the Texas Academic Performance Reports (TAPR) system.
#
# TEA uses an interactive SAS-based download system. This package downloads
# data via GET requests to the SAS broker endpoint with appropriate parameters.
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
  # 2013-2023: Uses consistent TAPR SAS broker interface
  # 2024+: Uses slightly different URL structure
  if (end_year < 2013 || end_year > 2025) {
    stop("end_year must be between 2013 and 2025")
  }

  message(paste("Downloading TEA enrollment data for", end_year, "..."))

  # Download campus data (reference + student info combined)
  message("  Downloading campus data...")
  campus_data <- download_tapr_combined(end_year, "C")

  # Download district data (reference + student info combined)
  message("  Downloading district data...")
  district_data <- download_tapr_combined(end_year, "D")

  # Add end_year column
  campus_data$end_year <- end_year
  district_data$end_year <- end_year

  list(
    campus = campus_data,
    district = district_data
  )
}


#' Download combined TAPR reference and student data
#'
#' Downloads data from TEA's TAPR system using GET requests to the SAS broker.
#' This function retrieves both reference (ID, name) and student (enrollment)
#' data in a single request.
#'
#' @param end_year School year end
#' @param sumlev Summary level: "C" for campus, "D" for district
#' @return Data frame
#' @keywords internal
download_tapr_combined <- function(end_year, sumlev) {

  # Determine dataset name and select appropriate keys
  if (sumlev == "C") {
    dsname <- "CSTUD"
    # IDENT = Campus/District ID and names
    # PET = Student Membership Counts/Percents (demographics)
    # PETG = Student Membership by Grade
    keys <- c("IDENT", "PET", "PETG")
    id_param <- "camp0=999999"  # 6 digits for "all campuses"
  } else {
    dsname <- "DSTUD"
    keys <- c("IDENT", "PET", "PETG")
    id_param <- "dist0=999999"
  }

  # Determine program path based on year
  # 2024+: Uses "Basic Download" folder structure
  # 2020-2023: Uses simpler path without "tapr/"
  if (end_year >= 2024) {
    prgopt <- paste0(end_year, "/tapr/Basic%20Download/xplore/getdata.sas")
  } else {
    prgopt <- paste0(end_year, "/xplore/getdata.sas")
  }

  # Build the URL with query parameters
  # TEA's SAS broker requires multiple key parameters for column selection
  base_url <- paste0("https://rptsvr1.tea.texas.gov/cgi/sas/broker/", dsname)
  key_params <- paste0("key=", keys, collapse = "&")

  url <- paste0(
    base_url, "?",
    "_service=marykay",
    "&year4=", end_year,
    "&prgopt=", prgopt,
    "&_program=perfrept.perfmast.sas",
    "&dsname=", dsname,
    "&sumlev=", sumlev,
    "&_debug=0",
    "&format=CSV",
    "&", id_param,
    "&_saveas=", dsname,
    "&datafmt=C",  # CSV format
    "&", key_params
  )

  # Create temp file for download
  tname <- tempfile(
    pattern = paste0("tea_", tolower(dsname), "_"),
    tmpdir = tempdir(),
    fileext = ".csv"
  )

  # Download using httr
  tryCatch({
    response <- httr::GET(
      url,
      httr::write_disk(tname, overwrite = TRUE),
      httr::timeout(300)
    )

    # Check for HTTP errors
    if (httr::http_error(response)) {
      stop(paste("HTTP error:", httr::status_code(response)))
    }

    # Check content type - should be CSV
    content_type <- httr::headers(response)$`content-type`
    if (!is.null(content_type) && !grepl("comma-separated|csv|text", content_type, ignore.case = TRUE)) {
      # May have received HTML error page
      first_lines <- readLines(tname, n = 3, warn = FALSE)
      if (any(grepl("^<html|^<HTML|^<!DOCTYPE|error", first_lines, ignore.case = TRUE))) {
        stop(paste("Received error page instead of CSV data for", dsname, "year", end_year))
      }
    }

    # Check file size (small files likely error pages)
    file_info <- file.info(tname)
    if (file_info$size < 500) {
      content <- readLines(tname, n = 10, warn = FALSE)
      if (any(grepl("error|not found|404|completed with errors", content, ignore.case = TRUE))) {
        stop(paste("SAS broker returned an error. Data may not be available for year", end_year))
      }
    }

  }, error = function(e) {
    stop(paste("Failed to download", dsname, "data for year", end_year,
               "\nError:", e$message))
  })

  # Read the CSV file
  df <- readr::read_csv(
    tname,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )

  # Clean up temp file
  unlink(tname)

  df
}


#' Get column mappings for TAPR data
#'
#' Returns a list mapping TAPR column names to standardized names.
#' TAPR uses a specific naming convention:
#' - First char: C=Campus, D=District, R=Region, S=State
#' - PET: Student membership/enrollment
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

    # Grade levels - Note: column names use PETG prefix
    grade_ee = c("CPETGEEC", "DPETGEEC"),
    grade_pk = c("CPETGPKC", "DPETGPKC"),
    grade_k = c("CPETGKNC", "DPETGKNC"),
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
