# ==============================================================================
# Raw Enrollment Data Download Functions
# ==============================================================================
#
# This file contains functions for downloading raw enrollment data from TEA.
# Data comes from two systems:
# - TAPR (Texas Academic Performance Reports): 2013-present
# - AEIS (Academic Excellence Indicator System): 2003-2012
#
# Both systems use an interactive SAS-based download interface. This package
# downloads data via GET requests to the SAS broker endpoint.
#
# Data structure:
# - CSTUD/cstud: Campus Student Information (enrollment by campus)
# - DSTUD/dstud: District Student Information (enrollment by district)
# - CREF/cref: Campus Reference (campus names, IDs, charter status)
# - DREF/dref: District Reference (district names, IDs)
#
# ==============================================================================

#' Download raw enrollment data from TEA
#'
#' Downloads campus and district enrollment data from TEA's reporting systems.
#' Uses TAPR for 2013+ and AEIS for 2003-2012.
#'
#' @param end_year School year end (2023-24 = 2024)
#' @return List with campus and district data frames
#' @keywords internal
get_raw_enr <- function(end_year) {

  # Validate year
  # AEIS: 2003-2012, TAPR: 2013-2025

  if (end_year < 2003 || end_year > 2025) {
    stop("end_year must be between 2003 and 2025")
  }

  message(paste("Downloading TEA enrollment data for", end_year, "..."))

  # Use appropriate download function based on year
  if (end_year <= 2012) {
    # AEIS system (2003-2012)
    campus_data <- download_aeis_data(end_year, "C")
    district_data <- download_aeis_data(end_year, "D")
  } else {
    # TAPR system (2013+)
    message("  Downloading campus data...")
    campus_data <- download_tapr_combined(end_year, "C")

    message("  Downloading district data...")
    district_data <- download_tapr_combined(end_year, "D")
  }

  # Add end_year column
  campus_data$end_year <- end_year
  district_data$end_year <- end_year

  list(
    campus = campus_data,
    district = district_data
  )
}


#' Download AEIS data (2003-2012)
#'
#' Downloads data from TEA's AEIS system using GET requests to the SAS broker.
#' AEIS uses separate requests for reference and student data.
#'
#' @param end_year School year end (2003-2012)
#' @param sumlev Summary level: "C" for campus, "D" for district
#' @return Data frame with combined reference and student data
#' @keywords internal
download_aeis_data <- function(end_year, sumlev) {

  message(paste0("  Downloading ", ifelse(sumlev == "C", "campus", "district"), " data (AEIS)..."))

  # Determine dataset names and ID parameter

  if (sumlev == "C") {
    stud_dsname <- "cstud"
    ref_dsname <- "cref"
    id_param <- "camp0=999999"
  } else {
    stud_dsname <- "dstud"
    ref_dsname <- "dref"
    id_param <- "dist0=999999"
  }

  # AEIS keys for student data
  # Note: multiracial (PETTWO) and pacific_islander (PETPCI) only available 2011+
  stud_keys <- c(
    "PETALL",  # Total
    "PETBLA",  # Black/African American
    "PETHIS",  # Hispanic
    "PETASI",  # Asian
    "PETIND",  # American Indian
    "PETECO",  # Economically Disadvantaged
    "PETLEP",  # LEP
    "PETSPE",  # Special Education
    "PETGEE",  # Early Education
    "PETGPK",  # Pre-K
    "PETGKN",  # Kindergarten
    "PETG01", "PETG02", "PETG03", "PETG04",
    "PETG05", "PETG06", "PETG07", "PETG08"
  )

  # Grade 9-12 use different key names in AEIS
  stud_keys <- c(stud_keys, "ETG9", "ETG0", "ETG1", "ETG2")

  # Add multiracial and pacific islander for 2011+
  if (end_year >= 2011) {
    stud_keys <- c(stud_keys, "PETTWO", "PETPCI")
  }

  # Add white - it's PETWHI in AEIS
  stud_keys <- c(stud_keys, "PETWHI")

  # AEIS keys for reference data
  ref_keys <- c("ISTNAM", "NTYNAM", "EGION", "FLCHAR")

  # Download student data
  stud_df <- download_aeis_file(end_year, stud_dsname, stud_keys, id_param)

  # Download reference data
  ref_df <- download_aeis_file(end_year, ref_dsname, ref_keys, id_param)

  # Merge by district/campus ID
  id_col <- if (sumlev == "C") "CAMPUS" else "DISTRICT"

  # Reference data has the ID column
  if (id_col %in% names(ref_df) && id_col %in% names(stud_df)) {
    df <- dplyr::left_join(stud_df, ref_df, by = id_col)
  } else {
    # Just use student data if merge fails
    df <- stud_df
  }

  df
}


#' Download a single AEIS data file
#'
#' @param end_year School year end
#' @param dsname Dataset name (cstud, dstud, cref, dref)
#' @param keys Vector of key names to request
#' @param id_param ID parameter string (e.g., "dist0=999999")
#' @return Data frame
#' @keywords internal
download_aeis_file <- function(end_year, dsname, keys, id_param) {

  prgopt <- paste0(end_year, "/xplore/getdata.sas")

  # Build URL
  base_url <- "https://rptsvr1.tea.texas.gov/cgi/sas/broker"
  key_params <- paste0("key=", keys, collapse = "&")

  url <- paste0(
    base_url, "?",
    "_service=marykay",
    "&year4=", end_year,
    "&prgopt=", prgopt,
    "&_program=perfrept.perfmast.sas",
    "&dsname=", dsname,
    "&sumlev=", toupper(substr(dsname, 1, 1)),
    "&_debug=0",
    "&", id_param,
    "&_saveas=", dsname,
    "&datafmt=C",
    "&", key_params
  )

  # Create temp file
  tname <- tempfile(
    pattern = paste0("tea_", dsname, "_"),
    tmpdir = tempdir(),
    fileext = ".csv"
  )

  # Download with longer timeout for AEIS (server is slow)
  tryCatch({
    response <- httr::GET(
      url,
      httr::write_disk(tname, overwrite = TRUE),
      httr::timeout(600)  # 10 minute timeout for slow AEIS server
    )

    if (httr::http_error(response)) {
      stop(paste("HTTP error:", httr::status_code(response)))
    }

    # Check for error page
    file_info <- file.info(tname)
    if (file_info$size < 500) {
      content <- readLines(tname, n = 10, warn = FALSE)
      if (any(grepl("error|completed with errors", content, ignore.case = TRUE))) {
        stop(paste("SAS broker returned an error for", dsname, "year", end_year))
      }
    }

  }, error = function(e) {
    stop(paste("Failed to download", dsname, "data for year", end_year,
               "\nError:", e$message))
  })

  # Read CSV
  df <- readr::read_csv(
    tname,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )

  unlink(tname)

  # Standardize AEIS column names to match TAPR format
  df <- standardize_aeis_columns(df, dsname)

  df
}


#' Standardize AEIS column names to match TAPR format
#'
#' @param df Data frame with AEIS column names
#' @param dsname Dataset name to determine prefix
#' @return Data frame with standardized column names
#' @keywords internal
standardize_aeis_columns <- function(df, dsname) {

  # Determine prefix based on dataset
  prefix <- toupper(substr(dsname, 1, 1))

  # Column name mappings: AEIS -> TAPR
  # Student data columns
  col_map <- c(
    # Demographics
    "DPETALLC" = "DPETALLC",  # Already correct
    "CPETALLC" = "CPETALLC",
    # AEIS uses different suffixes - need to add C for count
    "DPETBLAC" = "DPETBLAC",
    "CPETBLAC" = "CPETBLAC",
    "DPETHISC" = "DPETHISC",
    "CPETHISC" = "CPETHISC",
    "DPETASIC" = "DPETASIC",
    "CPETASIC" = "CPETASIC",
    "DPETINDC" = "DPETINDC",
    "CPETINDC" = "CPETINDC",
    "DPETWHIC" = "DPETWHIC",
    "CPETWHIC" = "CPETWHIC",
    "DPETPCIC" = "DPETPCIC",
    "CPETPCIC" = "CPETPCIC",
    "DPETTWOC" = "DPETTWOC",
    "CPETTWOC" = "CPETTWOC",
    # Special populations
    "DPETECOC" = "DPETECOC",
    "CPETECOC" = "CPETECOC",
    "DPETLEPC" = "DPETLEPC",
    "CPETLEPC" = "CPETLEPC",
    "DPETSPEC" = "DPETSPEC",
    "CPETSPEC" = "CPETSPEC",
    # Reference columns
    "DISTNAME" = "DISTNAME",
    "CAMPNAME" = "CAMPNAME",
    "CNTYNAME" = "CNTYNAME",
    "REGION" = "REGION",
    "DFLCHART" = "DFLCHART",
    "CFLCHART" = "CFLCHART"
  )

  # Rename grade columns: AEIS uses ETG9, ETG0, ETG1, ETG2 for grades 9-12
  # TAPR uses DPETG09C, DPETG10C, etc.
  grade_map <- c(
    "DETG9C" = paste0(prefix, "PETG09C"),
    "CETG9C" = paste0(prefix, "PETG09C"),
    "DETG0C" = paste0(prefix, "PETG10C"),
    "CETG0C" = paste0(prefix, "PETG10C"),
    "DETG1C" = paste0(prefix, "PETG11C"),
    "CETG1C" = paste0(prefix, "PETG11C"),
    "DETG2C" = paste0(prefix, "PETG12C"),
    "CETG2C" = paste0(prefix, "PETG12C")
  )

  # Apply renaming
  names(df) <- sapply(names(df), function(n) {
    if (n %in% names(grade_map)) {
      return(grade_map[n])
    }
    n
  })

  df
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
  # 2013-2023: Uses simpler path
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
