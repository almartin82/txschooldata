# ==============================================================================
# Raw Enrollment Data Download Functions
# ==============================================================================
#
# This file contains functions for downloading raw enrollment data from TEA.
# Data comes from three sources:
# - TAPR (Texas Academic Performance Reports): 2013-present
# - AEIS SAS broker (Academic Excellence Indicator System): 2003-2012
# - AEIS CGI (older AEIS format): 1997-2002
#
# Both 2003+ systems use an interactive SAS-based download interface. This
# package downloads data via GET requests to the SAS broker endpoint.
# The 1997-2002 data uses a simpler CGI endpoint with POST requests.
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
#' Uses TAPR for 2013+, AEIS SAS broker for 2003-2012, and AEIS CGI for 1997-2002.
#'
#' @param end_year School year end (e.g., 2023-24 = 2024). Valid range: 1997-2026.
#' @return List with campus and district data frames
#' @keywords internal
get_raw_enr <- function(end_year) {

  # Validate year
  # AEIS CGI: 1997-2002, AEIS SAS: 2003-2012, TAPR: 2013-2026

  if (end_year < 1997 || end_year > 2026) {
    stop("end_year must be between 1997 and 2026")
  }

  message(paste("Downloading TEA enrollment data for", end_year, "..."))

  # Use appropriate download function based on year
  if (end_year <= 2002) {
    # AEIS CGI system (1997-2002)
    campus_data <- download_aeis_cgi(end_year, "camp")
    district_data <- download_aeis_cgi(end_year, "dist")
  } else if (end_year <= 2012) {
    # AEIS SAS broker (2003-2012)
    campus_data <- download_aeis_data(end_year, "C")
    district_data <- download_aeis_data(end_year, "D")
  } else if (end_year <= 2025) {
    # TAPR system (2013-2025)
    message("  Downloading campus data...")
    campus_data <- download_tapr_combined(end_year, "C")

    message("  Downloading district data...")
    district_data <- download_tapr_combined(end_year, "D")
  } else {
    # Ad-hoc enrollment system (2026+, before TAPR publishes)
    campus_data <- download_adhoc_enrollment(end_year, "C")
    district_data <- download_adhoc_enrollment(end_year, "D")
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

    # Check for error page (HTML response instead of CSV)
    first_lines <- readLines(tname, n = 10, warn = FALSE)
    if (any(grepl("<!DOCTYPE|<html|<HTML|completed with errors", first_lines, ignore.case = TRUE))) {
      stop(paste("AEIS SAS broker returned an HTML error page for", dsname,
                 "year", end_year,
                 "- the xplore/getdata.sas endpoint may be decommissioned"))
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


#' Download AEIS data via CGI endpoint (1997-2002)
#'
#' Downloads data from TEA's older AEIS system using POST requests to CGI.
#' This system returns comma-delimited data without headers.
#'
#' @param end_year School year end (1997-2002)
#' @param level Summary level: "camp" for campus, "dist" for district
#' @return Data frame with combined reference and student data
#' @keywords internal
download_aeis_cgi <- function(end_year, level) {

  message(paste0("  Downloading ", level, " data (AEIS CGI)..."))

  # Build CGI URL - uses full year in path
  cgi_url <- paste0("https://rptsvr1.tea.texas.gov/cgi/perfreport/", end_year, "aeis.cgi")

  # Download reference data (names, IDs)
  ref_data <- httr::POST(
    cgi_url,
    body = list(level = level, file = "ref", suf = ".dat"),
    encode = "form",
    httr::timeout(120)
  )

  if (httr::http_error(ref_data)) {
    stop(paste("Failed to download reference data for year", end_year))
  }

  ref_content <- httr::content(ref_data, "text", encoding = "UTF-8")

  # Download layout for reference to get column names
  ref_layout <- httr::POST(
    cgi_url,
    body = list(level = level, file = "ref", suf = ".lyt"),
    encode = "form",
    httr::timeout(60)
  )
  ref_layout_text <- httr::content(ref_layout, "text", encoding = "UTF-8")
  ref_cols <- parse_aeis_layout(ref_layout_text)

  # Parse reference data
  ref_df <- readr::read_csv(
    ref_content,
    col_names = ref_cols,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )

  # Download student data (enrollment counts)
  stud_data <- httr::POST(
    cgi_url,
    body = list(level = level, file = "stud", suf = ".dat"),
    encode = "form",
    httr::timeout(120)
  )

  if (httr::http_error(stud_data)) {
    stop(paste("Failed to download student data for year", end_year))
  }

  stud_content <- httr::content(stud_data, "text", encoding = "UTF-8")

  # Download layout for student data
  stud_layout <- httr::POST(
    cgi_url,
    body = list(level = level, file = "stud", suf = ".lyt"),
    encode = "form",
    httr::timeout(60)
  )
  stud_layout_text <- httr::content(stud_layout, "text", encoding = "UTF-8")
  stud_cols <- parse_aeis_layout(stud_layout_text)

  # Parse student data
  stud_df <- readr::read_csv(
    stud_content,
    col_names = stud_cols,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )

  # Merge by ID column
  id_col <- if (level == "camp") "CAMPUS" else "DISTRICT"

  if (id_col %in% names(ref_df) && id_col %in% names(stud_df)) {
    df <- dplyr::left_join(stud_df, ref_df, by = id_col)
  } else {
    df <- stud_df
  }

  df
}


#' Parse AEIS layout file to extract column names
#'
#' @param layout_text Text content of layout file
#' @return Character vector of column names
#' @keywords internal
parse_aeis_layout <- function(layout_text) {
  lines <- strsplit(layout_text, "\n")[[1]]

  # Find lines with column definitions (start with 3-digit number)
  col_lines <- grep("^[0-9]{3} ", lines, value = TRUE)

  # Extract column names (second field after position number)
  col_names <- sapply(col_lines, function(line) {
    parts <- strsplit(trimws(line), "\\s+")[[1]]
    if (length(parts) >= 2) parts[2] else NA
  })

  col_names <- col_names[!is.na(col_names)]
  as.character(col_names)
}


#' Download combined TAPR reference and student data
#'
#' Downloads data from TEA's TAPR Data Download (dd_tapr) system using POST
#' requests. This replaced the old xplore/getdata.sas system which was
#' decommissioned by TEA.
#'
#' @param end_year School year end (2024+)
#' @param sumlev Summary level: "C" for campus, "D" for district
#' @return Data frame
#' @keywords internal
download_tapr_combined <- function(end_year, sumlev) {

  base_url <- "https://rptsvr1.tea.texas.gov/cgi/sas/broker"
  prgopt <- "reports/tapr/dd/dd_tapr_step_7.sas"

  # Keys for enrollment data: demographics, special pops, grades
  stud_keys <- c(
    "ETALL", "NTALL",
    "ETBLAC", "ETHISC", "ETWHIC", "ETASIC", "ETPCIC", "ETINDC", "ETTWOC",
    "NTECO", "NTLEP", "ETSPE",
    "ETGEEC", "ETGPKC", "ETGKNC",
    "ETG01C", "ETG02C", "ETG03C", "ETG04C", "ETG05C", "ETG06C",
    "ETG07C", "ETG08C", "ETG09C", "ETG10C", "ETG11C", "ETG12C"
  )
  ref_keys <- c("IDENT", "FLCHART", "NTYNAM", "EGION")

  if (sumlev == "C") {
    level <- "Campus"
    tapr <- "all_c"
  } else {
    level <- "District"
    tapr <- "all_d"
  }

  # Download student enrollment data (STUD)
  stud_body <- build_dd_tapr_body(end_year, prgopt, level, tapr, sumlev, "STUD", stud_keys)
  stud_df <- post_dd_tapr(base_url, stud_body, paste0(level, " STUD"), end_year)

  # Download reference data (REF)
  ref_body <- build_dd_tapr_body(end_year, prgopt, level, tapr, sumlev, "REF", ref_keys)
  ref_df <- post_dd_tapr(base_url, ref_body, paste0(level, " REF"), end_year)

  # Merge STUD + REF by ID column
  id_col <- if (sumlev == "C") "CAMPUS" else "DISTRICT"

  if (id_col %in% names(stud_df) && id_col %in% names(ref_df)) {
    # Remove duplicate columns from ref before joining
    ref_only_cols <- setdiff(names(ref_df), names(stud_df))
    ref_for_join <- ref_df[, c(id_col, ref_only_cols), drop = FALSE]
    df <- dplyr::left_join(stud_df, ref_for_join, by = id_col)
  } else {
    df <- stud_df
  }

  df
}


#' Build POST body for dd_tapr request
#'
#' @param end_year School year end
#' @param prgopt SAS program path
#' @param level "Campus" or "District"
#' @param tapr "all_c" or "all_d"
#' @param sumlev "C" or "D"
#' @param dsname "STUD" or "REF"
#' @param keys Character vector of key codes
#' @return URL-encoded body string
#' @keywords internal
build_dd_tapr_body <- function(end_year, prgopt, level, tapr, sumlev, dsname, keys) {

  key_params <- paste0("key=", keys, collapse = "&")

  paste0(
    "_service=marykay",
    "&_program=perfrept.perfmast.sas",
    "&prgopt=", prgopt,
    "&ccyy=", end_year,
    "&level=", level,
    "&tapr=", tapr,
    "&dsname=", dsname,
    "&sumlev=", sumlev,
    "&id=",
    "&_debug=0",
    "&", key_params
  )
}


#' POST to dd_tapr endpoint and parse TSV response
#'
#' @param url Base broker URL
#' @param body URL-encoded POST body
#' @param label Label for error messages
#' @param end_year School year end (for error messages)
#' @return Data frame
#' @keywords internal
post_dd_tapr <- function(url, body, label, end_year) {

  tname <- tempfile(
    pattern = paste0("tea_dd_tapr_"),
    tmpdir = tempdir(),
    fileext = ".tsv"
  )

  tryCatch({
    response <- httr::POST(
      url,
      body = body,
      encode = "raw",
      httr::content_type("application/x-www-form-urlencoded"),
      httr::write_disk(tname, overwrite = TRUE),
      httr::timeout(300)
    )

    if (httr::http_error(response)) {
      stop(paste("HTTP error:", httr::status_code(response)))
    }

    # Check for error page
    first_lines <- readLines(tname, n = 5, warn = FALSE)
    if (any(grepl("Please return to TEA|error|<!DOCTYPE", first_lines, ignore.case = TRUE))) {
      stop(paste("TEA returned error page for", label, "year", end_year))
    }

  }, error = function(e) {
    stop(paste("Failed to download", label, "data for year", end_year,
               "\nError:", e$message))
  })

  # dd_tapr returns TSV with two header rows:
  # Row 1: human-readable labels (skip)
  # Row 2: column codes (use as names)
  df <- readr::read_tsv(
    tname,
    skip = 1,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )

  unlink(tname)

  df
}


#' Download enrollment data from TEA's ad-hoc reporting system
#'
#' For years where TAPR data isn't available yet, TEA's ad-hoc Student
#' Enrollment Reports provide ethnicity and grade breakdowns. This function
#' downloads ethnicity and grade data separately, pivots to wide format,
#' and merges into a single data frame matching the TAPR column structure.
#'
#' Note: The ad-hoc system does NOT provide special population data
#' (economically disadvantaged, LEP, special education). Those columns
#' will be NA for years using this download path.
#'
#' @param end_year School year end (e.g., 2026 for 2025-26)
#' @param sumlev Summary level: "C" for campus, "D" for district
#' @return Data frame with columns matching TAPR wide format
#' @keywords internal
download_adhoc_enrollment <- function(end_year, sumlev) {

  # Two-digit year code used by the ad-hoc system
  year_code <- end_year %% 100

  if (sumlev == "C") {
    selsumm <- "sc"
    level_label <- "campus"
  } else {
    selsumm <- "sd"
    level_label <- "district"
  }

  message(paste0("  Downloading ", level_label, " data (ad-hoc system)..."))

  # Download ethnicity data
  message("    Fetching ethnicity breakdown...")
  eth_df <- download_adhoc_file(year_code, selsumm, "e ")

  # Download grade data
  message("    Fetching grade breakdown...")
  grade_df <- download_adhoc_file(year_code, selsumm, "g ")

  # Pivot ethnicity data to wide format
  eth_wide <- pivot_adhoc_ethnicity(eth_df, sumlev)

  # Pivot grade data to wide format
  grade_wide <- pivot_adhoc_grades(grade_df, sumlev)

  # Merge ethnicity and grade data by ID
  id_col <- if (sumlev == "C") "CAMPUS" else "DISTRICT"

  if (id_col %in% names(eth_wide) && id_col %in% names(grade_wide)) {
    # Remove duplicate columns from grade_wide before joining
    grade_only_cols <- setdiff(names(grade_wide), names(eth_wide))
    grade_for_join <- grade_wide[, c(id_col, grade_only_cols), drop = FALSE]
    df <- dplyr::left_join(eth_wide, grade_for_join, by = id_col)
  } else {
    df <- eth_wide
  }

  df
}


#' Download a single ad-hoc enrollment file from TEA
#'
#' @param year_code Two-digit year code (e.g., 26 for 2025-26)
#' @param selsumm Report type (e.g., "sc" for statewide campus)
#' @param grouping Grouping variable code (e.g., "e " for ethnicity)
#' @return Data frame parsed from CSV response
#' @keywords internal
download_adhoc_file <- function(year_code, selsumm, grouping) {

  url <- paste0(
    "https://rptsvr1.tea.texas.gov/cgi/sas/broker?",
    "_service=marykay",
    "&_program=adhoc.addispatch.sas",
    "&major=st&minor=e",
    "&endyear=", year_code,
    "&selsumm=", selsumm,
    "&format=C",
    "&charsln=120&linespg=60",
    "&loop=1&countykey=&oldnew=new",
    "&_debug=0&key=",
    "&grouping=", utils::URLencode(grouping, reserved = TRUE)
  )

  tname <- tempfile(
    pattern = "tea_adhoc_",
    tmpdir = tempdir(),
    fileext = ".csv"
  )

  tryCatch({
    response <- httr::GET(
      url,
      httr::write_disk(tname, overwrite = TRUE),
      httr::timeout(300)
    )

    if (httr::http_error(response)) {
      stop(paste("HTTP error:", httr::status_code(response)))
    }

    # Check for error page
    first_lines <- readLines(tname, n = 5, warn = FALSE)
    if (any(grepl("<!DOCTYPE|<html|error", first_lines, ignore.case = TRUE))) {
      stop("TEA returned an error page")
    }

  }, error = function(e) {
    stop(paste("Failed to download ad-hoc enrollment data:",
               "\nError:", e$message))
  })

  # Read file content and find the CSV header line
  all_lines <- readLines(tname, warn = FALSE)
  unlink(tname)

  header_idx <- grep("^\"YEAR\"", all_lines)
  if (length(header_idx) == 0) {
    stop("Could not find CSV header in ad-hoc response")
  }

  csv_text <- paste(all_lines[header_idx:length(all_lines)], collapse = "\n")

  readr::read_csv(
    I(csv_text),
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )
}


#' Pivot ad-hoc ethnicity data to wide format matching TAPR columns
#'
#' @param df Long-format data frame from ad-hoc ethnicity download
#' @param sumlev "C" for campus, "D" for district
#' @return Wide data frame with TAPR-compatible column names
#' @keywords internal
pivot_adhoc_ethnicity <- function(df, sumlev) {

  prefix <- sumlev  # "C" or "D"
  id_col <- if (sumlev == "C") "CAMPUS" else "DISTRICT"

  # Map ad-hoc ethnicity names to TAPR column suffixes
  eth_map <- c(
    "Asian" = paste0(prefix, "PETASIC"),
    "Black or African American" = paste0(prefix, "PETBLAC"),
    "Hispanic/Latino" = paste0(prefix, "PETHISC"),
    "White" = paste0(prefix, "PETWHIC"),
    "American Indian or Alaska Nat" = paste0(prefix, "PETINDC"),
    "Native Hawaiian/Other Pacific" = paste0(prefix, "PETPCIC"),
    "Two or more races" = paste0(prefix, "PETTWOC")
  )

  # Clean enrollment values (handle suppression markers like "<10")
  df$ENROLLMENT_NUM <- suppressWarnings(as.numeric(
    gsub("[^0-9.-]", "", df$ENROLLMENT)
  ))

  # Compute total per entity by summing ethnic groups
  # (handles suppressed values by using na.rm)
  totals <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_col))) |>
    dplyr::summarise(
      total_enr = sum(.data$ENROLLMENT_NUM, na.rm = TRUE),
      .groups = "drop"
    )

  # Map ethnicity to TAPR column name and filter to known groups
  df$tapr_col <- eth_map[df$ETHNICITY]
  df_mapped <- df[!is.na(df$tapr_col), ]

  # Determine which reference columns are available
  ref_cols <- intersect(
    c("REGION", "COUNTY NAME", "DISTRICT", "DISTRICT NAME",
      "CAMPUS", "CAMPUS NAME", "CHARTER STATUS"),
    names(df_mapped)
  )

  # Get unique reference info per entity
  ref_info <- df_mapped |>
    dplyr::select(dplyr::all_of(ref_cols)) |>
    dplyr::distinct()

  # Pivot ethnicity counts to wide
  wide_eth <- df_mapped |>
    dplyr::select(dplyr::all_of(c(id_col, "tapr_col", "ENROLLMENT_NUM"))) |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(id_col),
      names_from = "tapr_col",
      values_from = "ENROLLMENT_NUM",
      values_fn = sum
    )

  # Add total column
  total_col_name <- paste0(prefix, "PETALLC")
  wide_eth <- dplyr::left_join(wide_eth, totals, by = id_col)
  wide_eth[[total_col_name]] <- wide_eth$total_enr
  wide_eth$total_enr <- NULL

  # Join reference info
  result <- dplyr::left_join(ref_info, wide_eth, by = id_col)

  # Rename reference columns to match TAPR convention
  rename_map <- c(
    "COUNTY NAME" = "CNTYNAME",
    "DISTRICT NAME" = "DISTNAME",
    "CAMPUS NAME" = "CAMPNAME"
  )

  for (old_name in names(rename_map)) {
    if (old_name %in% names(result)) {
      names(result)[names(result) == old_name] <- rename_map[old_name]
    }
  }

  # Convert charter status to Y/N flag
  charter_col <- paste0(prefix, "FLCHART")
  if ("CHARTER STATUS" %in% names(result)) {
    result[[charter_col]] <- ifelse(
      result[["CHARTER STATUS"]] == "OPEN ENROLLMENT CHARTER", "Y", "N"
    )
    result[["CHARTER STATUS"]] <- NULL
  }

  result
}


#' Pivot ad-hoc grade data to wide format matching TAPR columns
#'
#' @param df Long-format data frame from ad-hoc grade download
#' @param sumlev "C" for campus, "D" for district
#' @return Wide data frame with TAPR-compatible grade column names
#' @keywords internal
pivot_adhoc_grades <- function(df, sumlev) {

  prefix <- sumlev  # "C" or "D"
  id_col <- if (sumlev == "C") "CAMPUS" else "DISTRICT"

  # Map ad-hoc grade names to TAPR column names
  grade_map <- c(
    "Early Education" = paste0(prefix, "PETGEEC"),
    "Pre-kindergarten" = paste0(prefix, "PETGPKC"),
    "Kindergarten" = paste0(prefix, "PETGKNC"),
    "Grade 1" = paste0(prefix, "PETG01C"),
    "Grade 2" = paste0(prefix, "PETG02C"),
    "Grade 3" = paste0(prefix, "PETG03C"),
    "Grade 4" = paste0(prefix, "PETG04C"),
    "Grade 5" = paste0(prefix, "PETG05C"),
    "Grade 6" = paste0(prefix, "PETG06C"),
    "Grade 7" = paste0(prefix, "PETG07C"),
    "Grade 8" = paste0(prefix, "PETG08C"),
    "Grade 9" = paste0(prefix, "PETG09C"),
    "Grade 10" = paste0(prefix, "PETG10C"),
    "Grade 11" = paste0(prefix, "PETG11C"),
    "Grade 12" = paste0(prefix, "PETG12C")
  )

  # Clean enrollment values
  df$ENROLLMENT_NUM <- suppressWarnings(as.numeric(
    gsub("[^0-9.-]", "", df$ENROLLMENT)
  ))

  # Map grade to TAPR column name
  df$tapr_col <- grade_map[df$GRADE]
  df_mapped <- df[!is.na(df$tapr_col), ]

  # Pivot to wide
  wide_grade <- df_mapped |>
    dplyr::select(dplyr::all_of(c(id_col, "tapr_col", "ENROLLMENT_NUM"))) |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(id_col),
      names_from = "tapr_col",
      values_from = "ENROLLMENT_NUM",
      values_fn = sum
    )

  wide_grade
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
    # Note: dd_tapr (2024+) uses NT-prefix names for econ_disadv and LEP
    econ_disadv = c("CPETECOC", "DPETECOC", "CPNTECOC", "DPNTECOC"),
    lep = c("CPETLEPC", "DPETLEPC", "CPNTLEPC", "DPNTLEPC"),
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
