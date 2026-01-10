# ==============================================================================
# STAAR Data Processing Functions
# ==============================================================================
#
# This file contains functions for processing raw STAAR data into a
# standardized format across years (2018, 2021-2022, 2023).
#
# Key challenges:
# - 2018: Uses r_ prefix, 5 LEP codes, no 504 indicators
# - 2021-2022: Uses RLA_ prefix, 8 LEP codes, with 504 indicators
# - 2023: Uses RLA_ prefix, same as 2021-2022
#
# This function normalizes column names and selects key metrics for analysis.
#
# ==============================================================================

#' Process raw STAAR data
#'
#' Standardizes raw STAAR data across years by normalizing column names and
#' selecting key metrics. Handles schema changes between 2018, 2021-2022, and 2023.
#'
#' @param raw_data Raw data frame from get_raw_staar()
#' @param year Assessment year
#' @param level "district" or "campus"
#' @param grade Grade level
#' @return Processed data frame with standardized columns
#' @export
process_staar <- function(raw_data, year, level, grade) {

  # Make a copy to avoid modifying original
  df <- raw_data

  # Normalize column names based on year
  if (year == 2018 || year %in% c(2021, 2022)) {
    # 2018, 2021-2022 use r_ prefix instead of RLA_
    names(df) <- gsub("^r_", "RLA_", names(df))

    # 2018 has 5 LEP codes, missing 3: lept, lepr, lepe
    # Add missing columns with NA if needed (only for 2018)
    if (year == 2018) {
      required_lep_cols <- c("RLA_lept_d", "RLA_lepr_d", "RLA_lepe_d",
                             "RLA_lept_unsatgl_nm", "RLA_lepr_unsatgl_nm", "RLA_lepe_unsatgl_nm",
                             "RLA_lept_approgl_nm", "RLA_lepr_approgl_nm", "RLA_lepe_approgl_nm",
                             "RLA_lept_meetsgl_nm", "RLA_lepr_meetsgl_nm", "RLA_lepe_meetsgl_nm",
                             "RLA_lept_mastrgl_nm", "RLA_lepr_mastrgl_nm", "RLA_lepe_mastrgl_nm",
                             "m_lept_d", "m_lepr_d", "m_lepe_d",
                             "m_lept_unsatgl_nm", "m_lepr_unsatgl_nm", "m_lepe_unsatgl_nm",
                             "m_lept_approgl_nm", "m_lepr_approgl_nm", "m_lepe_approgl_nm",
                             "m_lept_meetsgl_nm", "m_lepr_meetsgl_nm", "m_lepe_meetsgl_nm",
                             "m_lept_mastrgl_nm", "m_lepr_mastrgl_nm", "m_lepe_mastrgl_nm")

      for (col in required_lep_cols) {
        if (!col %in% names(df)) {
          df[[col]] <- NA
        }
      }

      # 2018 also lacks 504 columns
      required_504_cols <- c("RLA_y504_d", "RLA_n504_d", "RLA_v504_d",
                             "m_y504_d", "m_n504_d", "m_v504_d")

      for (col in required_504_cols) {
        if (!col %in% names(df)) {
          df[[col]] <- NA
        }
      }
    }
  }

  # Now extract and standardize key columns
  # Identify entity column
  if (level == "district") {
    entity_col <- "DISTRICT"
    name_col <- "DNAME"
  } else {
    entity_col <- "CAMPUS"
    name_col <- "CNAME"
  }

  # Build standardized data frame
  # Start with identifier columns
  result <- data.frame(
    district_id = as.character(df[[entity_col]]),
    year = as.integer(df$YEAR) + 2000,  # Convert 2-digit year to 4-digit
    region = as.integer(df$REGION),
    district_name = if (level == "district") as.character(df[[name_col]]) else NA,
    campus_id = if (level == "campus") as.character(df[[entity_col]]) else NA,
    campus_name = if (level == "campus") as.character(df[[name_col]]) else NA,
    grade = sprintf("%02d", grade),
    stringsAsFactors = FALSE
  )

  # RLA (Reading/Language Arts) metrics - all students
  # Column names are: RLA_all_d (total), RLA_all_unsatgl_nm, RLA_all_approgl_nm, etc.
  rla_prefix <- "RLA_all_"

  result$rla_all_total <- safe_numeric_staar(df[[paste0(rla_prefix, "d")]])
  result$rla_all_did_not_meet <- safe_numeric_staar(df[[paste0(rla_prefix, "unsatgl_nm")]])
  result$rla_all_approaches <- safe_numeric_staar(df[[paste0(rla_prefix, "approgl_nm")]])
  result$rla_all_meets <- safe_numeric_staar(df[[paste0(rla_prefix, "meetsgl_nm")]])
  result$rla_all_masters <- safe_numeric_staar(df[[paste0(rla_prefix, "mastrgl_nm")]])

  # Math metrics - all students
  # Column names are: m_all_d (total), m_all_unsatgl_nm, m_all_approgl_nm, etc.
  math_prefix <- "m_all_"

  result$math_all_total <- safe_numeric_staar(df[[paste0(math_prefix, "d")]])
  result$math_all_did_not_meet <- safe_numeric_staar(df[[paste0(math_prefix, "unsatgl_nm")]])
  result$math_all_approaches <- safe_numeric_staar(df[[paste0(math_prefix, "approgl_nm")]])
  result$math_all_meets <- safe_numeric_staar(df[[paste0(math_prefix, "meetsgl_nm")]])
  result$math_all_masters <- safe_numeric_staar(df[[paste0(math_prefix, "mastrgl_nm")]])

  # Convert to wide format with subject as rows
  result_long <- reshape_to_long_format(result)

  return(result_long)
}

#' Extract STAAR metrics for a specific subject and student group
#'
#' @param df Raw STAAR data frame
#' @param subject_prefix Subject column prefix ("RLA_" or "m_")
#' @param group Student group ("all", "sexm", "ethh", etc.)
#' @return List with total and performance level counts
#' @keywords internal
extract_staar_subject <- function(df, subject_prefix, group) {

  prefix <- paste0(subject_prefix, group, "_")

  # Extract total tested
  total_col <- paste0(prefix, "d")
  total <- safe_numeric_staar(df[[total_col]])

  # Extract performance levels
  did_not_meet <- safe_numeric_staar(df[[paste0(prefix, "unsatgl_nm")]])
  approaches <- safe_numeric_staar(df[[paste0(prefix, "approgl_nm")]])
  meets <- safe_numeric_staar(df[[paste0(prefix, "meetsgl_nm")]])
  masters <- safe_numeric_staar(df[[paste0(prefix, "mastrgl_nm")]])

  list(
    total = total,
    did_not_meet = did_not_meet,
    approaches = approaches,
    meets = meets,
    masters = masters
  )
}

#' Safe numeric conversion for STAAR data
#'
#' Handles suppressed values (blank, "*", "-") and converts to NA.
#'
#' @param x Character vector to convert
#' @return Numeric vector
#' @keywords internal
safe_numeric_staar <- function(x) {
  if (is.null(x)) return(NA_real_)

  x <- as.character(x)

  # Replace suppressed/invalid values with NA
  x[x %in% c("", "*", "-", ".", "NA", "NULL")] <- NA

  # Remove commas and convert
  x <- gsub(",", "", x)
  as.numeric(x)
}

#' Reshape wide format to long format with subject as rows
#'
#' @param wide_data Wide format data frame with separate RLA and Math columns
#' @return Long format data frame with subject column
#' @keywords internal
reshape_to_long_format <- function(wide_data) {

  # Select RLA columns
  rla_cols <- grep("^rla_", names(wide_data), value = TRUE)

  # Select Math columns
  math_cols <- grep("^math_", names(wide_data), value = TRUE)

  # Get identifier columns (not starting with rla_ or math_)
  id_cols <- setdiff(names(wide_data), c(rla_cols, math_cols))

  # Extract RLA row
  rla_data <- wide_data[, c(id_cols, rla_cols), drop = FALSE]
  names(rla_data) <- gsub("^rla_", "", names(rla_data))
  rla_data$subject <- "RLA"

  # Extract Math row
  math_data <- wide_data[, c(id_cols, math_cols), drop = FALSE]
  names(math_data) <- gsub("^math_", "", names(math_data))
  math_data$subject <- "Math"

  # Rename all_total to all_d (to match expected test output)
  # Actually, let's keep it as all_total for clarity

  # Bind rows
  result <- rbind(rla_data, math_data)

  # Reorder columns
  result <- result[, c(id_cols, "subject",
                       "all_total", "all_did_not_meet", "all_approaches",
                       "all_meets", "all_masters")]

  return(result)
}
