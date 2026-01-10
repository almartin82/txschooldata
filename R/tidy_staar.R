# ==============================================================================
# STAAR Data Tidying Functions
# ==============================================================================
#
# This file contains functions for converting processed STAAR data into
# tidy long format suitable for analysis and visualization.
#
# Transforms wide format (one row per district-subject) to long format
# (one row per district-subject-performance_level-student_group).
#
# ==============================================================================

#' Tidy processed STAAR data
#'
#' Converts processed STAAR data from wide format to tidy long format.
#' Each row represents a unique combination of entity, subject, performance level,
#' and student group with the corresponding count.
#'
#' @param processed_data Processed data frame from process_staar()
#' @return Tidy data frame in long format
#' @keywords internal
tidy_staar <- function(processed_data) {

  # Identify identifier columns
  id_cols <- c("district_id", "year", "region", "district_name",
               "campus_id", "campus_name", "grade", "subject")

  # Performance level columns (excluding all_total - that's the total count, not a level)
  perf_cols <- c("all_did_not_meet", "all_approaches",
                 "all_meets", "all_masters")

  # Manual pivot since column names don't follow a simple pattern
  # Create mapping from column names to performance levels
  perf_mapping <- c(
    "all_did_not_meet" = "did_not_meet",
    "all_approaches" = "approaches",
    "all_meets" = "meets",
    "all_masters" = "masters"
  )

  # Use tidyr::pivot_longer to pivot all performance columns
  tidy <- tidyr::pivot_longer(
    processed_data,
    cols = dplyr::all_of(perf_cols),
    names_to = "metric",
    values_to = "count",
    values_drop_na = TRUE
  )

  # Add performance level and student group
  tidy$performance_level <- perf_mapping[ tidy$metric ]
  tidy$student_group <- "all"

  # Remove metric column
  tidy$metric <- NULL

  # Filter out NA and zero counts
  tidy <- tidy[!is.na(tidy$count) & tidy$count > 0, ]

  # Reorder columns
  tidy <- tidy[, c(id_cols, "performance_level", "student_group", "count")]

  # Sort
  tidy <- dplyr::arrange(tidy, year, subject, district_id, campus_id, performance_level, student_group)

  # Add aggregation flags for convenience
  has_campus <- "campus_id" %in% names(processed_data)

  if (has_campus) {
    tidy$is_district <- !is.na(tidy$district_id) & is.na(tidy$campus_id)
    tidy$is_campus <- !is.na(tidy$campus_id)
  } else {
    tidy$is_district <- !is.na(tidy$district_id)
    tidy$is_campus <- FALSE
  }

  tidy
}
