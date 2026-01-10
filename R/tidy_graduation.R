# ==============================================================================
# Graduation Rate Data Tidying Functions
# ==============================================================================
#
# This file contains functions for transforming graduation rate data from
# wide format to long (tidy) format with a subgroup column.
#
# ==============================================================================

#' Tidy graduation rate data
#'
#' Transforms wide graduation rate data to long format with subgroup column.
#'
#' @param df A wide data.frame of processed graduation rate data
#' @return A long data.frame of tidied graduation rate data
#' @export
#' @examples
#' \dontrun{
#' wide_data <- fetch_grad(2024, tidy = FALSE)
#' tidy_data <- tidy_grad(wide_data)
#' }
tidy_grad <- function(df) {

  # Invariant columns (identifiers that stay the same)
  invariants <- c(
    "class_year", "type",
    "district_id", "campus_id",
    "district_name", "campus_name",
    "county", "region"
  )
  invariants <- invariants[invariants %in% names(df)]

  # Graduation rate subgroups to tidy
  rate_subgroups <- c(
    "all_students_grad_rate",
    "white_grad_rate",
    "black_grad_rate",
    "hispanic_grad_rate",
    "asian_grad_rate",
    "native_american_grad_rate",
    "multiracial_grad_rate",
    "female_grad_rate",
    "male_grad_rate",
    "econ_disadv_grad_rate",
    "special_ed_grad_rate",
    "lep_grad_rate",
    "at_risk_grad_rate"
  )

  # Map to cleaner subgroup names
  subgroup_names <- c(
    "all_students_grad_rate" = "all_students",
    "white_grad_rate" = "white",
    "black_grad_rate" = "black",
    "hispanic_grad_rate" = "hispanic",
    "asian_grad_rate" = "asian",
    "native_american_grad_rate" = "native_american",
    "multiracial_grad_rate" = "multiracial",
    "female_grad_rate" = "female",
    "male_grad_rate" = "male",
    "econ_disadv_grad_rate" = "econ_disadv",
    "special_ed_grad_rate" = "special_ed",
    "lep_grad_rate" = "lep",
    "at_risk_grad_rate" = "at_risk"
  )

  # Only include subgroups that exist in the data
  rate_subgroups <- rate_subgroups[rate_subgroups %in% names(df)]

  # Transform to long format
  tidy_subgroups <- purrr::map_df(
    rate_subgroups,
    function(.x) {
      df |>
        dplyr::select(dplyr::all_of(c(invariants, .x))) |>
        dplyr::rename(grad_rate = dplyr::all_of(.x)) |>
        dplyr::mutate(subgroup = subgroup_names[.x]) |>
        dplyr::select(dplyr::all_of(c(invariants, "subgroup", "grad_rate")))
    }
  )

  # Add aggregation flags
  tidy_subgroups <- tidy_subgroups |>
    dplyr::mutate(
      is_state = type == "State",
      is_district = type == "District",
      is_campus = type == "Campus"
    )

  tidy_subgroups
}


#' Identify graduation rate aggregation levels
#'
#' Adds boolean flags to identify state, district, and campus level records.
#'
#' @param df Graduation rate dataframe, output of tidy_grad
#' @return data.frame with boolean aggregation flags
#' @export
#' @examples
#' \dontrun{
#' tidy_data <- fetch_grad(2024)
#' # Data already has aggregation flags via tidy_grad
#' table(tidy_data$is_state, tidy_data$is_district, tidy_data$is_campus)
#' }
id_grad_aggs <- function(df) {
  # Already added in tidy_grad, but kept for consistency with enrollment pattern
  df
}
