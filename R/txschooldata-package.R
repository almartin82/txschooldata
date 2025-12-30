#' txschooldata: Fetch and Process Texas School Data
#'
#' Downloads and processes school data from the Texas Education Agency (TEA).
#' Provides functions for fetching enrollment data from PEIMS (Public Education
#' Information Management System) via the TAPR (Texas Academic Performance
#' Reports) system and transforming it into tidy format for analysis.
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{fetch_enr}}}{Fetch enrollment data for a school year}
#'   \item{\code{\link{fetch_enr_multi}}}{Fetch enrollment data for multiple years}
#'   \item{\code{\link{tidy_enr}}}{Transform wide data to tidy (long) format}
#'   \item{\code{\link{id_enr_aggs}}}{Add aggregation level flags}
#'   \item{\code{\link{enr_grade_aggs}}}{Create grade-level aggregations}
#' }
#'
#' @section Cache functions:
#' \describe{
#'   \item{\code{\link{cache_status}}}{View cached data files}
#'   \item{\code{\link{clear_cache}}}{Remove cached data files}
#' }
#'
#' @section ID System:
#' Texas uses a hierarchical ID system:
#' \itemize{
#'   \item District IDs: 6 digits (e.g., 101912 = Austin ISD)
#'   \item Campus IDs: 9 digits (district ID + 3-digit campus number)
#' }
#'
#' @section Data Sources:
#' Data is sourced from the Texas Education Agency's TAPR system:
#' \itemize{
#'   \item TAPR: \url{https://tea.texas.gov/texas-schools/accountability/academic-accountability/performance-reporting/texas-academic-performance-reports}
#'   \item PEIMS: \url{https://tea.texas.gov/reports-and-data/school-data/peims-data}
#' }
#'
#' @docType package
#' @name txschooldata-package
#' @aliases txschooldata
#' @keywords internal
"_PACKAGE"

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL
