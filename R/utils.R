# ==============================================================================
# Utility Functions
# ==============================================================================

#' @importFrom rlang .data
NULL


#' Get available years for Texas enrollment data
#'
#' Returns metadata about the range of school years for which enrollment data
#' is available from the Texas Education Agency.
#'
#' @return A list with components:
#'   \describe{
#'     \item{min_year}{Earliest available school year end (1997)}
#'     \item{max_year}{Latest available school year end (2024)}
#'     \item{description}{Human-readable description of the data availability}
#'   }
#' @export
#' @examples
#' years <- get_available_years()
#' years$min_year
#' years$max_year
#' years$description
get_available_years <- function() {
  list(
    min_year = 1997L,
    max_year = 2024L,
    description = "Texas enrollment data from TEA is available for school years 1996-97 through 2023-24 (end years 1997-2024). Data sources: AEIS CGI (1997-2002), AEIS SAS (2003-2012), TAPR (2013-2024)"
  )
}
