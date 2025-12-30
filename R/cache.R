# ==============================================================================
# Caching Functions
# ==============================================================================
#
# This file contains functions for caching downloaded data locally to avoid
# repeated downloads from TEA.
#
# ==============================================================================

#' Get cache directory path
#'
#' @return Path to cache directory
#' @keywords internal
get_cache_dir <- function() {

  # TODO: Implement get_cache_dir
  #
  # Use rappdirs::user_cache_dir("txschooldata") for cross-platform cache
  # Create directory if it doesn't exist

  cache_dir <- rappdirs::user_cache_dir("txschooldata")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  cache_dir
}


#' Get cache file path for given year and type
#'
#' @param end_year School year end
#' @param type Data type ("tidy" or "wide")
#' @return Full path to cache file
#' @keywords internal
get_cache_path <- function(end_year, type) {

  # TODO: Implement get_cache_path
  #
  # Return path like: {cache_dir}/{type}_{end_year}.rds

  file.path(get_cache_dir(), paste0(type, "_", end_year, ".rds"))
}


#' Check if cached data exists
#'
#' @param end_year School year end
#' @param type Data type ("tidy" or "wide")
#' @return TRUE if cache exists, FALSE otherwise
#' @keywords internal
cache_exists <- function(end_year, type) {

  # TODO: Implement cache_exists

  file.exists(get_cache_path(end_year, type))
}


#' Read data from cache
#'
#' @param end_year School year end
#' @param type Data type ("tidy" or "wide")
#' @return Cached data frame
#' @keywords internal
read_cache <- function(end_year, type) {

  # TODO: Implement read_cache

  readRDS(get_cache_path(end_year, type))
}


#' Write data to cache
#'
#' @param df Data frame to cache
#' @param end_year School year end
#' @param type Data type ("tidy" or "wide")
#' @keywords internal
write_cache <- function(df, end_year, type) {

  # TODO: Implement write_cache

  saveRDS(df, get_cache_path(end_year, type))
}


#' Clear cached data
#'
#' @param end_year Optional school year to clear. If NULL, clears all years.
#' @param type Optional data type to clear. If NULL, clears all types.
#' @export
clear_cache <- function(end_year = NULL, type = NULL) {

  # TODO: Implement clear_cache
  #
  # If end_year and type specified, clear that specific file
  # If only end_year, clear all types for that year
  # If only type, clear all years for that type
  # If neither, clear entire cache

  cache_dir <- get_cache_dir()

  if (!is.null(end_year) && !is.null(type)) {
    # Clear specific file
    unlink(get_cache_path(end_year, type))
  } else if (!is.null(end_year)) {
    # Clear all types for year
    files <- list.files(cache_dir, pattern = paste0("_", end_year, "\\.rds$"), full.names = TRUE)
    unlink(files)
  } else if (!is.null(type)) {
    # Clear all years for type
    files <- list.files(cache_dir, pattern = paste0("^", type, "_"), full.names = TRUE)
    unlink(files)
  } else {
    # Clear all
    files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
    unlink(files)
  }

  invisible(TRUE)
}
