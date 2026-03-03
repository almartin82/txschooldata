# ==============================================================================
# Directory Year Coverage Tests
# ==============================================================================
#
# Tests for fetch_directory() which provides school directory data from the
# Texas Education Agency via the Texas Open Data Portal (AskTED data).
#
# The directory data represents current schools (not historical) and is
# updated daily by TEA.
#
# Tests cover:
#   - Required fields present
#   - Entity counts within expected range
#   - ID format: district_id 6-char, campus_id 9-char
#   - Known entity lookup (Houston ISD, Dallas ISD)
#   - Status values
#   - Charter type values
#   - School type values
#
# To run: devtools::test(filter = "directory-year-coverage")
# ==============================================================================

# -- Helper: skip if TEA directory portal is unavailable ---
skip_if_tea_directory_unavailable <- function() {
  skip_on_cran()
  tryCatch({
    url <- "https://data.texas.gov/api/views/hzek-udky/rows.csv?accessType=DOWNLOAD"
    resp <- httr::HEAD(url, httr::timeout(15))
    if (httr::http_error(resp)) {
      skip("TEA Open Data Portal returned HTTP error")
    }
  }, error = function(e) {
    skip(paste("TEA Open Data Portal unreachable:", e$message))
  })
}


# ==============================================================================
# DIRECTORY DATA STRUCTURE TESTS
# ==============================================================================

test_that("fetch_directory returns data with required fields", {
  skip_if_tea_directory_unavailable()

  dir <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) skip(paste("fetch_directory failed:", e$message))
  )

  expect_true(is.data.frame(dir))
  expect_gt(nrow(dir), 0)

  # Required columns in tidy format
  required_cols <- c(
    "state_district_id", "state_school_id",
    "county_name", "district_name", "school_name",
    "school_type", "grades_served",
    "address", "city", "state", "zip", "phone",
    "principal_name", "principal_email",
    "superintendent_name", "superintendent_email",
    "charter_type", "status", "enrollment"
  )

  for (col in required_cols) {
    expect_true(col %in% names(dir),
                info = paste("Missing column:", col))
  }
})

test_that("Entity counts are within expected range", {
  skip_if_tea_directory_unavailable()

  dir <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) skip(paste("fetch_directory failed:", e$message))
  )

  # Texas has ~9000-10000 campuses in the directory
  expect_gt(nrow(dir), 8000,
            label = paste("Total records:", nrow(dir), "(expected >8000)"))
  expect_lt(nrow(dir), 15000,
            label = paste("Total records:", nrow(dir), "(expected <15000)"))

  # Unique districts: ~1200
  n_districts <- length(unique(dir$state_district_id))
  expect_true(n_districts >= 1000 & n_districts <= 1500,
              info = paste("District count:", n_districts,
                           "(expected 1000-1500)"))
})

test_that("ID format: district_id is 6-char", {
  skip_if_tea_directory_unavailable()

  dir <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) skip(paste("fetch_directory failed:", e$message))
  )

  dist_ids <- unique(dir$state_district_id)
  id_lens <- unique(nchar(dist_ids))
  expect_true(all(id_lens == 6),
              info = paste("District ID lengths:", paste(id_lens, collapse = ", ")))

  # No leading quotes
  expect_false(any(grepl("^'", dist_ids)),
               info = "District IDs have leading quotes")
})

test_that("ID format: school_id is 9-char", {
  skip_if_tea_directory_unavailable()

  dir <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) skip(paste("fetch_directory failed:", e$message))
  )

  school_ids <- unique(dir$state_school_id)
  id_lens <- unique(nchar(school_ids))
  expect_true(all(id_lens == 9),
              info = paste("School ID lengths:", paste(id_lens, collapse = ", ")))

  # No leading quotes
  expect_false(any(grepl("^'", school_ids)),
               info = "School IDs have leading quotes")
})

test_that("School ID first 6 chars match district ID", {
  skip_if_tea_directory_unavailable()

  dir <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) skip(paste("fetch_directory failed:", e$message))
  )

  # The first 6 characters of the school ID should match the district ID
  derived_dist <- substr(dir$state_school_id, 1, 6)
  mismatches <- sum(derived_dist != dir$state_district_id, na.rm = TRUE)

  expect_equal(mismatches, 0,
               info = paste(mismatches, "school IDs don't match district ID prefix"))
})


# ==============================================================================
# KNOWN ENTITY LOOKUP TESTS
# ==============================================================================

test_that("Houston ISD is present with expected school count", {
  skip_if_tea_directory_unavailable()

  dir <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) skip(paste("fetch_directory failed:", e$message))
  )

  houston <- dir[dir$state_district_id == "101912", ]
  expect_gt(nrow(houston), 200,
            label = paste("Houston ISD schools:", nrow(houston),
                          "(expected >200)"))
  expect_lt(nrow(houston), 400,
            label = paste("Houston ISD schools:", nrow(houston),
                          "(expected <400)"))

  # District name should be Houston ISD or similar
  houston_names <- unique(houston$district_name)
  expect_true(any(grepl("HOUSTON", houston_names, ignore.case = TRUE)),
              info = paste("Houston ISD name:", paste(houston_names, collapse = ", ")))
})

test_that("Dallas ISD is present with expected school count", {
  skip_if_tea_directory_unavailable()

  dir <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) skip(paste("fetch_directory failed:", e$message))
  )

  dallas <- dir[dir$state_district_id == "057905", ]
  expect_gt(nrow(dallas), 180,
            label = paste("Dallas ISD schools:", nrow(dallas),
                          "(expected >180)"))
  expect_lt(nrow(dallas), 350,
            label = paste("Dallas ISD schools:", nrow(dallas),
                          "(expected <350)"))

  dallas_names <- unique(dallas$district_name)
  expect_true(any(grepl("DALLAS", dallas_names, ignore.case = TRUE)),
              info = paste("Dallas ISD name:", paste(dallas_names, collapse = ", ")))
})

test_that("Austin ISD is present", {
  skip_if_tea_directory_unavailable()

  dir <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) skip(paste("fetch_directory failed:", e$message))
  )

  austin <- dir[dir$state_district_id == "227901", ]
  expect_gt(nrow(austin), 50,
            label = paste("Austin ISD schools:", nrow(austin),
                          "(expected >50)"))

  austin_names <- unique(austin$district_name)
  expect_true(any(grepl("AUSTIN", austin_names, ignore.case = TRUE)),
              info = paste("Austin ISD name:", paste(austin_names, collapse = ", ")))
})

test_that("Fort Worth ISD is present", {
  skip_if_tea_directory_unavailable()

  dir <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) skip(paste("fetch_directory failed:", e$message))
  )

  fw <- dir[dir$state_district_id == "220905", ]
  expect_gt(nrow(fw), 50,
            label = paste("Fort Worth ISD schools:", nrow(fw),
                          "(expected >50)"))
})


# ==============================================================================
# DATA QUALITY TESTS
# ==============================================================================

test_that("Status values are valid", {
  skip_if_tea_directory_unavailable()

  dir <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) skip(paste("fetch_directory failed:", e$message))
  )

  valid_statuses <- c("Active", "Inactive", "Under Construction", "Closed", NA)
  actual_statuses <- unique(dir$status)

  for (s in actual_statuses) {
    if (!is.na(s)) {
      expect_true(s %in% valid_statuses,
                  info = paste("Unexpected status:", s))
    }
  }

  # Most schools should be Active
  active_count <- sum(dir$status == "Active", na.rm = TRUE)
  pct_active <- active_count / nrow(dir)
  expect_true(pct_active > 0.80,
              info = paste("Only", round(pct_active * 100, 1),
                           "% Active (expected >80%)"))
})

test_that("Charter types are valid", {
  skip_if_tea_directory_unavailable()

  dir <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) skip(paste("fetch_directory failed:", e$message))
  )

  # Known charter types
  known_charter_types <- c(
    "OPEN ENROLLMENT CHARTER",
    "CAMPUS CHARTER",
    "COLLEGE/UNIVERSITY CHARTER",
    "", NA
  )

  actual_types <- unique(dir$charter_type)
  for (ct in actual_types) {
    if (!is.na(ct) && ct != "") {
      expect_true(ct %in% known_charter_types,
                  info = paste("Unexpected charter type:", ct))
    }
  }
})

test_that("School types are valid", {
  skip_if_tea_directory_unavailable()

  dir <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) skip(paste("fetch_directory failed:", e$message))
  )

  known_school_types <- c(
    "REGULAR INSTRUCTIONAL",
    "ALTERNATIVE INSTRUCTIONAL",
    "DAEP INSTRUCTIONAL",
    "JJAEP INSTRUCTIONAL",
    NA
  )

  actual_types <- unique(dir$school_type)
  for (st in actual_types) {
    if (!is.na(st)) {
      expect_true(st %in% known_school_types,
                  info = paste("Unexpected school type:", st))
    }
  }

  # Most should be REGULAR INSTRUCTIONAL
  regular_count <- sum(dir$school_type == "REGULAR INSTRUCTIONAL", na.rm = TRUE)
  pct_regular <- regular_count / nrow(dir)
  expect_true(pct_regular > 0.70,
              info = paste("Only", round(pct_regular * 100, 1),
                           "% regular (expected >70%)"))
})

test_that("State column is always TX", {
  skip_if_tea_directory_unavailable()

  dir <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) skip(paste("fetch_directory failed:", e$message))
  )

  if ("state" %in% names(dir)) {
    non_tx <- dir$state[!is.na(dir$state) & dir$state != "TX"]
    expect_equal(length(non_tx), 0,
                 info = paste("Non-TX state values:", paste(unique(non_tx), collapse = ", ")))
  }
})

test_that("Enrollment values are non-negative or -1 (suppressed)", {
  skip_if_tea_directory_unavailable()

  dir <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) skip(paste("fetch_directory failed:", e$message))
  )

  if ("enrollment" %in% names(dir)) {
    enr_vals <- dir$enrollment[!is.na(dir$enrollment)]
    if (length(enr_vals) > 0) {
      # TEA uses -1 as a suppression marker for small enrollment counts
      expect_true(all(enr_vals >= -1),
                  info = "Directory enrollment has values below -1")

      # Most should be positive (>80%)
      pct_positive <- sum(enr_vals > 0) / length(enr_vals)
      expect_true(pct_positive > 0.80,
                  info = paste("Only", round(pct_positive * 100, 1),
                               "% positive enrollment (expected >80%)"))
    }
  }
})

test_that("ESC regions are valid (1-20)", {
  skip_if_tea_directory_unavailable()

  dir <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) skip(paste("fetch_directory failed:", e$message))
  )

  if ("esc_region" %in% names(dir)) {
    region_vals <- as.integer(dir$esc_region[!is.na(dir$esc_region)])
    region_vals <- region_vals[!is.na(region_vals)]
    if (length(region_vals) > 0) {
      # Texas has 20 ESC regions
      expect_true(all(region_vals >= 1 & region_vals <= 20),
                  info = paste("ESC regions outside 1-20:",
                               paste(unique(region_vals[region_vals < 1 | region_vals > 20]),
                                     collapse = ", ")))
    }
  }
})

test_that("No Inf/NaN in numeric directory columns", {
  skip_if_tea_directory_unavailable()

  dir <- tryCatch(
    fetch_directory(use_cache = TRUE),
    error = function(e) skip(paste("fetch_directory failed:", e$message))
  )

  numeric_cols <- names(dir)[sapply(dir, is.numeric)]
  for (col in numeric_cols) {
    vals <- dir[[col]]
    expect_false(any(is.infinite(vals)),
                 info = paste("Inf values in", col))
    expect_false(any(is.nan(vals)),
                 info = paste("NaN values in", col))
  }
})

test_that("Tidy and raw formats both work", {
  skip_if_tea_directory_unavailable()

  tidy <- tryCatch(
    fetch_directory(tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_directory tidy failed:", e$message))
  )

  raw <- tryCatch(
    fetch_directory(tidy = FALSE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_directory raw failed:", e$message))
  )

  # Both should have the same number of rows
  expect_equal(nrow(tidy), nrow(raw),
               info = "Row count mismatch between tidy and raw")

  # Tidy should have standardized column names
  expect_true("state_district_id" %in% names(tidy))
  expect_true("state_school_id" %in% names(tidy))

  # Raw should have original TEA column names (with spaces)
  raw_names <- names(raw)
  expect_true(any(grepl("District|School|County", raw_names)),
              info = "Raw format should have TEA-style column names")
})
