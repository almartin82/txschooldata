# ==============================================================================
# LIVE Pipeline Tests for Texas School Data
# ==============================================================================
#
# These tests verify each step of the data pipeline using LIVE network calls.
# NO MOCKS - the goal is to detect when TEA websites change.
#
# Test Categories:
# 1. URL Availability - Can we reach TEA servers?
# 2. File Download - Can we download actual data files?
# 3. File Parsing - Can we parse the downloaded content?
# 4. Column Structure - Are expected columns present?
# 5. Data Correctness - Is the data valid and complete?
# 6. Year Filtering - Do individual years work?
# 7. Aggregation - Do totals add up?
# 8. Output Fidelity - Does processing preserve data integrity?
#
# ==============================================================================

# Helper function to skip if offline
skip_if_offline <- function() {

  skip_on_cran()
  tryCatch({
    response <- httr::HEAD("https://www.google.com", httr::timeout(5))
    if (httr::http_error(response)) skip("No network connectivity")
  }, error = function(e) skip("No network connectivity"))
}

# Helper to skip if TEA SAS broker is not returning valid CSV data.
# The TEA server periodically returns HTML error pages or truncated data
# instead of CSV, which causes cascading test failures.
skip_if_tea_unavailable <- function() {
  skip_if_offline()
  tryCatch({
    test_url <- build_tapr_test_url(2024, "CSTUD", "IDENT")
    temp <- tempfile(fileext = ".csv")
    on.exit(unlink(temp), add = TRUE)
    response <- httr::GET(
      test_url,
      httr::write_disk(temp, overwrite = TRUE),
      httr::timeout(60)
    )
    if (httr::http_error(response)) {
      skip("TEA SAS broker returned HTTP error")
    }
    first_lines <- readLines(temp, n = 3, warn = FALSE)
    if (any(grepl("<html|<HTML|<!DOCTYPE", first_lines))) {
      skip("TEA SAS broker returned HTML error page instead of CSV")
    }
    if (file.info(temp)$size < 500) {
      skip("TEA SAS broker returned empty/truncated response")
    }
  }, error = function(e) {
    skip(paste("TEA SAS broker unreachable:", e$message))
  })
}

# Helper to build TAPR URL
build_tapr_test_url <- function(year, dsname = "CSTUD", keys = c("IDENT")) {
  sumlev <- if (dsname == "CSTUD") "C" else "D"
  id_param <- if (dsname == "CSTUD") "camp0=999999" else "dist0=999999"

  if (year >= 2024) {
    prgopt <- paste0(year, "/tapr/Basic%20Download/xplore/getdata.sas")
  } else {
    prgopt <- paste0(year, "/xplore/getdata.sas")
  }

  key_params <- paste0("key=", keys, collapse = "&")

  paste0(
    "https://rptsvr1.tea.texas.gov/cgi/sas/broker/", dsname, "?",
    "_service=marykay&year4=", year,
    "&prgopt=", prgopt,
    "&_program=perfrept.perfmast.sas&dsname=", dsname,
    "&sumlev=", sumlev,
    "&_debug=0&format=CSV&", id_param,
    "&_saveas=", dsname, "&datafmt=C",
    "&", key_params
  )
}

# Test-session cache for downloaded data (avoid hammering TEA server)
.test_cache <- new.env(parent = emptyenv())

# Helper to download and parse TEA data (cached within test session)
# Returns NULL and calls skip() if TEA server returns an error page or
# unexpected data instead of valid CSV.
download_tea_csv <- function(year, dsname = "CSTUD", keys = c("IDENT", "PET")) {
  # Create cache key
  cache_key <- paste(year, dsname, paste(sort(keys), collapse = "_"), sep = "_")

  # Check cache first

  if (exists(cache_key, envir = .test_cache)) {
    return(get(cache_key, envir = .test_cache))
  }

  # Add delay to avoid rate limiting (1 second between requests)
  Sys.sleep(1)

  url <- build_tapr_test_url(year, dsname, keys)

  temp_file <- tempfile(fileext = ".csv")

  response <- tryCatch(
    httr::GET(
      url,
      httr::write_disk(temp_file, overwrite = TRUE),
      httr::timeout(180)
    ),
    error = function(e) {
      unlink(temp_file)
      skip(paste("TEA download failed:", e$message))
    }
  )

  if (httr::http_error(response)) {
    unlink(temp_file)
    skip(paste("TEA HTTP error:", httr::status_code(response)))
  }

  # Check for empty or error response
  file_size <- file.info(temp_file)$size
  if (file_size < 100) {
    unlink(temp_file)
    skip("TEA returned empty or error response")
  }

  # Check for HTML error page
  first_lines <- readLines(temp_file, n = 3, warn = FALSE)
  if (any(grepl("<html|<HTML|<!DOCTYPE", first_lines))) {
    unlink(temp_file)
    skip("TEA returned HTML error page instead of CSV data")
  }

  df <- readr::read_csv(
    temp_file,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )

  unlink(temp_file)

  # Cache result
  assign(cache_key, df, envir = .test_cache)

  df
}

# ==============================================================================
# 1. URL AVAILABILITY TESTS
# ==============================================================================

test_that("TEA SAS broker base URL is reachable", {
  skip_if_offline()

  response <- tryCatch(
    httr::HEAD(
      "https://rptsvr1.tea.texas.gov/cgi/sas/broker",
      httr::timeout(30)
    ),
    error = function(e) skip(paste("TEA SAS broker unreachable:", e$message))
  )
  expect_equal(httr::status_code(response), 200)
})

test_that("TAPR campus data URL returns HTTP 200 for 2024", {
  skip_if_offline()

  url <- build_tapr_test_url(2024, "CSTUD", "IDENT")
  response <- tryCatch(
    httr::HEAD(url, httr::timeout(60)),
    error = function(e) skip(paste("TEA URL unreachable:", e$message))
  )
  expect_equal(httr::status_code(response), 200)
})

test_that("TAPR district data URL returns HTTP 200 for 2024", {
  skip_if_offline()

  url <- build_tapr_test_url(2024, "DSTUD", "IDENT")
  response <- tryCatch(
    httr::HEAD(url, httr::timeout(60)),
    error = function(e) skip(paste("TEA URL unreachable:", e$message))
  )
  expect_equal(httr::status_code(response), 200)
})

test_that("TAPR URL returns HTTP 200 for historical years (2013-2023)", {
  skip_if_offline()

  # Test a sample of years across the TAPR era
  test_years <- c(2013, 2018, 2023)

  for (year in test_years) {
    url <- build_tapr_test_url(year, "CSTUD", "IDENT")
    response <- tryCatch(
      httr::HEAD(url, httr::timeout(60)),
      error = function(e) skip(paste("TEA URL unreachable for year", year, ":", e$message))
    )
    expect_equal(
      httr::status_code(response), 200,
      info = paste("Failed for year", year)
    )
  }
})

test_that("AEIS SAS broker URL returns HTTP 200 for 2003-2012", {
  skip_if_offline()

  # Test AEIS era (uses different URL pattern)
  test_years <- c(2005, 2010, 2012)

  for (year in test_years) {
    url <- paste0(
      "https://rptsvr1.tea.texas.gov/cgi/sas/broker?",
      "_service=marykay&year4=", year,
      "&prgopt=", year, "/xplore/getdata.sas",
      "&_program=perfrept.perfmast.sas&dsname=cstud&sumlev=C",
      "&_debug=0&camp0=999999&_saveas=cstud&datafmt=C",
      "&key=PETALL"
    )

    response <- tryCatch(
      httr::HEAD(url, httr::timeout(60)),
      error = function(e) skip(paste("TEA AEIS URL unreachable for year", year, ":", e$message))
    )
    expect_equal(
      httr::status_code(response), 200,
      info = paste("AEIS failed for year", year)
    )
  }
})

# ==============================================================================
# 2. FILE DOWNLOAD TESTS
# ==============================================================================

test_that("Can download campus enrollment CSV for 2024", {
  skip_if_tea_unavailable()

  url <- build_tapr_test_url(2024, "CSTUD", c("IDENT", "PET"))

  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  response <- tryCatch(
    httr::GET(
      url,
      httr::write_disk(temp_file, overwrite = TRUE),
      httr::timeout(180)
    ),
    error = function(e) skip(paste("TEA download failed:", e$message))
  )

  expect_equal(httr::status_code(response), 200)

  # File should not be empty (at least 1KB)
  file_size <- file.info(temp_file)$size
  expect_true(file_size > 1000,
              label = paste("File size:", file_size, "bytes (expected >1000)"))

  # Check content type suggests CSV
  content_type <- httr::headers(response)$`content-type`
  expect_true(
    grepl("csv|comma|text", content_type, ignore.case = TRUE),
    label = paste("Content-type:", content_type)
  )
})

test_that("Downloaded file is not an HTML error page", {
  skip_if_tea_unavailable()

  # Use the full key set for consistency
  url <- build_tapr_test_url(2024, "CSTUD", c("IDENT", "PET", "PETG"))

  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  Sys.sleep(1)  # Rate limiting
  response <- tryCatch(
    httr::GET(url, httr::write_disk(temp_file), httr::timeout(180)),
    error = function(e) skip(paste("TEA download failed:", e$message))
  )

  if (httr::http_error(response)) {
    skip(paste("TEA returned HTTP error:", httr::status_code(response)))
  }

  # Read first few lines
  first_lines <- readLines(temp_file, n = 5, warn = FALSE)

  # Should not contain HTML tags
  has_html <- any(grepl("<html|<HTML|<!DOCTYPE|<head|<body", first_lines))
  expect_false(has_html, label = "File appears to be HTML error page")

  # Should contain expected column headers
  has_headers <- any(grepl("CAMPUS|DISTRICT", first_lines))
  expect_true(has_headers, label = "File should contain CAMPUS/DISTRICT headers")
})

# ==============================================================================
# 3. FILE PARSING TESTS
# ==============================================================================

test_that("Can parse campus CSV with readr", {
  skip_if_tea_unavailable()

  df <- download_tea_csv(2024, "CSTUD", c("IDENT", "PET"))

  expect_true(is.data.frame(df))
  expect_true(nrow(df) > 0, label = "Data frame should have rows")
  expect_true(ncol(df) > 0, label = "Data frame should have columns")
})

test_that("Parsed CSV has expected number of rows (sanity check)", {
  skip_if_tea_unavailable()

  # Use full key set to share cache with other tests
  df <- download_tea_csv(2024, "CSTUD", c("IDENT", "PET", "PETG"))

  # Texas has ~9000 campuses - should have at least 8000 rows
  expect_true(nrow(df) > 8000,
              label = paste("Got", nrow(df), "rows, expected >8000 campuses"))

  # Should have fewer than 15000 (sanity upper bound)
  expect_true(nrow(df) < 15000,
              label = paste("Got", nrow(df), "rows, expected <15000"))
})

# ==============================================================================
# 4. COLUMN STRUCTURE TESTS
# ==============================================================================

test_that("Campus data has required ID columns", {
  skip_if_tea_unavailable()

  # Use full key set to share cache with other tests
  df <- download_tea_csv(2024, "CSTUD", c("IDENT", "PET", "PETG"))

  # Required ID columns
  expect_true("CAMPUS" %in% names(df), label = "Should have CAMPUS column")
  expect_true("DISTRICT" %in% names(df), label = "Should have DISTRICT column")
  expect_true("CAMPNAME" %in% names(df), label = "Should have CAMPNAME column")
  expect_true("DISTNAME" %in% names(df), label = "Should have DISTNAME column")
})

test_that("Campus data has enrollment count columns", {
  skip_if_tea_unavailable()

  # Use full key set to share cache with other tests
  df <- download_tea_csv(2024, "CSTUD", c("IDENT", "PET", "PETG"))

  # Key enrollment columns (C prefix = Campus, PET = student membership)
  expect_true("CPETALLC" %in% names(df), label = "Should have CPETALLC (total enrollment)")
  expect_true("CPETHISC" %in% names(df), label = "Should have CPETHISC (Hispanic)")
  expect_true("CPETWHIC" %in% names(df), label = "Should have CPETWHIC (White)")
  expect_true("CPETBLAC" %in% names(df), label = "Should have CPETBLAC (Black)")
})

test_that("Campus data has grade level columns", {
  skip_if_tea_unavailable()

  # Use full key set to share cache with other tests
  df <- download_tea_csv(2024, "CSTUD", c("IDENT", "PET", "PETG"))

  # Grade level columns
  grade_cols <- c("CPETGPKC", "CPETGKNC", "CPETG01C", "CPETG05C",
                  "CPETG09C", "CPETG12C")

  for (col in grade_cols) {
    expect_true(col %in% names(df),
                label = paste("Should have grade column:", col))
  }
})

# ==============================================================================
# 5. DATA CORRECTNESS TESTS
# ==============================================================================

test_that("Campus IDs have correct format (9 digits with leading zeros)", {
  skip_if_tea_unavailable()

  # Use full key set to share cache with other tests
  df <- download_tea_csv(2024, "CSTUD", c("IDENT", "PET", "PETG"))

  # All campus IDs should be 9 characters
  campus_lengths <- unique(nchar(df$CAMPUS))
  expect_true(
    all(nchar(df$CAMPUS) == 9),
    label = paste("Campus ID lengths:", paste(campus_lengths, collapse = ", "))
  )

  # All district IDs should be 6 characters
  district_lengths <- unique(nchar(df$DISTRICT))
  expect_true(
    all(nchar(df$DISTRICT) == 6),
    label = paste("District ID lengths:", paste(district_lengths, collapse = ", "))
  )
})

test_that("Enrollment counts are non-negative", {
  skip_if_tea_unavailable()

  # Use full key set to share cache with other tests
  df <- download_tea_csv(2024, "CSTUD", c("IDENT", "PET", "PETG"))

  # Convert total enrollment to numeric
  total <- suppressWarnings(as.numeric(df$CPETALLC))

  # All non-NA values should be >= 0
  expect_true(all(total >= 0, na.rm = TRUE),
              label = "All enrollment values should be non-negative")

  # Most campuses should have positive enrollment (>95%)
  pct_positive <- sum(total > 0, na.rm = TRUE) / length(total)
  expect_true(pct_positive > 0.95,
              label = paste0(round(pct_positive * 100, 1),
                             "% positive (expected >95%)"))
})

test_that("State total enrollment is in expected range", {
  skip_if_tea_unavailable()

  # Use full key set for district data
  df <- download_tea_csv(2024, "DSTUD", c("IDENT", "PET", "PETG"))

  # Sum district enrollments
  total <- sum(suppressWarnings(as.numeric(df$DPETALLC)), na.rm = TRUE)

  # Texas has ~5.5 million students
  expect_true(total > 5000000,
              label = paste("State total:", format(total, big.mark = ","),
                            "(expected >5M)"))
  expect_true(total < 7000000,
              label = paste("State total:", format(total, big.mark = ","),
                            "(expected <7M)"))
})

test_that("Demographic subgroups sum approximately to total", {
  skip_if_tea_unavailable()

  # Use full key set to share cache with other tests
  df <- download_tea_csv(2024, "DSTUD", c("IDENT", "PET", "PETG"))

  # Sum demographics
  to_num <- function(x) sum(suppressWarnings(as.numeric(x)), na.rm = TRUE)

  total <- to_num(df$DPETALLC)
  demo_sum <- to_num(df$DPETWHIC) + to_num(df$DPETBLAC) + to_num(df$DPETHISC) +
    to_num(df$DPETASIC) + to_num(df$DPETINDC) + to_num(df$DPETPCIC) +
    to_num(df$DPETTWOC)

  # Demographics should sum to ~100% of total (within 1%)
  ratio <- demo_sum / total
  expect_true(ratio > 0.99 && ratio < 1.01,
              label = paste("Demo/total ratio:", round(ratio, 4),
                            "(expected 0.99-1.01)"))
})

# ==============================================================================
# 6. YEAR FILTERING TESTS
# ==============================================================================

test_that("get_raw_enr returns data for sample TAPR years", {
  skip_if_tea_unavailable()

  # Test just 2024 to avoid hammering TEA server
  raw <- tryCatch(
    get_raw_enr(2024),
    error = function(e) skip(paste("TEA data fetch failed:", e$message))
  )

  expect_true(is.list(raw))
  expect_true("campus" %in% names(raw))
  expect_true("district" %in% names(raw))

  expect_true(nrow(raw$campus) > 5000,
              label = paste("Campus rows:", nrow(raw$campus)))
  expect_true(nrow(raw$district) > 500,
              label = paste("District rows:", nrow(raw$district)))
})

test_that("fetch_enr adds correct end_year to output", {
  skip_if_tea_unavailable()

  result <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = function(e) skip(paste("TEA data fetch failed:", e$message))
  )

  expect_true("end_year" %in% names(result))
  expect_true(all(result$end_year == 2024))
})

# ==============================================================================
# 7. AGGREGATION TESTS
# ==============================================================================

test_that("District totals approximately equal sum of campus totals", {
  skip_if_tea_unavailable()

  result <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = function(e) skip(paste("TEA data fetch failed:", e$message))
  )

  # Sum campus enrollments by district
  campus_sums <- result |>
    dplyr::filter(type == "Campus") |>
    dplyr::group_by(district_id) |>
    dplyr::summarize(campus_total = sum(row_total, na.rm = TRUE), .groups = "drop")

  # Get district totals
  district_totals <- result |>
    dplyr::filter(type == "District") |>
    dplyr::select(district_id, district_total = row_total)

  # Join and compare
  comparison <- dplyr::inner_join(campus_sums, district_totals, by = "district_id")

  # Most districts should match within 1%
  comparison$diff_pct <- abs(comparison$campus_total - comparison$district_total) /
    comparison$district_total

  matching <- sum(comparison$diff_pct < 0.01, na.rm = TRUE) / nrow(comparison)
  expect_true(matching > 0.90,
              label = paste0(round(matching * 100), "% match (expected >90%)"))
})

test_that("State total equals sum of district totals", {
  skip_if_tea_unavailable()

  result <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = function(e) skip(paste("TEA data fetch failed:", e$message))
  )

  state_total <- result |>
    dplyr::filter(type == "State") |>
    dplyr::pull(row_total)

  district_sum <- result |>
    dplyr::filter(type == "District") |>
    dplyr::summarize(total = sum(row_total, na.rm = TRUE)) |>
    dplyr::pull(total)

  # Should match exactly (state is computed from districts)
  expect_equal(state_total, district_sum)
})

# ==============================================================================
# 8. OUTPUT FIDELITY TESTS
# ==============================================================================

test_that("tidy=TRUE output maintains data integrity", {
  skip_if_tea_unavailable()

  wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = function(e) skip(paste("TEA data fetch failed:", e$message))
  )
  tidy <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("TEA data fetch failed:", e$message))
  )

  # Total enrollment in wide should equal sum in tidy for total_enrollment subgroup
  wide_state_total <- wide |>
    dplyr::filter(type == "State") |>
    dplyr::pull(row_total)

  tidy_state_total <- tidy |>
    dplyr::filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
    dplyr::pull(n_students)

  expect_equal(wide_state_total, tidy_state_total,
               label = "State totals should match between wide and tidy")
})

test_that("No Inf or NaN values in tidy output", {
  skip_if_tea_unavailable()

  result <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("TEA data fetch failed:", e$message))
  )

  numeric_cols <- names(result)[sapply(result, is.numeric)]

  for (col in numeric_cols) {
    expect_false(any(is.infinite(result[[col]])),
                 label = paste("No Inf in", col))
    expect_false(any(is.nan(result[[col]])),
                 label = paste("No NaN in", col))
  }
})

test_that("Percentages are in valid range (0-100)", {
  skip_if_tea_unavailable()

  result <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("TEA data fetch failed:", e$message))
  )

  # Check pct column if it exists
  if ("pct" %in% names(result)) {
    pct_vals <- result$pct[!is.na(result$pct)]
    expect_true(
      all(pct_vals >= 0 & pct_vals <= 100),
      label = paste("Pct range:", round(min(pct_vals), 2), "-",
                    round(max(pct_vals), 2))
    )
  }
})

test_that("All entity types are present in tidy output", {
  skip_if_tea_unavailable()

  result <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("TEA data fetch failed:", e$message))
  )

  # Check flags
  expect_true(any(result$is_state), label = "Should have state rows")
  expect_true(any(result$is_district), label = "Should have district rows")
  expect_true(any(result$is_campus), label = "Should have campus rows")

  # Each row should be exactly one type
  type_sums <- result$is_state + result$is_district + result$is_campus
  expect_true(all(type_sums == 1),
              label = "Each row should be exactly one entity type")
})

# ==============================================================================
# SPECIFIC YEAR TESTS - Verify each era works
# ==============================================================================

test_that("TAPR era (2013+) data downloads correctly", {
  skip_if_tea_unavailable()

  # Test most recent year
  result <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = FALSE),
    error = function(e) skip(paste("TEA TAPR data fetch failed:", e$message))
  )

  expect_true("Campus" %in% result$type)
  expect_true("District" %in% result$type)

  n_campus <- nrow(result[result$type == "Campus", ])
  expect_true(n_campus > 8000,
              label = paste("Campus count:", n_campus))
})

test_that("AEIS SAS era (2003-2012) data downloads correctly", {
  skip_if_tea_unavailable()

  result <- tryCatch(
    fetch_enr(2010, tidy = FALSE, use_cache = FALSE),
    error = function(e) skip(paste("TEA AEIS SAS data fetch failed:", e$message))
  )

  expect_true("Campus" %in% result$type)
  expect_true("District" %in% result$type)

  n_campus <- nrow(result[result$type == "Campus", ])
  expect_true(n_campus > 5000,
              label = paste("Campus count:", n_campus))
})

test_that("AEIS CGI era (1997-2002) data downloads correctly", {
  skip_if_tea_unavailable()

  result <- tryCatch(
    fetch_enr(2000, tidy = FALSE, use_cache = FALSE),
    error = function(e) skip(paste("TEA AEIS CGI data fetch failed:", e$message))
  )

  expect_true("Campus" %in% result$type)
  expect_true("District" %in% result$type)

  n_campus <- nrow(result[result$type == "Campus", ])
  expect_true(n_campus > 3000,
              label = paste("Campus count:", n_campus))
})
