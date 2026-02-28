# ==============================================================================
# Transformation Correctness Tests — STAAR Assessments
# ==============================================================================
#
# Tests verify STAAR-specific transformation correctness:
# - safe_numeric_staar suppression handling
# - Column prefix normalization (r_ -> RLA_, rla_ -> RLA_)
# - Year-to-4-digit conversion
# - Subject reshaping (wide -> long with subject column)
# - Tidy pivot (performance levels as rows)
# - Known-value pinning per available year
# - District ID formatting
#
# NOTE: Existing test-staar.R covers Houston ISD fidelity for 2018/2021/2023
# and basic structure checks. This file adds column normalization, suppression,
# and cross-year consistency tests WITHOUT duplicating those pinned values.
#
# ==============================================================================

skip_if_offline <- function() {
  skip_on_cran()
  if (!curl::has_internet()) skip("No internet connection")
}

# ==============================================================================
# 1. STAAR SUPPRESSION HANDLING
# ==============================================================================

test_that("safe_numeric_staar handles all suppression markers", {
  fn <- txschooldata:::safe_numeric_staar

  # NULL input
  expect_true(is.na(fn(NULL)))

  # Standard suppression markers
  markers <- c("", "*", "-", ".", "NA", "NULL")
  for (m in markers) {
    expect_true(is.na(fn(m)),
                info = paste("Marker not handled:", shQuote(m)))
  }
})

test_that("safe_numeric_staar handles commas and normal values", {
  fn <- txschooldata:::safe_numeric_staar

  expect_equal(fn("100"), 100)
  expect_equal(fn("1,234"), 1234)
  expect_equal(fn("0"), 0)
})

test_that("safe_numeric_staar handles vectors", {
  fn <- txschooldata:::safe_numeric_staar

  result <- fn(c("100", "*", "1,234", ".", "42"))
  expect_equal(result[1], 100)
  expect_true(is.na(result[2]))
  expect_equal(result[3], 1234)
  expect_true(is.na(result[4]))
  expect_equal(result[5], 42)
})

# ==============================================================================
# 2. COLUMN PREFIX NORMALIZATION
# ==============================================================================

test_that("process_staar normalizes column names consistently across years", {
  skip_if_offline()

  # All years should produce the same column structure after processing
  years_to_test <- c(2018, 2021, 2023)
  col_sets <- list()

  for (yr in years_to_test) {
    raw <- txschooldata:::get_raw_staar(yr, "district", 3)
    proc <- txschooldata:::process_staar(raw, yr, "district", 3)
    col_sets[[as.character(yr)]] <- sort(names(proc))
  }

  # All years should have identical columns
  expect_equal(col_sets[["2018"]], col_sets[["2021"]],
               info = "2018 and 2021 columns differ")
  expect_equal(col_sets[["2021"]], col_sets[["2023"]],
               info = "2021 and 2023 columns differ")
})

test_that("processed data has required standard columns", {
  skip_if_offline()

  raw <- txschooldata:::get_raw_staar(2023, "district", 3)
  proc <- txschooldata:::process_staar(raw, 2023, "district", 3)

  required <- c("district_id", "year", "region", "district_name", "grade",
                "subject", "all_total", "all_did_not_meet", "all_approaches",
                "all_meets", "all_masters")

  for (col in required) {
    expect_true(col %in% names(proc),
                info = paste("Missing column:", col))
  }
})

# ==============================================================================
# 3. YEAR CONVERSION
# ==============================================================================

test_that("STAAR year is converted from 2-digit to 4-digit", {
  skip_if_offline()

  raw <- txschooldata:::get_raw_staar(2023, "district", 3)
  proc <- txschooldata:::process_staar(raw, 2023, "district", 3)

  # Raw data has YEAR as 2-digit "23"; processed should be 2023
  expect_true(all(proc$year == 2023))
})

test_that("STAAR 2018 year is converted correctly", {
  skip_if_offline()

  raw <- txschooldata:::get_raw_staar(2018, "district", 3)
  proc <- txschooldata:::process_staar(raw, 2018, "district", 3)

  expect_true(all(proc$year == 2018))
})

# ==============================================================================
# 4. SUBJECT RESHAPING
# ==============================================================================

test_that("processed STAAR has both RLA and Math subjects", {
  skip_if_offline()

  raw <- txschooldata:::get_raw_staar(2023, "district", 3)
  proc <- txschooldata:::process_staar(raw, 2023, "district", 3)

  subjects <- unique(proc$subject)
  expect_true("RLA" %in% subjects)
  expect_true("Math" %in% subjects)
  expect_equal(length(subjects), 2)
})

test_that("each district has exactly 2 rows (RLA + Math) per grade", {
  skip_if_offline()

  raw <- txschooldata:::get_raw_staar(2023, "district", 3)
  proc <- txschooldata:::process_staar(raw, 2023, "district", 3)

  # Count rows per district
  counts <- table(proc$district_id)
  # Every district should have exactly 2 rows
  expect_true(all(counts == 2),
              info = paste("Districts with != 2 rows:",
                           sum(counts != 2)))
})

# ==============================================================================
# 5. TIDY PIVOT
# ==============================================================================

test_that("tidy_staar produces correct performance levels", {
  skip_if_offline()

  raw <- txschooldata:::get_raw_staar(2023, "district", 3)
  proc <- txschooldata:::process_staar(raw, 2023, "district", 3)
  tidy <- txschooldata:::tidy_staar(proc)

  perf_levels <- unique(tidy$performance_level)
  expected <- c("did_not_meet", "approaches", "meets", "masters")

  for (pl in expected) {
    expect_true(pl %in% perf_levels,
                info = paste("Missing performance_level:", pl))
  }
})

test_that("tidy_staar performance levels are cumulative: approaches >= meets >= masters", {
  skip_if_offline()

  raw <- txschooldata:::get_raw_staar(2023, "district", 3)
  proc <- txschooldata:::process_staar(raw, 2023, "district", 3)
  tidy <- txschooldata:::tidy_staar(proc)

  # STAAR performance levels are cumulative:
  # approaches = approaches or above (includes meets + masters)
  # meets = meets or above (includes masters)
  # masters = masters only
  # did_not_meet = did not meet approaches
  # So: approaches >= meets >= masters AND did_not_meet + approaches = total
  houston_rla <- tidy[tidy$district_id == "101912" &
                       tidy$subject == "RLA", ]

  dnm <- houston_rla$count[houston_rla$performance_level == "did_not_meet"]
  appr <- houston_rla$count[houston_rla$performance_level == "approaches"]
  meets <- houston_rla$count[houston_rla$performance_level == "meets"]
  masters <- houston_rla$count[houston_rla$performance_level == "masters"]

  # Cumulative: approaches >= meets >= masters
  expect_true(appr >= meets, info = "approaches should be >= meets")
  expect_true(meets >= masters, info = "meets should be >= masters")

  # did_not_meet + approaches should equal total tested
  houston_total <- proc$all_total[proc$district_id == "101912" &
                                   proc$subject == "RLA"]
  expect_equal(dnm + appr, houston_total)
})

test_that("tidy_staar filters out zero and NA counts", {
  skip_if_offline()

  raw <- txschooldata:::get_raw_staar(2023, "district", 3)
  proc <- txschooldata:::process_staar(raw, 2023, "district", 3)
  tidy <- txschooldata:::tidy_staar(proc)

  # No NA counts
  expect_false(any(is.na(tidy$count)))

  # No zero counts
  expect_false(any(tidy$count == 0))
})

# ==============================================================================
# 6. ID FORMATTING
# ==============================================================================

test_that("STAAR district IDs are 6 characters", {
  skip_if_offline()

  raw <- txschooldata:::get_raw_staar(2023, "district", 3)
  proc <- txschooldata:::process_staar(raw, 2023, "district", 3)

  ids <- proc$district_id[!is.na(proc$district_id)]
  expect_true(all(nchar(ids) == 6),
              info = paste("Non-6-char IDs found:",
                           paste(unique(nchar(ids)), collapse = ", ")))
})

test_that("STAAR campus IDs are 9 characters", {
  skip_if_offline()

  raw <- txschooldata:::get_raw_staar(2023, "campus", 3)
  proc <- txschooldata:::process_staar(raw, 2023, "campus", 3)

  ids <- proc$campus_id[!is.na(proc$campus_id)]
  expect_true(all(nchar(ids) == 9),
              info = paste("Non-9-char IDs found:",
                           paste(unique(nchar(ids)), collapse = ", ")))
})

# ==============================================================================
# 7. GRADE FORMATTING
# ==============================================================================

test_that("STAAR grade is zero-padded to 2 characters", {
  skip_if_offline()

  for (grade in 3:8) {
    raw <- txschooldata:::get_raw_staar(2023, "district", grade)
    proc <- txschooldata:::process_staar(raw, 2023, "district", grade)

    expected_grade <- sprintf("%02d", grade)
    expect_true(all(proc$grade == expected_grade),
                info = paste("Grade", grade, "not formatted as", expected_grade))
  }
})

# ==============================================================================
# 8. DATA QUALITY
# ==============================================================================

test_that("no negative counts in processed STAAR data", {
  skip_if_offline()

  raw <- txschooldata:::get_raw_staar(2023, "district", 3)
  proc <- txschooldata:::process_staar(raw, 2023, "district", 3)

  count_cols <- c("all_total", "all_did_not_meet", "all_approaches",
                  "all_meets", "all_masters")

  for (col in count_cols) {
    vals <- proc[[col]][!is.na(proc[[col]])]
    expect_true(all(vals >= 0),
                info = paste("Negative values in", col))
  }
})

test_that("no Inf or NaN in processed STAAR data", {
  skip_if_offline()

  raw <- txschooldata:::get_raw_staar(2023, "district", 3)
  proc <- txschooldata:::process_staar(raw, 2023, "district", 3)

  numeric_cols <- names(proc)[sapply(proc, is.numeric)]
  for (col in numeric_cols) {
    expect_false(any(is.infinite(proc[[col]])),
                 info = paste("Inf in", col))
    expect_false(any(is.nan(proc[[col]])),
                 info = paste("NaN in", col))
  }
})

# ==============================================================================
# 9. AGGREGATION FLAGS
# ==============================================================================

test_that("tidy_staar adds correct aggregation flags for district data", {
  skip_if_offline()

  raw <- txschooldata:::get_raw_staar(2023, "district", 3)
  proc <- txschooldata:::process_staar(raw, 2023, "district", 3)
  tidy <- txschooldata:::tidy_staar(proc)

  expect_true("is_district" %in% names(tidy))
  expect_true("is_campus" %in% names(tidy))

  # District-level data: is_district should be TRUE, is_campus FALSE
  expect_true(all(tidy$is_district))
  expect_true(all(!tidy$is_campus))
})

test_that("tidy_staar adds correct aggregation flags for campus data", {
  skip_if_offline()

  raw <- txschooldata:::get_raw_staar(2023, "campus", 3)
  proc <- txschooldata:::process_staar(raw, 2023, "campus", 3)
  tidy <- txschooldata:::tidy_staar(proc)

  # Campus-level data: is_campus should be TRUE
  expect_true(all(tidy$is_campus))
  expect_true(all(!tidy$is_district))
})

# ==============================================================================
# 10. CROSS-YEAR CONSISTENCY
# ==============================================================================

test_that("STAAR available years are 2018, 2021, 2022, 2023", {
  available <- txschooldata:::staar_available_years()
  expect_equal(available, c(2018, 2021, 2022, 2023))
})

test_that("STAAR grades are 3-8", {
  grades <- txschooldata:::get_staar_grades()
  expect_equal(grades, 3:8)
})

test_that("STAAR year validation rejects invalid years", {
  expect_error(txschooldata:::get_raw_staar(2017, "district", 3), "year must be one of")
  expect_error(txschooldata:::get_raw_staar(2019, "district", 3), "year must be one of")
  expect_error(txschooldata:::get_raw_staar(2020, "district", 3), "year must be one of")
  expect_error(txschooldata:::get_raw_staar(2024, "district", 3), "year must be one of")
})

test_that("STAAR level validation rejects invalid levels", {
  expect_error(txschooldata:::get_raw_staar(2023, "state", 3), "level must be")
  expect_error(txschooldata:::get_raw_staar(2023, "school", 3), "level must be")
})

test_that("STAAR grade validation rejects invalid grades", {
  expect_error(txschooldata:::get_raw_staar(2023, "district", 2), "grade must be between")
  expect_error(txschooldata:::get_raw_staar(2023, "district", 9), "grade must be between")
})
