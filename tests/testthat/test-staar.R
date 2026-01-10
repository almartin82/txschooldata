# Tests for STAAR assessment functions
# Note: Most tests are marked as skip_on_cran since they require network access

# Known good values for Houston ISD (Grade 3) for fidelity testing
# These values were extracted from the raw TEA data files

# 2023 - Houston ISD (101912)
HOUSTON_2023 <- list(
  district_id = "101912",
  year = 2023,
  grade = "03",
  district_name = "HOUSTON ISD",
  rla_all_total = 10559,
  rla_all_did_not_meet = 2954,
  rla_all_approaches = 7605,
  math_all_total = 10846,
  math_all_did_not_meet = 3488,
  math_all_approaches = 7358
)

# 2021 - Houston ISD (101912)
HOUSTON_2021 <- list(
  district_id = "101912",
  year = 2021,
  grade = "03",
  district_name = "HOUSTON ISD",
  rla_all_total = 9166,
  rla_all_did_not_meet = 3778,
  rla_all_approaches = 5388,
  math_all_total = 9447,
  math_all_did_not_meet = 4658,
  math_all_approaches = 4789
)

# 2018 - Houston ISD (101912)
HOUSTON_2018 <- list(
  district_id = "101912",
  year = 2018,
  grade = "03",
  district_name = "HOUSTON ISD",
  rla_all_total = 13472,
  rla_all_did_not_meet = 4242,
  rla_all_approaches = 597,
  math_all_total = 13721,
  math_all_did_not_meet = 3769,
  math_all_approaches = 622
)

test_that("get_raw_staar validates year parameter", {
  skip_on_cran()
  skip_if_offline()

  # Year 2017 is before valid range (2018-2023)
  expect_error(get_raw_staar(2017, "district", 3), "year must be one of")

  # Year 2024 is after valid range for legacy data
  expect_error(get_raw_staar(2024, "district", 3), "year must be one of")

  # Invalid level
  expect_error(get_raw_staar(2023, "invalid", 3), "level must be")

  # Invalid grade
  expect_error(get_raw_staar(2023, "district", 9), "grade must be between")
})

test_that("get_raw_staar downloads and parses STAAR files", {
  skip_on_cran()
  skip_if_offline()

  # Test 2023 (most recent)
  result_2023 <- get_raw_staar(2023, "district", 3)

  expect_true(is.data.frame(result_2023))
  expect_true(nrow(result_2023) > 0)  # Should have ~1200 districts

  # Check for Houston ISD
  houston_2023 <- result_2023[result_2023$DISTRICT == HOUSTON_2023$district_id, ]
  expect_equal(nrow(houston_2023), 1)
  expect_equal(as.character(houston_2023$YEAR), "23")  # Raw data has 2-digit year
  expect_equal(as.character(houston_2023$GRADE), HOUSTON_2023$grade)

  # Test 2021
  result_2021 <- get_raw_staar(2021, "district", 3)
  expect_true(is.data.frame(result_2021))
  expect_true(nrow(result_2021) > 0)

  # Test 2018
  result_2018 <- get_raw_staar(2018, "district", 3)
  expect_true(is.data.frame(result_2018))
  expect_true(nrow(result_2018) > 0)
})

test_that("process_staar standardizes columns across years", {
  skip_on_cran()
  skip_if_offline()

  # Get raw data for different years
  raw_2023 <- get_raw_staar(2023, "district", 3)
  raw_2021 <- get_raw_staar(2021, "district", 3)
  raw_2018 <- get_raw_staar(2018, "district", 3)

  # Process all years
  proc_2023 <- process_staar(raw_2023, 2023, "district", 3)
  proc_2021 <- process_staar(raw_2021, 2021, "district", 3)
  proc_2018 <- process_staar(raw_2018, 2018, "district", 3)

  # All should have the same column structure
  expect_true(all(names(proc_2023) == names(proc_2021)))
  expect_true(all(names(proc_2023) == names(proc_2018)))

  # Check for expected columns
  expected_cols <- c(
    "district_id", "year", "region", "district_name", "grade", "subject",
    "all_total", "all_did_not_meet", "all_approaches", "all_meets", "all_masters"
  )
  expect_true(all(expected_cols %in% names(proc_2023)))
})

test_that("process_staar maintains data fidelity for 2023", {
  skip_on_cran()
  skip_if_offline()

  raw_2023 <- get_raw_staar(2023, "district", 3)
  processed_2023 <- process_staar(raw_2023, 2023, "district", 3)

  # Find Houston ISD in processed data
  houston <- processed_2023[processed_2023$district_id == HOUSTON_2023$district_id, ]

  # Check RLA values
  houston_rla <- houston[houston$subject == "RLA", ]
  expect_equal(nrow(houston_rla), 1)
  expect_equal(houston_rla$all_total, HOUSTON_2023$rla_all_total)
  expect_equal(houston_rla$all_did_not_meet, HOUSTON_2023$rla_all_did_not_meet)
  expect_equal(houston_rla$all_approaches, HOUSTON_2023$rla_all_approaches)

  # Check Math values
  houston_math <- houston[houston$subject == "Math", ]
  expect_equal(nrow(houston_math), 1)
  expect_equal(houston_math$all_total, HOUSTON_2023$math_all_total)
  expect_equal(houston_math$all_did_not_meet, HOUSTON_2023$math_all_did_not_meet)
  expect_equal(houston_math$all_approaches, HOUSTON_2023$math_all_approaches)
})

test_that("process_staar maintains data fidelity for 2021", {
  skip_on_cran()
  skip_if_offline()

  raw_2021 <- get_raw_staar(2021, "district", 3)
  processed_2021 <- process_staar(raw_2021, 2021, "district", 3)

  # Find Houston ISD in processed data
  houston <- processed_2021[processed_2021$district_id == HOUSTON_2021$district_id, ]

  # Check RLA values
  houston_rla <- houston[houston$subject == "RLA", ]
  expect_equal(nrow(houston_rla), 1)
  expect_equal(houston_rla$all_total, HOUSTON_2021$rla_all_total)
  expect_equal(houston_rla$all_did_not_meet, HOUSTON_2021$rla_all_did_not_meet)
  expect_equal(houston_rla$all_approaches, HOUSTON_2021$rla_all_approaches)

  # Check Math values
  houston_math <- houston[houston$subject == "Math", ]
  expect_equal(nrow(houston_math), 1)
  expect_equal(houston_math$all_total, HOUSTON_2021$math_all_total)
  expect_equal(houston_math$all_did_not_meet, HOUSTON_2021$math_all_did_not_meet)
  expect_equal(houston_math$all_approaches, HOUSTON_2021$math_all_approaches)
})

test_that("process_staar maintains data fidelity for 2018", {
  skip_on_cran()
  skip_if_offline()

  raw_2018 <- get_raw_staar(2018, "district", 3)
  processed_2018 <- process_staar(raw_2018, 2018, "district", 3)

  # Find Houston ISD in processed data
  houston <- processed_2018[processed_2018$district_id == HOUSTON_2018$district_id, ]

  # Check RLA values
  houston_rla <- houston[houston$subject == "RLA", ]
  expect_equal(nrow(houston_rla), 1)
  expect_equal(houston_rla$all_total, HOUSTON_2018$rla_all_total)
  expect_equal(houston_rla$all_did_not_meet, HOUSTON_2018$rla_all_did_not_meet)

  # Check Math values
  houston_math <- houston[houston$subject == "Math", ]
  expect_equal(nrow(houston_math), 1)
  expect_equal(houston_math$all_total, HOUSTON_2018$math_all_total)
  expect_equal(houston_math$all_did_not_meet, HOUSTON_2018$math_all_did_not_meet)
})

test_that("tidy_staar produces correct long format", {
  skip_on_cran()
  skip_if_offline()

  # Get processed data
  raw_2023 <- get_raw_staar(2023, "district", 3)
  processed_2023 <- process_staar(raw_2023, 2023, "district", 3)

  # Tidy the data
  tidy_result <- tidy_staar(processed_2023)

  # Check structure
  expect_true("district_id" %in% names(tidy_result))
  expect_true("subject" %in% names(tidy_result))
  expect_true("performance_level" %in% names(tidy_result))
  expect_true("student_group" %in% names(tidy_result))
  expect_true("count" %in% names(tidy_result))

  # Check that performance levels are present
  perf_levels <- unique(tidy_result$performance_level)
  expect_true("did_not_meet" %in% perf_levels)
  expect_true("approaches" %in% perf_levels)
  expect_true("meets" %in% perf_levels)
  expect_true("masters" %in% perf_levels)

  # Check fidelity: Houston ISD performance level counts should match wide format
  houston_tidy <- tidy_result[tidy_result$district_id == HOUSTON_2023$district_id &
                              tidy_result$subject == "RLA" &
                              tidy_result$student_group == "all", ]

  houston_wide <- processed_2023[processed_2023$district_id == HOUSTON_2023$district_id &
                                 processed_2023$subject == "RLA", ]

  # Check individual performance levels match
  expect_equal(
    houston_tidy[houston_tidy$performance_level == "did_not_meet", ]$count,
    houston_wide$all_did_not_meet
  )
  expect_equal(
    houston_tidy[houston_tidy$performance_level == "approaches", ]$count,
    houston_wide$all_approaches
  )
  expect_equal(
    houston_tidy[houston_tidy$performance_level == "meets", ]$count,
    houston_wide$all_meets
  )
  expect_equal(
    houston_tidy[houston_tidy$performance_level == "masters", ]$count,
    houston_wide$all_masters
  )
})

test_that("fetch_staar wrapper function works end-to-end", {
  skip_on_cran()
  skip_if_offline()

  # Test with tidy = FALSE
  result_wide <- fetch_staar(2023, "district", 3, tidy = FALSE, use_cache = FALSE)

  expect_true(is.data.frame(result_wide))
  expect_true("district_id" %in% names(result_wide))
  expect_true("subject" %in% names(result_wide))
  expect_true("all_total" %in% names(result_wide))

  # Test with tidy = TRUE
  result_tidy <- fetch_staar(2023, "district", 3, tidy = TRUE, use_cache = FALSE)

  expect_true(is.data.frame(result_tidy))
  expect_true("district_id" %in% names(result_tidy))
  expect_true("performance_level" %in% names(result_tidy))
  expect_true("count" %in% names(result_tidy))

  # Check fidelity between wide and tidy
  houston_wide <- result_wide[result_wide$district_id == HOUSTON_2023$district_id &
                               result_wide$subject == "RLA", ]
  houston_tidy <- result_tidy[result_tidy$district_id == HOUSTON_2023$district_id &
                               result_tidy$subject == "RLA" &
                               result_tidy$student_group == "all", ]

  # Check individual performance levels match
  expect_equal(
    houston_tidy[houston_tidy$performance_level == "did_not_meet", ]$count,
    houston_wide$all_did_not_meet
  )
  expect_equal(
    houston_tidy[houston_tidy$performance_level == "approaches", ]$count,
    houston_wide$all_approaches
  )
  expect_equal(
    houston_tidy[houston_tidy$performance_level == "meets", ]$count,
    houston_wide$all_meets
  )
  expect_equal(
    houston_tidy[houston_tidy$performance_level == "masters", ]$count,
    houston_wide$all_masters
  )
})

test_that("STAAR data quality checks pass", {
  skip_on_cran()
  skip_if_offline()

  result <- fetch_staar(2023, "district", 3, tidy = FALSE, use_cache = FALSE)

  # No negative counts
  count_cols <- grep("_total|did_not_meet|approaches|meets|masters", names(result), value = TRUE)
  for (col in count_cols) {
    expect_true(all(result[[col]] >= 0, na.rm = TRUE),
                info = sprintf("Column %s has negative values", col))
  }

  # All districts have valid IDs
  expect_true(all(nchar(result$district_id) == 6 | is.na(result$district_id)))

  # All years are valid
  expect_true(all(result$year %in% c(2018, 2021, 2022, 2023)))
})
