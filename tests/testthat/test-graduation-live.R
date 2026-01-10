# ==============================================================================
# Live Graduation Rate Pipeline Tests
# ==============================================================================
#
# These tests verify that the graduation rate data pipeline works correctly
# with live TEA data. Tests cover URL availability, file download, parsing,
# and data quality checks.
#
# To run: devtools::test(filter = "graduation-live")
#
# ==============================================================================

test_that("Graduation rate URLs are accessible", {
  skip_on_cran()

  # Test 2024 URLs (most recent)
  class_year <- 2024
  base_url <- "https://tea.texas.gov/reports-and-data/school-performance/accountability-research/completion-graduation-and-dropout"

  campus_url <- paste0(base_url, "/campus-data-download-4yr-", class_year, ".xlsx")
  district_url <- paste0(base_url, "/district-data-download-4yr-", class_year, ".xlsx")
  county_url <- paste0(base_url, "/county-data-download-4yr-", class_year, ".xlsx")

  # Check HTTP status via GET (HEAD may not be supported)
  campus_resp <- httr::GET(campus_url, httr::timeout(10))
  district_resp <- httr::GET(district_url, httr::timeout(10))
  county_resp <- httr::GET(county_url, httr::timeout(10))

  expect_equal(httr::status_code(campus_resp), 200)
  expect_equal(httr::status_code(district_resp), 200)
  expect_equal(httr::status_code(county_resp), 200)
})


test_that("Graduation rate files download and parse correctly", {
  skip_on_cran()

  class_year <- 2024

  # Download raw data
  raw <- get_raw_grad(class_year)

  # Verify all levels are present
  expect_true(is.data.frame(raw$campus))
  expect_true(is.data.frame(raw$district))
  expect_true(is.data.frame(raw$county))

  # Verify reasonable row counts
  expect_gt(nrow(raw$district), 1000)  # Texas has ~1200 districts
  expect_gt(nrow(raw$campus), 3000)    # HS with grad rates (not all campuses)
})


test_that("Graduation rate data processes correctly", {
  skip_on_cran()

  class_year <- 2024

  # Get raw data
  raw <- get_raw_grad(class_year)

  # Process to standard schema
  processed <- process_grad(raw, class_year)

  # Verify expected columns exist
  expected_cols <- c(
    "class_year", "type", "district_id", "campus_id",
    "district_name", "campus_name", "county", "region",
    "all_students_grad_rate", "white_grad_rate", "black_grad_rate",
    "hispanic_grad_rate", "asian_grad_rate", "female_grad_rate",
    "male_grad_rate", "econ_disadv_grad_rate", "special_ed_grad_rate",
    "lep_grad_rate"
  )

  expect_true(all(expected_cols %in% names(processed)))

  # Verify data types
  expect_type(processed$class_year, "integer")
  expect_type(processed$district_id, "character")
  expect_type(processed$all_students_grad_rate, "double")

  # Verify all three types present
  expect_true("State" %in% unique(processed$type))
  expect_true("District" %in% unique(processed$type))
  expect_true("Campus" %in% unique(processed$type))
})


test_that("Graduation rate data tidies correctly", {
  skip_on_cran()

  class_year <- 2024

  # Get raw and process
  raw <- get_raw_grad(class_year)
  processed <- process_grad(raw, class_year)

  # Tidy
  tidy <- tidy_grad(processed)

  # Verify tidy format structure
  expect_true("subgroup" %in% names(tidy))
  expect_true("grad_rate" %in% names(tidy))

  # Verify key subgroups present
  expected_subgroups <- c(
    "all_students", "white", "black", "hispanic",
    "asian", "female", "male", "econ_disadv"
  )

  present_subgroups <- unique(tidy$subgroup)
  expect_true(all(expected_subgroups %in% present_subgroups))

  # Verify state level data
  state_data <- tidy[tidy$type == "State", ]
  expect_gt(nrow(state_data), 0)
})


test_that("Major districts are present in graduation rate data", {
  skip_on_cran()

  class_year <- 2024

  # Fetch data
  grad <- fetch_grad(class_year)

  # Check for major Texas school districts (using district IDs from graduation data)
  major_districts <- c(
    "227901" = "Austin ISD",
    "015914" = "Houston ISD",
    "057905" = "Dallas ISD",
    "220905" = "Fort Worth ISD"
  )

  district_data <- grad[grad$type == "District", ]

  for (id in names(major_districts)) {
    expect_true(id %in% district_data$district_id,
                info = paste("District", major_districts[id], "not found"))
  }
})


test_that("Graduation rate values are reasonable", {
  skip_on_cran()

  class_year <- 2024

  # Fetch data
  grad <- fetch_grad(class_year)

  # Filter to district and state level (skip campus due to small N)
  test_data <- grad[grad$type %in% c("State", "District") &
                   grad$subgroup == "all_students", ]

  # Remove NA values
  rates <- test_data$grad_rate[!is.na(test_data$grad_rate)]

  # Rates should be between 0 and 100
  expect_true(all(rates >= 0 & rates <= 100),
              info = "Graduation rates outside 0-100 range")

  # State graduation rate should be reasonable (between 80-95%)
  state_rate <- test_data$grad_rate[test_data$type == "State"]
  expect_true(state_rate >= 80 & state_rate <= 95,
              info = paste("State graduation rate outside expected range:", state_rate))

  # Mean district rate should be in reasonable range
  mean_rate <- mean(rates, na.rm = TRUE)
  expect_true(mean_rate >= 70 & mean_rate <= 95,
              info = paste("Mean district rate outside expected range:", mean_rate))
})


test_that("Fetch grad multi-year works correctly", {
  skip_on_cran()

  # Fetch 2 recent years (keep test fast)
  grad_multi <- fetch_grad_multi(c(2023, 2024))

  # Verify both years present
  expect_true(2023 %in% unique(grad_multi$class_year))
  expect_true(2024 %in% unique(grad_multi$class_year))

  # Verify reasonable row count
  expect_gt(nrow(grad_multi), 10000)
})


test_that("Wide format graduation rate data works", {
  skip_on_cran()

  class_year <- 2024

  # Fetch wide format
  grad_wide <- fetch_grad(class_year, tidy = FALSE)

  # Verify wide format has rate columns, not subgroup
  expect_true("all_students_grad_rate" %in% names(grad_wide))
  expect_false("subgroup" %in% names(grad_wide))

  # Verify data structure
  expect_true("white_grad_rate" %in% names(grad_wide))
  expect_true("black_grad_rate" %in% names(grad_wide))
  expect_true("hispanic_grad_rate" %in% names(grad_wide))
})


test_that("Graduation rate fidelity preserved across tidy/wide", {
  skip_on_cran()

  class_year <- 2024

  # Clear cache to ensure fresh data for both
  clear_cache(class_year, "grad_wide")
  clear_cache(class_year, "grad_tidy")

  # Fetch both formats
  grad_wide <- fetch_grad(class_year, tidy = FALSE, use_cache = TRUE)
  grad_tidy <- fetch_grad(class_year, tidy = TRUE, use_cache = TRUE)

  # Compare state-level all students rate
  wide_state <- grad_wide[grad_wide$type == "State", "all_students_grad_rate"]
  tidy_state <- grad_tidy[grad_tidy$type == "State" &
                          grad_tidy$subgroup == "all_students", "grad_rate"]

  expect_equal(as.numeric(wide_state), as.numeric(tidy_state),
               tolerance = 0.01,
               info = "Fidelity not preserved between wide and tidy formats")
})


test_that("Cache works for graduation rate data", {
  skip_on_cran()

  class_year <- 2024

  # Clear any existing cache
  clear_cache(class_year, "grad_tidy")

  # First fetch should download
  expect_false(cache_exists(class_year, "grad_tidy"))
  grad1 <- fetch_grad(class_year, use_cache = TRUE)

  # Should now be cached
  expect_true(cache_exists(class_year, "grad_tidy"))

  # Second fetch should use cache
  grad2 <- fetch_grad(class_year, use_cache = TRUE)

  # Results should be identical
  expect_identical(grad1, grad2)
})
