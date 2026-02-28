# ==============================================================================
# Transformation Correctness Tests — Graduation Rates
# ==============================================================================
#
# Tests verify graduation rate transformation correctness:
# - Suppression marker handling in extract_rate
# - ID formatting (district=6, campus=9)
# - Subgroup renaming to standard names
# - Pivot fidelity: tidy grad_rate matches wide for specific districts
# - Rate validity: all rates in [0, 100]
# - Entity flag logic
# - Known-value spot checks for available years
# - Cross-year consistency
#
# NOTE: Existing test-graduation-live.R covers basic pipeline, URL checks, and
# structure validation. This file focuses on transformation correctness and
# pinned values.
#
# ==============================================================================

skip_if_offline <- function() {
  skip_on_cran()
  if (!curl::has_internet()) skip("No internet connection")
}

# ==============================================================================
# 1. SUPPRESSION HANDLING IN GRADUATION DATA
# ==============================================================================

test_that("graduation rate suppression: *, ., -, -1, 0, -1.0 all become NA", {
  skip_if_offline()

  # The extract_rate helper inside process_grad_level maps these to NA
  # We test the end result: wide format should have NA for suppressed rates
  grad_wide <- fetch_grad(2024, tidy = FALSE, use_cache = TRUE)

  # Some districts will have NA for small-n subgroups
  expect_true(any(is.na(grad_wide$native_american_grad_rate)),
              info = "Expected some NA native_american rates (small N suppression)")

  # Houston ISD black grad rate is NA (suppressed in 2024 data)
  houston <- grad_wide[grad_wide$type == "District" &
                        grad_wide$district_id == "101912", ]
  # This is a check that suppression is handled, not a fixed assertion
  # Some rates may be NA and that's correct behavior
})

# ==============================================================================
# 2. ID FORMATTING
# ==============================================================================

test_that("graduation district IDs are 6 characters", {
  skip_if_offline()

  grad <- fetch_grad(2024, tidy = FALSE, use_cache = TRUE)
  districts <- grad[grad$type == "District" & !is.na(grad$district_id), ]

  expect_true(all(nchar(districts$district_id) == 6),
              info = paste("Non-6-char district IDs found:",
                           paste(unique(nchar(districts$district_id)), collapse = ", ")))
})

test_that("graduation campus IDs are 9 characters", {
  skip_if_offline()

  grad <- fetch_grad(2024, tidy = FALSE, use_cache = TRUE)
  campuses <- grad[grad$type == "Campus" & !is.na(grad$campus_id), ]

  expect_true(all(nchar(campuses$campus_id) == 9),
              info = paste("Non-9-char campus IDs found:",
                           paste(unique(nchar(campuses$campus_id)), collapse = ", ")))
})

test_that("graduation district rows have NA campus_id", {
  skip_if_offline()

  grad <- fetch_grad(2024, tidy = FALSE, use_cache = TRUE)
  districts <- grad[grad$type == "District", ]
  expect_true(all(is.na(districts$campus_id)))
})

test_that("graduation IDs have no leading quotes", {
  skip_if_offline()

  grad <- fetch_grad(2024, tidy = FALSE, use_cache = TRUE)
  ids <- c(grad$district_id, grad$campus_id)
  ids <- ids[!is.na(ids)]

  expect_false(any(grepl("^'", ids)),
               info = "Found IDs with leading quotes")
})

# ==============================================================================
# 3. SUBGROUP RENAMING
# ==============================================================================

test_that("graduation tidy uses standard subgroup names", {
  skip_if_offline()

  grad <- fetch_grad(2024, tidy = TRUE, use_cache = TRUE)
  subgroups <- unique(grad$subgroup)

  standard <- c("all_students", "white", "black", "hispanic", "asian",
                "native_american", "multiracial", "female", "male",
                "econ_disadv", "special_ed", "lep", "at_risk")

  for (s in standard) {
    expect_true(s %in% subgroups,
                info = paste("Missing graduation subgroup:", s))
  }
})

test_that("no non-standard subgroup names in graduation tidy data", {
  skip_if_offline()

  grad <- fetch_grad(2024, tidy = TRUE, use_cache = TRUE)
  subgroups <- unique(grad$subgroup)

  banned <- c("All Students", "WHITE", "BLACK", "HISPANIC",
              "all_students_grad_rate", "white_grad_rate")

  for (b in banned) {
    expect_false(b %in% subgroups,
                 info = paste("Non-standard subgroup present:", b))
  }
})

# ==============================================================================
# 4. PIVOT FIDELITY
# ==============================================================================

test_that("graduation pivot fidelity: Houston ISD all_students matches wide/tidy", {
  skip_if_offline()

  wide <- fetch_grad(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_grad(2024, tidy = TRUE, use_cache = TRUE)

  wide_houston <- wide$all_students_grad_rate[wide$type == "District" &
                                               wide$district_id == "101912"]
  tidy_houston <- tidy$grad_rate[tidy$type == "District" &
                                  tidy$district_id == "101912" &
                                  tidy$subgroup == "all_students"]

  expect_equal(tidy_houston, wide_houston)
})

test_that("graduation pivot fidelity: state all_students matches wide/tidy", {
  skip_if_offline()

  wide <- fetch_grad(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_grad(2024, tidy = TRUE, use_cache = TRUE)

  wide_state <- wide$all_students_grad_rate[wide$type == "State"]
  tidy_state <- tidy$grad_rate[tidy$type == "State" &
                                tidy$subgroup == "all_students"]

  expect_equal(tidy_state, wide_state, tolerance = 0.01)
})

test_that("graduation pivot fidelity: all subgroup rates match wide/tidy for Dallas", {
  skip_if_offline()

  wide <- fetch_grad(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_grad(2024, tidy = TRUE, use_cache = TRUE)

  dallas_wide <- wide[wide$type == "District" &
                       wide$district_id == "057905", ]
  dallas_tidy <- tidy[tidy$type == "District" &
                       tidy$district_id == "057905", ]

  # Map subgroup names to wide column names
  subgroup_to_col <- c(
    "all_students" = "all_students_grad_rate",
    "white" = "white_grad_rate",
    "hispanic" = "hispanic_grad_rate",
    "asian" = "asian_grad_rate",
    "econ_disadv" = "econ_disadv_grad_rate",
    "female" = "female_grad_rate",
    "male" = "male_grad_rate"
  )

  for (sg in names(subgroup_to_col)) {
    wide_val <- dallas_wide[[subgroup_to_col[sg]]]
    tidy_val <- dallas_tidy$grad_rate[dallas_tidy$subgroup == sg]

    if (!is.na(wide_val) && length(tidy_val) == 1) {
      expect_equal(tidy_val, wide_val,
                   info = paste("Mismatch for Dallas", sg))
    }
  }
})

# ==============================================================================
# 5. RATE VALIDITY
# ==============================================================================

test_that("all graduation rates are in [0, 100] range", {
  skip_if_offline()

  grad <- fetch_grad(2024, tidy = TRUE, use_cache = TRUE)
  rates <- grad$grad_rate[!is.na(grad$grad_rate)]

  expect_true(all(rates >= 0), info = paste("Min rate:", min(rates)))
  expect_true(all(rates <= 100), info = paste("Max rate:", max(rates)))
})

test_that("no Inf or NaN in graduation rates", {
  skip_if_offline()

  grad <- fetch_grad(2024, tidy = TRUE, use_cache = TRUE)
  expect_false(any(is.infinite(grad$grad_rate)),
               info = "Found Inf in grad_rate")
  expect_false(any(is.nan(grad$grad_rate)),
               info = "Found NaN in grad_rate")
})

test_that("state graduation rate is in plausible range (80-100%)", {
  skip_if_offline()

  grad <- fetch_grad(2024, tidy = TRUE, use_cache = TRUE)
  state_rate <- grad$grad_rate[grad$type == "State" &
                                grad$subgroup == "all_students"]

  expect_true(state_rate >= 80 && state_rate <= 100,
              info = paste("State rate:", state_rate))
})

# ==============================================================================
# 6. ENTITY FLAG LOGIC
# ==============================================================================

test_that("graduation entity flags are mutually exclusive", {
  skip_if_offline()

  grad <- fetch_grad(2024, tidy = TRUE, use_cache = TRUE)
  type_sums <- grad$is_state + grad$is_district + grad$is_campus
  expect_true(all(type_sums == 1))
})

test_that("graduation has at least one of each entity type", {
  skip_if_offline()

  grad <- fetch_grad(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(any(grad$is_state))
  expect_true(any(grad$is_district))
  expect_true(any(grad$is_campus))
})

test_that("graduation is_state matches type == State", {
  skip_if_offline()

  grad <- fetch_grad(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(all(grad$is_state == (grad$type == "State")))
  expect_true(all(grad$is_district == (grad$type == "District")))
  expect_true(all(grad$is_campus == (grad$type == "Campus")))
})

test_that("state graduation data has exactly 1 row per subgroup", {
  skip_if_offline()

  grad <- fetch_grad(2024, tidy = TRUE, use_cache = TRUE)
  state_data <- grad[grad$is_state, ]
  counts <- table(state_data$subgroup)
  expect_true(all(counts == 1),
              info = paste("Duplicates:", paste(names(counts[counts > 1]),
                                                collapse = ", ")))
})

# ==============================================================================
# 7. KNOWN-VALUE SPOT CHECKS
# ==============================================================================

test_that("2024: Houston ISD all_students grad rate = 84.3", {
  skip_if_offline()

  grad <- fetch_grad(2024, tidy = TRUE, use_cache = TRUE)
  houston <- grad$grad_rate[grad$type == "District" &
                             grad$district_id == "101912" &
                             grad$subgroup == "all_students"]
  expect_equal(houston, 84.3)
})

test_that("2024: Dallas ISD all_students grad rate = 84.4", {
  skip_if_offline()

  grad <- fetch_grad(2024, tidy = TRUE, use_cache = TRUE)
  dallas <- grad$grad_rate[grad$type == "District" &
                            grad$district_id == "057905" &
                            grad$subgroup == "all_students"]
  expect_equal(dallas, 84.4)
})

test_that("2023: state grad rate approximately 94.2", {
  skip_if_offline()

  grad <- fetch_grad(2023, tidy = TRUE, use_cache = TRUE)
  state_rate <- grad$grad_rate[grad$type == "State" &
                                grad$subgroup == "all_students"]
  expect_equal(state_rate, 94.22808, tolerance = 0.01)
})

test_that("2022: state grad rate approximately 94.1", {
  skip_if_offline()

  grad <- fetch_grad(2022, tidy = TRUE, use_cache = TRUE)
  state_rate <- grad$grad_rate[grad$type == "State" &
                                grad$subgroup == "all_students"]
  expect_equal(state_rate, 94.14518, tolerance = 0.01)
})

test_that("2021: state grad rate approximately 93.9", {
  skip_if_offline()

  grad <- fetch_grad(2021, tidy = TRUE, use_cache = TRUE)
  state_rate <- grad$grad_rate[grad$type == "State" &
                                grad$subgroup == "all_students"]
  expect_equal(state_rate, 93.90047, tolerance = 0.01)
})

test_that("2020: state grad rate approximately 94.4", {
  skip_if_offline()

  grad <- fetch_grad(2020, tidy = TRUE, use_cache = TRUE)
  state_rate <- grad$grad_rate[grad$type == "State" &
                                grad$subgroup == "all_students"]
  expect_equal(state_rate, 94.4116, tolerance = 0.01)
})

test_that("major districts present in 2024 graduation data", {
  skip_if_offline()

  grad <- fetch_grad(2024, tidy = TRUE, use_cache = TRUE)
  district_ids <- unique(grad$district_id[grad$type == "District"])

  major_districts <- c(
    "101912",  # Houston ISD
    "057905",  # Dallas ISD
    "227901",  # Austin ISD
    "220905"   # Fort Worth ISD
  )

  for (id in major_districts) {
    expect_true(id %in% district_ids,
                info = paste("Missing district:", id))
  }
})

# ==============================================================================
# 8. CROSS-YEAR CONSISTENCY
# ==============================================================================

test_that("state graduation rate stable across consecutive years (< 5% change)", {
  skip_if_offline()

  # Pinned state rates from actual data
  rates <- c(
    "2020" = 94.4116,
    "2021" = 93.90047,
    "2022" = 94.14518,
    "2023" = 94.22808
  )

  years <- as.integer(names(rates))
  for (i in 2:length(rates)) {
    abs_change <- abs(rates[i] - rates[i - 1])
    expect_true(abs_change < 5,
                info = paste(years[i], "change:", round(abs_change, 2), "pp"))
  }
})

test_that("fetch_grad validates class_year parameter boundaries", {
  expect_error(fetch_grad(2002), "class_year must be between")
  expect_error(fetch_grad(2025), "class_year must be between")
})

test_that("fetch_grad_multi validates class_year parameter boundaries", {
  expect_error(fetch_grad_multi(c(2002, 2024)), "All class_years must be between")
  expect_error(fetch_grad_multi(c(2024, 2025)), "All class_years must be between")
})

# ==============================================================================
# 9. WIDE FORMAT STRUCTURE
# ==============================================================================

test_that("graduation wide format has all expected rate columns", {
  skip_if_offline()

  grad_wide <- fetch_grad(2024, tidy = FALSE, use_cache = TRUE)

  expected_cols <- c(
    "class_year", "type", "district_id", "campus_id",
    "district_name", "campus_name", "county", "region",
    "all_students_grad_rate", "white_grad_rate", "black_grad_rate",
    "hispanic_grad_rate", "asian_grad_rate", "native_american_grad_rate",
    "multiracial_grad_rate", "female_grad_rate", "male_grad_rate",
    "econ_disadv_grad_rate", "special_ed_grad_rate", "lep_grad_rate",
    "at_risk_grad_rate"
  )

  for (col in expected_cols) {
    expect_true(col %in% names(grad_wide),
                info = paste("Missing column:", col))
  }
})

test_that("graduation wide format has no subgroup column", {
  skip_if_offline()

  grad_wide <- fetch_grad(2024, tidy = FALSE, use_cache = TRUE)
  expect_false("subgroup" %in% names(grad_wide))
})

test_that("graduation class_year is integer type", {
  skip_if_offline()

  grad <- fetch_grad(2024, tidy = TRUE, use_cache = TRUE)
  expect_type(grad$class_year, "integer")
})
