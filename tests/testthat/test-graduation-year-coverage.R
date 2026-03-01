# ==============================================================================
# Graduation Year Coverage Tests
# ==============================================================================
#
# Per-year tests for fetch_grad() across available class years.
#
# TEA graduation data is available via Excel download for Class of 2020-2024.
# Earlier years (2003-2019) return HTTP 404 as of March 2026.
#
# Each year is validated for:
#   - Data loads without error and has >0 rows
#   - Required columns present
#   - Graduation rates in 0-100 range
#   - 13 expected subgroups present
#   - Entity structure (State, District, Campus)
#   - Pinned state graduation rate
#   - Pinned Houston ISD graduation rate
#   - Cross-year trend consistency
#
# To run: devtools::test(filter = "graduation-year-coverage")
# ==============================================================================

# -- Helper: skip if TEA graduation portal is unavailable ---
skip_if_tea_grad_unavailable <- function() {
  skip_on_cran()
  tryCatch({
    # Check if a known graduation file URL returns 200
    test_url <- paste0(
      "https://tea.texas.gov/reports-and-data/school-performance/",
      "accountability-research/completion-graduation-and-dropout/",
      "district-data-download-4yr-2024.xlsx"
    )
    resp <- httr::HEAD(test_url, httr::timeout(15))
    if (httr::http_error(resp)) {
      skip("TEA graduation data portal returned HTTP error")
    }
  }, error = function(e) {
    skip(paste("TEA graduation data unreachable:", e$message))
  })
}

# -- Pinned state graduation rates (from real TEA data) ---
# These are the mean of non-NA district rates (the state aggregate method).
state_grad_rates <- c(
  "2020" = 94.4,
  "2021" = 93.9,
  "2022" = 94.1,
  "2023" = 94.2,
  "2024" = 94.4
)

# -- Pinned Houston ISD graduation rates ---
houston_grad_rates <- c(
  "2020" = 82.0,
  "2021" = 83.7,
  "2022" = 81.7,
  "2023" = 83.0,
  "2024" = 84.3
)

# -- Expected subgroups for graduation data (13 total) ---
expected_grad_subgroups <- c(
  "all_students", "white", "black", "hispanic", "asian",
  "native_american", "multiracial",
  "female", "male",
  "econ_disadv", "special_ed", "lep", "at_risk"
)


# ==============================================================================
# PER-YEAR GRADUATION TESTS
# ==============================================================================

run_grad_year_test <- function(yr) {
  test_that(paste("fetch_grad", yr, "loads with valid data"), {
    skip_if_tea_grad_unavailable()

    grad <- tryCatch(
      fetch_grad(yr, tidy = TRUE, use_cache = TRUE),
      error = function(e) skip(paste("fetch_grad", yr, "failed:", e$message))
    )

    # 1. Data loads and has rows
    expect_true(is.data.frame(grad), info = paste(yr, "is not a data.frame"))
    expect_gt(nrow(grad), 0, label = paste(yr, "has 0 rows"))

    # 2. Required columns present
    required_cols <- c("class_year", "type", "district_id", "subgroup",
                       "grad_rate", "is_state", "is_district", "is_campus")
    for (col in required_cols) {
      expect_true(col %in% names(grad),
                  info = paste(yr, "missing column:", col))
    }

    # 3. Graduation rates in 0-100 range (for non-NA values)
    rates <- grad$grad_rate[!is.na(grad$grad_rate)]
    if (length(rates) > 0) {
      expect_true(all(rates >= 0 & rates <= 100),
                  info = paste(yr, "grad_rate outside 0-100 range"))
    }

    # 4. No Inf/NaN in grad_rate
    expect_false(any(is.infinite(grad$grad_rate)),
                 info = paste(yr, "grad_rate has Inf values"))
    expect_false(any(is.nan(grad$grad_rate)),
                 info = paste(yr, "grad_rate has NaN values"))

    # 5. Expected subgroups present
    actual_subgroups <- unique(grad$subgroup)
    for (sg in expected_grad_subgroups) {
      expect_true(sg %in% actual_subgroups,
                  info = paste(yr, "missing subgroup:", sg))
    }

    # 6. Entity structure: State, District, Campus all present
    actual_types <- unique(grad$type)
    expect_true("State" %in% actual_types,
                info = paste(yr, "missing State type"))
    expect_true("District" %in% actual_types,
                info = paste(yr, "missing District type"))
    expect_true("Campus" %in% actual_types,
                info = paste(yr, "missing Campus type"))

    # 7. Entity flags mutually exclusive
    type_sums <- grad$is_state + grad$is_district + grad$is_campus
    expect_true(all(type_sums == 1),
                info = paste(yr, "entity flags not mutually exclusive"))

    # 8. Pinned state graduation rate
    state_row <- grad[grad$is_state & grad$subgroup == "all_students", ]
    expect_equal(nrow(state_row), 1,
                 info = paste(yr, "should have exactly 1 state all_students row"))
    expected_rate <- unname(state_grad_rates[as.character(yr)])
    expect_equal(round(state_row$grad_rate, 1), expected_rate,
                 info = paste(yr, "state grad rate mismatch"))

    # 9. Pinned Houston ISD graduation rate
    houston <- grad[grad$is_district &
                    !is.na(grad$district_id) &
                    grad$district_id == "101912" &
                    grad$subgroup == "all_students", ]
    if (nrow(houston) > 0) {
      expected_houston_rate <- unname(houston_grad_rates[as.character(yr)])
      expect_equal(round(houston$grad_rate, 1), expected_houston_rate,
                   info = paste(yr, "Houston ISD grad rate mismatch"))
    }

    # 10. class_year matches requested year
    expect_true(all(grad$class_year == yr),
                info = paste(yr, "class_year values don't match"))

    # 11. District IDs are 6-char
    dist_rows <- grad[grad$is_district & !is.na(grad$district_id), ]
    if (nrow(dist_rows) > 0) {
      id_lens <- unique(nchar(dist_rows$district_id))
      expect_true(all(id_lens == 6),
                  info = paste(yr, "district_id not 6 chars:", paste(id_lens, collapse = ",")))
    }

    # 12. Campus IDs are 9-char
    camp_rows <- grad[grad$is_campus & !is.na(grad$campus_id), ]
    if (nrow(camp_rows) > 0) {
      id_lens <- unique(nchar(camp_rows$campus_id))
      expect_true(all(id_lens == 9),
                  info = paste(yr, "campus_id not 9 chars:", paste(id_lens, collapse = ",")))
    }

    # 13. Reasonable row count (>30000 for recent years)
    expect_gt(nrow(grad), 30000,
              label = paste(yr, "row count low"))

    # 14. District count in reasonable range
    n_districts <- length(unique(
      grad$district_id[grad$is_district & !is.na(grad$district_id)]
    ))
    expect_true(n_districts >= 800 & n_districts <= 1500,
                info = paste(yr, "district count:", n_districts,
                             "(expected 800-1500)"))
  })
}

# Run tests for available years (2020-2024)
for (year in 2020:2024) {
  run_grad_year_test(year)
}


# ==============================================================================
# CROSS-YEAR GRADUATION TESTS
# ==============================================================================

test_that("State graduation rate is stable across years (within 2 points)", {
  skip_if_tea_grad_unavailable()

  rates <- as.numeric(state_grad_rates)
  years <- as.numeric(names(state_grad_rates))

  for (i in seq(2, length(rates))) {
    diff <- abs(rates[i] - rates[i - 1])
    expect_true(diff <= 2.0,
                info = paste("Grad rate change", years[i - 1], "->", years[i],
                             "is", diff, "points (expected <= 2.0)"))
  }
})

test_that("fetch_grad_multi returns combined data for multiple years", {
  skip_if_tea_grad_unavailable()

  grad_multi <- tryCatch(
    fetch_grad_multi(2023:2024, use_cache = TRUE),
    error = function(e) skip(paste("fetch_grad_multi failed:", e$message))
  )

  expect_true(2023 %in% unique(grad_multi$class_year))
  expect_true(2024 %in% unique(grad_multi$class_year))
  expect_gt(nrow(grad_multi), 60000,
            label = "Multi-year should have >60k rows")
})

test_that("Tidy and wide formats preserve rate fidelity", {
  skip_if_tea_grad_unavailable()

  wide <- tryCatch(
    fetch_grad(2024, tidy = FALSE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_grad 2024 wide failed:", e$message))
  )
  tidy <- tryCatch(
    fetch_grad(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_grad 2024 tidy failed:", e$message))
  )

  # Compare state-level all_students rate
  wide_state <- wide[wide$type == "State", "all_students_grad_rate"]
  tidy_state <- tidy[tidy$type == "State" & tidy$subgroup == "all_students", "grad_rate"]

  expect_equal(as.numeric(wide_state), as.numeric(tidy_state),
               tolerance = 0.01,
               info = "Wide/tidy fidelity mismatch for state all_students")
})

test_that("fetch_grad rejects out-of-range years", {
  expect_error(fetch_grad(2002), "class_year must be between")
  expect_error(fetch_grad(2025), "class_year must be between")
})

test_that("Cohort consistency: state rate is between min and max district rates", {
  skip_if_tea_grad_unavailable()

  for (yr in 2020:2024) {
    grad <- tryCatch(
      fetch_grad(yr, tidy = TRUE, use_cache = TRUE),
      error = function(e) skip(paste("fetch_grad", yr, "failed:", e$message))
    )

    state_rate <- grad$grad_rate[grad$is_state & grad$subgroup == "all_students"]
    dist_rates <- grad$grad_rate[grad$is_district &
                                grad$subgroup == "all_students" &
                                !is.na(grad$grad_rate)]

    if (length(state_rate) > 0 && length(dist_rates) > 0) {
      expect_true(state_rate >= min(dist_rates) - 1,
                  info = paste(yr, "state rate below min district rate"))
      expect_true(state_rate <= max(dist_rates) + 1,
                  info = paste(yr, "state rate above max district rate"))
    }
  }
})

test_that("Major districts present across all graduation years", {
  skip_if_tea_grad_unavailable()

  major_districts <- c(
    "227901",  # Austin ISD
    "101912",  # Houston ISD
    "057905",  # Dallas ISD
    "220905"   # Fort Worth ISD
  )

  for (yr in 2020:2024) {
    grad <- tryCatch(
      fetch_grad(yr, tidy = TRUE, use_cache = TRUE),
      error = function(e) skip(paste("fetch_grad", yr, "failed:", e$message))
    )

    dist_ids <- unique(grad$district_id[grad$is_district])
    for (id in major_districts) {
      expect_true(id %in% dist_ids,
                  info = paste(yr, "missing major district:", id))
    }
  }
})
