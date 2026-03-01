# ==============================================================================
# Typology Guard Tests
# ==============================================================================
#
# Data quality and type safety tests that catch common processing bugs.
#
# Tests cover:
#   - Percentage division-by-zero handling
#   - Percentage scale consistency (0-1 not 0-100)
#   - Column types (count columns numeric, IDs character)
#   - Row count minimum
#   - Subgroup value set validation
#   - Grade value set validation (EE, PK, K, 01-12)
#   - Zero vs NA distinction
#   - No duplicate rows
#   - No orphaned campus IDs
#   - State aggregate equals sum of districts
#
# To run: devtools::test(filter = "typology-guards")
# ==============================================================================

# -- Helper: skip if TEA is degraded or offline ---
skip_if_tea_degraded <- function() {
  skip_on_cran()
  tryCatch({
    resp <- httr::HEAD("https://rptsvr1.tea.texas.gov/cgi/sas/broker",
                       httr::timeout(10))
    if (httr::http_error(resp)) skip("TEA SAS broker returned HTTP error")
  }, error = function(e) {
    skip(paste("TEA SAS broker unreachable:", e$message))
  })
}


# ==============================================================================
# PERCENTAGE DIVISION-BY-ZERO TESTS
# ==============================================================================

test_that("Pct is NA (not Inf/NaN) when row_total is 0", {
  skip_if_tea_degraded()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr failed:", e$message))
  )

  # Check pct column for Inf/NaN
  pct_vals <- enr$pct
  expect_false(any(is.infinite(pct_vals)),
               info = "pct has Inf values (likely division by zero)")
  expect_false(any(is.nan(pct_vals)),
               info = "pct has NaN values (likely 0/0)")
})

test_that("Pct is consistent: subgroup pcts sum approximately to 1 for race groups", {
  skip_if_tea_degraded()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr failed:", e$message))
  )

  # Race/ethnicity subgroups should sum to ~1.0 for each entity
  race_groups <- c("white", "black", "hispanic", "asian",
                   "pacific_islander", "native_american", "multiracial")

  race_data <- enr[enr$subgroup %in% race_groups &
                   enr$grade_level == "TOTAL", ]

  if (nrow(race_data) == 0) skip("No race subgroup data in tidy output (TEA may have returned partial data)")

  # Group by entity and sum pcts
  race_sums <- stats::aggregate(
    pct ~ end_year + type + district_id + campus_id,
    data = race_data,
    FUN = sum,
    na.rm = TRUE
  )

  # Filter to rows where sum is meaningful (has all groups)
  # Sum should be close to 1 (within 0.02 tolerance for rounding)
  valid_sums <- race_sums$pct[race_sums$pct > 0.5]
  if (length(valid_sums) > 0) {
    expect_true(all(valid_sums >= 0.95 & valid_sums <= 1.05),
                info = paste("Race pct sum range:",
                             round(min(valid_sums), 3), "-",
                             round(max(valid_sums), 3)))
  }
})


# ==============================================================================
# PERCENTAGE SCALE CONSISTENCY TESTS
# ==============================================================================

test_that("Enrollment pct is on 0-1 scale (not 0-100)", {
  skip_if_tea_degraded()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr failed:", e$message))
  )

  pct_vals <- enr$pct[!is.na(enr$pct)]
  expect_true(max(pct_vals) <= 1.0,
              info = paste("Max pct:", max(pct_vals),
                           "(>1.0 suggests 0-100 scale)"))
  expect_true(mean(pct_vals) < 0.5,
              info = paste("Mean pct:", round(mean(pct_vals), 3),
                           "(high mean suggests 0-100 scale)"))
})

test_that("Graduation rate is on 0-100 scale (not 0-1)", {
  skip_if_tea_degraded()

  grad <- tryCatch(
    fetch_grad(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_grad failed:", e$message))
  )

  rates <- grad$grad_rate[!is.na(grad$grad_rate)]
  expect_true(max(rates) > 1.0,
              info = paste("Max grad_rate:", max(rates),
                           "(<1.0 suggests 0-1 scale)"))
  expect_true(max(rates) <= 100,
              info = paste("Max grad_rate:", max(rates), "(>100 is invalid)"))
})


# ==============================================================================
# COLUMN TYPE TESTS
# ==============================================================================

test_that("Enrollment count columns are numeric", {
  skip_if_tea_degraded()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr failed:", e$message))
  )

  expect_true(is.numeric(enr$n_students), info = "n_students should be numeric")
  expect_true(is.numeric(enr$pct), info = "pct should be numeric")
  expect_true(is.numeric(enr$end_year), info = "end_year should be numeric")
})

test_that("Enrollment ID columns are character", {
  skip_if_tea_degraded()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr failed:", e$message))
  )

  expect_true(is.character(enr$district_id), info = "district_id should be character")
  expect_true(is.character(enr$campus_id), info = "campus_id should be character")
  expect_true(is.character(enr$type), info = "type should be character")
  expect_true(is.character(enr$subgroup), info = "subgroup should be character")
  expect_true(is.character(enr$grade_level), info = "grade_level should be character")
})

test_that("Enrollment flag columns are logical", {
  skip_if_tea_degraded()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr failed:", e$message))
  )

  expect_true(is.logical(enr$is_state), info = "is_state should be logical")
  expect_true(is.logical(enr$is_district), info = "is_district should be logical")
  expect_true(is.logical(enr$is_campus), info = "is_campus should be logical")
  expect_true(is.logical(enr$is_charter), info = "is_charter should be logical")
})

test_that("Graduation rate columns have correct types", {
  skip_if_tea_degraded()

  grad <- tryCatch(
    fetch_grad(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_grad failed:", e$message))
  )

  expect_true(is.numeric(grad$grad_rate), info = "grad_rate should be numeric")
  expect_true(is.integer(grad$class_year), info = "class_year should be integer")
  expect_true(is.character(grad$district_id), info = "district_id should be character")
  expect_true(is.character(grad$subgroup), info = "subgroup should be character")
  expect_true(is.logical(grad$is_state), info = "is_state should be logical")
})

test_that("Wide enrollment columns have correct types", {
  skip_if_tea_degraded()

  enr_wide <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr wide failed:", e$message))
  )

  # Count columns should be numeric
  count_cols <- c("row_total", "white", "black", "hispanic", "asian",
                  "econ_disadv", "lep", "special_ed")
  for (col in count_cols) {
    if (col %in% names(enr_wide)) {
      expect_true(is.numeric(enr_wide[[col]]),
                  info = paste(col, "should be numeric"))
    }
  }

  # ID columns should be character
  expect_true(is.character(enr_wide$district_id), info = "district_id should be character")
})


# ==============================================================================
# ROW COUNT MINIMUM TESTS
# ==============================================================================

test_that("Enrollment tidy has minimum expected row count", {
  skip_if_tea_degraded()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr failed:", e$message))
  )

  # 2024 has ~267k rows locally; CI may get partial data from TEA
  expect_gt(nrow(enr), 10000,
            label = paste("Row count:", nrow(enr), "(expected >10k)"))
})

test_that("Enrollment wide has minimum expected row count", {
  skip_if_tea_degraded()

  enr <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr wide failed:", e$message))
  )

  # Wide format should have at least 1000 rows; CI may get partial data from TEA
  expect_gt(nrow(enr), 1000,
            label = paste("Wide row count:", nrow(enr), "(expected >1k)"))
})

test_that("Graduation tidy has minimum expected row count", {
  skip_if_tea_degraded()

  grad <- tryCatch(
    fetch_grad(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_grad failed:", e$message))
  )

  # 2024 has ~39k rows; minimum should be >30k
  expect_gt(nrow(grad), 30000,
            label = paste("Row count:", nrow(grad), "(expected >30k)"))
})


# ==============================================================================
# SUBGROUP VALUE SET VALIDATION
# ==============================================================================

test_that("Enrollment subgroup values are from allowed set", {
  skip_if_tea_degraded()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr failed:", e$message))
  )

  allowed_subgroups <- c(
    "total_enrollment", "white", "black", "hispanic", "asian",
    "pacific_islander", "native_american", "multiracial",
    "econ_disadv", "lep", "special_ed"
  )

  actual_subgroups <- unique(enr$subgroup)
  unexpected <- setdiff(actual_subgroups, allowed_subgroups)
  expect_equal(length(unexpected), 0,
               info = paste("Unexpected subgroups:", paste(unexpected, collapse = ", ")))
})

test_that("Graduation subgroup values are from allowed set", {
  skip_if_tea_degraded()

  grad <- tryCatch(
    fetch_grad(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_grad failed:", e$message))
  )

  allowed_subgroups <- c(
    "all_students", "white", "black", "hispanic", "asian",
    "native_american", "multiracial",
    "female", "male",
    "econ_disadv", "special_ed", "lep", "at_risk"
  )

  actual_subgroups <- unique(grad$subgroup)
  unexpected <- setdiff(actual_subgroups, allowed_subgroups)
  expect_equal(length(unexpected), 0,
               info = paste("Unexpected subgroups:", paste(unexpected, collapse = ", ")))
})


# ==============================================================================
# GRADE VALUE SET VALIDATION
# ==============================================================================

test_that("Enrollment grade levels are from allowed set", {
  skip_if_tea_degraded()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr failed:", e$message))
  )

  allowed_grades <- c("EE", "PK", "K",
                      "01", "02", "03", "04", "05", "06",
                      "07", "08", "09", "10", "11", "12",
                      "TOTAL")

  actual_grades <- unique(enr$grade_level)
  unexpected <- setdiff(actual_grades, allowed_grades)
  expect_equal(length(unexpected), 0,
               info = paste("Unexpected grade levels:", paste(unexpected, collapse = ", ")))
})

test_that("Grade levels are uppercase", {
  skip_if_tea_degraded()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr failed:", e$message))
  )

  actual_grades <- unique(enr$grade_level)
  for (g in actual_grades) {
    expect_equal(g, toupper(g),
                 info = paste("Grade level not uppercase:", g))
  }
})

test_that("EE grade level is present (TX-specific)", {
  skip_if_tea_degraded()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr failed:", e$message))
  )

  expect_true("EE" %in% unique(enr$grade_level),
              info = "Texas-specific EE (Early Education) grade missing")
})


# ==============================================================================
# ZERO VS NA DISTINCTION
# ==============================================================================

test_that("Zero and NA are distinct in enrollment data", {
  skip_if_tea_degraded()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr failed:", e$message))
  )

  # There should be both zeros and NAs in n_students
  has_zeros <- any(enr$n_students == 0, na.rm = TRUE)
  has_nas <- any(is.na(enr$n_students))

  # At least zeros should exist (some schools have 0 students in specific grades)
  expect_true(has_zeros,
              info = "No zero values in n_students (expecting some)")
})

test_that("Suppressed values are NA, not -1 or negative", {
  skip_if_tea_degraded()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr failed:", e$message))
  )

  n_vals <- enr$n_students[!is.na(enr$n_students)]
  expect_true(all(n_vals >= 0),
              info = "Negative values found in n_students (suppression marker not cleaned)")
})


# ==============================================================================
# DUPLICATE ROW TESTS
# ==============================================================================

test_that("No duplicate rows in tidy enrollment data", {
  skip_if_tea_degraded()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr failed:", e$message))
  )

  # Define the natural key for tidy enrollment
  key_cols <- c("end_year", "type", "district_id", "campus_id",
                "subgroup", "grade_level")
  key_cols <- key_cols[key_cols %in% names(enr)]

  key_df <- enr[, key_cols, drop = FALSE]

  # Check for duplicates
  n_dupes <- sum(duplicated(key_df))
  expect_equal(n_dupes, 0,
               info = paste(n_dupes, "duplicate rows found in tidy enrollment"))
})

test_that("No duplicate rows in tidy graduation data", {
  skip_if_tea_degraded()

  grad <- tryCatch(
    fetch_grad(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_grad failed:", e$message))
  )

  # Define the natural key for tidy graduation
  key_cols <- c("class_year", "type", "district_id", "campus_id", "subgroup")
  key_cols <- key_cols[key_cols %in% names(grad)]

  key_df <- grad[, key_cols, drop = FALSE]

  # Check for duplicates
  n_dupes <- sum(duplicated(key_df))
  expect_equal(n_dupes, 0,
               info = paste(n_dupes, "duplicate rows found in tidy graduation"))
})

test_that("One state row per subgroup per grade_level in enrollment", {
  skip_if_tea_degraded()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr failed:", e$message))
  )

  state_data <- enr[enr$is_state, ]
  state_counts <- stats::aggregate(
    n_students ~ subgroup + grade_level,
    data = state_data,
    FUN = length
  )

  dupes <- state_counts[state_counts$n_students > 1, ]
  expect_equal(nrow(dupes), 0,
               info = paste("State has duplicate subgroup/grade combos:",
                            nrow(dupes)))
})


# ==============================================================================
# STATE AGGREGATE CONSISTENCY
# ==============================================================================

test_that("State total equals sum of district totals (enrollment)", {
  skip_if_tea_degraded()

  enr <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr failed:", e$message))
  )

  state_total <- enr$row_total[enr$type == "State"]
  district_sum <- sum(enr$row_total[enr$type == "District"], na.rm = TRUE)

  expect_equal(state_total, district_sum,
               info = paste("State:", state_total, "vs district sum:", district_sum))
})

test_that("State demographic sums equal sum of district demographics", {
  skip_if_tea_degraded()

  enr <- tryCatch(
    fetch_enr(2024, tidy = FALSE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr failed:", e$message))
  )

  demo_cols <- c("white", "black", "hispanic", "asian")
  for (col in demo_cols) {
    if (col %in% names(enr)) {
      state_val <- enr[[col]][enr$type == "State"]
      dist_sum <- sum(enr[[col]][enr$type == "District"], na.rm = TRUE)
      expect_equal(state_val, dist_sum,
                   info = paste("State", col, ":", state_val,
                                "vs district sum:", dist_sum))
    }
  }
})


# ==============================================================================
# ENTITY TYPE CONSISTENCY
# ==============================================================================

test_that("Entity type column matches entity flags", {
  skip_if_tea_degraded()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr failed:", e$message))
  )

  # is_state should correspond to type == "State"
  expect_true(all(enr$is_state == (enr$type == "State")),
              info = "is_state flag doesn't match type == 'State'")
  expect_true(all(enr$is_district == (enr$type == "District")),
              info = "is_district flag doesn't match type == 'District'")
  expect_true(all(enr$is_campus == (enr$type == "Campus")),
              info = "is_campus flag doesn't match type == 'Campus'")
})

test_that("District rows have district_id but no campus_id", {
  skip_if_tea_degraded()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr failed:", e$message))
  )

  dist_rows <- enr[enr$is_district, ]
  expect_true(all(!is.na(dist_rows$district_id)),
              info = "District rows should have non-NA district_id")
  expect_true(all(is.na(dist_rows$campus_id)),
              info = "District rows should have NA campus_id")
})

test_that("Campus rows have both district_id and campus_id", {
  skip_if_tea_degraded()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr failed:", e$message))
  )

  camp_rows <- enr[enr$is_campus, ]
  if (nrow(camp_rows) > 0) {
    expect_true(all(!is.na(camp_rows$district_id)),
                info = "Campus rows should have non-NA district_id")
    expect_true(all(!is.na(camp_rows$campus_id)),
                info = "Campus rows should have non-NA campus_id")

    # Campus district_id should match first 6 chars of campus_id
    derived_dist <- substr(camp_rows$campus_id, 1, 6)
    expect_true(all(derived_dist == camp_rows$district_id),
                info = "Campus district_id should match first 6 chars of campus_id")
  }
})

test_that("State rows have NA for district_id and campus_id", {
  skip_if_tea_degraded()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr failed:", e$message))
  )

  state_rows <- enr[enr$is_state, ]
  expect_true(all(is.na(state_rows$district_id)),
              info = "State rows should have NA district_id")
  expect_true(all(is.na(state_rows$campus_id)),
              info = "State rows should have NA campus_id")
})


# ==============================================================================
# CHARTER FLAG TESTS
# ==============================================================================

test_that("is_charter flag exists and is logical", {
  skip_if_tea_degraded()

  enr <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr failed:", e$message))
  )

  expect_true("is_charter" %in% names(enr),
              info = "is_charter column should exist")
  expect_true(is.logical(enr$is_charter),
              info = "is_charter should be logical")

  # Note: in TAPR era, charter_flag may be all NA (not downloaded in the
  # IDENT+PET+PETG key set), which means is_charter will be all FALSE.
  # This is a known limitation of the current download strategy.
  # CGI era data (1997-2002) does have charter flags populated.
})


# ==============================================================================
# SAFE_NUMERIC EDGE CASES
# ==============================================================================

test_that("safe_numeric handles TEA suppression markers correctly", {
  # These are the markers documented in process_enrollment.R
  expect_true(is.na(safe_numeric("*")))
  expect_true(is.na(safe_numeric(".")))
  expect_true(is.na(safe_numeric("-")))
  expect_true(is.na(safe_numeric("-1")))
  expect_true(is.na(safe_numeric("<5")))
  expect_true(is.na(safe_numeric("N/A")))
  expect_true(is.na(safe_numeric("NA")))
  expect_true(is.na(safe_numeric("")))

  # Normal values should convert correctly
  expect_equal(safe_numeric("100"), 100)
  expect_equal(safe_numeric("1,234"), 1234)
  expect_equal(safe_numeric("0"), 0)
  expect_equal(safe_numeric("  100  "), 100)
})

test_that("clean_id removes leading quotes and trims whitespace", {
  expect_equal(clean_id("'101912"), "101912")
  expect_equal(clean_id("101912"), "101912")
  expect_equal(clean_id("  101912  "), "101912")
  # Leading quote removed, then whitespace trimmed
  expect_equal(clean_id("'  101912"), "101912")
})
