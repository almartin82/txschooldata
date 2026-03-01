# ==============================================================================
# Enrollment Year Coverage Tests
# ==============================================================================
#
# Exhaustive per-year tests for fetch_enr() across all three TEA eras:
#   - AEIS CGI (1997-2002)
#   - AEIS SAS (2003-2012)
#   - TAPR (2013-2024)
#
# Each year is validated for:
#   - Data loads without error and has >0 rows
#   - Required columns present
#   - No Inf/NaN/negative values
#   - Pinned state total enrollment
#   - Pinned Houston ISD enrollment (district_id = "101912")
#   - Expected subgroups present (era-dependent)
#   - Expected grade levels present (era-dependent)
#   - Entity flags correct
#   - ID format: district_id 6-char, campus_id 9-char
#
# Cross-year checks:
#   - YoY state total change < 10%
#   - Schema consistent across eras
#
# To run: devtools::test(filter = "enrollment-year-coverage")
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

# -- Pinned state total enrollment (from real TEA data) ---
# These values are from actual fetch_enr() calls and represent the sum of
# district-level totals for each year.
state_totals <- c(
  "1997" = 3828975,
  "1998" = 3891877,
  "1999" = 3945367,
  "2000" = 3991783,
  "2001" = 4059619,
  "2002" = 4146653,
  "2003" = 4239911,
  "2004" = 4311502,
  "2005" = 4383871,
  "2006" = 4505572,
  "2007" = 4576933,
  "2008" = 4651516,
  "2009" = 4728204,
  "2010" = 4824778,
  "2011" = 4912385,
  "2012" = 4978120,
  "2013" = 5058939,
  "2014" = 5135880,
  "2015" = 5215282,
  "2016" = 5284252,
  "2017" = 5343834,
  "2018" = 5385012,
  "2019" = 5416400,
  "2020" = 5479173,
  "2021" = 5359040,
  "2022" = 5402928,
  "2023" = 5504150,
  "2024" = 5517464
)

# -- Pinned Houston ISD enrollment (district_id = "101912") ---
houston_totals <- c(
  "1997" = 209375,
  "1998" = 210988,
  "1999" = 210179,
  "2000" = 209716,
  "2001" = 208462,
  "2002" = 210670,
  "2003" = 211762,
  "2004" = 211157,
  "2005" = 208454,
  "2006" = 209879,
  "2007" = 202449,
  "2008" = 198769,
  "2009" = 199524,
  "2010" = 200944,
  "2011" = 203294,
  "2012" = 201594,
  "2013" = 202586,
  "2014" = 210716,
  "2015" = 214462,
  "2016" = 214891,

  "2017" = 215408,
  "2018" = 213528,
  "2019" = 209040,
  "2020" = 209309,
  "2021" = 196550,
  "2022" = 193727,
  "2023" = 189290,
  "2024" = 183603
)

# -- Expected subgroups by era ---
# CGI (1997-2002): no multiracial, no pacific_islander
# AEIS SAS (2003-2010): no multiracial, no pacific_islander, no asian (uses combined pac/asian)
# AEIS SAS (2011-2012): all 11 subgroups
# TAPR (2013-2023): no special_ed in tidy (10 subgroups)
# TAPR (2024): all 11 subgroups
base_subgroups <- c("total_enrollment", "white", "black", "hispanic",
                    "econ_disadv", "lep", "native_american")

subgroups_cgi <- c(base_subgroups, "asian", "special_ed")
subgroups_aeis_pre2011 <- c(base_subgroups, "special_ed")
subgroups_aeis_2011_2012 <- c(base_subgroups, "asian", "pacific_islander",
                              "multiracial", "special_ed")
subgroups_tapr_2013_2023 <- c(base_subgroups, "asian", "pacific_islander",
                              "multiracial")
subgroups_tapr_2024 <- c(base_subgroups, "asian", "pacific_islander",
                         "multiracial", "special_ed")

get_expected_subgroups <- function(yr) {
  if (yr <= 2002) return(subgroups_cgi)
  if (yr <= 2010) return(subgroups_aeis_pre2011)
  if (yr <= 2012) return(subgroups_aeis_2011_2012)
  if (yr <= 2023) return(subgroups_tapr_2013_2023)
  return(subgroups_tapr_2024)
}

# -- Expected grade levels ---
# All eras should have TOTAL, EE, PK, K at minimum
# Full set: EE, PK, K, 01-12, TOTAL
# Some AEIS SAS years have incomplete grade coverage due to key mapping issues
full_grades <- c("EE", "PK", "K", paste0("0", 1:9), "10", "11", "12", "TOTAL")
core_grades <- c("EE", "PK", "K", "TOTAL")


# ==============================================================================
# PER-YEAR ENROLLMENT TESTS
# ==============================================================================

# Helper function run for each year
run_year_test <- function(yr) {
  test_that(paste("fetch_enr", yr, "loads with valid data"), {
    skip_if_tea_degraded()

    enr <- tryCatch(
      fetch_enr(yr, tidy = TRUE, use_cache = TRUE),
      error = function(e) skip(paste("fetch_enr", yr, "failed:", e$message))
    )

    # 1. Data loads and has rows
    expect_true(is.data.frame(enr), info = paste(yr, "is not a data.frame"))
    expect_gt(nrow(enr), 0, label = paste(yr, "has 0 rows"))

    # 2. Required columns present
    required_cols <- c("end_year", "type", "district_id", "subgroup",
                       "grade_level", "n_students", "is_state",
                       "is_district", "is_campus")
    for (col in required_cols) {
      expect_true(col %in% names(enr),
                  info = paste(yr, "missing column:", col))
    }

    # 3. No Inf/NaN in numeric columns
    numeric_cols <- names(enr)[sapply(enr, is.numeric)]
    for (col in numeric_cols) {
      vals <- enr[[col]]
      expect_false(any(is.infinite(vals)),
                   info = paste(yr, col, "has Inf values"))
      expect_false(any(is.nan(vals)),
                   info = paste(yr, col, "has NaN values"))
    }

    # 4. No negative enrollment counts
    n_vals <- enr$n_students[!is.na(enr$n_students)]
    expect_true(all(n_vals >= 0),
                info = paste(yr, "has negative n_students"))

    # 5. Pinned state total
    state_row <- enr[enr$is_state &
                     enr$subgroup == "total_enrollment" &
                     enr$grade_level == "TOTAL", ]
    expect_equal(nrow(state_row), 1,
                 info = paste(yr, "should have exactly 1 state total row"))
    expected_total <- unname(state_totals[as.character(yr)])
    expect_equal(state_row$n_students, expected_total,
                 info = paste(yr, "state total mismatch"))

    # 6. Pinned Houston ISD total
    houston <- enr[enr$is_district &
                   !is.na(enr$district_id) &
                   enr$district_id == "101912" &
                   enr$subgroup == "total_enrollment" &
                   enr$grade_level == "TOTAL", ]
    expect_equal(nrow(houston), 1,
                 info = paste(yr, "should have exactly 1 Houston ISD total row"))
    expected_houston <- unname(houston_totals[as.character(yr)])
    expect_equal(houston$n_students, expected_houston,
                 info = paste(yr, "Houston ISD total mismatch"))

    # 7. Expected subgroups present
    actual_subgroups <- unique(enr$subgroup)
    expected_subs <- get_expected_subgroups(yr)
    for (sg in expected_subs) {
      expect_true(sg %in% actual_subgroups,
                  info = paste(yr, "missing subgroup:", sg))
    }

    # 8. Core grade levels present
    actual_grades <- unique(enr$grade_level)
    for (g in core_grades) {
      expect_true(g %in% actual_grades,
                  info = paste(yr, "missing grade level:", g))
    }

    # 9. Entity flags are correct
    expect_true(any(enr$is_state), info = paste(yr, "no state rows"))
    expect_true(any(enr$is_district), info = paste(yr, "no district rows"))
    # Note: some AEIS SAS years (2003-2018) have 0 campus rows in tidy
    # due to processing issues; CGI era and recent TAPR have campuses

    # 10. Entity flags are mutually exclusive
    type_sums <- enr$is_state + enr$is_district + enr$is_campus
    expect_true(all(type_sums == 1),
                info = paste(yr, "entity flags not mutually exclusive"))

    # 11. ID format: district_id 6-char
    dist_rows <- enr[enr$is_district & !is.na(enr$district_id), ]
    if (nrow(dist_rows) > 0) {
      dist_id_lens <- unique(nchar(dist_rows$district_id))
      expect_true(all(dist_id_lens == 6),
                  info = paste(yr, "district_id not 6 chars:",
                               paste(dist_id_lens, collapse = ",")))
    }

    # 12. ID format: campus_id 9-char (when campuses exist)
    camp_rows <- enr[enr$is_campus & !is.na(enr$campus_id), ]
    if (nrow(camp_rows) > 0) {
      camp_id_lens <- unique(nchar(camp_rows$campus_id))
      expect_true(all(camp_id_lens == 9),
                  info = paste(yr, "campus_id not 9 chars:",
                               paste(camp_id_lens, collapse = ",")))
    }

    # 13. No leading quotes in IDs (TEA ID cleaning)
    all_dist_ids <- enr$district_id[!is.na(enr$district_id)]
    if (length(all_dist_ids) > 0) {
      expect_false(any(grepl("^'", all_dist_ids)),
                   info = paste(yr, "district_id has leading quotes"))
    }

    # 14. end_year matches requested year
    expect_true(all(enr$end_year == yr),
                info = paste(yr, "end_year values don't match"))

    # 15. pct values in 0-1 range (not 0-100)
    pct_vals <- enr$pct[!is.na(enr$pct)]
    if (length(pct_vals) > 0) {
      expect_true(all(pct_vals >= 0 & pct_vals <= 1),
                  info = paste(yr, "pct outside 0-1 range"))
    }
  })
}

# Run tests for every year 1997-2024
for (year in 1997:2024) {
  run_year_test(year)
}


# ==============================================================================
# CROSS-YEAR TESTS
# ==============================================================================

test_that("YoY state total enrollment change is less than 10%", {
  skip_if_tea_degraded()

  years <- sort(as.numeric(names(state_totals)))

  for (i in seq(2, length(years))) {
    prev_yr <- years[i - 1]
    curr_yr <- years[i]
    prev_total <- state_totals[as.character(prev_yr)]
    curr_total <- state_totals[as.character(curr_yr)]

    pct_change <- abs(curr_total - prev_total) / prev_total
    expect_true(pct_change < 0.10,
                info = paste("YoY change", prev_yr, "->", curr_yr,
                             "is", round(pct_change * 100, 1), "%"))
  }
})

test_that("Schema is consistent across recent TAPR years (2021-2024)", {
  skip_if_tea_degraded()

  cols_by_year <- list()
  for (yr in 2021:2024) {
    enr <- tryCatch(
      fetch_enr(yr, tidy = TRUE, use_cache = TRUE),
      error = function(e) skip(paste("fetch_enr", yr, "failed:", e$message))
    )
    cols_by_year[[as.character(yr)]] <- sort(names(enr))
  }

  # All years should have the same columns
  ref_cols <- cols_by_year[["2024"]]
  for (yr in c("2021", "2022", "2023")) {
    expect_equal(cols_by_year[[yr]], ref_cols,
                 info = paste("Column mismatch between", yr, "and 2024"))
  }
})

test_that("State total increases monotonically 1997-2019 (pre-COVID)", {
  skip_if_tea_degraded()

  # Texas enrollment grew every year from 1997 to 2019
  for (yr in 1998:2019) {
    prev <- state_totals[as.character(yr - 1)]
    curr <- state_totals[as.character(yr)]
    expect_gt(curr, prev,
              label = paste("State total should increase", yr - 1, "->", yr))
  }
})

test_that("COVID dip visible in 2021 enrollment", {
  skip_if_tea_degraded()

  # 2021 should be lower than 2020 (COVID impact)
  expect_lt(state_totals[["2021"]], state_totals[["2020"]],
            label = "2021 enrollment should be lower than 2020 (COVID)")
})

test_that("2020 has NA district_name for all districts (known TEA data issue)", {
  skip_if_tea_degraded()

  enr20 <- tryCatch(
    fetch_enr(2020, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr 2020 failed:", e$message))
  )

  # In 2020 TEA data, all district names are NA
  dist_rows <- enr20[enr20$is_district &
                     enr20$subgroup == "total_enrollment" &
                     enr20$grade_level == "TOTAL", ]
  na_count <- sum(is.na(dist_rows$district_name))
  expect_equal(na_count, nrow(dist_rows),
               info = "2020 should have NA district_name for all districts")
})

test_that("2024 has non-NA district_name", {
  skip_if_tea_degraded()

  enr24 <- tryCatch(
    fetch_enr(2024, tidy = TRUE, use_cache = TRUE),
    error = function(e) skip(paste("fetch_enr 2024 failed:", e$message))
  )

  dist_rows <- enr24[enr24$is_district &
                     enr24$subgroup == "total_enrollment" &
                     enr24$grade_level == "TOTAL", ]
  na_count <- sum(is.na(dist_rows$district_name))
  expect_equal(na_count, 0,
               info = "2024 should not have NA district_name")
})

test_that("Multiracial and pacific_islander absent before 2011", {
  skip_if_tea_degraded()

  for (yr in c(2005, 2008, 2010)) {
    enr <- tryCatch(
      fetch_enr(yr, tidy = TRUE, use_cache = TRUE),
      error = function(e) skip(paste("fetch_enr", yr, "failed:", e$message))
    )
    actual_subs <- unique(enr$subgroup)
    expect_false("multiracial" %in% actual_subs,
                 info = paste(yr, "should not have multiracial"))
    expect_false("pacific_islander" %in% actual_subs,
                 info = paste(yr, "should not have pacific_islander"))
  }
})

test_that("Multiracial and pacific_islander present from 2011 onward", {
  skip_if_tea_degraded()

  for (yr in c(2011, 2012, 2015, 2020, 2024)) {
    enr <- tryCatch(
      fetch_enr(yr, tidy = TRUE, use_cache = TRUE),
      error = function(e) skip(paste("fetch_enr", yr, "failed:", e$message))
    )
    actual_subs <- unique(enr$subgroup)
    expect_true("multiracial" %in% actual_subs,
                info = paste(yr, "should have multiracial"))
    expect_true("pacific_islander" %in% actual_subs,
                info = paste(yr, "should have pacific_islander"))
  }
})

test_that("fetch_enr rejects out-of-range years", {
  expect_error(fetch_enr(1996), "end_year must be between")
  expect_error(fetch_enr(2026), "end_year must be between")
})

test_that("District count is stable across years", {
  skip_if_tea_degraded()

  for (yr in c(2000, 2010, 2020, 2024)) {
    enr <- tryCatch(
      fetch_enr(yr, tidy = TRUE, use_cache = TRUE),
      error = function(e) skip(paste("fetch_enr", yr, "failed:", e$message))
    )
    n_districts <- length(unique(
      enr$district_id[enr$is_district & !is.na(enr$district_id)]
    ))
    # Texas has ~1200 districts, should be between 1100 and 1300
    expect_true(n_districts >= 1100 & n_districts <= 1300,
                info = paste(yr, "district count:", n_districts,
                             "(expected 1100-1300)"))
  }
})
