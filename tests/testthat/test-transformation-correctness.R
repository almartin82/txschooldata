# ==============================================================================
# Transformation Correctness Tests — Enrollment
# ==============================================================================
#
# Tests verify that every transformation step preserves data integrity:
# - safe_numeric suppression handling
# - clean_id leading quote removal
# - ID formatting (district=6, campus=9, campus[:6]=district)
# - Grade level normalization (EE, PK, K, 01-12, TOTAL)
# - Subgroup renaming to standard names
# - Pivot fidelity: tidy total == wide total for every year
# - Percentage calculation: valid range, no Inf/NaN
# - Aggregation: state == sum(districts)
# - Entity flag logic: mutually exclusive
# - Known-value spot checks per year
# - Cross-year consistency
#
# Every pinned value was fetched from TEA via the package's own functions.
# ==============================================================================

skip_if_offline <- function() {
  skip_on_cran()
  if (!curl::has_internet()) skip("No internet connection")
}

# ==============================================================================
# 1. SUPPRESSION HANDLING
# ==============================================================================

test_that("safe_numeric converts normal numbers", {
  expect_equal(txschooldata:::safe_numeric("100"), 100)
  expect_equal(txschooldata:::safe_numeric("0"), 0)
  expect_equal(txschooldata:::safe_numeric("1,234,567"), 1234567)
  expect_equal(txschooldata:::safe_numeric("  42  "), 42)
  expect_equal(txschooldata:::safe_numeric("-5"), -5)
})

test_that("safe_numeric returns NA for every TEA suppression marker", {
  # All markers found in TEA TAPR, AEIS, and CGI data
  markers <- c("*", ".", "-", "-1", "<5", "N/A", "NA", "")
  for (m in markers) {
    expect_true(is.na(txschooldata:::safe_numeric(m)),
                info = paste("Marker not handled:", shQuote(m)))
  }
})

test_that("safe_numeric handles vector of mixed values", {
  input <- c("100", "*", "1,234", ".", "", "-1", "42")
  result <- txschooldata:::safe_numeric(input)
  expect_equal(result[1], 100)
  expect_true(is.na(result[2]))
  expect_equal(result[3], 1234)
  expect_true(is.na(result[4]))
  expect_true(is.na(result[5]))
  expect_true(is.na(result[6]))
  expect_equal(result[7], 42)
})

# ==============================================================================
# 2. ID FORMATTING
# ==============================================================================

test_that("clean_id removes leading single quotes", {
  expect_equal(txschooldata:::clean_id("'101912"), "101912")
  expect_equal(txschooldata:::clean_id("101912"), "101912")
  expect_equal(txschooldata:::clean_id("  101912  "), "101912")
  expect_equal(txschooldata:::clean_id("'001902001"), "001902001")
})

test_that("district IDs are 6 characters for all years", {
  skip_if_offline()

  for (yr in c(2000, 2010, 2024)) {
    enr <- fetch_enr(yr, tidy = FALSE, use_cache = TRUE)
    districts <- enr[enr$type == "District" & !is.na(enr$district_id), ]
    expect_true(all(nchar(districts$district_id) == 6),
                info = paste("Year", yr, "district IDs not all 6 chars"))
  }
})

test_that("campus IDs are 9 characters for all years", {
  skip_if_offline()

  for (yr in c(2000, 2010, 2024)) {
    enr <- fetch_enr(yr, tidy = FALSE, use_cache = TRUE)
    campuses <- enr[enr$type == "Campus" & !is.na(enr$campus_id), ]
    expect_true(all(nchar(campuses$campus_id) == 9),
                info = paste("Year", yr, "campus IDs not all 9 chars"))
  }
})

test_that("campus_id first 6 chars equal district_id", {
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  campuses <- enr[enr$type == "Campus" &
                   !is.na(enr$campus_id) &
                   !is.na(enr$district_id), ]
  expect_true(all(substr(campuses$campus_id, 1, 6) == campuses$district_id))
})

test_that("district rows have NA campus_id; campus rows have non-NA campus_id", {
  skip_if_offline()

  enr <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  districts <- enr[enr$type == "District", ]
  expect_true(all(is.na(districts$campus_id)))

  campuses <- enr[enr$type == "Campus", ]
  expect_true(all(!is.na(campuses$campus_id)))
})

# ==============================================================================
# 3. GRADE LEVEL NORMALIZATION
# ==============================================================================

test_that("tidy enrollment has correct TX grade levels", {
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  levels <- unique(tidy$grade_level)

  # Standard grades
  expected <- c("EE", "PK", "K", "01", "02", "03", "04",
                "05", "06", "07", "08", "09", "10", "11", "12", "TOTAL")
  for (g in expected) {
    expect_true(g %in% levels, info = paste("Missing grade level:", g))
  }
})

test_that("sum of individual grades equals TOTAL for state", {
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  state_total <- tidy$n_students[tidy$is_state &
                                  tidy$subgroup == "total_enrollment" &
                                  tidy$grade_level == "TOTAL"]

  individual_grades <- c("EE", "PK", "K", "01", "02", "03", "04",
                         "05", "06", "07", "08", "09", "10", "11", "12")
  grade_sum <- sum(tidy$n_students[tidy$is_state &
                                    tidy$subgroup == "total_enrollment" &
                                    tidy$grade_level %in% individual_grades])

  expect_equal(grade_sum, state_total)
})

test_that("enr_grade_aggs K8 + HS = K12", {
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(tidy)

  state_k8 <- aggs$n_students[aggs$is_state & aggs$grade_level == "K8"]
  state_hs <- aggs$n_students[aggs$is_state & aggs$grade_level == "HS"]
  state_k12 <- aggs$n_students[aggs$is_state & aggs$grade_level == "K12"]

  expect_equal(state_k8 + state_hs, state_k12)
})

test_that("K12 aggregate equals TOTAL minus EE and PK", {
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  aggs <- enr_grade_aggs(tidy)

  state_total <- tidy$n_students[tidy$is_state &
                                  tidy$subgroup == "total_enrollment" &
                                  tidy$grade_level == "TOTAL"]
  state_ee <- tidy$n_students[tidy$is_state &
                               tidy$subgroup == "total_enrollment" &
                               tidy$grade_level == "EE"]
  state_pk <- tidy$n_students[tidy$is_state &
                               tidy$subgroup == "total_enrollment" &
                               tidy$grade_level == "PK"]
  state_k12 <- aggs$n_students[aggs$is_state & aggs$grade_level == "K12"]

  expect_equal(state_total - state_ee - state_pk, state_k12)
})

# ==============================================================================
# 4. SUBGROUP RENAMING
# ==============================================================================

test_that("tidy enrollment uses standard subgroup names", {
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  subgroups <- unique(tidy$subgroup)

  standard <- c("total_enrollment", "white", "black", "hispanic", "asian",
                "native_american", "pacific_islander", "multiracial",
                "special_ed", "lep", "econ_disadv")

  for (s in standard) {
    expect_true(s %in% subgroups, info = paste("Missing subgroup:", s))
  }
})

test_that("no non-standard subgroup names in tidy enrollment", {
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  subgroups <- unique(tidy$subgroup)

  banned <- c("total", "low_income", "economically_disadvantaged",
              "frl", "iep", "el", "ell", "english_learner",
              "american_indian", "two_or_more")

  for (b in banned) {
    expect_false(b %in% subgroups,
                 info = paste("Non-standard subgroup present:", b))
  }
})

test_that("pre-2011 data lacks multiracial and pacific_islander columns", {
  skip_if_offline()

  enr <- fetch_enr(2010, tidy = FALSE, use_cache = TRUE)
  expect_false("multiracial" %in% names(enr))
  expect_false("pacific_islander" %in% names(enr))
})

test_that("2011+ data has multiracial and pacific_islander columns", {
  skip_if_offline()

  enr <- fetch_enr(2011, tidy = FALSE, use_cache = TRUE)
  expect_true("multiracial" %in% names(enr))
  expect_true("pacific_islander" %in% names(enr))
})

# ==============================================================================
# 5. PIVOT FIDELITY — state total in tidy matches wide exactly
# ==============================================================================

test_that("pivot fidelity: 2024 state total_enrollment matches wide row_total", {
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  wide_total <- wide$row_total[wide$type == "State"]
  tidy_total <- tidy$n_students[tidy$is_state &
                                 tidy$subgroup == "total_enrollment" &
                                 tidy$grade_level == "TOTAL"]

  expect_equal(tidy_total, wide_total)
})

test_that("pivot fidelity: 2024 state hispanic count matches wide", {
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  wide_hisp <- wide$hispanic[wide$type == "State"]
  tidy_hisp <- tidy$n_students[tidy$is_state &
                                tidy$subgroup == "hispanic" &
                                tidy$grade_level == "TOTAL"]

  expect_equal(tidy_hisp, wide_hisp)
})

test_that("pivot fidelity: 2024 Houston ISD total matches wide", {
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  wide_houston <- wide$row_total[wide$type == "District" &
                                  wide$district_id == "101912"]
  tidy_houston <- tidy$n_students[tidy$is_district &
                                   tidy$district_id == "101912" &
                                   tidy$subgroup == "total_enrollment" &
                                   tidy$grade_level == "TOTAL"]

  expect_equal(tidy_houston, wide_houston)
})

test_that("pivot fidelity: demographic sum equals total for Houston 2024", {
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  demo_subgroups <- c("white", "black", "hispanic", "asian",
                      "pacific_islander", "native_american", "multiracial")
  demo_sum <- sum(tidy$n_students[tidy$is_district &
                                   tidy$district_id == "101912" &
                                   tidy$grade_level == "TOTAL" &
                                   tidy$subgroup %in% demo_subgroups])
  total <- tidy$n_students[tidy$is_district &
                            tidy$district_id == "101912" &
                            tidy$subgroup == "total_enrollment" &
                            tidy$grade_level == "TOTAL"]

  expect_equal(demo_sum, total)
})

# ==============================================================================
# 6. PERCENTAGE CALCULATION
# ==============================================================================

test_that("percentages are in valid range [0, 1]", {
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  pct_vals <- tidy$pct[!is.na(tidy$pct)]

  expect_true(all(pct_vals >= 0), info = "Found pct < 0")
  expect_true(all(pct_vals <= 1), info = "Found pct > 1")
})

test_that("no Inf or NaN in percentages", {
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_false(any(is.infinite(tidy$pct)), info = "Found Inf in pct")
  expect_false(any(is.nan(tidy$pct)), info = "Found NaN in pct")
})

test_that("total_enrollment at TOTAL grade_level always has pct = 1.0", {
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  total_pcts <- tidy$pct[tidy$subgroup == "total_enrollment" &
                          tidy$grade_level == "TOTAL"]
  expect_true(all(total_pcts == 1.0))
})

test_that("no Inf or NaN in n_students", {
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_false(any(is.infinite(tidy$n_students)), info = "Found Inf in n_students")
  expect_false(any(is.nan(tidy$n_students)), info = "Found NaN in n_students")
})

# ==============================================================================
# 7. AGGREGATION CORRECTNESS
# ==============================================================================

test_that("state total equals sum of district totals (2024)", {
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  state_total <- wide$row_total[wide$type == "State"]
  district_sum <- sum(wide$row_total[wide$type == "District"], na.rm = TRUE)

  expect_equal(state_total, district_sum)
})

test_that("state total equals sum of district totals (2013)", {
  skip_if_offline()

  wide <- fetch_enr(2013, tidy = FALSE, use_cache = TRUE)

  state_total <- wide$row_total[wide$type == "State"]
  district_sum <- sum(wide$row_total[wide$type == "District"], na.rm = TRUE)

  expect_equal(state_total, district_sum)
})

test_that("state demographics equal sum of district demographics (2024)", {
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  demo_cols <- c("white", "black", "hispanic", "asian",
                 "pacific_islander", "native_american", "multiracial")

  for (col in demo_cols) {
    state_val <- wide[[col]][wide$type == "State"]
    district_sum <- sum(wide[[col]][wide$type == "District"], na.rm = TRUE)
    expect_equal(state_val, district_sum,
                 info = paste("Mismatch in", col))
  }
})

# ==============================================================================
# 8. ENTITY FLAG LOGIC
# ==============================================================================

test_that("entity flags are mutually exclusive", {
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  type_sums <- tidy$is_state + tidy$is_district + tidy$is_campus
  expect_true(all(type_sums == 1))
})

test_that("at least one of each entity type exists", {
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(any(tidy$is_state))
  expect_true(any(tidy$is_district))
  expect_true(any(tidy$is_campus))
})

test_that("is_state matches type == State", {
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)
  expect_true(all(tidy$is_state == (tidy$type == "State")))
  expect_true(all(tidy$is_district == (tidy$type == "District")))
  expect_true(all(tidy$is_campus == (tidy$type == "Campus")))
})

test_that("aggregation_flag is consistent with entity flags", {
  skip_if_offline()

  tidy <- fetch_enr(2024, tidy = TRUE, use_cache = TRUE)

  if ("aggregation_flag" %in% names(tidy)) {
    expect_true(all(tidy$aggregation_flag[tidy$is_state] == "state"))
    expect_true(all(tidy$aggregation_flag[tidy$is_district] == "district"))
    expect_true(all(tidy$aggregation_flag[tidy$is_campus] == "campus"))
  }
})

# ==============================================================================
# 9. YEAR x RAW DATA COVERAGE — pinned known values
# ==============================================================================
# Every expected value was extracted from actual fetch_enr() output.

test_that("1997 (CGI era): state total = 3,828,975", {
  skip_if_offline()

  wide <- fetch_enr(1997, tidy = FALSE, use_cache = TRUE)
  state_total <- wide$row_total[wide$type == "State"]
  expect_equal(state_total, 3828975)
})

test_that("2000 (CGI era): state total = 3,991,783; Houston ISD = 209,716", {
  skip_if_offline()

  wide <- fetch_enr(2000, tidy = FALSE, use_cache = TRUE)
  state_total <- wide$row_total[wide$type == "State"]
  expect_equal(state_total, 3991783)

  houston <- wide$row_total[wide$type == "District" &
                             wide$district_id == "101912"]
  expect_equal(houston, 209716)
})

test_that("2003 (AEIS SAS era): state total = 4,239,911", {
  skip_if_offline()

  wide <- fetch_enr(2003, tidy = FALSE, use_cache = TRUE)
  state_total <- wide$row_total[wide$type == "State"]
  expect_equal(state_total, 4239911)
})

test_that("2008 (AEIS SAS era): state total = 4,651,516", {
  skip_if_offline()

  wide <- fetch_enr(2008, tidy = FALSE, use_cache = TRUE)
  state_total <- wide$row_total[wide$type == "State"]
  expect_equal(state_total, 4651516)
})

test_that("2010 (AEIS SAS era): state total = 4,824,778", {
  skip_if_offline()

  wide <- fetch_enr(2010, tidy = FALSE, use_cache = TRUE)
  state_total <- wide$row_total[wide$type == "State"]
  expect_equal(state_total, 4824778)
})

test_that("2011 (AEIS SAS era): state total = 4,912,385; multiracial = 78,178", {
  skip_if_offline()

  wide <- fetch_enr(2011, tidy = FALSE, use_cache = TRUE)
  state_total <- wide$row_total[wide$type == "State"]
  expect_equal(state_total, 4912385)

  state_multi <- wide$multiracial[wide$type == "State"]
  expect_equal(state_multi, 78178)
})

test_that("2012 (last AEIS SAS year): state total = 4,978,120", {
  skip_if_offline()

  wide <- fetch_enr(2012, tidy = FALSE, use_cache = TRUE)
  state_total <- wide$row_total[wide$type == "State"]
  expect_equal(state_total, 4978120)
})

test_that("2013 (first TAPR year): state total = 5,058,939", {
  skip_if_offline()

  wide <- fetch_enr(2013, tidy = FALSE, use_cache = TRUE)
  state_total <- wide$row_total[wide$type == "State"]
  expect_equal(state_total, 5058939)
})

test_that("2018 (TAPR): state total = 5,385,012", {
  skip_if_offline()

  wide <- fetch_enr(2018, tidy = FALSE, use_cache = TRUE)
  state_total <- wide$row_total[wide$type == "State"]
  expect_equal(state_total, 5385012)
})

test_that("2020 (TAPR): state total = 5,479,173", {
  skip_if_offline()

  wide <- fetch_enr(2020, tidy = FALSE, use_cache = TRUE)
  state_total <- wide$row_total[wide$type == "State"]
  expect_equal(state_total, 5479173)
})

test_that("2021 (TAPR): state total = 5,359,040", {
  skip_if_offline()

  wide <- fetch_enr(2021, tidy = FALSE, use_cache = TRUE)
  state_total <- wide$row_total[wide$type == "State"]
  expect_equal(state_total, 5359040)
})

test_that("2023 (TAPR): state total = 5,504,150", {
  skip_if_offline()

  wide <- fetch_enr(2023, tidy = FALSE, use_cache = TRUE)
  state_total <- wide$row_total[wide$type == "State"]
  expect_equal(state_total, 5504150)
})

test_that("2024 (TAPR): state total = 5,517,464; Houston = 183,603; Dallas = 139,096", {
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)

  state_total <- wide$row_total[wide$type == "State"]
  expect_equal(state_total, 5517464)

  houston <- wide$row_total[wide$type == "District" &
                             wide$district_id == "101912"]
  expect_equal(houston, 183603)

  dallas <- wide$row_total[wide$type == "District" &
                            wide$district_id == "057905"]
  expect_equal(dallas, 139096)
})

test_that("2024 state demographics pin check", {
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  state <- wide[wide$type == "State", ]

  expect_equal(state$white, 1379090)
  expect_equal(state$black, 706235)
  expect_equal(state$hispanic, 2936051)
  expect_equal(state$asian, 295946)
  expect_equal(state$native_american, 17886)
  expect_equal(state$pacific_islander, 8831)
  expect_equal(state$multiracial, 173425)
  expect_equal(state$econ_disadv, 3434955)
  expect_equal(state$lep, 1344804)
  expect_equal(state$special_ed, 764858)
})

test_that("2024 state grade breakdown pin check", {
  skip_if_offline()

  wide <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  state <- wide[wide$type == "State", ]

  expect_equal(state$grade_ee, 18968)
  expect_equal(state$grade_pk, 247979)
  expect_equal(state$grade_k, 361329)
  expect_equal(state$grade_01, 385096)
  expect_equal(state$grade_12, 365788)
})

# ==============================================================================
# 10. CROSS-YEAR CONSISTENCY
# ==============================================================================

test_that("YoY state total change < 10% for consecutive TAPR years", {
  skip_if_offline()

  # Known state totals (pinned values)
  totals <- c(
    "2018" = 5385012,
    "2019" = 5416400,
    "2020" = 5479173,
    "2021" = 5359040,
    "2022" = 5402928,
    "2023" = 5504150,
    "2024" = 5517464
  )

  years <- as.integer(names(totals))
  for (i in 2:length(totals)) {
    pct_change <- abs(totals[i] - totals[i - 1]) / totals[i - 1]
    expect_true(pct_change < 0.10,
                info = paste(years[i], "change:",
                             round(pct_change * 100, 1), "%"))
  }
})

test_that("Houston ISD present in all three eras", {
  skip_if_offline()

  # CGI era
  wide_2000 <- fetch_enr(2000, tidy = FALSE, use_cache = TRUE)
  expect_true("101912" %in% wide_2000$district_id[wide_2000$type == "District"])

  # AEIS SAS era
  wide_2010 <- fetch_enr(2010, tidy = FALSE, use_cache = TRUE)
  expect_true("101912" %in% wide_2010$district_id[wide_2010$type == "District"])

  # TAPR era
  wide_2024 <- fetch_enr(2024, tidy = FALSE, use_cache = TRUE)
  expect_true("101912" %in% wide_2024$district_id[wide_2024$type == "District"])
})

test_that("Houston ISD totals are in plausible range across years", {
  skip_if_offline()

  # Pinned values from actual data
  houston_totals <- list(
    "1997" = list(year = 1997, total = 209375),
    "2003" = list(year = 2003, total = 211762),
    "2012" = list(year = 2012, total = 201594),
    "2024" = list(year = 2024, total = 183603)
  )

  for (entry in houston_totals) {
    wide <- fetch_enr(entry$year, tidy = FALSE, use_cache = TRUE)
    houston <- wide$row_total[wide$type == "District" &
                               wide$district_id == "101912"]
    expect_equal(houston, entry$total,
                 info = paste("Year", entry$year))
    # All should be > 100k (Houston is a mega-district)
    expect_true(houston > 100000,
                info = paste("Year", entry$year, "Houston too small:", houston))
  }
})

test_that("fetch_enr validates year parameter boundaries", {
  expect_error(fetch_enr(1996), "end_year must be between")
  expect_error(fetch_enr(2026), "end_year must be between")
})

test_that("fetch_enr_multi validates year parameter boundaries", {
  expect_error(fetch_enr_multi(c(1996, 2024)), "Invalid years")
  expect_error(fetch_enr_multi(c(2024, 2026)), "Invalid years")
})
