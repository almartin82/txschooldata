## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.width = 8,
  fig.height = 5
)

## ----load-packages------------------------------------------------------------
library(txschooldata)
library(dplyr)
library(ggplot2)
library(scales)

## ----state-trend-plot---------------------------------------------------------
state_totals %>%
  ggplot(aes(x = end_year, y = n_students)) +
  geom_line(linewidth = 1.2, color = "#003366") +
  geom_point(size = 3, color = "#003366") +
  scale_y_continuous(labels = comma, limits = c(0, NA)) +
  labs(
    title = "Texas Public School Enrollment",
    subtitle = "Total K-12 enrollment, 2020-2024",
    x = "School Year (End)",
    y = "Total Students",
    caption = "Source: Texas Education Agency TAPR"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

## ----yoy-changes--------------------------------------------------------------
# Calculate year-over-year changes
state_changes <- state_totals %>%
  mutate(
    prev_year = lag(n_students),
    change = n_students - prev_year,
    pct_change = (change / prev_year) * 100
  ) %>%
  filter(!is.na(prev_year))

print(state_changes)

# Flag any jumps greater than 5%
large_changes <- state_changes %>%
  filter(abs(pct_change) > 5)

if (nrow(large_changes) > 0) {
  cat("\n** DATA QUALITY ALERT: Year-over-year changes exceeding 5%:\n")
  print(large_changes)
} else {
  cat("\n** All year-over-year changes are within +/- 5% threshold.\n")
}

## ----major-districts----------------------------------------------------------
# Define major districts to analyze
major_districts <- tibble(
  district_id = c("101912", "057905", "015907", "220905", "227901"),
  district_label = c("Houston ISD", "Dallas ISD", "San Antonio ISD",
                     "Fort Worth ISD", "Austin ISD")
)

# Extract enrollment for major districts
district_trends <- enr_multi %>%
  filter(
    is_district,
    subgroup == "total_enrollment",
    grade_level == "TOTAL",
    district_id %in% major_districts$district_id
  ) %>%
  left_join(major_districts, by = "district_id") %>%
  select(end_year, district_id, district_label, n_students) %>%
  arrange(district_label, end_year)

# Display current enrollment
current_enrollment <- district_trends %>%
  filter(end_year == max(end_year)) %>%
  arrange(desc(n_students))

print(current_enrollment)

## ----district-trend-plot------------------------------------------------------
district_trends %>%
  ggplot(aes(x = end_year, y = n_students, color = district_label)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = comma) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Major Texas District Enrollment Trends",
    subtitle = "2020-2024 school years",
    x = "School Year (End)",
    y = "Total Students",
    color = "District",
    caption = "Source: Texas Education Agency TAPR"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

## ----district-yoy-------------------------------------------------------------
district_changes <- district_trends %>%
  group_by(district_label) %>%
  mutate(
    prev_year = lag(n_students),
    change = n_students - prev_year,
    pct_change = round((change / prev_year) * 100, 2)
  ) %>%
  filter(!is.na(prev_year)) %>%
  ungroup()

# Pivot to show changes by year
district_change_summary <- district_changes %>%
  select(district_label, end_year, pct_change) %>%
  tidyr::pivot_wider(names_from = end_year, values_from = pct_change)

print(district_change_summary)

# Flag large changes
large_district_changes <- district_changes %>%
  filter(abs(pct_change) > 5)

if (nrow(large_district_changes) > 0) {
  cat("\n** DATA QUALITY ALERT: District changes exceeding 5%:\n")
  print(large_district_changes %>%
        select(district_label, end_year, n_students, change, pct_change))
} else {
  cat("\n** All major district changes are within +/- 5% threshold.\n")
}

## ----demographic-check--------------------------------------------------------
# Get 2024 state demographic data
state_2024 <- enr_multi %>%
  filter(is_state, end_year == 2024, grade_level == "TOTAL") %>%
  select(subgroup, n_students)

# Total enrollment
total <- state_2024 %>%
  filter(subgroup == "total_enrollment") %>%
  pull(n_students)

# Race/ethnicity subgroups
race_groups <- c("white", "black", "hispanic", "asian",
                 "native_american", "pacific_islander", "multiracial")
race_sum <- state_2024 %>%
  filter(subgroup %in% race_groups) %>%
  summarize(sum = sum(n_students, na.rm = TRUE)) %>%
  pull(sum)

cat("Total Enrollment:", comma(total), "\n")
cat("Sum of Race/Ethnicity Groups:", comma(race_sum), "\n")
cat("Difference:", comma(total - race_sum),
    "(", round((total - race_sum) / total * 100, 2), "%)\n")

# This difference may be due to students with unknown race/ethnicity
if (abs(total - race_sum) / total > 0.05) {
  cat("\n** DATA QUALITY NOTE: Race/ethnicity categories don't sum to total.\n")
  cat("   This may indicate missing demographic data or students with unknown race.\n")
}

## ----grade-check--------------------------------------------------------------
# Get grade-level enrollment for 2024 state
grades <- c("EE", "PK", "K", "01", "02", "03", "04", "05",
            "06", "07", "08", "09", "10", "11", "12")

grade_sum <- enr_multi %>%
  filter(is_state, end_year == 2024,
         subgroup == "total_enrollment",
         grade_level %in% grades) %>%
  summarize(sum = sum(n_students, na.rm = TRUE)) %>%
  pull(sum)

cat("Total Enrollment:", comma(total), "\n")
cat("Sum of Grade Levels:", comma(grade_sum), "\n")
cat("Difference:", comma(total - grade_sum),
    "(", round((total - grade_sum) / total * 100, 2), "%)\n")

if (abs(total - grade_sum) / total > 0.05) {
  cat("\n** DATA QUALITY NOTE: Grade levels don't sum to total.\n")
  cat("   This may indicate ungraded students or data reporting differences.\n")
}

## ----summary------------------------------------------------------------------
issues <- character()

# Check for large state-level changes
if (nrow(large_changes) > 0) {
  issues <- c(issues, paste(
    "Statewide enrollment changes >5% in:",
    paste(large_changes$end_year, collapse = ", ")
  ))
}

# Check for large district changes
if (nrow(large_district_changes) > 0) {
  issues <- c(issues, paste(
    "Major district changes >5%:",
    nrow(large_district_changes), "instances"
  ))
}

# Print summary
cat("=== Data Quality Summary ===\n\n")
cat("Years Analyzed:", min(years), "-", max(years), "\n")
cat("Total State Records:", comma(nrow(enr_multi)), "\n")
cat("Districts in Dataset:",
    enr_multi %>% filter(is_district) %>% distinct(district_id) %>% nrow(), "\n")
cat("Campuses in Dataset:",
    enr_multi %>% filter(is_campus) %>% distinct(campus_id) %>% nrow(), "\n\n")

if (length(issues) > 0) {
  cat("Issues Found:\n")
  for (issue in issues) {
    cat("  -", issue, "\n")
  }
} else {
  cat("No major data quality issues identified.\n")
}

