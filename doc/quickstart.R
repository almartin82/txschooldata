## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE
)

## ----install, eval=FALSE------------------------------------------------------
# # install.packages("remotes")
# remotes::install_github("almartin82/txschooldata")

## ----fetch-data---------------------------------------------------------------
library(txschooldata)
library(dplyr)

# Fetch 2024 data (2023-24 school year)
enr <- fetch_enr(2024)

## ----filter-levels------------------------------------------------------------
# State totals
enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")

# All districts
enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL")

# All campuses
enr |>
  filter(is_campus, subgroup == "total_enrollment", grade_level == "TOTAL")

## ----top-districts------------------------------------------------------------
enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  select(district_name, n_students) |>
  head(10)

## ----district-demo------------------------------------------------------------
# Austin ISD demographics
enr |>
  filter(
    district_id == "101912",
    is_district,
    grade_level == "TOTAL",
    subgroup != "total_enrollment"
  ) |>
  arrange(desc(n_students)) |>
  select(subgroup, n_students, pct)

## ----multi-year---------------------------------------------------------------
enr_multi <- fetch_enr_multi(2020:2024)

# State enrollment trend
enr_multi |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students)

## ----wide---------------------------------------------------------------------
enr_wide <- fetch_enr(2024, tidy = FALSE)

## ----viz----------------------------------------------------------------------
library(ggplot2)
library(scales)

enr_multi <- fetch_enr_multi(2020:2024)

# Demographics over time
enr_multi |>
  filter(
    is_state,
    subgroup %in% c("white", "black", "hispanic", "asian"),
    grade_level == "TOTAL"
  ) |>
  ggplot(aes(x = end_year, y = pct, color = subgroup)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Texas Public School Demographics",
    x = "School Year", y = "Percent", color = NULL
  ) +
  theme_minimal()

## ----cache--------------------------------------------------------------------
# View cached files
cache_status()

# Clear all cached data
clear_cache()

# Clear specific year
clear_cache(2024)

