# Getting Started with txschooldata

## Installation

``` r
# install.packages("remotes")
remotes::install_github("almartin82/txschooldata")
```

## Basic Usage

``` r
library(txschooldata)
library(dplyr)

# Fetch 2024 data (2023-24 school year)
enr <- fetch_enr(2024)
```

Data is returned in **tidy (long) format** by default. Each row is one
subgroup for one entity (state/district/campus).

## Key Columns

| Column                                 | Description                      |
|----------------------------------------|----------------------------------|
| `end_year`                             | School year end (2024 = 2023-24) |
| `district_id`                          | 6-digit district ID              |
| `district_name`                        | District name                    |
| `subgroup`                             | Demographic group                |
| `grade_level`                          | Grade (“TOTAL”, “K”, “01”-“12”)  |
| `n_students`                           | Student count                    |
| `pct`                                  | Percent of total                 |
| `is_state`, `is_district`, `is_campus` | Level flags                      |

## Filtering by Level

``` r
# State totals
enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")

# All districts
enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL")

# All campuses
enr |>
  filter(is_campus, subgroup == "total_enrollment", grade_level == "TOTAL")
```

## Top Districts

``` r
enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  select(district_name, n_students) |>
  head(10)
```

## Demographics for One District

``` r
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
```

## Multi-Year Trends

``` r
enr_multi <- fetch_enr_multi(2020:2024)

# State enrollment trend
enr_multi |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students)
```

## Wide Format

For one column per demographic:

``` r
enr_wide <- fetch_enr(2024, tidy = FALSE)
```

## Visualization

``` r
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
```

## Available Subgroups

**Demographics:** white, black, hispanic, asian, native_american,
pacific_islander, multiracial

**Special Populations:** special_ed, lep (English learners), econ_disadv

**Totals:** total_enrollment

## Cache Management

``` r
# View cached files
cache_status()

# Clear all cached data
clear_cache()

# Clear specific year
clear_cache(2024)
```

## Next Steps

- See [10 Things You Didn’t Know About Texas
  Schools](https://almartin82.github.io/txschooldata/articles/district-hooks.md)
  for analysis examples
- Use
  [`?fetch_enr`](https://almartin82.github.io/txschooldata/reference/fetch_enr.md)
  for complete function documentation
