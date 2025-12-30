# Getting Started with txschooldata

## Installation

Install from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("almartin82/txschooldata")
```

## Quick Example

Fetch the most recent year of Texas enrollment data:

``` r
library(txschooldata)
library(dplyr)

# Fetch 2024 enrollment data (2023-24 school year)
enr <- fetch_enr(2024)

head(enr)
```

## Understanding the Data

The data is returned in **tidy (long) format** by default:

- Each row is one subgroup for one district/campus/state
- `subgroup` identifies the demographic group (e.g., “total_enrollment”,
  “white”, “hispanic”, “econ_disadv”)
- `grade_level` shows the grade (“TOTAL”, “K”, “01”, “02”, etc.)
- `n_students` is the enrollment count
- `pct` is the percentage of total enrollment

### Data Schema

| Column          | Description          | Example                        |
|-----------------|----------------------|--------------------------------|
| `end_year`      | School year end      | 2024 (for 2023-24)             |
| `type`          | Aggregation type     | “State”, “District”, “Campus”  |
| `district_id`   | 6-digit district ID  | “101912”                       |
| `campus_id`     | 9-digit campus ID    | “101912001”                    |
| `district_name` | District name        | “AUSTIN ISD”                   |
| `campus_name`   | Campus/school name   | “AUSTIN HIGH SCHOOL”           |
| `county`        | County name          | “TRAVIS”                       |
| `region`        | ESC region number    | 13                             |
| `charter_flag`  | Charter indicator    | “Y” or “N”                     |
| `grade_level`   | Grade level          | “TOTAL”, “K”, “01”-“12”        |
| `subgroup`      | Demographic subgroup | “total_enrollment”, “hispanic” |
| `n_students`    | Student count        | 5432                           |
| `pct`           | Percent of total     | 0.45                           |
| `is_state`      | State-level flag     | TRUE/FALSE                     |
| `is_district`   | District-level flag  | TRUE/FALSE                     |
| `is_campus`     | Campus-level flag    | TRUE/FALSE                     |
| `is_charter`    | Charter school flag  | TRUE/FALSE                     |

### Available Subgroups

**Demographics:** - `white`, `black`, `hispanic`, `asian` -
`native_american`, `pacific_islander`, `multiracial`

**Special Populations:** - `special_ed` - Students receiving special
education services - `lep` - Limited English Proficiency students -
`econ_disadv` - Economically disadvantaged students

**Totals:** - `total_enrollment` - All students

``` r
enr |>
  filter(is_state) |>
  select(end_year, type, subgroup, grade_level, n_students) |>
  head(10)
```

## Texas Campus and District ID System

Texas uses a hierarchical ID system for districts and campuses:

- **District IDs**: 6 digits (e.g., `101912` for Austin ISD)
- **Campus IDs**: 9 digits (district ID + 3-digit campus number)

The first 6 digits of a campus ID identify its district:

``` r
# Campus ID breakdown
campus_id <- "101912001"
district_id <- substr(campus_id, 1, 6)   # "101912" (Austin ISD)
campus_num <- substr(campus_id, 7, 9)     # "001" (first campus)

# View district data
enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(district_id, district_name, n_students) |>
  head(5)
```

## Filtering by Aggregation Level

Use the boolean flags to filter data to specific levels:

``` r
# State totals
state <- enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL")
state |> select(end_year, n_students)

# All districts
districts <- enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL")
nrow(districts)  # ~1,200 districts

# All campuses
campuses <- enr |>
  filter(is_campus, subgroup == "total_enrollment", grade_level == "TOTAL")
nrow(campuses)  # ~9,000+ schools
```

## Common Analysis Examples

### Top 10 Districts by Enrollment

``` r
enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  select(district_name, n_students) |>
  head(10)
```

### Demographic Breakdown for a District

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

### Compare Charter vs Traditional Districts

``` r
# Total enrollment by charter status
enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  group_by(is_charter) |>
  summarize(
    n_districts = n(),
    total_students = sum(n_students, na.rm = TRUE),
    avg_enrollment = mean(n_students, na.rm = TRUE)
  )
```

## Wide Format

If you prefer wide format (one column per demographic), set
`tidy = FALSE`:

``` r
enr_wide <- fetch_enr(2024, tidy = FALSE)

enr_wide |>
  filter(type == "State") |>
  select(end_year, row_total, white, black, hispanic, asian, econ_disadv)
```

Wide format columns include: - `row_total` - Total enrollment - `white`,
`black`, `hispanic`, `asian`, etc. - Demographic counts - `grade_ee`,
`grade_pk`, `grade_k`, `grade_01` through `grade_12` - Grade-level
counts

## Historical Data

Fetch multiple years to analyze trends using
[`fetch_enr_multi()`](https://almartin82.github.io/txschooldata/reference/fetch_enr_multi.md):

``` r
# Fetch 3 years of data
enr_multi <- fetch_enr_multi(2022:2024)

# State enrollment trend
enr_multi |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students)
```

## Visualizing Enrollment Trends

``` r
library(ggplot2)
library(scales)

# Fetch multiple years
enr_multi <- fetch_enr_multi(2020:2024)

# Plot state enrollment over time
enr_multi |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  ggplot(aes(x = end_year, y = n_students)) +
  geom_line(linewidth = 1, color = "#002868") +
  geom_point(size = 3, color = "#002868") +
  scale_y_continuous(labels = comma, limits = c(0, NA)) +
  labs(
    title = "Texas Public School Enrollment",
    subtitle = "Total K-12 enrollment by school year",
    x = "School Year (End)",
    y = "Total Students"
  ) +
  theme_minimal()
```

### Demographic Trends

``` r
# Plot demographic composition over time
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
    x = "School Year (End)",
    y = "Percent of Total Enrollment",
    color = "Subgroup"
  ) +
  theme_minimal()
```

## Charter Schools

Filter to charter schools using the `is_charter` flag:

``` r
# Top charter districts
enr |>
  filter(is_district, is_charter, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  select(district_name, n_students) |>
  head(10)

# Charter school growth
enr_multi |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  group_by(end_year, is_charter) |>
  summarize(
    n_districts = n(),
    total_students = sum(n_students, na.rm = TRUE),
    .groups = "drop"
  )
```

## Grade-Level Aggregations

Use
[`enr_grade_aggs()`](https://almartin82.github.io/txschooldata/reference/enr_grade_aggs.md)
to create common grade groupings (K-8, High School, K-12):

``` r
# Create K-8, HS, and K-12 aggregates
grade_aggs <- enr_grade_aggs(enr)

# View K-12 enrollment by district
grade_aggs |>
  filter(is_district, grade_level == "K12") |>
  arrange(desc(n_students)) |>
  select(district_name, grade_level, n_students) |>
  head(10)

# Compare K-8 vs HS enrollment at state level
grade_aggs |>
  filter(is_state) |>
  select(grade_level, n_students)
```

## Cache Management

Downloaded data is cached locally to speed up subsequent requests:

``` r
# View cached files
cache_status()

# Clear all cached data
clear_cache()

# Clear specific year
clear_cache(2024)
```

The cache is stored in your user data directory (via `rappdirs`). Cached
data persists between R sessions.

## Data Quality Notes

- **Missing data**: Some campuses may have missing values for certain
  demographics if counts are suppressed for privacy
- **Small counts**: TEA may suppress counts below certain thresholds
- **Year coverage**: Currently supports 2020-2025. Earlier years may use
  different data formats

## Next Steps

- Use
  [`?fetch_enr`](https://almartin82.github.io/txschooldata/reference/fetch_enr.md)
  for full function documentation
- Use
  [`?tidy_enr`](https://almartin82.github.io/txschooldata/reference/tidy_enr.md)
  for data transformation details
- See
  [`?id_enr_aggs`](https://almartin82.github.io/txschooldata/reference/id_enr_aggs.md)
  for understanding aggregation flags
- See
  [`?enr_grade_aggs`](https://almartin82.github.io/txschooldata/reference/enr_grade_aggs.md)
  for grade-level aggregations
- Visit the [pkgdown site](https://almartin82.github.io/txschooldata/)
  for complete API reference
