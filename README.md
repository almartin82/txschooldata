# txschooldata

<!-- badges: start -->
[![R-CMD-check](https://github.com/almartin82/txschooldata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/almartin82/txschooldata/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/almartin82/txschooldata/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/almartin82/txschooldata/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

An R package for fetching and processing Texas school enrollment data from the Texas Education Agency (TEA) TAPR (Texas Academic Performance Reports) system.

## Overview

`txschooldata` provides a simple interface to download, cache, and analyze Texas public school enrollment data. The package handles:
- Downloading data from TEA's TAPR system
- Processing raw data into a standardized schema
- Transforming data into tidy (long) format for analysis
- Local caching for faster repeated access

## Installation

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("almartin82/txschooldata")
```

## Quick Start

```r
library(txschooldata)
library(dplyr)

# Fetch 2024 enrollment data (2023-24 school year)
enr <- fetch_enr(2024)

# View state-level enrollment
enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students)

# Top 10 districts by enrollment
enr |>
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  arrange(desc(n_students)) |>
  select(district_name, n_students) |>
  head(10)
```

### Wide Format

If you prefer one column per demographic group:

```r
enr_wide <- fetch_enr(2024, tidy = FALSE)
```

### Multi-Year Data

Fetch multiple years for trend analysis:

```r
enr_multi <- fetch_enr_multi(2020:2024)

# State enrollment trend
enr_multi |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(end_year, n_students)
```

## Documentation

Full documentation is available at the [pkgdown site](https://almartin82.github.io/txschooldata/).

## Available Data

**Years:** 2020-2025 (school years 2019-20 through 2024-25)

**Subgroups:**
- Demographics: white, black, hispanic, asian, native_american, pacific_islander, multiracial
- Special populations: special_ed, lep (limited English proficiency), econ_disadv (economically disadvantaged)
- Grade levels: EE, PK, K, 01-12

**Aggregation Levels:**
- State totals
- District totals
- Campus (school) level

## Data Source

Data is sourced from the **Texas Education Agency (TEA)** via the [TAPR (Texas Academic Performance Reports)](https://tea.texas.gov/reports-and-data) system.

PEIMS (Public Education Information Management System) is the state's comprehensive data collection system for public education.

## Campus ID System

Texas uses a hierarchical identifier system:

- **District IDs:** 6 digits (e.g., `101912` for Austin ISD)
- **Campus IDs:** 9 digits (district ID + 3-digit campus number, e.g., `101912001`)

```r
# Extract district from campus ID
campus_id <- "101912001"
district_id <- substr(campus_id, 1, 6)  # "101912"
```

## Related Packages

- [njschooldata](https://github.com/almartin82/njschooldata) - Similar package for New Jersey school data
- [ilschooldata](https://github.com/almartin82/ilschooldata) - Similar package for Illinois school data

## License
MIT
