# txschooldata Product Requirements Document

## Overview

txschooldata is an R package for fetching, processing, and analyzing
school data from the Texas Education Agency (TEA). It provides a
programmatic interface to PEIMS (Public Education Information Management
System) data, enabling researchers, analysts, and education policy
professionals to easily access Texas public school data.

## Background

### The Problem

Texas public school data is scattered across multiple TEA reports and
data files, with inconsistent formats across years. Researchers and
analysts spend significant time: - Finding the correct data files on the
TEA website - Understanding changing file formats and column names -
Writing custom parsing code for each data file - Dealing with suppressed
values and data quality issues

### The Opportunity

A well-designed R package can: - Provide a single, consistent API for
accessing Texas school data - Handle format changes transparently across
years - Cache data locally to reduce repeated downloads - Transform data
into analysis-ready tidy format

## Regulatory Context

### TAC 61.1040 - State Data Reporting Requirements

Texas Administrative Code (TAC) 61.1040 establishes requirements for
educational data reporting. Districts must submit data through PEIMS,
creating a standardized data collection that this package can leverage.

### September 2025 Bid Threshold Change

The Texas procurement bid threshold is changing to \$100,000 in
September 2025 (from \$50,000). This affects: - District purchasing
decisions for data and analytics tools - Potential market opportunity
for self-service data tools - Timing considerations for package
development and marketing

## Data Sources

### Primary Source: Texas Education Agency (TEA)

**Website**: <https://tea.texas.gov/reports-and-data>

**Key Data Collections**:

1.  **PEIMS Data**
    - Comprehensive student, staff, and financial data
    - Submitted by districts fall, mid-year, summer, and extended-year
    - Available at:
      <https://tea.texas.gov/reports-and-data/school-data/peims-data>
2.  **Texas Academic Performance Reports (TAPR)**
    - School and district performance data
    - Demographic breakdowns
    - Available at:
      <https://tea.texas.gov/texas-schools/accountability/academic-accountability/performance-reporting>
3.  **Enrollment Data**
    - Student counts by grade, demographic, program
    - Historical data available

### ID System

Texas uses a hierarchical ID system:

- **District IDs**: 6 digits
  - First 3 digits: County-district number
  - Last 3 digits: District within county
  - Example: 101912 = Houston ISD (Harris County)
- **Campus IDs**: 9 digits
  - First 6 digits: District ID
  - Last 3 digits: Campus number within district
  - Example: 101912001 = First campus in Houston ISD

### Data Scope

- **Number of Districts**: ~1,209 (2023-24)
- **Number of Campuses**: ~9,000+
- **Student Population**: ~5.5 million students
- **Historical Data**: Generally available back to 2005 or earlier

## Functional Requirements

### Core Functions

#### 1. Enrollment Data

``` r
# Fetch enrollment data for a given school year
fetch_enr(end_year, tidy = TRUE, use_cache = TRUE)

# Get raw enrollment data (internal)
get_raw_enr(end_year)

# Process raw data to standard schema (internal)
process_enr(df, end_year)

# Transform to tidy format
tidy_enr(df)
```

#### 2. Caching

``` r
# Check if cached data exists
cache_exists(end_year, type)

# Read from cache
read_cache(end_year, type)

# Write to cache
write_cache(df, end_year, type)

# Clear cache
clear_cache(end_year = NULL, type = NULL)
```

### Data Schema

#### Standard Enrollment Schema (Wide Format)

| Column           | Type      | Description                                     |
|------------------|-----------|-------------------------------------------------|
| end_year         | integer   | School year end (2024 = 2023-24)                |
| district_id      | character | 6-digit TEA district ID                         |
| campus_id        | character | 9-digit TEA campus ID (or NA for district rows) |
| district_name    | character | District name                                   |
| campus_name      | character | Campus name (or NA for district rows)           |
| type             | character | “State”, “District”, “Campus”                   |
| row_total        | integer   | Total enrollment                                |
| white            | integer   | White student count                             |
| black            | integer   | Black/African American student count            |
| hispanic         | integer   | Hispanic/Latino student count                   |
| asian            | integer   | Asian student count                             |
| native_american  | integer   | American Indian/Alaska Native count             |
| pacific_islander | integer   | Native Hawaiian/Pacific Islander count          |
| multiracial      | integer   | Two or more races count                         |
| special_ed       | integer   | Special education student count                 |
| lep              | integer   | Limited English Proficient count                |
| econ_disadv      | integer   | Economically disadvantaged count                |
| grade_pk         | integer   | Pre-K enrollment                                |
| grade_k          | integer   | Kindergarten enrollment                         |
| grade_01-12      | integer   | Grade 1-12 enrollment                           |

#### Tidy Format Schema

| Column        | Type      | Description                                |
|---------------|-----------|--------------------------------------------|
| end_year      | integer   | School year end                            |
| district_id   | character | District ID                                |
| campus_id     | character | Campus ID                                  |
| district_name | character | District name                              |
| campus_name   | character | Campus name                                |
| type          | character | Aggregation level                          |
| grade_level   | character | “TOTAL”, “PK”, “K”, “01”-“12”              |
| subgroup      | character | “total_enrollment”, “white”, “black”, etc. |
| n_students    | integer   | Student count                              |
| pct           | numeric   | Percentage of total (0-1)                  |

## Technical Requirements

### Dependencies

- R \>= 4.0
- dplyr, tidyr, purrr (tidyverse data manipulation)
- readxl, readr (file reading)
- downloader (robust file downloads)
- rappdirs (cross-platform cache directory)
- stringr (string manipulation)
- rlang (programming utilities)

### Caching

- Use `rappdirs::user_cache_dir("txschooldata")` for cache location
- Store as .rds files for fast R-native read/write
- Cache key: `{type}_{end_year}.rds`
- Provide cache management functions

### Error Handling

- Graceful degradation when TEA website is unavailable
- Clear error messages for invalid year parameters
- Warnings for data quality issues (suppression, etc.)

## Non-Functional Requirements

### Performance

- First fetch: \< 30 seconds per year
- Cached fetch: \< 1 second
- Memory: Handle full state dataset (~100K rows)

### Compatibility

- R \>= 4.0
- Cross-platform (Windows, macOS, Linux)
- Work with tidyverse workflows

### Documentation

- Roxygen2 documentation for all exported functions
- Package vignettes for common use cases
- Examples in function documentation

## Implementation Phases

### Phase 1: Core Enrollment Data (MVP)

1.  Basic fetch_enr() for recent years (2020+)
2.  Local caching
3.  Tidy format transformation
4.  Basic documentation

### Phase 2: Historical Data

1.  Support for legacy file formats (pre-2020)
2.  Handle format changes across years
3.  Consistent schema across all years

### Phase 3: Additional Data Types

1.  STAAR assessment data
2.  Staff/teacher data
3.  Financial data
4.  Graduation/dropout rates

### Phase 4: Advanced Features

1.  Multi-year data fetching
2.  Data validation and quality checks
3.  Cohort tracking utilities
4.  Enrollment projections

## Success Metrics

- Successfully fetch data for all available years
- 100% of exported functions documented
- Test coverage \> 80%
- CRAN-ready (pass R CMD check)

## Risks and Mitigations

| Risk                        | Mitigation                              |
|-----------------------------|-----------------------------------------|
| TEA changes file formats    | Version detection, graceful degradation |
| TEA removes historical data | Cache locally, document limitations     |
| Large file downloads        | Progress indicators, chunked downloads  |
| Data suppression            | Clear NA handling, documentation        |

## References

- Texas Education Agency: <https://tea.texas.gov>
- PEIMS Data Standards:
  <https://tea.texas.gov/reports-and-data/data-submission/peims>
- TEA Data Portal: <https://tea.texas.gov/reports-and-data>
- TAC 61.1040:
  [https://texreg.sos.state.tx.us/public/readtac\$ext.TacPage?sl=R&app=9&p_dir=&p_rloc=&p_tloc=&p_ploc=&pg=1&p_tac=&ti=19&pt=2&ch=61&rl=1040](https://texreg.sos.state.tx.us/public/readtac%24ext.TacPage?sl=R&app=9&p_dir=&p_rloc=&p_tloc=&p_ploc=&pg=1&p_tac=&ti=19&pt=2&ch=61&rl=1040)
