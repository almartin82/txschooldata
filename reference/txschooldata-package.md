# txschooldata: Fetch and Process Texas School Data

Downloads and processes school data from the Texas Education Agency
(TEA). Provides functions for fetching enrollment data from PEIMS
(Public Education Information Management System) via the TAPR (Texas
Academic Performance Reports) system and transforming it into tidy
format for analysis.

## Main functions

- [`fetch_enr`](https://almartin82.github.io/txschooldata/reference/fetch_enr.md):

  Fetch enrollment data for a school year

- [`fetch_enr_multi`](https://almartin82.github.io/txschooldata/reference/fetch_enr_multi.md):

  Fetch enrollment data for multiple years

- [`tidy_enr`](https://almartin82.github.io/txschooldata/reference/tidy_enr.md):

  Transform wide data to tidy (long) format

- [`id_enr_aggs`](https://almartin82.github.io/txschooldata/reference/id_enr_aggs.md):

  Add aggregation level flags

- [`enr_grade_aggs`](https://almartin82.github.io/txschooldata/reference/enr_grade_aggs.md):

  Create grade-level aggregations

## Cache functions

- [`cache_status`](https://almartin82.github.io/txschooldata/reference/cache_status.md):

  View cached data files

- [`clear_cache`](https://almartin82.github.io/txschooldata/reference/clear_cache.md):

  Remove cached data files

## ID System

Texas uses a hierarchical ID system:

- District IDs: 6 digits (e.g., 101912 = Austin ISD)

- Campus IDs: 9 digits (district ID + 3-digit campus number)

## Data Sources

Data is sourced from the Texas Education Agency's TAPR system:

- TAPR:
  <https://tea.texas.gov/texas-schools/accountability/academic-accountability/performance-reporting/texas-academic-performance-reports>

- PEIMS: <https://tea.texas.gov/reports-and-data/school-data/peims-data>

## See also

Useful links:

- <https://almartin82.github.io/txschooldata/>

- <https://github.com/almartin82/txschooldata>

- Report bugs at <https://github.com/almartin82/txschooldata/issues>

## Author

**Maintainer**: Al Martin <almartin@example.com>
