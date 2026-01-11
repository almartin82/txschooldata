# Package index

## Fetch Data

Download data from TEA

- [`fetch_enr()`](https://almartin82.github.io/txschooldata/reference/fetch_enr.md)
  : Fetch Texas enrollment data
- [`fetch_enr_multi()`](https://almartin82.github.io/txschooldata/reference/fetch_enr_multi.md)
  : Fetch enrollment data for multiple years
- [`fetch_grad()`](https://almartin82.github.io/txschooldata/reference/fetch_grad.md)
  : Fetch Texas graduation rate data
- [`fetch_grad_multi()`](https://almartin82.github.io/txschooldata/reference/fetch_grad_multi.md)
  : Fetch graduation rate data for multiple class years
- [`fetch_staar()`](https://almartin82.github.io/txschooldata/reference/fetch_staar.md)
  : Fetch STAAR assessment data
- [`get_available_years()`](https://almartin82.github.io/txschooldata/reference/get_available_years.md)
  : Get available years for Texas enrollment data

## Process & Tidy

Transform data into analysis-ready formats

- [`tidy_enr()`](https://almartin82.github.io/txschooldata/reference/tidy_enr.md)
  : Tidy enrollment data
- [`tidy_grad()`](https://almartin82.github.io/txschooldata/reference/tidy_grad.md)
  : Tidy graduation rate data
- [`tidy_staar()`](https://almartin82.github.io/txschooldata/reference/tidy_staar.md)
  : Tidy processed STAAR data
- [`id_enr_aggs()`](https://almartin82.github.io/txschooldata/reference/id_enr_aggs.md)
  : Identify enrollment aggregation levels
- [`id_grad_aggs()`](https://almartin82.github.io/txschooldata/reference/id_grad_aggs.md)
  : Identify graduation rate aggregation levels
- [`enr_grade_aggs()`](https://almartin82.github.io/txschooldata/reference/enr_grade_aggs.md)
  : Custom Enrollment Grade Level Aggregates

## Cache Management

Manage locally cached data

- [`cache_status()`](https://almartin82.github.io/txschooldata/reference/cache_status.md)
  : Show cache status
- [`clear_cache()`](https://almartin82.github.io/txschooldata/reference/clear_cache.md)
  : Clear the txschooldata cache
- [`clear_directory_cache()`](https://almartin82.github.io/txschooldata/reference/clear_directory_cache.md)
  : Clear school directory cache
- [`clear_staar_cache()`](https://almartin82.github.io/txschooldata/reference/clear_staar_cache.md)
  : Clear STAAR cache

## Directory Data

Fetch school directory information

- [`fetch_directory()`](https://almartin82.github.io/txschooldata/reference/fetch_directory.md)
  : Fetch Texas school directory data
- [`clear_directory_cache()`](https://almartin82.github.io/txschooldata/reference/clear_directory_cache.md)
  : Clear school directory cache
- [`get_raw_directory()`](https://almartin82.github.io/txschooldata/reference/get_raw_directory.md)
  : Get raw school directory data from TEA
- [`process_directory()`](https://almartin82.github.io/txschooldata/reference/process_directory.md)
  : Process raw school directory data to standard schema
- [`read_cache_directory()`](https://almartin82.github.io/txschooldata/reference/read_cache_directory.md)
  : Read directory data from cache
- [`write_cache_directory()`](https://almartin82.github.io/txschooldata/reference/write_cache_directory.md)
  : Write directory data to cache
- [`build_cache_path_directory()`](https://almartin82.github.io/txschooldata/reference/build_cache_path_directory.md)
  : Build cache file path for directory data
- [`build_directory_url()`](https://almartin82.github.io/txschooldata/reference/build_directory_url.md)
  : Build TEA school directory download URL
- [`cache_exists_directory()`](https://almartin82.github.io/txschooldata/reference/cache_exists_directory.md)
  : Check if cached directory data exists

## STAAR Data Helpers

Helper functions for STAAR assessment data

- [`get_raw_staar()`](https://almartin82.github.io/txschooldata/reference/get_raw_staar.md)
  : Download raw STAAR data from TEA
- [`process_staar()`](https://almartin82.github.io/txschooldata/reference/process_staar.md)
  : Process raw STAAR data
- [`staar_available_years()`](https://almartin82.github.io/txschooldata/reference/staar_available_years.md)
  : Get available STAAR years
- [`get_staar_grades()`](https://almartin82.github.io/txschooldata/reference/get_staar_grades.md)
  : Get STAAR grade levels
