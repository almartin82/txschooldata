# Process raw STAAR data

Standardizes raw STAAR data across years by normalizing column names and
selecting key metrics. Handles schema changes between 2018, 2021-2022,
and 2023.

## Usage

``` r
process_staar(raw_data, year, level, grade)
```

## Arguments

- raw_data:

  Raw data frame from get_raw_staar()

- year:

  Assessment year

- level:

  "district" or "campus"

- grade:

  Grade level

## Value

Processed data frame with standardized columns
