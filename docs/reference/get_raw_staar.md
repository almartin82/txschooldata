# Download raw STAAR data from TEA

Downloads STAAR aggregate data files from TEA's website. Legacy format
covers 2018-2023 (no data for 2019-2020).

## Usage

``` r
get_raw_staar(year, level, grade, subject = "english")
```

## Arguments

- year:

  Assessment year (2018, 2021, 2022, or 2023)

- level:

  "district" or "campus"

- grade:

  Grade level (3-8)

- subject:

  "english" or "spanish" (default: "english")

## Value

Data frame with raw STAAR data
