# Fetch STAAR assessment data

Downloads, processes, and optionally tidies STAAR assessment data from
TEA. Supports legacy STAAR aggregate data for years 2018, 2021-2023.

## Usage

``` r
fetch_staar(
  year,
  level,
  grade,
  subject = "english",
  tidy = FALSE,
  use_cache = TRUE
)
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

- tidy:

  Return tidy long format? (default: FALSE)

- use_cache:

  Use cached data if available? (default: TRUE)

## Value

Data frame with STAAR assessment data

## Examples

``` r
if (FALSE) { # \dontrun{
# Fetch district-level Grade 3 RLA and Math results for 2023
staar_2023 <- fetch_staar(2023, "district", 3)

# Fetch tidy format
staar_tidy <- fetch_staar(2023, "district", 3, tidy = TRUE)

# Fetch campus-level data
staar_campus <- fetch_staar(2023, "campus", 5)
} # }
```
