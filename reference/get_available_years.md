# Get available years for Texas enrollment data

Returns metadata about the range of school years for which enrollment
data is available from the Texas Education Agency.

## Usage

``` r
get_available_years()
```

## Value

A list with components:

- min_year:

  Earliest available school year end (1997)

- max_year:

  Latest available school year end (2025)

- description:

  Human-readable description of the data availability

## Examples

``` r
years <- get_available_years()
years$min_year
#> [1] 1997
years$max_year
#> [1] 2024
years$description
#> [1] "Texas enrollment data from TEA is available for school years 1996-97 through 2023-24 (end years 1997-2024). Data sources: AEIS CGI (1997-2002), AEIS SAS (2003-2012), TAPR (2013-2024)"
```
