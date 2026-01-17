# Identify graduation rate aggregation levels

Adds boolean flags to identify state, district, and campus level
records.

## Usage

``` r
id_grad_aggs(df)
```

## Arguments

- df:

  Graduation rate dataframe, output of tidy_grad

## Value

data.frame with boolean aggregation flags

## Examples

``` r
if (FALSE) { # \dontrun{
tidy_data <- fetch_grad(2024)
# Data already has aggregation flags via tidy_grad
table(tidy_data$is_state, tidy_data$is_district, tidy_data$is_campus)
} # }
```
