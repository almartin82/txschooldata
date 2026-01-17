# Tidy graduation rate data

Transforms wide graduation rate data to long format with subgroup
column.

## Usage

``` r
tidy_grad(df)
```

## Arguments

- df:

  A wide data.frame of processed graduation rate data

## Value

A long data.frame of tidied graduation rate data

## Examples

``` r
if (FALSE) { # \dontrun{
wide_data <- fetch_grad(2024, tidy = FALSE)
tidy_data <- tidy_grad(wide_data)
} # }
```
