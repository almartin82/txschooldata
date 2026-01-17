# Clean TEA ID values

TEA data often includes leading single quotes (') to force Excel to
treat IDs as text. This function removes those quotes and trims
whitespace.

## Usage

``` r
clean_id(x)
```

## Arguments

- x:

  Character vector of IDs

## Value

Cleaned character vector
