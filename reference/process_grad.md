# Process raw TEA graduation rate data

Transforms raw TEA graduation rate data into a standardized schema.
Combines campus, district, and county data into a single data frame.

## Usage

``` r
process_grad(raw_data, class_year)
```

## Arguments

- raw_data:

  List containing campus, district, and county data frames from
  get_raw_grad

- class_year:

  Class year

## Value

Processed data frame with standardized columns
