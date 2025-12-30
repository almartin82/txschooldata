# Process raw TEA enrollment data

Transforms raw TAPR data into a standardized schema combining campus and
district data.

## Usage

``` r
process_enr(raw_data, end_year)
```

## Arguments

- raw_data:

  List containing campus and district data frames from get_raw_enr

- end_year:

  School year end

## Value

Processed data frame with standardized columns
