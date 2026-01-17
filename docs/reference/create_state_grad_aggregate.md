# Create state-level aggregate for graduation rates

Creates a state aggregate row from district-level data. State rates
should match TEA's published statewide graduation rate.

## Usage

``` r
create_state_grad_aggregate(district_df, class_year)
```

## Arguments

- district_df:

  Processed district data frame

- class_year:

  Class year

## Value

Data frame with single state row
