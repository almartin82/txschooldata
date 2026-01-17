# Tidy processed STAAR data

Converts processed STAAR data from wide format to tidy long format. Each
row represents a unique combination of entity, subject, performance
level, and student group with the corresponding count.

## Usage

``` r
tidy_staar(processed_data)
```

## Arguments

- processed_data:

  Processed data frame from process_staar()

## Value

Tidy data frame in long format
