# Extract STAAR metrics for a specific subject and student group

Extract STAAR metrics for a specific subject and student group

## Usage

``` r
extract_staar_subject(df, subject_prefix, group)
```

## Arguments

- df:

  Raw STAAR data frame

- subject_prefix:

  Subject column prefix ("RLA\_" or "m\_")

- group:

  Student group ("all", "sexm", "ethh", etc.)

## Value

List with total and performance level counts
