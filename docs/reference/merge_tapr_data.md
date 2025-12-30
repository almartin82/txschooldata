# Merge TAPR reference and student data

Joins reference data (names, IDs) with student data (counts) based on
campus or district ID.

## Usage

``` r
merge_tapr_data(ref_data, stud_data, level)
```

## Arguments

- ref_data:

  Reference data frame (cref or dref)

- stud_data:

  Student data frame (cstud or dstud)

- level:

  Either "campus" or "district"

## Value

Merged data frame
