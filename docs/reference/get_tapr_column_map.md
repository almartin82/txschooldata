# Get column mappings for TAPR data

Returns a list mapping TAPR column names to standardized names. TAPR
uses a specific naming convention:

- First char: C=Campus, D=District, R=Region, S=State

- PET: Student enrollment

- Suffix: demographic or program code

## Usage

``` r
get_tapr_column_map()
```

## Value

Named list of column mappings
