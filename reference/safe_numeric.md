# Convert to numeric, handling suppression markers

TEA uses various markers for suppressed data (\*, \<5, -1, etc.) and may
use commas in large numbers.

## Usage

``` r
safe_numeric(x)
```

## Arguments

- x:

  Vector to convert

## Value

Numeric vector with NA for non-numeric values
