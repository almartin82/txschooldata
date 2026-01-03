# Download raw enrollment data from TEA

Downloads campus and district enrollment data from TEA's reporting
systems. Uses TAPR for 2013+, AEIS SAS broker for 2003-2012, and AEIS
CGI for 1997-2002.

## Usage

``` r
get_raw_enr(end_year)
```

## Arguments

- end_year:

  School year end (2023-24 = 2024)

## Value

List with campus and district data frames
