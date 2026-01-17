# Download AEIS data (2003-2012)

Downloads data from TEA's AEIS system using GET requests to the SAS
broker. AEIS uses separate requests for reference and student data.

## Usage

``` r
download_aeis_data(end_year, sumlev)
```

## Arguments

- end_year:

  School year end (2003-2012)

- sumlev:

  Summary level: "C" for campus, "D" for district

## Value

Data frame with combined reference and student data
