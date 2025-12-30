# Download combined TAPR reference and student data

Downloads data from TEA's TAPR system using GET requests to the SAS
broker. This function retrieves both reference (ID, name) and student
(enrollment) data in a single request.

## Usage

``` r
download_tapr_combined(end_year, sumlev)
```

## Arguments

- end_year:

  School year end

- sumlev:

  Summary level: "C" for campus, "D" for district

## Value

Data frame
