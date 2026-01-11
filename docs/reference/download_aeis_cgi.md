# Download AEIS data via CGI endpoint (1997-2002)

Downloads data from TEA's older AEIS system using POST requests to CGI.
This system returns comma-delimited data without headers.

## Usage

``` r
download_aeis_cgi(end_year, level)
```

## Arguments

- end_year:

  School year end (1997-2002)

- level:

  Summary level: "camp" for campus, "dist" for district

## Value

Data frame with combined reference and student data
