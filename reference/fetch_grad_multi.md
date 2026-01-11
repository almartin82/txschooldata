# Fetch graduation rate data for multiple class years

Downloads and combines graduation rate data for multiple class years.

## Usage

``` r
fetch_grad_multi(class_years, tidy = TRUE, use_cache = TRUE)
```

## Arguments

- class_years:

  Vector of class years (e.g., c(2018, 2019, 2020))

- tidy:

  If TRUE (default), returns data in long (tidy) format with subgroup
  column. If FALSE, returns wide format.

- use_cache:

  If TRUE (default), uses locally cached data when available.

## Value

Data frame with graduation rate data for all specified years

## Examples

``` r
if (FALSE) { # \dontrun{
# Get graduation rates for Class of 2018-2024
grad_multi <- fetch_grad_multi(2018:2024)

# Track state trends
grad_multi |>
  dplyr::filter(type == "State", subgroup == "all_students") |>
  dplyr::select(class_year, grad_rate)

# Compare districts over time
grad_multi |>
  dplyr::filter(district_id %in% c("101912", "015902")) |>
  dplyr::group_by(district_name, class_year) |>
  dplyr::summarize(
    rate = mean(grad_rate, na.rm = TRUE),
    .groups = "drop"
  )
} # }
```
