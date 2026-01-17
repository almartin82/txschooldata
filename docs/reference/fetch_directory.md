# Fetch Texas school directory data

Downloads and processes school directory data from the Texas Education
Agency via the Texas Open Data Portal. This includes all public schools
and districts with contact information and administrator names.

## Usage

``` r
fetch_directory(end_year = NULL, tidy = TRUE, use_cache = TRUE)
```

## Arguments

- end_year:

  Currently unused. The directory data represents current schools and is
  updated regularly. Included for API consistency with other fetch
  functions.

- tidy:

  If TRUE (default), returns data in a standardized format with
  consistent column names. If FALSE, returns raw column names from TEA.

- use_cache:

  If TRUE (default), uses locally cached data when available. Set to
  FALSE to force re-download from TEA.

## Value

A tibble with school directory data. Columns include:

- `state_district_id`: 6-digit district identifier (zero-padded)

- `state_school_id`: 9-digit campus identifier (district + campus)

- `school_name`: School name

- `district_name`: District name

- `county_name`: County name

- `school_type`: Type of instruction (e.g., "REGULAR INSTRUCTIONAL")

- `grades_served`: Grade range (e.g., "09-12")

- `address`: Street address

- `city`: City

- `state`: State (always "TX")

- `zip`: ZIP code

- `phone`: Phone number

- `principal_name`: School principal name

- `principal_email`: School email address

- `superintendent_name`: District superintendent name

- `superintendent_email`: District email address

- `charter_type`: Charter type (empty if not charter)

- `status`: School status (e.g., "Active")

- `enrollment`: School enrollment as of October snapshot

## Details

The directory data is downloaded from the Texas Open Data Portal, which
publishes TEA's AskTED (Texas Education Directory) data. This data is
updated daily by TEA and represents the current state of Texas public
schools and districts.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get school directory data
dir_data <- fetch_directory()

# Get raw format (original TEA column names)
dir_raw <- fetch_directory(tidy = FALSE)

# Force fresh download (ignore cache)
dir_fresh <- fetch_directory(use_cache = FALSE)

# Filter to active schools only
library(dplyr)
active_schools <- dir_data |>
  filter(status == "Active")

# Find all schools in a district
austin_schools <- dir_data |>
  filter(state_district_id == "227901")
} # }
```
