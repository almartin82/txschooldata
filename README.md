
<!-- README.md is generated from README.Rmd. Please edit that file -->

# txschooldata

<!-- badges: start -->

[![R-CMD-check](https://github.com/almartin82/txschooldata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/almartin82/txschooldata/actions/workflows/R-CMD-check.yaml)
[![Python
Tests](https://github.com/almartin82/txschooldata/actions/workflows/python-test.yaml/badge.svg)](https://github.com/almartin82/txschooldata/actions/workflows/python-test.yaml)
[![pkgdown](https://github.com/almartin82/txschooldata/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/almartin82/txschooldata/actions/workflows/pkgdown.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

An R package for fetching and processing Texas school enrollment and
assessment data from the Texas Education Agency (TEA).

**Documentation: <https://almartin82.github.io/txschooldata/>**

## Installation

``` r
# install.packages("remotes")
remotes::install_github("almartin82/txschooldata")
```

## Quick Start

### Enrollment Data

``` r
library(txschooldata)
library(dplyr)

# Fetch 2024 enrollment data (2023-24 school year)
enr <- fetch_enr(2024)

# Statewide total enrollment
enr |>
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") |>
  select(n_students)
#> 5,517,464 students
```

### STAAR Assessment Data

``` r
# Fetch 2023 STAAR data for Grade 3 Math (district level)
staar <- fetch_staar(2023, "district", 3, subject = "english")

# Get Houston ISD performance
staar |>
  filter(district_id == "101912", subject == "Math") |>
  select(district_name, all_total, all_did_not_meet, all_approaches, all_meets, all_masters)
#>   district_name all_total all_did_not_meet all_approaches all_meets all_masters
#> 1   HOUSTON ISD     10846             3488           7358      4716        3623
```

## What can you find with txschooldata?

Texas public schools enroll **5.5 million students** across 1,200+
districts. One function call pulls it all into R:

``` r
library(txschooldata)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Grab 5 years of enrollment data
enr <- fetch_enr_multi(2020:2024)
```

Here are ten narratives hiding in the numbers.

------------------------------------------------------------------------

### 1. COVID erased a decade of growth in one year

``` r
enr %>%
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, n_students) %>%
  arrange(end_year) %>%
  mutate(
    change = n_students - lag(n_students),
    pct_change = round(change / lag(n_students) * 100, 2)
  )
#>   end_year n_students  change pct_change
#> 1     2020    5479173      NA         NA
#> 2     2021    5359040 -120133      -2.19
#> 3     2022    5402928   43888       0.82
#> 4     2023    5504150  101222       1.87
#> 5     2024    5517464   13314       0.24
```

**-120,133 students** vanished between 2020 and 2021—equivalent to the
entire enrollment of El Paso ISD.

------------------------------------------------------------------------

### 2. One in four students is now an English learner

``` r
lep_trend <- enr %>%
  filter(is_state, subgroup == "lep", grade_level == "TOTAL") %>%
  select(end_year, n_students, pct) %>%
  mutate(pct_display = round(pct * 100, 1))

lep_trend %>% select(end_year, n_students, pct_display)
#>   end_year n_students pct_display
#> 1     2020    1112674        20.3
#> 2     2021    1108207        20.7
#> 3     2022    1171661        21.7
#> 4     2023    1269408        23.1
#> 5     2024    1344804        24.4
```

``` r
ggplot(lep_trend, aes(x = end_year, y = pct * 100)) +
  geom_line(linewidth = 1.2, color = "#E69F00") +
  geom_point(size = 3, color = "#E69F00") +
  geom_hline(yintercept = 25, linetype = "dashed", color = "gray50") +
  annotate("text", x = 2024.3, y = 25.5, label = "1 in 4", color = "gray40", size = 3.5) +
  scale_y_continuous(limits = c(19, 27)) +
  labs(
    title = "Rise of English Learners",
    subtitle = "LEP students as % of total enrollment",
    x = "School Year (End)", y = "Percent"
  ) +
  theme_minimal()
```

<figure>
<img
src="https://almartin82.github.io/txschooldata/articles/district-hooks_files/figure-html/lep-plot-1.png"
alt="LEP trend" />
<figcaption aria-hidden="true">LEP trend</figcaption>
</figure>

From 20.3% to **24.4%** in five years. Schools need more ESL teachers
than ever.

------------------------------------------------------------------------

### 3. Coppell ISD is Texas’s first Asian-majority district

``` r
enr %>%
  filter(is_district, subgroup == "asian", grade_level == "TOTAL", end_year == 2024) %>%
  inner_join(
    enr %>% filter(is_district, subgroup == "total_enrollment",
                   grade_level == "TOTAL", end_year == 2024) %>%
      select(district_id, total = n_students),
    by = "district_id"
  ) %>%
  filter(total >= 10000) %>%
  arrange(desc(pct)) %>%
  select(district_name, total, n_students, pct) %>%
  mutate(pct = round(pct * 100, 1)) %>%
  head(10)
#>     district_name total n_students  pct
#> 1     COPPELL ISD 13394       7591 56.7
#> 2      FRISCO ISD 66551      28349 42.6
#> 3     PROSPER ISD 28394       8312 29.3
#> 4       ALLEN ISD 21319       6201 29.1
#> 5   FORT BEND ISD 80034      22080 27.6
#> 6       PLANO ISD 47753      11207 23.5
#> 7  ROUND ROCK ISD 46042      10126 22.0
#> 8        KATY ISD 94589      16311 17.2
#> 9       WYLIE ISD 19166       3227 16.8
#> 10 LEWISVILLE ISD 48356       8123 16.8
```

**56.7% Asian**. Frisco ISD jumped from 33.6% to 42.6% in just three
years.

------------------------------------------------------------------------

### 4. Fort Worth ISD lost 14% of its students

``` r
d2020 <- enr %>%
  filter(is_district, subgroup == "total_enrollment",
         grade_level == "TOTAL", end_year == 2020) %>%
  select(district_id, n_2020 = n_students)

d2024 <- enr %>%
  filter(is_district, subgroup == "total_enrollment",
         grade_level == "TOTAL", end_year == 2024) %>%
  select(district_id, district_name, n_2024 = n_students)

d2020 %>%
  inner_join(d2024, by = "district_id") %>%
  mutate(
    change = n_2024 - n_2020,
    pct_change = round((change / n_2020) * 100, 1)
  ) %>%
  filter(n_2020 >= 10000) %>%
  arrange(pct_change) %>%
  select(district_name, n_2020, n_2024, change, pct_change) %>%
  head(10)
#>      district_name n_2020 n_2024 change pct_change
#> 1   FORT WORTH ISD  82704  70903 -11801      -14.3
#> 2       ALDINE ISD  67130  57737  -9393      -14.0
#> 3  BROWNSVILLE ISD  42989  37032  -5957      -13.9
#> 4   HARLANDALE ISD  13654  11781  -1873      -13.7
#> 5       YSLETA ISD  40404  34875  -5529      -13.7
#> 6       LAREDO ISD  23665  20557  -3108      -13.1
#> 7        ALIEF ISD  45281  39451  -5830      -12.9
#> 8      HOUSTON ISD 209309 183603 -25706      -12.3
#> 9      LA JOYA ISD  27276  23995  -3281      -12.0
#> 10     ABILENE ISD  16456  14482  -1974      -12.0
```

Urban districts are bleeding students to suburbs and charters.

------------------------------------------------------------------------

### 5. IDEA Public Schools grew 24% in three years

``` r
enr %>%
  filter(district_name == "IDEA PUBLIC SCHOOLS", is_district,
         subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, n_students) %>%
  arrange(end_year)
#>   end_year n_students
#> 1     2021      62158
#> 2     2022      67988
#> 3     2023      74217
#> 4     2024      76819
```

From 62,158 to **76,819 students**. Charter networks are reshaping Texas
education.

------------------------------------------------------------------------

### 6. Fort Bend ISD has no racial majority

``` r
enr %>%
  filter(district_name == "FORT BEND ISD", is_district, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian")) %>%
  select(end_year, subgroup, pct) %>%
  mutate(pct = round(pct * 100, 1)) %>%
  pivot_wider(names_from = subgroup, values_from = pct)
#> # A tibble: 4 × 5
#>   end_year white black hispanic asian
#>      <int> <dbl> <dbl>    <dbl> <dbl>
#> 1     2021  14.8  27.5     26.4  27.3
#> 2     2022  14.7  27.8     26.6  26.7
#> 3     2023  13.8  27.8     26.7  27.3
#> 4     2024  13.2  27.8     26.7  27.6
```

No group exceeds 28%. One of the most diverse large districts in
America.

------------------------------------------------------------------------

### 7. Kindergarten enrollment dropped 5.8%

``` r
enr %>%
  filter(is_state, subgroup == "total_enrollment",
         grade_level %in% c("PK", "K", "01", "05", "09", "12")) %>%
  select(end_year, grade_level, n_students) %>%
  pivot_wider(names_from = end_year, values_from = n_students) %>%
  mutate(
    change = `2024` - `2020`,
    pct_change = round(change / `2020` * 100, 1)
  )
#> # A tibble: 6 × 8
#>   grade_level `2020` `2021` `2022` `2023` `2024` change pct_change
#>   <chr>        <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>      <dbl>
#> 1 PK          248413 196560 222767 243493 247979   -434       -0.2
#> 2 K           383585 360865 370054 367180 361329 -22256       -5.8
#> 3 01          391175 380973 384494 399048 385096  -6079       -1.6
#> 4 05          417272 395436 387945 395111 399200 -18072       -4.3
#> 5 09          448929 436396 475437 477875 472595  23666        5.3
#> 6 12          352258 362888 360056 364317 365788  13530        3.8
```

**-22,256 kindergartners** since 2020. The pipeline is narrowing.

------------------------------------------------------------------------

### 8. 62% of students are economically disadvantaged

``` r
econ_trend <- enr %>%
  filter(is_state, subgroup == "econ_disadv", grade_level == "TOTAL") %>%
  select(end_year, n_students, pct) %>%
  mutate(pct = round(pct * 100, 1))

econ_trend
#>   end_year n_students  pct
#> 1     2020    3303974 60.3
#> 2     2021    3229178 60.3
#> 3     2022    3278452 60.7
#> 4     2023    3415987 62.1
#> 5     2024    3434955 62.3
```

``` r
ggplot(econ_trend, aes(x = end_year, y = pct)) +
  geom_col(fill = "#56B4E9", width = 0.6) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "gray50") +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.3, size = 3.5) +
  scale_y_continuous(limits = c(0, 70)) +
  labs(
    title = "Economically Disadvantaged Students",
    subtitle = "Percentage of total enrollment",
    x = "School Year (End)", y = "Percent"
  ) +
  theme_minimal()
```

<figure>
<img
src="https://almartin82.github.io/txschooldata/articles/district-hooks_files/figure-html/econ-plot-1.png"
alt="Econ disadvantaged trend" />
<figcaption aria-hidden="true">Econ disadvantaged trend</figcaption>
</figure>

Up from 60.3%. Nearly two-thirds of Texas students qualify.

------------------------------------------------------------------------

### 9. White students dropped below 25%

``` r
enr %>%
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian", "multiracial")) %>%
  select(end_year, subgroup, pct) %>%
  mutate(pct = round(pct * 100, 1)) %>%
  pivot_wider(names_from = subgroup, values_from = pct)
#> # A tibble: 5 × 6
#>   end_year white black hispanic asian multiracial
#>      <int> <dbl> <dbl>    <dbl> <dbl>       <dbl>
#> 1     2020  27    12.6     52.8   4.6         2.5
#> 2     2021  26.5  12.7     52.9   4.7         2.7
#> 3     2022  26.3  12.8     52.8   4.8         2.9
#> 4     2023  25.6  12.8     53     5.1         3  
#> 5     2024  25    12.8     53.2   5.4         3.1
```

``` r
enr %>%
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian")) %>%
  mutate(pct = pct * 100) %>%
  ggplot(aes(x = end_year, y = pct, color = subgroup)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("hispanic" = "#0072B2", "white" = "#E69F00",
               "black" = "#009E73", "asian" = "#CC79A7"),
    labels = c("Asian", "Black", "Hispanic", "White")
  ) +
  labs(
    title = "Texas Public School Demographics",
    x = "School Year (End)", y = "Percent of Enrollment",
    color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

<figure>
<img
src="https://almartin82.github.io/txschooldata/articles/district-hooks_files/figure-html/demo-plot-1.png"
alt="Demographics over time" />
<figcaption aria-hidden="true">Demographics over time</figcaption>
</figure>

Hispanic students are now **53.2%** of enrollment.

------------------------------------------------------------------------

### 10. 439 districts now have Hispanic majorities

``` r
enr %>%
  filter(is_district, subgroup == "hispanic", grade_level == "TOTAL") %>%
  mutate(majority = pct > 0.5) %>%
  group_by(end_year) %>%
  summarize(
    total_districts = n(),
    hispanic_majority = sum(majority),
    pct_majority = round(hispanic_majority / total_districts * 100, 1),
    .groups = "drop"
  )
#> # A tibble: 5 × 4
#>   end_year total_districts hispanic_majority pct_majority
#>      <int>           <int>             <int>        <dbl>
#> 1     2020            1202               419         34.9
#> 2     2021            1204               428         35.5
#> 3     2022            1207               433         35.9
#> 4     2023            1209               438         36.2
#> 5     2024            1207               439         36.4
```

Up from 419 in 2020. Now **36%** of all Texas school districts.

------------------------------------------------------------------------

## Graduation rates

Track four-year longitudinal graduation rates for the Class of
2003-2024:

``` r
library(txschooldata)

# Get Class of 2024 graduation rates
grad <- fetch_grad(2024)

# State graduation rate by subgroup
grad |>
  filter(type == "State") |>
  select(subgroup, grad_rate) |>
  arrange(desc(grad_rate))
#>           subgroup grad_rate
#> 1            asian  95.59655
#> 2           female  95.40719
#> 3            white  94.74369
#> 4     all_students  94.39027
#> 5         hispanic  93.69867
#> 6             male  93.38859
#> 7      econ_disadv  93.24518
#> 8      multiracial  93.09000
#> 9          at_risk  92.12205
#> 10             lep  91.25872
#> 11      special_ed  87.80512
#> 12           black        NA
#> 13 native_american        NA
```

Data available for all students plus race/ethnicity, gender, and special
populations (economically disadvantaged, special education, LEP,
at-risk).

------------------------------------------------------------------------

## Learn more

- **[Full analysis with more
  charts](https://almartin82.github.io/txschooldata/articles/district-hooks.html)**
- **[Getting started
  guide](https://almartin82.github.io/txschooldata/articles/quickstart.html)**
- **[Function
  reference](https://almartin82.github.io/txschooldata/reference/)**

## Data availability

This package pulls from three TEA reporting systems:

| System       | Years     | Notes                                |
|--------------|-----------|--------------------------------------|
| **AEIS CGI** | 1997-2002 | Older AEIS format via CGI endpoint   |
| **AEIS SAS** | 2003-2012 | Academic Excellence Indicator System |
| **TAPR**     | 2013-2025 | Texas Academic Performance Reports   |

All data is fetched directly from TEA servers—no manual downloads
required.

### What’s included

**Enrollment data:** - **Levels:** State, district (~1,200), and campus
(~9,000) - **Demographics:** White, Black, Hispanic, Asian, American
Indian, Pacific Islander, Two or More Races - **Special populations:**
Economically disadvantaged, LEP/English learners, Special education,
At-risk, Gifted - **Grade levels:** Early Education (EE) through Grade
12, plus totals

**Graduation rate data:** - **Years:** Class of 2003-2024 (four-year
longitudinal rates) - **Levels:** State, district, campus -
**Subgroups:** All students, race/ethnicity, gender, special
populations - **Note:** Uses federal calculation for consistency across
years

**STAAR assessment data:** - **Years:** 2018, 2021-2023 (legacy
format) - **Levels:** District (~1,200) and campus (~9,000) -
**Grades:** 3-8 - **Subjects:** Reading/Language Arts (RLA) and Math -
**Performance levels:** Did Not Meet, Approaches, Meets, Masters Grade
Level - **Note:** 2019-2020 not available (COVID-19 pandemic disrupted
testing)

### Formatting notes

- **IDs:** District IDs are 6 digits (e.g., `101912` for Austin ISD).
  Campus IDs are 9 digits (district + 3-digit campus number).
- **Tidy format:** By default, `fetch_enr()` returns long/tidy data with
  `subgroup`, `grade_level`, and `n_students` columns. Use
  `tidy = FALSE` for wide format. STAAR data works similarly with
  `fetch_staar()`.
- **Percentages:** The `pct` column is a proportion (0-1), not a
  percentage. Multiply by 100 for display.
- **Caching:** Data is cached locally after first download. Use
  `use_cache = FALSE` to force refresh.
- **STAAR performance levels:** TEA’s performance level counts may not
  sum to the total tested count. This is the TEA’s data structure
  (students may be counted in multiple categories depending on reporting
  methodology).

### Caveats

- **Asian/Pacific Islander:** Pre-2011 data combines Asian and Pacific
  Islander into a single “asian” category. Separate Pacific Islander
  data only available from 2011 onward (federal reporting change).
- **Two or More Races:** Only available from 2011 onward (federal
  reporting change)
- **Column names:** Standardized across years, but underlying TEA
  variable names differ between systems
- **Historical comparisons:** Definition of “economically disadvantaged”
  and other categories may shift over time

## Part of the State Schooldata Project

This package is part of the
[njschooldata](https://github.com/almartin82/njschooldata) family of
packages providing consistent access to state education data.

A simple, consistent interface for accessing state-published school data
in Python and R.

**All 50 state packages:**
[github.com/almartin82](https://github.com/almartin82?tab=repositories&q=schooldata)

## License

MIT
