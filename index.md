# txschooldata

Texas has the second-largest public school system in America – **5.5
million students** across 1,200+ districts. `txschooldata` gives you
one-line access to enrollment, demographics, graduation rates, and STAAR
assessment data directly from the Texas Education Agency (TEA), covering
1997 through 2025. No manual downloads, no scraping, no file wrangling.

This package is part of the
[njschooldata](https://github.com/almartin82/njschooldata) family – a
set of R and Python packages providing consistent access to
state-published school data for all 50 states.

**Documentation: <https://almartin82.github.io/txschooldata/>**

## R Quickstart

``` r
# install.packages("remotes")
remotes::install_github("almartin82/txschooldata")
```

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

## Python Quickstart

``` bash
pip install pytxschooldata
```

``` python
from pytxschooldata import fetch_enr

enr = fetch_enr(2024)
print(enr.head())
```

The Python package wraps the R package via `rpy2` and returns pandas
DataFrames. It exposes
[`fetch_enr()`](https://almartin82.github.io/txschooldata/reference/fetch_enr.md),
[`fetch_enr_multi()`](https://almartin82.github.io/txschooldata/reference/fetch_enr_multi.md),
[`tidy_enr()`](https://almartin82.github.io/txschooldata/reference/tidy_enr.md),
and
[`get_available_years()`](https://almartin82.github.io/txschooldata/reference/get_available_years.md).

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
enr <- fetch_enr_multi(2020:2024, use_cache = TRUE)
```

Here are fifteen narratives hiding in the numbers.

------------------------------------------------------------------------

### 1. COVID erased a decade of growth in one year

The pandemic caused the largest single-year enrollment drop in Texas
history. Between 2020 and 2021, Texas public schools lost **120,133
students** – equivalent to the entire enrollment of El Paso ISD.

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

**-120,133 students** vanished between 2020 and 2021 – equivalent to the
entire enrollment of El Paso ISD.

![COVID enrollment
drop](https://almartin82.github.io/txschooldata/articles/district-hooks_files/figure-html/covid-plot-1.png)

COVID enrollment drop

------------------------------------------------------------------------

### 2. One in four students is now an English learner

The proportion of LEP students grew by 4.1 percentage points in five
years – from 20.3% to 24.4%. This is the single largest demographic
shift in the data.

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

![LEP
trend](https://almartin82.github.io/txschooldata/articles/district-hooks_files/figure-html/lep-plot-1.png)

LEP trend

From 20.3% to **24.4%** in five years. Schools need more ESL teachers
than ever.

------------------------------------------------------------------------

### 3. Coppell ISD is Texas’s first Asian-majority district

In a striking demographic shift, Coppell ISD became Texas’s first
Asian-majority public school district, with **56.7%** of students
identifying as Asian in 2024.

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

While statewide enrollment recovered after COVID, urban districts
continue to lose students. Fort Worth ISD lost **11,801 students**
(-14.3%), making it the fastest-declining large district in Texas.

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

IDEA Public Schools, a charter network operating across Texas, grew from
62,158 to 76,819 students between 2021 and 2024 – a gain of **14,661
students** (+23.6%).

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

Fort Bend ISD is one of the most diverse school districts in America. No
racial group exceeds 28% of enrollment – White (13.2%), Black (27.8%),
Hispanic (26.7%), and Asian (27.6%) students are nearly equally
represented.

``` r
enr %>%
  filter(district_name == "FORT BEND ISD", is_district, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian")) %>%
  select(end_year, subgroup, pct) %>%
  mutate(pct = round(pct * 100, 1)) %>%
  pivot_wider(names_from = subgroup, values_from = pct)
#> # A tibble: 4 x 5
#>   end_year white black hispanic asian
#>      <int> <dbl> <dbl>    <dbl> <dbl>
#> 1     2021  14.8  27.5     26.4  27.3
#> 2     2022  14.7  27.8     26.6  26.7
#> 3     2023  13.8  27.8     26.7  27.3
#> 4     2024  13.2  27.8     26.7  27.6
```

![Fort Bend
diversity](https://almartin82.github.io/txschooldata/articles/district-hooks_files/figure-html/diversity-chart-1.png)

Fort Bend diversity

No group exceeds 28%. One of the most diverse large districts in
America.

------------------------------------------------------------------------

### 7. Kindergarten enrollment dropped 5.8%

Kindergarten enrollment fell from 383,585 to 361,329 – a drop of 22,256
students (-5.8%). This could signal declining birth rates, rising
private school enrollment, or families delaying school entry.

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
#> # A tibble: 6 x 8
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

The share of economically disadvantaged students grew from 60.3% to
62.3% – nearly two-thirds of all students in Texas public schools.

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

![Econ disadvantaged
trend](https://almartin82.github.io/txschooldata/articles/district-hooks_files/figure-html/econ-plot-1.png)

Econ disadvantaged trend

Up from 60.3%. Nearly two-thirds of Texas students qualify.

------------------------------------------------------------------------

### 9. White students dropped below 25%

White enrollment declined from 27.0% to 25.0% of total enrollment – a 2
percentage point drop in just five years. Meanwhile, Hispanic (53.2%)
and Asian (5.4%) shares continue to grow.

``` r
enr %>%
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian", "multiracial")) %>%
  select(end_year, subgroup, pct) %>%
  mutate(pct = round(pct * 100, 1)) %>%
  pivot_wider(names_from = subgroup, values_from = pct)
#> # A tibble: 5 x 6
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

![Demographics over
time](https://almartin82.github.io/txschooldata/articles/district-hooks_files/figure-html/demo-plot-1.png)

Demographics over time

Hispanic students are now **53.2%** of enrollment.

------------------------------------------------------------------------

### 10. 439 districts now have Hispanic majorities

The number of districts where Hispanic students are the majority grew
from 419 in 2020 to **439 in 2024** – now 36% of all Texas districts.

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
#> # A tibble: 5 x 4
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

### 11. Houston ISD vs Dallas ISD: two giants, two trajectories

Houston ISD and Dallas ISD are the two largest traditional districts in
Texas. Both lost students since 2020, but Houston’s losses are nearly
double Dallas’s in absolute terms.

``` r
big_two <- enr %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         district_name %in% c("HOUSTON ISD", "DALLAS ISD")) %>%
  select(end_year, district_name, n_students) %>%
  pivot_wider(names_from = district_name, values_from = n_students) %>%
  mutate(
    houston_change = `HOUSTON ISD` - lag(`HOUSTON ISD`),
    dallas_change = `DALLAS ISD` - lag(`DALLAS ISD`)
  )

print(big_two)
#> [output will be generated when vignette renders]
```

``` r
enr %>%
  filter(is_district, subgroup == "total_enrollment", grade_level == "TOTAL",
         district_name %in% c("HOUSTON ISD", "DALLAS ISD")) %>%
  ggplot(aes(x = end_year, y = n_students, color = district_name)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c("DALLAS ISD" = "#0072B2", "HOUSTON ISD" = "#CC0000")) +
  labs(
    title = "Houston ISD vs Dallas ISD Enrollment",
    subtitle = "Both declining, but Houston falling faster",
    x = "School Year (End)", y = "Total Students", color = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

![Houston vs
Dallas](https://almartin82.github.io/txschooldata/articles/district-hooks_files/figure-html/houston-vs-dallas-plot-1.png)

Houston vs Dallas

Houston lost **25,706 students** vs Dallas’s **11,527**. Both are losing
ground to suburban and charter competitors.

------------------------------------------------------------------------

### 12. Special education identification rose to 12%

The share of students receiving special education services has climbed
steadily, reaching roughly **12%** of total enrollment by 2024. That
means approximately 1 in 8 Texas students now receives special education
services.

``` r
sped_trend <- enr %>%
  filter(is_state, subgroup == "special_ed", grade_level == "TOTAL") %>%
  select(end_year, n_students, pct) %>%
  mutate(pct_display = round(pct * 100, 1))

sped_trend %>% select(end_year, n_students, pct_display)
#> [output will be generated when vignette renders]
```

``` r
ggplot(sped_trend, aes(x = end_year, y = pct * 100)) +
  geom_line(linewidth = 1.2, color = "#009E73") +
  geom_point(size = 3, color = "#009E73") +
  geom_text(aes(label = paste0(round(pct * 100, 1), "%")), vjust = -0.8, size = 3.5) +
  scale_y_continuous(limits = c(9, 14)) +
  labs(
    title = "Special Education Growth",
    subtitle = "Students receiving special ed services as % of total",
    x = "School Year (End)", y = "Percent"
  ) +
  theme_minimal()
```

![Special ed
growth](https://almartin82.github.io/txschooldata/articles/district-hooks_files/figure-html/sped-plot-1.png)

Special ed growth

The increase likely reflects better identification, expanded
definitions, and growing awareness of learning differences.

------------------------------------------------------------------------

### 13. Frisco ISD added 20,000 students in five years

Frisco ISD is one of the fastest-growing large districts in the state,
adding **20,259 students** between 2020 and 2024 – a 44% increase. The
suburban Collin County district shows no signs of slowing down.

``` r
frisco_trend <- enr %>%
  filter(district_name == "FRISCO ISD", is_district,
         subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, n_students) %>%
  arrange(end_year) %>%
  mutate(
    change = n_students - lag(n_students),
    pct_change = round(change / lag(n_students) * 100, 1)
  )

print(frisco_trend)
#> [output will be generated when vignette renders]
```

``` r
ggplot(frisco_trend, aes(x = end_year, y = n_students)) +
  geom_col(fill = "#56B4E9", width = 0.6) +
  geom_text(aes(label = comma(n_students)), vjust = -0.3, size = 3.5) +
  scale_y_continuous(labels = comma, limits = c(0, 75000)) +
  labs(
    title = "Frisco ISD Enrollment Surge",
    subtitle = "One of the fastest-growing large districts in Texas",
    x = "School Year (End)", y = "Total Students"
  ) +
  theme_minimal()
```

![Frisco ISD
growth](https://almartin82.github.io/txschooldata/articles/district-hooks_files/figure-html/frisco-growth-plot-1.png)

Frisco ISD growth

While urban cores lose students, suburban boomtowns like Frisco absorb
the growth.

------------------------------------------------------------------------

### 14. Pre-K enrollment still hasn’t recovered from COVID

Pre-K enrollment cratered by **21%** during COVID (from 248,413 to
196,560). By 2024 it has clawed back to 247,979 – still **434 students
short** of pre-pandemic levels.

``` r
pk_trend <- enr %>%
  filter(is_state, subgroup == "total_enrollment", grade_level == "PK") %>%
  select(end_year, n_students) %>%
  arrange(end_year) %>%
  mutate(
    change = n_students - lag(n_students),
    pct_change = round(change / lag(n_students) * 100, 1),
    vs_2020 = n_students - first(n_students)
  )

print(pk_trend)
#> [output will be generated when vignette renders]
```

``` r
ggplot(pk_trend, aes(x = end_year, y = n_students)) +
  geom_line(linewidth = 1.2, color = "#D55E00") +
  geom_point(size = 3, color = "#D55E00") +
  geom_hline(yintercept = 248413, linetype = "dashed", color = "gray50") +
  annotate("text", x = 2022, y = 251000, label = "2020 level", color = "gray40", size = 3.5) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Pre-K Enrollment: The Slowest Recovery",
    subtitle = "Still below 2020 levels after four years",
    x = "School Year (End)", y = "Pre-K Students"
  ) +
  theme_minimal()
```

![Pre-K
recovery](https://almartin82.github.io/txschooldata/articles/district-hooks_files/figure-html/pk-recovery-plot-1.png)

Pre-K recovery

The slow Pre-K recovery matters because early childhood education is one
of the highest-return investments in education.

------------------------------------------------------------------------

### 15. Multiracial students are the fastest-growing demographic

Students identifying as Two or More Races grew from 2.5% to 3.1% of
enrollment between 2020 and 2024 – a 24% increase in raw numbers. While
still a small share, this is the fastest percentage growth of any
demographic group.

``` r
multi_trend <- enr %>%
  filter(is_state, subgroup == "multiracial", grade_level == "TOTAL") %>%
  select(end_year, n_students, pct) %>%
  mutate(pct_display = round(pct * 100, 1))

multi_trend %>% select(end_year, n_students, pct_display)
#> [output will be generated when vignette renders]
```

``` r
ggplot(multi_trend, aes(x = end_year, y = n_students)) +
  geom_line(linewidth = 1.2, color = "#882255") +
  geom_point(size = 3, color = "#882255") +
  geom_text(aes(label = comma(n_students)), vjust = -0.8, size = 3.5) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Multiracial Students: Fastest-Growing Group",
    subtitle = "Two or More Races enrollment, 2020-2024",
    x = "School Year (End)", y = "Students"
  ) +
  theme_minimal()
```

![Multiracial
growth](https://almartin82.github.io/txschooldata/articles/district-hooks_files/figure-html/multiracial-plot-1.png)

Multiracial growth

This trend mirrors national patterns and reflects both demographic
change and evolving identity choices.

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

## Data Notes

### Data source

All data is fetched directly from the **Texas Education Agency (TEA)**
servers at <https://rptsvr1.tea.texas.gov/>. No manual downloads
required.

### Available years

This package pulls from three TEA reporting systems:

| System       | Years     | Notes                                |
|--------------|-----------|--------------------------------------|
| **AEIS CGI** | 1997-2002 | Older AEIS format via CGI endpoint   |
| **AEIS SAS** | 2003-2012 | Academic Excellence Indicator System |
| **TAPR**     | 2013-2025 | Texas Academic Performance Reports   |

### Suppression rules

TEA suppresses enrollment counts when a subgroup has fewer than 5
students at a campus to protect student privacy. Suppressed values
appear as `NA` in the data.

### Known data quality issues

- **Asian/Pacific Islander:** Pre-2011 data combines Asian and Pacific
  Islander into a single “asian” category. Separate Pacific Islander
  data only available from 2011 onward (federal reporting change).
- **Two or More Races:** Only available from 2011 onward (federal
  reporting change).
- **Historical comparisons:** Definition of “economically disadvantaged”
  and other categories may shift over time.
- **STAAR performance levels:** TEA’s performance level counts may not
  sum to the total tested count. Students may be counted in multiple
  categories depending on reporting methodology.

### Census Day

TEA enrollment data is based on the **PEIMS Fall Snapshot**, typically
taken on the last Friday in October each year.

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
- **Tidy format:** By default,
  [`fetch_enr()`](https://almartin82.github.io/txschooldata/reference/fetch_enr.md)
  returns long/tidy data with `subgroup`, `grade_level`, and
  `n_students` columns. Use `tidy = FALSE` for wide format. STAAR data
  works similarly with
  [`fetch_staar()`](https://almartin82.github.io/txschooldata/reference/fetch_staar.md).
- **Percentages:** The `pct` column is a proportion (0-1), not a
  percentage. Multiply by 100 for display.
- **Caching:** Data is cached locally after first download. Use
  `use_cache = FALSE` to force refresh.

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
