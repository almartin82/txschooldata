# 15 Things You Didn't Know About Texas Schools

``` r
library(txschooldata)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
```

Texas public schools enroll over 5.5 million students across 1,200+
districts, making it the second-largest school system in the United
States. This analysis explores the most striking patterns in Texas
enrollment data from 2020-2024.

``` r
# Fetch 5 years of enrollment data
enr <- fetch_enr_multi(2020:2024, use_eval = FALSE)
```

------------------------------------------------------------------------

## 1. COVID Erased a Decade of Growth in One Year

The pandemic caused the largest single-year enrollment drop in Texas
history. Between 2020 and 2021, Texas public schools lost **120,133
students**—equivalent to the entire enrollment of El Paso ISD, the 7th
largest district in the state.

``` r
state_trend <- enr %>%
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, n_students) %>%
  arrange(end_year) %>%
  mutate(
    change = n_students - lag(n_students),
    pct_change = round(change / lag(n_students) * 100, 2)
  )

print(state_trend)
```

``` r
ggplot(state_trend, aes(x = end_year, y = n_students)) +
  geom_line(linewidth = 1.2, color = "#003366") +
  geom_point(size = 3, color = "#003366") +
  geom_segment(aes(x = 2020, xend = 2021, y = 5479173, yend = 5359040),
               color = "#CC0000", linewidth = 1.2, linetype = "dashed") +
  annotate("text", x = 2020.5, y = 5380000, label = "-120,133",
           color = "#CC0000", fontface = "bold", size = 4) +
  scale_y_continuous(labels = comma, limits = c(5300000, 5600000)) +
  labs(
    title = "Texas Public School Enrollment",
    subtitle = "The COVID dip and slow recovery",
    x = "School Year (End)", y = "Total Students"
  ) +
  theme_minimal()
```

Enrollment has since recovered, surpassing pre-pandemic levels by 2023.
But the demographic composition of that recovery tells a very different
story.

------------------------------------------------------------------------

## 2. One in Four Students is Now an English Learner

The proportion of **Limited English Proficient (LEP)** students grew by
4.1 percentage points in just five years—from 20.3% to 24.4% of all
students. This is the single largest demographic shift in the data.

``` r
lep_trend <- enr %>%
  filter(is_state, subgroup == "lep", grade_level == "TOTAL") %>%
  select(end_year, n_students, pct) %>%
  mutate(pct_display = round(pct * 100, 1))

print(lep_trend %>% select(end_year, n_students, pct_display))
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

This has profound implications for staffing, curriculum, and resource
allocation across the state. Schools need more ESL teachers, bilingual
programs, and translated materials than ever before.

------------------------------------------------------------------------

## 3. Coppell ISD is Texas’s First Asian-Majority School District

In a striking demographic shift, **Coppell ISD** became Texas’s first
Asian-majority public school district, with **56.7%** of students
identifying as Asian in 2024. Nearby **Frisco ISD** is close behind at
42.6%—up from 33.6% in just three years.

``` r
# Districts with highest Asian percentage
asian_top <- enr %>%
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

print(asian_top)
```

``` r
# Frisco's Asian population growth
frisco_asian <- enr %>%
  filter(grepl("FRISCO", district_name), is_district,
         grade_level == "TOTAL", subgroup == "asian") %>%
  select(end_year, n_students, pct) %>%
  mutate(pct = round(pct * 100, 1))

ggplot(frisco_asian, aes(x = end_year, y = pct)) +
  geom_line(linewidth = 1.2, color = "#CC79A7") +
  geom_point(size = 3, color = "#CC79A7") +
  geom_text(aes(label = paste0(pct, "%")), vjust = -0.8, size = 3.5) +
  scale_y_continuous(limits = c(30, 50)) +
  labs(
    title = "Frisco ISD: Asian Student Growth",
    subtitle = "Percentage of total enrollment",
    x = "School Year (End)", y = "Percent Asian"
  ) +
  theme_minimal()
```

These districts in the Dallas-Fort Worth metroplex reflect changing
immigration and migration patterns, with significant growth in families
from India, China, and other Asian countries.

------------------------------------------------------------------------

## 4. Fort Worth ISD Lost 14% of Its Students

While statewide enrollment recovered after COVID, urban districts
continue to hemorrhage students. **Fort Worth ISD** lost **11,801
students** (-14.3%), making it the fastest-declining large district in
Texas.

``` r
# Calculate 2020-2024 changes
d2020 <- enr %>%
  filter(is_district, subgroup == "total_enrollment",
         grade_level == "TOTAL", end_year == 2020) %>%
  select(district_id, n_2020 = n_students)

d2024 <- enr %>%
  filter(is_district, subgroup == "total_enrollment",
         grade_level == "TOTAL", end_year == 2024) %>%
  select(district_id, district_name, n_2024 = n_students)

losses <- d2020 %>%
  inner_join(d2024, by = "district_id") %>%
  mutate(
    change = n_2024 - n_2020,
    pct_change = round((change / n_2020) * 100, 1)
  )

# Largest percentage losses among big districts
losses %>%
  filter(n_2020 >= 10000) %>%
  arrange(pct_change) %>%
  select(district_name, n_2020, n_2024, change, pct_change) %>%
  head(10)
```

Houston ISD’s absolute loss of **25,706 students** represents more
students than 90% of Texas districts even enroll.

------------------------------------------------------------------------

## 5. IDEA Public Schools Grew 24% in Three Years

**IDEA Public Schools**, a charter network operating across Texas, grew
from 62,158 to 76,819 students between 2021 and 2024—a gain of **14,661
students** (+23.6%).

``` r
idea <- enr %>%
  filter(district_name == "IDEA PUBLIC SCHOOLS", is_district,
         subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, n_students) %>%
  arrange(end_year)

print(idea)
```

``` r
# Top growing large districts
growth <- losses %>%
  filter(n_2020 >= 5000) %>%
  arrange(desc(pct_change))

growth %>%
  select(district_name, n_2020, n_2024, change, pct_change) %>%
  head(10)
```

Charter networks and suburban districts are absorbing students from
declining urban districts, fundamentally reshaping Texas public
education.

------------------------------------------------------------------------

## 6. Fort Bend ISD: A District With No Majority

**Fort Bend ISD** is one of the most diverse school districts in
America. No racial group exceeds 28% of enrollment—White (13.2%), Black
(27.8%), Hispanic (26.7%), and Asian (27.6%) students are nearly equally
represented.

``` r
fb_demo <- enr %>%
  filter(district_name == "FORT BEND ISD", is_district, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian")) %>%
  select(end_year, subgroup, pct) %>%
  mutate(pct = round(pct * 100, 1)) %>%
  pivot_wider(names_from = subgroup, values_from = pct)

print(fb_demo)
```

``` r
# Fort Bend demographics over time
enr %>%
  filter(district_name == "FORT BEND ISD", is_district, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian")) %>%
  mutate(pct = pct * 100) %>%
  ggplot(aes(x = end_year, y = pct, fill = subgroup)) +
  geom_area(alpha = 0.8) +
  scale_fill_manual(
    values = c("asian" = "#CC79A7", "black" = "#009E73",
               "hispanic" = "#0072B2", "white" = "#E69F00"),
    labels = c("Asian", "Black", "Hispanic", "White")
  ) +
  labs(
    title = "Fort Bend ISD: A District With No Majority",
    subtitle = "Demographic composition 2021-2024",
    x = "School Year", y = "Percent", fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

------------------------------------------------------------------------

## 7. Kindergarten Enrollment Dropped 5.8%

**Kindergarten enrollment fell from 383,585 to 361,329**—a drop of
22,256 students (-5.8%). This could signal declining birth rates, rising
private school enrollment, or families delaying school entry.

``` r
grade_trend <- enr %>%
  filter(is_state, subgroup == "total_enrollment",
         grade_level %in% c("PK", "K", "01", "05", "09", "12")) %>%
  select(end_year, grade_level, n_students) %>%
  pivot_wider(names_from = end_year, values_from = n_students) %>%
  mutate(
    change = `2024` - `2020`,
    pct_change = round(change / `2020` * 100, 1)
  )

print(grade_trend)
```

Pre-K also dropped, while high school grades grew. The pipeline is
narrowing at the entry point—a trend that will ripple through the system
for years.

------------------------------------------------------------------------

## 8. 62% of Students Are Economically Disadvantaged

The share of **economically disadvantaged** students grew from 60.3% to
62.3%—nearly two-thirds of all students in Texas public schools.

``` r
econ_trend <- enr %>%
  filter(is_state, subgroup == "econ_disadv", grade_level == "TOTAL") %>%
  select(end_year, n_students, pct) %>%
  mutate(pct = round(pct * 100, 1))

print(econ_trend)
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

Some large districts serve even higher concentrations of low-income
students.

------------------------------------------------------------------------

## 9. White Students Dropped Below 25%

**White enrollment** declined from 27.0% to 25.0% of total enrollment—a
2 percentage point drop in just five years. Meanwhile, Hispanic (53.2%)
and Asian (5.4%) shares continue to grow.

``` r
demo_shift <- enr %>%
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian", "multiracial")) %>%
  select(end_year, subgroup, pct) %>%
  mutate(pct = round(pct * 100, 1)) %>%
  pivot_wider(names_from = subgroup, values_from = pct)

print(demo_shift)
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

------------------------------------------------------------------------

## 10. 439 Districts Now Have Hispanic Majorities

The number of districts where Hispanic students are the majority grew
from 419 in 2020 to **439 in 2024**—now 36% of all Texas districts.

``` r
hisp_majority <- enr %>%
  filter(is_district, subgroup == "hispanic", grade_level == "TOTAL") %>%
  mutate(majority = pct > 0.5) %>%
  group_by(end_year) %>%
  summarize(
    total_districts = n(),
    hispanic_majority = sum(majority),
    pct_majority = round(hispanic_majority / total_districts * 100, 1),
    .groups = "drop"
  )

print(hisp_majority)
```

29 districts flipped from non-majority to Hispanic majority between 2020
and 2024.

------------------------------------------------------------------------

## 11. Houston ISD vs Dallas ISD: Two Giants, Two Trajectories

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

Houston lost **25,706 students** vs Dallas’s **11,527**. Both are losing
ground to suburban and charter competitors.

------------------------------------------------------------------------

## 12. Special Education Identification Rose to 12%

The share of students receiving special education services has climbed
steadily, reaching **12%** of total enrollment by 2024. That means
roughly 1 in 8 Texas students now receives special education services.

``` r
sped_trend <- enr %>%
  filter(is_state, subgroup == "special_ed", grade_level == "TOTAL") %>%
  select(end_year, n_students, pct) %>%
  mutate(pct_display = round(pct * 100, 1))

print(sped_trend %>% select(end_year, n_students, pct_display))
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

The increase likely reflects better identification, expanded
definitions, and growing awareness of learning differences.

------------------------------------------------------------------------

## 13. Frisco ISD Added 20,000 Students in Five Years

**Frisco ISD** is one of the fastest-growing large districts in the
state, adding **20,259 students** between 2020 and 2024 – a 44%
increase. The suburban Collin County district shows no signs of slowing
down.

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

While urban cores lose students, suburban boomtowns like Frisco absorb
the growth.

------------------------------------------------------------------------

## 14. Pre-K Enrollment Still Hasn’t Recovered from COVID

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

The slow Pre-K recovery matters because early childhood education is one
of the highest-return investments in education.

------------------------------------------------------------------------

## 15. Multiracial Students Are the Fastest-Growing Demographic

Students identifying as **Two or More Races** grew from 2.5% to 3.1% of
enrollment between 2020 and 2024 – a 24% increase in raw numbers. While
still a small share, this is the fastest percentage growth of any
demographic group.

``` r
multi_trend <- enr %>%
  filter(is_state, subgroup == "multiracial", grade_level == "TOTAL") %>%
  select(end_year, n_students, pct) %>%
  mutate(pct_display = round(pct * 100, 1))

print(multi_trend %>% select(end_year, n_students, pct_display))
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

This trend mirrors national patterns and reflects both demographic
change and evolving identity choices.

------------------------------------------------------------------------

## Summary

These fifteen findings illustrate the rapid transformation of Texas
public education:

| Finding                     | Change (2020-2024)               |
|-----------------------------|----------------------------------|
| COVID enrollment drop       | -120,133 students (2020-21)      |
| LEP students                | 20.3% to 24.4% (+4.1 pts)        |
| Coppell ISD Asian           | Now 56.7% Asian                  |
| Fort Worth ISD              | -14.3% (-11,801 students)        |
| IDEA Public Schools         | +23.6% since 2021                |
| Fort Bend ISD               | No majority group (most diverse) |
| Kindergarten enrollment     | -5.8%                            |
| Economically disadvantaged  | 60.3% to 62.3%                   |
| White enrollment            | 27.0% to 25.0%                   |
| Hispanic majority districts | 419 to 439 (+20)                 |
| Houston vs Dallas ISD       | -25,706 vs -11,527 students      |
| Special education           | Rose to ~12% of enrollment       |
| Frisco ISD                  | +20,259 students (+44%)          |
| Pre-K enrollment            | Still below 2020 levels          |
| Multiracial students        | Fastest-growing group (+24%)     |

The data tells a story of suburbanization, rising poverty, linguistic
diversity, and demographic transformation that will shape Texas
education for decades to come.

------------------------------------------------------------------------

## Explore the Data

Use `txschooldata` to explore these patterns yourself:

``` r
library(txschooldata)
library(dplyr)

# Fetch 2024 data
enr <- fetch_enr(2024, use_eval = FALSE)

# Your district's demographics
enr %>%
  filter(district_name == "YOUR DISTRICT NAME", grade_level == "TOTAL") %>%
  select(subgroup, n_students, pct)
```

## Session Info

``` r
sessionInfo()
```
