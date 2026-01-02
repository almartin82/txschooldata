## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.width = 8,
  fig.height = 5
)

## ----load-packages------------------------------------------------------------
library(txschooldata)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

## ----covid-drop---------------------------------------------------------------
state_trend <- enr %>%
  filter(is_state, subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, n_students) %>%
  arrange(end_year) %>%
  mutate(
    change = n_students - lag(n_students),
    pct_change = round(change / lag(n_students) * 100, 2)
  )

print(state_trend)

## ----covid-plot---------------------------------------------------------------
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

## ----lep-growth---------------------------------------------------------------
lep_trend <- enr %>%
  filter(is_state, subgroup == "lep", grade_level == "TOTAL") %>%
  select(end_year, n_students, pct) %>%
  mutate(pct_display = round(pct * 100, 1))

print(lep_trend %>% select(end_year, n_students, pct_display))

## ----lep-plot-----------------------------------------------------------------
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

## ----asian-districts----------------------------------------------------------
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

## ----frisco-trend-------------------------------------------------------------
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

## ----urban-decline------------------------------------------------------------
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

## ----idea-growth--------------------------------------------------------------
idea <- enr %>%
  filter(district_name == "IDEA PUBLIC SCHOOLS", is_district,
         subgroup == "total_enrollment", grade_level == "TOTAL") %>%
  select(end_year, n_students) %>%
  arrange(end_year)

print(idea)

## ----charter-networks---------------------------------------------------------
# Top growing large districts
growth <- losses %>%
  filter(n_2020 >= 5000) %>%
  arrange(desc(pct_change))

growth %>%
  select(district_name, n_2020, n_2024, change, pct_change) %>%
  head(10)

## ----fort-bend----------------------------------------------------------------
fb_demo <- enr %>%
  filter(district_name == "FORT BEND ISD", is_district, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian")) %>%
  select(end_year, subgroup, pct) %>%
  mutate(pct = round(pct * 100, 1)) %>%
  pivot_wider(names_from = subgroup, values_from = pct)

print(fb_demo)

## ----diversity-chart----------------------------------------------------------
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

## ----grade-shifts-------------------------------------------------------------
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

## ----econ-disadv--------------------------------------------------------------
econ_trend <- enr %>%
  filter(is_state, subgroup == "econ_disadv", grade_level == "TOTAL") %>%
  select(end_year, n_students, pct) %>%
  mutate(pct = round(pct * 100, 1))

print(econ_trend)

## ----econ-plot----------------------------------------------------------------
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

## ----demographic-shift--------------------------------------------------------
demo_shift <- enr %>%
  filter(is_state, grade_level == "TOTAL",
         subgroup %in% c("white", "black", "hispanic", "asian", "multiracial")) %>%
  select(end_year, subgroup, pct) %>%
  mutate(pct = round(pct * 100, 1)) %>%
  pivot_wider(names_from = subgroup, values_from = pct)

print(demo_shift)

## ----demo-plot----------------------------------------------------------------
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

## ----hispanic-majority--------------------------------------------------------
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

## ----explore-example, eval=FALSE----------------------------------------------
# library(txschooldata)
# library(dplyr)
# 
# # Fetch 2024 data
# enr <- fetch_enr(2024)
# 
# # Your district's demographics
# enr %>%
#   filter(district_name == "YOUR DISTRICT NAME", grade_level == "TOTAL") %>%
#   select(subgroup, n_students, pct)

