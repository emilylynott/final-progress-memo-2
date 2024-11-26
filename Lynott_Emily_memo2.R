## load packages ----
library(tidyverse)
alcohol_gho <- readxl::read_xlsx("data/data.xlsx")


## tidying ----
alcohol_gho_tidy <- alcohol_gho |>
  pivot_wider(
    names_from = "dimension", 
    values_from = "subgroup") |>
  mutate(
    indicator_name = factor(indicator_name), 
    indicator_name = fct_recode(indicator_name, 
      "liters (pure alcohol) consumption per capita; 15+; drinkers only" = "Alcohol, drinkers only per capita (15+) consumption (in litres of pure alcohol)",
      "% abstainers, lifetime; NA; population" = "Alcohol, abstainers lifetime (%)",
      "% abstainers, past 12 mo; NA; population" = "Alcohol, abstainers past 12 months (%)",
      "% consumers, past 12 mo; NA; population" = "Alcohol, consumers past 12 months (%)",
      "% former drinkers; NA; population" = "Alcohol, former drinkers (%)", 
      "% heavy episodic drinkers, past 30 days; 15+; drinkers only" = "Alcohol, heavy episodic drinking (15+), drinkers only, past 30 days (%)", 
      "% dependence, 12 mo prevalence; 15+; population" = "Alcohol dependence (15+), 12 month prevalence (%)",
      "% use diorders, 12 mo prevalence; 15+; population" = "Alcohol use disorders (15+), 12 month prevalence (%)",
      "liters (pure alcohol) consumption per capita; 15+; population" = "Alcohol, total per capita (15+) consumption (SDG Indicator 3.5.2) (in litres of pure alcohol)", 
      "% heavy episodic drinkers, past 30 days; 15+; population, age standardized" = "Alcohol, heavy episodic drinking (15+) past 30 days (age-standardized) (%)", 
      "% alcohol-attributable fractions: liver cirrhosis deaths; 15+; population" = "Alcohol-attributable fractions (15+), liver cirrhosis deaths (%)",
      "% alcohol-attributable fractions: road traffic crash deaths; 15+; population" = "Alcohol-attributable fractions (15+), road traffic crash deaths (%)", 
      "% alcohol-attributable fractions: all-cause deaths; NA; population" = "Alcohol-attributable fractions, all-cause deaths (%)", 
      "grams average daily intake; NA; drinkers only" = "Alcohol, average daily intake among drinkers (grams)", 
      "% harmful use, 12 mo prevalence; 15+; population" = "Alcohol, harmful use (15+), 12 month prevalence (%)", 
      "% heavy episodic drinkers; 15-19; population" = "15-19 years old heavy episodic drinkers (population) (%)", 
      "% heavy episodic drinkers; 15-19; drinkers only" = "15-19 years old heavy episodic drinkers (drinkers only) (%)", 
      "% current drinkers; 15-19; population" = "15-19 years old, current drinkers (%)", 
      "% any alcoholic beverage consumed, past 12 mo; 15-19; population" = "15-years old any alcoholic beverage consumed in past 12 months (%)", 
      "% any alcoholic beverage consumed, past 30 days; 13-15; population" = "13-15-years old any alcoholic beverage consumed in past 30 days (%)", 
      "% first drink before 14; 13-15; population" = "13-15-years old first drink before age 14 (%)", 
      "% heavy episodic drinking, past 30 days; 15-19; population" = "Heavy episodic drinking (youth 15 -19 years) past 30 days (%)", 
      "% heavy episodic drinking, past 30 days; 15-19; drinkers only" = "Heavy episodic drinking (youth 15 -19 years), drinkers only, past 30 days (%)"
    ) 
  ) |>
  separate_wider_delim(
    indicator_name, 
    delim = ";", 
    names = c("indicator_behavior", "indicator_age", "indicator_population")
  ) |>
  mutate(
    indicator_behavior = factor(indicator_behavior)
  )
  
  # names <- alcohol_gho_tidy |>
#   distinct(indicator_name)

tidy_general<- alcohol_gho_tidy |>
  filter(
    indicator_behavior %in% c(
      "% abstainers, lifetime", 
      "% abstainers, past 12 mo", 
      "% consumers, past 12 mo", 
      "% former drinkers"
    )
  )

tidy_general |>
  select(indicator_behavior, estimate) |>
  group_by(indicator_behavior) |>
  ggplot(aes(x = indicator_behavior, y = estimate)) +
  geom_boxplot()

tidy_general |>
  select(indicator_behavior, estimate) |>
  group_by(indicator_behavior) |>
  summarize(
    median_estimate = median(estimate, na.rm = TRUE), 
    IQR_estimate = IQR(estimate, na.rm = TRUE),
    min_estimate = min(estimate, na.rm = TRUE),
    max_estimate = max(estimate, na.rm = TRUE)
  ) |>
  knitr::kable()

## income analysis

gen_income <- tidy_general |>
  select(indicator_behavior, wbincome2024, estimate) |>
  drop_na(wbincome2024) |>
  group_by(indicator_behavior, wbincome2024) |>
  summarize(avg_pct = mean(estimate, na.rm = TRUE), .groups = "drop") |>
gen_income |>  
  ggplot(aes(x = indicator_behavior, y = avg_pct)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ wbincome2024, ncol = 2) +
  coord_flip()

gen_income |>
  pivot_wider(
    names_from = "indicator_behavior", 
    values_from = "avg_pct"
  ) |>
  knitr::kable(caption = "Table 1: Average % of population engaging in indicator behavior by income")

## regional analysis ----

gen_regions <- tidy_general |>
  select(indicator_behavior, whoreg6, estimate) |>
  group_by(indicator_behavior, whoreg6) |>
  summarize(avg_pct = mean(estimate, na.rm = TRUE), .groups = "drop") 

gen_regions |>
  ggplot(aes(x = indicator_behavior, y = avg_pct)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~ whoreg6, ncol = 2) +
  coord_flip()

gen_regions |>
  pivot_wider(
    names_from = "indicator_behavior", 
    values_from = "avg_pct"
  ) |>
  knitr::kable()

## european countries

tidy_general |>
  select(
    indicator_behavior, 
    whoreg6, 
    setting, 
    setting_average
  ) |>
  filter(whoreg6 == "European") |>
  summarize(
    setting_average = mean(setting_average), 
    .by = c(indicator_behavior, setting)
  ) |>
  pivot_wider(
    names_from = "indicator_behavior", 
    values_from = "setting_average"
  ) |>
  arrange(desc(!!sym("% consumers, past 12 mo"))) |>
  slice_head(n = 10) |>
  knitr::kable()
