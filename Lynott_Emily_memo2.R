## load packages ----
library(tidyverse)

## tidying ----
alcohol_gho_tidy <- alcohol_gho |>
  pivot_wider(
    names_from = "dimension", 
    values_from = "subgroup") |>
  mutate(
    indicator_name = factor(indicator_name), 
    indicator_name = fct_recode(indicator_name, 
      "liters (pure alcohol) consumption per capita; 15+; drinkers only; NA" = "Alcohol, drinkers only per capita (15+) consumption (in litres of pure alcohol)",
      "% abstainers; NA; population; lifetime" = "Alcohol, abstainers lifetime (%)",
      "% abstainers; NA; population; past 12 mo" = "Alcohol, abstainers past 12 months (%)",
      "% consumers; NA; population; past 12 mo" = "Alcohol, consumers past 12 months (%)",
      "% former drinkers; NA; population; NA" = "Alcohol, former drinkers (%)", 
      "% heavy episodic drinkers; 15+; drinkers only; past 30 days" = "Alcohol, heavy episodic drinking (15+), drinkers only, past 30 days (%)", 
      "% dependence; 15+; population; 12 mo prevalence" = "Alcohol dependence (15+), 12 month prevalence (%)",
      "% use diorders; 15+; population; 12 mo prevalence" = "Alcohol use disorders (15+), 12 month prevalence (%)",
      "liters (pure alcohol) consumption per capita; 15+; population; NA" = "Alcohol, total per capita (15+) consumption (SDG Indicator 3.5.2) (in litres of pure alcohol)", 
      "% heavy episodic drinkers; 15+; population, age standardized; past 30 days" = "Alcohol, heavy episodic drinking (15+) past 30 days (age-standardized) (%)", 
      "% alcohol-attributable fractions: liver cirrhosis deaths; 15+; population; NA" = "Alcohol-attributable fractions (15+), liver cirrhosis deaths (%)",
      "% alcohol-attributable fractions: road traffic crash deaths; 15+; population; NA" = "Alcohol-attributable fractions (15+), road traffic crash deaths (%)", 
      "% alcohol-attributable fractions: all-cause deaths; NA; population; NA" = "Alcohol-attributable fractions, all-cause deaths (%)", 
      "grams average daily intake; NA; drinkers only; NA" = "Alcohol, average daily intake among drinkers (grams)", 
      "% harmful use; 15+; population; 12 mo prevalence" = "Alcohol, harmful use (15+), 12 month prevalence (%)", 
      "% heavy episodic drinkers; 15-19; population; NA" = "15-19 years old heavy episodic drinkers (population) (%)", 
      "% heavy episodic drinkers; 15-19; drinkers only; NA" = "15-19 years old heavy episodic drinkers (drinkers only) (%)", 
      "% current drinkers; 15-19; population; NA" = "15-19 years old, current drinkers (%)", 
      "% any alcoholic beverage consumed; 15-19; population; past 12 mo" = "15-years old any alcoholic beverage consumed in past 12 months (%)", 
      "% any alcoholic beverage consumed; 13-15; population; past 30 days" = "13-15-years old any alcoholic beverage consumed in past 30 days (%)", 
      "% first drink before 14; 13-15; population; NA" = "13-15-years old first drink before age 14 (%)", 
      "% heavy episodic drinking; 15-19; population; past 30 days" = "Heavy episodic drinking (youth 15 -19 years) past 30 days (%)", 
      "% heavy episodic drinking; 15-19; drinkers only; past 30 days" = "Heavy episodic drinking (youth 15 -19 years), drinkers only, past 30 days (%)"
    ) 
  ) |>
  separate_wider_delim(
    indicator_name, 
    delim = ";", 
    names = c("indicator_behavior", "indicator_age", "indicator_population", "indicator_time")
  )
  

# names <- alcohol_gho_tidy |>
#   distinct(indicator_name)