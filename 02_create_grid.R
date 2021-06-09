# Computing expectations over entire grid of options
library(tidyverse)
library(feather)
library(readxl)

# Resource locations ----
vslgwb_location <- "../cbs_validationdata/cbsdata/BouwenWonen/VSLGWBTAB/VSLGWB2019TAB03V1.sav"
vslpostcode_location <- "../cbs_validationdata/cbsdata/BouwenWonen/VSLPOSTCODEBUS/VSLPOSTCODEBUSV2020031.sav"

# Creating the grid ----
# first, for each outcome determine which dataset to draw from
# these datasets will be loaded in memory during model fitting
outcome_source <- tribble(
  ~outcome,                   ~binary, ~data_source,
  "income",                   FALSE,   "main_cohort",
  "income_perc",              FALSE,   "main_cohort",
  "hbo_attained",             TRUE,    "main_cohort",
  "wo_attained",              TRUE,    "main_cohort",
  "hourly_income",            FALSE,   "main_cohort",
  "hourly_income_perc",       FALSE,   "main_cohort",
  "hours_per_week",           FALSE,   "main_cohort",
  "longest_contract_flex",    TRUE,    "main_cohort",
  "has_worked",               TRUE,    "main_cohort",
  "hours_per_week_NA0",       FALSE,   "main_cohort",
  "employed",                 TRUE,    "main_cohort",
  "social_benefits",          TRUE,    "main_cohort",
  "disability",               TRUE,    "main_cohort",
  "pharma_costs",             TRUE,    "main_cohort",
  "basis_ggz_costs",          TRUE,    "main_cohort",
  "specialist_costs",         TRUE,    "main_cohort",
  "hospital_costs",           TRUE,    "main_cohort",
  "total_health_costs",       FALSE,   "main_cohort",
  "child_total_health_costs", FALSE,   "youth_protection_cohort",
  "youth_protection",         TRUE,    "youth_protection_cohort",
  "low_birthweight",          TRUE,    "perinatal_cohort",
  "premature_birth",          TRUE,    "perinatal_cohort",
  "vmbo_hoog_plus",           TRUE,    "high_school_cohort",
  "havo_plus",                TRUE,    "high_school_cohort",
  "vwo_plus",                 TRUE,    "high_school_cohort",
  "wpo_math",                 TRUE,    "elementary_school_cohort",
  "wpo_reading",              TRUE,    "elementary_school_cohort",
  "wpo_language",             TRUE,    "elementary_school_cohort",
  "vmbo_hoog_plus_test",      TRUE,    "elementary_school_cohort",
  "havo.plus.test",           TRUE,    "elementary_school_cohort",
  "vwo.plus.test",            TRUE,    "elementary_school_cohort",
  "vmbo_hoog_plus_final",     TRUE,    "elementary_school_cohort",
  "havo_plus_final",          TRUE,    "elementary_school_cohort",
  "vwo_plus_final",           TRUE,    "elementary_school_cohort",
  "under_advice",             TRUE,    "elementary_school_cohort",
  "over_advice",              TRUE,    "elementary_school_cohort"
)


# Predictors
income_groups    <- c("all", "high", "mid", "low")
gender_groups    <- c("all", "male", "female")
migration_groups <- c("all", "native", "turkey", "morocco", "surinam", "antilles", "otherswestern", "othersnonwestern", 
                      "totalnonwestern")

# Region types
# TODO: postcode_sf uit 2019
gwb_tab <- read_spss(vslgwb_location)
pc4_tab <- read_spss(vslpostcode_location)

regions <- tribble(
  ~region_type,    ~region_id, 
  "postcode3",     unique(substr(pc4_tab$POSTCODENUM, 1, 3)),
  "postcode4",     unique(pc4_tab$POSTCODENUM),
  "gemeente_code", unique(gwb_tab$Gem2019),
  "wijk_code",     unique(gwb_tab$WC2019),
  "buurt_code",    unique(gwb_tab$BC2019),
  "corop_code",    unique(as.character(corop_table$COROP2019))
)

# clear memory
rm(postcode_sf, gemeente_sf, wijk_sf, buurt_sf, corop_table)

# create the basic grid
model_grid <- 
  expand_grid(
    outcome         = outcome_source$outcome, 
    region_type     = regions$region_type, 
    income_group    = income_groups, 
    gender_group    = gender_groups, 
    migration_group = migration_groups
  ) %>% 
  left_join(outcome_source, by = "outcome") %>% 
  mutate(across(everything(), as_factor))

# add the region ids and unnest (expand) into long format
# NB: this step takes 1-3 minutes, depending on the hardware
model_grid <-
  model_grid %>% 
  left_join(regions, by = "region_type") %>% 
  unnest_longer(region_id) %>%
  mutate(region_id = as_factor(region_id), region_type = as_factor(region_type)) %>% 
  select(outcome, data_source, income_group, gender_group, migration_group, region_type, region_id)

# Write the entire model grid as a feather file (can be read by row or by column)
write_feather(model_grid, "data/model_grid.feather")

