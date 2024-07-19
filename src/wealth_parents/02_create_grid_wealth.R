# Computing expectations over entire grid of options
library(tidyverse)
library(feather)
library(readxl)
library(haven)
library(lubridate)

# Resource locations ----
pwr_location <- "K:/Utilities/HULPbestanden/PWR/PWR2023.sav"
gwb_location <- "K:/Utilities/HULPbestanden/PC6cryptgwb/PC6gwb2023V1.sav"
corop_location <- "K:/Utilities/HULPbestanden/GebiedeninNederland/Gemeenten en COROP vanaf 1981.xlsx"
# Pay attention: CBS randomly decides to move / remove these things :(

# Creating the grid ----
# first, for each outcome determine which dataset to draw from
# these datasets will be loaded in memory during model fitting
outcome_source <- tribble(
  ~outcome,                         ~binary,    ~data_source,
  "c30_income",                     FALSE,      "main_cohort",
  "c30_income_perc",                FALSE,      "main_cohort",
  "c30_hbo_attained",               TRUE,       "main_cohort",
  "c30_wo_attained",                TRUE,       "main_cohort",
  "c30_hourly_wage",                FALSE,      "main_cohort",
  "c30_hrs_work_pw",                FALSE,      "main_cohort",
  "c30_permanent_contract",         TRUE,       "main_cohort",
  "c30_employed",                   TRUE,       "main_cohort",
  "c30_social_assistance",          TRUE,       "main_cohort",
  "c30_disability",                 TRUE,       "main_cohort",
  "c30_pharma",                     TRUE,       "main_cohort",
  "c30_basic_mhc",                  TRUE,       "main_cohort",
  "c30_specialist_mhc",             TRUE,       "main_cohort",
  "c30_hospital",                   TRUE,       "main_cohort",
  "c30_total_health_costs",         FALSE,      "main_cohort",
  "c30_hourly_wage_max_11",         TRUE,       "main_cohort",
  "c30_hourly_wage_max_14",         TRUE,       "main_cohort",
  "c30_debt",                       FALSE,      "main_cohort",
  "c30_homeowner",                  TRUE,       "main_cohort",
  "c30_wealth",                     FALSE,      "main_cohort",
  "c30_wealth_no_home",             FALSE,      "main_cohort",
  "c30_home_wealth",                FALSE,      "main_cohort",
  "c30_gifts_received",             TRUE,       "main_cohort",
  "c30_sum_gifts",                  FALSE,      "main_cohort",
  "c30_young_mothers",              TRUE,       "main_cohort",
  "c30_household_income",           FALSE,      "main_cohort",
  "c30_household_income_perc",      FALSE,      "main_cohort",
  "c30_living_space_pp",            FALSE,      "main_cohort",
  "c30_age_left_parents",           FALSE,      "main_cohort",
  
  "c21_high_school_attained",       TRUE,       "students_cohort",
  "c21_hbo_followed",               TRUE,       "students_cohort",
  "c21_uni_followed",               TRUE,       "students_cohort",
  "c21_living_with_parents",        TRUE,       "students_cohort",
  "c21_young_parents",              TRUE,       "students_cohort",
  
  "c16_vmbo_gl",                    TRUE,       "high_school_cohort",
  "c16_havo",                       TRUE,       "high_school_cohort",
  "c16_vwo",                        TRUE,       "high_school_cohort",
  "c16_youth_protection",           TRUE,       "high_school_cohort",
  "c16_youth_health_costs",         FALSE,      "high_school_cohort",
  "c16_living_space_pp",            FALSE,      "high_school_cohort",
  
  "c11_math",                       TRUE,       "elementary_school_cohort",
  "c11_reading",                    TRUE,       "elementary_school_cohort",
  "c11_language",                   TRUE,       "elementary_school_cohort",
  "c11_vmbo_gl_test",               TRUE,       "elementary_school_cohort",
  "c11_havo_test",                  TRUE,       "elementary_school_cohort",
  "c11_vwo_test",                   TRUE,       "elementary_school_cohort",
  "c11_vmbo_gl_final",              TRUE,       "elementary_school_cohort",
  "c11_havo_final",                 TRUE,       "elementary_school_cohort",
  "c11_vwo_final",                  TRUE,       "elementary_school_cohort",
  "c11_under_advice",               TRUE,       "elementary_school_cohort",
  "c11_over_advice",                TRUE,       "elementary_school_cohort",
  "c11_youth_protection",           TRUE,       "elementary_school_cohort",
  "c11_youth_health_costs",         FALSE,      "elementary_school_cohort",
  "c11_living_space_pp",            FALSE,      "elementary_school_cohort",
  
  "c11_class_vmbo_gl_test",         TRUE,       "classroom_cohort",
  "c11_class_havo_test",            TRUE,       "classroom_cohort",
  "c11_class_vwo_test",             TRUE,       "classroom_cohort",
  "c11_class_size",                 FALSE,      "classroom_cohort",
  "c11_class_foreign_born_parents", TRUE,       "classroom_cohort",
  "c11_class_math",                 TRUE,       "classroom_cohort",
  "c11_class_language",             TRUE,       "classroom_cohort",
  "c11_class_reading",              TRUE,       "classroom_cohort",
  "c11_class_income_below_25th",    TRUE,       "classroom_cohort",
  "c11_class_income_above_75th",    TRUE,       "classroom_cohort",
  
  "c00_sga",                        TRUE,       "perinatal_cohort",
  "c00_preterm_birth",              TRUE,       "perinatal_cohort",
  
  "c00_perinatal_mortality",        TRUE,       "child_mortality_cohort",
  "c00_neonatal_mortality",         TRUE,       "child_mortality_cohort",
  "c00_infant_mortality",           TRUE,       "child_mortality_cohort"
)


# Predictors
wealth_groups    <- c("all", "High", "Mid", "Low")
gender_groups    <- c("all", "Mannen", "Vrouwen")
migration_groups <- c("all", "Nederland", "Turkije", "Marokko", "Suriname", 
                      "Nederlandse Antillen (oud)", "Overig")
household_groups <- c("all", "single parent", "two parents")

# Region types
pc4_tab <- read_spss(pwr_location) %>% select(postc)
gwb_tab <- read_spss(gwb_location) %>% select(-postcode_crypt) %>% distinct()
corop_tab <- read_xlsx(corop_location)


# convert to gemeente-indeling 2023
gwb_tab <- 
  gwb_tab %>% 
  mutate(gemcode = 
           ifelse(gemcode %in% c("0501", "0530", "0614"), 
                  "1992", gemcode), 
         gemcode = ifelse(gemcode == '0457', "0363", gemcode))



regions <- tribble(
  ~region_type,    ~region_id, 
  "all",           "all",
  "corop_code",    unique(as.character(corop_tab$COROP2022)),
  "postcode3",     unique(substr(pc4_tab$postc, 1, 3)),
  "postcode4",     unique(substr(pc4_tab$postc, 1, 4)),
  "gemeente_code", unique(as.character(gwb_tab$gemcode)),
  "wijk_code",     unique(as.character(gwb_tab$wijkcode))
)

# create the basic grid
model_grid <- 
  expand_grid(
    outcome         = outcome_source$outcome, 
    region_type     = regions$region_type, 
    wealth_group    = wealth_groups, 
    gender_group    = gender_groups, 
    migration_group = migration_groups,
    household_group = household_groups
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
  select(outcome, binary, data_source, wealth_group, gender_group, household_group,
         migration_group, region_type, region_id)

# replace region_type for youth protection 
model_grid <-
  model_grid %>%
  mutate(region_type = as.character(region_type),
         region_type = ifelse(outcome %in% c('c11_youth_protection', 'c16_youth_protection') & region_type != 'all', 
                              paste0(region_type, '_birth'), region_type)) %>%
  mutate(region_type = as_factor(region_type))


# remove corop-level for younger cohorts other than main cohort
model_grid <-
  model_grid %>%
  filter(!(region_type == 'corop_code' & data_source != 'main_cohort')) %>%
  filter(region_type != 'corop_code_birth')


# Write the entire model grid as a feather file (can be read by row or by column)
write_feather(model_grid, "input/wealth/model_grid.feather")



