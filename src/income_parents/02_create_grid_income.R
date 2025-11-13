# Computing expectations over entire grid of options
library(tidyverse)
library(feather)
library(readxl)
library(haven)
library(lubridate)

# Resource locations ----
pwr_location <- "K:/Utilities/HULPbestanden/PWR/PWR2024.sav"
gwb_location <- "K:/Utilities/HULPbestanden/PC6cryptgwb/PC6gwb2024V1.sav"
corop_location <- "K:/Utilities/HULPbestanden/GebiedeninNederland/Gemeenten en COROP vanaf 1981.xlsx"
# Pay attention: CBS randomly decides to move / remove these things :(

# Creating the grid ----
# first, for each outcome determine which dataset to draw from
# these datasets will be loaded in memory during model fitting
outcome_source <- tribble(
  ~outcome,                         ~binary,    ~data_source,
  "c35_hbo_attained",               TRUE,       "age35_cohort",
  "c35_wo_attained",                TRUE,       "age35_cohort",
  "c35_pharma",                     TRUE,       "age35_cohort",
  "c35_basic_mhc",                  TRUE,       "age35_cohort",
  "c35_specialist_mhc",             TRUE,       "age35_cohort",
  "c35_mhc",                        TRUE,       "age35_cohort",
  "c35_hospital",                   TRUE,       "age35_cohort",
  "c35_total_health_costs",         FALSE,      "age35_cohort",
  
  "c35_individual_income",          FALSE,      "age35_cohort",
  "c35_individual_income_perc",     FALSE,      "age35_cohort",
  "c35_household_income",           FALSE,      "age35_cohort",
  "c35_household_income_perc",      FALSE,      "age35_cohort",
  "c35_household_below_poverty",    FALSE,      "age35_cohort",

  "c35_disposable_household_income",      FALSE, "age35_cohort",
  "c35_disposable_household_income_perc", FALSE, "age35_cohort",
  "c35_primary_household_income",         FALSE, "age35_cohort",
  "c35_primary_household_income_perc",    FALSE, "age35_cohort",
  "c35_individual_earnings",              FALSE, "age35_cohort",
  "c35_individual_earnings_perc",         FALSE, "age35_cohort",
  "top_20_household_income",              FALSE, "age35_cohort",
  "bottom_20_household_income",           FALSE, "age35_cohort",
  "top_20_income",                        FALSE, "age35_cohort",
  "bottom_20_income",                     FALSE, "age35_cohort",
 
  "c35_employed",                   TRUE,       "age35_cohort",
  "c35_social_assistance",          TRUE,       "age35_cohort",
  "c35_disability",                 TRUE,       "age35_cohort",
  "c35_hourly_wage",                FALSE,      "age35_cohort",
  "c35_hrs_work_pw",                FALSE,      "age35_cohort",
  "c35_permanent_contract",         TRUE,       "age35_cohort",
  "c35_hourly_wage_max_16",         TRUE,       "age35_cohort",
  
  "c35_debt",                       FALSE,      "age35_cohort",
  "c35_wealth",                     FALSE,      "age35_cohort",
  "c35_wealth_no_home",             FALSE,      "age35_cohort",
  "c35_home_wealth",                FALSE,      "age35_cohort",
  "c35_gifts_received",             TRUE,       "age35_cohort",
  "c35_sum_gifts",                  FALSE,      "age35_cohort",
  "c35_living_space_pp",            FALSE,      "age35_cohort",
  "c35_homeowner",                  TRUE,       "age35_cohort",
  "c35_age_left_parents",           FALSE,      "age35_cohort",
  
  "c21_high_school_attained",       TRUE,       "age21_cohort",
  "c21_hbo_followed",               TRUE,       "age21_cohort",
  "c21_uni_followed",               TRUE,       "age21_cohort",
  "c21_living_with_parents",        TRUE,       "age21_cohort",
  "c21_young_parents",              TRUE,       "age21_cohort",
  "c21_pharma",                     TRUE,       "age21_cohort",
  "c21_basic_mhc",                  TRUE,       "age21_cohort",
  "c21_specialist_mhc",             TRUE,       "age21_cohort",
  "c21_mhc",                        TRUE,       "age21_cohort",
  "c21_hospital",                   TRUE,       "age21_cohort",
  "c21_total_health_costs",         FALSE,      "age21_cohort",
  
  "c16_vmbo_gl",                    TRUE,       "age16_cohort",
  "c16_havo",                       TRUE,       "age16_cohort",
  "c16_vwo",                        TRUE,       "age16_cohort",
  "c16_youth_protection",           TRUE,       "age16_cohort",
  "c16_youth_health_costs",         FALSE,      "age16_cohort",
  "c16_living_space_pp",            FALSE,      "age16_cohort",
  
  "c16_secondary_class_foreign_born_parents", FALSE, "age16_cohort",
  "c16_secondary_class_income_below_25th",    FALSE, "age16_cohort",
  "c16_secondary_class_income_below_50th",    FALSE, "age16_cohort",
  "c16_secondary_class_income_above_75th",    FALSE, "age16_cohort",
  "c16_neighborhood_foreign_born_parents",    FALSE, "age16_cohort",
  "c16_neighborhood_income_below_25th",       FALSE, "age16_cohort",
  "c16_neighborhood_income_below_50th",       FALSE, "age16_cohort",
  "c16_neighborhood_income_above_75th",       FALSE, "age16_cohort",
  
  "c11_math",                       TRUE,       "prim8_a_cohort",
  "c11_reading",                    TRUE,       "prim8_a_cohort",
  "c11_language",                   TRUE,       "prim8_a_cohort",
  "c11_vmbo_gl_test",               TRUE,       "prim8_a_cohort",
  "c11_havo_test",                  TRUE,       "prim8_a_cohort",
  "c11_vwo_test",                   TRUE,       "prim8_a_cohort",
  "c11_vmbo_gl_final",              TRUE,       "prim8_a_cohort",
  "c11_havo_final",                 TRUE,       "prim8_a_cohort",
  "c11_vwo_final",                  TRUE,       "prim8_a_cohort",
  "c11_youth_protection",           TRUE,       "prim8_a_cohort",
  "c11_youth_health_costs",         FALSE,      "prim8_a_cohort",
  "c11_living_space_pp",            FALSE,      "prim8_a_cohort",

  "c11_class_vmbo_gl_test",         FALSE,       "prim8_a_cohort",
  "c11_class_havo_test",            FALSE,       "prim8_a_cohort",
  "c11_class_vwo_test",             FALSE,       "prim8_a_cohort",
  "c11_class_size",                 FALSE,       "prim8_a_cohort",
  "c11_class_math",                 FALSE,       "prim8_a_cohort",
  "c11_class_language",             FALSE,       "prim8_a_cohort",
  "c11_class_reading",              FALSE,       "prim8_a_cohort",
  "c11_class_foreign_born_parents", FALSE,       "prim8_a_cohort",
  "c11_class_income_below_25th",    FALSE,       "prim8_a_cohort",
  "c11_class_income_below_50th",    FALSE,       "prim8_a_cohort",
  "c11_class_income_above_75th",    FALSE,       "prim8_a_cohort",

  "c11_primary_neighborhood_foreign_born_parents", FALSE, "prim8_a_cohort",
  "c11_primary_neighborhood_income_below_25th",    FALSE, "prim8_a_cohort",
  "c11_primary_neighborhood_income_below_50th",    FALSE, "prim8_a_cohort",
  "c11_primary_neighborhoods_income_above_75th",   FALSE, "prim8_a_cohort",

  "c11_math",                       TRUE,       "prim8_b_cohort",
  "c11_reading",                    TRUE,       "prim8_b_cohort",
  "c11_language",                   TRUE,       "prim8_b_cohort",
  "c11_vmbo_gl_test",               TRUE,       "prim8_b_cohort",
  "c11_havo_test",                  TRUE,       "prim8_b_cohort",
  "c11_vwo_test",                   TRUE,       "prim8_b_cohort",
  "c11_vmbo_gl_final",              TRUE,       "prim8_b_cohort",
  "c11_havo_final",                 TRUE,       "prim8_b_cohort",
  "c11_vwo_final",                  TRUE,       "prim8_b_cohort",
  "c11_youth_protection",           TRUE,       "prim8_b_cohort",
  "c11_youth_health_costs",         FALSE,      "prim8_b_cohort",
  "c11_b_living_space_pp",          FALSE,      "prim8_b_cohort",

  "c11_b_class_vmbo_gl_test",         FALSE,       "prim8_b_cohort",
  "c11_b_class_havo_test",            FALSE,       "prim8_b_cohort",
  "c11_b_class_vwo_test",             FALSE,       "prim8_b_cohort",
  "c11_b_class_size",                 FALSE,       "prim8_b_cohort",
  "c11_b_class_math",                 FALSE,       "prim8_b_cohort",
  "c11_b_class_language",             FALSE,       "prim8_b_cohort",
  "c11_b_class_reading",              FALSE,       "prim8_b_cohort",
  "c11_b_class_foreign_born_parents", FALSE,       "prim8_b_cohort",
  "c11_b_class_income_below_25th",    FALSE,       "prim8_b_cohort",
  "c11_b_class_income_below_50th",    FALSE,       "prim8_b_cohort",
  "c11_b_class_income_above_75th",    FALSE,       "prim8_b_cohort",

  "c11_b_primary_neighborhood_foreign_born_parents", FALSE, "prim8_b_cohort",
  "c11_b_primary_neighborhood_income_below_25th",    FALSE, "prim8_b_cohort",
  "c11_b_primary_neighborhood_income_below_50th",    FALSE, "prim8_b_cohort",
  "c11_b_primary_neighborhoods_income_above_75th",   FALSE, "prim8_b_cohort"
  
  # "c00_a_perinatal_mortality",        TRUE,        "infant_mortality_a_cohort",
  # "c00_a_neonatal_mortality",         TRUE,        "infant_mortality_a_cohort",
  # "c00_a_infant_mortality",           TRUE,        "infant_mortality_a_cohort",
  # "c00_b_perinatal_mortality",        TRUE,        "infant_mortality_b_cohort",
  # "c00_b_neonatal_mortality",         TRUE,        "infant_mortality_b_cohort",
  # "c00_b_infant_mortality",           TRUE,        "infant_mortality_b_cohort",
  # 
  # "c00_a_sga",                        TRUE,        "newborns_a_cohort",
  # "c00_a_preterm_birth",              TRUE,        "newborns_a_cohort",
  # "c00_b_sga",                        TRUE,        "newborns_b_cohort",
  # "c00_b_preterm_birth",              TRUE,        "newborns_b_cohort"
  
)


# Predictors
income_groups    <- c("all", "Very_Low", "Low", "Mid", "High", "Very_High")
gender_groups    <- c("all", "Men", "Women")
migration_groups <- c("all", "Morocco", "No Migration Background", "Dutch Caribbean",
                      "Suriname", "Turkey", 'Other', "has_migration")
household_groups <- c("all", "Single Parent", "Two Parents")



# Region types
pc4_tab <- read_spss(pwr_location) %>% select(postc)
gwb_tab <- read_spss(gwb_location) %>% select(-postcode_crypt, -buurtcode) %>% distinct()
corop_tab <- read_xlsx(corop_location) %>% select(COROP2023)


regions <- tribble(
  ~region_type,        ~region_id, 
  "all",               "all", 
  "corop_code",        unique(as.character(corop_tab$COROP2023)),
  "municipality_code", unique(as.character(gwb_tab$gemcode)),
  "postcode3",         unique(substr(pc4_tab$postc, 1, 3)),
  "postcode4",         unique(substr(pc4_tab$postc, 1, 4)),
  "neighborhood_code", unique(as.character(gwb_tab$wijkcode))
)
rm(pc4_tab, gwb_tab, corop_tab)


# create the basic grid
model_grid <- 
  expand_grid(
    outcome         = outcome_source$outcome, 
    region_type     = regions$region_type, 
    income_group    = income_groups, 
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
  select(outcome, binary, data_source, income_group, gender_group, migration_group, 
         household_group, region_type, region_id)

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
  filter(!(region_type == 'corop_code' & data_source != 'age35_cohort')) %>%
  filter(region_type != 'corop_code_birth')
  

# Write the entire model grid as a feather file (can be read by row or by column)
write_feather(model_grid, "input/income/model_grid.feather")


