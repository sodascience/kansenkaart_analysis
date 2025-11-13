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
education_groups <- c("all", "University", "Higher Professional Education", "Other")
gender_groups    <- c("all", "Men", "Women")
migration_groups <- c("all", "Morocco", "No Migration Background", "Dutch Caribbean",
                      "Suriname", "Turkey", 'Other', "has_migration")
household_groups <- c("all", "Single Parent", "Two Parents")


# Region types
pc4_tab <- read_spss(pwr_location) %>% select(postc)
gwb_tab <- read_spss(gwb_location) %>% select(-postcode_crypt, -buurtcode) %>% distinct()
corop_tab <- read_xlsx(corop_location)


regions <- tribble(
  ~region_type,       ~region_id, 
   "all",              "all",
  "postcode3",         unique(substr(pc4_tab$postc, 1, 3)),
  "postcode4",         unique(substr(pc4_tab$postc, 1, 4)),
  "municipality_code", unique(as.character(gwb_tab$gemcode)),
  "neighborhood_code", unique(as.character(gwb_tab$wijkcode))
)

# create the basic grid
model_grid <- 
  expand_grid(
    outcome         = outcome_source$outcome, 
    region_type     = regions$region_type, 
    education_group = education_groups, 
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
  select(outcome, binary, data_source, education_group, household_group,
         gender_group, migration_group, region_type, region_id)

# replace region_type for youth protection 
model_grid <-
  model_grid %>%
  mutate(region_type = as.character(region_type),
         region_type = ifelse(outcome %in% c('c16_youth_protection', 
                                             'c11_youth_protection') & region_type != 'all', 
                              paste0(region_type, '_birth'), region_type)) %>%
  mutate(region_type = as_factor(region_type))


# Write the entire model grid as a feather file (can be read by row or by column)
write_feather(model_grid, "input/educ/model_grid.feather")


