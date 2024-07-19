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
  
  "c11_class_vmbo_gl_test",         FALSE,       "elementary_school_cohort",
  "c11_class_havo_test",            FALSE,       "elementary_school_cohort",
  "c11_class_vwo_test",             FALSE,       "elementary_school_cohort",
  "c11_class_size",                 FALSE,       "elementary_school_cohort",
  "c11_class_foreign_born_parents", FALSE,       "elementary_school_cohort",
  "c11_class_math",                 FALSE,       "elementary_school_cohort",
  "c11_class_language",             FALSE,       "elementary_school_cohort",
  "c11_class_reading",              FALSE,       "elementary_school_cohort",
  "c11_class_income_below_25th",    FALSE,       "elementary_school_cohort",
  "c11_class_income_below_50th",    FALSE,       "elementary_school_cohort",
  "c11_class_income_above_75th",    FALSE,       "elementary_school_cohort",
  
  "c00_sga",                        TRUE,       "perinatal_cohort",
  "c00_preterm_birth",              TRUE,       "perinatal_cohort",
  
  "c00_perinatal_mortality",        TRUE,       "child_mortality_cohort",
  "c00_neonatal_mortality",         TRUE,       "child_mortality_cohort",
  "c00_infant_mortality",           TRUE,       "child_mortality_cohort"
)



# Predictors
education_groups <- c("all", "wo", "hbo", "other")
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
  # "corop_code",    unique(as.character(corop_tab$COROP2022)),
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


table(model_grid$region_type)

# Write the entire model grid as a feather file (can be read by row or by column)
write_feather(model_grid, "input/educ/model_grid.feather")


