# Preprocessing data
# only retain an efficient version of the variables we really need (i.e., 
# factors for categorical variables!)
library(tidyverse)

raw_data_dir <- "raw_data"
processed_dir <- "input"

filter_vars <- c("income_group", "wealth_group", "migration_background",
                 "has_migration", "sex", "type_household",
                 "income_group_tails", "wealth_group_tails")

filter_edu_vars <- c("income_group", "wealth_group", "migration_background",
                     "has_migration", "sex", "type_household", "parents_education",
                     "income_group_tails", "wealth_group_tails")

region_vars <- c("postcode3", "postcode4", "municipality_code", "neighborhood_code", "corop_code")

region_vars_school <- c("postcode3", "postcode4", "municipality_code", "neighborhood_code", "corop_code",
                        "postcode3_birth", "postcode4_birth", "municipality_code_birth",
                        "neighborhood_code_birth", "corop_code_birth")
predictor_vars <- c("income_parents_perc", "wealth_parents_perc")


#### AGE35 COHORT ####
cat("Processing age35 cohort...\n")
age35_cohort <- read_rds(file.path(raw_data_dir, "age35_cohort.rds"))
cat("Size before processing: ", format(object.size(age35_cohort), "MB"), "\n")
age35_cohort <-
  age35_cohort %>%
  ungroup() %>%
  select(RINPERSOONS, RINPERSOON, all_of(predictor_vars), all_of(filter_vars),
         all_of(region_vars), starts_with("c35_")) %>%
  mutate(across(all_of(region_vars), as.factor)) %>%
  mutate(corop_code = gsub("CR", "", corop_code))
age35_cohort %>% write_rds(file.path(processed_dir, "age35_cohort.rds"))
cat("Size after processing: ", format(object.size(age35_cohort), "MB"), "\n\n")
#rm(age35_cohort)


#### AGE21 COHORT ####
cat("Processing age21 cohort...\n")
age21_cohort <- read_rds(file.path(raw_data_dir, "age21_cohort.rds"))
cat("Size before processing: ", format(object.size(age21_cohort), "MB"), "\n")
age21_cohort <- 
  age21_cohort %>%
  ungroup() %>% 
  select(RINPERSOONS, RINPERSOON, all_of(predictor_vars), all_of(filter_vars), 
         all_of(region_vars), starts_with("c21_")) %>% 
  mutate(across(all_of(region_vars), as.factor)) %>%
  mutate(corop_code = gsub("CR", "", corop_code))
# deselect unnecessary variables
age21_cohort <- age21_cohort %>% select(-c(starts_with("c21_age"))) 
age21_cohort %>% write_rds(file.path(processed_dir, "age21_cohort.rds"))
cat("Size after processing: ", format(object.size(age21_cohort), "MB"), "\n\n")
#rm(age21_cohort)
 

#### AGE16 COHORT ####
cat("Processing age16 cohort...\n")
age16_cohort <- read_rds(file.path(raw_data_dir, "age16_cohort.rds"))
cat("Size before processing: ", format(object.size(age16_cohort), "MB"), "\n")
age16_cohort <- 
  age16_cohort %>%
  ungroup() %>% 
  select(RINPERSOONS, RINPERSOON, all_of(predictor_vars), all_of(filter_vars), 
         all_of(region_vars_school), starts_with("c16_")) %>% 
  mutate(across(all_of(region_vars_school), as.factor)) %>%
  mutate(corop_code = gsub("CR", "", corop_code))
age16_cohort %>% write_rds(file.path(processed_dir, "age16_cohort.rds"))
cat("Size after processing: ", format(object.size(age16_cohort), "MB"), "\n\n")
#rm(age16_cohort)


#### PRIM8 A COHORT ####
cat("Processing prim8 a cohort...\n")
prim8_cohort <- read_rds(file.path(raw_data_dir, "prim8__a_cohort.rds"))
cat("Size before processing: ", format(object.size(prim8_cohort), "MB"), "\n")
prim8_cohort <- 
  prim8_cohort %>%
  ungroup() %>% 
  select(RINPERSOONS, RINPERSOON, all_of(predictor_vars), parents_education, 
         all_of(filter_edu_vars), all_of(region_vars_school), starts_with("c11_")) %>% 
  mutate(across(all_of(region_vars_school), as.factor)) 
prim8_cohort %>% write_rds(file.path(processed_dir, "prim8__a_cohort.rds"))
cat("Size after processing: ", format(object.size(prim8_cohort), "MB"), "\n\n")
rm(prim8_cohort)

#### PRIM8 B COHORT ####
cat("Processing prim8 b cohort...\n")
prim8_cohort <- read_rds(file.path(raw_data_dir, "prim8_b_cohort.rds"))
cat("Size before processing: ", format(object.size(prim8_cohort), "MB"), "\n")
prim8_cohort <- 
  prim8_cohort %>%
  ungroup() %>% 
  select(RINPERSOONS, RINPERSOON, all_of(predictor_vars), parents_education, 
         all_of(filter_edu_vars), all_of(region_vars_school), starts_with("c11_")) %>% 
  mutate(across(all_of(region_vars_school), as.factor)) 
prim8_cohort %>% write_rds(file.path(processed_dir, "prim8_b_cohort.rds"))
cat("Size after processing: ", format(object.size(prim8_cohort), "MB"), "\n\n")
rm(prim8_cohort)


#### NEWBORNS A COHORT ####
cat("Processing newborns a cohort...\n")
newborns_cohort <- read_rds(file.path(raw_data_dir, "newborns_a_cohort.rds"))
cat("Size before processing: ", format(object.size(newborns_cohort), "MB"), "\n")
newborns_cohort <- 
  newborns_cohort %>%
  ungroup() %>% 
  select(RINPERSOONS, RINPERSOON, all_of(predictor_vars), parents_education, 
         all_of(filter_edu_vars), all_of(region_vars), starts_with("c00_")) %>% 
  mutate(across(all_of(region_vars), as.factor))
newborns_cohort %>% write_rds(file.path(processed_dir, "newborns_a_cohort.rds"))
cat("Size after processing: ", format(object.size(newborns_cohort), "MB"), "\n\n")
rm(newborns_cohort)

#### NEWBORNS B COHORT ####
cat("Processing newborns b cohort...\n")
newborns_cohort <- read_rds(file.path(raw_data_dir, "newborns_b_cohort.rds"))
cat("Size before processing: ", format(object.size(newborns_cohort), "MB"), "\n")
newborns_cohort <- 
  newborns_cohort %>%
  ungroup() %>% 
  select(RINPERSOONS, RINPERSOON, all_of(predictor_vars), parents_education, 
         all_of(filter_edu_vars), all_of(region_vars), starts_with("c00_")) %>% 
  mutate(across(all_of(region_vars), as.factor))
newborns_cohort %>% write_rds(file.path(processed_dir, "newborns_b_cohort.rds"))
cat("Size after processing: ", format(object.size(newborns_cohort), "MB"), "\n\n")
rm(newborns_cohort)


#### INFANT MORTALITY A COHORT ####
cat("Processing infant mortality a cohort...\n")
infant_mortality_cohort <- read_rds(file.path(raw_data_dir, "infant_mortality_a_cohort.rds"))
cat("Size before processing: ", format(object.size(infant_mortality_cohort), "MB"), "\n")
infant_mortality_cohort <- 
  infant_mortality_cohort %>%
  ungroup() %>% 
  select(RINPERSOONS, RINPERSOON, all_of(predictor_vars), parents_education, 
         all_of(filter_edu_vars), all_of(region_vars), starts_with("c00_")) %>% 
  mutate(across(all_of(region_vars), as.factor)) 
infant_mortality_cohort %>% write_rds(file.path(processed_dir, "infant_mortality_a_cohort.rds"))
cat("Size after processing: ", format(object.size(infant_mortality_cohort), "MB"), "\n\n")
rm(infant_mortality_cohort)


#### INFANT MORTALITY B COHORT ####
cat("Processing infant mortality b cohort...\n")
infant_mortality_cohort <- read_rds(file.path(raw_data_dir, "infant_mortality_b_cohort.rds"))
cat("Size before processing: ", format(object.size(infant_mortality_cohort), "MB"), "\n")
infant_mortality_cohort <- 
  infant_mortality_cohort %>%
  ungroup() %>% 
  select(RINPERSOONS, RINPERSOON, all_of(predictor_vars), parents_education, 
         all_of(filter_edu_vars), all_of(region_vars), starts_with("c00_")) %>% 
  mutate(across(all_of(region_vars), as.factor)) 
infant_mortality_cohort %>% write_rds(file.path(processed_dir, "infant_mortality_b_cohort.rds"))
cat("Size after processing: ", format(object.size(infant_mortality_cohort), "MB"), "\n\n")
rm(infant_mortality_cohort)

