# Preprocessing data
# only retain an efficient version of the variables we really need (i.e., 
# factors for categorical variables!)
library(tidyverse)

raw_data_dir <- "raw_data"
processed_dir <- "input"

filter_vars <- c("income_group", "wealth_group", "migration_third", 
                 "has_migration", "geslacht", "type_hh")
filter_edu_vars <- c("income_group", "wealth_group", "migration_third", 
                     "has_migration", "geslacht", "type_hh", "parents_education")
region_vars <- c("postcode3", "postcode4", "gemeente_code", "wijk_code", "corop_code")
region_vars_school <- c("postcode3", "postcode4", "gemeente_code", "wijk_code", "corop_code",
                        "postcode3_birth", "postcode4_birth", "gemeente_code_birth", 
                        "wijk_code_birth", "corop_code_birth")
predictor_vars <- c("income_parents_perc", "wealth_parents_perc")


#### MAIN COHORT ####
cat("Processing main cohort...\n")
main_cohort <- read_rds(file.path(raw_data_dir, "main_cohort.rds"))
cat("Size before processing: ", format(object.size(main_cohort), "MB"), "\n")
main_cohort <- 
  main_cohort %>% 
  ungroup() %>%
  select(RINPERSOONS, RINPERSOON, all_of(predictor_vars), all_of(filter_vars), 
         all_of(region_vars), starts_with("c30_")) %>% 
  mutate(across(all_of(region_vars), as.factor))
main_cohort %>% write_rds(file.path(processed_dir, "main_cohort.rds"))
cat("Size after processing: ", format(object.size(main_cohort), "MB"), "\n\n")
rm(main_cohort)


#### STUDENTS COHORT ####
cat("Processing students cohort...\n")
students_cohort <- read_rds(file.path(raw_data_dir, "students_cohort.rds"))
cat("Size before processing: ", format(object.size(students_cohort), "MB"), "\n")
students_cohort <- 
  students_cohort %>%
  ungroup() %>% 
  select(RINPERSOONS, RINPERSOON, all_of(predictor_vars), all_of(filter_vars), 
         all_of(region_vars), starts_with("c21_")) %>% 
  mutate(across(all_of(region_vars), as.factor)) 
students_cohort %>% write_rds(file.path(processed_dir, "students_cohort.rds"))
cat("Size after processing: ", format(object.size(students_cohort), "MB"), "\n\n")
rm(students_cohort)


#### HIGH SCHOOL COHORT ####
cat("Processing high school cohort...\n")
high_school_cohort <- read_rds(file.path(raw_data_dir, "high_school_cohort.rds"))
cat("Size before processing: ", format(object.size(high_school_cohort), "MB"), "\n")
# high_school_cohort <- high_school_cohort %>% 
# rename(has_migration = has_migration_background)
high_school_cohort <- 
  high_school_cohort %>%
  ungroup() %>% 
  select(RINPERSOONS, RINPERSOON, all_of(predictor_vars), all_of(filter_vars), 
         all_of(region_vars_school), starts_with("c16_")) %>% 
  mutate(across(all_of(region_vars_school), as.factor)) 
high_school_cohort %>% write_rds(file.path(processed_dir, "high_school_cohort.rds"))
cat("Size after processing: ", format(object.size(high_school_cohort), "MB"), "\n\n")
rm(high_school_cohort)


#### ELEMENTARY SCHOOL COHORT ####
cat("Processing elementary school cohort...\n")
elementary_school_cohort <- read_rds(file.path(raw_data_dir, "elementary_school_cohort.rds"))
cat("Size before processing: ", format(object.size(elementary_school_cohort), "MB"), "\n")
elementary_school_cohort <- 
  elementary_school_cohort %>%
  ungroup() %>% 
  select(RINPERSOONS, RINPERSOON, all_of(predictor_vars), parents_education, 
         all_of(filter_edu_vars), all_of(region_vars_school), starts_with("c11_")) %>% 
  mutate(across(all_of(region_vars_school), as.factor)) 
elementary_school_cohort %>% write_rds(file.path(processed_dir, "elementary_school_cohort.rds"))
cat("Size after processing: ", format(object.size(elementary_school_cohort), "MB"), "\n\n")
rm(elementary_school_cohort)


#### CLASSROOM COHORT ####
cat("Processing classroom cohort...\n")
classroom_cohort <- read_rds(file.path(raw_data_dir, "classroom_cohort.rds"))
cat("Size before processing: ", format(object.size(classroom_cohort), "MB"), "\n")
classroom_cohort <- 
  classroom_cohort %>%
  ungroup() %>% 
  select(RINPERSOONS, RINPERSOON, all_of(predictor_vars), parents_education, 
         all_of(filter_edu_vars), all_of(region_vars), starts_with("c11_")) %>% 
  mutate(across(all_of(region_vars), as.factor)) 
classroom_cohort %>% write_rds(file.path(processed_dir, "classroom_cohort.rds"))
cat("Size after processing: ", format(object.size(classroom_cohort), "MB"), "\n\n")
rm(classroom_cohort)


#### PERINATAL COHORT ####
cat("Processing perinatal cohort...\n")
perinatal_cohort <- read_rds(file.path(raw_data_dir, "perinatal_cohort.rds"))
cat("Size before processing: ", format(object.size(perinatal_cohort), "MB"), "\n")
perinatal_cohort <- 
  perinatal_cohort %>%
  ungroup() %>% 
  select(RINPERSOONS, RINPERSOON, all_of(predictor_vars), parents_education, 
         all_of(filter_edu_vars), all_of(region_vars), starts_with("c00_")) %>% 
  mutate(across(all_of(region_vars), as.factor))%>%

  mutate(geslacht = recode(geslacht, 'jongen' = 'Mannen', 'meisje' = 'Vrouwen'),
         geslacht = as.factor(geslacht)) 
perinatal_cohort %>% write_rds(file.path(processed_dir, "perinatal_cohort.rds"))
cat("Size after processing: ", format(object.size(perinatal_cohort), "MB"), "\n\n")
rm(perinatal_cohort)


#### CHILD MORTALITY COHORT ####
cat("Processing child mortality cohort...\n")
child_mortality_cohort <- read_rds(file.path(raw_data_dir, "child_mortality_cohort.rds"))
cat("Size before processing: ", format(object.size(child_mortality_cohort), "MB"), "\n")
child_mortality_cohort <- 
  child_mortality_cohort %>%
  ungroup() %>% 
  select(RINPERSOONS, RINPERSOON, all_of(predictor_vars), parents_education, 
         all_of(filter_edu_vars), all_of(region_vars), starts_with("c00_")) %>% 
  mutate(across(all_of(region_vars), as.factor)) %>% 
# recode gender
  mutate(geslacht = recode(geslacht, 'jongen' = 'Mannen', 'meisje' = 'Vrouwen'),
         geslacht = as.factor(geslacht)) 
child_mortality_cohort %>% write_rds(file.path(processed_dir, "child_mortality_cohort.rds"))
cat("Size after processing: ", format(object.size(child_mortality_cohort), "MB"), "\n\n")
rm(child_mortality_cohort)


