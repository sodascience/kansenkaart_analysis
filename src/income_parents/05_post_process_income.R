# post-process results 
library(tidyverse)
library(progress)
library(writexl)
library(stringr)

data_dir <- "input/income"
output_dir <- "output/income"
export_dir <- "export/income/"

expectation_grid <- read_rds(file.path(output_dir, "expectation_grid.rds"))


# disclosure control
expectation_grid <- 
  expectation_grid %>% 
  mutate(
    not_disclose = ifelse(binary == "TRUE", (est*n < 10 | (1-est)*n < 10), n < 10),
    est = ifelse(not_disclose, NA, est),
    lwr = ifelse(not_disclose, NA, lwr),
    upr = ifelse(not_disclose, NA, upr),
    n = ifelse(not_disclose, NA, n)
  ) 


# remove estimates with 0
expectation_grid <- 
  expectation_grid %>% 
  mutate(
    lwr = ifelse(est == 0, NA, lwr),
    upr = ifelse(est == 0, NA, upr),
    n   = ifelse(est == 0, NA, n),
    est = ifelse(est == 0, NA, est),
  ) %>%
  filter(!is.na(n))



# write_rds(expectation_grid, file.path(output_dir, "expectation_grid_controlled.rds"))



# Separate (outcome files per region) ----
pb <- progress_bar$new(total = length(unique(expectation_grid$outcome))*nlevels(expectation_grid$region_type))
for (oc in unique(expectation_grid$outcome)) {
  dr <- file.path(export_dir, oc)
  if (!dir.exists(dr)) dir.create(dr)
  
  
  if (oc %in% c("c11_youth_protection", "c16_youth_protection")) {
    region_levels <- c('all', 'gemeente_code_birth', 'postcode3_birth', 
                       'postcode4_birth', 'wijk_code_birth')
    
  } else if (grepl('^c30_', oc)) {
    region_levels <- c('all', 'corop_code', 'gemeente_code', 'postcode3', 'postcode4', 'wijk_code')
    
    
  } else {
    region_levels <- c('all', 'gemeente_code', 'postcode3', 'postcode4', 'wijk_code')
    
  }
  
  for (rt in region_levels) {
    fp <- file.path(dr, paste0(oc, "_", rt, ".csv"))
    df <- expectation_grid %>% 
      filter(outcome == oc, region_type == rt) %>% 
      select(outcome, income_group, gender_group, migration_group, household_group, 
             region_type, region_id, n, est, lwr, upr)
    write_excel_csv(df, fp, na = "")
    pb$tick()
  }
}



# test <- read_csv('H:/IGM project/kansenkaart_analysis/export/c30_age_left_parents/c30_age_left_parents_postcode4.csv')
# head(test)


# Reformat (wide) ----
# function to convert expectation grid to webmapper wide format
# oc = outcome name

# change labels
# expectation_grid <-
#   expectation_grid %>%
#   mutate(gender_group = recode(gender_group, 
#                                "Mannen" = "male",
#                                "Vrouwen" = "female"),
#          migration_group = recode(migration_group, 
#                                   "Nederland" = "native", 
#                                   "Turkije" = "turkey", 
#                                   "Marokko" = "morocco", 
#                                   "Suriname" = "surinam", 
#                                   "Nederlandse Antillen (oud)" = "antilles", 
#                                   "Westers" = "otherswestern", 
#                                   "NietWesters" = "othersnonwestern"))
# 
# 
# 
# format_wide <- function(oc, expectation_grid) {
#   df <- expectation_grid %>% 
#     filter(outcome == oc, 
#            (region_type == "gemeente_code" | region_type == "postcode3" |
#               region_type == "postcode4"))
#   
#   # estimates table
#   est_tab <- 
#     df %>% 
#     select(name = region_id, type = region_type, income_group, 
#            gender_group, migration_group, est) %>% 
#     group_by(type) %>% 
#     pivot_wider(
#       names_from = c(income_group, gender_group, migration_group),
#       names_sep = "_",
#       values_from = est
#     )
#   
#   bw_tab <- 
#     df %>% 
#     select(name = region_id, type = region_type, income_group, gender_group, migration_group, upr, est) %>% 
#     group_by(type) %>% 
#     mutate(bw = upr - est) %>% 
#     select(-upr, -est) %>% 
#     pivot_wider(
#       names_from = c(income_group, gender_group, migration_group),
#       names_sep = "_",
#       names_prefix = "BW_",
#       values_from = bw
#     )
#   
#   out_tab <- left_join(est_tab, bw_tab, by = c("name", "type"))
#   
#   # ID, name, type column
#   out_tab <- out_tab %>% mutate(id = NA) %>% select(id, everything())
#   
#   out_tab
# }
# 
# pb <- progress_bar$new(total = length(unique(expectation_grid$outcome)))
# for (oc in unique(expectation_grid$outcome)) {
#   fp <- file.path("export_wide", paste0(oc, ".xlsx"))
#   df <- format_wide(oc, expectation_grid)
#   write_xlsx(df, fp, col_names = TRUE)
#   pb$tick()
# }

