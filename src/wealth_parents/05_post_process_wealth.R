# post-process results 
library(tidyverse)

data_dir <- "input/wealth"
output_dir <- "output/wealth"
export_dir <- "export/wealth/"

expectation_grid <- read_rds(file.path(output_dir, "expectation_grid.rds"))

# disclosure control 
expectation_grid <- 
  expectation_grid %>% 
  mutate(
    not_disclose = ifelse(binary == "TRUE", (est*n < 10 | (1-est)*n < 10), n < 25),
    est = ifelse(not_disclose, NA, est),
    lwr = ifelse(not_disclose, NA, lwr),
    upr = ifelse(not_disclose, NA, upr),
    n   = ifelse(not_disclose, NA, n)
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
    
  } else {
    region_levels <- c('all', 'gemeente_code', 'postcode3', 'postcode4', 'wijk_code')
    
  }
  
  for (rt in region_levels) {
    fp <- file.path(dr, paste0(oc, "_", rt, ".csv"))
    df <- expectation_grid %>% 
      filter(outcome == oc, region_type == rt) %>% 
      select(outcome, wealth_group, gender_group, migration_group, household_group, 
             region_type, region_id, n, est, lwr, upr)
    write_excel_csv(df, fp, na = "")
    pb$tick()
  }
}

