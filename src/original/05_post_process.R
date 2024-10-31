# post-process results 
library(tidyverse)
library(progress)
library(writexl)

# Read data ----
# this takes a few seconds!
expectation_grid <- read_rds("data/expectation_grid.rds")


# Disclosure control ----
# this takes a few seconds
expectation_grid <- 
  expectation_grid %>% 
  mutate(
    not_disclose = ifelse(binary, est*n < 10, n < 25),
    est = ifelse(not_disclose, NA, est),
    lwr = ifelse(not_disclose, NA, lwr),
    upr = ifelse(not_disclose, NA, upr)
  )

# Separate (outcome files per region) ----
pb <- progress_bar$new(total = length(unique(expectation_grid$outcome))*nlevels(expectation_grid$region_type))
for (oc in unique(expectation_grid$outcome)) {
  dr <- file.path("export", oc)
  if (!dir.exists(dr)) dir.create(dr)
  for (rt in levels(expectation_grid$region_type)) {
    fp <- file.path(dr, paste0(oc, "_", rt, ".csv"))
    df <- expectation_grid %>% 
      filter(outcome == oc, region_type == rt) %>% 
      select(outcome, income_group, gender_group, migration_group, region_type, region_id, est, lwr, upr)
    write_excel_csv(df, fp, na = "")
    pb$tick()
  }
}

# Reformat (wide) ----
# function to convert expectation grid to webmapper wide format
# oc = outcome name
format_wide <- function(oc, expectation_grid) {
  df <- expectation_grid %>% filter(outcome == oc)
  
  # estimates table
  est_tab <- 
    df %>% 
    select(name = region_id, type = region_type, income_group, gender_group, migration_group, est) %>% 
    group_by(type) %>% 
    pivot_wider(
      names_from = c(income_group, gender_group, migration_group),
      names_sep = "_",
      values_from = est
    )
  
  bw_tab <- 
    df %>% 
    select(name = region_id, type = region_type, income_group, gender_group, migration_group, upr, lwr) %>% 
    group_by(type) %>% 
    mutate(bw = upr - lwr) %>% 
    select(-upr, -lwr) %>% 
    pivot_wider(
      names_from = c(income_group, gender_group, migration_group),
      names_sep = "_",
      names_prefix = "BW_",
      values_from = bw
    )
  
  out_tab <- left_join(est_tab, bw_tab, by = c("name", "type"))
  
  # ID, name, type column
  out_tab <- out_tab %>% mutate(id = NA) %>% select(id, everything())
  
  out_tab
}

pb <- progress_bar$new(total = length(unique(expectation_grid$outcome)))
for (oc in unique(expectation_grid$outcome)) {
  fp <- file.path("export_wide", paste0(oc, ".xlsx"))
  df <- format_wide(oc, expectation_grid)
  write_xlsx(df, fp, col_names = TRUE)
  pb$tick()
}

