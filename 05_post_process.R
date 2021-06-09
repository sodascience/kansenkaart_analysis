# post-process results 
library(tidyverse)

expectation_grid <- read_rds("data/expectation_grid.rds")


# export to webmapper format
expectation_grid %>% 
  filter(outcome == "income", region_type == "postcode3") %>% 
  select(name = region_id, type = region_type, income_group, gender_group, migration_group, est) %>% 
  pivot_wider(
    names_from = c(income_group, gender_group, migration_group),
    names_sep = "_",
    values_from = est
  ) %>% View()
