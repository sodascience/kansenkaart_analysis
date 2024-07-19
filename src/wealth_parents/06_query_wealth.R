# example of results querying
library(tidyverse)

expectation_grid <- read_rds("output/wealth/expectation_grid.rds")

levels(expectation_grid$outcome)
levels(expectation_grid$income_group)
levels(expectation_grid$gender_group)
levels(expectation_grid$migration_group)
levels(expectation_grid$region_type)

expectation_grid %>% 
  filter(
    outcome == "basis_ggz_costs",
    income_group == "all",
    gender_group == "Vrouwen",
    migration_group == "all",
    region_type == "postcode3"
  )
