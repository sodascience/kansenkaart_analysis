# post-process results 
library(tidyverse)

data_dir <- "input/educ"
output_dir <- "output/educ"

expectation_grid <- read_rds(file.path(output_dir, "expectation_grid.rds"))

# disclosure control
expectation_grid <- 
  expectation_grid %>% 
  mutate(
    not_disclose = ifelse(binary == "TRUE", (est*n < 10 | (1-est)*n < 10), n < 25),
    est = ifelse(not_disclose, NA, est),
    lwr = ifelse(not_disclose, NA, lwr),
    upr = ifelse(not_disclose, NA, upr)
  )

write_rds(expectation_grid, file.path(output_dir, "expectation_grid_controlled.rds"))

