# Combine the results from the different files
# result: a big matrix with many rows
library(tidyverse)
library(feather)

data_dir <- "input"
output_dir <- "output/wealth"

# read the model grid into memory (big!)
model_grid <- read_feather(file.path(data_dir, "model_grid.feather"))
n_total <- nrow(model_grid)

# add new columns
model_grid <- model_grid %>% mutate(est = NA_real_, lwr = NA_real_, upr = NA_real_, n = NA_real_)

# fill the new columns with info from array job files
files <- list.files("output", full.names = TRUE)

# get chunk size for first file
chunk_size <- nrow(read_rds(files[1]))

for (fn in files) {
  cat(fn, "\n")
  task_id <- parse_number(fn)
  chunk_idx <- ((task_id - 1)*chunk_size + 1):min(task_id*chunk_size, n_total)
  model_grid[chunk_idx, 9:12] <- read_rds(fn)
}

# write the results to disk
write_rds(model_grid, file.path(output_dir, "expectation_grid.rds")) 
# CHANGE FILE PATHS

