#!/usr/bin/env Rscript

# Description ----
# R script for computing expectations in slurm array jobs
# usage: Rscript 03_array_job.R $SLURM_ARRAY_TASK_ID

start <- Sys.time()

# Packages ----
library(tidyverse)
library(feather)
library(parallel)

# Chunking ----
# first we compute how many models to run on this node
# each node has 16 cores, run for about 10 minutes
n_cores  <- 24
# each core can estimate about 25 models per second
mod_rate <- 25
# we want to run each job for about 10 minutes (600 seconds)
job_time <- 600
# get final chunk size
chunk_size <- n_cores*mod_rate*job_time

# then we get the current task id and assign the right chunk to the current job
task_id   <- parse_integer(commandArgs(trailingOnly = TRUE)[1])
n_total   <- feather_metadata("data/model_grid.feather")$dim[1]
chunk_idx <- ((task_id - 1)*chunk_size + 1):min(task_id*chunk_size, n_total)


# Running ----
# load datasets
main_cohort              <- read_rds("data/main_cohort.rds")
youth_protection_cohort  <- read_rds("data/youth_protection_cohort.rds")
perinatal_cohort         <- read_rds("data/perinatal_cohort.rds")
high_school_cohort       <- read_rds("data/high_school_cohort.rds")
elementary_school_cohort <- read_rds("data/elementary_school_cohort.rds")

# load the model function
source("01_expectation_function.R")

# load the model parameter grid for the current chunk
model_grid <- feather("data/model_grid.feather")[chunk_idx,]

# create the cluster for this node
clus <- makeCluster(n_cores)

# load required packages on each core
pack <- clusterEvalQ(clus, {
  library(tidyverse)
  library(feather)
})

# copy data and model function to each core
clusterExport(clus, c("main_cohort", "youth_protection_cohort", "perinatal_cohort", 
                      "high_school_cohort", "elementary_school_cohort", 
                      "model_grid", "kansenkaart_expect"))

# compute model with load-balancing parallel apply
out_matrix <- parSapplyLB(
  cl  = clus, 
  X   = seq.int(chunk_size), 
  FUN = function(row) {
    settings <- lapply(model_grid[row,], as.character)
    out <- tryCatch(kansenkaart_expect(
      cohort_dat    = get(settings$data_source),
      outcome_name  = settings$outcome,
      region_id     = settings$region_id,
      region_type   = settings$region_type,
      income_grp    = settings$income_group,
      migration_grp = settings$migration_group,
      gender_grp    = settings$gender_group
    ), error = function(e) rep(NA, 4))
    
    # for efficiency: convert to numeric vector
    unname(unlist(out))
  }
)

# stop the cluster
stopCluster(clus)

# Storing ----
out_matrix <- t(out_matrix)
colnames(out_matrix) <- c("est", "lwr", "upr", "n")
file_name <- paste0("out_matrix_", str_pad(task_id, 5, pad = "0"), ".rds")
write_rds(out_matrix, paste0("output/", file_name))

cat("\n\nElapsed time:\n")
print(Sys.time() - start)
