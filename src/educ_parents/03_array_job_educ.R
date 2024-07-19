#!/usr/bin/env Rscript

# Description ----
# R script for computing expectations in slurm array jobs
# usage: Rscript 03_array_job.R $SLURM_ARRAY_TASK_ID input

# Setting up logging ----
start <- Sys.time()
tcat <- function(...) {
  cat("\n", format(Sys.time()), "| ")
  cat(..., "\n")
}

# Packages ----
tcat("Loading packages")
library(tidyverse)
library(feather)
library(parallel)


# Args ----
tcat("Parsing arguments")
args     <- commandArgs(trailingOnly = TRUE)
task_id  <- parse_integer(args[1])
data_dir <- parse_character(args[2])
tcat("task_id =", task_id, "; data_dir =", data_dir)

# Chunking ----
tcat("Setting up chunks")
# first we compute how many models to run on this node
# each node has 128 cores, use 96
n_cores  <- 96
# each core can estimate about 14 models per second
mod_rate <- 14
# we want to run each job for about 1 hour (3600 seconds)
job_time <- 3600
# get final chunk size
chunk_size <- n_cores*mod_rate*job_time

# then we get the current task id and assign the right chunk to the current job
#n_total   <- feather_metadata(file.path(data_dir, "educ/model_grid.feather"))$dim[1]
n_total   <- feather_metadata(file.path(data_dir, "educ/model_grid_subset.feather"))$dim[1]
chunk_idx <- ((task_id - 1)*chunk_size + 1):min(task_id*chunk_size, n_total)



# Setting up cluster ----

# load expectation function
source("src/educ_parents/01_expectation_function_educ.R")

# load the model parameter grid for the current chunk
tcat("Loading parameters...")
# model_grid <- feather(file.path(data_dir, "educ/model_grid.feather"))[chunk_idx,]
model_grid <- feather(file.path(data_dir, "educ/model_grid_subset.feather"))[chunk_idx,]


# load required datasets
req_data <- as.character(unique(model_grid$data_source))
tcat("Loading datasets:", req_data, "...")
for (dset in req_data) {
  tcat("Loading", dset)
  assign(dset, read_rds(file.path(data_dir, paste0(dset, ".rds"))))
}
tcat("Data loaded.")

# create the fork cluster for this node
tcat("Initializing fork cluster...")
clus <- makeForkCluster(n_cores)

# # copy data and model function to each thread in the cluster
# # only needed on systems without FORK capability (windows)
# tcat("Creating cluster & copying data to threads...")
# clus <- makeCluster(n_cores)
# exports <- c(req_data, "model_grid", "kansenkaart_expect")
# clusterExport(clus, exports)

# load required packages on each core
pack <- clusterEvalQ(clus, {
  library(tidyverse)
  library(feather)
})

# compute model with load-balancing parallel apply
tcat("Computing models with load-balancing...")
out_matrix <- parSapplyLB(
  cl  = clus,
  X   = seq.int(chunk_size), 
  FUN = function(row) {
    settings <- lapply(model_grid[row, ],function(x) as.character(x))
    out <- tryCatch(kansenkaart_expect(
      cohort_dat    = get(settings$data_source),
      outcome_name  = settings$outcome,
      region_id     = settings$region_id,
      region_type   = settings$region_type,
      educ_grp      = settings$education_group,
      migration_grp = settings$migration_group,
      gender_grp    = settings$gender_group,
      hh_grp        = settings$household_group
    ), error = function(e) rep(NA, 4))
    
    # for efficiency: convert to numeric vector
    unname(unlist(out))
  }
)

# stop the cluster
stopCluster(clus)

# Storing ----
tcat("Done! Storing results...")
out_matrix <- t(out_matrix)
colnames(out_matrix) <- c("est", "lwr", "upr", "n")
file_name <- paste0("out_matrix_", paste(rep("0", 5-nchar(task_id)), collapse = ""), task_id, ".rds")
write_rds(out_matrix, file.path("output", file_name))


tcat("Done.")
tcat("Total elapsed time:", format(Sys.time() - start))
