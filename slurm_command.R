library(feather)

# job command for slurm
# each node has 24 cores
n_cores  <- 24
# each core can estimate about 1 model per second
mod_rate <- 5
# we want to run each job for about 1 hours (3600 seconds)
job_time <- 3600
# get final chunk size
chunk_size <- n_cores*mod_rate*job_time

# get grid length
N <- feather_metadata("input/income/model_grid.feather")$dim[1]

# number of jobs
ceiling(N/chunk_size)


# sinfo
# sbatch --array=1-491 job.sh
# squeue

# some failed, still run: 
# sbatch --array=21,34,103,131,150,183,197,203,208,234,241,250,257,261,298 job.sh