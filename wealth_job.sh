#!/bin/bash
#SBATCH -n 1
#SBATCH --exclusive
#SBATCH -t 10:00:00
#SBATCH -p comp_env
#SBATCH -o ./logs/output.%a.out

# loading modules
echo "Loading modules"
module load 2022
module load R

# running script
echo "Starting R script..."
Rscript $HOME/src/wealth_parents/03_array_job_wealth.R $SLURM_ARRAY_TASK_ID input


