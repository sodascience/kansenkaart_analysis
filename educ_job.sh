#!/bin/bash
#SBATCH -N 1
#SBATCH --exclusive
#SBATCH -t 24:00:00
#SBATCH -p comp_env
#SBATCH -o ./logs/educ/output.%a.out

# loading modules
echo "Loading modules"
module load 2022
module load R/4.2.1-foss-2022a

# running script
echo "Starting R script..."
Rscript $PROJECT/src/educ_parents/03_array_job_educ.R $SLURM_ARRAY_TASK_ID input