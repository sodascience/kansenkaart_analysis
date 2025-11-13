#!/bin/bash
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -c 128
#SBATCH --exclusive
#SBATCH -t 24:00:00
#SBATCH -p comp_env
#SBATCH -o ./logs/wealth/output.%a.out

# loading modules
echo "Loading modules"
module load 2022
module load R/4.2.1-foss-2022a

#module load 2024
#module load R-bundle-CRAN/2024.11-foss-2024a

#module load 2023
#module load R-bundle-CRAN/2023.12-foss-2023a

# running script
echo "Starting R script..."
Rscript $PROJECT/src/wealth_parents/03_array_job_wealth.R $SLURM_ARRAY_TASK_ID input

