#!/bin/bash

# Set job requirements
#SBATCH -n 1
#SBATCH -t 00:15:00
#SBATCH -o ./logs/output.%a.out # STDOUT
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=e.vankesteren1@uu.nl

# Loading modules
module load R

# Run the script
Rscript "03_array_job.R" $SLURM_ARRAY_TASK_ID

