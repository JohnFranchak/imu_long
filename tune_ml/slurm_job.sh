#!/bin/bash -l
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem-per-cpu=1G
#SBATCH --time=00:15:00 # 60 minutes
#SBATCH --mail-user=franchak@ucr.edu
#SBATCH --mail-type=ALL
#SBATCH --job-name="tidy_models"
#SBATCH -p intel

Rscript classification_tidymodels.R