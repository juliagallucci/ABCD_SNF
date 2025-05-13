#!/bin/bash
#SBATCH --job-name=snf_clusters_%A
#SBATCH --time=24:00:00
#SBATCH --array=2-6
#SBATCH --mem-per-cpu=9000
#SBATCH --chdir=/projects/jgallucci/abcd/code
#SBATCH --output=/projects/jgallucci/abcd/output/SNF_Clusters/splits/logs/slurm_%j.out
#SBATCH --error=/projects/jgallucci/abcd/output/SNF_Clusters/splits/logs/slurm_%j.err



# script to run SNF across parameters in parallel by cluster number 

# add R module
module load R

Rscript justSNF.R ${SLURM_ARRAY_TASK_ID}