#!/bin/sh
#SBATCH --mail-user=you@umich.edu
#SBATCH --mail-type=BEGIN,END
#SBATCH --cpus-per-task=1
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=4g
#SBATCH --time=24:00:00
#SBATCH --account=pschloss1
#SBATCH --partition=standard
#SBATCH --output=%x.o%A_%a
#SBATCH --array 1-100

SEED=$((SLURM_ARRAY_TASK_ID))
echo $SEED

make processed_data/l2_genus_$SEED.Rds
