#!/bin/bash
#SBATCH --mail-user=you@umich.edu
#SBATCH --mail-type=BEGIN,END
#SBATCH --cpus-per-task=16
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem-per-cpu=4g 
#SBATCH --time=2:00:00
#SBATCH --account=pschloss1
#SBATCH --partition=standard
#SBATCH --output=%x.o%A_%a

make -j 16 processed_data/l2_genus_pooled_performance.tsv
