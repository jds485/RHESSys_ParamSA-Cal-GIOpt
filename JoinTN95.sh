#!/bin/bash
#SBATCH -D /scratch/js4yd/MorrisSA/TNprocessing/
#SBATCH -o /scratch/js4yd/MorrisSA/TNprocessing/JoinTNHill95Run_All.out
#SBATCH --ntasks=1                              # Number of tasks per serial job (must be 1)
#SBATCH -p largemem           									# Queue partition name
#SBATCH -A quinnlab       											# allocation name
#SBATCH -t 01:00:00       											# Run time per serial job (hh:mm:ss) - up to 36 hours
#SBATCH --mail-user=js4yd@virginia.edu          # address for email notification
#SBATCH --mail-type=ALL                  		    # email at Begin and End of job

module load gcc/7.1.0 openmpi/3.1.4 R/3.5.3

Rscript TNFileJoining_Hill95.R