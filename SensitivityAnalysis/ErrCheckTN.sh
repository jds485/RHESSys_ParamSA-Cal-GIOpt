#!/bin/bash
#SBATCH -D /scratch/js4yd/MorrisSA/TNprocessing/output
#SBATCH -o /scratch/js4yd/MorrisSA/TNprocessing/TNExtractErrCheck.out
#SBATCH --ntasks=1                              # Number of tasks per serial job (must be 1)
#SBATCH -p standard           									# Queue partition name "standard"
#SBATCH -A quinnlab       											# allocation name
#SBATCH -t 00:20:00       											# Run time per serial job (hh:mm:ss) - up to 36 hours
#SBATCH --mail-user=js4yd@virginia.edu          # address for email notification
#SBATCH --mail-type=ALL                  		    # email at Begin and End of job

module load gcc/7.1.0 openmpi/3.1.4 R/3.5.3

#arguments are: 
#1. directory where TN extraction output files are located 
#2. Number of days in the simulation period, which is the expected number of output files
Rscript /scratch/js4yd/MorrisSA/TNprocessing/ErrCheckTNFileExtraction.R '/scratch/js4yd/MorrisSA/TNprocessing/output' '3973'