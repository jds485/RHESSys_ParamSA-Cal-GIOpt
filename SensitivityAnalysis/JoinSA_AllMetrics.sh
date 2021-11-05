#!/bin/bash
#SBATCH -D /scratch/js4yd/MorrisSA/SAmetrics/
#SBATCH -o /scratch/js4yd/MorrisSA/SAmetrics/Run_JoinSA.out
#SBATCH --ntasks=1                              # Number of tasks per serial job (must be 1)
#SBATCH -p standard           									# Queue partition name "standard"
#SBATCH -A quinnlab       											# allocation name
#SBATCH -t 00:05:00       											# Run time per serial job (hh:mm:ss) - up to 36 hours
#SBATCH --mail-user=js4yd@virginia.edu          # address for email notification
#SBATCH --mail-type=ALL                  		    # email at Begin and End of job

module load gcc/7.1.0 openmpi/3.1.4 R/3.5.3

#arguments are:
#1. directory where ExtractSA.sh wrote data for each trajectory
#2. full path to MorrisSA/SAmetrics/MorrisSamples_BeforeProcessing.csv file
#3. directory where the joined files will be written.
Rscript JoinSAtrajectories_AllMetrics.R '/scratch/js4yd/MorrisSA/SAmetrics/SAdata/' "/scratch/js4yd/MorrisSA/SAmetrics/MorrisSamples_BeforeProcessing.csv" '/scratch/js4yd/MorrisSA/SAmetrics/'