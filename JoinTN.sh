#!/bin/bash
#SBATCH -D /scratch/js4yd/MorrisSA/TNprocessing/
#SBATCH -o /scratch/js4yd/MorrisSA/TNprocessing/JoinTNHillMedRun_All.out
#SBATCH --ntasks=1                              # Number of tasks per serial job (must be 1)
#SBATCH -p largemem           									# Queue partition name
#SBATCH -A quinnlab       											# allocation name
#SBATCH -t 01:00:00       											# Run time per serial job (hh:mm:ss) - up to 36 hours
#SBATCH --mail-user=js4yd@virginia.edu          # address for email notification
#SBATCH --mail-type=ALL                  		    # email at Begin and End of job

module load gcc/7.1.0 openmpi/3.1.4 R/3.5.3

#arguments are: 
#1. directory where files in arguments 2 and 3 are located. Files in arguments 5 and 6 will also be written to this directory
#2. text file from the basin streamflows with only the column names and 2 rows of data (to speed up processing)
#3. the hillslope streamflows file
#4. directory where the TN a_ and h_ files are located
#5. name of the Rdata file containing all R variables created when running this script
#6. name of the TN data text file that will be written 
Rscript TNFileJoining_HillMed.R "/scratch/js4yd/MorrisSA/TNprocessing/" 'DateColumnNames.txt' 'SAResults_HillStreamflow_p6_t.txt' '/scratch/js4yd/MorrisSA/TNprocessing/TNdata/' "TNSAreps_HillMed_All.RData" 'SAResults_HillTNMed_p3_All.txt'