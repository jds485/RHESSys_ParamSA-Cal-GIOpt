#!/bin/bash
#SBATCH -D /scratch/js4yd/Bais910Hill30mOptGI_MOROMinMax
#SBATCH -o /scratch/js4yd/Bais910Hill30mOptGI_MOROMinMax/Run_JoinOptMinMax.out
#SBATCH --ntasks=1                              # Number of tasks per serial job (must be 1)
#SBATCH -p standard           									# Queue partition name "standard"
#SBATCH -A quinnlab       											# allocation name
#SBATCH -t 3:00:00       											# Run time per serial job (hh:mm:ss) - up to 36 hours
#SBATCH --mail-user=js4yd@virginia.edu          # address for email notification
#SBATCH --mail-type=ALL                  		    # email at Begin and End of job

module purge
module load gcc/7.1.0 openmpi/3.1.4 R/3.5.3

#arguments are:
#1: directory with RHESSysRuns
#2: Extension to output
#3: save directory

Rscript JoinOptMOROMinMax.R '/project/quinnlab/js4yd/Bais910Hill30mOptGI_MOROMinMax/RHESSysRuns' '/RHESSys_Baisman30m_g74' '/scratch/js4yd/Bais910Hill30mOptGI_MOROMinMax/'