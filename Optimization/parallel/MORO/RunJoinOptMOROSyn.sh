#!/bin/bash
#SBATCH -D /scratch/js4yd/
#SBATCH -o /scratch/js4yd/Run_JoinOptMOROSyn.out
#SBATCH --ntasks=1                              # Number of tasks per serial job (must be 1)
#SBATCH -p standard           									# Queue partition name "standard"
#SBATCH -A quinnlab       											# allocation name
#SBATCH -t 00:05:00       											# Run time per serial job (hh:mm:ss) - up to 36 hours
#SBATCH --mail-user=js4yd@virginia.edu          # address for email notification
#SBATCH --mail-type=ALL                  		    # email at Begin and End of job

module purge
module load gcc/7.1.0 openmpi/3.1.4 R/3.5.3

#arguments are:
#1: directory with RHESSysRuns
#2: Extension to output ('/RHESSys_Baisman30m_g74')
#3: save directory
#4: original decision variables and objectives file with IDs for master and NFE

Rscript JoinOptMOROSyn.R '/scratch/js4yd/Bais910Hill30mOptimizeGI_MORO/RHESSysRuns' '/RHESSys_Baisman30m_g74' '/scratch/js4yd/' '/scratch/js4yd/Bais910Hill30mOptimizeGI_MORO/GInolic_IDs.referenceDVO'