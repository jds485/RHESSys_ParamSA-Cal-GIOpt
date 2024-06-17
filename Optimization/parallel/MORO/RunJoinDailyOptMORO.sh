#!/bin/bash
#SBATCH -D /nv/vol288/quinnlab-value/js4yd/Bais910Hill30mOptGI_MORO/
#SBATCH -o /nv/vol288/quinnlab-value/js4yd/Bais910Hill30mOptGI_MORO/Run_JoinDailyOptMORO.out
#SBATCH --ntasks=1                              # Number of tasks per serial job (must be 1)
#SBATCH -p standard           									# Queue partition name "standard"
#SBATCH -A quinnlab       											# allocation name
#SBATCH -t 05:00:00       											# Run time per serial job (hh:mm:ss) - up to 36 hours
#SBATCH --mail-user=js4yd@virginia.edu          # address for email notification
#SBATCH --mail-type=ALL                  		    # email at Begin and End of job

module purge
module load gcc/7.1.0 openmpi/3.1.4 R/3.5.3

#arguments are:
#1: directory with RHESSysRuns
#2: Extension to output ('/RHESSys_Baisman30m_g74')
#3: save directory
#4: original decision variables and objectives file with IDs for master and NFE
#5: number of parameter sets evaluated
#6: RHESSys grid cell resolution
#7: Start date for evaluations

Rscript JoinDailyOptMORO.R '/nv/vol288/quinnlab-value/js4yd/Bais910Hill30mOptGI_MORO/RHESSysRuns' '/RHESSys_Baisman30m_g74' '/nv/vol288/quinnlab-value/js4yd/Bais910Hill30mOptGI_MORO/' '/nv/vol288/quinnlab-value/js4yd/Bais910Hill30mOptGI_MORO/GInolic_IDs.referenceDVO' '9' '30' '2004-10-01' 
