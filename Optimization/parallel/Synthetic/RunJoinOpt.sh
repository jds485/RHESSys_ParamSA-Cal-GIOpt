#!/bin/bash
#SBATCH -D /nv/vol288/quinnlab-value/js4yd/Bais910Hill30mOptGI_Syn/
#SBATCH -o /scratch/js4yd/Run_JoinOptSyn.out
#SBATCH --ntasks=1                              # Number of tasks per serial job (must be 1)
#SBATCH -p standard           									# Queue partition name "standard"
#SBATCH -A quinnlab       											# allocation name
#SBATCH -t 20:00:00       											# Run time per serial job (hh:mm:ss) - up to 36 hours
#SBATCH --mail-user=js4yd@virginia.edu          # address for email notification
#SBATCH --mail-type=ALL                  		    # email at Begin and End of job

module purge
module load gcc/7.1.0 openmpi/3.1.4 R/3.5.3

#arguments are:
#1: directory with RHESSysRuns ('/nv/vol288/quinnlab-value/js4yd/Baisman30mDREAMzs/RHESSysRuns')
#2: Extension to output ('/RHESSys_Baisman30m_g74')
#3: save directory ('/nv/vol288/quinnlab-value/js4yd/Baisman30mDREAMzs/')

Rscript JoinOpt.R '/nv/vol288/quinnlab-value/js4yd/Bais910Hill30mOptGI_Syn/RHESSysRuns' '/RHESSys_Baisman30m_g74' '/scratch/js4yd/'