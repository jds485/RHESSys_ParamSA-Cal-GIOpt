#!/bin/bash
#SBATCH -D /sfs/lustre/bahamut/scratch/js4yd/Bais910Hill30mDREAMzs-10Ch/
#SBATCH -o /sfs/lustre/bahamut/scratch/js4yd/Bais910Hill30mDREAMzs-10Ch/Run_JoinDREAM-10Ch-s1200.out
#SBATCH --ntasks=1                              # Number of tasks per serial job (must be 1)
#SBATCH -p standard           									# Queue partition name "standard"
#SBATCH -A quinnlab       											# allocation name
#SBATCH -t 00:30:00       											# Run time per serial job (hh:mm:ss) - up to 36 hours
#SBATCH --mail-user=js4yd@virginia.edu          # address for email notification
#SBATCH --mail-type=ALL                  		    # email at Begin and End of job

module purge
module load gcc/7.1.0 openmpi/3.1.4 R/3.5.3

#arguments are:
#1: number of chains
#2: number of replicates in the chain
#3: directory with RHESSysRuns ('/nv/vol288/quinnlab-value/js4yd/Baisman30mDREAMzs/RHESSysRuns')
#4: Extension to output ('/RHESSys_Baisman30m_g74')
#5: save directory ('/nv/vol288/quinnlab-value/js4yd/Baisman30mDREAMzs/')
#6: starting value for the chain step index
#7: dataset name to append to column

Rscript JoinDREAM.R '10' '400' '/sfs/lustre/bahamut/scratch/js4yd/Bais910Hill30mDREAMzs-10Ch/RHESSysRuns' '/RHESSys_Baisman30m_g74' '/sfs/lustre/bahamut/scratch/js4yd/Bais910Hill30mDREAMzs-10Ch/' '801' 'Ch10'