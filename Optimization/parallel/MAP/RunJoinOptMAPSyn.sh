#!/bin/bash
#SBATCH -D /nv/vol288/quinnlab-value/BaisOptMAP/Bais910Hill30mOptimizeGI_MAP/
#SBATCH -o /nv/vol288/quinnlab-value/BaisOptMAP/Bais910Hill30mOptimizeGI_MAP/Run_JoinOptMAPSyn.out
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

Rscript JoinOptMAPSyn.R '/nv/vol288/quinnlab-value/BaisOptMAP/Bais910Hill30mOptimizeGI_MAP/RHESSysRuns/SynEval' '/RHESSys_Baisman30m_g74' '/nv/vol288/quinnlab-value/BaisOptMAP/Bais910Hill30mOptimizeGI_MAP/' '/nv/vol288/quinnlab-value/BaisOptMAP/Bais910Hill30mOptimizeGI_MAP/GInolic_IDs.referenceDVO'