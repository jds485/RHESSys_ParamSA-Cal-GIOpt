#!/bin/bash
#SBATCH -D /scratch/js4yd/Bais910Hill30mDREAMzs-10Ch/		# working directory
#SBATCH -o /scratch/js4yd/Bais910Hill30mDREAMzs-10Ch/RHESSysDREAM-A10-s1200.out   # Name of the output file (eg. myMPI.oJobID)
#SBATCH -N 1            					# Total number of nodes to request
#SBATCH --ntasks-per-node 11           		# Number of processors per node
#SBATCH -p standard           				# Queue name
#SBATCH -A quinnlab       					# allocation name
#SBATCH -t 3-12:00:00       					# Run time (days-hh:mm:ss)
#SBATCH --mail-user=js4yd@virginia.edu     # address for email notification
#SBATCH --mail-type=ALL 					# email at Begin and End of job

module purge
module load gcc/7.1.0 openmpi/3.1.4 R/3.5.3 singularity python/3.6.6

#System arguments
# The following must always change between sucessive chain step runs: 2, 9, 30
#1 - number of cores to make, equal to number of chains (string)
#2 - R random seed to run the MCMC (string)
#3 - Observed streamflow record path (.txt file, tab separated with headers)
#4 - Start Date for comparison - first day after spinup is complete (e.g., '2004-10-01')
#5 - End Date for comparison (e.g., '2013-09-30')
#6 - worldfile location (csv file)
#7 - patch resolution (m)
#8 - File that describes the parameter names and bounds (.csv file with headers)
##### 9 is Different than RunRHESSysDREAMCal-10Ch.sh#####
#9 - File with output from previous chain run (.RData file)
#10 - Full path to working directory for chain runs (RHESSysRuns)
#11 - iterations, Need at least one more iteration than the burnin to report a value to the output chain
#12 - eps, normal noise to the proposal update.
#13 - e, multiplier to gamma in proposal update 
#14 - ZupdateFrequency, Z is not updated until after burnin if the update frequency is greater than 1. Making equal to updateInterval.
#15 - pSnooker, chance of a snooker update as a proportion
#16 - DEpairs, Number of chains used to compute the DE proposal update. In papers, randomly choose 1, 2, or 3 to use.
#17 - nCR, nCR >=2 recommended in papers
#18 - updateInterval, must be greater than or equal to nCR.
#19 - burnin, must be greater than or equal to adaptation, or all of the adaptation steps must be manually removed before summarizing chains.
#20 - adaptation, should be about 20 % of the total NFE. Doing this manually.
#21 - thin, thin the chain, or set to 1 to allow for post-solver manual thinning
#22 - path to container image file '/share/resources/containers/singularity/rhessys/rhessys_v3.img'
#23 - path to def files '/scratch/js4yd/Bais910Hill30mDREAMzs-10Ch/RHESSys_Baisman30m_g74/defs/'
#24 - round tolerance for def files '10'
#25 - 'BaismanCalibrationParameterProblemFile.csv'
#26 - RHESSys project name 'RHESSys_Baisman30m_g74'
#27 - full path to project directory '/scratch/js4yd/Bais910Hill30mDREAMzs-10Ch/RHESSys_Baisman30m_g74/'
#28 - path to RHESSys exe file '/scratch/js4yd/RHESSysEastCoast_Optimized/rhessys5.20.0.develop_optimsize'
#29 - path to the likelihood functions '/scratch/js4yd/Bais910Hill30mDREAMzs-10Ch/LikelihoodFun/'
#30 - suffix for the output file information '-10Ch_s###'
#31 - Initial random seed for Python '1'
#32 - Initial random seed for flow likelihood '1020'
#33 - path to the processed streamflow observation .txt file
#34 - Number of initial locations for the multi-start MLE solver - Flow

Rscript /scratch/js4yd/Bais910Hill30mDREAMzs-10Ch/RHESSysRuns/RHESSysDREAM_NoG2W_rr_arg.R '10' '24922' '/scratch/js4yd/Bais910Hill30mDREAMzs-10Ch/obs/QSyn910_1.txt' '2004-10-01' '2013-09-30' '/scratch/js4yd/Bais910Hill30mDREAMzs-10Ch/RHESSys_Baisman30m_g74/worldfiles/worldfile.csv' '30' '/scratch/js4yd/Bais910Hill30mDREAMzs-10Ch/RHESSysRuns/BaismanCalibrationParameterProblemFile_NewGWBounds.csv' '/scratch/js4yd/Bais910Hill30mDREAMzs-10Ch/RHESSysRuns/OutputWorkspace-10Ch_s800.RData' '/scratch/js4yd/Bais910Hill30mDREAMzs-10Ch/RHESSysRuns/' '4000' '0' '0.05' '3' '0.1' '2' '10' '10' '0' '4000' '1' '/share/resources/containers/singularity/rhessys/rhessys_v3.img' '/scratch/js4yd/Bais910Hill30mDREAMzs-10Ch/RHESSys_Baisman30m_g74/defs/' '10' 'BaismanCalibrationParameterProblemFile_NewGWBounds.csv' 'RHESSys_Baisman30m_g74' '/scratch/js4yd/Bais910Hill30mDREAMzs-10Ch/RHESSys_Baisman30m_g74/' '/scratch/js4yd/RHESSysEastCoast_Optimized/rhessys5.20.0.develop_optimsize' '/scratch/js4yd/Bais910Hill30mDREAMzs-10Ch/LikelihoodFun/' '-10Ch_s1200' '1' '1020' '/scratch/js4yd/Bais910Hill30mDREAMzs-10Ch/obs/QSyn910_1_p.txt' '500'