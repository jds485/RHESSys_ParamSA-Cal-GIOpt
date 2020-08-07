#!/bin/bash
#SBATCH -D /scratch/js4yd/Baisman30mDREAMzs-10Ch/		# working directory
#SBATCH -o /scratch/js4yd/Baisman30mDREAMzs-10Ch/RHESSysDREAM-A10-s500.out   # Name of the output file (eg. myMPI.oJobID)
#SBATCH -N 1            					# Total number of nodes to request
#SBATCH --ntasks-per-node 11           		# Number of processors per node
#SBATCH -p standard           				# Queue name
#SBATCH -A quinnlab       					# allocation name
#SBATCH -t 4-00:00:00       					# Run time (days-hh:mm:ss)
#SBATCH --mail-user=js4yd@virginia.edu     # address for email notification
#SBATCH --mail-type=ALL 					# email at Begin and End of job

module purge
module load gcc/7.1.0 openmpi/3.1.4 R/3.5.3 singularity python/3.6.6

#System arguments
#1 - number of cores to make, equal to number of chains (string)
#2 - R random seed to run the MCMC (string)
#3 - WRTDS script path (R script)
#4 - Observed streamflow record path (.txt file, tab separated with headers)
#5 - Observed TN record path (.txt file, tab separated with headers)
#6 - Start Date for comparison - first day after spinup is complete (e.g., '2004-10-01')
#7 - End Date for comparison (e.g., '2013-09-30')
#8 - worldfile location (csv file)
#9 - patch resolution (m)
#10 - WRTDS TabInt file (txt file, tab separated with headers)
#11 - WRTDS TabYear file (txt file, tab separated with headers)
#12 - WRTDS TabLogQ file (txt file, tab separated with headers)
#13 - WRTDS TabLogQ2 file (txt file, tab separated with headers)
#14 - WRTDS TabSinYear file (txt file, tab separated with headers)
#15 - WRTDS TabCosYear file (txt file, tab separated with headers)
#16 - WRTDS TabLogErr file (txt file, tab separated with headers)
#17 - File that describes the parameter names and bounds (.csv file with headers)
##### 18 is Different than RunRHESSysDREAMCal-10Ch.sh#####
#18 - File with output from previous chain run (.RData file)
#19 - Full path to working directory for chain runs (RHESSysRuns)
#20 - iterations, Need at least one more iteration than the burnin to report a value to the output chain
#21 - eps, normal noise to the proposal update.
#22 - e, multiplier to gamma in proposal update 
#23 - ZupdateFrequency, Z is not updated until after burnin if the update frequency is greater than 1. Making equal to updateInterval.
#24 - pSnooker, chance of a snooker update as a proportion
#25 - DEpairs, Number of chains used to compute the DE proposal update. In papers, randomly choose 1, 2, or 3 to use.
#26 - nCR, nCR >=2 recommended in papers
#27 - updateInterval, must be greater than or equal to nCR.
#28 - burnin, must be greater than or equal to adaptation, or all of the adaptation steps must be manually removed before summarizing chains.
#29 - adaptation, should be about 20 % of the total NFE. Doing this manually.
#30 - thin, thin the chain, or set to 1 to allow for post-solver manual thinning
#31 - path to container image file '/share/resources/containers/singularity/rhessys/rhessys_v3.img'
#32 - path to def files '/scratch/js4yd/Baisman30mDREAMzs-10Ch/RHESSys_Baisman30m_g74/defs/'
#33 - round tolerance for def files '10'
#34 - 'BaismanCalibrationParameterProblemFile.csv'
#35 - RHESSys project name 'RHESSys_Baisman30m_g74'
#36 - full path to project directory '/scratch/js4yd/Baisman30mDREAMzs-10Ch/RHESSys_Baisman30m_g74/'
#37 - path to RHESSys exe file '/scratch/js4yd/RHESSysEastCoast_Optimized/rhessys5.20.0.develop_optimsize'
#38 - path to the likelihood functions '/scratch/js4yd/Baisman30mDREAMzs-10Ch/LikelihoodFun/'
#39 - suffix for the output file information '-10Ch_s###'
#40 - Initial random seed for Python '1'
#41 - Initial random seed for flow likelihood '1020'
#42 - Initial random seed for TN likelihood '2010'
#43 - path to the processed streamflow observation .txt file
#44 - path to the processed TN observation .txt file
#45 - Number of initial locations for the multi-start MLE solver - Flow
#46 - Number of initial locations for the multi-start MLE solver - TN

Rscript /scratch/js4yd/Baisman30mDREAMzs-10Ch/RHESSysRuns/RHESSysDREAM_NoG2W_rr_arg.R '10' '30624' '/scratch/js4yd/Baisman30mDREAMzs-10Ch/TNFun/WRTDS_modifiedFunctions.R' '/scratch/js4yd/Baisman30mDREAMzs-10Ch/obs/BaismanStreamflow_Feb2020Revised_Cal.txt' '/scratch/js4yd/Baisman30mDREAMzs-10Ch/obs/TN_Feb2020Revised_Cal.txt' '2004-10-01' '2013-09-30' '/scratch/js4yd/Baisman30mDREAMzs-10Ch/RHESSys_Baisman30m_g74/worldfiles/worldfile.csv' '30' '/scratch/js4yd/Baisman30mDREAMzs-10Ch/TNFun/TabIntMod5QLQ_p5.txt' '/scratch/js4yd/Baisman30mDREAMzs-10Ch/TNFun/TabYearMod5QLQ_p4.txt' '/scratch/js4yd/Baisman30mDREAMzs-10Ch/TNFun/TabLogQMod5QLQ_p4.txt' '/scratch/js4yd/Baisman30mDREAMzs-10Ch/TNFun/TabLogQ2Mod5QLQ_p4.txt' '/scratch/js4yd/Baisman30mDREAMzs-10Ch/TNFun/TabSinYearMod5QLQ_p4.txt' '/scratch/js4yd/Baisman30mDREAMzs-10Ch/TNFun/TabCosYearMod5QLQ_p4.txt' '/scratch/js4yd/Baisman30mDREAMzs-10Ch/TNFun/TabLogErrMod5QLQ_p5.txt' '/scratch/js4yd/Baisman30mDREAMzs-10Ch/RHESSysRuns/BaismanCalibrationParameterProblemFile.csv' '/scratch/js4yd/Baisman30mDREAMzs-10Ch/RHESSysRuns/OutputWorkspace-10Ch_rr.RData' '/scratch/js4yd/Baisman30mDREAMzs-10Ch/RHESSysRuns/' '1000' '0' '0.05' '3' '0.1' '2' '3' '3' '0' '0' '1' '/share/resources/containers/singularity/rhessys/rhessys_v3.img' '/scratch/js4yd/Baisman30mDREAMzs-10Ch/RHESSys_Baisman30m_g74/defs/' '10' 'BaismanCalibrationParameterProblemFile.csv' 'RHESSys_Baisman30m_g74' '/scratch/js4yd/Baisman30mDREAMzs-10Ch/RHESSys_Baisman30m_g74/' '/scratch/js4yd/RHESSysEastCoast_Optimized/rhessys5.20.0.develop_optimsize' '/scratch/js4yd/Baisman30mDREAMzs-10Ch/LikelihoodFun/' '-10Ch_s500' '1' '1020' '2010' '/scratch/js4yd/Baisman30mDREAMzs-10Ch/obs/BaismanStreamflow_Feb2020Revised_Cal_p.txt' '/scratch/js4yd/Baisman30mDREAMzs-10Ch/obs/TN_Feb2020Revised_Cal_p.txt' '20' '20'