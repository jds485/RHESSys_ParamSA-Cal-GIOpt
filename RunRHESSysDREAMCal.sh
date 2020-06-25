#!/bin/bash
#SBATCH -D /scratch/js4yd/Baisman30mDREAMzs/		# working directory
#SBATCH -o /scratch/js4yd/Baisman30mDREAMzs/RHESSysDREAM.out   # Name of the output file (eg. myMPI.oJobID)
#SBATCH -N 1            					# Total number of nodes to request
#SBATCH --ntasks-per-node 4           		# Number of processors per node
#SBATCH -p standard           				# Queue name
#SBATCH -A quinnlab       					# allocation name
#SBATCH -t 02:30:00       					# Run time (hh:mm:ss)
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
#18 - File with chain starting locations (.csv file with headers)
#19 - Full path to working directory for chain runs (RHESSysRuns)
Rscript /sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/RHESSysDREAM.R '3' '91574' '/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/TNFun/WRTDS_modifiedFunctions.R' '/scratch/js4yd/Baisman30mDREAMzs/obs/BaismanStreamflow_Feb2020Revised_Cal.txt' '/scratch/js4yd/Baisman30mDREAMzs/obs/TN_Feb2020Revised_Cal.txt' '2004-10-01' '2013-09-30' '/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/RHESSys_Baisman30m_g74/worldfiles/worldfile.csv' '30' '/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/TNFun/TabIntMod5QLQ_p5.txt' '/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/TNFun/TabYearMod5QLQ_p4.txt' '/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/TNFun/TabLogQMod5QLQ_p4.txt' '/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/TNFun/TabLogQ2Mod5QLQ_p4.txt' '/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/TNFun/TabSinYearMod5QLQ_p4.txt' '/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/TNFun/TabCosYearMod5QLQ_p4.txt' '/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/TNFun/TabLogErrMod5QLQ_p5.txt' '/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/BaismanCalibrationParameterProblemFile.csv' '/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/BaismanChainStarts_AfterProcessing_Top3.csv' '/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/'