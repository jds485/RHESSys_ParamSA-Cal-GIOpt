module load singularity gcc/7.1.0 openmpi/3.1.4 R/3.5.3

#Execute DREAM_ParameterBoundChecks_ChainStarts.py to adjust the chain starting locations to meet parameter constraints, if needed. This is completed using the version of Python in the singularity container

#arguments 
#1: working directory
#2: random seed - should be different for each step in chain. All chains processed at once in this script
#3: directory of def files
#4: round tolerance <= 10
#5: problem file name with extension
#6: chain parameter sample text file name without extension (e.g., 'BaismanChainStarts', 'BaismanChain_1' where 1 is chain iteration)

singularity exec /share/resources/containers/singularity/rhessys/rhessys_v3.img python /scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/DREAM_ParameterBoundChecks_ChainStarts.py '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/' '75843' '/scratch/js4yd/Baisman30mDREAMzs/RHESSys_Baisman30m_g74/defs/' '10' 'BaismanCalibrationParameterProblemFile.csv' 'BaismanChainStarts'