#!/bin/bash
#SBATCH -D /sfs/lustre/bahamut/scratch/js4yd/Bais910Hill30mDREAMzs-10Ch/
#SBATCH -o /sfs/lustre/bahamut/scratch/js4yd/Bais910Hill30mDREAMzs-10Ch/Run_MoveDREAM-10Ch-s1200.out
#SBATCH --ntasks=1                              # Number of tasks per serial job (must be 1)
#SBATCH -p standard           									# Queue partition name "standard"
#SBATCH -A quinnlab       											# allocation name
#SBATCH -t 08:00:00       											# Run time per serial job (hh:mm:ss) - up to 36 hours
#SBATCH --mail-user=js4yd@virginia.edu          # address for email notification
#SBATCH --mail-type=ALL                  		    # email at Begin and End of job

#Script to move files to permanent storage.
#Takes about 12 seconds per RHESSys directory on average over all steps in this script.

module purge

#Directory where files to be moved are located
cd /sfs/lustre/bahamut/scratch/js4yd/Bais910Hill30mDREAMzs-10Ch/

#Directory to move into (must already exist with RHESSysRuns/output directory)
VAL='/nv/vol288/quinnlab-value/js4yd/Bais910Hill30mDREAMzs-10Ch'

#Move completed RHESSys run data
mv ./RHESSysRuns/Run* "$VAL"/RHESSysRuns

#Move output files
mv ./RHESSysRuns/output/* "$VAL"/RHESSysRuns/output

#Move all of the parameter sets for chains
mv ./RHESSysRuns/Chain* "$VAL"/RHESSysRuns

#Move all of the Accept Reject files
mv ./RHESSysRuns/Accept* "$VAL"/RHESSysRuns

#Move output chain list
mv ./RHESSysRuns/OutputChains-10Ch_s1200.yaml "$VAL"/RHESSysRuns/

#Copy the output workspace
cp ./RHESSysRuns/OutputWorkspace-10Ch_s1200.RData "$VAL"/RHESSysRuns/

#Move compiled output
mv ./LikeParamsQ_c_s1200.csv "$VAL"/
mv ./Q_c_s1200.txt "$VAL"/
mv ./AcceptReject_c_s1200.txt "$VAL"/
mv ./Run_JoinDREAM-10Ch-s1200.out "$VAL"/
mv ./RHESSysDREAM-A10-s1200.out "$VAL"/
mv ./ParamsLikes_c_s1200.csv "$VAL"/
mv ./Params_c_s1200.csv "$VAL"/