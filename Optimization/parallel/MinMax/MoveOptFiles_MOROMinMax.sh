#!/bin/bash
#SBATCH -D /sfs/lustre/bahamut/scratch/js4yd/Bais910Hill30mOptGI_MOROMinMax/
#SBATCH -o /sfs/lustre/bahamut/scratch/js4yd/Bais910Hill30mOptGI_MOROMinMax/Run_MoveOpt-MORO.out
#SBATCH --ntasks=1                              # Number of tasks per serial job (must be 1)
#SBATCH -p standard           									# Queue partition name "standard"
#SBATCH -A quinnlab       											# allocation name
#SBATCH -t 08:00:00       											# Run time per serial job (hh:mm:ss) - up to 36 hours
#SBATCH --mail-user=js4yd@virginia.edu          # address for email notification
#SBATCH --mail-type=ALL                  		    # email at Begin and End of job

#Script to move files to permanent storage.
module purge

#Directory where files to be moved are located
cd /sfs/lustre/bahamut/scratch/js4yd/Bais910Hill30mOptGI_MOROMinMax/

#Directory to move into (must already exist with RHESSysRuns/output directory)
VAL='/project/quinnlab/js4yd/Bais910Hill30mOptGI_MOROMinMax'

#Move completed RHESSys run data. Must be done in increments
mv ./RHESSysRuns/Run9* "$VAL"/RHESSysRuns
mv ./RHESSysRuns/Run8* "$VAL"/RHESSysRuns
mv ./RHESSysRuns/Run7* "$VAL"/RHESSysRuns
mv ./RHESSysRuns/Run6* "$VAL"/RHESSysRuns
mv ./RHESSysRuns/Run5* "$VAL"/RHESSysRuns
mv ./RHESSysRuns/Run4* "$VAL"/RHESSysRuns
mv ./RHESSysRuns/Run3* "$VAL"/RHESSysRuns
mv ./RHESSysRuns/Run2* "$VAL"/RHESSysRuns
mv ./RHESSysRuns/Run* "$VAL"/RHESSysRuns

#Move output files. Must be done in increments
mv ./RHESSysRuns/output/n9* "$VAL"/RHESSysRuns/output
mv ./RHESSysRuns/output/n8* "$VAL"/RHESSysRuns/output
mv ./RHESSysRuns/output/n7* "$VAL"/RHESSysRuns/output
mv ./RHESSysRuns/output/n6* "$VAL"/RHESSysRuns/output
mv ./RHESSysRuns/output/n5* "$VAL"/RHESSysRuns/output
mv ./RHESSysRuns/output/n4* "$VAL"/RHESSysRuns/output
mv ./RHESSysRuns/output/n3* "$VAL"/RHESSysRuns/output
mv ./RHESSysRuns/output/n2* "$VAL"/RHESSysRuns/output
mv ./RHESSysRuns/output/* "$VAL"/RHESSysRuns/output