#!/bin/bash
#SBATCH -D /scratch/js4yd/Bais910Hill30mDREAMzs-10Ch
#SBATCH -o /scratch/js4yd/Bais910Hill30mDREAMzs-10Ch/RunHill910_Syn.out
#SBATCH --ntasks=1                              # Number of tasks per serial job (must be 1)
#SBATCH -p standard           									# Queue partition name "standard"
#SBATCH -A quinnlab       											# allocation name
#SBATCH -t 00:30:00       											# Run time per serial job (hh:mm:ss) - up to 36 hours
#SBATCH --mail-user=js4yd@virginia.edu          # address for email notification
#SBATCH --mail-type=ALL                  		    # email at Begin and End of job

module purge

#Set directory variables
SINGIMAGE='/share/resources/containers/singularity/rhessys'
BASEDIR='/scratch/js4yd/Bais910Hill30mDREAMzs-10Ch'
RLIBPATH='/home/js4yd/R/x86_64-pc-linux-gnu-library/3.5'

#Set variables that don't change in loop
EPSGCODE='EPSG:26918'
RESOLUTION=30 #spatial resolution (meters) of the grids
RHESSysNAME='RHESSys_Baisman30m_g74' # e.g., rhessys_baisman10m
LOCATION_NAME="g74_$RHESSysNAME"
MAPSET=PERMANENT
RHESSysModelLoc="/scratch/js4yd/RHESSysEastCoast_Optimized"

cd "$BASEDIR"/RHESSysRuns
singularity exec "$SINGIMAGE"/rhessys_v3.img python 'MakeDefs_fn_Chains.py' 'Bais910_Syn_AfterProcessing.csv' "$BASEDIR"/RHESSysRuns "$BASEDIR"/"$RHESSysNAME"/defs "$RHESSysNAME" 'BaismanCalibrationParameterProblemFile_NewGWBounds.csv' '10' '1' '1'

#Change into directory that was just made and copy in files needed to run RHESSys
# full path to the project location;
PROJDIR="$BASEDIR"/RHESSysRuns/Run1_Ch1
cd "$PROJDIR"

#Copy other folders and files for RHESSys to ./"$RHESSysNAME"
cp -r "$BASEDIR"/"$RHESSysNAME"/worldfiles ./"$RHESSysNAME"
cp -r "$BASEDIR"/"$RHESSysNAME"/tecfiles ./"$RHESSysNAME"
cp -r "$BASEDIR"/"$RHESSysNAME"/output ./"$RHESSysNAME"
cp -r "$BASEDIR"/"$RHESSysNAME"/flows ./"$RHESSysNAME"
cp -r "$BASEDIR"/"$RHESSysNAME"/clim ./"$RHESSysNAME"

#Run RHESSys
cd "$PROJDIR"/"$RHESSysNAME"
$RHESSysModelLoc/rhessys5.20.0.develop_optimsize -st 1999 11 15 1 -ed 2013 10 1 1 -b -h -newcaprise -gwtoriparian -capMax 0.01 -slowDrain -t tecfiles/tec_daily_cal.txt -w worldfiles/worldfile -whdr worldfiles/worldfile.hdr -r flows/subflow.txt flows/surfflow.txt -pre output/Run -s 1 1 1 -sv 1 1 -gw 1 1 -svalt 1 1 -vgsen 1 1 1 -snowTs 1 -snowEs 1 -capr 0.001

#Remove some un-needed output
rm "$PROJDIR"/"$RHESSysNAME"/output/Run_basin.hourly
rm "$PROJDIR"/"$RHESSysNAME"/output/Run_basin.monthly
rm "$PROJDIR"/"$RHESSysNAME"/output/Run_basin.yearly
rm "$PROJDIR"/"$RHESSysNAME"/output/Run_hillslope.hourly
rm "$PROJDIR"/"$RHESSysNAME"/output/Run_hillslope.monthly
rm "$PROJDIR"/"$RHESSysNAME"/output/Run_hillslope.yearly