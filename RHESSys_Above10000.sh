#!/bin/bash
#SBATCH -D /scratch/js4yd/MorrisSA/RHESSysRuns/
#SBATCH -o /scratch/js4yd/MorrisSA/RHESSysRuns/output/Run_P9999_%a.out
#SBATCH --ntasks=1                              # Number of tasks per serial job (must be 1)
#SBATCH -p standard           									# Queue partition name "standard"
#SBATCH -A quinnlab       											# allocation name
#SBATCH -t 01:00:00       											# Run time per serial job (hh:mm:ss) - up to 36 hours
#SBATCH --mem-per-cpu=12288                     # Memory per cpu (Megabytes)
#SBATCH --mail-user=js4yd@virginia.edu          # address for email notification
#SBATCH --mail-type=ALL                  		    # email at Begin and End of job

#This shell script is for running only RHESSys. Useful if a run fails during a RHESSys simulation.

module load singularity

#Set directory variables
SINGIMAGE='/share/resources/containers/singularity/rhessys'
BASEDIR='/scratch/js4yd/MorrisSA'

#Set variables that don't change in loop
EPSGCODE='EPSG:26918'
RESOLUTION=30 #spatial resolution (meters) of the grids
RHESSysNAME='RHESSys_Baisman30m_g74' # e.g., rhessys_baisman10m
LOCATION_NAME="g74_$RHESSysNAME"
MAPSET=PERMANENT
RHESSysModelLoc="/scratch/js4yd/RHESSysEastCoast"

#Set the index, i, to be the task ID minus 1
SUBONE=9999
i=$(expr ${SLURM_ARRAY_TASK_ID} + $SUBONE) 

PROJDIR="$BASEDIR"/RHESSysRuns/Run"$i" 

#Run RHESSys
cd "$PROJDIR"/"$RHESSysNAME"
"$RHESSysModelLoc"/rhessys5.20.0.develop -st 1999 11 15 1 -ed 2010 10 1 1 -b -h -newcaprise -gwtoriparian -capMax 0.01 -slowDrain -t tecfiles/tec_daily_SA.txt -w worldfiles/worldfile -whdr worldfiles/worldfile.hdr -r flows/subflow.txt flows/surfflow.txt -pre output/BaismanRun"$i" -s 1 1 1 -sv 1 1 -gw 1 1 -svalt 1 1 -vgsen 1 1 1 -snowTs 1 -snowEs 1 -capr 0.001

#Remove the flows, tecfiles, and clim folders.
rm -r "$PROJDIR"/"$RHESSysNAME"/flows
rm -r "$PROJDIR"/"$RHESSysNAME"/clim
rm -r "$PROJDIR"/"$RHESSysNAME"/tecfiles