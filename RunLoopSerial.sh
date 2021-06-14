#!/bin/bash
#SBATCH -D /scratch/js4yd/MorrisSA/RHESSysRuns/
#SBATCH -o /scratch/js4yd/MorrisSA/RHESSysRuns/output/Run_P9999_373.out
#SBATCH -N 1            												# Total number of nodes to request (up to 120)
#SBATCH -n 1           													# Number of processors per node (up to 20)
#SBATCH -p standard           									# Queue partition name "standard"
#SBATCH -A quinnlab       											# allocation name
#SBATCH -t 01:00:00       											# Run time (hh:mm:ss) - up to 36 hours
#SBATCH --mail-user=js4yd@virginia.edu          # address for email notification
#SBATCH --mail-type=ALL                  		    # email at Begin and End of job

#This shell script is used for running one replicate at a time.

module load singularity

#Set directory variables
SINGIMAGE='/share/resources/containers/singularity/rhessys'
BASEDIR='/scratch/js4yd/MorrisSA'
RLIBPATH='/home/js4yd/R/x86_64-pc-linux-gnu-library/3.5'

#Set variables that don't change in loop
EPSGCODE='EPSG:26918'
RESOLUTION=30 #spatial resolution (meters) of the grids
RHESSysNAME='RHESSys_Baisman30m_g74' # e.g., rhessys_baisman10m
LOCATION_NAME="g74_$RHESSysNAME"
MAPSET=PERMANENT
RHESSysModelLoc="/scratch/js4yd/RHESSysEastCoast"

i=10372

cd "$BASEDIR"/RHESSysRuns
#Use the MakeDefs_fn.py script to make the ith replicate directory and the defs directory with the ith def file combination
#Execute using the version of Python in the singularity container
singularity exec "$SINGIMAGE"/rhessys_v3.img python "$BASEDIR"/RHESSysRuns/MakeDefs_fn.py "$i" "$BASEDIR"/RHESSysRuns "$BASEDIR"/"$RHESSysNAME"/defs "$RHESSysNAME" 'BaismanMorrisSamplingProblemFile_Full.csv' '10'

#Change into directory that was just made and copy in files needed to run RHESSys
# full path to the project location;
PROJDIR="$BASEDIR"/RHESSysRuns/Run"$i" 
cd "$PROJDIR"
#Make directories that RHESSys needs and copy only the required files into them
mkdir ./grass_dataset
cp -r "$BASEDIR"/grass_dataset/"$LOCATION_NAME" ./grass_dataset

mkdir ./GIS2RHESSys
cp "$BASEDIR"/GIS2RHESSys/lulcCollectionEC_SA.csv ./GIS2RHESSys
cp "$BASEDIR"/GIS2RHESSys/soilCollection_SA.csv ./GIS2RHESSys
cp "$BASEDIR"/GIS2RHESSys/vegCollection_modified_SA.csv ./GIS2RHESSys
#The vegetation file needs to be modified by the def file data
#Run the vegetation modification script
singularity exec "$SINGIMAGE"/rhessys_v3.img python "$BASEDIR"/RHESSysRuns/ModifyVeg.py "$BASEDIR"/RHESSysRuns/Run"$i"/GIS2RHESSys "$PROJDIR"/"$RHESSysNAME" "$PROJDIR"/"$RHESSysNAME"/defs 'vegCollection_modified_SA.csv'

cd "$PROJDIR"
mkdir ./GIS2RHESSys/libraries
cp "$BASEDIR"/GIS2RHESSys/libraries/g2w_cf_RHESSysEC.R ./GIS2RHESSys/libraries
cp "$BASEDIR"/GIS2RHESSys/libraries/LIB_RHESSys_writeTable2World.R ./GIS2RHESSys/libraries
mkdir ./Date_analysis
cp "$BASEDIR"/Date_analysis/climate_extension.R ./Date_analysis
cp "$BASEDIR"/Date_analysis/LIB_dailytimeseries3.R ./Date_analysis
cp "$BASEDIR"/Date_analysis/LIB_misc.r ./Date_analysis

#Copy folders and files to ./"$RHESSysNAME"
cp -r "$BASEDIR"/"$RHESSysNAME"/worldfiles ./"$RHESSysNAME"
cp -r "$BASEDIR"/"$RHESSysNAME"/tecfiles ./"$RHESSysNAME"
cp -r "$BASEDIR"/"$RHESSysNAME"/output ./"$RHESSysNAME"
cp -r "$BASEDIR"/"$RHESSysNAME"/flows ./"$RHESSysNAME"
cp -r "$BASEDIR"/"$RHESSysNAME"/clim ./"$RHESSysNAME"
cp "$BASEDIR"/"$RHESSysNAME"/soil_cat_mukey.csv ./"$RHESSysNAME"
cp "$BASEDIR"/"$RHESSysNAME"/lulcFrac30m.csv ./"$RHESSysNAME"

#Make the g2W_template file and then make the worldfile
##################################################################################
# 1.2
GITHUBLIBRARIES="$BASEDIR"/RHESSysRuns/Run"$i"/GIS2RHESSys/libraries
GISDBASE="$PROJDIR"/grass_dataset
LOCATION="$GISDBASE"/$LOCATION_NAME
##################################################################################
## 2.1 Specify output information for RHESSys input files that GIS2RHESSys outputs
templateFile="$PROJDIR"/"$RHESSysNAME"/g2w_template.txt

# set paths for RHESSys input files
# 1 = Yes, output this file; 0 = No, do not output this file
echo outputWorldfile \""$PROJDIR"/"$RHESSysNAME"/worldfiles/worldfile.csv\" 1 > "$templateFile"
echo outputWorldfileHDR \""$PROJDIR"/"$RHESSysNAME"/worldfiles/worldfile.hdr\" 0 >> "$templateFile"
echo outputDefs \""$PROJDIR"/"$RHESSysNAME"/defs\" 0 >> "$templateFile"
echo outputSurfFlow \""$PROJDIR"/"$RHESSysNAME"/flows/surfflow.txt\" 0 >> "$templateFile"
echo outputSubFlow \""$PROJDIR"/"$RHESSysNAME"/flows/subflow.txt\" 0 >> "$templateFile"

# set climate station ID and file name
echo stationID 101 >> "$templateFile"
echo stationFile \""clim/Oregon.base"\" >> "$templateFile"

# the following maps that must be provided with syntex:
# echo keyword <map> >> "$templateFile"
echo basinMap basin >> "$templateFile"
echo hillslopeMap hill >> "$templateFile"
echo zoneMAP zone_cluster >> "$templateFile"
echo patchMAP patch >> "$templateFile"
echo soilidMAP soil_texture >> "$templateFile"
echo xMAP xmap >> "$templateFile"
echo yMAP ymap >> "$templateFile"
echo demMAP dem >> "$templateFile"
echo slopeMap slope >> "$templateFile"
echo aspectMAP aspect >> "$templateFile"
echo twiMAP wetness_index >> "$templateFile"
echo whorizonMAP west_180 >> "$templateFile"
echo ehorizonMAP east_000 >> "$templateFile"
echo isohyetMAP isohyet >> "$templateFile"
echo rowMap rowmap >> "$templateFile"
echo colMap colmap >> "$templateFile"
echo drainMap drain >> "$templateFile"
# ... impervious and its breakdown
echo impFracMAP impFrac >> "$templateFile"
echo roofMAP roofFrac >> "$templateFile"
echo drivewayMAP drivewayFrac >> "$templateFile"
echo pavedRoadFracMAP pavedroadFrac >> "$templateFile"
# ... forest vegetations
echo forestFracMAP forestFrac >> "$templateFile"
echo forestBaseFracMAP forestBaseFrac >> "$templateFile"
echo forestGIEvFracMAP forestGIEvFrac >> "$templateFile"
echo tree1StratumID tree1StratumID >> "$templateFile"
echo tree1FFrac tree1FFrac >> "$templateFile"
echo tree1LAI tree1LAI >> "$templateFile"
echo tree2StratumID tree2StratumID >> "$templateFile"
echo tree2FFrac tree2FFrac >> "$templateFile"
echo tree2LAI tree2LAI >> "$templateFile"
# ... shrub vegetation
echo shrubFracMAP shrubFrac >> "$templateFile"
#echo shrub1StratumID shrub1StratumID >> "$templateFile"
#echo shrub1FFrac shrub1FFrac >> "$templateFile"
#echo shrub1LAI shrub1LAI >> "$templateFile"
# ... crop vegetation
echo cropFracMAP cropFrac >> "$templateFile"
#echo crop1StratumID crop1StratumID >> "$templateFile"
#echo crop1FFrac crop1FFrac >> "$templateFile"
#echo crop1LAI crop1LAI >> "$templateFile"
# ... lawn/pasture vegetation
echo grassFracMAP lawnFrac >> "$templateFile"
echo grass1StratumID grass1StratumID >> "$templateFile"
echo grass1FFrac grass1FFrac >> "$templateFile"
echo grass1LAI grass1LAI >> "$templateFile"
# ... modeling stream-grids
echo streamMap str >> "$templateFile"
#
# The following maps are optional; User can comment out the lines that do not apply using "#" up front.
echo streamFullExtension strExt >> "$templateFile"
#echo unpavedroadMap NA >> "$templateFile"
echo riparianMAP riparian_hands >> "$templateFile"
#echo sewercoverMAP sewercover >> "$templateFile"
echo septicMAP septic >> "$templateFile"
#echo pipecoverMAP NA >> "$templateFile"
echo stormdrainMAP roadExit >> "$templateFile"
echo compactedsoilMAP compactedsoil >> "$templateFile"
echo additionalSurfaceDrainMAP addsurfdrain >> "$templateFile"
##################################################################################
# 2.2
# assumes you have saved the modified vegCollection.csv file on Rivanna @ location "$PROJDIR"/"$RHESSysNAME"/
cd "$PROJDIR"
singularity exec "$SINGIMAGE"/rhessys_v3.img grass74 "$LOCATION"/$MAPSET --exec Rscript "$GITHUBLIBRARIES"/g2w_cf_RHESSysEC.R "$PROJDIR" "$PROJDIR"/"$RHESSysNAME"/vegCollection_modified_SA.csv "$PROJDIR"/GIS2RHESSys/soilCollection_SA.csv "$PROJDIR"/GIS2RHESSys/lulcCollectionEC_SA.csv "$templateFile" "$RLIBPATH"

singularity exec "$SINGIMAGE"/rhessys_v3.img Rscript "$GITHUBLIBRARIES"/LIB_RHESSys_writeTable2World.R NA "$PROJDIR"/"$RHESSysNAME"/worldfiles/worldfile.csv "$PROJDIR"/"$RHESSysNAME"/worldfiles/worldfile "$RLIBPATH"

#Delete the GRASS directory and code libraries for space concerns
rm -r "$PROJDIR"/grass_dataset
rm -r "$PROJDIR"/Date_analysis
rm -r "$PROJDIR"/GIS2RHESSys
rm "$PROJDIR"/"$RHESSysNAME"/lulcFrac30m.csv
rm "$PROJDIR"/"$RHESSysNAME"/soil_cat_mukey.csv
rm "$PROJDIR"/"$RHESSysNAME"/vegCollection_modified_SA.csv
rm "$PROJDIR"/"$RHESSysNAME"/g2w_template.txt

#Run RHESSys
cd "$PROJDIR"/"$RHESSysNAME"
"$RHESSysModelLoc"/rhessys5.20.0.develop -st 1999 11 15 1 -ed 2010 10 1 1 -b -h -newcaprise -gwtoriparian -capMax 0.01 -slowDrain -t tecfiles/tec_daily_SA.txt -w worldfiles/worldfile -whdr worldfiles/worldfile.hdr -r flows/subflow.txt flows/surfflow.txt -pre output/BaismanRun"$i" -s 1 1 1 -sv 1 1 -gw 1 1 -svalt 1 1 -vgsen 1 1 1 -snowTs 1 -snowEs 1 -capr 0.001

#Remove the flows, tecfiles, and clim folders.
rm -r "$PROJDIR"/"$RHESSysNAME"/flows
rm -r "$PROJDIR"/"$RHESSysNAME"/clim
rm -r "$PROJDIR"/"$RHESSysNAME"/tecfiles