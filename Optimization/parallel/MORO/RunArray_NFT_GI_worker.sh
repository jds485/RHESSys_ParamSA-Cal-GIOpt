# Command arguments
#1: NFE
#2: i (parameterization)
#3: master number
N=$1
i=$2
M=$3
W=$4

module load singularity gcc/7.1.0 openmpi/3.1.4 R/3.5.3

#Set directory variables
SINGIMAGE='/share/resources/containers/singularity/rhessys'
BASEDIR='/scratch/js4yd/Bais910Hill30mOptimizeGI_MORO'
RLIBPATH='/home/js4yd/R/x86_64-pc-linux-gnu-library/3.5'

#Set variables that don't change in loop
RESOLUTION=30 #spatial resolution (meters) of the grids
RHESSysNAME='RHESSys_Baisman30m_g74'
LOCATION_NAME="g74_$RHESSysNAME"
MAPSET=PERMANENT
RHESSysModelLoc="/scratch/js4yd/RHESSysEastCoast_Optimized"

# full path to the replicate location
PROJDIR="$BASEDIR"/RHESSysRuns/Run"$N"_P"$i"_M"$M"
#Make new directory for the replicate
mkdir "$PROJDIR"
#Make the directory in which the RHESSys simulation will be run
mkdir "$PROJDIR"/"$RHESSysNAME"

#Move the decision variables to this directory
mv ./n"$N"_i"$i"_m"$M".txt "$PROJDIR"/"$RHESSysNAME"

#Copy folders and files for RHESSys to "$PROJDIR"/"$RHESSysNAME"
cp -r "$BASEDIR"/"$RHESSysNAME"/worldfiles "$PROJDIR"/"$RHESSysNAME"
cp -r "$BASEDIR"/"$RHESSysNAME"/tecfiles "$PROJDIR"/"$RHESSysNAME"
cp -r "$BASEDIR"/"$RHESSysNAME"/output "$PROJDIR"/"$RHESSysNAME"
cp -r "$BASEDIR"/"$RHESSysNAME"/flows "$PROJDIR"/"$RHESSysNAME"
mkdir "$PROJDIR"/"$RHESSysNAME"/defs
cp "$BASEDIR"/"$RHESSysNAME"/defs"$i"/* "$PROJDIR"/"$RHESSysNAME"/defs/
cp -r "$BASEDIR"/"$RHESSysNAME"/clim "$PROJDIR"/"$RHESSysNAME"
cp "$BASEDIR"/"$RHESSysNAME"/lulcFrac30m.csv "$PROJDIR"/"$RHESSysNAME"
cp "$BASEDIR"/"$RHESSysNAME"/MaxGI30m910.csv "$PROJDIR"/"$RHESSysNAME"

#Make directories that RHESSys needs and copy only the required files into them
GISDBASE="$BASEDIR"/grass_dataset_m"$M"_w"$W"_i"$i"
#Check if the GISDBASE already exists
if [[ -d "$GISDBASE" ]]
then
  #Delete the lulc directory that was created last iteration
  rm -r "$GISDBASE"/lulcRAW
else
  #Doesn't already exist. Make new directory and copy in files
  mkdir "$GISDBASE"
  cp -r "$BASEDIR"/grass_dataset/"$LOCATION_NAME" "$GISDBASE"
  #copy RHESSys executable into this directory
  cp "$RHESSysModelLoc"/rhessys5.20.0.develop_optimsize "$GISDBASE"
  #copy lulc file
  mkdir "$GISDBASE"/raw_data
  cp "$BASEDIR"/raw_data/BARN_1mLC_UTM.tif "$GISDBASE"/raw_data
fi

mkdir "$PROJDIR"/GIS2RHESSys
cp "$BASEDIR"/GIS2RHESSys/lulcCollectionEC_Cal.csv "$PROJDIR"/GIS2RHESSys
cp "$BASEDIR"/GIS2RHESSys/soilCollection_Cal910.csv "$PROJDIR"/GIS2RHESSys
cp "$BASEDIR"/GIS2RHESSys/vegCollection_modified_Opt910.csv "$PROJDIR"/GIS2RHESSys
cp "$BASEDIR"/GIS2RHESSys/lulc_1m_Chesapeake_Conservancy.csv "$PROJDIR"/GIS2RHESSys
#The vegetation file needs to be modified by the def file data
#Run the vegetation modification script
#1: vegetation csv file starting location 
#2: desired output file location
#3: def file directory
#4: csv file name
singularity exec "$SINGIMAGE"/rhessys_v3.img python "$BASEDIR"/RHESSysRuns/ModifyVeg.py "$PROJDIR"/GIS2RHESSys "$PROJDIR"/"$RHESSysNAME" "$PROJDIR"/"$RHESSysNAME"/defs 'vegCollection_modified_Opt910.csv'

GITHUBLIBRARIES="$PROJDIR"/GIS2RHESSys/libraries
mkdir "$GITHUBLIBRARIES"
cp "$BASEDIR"/GIS2RHESSys/libraries/g2w_cf_RHESSysEC.R "$GITHUBLIBRARIES"
cp "$BASEDIR"/GIS2RHESSys/libraries/LIB_RHESSys_writeTable2World.R "$GITHUBLIBRARIES"
cp "$BASEDIR"/GIS2RHESSys/libraries/aggregate_lulcFrac_write2GIS.R "$GITHUBLIBRARIES"

mkdir "$PROJDIR"/Date_analysis
cp "$BASEDIR"/Date_analysis/climate_extension.R "$PROJDIR"/Date_analysis
cp "$BASEDIR"/Date_analysis/LIB_dailytimeseries3.R "$PROJDIR"/Date_analysis
cp "$BASEDIR"/Date_analysis/LIB_misc.r "$PROJDIR"/Date_analysis

mkdir "$PROJDIR"/GIAllocation
cp "$BASEDIR"/GIAllocation/AllocateGI_910_Rivanna.R "$PROJDIR"/GIAllocation
cp "$BASEDIR"/GIAllocation/CalcObjs.R "$PROJDIR"/GIAllocation

##################################################################################
# 1.2
LOCATION="$GISDBASE"/$LOCATION_NAME

###################################################################################
## 1.9 Extract Land Use and Land Cover Information
downloadedLULCfile="$GISDBASE"/raw_data/BARN_1mLC_UTM.tif
singularity exec "$SINGIMAGE"/rhessys_v3.img grass74 "$LOCATION"/$MAPSET --exec r.in.gdal -e --overwrite input="$downloadedLULCfile" output=lulcRAW location=lulcRAW
LOCATIONLULC="$GISDBASE"/lulcRAW
singularity exec "$SINGIMAGE"/rhessys_v3.img grass74 "$LOCATIONLULC"/$MAPSET --exec v.proj location=$LOCATION_NAME mapset=$MAPSET input=patch output=patch$RESOLUTION'm'
singularity exec "$SINGIMAGE"/rhessys_v3.img grass74 "$LOCATIONLULC"/$MAPSET --exec v.to.rast input=patch$RESOLUTION'm' output=patch$RESOLUTION'm' use=attr attribute_column=value

#Modify the lulcFrac30m.csv file
#Arguments:
#1: random seed = i
#2: csv file describing the maximum GI allocation for each patch ('MaxGI30m.csv')
#3: csv file with LULC for every patch ('lulcFrac30m.csv')
#4: directory to files #2 and #3 ("$PROJDIR"/"$RHESSysNAME")
#5: Number of hillslopes whose patches can have GI (2)
#6: Number of decision variables for each hillslope (3)
#7: Output filename ('lulcFrac30m_GI.csv')
#8: GI resolution (side of a square), in meters ('9')
#9: Total patch area, m2 ('900')
#10: Decision Variable file name
seed=$(expr $i + 19 \* $N + $M \* 10000)
singularity exec "$SINGIMAGE"/rhessys_v3.img Rscript "$PROJDIR"/GIAllocation/AllocateGI_910_Rivanna.R "$seed" 'MaxGI30m910.csv' 'lulcFrac30m.csv' "$PROJDIR"/"$RHESSysNAME" '2' '3' 'lulcFrac30m_GI.csv' '9' '900' n"$N"_i"$i"_m"$M".txt

#Remove the old lulc file
rm "$PROJDIR"/"$RHESSysNAME"/lulcFrac30m.csv

#Make new lulcFrac GIS files
singularity exec "$SINGIMAGE"/rhessys_v3.img grass74 "$LOCATION"/$MAPSET --exec Rscript "$GITHUBLIBRARIES"/aggregate_lulcFrac_write2GIS.R patch "$PROJDIR"/"$RHESSysNAME"/lulcFrac$RESOLUTION'm_GI.csv' "$PROJDIR"/GIS2RHESSys/lulc_1m_Chesapeake_Conservancy.csv "$RLIBPATH"

###################################################################################
## 1.10 Assign IDs to land uses
#Will need to add all vegetation IDs here

#Note: vegetation ID for grass of 3
singularity exec "$SINGIMAGE"/rhessys_v3.img grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="grass1StratumID = if(lawnFrac>0,3,null())"
singularity exec "$SINGIMAGE"/rhessys_v3.img grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="grass1FFrac = if(lawnFrac>0,1.0,null())"
#Note: default LAI for grass of 1.5 - maximum in a growth season
singularity exec "$SINGIMAGE"/rhessys_v3.img grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="grass1LAI = if(lawnFrac>0,1.5,null())"

#Note: vegetation ID for trees of 102
singularity exec "$SINGIMAGE"/rhessys_v3.img grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="tree1StratumID = if(forestBaseFrac>0,102,null())"
singularity exec "$SINGIMAGE"/rhessys_v3.img grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="tree1FFrac = if(forestBaseFrac>0,1.0,null())"
#Note: default LAI for grass of 5.5 - maximum in a growth season
singularity exec "$SINGIMAGE"/rhessys_v3.img grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="tree1LAI = if(forestBaseFrac>0,5.5,null())"

#Note: GI #1 - Evergreen tree with fixed parameters, vegetation ID of 414
singularity exec "$SINGIMAGE"/rhessys_v3.img grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="tree2StratumID = if(forestGIEvFrac>0,414,null())"
singularity exec "$SINGIMAGE"/rhessys_v3.img grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="tree2FFrac = if(forestGIEvFrac>0,1.0,null())"
#Note: default LAI for grass of 5.5 - maximum in a growth season
singularity exec "$SINGIMAGE"/rhessys_v3.img grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="tree2LAI = if(forestGIEvFrac>0,5.5,null())"

#Make the g2W_template file and then make the worldfile
##################################################################################
## 2.1 Specify output information for RHESSys input files that GIS2RHESSys outputs
templateFile="$PROJDIR"/"$RHESSysNAME"/g2w_template.txt

# set paths for RHESSys input files
# 1 = Yes, output this file; 0 = No, do not output this file
echo outputWorldfile \""$PROJDIR"/"$RHESSysNAME"/worldfiles/worldfile.csv\" 1 > "$templateFile"
echo outputWorldfileHDR \""$PROJDIR"/"$RHESSysNAME"/worldfiles/worldfile.hdr\" 1 >> "$templateFile"
echo outputDefs \""$PROJDIR"/"$RHESSysNAME"/defs\" 0 >> "$templateFile"
echo outputSurfFlow \""$PROJDIR"/"$RHESSysNAME"/flows/surfflow.txt\" 0 >> "$templateFile"
echo outputSubFlow \""$PROJDIR"/"$RHESSysNAME"/flows/subflow.txt\" 0 >> "$templateFile"

# set climate station ID and file name
echo stationID 101 >> "$templateFile"
echo stationFile \""clim/Cal_Feb2020Revised.base"\" >> "$templateFile"

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
singularity exec "$SINGIMAGE"/rhessys_v3.img grass74 "$LOCATION"/$MAPSET --exec Rscript "$GITHUBLIBRARIES"/g2w_cf_RHESSysEC.R "$PROJDIR" "$PROJDIR"/"$RHESSysNAME"/vegCollection_modified_Opt910.csv "$PROJDIR"/GIS2RHESSys/soilCollection_Cal910.csv "$PROJDIR"/GIS2RHESSys/lulcCollectionEC_Cal.csv "$templateFile" "$RLIBPATH"

singularity exec "$SINGIMAGE"/rhessys_v3.img Rscript "$GITHUBLIBRARIES"/LIB_RHESSys_writeTable2World.R NA "$PROJDIR"/"$RHESSysNAME"/worldfiles/worldfile.csv "$PROJDIR"/"$RHESSysNAME"/worldfiles/worldfile "$RLIBPATH"

#Delete the GRASS directory and code libraries for space concerns
rm -r "$PROJDIR"/Date_analysis
rm -r "$PROJDIR"/GIS2RHESSys
rm "$PROJDIR"/"$RHESSysNAME"/g2w_template.txt
rm "$PROJDIR"/"$RHESSysNAME"/MaxGI30m910.csv

#Run RHESSys
ODIR=$(exec pwd)
cd "$PROJDIR"/"$RHESSysNAME"
"$GISDBASE"/rhessys5.20.0.develop_optimsize -st 1999 11 15 1 -ed 2013 10 1 1 -b -h -newcaprise -gwtoriparian -capMax 0.01 -slowDrain -t tecfiles/tec_daily_cal.txt -w worldfiles/worldfile -whdr worldfiles/worldfile.hdr -r flows/subflow.txt flows/surfflow.txt -pre output/Run"$N"_P"$i"_M"$M" -s 1 1 1 -sv 1 1 -gw 1 1 -svalt 1 1 -vgsen 1 1 1 -snowTs 1 -snowEs 1 -capr 0.001

#Remove the flows, tecfiles, and clim folders.
rm -r ./flows
rm -r ./clim
rm -r ./tecfiles
rm -r ./worldfiles
#Remove some un-needed output
rm ./output/Run"$N"_P"$i"_M"$M"_basin.hourly
rm ./output/Run"$N"_P"$i"_M"$M"_basin.monthly
rm ./output/Run"$N"_P"$i"_M"$M"_basin.yearly
rm ./output/Run"$N"_P"$i"_M"$M"_hillslope.hourly
rm ./output/Run"$N"_P"$i"_M"$M"_hillslope.monthly
rm ./output/Run"$N"_P"$i"_M"$M"_hillslope.yearly
rm ./output/*.params

#Process RHESSys output and compute objectives
#1: NFE
#2: i (parameterization)
#3: master number
#4: directory of simulated streamflow file
#5: random seed
#6: full path to the likelihood parameter csv file for the ith parameterization
#7: modified lulcFrac file
#8: directory to place a copy of the objectives for Borg to read
#9: number of error timeseries to draw (1 for synthetic, 1000 for not)
if [[ $i -eq 0 ]]
then
  REP=1
  seed2=157
else
  REP=1000
  seed2=$(expr 2000 + $i + 19 \* $N + $M \* 10000)
fi
Rscript ../GIAllocation/CalcObjs.R "$N" "$i" "$M" ./output "$seed2" ./defs/Params_logLQ_"$i".csv ./lulcFrac30m_GI.csv "$ODIR" "$REP"

rm -r ./defs
rm -r ./vegCollection_modified_Opt910.csv
rm -r ../GIAllocation

cd "$ODIR"