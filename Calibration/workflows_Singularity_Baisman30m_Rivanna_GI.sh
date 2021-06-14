#!/bin/bash

#Fixme: This script should be submitted as a SLURM job

# STEP 1: after login to Rivanna, you need to manually type in the following command to use the singularity image.
# singularity shell /share/resources/containers/singularity/rhessys/rhessys_v3.img
# STEP 2: in the singularity image, run command: sh workflows_Singularity_Baisman30m_Rivanna_GI.sh
# STEP 3: wait and check for errors.
# STEP 4: after this GI setup, you can run RHESSys models using a different shell script.

##################################################################################
# 1.1 Set directory locations
#Should download these libraries to computer and not download every run. If you want to use the library from GitHub, use the commented-out command.
#GITHUBLIBRARIES="https://raw.githubusercontent.com/laurencelin/GIS2RHESSys/master/libraries"
GITHUBLIBRARIES='/scratch/js4yd/TestRHESSysNewGIScripts/GIS2RHESSys/libraries'
SSURGOLIBRARIES='/scratch/js4yd/TestRHESSysNewGIScripts/ssurgo_extraction'
RLIBPATH='/home/js4yd/R/x86_64-pc-linux-gnu-library/3.5'

##################################################################################
# 1.2 Set Desired File Names
# full Linux path to the project location. Folders will be created in this directory, and this directory should have the raw_data in it.
PROJDIR='/scratch/js4yd/TestRHESSysNewGIScripts'

#Set the desired projection system
EPSGCODE='EPSG:26918'

#Desired spatial resolution (meters) of the raster grid cells
RESOLUTION=30 

#Location of GIS data to be generated
GISDBASE="$PROJDIR"/grass_dataset

#Set the name of the directory in folder for use in GRASS
MAPSET=PERMANENT

#Name of the folder that contains the RHESSys setup information
RHESSysNAME='RHESSys_Baisman30m_g74' # e.g., rhessys_baisman10m

#Names of folders for GRASS data for this iteration
LOCATION_NAME="g74_$RHESSysNAME"
LOCATION="$GISDBASE"/$LOCATION_NAME

##################################################################################
# 1.3 Gather the raw data DEM, and resample to defined grid resolution.

##################################################################################
# 1.4 Define the catchment outlet location and create a point for that location.

##################################################################################
# 1.5 Delineate Watershed with Grass and R scripts

##################################################################################
## 1.6 Use cluster analysis of slope and aspect to determine zones

####################################################################################
## 1.7 Calculate isohyets

####################################################################################
## 1.8 Evaluate soil information

###################################################################################
## 1.9 Extract Land Use and Land Cover Information
#Assuming there is already a folder lulcRAW, it should be deleted and recreated
#LOCATIONLULC="$GISDBASE"/lulcRAW
#rm -r "$LOCATIONLULC"

downloadedLULCfile="$PROJDIR"/'raw_data'/'BARN_1mLC_UTM.tif'
grass74 "$LOCATION"/$MAPSET --exec r.in.gdal -e --overwrite input="$downloadedLULCfile" output=lulcRAW location=lulcRAW
LOCATIONLULC="$GISDBASE"/lulcRAW
grass74 "$LOCATIONLULC"/$MAPSET --exec v.proj location=$LOCATION_NAME mapset=$MAPSET input=patch output=patch$RESOLUTION'm'
grass74 "$LOCATIONLULC"/$MAPSET --exec v.to.rast input=patch$RESOLUTION'm' output=patch$RESOLUTION'm' use=attr attribute_column=value

#Assuming that a modified lulcFrac file has been made, this is from where the script would need to be re-run.
grass74 "$LOCATION"/$MAPSET --exec Rscript "$GITHUBLIBRARIES"/aggregate_lulcFrac_write2GIS.R patch "$PROJDIR"/"$RHESSysNAME"/lulcFrac$RESOLUTION'm.csv' "$PROJDIR"/GIS2RHESSys/lulc_1m_Chesapeake_Conservancy.csv "$RLIBPATH"

###################################################################################
## 1.10 Assign IDs to land uses
#Will need to add all vegetation IDs here

#Note: vegetation ID for grass of 3
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="grass1StratumID = if(lawnFrac>0,3,null())"
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="grass1FFrac = if(lawnFrac>0,1.0,null())"
#Note: default LAI for grass of 1.5 - maximum in a growth season
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="grass1LAI = if(lawnFrac>0,1.5,null())"

#Note: vegetation ID for trees of 102
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="tree1StratumID = if(forestBaseFrac>0,102,null())"
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="tree1FFrac = if(forestBaseFrac>0,1.0,null())"
#Note: default LAI for grass of 5.5 - maximum in a growth season
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="tree1LAI = if(forestBaseFrac>0,5.5,null())"

#Note: GI #1 - Evergreen tree with fixed parameters, vegetation ID of 414
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="tree2StratumID = if(forestGIEvFrac>0,414,null())"
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="tree2FFrac = if(forestGIEvFrac>0,1.0,null())"
#Note: default LAI for grass of 5.5 - maximum in a growth season
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="tree2LAI = if(forestGIEvFrac>0,5.5,null())"

###################################################################################
## 1.11 Add road information

###################################################################################
## 1.12 Analyze elevation flowpaths

###################################################################################
## 1.13 Evaluate infrastructure: storm drains, sewer line locations (based on road locations), and compactness of soil (based on house locations)

###################################################################################
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
#
##################################################################################
# 2.2
grass74 "$LOCATION"/$MAPSET --exec Rscript "$GITHUBLIBRARIES"/g2w_cf_RHESSysEC.R "$PROJDIR" '/scratch/js4yd/TestRHESSysNewGIScripts/GIS2RHESSys/vegCollection_modified_Opt.csv' '/scratch/js4yd/TestRHESSysNewGIScripts/GIS2RHESSys/soilCollection_Cal.csv' '/scratch/js4yd/TestRHESSysNewGIScripts/GIS2RHESSys/lulcCollectionEC_Cal.csv' "$templateFile" "$RLIBPATH"

Rscript "$GITHUBLIBRARIES"/LIB_RHESSys_writeTable2World.R NA "$PROJDIR"/"$RHESSysNAME"/worldfiles/worldfile.csv "$PROJDIR"/"$RHESSysNAME"/worldfiles/worldfile "$RLIBPATH"