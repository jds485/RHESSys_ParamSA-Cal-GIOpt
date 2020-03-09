#!/bin/bash

# STEP 1: after login to Rivanna, you need to manually type in the following command to use the singularity image.
# singularity shell /share/resources/containers/singularity/rhessys/rhessys_v3.img
#
# STEP 2: create a working directory in /scratch/<user>/ space and upload the "raw_data" to it
# STEP 3: fill in the user information below and upload this script to Rivanna
# STEP 4: in the singularity image, run command: sh workflows_Baisman_30m_rivanna_fromScratch.sh
# STEP 5: wait and hope not to see errors; otherwise contact Laurence.
# STEP 6: after this fromScratch setup, you can do the Modification script.

##################################################################################
# 1.1 Set directory locations
#Should load this library to computer and not download every run. If you want to use the library from GitHub, use this commented command
#GITHUBLIBRARIES="https://raw.githubusercontent.com/laurencelin/GIS2RHESSys/master/libraries"
GITHUBLIBRARIES='/scratch/js4yd/MorrisSA/GIS2RHESSys/libraries'

#Should do this instead of using web to download below.
SSURGOLIBRARIES='/scratch/js4yd/MorrisSA/ssurgo_extraction'

##################################################################################
# 1.2 Set Desired File Names
# please fill in the <user>, <foldername>, <rhessysfoldername> below
PROJDIR='/scratch/js4yd/MorrisSA' # full Linux path to the project location; Folders will be created in this directory, and this directory should have the raw_data in it.

#Set the desired projection system (or is this the projection system that the input data are in?)
EPSGCODE='EPSG:26918'

#spatial resolution (meters) of the grids
RESOLUTION=30 

#Fixme: this may need to be duplicated for each of the models, instead of using just one.
#May just require that LOCATION below is what is referenced for each grass dataset, and therefore not need to be looped.
GISDBASE="$PROJDIR"/grass_dataset

#Set the name of the directory in folder for use in GRASS
MAPSET=PERMANENT


### ... create project folder structures
mkdir "$GISDBASE"
#mkdir "$PROJDIR"/raw_data ##<<--------------- upload by scp -r command to the "$PROJDIR"

### Set variables that are held constant in each loop iteration:
#Section 1.4
gageLat='39.47947' # catchment outlet WGS84 Lat (decimal degree)
gageLong='-76.67803' # catchment outlet WGS84 Long (decimal degree; includes the negative sign if applied)

#Section 1.5
#Set expected area of watershed, and stream extent thresholds
expectedDrainageArea=3807283 # meter squre
expectedThresholdModelStr=350000 # meter sq. # note to include Pond Branch as a channel; 390*900=351000
expectedThresholdStrExt=100000 # meter sq.
GRASS_thres=$(($expectedThresholdModelStr/$RESOLUTION/$RESOLUTION)) # grid cell for stream network and hillslope configuration
GRASS_thresII=$(($expectedThresholdStrExt/$RESOLUTION/$RESOLUTION))
GRASS_drainarea_lowerbound=$((98*$expectedDrainageArea/$RESOLUTION/$RESOLUTION/100)) # (allow 2% error)
GRASS_drainarea_upperbound=$((102*$expectedDrainageArea/$RESOLUTION/$RESOLUTION/100)) # (allow 2% error)

#Name of the folder that contains the RHESSys setup information
RHESSysNAME='RHESSys_Baisman30m_g74' # e.g., rhessys_baisman10m

#Names of folders for GRASS data for this iteration
LOCATION_NAME="g74_$RHESSysNAME"
LOCATION="$GISDBASE"/$LOCATION_NAME

### ... create rhessys folder structures for this iteration
mkdir "$PROJDIR"/"$RHESSysNAME"
mkdir "$PROJDIR"/"$RHESSysNAME"/defs
mkdir "$PROJDIR"/"$RHESSysNAME"/flows
mkdir "$PROJDIR"/"$RHESSysNAME"/worldfiles
mkdir "$PROJDIR"/"$RHESSysNAME"/clim
mkdir "$PROJDIR"/"$RHESSysNAME"/tecfiles
mkdir "$PROJDIR"/"$RHESSysNAME"/output

### ... create grass database (-c) in specified projection system and folder, and immediately exit grass (-e)
grass74 -c $EPSGCODE -e "$LOCATION"

##################################################################################
# 1.3 Gather the raw data DEM, and resample to defined grid resolution.
#Fixme: This would not be needed in the loop. DEM gets created once. Can check if file exists, and execute if it doesn't
downloadedDEMfile="$PROJDIR"/raw_data/BR_DEM1m_CBP_filled.tif
#Import raster DEM file using GDAL
grass74 "$LOCATION"/$MAPSET --exec r.in.gdal -e --overwrite input="$downloadedDEMfile" output=demRAW location=elevationRAW
LOCATIONDEM="$GISDBASE"/elevationRAW
grass74 "$LOCATIONDEM"/$MAPSET --exec g.region raster=demRAW
#This has options to print the region and to align the region to the specified resolution
grass74 "$LOCATIONDEM"/$MAPSET --exec g.region res=$RESOLUTION -a -p
#Resample DEM 
grass74 "$LOCATIONDEM"/$MAPSET --exec r.resamp.stats -w input=demRAW output=dem$RESOLUTION'm' ### skip this if no resampling spatial scale
#Write the new resample DEM to a GeoTIFF file
grass74 "$LOCATIONDEM"/$MAPSET --exec r.out.gdal --overwrite input=dem$RESOLUTION'm' output="$PROJDIR"/raw_data/dem$RESOLUTION'm.tif' format=GTiff

#Fixme: This part would be included in the loop over replicates
### ... import the (rescaled) elevation data into ""$LOCATION"/$MAPSET"
grass74 "$LOCATION"/$MAPSET --exec r.in.gdal -o -e --overwrite input="$PROJDIR"/raw_data/dem$RESOLUTION'm.tif' output=dem
grass74 "$LOCATION"/$MAPSET --exec g.region raster=dem

#Fixme: Why is this folder deleted? JDS commenting out to report intermediate file.
#rm -rf "$LOCATIONDEM"

##################################################################################
# 1.4 Define the catchment outlet location and create a point for that location.
#Convert gauge coordinates from WGS84 to specified EPSG code
eval $(grass74 "$LOCATION"/$MAPSET --exec m.proj -i coordinates=$gageLong,$gageLat separator=space | awk '{print "xyCoord=" $1 "," $2}')
echo $xyCoord | grass74 "$LOCATION"/$MAPSET --exec v.in.ascii in=- out=outlet x=1 y=2 separator=, --overwrite

##################################################################################
# 1.5 Delineate Watershed with Grass and R scripts
# Delineate watershed and subbasins using the grass shell scripts and r script.
#Run from shell script: Doesn't work for JDS
#grass74 "$LOCATION"/$MAPSET --exec bash "$GITHUBLIBRARIES"/grass_delineation_1.sh $GRASS_thres $GRASS_drainarea_lowerbound $GRASS_drainarea_upperbound

#Contents of shell script
inputThreshold=$GRASS_thres
inputLower=$GRASS_drainarea_lowerbound
inputUpper=$GRASS_drainarea_upperbound

grass74 "$LOCATION"/$MAPSET --exec g.region raster=dem
grass74 "$LOCATION"/$MAPSET --exec r.mask -r
# set x, y, and physical variables (e.g., slope, aspect ... etc) based on elevation
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="xmap = x()"
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="ymap = y()"
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="rowmap = row()"
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="colmap = col()"
grass74 "$LOCATION"/$MAPSET --exec r.slope.aspect --overwrite elevation=dem slope=slope_ aspect=aspect_
grass74 "$LOCATION"/$MAPSET --exec r.horizon --overwrite -d elevation=dem direction=180 output="west" distance='1.0'
grass74 "$LOCATION"/$MAPSET --exec r.horizon --overwrite -d elevation=dem direction=0 output="east" distance='1.0'

grass74 "$LOCATION"/$MAPSET --exec r.watershed --overwrite elevation=dem accumulation=uaa drainage=drain tci=wetness_index
grass74 "$LOCATION"/$MAPSET --exec r.watershed --overwrite elevation=dem threshold=$inputThreshold basin=sub_ stream=str_ half_basin=hill_
eval $(grass74 "$LOCATION"/$MAPSET --exec r.what --quiet map=sub_ points=outlet separator=space | awk '{print "outletSUB=" $3}')
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="tmp = if(sub_== $outletSUB && str_>0, uaa, null())"

grass74 "$LOCATION"/$MAPSET --exec r.to.vect --overwrite input=tmp output=tmp type=line
eval $(grass74 "$LOCATION"/$MAPSET --exec v.distance -p from=outlet from_type=point to=tmp to_type=line upload=to_x,to_y separator=space | awk '{print "xyCoord=" $2 "," $3}')
# delineate catchment based on the nearest outlet point on the stream/uaa ##--- this step is contrained by D8
grass74 "$LOCATION"/$MAPSET --exec r.water.outlet --overwrite input=drain output=basin_ coordinates=$xyCoord

grass74 "$LOCATION"/$MAPSET --exec r.to.vect --overwrite input=sub_ output=tmp type=area
grass74 "$LOCATION"/$MAPSET --exec v.rast.stats -c map=tmp raster=basin_ column_prefix=select method=average
grass74 "$LOCATION"/$MAPSET --exec v.to.rast --overwrite input=tmp type=area where=select_average>0 output=tmp use=attr attribute_column=select_average


grass74 "$LOCATION"/$MAPSET -text --exec Rscript "$GITHUBLIBRARIES"/basin_determine.R

grass74 "$LOCATION"/$MAPSET --exec r.watershed elevation=dem threshold=$GRASS_thresII stream=strExt --overwrite # full stream extension;

#Run from shell script: Doesn't work for JDS
#grass74 "$LOCATION"/$MAPSET --exec bash "$GITHUBLIBRARIES"/grass_delineation_2.sh

#Contents of shell script
eval $(grass74 "$LOCATION"/$MAPSET --exec r.info map=dem | awk 'NR==13{print "maxcol="$3}')
grass74 "$LOCATION"/$MAPSET --exec g.region zoom=basin
grass74 "$LOCATION"/$MAPSET --exec r.mask raster=basin
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="basin = if(isnull(hill),null(),1)"
grass74 "$LOCATION"/$MAPSET --exec g.region raster=dem
grass74 "$LOCATION"/$MAPSET --exec g.region zoom=basin
grass74 "$LOCATION"/$MAPSET --exec r.mask -r
grass74 "$LOCATION"/$MAPSET --exec r.mask raster=basin
# set patch and other variables
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="patch = row()*$maxcol+col()"
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="ZERO = 0"
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="ONE = 1"
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="slope = if(isnull(slope_),0.143,slope_)"
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="aspect = if(isnull(aspect_),abs(drain)*45,aspect_)"
grass74 "$LOCATION"/$MAPSET --exec g.remove -f type=vector name=tmp
grass74 "$LOCATION"/$MAPSET --exec g.remove -f type=raster name=tmp

##################################################################################
## 1.6 Use cluster analysis of slope and aspect to determine zones
grass74 "$LOCATION"/$MAPSET --exec Rscript "$GITHUBLIBRARIES"/zone_cluster.R dem slope aspect hill zone_cluster
####################################################################################
## 1.7 Calculate isohyets
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="isohyet = 1"
####################################################################################
## 1.8 Evaluate soil information
downloadedSSURGOdirectoryPATH="$PROJDIR"/raw_data/MD005
grass74 "$LOCATION"/$MAPSET --exec v.in.ogr --overwrite input="$downloadedSSURGOdirectoryPATH"/spatial/soilmu_a_"$(echo ${downloadedSSURGOdirectoryPATH##*/} | tr '[A-Z]' '[a-z]')".shp output=ssurgo location=soilRAW
LOCATIONSOIL="$GISDBASE"/soilRAW

grass74 "$LOCATION"/$MAPSET --exec v.proj --overwrite location=soilRAW mapset=PERMANENT input=ssurgo output=ssurgo
grass74 "$LOCATION"/$MAPSET --exec v.to.rast --overwrite input=ssurgo use=cat output=soil_ssurgo
grass74 "$LOCATION"/$MAPSET --exec v.db.select --overwrite map=ssurgo separator=comma file="$PROJDIR"/"$RHESSysNAME"/soil_cat_mukey.csv

grass74 "$LOCATION"/$MAPSET --exec Rscript "$SSURGOLIBRARIES"/ssurgo_extraction.R "$downloadedSSURGOdirectoryPATH"
grass74 "$LOCATION"/$MAPSET --exec Rscript "$SSURGOLIBRARIES"/ssurgo_soiltexture2gis.R "$PROJDIR"/"$RHESSysNAME"/soil_cat_mukey.csv "$downloadedSSURGOdirectoryPATH"/soil_mukey_texture.csv
#Fixme: Why is this folder deleted? JDS commenting out to report intermediate file.
#rm -rf "$LOCATIONSOIL"

###################################################################################
## 1.9 Extract Land Use and Land Cover Information
#For adding GI, will want to implement them at the 1m scale, and then have them computed as part of the fraction of patches in later steps in this script.

downloadedLULCfile="$PROJDIR"/raw_data/'BARN_1mLC_UTM.tif'
grass74 "$LOCATION"/$MAPSET --exec r.in.gdal -e --overwrite input="$downloadedLULCfile" output=lulcRAW location=lulcRAW
LOCATIONLULC="$GISDBASE"/lulcRAW
grass74 "$LOCATION"/$MAPSET --exec r.to.vect input=patch output=patch type=area
grass74 "$LOCATIONLULC"/$MAPSET --exec v.proj location=$LOCATION_NAME mapset=$MAPSET input=patch output=patch$RESOLUTION'm'
grass74 "$LOCATIONLULC"/$MAPSET --exec v.to.rast input=patch$RESOLUTION'm' output=patch$RESOLUTION'm' use=attr attribute_column=value

grass74 "$LOCATIONLULC"/$MAPSET --exec Rscript "$GITHUBLIBRARIES"/aggregate_lulcFrac.R patch$RESOLUTION'm' lulcRAW "$PROJDIR"/"$RHESSysNAME"/lulcFrac$RESOLUTION'm.csv'

grass74 "$LOCATION"/$MAPSET --exec Rscript "$GITHUBLIBRARIES"/aggregate_lulcFrac_write2GIS.R patch "$PROJDIR"/"$RHESSysNAME"/lulcFrac$RESOLUTION'm.csv' "$PROJDIR"/GIS2RHESSys/lulc_1m_Chesapeake_Conservancy.csv

#Fixme: Why is this folder deleted? JDS commenting out to report intermediate file.
#rm -rf "$LOCATIONLULC"

###################################################################################
## 1.10 Assign IDs to land uses
#Note: vegetation ID for grass of 3
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="grass1StratumID = if(lawnFrac>0,3,null())"
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="grass1FFrac = if(lawnFrac>0,1.0,null())"
#Note: default LAI for grass of 1.5 - maximum in a growth season
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="grass1LAI = if(lawnFrac>0,1.5,null())"

#Note: vegetation ID for trees of 102
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="tree1StratumID = if(forestFrac>0,102,null())"
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="tree1FFrac = if(forestFrac>0,1.0,null())"
#Note: default LAI for grass of 5.5 - maximum in a growth season
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="tree1LAI = if(forestFrac>0,5.5,null())"

###################################################################################
## 1.11 Add road information
downloadedROADfile="$PROJDIR"/raw_data/'Roads_GDT_MSA.shp'
grass74 "$LOCATION"/$MAPSET --exec v.in.ogr --overwrite input="$downloadedROADfile" output=roads location=roadRAW
LOCATIONROAD="$GISDBASE"/roadRAW
grass74 "$LOCATION"/$MAPSET --exec v.proj --overwrite location=roadRAW mapset=$MAPSET input=roads output=roads
grass74 "$LOCATION"/$MAPSET --exec v.to.rast --overwrite input=roads output=vector_roads use=cat
#Fixme: Why is this folder deleted? JDS commenting out to report intermediate file.
#rm -rf "$LOCATIONROAD"

###################################################################################
## 1.12 Analyze elevation flowpaths
grass74 "$LOCATION"/$MAPSET --exec Rscript "$GITHUBLIBRARIES"/elevation_analysis.R dem colmap rowmap drain hill strExt
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc expression="riparian_hands = if( handsDEM < 5, 1, null())" --overwrite

###################################################################################
## 1.13 Evaluate infrastructure: storm drains, sewer line locations (based on road locations), and compactness of soil (based on house locations)
#### ... storm drinage along the paved roads
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc expression="roadDEM = if(pavedroadFrac>0,dem,null())"
grass74 "$LOCATION"/$MAPSET --exec r.watershed -s --overwrite elevation=roadDEM drainage=roadDrain
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="roadExit = if(roadDrain<0, patch, null())"

#### ... sewer drainage
#### ... assume sewer lines go along major roads (imported from shapefile) and sewer lines drain a neigbourhood area
#Assumes a buffer of 30 m? and several other assumptions for where sewers are located
grass74 "$LOCATION"/$MAPSET --exec r.buffer --overwrite input=vector_roads output=roadbuff distances=30
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="sewercover = if( (lawnFrac>0.1 || impFrac >0.1) && pavedroadFrac<0.3 && roadbuff>0, 1, null())"

#### ... soil compactness
#Assumes a buffer of 30 m?
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="roof = if( (roofFrac > 0.1), 1, null())"
grass74 "$LOCATION"/$MAPSET --exec r.buffer --overwrite input=roof output=roofBuff distances=30
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="naturalLand = if( (forestFrac>=1 || lawnFrac>=1)&& isnull(roofBuff), 1 , null())"
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="compactedsoil = if(isnull(naturalLand),basin,null())"

#### ... additional surface drainage  (other than roof, driveway, parking, and paved roads), e.g., drainage hole in playground / garden
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc --overwrite expression="addsurfdrain = if( (forestFrac<1.0) && (roofBuff>0), 1 , null())"

#### ... septic
#### ... lawn area within 30 m of the house
#grass74 "$LOCATION"/$MAPSET --exec r.mapcalc -s --overwrite expression="septic = if( (roofBuff==2) && (lawnFrac>=0) && (rand(0,100)>=60), 1 , null())"

#Removed random septic here. Placing septic systems around all houses.
grass74 "$LOCATION"/$MAPSET --exec r.mapcalc -s --overwrite expression="septic = if( (roofBuff==2) && (lawnFrac>=0), 1 , null())"

###################################################################################
## 2.1 Specify output information for RHESSys input files that GIS2RHESSys outputs
templateFile="$PROJDIR"/"$RHESSysNAME"/g2w_template.txt

# set paths for RHESSys input files
# 1 = Yes, output this file; 0 = No, do not output this file
echo outputWorldfile \""$PROJDIR"/"$RHESSysNAME"/worldfiles/worldfile.csv\" 1 > "$templateFile"
echo outputWorldfileHDR \""$PROJDIR"/"$RHESSysNAME"/worldfiles/worldfile.hdr\" 1 >> "$templateFile"
echo outputDefs \""$PROJDIR"/"$RHESSysNAME"/defs\" 0 >> "$templateFile"
echo outputSurfFlow \""$PROJDIR"/"$RHESSysNAME"/flows/surfflow.txt\" 1 >> "$templateFile"
echo outputSubFlow \""$PROJDIR"/"$RHESSysNAME"/flows/subflow.txt\" 1 >> "$templateFile"

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
echo tree1StratumID tree1StratumID >> "$templateFile"
echo tree1FFrac tree1FFrac >> "$templateFile"
echo tree1LAI tree1LAI >> "$templateFile"
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
grass74 "$LOCATION"/$MAPSET --exec Rscript "$GITHUBLIBRARIES"/g2w_cf_RHESSysEC.R "$PROJDIR" default default default "$templateFile"
Rscript "$GITHUBLIBRARIES"/LIB_RHESSys_writeTable2World.R NA "$PROJDIR"/"$RHESSysNAME"/worldfiles/worldfile.csv "$PROJDIR"/"$RHESSysNAME"/worldfiles/worldfile
