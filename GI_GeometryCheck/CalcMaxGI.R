#Script to determine how much GI can be allocated to each grid cell

#Load libraries----
library(sp)
library(rgdal)
library(raster)
library(rgeos)

setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\RHESSys_ParameterSA\\GI_GeometryCheck")

#Load files----
#Read in the grid cell centers and make into a spatial dataframe
Cells = read.csv('worldfile.csv', stringsAsFactors = FALSE)
coordinates(Cells) = c('patchX', 'patchY')
proj4string(Cells) = CRS('+init=epsg:26918')

#Read in the original aggregated 30 m res LULC file
lc30 = read.csv('lulcFrac30m.csv', stringsAsFactors = FALSE)
#Add GI columns
lc30$lulc16 = lc30$lulc15 = lc30$lulc14 = lc30$lulc13 = 0

#Read in the original 1 m LULC file 
lc1_rast = raster('BARN_1mLC_UTM.tif')
#Convert to cell centers
lc1 = rasterToPoints(x = lc1_rast, spatial = TRUE, fun = NULL)
#Transform to same projection as Cells
lc1 = spTransform(lc1, CRSobj = CRS('+init=epsg:26918'))

#Buffer the Cells by 30 m and clip the lc1 data to that to reduce the size of the dataframe
Cells_buff30m = buffer(Cells, width = 30, dissolve = TRUE)

#Clip lc1 to this buffer
lc1 = lc1[Cells_buff30m,]

#Implement constraints on the locations where GI may be placed----
# 1. GI cannot be placed within a 1 m buffer from impervious surfaces----
#Impervious surfaces (with and without vegetation cover) are lulc 7, 8, 9, 10, 11, and 12. 
#Using a radial buffer from the grid cell center of an impervious cell. 
#The buffer may need to be > 1 m in radius to get corner cell centers whose cells are within 1 m away.
test = buffer(lc1[lc1$BARN_1mLC_UTM %in% c(7:12),], width = 1, dissolve = TRUE)

# 2. GI cannot be placed on septic grid cells---- 
# Septic cells are listed in the worldfile csv by patch ID number. Septic is PatchLandID 4.
Cells[Cells$patchLandID == 4,]

#3. The spatial area increment for GI is a square of 9, 1 m2 cells with lulc5 that meet the constraint in steps 1 and 2. 
# Finding which of the 900 1 m2 cells fall within each 900 m2 patch cell. 
# Clipp the grid cell centers of the 1 m2 cells to the 900 m2 raster cells (or vector grid)
# Loop over the patches and return the maximum possible GI fraction. 


#Find the maximum possible amount of GI for each patch----
for (p in 1:nrow(Cells)){
  
}

#Write a file containing the maximum GI allocation for each cell
write.csv( , 'MaxGI.csv', row.names = FALSE)