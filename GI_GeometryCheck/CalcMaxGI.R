#Script to determine how much GI can be allocated to each grid cell

#Set working directory----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\RHESSys_ParameterSA\\GI_GeometryCheck")

#Load libraries----
library(sp)
library(rgdal)
library(raster)
library(rgeos)

#Load files----
#Read in the grid cell centers and make into a spatial dataframe
Cells = read.csv('worldfile.csv', stringsAsFactors = FALSE)
coordinates(Cells) = c('patchX', 'patchY')
proj4string(Cells) = CRS('+init=epsg:26918')

#Read the polygon grid of 30 m patches
CellGrid = readOGR(dsn = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\RHESSys_ParameterSA\\GI_GeometryCheck\\PatchGrid", layer = 'patch', stringsAsFactors = FALSE)
#Transform to same projection as Cells
CellGrid = spTransform(CellGrid, CRSobj = CRS('+init=epsg:26918'))

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
#Make into a data frame
Cells_buff30m = SpatialPolygonsDataFrame(Sr = Cells_buff30m, data = as.data.frame(1))

#Save datasets
writeOGR(Cells_buff30m, dsn = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\RHESSys_ParameterSA\\GI_GeometryCheck", layer = 'PatchBuff30m', driver = 'ESRI Shapefile')
writeOGR(lc1, dsn = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\RHESSys_ParameterSA\\GI_GeometryCheck", layer = 'LULCCenters1m', driver = 'ESRI Shapefile')

#Clip lc1 to this buffer (takes 10 mins)
lc1 = lc1[Cells_buff30m,]
writeOGR(lc1, dsn = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\RHESSys_ParameterSA\\GI_GeometryCheck", layer = 'LULCCenters1m_cpR', driver = 'ESRI Shapefile')

#Number the cells
lc1$ID = seq(1,nrow(lc1@data),1)
#Obtain the patch ID for each 1 m2 cell
lc1$patchID = 0
#Loop over the patch cell centers and find the 900 1 m^2 cells that are closest to the cell center
#Fixme: this should be parallelized
for (p in 1:length(unique(Cells$patchID))){
  #Select only 1 stratum when there are multiple strata for a single location.
  patch = Cells[which(Cells$patchID == unique(Cells$patchID)[p])[1],]
  Cell_p = CellGrid[CellGrid$value == patch$patchID,]
  
  #Select all 1 m^2 cell centers in this patch
  cellPts = lc1[Cell_p,]
  
  #Check there are 900 points selected
  if (nrow(cellPts) != 900){
    print(paste('Cells not equal to 900 for patch', patch$patchID))
    stop()
  }
  #Check if any of the 1 m2 cells already have been assigned a patchID
  if (any(lc1$patchID[lc1$ID %in% cellPts$ID] != 0)){
    print(paste('patchID already assigned to a cell for index', p))
    stop()
  }
  
  #Assign the patch ID to these cells
  lc1$patchID[lc1$ID %in% cellPts$ID] = patch$patchID
  
  rm(cellPts, Cell_p, patch)
}
rm(p)

#Implement constraints on the locations where GI may be placed----
# 1. GI cannot be placed within a 2 m buffer from impervious surfaces----
# Impervious surfaces (with and without vegetation cover) are lulc 7, 8, 9, 10, 11, and 12. 
# Using a 2.9 m radial buffer from the grid cell center of an impervious cell to select corner cell centers 
# whose cells are within 2 m away.
Impervious1 = lc1[lc1$BARN_1mLC_UTM %in% c(7:12),]
ImpBuff = buffer(Impervious1, width = 2.9, dissolve = TRUE)

#Remove these cells from the possible options for centers of GI.
# Takes a while to do this step, but it cannot be sped up.
CenterOptions = lc1[ImpBuff,]
CenterOptions = lc1[-which(lc1$ID %in% CenterOptions$ID),]

#Remove all cells with patchID = 0 (not in watershed)---- 
# This has to be completed after buffering impervious surfaces so that roads located within 2.9 m of the watershed boundary inform placement of GI.
lc1 = lc1[-which(lc1$patchID == 0),]
CenterOptions = CenterOptions[-which(CenterOptions$patchID == 0),]

#Remove all of 10, 11, and 12 (vegetation covered areas) from possible GI buffer locations
# Note these are already removed from CenterOptions because of the ImpBuff
BufferOptions = lc1[lc1$BARN_1mLC_UTM != 10,]
BufferOptions = BufferOptions[BufferOptions$BARN_1mLC_UTM != 11,]
BufferOptions = BufferOptions[BufferOptions$BARN_1mLC_UTM != 12,]

#Remove all roofs from GI buffer locations
BufferOptions = BufferOptions[BufferOptions$BARN_1mLC_UTM != 7,]

# 2. GI center location cannot be placed within 8 m of major roads
#Load buffered roads file (Route 25)
Rte25 = readOGR(dsn = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\RHESSys_ParameterSA\\GI_GeometryCheck", layer = 'Route25_Buff8m', stringsAsFactors=FALSE)
Rte25 = spTransform(Rte25, CRSobj = CRS('+init=epsg:26918'))
Rte25_Cells = CenterOptions[Rte25,]
CenterOptions = CenterOptions[-which(CenterOptions$ID %in% Rte25_Cells$ID),]

# 3. The center cell must be grass (lulc5) and the buffer cannot be tree (lulc3)----
CenterOptions = CenterOptions[CenterOptions$BARN_1mLC_UTM == 5,]
BufferOptions = BufferOptions[BufferOptions$BARN_1mLC_UTM != 3,]

#Testing a buffer that is only grass
BufferOptions_grass = BufferOptions[BufferOptions$BARN_1mLC_UTM == 5,]

# 4. No trees may be placed over the power line cells (a 15 m buffer is hand-drawn when digitized in Google Earth and ArcMap)
#Used to process power line KML to shapefile:
#Load power line KML files, transform to CRS, and write as shapefiles
#PL1 = readOGR(dsn = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\Boundaries\\BaisPowerLine1.kml", stringsAsFactors = FALSE)
#PL2 = readOGR(dsn = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\Boundaries\\BaisPowerLine2.kml", stringsAsFactors = FALSE)
#PL1 = spTransform(PL1, CRSobj = CRS('+init=epsg:26918'))
#PL2 = spTransform(PL2, CRSobj = CRS('+init=epsg:26918'))
#writeOGR(PL1, dsn = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\Boundaries", layer = 'BaisPowerLine1', driver = 'ESRI Shapefile')
#writeOGR(PL2, dsn = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\Boundaries", layer = 'BaisPowerLine2', driver = 'ESRI Shapefile')
#rm(PL1, PL2)

#Load in the processed power line shapefiles
PL1 = readOGR(dsn = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\Boundaries\\BaisPowerLine1.shp", stringsAsFactors = FALSE)
PL2 = readOGR(dsn = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\Boundaries\\BaisPowerLine2.shp", stringsAsFactors = FALSE)
PL1 = spTransform(PL1, CRSobj = CRS('+init=epsg:26918'))
PL2 = spTransform(PL2, CRSobj = CRS('+init=epsg:26918'))

#Remove these cells from Center and Boundary options
PL1_Center = CenterOptions[PL1,]
PL2_Center = CenterOptions[PL2,]
CenterOptions = CenterOptions[-which(CenterOptions$ID %in% PL1_Center$ID),]
CenterOptions = CenterOptions[-which(CenterOptions$ID %in% PL2_Center$ID),]

PL1_Bound = BufferOptions[PL1,]
PL2_Bound = BufferOptions[PL2,]
BufferOptions = BufferOptions[-which(BufferOptions$ID %in% PL1_Bound$ID),]
BufferOptions = BufferOptions[-which(BufferOptions$ID %in% PL2_Bound$ID),]

PL1_Boundgr = BufferOptions_grass[PL1,]
PL2_Boundgr = BufferOptions_grass[PL2,]
BufferOptions_grass = BufferOptions_grass[-which(BufferOptions_grass$ID %in% PL1_Boundgr$ID),]
BufferOptions_grass = BufferOptions_grass[-which(BufferOptions_grass$ID %in% PL2_Boundgr$ID),]

# 5. GI should only be placed in the suburban hillslopes
# Hillslopes 1 - 8 are mostly all forrested, so remove all cells in those hillslopes
for (h in 1:8){
  if (length(which(CenterOptions$patchID %in% Cells$patchID[Cells$hillID == h])) > 0){
    CenterOptions = CenterOptions[-which(CenterOptions$patchID %in% Cells$patchID[Cells$hillID == h]), ] 
  }
  if (length(which(BufferOptions$patchID %in% Cells$patchID[Cells$hillID == h])) > 0){
    BufferOptions = BufferOptions[-which(BufferOptions$patchID %in% Cells$patchID[Cells$hillID == h]), ]
  }
  if (length(which(BufferOptions_grass$patchID %in% Cells$patchID[Cells$hillID == h])) > 0){
    BufferOptions_grass = BufferOptions_grass[-which(BufferOptions_grass$patchID %in% Cells$patchID[Cells$hillID == h]), ]
  }
}
rm(h)

# 6. GI should be limited on septic grid cells---- 
# Septic cells are listed in the worldfile csv by patch ID number. Septic is PatchLandID 4.
# assume that a minimum of 100 grass cells are needed for the septic tank. All others can have GI
unique(Cells$patchID[Cells$patchLandID == 4])

# 7. The spatial area increment for GI is a square of 9, 1 m2 cells that meet the constraints in steps 1 and 2. 
# Loop over the patches and return the maximum possible GI fraction. 

#Find the maximum possible amount of GI for each patch----
#Matrix for maximum
lcMax = matrix(0, nrow = nrow(lc30), ncol = 2)
lcMax[,1] = lc30$patchID
#Loop over all of the 30 m patches for which a GI center can be located
GI_p = unique(CenterOptions$patchID[CenterOptions$BARN_1mLC_UTM == 5])
for (p in 1:length(GI_p)){
  #Get the center and buffer cell options for this patch. 
  co = CenterOptions[CenterOptions$patchID == GI_p[p],]
  bo = BufferOptions[BufferOptions$patchID == GI_p[p],]
  
  #ID, smallest to largest
  #loop over the 1m patches to determine which can have GI trees
  #ID for all possible GI center and buffer. 
  ID = vector('numeric')
  Max = 0
  for (l in 1:length(co$ID)){
    #Buffer the 1m cell center
    b = buffer(co[co$ID == co$ID[l],], width = 1.5, dissolve = TRUE)
    #Select all 1m cell centers within 1.5m in the buffer location dataset
    selCells = bo[b,]
    #If there are 9 cells, it can be a GI location
    if (nrow(selCells) == 9){
      #Add IDs to vector
      ID = c(ID, selCells$ID)
      
      #Remove these IDs from the buffer location options
      bo = bo[-which(bo$ID %in% selCells$ID),]
      Max = Max + 9
    }else if (nrow(selCells) > 9){
      print(paste('More than 9 cells for location', co$ID[l]))
      stop()
    }
  }
  rm(b, selCells, l)
  
  #ID, largest to smallest
  #loop over the 1m patches to determine which can have GI trees
  #ID for all possible GI center and buffer. 
  IDls = vector('numeric')
  Maxls = 0
  co = co[order(co$ID, decreasing = TRUE),]
  bo = BufferOptions[BufferOptions$patchID == GI_p[p],]
  for (l in 1:length(co$ID)){
    #Buffer the 1m cell center
    b = buffer(co[co$ID == co$ID[l],], width = 1.5, dissolve = TRUE)
    #Select all 1m cell centers within 1.5m in the buffer location dataset
    selCells = bo[b,]
    #If there are 9 cells, it can be a GI location
    if (nrow(selCells) == 9){
      #Add IDs to vector
      IDls = c(IDls, selCells$ID)
      
      #Remove these IDs from the buffer location options
      bo = bo[-which(bo$ID %in% selCells$ID),]
      Maxls = Maxls + 9
    }else if (nrow(selCells) > 9){
      print(paste('More than 9 cells for location', co$ID[l]))
      stop()
    }
  }
  rm(b, selCells, l)
  
  #Determine which land cover types are converted to GI land cover
  
  lcMax[p,2] = max(Max, Maxls)
}
rm(p, ID, Max, bo, co, Maxls, IDls)


#Used to make plots of individual 30x30 m patches
plot(CellGrid[CellGrid$value == GI_p[p],], col = NA, border = 'black')
plot(BufferOptions[BufferOptions$patchID == GI_p[p],], pch = 15, add = T, col = 'red')
plot(CenterOptions[CenterOptions$patchID == GI_p[p],], pch = 15, add = T)
plot(BufferOptions[BufferOptions$ID %in% ID,], pch = 15, add = T, col = 'green')
plot(BufferOptions[BufferOptions$ID %in% IDls,], pch = 15, add = T, col = 'green')

plot(Cells, pch = 15)
plot(Cells[Cells$hillID %in% 1:8,], pch = 15, col = 'red', add = T)
plot(Cells[which((Cells$hillID == 2) & (Cells$patchLandID == 1)),], pch = 15, col = 'purple', add = T)
plot(BufferOptions, pch = 15, col = 'green', cex = 0.1, add = T)
#Used to show impact of using grass-only in the buffer cells.
#plot(BufferOptions_grass, pch = 15, col = 'yellow', cex = 0.1, add = T)
plot(CenterOptions, pch = 15, col = 'darkgreen', cex = 0.1, add = T)

#Write a file containing the maximum GI allocation for each cell
write.csv( , 'MaxGI.csv', row.names = FALSE)