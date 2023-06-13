#Script to determine how much GI can be allocated to each grid cell

#Set working directory----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation")

#Load libraries----
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(foreach)
library(parallel)
library(iterators)
library(doParallel)
library(GISTools)
library(scico)
library(TeachingDemos)

#Load color functions - from JDS github repo: Geothermal_ESDA
source('C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\Hydrology\\USGSGauges\\ColorFunctions.R')

#Load files----
#Read in the grid cell centers and make into a spatial dataframe
Cells = read.csv('worldfile.csv', stringsAsFactors = FALSE)
coordinates(Cells) = c('patchX', 'patchY')
proj4string(Cells) = CRS('+init=epsg:26918')

#Read the polygon grid of 30 m patches
CellGrid = readOGR(dsn = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation\\PatchGrid", layer = 'patch', stringsAsFactors = FALSE)
#Transform to same projection as Cells
CellGrid = spTransform(CellGrid, CRSobj = CRS('+init=epsg:26918'))

#Read in the original aggregated 30 m res LULC file
lc30 = read.csv('lulcFrac30m.csv', stringsAsFactors = FALSE)

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
writeOGR(Cells_buff30m, dsn = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation", layer = 'PatchBuff30m', driver = 'ESRI Shapefile')
writeOGR(lc1, dsn = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation", layer = 'LULCCenters1m', driver = 'ESRI Shapefile')

#Clip lc1 to this buffer (takes 10 mins)
lc1 = lc1[Cells_buff30m,]
writeOGR(lc1, dsn = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation", layer = 'LULCCenters1m_cpR', driver = 'ESRI Shapefile')

#Number the cells
lc1$ID = seq(1,nrow(lc1@data),1)
#Obtain the patch and hill ID for each 1 m2 cell
lc1$patchID = 0
lc1$hillID = 0
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
  
  #Assign the patch ID and hillID to these cells
  lc1$patchID[lc1$ID %in% cellPts$ID] = patch$patchID
  lc1$hillID[lc1$ID %in% cellPts$ID] = patch$hillID
  
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

#  Remove all cells with patchID = 0 (not in watershed)---- 
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

# 2. GI center location cannot be placed within 8 m of major roads----
#Load buffered roads file (Route 25)
Rte25 = readOGR(dsn = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation", layer = 'Route25_Buff8m', stringsAsFactors=FALSE)
Rte25 = spTransform(Rte25, CRSobj = CRS('+init=epsg:26918'))
Rte25_Cells = CenterOptions[Rte25,]
CenterOptions = CenterOptions[-which(CenterOptions$ID %in% Rte25_Cells$ID),]

# 3. The center cell must be grass (lulc5) and the buffer cannot be tree (lulc3)----
CenterOptions = CenterOptions[CenterOptions$BARN_1mLC_UTM == 5,]
BufferOptions = BufferOptions[BufferOptions$BARN_1mLC_UTM != 3,]

#Testing a buffer that is only grass
BufferOptions_grass = BufferOptions[BufferOptions$BARN_1mLC_UTM == 5,]

# 4. No trees may be placed over the power line cells----
# (a 15 m buffer is hand-drawn when digitized in Google Earth and ArcMap)
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

# 5. GI should only be placed in the suburban hillslopes----
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

# 6. The spatial area increment for GI is a square of 9, 1 m2 cells that meet the constraints in steps 1 and 2----
#Loop over the patches and return the maximum possible GI fraction for each patch----
#Matrix for maximum
lcMax = matrix(0, nrow = nrow(lc30), ncol = 2)
lcMax[,1] = lc30$patchID
#Loop over all of the 30 m patches for which a GI center can be located
GI_p = unique(CenterOptions$patchID)

cl = makeCluster(detectCores() - 1)
registerDoParallel(cl)
MaxGI = foreach(p = 1:length(GI_p), .packages = c('sp', 'raster'), .combine = c, .inorder = TRUE, 
                .noexport = c("BufferOptions_grass", "CellGrid", "Cells", "Cells_buff30m", "ImpBuff", "Impervious1", "lc1", "lc1_rast", "lc30", 
                              "PL1", "PL1_Bound", "PL1_Boundgr", "PL1_Center", "PL2", "PL2_Bound", "PL2_Boundgr", "PL2_Center", "Rte25", "Rte25_Cells")) %dopar% {
  #Get the center and buffer cell options for this patch. 
  co = CenterOptions[CenterOptions$patchID == GI_p[p],]
  bo = BufferOptions[BufferOptions$patchID == GI_p[p],]
  
  #ID, smallest to largest
  #loop over the 1m patches to determine which can have GI trees
  #ID for all possible GI center and buffer. 
  #ID = vector('numeric')
  Max = 0
  for (l in 1:length(co$ID)){
    #Buffer the 1m cell center
    b = buffer(co[co$ID == co$ID[l],], width = 1.5, dissolve = TRUE)
    #Select all 1m cell centers within 1.5m in the buffer location dataset
    selCells = bo[b,]
    #If there are 9 cells, it can be a GI location
    if (nrow(selCells) == 9){
      #Add IDs to vector
      #ID = c(ID, selCells$ID)
      
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
  #IDls = vector('numeric')
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
      #IDls = c(IDls, selCells$ID)
      
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
  
  max(Max, Maxls)
}
#rm(p, ID, Max, bo, co, Maxls, IDls)
stopCluster(cl)

#Fill based on ID number
for (p in 1:length(GI_p)){
  lcMax[lcMax[,1] == GI_p[p],2] = MaxGI[p]
}
rm(p)

#Grass only----
#Find the maximum possible amount of GI for each patch
#Matrix for maximum
lcMax_gr = matrix(0, nrow = nrow(lc30), ncol = 2)
lcMax_gr[,1] = lc30$patchID
#Loop over all of the 30 m patches for which a GI center can be located
GI_p = unique(CenterOptions$patchID)

cl = makeCluster(detectCores() - 1)
registerDoParallel(cl)
MaxGI_gr = foreach(p = 1:length(GI_p), .packages = c('sp', 'raster'), .combine = c, .inorder = TRUE, 
                   .noexport = c("BufferOptions", "CellGrid", "Cells", "Cells_buff30m", "ImpBuff", "Impervious1", "lc1", "lc1_rast", "lc30", "lcMax",
                                 "PL1", "PL1_Bound", "PL1_Boundgr", "PL1_Center", "PL2", "PL2_Bound", "PL2_Boundgr", "PL2_Center", "Rte25", "Rte25_Cells")) %dopar% {
  #Get the center and buffer cell options for this patch. 
  co = CenterOptions[CenterOptions$patchID == GI_p[p],]
  bo = BufferOptions_grass[BufferOptions_grass$patchID == GI_p[p],]
  
  #ID, smallest to largest
  #loop over the 1m patches to determine which can have GI trees
  #ID for all possible GI center and buffer. 
  #ID = vector('numeric')
  Max = 0
  for (l in 1:length(co$ID)){
    #Buffer the 1m cell center
    b = buffer(co[co$ID == co$ID[l],], width = 1.5, dissolve = TRUE)
    #Select all 1m cell centers within 1.5m in the buffer location dataset
    selCells = bo[b,]
    #If there are 9 cells, it can be a GI location
    if (nrow(selCells) == 9){
      #Add IDs to vector
      #ID = c(ID, selCells$ID)
      
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
  #IDls = vector('numeric')
  Maxls = 0
  co = co[order(co$ID, decreasing = TRUE),]
  bo = BufferOptions_grass[BufferOptions_grass$patchID == GI_p[p],]
  for (l in 1:length(co$ID)){
    #Buffer the 1m cell center
    b = buffer(co[co$ID == co$ID[l],], width = 1.5, dissolve = TRUE)
    #Select all 1m cell centers within 1.5m in the buffer location dataset
    selCells = bo[b,]
    #If there are 9 cells, it can be a GI location
    if (nrow(selCells) == 9){
      #Add IDs to vector
      #IDls = c(IDls, selCells$ID)
      
      #Remove these IDs from the buffer location options
      bo = bo[-which(bo$ID %in% selCells$ID),]
      Maxls = Maxls + 9
    }else if (nrow(selCells) > 9){
      print(paste('More than 9 cells for location', co$ID[l]))
      stop()
    }
  }
  rm(b, selCells, l)
  
  max(Max, Maxls)
}
#rm(p, ID, Max, bo, co, Maxls, IDls)
stopCluster(cl)

#Fill based on ID number
for (p in 1:length(GI_p)){
  lcMax_gr[lcMax_gr[,1] == GI_p[p],2] = MaxGI_gr[p]
}
rm(p)

#Used to make plots of individual 30x30 m patches when loop is run in serial
#plot(CellGrid[CellGrid$value == GI_p[p],], col = NA, border = 'black')
#plot(BufferOptions[BufferOptions$patchID == GI_p[p],], pch = 15, add = T, col = 'red')
#plot(CenterOptions[CenterOptions$patchID == GI_p[p],], pch = 15, add = T)
#plot(BufferOptions[BufferOptions$ID %in% ID,], pch = 15, add = T, col = 'green')
#plot(BufferOptions[BufferOptions$ID %in% IDls,], pch = 15, add = T, col = 'green')

#Used to plot the cell center and buffer options
plot(Cells, pch = 15)
plot(Cells[Cells$hillID %in% 1:8,], pch = 15, col = 'red', add = T)
plot(Cells[which((Cells$hillID == 2) & (Cells$patchLandID == 1)),], pch = 15, col = 'purple', add = T)
#plot(BufferOptions, pch = 15, col = 'green', cex = 0.1, add = T)
#Used to show impact of using grass-only in the buffer cells.
plot(BufferOptions_grass, pch = 15, col = 'yellow', cex = 0.1, add = T)
plot(CenterOptions, pch = 15, col = 'darkgreen', cex = 0.1, add = T)

# 7. GI should be limited on septic grid cells---- 
# Septic cells are listed in the worldfile csv by patch ID number. Septic is PatchLandID 4.
# assume that a minimum of 150 grass cells are needed for the septic tank. All others can have GI.
# Implementing as a subtraction from the max for now - no geometric requirements analyzed.
SepticPatch = unique(Cells$patchID[Cells$patchLandID == 4])
for (s in 1:length(SepticPatch)){
  #Check if this patch has GI assigned
  if (lcMax_gr[which(lcMax_gr[,1] == SepticPatch[s]),2] > 0){
    #Check the amount of grass cells in this patch
    grarea = lc30$lulc5[lc30$patchID == SepticPatch[s]]
    
    #Check if there are more than 150 grass cells
    if (grarea > 150){
      #Can have GI in this patch. Compute the maximum possible area.
      grMax = grarea - 150
      #Round down to the nearest whole number divisible by 9.
      grMax = grMax - (grMax %% 9)
      if (grMax > 0){
        #Grass area can have GI
        #Adjust MaxGI if it's larger than the grMax
        lcMax_gr[which(lcMax_gr[,1] == SepticPatch[s]),2] = min(lcMax_gr[which(lcMax_gr[,1] == SepticPatch[s]),2], grMax)
      }else{
        #Not enough grass area for GI
        lcMax_gr[which(lcMax_gr[,1] == SepticPatch[s]),2] = 0
      }
    }else{
      #Not enough land for GI. Set to 0.
      lcMax_gr[which(lcMax_gr[,1] == SepticPatch[s]),2] = 0
    }
  }
}
rm(s, grMax, grarea)

#Septic overlay plot
plot(Cells, pch = 15)
plot(Cells[Cells$patchID %in% SepticPatch,], col = 'purple', add = T, pch = 15)
plot(Cells[Cells$patchID %in% lc30$patchID[which((lc30$patchID %in% SepticPatch) & (lc30$lulc5 >= 100))],], col = 'red', add = T, pch = 15)
plot(Cells[Cells$patchID %in% lc30$patchID[which((lc30$patchID %in% SepticPatch) & (lc30$lulc5 >= 150))],], col = 'orange', add = T, pch = 15)
plot(Cells[Cells$patchID %in% lc30$patchID[which((lc30$patchID %in% SepticPatch) & (lc30$lulc5 >= 200))],], col = 'green', add = T, pch = 15)

#Plot the maximum percentage of GI in each patch----
#Color by percentage
scaleRange = c(0,1)
scaleBy = 0.2
Pal = rev(rainbow((scaleRange[2] - scaleRange[1])/scaleBy))
Pal[1] = 'black'

png('MaxGI_grass_pct900.png', res = 300, units = 'in', width = 5, height = 5)
par(mar= c(0,0,0,0))
plot(CellGrid[order(CellGrid$value, decreasing = FALSE),], pch = 15, col = colFun(lcMax_gr[order(lcMax_gr[,1], decreasing = FALSE),2]/900))
legend('bottomright', title = expression(bold("GI / Area")), legend = c("0 - <0.2", "0.2 - <0.4", "0.4 - <0.6", "0.6 - <0.8", "0.8 - 1"), col = colFun(seq(scaleRange[1], scaleRange[2], scaleBy)), pch = 15)
dev.off()

png('MaxGI_grass_pctGrass.png', res = 300, units = 'in', width = 5, height = 5)
par(mar= c(0,0,0,0))
plot(CellGrid[order(CellGrid$value, decreasing = FALSE),], pch = 15, col = colFun(lcMax_gr[order(lcMax_gr[,1], decreasing = FALSE),2]/lc30$lulc5))
legend('bottomright', title = expression(bold("GI / Grass")), legend = c("0 - <0.2", "0.2 - <0.4", "0.4 - <0.6", "0.6 - <0.8", "0.8 - 1", "No Grass"), col = c(colFun(seq(scaleRange[1], scaleRange[2]-scaleBy, scaleBy)), 'black'), pch = c(15,15,15,15,15,22))
dev.off()

#Upslope, Midslope, and Downslope plot----
Cells$MaxGI = 0
CellGrid$MaxGI = 0
CellGrid$elev = 0
CellGrid$hillID = 0
for(p in 1:nrow(lcMax_gr)){
  Cells$MaxGI[Cells$patchID == lcMax_gr[p,1]] = lcMax_gr[p,2]
  CellGrid$MaxGI[CellGrid$value == lcMax_gr[p,1]] = lcMax_gr[p,2]
  CellGrid$elev[CellGrid$value == lcMax_gr[p,1]] = Cells$patchZ[Cells$patchID == lcMax_gr[p,1]][1]
  CellGrid$hillID[CellGrid$value == lcMax_gr[p,1]] = Cells$hillID[Cells$patchID == lcMax_gr[p,1]][1]
}
rm(p)

# Up, Mid, Down Defined once for use in all hillslopes----
Upslope = max(CellGrid$elev[CellGrid$MaxGI > 0])+1
Midslope = as.numeric(quantile(CellGrid$elev[CellGrid$MaxGI > 0], probs = c(0.6667)))
Downslope = as.numeric(quantile(CellGrid$elev[CellGrid$MaxGI > 0], probs = c(0.3333)))

png('UpMidDownSlope.png', res = 300, units = 'in', width = 5, height = 5)
par(mar= c(0,0,0,0))
plot(CellGrid, col = 'black')
#plot(CellGrid[CellGrid$MaxGI > 0,], add = TRUE, col = 'black')
plot(CellGrid[which((CellGrid$MaxGI > 0) & (CellGrid$elev <= Downslope)),], col = 'purple', add = TRUE)
plot(CellGrid[which((CellGrid$MaxGI > 0) & (CellGrid$elev > Downslope) & (CellGrid$elev <= Midslope)),], col = 'green', add = TRUE)
plot(CellGrid[which((CellGrid$MaxGI > 0) & (CellGrid$elev > Midslope) & (CellGrid$elev <= Upslope)),], col = 'red', add = TRUE)
legend('bottomright', legend = c("Upslope", 'Midslope', 'Downslope', 'No GI Possible'), col = c('red', 'green', 'purple', 'black'), pch = 15)
dev.off()

#Check that number of patches is within 1
nrow(CellGrid[which((CellGrid$MaxGI > 0) & (CellGrid$elev <= Downslope)),])
nrow(CellGrid[which((CellGrid$MaxGI > 0) & (CellGrid$elev > Downslope) & (CellGrid$elev <= Midslope)),])
nrow(CellGrid[which((CellGrid$MaxGI > 0) & (CellGrid$elev > Midslope) & (CellGrid$elev <= Upslope)),])
#Compare the maximum area of GI for each
sum(CellGrid$MaxGI[which((CellGrid$MaxGI > 0) & (CellGrid$elev <= Downslope))])
sum(CellGrid$MaxGI[which((CellGrid$MaxGI > 0) & (CellGrid$elev > Downslope) & (CellGrid$elev <= Midslope))])
sum(CellGrid$MaxGI[which((CellGrid$MaxGI > 0) & (CellGrid$elev > Midslope) & (CellGrid$elev <= Upslope))])

# Up, Mid, Down defined for individual hillslopes----
Upslope_h = Midslope_h = Downslope_h = matrix(0, nrow = length(unique(Cells$hillID)), ncol = 2)
for (h in 1:length(unique(Cells$hillID))){
  Upslope_h[h,] = c(h, max(CellGrid$elev[which((CellGrid$MaxGI > 0) & (CellGrid$hillID == h))])+1)
  Midslope_h[h,] = c(h, as.numeric(quantile(CellGrid$elev[which((CellGrid$MaxGI > 0) & (CellGrid$hillID == h))], probs = c(0.6667))))
  Downslope_h[h,] = c(h, as.numeric(quantile(CellGrid$elev[which((CellGrid$MaxGI > 0) & (CellGrid$hillID == h))], probs = c(0.3333))))
}
rm(h)

png('UpMidDownSlope_hill.png', res = 300, units = 'in', width = 5, height = 5)
par(mar= c(0,0,0,0))
plot(CellGrid, col = 'black')
#plot(CellGrid[CellGrid$MaxGI > 0,], add = TRUE, col = 'black')
for (h in 9:14){
  plot(CellGrid[which((CellGrid$hillID == h) & (CellGrid$MaxGI > 0) & (CellGrid$elev <= Downslope_h[h,2])),], col = 'purple', add = TRUE)
  plot(CellGrid[which((CellGrid$hillID == h) & (CellGrid$MaxGI > 0) & (CellGrid$elev > Downslope_h[h,2]) & (CellGrid$elev <= Midslope_h[h,2])),], col = 'green', add = TRUE)
  plot(CellGrid[which((CellGrid$hillID == h) & (CellGrid$MaxGI > 0) & (CellGrid$elev > Midslope_h[h,2]) & (CellGrid$elev <= Upslope_h[h,2])),], col = 'red', add = TRUE)
}
rm(h)
legend('bottomright', title = expression(bold('Hillslope Specific Cutoffs')), legend = c("Upslope", 'Midslope', 'Downslope', 'No GI Possible'), col = c('red', 'green', 'purple', 'black'), pch = 15)
dev.off()

#Load streams
Streams = readOGR(dsn = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\Hydrology\\NHD_H_Maryland_State_Shape\\Shape", layer = 'MDstreams', stringsAsFactors = FALSE)
Streams = spTransform(Streams, CRSobj = CRS('+init=epsg:26918'))

#Change all data to degrees
CellsWGS = spTransform(Cells, CRSobj = CRS('+init=epsg:4326'))
StreamsWGS = spTransform(Streams, CRSobj = CRS('+init=epsg:4326'))

png('UpMidDownSlope_hill_proposal.png', res = 300, units = 'in', width = 6, height = 6)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0)
#plot(CellGrid[CellGrid$MaxGI > 0,], add = TRUE, col = 'black')
for (h in 9:14){
  plot(CellsWGS[CellsWGS$hillID == h,], col = 'gray', add = TRUE, lwd=7, pch = 22)
  plot(CellsWGS[CellsWGS$hillID == h,], col = 'black', add = TRUE, pch = 15, cex = 0.7)
  plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS$MaxGI > 0) & (CellsWGS$patchZ <= Downslope_h[h,2])),], col = 'purple', add = TRUE, pch = 15, cex = 0.7)
  plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS$MaxGI > 0) & (CellsWGS$patchZ > Downslope_h[h,2]) & (CellsWGS$patchZ <= Midslope_h[h,2])),], col = 'green', add = TRUE, pch = 15, cex = 0.7)
  plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS$MaxGI > 0) & (CellsWGS$patchZ > Midslope_h[h,2]) & (CellsWGS$patchZ <= Upslope_h[h,2])),], col = 'darkorange', add = TRUE, pch = 15, cex = 0.7)
}
rm(h)
#Add stream flowlines
plot(StreamsWGS, col = 'blue', add = TRUE, lwd = 2)
legend('bottomright', title = expression(bold('Possible GI Patches')), legend = c("Upslope", 'Midslope', 'Downslope', 'No GI Possible'), col = c('darkorange', 'green', 'purple', 'black'), pch = 15)
legend('topright', legend = c('Suburban Hillslope Outline', 'Approximate Stream Location'), col = c('gray', 'blue'), pch = c(22,NA), lty = c(NA, 1), lwd = 2, pt.cex = 2, pt.lwd = 5)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

#Check that number of patches is within 1 for each hillslope
AreaCount_h = CellCount_h = matrix(0, nrow = 6, ncol = 3)
for (h in 9:14){
  CellCount_h[h-8,1] = nrow(CellGrid[which((CellGrid$hillID == h) & (CellGrid$MaxGI > 0) & (CellGrid$elev <= Downslope_h[h,2])),])
  CellCount_h[h-8,2] = nrow(CellGrid[which((CellGrid$hillID == h) & (CellGrid$MaxGI > 0) & (CellGrid$elev > Downslope_h[h,2]) & (CellGrid$elev <= Midslope_h[h,2])),])
  CellCount_h[h-8,3] = nrow(CellGrid[which((CellGrid$hillID == h) & (CellGrid$MaxGI > 0) & (CellGrid$elev > Midslope_h[h,2]) & (CellGrid$elev <= Upslope_h[h,2])),])
  #Compare the maximum area of GI for each
  AreaCount_h[h-8,1] = sum(CellGrid$MaxGI[which((CellGrid$hillID == h) & (CellGrid$MaxGI > 0) & (CellGrid$elev <= Downslope_h[h,2]))])
  AreaCount_h[h-8,2] = sum(CellGrid$MaxGI[which((CellGrid$hillID == h) & (CellGrid$MaxGI > 0) & (CellGrid$elev > Downslope_h[h,2]) & (CellGrid$elev <= Midslope_h[h,2]))])
  AreaCount_h[h-8,3] = sum(CellGrid$MaxGI[which((CellGrid$hillID == h) & (CellGrid$MaxGI > 0) & (CellGrid$elev > Midslope_h[h,2]) & (CellGrid$elev <= Upslope_h[h,2]))])
}
rm(h)
colnames(AreaCount_h) = colnames(CellCount_h) = c('Downslope', 'Midslope', 'Upslope')
rownames(AreaCount_h) = rownames(CellCount_h) = seq(9,14,1)

#Add up, mid, downslope information to the max GI table
lcMax_gr = as.data.frame(lcMax_gr)
colnames(lcMax_gr) = c('patchID', 'MaxGI')
lcMax_gr$Upslope = lcMax_gr$Midslope = lcMax_gr$Downslope = 0
for (h in 9:14){
  lcMax_gr$Downslope[lcMax_gr[,1] %in% CellGrid$value[which((CellGrid$hillID == h) & (CellGrid$MaxGI > 0) & (CellGrid$elev <= Downslope_h[h,2]))]] = h
  lcMax_gr$Midslope[lcMax_gr[,1] %in% CellGrid$value[which((CellGrid$hillID == h) & (CellGrid$MaxGI > 0) & (CellGrid$elev > Downslope_h[h,2]) & (CellGrid$elev <= Midslope_h[h,2]))]] = h
  lcMax_gr$Upslope[lcMax_gr[,1] %in% CellGrid$value[which((CellGrid$hillID == h) & (CellGrid$MaxGI > 0) & (CellGrid$elev > Midslope_h[h,2]) & (CellGrid$elev <= Upslope_h[h,2]))]] = h
}
rm(h)

#Write a file containing the maximum GI allocation for each cell----
write.csv(lcMax_gr, 'MaxGI30m.csv', row.names = FALSE)

#Write a processed cell grid file for use in map plotting----
writeOGR(CellGrid, dsn = 'PatchGrid', layer = 'patch_p', driver = 'ESRI Shapefile')

#Save data----
save.image("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation\\CalcMaxGI_Aug17.RData")

#Proposal Land Use----
png('LandUseCoarse_proposal.png', res = 300, units = 'in', width = 6, height = 6)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex = 0.7)
plot(CellsWGS[CellsWGS$patchLandID == 1,], col = 'green', pch = 15, add = TRUE, cex = 0.7)
plot(CellsWGS[CellsWGS$patchLandID == 2,], col = 'darkgreen', pch = 15, add = TRUE, cex = 0.7)
plot(CellsWGS[CellsWGS$patchLandID == 3,], col = 'red', pch = 15, add = TRUE, cex = 0.7)
plot(CellsWGS[CellsWGS$patchLandID == 4,], col = 'red', pch = 15, add = TRUE, cex = 0.7)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

png('LandUseCoarse_hill_proposal.png', res = 300, units = 'in', width = 6, height = 6)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex = 0.7)
plot(CellsWGS[CellsWGS$patchLandID == 1,], col = 'green', pch = 15, add = TRUE, cex = 0.7)
plot(CellsWGS[CellsWGS$patchLandID == 2,], col = 'darkgreen', pch = 15, add = TRUE, cex = 0.7)
plot(CellsWGS[CellsWGS$patchLandID == 3,], col = 'orange', pch = 15, add = TRUE, cex = 0.7)
plot(CellsWGS[CellsWGS$patchLandID == 4,], col = 'orange', pch = 15, add = TRUE, cex = 0.7)
for (h in 9:14){
  plot(CellsWGS[CellsWGS$hillID == h,], col = 'gray', add = TRUE, lwd=7, pch = 22)
  plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS$patchLandID == 1)),], col = 'green', pch = 15, add = TRUE, cex = 0.7)
  plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS$patchLandID == 2)),], col = 'darkgreen', pch = 15, add = TRUE, cex = 0.7)
  plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS$patchLandID == 3)),], col = 'orange', pch = 15, add = TRUE, cex = 0.7)
  plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS$patchLandID == 4)),], col = 'orange', pch = 15, add = TRUE, cex = 0.7)
}
rm(h)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
#Add stream flowlines
plot(StreamsWGS, col = 'blue', add = TRUE, lwd = 2)
legend('bottomright', title = expression(bold('Land Cover')), legend = c("Developed", 'Undeveloped', 'Forest'), col = c('orange', 'green', 'darkgreen'), pch = 15)
legend('topright', legend = c('Suburban Hillslope Outline', 'Approximate Stream Location'), col = c('gray', 'blue'), pch = c(22,NA), lty = c(NA, 1), lwd = 2, pt.cex = 2, pt.lwd = 5)
dev.off()

png('LandUseCoarse_stream_proposal.png', res = 300, units = 'in', width = 6, height = 6)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex = 0.7)
plot(CellsWGS[CellsWGS$patchLandID == 1,], col = 'green', pch = 15, add = TRUE, cex = 0.7)
plot(CellsWGS[CellsWGS$patchLandID == 2,], col = 'darkgreen', pch = 15, add = TRUE, cex = 0.7)
plot(CellsWGS[CellsWGS$patchLandID == 3,], col = 'orange', pch = 15, add = TRUE, cex = 0.7)
plot(CellsWGS[CellsWGS$patchLandID == 4,], col = 'orange', pch = 15, add = TRUE, cex = 0.7)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
#Add stream flowlines
plot(StreamsWGS, col = 'blue', add = TRUE, lwd = 2)
legend('bottomright', title = expression(bold('Land Cover')), legend = c("Developed", 'Undeveloped', 'Forest'), col = c('orange', 'green', 'darkgreen'), pch = 15)
legend('topright', legend = c('Approximate Stream Location'), col = c('blue'), pch = c(NA), lty = c(1), lwd = 2, pt.cex = 2, pt.lwd = 5)
dev.off()

BufferOptionsWGS = spTransform(BufferOptions_grass, CRSobj = CRS('+init=epsg:4326'))
CenterOptionsWGS = spTransform(CenterOptions, CRSobj = CRS('+init=epsg:4326'))
lc1WGS = spTransform(lc1, CRSobj = CRS('+init=epsg:4326'))

png('LandUseFine_proposal.png', res = 300, units = 'in', width = 6, height = 6)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex = 0.7)
plot(lc1WGS[lc1WGS$BARN_1mLC_UTM == 1,], col = 'blue', cex = 0.05, add = T)
plot(lc1WGS[lc1WGS$BARN_1mLC_UTM == 3,], col = 'darkgreen', cex = 0.05, add = T)
plot(lc1WGS[lc1WGS$BARN_1mLC_UTM == 5,], col = 'green', cex = 0.05, add = T)
plot(lc1WGS[lc1WGS$BARN_1mLC_UTM == 6,], col = 'green', cex = 0.05, add = T)
plot(lc1WGS[lc1WGS$BARN_1mLC_UTM %in% c(7,8,9,10,11,12),], col = 'red', cex = 0.05, add = T)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

colLU = scico(4, palette = scico_palette_names()[22])

png('LandUseFine_proposal_NewCols.png', res = 300, units = 'in', width = 6, height = 6)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex = 0.7)
plot(lc1WGS[lc1WGS$BARN_1mLC_UTM == 1,], col = colLU[4], cex = 0.05, add = T)
plot(lc1WGS[lc1WGS$BARN_1mLC_UTM == 3,], col = colLU[2], cex = 0.05, add = T)
plot(lc1WGS[lc1WGS$BARN_1mLC_UTM == 5,], col = colLU[3], cex = 0.05, add = T)
plot(lc1WGS[lc1WGS$BARN_1mLC_UTM == 6,], col = colLU[3], cex = 0.05, add = T)
plot(lc1WGS[lc1WGS$BARN_1mLC_UTM %in% c(7,8,9,10,11,12),], col = colLU[1], cex = 0.05, add = T)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

#Test what green and gray look like in grayscale
plot(seq(0,4,1), col = c(col2gray('green'), col2gray('darkgreen'), col2gray('blue'), col2gray('black'), gray(0.9)), pch = 15)
#Converter: https://onlineimagetools.com/grayscale-image

png('LandUseFine_SApaperNoHillslopes.png', res = 300, units = 'in', width = 6, height = 6)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex = 0.7)
plot(lc1WGS[which((lc1WGS$BARN_1mLC_UTM == 1)),], col = 'blue', cex = 0.05, add = T)
plot(lc1WGS[which((lc1WGS$BARN_1mLC_UTM == 3)),], col = 'darkgreen', cex = 0.05, add = T)
plot(lc1WGS[which((lc1WGS$BARN_1mLC_UTM == 5)),], col = 'green', cex = 0.05, add = T)
plot(lc1WGS[which((lc1WGS$BARN_1mLC_UTM == 6)),], col = 'green', cex = 0.05, add = T)
plot(lc1WGS[which((lc1WGS$BARN_1mLC_UTM %in% c(7,8,9,10,11,12))),], col = 'black', cex = 0.05, add = T)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
legend('topleft', pch = 15, legend = c('Impervious', 'Forested', 'Grass'), col = c('black', 'darkgreen', 'green'))
dev.off()

pdf(file = 'LandUseFine_SApaperNoHillslopes.pdf', width = 6, height = 6, title = 'SAfig2a')
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'darkgreen', pch = 15, lwd = 0, cex = 0.7)
plot(lc1WGS[which((lc1WGS$BARN_1mLC_UTM == 1)),], col = 'blue', cex = 0.05, add = T, pch = 15)
plot(lc1WGS[which((lc1WGS$BARN_1mLC_UTM %in% c(5,6))),], col = 'green', cex = 0.05, add = T, pch = 15)
plot(lc1WGS[which((lc1WGS$BARN_1mLC_UTM %in% c(7,8,9,10,11,12))),], col = 'black', cex = 0.05, add = T, pch = 15)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
legend('topleft', pch = 15, legend = c('Impervious', 'Forested', 'Grass'), col = c('black', 'darkgreen', 'green'))
dev.off()

png('LandUseFine_SApaper2.png', res = 300, units = 'in', width = 6, height = 6)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = NA, pch = 15, lwd = 0, cex = 0.7)
for (h in 1:14){
  temp = CellsWGS[CellsWGS$hillID == h,]
  plot(temp, col = gray(0.9), add = TRUE, lwd=7, pch = 22)
  plot(temp, col = 'darkgreen', add = TRUE, pch = 15, cex = 0.7)
  plot(lc1WGS[which((lc1WGS$hillID == h) & (lc1WGS$BARN_1mLC_UTM == 1)),], col = 'blue', cex = 0.05, add = T, pch = 15)
  plot(lc1WGS[which((lc1WGS$hillID == h) & (lc1WGS$BARN_1mLC_UTM %in% c(5,6))),], col = 'green', cex = 0.05, add = T, pch = 15)
  plot(lc1WGS[which((lc1WGS$hillID == h) & (lc1WGS$BARN_1mLC_UTM %in% c(7,8,9,10,11,12))),], col = 'black', cex = 0.05, add = T, pch = 15)
}
rm(temp,h)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
legend('topleft', pch = 15, legend = c('Impervious', 'Forested', 'Grass'), col = c('black', 'darkgreen', 'green'))
legend('topright', legend = c('Hillslope Outline'), col = gray(0.9), pch = 22, pt.cex = 2, pt.lwd=3)
dev.off()

pdf(file = 'LandUseFine_SApaper.pdf', width = 6, height = 6, title = 'SAfig2a')
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = NA, pch = 15, lwd = 0, cex = 0.7)
for (h in 1:14){
  temp = CellsWGS[CellsWGS$hillID == h,]
  plot(temp, col = gray(0.9), add = TRUE, lwd=7, pch = 22)
  plot(temp, col = 'darkgreen', add = TRUE, pch = 15, cex = 0.7)
  plot(lc1WGS[which((lc1WGS$hillID == h) & (lc1WGS$BARN_1mLC_UTM == 1)),], col = 'blue', cex = 0.05, add = T, pch = 15)
  plot(lc1WGS[which((lc1WGS$hillID == h) & (lc1WGS$BARN_1mLC_UTM %in% c(5,6))),], col = 'green', cex = 0.05, add = T, pch = 15)
  plot(lc1WGS[which((lc1WGS$hillID == h) & (lc1WGS$BARN_1mLC_UTM %in% c(7,8,9,10,11,12))),], col = 'black', cex = 0.05, add = T, pch = 15)
}
rm(temp,h)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
legend('topleft', pch = 15, legend = c('Impervious', 'Forested', 'Grass'), col = c('black', 'darkgreen', 'green'))
legend('topright', legend = c('Hillslope Outline'), col = gray(0.9), pch = 22, pt.cex = 2, pt.lwd=3)
dev.off()

SoilCols = scico(n = 4, palette = 'vik')
png('Soils_SApaper.png', res = 300, units = 'in', width = 6, height = 6)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex = 0.7)
for (h in 1:14){
  temp = CellsWGS[CellsWGS$hillID == h,]
  plot(temp, col = gray(0.9), add = TRUE, lwd=7, pch = 22)
  plot(temp, col = 'black', add = TRUE, pch = 15, cex = 0.7)
  plot(CellsWGS[which((CellsWGS$hillID == h) & (Cells$patchsoilID == 8)),], col = SoilCols[1], add = T, pch = 15,cex=0.7)
  plot(CellsWGS[which((CellsWGS$hillID == h) & (Cells$patchsoilID == 108)),], col = SoilCols[2], add = T, pch = 15,cex=0.7)
  plot(CellsWGS[which((CellsWGS$hillID == h) & (Cells$patchsoilID == 9)),], col = SoilCols[3], add = T, pch = 15,cex=0.7)
  plot(CellsWGS[which((CellsWGS$hillID == h) & (Cells$patchsoilID == 109)),], col = SoilCols[4], add = T, pch = 15,cex=0.7)
}
rm(temp,h)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
legend('bottomright', title = expression(bold('Soils')), pch = 15, legend = c('Riparian S8', 'Riparian S108', 'Other S9', 'Other S109'), col = SoilCols)
legend('topright', legend = c('Hillslope Outline'), col = gray(0.9), pch = 22, pt.cex = 2, pt.lwd=3)
dev.off()

pdf(file = 'Soils_SApaper.pdf', width = 6, height = 6, title = 'SAfig2b')
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex = 0.7)
for (h in 1:14){
  temp = CellsWGS[CellsWGS$hillID == h,]
  plot(temp, col = gray(0.9), add = TRUE, lwd=7, pch = 22)
  plot(temp, col = 'black', add = TRUE, pch = 15, cex = 0.7)
  plot(CellsWGS[which((CellsWGS$hillID == h) & (Cells$patchsoilID == 8)),], col = SoilCols[1], add = T, pch = 15,cex=0.7)
  plot(CellsWGS[which((CellsWGS$hillID == h) & (Cells$patchsoilID == 108)),], col = SoilCols[2], add = T, pch = 15,cex=0.7)
  plot(CellsWGS[which((CellsWGS$hillID == h) & (Cells$patchsoilID == 9)),], col = SoilCols[3], add = T, pch = 15,cex=0.7)
  plot(CellsWGS[which((CellsWGS$hillID == h) & (Cells$patchsoilID == 109)),], col = SoilCols[4], add = T, pch = 15,cex=0.7)
}
rm(temp,h)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
legend('bottomright', title = expression(bold('Soils')), pch = 15, legend = c('Riparian S8', 'Riparian S108', 'Other S9', 'Other S109'), col = SoilCols)
legend('topright', legend = c('Hillslope Outline'), col = gray(0.9), pch = 22, pt.cex = 2, pt.lwd=3)
dev.off()

png('Panel_LandUseFine_UpMidDownslope_MOROpaper.png', res = 300, units = 'in', width = 12, height = 6)
layout(rbind(c(1,2)))
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = NA, pch = 15, lwd = 0, cex = 0.7)
for (h in 1:8){
  temp = CellsWGS[CellsWGS$hillID == h,]
  plot(temp, col = 'darkgreen', add = TRUE, pch = 15, cex = 0.7)
  plot(lc1WGS[which((lc1WGS$hillID == h) & (lc1WGS$BARN_1mLC_UTM == 1)),], col = 'blue', cex = 0.05, add = T, pch = 15)
  plot(lc1WGS[which((lc1WGS$hillID == h) & (lc1WGS$BARN_1mLC_UTM %in% c(5,6))),], col = 'green', cex = 0.05, add = T, pch = 15)
  plot(lc1WGS[which((lc1WGS$hillID == h) & (lc1WGS$BARN_1mLC_UTM %in% c(7,8,9,10,11,12))),], col = 'black', cex = 0.05, add = T, pch = 15)
}
for (h in 14:9){
  if(h == 9| h == 10){
    plot(CellsWGS[CellsWGS$hillID == h,], col = 'gray', add = TRUE, lwd=7, pch = 22)
  }else if (h %in% seq(11,14,1)){
    plot(CellsWGS[CellsWGS$hillID == h,], col = 'gray20', add = TRUE, lwd=7, pch = 22)
  }
  temp = CellsWGS[CellsWGS$hillID == h,]
  plot(temp, col = 'darkgreen', add = TRUE, pch = 15, cex = 0.7)
  plot(lc1WGS[which((lc1WGS$hillID == h) & (lc1WGS$BARN_1mLC_UTM == 1)),], col = 'blue', cex = 0.05, add = T, pch = 15)
  plot(lc1WGS[which((lc1WGS$hillID == h) & (lc1WGS$BARN_1mLC_UTM %in% c(5,6))),], col = 'green', cex = 0.05, add = T, pch = 15)
  plot(lc1WGS[which((lc1WGS$hillID == h) & (lc1WGS$BARN_1mLC_UTM %in% c(7,8,9,10,11,12))),], col = 'black', cex = 0.05, add = T, pch = 15)
}
rm(temp,h)
#Add stream flowlines
plot(StreamsWGS, col = 'blue', add = TRUE, lwd = 2)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
legend('topleft', pch = 15, legend = c('Impervious', 'Forested', 'Grass'), col = c('black', 'darkgreen', 'green'))
legend('bottomright', legend = c('Suburban Hillslope Outline', 'Selected for Optimization', 'Stream'), 
       col = c('gray20', 'gray', 'blue'), pch = c(22, 22, NA), lty = c(NA, NA, 1), lwd = 2, pt.cex = 2, pt.lwd = 5)
par(xpd = TRUE)
text(x = -76.717, y = 39.495, 'A', cex = 2)
par(xpd = FALSE)

#Decision vars
plot(CellsWGS, col = 'black', pch = 15, lwd = 0)
#plot(CellGrid[CellGrid$MaxGI > 0,], add = TRUE, col = 'black')
for (h in 14:9){
  if(h == 9| h == 10){
    plot(CellsWGS[CellsWGS$hillID == h,], col = 'gray', add = TRUE, lwd=7, pch = 22)
  }else{
    plot(CellsWGS[CellsWGS$hillID == h,], col = 'gray20', add = TRUE, lwd=7, pch = 22)
  }
  plot(CellsWGS[CellsWGS$hillID == h,], col = 'black', add = TRUE, pch = 15, cex = 0.7)
  plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS$MaxGI > 0) & (CellsWGS$patchZ <= Downslope_h[h,2])),], col = 'purple', add = TRUE, pch = 15, cex = 0.7)
  plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS$MaxGI > 0) & (CellsWGS$patchZ > Downslope_h[h,2]) & (CellsWGS$patchZ <= Midslope_h[h,2])),], col = 'green', add = TRUE, pch = 15, cex = 0.7)
  plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS$MaxGI > 0) & (CellsWGS$patchZ > Midslope_h[h,2]) & (CellsWGS$patchZ <= Upslope_h[h,2])),], col = 'darkorange', add = TRUE, pch = 15, cex = 0.7)
}
rm(h)
#Add stream flowlines
plot(StreamsWGS, col = 'blue', add = TRUE, lwd = 2)
legend('bottomright', title = expression(bold('Possible GI Patches')), legend = c("Upslope", 'Midslope', 'Downslope', 'No GI Possible'), col = c('darkorange', 'green', 'purple', 'black'), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
par(xpd = TRUE)
text(x = -76.717, y = 39.495, 'B', cex = 2)
par(xpd = FALSE)
dev.off()

pdf('Panel_LandUseFine_UpMidDownslope_MOROpaper.pdf', width = 12, height = 6)
layout(rbind(c(1,2)))
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = NA, pch = 15, lwd = 0, cex = 0.7)
for (h in 1:8){
  temp = CellsWGS[CellsWGS$hillID == h,]
  plot(temp, col = 'darkgreen', add = TRUE, pch = 15, cex = 0.7)
  plot(lc1WGS[which((lc1WGS$hillID == h) & (lc1WGS$BARN_1mLC_UTM == 1)),], col = 'blue', cex = 0.05, add = T, pch = 15)
  plot(lc1WGS[which((lc1WGS$hillID == h) & (lc1WGS$BARN_1mLC_UTM %in% c(5,6))),], col = 'green', cex = 0.05, add = T, pch = 15)
  plot(lc1WGS[which((lc1WGS$hillID == h) & (lc1WGS$BARN_1mLC_UTM %in% c(7,8,9,10,11,12))),], col = 'black', cex = 0.05, add = T, pch = 15)
}
for (h in 14:9){
  if(h == 9| h == 10){
    plot(CellsWGS[CellsWGS$hillID == h,], col = 'gray', add = TRUE, lwd=7, pch = 22)
  }else if (h %in% seq(11,14,1)){
    plot(CellsWGS[CellsWGS$hillID == h,], col = 'gray20', add = TRUE, lwd=7, pch = 22)
  }
  temp = CellsWGS[CellsWGS$hillID == h,]
  plot(temp, col = 'darkgreen', add = TRUE, pch = 15, cex = 0.7)
  plot(lc1WGS[which((lc1WGS$hillID == h) & (lc1WGS$BARN_1mLC_UTM == 1)),], col = 'blue', cex = 0.05, add = T, pch = 15)
  plot(lc1WGS[which((lc1WGS$hillID == h) & (lc1WGS$BARN_1mLC_UTM %in% c(5,6))),], col = 'green', cex = 0.05, add = T, pch = 15)
  plot(lc1WGS[which((lc1WGS$hillID == h) & (lc1WGS$BARN_1mLC_UTM %in% c(7,8,9,10,11,12))),], col = 'black', cex = 0.05, add = T, pch = 15)
}
rm(temp,h)
#Add stream flowlines
plot(StreamsWGS, col = 'blue', add = TRUE, lwd = 2)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
legend('topleft', pch = 15, legend = c('Impervious', 'Forested', 'Grass'), col = c('black', 'darkgreen', 'green'))
legend('bottomright', legend = c('Suburban Hillslope Outline', 'Selected for Optimization', 'Stream'), 
       col = c('gray20', 'gray', 'blue'), pch = c(22, 22, NA), lty = c(NA, NA, 1), lwd = 2, pt.cex = 2, pt.lwd = 5)
par(xpd = TRUE)
text(x = -76.717, y = 39.495, 'A', cex = 2)
par(xpd = FALSE)

#Decision vars
plot(CellsWGS, col = 'black', pch = 15, lwd = 0)
#plot(CellGrid[CellGrid$MaxGI > 0,], add = TRUE, col = 'black')
for (h in 14:9){
  if(h == 9| h == 10){
    plot(CellsWGS[CellsWGS$hillID == h,], col = 'gray', add = TRUE, lwd=7, pch = 22)
  }else{
    plot(CellsWGS[CellsWGS$hillID == h,], col = 'gray20', add = TRUE, lwd=7, pch = 22)
  }
  plot(CellsWGS[CellsWGS$hillID == h,], col = 'black', add = TRUE, pch = 15, cex = 0.7)
  plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS$MaxGI > 0) & (CellsWGS$patchZ <= Downslope_h[h,2])),], col = 'purple', add = TRUE, pch = 15, cex = 0.7)
  plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS$MaxGI > 0) & (CellsWGS$patchZ > Downslope_h[h,2]) & (CellsWGS$patchZ <= Midslope_h[h,2])),], col = 'green', add = TRUE, pch = 15, cex = 0.7)
  plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS$MaxGI > 0) & (CellsWGS$patchZ > Midslope_h[h,2]) & (CellsWGS$patchZ <= Upslope_h[h,2])),], col = 'darkorange', add = TRUE, pch = 15, cex = 0.7)
}
rm(h)
#Add stream flowlines
plot(StreamsWGS, col = 'blue', add = TRUE, lwd = 2)
legend('bottomright', title = expression(bold('Possible GI Patches')), legend = c("Upslope", 'Midslope', 'Downslope', 'No GI Possible'), col = c('darkorange', 'green', 'purple', 'black'), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
par(xpd = TRUE)
text(x = -76.717, y = 39.495, 'B', cex = 2)
par(xpd = FALSE)
dev.off()

#Get the Max GI for only hillslopes 9 and 10----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12")
lcMax_gr910 = lcMax_gr[lcMax_gr$patchID %in% Cells$patchID[Cells$hillID %in% c(9,10)],]

#Reassign the patchID number to match the RHESSys setup for only those hillslopes
Cells910 = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\worldfile.csv", stringsAsFactors = FALSE)
coordinates(Cells910) = c('patchX', 'patchY')
proj4string(Cells910) = CRS('+init=epsg:26918')

lcMax_gr910$patchID2 = 0
for (i in 1:nrow(lcMax_gr910)){
  lcMax_gr910$patchID2[i] = Cells910$patchID[which((Cells910$patchX == Cells$patchX[Cells$patchID == lcMax_gr910$patchID[i]][1]) & (Cells910$patchY == Cells$patchY[Cells$patchID == lcMax_gr910$patchID[i]][1]))][1]
}
#These are the same! Drop the patchID2 column and save
lcMax_gr910 = lcMax_gr910[,-which(colnames(lcMax_gr910) == 'patchID2')]

write.csv(lcMax_gr910, 'MaxGI30m910.csv', row.names = FALSE)