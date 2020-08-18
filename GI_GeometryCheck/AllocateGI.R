#Script to allocate Green Infrastructure to grid cells

print('Start GI Allocation')

#Fixme: Add once completed
#Arguments:
#1: random seed
#2: path to csv file describing the maximum GI allocation for each patch
#3: path to csv file with LULC for every patch
#4: directory to file #3
#5: Number of hillslopes whose patches can have GI
#6: Output filename
#arg = commandArgs(T)

#setwd(arg[4])

#Fixme: read in an externally generated file?
# It would be nice if this file in the name or in the contents contained an identifier 
# for the parameter set and the optimization replicate so that they can be used for the
# random seed selection and (possibly) the file names.
#Input from Borg is what fraction of each hillslope+location(up, mid, downslope) to change to GI
# One row per hillslope, 9-14
# columns: Downslope, Midslope, Upslope to match MaxGI column order
HillPct = matrix(seq(0.1,0.27,0.01), nrow = 6, ncol = 3)
#HillPct = matrix(seq(0.7,0.87,0.01), nrow = 6, ncol = 3)
#HillPct = matrix(rep(1,18), nrow = 6, ncol = 3)
#HillPct = matrix(rep(0,18), nrow = 6, ncol = 3)

#Read in the maximum possible GI allocation amount for each patch.
MaxGI = read.csv('C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation\\MaxGI30m.csv', stringsAsFactors = FALSE)

#Read in the original aggregated 30 m res LULC file
lc30 = read.csv('C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation\\lulcFrac30m.csv', stringsAsFactors = FALSE)
#Add GI columns
lc30$lulc16 = lc30$lulc15 = lc30$lulc14 = lc30$lulc13 = 0

#Allocate GI randomly within each hillslopes----
#Random seed based on Borg replicate
set.seed(20811)

#Loop over hillslopes
for (h in 9:14){
  #Loop over locations
  for (l in 1:3){
    #Only add GI if the proportion is larger than 0
    if (HillPct[h-8,l] > 0){
      #Select the patches in this hillslope location
      selCells = MaxGI[MaxGI[,2+l] == h, 1:2]
      if (((sum(selCells[,2])*HillPct[h-8,l]) %% 9) != 0){
        #Determine how much total area should be converted and round up to the nearest multiple of 9
        AreaAlloc = sum(selCells[,2])*HillPct[h-8,l] + (9 - ((sum(selCells[,2])*HillPct[h-8,l]) %% 9))
      }else{
        AreaAlloc = sum(selCells[,2])*HillPct[h-8,l]
      }
      #Make sure this is not more than the max, and set to max if it is
      if (AreaAlloc > sum(selCells[,2])){
        AreaAlloc = sum(selCells[,2])
        print('Calculated area of GI to allocate is larger than the max. Setting to maximum area.')
      }
      
      #Allocate GI
      while(AreaAlloc > 0){
        #Randomly select a patch to allocate GI
        Ind = ceiling(runif(n = 1, min = 0, max = nrow(selCells)))
        if (selCells$MaxGI[Ind] >= 9){
          #If space is available, allocate a random amount of GI between the min and max possible for the patch.
          area1 = runif(n = 1, min = 0, max = selCells$MaxGI[Ind])
          if ((area1 %% 9) != 0){
            #round up to the nearest multiple of 9
            area2 = area1 + (9 - (area1 %% 9))
          }else{
            area2 = area1
          }
          #Check if area2 is greater than AreaAlloc, and set to AreaAlloc if it is
          if (area2 > AreaAlloc){
            area2 = AreaAlloc
          }
          
          #Update the maximum possible area to cover for this patch
          selCells$MaxGI[Ind] = selCells$MaxGI[Ind] - area2
          
          #Update the lc30 file
          #Add to Forest GI cover
          lc30$lulc13[lc30$patchID == selCells$patchID[Ind]] = lc30$lulc13[lc30$patchID == selCells$patchID[Ind]] + area2
          #Subtract from Grass cover
          lc30$lulc5[lc30$patchID == selCells$patchID[Ind]] = lc30$lulc5[lc30$patchID == selCells$patchID[Ind]] - area2
          
          #Update area to be allocated
          AreaAlloc = AreaAlloc - area2
          rm(area1, area2)
        }
        #If this patch is full, do not allocate and instead select a new patch
      }
      
      #Make sure the grass is >=0
      if (any(lc30$lulc5 < 0)){
        print('Some grass have areas less than 0.')
      }
      
      #Check the max requirements for each patch
      if (any(lc30$lulc13 > MaxGI$MaxGI)){
        print('Some GI have areas greater than their max.')
      }
    }
  }
}
rm(h, l, AreaAlloc, Ind, selCells)

#Check that the sum of each row = total area (900)
if (any(rowSums(lc30[,-c(1,2)]) != 900)){
  stop('Some rows do not sum to 900')
}

#Output the lulcFrac30m.csv file with the updated GI changes
write.csv(lc30, file = 'lulcFrac30m_GI.csv', row.names = FALSE)

print('End GI Allocation')


#Plot of the GI fraction in each patch as percentage of the maximum possible----
library(sp)
library(rgdal)

#Load color functions - from JDS github repo: Geothermal_ESDA
source('C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\Hydrology\\USGSGauges\\ColorFunctions.R')

#Load processed cell grid file
CellGrid = readOGR(dsn = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation\\PatchGrid", layer = 'patch_p', stringsAsFactors = FALSE)

scaleRange = c(0,1)
scaleBy = 0.2
Pal = rev(rainbow((scaleRange[2] - scaleRange[1])/scaleBy))

png('GI_pctMax.png', res = 300, units = 'in', width = 5, height = 5)
par(mar= c(0,0,0,0))
#All cells
plot(CellGrid, col = 'white')
#All possible GI locations
plot(CellGrid[order(CellGrid$value, decreasing = FALSE),][MaxGI$MaxGI > 0,], pch = 15, col = 'black', add = T)
#Where GI were located
plot(CellGrid[order(CellGrid$value, decreasing = FALSE),][lc30$lulc13 > 0,], pch = 15, col = colFun(lc30$lulc13[lc30$lulc13 > 0]/MaxGI$MaxGI[lc30$lulc13 > 0]), add = T)
legend('bottomright', title = expression(bold("GI / Max Possible")), legend = c("0", ">0 - <0.2", "0.2 - <0.4", "0.4 - <0.6", "0.6 - <0.8", "0.8 - 1", 'No GI Possible'), col = c('black', colFun(seq(scaleRange[1], scaleRange[2]-scaleBy, scaleBy)), 'black'), pch = c(rep(15,6), 22))
dev.off()