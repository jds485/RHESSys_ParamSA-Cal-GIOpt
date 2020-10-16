#Script to allocate Green Infrastructure to grid cells

print('Start GI Allocation')

#Get original working directory
od = getwd()

#Arguments:
#1: random seed
#2: csv file describing the maximum GI allocation for each patch ('MaxGI30m.csv')
#3: csv file with LULC for every patch ('lulcFrac30m.csv')
#4: directory to files #2 and #3 ('C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation')
#5: Number of hillslopes whose patches can have GI (6)
#6: Number of decision variables for each hillslope (3)
#7: Output filename ('lulcFrac30m_GI.csv')
#8: GI resolution (side of a square), in meters ('9')
#9: Total patch area, m2 ('900')
#10: Decision Variable file name
arg = commandArgs(T)

setwd(arg[4])

#Read in the file generated from the c++ function
#Entries are what fraction of each hillslope section to change to GI
# columns: 9 Downslope, 9 Midslope, 9 Upslope, 10 Downslope, 10 Midslope, 10 Upslope
fd = as.numeric(read.table(arg[10], header = FALSE, sep = '\t', stringsAsFactors = FALSE))

#reformat data into a matrix for calculation in the function below
HillPct = matrix(rep(0,as.numeric(arg[5])*as.numeric(arg[6])), nrow = as.numeric(arg[5]), ncol = as.numeric(arg[6]))
HillPct[1,] = fd[1:3]
HillPct[2,] = fd[4:6]

#Read in the maximum possible GI allocation amount for each patch.
MaxGI = read.csv(arg[2], stringsAsFactors = FALSE)

#Read in the original aggregated 30 m res LULC file
lc30 = read.csv(arg[3], stringsAsFactors = FALSE)
#Add GI columns if they do not already exist
if (!('lulc13' %in% colnames(lc30))){
  lc30$lulc16 = lc30$lulc15 = lc30$lulc14 = lc30$lulc13 = 0
}

#Allocate GI randomly within each hillslopes----
set.seed(as.numeric(arg[1]))

#Fixme: this loop is not generalized.
#Loop over hillslopes
for (h in 9:10){
  #Loop over locations
  for (l in 1:as.numeric(arg[6])){
    #Only add GI if the proportion is larger than 0
    if (HillPct[h-8,l] > 0){
      #Select the patches in this hillslope location
      selCells = MaxGI[MaxGI[,2+l] == h, 1:2]
      if (((sum(selCells[,2])*HillPct[h-8,l]) %% as.numeric(arg[8])) != 0){
        #Determine how much total area should be converted and round up to the nearest multiple of as.numeric(arg[8])
        AreaAlloc = sum(selCells[,2])*HillPct[h-8,l] + (as.numeric(arg[8]) - ((sum(selCells[,2])*HillPct[h-8,l]) %% as.numeric(arg[8])))
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
        if (selCells$MaxGI[Ind] >= as.numeric(arg[8])){
          #If space is available, allocate a random amount of GI between the min and max possible for the patch.
          area1 = runif(n = 1, min = 0, max = selCells$MaxGI[Ind])
          if ((area1 %% as.numeric(arg[8])) != 0){
            #round up to the nearest multiple of as.numeric(arg[8])
            area2 = area1 + (as.numeric(arg[8]) - (area1 %% as.numeric(arg[8])))
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

#Check that the sum of each row = total area (as.numeric(arg[9]))
if (any(rowSums(lc30[,-c(1,2)]) != as.numeric(arg[9]))){
  stop(paste('Some rows do not sum to', arg[9]))
}

#Output the lulcFrac30m.csv file with the updated GI changes
write.csv(lc30, file = arg[7], row.names = FALSE)

#Set original working directory
setwd(od)
print('End GI Allocation')