#Script to compile streamflows into a matrix for hillslopes
#Arguments:
#1: patch resolution
#2: main working directory
#3: project directory name
#4: number of replicates
#5: Starting date
#6: Replicate starting number
#7: File end name
arg = commandArgs(T)

library(vroom)

startNum = as.numeric(arg[6])
numReps = as.numeric(arg[4])

#Compute streamflow conversion factor----
world = read.csv(paste0(arg[2], arg[3], '/worldfiles/worldfile.csv'), stringsAsFactors = FALSE)
res = as.numeric(arg[1])

#Get hillslope areas and conversion factor for streamflow in hillslopes
uhills = unique(world$hillID)
Area.Hills = matrix(NA, nrow = length(uhills), ncol = 2)
#Multiplier conversion for streamflow (mm/d)*conversion_b -> cfs
conversion_h = matrix(NA, nrow = length(uhills), ncol = 2)
for (h in 1:length(uhills)){
  Area.Hills[h,1] = uhills[h]
  conversion_h[h,1] = uhills[h]
  #some patches have multiple strata, so their area cannot be counted from the count of cells.
  Area.Hills[h,2] = length(which(world[which(duplicated(world$patchID) == FALSE),]$hillID == uhills[h]))*res^2
  conversion_h[h,2] = Area.Hills[h,2]/1000/(.3048^3)/24/3600
}
rm(h)

#Aggregate into one file to matplot
setwd(paste0(arg[2], 'RHESSysRuns/Run', startNum, '/', arg[3], '/output'))
#Read in simulated basin streamflow
Q1 = vroom(paste0(getwd(), '/Run', startNum, '_basin.daily'), delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
#Make a new Date column
Q1$Date = as.Date(paste0(Q1$year, '-', Q1$month, '-', Q1$day))
#Retain only streamflow and Date columns for space
Q1 = as.data.frame(Q1[,c('Date','streamflow')])
#Trim off spin-up years
Q1 = Q1[which(as.Date(Q1$Date) >= as.Date(arg[5])),]

# Loop through all of the folders and extract the data needed----
HillStreamflow = HillSatDef = HillDetStore = HillET = matrix(NA, nrow = numReps*length(uhills), ncol = (2 + nrow(Q1)))

for (i in 1:numReps){
  Ind = seq(startNum, startNum+numReps-1, 1)
  setwd(paste0(arg[2], 'RHESSysRuns/Run', Ind[i], '/', arg[3], '/output'))
  hs = vroom(paste0(getwd(), '/Run', Ind[i], '_hillslope.daily'), delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
  
  #Make a new date column
  hs$Date = as.Date(paste0(hs$year, '-', hs$month, '-', hs$day))
  
  #Trim off spin-up years
  hs = hs[which(as.Date(hs$Date) >= as.Date(arg[5])),]
  
  #Save basin and hillslope timeseries----
  IndSave = i
  for (h in 1:length(uhills)){
    #Streamflow with conversion
    HillStreamflow[IndSave + numReps*(h-1),] = c(IndSave, uhills[h], hs$streamflow[hs$hillID == uhills[h]]*conversion_h[conversion_h[,1] == uhills[h], 2])
    HillSatDef[IndSave + numReps*(h-1),] = c(IndSave, uhills[h], hs$sat_def[hs$hillID == uhills[h]])
    HillDetStore[IndSave + numReps*(h-1),] = c(IndSave, uhills[h], hs$detention_store[hs$hillID == uhills[h]])
    HillET[IndSave + numReps*(h-1),] = c(IndSave, uhills[h], hs$evap[hs$hillID == uhills[h]] + hs$trans[hs$hillID == uhills[h]])
  }
  rm(h, hs, IndSave)
}

#  Make matrices into dataframes----
HillStreamflow = as.data.frame(HillStreamflow)
HillSatDef = as.data.frame(HillSatDef)
HillDetStore = as.data.frame(HillDetStore)
HillET = as.data.frame(HillET)
colnames(HillStreamflow) = colnames(HillSatDef) = colnames(HillDetStore) = colnames(HillET) = c('Replicate', 'HillID', as.character(Q1$Date))

#Save files
setwd(arg[2])
options(scipen = 999)
write.table(HillStreamflow, file = paste0(getwd(), '/FlowGI', arg[7], '_c_h.txt'), col.names = TRUE, row.names = FALSE, sep = '\t')
write.table(HillSatDef, file = paste0(getwd(), '/SatDefGI', arg[7], '_c_h.txt'), col.names = TRUE, row.names = FALSE, sep = '\t')
write.table(HillDetStore, file = paste0(getwd(), '/DetStoreGI', arg[7], '_c_h.txt'), col.names = TRUE, row.names = FALSE, sep = '\t')
write.table(HillET, file = paste0(getwd(), '/ETGI', arg[7], '_c_h.txt'), col.names = TRUE, row.names = FALSE, sep = '\t')
options(scipen = 0)