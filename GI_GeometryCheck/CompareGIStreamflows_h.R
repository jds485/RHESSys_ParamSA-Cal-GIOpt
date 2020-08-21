#Script to compile streamflows into a matrix for hillslopes

library(vroom)

numReps = 100

#Compute streamflow conversion factor----
world = read.csv('/scratch/js4yd/GI_RandomSeedEval/RHESSys_Baisman30m_g74/worldfiles/worldfile.csv', stringsAsFactors = FALSE)
res = 30

#Get hillslope areas and conversion factor for streamflow in hillslopes
uhills = unique(world$hillID)
Area.Hills = matrix(NA, nrow = length(uhills), ncol = 2)
#Multiplier conversion for streamflow (mm/d)*conversion_b -> cfs
conversion_h = matrix(NA, nrow = length(uhills), ncol = 2)
for (h in 1:length(uhills)){
  Area.Hills[h,1] = h
  conversion_h[h,1] = h
  #some patches have multiple strata, so their area cannot be counted from the count of cells.
  Area.Hills[h,2] = length(which(world[which(duplicated(world$patchID) == FALSE),]$hillID == h))*res^2
  conversion_h[h,2] = Area.Hills[h,2]/1000/(.3048^3)/24/3600
}
rm(h)

#Aggregate into one file to matplot
setwd('/scratch/js4yd/GI_RandomSeedEval/RHESSysRuns/Run1/RHESSys_Baisman30m_g74/output')
#Read in simulated basin streamflow
Q1 = vroom(paste0(getwd(), '/Run1_basin.daily'), delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
#Make a new Date column
Q1$Date = as.Date(paste0(Q1$year, '-', Q1$month, '-', Q1$day))
#Retain only streamflow and Date columns for space
Q1 = as.data.frame(Q1[,c('Date','streamflow')])
#Trim off spin-up years
Q1 = Q1[which(as.Date(Q1$Date) >= as.Date('2004-10-01')),]

# Loop through all of the folders and extract the data needed----
HillStreamflow = HillSatDef = HillDetStore = HillET = matrix(NA, nrow = numReps*length(uhills), ncol = (2 + nrow(Q1)))

for (i in 1:numReps){
  setwd(paste0('/scratch/js4yd/GI_RandomSeedEval/RHESSysRuns/Run', i, '/RHESSys_Baisman30m_g74/output'))
  hs = vroom(paste0(getwd(), '/Run', i, '_hillslope.daily'), delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
  
  #Make a new date column
  hs$Date = as.Date(paste0(hs$year, '-', hs$month, '-', hs$day))
  
  #Trim off spin-up years
  hs = hs[which(as.Date(hs$Date) >= as.Date('2004-10-01')),]
  
  #Save basin and hillslope timeseries----
  IndSave = i
  for (h in 1:length(uhills)){
    #Streamflow with conversion
    HillStreamflow[IndSave + numReps*(uhills[h]-1),] = c(IndSave, uhills[h], hs$streamflow[hs$hillID == uhills[h]]*conversion_h[conversion_h[,1] == uhills[h], 2])
    HillSatDef[IndSave + numReps*(uhills[h]-1),] = c(IndSave, uhills[h], hs$sat_def[hs$hillID == uhills[h]])
    HillDetStore[IndSave + numReps*(uhills[h]-1),] = c(IndSave, uhills[h], hs$detention_store[hs$hillID == uhills[h]])
    HillET[IndSave + numReps*(uhills[h]-1),] = c(IndSave, uhills[h], hs$evap[hs$hillID == uhills[h]] + hs$trans[hs$hillID == uhills[h]])
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
setwd('/scratch/js4yd/GI_RandomSeedEval')
options(scipen = 999)
write.table(HillStreamflow, file = paste0(getwd(), '/FlowGI_c_h.txt'), col.names = TRUE, row.names = FALSE, sep = '\t')
write.table(HillSatDef, file = paste0(getwd(), '/SatDefGI_c_h.txt'), col.names = TRUE, row.names = FALSE, sep = '\t')
write.table(HillDetStore, file = paste0(getwd(), '/DetStoreGI_c_h.txt'), col.names = TRUE, row.names = FALSE, sep = '\t')
write.table(HillET, file = paste0(getwd(), '/ETGI_c_h.txt'), col.names = TRUE, row.names = FALSE, sep = '\t')
options(scipen = 0)