#Script to compile streamflows into a matrix and plot

library(vroom)

#Compute streamflow conversion factor----
world = read.csv('/scratch/js4yd/GI_RandomSeedEval/RHESSys_Baisman30m_g74/worldfiles/worldfile.csv', stringsAsFactors = FALSE)
res = 30
#Taking the unique patch IDs because strata can exist in more than one patch.
Area.basin = length(unique(world$patchID))*res^2
rm(world, res)
#Multiplier conversion for basin streamflow (mm/d)*conversion_b -> cfs
conversion_b = Area.basin/1000/(.3048^3)/24/3600

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
Qmat = SatDefMat = DetStoreMat = ETMat = matrix(NA, nrow = nrow(Q1), ncol = 101)
for (i in 1:100){
  setwd(paste0('/scratch/js4yd/GI_RandomSeedEval/RHESSysRuns/Run', i, '/RHESSys_Baisman30m_g74/output'))
  #Read in simulated basin streamflow
  Q = vroom(paste0(getwd(), '/Run', i, '_basin.daily'), delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
  
  #Make a new Date column
  Q$Date = as.Date(paste0(Q$year, '-', Q$month, '-', Q$day))
  
  #Trim off spin-up years
  Q = Q[which(as.Date(Q$Date) >= as.Date('2004-10-01')),]
  
  #Convert simulated streamflow to cfs units
  Q$streamflow = round(Q$streamflow*conversion_b, 6)
  
  #Add to Qmat
  Qmat[,i+1] = Q$streamflow
  SatDefMat[,i+1] = Q$sat_def
  DetStoreMat[,i+1] = Q$detention_store
  ETMat[,i+1] = Q$trans + Q$evap
  
  #Retain only streamflow and Date columns for space
  Q = as.data.frame(Q[,c('Date','streamflow')])
  
  #Write streamflow to file for loading in Python likelihood function
  options(scipen = 999)
  write.table(Q, file = paste0(getwd(), '/Q.txt'), col.names = TRUE, row.names = FALSE, sep = '\t')
  options(scipen = 0)
}

#Convert to dataframe
Qmat = as.data.frame(Qmat)
SatDefMat = as.data.frame(SatDefMat)
DetStoreMat = as.data.frame(DetStoreMat)
ETMat = as.data.frame(ETMat)
colnames(Qmat) = colnames(SatDefMat) = colnames(DetStoreMat) = colnames(ETMat) = c('Date', paste0('Run', seq(1,100,1)))

#Add dates
Qmat$Date = SatDefMat$Date = DetStoreMat$Date = ETMat$Date = as.Date(Q1$Date)

#Save files
setwd('/scratch/js4yd/GI_RandomSeedEval')
options(scipen = 999)
write.table(Qmat, 'FlowGI_c.txt', col.names = TRUE, row.names = FALSE, sep = '\t')
write.table(SatDefMat, 'SatDefGI_c.txt', col.names = TRUE, row.names = FALSE, sep = '\t')
write.table(DetStoreMat, 'DetStoreGI_c.txt', col.names = TRUE, row.names = FALSE, sep = '\t')
write.table(ETMat, 'ETGI_c.txt', col.names = TRUE, row.names = FALSE, sep = '\t')
options(scipen = 0)