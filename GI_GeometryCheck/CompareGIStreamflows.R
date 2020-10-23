#Script to compile streamflows into a matrix and plot
#Arguments:
#1: patch resolution
#2: main working directory
#3: project directory name
#4: Number of replicates
#5: Starting date
#6: Replicate starting number
#7: File end name
arg = commandArgs(T)

startNum = as.numeric(arg[6])
reps = as.numeric(arg[4])

library(vroom)

#Compute streamflow conversion factor----
world = read.csv(paste0(arg[2], arg[3], '/worldfiles/worldfile.csv'), stringsAsFactors = FALSE)
res = as.numeric(arg[1])
#Taking the unique patch IDs because strata can exist in more than one patch.
Area.basin = length(unique(world$patchID))*res^2
rm(world, res)
#Multiplier conversion for basin streamflow (mm/d)*conversion_b -> cfs
conversion_b = Area.basin/1000/(.3048^3)/24/3600

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
Qmat = SatDefMat = DetStoreMat = ETMat = matrix(NA, nrow = nrow(Q1), ncol = (reps+1))

for (i in 1:reps){
  Ind = seq(startNum, startNum+reps-1, 1)
  setwd(paste0(arg[2], 'RHESSysRuns/Run', Ind[i], '/', arg[3], '/output'))
  #Read in simulated basin streamflow
  Q = vroom(paste0(getwd(), '/Run', Ind[i], '_basin.daily'), delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
  
  #Make a new Date column
  Q$Date = as.Date(paste0(Q$year, '-', Q$month, '-', Q$day))
  
  #Trim off spin-up years
  Q = Q[which(as.Date(Q$Date) >= as.Date(arg[5])),]
  
  #Convert simulated streamflow to cfs units
  Q$streamflow = round(Q$streamflow*conversion_b, 6)
  
  #Add to Qmat
  Qmat[,i+1] = Q$streamflow
  SatDefMat[,i+1] = Q$sat_def
  DetStoreMat[,i+1] = Q$detention_store
  ETMat[,i+1] = Q$trans + Q$evap
  
  #Retain only streamflow and Date columns for space
  Q = as.data.frame(Q[,c('Date','streamflow')])
  
  #Write streamflow to file
  options(scipen = 999)
  write.table(Q, file = paste0(getwd(), '/Q.txt'), col.names = TRUE, row.names = FALSE, sep = '\t')
  options(scipen = 0)
}

#Convert to dataframe
Qmat = as.data.frame(Qmat)
SatDefMat = as.data.frame(SatDefMat)
DetStoreMat = as.data.frame(DetStoreMat)
ETMat = as.data.frame(ETMat)
colnames(Qmat) = colnames(SatDefMat) = colnames(DetStoreMat) = colnames(ETMat) = c('Date', paste0('Run', seq(1,reps,1)))

#Add dates
Qmat$Date = SatDefMat$Date = DetStoreMat$Date = ETMat$Date = as.Date(Q1$Date)

#Save files
setwd(arg[2])
options(scipen = 999)
write.table(Qmat, paste0('FlowGI', arg[7], '_c.txt'), col.names = TRUE, row.names = FALSE, sep = '\t')
write.table(SatDefMat, paste0('SatDefGI', arg[7], '_c.txt'), col.names = TRUE, row.names = FALSE, sep = '\t')
write.table(DetStoreMat, paste0('DetStoreGI', arg[7], '_c.txt'), col.names = TRUE, row.names = FALSE, sep = '\t')
write.table(ETMat, paste0('ETGI', arg[7], '_c.txt'), col.names = TRUE, row.names = FALSE, sep = '\t')
options(scipen = 0)