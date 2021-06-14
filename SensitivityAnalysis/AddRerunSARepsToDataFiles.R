#Script to add the 5 rerun SA replicates to the existing basin and hillslope streamflows and TN datasets

library(vroom)
library(EGRET)
library(survival)
library(pracma)


#Load in the modified WRTDS code----
setwd('C:\\Users\\js4yd\\OneDrive - University of Virginia\\RHESSys_ParameterSA')
source('WRTDS_modifiedFunctions.R')

#Load Tables----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\WRTDS")
TabInt = as.matrix(read.table(file = 'TabIntMod4_p5.txt', sep = '\t', header = TRUE, check.names = FALSE))
TabYear = as.matrix(read.table(file = 'TabYearMod4_p4.txt', sep = '\t', header = TRUE, check.names = FALSE))
TabLogQ = as.matrix(read.table(file = 'TabLogQMod4_p4.txt', sep = '\t', header = TRUE, check.names = FALSE))
TabSinYear = as.matrix(read.table(file = 'TabSinYearMod4_p4.txt', sep = '\t', header = TRUE, check.names = FALSE))
TabCosYear = as.matrix(read.table(file = 'TabCosYearMod4_p4.txt', sep = '\t', header = TRUE, check.names = FALSE))
TabLogErr = as.matrix(read.table(file = 'TabLogErrMod4_p5.txt', sep = '\t', header = TRUE, check.names = FALSE))

numReps = 10880

#Process area of basin and hillslope information from the worldfile----
res = 30

world = read.csv('C:/Users/js4yd/Documents/BaismanSA/RHESSysRuns/Run0/worldfiles/worldfile.csv', stringsAsFactors = FALSE)

#Taking the unique patch IDs because strata can exist in more than one patch.
Area.basin = length(unique(world$patchID))*res^2
#Multiplier conversion for basin streamflow (mm/d)*conversion_b -> cfs
conversion_b = Area.basin/1000/(.3048^3)/24/3600

#Get hillslope areas and conversion factor for streamflow in hillslopes
uhills = unique(world$hillID)
Area.Hills = matrix(NA, nrow = length(uhills), ncol = 2)
conversion_h = matrix(NA, nrow = length(uhills), ncol = 2)
for (h in 1:length(uhills)){
  Area.Hills[h,1] = h
  conversion_h[h,1] = h
  #some patches have multiple strata, so their area cannot be counted from the count of cells.
  Area.Hills[h,2] = length(which(world[which(duplicated(world$patchID) == FALSE),]$hillID == h))*res^2
  conversion_h[h,2] = Area.Hills[h,2]/1000/(.3048^3)/24/3600
}
rm(h)

#Load the new SA replicate streamflow data----
setwd('C:\\Users\\js4yd\\Documents\\BaismanSA\\RHESSysRuns')
#644
bs644 = vroom(paste0(getwd(), '/Run644/RHESSys_Baisman30m_g74/output/BaismanRun644_basin.daily'), delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
hs644 = vroom(paste0(getwd(), '/Run644/RHESSys_Baisman30m_g74/output/BaismanRun644_hillslope.daily'), delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
#Make a new date column
bs644$Date = as.Date(paste0(bs644$year, '-', bs644$month, '-', bs644$day))
hs644$Date = as.Date(paste0(hs644$year, '-', hs644$month, '-', hs644$day))
#5837
bs5837 = vroom(paste0(getwd(), '/Run5837/RHESSys_Baisman30m_g74/output/BaismanRun5837_basin.daily'), delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
hs5837 = vroom(paste0(getwd(), '/Run5837/RHESSys_Baisman30m_g74/output/BaismanRun5837_hillslope.daily'), delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
#Make a new date column
bs5837$Date = as.Date(paste0(bs5837$year, '-', bs5837$month, '-', bs5837$day))
hs5837$Date = as.Date(paste0(hs5837$year, '-', hs5837$month, '-', hs5837$day))
#5388
bs5388 = vroom(paste0(getwd(), '/Run5388/RHESSys_Baisman30m_g74/output/BaismanRun5388_basin.daily'), delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
hs5388 = vroom(paste0(getwd(), '/Run5388/RHESSys_Baisman30m_g74/output/BaismanRun5388_hillslope.daily'), delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
#Make a new date column
bs5388$Date = as.Date(paste0(bs5388$year, '-', bs5388$month, '-', bs5388$day))
hs5388$Date = as.Date(paste0(hs5388$year, '-', hs5388$month, '-', hs5388$day))
#3365
bs3365 = vroom(paste0(getwd(), '/Run3365/RHESSys_Baisman30m_g74/output/BaismanRun3365_basin.daily'), delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
hs3365 = vroom(paste0(getwd(), '/Run3365/RHESSys_Baisman30m_g74/output/BaismanRun3365_hillslope.daily'), delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
#Make a new date column
bs3365$Date = as.Date(paste0(bs3365$year, '-', bs3365$month, '-', bs3365$day))
hs3365$Date = as.Date(paste0(hs3365$year, '-', hs3365$month, '-', hs3365$day))
#4913
bs4913 = vroom(paste0(getwd(), '/Run4913/RHESSys_Baisman30m_g74/output/BaismanRun4913_basin.daily'), delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
hs4913 = vroom(paste0(getwd(), '/Run4913/RHESSys_Baisman30m_g74/output/BaismanRun4913_hillslope.daily'), delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
#Make a new date column
bs4913$Date = as.Date(paste0(bs4913$year, '-', bs4913$month, '-', bs4913$day))
hs4913$Date = as.Date(paste0(hs4913$year, '-', hs4913$month, '-', hs4913$day))

#Load basin and hillslope streamflow files in their correct order----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR")
BasinSF = read.table(file = 'SAResults_BasinStreamflow_p4_Reordered.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillSF = read.table(file = 'SAResults_HillStreamflow_p6_Reordered.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)

#Add the re-run data in the correct rows of these files----
#These rows should be changed: c(645,5838,4914,3366,5389)
BasinSF[645,] = c(645, round(bs644$streamflow*conversion_b, 4))
BasinSF[5838,] = c(5838, round(bs5837$streamflow*conversion_b, 4))
BasinSF[4914,] = c(4914, round(bs4913$streamflow*conversion_b, 4))
BasinSF[3366,] = c(3366, round(bs3365$streamflow*conversion_b, 4))
BasinSF[5389,] = c(5389, round(bs5388$streamflow*conversion_b, 4))

for (h in 1:length(uhills)){
  HillSF[645 + numReps*(uhills[h]-1),] = c(645, uhills[h], round(hs644$streamflow[hs644$hillID == uhills[h]]*conversion_h[conversion_h[,1] == uhills[h], 2], 6))
  HillSF[5838 + numReps*(uhills[h]-1),] = c(5838, uhills[h], round(hs5837$streamflow[hs5837$hillID == uhills[h]]*conversion_h[conversion_h[,1] == uhills[h], 2], 6))
  HillSF[4914 + numReps*(uhills[h]-1),] = c(4914, uhills[h], round(hs4913$streamflow[hs4913$hillID == uhills[h]]*conversion_h[conversion_h[,1] == uhills[h], 2], 6))
  HillSF[3366 + numReps*(uhills[h]-1),] = c(3366, uhills[h], round(hs3365$streamflow[hs3365$hillID == uhills[h]]*conversion_h[conversion_h[,1] == uhills[h], 2], 6))
  HillSF[5389 + numReps*(uhills[h]-1),] = c(5389, uhills[h], round(hs5388$streamflow[hs5388$hillID == uhills[h]]*conversion_h[conversion_h[,1] == uhills[h], 2], 6))
}

#Save new dataset----
#write.csv(BasinSF, file = 'SAResults_BasinStreamflow_p4_Reordered_Add5.csv', row.names = FALSE)
#write.csv(HillSF, file = 'SAResults_HillStreamflow_p6_Reordered_Add5.csv', row.names = FALSE)
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR")
write.table(BasinSF, file = 'SAResults_BasinStreamflow_p4_Reordered_Add5.txt', row.names = FALSE, sep = '\t')
write.table(HillSF, file = 'SAResults_HillStreamflow_p6_Reordered_Add5.txt', row.names = FALSE, sep = '\t')

#Load the saved dataset----
BasinSF = read.table(file = 'SAResults_BasinStreamflow_p4_Reordered_Add5.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillSF = read.table(file = 'SAResults_HillStreamflow_p6_Reordered_Add5.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)

#Make transposed streamflow data for TN processing----
write.table(t(BasinSF), file = 'SAResults_BasinStreamflow_p4_t_Reordered_Add5.txt', sep = '\t', row.names = colnames(BasinSF))
write.table(t(HillSF), file = 'SAResults_HillStreamflow_p6_t_Reordered_Add5.txt', sep = '\t', row.names = colnames(HillSF))

#Load in the transposed streamflow data with rows as the dates
BasinSF_t = read.table(file = 'SAResults_BasinStreamflow_p4_t_Reordered_Add5.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE, nrows = 1, skip=d+1, row.names = 1)
HillSF_t = read.table(file = 'SAResults_HillStreamflow_p4_t_Reordered_Add5.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE, nrows = 1, skip=d+2, row.names = 1)

#Compute TN for these 5 rerun replicates----
a644 = a3365 = a4913 = a5388 = a5837 = matrix(NA, ncol = 3, nrow = ncol(BasinSF)-1)
h644 = h3365 = h4913 = h5388 = h5837 = matrix(NA, ncol = 4, nrow = (ncol(BasinSF)-1)*length(uhills))
rowt = as.numeric(rownames(TabInt))
colt = as.numeric(colnames(TabInt))
dtest = colnames(BasinSF)[-1]
for (i in 1:nrow(bs3365)){
  a644[i,] = predictWRTDS(Flow = BasinSF[645,i+1], Date = dtest[i], rowt = rowt, colt = colt, TabInt = TabInt, TabYear = TabYear, TabLogQ = TabLogQ, TabSinYear = TabSinYear, TabCosYear = TabCosYear, TabLogErr = TabLogErr, TabLogQ2 = NULL)
  a5837[i,] = predictWRTDS(Flow = BasinSF[5838,i+1], Date = dtest[i], rowt = rowt, colt = colt, TabInt = TabInt, TabYear = TabYear, TabLogQ = TabLogQ, TabSinYear = TabSinYear, TabCosYear = TabCosYear, TabLogErr = TabLogErr, TabLogQ2 = NULL)
  a4913[i,] = predictWRTDS(Flow = BasinSF[4914,i+1], Date = dtest[i], rowt = rowt, colt = colt, TabInt = TabInt, TabYear = TabYear, TabLogQ = TabLogQ, TabSinYear = TabSinYear, TabCosYear = TabCosYear, TabLogErr = TabLogErr, TabLogQ2 = NULL)
  a3365[i,] = predictWRTDS(Flow = BasinSF[3366,i+1], Date = dtest[i], rowt = rowt, colt = colt, TabInt = TabInt, TabYear = TabYear, TabLogQ = TabLogQ, TabSinYear = TabSinYear, TabCosYear = TabCosYear, TabLogErr = TabLogErr, TabLogQ2 = NULL)
  a5388[i,] = predictWRTDS(Flow = BasinSF[5389,i+1], Date = dtest[i], rowt = rowt, colt = colt, TabInt = TabInt, TabYear = TabYear, TabLogQ = TabLogQ, TabSinYear = TabSinYear, TabCosYear = TabCosYear, TabLogErr = TabLogErr, TabLogQ2 = NULL)
  
  for (h in 1:length(uhills)){
    h644[i + (ncol(BasinSF)-1)*(uhills[h]-1),] = c(uhills[h], predictWRTDS(Flow = HillSF[645 + numReps*(uhills[h]-1),i+2], Date = dtest[i], rowt = rowt, colt = colt, TabInt = TabInt, TabYear = TabYear, TabLogQ = TabLogQ, TabSinYear = TabSinYear, TabCosYear = TabCosYear, TabLogErr = TabLogErr, TabLogQ2 = NULL))
    h5837[i + (ncol(BasinSF)-1)*(uhills[h]-1),] = c(uhills[h], predictWRTDS(Flow = HillSF[5838 + numReps*(uhills[h]-1),i+2], Date = dtest[i], rowt = rowt, colt = colt, TabInt = TabInt, TabYear = TabYear, TabLogQ = TabLogQ, TabSinYear = TabSinYear, TabCosYear = TabCosYear, TabLogErr = TabLogErr, TabLogQ2 = NULL))
    h4913[i + (ncol(BasinSF)-1)*(uhills[h]-1),] = c(uhills[h], predictWRTDS(Flow = HillSF[4914 + numReps*(uhills[h]-1),i+2], Date = dtest[i], rowt = rowt, colt = colt, TabInt = TabInt, TabYear = TabYear, TabLogQ = TabLogQ, TabSinYear = TabSinYear, TabCosYear = TabCosYear, TabLogErr = TabLogErr, TabLogQ2 = NULL))
    h3365[i + (ncol(BasinSF)-1)*(uhills[h]-1),] = c(uhills[h], predictWRTDS(Flow = HillSF[3366 + numReps*(uhills[h]-1),i+2], Date = dtest[i], rowt = rowt, colt = colt, TabInt = TabInt, TabYear = TabYear, TabLogQ = TabLogQ, TabSinYear = TabSinYear, TabCosYear = TabCosYear, TabLogErr = TabLogErr, TabLogQ2 = NULL))
    h5388[i + (ncol(BasinSF)-1)*(uhills[h]-1),] = c(uhills[h], predictWRTDS(Flow = HillSF[5389 + numReps*(uhills[h]-1),i+2], Date = dtest[i], rowt = rowt, colt = colt, TabInt = TabInt, TabYear = TabYear, TabLogQ = TabLogQ, TabSinYear = TabSinYear, TabCosYear = TabCosYear, TabLogErr = TabLogErr, TabLogQ2 = NULL))
  }
}
rm(rowt, colt, dtest)

#Load the basin and hillslope TN data in the correct order----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR")
BasinTN05 = read.table(file = 'SAResults_BasinTN05_p3_All_Reordered.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
BasinTNMed = read.table(file = 'SAResults_BasinTNMed_p3_All_Reordered.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
BasinTN95 = read.table(file = 'SAResults_BasinTN95_p3_All_Reordered.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillTN05 = read.table(file = 'SAResults_HillTN05_p3_All_Reordered.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillTNMed = read.table(file = 'SAResults_HillTNMed_p3_All_Reordered.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillTN95 = read.table(file = 'SAResults_HillTN95_p3_All_Reordered.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)

#Add the re-run data to the correct rows of these files----
#These rows should be changed: c(645,5838,4914,3366,5389)
BasinTN05[645,] = c(645, round(a644[,1], 3))
BasinTN05[5838,] = c(5838, round(a5837[,1], 3))
BasinTN05[4914,] = c(4914, round(a4913[,1], 3))
BasinTN05[3366,] = c(3366, round(a3365[,1], 3))
BasinTN05[5389,] = c(5389, round(a5388[,1], 3))
BasinTNMed[645,] = c(645, round(a644[,2], 3))
BasinTNMed[5838,] = c(5838, round(a5837[,2], 3))
BasinTNMed[4914,] = c(4914, round(a4913[,2], 3))
BasinTNMed[3366,] = c(3366, round(a3365[,2], 3))
BasinTNMed[5389,] = c(5389, round(a5388[,2], 3))
BasinTN95[645,] = c(645, round(a644[,3], 3))
BasinTN95[5838,] = c(5838, round(a5837[,3], 3))
BasinTN95[4914,] = c(4914, round(a4913[,3], 3))
BasinTN95[3366,] = c(3366, round(a3365[,3], 3))
BasinTN95[5389,] = c(5389, round(a5388[,3], 3))

for (h in 1:length(uhills)){
  HillTN05[645 + numReps*(uhills[h]-1),] = c(645, uhills[h], round(h644[which(h644[,1] == uhills[h]),2], 3))
  HillTN05[5838 + numReps*(uhills[h]-1),] = c(5838, uhills[h], round(h5837[which(h5837[,1] == uhills[h]),2], 3))
  HillTN05[4914 + numReps*(uhills[h]-1),] = c(4914, uhills[h], round(h4913[which(h4913[,1] == uhills[h]),2], 3))
  HillTN05[3366 + numReps*(uhills[h]-1),] = c(3366, uhills[h], round(h3365[which(h3365[,1] == uhills[h]),2], 3))
  HillTN05[5389 + numReps*(uhills[h]-1),] = c(5389, uhills[h], round(h5388[which(h5388[,1] == uhills[h]),2], 3))
  HillTNMed[645 + numReps*(uhills[h]-1),] = c(645, uhills[h], round(h644[which(h644[,1] == uhills[h]),3], 3))
  HillTNMed[5838 + numReps*(uhills[h]-1),] = c(5838, uhills[h], round(h5837[which(h5837[,1] == uhills[h]),3], 3))
  HillTNMed[4914 + numReps*(uhills[h]-1),] = c(4914, uhills[h], round(h4913[which(h4913[,1] == uhills[h]),3], 3))
  HillTNMed[3366 + numReps*(uhills[h]-1),] = c(3366, uhills[h], round(h3365[which(h3365[,1] == uhills[h]),3], 3))
  HillTNMed[5389 + numReps*(uhills[h]-1),] = c(5389, uhills[h], round(h5388[which(h5388[,1] == uhills[h]),3], 3))
  HillTN95[645 + numReps*(uhills[h]-1),] = c(645, uhills[h], round(h644[which(h644[,1] == uhills[h]),4], 3))
  HillTN95[5838 + numReps*(uhills[h]-1),] = c(5838, uhills[h], round(h5837[which(h5837[,1] == uhills[h]),4], 3))
  HillTN95[4914 + numReps*(uhills[h]-1),] = c(4914, uhills[h], round(h4913[which(h4913[,1] == uhills[h]),4], 3))
  HillTN95[3366 + numReps*(uhills[h]-1),] = c(3366, uhills[h], round(h3365[which(h3365[,1] == uhills[h]),4], 3))
  HillTN95[5389 + numReps*(uhills[h]-1),] = c(5389, uhills[h], round(h5388[which(h5388[,1] == uhills[h]),4], 3))
}

#Save new dataset----
# write.csv(BasinTN05, file = 'SAResults_BasinTN05_p3_All_Reordered_Add5.csv', row.names = FALSE)
# write.csv(BasinTNMed, file = 'SAResults_BasinTNMed_p3_All_Reordered_Add5.csv', row.names = FALSE)
# write.csv(BasinTN95, file = 'SAResults_BasinTN95_p3_All_Reordered_Add5.csv', row.names = FALSE)
# write.csv(HillTN05, file = 'SAResults_HillTN05_p3_All_Reordered_Add5.csv', row.names = FALSE)
# write.csv(HillTNMed, file = 'SAResults_HillTNMed_p3_All_Reordered_Add5.csv', row.names = FALSE)
# write.csv(HillTN95, file = 'SAResults_HillTN95_p3_All_Reordered_Add5.csv', row.names = FALSE)
"C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR"
write.table(BasinTN05, file = 'SAResults_BasinTN05_p3_All_Reordered_Add5.txt', row.names = FALSE, sep = '\t')
write.table(BasinTNMed, file = 'SAResults_BasinTNMed_p3_All_Reordered_Add5.txt', row.names = FALSE, sep = '\t')
write.table(BasinTN95, file = 'SAResults_BasinTN95_p3_All_Reordered_Add5.txt', row.names = FALSE, sep = '\t')
write.table(HillTN05, file = 'SAResults_HillTN05_p3_All_Reordered_Add5.txt', row.names = FALSE, sep = '\t')
write.table(HillTNMed, file = 'SAResults_HillTNMed_p3_All_Reordered_Add5.txt', row.names = FALSE, sep = '\t')
write.table(HillTN95, file = 'SAResults_HillTN95_p3_All_Reordered_Add5.txt', row.names = FALSE, sep = '\t')

#Load the saved datasets----
BasinTN05 = read.table(file = 'SAResults_BasinTN05_p3_All_Reordered_Add5.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
BasinTNMed = read.table(file = 'SAResults_BasinTNMed_p3_All_Reordered_Add5.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
BasinTN95 = read.table(file = 'SAResults_BasinTN95_p3_All_Reordered_Add5.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillTN05 = read.table(file = 'SAResults_HillTN05_p3_All_Reordered_Add5.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillTNMed = read.table(file = 'SAResults_HillTNMed_p3_All_Reordered_Add5.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillTN95 = read.table(file = 'SAResults_HillTN95_p3_All_Reordered_Add5.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)

#Load in the existing EE data files----
EEs05_b = read.table(file = paste0(getwd(), '/EEs05_b_All.txt'), sep = '\t', stringsAsFactors = FALSE, header = TRUE)
EEs95_b = read.table(file = paste0(getwd(), '/EEs95_b_All.txt'), sep = '\t', stringsAsFactors = FALSE, header = TRUE) 
EEsot_b = read.table(file = paste0(getwd(), '/EEsot_b_All.txt'), sep = '\t', stringsAsFactors = FALSE, header = TRUE)
EEsTN05_b = read.table(file = paste0(getwd(), '/EEsTN05_b_All.txt'), sep = '\t', stringsAsFactors = FALSE, header = TRUE) 
EEsTNMed_b = read.table(file = paste0(getwd(), '/EEsTNMed_b_All.txt'), sep = '\t', stringsAsFactors = FALSE, header = TRUE) 
EEsTN95_b = read.table(file = paste0(getwd(), '/EEsTN95_b_All.txt'), sep = '\t', stringsAsFactors = FALSE, header = TRUE)
EEs05_h = read.table(file = paste0(getwd(), '/EEs05_h_All.txt'), sep = '\t', stringsAsFactors = FALSE, header = TRUE)
EEs95_h = read.table(file = paste0(getwd(), '/EEs95_h_All.txt'), sep = '\t', stringsAsFactors = FALSE, header = TRUE)
EEsot_h = read.table(file = paste0(getwd(), '/EEsot_h_All.txt'), sep = '\t', stringsAsFactors = FALSE, header = TRUE)
EEsTN05_h = read.table(file = paste0(getwd(), '/EEsTN05_h_All.txt'), sep = '\t', stringsAsFactors = FALSE, header = TRUE) 
EEsTNMed_h = read.table(file = paste0(getwd(), '/EEsTNMed_h_All.txt'), sep = '\t', stringsAsFactors = FALSE, header = TRUE) 
EEsTN95_h = read.table(file = paste0(getwd(), '/EEsTN95_h_All.txt'), sep = '\t', stringsAsFactors = FALSE, header = TRUE)
Deltas = read.table(file = paste0(getwd(), '/Deltas_All.txt'), sep = '\t', stringsAsFactors = FALSE, header = TRUE)

#Load Morris parameter files and ranges----
InputParams = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs\\MorrisSampleLocs\\MorrisSamples_AfterProcessing.csv", stringsAsFactors = FALSE)
InputParams_Add5 = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs\\MorrisSampleLocs\\MorrisSamples_AfterProcessing_EditToRerun5SA_comp.csv", stringsAsFactors = FALSE)
#Also need the original, unmodified Morris file to know which parameter changed in each trajectory iteration
OrigParams = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs\\MorrisSampleLocs\\MorrisSamples_BeforeProcessing.csv", stringsAsFactors = FALSE)
#And need the parameter ranges to scale the deltas
ParamRanges = read.csv('C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs\\MorrisSampleLocs\\BaismanMorrisSamplingProblemFile_Full.csv', stringsAsFactors = FALSE)

#Remove all of the parameters with _orig. They were not modified
InputParams = InputParams[-grep(x = colnames(InputParams), pattern = '_orig', fixed = TRUE)]
InputParams_Add5 = InputParams_Add5[-grep(x = colnames(InputParams_Add5), pattern = '_orig', fixed = TRUE)]
#Get the number of parameters
cols = ncol(InputParams_Add5)

#Specify number of trajectories----
r = 40

# Remove observations earlier than 10/01/2004 (SA timeperiod start)----
HillSF = HillSF[, c(1, 2, which(as.Date(colnames(HillSF[,-c(1,2)])) >= as.Date('2004-10-01'))+2)]
HillTN05 = HillTN05[, c(1, 2, which(as.Date(colnames(HillTN05[,-c(1,2)])) >= as.Date('2004-10-01'))+2)]
HillTNMed = HillTNMed[, c(1, 2, which(as.Date(colnames(HillTNMed[,-c(1,2)])) >= as.Date('2004-10-01'))+2)]
HillTN95 = HillTN95[, c(1, 2, which(as.Date(colnames(HillTN95[,-c(1,2)])) >= as.Date('2004-10-01'))+2)]

BasinSF = BasinSF[, c(1, which(as.Date(colnames(BasinSF[,-1])) >= as.Date('2004-10-01'))+1)]
BasinTN05 = BasinTN05[, c(1, which(as.Date(colnames(BasinTN05[,-1])) >= as.Date('2004-10-01'))+1)]
BasinTNMed = BasinTNMed[, c(1, which(as.Date(colnames(BasinTNMed[,-1])) >= as.Date('2004-10-01'))+1)]
BasinTN95 = BasinTN95[, c(1, which(as.Date(colnames(BasinTN95[,-1])) >= as.Date('2004-10-01'))+1)]

# Order by replicate ID. This is done to ensure that the trajectories are in order. They should already be in order, though.----
BasinSF = BasinSF[order(BasinSF$Replicate),]
BasinTN05 = BasinTN05[order(BasinTN05$Replicate),]
BasinTNMed = BasinTNMed[order(BasinTNMed$Replicate),]
BasinTN95 = BasinTN95[order(BasinTN95$Replicate),]

#For hillslopes, need to reorder within the hillslope IDs
for (h in 1:length(unique(HillSF$HillID))){
  HillSF[(1+nrow(BasinSF)*(h-1)):(1+nrow(BasinSF)*(h-1)+(nrow(BasinSF)-1)),] = HillSF[(1+nrow(BasinSF)*(h-1)):(1+nrow(BasinSF)*(h-1)+(nrow(BasinSF)-1)),][order(HillSF[(1+nrow(BasinSF)*(h-1)):(1+nrow(BasinSF)*(h-1)+(nrow(BasinSF)-1)),]$Replicate),]
  HillTN05[(1+nrow(BasinSF)*(h-1)):(1+nrow(BasinSF)*(h-1)+(nrow(BasinSF)-1)),] = HillTN05[(1+nrow(BasinSF)*(h-1)):(1+nrow(BasinSF)*(h-1)+(nrow(BasinSF)-1)),][order(HillTN05[(1+nrow(BasinSF)*(h-1)):(1+nrow(BasinSF)*(h-1)+(nrow(BasinSF)-1)),]$Replicate),]
  HillTNMed[(1+nrow(BasinSF)*(h-1)):(1+nrow(BasinSF)*(h-1)+(nrow(BasinSF)-1)),] = HillTNMed[(1+nrow(BasinSF)*(h-1)):(1+nrow(BasinSF)*(h-1)+(nrow(BasinSF)-1)),][order(HillTNMed[(1+nrow(BasinSF)*(h-1)):(1+nrow(BasinSF)*(h-1)+(nrow(BasinSF)-1)),]$Replicate),]
  HillTN95[(1+nrow(BasinSF)*(h-1)):(1+nrow(BasinSF)*(h-1)+(nrow(BasinSF)-1)),] = HillTN95[(1+nrow(BasinSF)*(h-1)):(1+nrow(BasinSF)*(h-1)+(nrow(BasinSF)-1)),][order(HillTN95[(1+nrow(BasinSF)*(h-1)):(1+nrow(BasinSF)*(h-1)+(nrow(BasinSF)-1)),]$Replicate),]
}

#Load the observed streamflow record----
obs = read.table("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\obs\\BaismanStreamflow_Cal.txt", header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, sep = '\t')
#Remove observations later than 9/30/2010 (SA timeperiod end)
obs = obs[as.Date(obs$Date) <= as.Date(colnames(BasinSF)[ncol(BasinSF)]),]
#Remove observations earlier than 10/01/2004 (SA timeperiod start)
obs = obs[as.Date(obs$Date) >= as.Date('2004-10-01'),]

# Find the days with the highest 5th percentile, lowest 5th percentile and all other flows----
q05 = quantile(x = obs$Flow, probs = 0.05)
q95 = quantile(x = obs$Flow, probs = 0.95)

obs05 = obs[obs$Flow <= q05,]
obs95 = obs[obs$Flow >= q95,]
days05 = as.Date(obs05$Date)
days95 = as.Date(obs95$Date)
obsot = obs[-which(as.Date(obs$Date) %in% c(days05, days95)),]
daysot = as.Date(obsot$Date)

#  Coverage is not great for the low flow metric. Use instead the lowest 5th percentile in each year of data----
#Find the indices at which there is a change in the water year, as defined by Oct. 1
IndsNewYear = c(colnames(BasinSF)[grep(colnames(BasinSF),pattern = '-10-01', fixed=TRUE)], colnames(BasinSF)[ncol(BasinSF)])
obs05 = NULL
for (iy in 1:(length(IndsNewYear)-1)){
  IndW = which((as.Date(obs$Date) >= as.Date(IndsNewYear[iy])) & (as.Date(obs$Date) < as.Date(IndsNewYear[iy+1])))
  q05 = quantile(x = obs$Flow[IndW], probs = 0.05)
  obs05 = rbind(obs05, obs[IndW,][obs$Flow[IndW] <= q05,])
}
rm(iy, q05, IndW)
days05 = as.Date(obs05$Date)
days95 = as.Date(obs95$Date)
obsot = obs[-which(as.Date(obs$Date) %in% c(days05, days95)),]
daysot = as.Date(obsot$Date)

#  Coverage for middle flows is slighly biased to earlier years because of upper flow metric. Test yearly 5ths for both upper and lower----
#Find the indices at which there is a change in the water year, as defined by Oct. 1
obs05 = obs95 = NULL
for (iy in 1:(length(IndsNewYear)-1)){
  IndW = which((as.Date(obs$Date) >= as.Date(IndsNewYear[iy])) & (as.Date(obs$Date) < as.Date(IndsNewYear[iy+1])))
  q05 = quantile(x = obs$Flow[IndW], probs = 0.05)
  q95 = quantile(x = obs$Flow[IndW], probs = 0.95)
  
  obs05 = rbind(obs05, obs[IndW,][obs$Flow[IndW] <= q05,])
  obs95 = rbind(obs95, obs[IndW,][obs$Flow[IndW] >= q95,])
}
rm(iy, q05, q95, IndW)
days05 = as.Date(obs05$Date)
days95 = as.Date(obs95$Date)
obsot = obs[-which(as.Date(obs$Date) %in% c(days05, days95)),]
daysot = as.Date(obsot$Date)

# Selected metric: lower 5th yearly, upper 5th global, and all else----
q95 = quantile(x = obs$Flow, probs = 0.95)
obs95 = obs[obs$Flow >= q95,]
days95 = as.Date(obs95$Date)
obs05 = NULL
for (iy in 1:(length(IndsNewYear)-1)){
  IndW = which((as.Date(obs$Date) >= as.Date(IndsNewYear[iy])) & (as.Date(obs$Date) < as.Date(IndsNewYear[iy+1])))
  q05 = quantile(x = obs$Flow[IndW], probs = 0.05)
  obs05 = rbind(obs05, obs[IndW,][obs$Flow[IndW] <= q05,])
}
rm(iy, q05, IndW, q95)
days05 = as.Date(obs05$Date)
obsot = obs[-which(as.Date(obs$Date) %in% c(days05, days95)),]
daysot = as.Date(obsot$Date)

#Create the 05, 95, and other datasets for streamflow----
HillSF05 = HillSF[,-c(1,2)][,which(as.Date(colnames(HillSF[1,-c(1,2)])) %in% days05)]
HillSF95 = HillSF[,-c(1,2)][,which(as.Date(colnames(HillSF[1,-c(1,2)])) %in% days95)]
HillSFot = HillSF[,-c(1,2)][,which(as.Date(colnames(HillSF[1,-c(1,2)])) %in% daysot)]

BasinSF05 = BasinSF[,-1][,which(as.Date(colnames(BasinSF[1,-1])) %in% days05)]
BasinSF95 = BasinSF[,-1][,which(as.Date(colnames(BasinSF[1,-1])) %in% days95)]
BasinSFot = BasinSF[,-1][,which(as.Date(colnames(BasinSF[1,-1])) %in% daysot)]

#Create the median hillslope streamflows that will be used as a reference point to compute Morris EEs----
uhills = sort(unique(HillSF$HillID))
MedHills = matrix(NA, nrow = length(uhills), ncol = ncol(BasinSF))
for (h in 1:length(uhills)){
  MedHills[h,] = c(uhills[h], apply(X = HillSF[which(HillSF$HillID == uhills[h]),-c(1,2)], MARGIN = 2, FUN = median))
}
rm(h)

colnames(MedHills) = colnames(BasinSF)

#Get the MedHills as 05, 95, and ot days
MedHills05 = MedHills[,c(1, which(as.Date(colnames(MedHills)[-1]) %in% as.Date(days05)))]
MedHills95 = MedHills[,c(1, which(as.Date(colnames(MedHills)[-1]) %in% as.Date(days95)))]
MedHillsot = MedHills[,c(1, which(as.Date(colnames(MedHills)[-1]) %in% as.Date(daysot)))]
rm(MedHills)

#Load the observed TN record----
obsTN = read.table("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\obs\\TN_Cal.txt", header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, sep = '\t')
#Remove observations later than 9/30/2010 (SA timeperiod end)
obsTN = obsTN[as.Date(obsTN$Date) <= as.Date(colnames(BasinSF)[ncol(BasinSF)]),]
#Remove observations earlier than 10/01/2004 (SA timeperiod start)
obsTN = obsTN[as.Date(obsTN$Date) >= as.Date('2004-10-01'),]
#Remove all NA days
obsTN = obsTN[!is.na(obsTN$TN),]

#Create the 05, 95, and log-mean datasets for TN----
#Select only the days that have observations
BasinTN05 = BasinTN05[,c(1,which(as.Date(colnames(BasinTN05[,-1])) %in% as.Date(obsTN$Date))+1)]
BasinTNMed = BasinTNMed[,c(1,which(as.Date(colnames(BasinTNMed[,-1])) %in% as.Date(obsTN$Date))+1)]
BasinTN95 = BasinTN95[,c(1,which(as.Date(colnames(BasinTN95[,-1])) %in% as.Date(obsTN$Date))+1)]

HillTN05 = HillTN05[,c(1,2,which(as.Date(colnames(HillTN05[,-c(1,2)])) %in% as.Date(obsTN$Date))+2)]
HillTNMed = HillTNMed[,c(1,2,which(as.Date(colnames(HillTNMed[,-c(1,2)])) %in% as.Date(obsTN$Date))+2)]
HillTN95 = HillTN95[,c(1,2,which(as.Date(colnames(HillTN95[,-c(1,2)])) %in% as.Date(obsTN$Date))+2)]

#Create the median hillslope TNs that will be used as a reference point to compute Morris EEs----
MedTN05Hills = MedTNMedHills = MedTN95Hills = matrix(NA, nrow = length(uhills), ncol = ncol(HillTN05)-1)
for (h in 1:length(uhills)){
  MedTN05Hills[h,] = c(uhills[h], apply(X = HillTN05[which(HillTN05$HillID == uhills[h]),-c(1,2)], MARGIN = 2, FUN = median))
  MedTNMedHills[h,] = c(uhills[h], apply(X = HillTNMed[which(HillTNMed$HillID == uhills[h]),-c(1,2)], MARGIN = 2, FUN = median))
  MedTN95Hills[h,] = c(uhills[h], apply(X = HillTN95[which(HillTN95$HillID == uhills[h]),-c(1,2)], MARGIN = 2, FUN = median))
}
rm(h)

#Compute the EEs for these replicates----
#Store the column names. These are the dates
colnms = colnames(BasinSF[,-1])

#Compute the EEs for all parameters in the trajectory
#Determine the base index. This is used for basin, and the first hillslope value
#Trajectory number
#644, 3365, 4913, 5388, 5837
t = c(3, 13, 19, 20, 22)
i = c(100, 101, 17, 220, 125)

for(A in 1:length(t)){
  ind = i[A]+(1+cols)*(t[A]-1)
  #Will want to make this equal to the known row numbers.
  
  #Find the parameter column that was changed, before any processing was completed. 
  #This is the same column that will be used for the Deltas and EEs
  parm = which((OrigParams[ind+1,] - OrigParams[ind,]) != 0)
  #Computes the exact delta from the modified sampling file. Allows for positive and negative deltas.
  delta = InputParams_Add5[ind+1,parm] - InputParams_Add5[ind,parm]
  #Adjust delta to the range sampled for the parameter
  delta = delta/abs(ParamRanges$Upper[parm] - ParamRanges$Lower[parm])
  Deltas[t[A], parm] = delta  
  
  #Sum absolute errors
  diff05 = (sum(abs((BasinSF05[ind+1,] - obs05$Flow))) - sum(abs((BasinSF05[ind,] - obs05$Flow))))  
  diff95 = (sum(abs((BasinSF95[ind+1,] - obs95$Flow))) - sum(abs((BasinSF95[ind,] - obs95$Flow))))
  diffot = (sum(abs((BasinSFot[ind+1,] - obsot$Flow))) - sum(abs((BasinSFot[ind,] - obsot$Flow))))
  
  #TN
  diffTN05 = (sum(abs((BasinTN05[ind+1,-1] - obsTN$TN))) - sum(abs((BasinTN05[ind,-1] - obsTN$TN))))  
  diffTNMed = (sum(abs((BasinTNMed[ind+1,-1] - obsTN$TN))) - sum(abs((BasinTNMed[ind,-1] - obsTN$TN))))
  diffTN95 = (sum(abs((BasinTN95[ind+1,-1] - obsTN$TN))) - sum(abs((BasinTN95[ind,-1] - obsTN$TN))))
  
  #Computes EEs with specified metric
  EEs05_b[t[A], parm] = diff05/delta
  EEs95_b[t[A], parm] = diff95/delta
  EEsot_b[t[A], parm] = diffot/delta
  
  EEsTN05_b[t[A], parm] = diffTN05/delta
  EEsTNMed_b[t[A], parm] = diffTNMed/delta
  EEsTN95_b[t[A], parm] = diffTN95/delta
  
  #Hillslopes
  #Compare to median streamflow and TN
  diff05_h = diff95_h = diffot_h = diffTN05_h = diffTNMed_h = diffTN95_h = NA
  for (hi in 1:length(uhills)){
    #Get the indices for this hillslope
    IndsHill = ind+(uhills[hi]-1)*nrow(BasinSF)
    #SF
    diff05_h = (sum(abs((HillSF05[IndsHill+1,] - MedHills05[which(MedHills05[,1] == uhills[hi]),-1]))) - sum(abs((HillSF05[IndsHill,] - MedHills05[which(MedHills05[,1] == uhills[hi]),-1]))))  
    diff95_h = (sum(abs((HillSF95[IndsHill+1,] - MedHills95[which(MedHills95[,1] == uhills[hi]),-1]))) - sum(abs((HillSF95[IndsHill,] - MedHills95[which(MedHills95[,1] == uhills[hi]),-1]))))
    diffot_h = (sum(abs((HillSFot[IndsHill+1,] - MedHillsot[which(MedHillsot[,1] == uhills[hi]),-1]))) - sum(abs((HillSFot[IndsHill,] - MedHillsot[which(MedHillsot[,1] == uhills[hi]),-1]))))
    
    #TN
    diffTN05_h = (sum(abs((HillTN05[IndsHill+1,-c(1,2)] - MedTN05Hills[which(MedTN05Hills[,1] == uhills[hi]),-1]))) - sum(abs((HillTN05[IndsHill,-c(1,2)] - MedTN05Hills[which(MedTN05Hills[,1] == uhills[hi]),-1]))))  
    diffTNMed_h = (sum(abs((HillTNMed[IndsHill+1,-c(1,2)] - MedTNMedHills[which(MedTNMedHills[,1] == uhills[hi]),-1]))) - sum(abs((HillTNMed[IndsHill,-c(1,2)] - MedTNMedHills[which(MedTNMedHills[,1] == uhills[hi]),-1]))))
    diffTN95_h = (sum(abs((HillTN95[IndsHill+1,-c(1,2)] - MedTN95Hills[which(MedTN95Hills[,1] == uhills[hi]),-1]))) - sum(abs((HillTN95[IndsHill,-c(1,2)] - MedTN95Hills[which(MedTN95Hills[,1] == uhills[hi]),-1]))))
    
    EEs05_h[(t[A]-1)*(length(uhills)) + uhills[hi], c(1,parm+1)] = c(uhills[hi], diff05_h/delta)
    EEs95_h[(t[A]-1)*(length(uhills)) + uhills[hi], c(1,parm+1)] = c(uhills[hi], diff95_h/delta)
    EEsot_h[(t[A]-1)*(length(uhills)) + uhills[hi], c(1,parm+1)] = c(uhills[hi], diffot_h/delta)
    
    EEsTN05_h[(t[A]-1)*(length(uhills)) + uhills[hi], c(1,parm+1)] = c(uhills[hi], diffTN05_h/delta)
    EEsTNMed_h[(t[A]-1)*(length(uhills)) + uhills[hi], c(1,parm+1)] = c(uhills[hi], diffTNMed_h/delta)
    EEsTN95_h[(t[A]-1)*(length(uhills)) + uhills[hi], c(1,parm+1)] = c(uhills[hi], diffTN95_h/delta)
  }
  rm(hi, parm, delta, diff05_h, diff05, diff95, diff95_h, diffot, diffot_h, diffTN05, diffTN05_h, diffTN95, diffTN95_h, diffTNMed, diffTNMed_h)
}
rm(A, i, t)

#Check if EEs have NA values
any(is.na(EEs05_b))
any(is.na(EEs95_b))
any(is.na(EEsot_b))
any(is.na(EEsTN05_b))
any(is.na(EEsTN95_b))
any(is.na(EEsTNMed_b))
any(is.na(EEs05_h))
any(is.na(EEs95_h))
any(is.na(EEsot_h))
any(is.na(EEsTN05_h))
any(is.na(EEsTN95_h))
any(is.na(EEsTNMed_h))

#Save EE datasets----
write.table(EEs05_b, file = paste0(getwd(), '/EEs05_b_All_Add5.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)
write.table(EEs95_b, file = paste0(getwd(), '/EEs95_b_All_Add5.txt'), sep = '\t', row.names = FALSE, col.names = TRUE) 
write.table(EEsot_b, file = paste0(getwd(), '/EEsot_b_All_Add5.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)
write.table(EEsTN05_b, file = paste0(getwd(), '/EEsTN05_b_All_Add5.txt'), sep = '\t', row.names = FALSE, col.names = TRUE) 
write.table(EEsTNMed_b, file = paste0(getwd(), '/EEsTNMed_b_All_Add5.txt'), sep = '\t', row.names = FALSE, col.names = TRUE) 
write.table(EEsTN95_b, file = paste0(getwd(), '/EEsTN95_b_All_Add5.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)
write.table(EEs05_h, file = paste0(getwd(), '/EEs05_h_All_Add5.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)
write.table(EEs95_h, file = paste0(getwd(), '/EEs95_h_All_Add5.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)
write.table(EEsot_h, file = paste0(getwd(), '/EEsot_h_All_Add5.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)
write.table(EEsTN05_h, file = paste0(getwd(), '/EEsTN05_h_All_Add5.txt'), sep = '\t', row.names = FALSE, col.names = TRUE) 
write.table(EEsTNMed_h, file = paste0(getwd(), '/EEsTNMed_h_All_Add5.txt'), sep = '\t', row.names = FALSE, col.names = TRUE) 
write.table(EEsTN95_h, file = paste0(getwd(), '/EEsTN95_h_All_Add5.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)
write.table(Deltas, file = paste0(getwd(), '/Deltas_All_Add5.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)

save.image("C:/Users/js4yd/OneDrive - University of Virginia/BES_Data/BES_Data/RHESSysFiles/BR&POBR/EEs_All_Add5.RData")
#Make transposed streamflow and TN datasets for likelihood analysis script----
BasinSF = read.table(file = 'SAResults_BasinStreamflow_p4_Reordered_Add5.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
BasinTNMed = read.table(file = 'SAResults_BasinTNMed_p3_All_Reordered_Add5.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)

#Columns are Date followed by Replicate#
tBasinSF = t(BasinSF)
tBasinSF = tBasinSF[-1,]
tBasinSF = cbind(rownames(tBasinSF), tBasinSF)
colnames(tBasinSF) = c('Date', paste0('Replicate', seq(1,nrow(BasinSF),1)))
write.table(tBasinSF, file = 'SAResults_BasinStreamflow_p4_Reordered_Add5_Likes.txt', row.names = FALSE, col.names = TRUE, sep = '\t')

#Columns are Date followed by Replicate#
tBasinTNMed = t(BasinTNMed)
tBasinTNMed = tBasinTNMed[-1,]
tBasinTNMed = cbind(rownames(tBasinTNMed), tBasinTNMed)
colnames(tBasinTNMed) = c('Date', paste0('Replicate', seq(1,nrow(BasinTNMed),1)))
write.table(tBasinTNMed, file = 'SAResults_BasinTNMed_p3_All_Reordered_Add5_Likes.txt', row.names = FALSE, col.names = TRUE, sep = '\t')
