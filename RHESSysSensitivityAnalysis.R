#Script for calculating Morris EEs from output streamflow and TN data

#######################################
#Fixme: update with the _Add5 files
#######################################

#Set directories----
#Color functions - from JDS github repo: Geothermal_ESDA
dir_ColFuns = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\Hydrology\\USGSGauges"

#Load libraries----
#This library may be useful for analysis, but is not used right now
#library(sensitivity)
library(vroom)

#Load functions----
source(paste0(dir_ColFuns, '\\ColorFunctions.R'))

#Load Morris parameter files and ranges----
InputParams = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs\\MorrisSampleLocs\\MorrisSamples_AfterProcessing_EditToRerun5SA_comp.csv", stringsAsFactors = FALSE)
#Also need the original, unmodified Morris file to know which parameter changed in each trajectory iteration
OrigParams = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs\\MorrisSampleLocs\\MorrisSamples_BeforeProcessing.csv", stringsAsFactors = FALSE)
#And need the parameter ranges to scale the deltas
ParamRanges = read.csv('C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs\\MorrisSampleLocs\\BaismanMorrisSamplingProblemFile_Full.csv', stringsAsFactors = FALSE)

#Remove all of the parameters with _orig. They were not modified
InputParams = InputParams[-grep(x = colnames(InputParams), pattern = '_orig', fixed = TRUE)]
#Get the number of parameters
cols = ncol(InputParams)

#Specify number of trajectories----
r = 40

#Load RHESSys streamflow data----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR")
BasinSF = vroom(file = 'SAResults_BasinStreamflow_p4_Reordered_Add5.txt', delim = '\t', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
HillSF = vroom(file = 'SAResults_HillStreamflow_p6_Reordered_Add5.txt', delim = '\t', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)

#Load WRTDS TN data----
BasinTN05 = vroom(file = 'SAResults_BasinTN05_p3_All_Reordered_Add5.txt', delim = '\t', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
BasinTNMed = vroom(file = 'SAResults_BasinTNMed_p3_All_Reordered_Add5.txt', delim = '\t', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
BasinTN95 = vroom(file = 'SAResults_BasinTN95_p3_All_Reordered_Add5.txt', delim = '\t', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
HillTN05 = vroom(file = 'SAResults_HillTN05_p3_All_Reordered_Add5.txt', delim = '\t', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
HillTNMed = vroom(file = 'SAResults_HillTNMed_p3_All_Reordered_Add5.txt', delim = '\t', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
HillTN95 = vroom(file = 'SAResults_HillTN95_p3_All_Reordered_Add5.txt', delim = '\t', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)

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

#  Check what days these correspond to - want a uniform distribution in time----
png('PercentileFlowChecks.png', res = 300, units = 'in', width = 15, height = 5)
layout(rbind(c(1,2,3)))
hist(as.Date(days05), breaks = 30, freq = TRUE, axes = FALSE, xlim = c(as.Date('2004-10-01'), as.Date('2010-09-30')), main = 'Lower 5th Percentile Flow Dates')
axis(side = 1, at = as.Date(paste0(seq(2004,2011,1), '-01-01')), labels = format.Date(as.Date(paste0(seq(2004,2011,1), '-01-01')), "%Y"))
axis(side = 2)

hist(as.Date(daysot), breaks = 30, freq = TRUE, axes = FALSE, xlim = c(as.Date('2004-10-01'), as.Date('2010-09-30')), main = '5th - 95th Percentile Flow Dates')
axis(side = 1, at = as.Date(paste0(seq(2004,2011,1), '-01-01')), labels = format.Date(as.Date(paste0(seq(2004,2011,1), '-01-01')), "%Y"))
axis(side = 2)

hist(as.Date(days95), breaks = 30, freq = TRUE, axes = FALSE, xlim = c(as.Date('2004-10-01'), as.Date('2010-09-30')), main = 'Upper 5th Percentile Flow Dates')
axis(side = 1, at = as.Date(paste0(seq(2004,2011,1), '-01-01')), labels = format.Date(as.Date(paste0(seq(2004,2011,1), '-01-01')), "%Y"))
axis(side = 2)
dev.off()

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

#Check what days these correspond to - want a uniform distribution in time
png('PercentileFlowChecks_LowYearly.png', res = 300, units = 'in', width = 15, height = 5)
layout(rbind(c(1,2,3)))
hist(as.Date(days05), breaks = 30, freq = TRUE, axes = FALSE, xlim = c(as.Date('2004-10-01'), as.Date('2010-09-30')), main = 'Lower 5th Percentile Flow Dates')
axis(side = 1, at = as.Date(paste0(seq(2004,2011,1), '-01-01')), labels = format.Date(as.Date(paste0(seq(2004,2011,1), '-01-01')), "%Y"))
axis(side = 2)

hist(as.Date(daysot), breaks = 30, freq = TRUE, axes = FALSE, xlim = c(as.Date('2004-10-01'), as.Date('2010-09-30')), main = '5th - 95th Percentile Flow Dates')
axis(side = 1, at = as.Date(paste0(seq(2004,2011,1), '-01-01')), labels = format.Date(as.Date(paste0(seq(2004,2011,1), '-01-01')), "%Y"))
axis(side = 2)

hist(as.Date(days95), breaks = 30, freq = TRUE, axes = FALSE, xlim = c(as.Date('2004-10-01'), as.Date('2010-09-30')), main = 'Upper 5th Percentile Flow Dates')
axis(side = 1, at = as.Date(paste0(seq(2004,2011,1), '-01-01')), labels = format.Date(as.Date(paste0(seq(2004,2011,1), '-01-01')), "%Y"))
axis(side = 2)
dev.off()

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

#Check what days these correspond to - want a uniform distribution in time
png('PercentileFlowChecks_BothYearly.png', res = 300, units = 'in', width = 15, height = 5)
layout(rbind(c(1,2,3)))
hist(as.Date(days05), breaks = 30, freq = TRUE, axes = FALSE, xlim = c(as.Date('2004-10-01'), as.Date('2010-09-30')), main = 'Lower 5th Percentile Flow Dates')
axis(side = 1, at = as.Date(paste0(seq(2004,2011,1), '-01-01')), labels = format.Date(as.Date(paste0(seq(2004,2011,1), '-01-01')), "%Y"))
axis(side = 2)

hist(as.Date(daysot), breaks = 30, freq = TRUE, axes = FALSE, xlim = c(as.Date('2004-10-01'), as.Date('2010-09-30')), main = '5th - 95th Percentile Flow Dates')
axis(side = 1, at = as.Date(paste0(seq(2004,2011,1), '-01-01')), labels = format.Date(as.Date(paste0(seq(2004,2011,1), '-01-01')), "%Y"))
axis(side = 2)

hist(as.Date(days95), breaks = 30, freq = TRUE, axes = FALSE, xlim = c(as.Date('2004-10-01'), as.Date('2010-09-30')), main = 'Upper 5th Percentile Flow Dates')
axis(side = 1, at = as.Date(paste0(seq(2004,2011,1), '-01-01')), labels = format.Date(as.Date(paste0(seq(2004,2011,1), '-01-01')), "%Y"))
axis(side = 2)
dev.off()

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

#Save input data for Rivanna run----
save.image(file = "C:/Users/js4yd/OneDrive - University of Virginia/BES_Data/BES_Data/RHESSysFiles/BR&POBR/EEs_All_Setup.RData")
#Run on Rivanna: Create a storage matrix for the EEs for each parameter----
#EEs05_b = EEs95_b = EEsot_b = EEsTN05_b = EEsTNMed_b = EEsTN95_b = matrix(NA, nrow = r, ncol = cols)
#Need an extra column for the hillslope ID
#EEs05_h = EEs95_h = EEsot_h = EEsTN05_h = EEsTNMed_h = EEsTN95_h = matrix(NA, nrow = r*length(uhills), ncol = cols+1)
#Create a stoage matrix for the deltas
#Deltas = matrix(NA, nrow = r, ncol = cols)
#Store the column names. These are the dates
#colnms = colnames(BasinSF[,-1])
#Run on Rivanna: Loop over the trajectories----
#Fixme: this should be parallelized - fixed by using job array and rejoin on Rivanna
# tic = Sys.time()
# for (t in 1:r){
#   #Compute the EEs for all parameters in the trajectory
#   for (i in 1:cols){
#     #Determine the base index. This is used for basin, and the first hillslope value
#     ind = i+(1+cols)*(t-1)
#     
#     #Find the parameter column that was changed, before any processing was completed. 
#     #This is the same column that will be used for the Deltas and EEs
#     parm = which((OrigParams[ind+1,] - OrigParams[ind,]) != 0)
#     #Computes the exact delta from the modified sampling file. Allows for positive and negative deltas.
#     delta = InputParams[ind+1,parm] - InputParams[ind,parm]
#     #Adjust delta to the range sampled for the parameter
#     delta = delta/abs(ParamRanges$Upper[parm] - ParamRanges$Lower[parm])
#     Deltas[t, parm] = delta  
#     #Fixme: record deltas for every variable, not only the parameter that changed
#     #delta for other parameters could be larger than the OAT parameter that was supposed to change
#     
#     #Compute the difference for highest 5th percentile, lowest 5th percentile and all other flows, as defined by the days from the observed streamflow record
#     #SSE - not using because we don't think it's appropriate for Morris method
#     #Fixme: compare SSE to sum of absolute errors. Is there theoretical proof for Morris?
#     #diff05 = (sum((BasinSF05[ind+1,] - obs05$Flow)^2) - sum((BasinSF05[ind,] - obs05$Flow)^2))  
#     #diff95 = (sum((BasinSF95[ind+1,] - obs95$Flow)^2) - sum((BasinSF95[ind,] - obs95$Flow)^2))
#     #diffot = (sum((BasinSFot[ind+1,] - obsot$Flow)^2) - sum((BasinSFot[ind,] - obsot$Flow)^2))
#     
#     #Fixme: Compare to using the median flow instead of the observed flow for basin
#     
#     #Sum absolute errors
#     diff05 = (sum(abs((BasinSF05[ind+1,] - obs05$Flow))) - sum(abs((BasinSF05[ind,] - obs05$Flow))))  
#     diff95 = (sum(abs((BasinSF95[ind+1,] - obs95$Flow))) - sum(abs((BasinSF95[ind,] - obs95$Flow))))
#     diffot = (sum(abs((BasinSFot[ind+1,] - obsot$Flow))) - sum(abs((BasinSFot[ind,] - obsot$Flow))))
#     
#     #TN
#     diffTN05 = (sum(abs((BasinTN05[ind+1,-1] - obsTN$TN))) - sum(abs((BasinTN05[ind,-1] - obsTN$TN))))  
#     diffTNMed = (sum(abs((BasinTNMed[ind+1,-1] - obsTN$TN))) - sum(abs((BasinTNMed[ind,-1] - obsTN$TN))))
#     diffTN95 = (sum(abs((BasinTN95[ind+1,-1] - obsTN$TN))) - sum(abs((BasinTN95[ind,-1] - obsTN$TN))))
#     
#     #Computes EEs with specified metric
#     EEs05_b[t, parm] = diff05/delta
#     EEs95_b[t, parm] = diff95/delta
#     EEsot_b[t, parm] = diffot/delta
#     
#     EEsTN05_b[t, parm] = diffTN05/delta
#     EEsTNMed_b[t, parm] = diffTNMed/delta
#     EEsTN95_b[t, parm] = diffTN95/delta
#     
#     #Hillslopes
#     #Fixme: Compare to median streamflow and TN
#     diff05_h = diff95_h = diffot_h = diffTN05_h = diffTNMed_h = diffTN95_h = NA
#     for (hi in 1:length(uhills)){
#       #Get the indices for this hillslope
#       IndsHill = ind+(uhills[hi]-1)*nrow(BasinSF)
#       #SF
#       diff05_h = (sum(abs((HillSF05[IndsHill+1,] - MedHills05[which(MedHills05[,1] == uhills[hi]),-1]))) - sum(abs((HillSF05[IndsHill,] - MedHills05[which(MedHills05[,1] == uhills[hi]),-1]))))  
#       diff95_h = (sum(abs((HillSF95[IndsHill+1,] - MedHills95[which(MedHills95[,1] == uhills[hi]),-1]))) - sum(abs((HillSF95[IndsHill,] - MedHills95[which(MedHills95[,1] == uhills[hi]),-1]))))
#       diffot_h = (sum(abs((HillSFot[IndsHill+1,] - MedHillsot[which(MedHillsot[,1] == uhills[hi]),-1]))) - sum(abs((HillSFot[IndsHill,] - MedHillsot[which(MedHillsot[,1] == uhills[hi]),-1]))))
#       
#       #TN
#       diffTN05_h = (sum(abs((HillTN05[IndsHill+1,-c(1,2)] - MedTN05Hills[which(MedTN05Hills[,1] == uhills[hi]),-1]))) - sum(abs((HillTN05[IndsHill,-c(1,2)] - MedTN05Hills[which(MedTN05Hills[,1] == uhills[hi]),-1]))))  
#       diffTNMed_h = (sum(abs((HillTNMed[IndsHill+1,-c(1,2)] - MedTNMedHills[which(MedTNMedHills[,1] == uhills[hi]),-1]))) - sum(abs((HillTNMed[IndsHill,-c(1,2)] - MedTNMedHills[which(MedTNMedHills[,1] == uhills[hi]),-1]))))
#       diffTN95_h = (sum(abs((HillTN95[IndsHill+1,-c(1,2)] - MedTN95Hills[which(MedTN95Hills[,1] == uhills[hi]),-1]))) - sum(abs((HillTN95[IndsHill,-c(1,2)] - MedTN95Hills[which(MedTN95Hills[,1] == uhills[hi]),-1]))))
#       
#       EEs05_h[t+(hi-1)*r, c(1,parm+1)] = c(uhills[hi], diff05_h/delta)
#       EEs95_h[t+(hi-1)*r, c(1,parm+1)] = c(uhills[hi], diff95_h/delta)
#       EEsot_h[t+(hi-1)*r, c(1,parm+1)] = c(uhills[hi], diffot_h/delta)
#       
#       EEsTN05_h[t+(hi-1)*r, c(1,parm+1)] = c(uhills[hi], diffTN05_h/delta)
#       EEsTNMed_h[t+(hi-1)*r, c(1,parm+1)] = c(uhills[hi], diffTNMed_h/delta)
#       EEsTN95_h[t+(hi-1)*r, c(1,parm+1)] = c(uhills[hi], diffTN95_h/delta)
#     }
#   }
# }
# toc = Sys.time()
# #32.5 mins per trajectory
# print(toc-tic)
# rm(i, t, hi, parm, ind, delta, diff05, diff05_h, diff95, diff95_h, diffot, diffot_h, diffTN05, diffTN05_h, diffTN95, diffTN95_h, diffTNMed, diffTNMed_h, IndsHill)
# 
# save.image(file = 'EEs_Corr.RData')

#Load data from Rivanna run----
#Input data load - loads all data that were saved in the save.image file above this line
load("C:/Users/js4yd/OneDrive - University of Virginia/BES_Data/BES_Data/RHESSysFiles/BR&POBR/EEs_All_Setup.RData")

#Load EE and Deltas info----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR")
Deltas = read.table(file = paste0(getwd(), '/Deltas_All_Add5.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEs05_b = read.table(file = paste0(getwd(), '/EEs05_b_All_Add5.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEs95_b = read.table(file = paste0(getwd(), '/EEs95_b_All_Add5.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE) 
EEsot_b = read.table(file = paste0(getwd(), '/EEsot_b_All_Add5.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEsTN05_b = read.table(file = paste0(getwd(), '/EEsTN05_b_All_Add5.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE) 
EEsTNMed_b = read.table(file = paste0(getwd(), '/EEsTNMed_b_All_Add5.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE) 
EEsTN95_b = read.table(file = paste0(getwd(), '/EEsTN95_b_All_Add5.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEs05_h = read.table(file = paste0(getwd(), '/EEs05_h_All_Add5.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEs95_h = read.table(file = paste0(getwd(), '/EEs95_h_All_Add5.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEsot_h = read.table(file = paste0(getwd(), '/EEsot_h_All_Add5.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEsTN05_h = read.table(file = paste0(getwd(), '/EEsTN05_h_All_Add5.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE) 
EEsTNMed_h = read.table(file = paste0(getwd(), '/EEsTNMed_h_All_Add5.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE) 
EEsTN95_h = read.table(file = paste0(getwd(), '/EEsTN95_h_All_Add5.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)

#Check if EEs have NA values because of sampling scheme errors
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

#Aggregate the EEs for variables that require it----
AggregateEEs = function(EEs, #The elementary effect matrix
                        ColNames, #Matrix or vector containing the names of the columns. One row per item to be summed
                        FUN #The function to use to aggregate the columns (e.g., mean, max)
                        )
{
  EEmat = matrix(NA, nrow = nrow(ColNames), ncol = nrow(EEs))
  for (i in 1:nrow(EEmat)){
    EEmat[i,] = apply(X = EEs[, colnames(EEs) %in% ColNames[i,]], MARGIN = 1, FUN = FUN)
  }
  return(t(EEmat))
}

#Mean absolute value function
meanabs = function(x){
  mean(abs(x))
}

#Vector of column names that are aggregated. Later, they will be removed from the dataframe.
ColsAggregated = c('s8_silt', 's8_sand', 's8_clay', 's108_silt', 's108_sand', 's108_clay', 's9_silt', 's9_sand', 's9_clay', 's109_silt', 's109_sand', 's109_clay',
                   'v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance',
                   'v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance',
                   'v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig', 'v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig',
                   'v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig', 'v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig',
                   's108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v', 's109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v',
                   's108_m', 's8_m', 's109_m', 's9_m',
                   's108_porosity_0', 's8_porosity_0', 's109_porosity_0', 's9_porosity_0',
                   'v102_epc.topt', 'v102_epc.tmax', 'v3_epc.topt', 'v3_epc.tmax',
                   'v102_epc.leaf_cn', 'v102_epc.leaflitr_cn', 'v3_epc.leaf_cn', 'v3_epc.leaflitr_cn')

# Sum constrained variables: 10 new averaged/max variables----
#  Soil Texture----
# 's8_silt' 's8_sand' 's8_clay'
# 's108_silt' 's108_sand' 's108_clay'
# 's9_silt' 's9_sand' 's9_clay'
# 's109_silt' 's109_sand' 's109_clay'
EEs05_b[,seq(ncol(EEs05_b)+1,ncol(EEs05_b)+4,1)] = AggregateEEs(EEs = EEs05_b, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEs05_b)[(ncol(EEs05_b)-3):ncol(EEs05_b)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEs95_b[,seq(ncol(EEs95_b)+1,ncol(EEs95_b)+4,1)] = AggregateEEs(EEs = EEs95_b, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEs95_b)[(ncol(EEs95_b)-3):ncol(EEs95_b)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEsot_b[,seq(ncol(EEsot_b)+1,ncol(EEsot_b)+4,1)] = AggregateEEs(EEs = EEsot_b, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEsot_b)[(ncol(EEsot_b)-3):ncol(EEsot_b)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEsTN05_b[,seq(ncol(EEsTN05_b)+1,ncol(EEsTN05_b)+4,1)] = AggregateEEs(EEs = EEsTN05_b, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEsTN05_b)[(ncol(EEsTN05_b)-3):ncol(EEsTN05_b)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEsTN95_b[,seq(ncol(EEsTN95_b)+1,ncol(EEsTN95_b)+4,1)] = AggregateEEs(EEs = EEsTN95_b, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEsTN95_b)[(ncol(EEsTN95_b)-3):ncol(EEsTN95_b)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEsTNMed_b[,seq(ncol(EEsTNMed_b)+1,ncol(EEsTNMed_b)+4,1)] = AggregateEEs(EEs = EEsTNMed_b, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEsTNMed_b)[(ncol(EEsTNMed_b)-3):ncol(EEsTNMed_b)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEs05_h[,seq(ncol(EEs05_h)+1,ncol(EEs05_h)+4,1)] = AggregateEEs(EEs = EEs05_h, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEs05_h)[(ncol(EEs05_h)-3):ncol(EEs05_h)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEs95_h[,seq(ncol(EEs95_h)+1,ncol(EEs95_h)+4,1)] = AggregateEEs(EEs = EEs95_h, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEs95_h)[(ncol(EEs95_h)-3):ncol(EEs95_h)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEsot_h[,seq(ncol(EEsot_h)+1,ncol(EEsot_h)+4,1)] = AggregateEEs(EEs = EEsot_h, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEsot_h)[(ncol(EEsot_h)-3):ncol(EEsot_h)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEsTN05_h[,seq(ncol(EEsTN05_h)+1,ncol(EEsTN05_h)+4,1)] = AggregateEEs(EEs = EEsTN05_h, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEsTN05_h)[(ncol(EEsTN05_h)-3):ncol(EEsTN05_h)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEsTN95_h[,seq(ncol(EEsTN95_h)+1,ncol(EEsTN95_h)+4,1)] = AggregateEEs(EEs = EEsTN95_h, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEsTN95_h)[(ncol(EEsTN95_h)-3):ncol(EEsTN95_h)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEsTNMed_h[,seq(ncol(EEsTNMed_h)+1,ncol(EEsTNMed_h)+4,1)] = AggregateEEs(EEs = EEsTNMed_h, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEsTNMed_h)[(ncol(EEsTNMed_h)-3):ncol(EEsTNMed_h)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

#  K Radiation----
# 'v102_K_absorptance' 'v102_K_reflectance' 'v102_K_transmittance'
EEs05_b[,seq(ncol(EEs05_b)+1,ncol(EEs05_b)+1,1)] = AggregateEEs(EEs = EEs05_b, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEs05_b)[(ncol(EEs05_b)-0):ncol(EEs05_b)] = 'v102_K_All'

EEs95_b[,seq(ncol(EEs95_b)+1,ncol(EEs95_b)+1,1)] = AggregateEEs(EEs = EEs95_b, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEs95_b)[(ncol(EEs95_b)-0):ncol(EEs95_b)] = 'v102_K_All'

EEsot_b[,seq(ncol(EEsot_b)+1,ncol(EEsot_b)+1,1)] = AggregateEEs(EEs = EEsot_b, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEsot_b)[(ncol(EEsot_b)-0):ncol(EEsot_b)] = 'v102_K_All'

EEsTN05_b[,seq(ncol(EEsTN05_b)+1,ncol(EEsTN05_b)+1,1)] = AggregateEEs(EEs = EEsTN05_b, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEsTN05_b)[(ncol(EEsTN05_b)-0):ncol(EEsTN05_b)] = 'v102_K_All'

EEsTN95_b[,seq(ncol(EEsTN95_b)+1,ncol(EEsTN95_b)+1,1)] = AggregateEEs(EEs = EEsTN95_b, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEsTN95_b)[(ncol(EEsTN95_b)-0):ncol(EEsTN95_b)] = 'v102_K_All'

EEsTNMed_b[,seq(ncol(EEsTNMed_b)+1,ncol(EEsTNMed_b)+1,1)] = AggregateEEs(EEs = EEsTNMed_b, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEsTNMed_b)[(ncol(EEsTNMed_b)-0):ncol(EEsTNMed_b)] = 'v102_K_All'

EEs05_h[,seq(ncol(EEs05_h)+1,ncol(EEs05_h)+1,1)] = AggregateEEs(EEs = EEs05_h, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEs05_h)[(ncol(EEs05_h)-0):ncol(EEs05_h)] = 'v102_K_All'

EEs95_h[,seq(ncol(EEs95_h)+1,ncol(EEs95_h)+1,1)] = AggregateEEs(EEs = EEs95_h, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEs95_h)[(ncol(EEs95_h)-0):ncol(EEs95_h)] = 'v102_K_All'

EEsot_h[,seq(ncol(EEsot_h)+1,ncol(EEsot_h)+1,1)] = AggregateEEs(EEs = EEsot_h, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEsot_h)[(ncol(EEsot_h)-0):ncol(EEsot_h)] = 'v102_K_All'

EEsTN05_h[,seq(ncol(EEsTN05_h)+1,ncol(EEsTN05_h)+1,1)] = AggregateEEs(EEs = EEsTN05_h, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEsTN05_h)[(ncol(EEsTN05_h)-0):ncol(EEsTN05_h)] = 'v102_K_All'

EEsTN95_h[,seq(ncol(EEsTN95_h)+1,ncol(EEsTN95_h)+1,1)] = AggregateEEs(EEs = EEsTN95_h, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEsTN95_h)[(ncol(EEsTN95_h)-0):ncol(EEsTN95_h)] = 'v102_K_All'

EEsTNMed_h[,seq(ncol(EEsTNMed_h)+1,ncol(EEsTNMed_h)+1,1)] = AggregateEEs(EEs = EEsTNMed_h, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEsTNMed_h)[(ncol(EEsTNMed_h)-0):ncol(EEsTNMed_h)] = 'v102_K_All'

#  PAR Radiation----
# 'v102_PAR_absorptance' 'v102_PAR_reflectance' 'v102_PAR_transmittance'
EEs05_b[,seq(ncol(EEs05_b)+1,ncol(EEs05_b)+1,1)] = AggregateEEs(EEs = EEs05_b, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEs05_b)[(ncol(EEs05_b)-0):ncol(EEs05_b)] = 'v102_PAR_All'

EEs95_b[,seq(ncol(EEs95_b)+1,ncol(EEs95_b)+1,1)] = AggregateEEs(EEs = EEs95_b, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEs95_b)[(ncol(EEs95_b)-0):ncol(EEs95_b)] = 'v102_PAR_All'

EEsot_b[,seq(ncol(EEsot_b)+1,ncol(EEsot_b)+1,1)] = AggregateEEs(EEs = EEsot_b, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEsot_b)[(ncol(EEsot_b)-0):ncol(EEsot_b)] = 'v102_PAR_All'

EEsTN05_b[,seq(ncol(EEsTN05_b)+1,ncol(EEsTN05_b)+1,1)] = AggregateEEs(EEs = EEsTN05_b, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEsTN05_b)[(ncol(EEsTN05_b)-0):ncol(EEsTN05_b)] = 'v102_PAR_All'

EEsTN95_b[,seq(ncol(EEsTN95_b)+1,ncol(EEsTN95_b)+1,1)] = AggregateEEs(EEs = EEsTN95_b, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEsTN95_b)[(ncol(EEsTN95_b)-0):ncol(EEsTN95_b)] = 'v102_PAR_All'

EEsTNMed_b[,seq(ncol(EEsTNMed_b)+1,ncol(EEsTNMed_b)+1,1)] = AggregateEEs(EEs = EEsTNMed_b, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEsTNMed_b)[(ncol(EEsTNMed_b)-0):ncol(EEsTNMed_b)] = 'v102_PAR_All'

EEs05_h[,seq(ncol(EEs05_h)+1,ncol(EEs05_h)+1,1)] = AggregateEEs(EEs = EEs05_h, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEs05_h)[(ncol(EEs05_h)-0):ncol(EEs05_h)] = 'v102_PAR_All'

EEs95_h[,seq(ncol(EEs95_h)+1,ncol(EEs95_h)+1,1)] = AggregateEEs(EEs = EEs95_h, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEs95_h)[(ncol(EEs95_h)-0):ncol(EEs95_h)] = 'v102_PAR_All'

EEsot_h[,seq(ncol(EEsot_h)+1,ncol(EEsot_h)+1,1)] = AggregateEEs(EEs = EEsot_h, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEsot_h)[(ncol(EEsot_h)-0):ncol(EEsot_h)] = 'v102_PAR_All'

EEsTN05_h[,seq(ncol(EEsTN05_h)+1,ncol(EEsTN05_h)+1,1)] = AggregateEEs(EEs = EEsTN05_h, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEsTN05_h)[(ncol(EEsTN05_h)-0):ncol(EEsTN05_h)] = 'v102_PAR_All'

EEsTN95_h[,seq(ncol(EEsTN95_h)+1,ncol(EEsTN95_h)+1,1)] = AggregateEEs(EEs = EEsTN95_h, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEsTN95_h)[(ncol(EEsTN95_h)-0):ncol(EEsTN95_h)] = 'v102_PAR_All'

EEsTNMed_h[,seq(ncol(EEsTNMed_h)+1,ncol(EEsTNMed_h)+1,1)] = AggregateEEs(EEs = EEsTNMed_h, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEsTNMed_h)[(ncol(EEsTNMed_h)-0):ncol(EEsTNMed_h)] = 'v102_PAR_All'

#  Fine Root to Litter----
# 'v102_epc.frootlitr_fcel' 'v102_epc.frootlitr_flab' 'v102_epc.frootlitr_flig'
# 'v3_epc.frootlitr_fcel' 'v3_epc.frootlitr_flab' 'v3_epc.frootlitr_flig'
EEs05_b[,seq(ncol(EEs05_b)+1,ncol(EEs05_b)+2,1)] = AggregateEEs(EEs = EEs05_b, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEs05_b)[(ncol(EEs05_b)-1):ncol(EEs05_b)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEs95_b[,seq(ncol(EEs95_b)+1,ncol(EEs95_b)+2,1)] = AggregateEEs(EEs = EEs95_b, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEs95_b)[(ncol(EEs95_b)-1):ncol(EEs95_b)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEsot_b[,seq(ncol(EEsot_b)+1,ncol(EEsot_b)+2,1)] = AggregateEEs(EEs = EEsot_b, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEsot_b)[(ncol(EEsot_b)-1):ncol(EEsot_b)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEsTN05_b[,seq(ncol(EEsTN05_b)+1,ncol(EEsTN05_b)+2,1)] = AggregateEEs(EEs = EEsTN05_b, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEsTN05_b)[(ncol(EEsTN05_b)-1):ncol(EEsTN05_b)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEsTN95_b[,seq(ncol(EEsTN95_b)+1,ncol(EEsTN95_b)+2,1)] = AggregateEEs(EEs = EEsTN95_b, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEsTN95_b)[(ncol(EEsTN95_b)-1):ncol(EEsTN95_b)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEsTNMed_b[,seq(ncol(EEsTNMed_b)+1,ncol(EEsTNMed_b)+2,1)] = AggregateEEs(EEs = EEsTNMed_b, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEsTNMed_b)[(ncol(EEsTNMed_b)-1):ncol(EEsTNMed_b)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEs05_h[,seq(ncol(EEs05_h)+1,ncol(EEs05_h)+2,1)] = AggregateEEs(EEs = EEs05_h, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEs05_h)[(ncol(EEs05_h)-1):ncol(EEs05_h)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEs95_h[,seq(ncol(EEs95_h)+1,ncol(EEs95_h)+2,1)] = AggregateEEs(EEs = EEs95_h, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEs95_h)[(ncol(EEs95_h)-1):ncol(EEs95_h)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEsot_h[,seq(ncol(EEsot_h)+1,ncol(EEsot_h)+2,1)] = AggregateEEs(EEs = EEsot_h, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEsot_h)[(ncol(EEsot_h)-1):ncol(EEsot_h)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEsTN05_h[,seq(ncol(EEsTN05_h)+1,ncol(EEsTN05_h)+2,1)] = AggregateEEs(EEs = EEsTN05_h, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEsTN05_h)[(ncol(EEsTN05_h)-1):ncol(EEsTN05_h)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEsTN95_h[,seq(ncol(EEsTN95_h)+1,ncol(EEsTN95_h)+2,1)] = AggregateEEs(EEs = EEsTN95_h, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEsTN95_h)[(ncol(EEsTN95_h)-1):ncol(EEsTN95_h)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEsTNMed_h[,seq(ncol(EEsTNMed_h)+1,ncol(EEsTNMed_h)+2,1)] = AggregateEEs(EEs = EEsTNMed_h, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEsTNMed_h)[(ncol(EEsTNMed_h)-1):ncol(EEsTNMed_h)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

#  Leaf to Litter----
# 'v102_epc.leaflitr_fcel' 'v102_epc.leaflitr_flab' 'v102_epc.leaflitr_flig'
# 'v3_epc.leaflitr_fcel' 'v3_epc.leaflitr_flab' 'v3_epc.leaflitr_flig'
EEs05_b[,seq(ncol(EEs05_b)+1,ncol(EEs05_b)+2,1)] = AggregateEEs(EEs = EEs05_b, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEs05_b)[(ncol(EEs05_b)-1):ncol(EEs05_b)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEs95_b[,seq(ncol(EEs95_b)+1,ncol(EEs95_b)+2,1)] = AggregateEEs(EEs = EEs95_b, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEs95_b)[(ncol(EEs95_b)-1):ncol(EEs95_b)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEsot_b[,seq(ncol(EEsot_b)+1,ncol(EEsot_b)+2,1)] = AggregateEEs(EEs = EEsot_b, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEsot_b)[(ncol(EEsot_b)-1):ncol(EEsot_b)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEsTN05_b[,seq(ncol(EEsTN05_b)+1,ncol(EEsTN05_b)+2,1)] = AggregateEEs(EEs = EEsTN05_b, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEsTN05_b)[(ncol(EEsTN05_b)-1):ncol(EEsTN05_b)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEsTN95_b[,seq(ncol(EEsTN95_b)+1,ncol(EEsTN95_b)+2,1)] = AggregateEEs(EEs = EEsTN95_b, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEsTN95_b)[(ncol(EEsTN95_b)-1):ncol(EEsTN95_b)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEsTNMed_b[,seq(ncol(EEsTNMed_b)+1,ncol(EEsTNMed_b)+2,1)] = AggregateEEs(EEs = EEsTNMed_b, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEsTNMed_b)[(ncol(EEsTNMed_b)-1):ncol(EEsTNMed_b)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEs05_h[,seq(ncol(EEs05_h)+1,ncol(EEs05_h)+2,1)] = AggregateEEs(EEs = EEs05_h, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEs05_h)[(ncol(EEs05_h)-1):ncol(EEs05_h)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEs95_h[,seq(ncol(EEs95_h)+1,ncol(EEs95_h)+2,1)] = AggregateEEs(EEs = EEs95_h, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEs95_h)[(ncol(EEs95_h)-1):ncol(EEs95_h)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEsot_h[,seq(ncol(EEsot_h)+1,ncol(EEsot_h)+2,1)] = AggregateEEs(EEs = EEsot_h, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEsot_h)[(ncol(EEsot_h)-1):ncol(EEsot_h)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEsTN05_h[,seq(ncol(EEsTN05_h)+1,ncol(EEsTN05_h)+2,1)] = AggregateEEs(EEs = EEsTN05_h, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEsTN05_h)[(ncol(EEsTN05_h)-1):ncol(EEsTN05_h)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEsTN95_h[,seq(ncol(EEsTN95_h)+1,ncol(EEsTN95_h)+2,1)] = AggregateEEs(EEs = EEsTN95_h, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEsTN95_h)[(ncol(EEsTN95_h)-1):ncol(EEsTN95_h)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEsTNMed_h[,seq(ncol(EEsTNMed_h)+1,ncol(EEsTNMed_h)+2,1)] = AggregateEEs(EEs = EEsTNMed_h, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEsTNMed_h)[(ncol(EEsTNMed_h)-1):ncol(EEsTNMed_h)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

# Inequality relations: 10 new averaged/max variables----
#  Soil Sat. Cond.----
# 's109_Ksat_0' 's9_Ksat_0' 's109_Ksat_0_v' 's9_Ksat_0_v'
# 's108_Ksat_0' 's8_Ksat_0' 's108_Ksat_0_v' 's8_Ksat_0_v'
EEs05_b[,seq(ncol(EEs05_b)+1,ncol(EEs05_b)+2,1)] = AggregateEEs(EEs = EEs05_b, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEs05_b)[(ncol(EEs05_b)-1):ncol(EEs05_b)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEs95_b[,seq(ncol(EEs95_b)+1,ncol(EEs95_b)+2,1)] = AggregateEEs(EEs = EEs95_b, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEs95_b)[(ncol(EEs95_b)-1):ncol(EEs95_b)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEsot_b[,seq(ncol(EEsot_b)+1,ncol(EEsot_b)+2,1)] = AggregateEEs(EEs = EEsot_b, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEsot_b)[(ncol(EEsot_b)-1):ncol(EEsot_b)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEsTN05_b[,seq(ncol(EEsTN05_b)+1,ncol(EEsTN05_b)+2,1)] = AggregateEEs(EEs = EEsTN05_b, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEsTN05_b)[(ncol(EEsTN05_b)-1):ncol(EEsTN05_b)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEsTN95_b[,seq(ncol(EEsTN95_b)+1,ncol(EEsTN95_b)+2,1)] = AggregateEEs(EEs = EEsTN95_b, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEsTN95_b)[(ncol(EEsTN95_b)-1):ncol(EEsTN95_b)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEsTNMed_b[,seq(ncol(EEsTNMed_b)+1,ncol(EEsTNMed_b)+2,1)] = AggregateEEs(EEs = EEsTNMed_b, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEsTNMed_b)[(ncol(EEsTNMed_b)-1):ncol(EEsTNMed_b)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEs05_h[,seq(ncol(EEs05_h)+1,ncol(EEs05_h)+2,1)] = AggregateEEs(EEs = EEs05_h, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEs05_h)[(ncol(EEs05_h)-1):ncol(EEs05_h)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEs95_h[,seq(ncol(EEs95_h)+1,ncol(EEs95_h)+2,1)] = AggregateEEs(EEs = EEs95_h, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEs95_h)[(ncol(EEs95_h)-1):ncol(EEs95_h)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEsot_h[,seq(ncol(EEsot_h)+1,ncol(EEsot_h)+2,1)] = AggregateEEs(EEs = EEsot_h, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEsot_h)[(ncol(EEsot_h)-1):ncol(EEsot_h)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEsTN05_h[,seq(ncol(EEsTN05_h)+1,ncol(EEsTN05_h)+2,1)] = AggregateEEs(EEs = EEsTN05_h, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEsTN05_h)[(ncol(EEsTN05_h)-1):ncol(EEsTN05_h)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEsTN95_h[,seq(ncol(EEsTN95_h)+1,ncol(EEsTN95_h)+2,1)] = AggregateEEs(EEs = EEsTN95_h, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEsTN95_h)[(ncol(EEsTN95_h)-1):ncol(EEsTN95_h)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEsTNMed_h[,seq(ncol(EEsTNMed_h)+1,ncol(EEsTNMed_h)+2,1)] = AggregateEEs(EEs = EEsTNMed_h, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEsTNMed_h)[(ncol(EEsTNMed_h)-1):ncol(EEsTNMed_h)] = c('Soil8_Ksat', 'Soil9_Ksat')

#  Soil m decay----
# 's109_m' 's9_m'
# 's108_m' 's8_m'
EEs05_b[,seq(ncol(EEs05_b)+1,ncol(EEs05_b)+2,1)] = AggregateEEs(EEs = EEs05_b, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEs05_b)[(ncol(EEs05_b)-1):ncol(EEs05_b)] = c('Soil8_m', 'Soil9_m')

EEs95_b[,seq(ncol(EEs95_b)+1,ncol(EEs95_b)+2,1)] = AggregateEEs(EEs = EEs95_b, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEs95_b)[(ncol(EEs95_b)-1):ncol(EEs95_b)] = c('Soil8_m', 'Soil9_m')

EEsot_b[,seq(ncol(EEsot_b)+1,ncol(EEsot_b)+2,1)] = AggregateEEs(EEs = EEsot_b, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEsot_b)[(ncol(EEsot_b)-1):ncol(EEsot_b)] = c('Soil8_m', 'Soil9_m')

EEsTN05_b[,seq(ncol(EEsTN05_b)+1,ncol(EEsTN05_b)+2,1)] = AggregateEEs(EEs = EEsTN05_b, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEsTN05_b)[(ncol(EEsTN05_b)-1):ncol(EEsTN05_b)] = c('Soil8_m', 'Soil9_m')

EEsTN95_b[,seq(ncol(EEsTN95_b)+1,ncol(EEsTN95_b)+2,1)] = AggregateEEs(EEs = EEsTN95_b, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEsTN95_b)[(ncol(EEsTN95_b)-1):ncol(EEsTN95_b)] = c('Soil8_m', 'Soil9_m')

EEsTNMed_b[,seq(ncol(EEsTNMed_b)+1,ncol(EEsTNMed_b)+2,1)] = AggregateEEs(EEs = EEsTNMed_b, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEsTNMed_b)[(ncol(EEsTNMed_b)-1):ncol(EEsTNMed_b)] = c('Soil8_m', 'Soil9_m')

EEs05_h[,seq(ncol(EEs05_h)+1,ncol(EEs05_h)+2,1)] = AggregateEEs(EEs = EEs05_h, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEs05_h)[(ncol(EEs05_h)-1):ncol(EEs05_h)] = c('Soil8_m', 'Soil9_m')

EEs95_h[,seq(ncol(EEs95_h)+1,ncol(EEs95_h)+2,1)] = AggregateEEs(EEs = EEs95_h, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEs95_h)[(ncol(EEs95_h)-1):ncol(EEs95_h)] = c('Soil8_m', 'Soil9_m')

EEsot_h[,seq(ncol(EEsot_h)+1,ncol(EEsot_h)+2,1)] = AggregateEEs(EEs = EEsot_h, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEsot_h)[(ncol(EEsot_h)-1):ncol(EEsot_h)] = c('Soil8_m', 'Soil9_m')

EEsTN05_h[,seq(ncol(EEsTN05_h)+1,ncol(EEsTN05_h)+2,1)] = AggregateEEs(EEs = EEsTN05_h, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEsTN05_h)[(ncol(EEsTN05_h)-1):ncol(EEsTN05_h)] = c('Soil8_m', 'Soil9_m')

EEsTN95_h[,seq(ncol(EEsTN95_h)+1,ncol(EEsTN95_h)+2,1)] = AggregateEEs(EEs = EEsTN95_h, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEsTN95_h)[(ncol(EEsTN95_h)-1):ncol(EEsTN95_h)] = c('Soil8_m', 'Soil9_m')

EEsTNMed_h[,seq(ncol(EEsTNMed_h)+1,ncol(EEsTNMed_h)+2,1)] = AggregateEEs(EEs = EEsTNMed_h, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEsTNMed_h)[(ncol(EEsTNMed_h)-1):ncol(EEsTNMed_h)] = c('Soil8_m', 'Soil9_m')

#  Soil porosity at surface----
# 's109_porosity_0' 's9_porosity_0'
# 's108_porosity_0' 's8_porosity_0'
EEs05_b[,seq(ncol(EEs05_b)+1,ncol(EEs05_b)+2,1)] = AggregateEEs(EEs = EEs05_b, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEs05_b)[(ncol(EEs05_b)-1):ncol(EEs05_b)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEs95_b[,seq(ncol(EEs95_b)+1,ncol(EEs95_b)+2,1)] = AggregateEEs(EEs = EEs95_b, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEs95_b)[(ncol(EEs95_b)-1):ncol(EEs95_b)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEsot_b[,seq(ncol(EEsot_b)+1,ncol(EEsot_b)+2,1)] = AggregateEEs(EEs = EEsot_b, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEsot_b)[(ncol(EEsot_b)-1):ncol(EEsot_b)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEsTN05_b[,seq(ncol(EEsTN05_b)+1,ncol(EEsTN05_b)+2,1)] = AggregateEEs(EEs = EEsTN05_b, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEsTN05_b)[(ncol(EEsTN05_b)-1):ncol(EEsTN05_b)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEsTN95_b[,seq(ncol(EEsTN95_b)+1,ncol(EEsTN95_b)+2,1)] = AggregateEEs(EEs = EEsTN95_b, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEsTN95_b)[(ncol(EEsTN95_b)-1):ncol(EEsTN95_b)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEsTNMed_b[,seq(ncol(EEsTNMed_b)+1,ncol(EEsTNMed_b)+2,1)] = AggregateEEs(EEs = EEsTNMed_b, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEsTNMed_b)[(ncol(EEsTNMed_b)-1):ncol(EEsTNMed_b)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEs05_h[,seq(ncol(EEs05_h)+1,ncol(EEs05_h)+2,1)] = AggregateEEs(EEs = EEs05_h, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEs05_h)[(ncol(EEs05_h)-1):ncol(EEs05_h)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEs95_h[,seq(ncol(EEs95_h)+1,ncol(EEs95_h)+2,1)] = AggregateEEs(EEs = EEs95_h, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEs95_h)[(ncol(EEs95_h)-1):ncol(EEs95_h)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEsot_h[,seq(ncol(EEsot_h)+1,ncol(EEsot_h)+2,1)] = AggregateEEs(EEs = EEsot_h, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEsot_h)[(ncol(EEsot_h)-1):ncol(EEsot_h)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEsTN05_h[,seq(ncol(EEsTN05_h)+1,ncol(EEsTN05_h)+2,1)] = AggregateEEs(EEs = EEsTN05_h, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEsTN05_h)[(ncol(EEsTN05_h)-1):ncol(EEsTN05_h)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEsTN95_h[,seq(ncol(EEsTN95_h)+1,ncol(EEsTN95_h)+2,1)] = AggregateEEs(EEs = EEsTN95_h, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEsTN95_h)[(ncol(EEsTN95_h)-1):ncol(EEsTN95_h)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEsTNMed_h[,seq(ncol(EEsTNMed_h)+1,ncol(EEsTNMed_h)+2,1)] = AggregateEEs(EEs = EEsTNMed_h, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEsTNMed_h)[(ncol(EEsTNMed_h)-1):ncol(EEsTNMed_h)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

#  Optimal growing temperature----
# 'v102_epc.topt' 'v102_epc.tmax'
# 'v3_epc.topt' 'v3_epc.tmax'
EEs05_b[,seq(ncol(EEs05_b)+1,ncol(EEs05_b)+2,1)] = AggregateEEs(EEs = EEs05_b, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEs05_b)[(ncol(EEs05_b)-1):ncol(EEs05_b)] = c('v102_Temp', 'v3_Temp')

EEs95_b[,seq(ncol(EEs95_b)+1,ncol(EEs95_b)+2,1)] = AggregateEEs(EEs = EEs95_b, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEs95_b)[(ncol(EEs95_b)-1):ncol(EEs95_b)] = c('v102_Temp', 'v3_Temp')

EEsot_b[,seq(ncol(EEsot_b)+1,ncol(EEsot_b)+2,1)] = AggregateEEs(EEs = EEsot_b, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEsot_b)[(ncol(EEsot_b)-1):ncol(EEsot_b)] = c('v102_Temp', 'v3_Temp')

EEsTN05_b[,seq(ncol(EEsTN05_b)+1,ncol(EEsTN05_b)+2,1)] = AggregateEEs(EEs = EEsTN05_b, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEsTN05_b)[(ncol(EEsTN05_b)-1):ncol(EEsTN05_b)] = c('v102_Temp', 'v3_Temp')

EEsTN95_b[,seq(ncol(EEsTN95_b)+1,ncol(EEsTN95_b)+2,1)] = AggregateEEs(EEs = EEsTN95_b, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEsTN95_b)[(ncol(EEsTN95_b)-1):ncol(EEsTN95_b)] = c('v102_Temp', 'v3_Temp')

EEsTNMed_b[,seq(ncol(EEsTNMed_b)+1,ncol(EEsTNMed_b)+2,1)] = AggregateEEs(EEs = EEsTNMed_b, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEsTNMed_b)[(ncol(EEsTNMed_b)-1):ncol(EEsTNMed_b)] = c('v102_Temp', 'v3_Temp')

EEs05_h[,seq(ncol(EEs05_h)+1,ncol(EEs05_h)+2,1)] = AggregateEEs(EEs = EEs05_h, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEs05_h)[(ncol(EEs05_h)-1):ncol(EEs05_h)] = c('v102_Temp', 'v3_Temp')

EEs95_h[,seq(ncol(EEs95_h)+1,ncol(EEs95_h)+2,1)] = AggregateEEs(EEs = EEs95_h, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEs95_h)[(ncol(EEs95_h)-1):ncol(EEs95_h)] = c('v102_Temp', 'v3_Temp')

EEsot_h[,seq(ncol(EEsot_h)+1,ncol(EEsot_h)+2,1)] = AggregateEEs(EEs = EEsot_h, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEsot_h)[(ncol(EEsot_h)-1):ncol(EEsot_h)] = c('v102_Temp', 'v3_Temp')

EEsTN05_h[,seq(ncol(EEsTN05_h)+1,ncol(EEsTN05_h)+2,1)] = AggregateEEs(EEs = EEsTN05_h, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEsTN05_h)[(ncol(EEsTN05_h)-1):ncol(EEsTN05_h)] = c('v102_Temp', 'v3_Temp')

EEsTN95_h[,seq(ncol(EEsTN95_h)+1,ncol(EEsTN95_h)+2,1)] = AggregateEEs(EEs = EEsTN95_h, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEsTN95_h)[(ncol(EEsTN95_h)-1):ncol(EEsTN95_h)] = c('v102_Temp', 'v3_Temp')

EEsTNMed_h[,seq(ncol(EEsTNMed_h)+1,ncol(EEsTNMed_h)+2,1)] = AggregateEEs(EEs = EEsTNMed_h, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEsTNMed_h)[(ncol(EEsTNMed_h)-1):ncol(EEsTNMed_h)] = c('v102_Temp', 'v3_Temp')

#  Leaf and Leaf Litter CN ratio----
# 'v102_epc.leaf_cn' 'v102_epc.leaflitr_cn'
# 'v3_epc.leaf_cn' 'v3_epc.leaflitr_cn'
EEs05_b[,seq(ncol(EEs05_b)+1,ncol(EEs05_b)+2,1)] = AggregateEEs(EEs = EEs05_b, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEs05_b)[(ncol(EEs05_b)-1):ncol(EEs05_b)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEs95_b[,seq(ncol(EEs95_b)+1,ncol(EEs95_b)+2,1)] = AggregateEEs(EEs = EEs95_b, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEs95_b)[(ncol(EEs95_b)-1):ncol(EEs95_b)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEsot_b[,seq(ncol(EEsot_b)+1,ncol(EEsot_b)+2,1)] = AggregateEEs(EEs = EEsot_b, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEsot_b)[(ncol(EEsot_b)-1):ncol(EEsot_b)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEsTN05_b[,seq(ncol(EEsTN05_b)+1,ncol(EEsTN05_b)+2,1)] = AggregateEEs(EEs = EEsTN05_b, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEsTN05_b)[(ncol(EEsTN05_b)-1):ncol(EEsTN05_b)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEsTN95_b[,seq(ncol(EEsTN95_b)+1,ncol(EEsTN95_b)+2,1)] = AggregateEEs(EEs = EEsTN95_b, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEsTN95_b)[(ncol(EEsTN95_b)-1):ncol(EEsTN95_b)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEsTNMed_b[,seq(ncol(EEsTNMed_b)+1,ncol(EEsTNMed_b)+2,1)] = AggregateEEs(EEs = EEsTNMed_b, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEsTNMed_b)[(ncol(EEsTNMed_b)-1):ncol(EEsTNMed_b)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEs05_h[,seq(ncol(EEs05_h)+1,ncol(EEs05_h)+2,1)] = AggregateEEs(EEs = EEs05_h, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEs05_h)[(ncol(EEs05_h)-1):ncol(EEs05_h)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEs95_h[,seq(ncol(EEs95_h)+1,ncol(EEs95_h)+2,1)] = AggregateEEs(EEs = EEs95_h, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEs95_h)[(ncol(EEs95_h)-1):ncol(EEs95_h)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEsot_h[,seq(ncol(EEsot_h)+1,ncol(EEsot_h)+2,1)] = AggregateEEs(EEs = EEsot_h, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEsot_h)[(ncol(EEsot_h)-1):ncol(EEsot_h)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEsTN05_h[,seq(ncol(EEsTN05_h)+1,ncol(EEsTN05_h)+2,1)] = AggregateEEs(EEs = EEsTN05_h, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEsTN05_h)[(ncol(EEsTN05_h)-1):ncol(EEsTN05_h)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEsTN95_h[,seq(ncol(EEsTN95_h)+1,ncol(EEsTN95_h)+2,1)] = AggregateEEs(EEs = EEsTN95_h, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEsTN95_h)[(ncol(EEsTN95_h)-1):ncol(EEsTN95_h)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEsTNMed_h[,seq(ncol(EEsTNMed_h)+1,ncol(EEsTNMed_h)+2,1)] = AggregateEEs(EEs = EEsTNMed_h, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEsTNMed_h)[(ncol(EEsTNMed_h)-1):ncol(EEsTNMed_h)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

# Compute bootstrapped samples of the mean absolute value with replacement of the EEs for each parameter----
#Each parameter is done separately as opposed to resampling by trajectory to avoid correlation in SDs of bootstrapped results
set.seed(12319)
#Number of bootstrapped samples to take
reps = 1000
#Matrix to store the computed mean absolute value of the elementary effects
EEs05_b_mua = matrix(NA, ncol = nrow(ParamRanges), nrow = reps)
EEs95_b_mua = matrix(NA, ncol = nrow(ParamRanges), nrow = reps)
EEsot_b_mua = matrix(NA, ncol = nrow(ParamRanges), nrow = reps)
EEsTN05_b_mua = matrix(NA, ncol = nrow(ParamRanges), nrow = reps)
EEsTN95_b_mua = matrix(NA, ncol = nrow(ParamRanges), nrow = reps)
EEsTNMed_b_mua = matrix(NA, ncol = nrow(ParamRanges), nrow = reps)
EEs05_h_mua = matrix(NA, ncol = nrow(ParamRanges)+1, nrow = reps*length(uhills))
EEs95_h_mua = matrix(NA, ncol = nrow(ParamRanges)+1, nrow = reps*length(uhills))
EEsot_h_mua = matrix(NA, ncol = nrow(ParamRanges)+1, nrow = reps*length(uhills))
EEsTN05_h_mua = matrix(NA, ncol = nrow(ParamRanges)+1, nrow = reps*length(uhills))
EEsTN95_h_mua = matrix(NA, ncol = nrow(ParamRanges)+1, nrow = reps*length(uhills))
EEsTNMed_h_mua = matrix(NA, ncol = nrow(ParamRanges)+1, nrow = reps*length(uhills))

#Array to store the matrices of the bootstrapped samples for constrained variables. These will have to be aggregated afterwards.
EEs05_b_mua_cons = array(NA, dim = c(reps,r,length(ColsAggregated)))
EEs95_b_mua_cons = array(NA, dim = c(reps,r,length(ColsAggregated)))
EEsot_b_mua_cons = array(NA, dim = c(reps,r,length(ColsAggregated)))
EEsTN05_b_mua_cons = array(NA, dim = c(reps,r,length(ColsAggregated)))
EEsTN95_b_mua_cons = array(NA, dim = c(reps,r,length(ColsAggregated)))
EEsTNMed_b_mua_cons = array(NA, dim = c(reps,r,length(ColsAggregated)))
EEs05_h_mua_cons = array(NA, dim = c(reps*length(uhills),r,length(ColsAggregated)+1))
EEs95_h_mua_cons = array(NA, dim = c(reps*length(uhills),r,length(ColsAggregated)+1))
EEsot_h_mua_cons = array(NA, dim = c(reps*length(uhills),r,length(ColsAggregated)+1))
EEsTN05_h_mua_cons = array(NA, dim = c(reps*length(uhills),r,length(ColsAggregated)+1))
EEsTN95_h_mua_cons = array(NA, dim = c(reps*length(uhills),r,length(ColsAggregated)+1))
EEsTNMed_h_mua_cons = array(NA, dim = c(reps*length(uhills),r,length(ColsAggregated)+1))

#Loop over all of the parameters
for (p in 1:(nrow(ParamRanges))){
  #Draw bootstrapped samples for that parameter
  tReps = t(replicate(expr = ceiling(runif(n = r, min = 0, max = r)), n = reps))
  #Extract the EEs for that parameter using the indices in Reps.
  EEs05_b_mua[,p] = apply(matrix(EEs05_b[tReps,p], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
  EEs95_b_mua[,p] = apply(matrix(EEs95_b[tReps,p], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
  EEsot_b_mua[,p] = apply(matrix(EEsot_b[tReps,p], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
  EEsTN05_b_mua[,p] = apply(matrix(EEsTN05_b[tReps,p], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
  EEsTN95_b_mua[,p] = apply(matrix(EEsTN95_b[tReps,p], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
  EEsTNMed_b_mua[,p] = apply(matrix(EEsTNMed_b[tReps,p], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
  for (h in 1:length(uhills)){
    indsh = seq(1+(uhills[h]-1)*reps,reps*uhills[h],1)
    EEs05_h_mua[indsh, p+1] = apply(matrix(EEs05_h[EEs05_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
    EEs05_h_mua[indsh, 1] = uhills[h]
    
    EEs95_h_mua[indsh, p+1] = apply(matrix(EEs95_h[EEs95_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
    EEs95_h_mua[indsh, 1] = uhills[h]
    
    EEsot_h_mua[indsh, p+1] = apply(matrix(EEsot_h[EEsot_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
    EEsot_h_mua[indsh, 1] = uhills[h]
    
    EEsTN05_h_mua[indsh, p+1] = apply(matrix(EEsTN05_h[EEsTN05_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
    EEsTN05_h_mua[indsh, 1] = uhills[h]
    
    EEsTN95_h_mua[indsh, p+1] = apply(matrix(EEsTN95_h[EEsTN95_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
    EEsTN95_h_mua[indsh, 1] = uhills[h]
    
    EEsTNMed_h_mua[indsh, p+1] = apply(matrix(EEsTNMed_h[EEsTNMed_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
    EEsTNMed_h_mua[indsh, 1] = uhills[h]
  }
  rm(indsh, h)
  #If the variable p is one of the ones that is used to aggregate, save the bootstrapped EEs array
  if (colnames(EEs05_b)[p] %in% ColsAggregated){
    indp = which(ColsAggregated == colnames(EEs05_b)[p])
    EEs05_b_mua_cons[ , , indp] = matrix(EEs05_b[tReps,p], ncol = r, nrow = reps)
    EEs95_b_mua_cons[ , , indp] = matrix(EEs95_b[tReps,p], ncol = r, nrow = reps)
    EEsot_b_mua_cons[ , , indp] = matrix(EEsot_b[tReps,p], ncol = r, nrow = reps)
    EEsTN05_b_mua_cons[ , , indp] = matrix(EEsTN05_b[tReps,p], ncol = r, nrow = reps)
    EEsTN95_b_mua_cons[ , , indp] = matrix(EEsTN95_b[tReps,p], ncol = r, nrow = reps)
    EEsTNMed_b_mua_cons[ , , indp] = matrix(EEsTNMed_b[tReps,p], ncol = r, nrow = reps)
    
    for (h in 1:length(uhills)){
      indsh = seq(1+(uhills[h]-1)*reps,reps*uhills[h],1)
      EEs05_h_mua_cons[indsh, , indp+1] = matrix(EEs05_h[EEs05_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps)
      EEs95_h_mua_cons[indsh, , indp+1] = matrix(EEs95_h[EEs95_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps)
      EEsot_h_mua_cons[indsh, , indp+1] = matrix(EEsot_h[EEsot_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps)
      EEsTN05_h_mua_cons[indsh, , indp+1] = matrix(EEsTN05_h[EEsTN05_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps)
      EEsTN95_h_mua_cons[indsh, , indp+1] = matrix(EEsTN95_h[EEsTN95_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps)
      EEsTNMed_h_mua_cons[indsh, , indp+1] = matrix(EEsTNMed_h[EEsTNMed_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps)
      
      EEs05_h_mua_cons[indsh, , 1] = uhills[h]
      EEs95_h_mua_cons[indsh, , 1] = uhills[h]
      EEsot_h_mua_cons[indsh, , 1] = uhills[h]
      EEsTN05_h_mua_cons[indsh, , 1] = uhills[h]
      EEsTN95_h_mua_cons[indsh, , 1] = uhills[h]
      EEsTNMed_h_mua_cons[indsh, , 1] = uhills[h]
    }
  }
}
rm(p, tReps)
#Assign column names
colnames(EEs05_b_mua) = colnames(EEs95_b_mua) = colnames(EEsot_b_mua) = colnames(EEsTN05_b_mua) = colnames(EEsTN95_b_mua) = colnames(EEsTNMed_b_mua) = colnames(EEs05_b)[1:nrow(ParamRanges)]
colnames(EEs05_h_mua) = colnames(EEs95_h_mua) = colnames(EEsot_h_mua) = colnames(EEsTN05_h_mua) = colnames(EEsTN95_h_mua) = colnames(EEsTNMed_h_mua) = colnames(EEs05_h)[1:(nrow(ParamRanges)+1)]
#Name the arrays
dimnames(EEs05_b_mua_cons) = dimnames(EEs95_b_mua_cons) = dimnames(EEsot_b_mua_cons) = dimnames(EEsTN05_b_mua_cons) = dimnames(EEsTN95_b_mua_cons) = dimnames(EEsTNMed_b_mua_cons) = list(Reps = seq(1,reps,1), Traj = seq(1,r,1), Cols = c(ColsAggregated))
dimnames(EEs05_h_mua_cons) = dimnames(EEs95_h_mua_cons) = dimnames(EEsot_h_mua_cons) = dimnames(EEsTN05_h_mua_cons) = dimnames(EEsTN95_h_mua_cons) = dimnames(EEsTNMed_h_mua_cons) = list(Reps = seq(1,reps*length(uhills),1), Traj = seq(1,r,1), Cols = c('HillID', ColsAggregated))

# Aggregate the variables that are constrained----
#  EEs05_b_mua----
EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's8_silt')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's8_sand')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 's8_SoilTexture'
EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's108_silt')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's108_sand')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 's108_SoilTexture'
EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's9_silt')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's9_sand')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 's9_SoilTexture'
EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's109_silt')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's109_sand')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 's109_SoilTexture'

#Fixme: these results are the same as this. Cannot make the ColNames have >1 row because the resulting matrix will have 40*rows to aggregate. 
#apply(apply(X = EEs05_b_mua_cons, MARGIN = 1, FUN = AggregateEEs, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), meanabs), 2, mean)
EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_K_absorptance')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_K_reflectance')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'v102_K_All'

EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_absorptance')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_reflectance')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'v102_PAR_All'

EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_fcel')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flab')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'v102_epc.frootlitr_All'
EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_fcel')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flab')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'v3_epc.frootlitr_All'

EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_fcel')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flab')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'v102_epc.leaflitr_All'
EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_fcel')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flab')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'v3_epc.leaflitr_All'

EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0_v')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'Soil8_Ksat'
EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0_v')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'Soil9_Ksat'

EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's108_m')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'Soil8_m'
EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's109_m')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'Soil9_m'

EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's108_porosity_0')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'Soil8_porosity_0'
EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's109_porosity_0')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'Soil9_porosity_0'

EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.topt')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'v102_Temp'
EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.topt')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'v3_Temp'

EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaf_cn')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'v102_epc.LeafLitrCN'
EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaf_cn')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'v3_epc.LeafLitrCN'

#  EEs95_b_mua----
EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's8_silt')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's8_sand')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 's8_SoilTexture'
EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's108_silt')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's108_sand')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 's108_SoilTexture'
EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's9_silt')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's9_sand')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 's9_SoilTexture'
EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's109_silt')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's109_sand')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 's109_SoilTexture'

#Fixme: these results are the same as this. Cannot make the ColNames have >1 row because the resulting matrix will have 40*rows to aggregate. 
#apply(apply(X = EEs95_b_mua_cons, MARGIN = 1, FUN = AggregateEEs, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), meanabs), 2, mean)
EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_K_absorptance')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_K_reflectance')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'v102_K_All'

EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_absorptance')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_reflectance')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'v102_PAR_All'

EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_fcel')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flab')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'v102_epc.frootlitr_All'
EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_fcel')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flab')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'v3_epc.frootlitr_All'

EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_fcel')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flab')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'v102_epc.leaflitr_All'
EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_fcel')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flab')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'v3_epc.leaflitr_All'

EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0_v')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'Soil8_Ksat'
EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0_v')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'Soil9_Ksat'

EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's108_m')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'Soil8_m'
EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's109_m')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'Soil9_m'

EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's108_porosity_0')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'Soil8_porosity_0'
EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's109_porosity_0')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'Soil9_porosity_0'

EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.topt')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'v102_Temp'
EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.topt')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'v3_Temp'

EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaf_cn')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'v102_epc.LeafLitrCN'
EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaf_cn')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'v3_epc.LeafLitrCN'

#  EEsot_b_mua----
EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's8_silt')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's8_sand')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 's8_SoilTexture'
EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's108_silt')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's108_sand')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 's108_SoilTexture'
EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's9_silt')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's9_sand')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 's9_SoilTexture'
EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's109_silt')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's109_sand')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 's109_SoilTexture'

#Fixme: these results are the same as this. Cannot make the ColNames have >1 row because the resulting matrix will have 40*rows to aggregate. 
#apply(apply(X = EEsot_b_mua_cons, MARGIN = 1, FUN = AggregateEEs, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), meanabs), 2, mean)
EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_K_absorptance')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_K_reflectance')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'v102_K_All'

EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_absorptance')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_reflectance')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'v102_PAR_All'

EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_fcel')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flab')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'v102_epc.frootlitr_All'
EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_fcel')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flab')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'v3_epc.frootlitr_All'

EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_fcel')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flab')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'v102_epc.leaflitr_All'
EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_fcel')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flab')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'v3_epc.leaflitr_All'

EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0_v')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'Soil8_Ksat'
EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0_v')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'Soil9_Ksat'

EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's108_m')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'Soil8_m'
EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's109_m')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'Soil9_m'

EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's108_porosity_0')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'Soil8_porosity_0'
EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's109_porosity_0')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'Soil9_porosity_0'

EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_epc.topt')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'v102_Temp'
EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v3_epc.topt')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'v3_Temp'

EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaf_cn')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'v102_epc.LeafLitrCN'
EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaf_cn')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'v3_epc.LeafLitrCN'

#  EEsTN05_b_mua----
EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's8_silt')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's8_sand')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 's8_SoilTexture'
EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's108_silt')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's108_sand')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 's108_SoilTexture'
EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's9_silt')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's9_sand')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 's9_SoilTexture'
EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's109_silt')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's109_sand')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 's109_SoilTexture'

#Fixme: these results are the same as this. Cannot make the ColNames have >1 row because the resulting matrix will have 40*rows to aggregate. 
#apply(apply(X = EEsTN05_b_mua_cons, MARGIN = 1, FUN = AggregateEEs, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), meanabs), 2, mean)
EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_K_absorptance')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_K_reflectance')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'v102_K_All'

EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_absorptance')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_reflectance')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'v102_PAR_All'

EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_fcel')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flab')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'v102_epc.frootlitr_All'
EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_fcel')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flab')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'v3_epc.frootlitr_All'

EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_fcel')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flab')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'v102_epc.leaflitr_All'
EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_fcel')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flab')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'v3_epc.leaflitr_All'

EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0_v')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'Soil8_Ksat'
EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0_v')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'Soil9_Ksat'

EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's108_m')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'Soil8_m'
EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's109_m')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'Soil9_m'

EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's108_porosity_0')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'Soil8_porosity_0'
EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's109_porosity_0')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'Soil9_porosity_0'

EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.topt')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'v102_Temp'
EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.topt')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'v3_Temp'

EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaf_cn')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'v102_epc.LeafLitrCN'
EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaf_cn')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'v3_epc.LeafLitrCN'

#  EEsTN95_b_mua----
EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's8_silt')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's8_sand')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 's8_SoilTexture'
EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's108_silt')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's108_sand')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 's108_SoilTexture'
EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's9_silt')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's9_sand')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 's9_SoilTexture'
EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's109_silt')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's109_sand')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 's109_SoilTexture'

#Fixme: these results are the same as this. Cannot make the ColNames have >1 row because the resulting matrix will have 40*rows to aggregate. 
#apply(apply(X = EEsTN95_b_mua_cons, MARGIN = 1, FUN = AggregateEEs, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), meanabs), 2, mean)
EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_K_absorptance')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_K_reflectance')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'v102_K_All'

EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_absorptance')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_reflectance')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'v102_PAR_All'

EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_fcel')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flab')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'v102_epc.frootlitr_All'
EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_fcel')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flab')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'v3_epc.frootlitr_All'

EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_fcel')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flab')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'v102_epc.leaflitr_All'
EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_fcel')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flab')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'v3_epc.leaflitr_All'

EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0_v')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'Soil8_Ksat'
EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0_v')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'Soil9_Ksat'

EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's108_m')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'Soil8_m'
EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's109_m')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'Soil9_m'

EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's108_porosity_0')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'Soil8_porosity_0'
EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's109_porosity_0')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'Soil9_porosity_0'

EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.topt')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'v102_Temp'
EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.topt')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'v3_Temp'

EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaf_cn')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'v102_epc.LeafLitrCN'
EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaf_cn')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'v3_epc.LeafLitrCN'

#  EEsTNMed_b_mua----
EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's8_silt')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's8_sand')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 's8_SoilTexture'
EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's108_silt')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's108_sand')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 's108_SoilTexture'
EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's9_silt')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's9_sand')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 's9_SoilTexture'
EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's109_silt')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's109_sand')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 's109_SoilTexture'

#Fixme: these results are the same as this. Cannot make the ColNames have >1 row because the resulting matrix will have 40*rows to aggregate. 
#apply(apply(X = EEsTNMed_b_mua_cons, MARGIN = 1, FUN = AggregateEEs, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), meanabs), 2, mean)
EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_K_absorptance')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_K_reflectance')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'v102_K_All'

EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_absorptance')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_reflectance')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'v102_PAR_All'

EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_fcel')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flab')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'v102_epc.frootlitr_All'
EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_fcel')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flab')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'v3_epc.frootlitr_All'

EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_fcel')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flab')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'v102_epc.leaflitr_All'
EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_fcel')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flab')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'v3_epc.leaflitr_All'

EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0_v')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'Soil8_Ksat'
EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0_v')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'Soil9_Ksat'

EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's108_m')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'Soil8_m'
EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's109_m')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'Soil9_m'

EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's108_porosity_0')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'Soil8_porosity_0'
EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's109_porosity_0')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'Soil9_porosity_0'

EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_epc.topt')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'v102_Temp'
EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v3_epc.topt')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'v3_Temp'

EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaf_cn')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'v102_epc.LeafLitrCN'
EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaf_cn')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'v3_epc.LeafLitrCN'

#  EEs05_h_mua----
EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's8_silt')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's8_sand')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 's8_SoilTexture'
EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's108_silt')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's108_sand')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 's108_SoilTexture'
EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's9_silt')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's9_sand')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 's9_SoilTexture'
EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's109_silt')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's109_sand')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 's109_SoilTexture'

#Fixme: these results are the same as this. Cannot make the ColNames have >1 row because the resulting matrix will have 40*rows to aggregate. 
#apply(apply(X = EEs05_h_mua_cons, MARGIN = 1, FUN = AggregateEEs, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), meanabs), 2, mean)
EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_K_absorptance')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_K_reflectance')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'v102_K_All'

EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_PAR_absorptance')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_PAR_reflectance')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'v102_PAR_All'

EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_epc.frootlitr_fcel')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_epc.frootlitr_flab')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'v102_epc.frootlitr_All'
EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v3_epc.frootlitr_fcel')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v3_epc.frootlitr_flab')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'v3_epc.frootlitr_All'

EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_epc.leaflitr_fcel')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_epc.leaflitr_flab')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'v102_epc.leaflitr_All'
EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v3_epc.leaflitr_fcel')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v3_epc.leaflitr_flab')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'v3_epc.leaflitr_All'

EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's108_Ksat_0')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's8_Ksat_0')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's108_Ksat_0_v')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'Soil8_Ksat'
EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's109_Ksat_0')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's9_Ksat_0')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's109_Ksat_0_v')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'Soil9_Ksat'

EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's108_m')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'Soil8_m'
EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's109_m')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'Soil9_m'

EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's108_porosity_0')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'Soil8_porosity_0'
EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's109_porosity_0')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'Soil9_porosity_0'

EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_epc.topt')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'v102_Temp'
EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v3_epc.topt')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'v3_Temp'

EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_epc.leaf_cn')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'v102_epc.LeafLitrCN'
EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v3_epc.leaf_cn')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'v3_epc.LeafLitrCN'

#  EEs95_h_mua----
EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's8_silt')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's8_sand')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 's8_SoilTexture'
EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's108_silt')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's108_sand')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 's108_SoilTexture'
EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's9_silt')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's9_sand')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 's9_SoilTexture'
EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's109_silt')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's109_sand')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 's109_SoilTexture'

#Fixme: these results are the same as this. Cannot make the ColNames have >1 row because the resulting matrix will have 40*rows to aggregate. 
#apply(apply(X = EEs95_h_mua_cons, MARGIN = 1, FUN = AggregateEEs, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), meanabs), 2, mean)
EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_K_absorptance')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_K_reflectance')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'v102_K_All'

EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_PAR_absorptance')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_PAR_reflectance')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'v102_PAR_All'

EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_epc.frootlitr_fcel')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_epc.frootlitr_flab')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'v102_epc.frootlitr_All'
EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v3_epc.frootlitr_fcel')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v3_epc.frootlitr_flab')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'v3_epc.frootlitr_All'

EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_epc.leaflitr_fcel')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_epc.leaflitr_flab')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'v102_epc.leaflitr_All'
EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v3_epc.leaflitr_fcel')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v3_epc.leaflitr_flab')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'v3_epc.leaflitr_All'

EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's108_Ksat_0')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's8_Ksat_0')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's108_Ksat_0_v')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'Soil8_Ksat'
EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's109_Ksat_0')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's9_Ksat_0')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's109_Ksat_0_v')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'Soil9_Ksat'

EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's108_m')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'Soil8_m'
EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's109_m')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'Soil9_m'

EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's108_porosity_0')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'Soil8_porosity_0'
EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's109_porosity_0')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'Soil9_porosity_0'

EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_epc.topt')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'v102_Temp'
EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v3_epc.topt')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'v3_Temp'

EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_epc.leaf_cn')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'v102_epc.LeafLitrCN'
EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v3_epc.leaf_cn')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'v3_epc.LeafLitrCN'
#  EEsot_h_mua----
EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's8_silt')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's8_sand')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 's8_SoilTexture'
EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's108_silt')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's108_sand')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 's108_SoilTexture'
EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's9_silt')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's9_sand')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 's9_SoilTexture'
EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's109_silt')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's109_sand')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 's109_SoilTexture'

#Fixme: these results are the same as this. Cannot make the ColNames have >1 row because the resulting matrix will have 40*rows to aggregate. 
#apply(apply(X = EEsot_h_mua_cons, MARGIN = 1, FUN = AggregateEEs, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), meanabs), 2, mean)
EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_K_absorptance')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_K_reflectance')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'v102_K_All'

EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_PAR_absorptance')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_PAR_reflectance')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'v102_PAR_All'

EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_epc.frootlitr_fcel')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_epc.frootlitr_flab')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'v102_epc.frootlitr_All'
EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v3_epc.frootlitr_fcel')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v3_epc.frootlitr_flab')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'v3_epc.frootlitr_All'

EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_epc.leaflitr_fcel')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_epc.leaflitr_flab')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'v102_epc.leaflitr_All'
EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v3_epc.leaflitr_fcel')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v3_epc.leaflitr_flab')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'v3_epc.leaflitr_All'

EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's108_Ksat_0')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's8_Ksat_0')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's108_Ksat_0_v')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'Soil8_Ksat'
EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's109_Ksat_0')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's9_Ksat_0')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's109_Ksat_0_v')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'Soil9_Ksat'

EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's108_m')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'Soil8_m'
EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's109_m')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'Soil9_m'

EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's108_porosity_0')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'Soil8_porosity_0'
EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's109_porosity_0')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'Soil9_porosity_0'

EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_epc.topt')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'v102_Temp'
EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v3_epc.topt')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'v3_Temp'

EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_epc.leaf_cn')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'v102_epc.LeafLitrCN'
EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v3_epc.leaf_cn')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'v3_epc.LeafLitrCN'
#  EEsTN05_h_mua----
EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's8_silt')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's8_sand')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 's8_SoilTexture'
EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's108_silt')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's108_sand')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 's108_SoilTexture'
EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's9_silt')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's9_sand')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 's9_SoilTexture'
EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's109_silt')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's109_sand')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 's109_SoilTexture'

#Fixme: these results are the same as this. Cannot make the ColNames have >1 row because the resulting matrix will have 40*rows to aggregate. 
#apply(apply(X = EEsTN05_h_mua_cons, MARGIN = 1, FUN = AggregateEEs, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), meanabs), 2, mean)
EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_K_absorptance')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_K_reflectance')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'v102_K_All'

EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_PAR_absorptance')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_PAR_reflectance')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'v102_PAR_All'

EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_epc.frootlitr_fcel')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_epc.frootlitr_flab')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'v102_epc.frootlitr_All'
EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v3_epc.frootlitr_fcel')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v3_epc.frootlitr_flab')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'v3_epc.frootlitr_All'

EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_epc.leaflitr_fcel')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_epc.leaflitr_flab')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'v102_epc.leaflitr_All'
EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v3_epc.leaflitr_fcel')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v3_epc.leaflitr_flab')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'v3_epc.leaflitr_All'

EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's108_Ksat_0')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's8_Ksat_0')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's108_Ksat_0_v')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'Soil8_Ksat'
EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's109_Ksat_0')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's9_Ksat_0')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's109_Ksat_0_v')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'Soil9_Ksat'

EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's108_m')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'Soil8_m'
EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's109_m')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'Soil9_m'

EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's108_porosity_0')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'Soil8_porosity_0'
EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's109_porosity_0')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'Soil9_porosity_0'

EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_epc.topt')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'v102_Temp'
EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v3_epc.topt')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'v3_Temp'

EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_epc.leaf_cn')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'v102_epc.LeafLitrCN'
EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v3_epc.leaf_cn')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'v3_epc.LeafLitrCN'
#  EEsTN95_h_mua----
EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's8_silt')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's8_sand')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 's8_SoilTexture'
EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's108_silt')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's108_sand')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 's108_SoilTexture'
EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's9_silt')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's9_sand')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 's9_SoilTexture'
EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's109_silt')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's109_sand')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 's109_SoilTexture'

#Fixme: these results are the same as this. Cannot make the ColNames have >1 row because the resulting matrix will have 40*rows to aggregate. 
#apply(apply(X = EEsTN95_h_mua_cons, MARGIN = 1, FUN = AggregateEEs, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), meanabs), 2, mean)
EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_K_absorptance')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_K_reflectance')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'v102_K_All'

EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_PAR_absorptance')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_PAR_reflectance')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'v102_PAR_All'

EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_epc.frootlitr_fcel')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_epc.frootlitr_flab')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'v102_epc.frootlitr_All'
EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v3_epc.frootlitr_fcel')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v3_epc.frootlitr_flab')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'v3_epc.frootlitr_All'

EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_epc.leaflitr_fcel')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_epc.leaflitr_flab')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'v102_epc.leaflitr_All'
EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v3_epc.leaflitr_fcel')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v3_epc.leaflitr_flab')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'v3_epc.leaflitr_All'

EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's108_Ksat_0')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's8_Ksat_0')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's108_Ksat_0_v')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'Soil8_Ksat'
EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's109_Ksat_0')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's9_Ksat_0')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's109_Ksat_0_v')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'Soil9_Ksat'

EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's108_m')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'Soil8_m'
EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's109_m')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'Soil9_m'

EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's108_porosity_0')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'Soil8_porosity_0'
EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's109_porosity_0')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'Soil9_porosity_0'

EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_epc.topt')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'v102_Temp'
EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v3_epc.topt')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'v3_Temp'

EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_epc.leaf_cn')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'v102_epc.LeafLitrCN'
EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v3_epc.leaf_cn')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'v3_epc.LeafLitrCN'
#  EEsTNMed_h_mua----
EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's8_silt')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's8_sand')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 's8_SoilTexture'
EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's108_silt')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's108_sand')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 's108_SoilTexture'
EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's9_silt')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's9_sand')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 's9_SoilTexture'
EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's109_silt')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's109_sand')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 's109_SoilTexture'

#Fixme: these results are the same as this. Cannot make the ColNames have >1 row because the resulting matrix will have 40*rows to aggregate. 
#apply(apply(X = EEsTNMed_h_mua_cons, MARGIN = 1, FUN = AggregateEEs, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), meanabs), 2, mean)
EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_K_absorptance')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_K_reflectance')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'v102_K_All'

EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_PAR_absorptance')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_PAR_reflectance')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'v102_PAR_All'

EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_epc.frootlitr_fcel')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_epc.frootlitr_flab')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'v102_epc.frootlitr_All'
EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v3_epc.frootlitr_fcel')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v3_epc.frootlitr_flab')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'v3_epc.frootlitr_All'

EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_epc.leaflitr_fcel')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_epc.leaflitr_flab')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'v102_epc.leaflitr_All'
EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v3_epc.leaflitr_fcel')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v3_epc.leaflitr_flab')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'v3_epc.leaflitr_All'

EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's108_Ksat_0')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's8_Ksat_0')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's108_Ksat_0_v')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'Soil8_Ksat'
EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's109_Ksat_0')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's9_Ksat_0')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's109_Ksat_0_v')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'Soil9_Ksat'

EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's108_m')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'Soil8_m'
EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's109_m')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'Soil9_m'

EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's108_porosity_0')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'Soil8_porosity_0'
EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's109_porosity_0')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'Soil9_porosity_0'

EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_epc.topt')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'v102_Temp'
EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v3_epc.topt')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'v3_Temp'

EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_epc.leaf_cn')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'v102_epc.LeafLitrCN'
EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v3_epc.leaf_cn')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'v3_epc.LeafLitrCN'

# Compute the mean, 5th, and 95th quantiles of the EE distributions----
EEs05_b_mua_m = apply(X = EEs05_b_mua, MARGIN = 2, FUN = mean)
EEs05_b_mua_05 = apply(X = EEs05_b_mua, MARGIN = 2, FUN = quantile, probs = 0.05)
EEs05_b_mua_95 = apply(X = EEs05_b_mua, MARGIN = 2, FUN = quantile, probs = 0.95)

EEs95_b_mua_m = apply(X = EEs95_b_mua, MARGIN = 2, FUN = mean)
EEs95_b_mua_05 = apply(X = EEs95_b_mua, MARGIN = 2, FUN = quantile, probs = 0.05)
EEs95_b_mua_95 = apply(X = EEs95_b_mua, MARGIN = 2, FUN = quantile, probs = 0.95)

EEsot_b_mua_m = apply(X = EEsot_b_mua, MARGIN = 2, FUN = mean)
EEsot_b_mua_05 = apply(X = EEsot_b_mua, MARGIN = 2, FUN = quantile, probs = 0.05)
EEsot_b_mua_95 = apply(X = EEsot_b_mua, MARGIN = 2, FUN = quantile, probs = 0.95)

EEsTN05_b_mua_m = apply(X = EEsTN05_b_mua, MARGIN = 2, FUN = mean)
EEsTN05_b_mua_05 = apply(X = EEsTN05_b_mua, MARGIN = 2, FUN = quantile, probs = 0.05)
EEsTN05_b_mua_95 = apply(X = EEsTN05_b_mua, MARGIN = 2, FUN = quantile, probs = 0.95)

EEsTN95_b_mua_m = apply(X = EEsTN95_b_mua, MARGIN = 2, FUN = mean)
EEsTN95_b_mua_05 = apply(X = EEsTN95_b_mua, MARGIN = 2, FUN = quantile, probs = 0.05)
EEsTN95_b_mua_95 = apply(X = EEsTN95_b_mua, MARGIN = 2, FUN = quantile, probs = 0.95)

EEsTNMed_b_mua_m = apply(X = EEsTNMed_b_mua, MARGIN = 2, FUN = mean)
EEsTNMed_b_mua_05 = apply(X = EEsTNMed_b_mua, MARGIN = 2, FUN = quantile, probs = 0.05)
EEsTNMed_b_mua_95 = apply(X = EEsTNMed_b_mua, MARGIN = 2, FUN = quantile, probs = 0.95)

#Loop over hillslopes
EEs05_h_mua_m = EEs95_h_mua_m = EEsot_h_mua_m = EEsTN05_h_mua_m = EEsTN95_h_mua_m = EEsTNMed_h_mua_m = matrix(NA, nrow = length(uhills), ncol = length(EEs05_b_mua_05)+1)
EEs05_h_mua_05 = EEs95_h_mua_05 = EEsot_h_mua_05 = EEsTN05_h_mua_05 = EEsTN95_h_mua_05 = EEsTNMed_h_mua_05 = matrix(NA, nrow = length(uhills), ncol = length(EEs05_b_mua_05)+1)
EEs05_h_mua_95 = EEs95_h_mua_95 = EEsot_h_mua_95 = EEsTN05_h_mua_95 = EEsTN95_h_mua_95 = EEsTNMed_h_mua_95 = matrix(NA, nrow = length(uhills), ncol = length(EEs05_b_mua_05)+1)

for (h in 1:length(uhills)){
  EEs05_h_mua_m[uhills[h], ] = apply(X = EEs05_h_mua[EEs05_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  EEs05_h_mua_05[uhills[h], ] = apply(X = EEs05_h_mua[EEs05_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  EEs05_h_mua_95[uhills[h], ] = apply(X = EEs05_h_mua[EEs05_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  
  EEs95_h_mua_m[uhills[h], ] = apply(X = EEs95_h_mua[EEs95_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  EEs95_h_mua_05[uhills[h], ] = apply(X = EEs95_h_mua[EEs95_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  EEs95_h_mua_95[uhills[h], ] = apply(X = EEs95_h_mua[EEs95_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  
  EEsot_h_mua_m[uhills[h], ] = apply(X = EEsot_h_mua[EEsot_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  EEsot_h_mua_05[uhills[h], ] = apply(X = EEsot_h_mua[EEsot_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  EEsot_h_mua_95[uhills[h], ] = apply(X = EEsot_h_mua[EEsot_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  
  EEsTN05_h_mua_m[uhills[h], ] = apply(X = EEsTN05_h_mua[EEsTN05_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  EEsTN05_h_mua_05[uhills[h], ] = apply(X = EEsTN05_h_mua[EEsTN05_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  EEsTN05_h_mua_95[uhills[h], ] = apply(X = EEsTN05_h_mua[EEsTN05_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  
  EEsTN95_h_mua_m[uhills[h], ] = apply(X = EEsTN95_h_mua[EEsTN95_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  EEsTN95_h_mua_05[uhills[h], ] = apply(X = EEsTN95_h_mua[EEsTN95_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  EEsTN95_h_mua_95[uhills[h], ] = apply(X = EEsTN95_h_mua[EEsTN95_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  
  EEsTNMed_h_mua_m[uhills[h], ] = apply(X = EEsTNMed_h_mua[EEsTNMed_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  EEsTNMed_h_mua_05[uhills[h], ] = apply(X = EEsTNMed_h_mua[EEsTNMed_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  EEsTNMed_h_mua_95[uhills[h], ] = apply(X = EEsTNMed_h_mua[EEsTNMed_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
}

# Make plots of the distributions for variables----
for (p in 1:ncol(EEs05_b_mua)){
  png(paste0('EEs05_b_mua_', colnames(EEs05_b_mua)[p], '.png'), res = 300, units = 'in', width = 5, height = 5)
  hist(EEs05_b_mua[,p], breaks = 50, main = colnames(EEs05_b_mua)[p], xlab = 'Elementary Effect', cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
  dev.off()
}
rm(p)

#Compute the mean, standard deviation, and absolute mean of the EEs from the original sample----
muEEs05_b = apply(X = EEs05_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = mean)
muEEs95_b = apply(X = EEs95_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = mean)
muEEsot_b = apply(X = EEsot_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = mean)
muEEsTN05_b = apply(X = EEsTN05_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = mean)
muEEsTN95_b = apply(X = EEsTN95_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = mean)
muEEsTNMed_b = apply(X = EEsTNMed_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = mean)

sdEEs05_b = apply(X = EEs05_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = sd)
sdEEs95_b = apply(X = EEs95_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = sd)
sdEEsot_b = apply(X = EEsot_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = sd)
sdEEsTN05_b = apply(X = EEsTN05_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = sd)
sdEEsTN95_b = apply(X = EEsTN95_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = sd)
sdEEsTNMed_b = apply(X = EEsTNMed_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = sd)

muaEEs05_b = apply(X = abs(EEs05_b[,1:nrow(ParamRanges)]), MARGIN = 2, FUN = mean)
muaEEs95_b = apply(X = abs(EEs95_b[,1:nrow(ParamRanges)]), MARGIN = 2, FUN = mean)
muaEEsot_b = apply(X = abs(EEsot_b[,1:nrow(ParamRanges)]), MARGIN = 2, FUN = mean)
muaEEsTN05_b = apply(X = abs(EEsTN05_b[,1:nrow(ParamRanges)]), MARGIN = 2, FUN = mean)
muaEEsTN95_b = apply(X = abs(EEsTN95_b[,1:nrow(ParamRanges)]), MARGIN = 2, FUN = mean)
muaEEsTNMed_b = apply(X = abs(EEsTNMed_b[,1:nrow(ParamRanges)]), MARGIN = 2, FUN = mean)

#Hillslopes will have one per hillslope
muEEs05_h = muEEs95_h = muEEsot_h = muEEsTN05_h = muEEsTN95_h = muEEsTNMed_h = sdEEs05_h = sdEEs95_h = sdEEsot_h = sdEEsTN05_h = sdEEsTN95_h = sdEEsTNMed_h = muaEEs05_h = muaEEs95_h = muaEEsot_h = muaEEsTN05_h = muaEEsTN95_h = muaEEsTNMed_h = matrix(NA, nrow = length(uhills), ncol = as.numeric(cols))
for (h in 1:length(uhills)){
  inds = seq(h, r*length(uhills)-(length(uhills)-h), length(uhills))
  muEEs05_h[h,] = apply(X = EEs05_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = mean)
  muEEs95_h[h,] = apply(X = EEs95_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = mean)
  muEEsot_h[h,] = apply(X = EEsot_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = mean)
  muEEsTN05_h[h,] = apply(X = EEsTN05_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = mean)
  muEEsTN95_h[h,] = apply(X = EEsTN95_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = mean)
  muEEsTNMed_h[h,] = apply(X = EEsTNMed_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = mean)
  
  sdEEs05_h[h,] = apply(X = EEs05_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = sd)
  sdEEs95_h[h,] = apply(X = EEs95_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = sd)
  sdEEsot_h[h,] = apply(X = EEsot_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = sd)
  sdEEsTN05_h[h,] = apply(X = EEsTN05_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = sd)
  sdEEsTN95_h[h,] = apply(X = EEsTN95_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = sd)
  sdEEsTNMed_h[h,] = apply(X = EEsTNMed_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = sd)
  
  muaEEs05_h[h,] = apply(X = abs(EEs05_h[inds,2:(nrow(ParamRanges)+1)]), MARGIN = 2, FUN = mean)
  muaEEs95_h[h,] = apply(X = abs(EEs95_h[inds,2:(nrow(ParamRanges)+1)]), MARGIN = 2, FUN = mean)
  muaEEsot_h[h,] = apply(X = abs(EEsot_h[inds,2:(nrow(ParamRanges)+1)]), MARGIN = 2, FUN = mean)
  muaEEsTN05_h[h,] = apply(X = abs(EEsTN05_h[inds,2:(nrow(ParamRanges)+1)]), MARGIN = 2, FUN = mean)
  muaEEsTN95_h[h,] = apply(X = abs(EEsTN95_h[inds,2:(nrow(ParamRanges)+1)]), MARGIN = 2, FUN = mean)
  muaEEsTNMed_h[h,] = apply(X = abs(EEsTNMed_h[inds,2:(nrow(ParamRanges)+1)]), MARGIN = 2, FUN = mean)
}
rm(inds)

#Make a list of ordered parameter names for mua using original sample----
RanksMua05_b = data.frame(Param = colnames(InputParams)[rev(order(muaEEs05_b))], EE05_b = muaEEs05_b[rev(order(muaEEs05_b))], stringsAsFactors = FALSE)
RanksMua95_b = data.frame(Param = colnames(InputParams)[rev(order(muaEEs95_b))], EE95_b = muaEEs95_b[rev(order(muaEEs95_b))], stringsAsFactors = FALSE)
RanksMuaot_b = data.frame(Param = colnames(InputParams)[rev(order(muaEEsot_b))], EEot_b = muaEEsot_b[rev(order(muaEEsot_b))], stringsAsFactors = FALSE)
RanksMuaTN05_b = data.frame(Param = colnames(InputParams)[rev(order(muaEEsTN05_b))], EETN05_b = muaEEsTN05_b[rev(order(muaEEsTN05_b))], stringsAsFactors = FALSE)
RanksMuaTN95_b = data.frame(Param = colnames(InputParams)[rev(order(muaEEsTN95_b))], EETN95_b = muaEEsTN95_b[rev(order(muaEEsTN95_b))], stringsAsFactors = FALSE)
RanksMuaTNMed_b = data.frame(Param = colnames(InputParams)[rev(order(muaEEsTNMed_b))], EETNMed_b = muaEEsTNMed_b[rev(order(muaEEsTNMed_b))], stringsAsFactors = FALSE)

RanksMua05_h = RanksMua95_h = RanksMuaot_h = RanksMuaTN05_h = RanksMuaTN95_h = RanksMuaTNMed_h = list()
for (h in 1:length(uhills)){
  RanksMua05_h = c(RanksMua05_h, list(data.frame(Param = colnames(InputParams)[rev(order(muaEEs05_h[h,]))], EE05_h = muaEEs05_h[h,][rev(order(muaEEs05_h[h,]))], stringsAsFactors = FALSE)))
  RanksMua95_h = c(RanksMua95_h, list(data.frame(Param = colnames(InputParams)[rev(order(muaEEs95_h[h,]))], EE05_h = muaEEs95_h[h,][rev(order(muaEEs95_h[h,]))], stringsAsFactors = FALSE)))
  RanksMuaot_h = c(RanksMuaot_h, list(data.frame(Param = colnames(InputParams)[rev(order(muaEEsot_h[h,]))], EE05_h = muaEEsot_h[h,][rev(order(muaEEsot_h[h,]))], stringsAsFactors = FALSE)))
  RanksMuaTN05_h = c(RanksMuaTN05_h, list(data.frame(Param = colnames(InputParams)[rev(order(muaEEsTN05_h[h,]))], EE05_h = muaEEsTN05_h[h,][rev(order(muaEEsTN05_h[h,]))], stringsAsFactors = FALSE)))
  RanksMuaTN95_h = c(RanksMuaTN95_h, list(data.frame(Param = colnames(InputParams)[rev(order(muaEEsTN95_h[h,]))], EE05_h = muaEEsTN95_h[h,][rev(order(muaEEsTN95_h[h,]))], stringsAsFactors = FALSE)))
  RanksMuaTNMed_h = c(RanksMuaTNMed_h, list(data.frame(Param = colnames(InputParams)[rev(order(muaEEsTNMed_h[h,]))], EE05_h = muaEEsTNMed_h[h,][rev(order(muaEEsTNMed_h[h,]))], stringsAsFactors = FALSE)))
}
names(RanksMua05_h) = names(RanksMua95_h) = names(RanksMuaot_h) = names(RanksMuaTN05_h) = names(RanksMuaTN95_h) = names(RanksMuaTNMed_h) = paste0('Hill', seq(1,14,1))
rm(h)

# Get the unique variables from the flow and TN metric lists----
#Top 10% of the parameters is desired. Get the index of the 10% of non-zero EEs, rounded up
Top10 = ceiling((length(muaEEs05_b) - length(which(muaEEs05_b == 0)))*0.1)
RanksMua_b = unique(c(RanksMua05_b$Param[1:Top10],RanksMuaot_b$Param[1:Top10],RanksMua95_b$Param[1:Top10]))
RanksMuaTN_b = unique(c(RanksMuaTN05_b$Param[1:Top10],RanksMuaTNMed_b$Param[1:Top10],RanksMuaTN95_b$Param[1:Top10]))

#Unique across basin and TN are same for top 10%
length(unique(c(RanksMua_b, RanksMuaTN_b)))

#RanksMua_h = unique(c(RanksMua05_b$Param[1:40],RanksMuaot_b$Param[1:40],RanksMua95_b$Param[1:40]))
#RanksMuaTN_h = unique(c(RanksMuaTN05_b$Param[1:40],RanksMuaTNMed_b$Param[1:40],RanksMuaTN95_b$Param[1:40]))

# Select all parameters whose 95th quantile estimate of EE is greater than the selected threshold point
ParamSelect_b = unique(c(names(EEs05_b_mua_95[1:nrow(ParamRanges)][EEs05_b_mua_95[1:nrow(ParamRanges)] >= RanksMua05_b$EE05_b[Top10]]),
                         names(EEsot_b_mua_95[1:nrow(ParamRanges)][EEsot_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaot_b$EEot_b[Top10]]),
                         names(EEs95_b_mua_95[1:nrow(ParamRanges)][EEs95_b_mua_95[1:nrow(ParamRanges)] >= RanksMua95_b$EE95_b[Top10]])))
ParamSelectTN_b = unique(c(names(EEsTN05_b_mua_95[1:nrow(ParamRanges)][EEsTN05_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaTN05_b$EETN05_b[Top10]]),
                         names(EEsTNMed_b_mua_95[1:nrow(ParamRanges)][EEsTNMed_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaTNMed_b$EETNMed_b[Top10]]),
                         names(EEsTN95_b_mua_95[1:nrow(ParamRanges)][EEsTN95_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaTN95_b$EETN95_b[Top10]])))

#Some differences using this method
length(unique(c(ParamSelect_b, ParamSelectTN_b)))

# Same, but dropping the aggregated parameters
Top10_Agg = ceiling((length(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*0.1)
RanksMua_b_Agg = unique(c(names(sort(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[1:Top10_Agg]),
                          names(sort(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[1:Top10_Agg]),
                          names(sort(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[1:Top10_Agg])))
RanksMuaTN_b_Agg = unique(c(names(sort(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[1:Top10_Agg]),
                            names(sort(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[1:Top10_Agg]),
                            names(sort(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[1:Top10_Agg])))

# Select all parameters whose 95th quantile estimate of EE is greater than the selected threshold point
ParamSelect_b_Agg = unique(c(names(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] >= sort(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10_Agg]]),
                         names(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)] >= sort(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10_Agg]]),
                         names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)] >= sort(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10_Agg]])))
ParamSelectTN_b_Agg = unique(c(names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)] >= sort(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10_Agg]]),
                           names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)] >= sort(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10_Agg]]),
                           names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)] >= sort(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10_Agg]])))

length(unique(c(ParamSelect_b_Agg, ParamSelectTN_b_Agg)))

#Make a plot of the number of parameters selected versus the threshold percentage----
x = seq(0.01,1,0.01)
ParamTotals = vector('numeric', length(x))
for (i in 1:length(x)){
  Tops = ceiling((length(muaEEs05_b) - length(which(muaEEs05_b == 0)))*x[i])
  # Select all parameters whose 95th quantile estimate of EE is greater than the selected threshold point
  pb = unique(c(names(EEs05_b_mua_95[1:nrow(ParamRanges)][EEs05_b_mua_95[1:nrow(ParamRanges)] >= RanksMua05_b$EE05_b[Tops]]),
                           names(EEsot_b_mua_95[1:nrow(ParamRanges)][EEsot_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaot_b$EEot_b[Tops]]),
                           names(EEs95_b_mua_95[1:nrow(ParamRanges)][EEs95_b_mua_95[1:nrow(ParamRanges)] >= RanksMua95_b$EE95_b[Tops]])))
  pbTN = unique(c(names(EEsTN05_b_mua_95[1:nrow(ParamRanges)][EEsTN05_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaTN05_b$EETN05_b[Tops]]),
                             names(EEsTNMed_b_mua_95[1:nrow(ParamRanges)][EEsTNMed_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaTNMed_b$EETNMed_b[Tops]]),
                             names(EEsTN95_b_mua_95[1:nrow(ParamRanges)][EEsTN95_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaTN95_b$EETN95_b[Tops]])))
  
  ParamTotals[i] = length(unique(c(pb, pbTN)))
}
rm(pb, pbTN, Tops, i, x)

png('ParamsInThresholdCutoff.png', res = 300, units = 'in', width = 5, height = 5)
plot(x = seq(1,100,1), y = ParamTotals, type = 'l', xlab = 'Top X Percent Selected', ylab = 'Number of Parameters Selected', ylim = c(0,120), xlim = c(0,100))
par(new=TRUE)
plot(x = seq(1,100,1), y = ceiling((length(muaEEs05_b) - length(which(muaEEs05_b == 0)))*seq(0.01,1,0.01)), type = 'l', ylim = c(0,120), xlim = c(0,100), col = 'red', xlab = '', ylab = '', axes = FALSE)
legend('topleft', legend = c('Based on Mean', 'Based on 95th Percentile'), lty = 1, col = c('red', 'black'))
dev.off()

x = seq(0.01,1,0.01)
ParamTotals = vector('numeric', length(x))
for (i in 1:length(x)){
  Tops = ceiling((length(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  
  # Select all parameters whose 95th quantile estimate of EE is greater than the selected threshold point
  pb = unique(c(names(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] >= sort(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Tops]]),
                               names(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)] >= sort(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Tops]]),
                               names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)] >= sort(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Tops]])))
  pbTN = unique(c(names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)] >= sort(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Tops]]),
                                 names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)] >= sort(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Tops]]),
                                 names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)] >= sort(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Tops]])))
  
  ParamTotals[i] = length(unique(c(pb, pbTN)))
}
rm(pb, pbTN, Tops, i, x)

png('ParamsInThresholdCutoff_Agg.png', res = 300, units = 'in', width = 5, height = 5)
plot(x = seq(1,100,1), y = ParamTotals, type = 'l', xlab = 'Top X Percent Selected', ylab = 'Number of Parameters Selected', ylim = c(0,120), xlim = c(0,100))
par(new=TRUE)
plot(x = seq(1,100,1), y = ceiling((length(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*seq(0.01,1,0.01)), type = 'l', ylim = c(0,120), xlim = c(0,100), col = 'red', xlab = '', ylab = '', axes = FALSE)
legend('topleft', legend = c('Based on Mean', 'Based on 95th Percentile'), lty = 1, col = c('red', 'black'))
dev.off()

#Color scheme for Categories of Variables----
#One color per category
colos = c(rainbow(12), 'black')
colos[3] = 'gray'
#Assign colors to the categories
ColPlots = vector('character', length=cols)
for (i in 1:cols){
  if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 'h'){
    ColPlots[i] = colos[1]
  }else if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 'z'){
    ColPlots[i] = colos[2]
  }else if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 's9'){
    ColPlots[i] = colos[3]
  }else if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 's109'){
    ColPlots[i] = colos[4]
  }else if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 's8'){
    ColPlots[i] = colos[5]
  }else if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 's108'){
    ColPlots[i] = colos[6]
  }else if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 'l1'){
    ColPlots[i] = colos[7]
  }else if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 'l2'){
    ColPlots[i] = colos[8]
  }else if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 'l3'){
    ColPlots[i] = colos[9]
  }else if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 'l4'){
    ColPlots[i] = colos[10]
  }else if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 'v102'){
    ColPlots[i] = colos[11]
  }else if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 'v3'){
    ColPlots[i] = colos[12]
  }else if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 'v4'){
    ColPlots[i] = colos[13]
  }
}
#Make plots of the sd vs. mu----
png('EE05_sdVsMu_b.png', res = 300, units = 'in', height = 7, width = 7)
plot(x = muEEs05_b, y = sdEEs05_b, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SAE for Lower 5th Percentile of Flow', col = ColPlots)
legend('bottomleft', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos)
dev.off()
png('EE95_sdVsMu_b.png', res = 300, units = 'in', height = 7, width = 7)
plot(x = muEEs95_b, y = sdEEs95_b, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SAE for Upper 5th Percentile of Flow', col = ColPlots)
legend('topleft', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos)
dev.off()
png('EEot_sdVsMu_b.png', res = 300, units = 'in', height = 7, width = 7)
plot(x = muEEsot_b, y = sdEEsot_b, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SAE for 5th-95th Percentile Flows', col = ColPlots, xlim = c(-3000,3000))
legend('bottomleft', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos)
dev.off()
png('EETN05_sdVsMu_b.png', res = 300, units = 'in', height = 7, width = 7)
plot(x = muEEsTN05_b, y = sdEEsTN05_b, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SAE for Lower 5th Quantile of TN', col = ColPlots, xlim=c(-40,40))
legend('bottomleft', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos)
dev.off()
png('EETN95_sdVsMu_b.png', res = 300, units = 'in', height = 7, width = 7)
plot(x = muEEsTN95_b, y = sdEEsTN95_b, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SAE for Upper 5th Quantile of TN', col = ColPlots, xlim = c(-15,15))
legend('bottomright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos)
dev.off()
png('EETNMed_sdVsMu_b.png', res = 300, units = 'in', height = 7, width = 7)
plot(x = muEEsTNMed_b, y = sdEEsTNMed_b, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SAE for Mean of TN', col = ColPlots, xlim = c(-30,30))
legend('bottomleft', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos)
dev.off()

#Make plots of the ranking for mean absolute value----
png('EE05_mua_b.png', res = 300, units = 'in', height = 7, width = 7)
barplot(height = EEs05_b_mua_m[1:nrow(ParamRanges)]/max(EEs05_b_mua_95[1:nrow(ParamRanges)]), ylab = 'Normalized Mean Abs. Val. of Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Lower 5th Percentile of Flow', col = ColPlots, border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,300))
arrows(seq(0.5,270.5,1), EEs05_b_mua_05[1:nrow(ParamRanges)]/max(EEs05_b_mua_95[1:nrow(ParamRanges)]), seq(0.5,270.5,1), EEs05_b_mua_95[1:nrow(ParamRanges)]/max(EEs05_b_mua_95[1:nrow(ParamRanges)]), length=0.05, angle=90, code=3)
#Make lines separating the different classes of variables
lines(x = c(-100,300), y = c(RanksMua05_b[Top10,2]/max(EEs05_b_mua_95[1:nrow(ParamRanges)]),RanksMua05_b[Top10,2]/max(EEs05_b_mua_95[1:nrow(ParamRanges)])), col = 'black', lwd = 2)
legend('topright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos, cex = 1.3)
dev.off()

png('EE05_mua_b_thresh12.png', res = 300, units = 'in', height = 7, width = 7)
barplot(height = muaEEs05_b/max(muaEEs05_b), ylab = 'Normalized Mean Abs. Val. of Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Lower 5th Percentile of Flow', col = ColPlots, border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
#Make lines separating the different classes of variables
lines(x = c(-100,300), y = c(RanksMua05_b[Top10,2]/max(muaEEs05_b),RanksMua05_b[Top10,2]/max(muaEEs05_b)))
legend('topright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos, cex = 1.3)
dev.off()

png('EE05_mua_b_lines.png', res = 300, units = 'in', height = 7, width = 7)
barplot(height = muaEEs05_b, ylab = 'Mean Absolute Value of the Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Lower 5th Percentile of Flow', col = ColPlots, border = NA, ylim = c(0,120), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
#Make lines separating the different classes of variables
lines(x = c(-1,-1), y = c(-10,130), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^h_')[length(grep(colnames(InputParams), pattern = '^h_'))], 0.5+grep(colnames(InputParams), pattern = '^h_')[length(grep(colnames(InputParams), pattern = '^h_'))]), y = c(-10,130), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^z_')[length(grep(colnames(InputParams), pattern = '^z_'))], 0.5+grep(colnames(InputParams), pattern = '^z_')[length(grep(colnames(InputParams), pattern = '^z_'))]), y = c(-10,130), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^s9_')[length(grep(colnames(InputParams), pattern = '^s9_'))], 0.5+grep(colnames(InputParams), pattern = '^s9_')[length(grep(colnames(InputParams), pattern = '^s9_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^s109_')[length(grep(colnames(InputParams), pattern = '^s109_'))], 0.5+grep(colnames(InputParams), pattern = '^s109_')[length(grep(colnames(InputParams), pattern = '^s109_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^s8_')[length(grep(colnames(InputParams), pattern = '^s8_'))], 0.5+grep(colnames(InputParams), pattern = '^s8_')[length(grep(colnames(InputParams), pattern = '^s8_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^s108_')[length(grep(colnames(InputParams), pattern = '^s108_'))], 0.5+grep(colnames(InputParams), pattern = '^s108_')[length(grep(colnames(InputParams), pattern = '^s108_'))]), y = c(-10,130), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^l1_')[length(grep(colnames(InputParams), pattern = '^l1_'))], 0.5+grep(colnames(InputParams), pattern = '^l1_')[length(grep(colnames(InputParams), pattern = '^l1_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^l2_')[length(grep(colnames(InputParams), pattern = '^l2_'))], 0.5+grep(colnames(InputParams), pattern = '^l2_')[length(grep(colnames(InputParams), pattern = '^l2_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^l3_')[length(grep(colnames(InputParams), pattern = '^l3_'))], 0.5+grep(colnames(InputParams), pattern = '^l3_')[length(grep(colnames(InputParams), pattern = '^l3_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^l4_')[length(grep(colnames(InputParams), pattern = '^l4_'))], 0.5+grep(colnames(InputParams), pattern = '^l4_')[length(grep(colnames(InputParams), pattern = '^l4_'))]), y = c(-10,130), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^v102_')[length(grep(colnames(InputParams), pattern = '^v102_'))], 0.5+grep(colnames(InputParams), pattern = '^v102_')[length(grep(colnames(InputParams), pattern = '^v102_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^v3_')[length(grep(colnames(InputParams), pattern = '^v3_'))], 0.5+grep(colnames(InputParams), pattern = '^v3_')[length(grep(colnames(InputParams), pattern = '^v3_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^v4_')[length(grep(colnames(InputParams), pattern = '^v4_'))], 0.5+grep(colnames(InputParams), pattern = '^v4_')[length(grep(colnames(InputParams), pattern = '^v4_'))]), y = c(-10,130), lty = 1, col = 'black')
legend('topright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos, cex = 1.3)
dev.off()

png('EE95_mua_b.png', res = 300, units = 'in', height = 7, width = 7)
barplot(height = muaEEs95_b/max(muaEEs95_b), ylab = 'Normalized Mean Abs. Val. of Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Upper 5th Percentile of Flow', col = ColPlots, border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
#Make lines separating the different classes of variables
legend('topright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos, cex = 1.3)
dev.off()

png('EE95_mua_b_thresh12.png', res = 300, units = 'in', height = 7, width = 7)
barplot(height = muaEEs95_b/max(muaEEs95_b), ylab = 'Normalized Mean Abs. Val. of Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Upper 5th Percentile of Flow', col = ColPlots, border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
#Make lines separating the different classes of variables
lines(x = c(-100,300), y = c(RanksMua95_b[Top10,2]/max(muaEEs95_b),RanksMua95_b[Top10,2]/max(muaEEs95_b)))
legend('topright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos, cex = 1.3)
dev.off()

png('EE95_mua_b_lines.png', res = 300, units = 'in', height = 7, width = 7)
barplot(height = muaEEs95_b, ylab = 'Mean Absolute Value of the Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Upper 5th Percentile of Flow', col = ColPlots, border = NA, ylim = c(0,120), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
#Make lines separating the different classes of variables
lines(x = c(-1,-1), y = c(-10,130), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^h_')[length(grep(colnames(InputParams), pattern = '^h_'))], 0.5+grep(colnames(InputParams), pattern = '^h_')[length(grep(colnames(InputParams), pattern = '^h_'))]), y = c(-10,130), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^z_')[length(grep(colnames(InputParams), pattern = '^z_'))], 0.5+grep(colnames(InputParams), pattern = '^z_')[length(grep(colnames(InputParams), pattern = '^z_'))]), y = c(-10,130), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^s9_')[length(grep(colnames(InputParams), pattern = '^s9_'))], 0.5+grep(colnames(InputParams), pattern = '^s9_')[length(grep(colnames(InputParams), pattern = '^s9_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^s109_')[length(grep(colnames(InputParams), pattern = '^s109_'))], 0.5+grep(colnames(InputParams), pattern = '^s109_')[length(grep(colnames(InputParams), pattern = '^s109_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^s8_')[length(grep(colnames(InputParams), pattern = '^s8_'))], 0.5+grep(colnames(InputParams), pattern = '^s8_')[length(grep(colnames(InputParams), pattern = '^s8_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^s108_')[length(grep(colnames(InputParams), pattern = '^s108_'))], 0.5+grep(colnames(InputParams), pattern = '^s108_')[length(grep(colnames(InputParams), pattern = '^s108_'))]), y = c(-10,130), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^l1_')[length(grep(colnames(InputParams), pattern = '^l1_'))], 0.5+grep(colnames(InputParams), pattern = '^l1_')[length(grep(colnames(InputParams), pattern = '^l1_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^l2_')[length(grep(colnames(InputParams), pattern = '^l2_'))], 0.5+grep(colnames(InputParams), pattern = '^l2_')[length(grep(colnames(InputParams), pattern = '^l2_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^l3_')[length(grep(colnames(InputParams), pattern = '^l3_'))], 0.5+grep(colnames(InputParams), pattern = '^l3_')[length(grep(colnames(InputParams), pattern = '^l3_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^l4_')[length(grep(colnames(InputParams), pattern = '^l4_'))], 0.5+grep(colnames(InputParams), pattern = '^l4_')[length(grep(colnames(InputParams), pattern = '^l4_'))]), y = c(-10,130), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^v102_')[length(grep(colnames(InputParams), pattern = '^v102_'))], 0.5+grep(colnames(InputParams), pattern = '^v102_')[length(grep(colnames(InputParams), pattern = '^v102_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^v3_')[length(grep(colnames(InputParams), pattern = '^v3_'))], 0.5+grep(colnames(InputParams), pattern = '^v3_')[length(grep(colnames(InputParams), pattern = '^v3_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^v4_')[length(grep(colnames(InputParams), pattern = '^v4_'))], 0.5+grep(colnames(InputParams), pattern = '^v4_')[length(grep(colnames(InputParams), pattern = '^v4_'))]), y = c(-10,130), lty = 1, col = 'black')
legend('topright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos, cex = 1.3)
dev.off()

png('EEot_mua_b.png', res = 300, units = 'in', height = 7, width = 7)
barplot(height = muaEEsot_b/max(muaEEsot_b), ylab = 'Normalized Mean Abs. Val. of Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for 5th-95th Percentile Flows', col = ColPlots, border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
#Make lines separating the different classes of variables
legend('topright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos, cex = 1.3)
dev.off()

png('EEot_mua_b_thresh12.png', res = 300, units = 'in', height = 7, width = 7)
barplot(height = muaEEsot_b/max(muaEEsot_b), ylab = 'Normalized Mean Abs. Val. of Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for 5th-95th Percentile Flows', col = ColPlots, border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
#Make lines separating the different classes of variables
lines(x = c(-100,300), y = c(RanksMuaot_b[Top10,2]/max(muaEEsot_b),RanksMuaot_b[Top10,2]/max(muaEEsot_b)))
legend('topright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos, cex = 1.3)
dev.off()

png('EEot_mua_b_lines.png', res = 300, units = 'in', height = 7, width = 7)
barplot(height = muaEEsot_b, ylab = 'Mean Absolute Value of the Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for 5th-95th Percentile Flows', col = ColPlots, border = NA, ylim = c(0,3000), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
#Make lines separating the different classes of variables
lines(x = c(-1,-1), y = c(-10,3100), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^h_')[length(grep(colnames(InputParams), pattern = '^h_'))], 0.5+grep(colnames(InputParams), pattern = '^h_')[length(grep(colnames(InputParams), pattern = '^h_'))]), y = c(-10,3100), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^z_')[length(grep(colnames(InputParams), pattern = '^z_'))], 0.5+grep(colnames(InputParams), pattern = '^z_')[length(grep(colnames(InputParams), pattern = '^z_'))]), y = c(-10,3100), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^s9_')[length(grep(colnames(InputParams), pattern = '^s9_'))], 0.5+grep(colnames(InputParams), pattern = '^s9_')[length(grep(colnames(InputParams), pattern = '^s9_'))]), y = c(-10,3100), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^s109_')[length(grep(colnames(InputParams), pattern = '^s109_'))], 0.5+grep(colnames(InputParams), pattern = '^s109_')[length(grep(colnames(InputParams), pattern = '^s109_'))]), y = c(-10,3100), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^s8_')[length(grep(colnames(InputParams), pattern = '^s8_'))], 0.5+grep(colnames(InputParams), pattern = '^s8_')[length(grep(colnames(InputParams), pattern = '^s8_'))]), y = c(-10,3100), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^s108_')[length(grep(colnames(InputParams), pattern = '^s108_'))], 0.5+grep(colnames(InputParams), pattern = '^s108_')[length(grep(colnames(InputParams), pattern = '^s108_'))]), y = c(-10,3100), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^l1_')[length(grep(colnames(InputParams), pattern = '^l1_'))], 0.5+grep(colnames(InputParams), pattern = '^l1_')[length(grep(colnames(InputParams), pattern = '^l1_'))]), y = c(-10,3100), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^l2_')[length(grep(colnames(InputParams), pattern = '^l2_'))], 0.5+grep(colnames(InputParams), pattern = '^l2_')[length(grep(colnames(InputParams), pattern = '^l2_'))]), y = c(-10,3100), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^l3_')[length(grep(colnames(InputParams), pattern = '^l3_'))], 0.5+grep(colnames(InputParams), pattern = '^l3_')[length(grep(colnames(InputParams), pattern = '^l3_'))]), y = c(-10,3100), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^l4_')[length(grep(colnames(InputParams), pattern = '^l4_'))], 0.5+grep(colnames(InputParams), pattern = '^l4_')[length(grep(colnames(InputParams), pattern = '^l4_'))]), y = c(-10,3100), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^v102_')[length(grep(colnames(InputParams), pattern = '^v102_'))], 0.5+grep(colnames(InputParams), pattern = '^v102_')[length(grep(colnames(InputParams), pattern = '^v102_'))]), y = c(-10,3100), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^v3_')[length(grep(colnames(InputParams), pattern = '^v3_'))], 0.5+grep(colnames(InputParams), pattern = '^v3_')[length(grep(colnames(InputParams), pattern = '^v3_'))]), y = c(-10,3100), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^v4_')[length(grep(colnames(InputParams), pattern = '^v4_'))], 0.5+grep(colnames(InputParams), pattern = '^v4_')[length(grep(colnames(InputParams), pattern = '^v4_'))]), y = c(-10,3100), lty = 1, col = 'black')
legend('topright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos, cex = 1.3)
dev.off()

png('EETN05_mua_b.png', res = 300, units = 'in', height = 7, width = 7)
barplot(height = muaEEsTN05_b/max(muaEEsTN05_b), ylab = 'Normalized Mean Abs. Val. of Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Lower 5th Quantile of TN', col = ColPlots, border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
#Make lines separating the different classes of variables
legend('topright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos, cex = 1.3)
dev.off()

png('EETN05_mua_b_lines.png', res = 300, units = 'in', height = 7, width = 7)
barplot(height = muaEEsTN05_b, ylab = 'Mean Absolute Value of the Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Lower 5th Quantile of TN', col = ColPlots, border = NA, ylim = c(0,40), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
#Make lines separating the different classes of variables
lines(x = c(-1,-1), y = c(-10,130), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^h_')[length(grep(colnames(InputParams), pattern = '^h_'))], 0.5+grep(colnames(InputParams), pattern = '^h_')[length(grep(colnames(InputParams), pattern = '^h_'))]), y = c(-10,130), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^z_')[length(grep(colnames(InputParams), pattern = '^z_'))], 0.5+grep(colnames(InputParams), pattern = '^z_')[length(grep(colnames(InputParams), pattern = '^z_'))]), y = c(-10,130), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^s9_')[length(grep(colnames(InputParams), pattern = '^s9_'))], 0.5+grep(colnames(InputParams), pattern = '^s9_')[length(grep(colnames(InputParams), pattern = '^s9_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^s109_')[length(grep(colnames(InputParams), pattern = '^s109_'))], 0.5+grep(colnames(InputParams), pattern = '^s109_')[length(grep(colnames(InputParams), pattern = '^s109_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^s8_')[length(grep(colnames(InputParams), pattern = '^s8_'))], 0.5+grep(colnames(InputParams), pattern = '^s8_')[length(grep(colnames(InputParams), pattern = '^s8_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^s108_')[length(grep(colnames(InputParams), pattern = '^s108_'))], 0.5+grep(colnames(InputParams), pattern = '^s108_')[length(grep(colnames(InputParams), pattern = '^s108_'))]), y = c(-10,130), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^l1_')[length(grep(colnames(InputParams), pattern = '^l1_'))], 0.5+grep(colnames(InputParams), pattern = '^l1_')[length(grep(colnames(InputParams), pattern = '^l1_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^l2_')[length(grep(colnames(InputParams), pattern = '^l2_'))], 0.5+grep(colnames(InputParams), pattern = '^l2_')[length(grep(colnames(InputParams), pattern = '^l2_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^l3_')[length(grep(colnames(InputParams), pattern = '^l3_'))], 0.5+grep(colnames(InputParams), pattern = '^l3_')[length(grep(colnames(InputParams), pattern = '^l3_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^l4_')[length(grep(colnames(InputParams), pattern = '^l4_'))], 0.5+grep(colnames(InputParams), pattern = '^l4_')[length(grep(colnames(InputParams), pattern = '^l4_'))]), y = c(-10,130), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^v102_')[length(grep(colnames(InputParams), pattern = '^v102_'))], 0.5+grep(colnames(InputParams), pattern = '^v102_')[length(grep(colnames(InputParams), pattern = '^v102_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^v3_')[length(grep(colnames(InputParams), pattern = '^v3_'))], 0.5+grep(colnames(InputParams), pattern = '^v3_')[length(grep(colnames(InputParams), pattern = '^v3_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^v4_')[length(grep(colnames(InputParams), pattern = '^v4_'))], 0.5+grep(colnames(InputParams), pattern = '^v4_')[length(grep(colnames(InputParams), pattern = '^v4_'))]), y = c(-10,130), lty = 1, col = 'black')
legend('topright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos, cex = 1.3)
dev.off()

png('EETN95_mua_b.png', res = 300, units = 'in', height = 7, width = 7)
barplot(height = muaEEsTN95_b/max(muaEEsTN95_b), ylab = 'Normalized Mean Abs. Val. of Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Upper 5th Quantile of TN', col = ColPlots, border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
#Make lines separating the different classes of variables
legend('topright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos, cex = 1.3)
dev.off()

png('EETN95_mua_b_lines.png', res = 300, units = 'in', height = 7, width = 7)
barplot(height = muaEEsTN95_b, ylab = 'Mean Absolute Value of the Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Upper 5th Quantile of TN', col = ColPlots, border = NA, ylim = c(0,20), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
#Make lines separating the different classes of variables
lines(x = c(-1,-1), y = c(-10,130), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^h_')[length(grep(colnames(InputParams), pattern = '^h_'))], 0.5+grep(colnames(InputParams), pattern = '^h_')[length(grep(colnames(InputParams), pattern = '^h_'))]), y = c(-10,130), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^z_')[length(grep(colnames(InputParams), pattern = '^z_'))], 0.5+grep(colnames(InputParams), pattern = '^z_')[length(grep(colnames(InputParams), pattern = '^z_'))]), y = c(-10,130), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^s9_')[length(grep(colnames(InputParams), pattern = '^s9_'))], 0.5+grep(colnames(InputParams), pattern = '^s9_')[length(grep(colnames(InputParams), pattern = '^s9_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^s109_')[length(grep(colnames(InputParams), pattern = '^s109_'))], 0.5+grep(colnames(InputParams), pattern = '^s109_')[length(grep(colnames(InputParams), pattern = '^s109_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^s8_')[length(grep(colnames(InputParams), pattern = '^s8_'))], 0.5+grep(colnames(InputParams), pattern = '^s8_')[length(grep(colnames(InputParams), pattern = '^s8_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^s108_')[length(grep(colnames(InputParams), pattern = '^s108_'))], 0.5+grep(colnames(InputParams), pattern = '^s108_')[length(grep(colnames(InputParams), pattern = '^s108_'))]), y = c(-10,130), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^l1_')[length(grep(colnames(InputParams), pattern = '^l1_'))], 0.5+grep(colnames(InputParams), pattern = '^l1_')[length(grep(colnames(InputParams), pattern = '^l1_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^l2_')[length(grep(colnames(InputParams), pattern = '^l2_'))], 0.5+grep(colnames(InputParams), pattern = '^l2_')[length(grep(colnames(InputParams), pattern = '^l2_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^l3_')[length(grep(colnames(InputParams), pattern = '^l3_'))], 0.5+grep(colnames(InputParams), pattern = '^l3_')[length(grep(colnames(InputParams), pattern = '^l3_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^l4_')[length(grep(colnames(InputParams), pattern = '^l4_'))], 0.5+grep(colnames(InputParams), pattern = '^l4_')[length(grep(colnames(InputParams), pattern = '^l4_'))]), y = c(-10,130), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^v102_')[length(grep(colnames(InputParams), pattern = '^v102_'))], 0.5+grep(colnames(InputParams), pattern = '^v102_')[length(grep(colnames(InputParams), pattern = '^v102_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^v3_')[length(grep(colnames(InputParams), pattern = '^v3_'))], 0.5+grep(colnames(InputParams), pattern = '^v3_')[length(grep(colnames(InputParams), pattern = '^v3_'))]), y = c(-10,130), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^v4_')[length(grep(colnames(InputParams), pattern = '^v4_'))], 0.5+grep(colnames(InputParams), pattern = '^v4_')[length(grep(colnames(InputParams), pattern = '^v4_'))]), y = c(-10,130), lty = 1, col = 'black')
legend('topright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos, cex = 1.3)
dev.off()

png('EETNMed_mua_b.png', res = 300, units = 'in', height = 7, width = 7)
barplot(height = muaEEsTNMed_b/max(muaEEsTNMed_b), ylab = 'Normalized Mean Abs. Val. of Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Mean of TN', col = ColPlots, border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
#Make lines separating the different classes of variables
legend('topright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos, cex = 1.3)
dev.off()

png('EETNMed_mua_b_thresh12.png', res = 300, units = 'in', height = 7, width = 7)
barplot(height = muaEEsTNMed_b/max(muaEEsTNMed_b), ylab = 'Normalized Mean Abs. Val. of Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Mean of TN', col = ColPlots, border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
#Make lines separating the different classes of variables
lines(x = c(-100,300), y = c(RanksMuaTNMed_b[Top10,2]/max(muaEEsTNMed_b),RanksMuaTNMed_b[Top10,2]/max(muaEEsTNMed_b)))
legend('topright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos, cex = 1.3)
dev.off()

png('EETNMed_mua_b_lines.png', res = 300, units = 'in', height = 7, width = 7)
barplot(height = muaEEsTNMed_b, ylab = 'Mean Absolute Value of the Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Mean of TN', col = ColPlots, border = NA, ylim = c(0,30), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
#Make lines separating the different classes of variables
lines(x = c(-1,-1), y = c(-10,3100), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^h_')[length(grep(colnames(InputParams), pattern = '^h_'))], 0.5+grep(colnames(InputParams), pattern = '^h_')[length(grep(colnames(InputParams), pattern = '^h_'))]), y = c(-10,3100), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^z_')[length(grep(colnames(InputParams), pattern = '^z_'))], 0.5+grep(colnames(InputParams), pattern = '^z_')[length(grep(colnames(InputParams), pattern = '^z_'))]), y = c(-10,3100), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^s9_')[length(grep(colnames(InputParams), pattern = '^s9_'))], 0.5+grep(colnames(InputParams), pattern = '^s9_')[length(grep(colnames(InputParams), pattern = '^s9_'))]), y = c(-10,3100), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^s109_')[length(grep(colnames(InputParams), pattern = '^s109_'))], 0.5+grep(colnames(InputParams), pattern = '^s109_')[length(grep(colnames(InputParams), pattern = '^s109_'))]), y = c(-10,3100), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^s8_')[length(grep(colnames(InputParams), pattern = '^s8_'))], 0.5+grep(colnames(InputParams), pattern = '^s8_')[length(grep(colnames(InputParams), pattern = '^s8_'))]), y = c(-10,3100), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^s108_')[length(grep(colnames(InputParams), pattern = '^s108_'))], 0.5+grep(colnames(InputParams), pattern = '^s108_')[length(grep(colnames(InputParams), pattern = '^s108_'))]), y = c(-10,3100), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^l1_')[length(grep(colnames(InputParams), pattern = '^l1_'))], 0.5+grep(colnames(InputParams), pattern = '^l1_')[length(grep(colnames(InputParams), pattern = '^l1_'))]), y = c(-10,3100), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^l2_')[length(grep(colnames(InputParams), pattern = '^l2_'))], 0.5+grep(colnames(InputParams), pattern = '^l2_')[length(grep(colnames(InputParams), pattern = '^l2_'))]), y = c(-10,3100), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^l3_')[length(grep(colnames(InputParams), pattern = '^l3_'))], 0.5+grep(colnames(InputParams), pattern = '^l3_')[length(grep(colnames(InputParams), pattern = '^l3_'))]), y = c(-10,3100), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^l4_')[length(grep(colnames(InputParams), pattern = '^l4_'))], 0.5+grep(colnames(InputParams), pattern = '^l4_')[length(grep(colnames(InputParams), pattern = '^l4_'))]), y = c(-10,3100), lty = 1, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^v102_')[length(grep(colnames(InputParams), pattern = '^v102_'))], 0.5+grep(colnames(InputParams), pattern = '^v102_')[length(grep(colnames(InputParams), pattern = '^v102_'))]), y = c(-10,3100), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^v3_')[length(grep(colnames(InputParams), pattern = '^v3_'))], 0.5+grep(colnames(InputParams), pattern = '^v3_')[length(grep(colnames(InputParams), pattern = '^v3_'))]), y = c(-10,3100), lty = 2, col = 'black')
lines(x = c(0.5+grep(colnames(InputParams), pattern = '^v4_')[length(grep(colnames(InputParams), pattern = '^v4_'))], 0.5+grep(colnames(InputParams), pattern = '^v4_')[length(grep(colnames(InputParams), pattern = '^v4_'))]), y = c(-10,3100), lty = 1, col = 'black')
legend('topright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos, cex = 1.3)
dev.off()

#Hillslope Plots for mua----
#Testing for normalized EE value. Rank order may be better to just see if the order matters.

colPal = colorRampPalette(colors = rev(c('red', 'orange', 'yellow', 'green', 'blue')))
scaleRange = c(0, 1)
scaleBy = 0.1
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)

#Make a grid of the parameters in top 10% for hillslopes
SortRanksMua_b = RanksMua_b[c(1, 11, 9, 2, 4, 8, 3, 5, 12, 14, 13, 7, 10, 6)]
png('HillNormalizedTop12.png', res = 300, height = 6, width = 6, units = 'in')
for (h in 1:length(uhills)){
  #Loop over the top 10% ranks for basin
  for (j in 1:length(SortRanksMua_b)){
    plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,15), pch = 15, xlab = 'Hillslope ID', ylab = 'Parameter', col = colFun(RanksMua95_h[[h]]$EE05_h[RanksMua95_h[[h]]$Param == SortRanksMua_b[j]] / max(RanksMua95_h[[h]]$EE05_h)), axes = FALSE)
    par(new=TRUE)
  }
}
axis(side = 1, at = seq(1,14,1), labels = TRUE)
axis(side = 2, at = seq(1,14,1), labels = FALSE)
dev.off()

colPal = colorRampPalette(colors = rev(c('red', 'orange', 'gray', 'green', 'blue')))
scaleRange = c(0, 50)
scaleBy = 15
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)
Ranks=NULL
png('HillRankTop12.png', res = 300, height = 6, width = 6, units = 'in')
par(mar = c(1,11,0,5))
for (h in 1:length(uhills)){
  #Loop over the top 10% ranks for basin
  for (j in 1:length(SortRanksMua_b)){
    if(j == 1){
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,15), pch = 15, xlab = 'Hillslope ID', ylab = 'Parameter', col = colFun(which(RanksMua95_h[[h]]$Param == SortRanksMua_b[j])), axes = FALSE, cex.lab = 1.5)
    }else{
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,15), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua95_h[[h]]$Param == SortRanksMua_b[j])), axes = FALSE)
    }
    par(new=TRUE)
    Ranks = c(Ranks,which(RanksMua95_h[[h]]$Param == SortRanksMua_b[j]))
  }
}
par(new=FALSE)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,14,1), labels = SortRanksMua_b, las = 1)
#legend('top', title = expression(bold('Parameter Sensitivity Rank')), legend = c('1 - 14', '15 - 29', '30 - 44', '45 - 60', '61 - 101'), horiz = TRUE, pch = 15, col = colFun(seq(0,60,15)), inset = -0.1, xpd = TRUE)
legend('right', title = expression(bold('Rank')), legend = c('1 - 14', '15 - 29', '30 - 44', '45 - 60', '61 - 101'), pch = 15, col = colFun(seq(0,60,15)), inset = -0.3, xpd = TRUE)
dev.off()

#Plots that drop the multiplier variables and use only the aggregated variables----


#Compare the parameters affected by multipliers after aggregating constrained parameters to see if they are different in sensitivity----
#Using m as an example for now because it's in the top 12
png('BasinKsatDecayEEs.png', res = 300, width = 3, height = 5, units = 'in')
barplot(height = RanksMua05_b$EE05_b[c(grep(RanksMua05_b$Param, pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,2)], grep(RanksMua05_b$Param, pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,2)])]/max(RanksMua05_b$EE05_b), names.arg = c('s9_m', 's109_m', 's8_m', 's108_m'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5)
dev.off()

#Parameter corrleation plots----
#?sensitivity::pcc()
#?sensitivity::plot3d.morris()
#?sensitivity::morrisMultOut()
#?sensitivity::morris()

#Compute the correlation of parameters that have non-zero EEs----
#Norm of a vector
norm_vec <- function(x){
  sqrt(sum(x^2))
}
#Matrix for the similarity measure
#Determine number of columns based on number of non-zero EEs
CorCols = length(which(RanksMua05_b$EE05_b != 0))
CosPhi05_b = CosPhi95_b = CosPhiot_b = matrix(NA, nrow = CorCols, ncol = CorCols)
for (i in 1:CorCols){
  for (j in 1:CorCols){
    CosPhi05_b[i,j] = abs(t(EEs05_b[,which(muaEEs05_b != 0)][,i]) %*% EEs05_b[,which(muaEEs05_b != 0)][,j])/norm_vec(EEs05_b[,which(muaEEs05_b != 0)][,i])/norm_vec(EEs05_b[,which(muaEEs05_b != 0)][,j])
    CosPhi95_b[i,j] = abs(t(EEs95_b[,which(muaEEs95_b != 0)][,i]) %*% EEs95_b[,which(muaEEs95_b != 0)][,j])/norm_vec(EEs95_b[,which(muaEEs95_b != 0)][,i])/norm_vec(EEs95_b[,which(muaEEs95_b != 0)][,j])
    CosPhiot_b[i,j] = abs(t(EEsot_b[,which(muaEEsot_b != 0)][,i]) %*% EEsot_b[,which(muaEEsot_b != 0)][,j])/norm_vec(EEsot_b[,which(muaEEsot_b != 0)][,i])/norm_vec(EEsot_b[,which(muaEEsot_b != 0)][,j])
  }
}

#Set NA values to 0
CosPhi05_b[is.na(CosPhi05_b)] = 0
CosPhi95_b[is.na(CosPhi95_b)] = 0
CosPhiot_b[is.na(CosPhiot_b)] = 0

colnames(CosPhi05_b) = colnames(OrigParams[which(muaEEs05_b != 0)])
colnames(CosPhi95_b) = colnames(OrigParams[which(muaEEs95_b != 0)])
colnames(CosPhiot_b) = colnames(OrigParams[which(muaEEsot_b != 0)])
rownames(CosPhi05_b) = colnames(OrigParams[which(muaEEs05_b != 0)])
rownames(CosPhi95_b) = colnames(OrigParams[which(muaEEs95_b != 0)])
rownames(CosPhiot_b) = colnames(OrigParams[which(muaEEsot_b != 0)])

#Heatmap
png('ParamCorrelations_EE05.png', res = 300, height = 8, width = 8, units = 'in')
heatmap(x = CosPhi05_b, na.rm = TRUE, symm = TRUE, revC = TRUE)
dev.off()

png('ParamCorrelations_EE95.png', res = 300, height = 8, width = 8, units = 'in')
heatmap(x = CosPhi95_b, na.rm = TRUE, symm = TRUE, revC = TRUE)
dev.off()

png('ParamCorrelations_EEot.png', res = 300, height = 8, width = 8, units = 'in')
heatmap(x = CosPhiot_b, na.rm = TRUE, symm = TRUE, revC = TRUE)
dev.off()

#Perform heirarchical clustering for discovering variable clusters
#https://uc-r.github.io/hc_clustering

#List the variables that are highly similar

#Show SA metrics for the basin and for each hillslope - ranks, and maps for hillslope----


#Diagnose problems with EEs----
for (i in 1:cols){
  ind = i+(1+cols)*(t-1)
  parm = which((OrigParams[ind+1,] - OrigParams[ind,]) != 0)
  if(parm == 65){
    print(i)
    break
  }
}

#For all of these, the input parameter did not change when it was supposed to (it did change in the original input file)
#(3365 and 3366) for v102_epc.frootlitr_fcel is NaN because delta and change in values are 0
#(4913 and 4914) for v102_epc.frootlitr_fcel is NaN
#(5837 and 5838) for "s8_Ksat_0_v" is NaN
#(644 and 645) for "s109_Ksat_0_v" is NaN
#(5388 and 5389) for "s109_Ksat_0_v" is NaN

for (i in 1:r){
  j = which(Deltas[r,] == min(abs(Deltas)))
  if (length(j) > 0){
    print(j)
  }
}

#Resample values for the NaN replicates
#644:
set.seed(644)
runif(n = 1, min = 0.1, max = 0.36767)
#3365:
set.seed(3365)
runif(n = 1, min = 0.4, max = 0.5)
#4913:
set.seed(4913)
runif(n = 1, min = 0.4, max = 0.5)
#5388
set.seed(5388)
runif(n = 1, min = 0.1, max = .909091)
#5837
#set manually to 0.4 because both Ksat0 and Ksat0v needed adjustment from lower bound

#Save parameters to be used for calibration in a new file----
ParamRanges_Cal = ParamRanges[,]
#Add all of the parameters of the likelihood function as well
# (kurotsis) beta=-1: uniform, beta=0: Gaussian, beta=1: double exponential
# (skewness) xi=1: symmetric, xi<1: negatively skewed, xi>1: positively skewed
# (standard deviation when mean=0)
# (linear rate of change in standard deviation with mean)
# (lag-1 auto-correlation), phi_1=0: no auto-correlation, phi_1=1: perfect auto-correlation
# (mean bias factor)
LikelihoodParams = cbind(c('PL_beta', 'PL_xi', 'PL_sigma_0', 'PL_sigma_1', 'PL_phi_1', 'PL_mu_h'), c('beta', 'xi', 'sigma_0', 'sigma_1', 'phi_1', 'mu_h'), 
                         c(-1, 0, 0, 0, 0, 0), c(1, 10, 1, 1, 1, 100))
ParamRanges_Cal = rbind(ParamRanges_Cal, LikelihoodParams)

write.csv(ParamRanges_Cal, file = 'BaismanCalibrationParameterProblemFile.csv', row.names = FALSE, col.names = TRUE)

#Save a file of chain starting locations for these parameters----
# Take a random sample of N from the 100 most likely, where N is number of chains----
N = 30
TopLikes = seq(1,100,1)
set.seed(8356)
#Sample without replacement to get the 30 starting locations
SelTopLikes = vector('numeric', length = N)
for (i in 1:N){
  IndTop = round(runif(n = 1, min = 1, max = length(TopLikes)),0)
  SelTopLikes[i] = TopLikes[IndTop]
  TopLikes = TopLikes[-IndTop]
}
SelTopLikes = sort(SelTopLikes)

# Load in the likelihoods of the SA runs----

# Gather the selected likelihood run indices----
RunIndsTopLikes = 

#Get the parameters for those run indices into a matrix - use only the calibration parameters
ChainStarts = matrix()

write.csv(ChainStarts, file = 'BaismanChainStarts.txt', sep = '\t', row.names = FALSE, col.names = TRUE)
