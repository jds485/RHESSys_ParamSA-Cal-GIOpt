#Script for calculating Morris EEs from output streamflow and TN data
#This library may be useful for analysis, but is not used right now
#library(sensitivity)

#Load Morris parameter files and ranges----
InputParams = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs\\MorrisSampleLocs\\MorrisSamples_AfterProcessing.csv", stringsAsFactors = FALSE)
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
BasinSF = read.table(file = 'SAResults_BasinStreamflow_p4_Reordered.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillSF = read.table(file = 'SAResults_HillStreamflow_p6_Reordered.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)

#Load WRTDS TN data----
BasinTN05 = read.table(file = 'SAResults_BasinTN05_p3_All_Reordered.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
BasinTNMed = read.table(file = 'SAResults_BasinTNMed_p3_All_Reordered.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
BasinTN95 = read.table(file = 'SAResults_BasinTN95_p3_All_Reordered.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillTN05 = read.table(file = 'SAResults_HillTN05_p3_All_Reordered.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillTNMed = read.table(file = 'SAResults_HillTNMed_p3_All_Reordered.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillTN95 = read.table(file = 'SAResults_HillTN95_p3_All_Reordered.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)

#Remove observations earlier than 10/01/2004 (SA timeperiod start)----
HillSF = HillSF[, c(1, 2, which(as.Date(colnames(HillSF[,-c(1,2)])) >= as.Date('2004-10-01'))+2)]
HillTN05 = HillTN05[, c(1, 2, which(as.Date(colnames(HillTN05[,-c(1,2)])) >= as.Date('2004-10-01'))+2)]
HillTNMed = HillTNMed[, c(1, 2, which(as.Date(colnames(HillTNMed[,-c(1,2)])) >= as.Date('2004-10-01'))+2)]
HillTN95 = HillTN95[, c(1, 2, which(as.Date(colnames(HillTN95[,-c(1,2)])) >= as.Date('2004-10-01'))+2)]

BasinSF = BasinSF[, c(1, which(as.Date(colnames(BasinSF[,-1])) >= as.Date('2004-10-01'))+1)]
BasinTN05 = BasinTN05[, c(1, which(as.Date(colnames(BasinTN05[,-1])) >= as.Date('2004-10-01'))+1)]
BasinTNMed = BasinTNMed[, c(1, which(as.Date(colnames(BasinTNMed[,-1])) >= as.Date('2004-10-01'))+1)]
BasinTN95 = BasinTN95[, c(1, which(as.Date(colnames(BasinTN95[,-1])) >= as.Date('2004-10-01'))+1)]

#Order by replicate ID. This is done to ensure that the trajectories are in order. They should already be in order, though.----
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

#Find the days with the highest 5th percentile, lowest 5th percentile and all other flows----
q05 = quantile(x = obs$Flow, probs = 0.05)
q95 = quantile(x = obs$Flow, probs = 0.95)

obs05 = obs[obs$Flow <= q05,]
obs95 = obs[obs$Flow >= q95,]
days05 = as.Date(obs05$Date)
days95 = as.Date(obs95$Date)
obsot = obs[-which(as.Date(obs$Date) %in% c(days05, days95)),]
daysot = as.Date(obsot$Date)

#Check what days these correspond to - want a uniform distribution in time----
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

#Coverage is not great for the low flow metric. Use instead the lowest 5th percentile in each year of data----
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

#Coverage for middle flows is slighly biased to earlier years because of upper flow metric. Test yearly 5ths for both upper and lower----
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

#Selected metric: lower 5th yearly, upper 5th global, and all else----
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

#Create the median hillslope streamflows----
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

#Create the median hillslope TNs----
MedTN05Hills = MedTNMedHills = MedTN95Hills = matrix(NA, nrow = length(uhills), ncol = ncol(HillTN05)-1)
for (h in 1:length(uhills)){
  MedTN05Hills[h,] = c(uhills[h], apply(X = HillTN05[which(HillTN05$HillID == uhills[h]),-c(1,2)], MARGIN = 2, FUN = median))
  MedTNMedHills[h,] = c(uhills[h], apply(X = HillTNMed[which(HillTNMed$HillID == uhills[h]),-c(1,2)], MARGIN = 2, FUN = median))
  MedTN95Hills[h,] = c(uhills[h], apply(X = HillTN95[which(HillTN95$HillID == uhills[h]),-c(1,2)], MARGIN = 2, FUN = median))
}
rm(h)

#Create a storage matrix for the EEs for each parameter
EEs05_b = EEs95_b = EEsot_b = EEsTN05_b = EEsTNMed_b = EEsTN95_b = matrix(NA, nrow = r, ncol = cols)
#Need an extra column for the hillslope ID
EEs05_h = EEs95_h = EEsot_h = EEsTN05_h = EEsTNMed_h = EEsTN95_h = matrix(NA, nrow = r*length(uhills), ncol = cols+1)
#Create a stoage matrix for the deltas
Deltas = matrix(NA, nrow = r, ncol = cols)
#Store the column names. These are the dates
colnms = colnames(BasinSF[,-1])
#Loop over the trajectories
#Fixme: this should be parallelized
tic = Sys.time()
for (t in 1:r){
  #Compute the EEs for all parameters in the trajectory
  for (i in 1:cols){
    #Determine the base index. This is used for basin, and the first hillslope value
    ind = i+(1+cols)*(t-1)
    
    #Find the parameter column that was changed, before any processing was completed. 
    #This is the same column that will be used for the Deltas and EEs
    parm = which((OrigParams[ind+1,] - OrigParams[ind,]) != 0)
    #Computes the exact delta from the modified sampling file. Allows for positive and negative deltas.
    delta = InputParams[ind+1,parm] - InputParams[ind,parm]
    #Adjust delta to the range sampled for the parameter
    delta = delta/abs(ParamRanges$Upper[parm] - ParamRanges$Lower[parm])
    Deltas[t, parm] = delta  
    #Fixme: record deltas for every variable, not only the parameter that changed
    #delta for other parameters could be larger than the OAT parameter that was supposed to change
    
    #Compute the difference for highest 5th percentile, lowest 5th percentile and all other flows, as defined by the days from the observed streamflow record
    #SSE - not using because we don't think it's appropriate for Morris method
    #Fixme: compare SSE to sum of absolute errors. Is there theoretical proof for Morris?
    #diff05 = (sum((BasinSF05[ind+1,] - obs05$Flow)^2) - sum((BasinSF05[ind,] - obs05$Flow)^2))  
    #diff95 = (sum((BasinSF95[ind+1,] - obs95$Flow)^2) - sum((BasinSF95[ind,] - obs95$Flow)^2))
    #diffot = (sum((BasinSFot[ind+1,] - obsot$Flow)^2) - sum((BasinSFot[ind,] - obsot$Flow)^2))
    
    #Fixme: Compare to using the median flow instead of the observed flow for basin
    
    #Sum absolute errors
    diff05 = (sum(abs((BasinSF05[ind+1,] - obs05$Flow))) - sum(abs((BasinSF05[ind,] - obs05$Flow))))  
    diff95 = (sum(abs((BasinSF95[ind+1,] - obs95$Flow))) - sum(abs((BasinSF95[ind,] - obs95$Flow))))
    diffot = (sum(abs((BasinSFot[ind+1,] - obsot$Flow))) - sum(abs((BasinSFot[ind,] - obsot$Flow))))
    
    #TN
    diffTN05 = (sum(abs((BasinTN05[ind+1,-1] - obsTN$TN))) - sum(abs((BasinTN05[ind,-1] - obsTN$TN))))  
    diffTNMed = (sum(abs((BasinTNMed[ind+1,-1] - obsTN$TN))) - sum(abs((BasinTNMed[ind,-1] - obsTN$TN))))
    diffTN95 = (sum(abs((BasinTN95[ind+1,-1] - obsTN$TN))) - sum(abs((BasinTN95[ind,-1] - obsTN$TN))))
    
    #Computes EEs with specified metric
    EEs05_b[t, parm] = diff05/delta
    EEs95_b[t, parm] = diff95/delta
    EEsot_b[t, parm] = diffot/delta
    
    EEsTN05_b[t, parm] = diffTN05/delta
    EEsTNMed_b[t, parm] = diffTNMed/delta
    EEsTN95_b[t, parm] = diffTN95/delta
    
    #Hillslopes
    #Fixme: Compare to median streamflow and TN
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
      
      EEs05_h[t+(hi-1)*r, c(1,parm+1)] = c(uhills[hi], diff05_h/delta)
      EEs95_h[t+(hi-1)*r, c(1,parm+1)] = c(uhills[hi], diff95_h/delta)
      EEsot_h[t+(hi-1)*r, c(1,parm+1)] = c(uhills[hi], diffot_h/delta)
      
      EEsTN05_h[t+(hi-1)*r, c(1,parm+1)] = c(uhills[hi], diffTN05_h/delta)
      EEsTNMed_h[t+(hi-1)*r, c(1,parm+1)] = c(uhills[hi], diffTNMed_h/delta)
      EEsTN95_h[t+(hi-1)*r, c(1,parm+1)] = c(uhills[hi], diffTN95_h/delta)
    }
  }
}
toc = Sys.time()
#32.5 mins per trajectory
print(toc-tic)
rm(i, t, hi, parm, ind, delta, diff05, diff05_h, diff95, diff95_h, diffot, diffot_h, diffTN05, diffTN05_h, diffTN95, diffTN95_h, diffTNMed, diffTNMed_h, IndsHill)

save.image(file = 'EEs_Corr.RData')

#Fixme: Some EEs have NA values
#For now, just setting those to NA
EEs05_b[is.nan(EEs05_b)] = NA
EEs95_b[is.nan(EEs95_b)] = NA
EEsot_b[is.nan(EEsot_b)] = NA
EEs05_h[is.nan(EEs05_h)] = NA
EEs95_h[is.nan(EEs95_h)] = NA
EEsot_h[is.nan(EEsot_h)] = NA
EEsTN05_b[is.nan(EEsTN05_b)] = NA
EEsTN95_b[is.nan(EEsTN95_b)] = NA
EEsTNMed_b[is.nan(EEsTNMed_b)] = NA
EEsTN05_h[is.nan(EEsTN05_h)] = NA
EEsTN95_h[is.nan(EEsTN95_h)] = NA
EEsTNMed_h[is.nan(EEsTNMed_h)] = NA

#Fixme: Average the EEs for variables that require it

#Using the EE data, compute the mean, standard deviation, and absolute mean
muEEs05_b = apply(X = EEs05_b, MARGIN = 2, FUN = mean, na.rm=TRUE)
muEEs95_b = apply(X = EEs95_b, MARGIN = 2, FUN = mean, na.rm=TRUE)
muEEsot_b = apply(X = EEsot_b, MARGIN = 2, FUN = mean, na.rm=TRUE)
muEEsTN05_b = apply(X = EEsTN05_b, MARGIN = 2, FUN = mean, na.rm=TRUE)
muEEsTN95_b = apply(X = EEsTN95_b, MARGIN = 2, FUN = mean, na.rm=TRUE)
muEEsTNMed_b = apply(X = EEsTNMed_b, MARGIN = 2, FUN = mean, na.rm=TRUE)

sdEEs05_b = apply(X = EEs05_b, MARGIN = 2, FUN = sd, na.rm=TRUE)
sdEEs95_b = apply(X = EEs95_b, MARGIN = 2, FUN = sd, na.rm=TRUE)
sdEEsot_b = apply(X = EEsot_b, MARGIN = 2, FUN = sd, na.rm=TRUE)
sdEEsTN05_b = apply(X = EEsTN05_b, MARGIN = 2, FUN = sd, na.rm=TRUE)
sdEEsTN95_b = apply(X = EEsTN95_b, MARGIN = 2, FUN = sd, na.rm=TRUE)
sdEEsTNMed_b = apply(X = EEsTNMed_b, MARGIN = 2, FUN = sd, na.rm=TRUE)

muaEEs05_b = apply(X = abs(EEs05_b), MARGIN = 2, FUN = mean, na.rm=TRUE)
muaEEs95_b = apply(X = abs(EEs95_b), MARGIN = 2, FUN = mean, na.rm=TRUE)
muaEEsot_b = apply(X = abs(EEsot_b), MARGIN = 2, FUN = mean, na.rm=TRUE)
muaEEsTN05_b = apply(X = abs(EEsTN05_b), MARGIN = 2, FUN = mean, na.rm=TRUE)
muaEEsTN95_b = apply(X = abs(EEsTN95_b), MARGIN = 2, FUN = mean, na.rm=TRUE)
muaEEsTNMed_b = apply(X = abs(EEsTNMed_b), MARGIN = 2, FUN = mean, na.rm=TRUE)

#Hillslopes will have one per hillslope
muEEs05_h = muEEs95_h = muEEsot_h = muEEsTN05_h = muEEs95_h = muEEsTNMed_h = sdEEs05_h = sdEEs95_h = sdEEsot_h = sdEEsTN05_h = sdEEs95_h = sdEEsTNMed_h = muaEEs05_h = muaEEs95_h = muaEEsot_h = muaEEsTN05_h = muaEEs95_h = muaEEsTNMed_h = matrix(NA, nrow = length(uhills), ncol = as.numeric(cols))
for (h in 1:length(uhills)){
  ind = 1+(h-1)*r
  muEEs05_h[h,] = apply(X = EEs05_h[ind:(ind+(r-1)),], MARGIN = 2, FUN = mean, na.rm=TRUE)
  muEEs95_h[h,] = apply(X = EEs95_h[ind:(ind+(r-1)),], MARGIN = 2, FUN = mean, na.rm=TRUE)
  muEEsot_h[h,] = apply(X = EEsot_h[ind:(ind+(r-1)),], MARGIN = 2, FUN = mean, na.rm=TRUE)
  muEEsTN05_h[h,] = apply(X = EEsTN05_h[ind:(ind+(r-1)),], MARGIN = 2, FUN = mean, na.rm=TRUE)
  muEEsTN95_h[h,] = apply(X = EEsTN95_h[ind:(ind+(r-1)),], MARGIN = 2, FUN = mean, na.rm=TRUE)
  muEEsTNMed_h[h,] = apply(X = EEsTNMed_h[ind:(ind+(r-1)),], MARGIN = 2, FUN = mean, na.rm=TRUE)
  
  sdEEs05_h[h,] = apply(X = EEs05_h[ind:(ind+(r-1)),], MARGIN = 2, FUN = sd, na.rm=TRUE)
  sdEEs95_h[h,] = apply(X = EEs95_h[ind:(ind+(r-1)),], MARGIN = 2, FUN = sd, na.rm=TRUE)
  sdEEsot_h[h,] = apply(X = EEsot_h[ind:(ind+(r-1)),], MARGIN = 2, FUN = sd, na.rm=TRUE)
  sdEEsTN05_h[h,] = apply(X = EEsTN05_h[ind:(ind+(r-1)),], MARGIN = 2, FUN = sd, na.rm=TRUE)
  sdEEsTN95_h[h,] = apply(X = EEsTN95_h[ind:(ind+(r-1)),], MARGIN = 2, FUN = sd, na.rm=TRUE)
  sdEEsTNMed_h[h,] = apply(X = EEsTNMed_h[ind:(ind+(r-1)),], MARGIN = 2, FUN = sd, na.rm=TRUE)
  
  muaEEs05_h[h,] = apply(X = abs(EEs05_h[ind:(ind+(r-1)),]), MARGIN = 2, FUN = mean, na.rm=TRUE)
  muaEEs95_h[h,] = apply(X = abs(EEs95_h[ind:(ind+(r-1)),]), MARGIN = 2, FUN = mean, na.rm=TRUE)
  muaEEsot_h[h,] = apply(X = abs(EEsot_h[ind:(ind+(r-1)),]), MARGIN = 2, FUN = mean, na.rm=TRUE)
  muaEEsTN05_h[h,] = apply(X = abs(EEsTN05_h[ind:(ind+(r-1)),]), MARGIN = 2, FUN = mean, na.rm=TRUE)
  muaEEsTN95_h[h,] = apply(X = abs(EEsTN95_h[ind:(ind+(r-1)),]), MARGIN = 2, FUN = mean, na.rm=TRUE)
  muaEEsTNMed_h[h,] = apply(X = abs(EEsTNMed_h[ind:(ind+(r-1)),]), MARGIN = 2, FUN = mean, na.rm=TRUE)
}

#Make plots of the sd vs. mu
colos = rainbow(cols)
png('EE05_sdVsMu_b.png', res = 300, units = 'in', height = 5, width = 5)
plot(x = muEEs05_b, y = sdEEs05_b, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SAE for Lower 5th Percentile of Flow', col = colos)
dev.off()
png('EE95_sdVsMu_b.png', res = 300, units = 'in', height = 5, width = 5)
plot(x = muEEs95_b, y = sdEEs95_b, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SAE for Upper 5th Percentile of Flow', col = colos)
dev.off()
png('EEot_sdVsMu_b.png', res = 300, units = 'in', height = 5, width = 5)
plot(x = muEEsot_b, y = sdEEsot_b, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SAE for 5th-95th Percentile Flows', col = colos)
dev.off()
png('EETN05_sdVsMu_b.png', res = 300, units = 'in', height = 5, width = 5)
plot(x = muEEsTN05_b, y = sdEEsTN05_b, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SAE for Lower 5th Quantile of TN', col = colos)
dev.off()
png('EETN95_sdVsMu_b.png', res = 300, units = 'in', height = 5, width = 5)
plot(x = muEEsTN95_b, y = sdEEsTN95_b, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SAE for Upper 5th Quantile of TN', col = colos)
dev.off()
png('EETNMed_sdVsMu_b.png', res = 300, units = 'in', height = 5, width = 5)
plot(x = muEEsTNMed_b, y = sdEEsTNMed_b, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SAE for Mean of TN', col = colos)
dev.off()



#Make plots of the ranking for mean absolute value
png('EE05_mua.png', res = 300, units = 'in', height = 5, width = 5)
barplot(height = muaEEs05_b, ylab = 'Mean Absolute Value of the Elementary Effect', xlab = 'Parameters', names.arg = seq(1,cols,1), main = 'Metric: SSE for Lower 5th Percentile of Flow')
dev.off()
png('EE95_mua.png', res = 300, units = 'in', height = 5, width = 5)
barplot(height = muaEEs95_b, ylab = 'Mean Absolute Value of the Elementary Effect', xlab = 'Parameters', names.arg = seq(1,cols,1), main = 'Metric: SSE for Upper 5th Percentile of Flow')
dev.off()
png('EEot_mua.png', res = 300, units = 'in', height = 5, width = 5)
barplot(height = muaEEsot_b, ylab = 'Mean Absolute Value of the Elementary Effect', xlab = 'Parameters', names.arg = seq(1,cols,1), main = 'Metric: SSE for 5th-95th Percentile Flows')
dev.off()

#Save a list of ordered names for mua
RanksMua05_b = data.frame(Param = colnames(InputParams)[rev(order(muaEEs05_b))], EE05_b = muaEEs05_b[rev(order(muaEEs05_b))], stringsAsFactors = FALSE)
RanksMua95_b = data.frame(Param = colnames(InputParams)[rev(order(muaEEs95_b))], EE95_b = muaEEs95_b[rev(order(muaEEs95_b))], stringsAsFactors = FALSE)
RanksMuaot_b = data.frame(Param = colnames(InputParams)[rev(order(muaEEsot_b))], EEot_b = muaEEsot_b[rev(order(muaEEsot_b))], stringsAsFactors = FALSE)

#Get the unique variables from this list
RanksMua_b = unique(c(RanksMua05_b$Param[1:40],RanksMuaot_b$Param[1:40],RanksMua95_b$Param[1:40]))

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

#List the variables that are highly similar (closer to 1)


#Compute bootstrapped samples with replacement of the EEs for each parameter (separately as opposed to by trajectory to avoid correlation in SDs of bootstrapped results)


#Compare EEs for multiplier variables


#Parameter corrleation plots
#sensitivity::pcc()
#sensitivity::plot3d.morris()
#?sensitivity::morrisMultOut()
#?sensitivity::morris()

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
