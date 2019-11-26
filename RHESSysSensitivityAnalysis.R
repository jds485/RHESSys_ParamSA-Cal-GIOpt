#Script for calculating Morris EEs from output streamflow and TN data
#This library may be useful for analysis, but is not used right now
#library(sensitivity)

#Load Morris parameter files and ranges----
InputParams = read.csv("C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs\\MorrisSampleLocs\\MorrisSamples_AfterProcessing.csv", stringsAsFactors = FALSE)
#Also need the original, unmodified Morris file to know which parameter changed in each trajectory iteration
OrigParams = read.csv("C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs\\MorrisSampleLocs\\MorrisSamples_BeforeProcessing.csv", stringsAsFactors = FALSE)
#And need the parameter ranges to scale the deltas
ParamRanges = read.csv('C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs\\MorrisSampleLocs\\BaismanMorrisSamplingProblemFile_Full.csv', stringsAsFactors = FALSE)

#Remove all of the parameters with _orig. They were not modified
InputParams = InputParams[-grep(x = colnames(InputParams), pattern = '_orig', fixed = TRUE)]
#Get the number of parameters
cols = ncol(InputParams)

#Specify number of trajectories----
r = 40

#Load RHESSys streamflow data----
setwd("C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR")
BasinSF = read.table(file = 'SAResults_BasinStreamflow_p4.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillSF = read.table(file = 'SAResults_HillStreamflow_p6.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)

#Load WRTDS TN data----
BasinTN05 = read.table(file = 'SAResults_BasinTN05_p3.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
BasinTNMed = read.table(file = 'SAResults_BasinTNMed_p3.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
BasinTN95 = read.table(file = 'SAResults_BasinTN95_p3.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillTN05 = read.table(file = 'SAResults_HillslopeTN05_p3.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillTNMed = read.table(file = 'SAResults_HillslopeTNMed_p3.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillTN95 = read.table(file = 'SAResults_HillslopeTN95_p3.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
colnames(BasinTN05) = colnames(BasinTNMed) = colnames(BasinTN95) = colnames(HillTN05) = colnames(HillTNMed) = colnames(HillTN95) = colnames(BasinSF)

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

HillSF = HillSF[order(HillSF$Replicate),]
HillTN05 = HillTN05[order(HillTN05$Replicate),]
HillTNMed = HillTNMed[order(HillTNMed$Replicate),]
HillTN95 = HillTN95[order(HillTN95$Replicate),]

#Load the observed streamflow record----
obs = read.table("C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\obs\\BaismanStreamflow_Cal.txt", header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, sep = '\t')
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

#Load the observed TN record----
obsTN = read.table("C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\obs\\TN_Cal.txt", header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, sep = '\t')
#Remove observations later than 9/30/2010 (SA timeperiod end)
obsTN = obsTN[as.Date(obsTN$Date) <= as.Date(colnames(BasinSF)[ncol(BasinSF)]),]
#Remove observations earlier than 10/01/2004 (SA timeperiod start)
obsTN = obsTN[as.Date(obsTN$Date) >= as.Date('2004-10-01'),]

#Create the 05, 95, and log-mean datasets for TN----
#Select only the days that have observations
BasinTN05 = BasinTN05[,-1][,which(as.Date(colnames(BasinTN05[,-1])) %in% days05)]
BasinTNMed = BasinTNMed[,-1][,which(as.Date(colnames(BasinTNMed[,-1])) %in% days95)]
BasinTN95 = BasinTN95[,-1][,which(as.Date(colnames(BasinTN95[,-1])) %in% daysot)]

#Create a storage matrix for the EEs for each parameter
EEs05_b = EEs95_b = EEsot_b = EEsTN05_b = EEsTNMed_b = EEsTN95_b = EEs05_h = EEs95_h = EEsot_h = EEsTN05_h = EEsTNMed_h = EEsTN95_h = matrix(NA, nrow = r, ncol = cols)
#Create a stoage matrix for the deltas
Deltas = matrix(NA, nrow = r, ncol = cols)
#Store the column names. These are the dates
colnms = colnames(BasinSF[,-1])
#Loop over the trajectories
for (t in 1:r){
  #Compute the EEs for all parameters in the trajectory
  for (i in 1:cols){
    ind = i+(1+cols)*(t-1)
    #Compute the difference for highest 5th percentile, lowest 5th percentile and all other flows, as defined by the days from the observed streamflow record
    #SSE - not using because we don't think it's appropriate for Morris method
    #Fixme: compare SSE to sum of absolute errors. Is there theoretical proof for Morris?
    #diff05 = (sum((BasinSF05[ind+1,-1] - obs05$Flow)^2) - sum((BasinSF05[ind,-1] - obs05$Flow)^2))  
    #diff95 = (sum((BasinSF95[ind+1,-1] - obs95$Flow)^2) - sum((BasinSF95[ind,-1] - obs95$Flow)^2))
    #diffot = (sum((BasinSFot[ind+1,-1] - obsot$Flow)^2) - sum((BasinSFot[ind,-1] - obsot$Flow)^2))
    
    #Sum absolute errors
    diff05 = (sum(abs((BasinSF05[ind+1,-1] - obs05$Flow))) - sum(abs((BasinSF05[ind,-1] - obs05$Flow))))  
    diff95 = (sum(abs((BasinSF95[ind+1,-1] - obs95$Flow))) - sum(abs((BasinSF95[ind,-1] - obs95$Flow))))
    diffot = (sum(abs((BasinSFot[ind+1,-1] - obsot$Flow))) - sum(abs((BasinSFot[ind,-1] - obsot$Flow))))
    
    #Fixme: Should select only the days that have data to compare
    diffTN05 = (sum(abs((BasinTN05[ind+1,-1] - obsTN$TN)), na.rm = TRUE) - sum(abs((BasinTN05[ind,-1] - obsTN$TN)), na.rm = TRUE))  
    diffTNMed = (sum(abs((BasinTNMed[ind+1,-1] - obsTN$TN)), na.rm = TRUE) - sum(abs((BasinTNMed[ind,-1] - obsTN$TN)), na.rm = TRUE))
    diffTN95 = (sum(abs((BasinTN95[ind+1,-1] - obsTN$TN)), na.rm = TRUE) - sum(abs((BasinTN95[ind,-1] - obsTN$TN)), na.rm = TRUE))
    
    #Hillslopes
    #Fixme: Compare to median streamflow
    #Fixme: add uhills
    #Fixme: dimensions of EEs may need to change for hillslopes, and other lengths may be off
    diff05_h = diff95_h = diffot_h = diffTN05_h = diffTNMed_h = diffTN95_h = vector('numeric', length = length(diff05))
    for (hi in 1:length(uhills)){
      #Get the indices for this hillslope
      IndsHill = 
      diff05_h[IndsHill] = (sum(abs((HillSF05[ind+1,-c(1,2)] - obs05$Flow))) - sum(abs((HillSF05[ind,-c(1,2)] - obs05$Flow))))  
      diff95_h[IndsHill] = (sum(abs((HillSF95[ind+1,-c(1,2)] - obs95$Flow))) - sum(abs((HillSF95[ind,-c(1,2)] - obs95$Flow))))
      diffot_h[IndsHill] = (sum(abs((HillSFot[ind+1,-c(1,2)] - obsot$Flow))) - sum(abs((HillSFot[ind,-c(1,2)] - obsot$Flow))))
      
      #TN
    }
    
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
    
    #Computes EEs with specified metric
    EEs05_b[t, parm] = diff05/delta
    EEs95_b[t, parm] = diff95/delta
    EEsot_b[t, parm] = diffot/delta
    
    EEsTN05_b[t, parm] = diffTN05/delta
    EEsTNMed_b[t, parm] = diffTNMed/delta
    EEsTN95_b[t, parm] = diffTN95/delta
    
    EEs05_h[t, parm] = diff05_h/delta
    EEs95_h[t, parm] = diff95_h/delta
    EEsot_h[t, parm] = diffot_h/delta
    
    EEsTN05_h[t, parm] = diffTN05_h/delta
    EEsTNMed_h[t, parm] = diffTNMed_h/delta
    EEsTN95_h[t, parm] = diffTN95_h/delta
  }
}

save.image(file = 'EEs.RData')

#Fixme: Some EEs have NA or Inf values
#For now, just setting those to NA
EEs05[is.infinite(EEs05)] = NA
EEs95[is.infinite(EEs95)] = NA
EEsot[is.infinite(EEsot)] = NA
EEs05[is.nan(EEs05)] = NA
EEs95[is.nan(EEs95)] = NA
EEsot[is.nan(EEsot)] = NA

#Fixme: Average the EEs for variables that require it

#Using the EE data, compute the mean, standard deviation, and absolute mean
muEEs05 = apply(X = EEs05, MARGIN = 2, FUN = mean, na.rm=TRUE)
muEEs95 = apply(X = EEs95, MARGIN = 2, FUN = mean, na.rm=TRUE)
muEEsot = apply(X = EEsot, MARGIN = 2, FUN = mean, na.rm=TRUE)

sdEEs05 = apply(X = EEs05, MARGIN = 2, FUN = sd, na.rm=TRUE)
sdEEs95 = apply(X = EEs95, MARGIN = 2, FUN = sd, na.rm=TRUE)
sdEEsot = apply(X = EEsot, MARGIN = 2, FUN = sd, na.rm=TRUE)

muaEEs05 = apply(X = abs(EEs05), MARGIN = 2, FUN = mean, na.rm=TRUE)
muaEEs95 = apply(X = abs(EEs95), MARGIN = 2, FUN = mean, na.rm=TRUE)
muaEEsot = apply(X = abs(EEsot), MARGIN = 2, FUN = mean, na.rm=TRUE)

#Make plots of the sd vs. mu
colos = rainbow(cols)
png('EE05_sdVsMu.png', res = 300, units = 'in', height = 5, width = 5)
plot(x = muEEs05, y = sdEEs05, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SSE for Lower 5th Percentile of Flow', col = colos)
dev.off()
png('EE95_sdVsMu.png', res = 300, units = 'in', height = 5, width = 5)
plot(x = muEEs95, y = sdEEs95, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SSE for Upper 5th Percentile of Flow', col = colos)
dev.off()
png('EEot_sdVsMu.png', res = 300, units = 'in', height = 5, width = 5)
plot(x = muEEsot, y = sdEEsot, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SSE for 5th-95th Percentile Flows', col = colos)
dev.off()

#Make plots of the ranking for mean absolute value
png('EE05_mua.png', res = 300, units = 'in', height = 5, width = 5)
barplot(height = muaEEs05, ylab = 'Mean Absolute Value of the Elementary Effect', xlab = 'Parameters', names.arg = seq(1,cols,1), main = 'Metric: SSE for Lower 5th Percentile of Flow')
dev.off()
png('EE95_mua.png', res = 300, units = 'in', height = 5, width = 5)
barplot(height = muaEEs95, ylab = 'Mean Absolute Value of the Elementary Effect', xlab = 'Parameters', names.arg = seq(1,cols,1), main = 'Metric: SSE for Upper 5th Percentile of Flow')
dev.off()
png('EEot_mua.png', res = 300, units = 'in', height = 5, width = 5)
barplot(height = muaEEsot, ylab = 'Mean Absolute Value of the Elementary Effect', xlab = 'Parameters', names.arg = seq(1,cols,1), main = 'Metric: SSE for 5th-95th Percentile Flows')
dev.off()

#Save a list of ordered names for mua
RanksMua05 = colnames(InputParams)[rev(order(muaEEs05))]
RanksMua95 = colnames(InputParams)[rev(order(muaEEs95))]
RanksMuaot = colnames(InputParams)[rev(order(muaEEsot))]

#Compute the correlation of parameters----
#Norm of a vector
norm_vec <- function(x){
  sqrt(sum(x^2))
}
#Matrix for the similarity measure
CosPhi05 = CosPhi95 = CosPhiot = matrix(NA, nrow = cols, ncol = cols)
for (i in 1:cols){
  for (j in 1:cols){
    CosPhi05[i,j] = abs(t(EEs05[,i]) %*% EEs05[,j])/norm_vec(EEs05[,i])/norm_vec(EEs05[,j])
    CosPhi95[i,j] = abs(t(EEs95[,i]) %*% EEs95[,j])/norm_vec(EEs95[,i])/norm_vec(EEs95[,j])
    CosPhiot[i,j] = abs(t(EEsot[,i]) %*% EEsot[,j])/norm_vec(EEsot[,i])/norm_vec(EEsot[,j])
  }
}

#Set NA values to 0
CosPhi05[is.na(CosPhi05)] = 0
CosPhi95[is.na(CosPhi95)] = 0
CosPhiot[is.na(CosPhiot)] = 0

colnames(CosPhi05) = colnames(OrigParams)
colnames(CosPhi95) = colnames(OrigParams)
colnames(CosPhiot) = colnames(OrigParams)
rownames(CosPhi05) = colnames(OrigParams)
rownames(CosPhi95) = colnames(OrigParams)
rownames(CosPhiot) = colnames(OrigParams)

#Heatmap
png('ParamCorrelations_EE05.png', res = 300, height = 8, width = 8, units = 'in')
heatmap(x = CosPhi05, na.rm = TRUE, symm = TRUE, revC = TRUE)
dev.off()

png('ParamCorrelations_EE95.png', res = 300, height = 8, width = 8, units = 'in')
heatmap(x = CosPhi95, na.rm = TRUE, symm = TRUE, revC = TRUE)
dev.off()

png('ParamCorrelations_EEot.png', res = 300, height = 8, width = 8, units = 'in')
heatmap(x = CosPhiot, na.rm = TRUE, symm = TRUE, revC = TRUE)
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
#(4913 and 4914) for v102_epc.frootlitr_fcel is Inf because the delta is 0 but change in values is not 0
#(5837 and 5838) for "s8_Ksat_0_v" is Inf because the delta is 0 but change in values is not 0
#(644 and 645) for "s109_Ksat_0_v" is NaN
#(5388 and 5389) for "s109_Ksat_0_v" is NaN

for (i in 1:r){
  j = which(Deltas[r,] == min(abs(Deltas)))
  if (length(j) > 0){
    print(j)
  }
}
