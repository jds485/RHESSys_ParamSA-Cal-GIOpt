library(sensitivity)

#Load Morris parameter file
InputParams = read.csv("C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs\\MorrisSampleLocs\\MorrisSamples_AfterProcessing.csv", stringsAsFactors = FALSE)
#Also need the original, unmodified Morris file to know which parameter changed in each trajectory iteration
OrigParams = read.csv("C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs\\MorrisSampleLocs\\MorrisSamples_BeforeProcessing.csv", stringsAsFactors = FALSE)
#And need the parameter ranges to scale the deltas
ParamRanges = read.csv('C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs\\MorrisSampleLocs\\BaismanMorrisSamplingProblemFile_Full.csv', stringsAsFactors = FALSE)

#Remove all of the parameters with _orig. They were not modified
InputParams = InputParams[-grep(x = colnames(InputParams), pattern = '_orig', fixed = TRUE)]
#Get the number of parameters
cols = ncol(InputParams)

#Number of trajectories
r = 40

#Load RHESSys streamflow data
setwd("C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR")
BasinSF = read.table(file = 'SAResults_BasinStreamflow_p4.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
#HillSF = read.table(file = 'SAResults_HillStreamflow_p6.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)

#Remove observations earlier than 10/01/2004 (SA timeperiod start)
BasinSF = BasinSF[, c(1, which(as.Date(colnames(BasinSF[,-1])) >= as.Date('2004-10-01')))]
#Order by replicate ID. This is done to ensure that the trajectories are in order
BasinSF = BasinSF[order(BasinSF$Replicate),]

#Load the observed streamflow record
obs = read.table("C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\obs\\BaismanStreamflow_Cal.txt", header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, sep = '\t')
#Remove observations later than 9/30/2010 (SA timeperiod end)
obs = obs[as.Date(obs$Date) <= as.Date(colnames(BasinSF)[ncol(BasinSF)]),]
#Remove observations earlier than 10/01/2004 (SA timeperiod start)
obs = obs[as.Date(obs$Date) >= as.Date('2004-10-01'),]

#Find the days with the highest 5th percentile, lowest 5th percentile and all other flows
q05 = quantile(x = obs$Flow, probs = 0.05)
q95 = quantile(x = obs$Flow, probs = 0.95)

obs05 = obs[obs$Flow <= q05,]
obs95 = obs[obs$Flow >= q95,]
days05 = as.Date(obs05$Date)
days95 = as.Date(obs95$Date)
obsot = obs[-which(as.Date(obs$Date) %in% c(days05, days95)),]
daysot = as.Date(obsot$Date)

#Check what days these correspond to - want a uniform distribution in time
png('PercentileFlowChecks.png', res = 300, units = 'in', width = 10, height = 5)
layout(rbind(c(1,2)))
hist(as.Date(days05), breaks = 30, freq = TRUE, axes = FALSE, xlim = c(as.Date('2004-10-01'), as.Date('2010-09-30')), main = 'Lower 5th Percentile Flow Dates')
axis(side = 1, at = as.Date(paste0(seq(2004,2011,1), '-01-01')), labels = format.Date(as.Date(paste0(seq(2004,2011,1), '-01-01')), "%Y"))
axis(side = 2)

hist(as.Date(days95), breaks = 30, freq = TRUE, axes = FALSE, xlim = c(as.Date('2004-10-01'), as.Date('2010-09-30')), main = 'Upper 5th Percentile Flow Dates')
axis(side = 1, at = as.Date(paste0(seq(2004,2011,1), '-01-01')), labels = format.Date(as.Date(paste0(seq(2004,2011,1), '-01-01')), "%Y"))
axis(side = 2)
dev.off()

hist(as.Date(daysot), breaks = 30, freq = TRUE, axes = FALSE, xlim = c(as.Date('2004-10-01'), as.Date('2010-09-30')), main = 'Lower 5th Percentile Flow Dates')
axis(side = 1, at = as.Date(paste0(seq(2004,2011,1), '-01-01')), labels = format.Date(as.Date(paste0(seq(2004,2011,1), '-01-01')), "%Y"))
axis(side = 2)

#Create the 05, 95, and other datasets
BasinSF05 = BasinSF[,-1][,which(as.Date(colnames(BasinSF[,-1])) %in% days05)]
#Fixme: The last date is not 9/30/2010. It is 9/29/2010
BasinSF95 = BasinSF[,-1][,which(as.Date(colnames(BasinSF[,-1])) %in% days95)]
BasinSFot = BasinSF[,-1][,which(as.Date(colnames(BasinSF[,-1])) %in% daysot)]

#Create a storage matrix for the EEs for each parameter
EEs05 = EEs95 = EEsot = matrix(NA, nrow = r, ncol = cols)
#Create a stoage matrix for the deltas
Deltas = matrix(NA, nrow = r, ncol = cols)
#Store the column names
colnms = colnames(BasinSF[,-1])
#Loop over the trajectories
for (t in 1:r){
  #Compute the EEs for all parameters in the trajectory
  for (i in 1:cols){
    ind = i+(1+cols)*(t-1)
    #Compute the difference for highest 5th percentile, lowest 5th percentile and all other flows, as defined by the days from the observed streamflow record
    #SSE
    diff05 = (sum((BasinSF05[ind+1,-1] - obs05$Flow)^2) - sum((BasinSF05[ind,-1] - obs05$Flow)^2))  
    diff95 = (sum((BasinSF95[ind+1,-1] - obs95$Flow)^2) - sum((BasinSF95[ind,-1] - obs95$Flow)^2))
    diffot = (sum((BasinSFot[ind+1,-1] - obsot$Flow)^2) - sum((BasinSFot[ind,-1] - obsot$Flow)^2))
    
    #Sum errors
    #diff05 = (sum((BasinSF05[ind+1,-1] - obs05$Flow)) - sum((BasinSF05[ind,-1] - obs05$Flow)))  
    #diff95 = (sum((BasinSF95[ind+1,-1] - obs95$Flow)) - sum((BasinSF95[ind,-1] - obs95$Flow)))
    #diffot = (sum((BasinSFot[ind+1,-1] - obsot$Flow)) - sum((BasinSFot[ind,-1] - obsot$Flow)))
    
    #Find the parameter column that was changed, before any processing was completed. 
    #This is the same column that will be used for the Deltas and EEs
    parm = which((OrigParams[ind+1,] - OrigParams[ind,]) != 0)
    #Computes the exact delta from the modified sampling file
    delta = InputParams[ind+1,parm] - InputParams[ind,parm]
    #Adjust delta to the range sampled for the parameter
    delta = delta/(ParamRanges$Upper[parm] - ParamRanges$Lower[parm])
    Deltas[t, parm] = delta  
    #Fixme: records deltas for every variable, not only the parameter that changed
    #delta for other parameters could be larger than the OAT 	parameter that was supposed to change
    
    #Computes EEs with specified metric
    EEs05[t, parm] = diff05/delta
    EEs95[t, parm] = diff95/delta
    EEsot[t, parm] = diffot/delta
  }
}

save.image(file = 'EEs.RData')

#Fixme: Average the EEs for variables that require it

#Using the EE data, compute the mean, standard deviation, and absolute mean
muEEs05 = apply(X = EEs05, MARGIN = 2, FUN = mean)
muEEs95 = apply(X = EEs95, MARGIN = 2, FUN = mean)
muEEsot = apply(X = EEsot, MARGIN = 2, FUN = mean)

sdEEs05 = apply(X = EEs05, MARGIN = 2, FUN = sd)
sdEEs95 = apply(X = EEs95, MARGIN = 2, FUN = sd)
sdEEsot = apply(X = EEsot, MARGIN = 2, FUN = sd)

muaEEs05 = apply(X = abs(EEs05), MARGIN = 2, FUN = mean)
muaEEs95 = apply(X = abs(EEs95), MARGIN = 2, FUN = mean)
muaEEsot = apply(X = abs(EEsot), MARGIN = 2, FUN = mean)

#Make plots of the sd vs. mu
cols = rainbow(cols)
png('EE05_sdVsMu.png', res = 300, units = 'in', height = 5, width = 5)
plot(x = muEEs05, y = sdEEs05, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SSE for Lower 5th Percentile of Flow', col = cols)
dev.off()
png('EE95_sdVsMu.png', res = 300, units = 'in', height = 5, width = 5)
plot(x = muEEs95, y = sdEEs95, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SSE for Upper 5th Percentile of Flow', col = cols)
dev.off()
png('EEot_sdVsMu.png', res = 300, units = 'in', height = 5, width = 5)
plot(x = muEEsot, y = sdEEsot, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SSE for 5th-95th Percentile Flows', col = cols)
dev.off()

#Make plots of the ranking for mean absolute value
png('EE05_mua.png', res = 300, units = 'in', height = 5, width = 5)
barplot(height = muaEEs05, ylab = 'Mean Absolute Value of the Elementary Effect', xlab = 'Parameters', names.arg = seq(1,cols,1), main = 'Metric: SSE for Lower 5th Percentile of Flow')
dev.off()
png('EE95_mua.png', res = 300, units = 'in', height = 5, width = 5)
plot(x = muaEEs95, y = sdEEs95, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SSE for Upper 5th Percentile of Flow')
dev.off()
png('EEot_mua.png', res = 300, units = 'in', height = 5, width = 5)
plot(x = muaEEsot, y = sdEEsot, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SSE for 5th-95th Percentile Flows')
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

#Perform heirarchical clustering for discovering variable clusters
#https://uc-r.github.io/hc_clustering

#List the variables that are highly similar (closer to 1)

#Computes bootstrapped samples with replacement of the EEs for each parameter (separately as opposed to by trajectory to avoid correlation in SDs of bootstrapped results)


#Compare EEs for multiplier variables


#Parameter corrleation plots
#sensitivity::pcc()
#sensitivity::plot3d.morris()
#?sensitivity::morrisMultOut()
#?sensitivity::morris()

#Show SA metrics for the basin and for each hillslope - ranks, and maps for hillslope----