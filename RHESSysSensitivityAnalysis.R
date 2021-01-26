#Script for calculating Morris EEs from output streamflow and TN data

#Set directories----
#Color functions - from JDS github repo: Geothermal_ESDA
dir_ColFuns = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\Hydrology\\USGSGauges"

#Load libraries----
library(vroom)
library(cluster)
library(factoextra)
library(dendextend)
library(tidyverse)
library(scico)

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

#   Paper figure----
q05 = quantile(x = obs$Flow, probs = 0.05)
q95 = quantile(x = obs$Flow, probs = 0.95)

obs05 = obs[obs$Flow <= q05,]
obs95 = obs[obs$Flow >= q95,]
days05 = as.Date(obs05$Date)
days95 = as.Date(obs95$Date)
obsot = obs[-which(as.Date(obs$Date) %in% c(days05, days95)),]
daysot = as.Date(obsot$Date)

pdf(file = 'figA1.pdf', width = 6, height = 6, colormodel = 'gray')
layout(rbind(c(1,2,3), c(4,5,6), c(7,8,9)))
par(mar=c(3,3,5,0.5))
hist(as.Date(days05), breaks = seq(as.Date('2004-10-01'), as.Date('2010-09-30'), length.out = 30), freq = TRUE, axes = FALSE, xlim = c(as.Date('2004-10-01'), as.Date('2010-09-30')), main = '', xlab = '', ylab = '', cex.axis = 0.5, cex.lab = 0.5, cex.main = 0.7, ylim = c(0,100))
mtext('Lower 5th Quantile Flow Dates', side = 3, line = 1, cex = 0.5)
mtext('Year', side = 1, line = 2, cex = 0.5)
mtext('Frequency', side = 2, line = 2, cex = 0.5)
axis(side = 1, at = as.Date(paste0(seq(2004,2011,1), '-01-01')), labels = format.Date(as.Date(paste0(seq(2004,2011,1), '-01-01')), "%Y"), cex.axis = 0.5, padj = -1)
axis(side = 2, cex.axis = 0.5)

hist(as.Date(daysot), breaks = seq(as.Date('2004-10-01'), as.Date('2010-09-30'), length.out = 30), freq = TRUE, axes = FALSE, xlim = c(as.Date('2004-10-01'), as.Date('2010-09-30')), main = '', xlab = '', ylab = '', cex.axis = 0.5, cex.lab = 0.5, cex.main = 0.7, ylim = c(0,100))
mtext('5th to 95th Quantile Flow Dates', side = 3, line = 1, cex = 0.5)
mtext('Year', side = 1, line = 2, cex = 0.5)
mtext('Frequency', side = 2, line = 2, cex = 0.5)
axis(side = 1, at = as.Date(paste0(seq(2004,2011,1), '-01-01')), labels = format.Date(as.Date(paste0(seq(2004,2011,1), '-01-01')), "%Y"), cex.axis = 0.5, padj = -1)
axis(side = 2, cex.axis = 0.5)

mtext(side = 3, text = expression(bold('Quantiles: Full Record')), line = 3)

hist(as.Date(days95), breaks = seq(as.Date('2004-10-01'), as.Date('2010-09-30'), length.out = 30), freq = TRUE, axes = FALSE, xlim = c(as.Date('2004-10-01'), as.Date('2010-09-30')), main = '', xlab = '', ylab = '', cex.axis = 0.5, cex.lab = 0.5, cex.main = 0.7, ylim = c(0,100))
mtext('Upper 5th Quantile Flow Dates', side = 3, line = 1, cex = 0.5)
mtext('Year', side = 1, line = 2, cex = 0.5)
mtext('Frequency', side = 2, line = 2, cex = 0.5)
axis(side = 1, at = as.Date(paste0(seq(2004,2011,1), '-01-01')), labels = format.Date(as.Date(paste0(seq(2004,2011,1), '-01-01')), "%Y"), cex.axis = 0.5, padj = -1)
axis(side = 2, cex.axis = 0.5)

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

hist(as.Date(days05), breaks = seq(as.Date('2004-10-01'), as.Date('2010-09-30'), length.out = 30), freq = TRUE, axes = FALSE, xlim = c(as.Date('2004-10-01'), as.Date('2010-09-30')), main = '', xlab = '', ylab = '', cex.axis = 0.5, cex.lab = 0.5, cex.main = 0.7, ylim = c(0,100))
mtext('Lower 5th Quantile Flow Dates', side = 3, line = 1, cex = 0.5)
mtext('Year', side = 1, line = 2, cex = 0.5)
mtext('Frequency', side = 2, line = 2, cex = 0.5)
axis(side = 1, at = as.Date(paste0(seq(2004,2011,1), '-01-01')), labels = format.Date(as.Date(paste0(seq(2004,2011,1), '-01-01')), "%Y"), cex.axis = 0.5, padj = -1)
axis(side = 2, cex.axis = 0.5)

hist(as.Date(daysot), breaks = seq(as.Date('2004-10-01'), as.Date('2010-09-30'), length.out = 30), freq = TRUE, axes = FALSE, xlim = c(as.Date('2004-10-01'), as.Date('2010-09-30')), main = '', xlab = '', ylab = '', cex.axis = 0.5, cex.lab = 0.5, cex.main = 0.7, ylim = c(0,100))
mtext('5th to 95th Quantile Flow Dates', side = 3, line = 1, cex = 0.5)
mtext('Year', side = 1, line = 2, cex = 0.5)
mtext('Frequency', side = 2, line = 2, cex = 0.5)
axis(side = 1, at = as.Date(paste0(seq(2004,2011,1), '-01-01')), labels = format.Date(as.Date(paste0(seq(2004,2011,1), '-01-01')), "%Y"), cex.axis = 0.5, padj = -1)
axis(side = 2, cex.axis = 0.5)

mtext(side = 3, text = expression(bold('Quantiles: Annually')), line = 3)

hist(as.Date(days95), breaks = seq(as.Date('2004-10-01'), as.Date('2010-09-30'), length.out = 30), freq = TRUE, axes = FALSE, xlim = c(as.Date('2004-10-01'), as.Date('2010-09-30')), main = '', xlab = '', ylab = '', cex.axis = 0.5, cex.lab = 0.5, cex.main = 0.7, ylim = c(0,100))
mtext('Upper 5th Quantile Flow Dates', side = 3, line = 1, cex = 0.5)
mtext('Year', side = 1, line = 2, cex = 0.5)
mtext('Frequency', side = 2, line = 2, cex = 0.5)
axis(side = 1, at = as.Date(paste0(seq(2004,2011,1), '-01-01')), labels = format.Date(as.Date(paste0(seq(2004,2011,1), '-01-01')), "%Y"), cex.axis = 0.5, padj = -1)
axis(side = 2, cex.axis = 0.5)

IndsNewYear = c(colnames(BasinSF)[grep(colnames(BasinSF),pattern = '-10-01', fixed=TRUE)], colnames(BasinSF)[ncol(BasinSF)])
q95 = quantile(x = obs$Flow, probs = 0.95)
obs95 = obs[obs$Flow >= q95,]
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

hist(as.Date(days05), breaks = seq(as.Date('2004-10-01'), as.Date('2010-09-30'), length.out = 30), freq = TRUE, axes = FALSE, xlim = c(as.Date('2004-10-01'), as.Date('2010-09-30')), main = '', xlab = '', ylab = '', cex.axis = 0.5, cex.lab = 0.5, cex.main = 0.7, ylim = c(0,100))
mtext('Lower 5th Quantile Flow Dates', side = 3, line = 1, cex = 0.5)
mtext('Year', side = 1, line = 2, cex = 0.5)
mtext('Frequency', side = 2, line = 2, cex = 0.5)
axis(side = 1, at = as.Date(paste0(seq(2004,2011,1), '-01-01')), labels = format.Date(as.Date(paste0(seq(2004,2011,1), '-01-01')), "%Y"), cex.axis = 0.5, padj = -1)
axis(side = 2, cex.axis = 0.5)

hist(as.Date(daysot), breaks = seq(as.Date('2004-10-01'), as.Date('2010-09-30'), length.out = 30), freq = TRUE, axes = FALSE, xlim = c(as.Date('2004-10-01'), as.Date('2010-09-30')), main = '', xlab = '', ylab = '', cex.axis = 0.5, cex.lab = 0.5, cex.main = 0.7, ylim = c(0,100))
mtext('5th to 95th Quantile Flow Dates', side = 3, line = 1, cex = 0.5)
mtext('Year', side = 1, line = 2, cex = 0.5)
mtext('Frequency', side = 2, line = 2, cex = 0.5)
axis(side = 1, at = as.Date(paste0(seq(2004,2011,1), '-01-01')), labels = format.Date(as.Date(paste0(seq(2004,2011,1), '-01-01')), "%Y"), cex.axis = 0.5, padj = -1)
axis(side = 2, cex.axis = 0.5)

mtext(side = 3, text = expression(bold('Quantiles: Preferred')), line = 3)

hist(as.Date(days95), breaks = seq(as.Date('2004-10-01'), as.Date('2010-09-30'), length.out = 30), freq = TRUE, axes = FALSE, xlim = c(as.Date('2004-10-01'), as.Date('2010-09-30')), main = '', xlab = '', ylab = '', cex.axis = 0.5, cex.lab = 0.5, cex.main = 0.7, ylim = c(0,100))
mtext('Upper 5th Quantile Flow Dates', side = 3, line = 1, cex = 0.5)
mtext('Year', side = 1, line = 2, cex = 0.5)
mtext('Frequency', side = 2, line = 2, cex = 0.5)
axis(side = 1, at = as.Date(paste0(seq(2004,2011,1), '-01-01')), labels = format.Date(as.Date(paste0(seq(2004,2011,1), '-01-01')), "%Y"), cex.axis = 0.5, padj = -1)
axis(side = 2, cex.axis = 0.5)
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
#These are not calculated correctly. All shifted off by 1 day. But maybe it doesn't matter because the baseline is constant and it's absolute error.
MedHills05 = MedHills[,c(1, which(as.Date(colnames(MedHills)[-1]) %in% as.Date(days05)))]
MedHills95 = MedHills[,c(1, which(as.Date(colnames(MedHills)[-1]) %in% as.Date(days95)))]
MedHillsot = MedHills[,c(1, which(as.Date(colnames(MedHills)[-1]) %in% as.Date(daysot)))]
#This is the fixed method
#MedHills05 = MedHills[,c(1, 1+which(as.Date(colnames(MedHills)[-1]) %in% as.Date(days05)))]
#MedHills95 = MedHills[,c(1, 1+which(as.Date(colnames(MedHills)[-1]) %in% as.Date(days95)))]
#MedHillsot = MedHills[,c(1, 1+which(as.Date(colnames(MedHills)[-1]) %in% as.Date(daysot)))]
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

# Load EE and Deltas info----
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
if (any(c(any(is.na(EEs05_b)),
any(is.na(EEs95_b)),
any(is.na(EEsot_b)),
any(is.na(EEsTN05_b)),
any(is.na(EEsTN95_b)),
any(is.na(EEsTNMed_b)),
any(is.na(EEs05_h)),
any(is.na(EEs95_h)),
any(is.na(EEsot_h)),
any(is.na(EEsTN05_h)),
any(is.na(EEsTN95_h)),
any(is.na(EEsTNMed_h))))){
  print('There are NA Values in EE tables. Please fix.')
}

#Corrections are needed to v102_epc.frootlitr_fcel to correct a numerical issue
EEs05_b$v102_epc.frootlitr_fcel[c(13,19)] = 0
EEsot_b$v102_epc.frootlitr_fcel[c(13,19)] = 0
EEs95_b$v102_epc.frootlitr_fcel[c(13,19)] = 0
EEsTN05_b$v102_epc.frootlitr_fcel[c(13,19)] = 0
EEsTNMed_b$v102_epc.frootlitr_fcel[c(13,19)] = 0
EEsTN95_b$v102_epc.frootlitr_fcel[c(13,19)] = 0

EEs05_h$v102_epc.frootlitr_fcel[c(12*14+seq(1,14,1),14*18+seq(1,14,1))] = 0
EEsot_h$v102_epc.frootlitr_fcel[c(12*14+seq(1,14,1),14*18+seq(1,14,1))] = 0
EEs95_h$v102_epc.frootlitr_fcel[c(12*14+seq(1,14,1),14*18+seq(1,14,1))] = 0
EEsTN05_h$v102_epc.frootlitr_fcel[c(12*14+seq(1,14,1),14*18+seq(1,14,1))] = 0
EEsTNMed_h$v102_epc.frootlitr_fcel[c(12*14+seq(1,14,1),14*18+seq(1,14,1))] = 0
EEsTN95_h$v102_epc.frootlitr_fcel[c(12*14+seq(1,14,1),14*18+seq(1,14,1))] = 0

#Aggregate the EEs for variables that require it----
#Fixme: move to a function file
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

# Make a vector of the column names that will be aggregated---- 
#Later, they will be removed from the dataframe.
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
#Indicator key for the aggregated name.
ColsAggregated_key = c('s8_SoilTexture', 's8_SoilTexture', 's8_SoilTexture', 's108_SoilTexture', 's108_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's9_SoilTexture', 's9_SoilTexture', 's109_SoilTexture', 's109_SoilTexture', 's109_SoilTexture',
                      'v102_K_All', 'v102_K_All', 'v102_K_All',
                      'v102_PAR_All', 'v102_PAR_All', 'v102_PAR_All',
                      'v102_epc.frootlitr_All', 'v102_epc.frootlitr_All', 'v102_epc.frootlitr_All', 'v3_epc.frootlitr_All', 'v3_epc.frootlitr_All', 'v3_epc.frootlitr_All',
                      'v102_epc.leaflitr_All', 'v102_epc.leaflitr_All', 'v102_epc.leaflitr_All', 'v3_epc.leaflitr_All', 'v3_epc.leaflitr_All', 'v3_epc.leaflitr_All',
                      'Soil8_Ksat', 'Soil8_Ksat', 'Soil8_Ksat', 'Soil8_Ksat', 'Soil9_Ksat', 'Soil9_Ksat', 'Soil9_Ksat', 'Soil9_Ksat',
                      'Soil8_m', 'Soil8_m', 'Soil9_m', 'Soil9_m',
                      'Soil8_porosity_0', 'Soil8_porosity_0', 'Soil9_porosity_0', 'Soil9_porosity_0',
                      'v102_Temp', 'v102_Temp', 'v3_Temp', 'v3_Temp',
                      'v102_epc.LeafLitrCN', 'v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

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
  #Extract the EEs for that parameter using the indices in tReps.
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
  #If the variable p is one of the ones that is used to aggregate, save the bootstrapped EEs array for aggregation later
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
    rm(indsh, h, indp)
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
#Name the columns
colnames(EEs05_h_mua_m) = colnames(EEs95_h_mua_m) = colnames(EEsot_h_mua_m) = colnames(EEsTN05_h_mua_m) = colnames(EEsTN95_h_mua_m) = colnames(EEsTNMed_h_mua_m) = c('HillID', names(EEs05_b_mua_m))
colnames(EEs05_h_mua_05) = colnames(EEs95_h_mua_05) = colnames(EEsot_h_mua_05) = colnames(EEsTN05_h_mua_05) = colnames(EEsTN95_h_mua_05) = colnames(EEsTNMed_h_mua_05) = c('HillID', names(EEs05_b_mua_05))
colnames(EEs05_h_mua_95) = colnames(EEs95_h_mua_95) = colnames(EEsot_h_mua_95) = colnames(EEsTN05_h_mua_95) = colnames(EEsTN95_h_mua_95) = colnames(EEsTNMed_h_mua_95) = c('HillID', names(EEs05_b_mua_95))

# Make plots of the distributions for variables----
#Fixme: currently only plotting for one of the 6 metrics. Not sure it's necessary to show more.
for (p in 1:ncol(EEs05_b_mua)){
  png(paste0(getwd(), '/EEs05_b_muaDists/EEs05_b_mua_', colnames(EEs05_b_mua)[p], '.png'), res = 300, units = 'in', width = 5, height = 5)
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
rm(inds, h)

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
  RanksMua95_h = c(RanksMua95_h, list(data.frame(Param = colnames(InputParams)[rev(order(muaEEs95_h[h,]))], EE95_h = muaEEs95_h[h,][rev(order(muaEEs95_h[h,]))], stringsAsFactors = FALSE)))
  RanksMuaot_h = c(RanksMuaot_h, list(data.frame(Param = colnames(InputParams)[rev(order(muaEEsot_h[h,]))], EEot_h = muaEEsot_h[h,][rev(order(muaEEsot_h[h,]))], stringsAsFactors = FALSE)))
  RanksMuaTN05_h = c(RanksMuaTN05_h, list(data.frame(Param = colnames(InputParams)[rev(order(muaEEsTN05_h[h,]))], EETN05_h = muaEEsTN05_h[h,][rev(order(muaEEsTN05_h[h,]))], stringsAsFactors = FALSE)))
  RanksMuaTN95_h = c(RanksMuaTN95_h, list(data.frame(Param = colnames(InputParams)[rev(order(muaEEsTN95_h[h,]))], EETN95_h = muaEEsTN95_h[h,][rev(order(muaEEsTN95_h[h,]))], stringsAsFactors = FALSE)))
  RanksMuaTNMed_h = c(RanksMuaTNMed_h, list(data.frame(Param = colnames(InputParams)[rev(order(muaEEsTNMed_h[h,]))], EETNMed_h = muaEEsTNMed_h[h,][rev(order(muaEEsTNMed_h[h,]))], stringsAsFactors = FALSE)))
}
names(RanksMua05_h) = names(RanksMua95_h) = names(RanksMuaot_h) = names(RanksMuaTN05_h) = names(RanksMuaTN95_h) = names(RanksMuaTNMed_h) = paste0('Hill', seq(1,14,1))
rm(h)

# Using Aggregated variables----
RanksMua05_b_Agg = data.frame(Param = names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)])[order(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)],decreasing = TRUE)], EE05_b = as.numeric(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), stringsAsFactors = FALSE)
RanksMua95_b_Agg = data.frame(Param = names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])[order(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)],decreasing = TRUE)], EE95_b = as.numeric(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), stringsAsFactors = FALSE)
RanksMuaot_b_Agg = data.frame(Param = names(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)])[order(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)],decreasing = TRUE)], EEot_b = as.numeric(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][order(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), stringsAsFactors = FALSE)
RanksMuaTN05_b_Agg = data.frame(Param = names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])[order(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)],decreasing = TRUE)], EETN05_b = as.numeric(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), stringsAsFactors = FALSE)
RanksMuaTN95_b_Agg = data.frame(Param = names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])[order(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)],decreasing = TRUE)], EETN95_b = as.numeric(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), stringsAsFactors = FALSE)
RanksMuaTNMed_b_Agg = data.frame(Param = names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])[order(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)],decreasing = TRUE)], EETNMed_b = as.numeric(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), stringsAsFactors = FALSE)

RanksMua05_h_Agg = RanksMua95_h_Agg = RanksMuaot_h_Agg = RanksMuaTN05_h_Agg = RanksMuaTN95_h_Agg = RanksMuaTNMed_h_Agg = list()
for (h in 1:length(uhills)){
  RanksMua05_h_Agg = c(RanksMua05_h_Agg, list(data.frame(Param = colnames(EEs05_h_mua_m)[-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))][order(EEs05_h_mua_m[h, -c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)], EE05_h = as.numeric(EEs05_h_mua_m[h, -c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))][order(EEs05_h_mua_m[h, -c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)]), stringsAsFactors = FALSE)))
  RanksMua95_h_Agg = c(RanksMua95_h_Agg, list(data.frame(Param = colnames(EEs95_h_mua_m)[-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))][order(EEs95_h_mua_m[h, -c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)], EE95_h = as.numeric(EEs95_h_mua_m[h, -c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))][order(EEs95_h_mua_m[h, -c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)]), stringsAsFactors = FALSE)))
  RanksMuaot_h_Agg = c(RanksMuaot_h_Agg, list(data.frame(Param = colnames(EEsot_h_mua_m)[-c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))][order(EEsot_h_mua_m[h, -c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)], EEot_h = as.numeric(EEsot_h_mua_m[h, -c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))][order(EEsot_h_mua_m[h, -c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)]), stringsAsFactors = FALSE)))
  RanksMuaTN05_h_Agg = c(RanksMuaTN05_h_Agg, list(data.frame(Param = colnames(EEsTN05_h_mua_m)[-c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))][order(EEsTN05_h_mua_m[h, -c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)], EETN05_h = as.numeric(EEsTN05_h_mua_m[h, -c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))][order(EEsTN05_h_mua_m[h, -c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)]), stringsAsFactors = FALSE)))
  RanksMuaTN95_h_Agg = c(RanksMuaTN95_h_Agg, list(data.frame(Param = colnames(EEsTN95_h_mua_m)[-c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))][order(EEsTN95_h_mua_m[h, -c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)], EETN95_h = as.numeric(EEsTN95_h_mua_m[h, -c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))][order(EEsTN95_h_mua_m[h, -c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)]), stringsAsFactors = FALSE)))
  RanksMuaTNMed_h_Agg = c(RanksMuaTNMed_h_Agg, list(data.frame(Param = colnames(EEsTNMed_h_mua_m)[-c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))][order(EEsTNMed_h_mua_m[h, -c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)], EETNMed_h = as.numeric(EEsTNMed_h_mua_m[h, -c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))][order(EEsTNMed_h_mua_m[h, -c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)]), stringsAsFactors = FALSE)))
}
names(RanksMua05_h_Agg) = names(RanksMua95_h_Agg) = names(RanksMuaot_h_Agg) = names(RanksMuaTN05_h_Agg) = names(RanksMuaTN95_h_Agg) = names(RanksMuaTNMed_h_Agg) = paste0('Hill', seq(1,14,1))
rm(h)

#Paper SI table info with confidence intervals added----
RanksMua05_b_AggCI = cbind(RanksMua05_b_Agg, data.frame(P05 = as.numeric(EEs05_b_mua_05[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), P95 = as.numeric(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)])))
RanksMuaot_b_AggCI = cbind(RanksMuaot_b_Agg, data.frame(P05 = as.numeric(EEsot_b_mua_05[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][order(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), P95 = as.numeric(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][order(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)])))
RanksMua95_b_AggCI = cbind(RanksMua95_b_Agg, data.frame(P05 = as.numeric(EEs95_b_mua_05[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), P95 = as.numeric(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)])))
RanksMuaTN05_b_AggCI = cbind(RanksMuaTN05_b_Agg, data.frame(P05 = as.numeric(EEsTN05_b_mua_05[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), P95 = as.numeric(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)])))
RanksMuaTNMed_b_AggCI = cbind(RanksMuaTNMed_b_Agg, data.frame(P05 = as.numeric(EEsTNMed_b_mua_05[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), P95 = as.numeric(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)])))
RanksMuaTN95_b_AggCI = cbind(RanksMuaTN95_b_Agg, data.frame(P05 = as.numeric(EEsTN95_b_mua_05[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), P95 = as.numeric(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)])))

RanksMua05_b_AggCI[,c(2,3,4)] = signif(RanksMua05_b_AggCI[,c(2,3,4)], 3)
RanksMuaot_b_AggCI[,c(2,3,4)] = signif(RanksMuaot_b_AggCI[,c(2,3,4)], 3)
RanksMua95_b_AggCI[,c(2,3,4)] = signif(RanksMua95_b_AggCI[,c(2,3,4)], 3)
RanksMuaTN05_b_AggCI[,c(2,3,4)] = signif(RanksMuaTN05_b_AggCI[,c(2,3,4)], 3)
RanksMuaTNMed_b_AggCI[,c(2,3,4)] = signif(RanksMuaTNMed_b_AggCI[,c(2,3,4)], 3)
RanksMuaTN95_b_AggCI[,c(2,3,4)] = signif(RanksMuaTN95_b_AggCI[,c(2,3,4)], 3)

#In order of parameters in Fig 1
Fig1Order = names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)])
RanksMua05_b_AggCI_FigOrder = RanksMua05_b_AggCI[match(table = RanksMua05_b_AggCI$Param, x = Fig1Order),] 
RanksMuaot_b_AggCI_FigOrder = RanksMuaot_b_AggCI[match(table = RanksMuaot_b_AggCI$Param, x = Fig1Order),]
RanksMua95_b_AggCI_FigOrder = RanksMua95_b_AggCI[match(table = RanksMua95_b_AggCI$Param, x = Fig1Order),]
RanksMuaTN05_b_AggCI_FigOrder = RanksMuaTN05_b_AggCI[match(table = RanksMuaTN05_b_AggCI$Param, x = Fig1Order),]
RanksMuaTNMed_b_AggCI_FigOrder = RanksMuaTNMed_b_AggCI[match(table = RanksMuaTNMed_b_AggCI$Param, x = Fig1Order),]
RanksMuaTN95_b_AggCI_FigOrder = RanksMuaTN95_b_AggCI[match(table = RanksMuaTN95_b_AggCI$Param, x = Fig1Order),]

#Get the top X% unique variables based on the flow and TN metrics----
#Top 10% of the parameters is desired. Get the index of the 10% of non-zero EEs, rounded up
#Note that one could use the bootstrapped mean or the original mean. 
#Maximum difference in EE value:
MaxDiffEEs05Boot = max(abs(muaEEs05_b/max(muaEEs05_b) - EEs05_b_mua_m[1:271]/max(EEs05_b_mua_m[1:271])))
MaxDiffEEsotBoot = max(abs(muaEEsot_b/max(muaEEsot_b) - EEsot_b_mua_m[1:271]/max(EEsot_b_mua_m[1:271])))
MaxDiffEEs95Boot = max(abs(muaEEs95_b/max(muaEEs95_b) - EEs95_b_mua_m[1:271]/max(EEs95_b_mua_m[1:271])))
MaxDiffEEsTN05Boot = max(abs(muaEEsTN05_b/max(muaEEsTN05_b) - EEsTN05_b_mua_m[1:271]/max(EEsTN05_b_mua_m[1:271])))
MaxDiffEEsTNMedBoot = max(abs(muaEEsTNMed_b/max(muaEEsTNMed_b) - EEsTNMed_b_mua_m[1:271]/max(EEsTNMed_b_mua_m[1:271])))
MaxDiffEEsTN95Boot = max(abs(muaEEsTN95_b/max(muaEEsTN95_b) - EEsTN95_b_mua_m[1:271]/max(EEsTN95_b_mua_m[1:271])))

Top1005 = ceiling((length(muaEEs05_b) - length(which(muaEEs05_b == 0)))*0.1)
Top10ot = ceiling((length(muaEEsot_b) - length(which(muaEEsot_b == 0)))*0.1)
Top1095 = ceiling((length(muaEEs95_b) - length(which(muaEEs95_b == 0)))*0.1)
Top10TN05 = ceiling((length(muaEEsTN05_b) - length(which(muaEEsTN05_b == 0)))*0.1)
Top10TNMed = ceiling((length(muaEEsTNMed_b) - length(which(muaEEsTNMed_b == 0)))*0.1)
Top10TN95 = ceiling((length(muaEEsTN95_b) - length(which(muaEEsTN95_b == 0)))*0.1)

RanksMua_b = unique(c(RanksMua05_b$Param[1:Top1005],RanksMuaot_b$Param[1:Top10ot],RanksMua95_b$Param[1:Top1095]))
RanksMuaTN_b = unique(c(RanksMuaTN05_b$Param[1:Top10TN05],RanksMuaTNMed_b$Param[1:Top10TNMed],RanksMuaTN95_b$Param[1:Top10TN95]))

#Unique across basin and TN are same for top 10%
length(unique(c(RanksMua_b, RanksMuaTN_b)))

# Select all parameters whose 95th quantile estimate of EE is greater than the selected threshold point
ParamSelect_b = unique(c(names(EEs05_b_mua_95[1:nrow(ParamRanges)][EEs05_b_mua_95[1:nrow(ParamRanges)] >= RanksMua05_b$EE05_b[Top1005]]),
                         names(EEsot_b_mua_95[1:nrow(ParamRanges)][EEsot_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaot_b$EEot_b[Top10ot]]),
                         names(EEs95_b_mua_95[1:nrow(ParamRanges)][EEs95_b_mua_95[1:nrow(ParamRanges)] >= RanksMua95_b$EE95_b[Top1095]])))
ParamSelectTN_b = unique(c(names(EEsTN05_b_mua_95[1:nrow(ParamRanges)][EEsTN05_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaTN05_b$EETN05_b[Top10TN05]]),
                         names(EEsTNMed_b_mua_95[1:nrow(ParamRanges)][EEsTNMed_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaTNMed_b$EETNMed_b[Top10TNMed]]),
                         names(EEsTN95_b_mua_95[1:nrow(ParamRanges)][EEsTN95_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaTN95_b$EETN95_b[Top10TN95]])))

#Some differences using this method
length(unique(c(ParamSelect_b, ParamSelectTN_b)))

# Same, but dropping the aggregated parameters----
Top1005_Agg = ceiling((length(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*0.1)
Top10ot_Agg = ceiling((length(EEsot_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsot_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*0.1)
Top1095_Agg = ceiling((length(EEs95_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEs95_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*0.1)
Top10TN05_Agg = ceiling((length(EEsTN05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsTN05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*0.1)
Top10TNMed_Agg = ceiling((length(EEsTNMed_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsTNMed_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*0.1)
Top10TN95_Agg = ceiling((length(EEsTN95_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsTN95_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*0.1)

RanksMua_b_Agg = unique(c(RanksMua05_b_Agg$Param[1:Top1005_Agg],RanksMuaot_b_Agg$Param[1:Top10ot_Agg],RanksMua95_b_Agg$Param[1:Top1095_Agg]))
RanksMuaTN_b_Agg = unique(c(RanksMuaTN05_b_Agg$Param[1:Top10TN05_Agg],RanksMuaTNMed_b_Agg$Param[1:Top10TNMed_Agg],RanksMuaTN95_b_Agg$Param[1:Top10TN95_Agg]))

# Select all parameters whose 95th quantile estimate of EE is greater than the selected threshold point
ParamSelect_b_Agg = unique(c(names(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] >= RanksMua05_b_Agg$EE05_b[Top1005_Agg]]),
                         names(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)] >= RanksMuaot_b_Agg$EEot_b[Top10ot_Agg]]),
                         names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)] >= RanksMua95_b_Agg$EE95_b[Top1095_Agg]])))
ParamSelectTN_b_Agg = unique(c(names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)] >= RanksMuaTN05_b_Agg$EETN05_b[Top10TN05_Agg]]),
                           names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)] >= RanksMuaTNMed_b_Agg$EETNMed_b[Top10TNMed_Agg]]),
                           names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)] >= RanksMuaTN95_b_Agg$EETN95_b[Top10TN95_Agg]])))

length(unique(c(ParamSelect_b_Agg, ParamSelectTN_b_Agg)))

#  Hillslope based selection of parameters----
RanksMua_h_Agg = RanksMuaTN_h_Agg = NULL
for (h in 1:length(uhills)){
  Top10h05_Agg = ceiling((length(EEs05_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEs05_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*0.1)
  Top10hot_Agg = ceiling((length(EEsot_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEsot_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*0.1)
  Top10h95_Agg = ceiling((length(EEs95_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEs95_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*0.1)
  Top10hTN05_Agg = ceiling((length(EEsTN05_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEsTN05_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*0.1)
  Top10hTNMed_Agg = ceiling((length(EEsTNMed_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEsTNMed_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*0.1)
  Top10hTN95_Agg = ceiling((length(EEsTN95_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEsTN95_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*0.1)
  
  RanksMua_h_Agg = unique(c(RanksMua_h_Agg, RanksMua05_h_Agg[[h]]$Param[1:Top10h05_Agg],RanksMuaot_h_Agg[[h]]$Param[1:Top10hot_Agg],RanksMua95_h_Agg[[h]]$Param[1:Top10h95_Agg]))
  RanksMuaTN_h_Agg = unique(c(RanksMuaTN_h_Agg, RanksMuaTN05_h_Agg[[h]]$Param[1:Top10hTN05_Agg],RanksMuaTNMed_h_Agg[[h]]$Param[1:Top10hTNMed_Agg],RanksMuaTN95_h_Agg[[h]]$Param[1:Top10hTN95_Agg]))
}
rm(h)

RanksMua_h_Agg910 = NULL
for (h in 9:10){
  Top10h05_Agg = ceiling((length(EEs05_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEs05_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*0.1)
  Top10hot_Agg = ceiling((length(EEsot_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEsot_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*0.1)
  Top10h95_Agg = ceiling((length(EEs95_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEs95_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*0.1)

  RanksMua_h_Agg910 = unique(c(RanksMua_h_Agg910, RanksMua05_h_Agg[[h]]$Param[1:Top10h05_Agg],RanksMuaot_h_Agg[[h]]$Param[1:Top10hot_Agg],RanksMua95_h_Agg[[h]]$Param[1:Top10h95_Agg]))
}
rm(h)

# Select all parameters whose 95th quantile estimate of EE is greater than the selected threshold point
ParamSelect_h_Agg = ParamSelectTN_h_Agg = NULL
for (h in 1:length(uhills)){
  ParamSelect_h_Agg = unique(c(ParamSelect_h_Agg, colnames(EEs05_h_mua_95)[-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))][EEs05_h_mua_95[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] >= RanksMua05_h_Agg[[h]][Top10h05_Agg,2]],
                               colnames(EEsot_h_mua_95)[-c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))][EEsot_h_mua_95[h,-c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))] >= RanksMuaot_h_Agg[[h]][Top10hot_Agg,2]],
                               colnames(EEs95_h_mua_95)[-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))][EEs95_h_mua_95[h,-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))] >= RanksMua95_h_Agg[[h]][Top10h95_Agg,2]]))
  ParamSelectTN_h_Agg = unique(c(ParamSelectTN_h_Agg, colnames(EEsTN05_h_mua_95)[-c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))][EEsTN05_h_mua_95[h,-c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))] >= RanksMuaTN05_h_Agg[[h]][Top10hTN05_Agg,2]],
                                                      colnames(EEsTNMed_h_mua_95)[-c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))][EEsTNMed_h_mua_95[h,-c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))] >= RanksMuaTNMed_h_Agg[[h]][Top10hTNMed_Agg,2]],
                                                      colnames(EEsTN95_h_mua_95)[-c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))][EEsTN95_h_mua_95[h,-c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))] >= RanksMuaTN95_h_Agg[[h]][Top10hTN95_Agg,2]]))
}
rm(h)

length(unique(c(ParamSelect_h_Agg, ParamSelectTN_h_Agg)))

ParamSelect_h_Agg910 = NULL
for (h in 9:10){
  ParamSelect_h_Agg910 = unique(c(ParamSelect_h_Agg910, colnames(EEs05_h_mua_95)[-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))][EEs05_h_mua_95[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] >= RanksMua05_h_Agg[[h]][Top10h05_Agg,2]],
                               colnames(EEsot_h_mua_95)[-c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))][EEsot_h_mua_95[h,-c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))] >= RanksMuaot_h_Agg[[h]][Top10hot_Agg,2]],
                               colnames(EEs95_h_mua_95)[-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))][EEs95_h_mua_95[h,-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))] >= RanksMua95_h_Agg[[h]][Top10h95_Agg,2]]))
}
rm(h)

#Make a plot of the number of parameters selected versus the threshold percentage----
x = seq(0.01,1,0.01)
ParamTotals_ThreshPercent = vector('numeric', length(x))
for (i in 1:length(x)){
  Tops = ceiling((length(muaEEs05_b) - length(which(muaEEs05_b == 0)))*x[i])
  # Select all parameters whose 95th quantile estimate of EE is greater than the selected threshold point
  pb = unique(c(names(EEs05_b_mua_95[1:nrow(ParamRanges)][EEs05_b_mua_95[1:nrow(ParamRanges)] >= RanksMua05_b$EE05_b[Tops]]),
                           names(EEsot_b_mua_95[1:nrow(ParamRanges)][EEsot_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaot_b$EEot_b[Tops]]),
                           names(EEs95_b_mua_95[1:nrow(ParamRanges)][EEs95_b_mua_95[1:nrow(ParamRanges)] >= RanksMua95_b$EE95_b[Tops]])))
  pbTN = unique(c(names(EEsTN05_b_mua_95[1:nrow(ParamRanges)][EEsTN05_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaTN05_b$EETN05_b[Tops]]),
                             names(EEsTNMed_b_mua_95[1:nrow(ParamRanges)][EEsTNMed_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaTNMed_b$EETNMed_b[Tops]]),
                             names(EEsTN95_b_mua_95[1:nrow(ParamRanges)][EEsTN95_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaTN95_b$EETN95_b[Tops]])))
  
  ParamTotals_ThreshPercent[i] = length(unique(c(pb, pbTN)))
}
rm(pb, pbTN, Tops, i, x)

png('ParamsInThresholdCutoff.png', res = 300, units = 'in', width = 5, height = 5)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent, type = 'l', xlab = 'Top X Percent Selected', ylab = 'Number of Parameters Selected', ylim = c(0,120), xlim = c(0,100))
par(new=TRUE)
plot(x = seq(1,100,1), y = ceiling((length(muaEEs05_b) - length(which(muaEEs05_b == 0)))*seq(0.01,1,0.01)), type = 'l', ylim = c(0,120), xlim = c(0,100), col = 'red', xlab = '', ylab = '', axes = FALSE)
legend('topleft', legend = c('Based on Mean', 'Based on 95th Percentile'), lty = 1, col = c('red', 'black'))
dev.off()

# Aggregated----
x = seq(0.01,1,0.01)
ParamTotals_ThreshPercent_Agg = vector('numeric', length(x))
for (i in 1:length(x)){
  Tops05 = ceiling((length(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  Topsot = ceiling((length(EEsot_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsot_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  Tops95 = ceiling((length(EEs95_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEs95_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  TopsTN05 = ceiling((length(EEsTN05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsTN05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  TopsTNMed = ceiling((length(EEsTNMed_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsTNMed_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  TopsTN95 = ceiling((length(EEsTN95_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsTN95_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  
  # Select all parameters whose 95th quantile estimate of EE is greater than the selected threshold point
  pb = unique(c(names(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] >= RanksMua05_b_Agg$EE05_b[Tops05]]),
                               names(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)] >= RanksMuaot_b_Agg$EEot_b[Topsot]]),
                               names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)] >= RanksMua95_b_Agg$EE95_b[Tops95]])))
  pbTN = unique(c(names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)] >= RanksMuaTN05_b_Agg$EETN05_b[TopsTN05]]),
                                 names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)] >= RanksMuaTNMed_b_Agg$EETNMed_b[TopsTNMed]]),
                                 names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)] >= RanksMuaTN95_b_Agg$EETN95_b[TopsTN95]])))
  
  ParamTotals_ThreshPercent_Agg[i] = length(unique(c(pb, pbTN)))
}
rm(pb, pbTN, Tops05, Topsot, Tops95, TopsTN05, TopsTNMed, TopsTN95, i, x)

png('ParamsInThresholdCutoff_Agg.png', res = 300, units = 'in', width = 5, height = 5)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_Agg, type = 'l', xlab = 'Top X Percent Selected', ylab = 'Number of Parameters Selected', ylim = c(0,105), xlim = c(0,100))
par(new=TRUE)
plot(x = seq(1,100,1), y = ceiling((length(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*seq(0.01,1,0.01)), type = 'l', ylim = c(0,105), xlim = c(0,100), col = 'red', xlab = '', ylab = '', axes = FALSE)
legend('topleft', legend = c('Based on Mean', 'Based on 95th Percentile'), lty = 1, col = c('red', 'black'))
dev.off()

# Aggregated with hillslope variables----
x = seq(0.01,1,0.01)
ParamTotals_ThreshPercent_h_Agg = vector('numeric', length(x))
ParamTotals_ThreshPercent_onlyh_Agg = vector('numeric', length(x))
for (i in 1:length(x)){
  Tops05 = ceiling((length(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  Topsot = ceiling((length(EEsot_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsot_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  Tops95 = ceiling((length(EEs95_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEs95_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  TopsTN05 = ceiling((length(EEsTN05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsTN05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  TopsTNMed = ceiling((length(EEsTNMed_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsTNMed_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  TopsTN95 = ceiling((length(EEsTN95_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsTN95_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  
  # Select all parameters whose 95th quantile estimate of EE is greater than the selected threshold point
  pb = unique(c(names(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] >= RanksMua05_b_Agg$EE05_b[Tops05]]),
                names(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)] >= RanksMuaot_b_Agg$EEot_b[Topsot]]),
                names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)] >= RanksMua95_b_Agg$EE95_b[Tops95]])))
  pbTN = unique(c(names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)] >= RanksMuaTN05_b_Agg$EETN05_b[TopsTN05]]),
                  names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)] >= RanksMuaTNMed_b_Agg$EETNMed_b[TopsTNMed]]),
                  names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)] >= RanksMuaTN95_b_Agg$EETN95_b[TopsTN95]])))
  
  # Select all parameters whose 95th quantile estimate of EE is greater than the selected threshold point
  ph = phTN = NULL
  if (round(x[i],2) == 0.1){
    ph10 = phTN10 = list()
  }
  for (h in 1:length(uhills)){
    Topsh05 = ceiling((length(EEs05_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEs05_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*x[i])
    Topshot = ceiling((length(EEsot_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEsot_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*x[i])
    Topsh95 = ceiling((length(EEs95_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEs95_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*x[i])
    TopshTN05 = ceiling((length(EEsTN05_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEsTN05_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*x[i])
    TopshTNMed = ceiling((length(EEsTNMed_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEsTNMed_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*x[i])
    TopshTN95 = ceiling((length(EEsTN95_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEsTN95_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*x[i])
    
    ph = unique(c(ph, colnames(EEs05_h_mua_95)[-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))][EEs05_h_mua_95[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] >= RanksMua05_h_Agg[[h]][Topsh05,2]],
                                 colnames(EEsot_h_mua_95)[-c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))][EEsot_h_mua_95[h,-c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))] >= RanksMuaot_h_Agg[[h]][Topshot,2]],
                                 colnames(EEs95_h_mua_95)[-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))][EEs95_h_mua_95[h,-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))] >= RanksMua95_h_Agg[[h]][Topsh95,2]]))
    phTN = unique(c(phTN, colnames(EEsTN05_h_mua_95)[-c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))][EEsTN05_h_mua_95[h,-c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))] >= RanksMuaTN05_h_Agg[[h]][TopshTN05,2]],
                                   colnames(EEsTNMed_h_mua_95)[-c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))][EEsTNMed_h_mua_95[h,-c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))] >= RanksMuaTNMed_h_Agg[[h]][TopshTNMed,2]],
                                   colnames(EEsTN95_h_mua_95)[-c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))][EEsTN95_h_mua_95[h,-c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))] >= RanksMuaTN95_h_Agg[[h]][TopshTN95,2]]))
    if (round(x[i],2) == 0.10){
      ph10 = c(ph10, list(unique(c(colnames(EEs05_h_mua_95)[-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))][EEs05_h_mua_95[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] >= RanksMua05_h_Agg[[h]][Topsh05,2]],
                      colnames(EEsot_h_mua_95)[-c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))][EEsot_h_mua_95[h,-c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))] >= RanksMuaot_h_Agg[[h]][Topshot,2]],
                      colnames(EEs95_h_mua_95)[-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))][EEs95_h_mua_95[h,-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))] >= RanksMua95_h_Agg[[h]][Topsh95,2]]))))
      phTN10 = c(phTN10, list(unique(c(colnames(EEsTN05_h_mua_95)[-c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))][EEsTN05_h_mua_95[h,-c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))] >= RanksMuaTN05_h_Agg[[h]][TopshTN05,2]],
                      colnames(EEsTNMed_h_mua_95)[-c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))][EEsTNMed_h_mua_95[h,-c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))] >= RanksMuaTNMed_h_Agg[[h]][TopshTNMed,2]],
                      colnames(EEsTN95_h_mua_95)[-c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))][EEsTN95_h_mua_95[h,-c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))] >= RanksMuaTN95_h_Agg[[h]][TopshTN95,2]]))))
    }
  }
  
  ParamTotals_ThreshPercent_h_Agg[i] = length(unique(c(pb, pbTN, ph, phTN)))
  ParamTotals_ThreshPercent_onlyh_Agg[i] = length(unique(c(ph, phTN)))
}
rm(pb, pbTN, Tops05, Tops95, Topsot, Topsh05, Topsh95, Topshot, TopsTN05, TopsTNMed, TopsTN95, TopshTN05, TopshTNMed, TopshTN95, i, x, h, ph, phTN)

png('ParamsInThresholdCutoff_h_Agg.png', res = 300, units = 'in', width = 5, height = 5)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_h_Agg, type = 'l', xlab = 'Top X Percent Selected', ylab = 'Number of Parameters Selected', ylim = c(0,105), xlim = c(0,100))
par(new=TRUE)
plot(x = seq(1,100,1), y = ceiling((length(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*seq(0.01,1,0.01)), type = 'l', ylim = c(0,105), xlim = c(0,100), col = 'red', xlab = '', ylab = '', axes = FALSE)
legend('topleft', legend = c('Based on Mean', 'Based on 95th Percentile'), lty = 1, col = c('red', 'black'))
dev.off()

png('ParamsInThresholdCutoff_bh_Agg.png', res = 300, units = 'in', width = 5, height = 5)
par(mar=c(4,4,0.5,0.5))
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_onlyh_Agg, type = 'l', xlab = 'Top X Percent Selected', ylab = 'Number of Parameters Selected', ylim = c(0,105), xlim = c(0,100), col = 'gray')
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_h_Agg, type = 'l', ylim = c(0,105), xlim = c(0,100), col = 'black', xlab = '', ylab = '', axes = FALSE)
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_Agg, type = 'o', ylim = c(0,105), xlim = c(0,100), col = 'blue', xlab = '', ylab = '', axes = FALSE, pch = 15, cex = 0.3)
lines(c(10,10), c(-10,200), lty=2, col='gray')
legend('bottomright', legend = c('Hillslope + Basin Outlets', 'Hillslope Outlets Only', 'Basin Outlet Only', 'Selected Percentage'), lty = c(1,1,1,2), col = c('black', 'gray', 'blue', 'gray'), pch = c(NA,NA,15,NA), pt.cex = 0.3)
dev.off()

#Color scheme for plotting EEs in categories of parameters----
#One color per category
colos = c(rainbow(12), 'black')
colos[3] = 'gray'
colos_paper = c(scico::scico(n = 5, palette = 'hawaii'), 'black')

# Unaggregated----
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
rm(i)

# Aggregated----
#Assign colors to the categories
ColPlots_Agg = vector('character', length=length(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)]))
ColPlots_Agg_Paper = ColPlots_Agg
for (i in 1:length(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)])){
  if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 'h'){
    ColPlots_Agg[i] = colos[1]
    ColPlots_Agg_Paper[i] = colos_paper[1]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 'z'){
    ColPlots_Agg[i] = colos[2]
    ColPlots_Agg_Paper[i] = colos_paper[5]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 's9'){
    ColPlots_Agg[i] = colos[3]
    ColPlots_Agg_Paper[i] = colos_paper[2]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 's109'){
    ColPlots_Agg[i] = colos[4]
    ColPlots_Agg_Paper[i] = colos_paper[2]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 's8'){
    ColPlots_Agg[i] = colos[5]
    ColPlots_Agg_Paper[i] = colos_paper[2]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 's108'){
    ColPlots_Agg[i] = colos[6]
    ColPlots_Agg_Paper[i] = colos_paper[2]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 'l1'){
    ColPlots_Agg[i] = colos[7]
    ColPlots_Agg_Paper[i] = colos_paper[3]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 'l2'){
    ColPlots_Agg[i] = colos[8]
    ColPlots_Agg_Paper[i] = colos_paper[3]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 'l3'){
    ColPlots_Agg[i] = colos[9]
    ColPlots_Agg_Paper[i] = colos_paper[3]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 'l4'){
    ColPlots_Agg[i] = colos[10]
    ColPlots_Agg_Paper[i] = colos_paper[3]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 'v102'){
    ColPlots_Agg[i] = colos[11]
    ColPlots_Agg_Paper[i] = colos_paper[4]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 'v3'){
    ColPlots_Agg[i] = colos[12]
    ColPlots_Agg_Paper[i] = colos_paper[4]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 'v4'){
    ColPlots_Agg[i] = colos[13]
    ColPlots_Agg_Paper[i] = colos_paper[6]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 'Soil8'){
    ColPlots_Agg[i] = 'darkgreen'
    ColPlots_Agg_Paper[i] = colos_paper[2]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 'Soil9'){
    ColPlots_Agg[i] = 'darkgray'
    ColPlots_Agg_Paper[i] = colos_paper[2]
  }
}
rm(i)

#Make plots of the sd vs. mu of EEs - basin----
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

#Make plots of the ranking for mean absolute value - basin----
png('EE05_mua_b.png', res = 300, units = 'in', height = 7, width = 7)
barplot(height = EEs05_b_mua_m[1:nrow(ParamRanges)]/max(EEs05_b_mua_95[1:nrow(ParamRanges)]), ylab = 'Normalized Mean Abs. Val. of Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Lower 5th Percentile of Flow', col = ColPlots, border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,300))
arrows(seq(0.5,nrow(ParamRanges)-0.5,1), EEs05_b_mua_05[1:nrow(ParamRanges)]/max(EEs05_b_mua_95[1:nrow(ParamRanges)]), seq(0.5,nrow(ParamRanges)-0.5,1), EEs05_b_mua_95[1:nrow(ParamRanges)]/max(EEs05_b_mua_95[1:nrow(ParamRanges)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(RanksMua05_b[Top1005,2]/max(EEs05_b_mua_95[1:nrow(ParamRanges)]),RanksMua05_b[Top1005,2]/max(EEs05_b_mua_95[1:nrow(ParamRanges)])), col = 'black', lwd = 2)
legend('topright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos, cex = 1.3)
dev.off()

png('EE05_mua_b_Agg.png', res = 300, units = 'in', height = 7, width = 7)
barplot(height = EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Lower 5th Percentile of Flow', col = ColPlots_Agg, border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,300))
arrows(seq(0.5,length(EEs05_b_mua_05[-which(names(EEs05_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs05_b_mua_05[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEs05_b_mua_05[-which(names(EEs05_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1005_Agg])/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1005_Agg])/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)
legend('topright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos, cex = 1.3)
dev.off()

png('EE05_mua_b_Agg_Paper.png', res = 300, units = 'in', height = 7, width = 7)
barplot(height = EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Lower 5th Percentile of Flow', col = ColPlots_Agg_Paper[order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEs05_b_mua_05[-which(names(EEs05_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs05_b_mua_05[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEs05_b_mua_05[-which(names(EEs05_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1005_Agg])/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1005_Agg])/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)
#legend('topright', legend = c('Hillslope: GW', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Soil: #8', 'Soil: Comp. #8', 'Soil: #9', 'Soil: Comp. #9', 'Veg: Trees', 'Veg: Grass', 'Buildings', 'Zone: Atm.'), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), cex = 1, ncol = 2)
legend('topright', legend = c('Hillslope: GW', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Soil: Riparian', 'Soil: Other', 'Veg: Trees', 'Veg: Grass', 'Buildings', 'Zone: Atm.'), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), cex = 1.2, ncol = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEs05_b_mua_m)[-which(names(EEs05_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
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
barplot(height = EEs95_b_mua_m[1:nrow(ParamRanges)]/max(EEs95_b_mua_95[1:nrow(ParamRanges)]), ylab = 'Normalized Mean Abs. Val. of Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Upper 5th Percentile of Flow', col = ColPlots, border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,300))
arrows(seq(0.5,nrow(ParamRanges)-0.5,1), EEs95_b_mua_05[1:nrow(ParamRanges)]/max(EEs95_b_mua_95[1:nrow(ParamRanges)]), seq(0.5,nrow(ParamRanges)-0.5,1), EEs95_b_mua_95[1:nrow(ParamRanges)]/max(EEs95_b_mua_95[1:nrow(ParamRanges)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(RanksMua95_b[Top1095,2]/max(EEs95_b_mua_95[1:nrow(ParamRanges)]),RanksMua95_b[Top1095,2]/max(EEs95_b_mua_95[1:nrow(ParamRanges)])), col = 'black', lwd = 2)
legend('topright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos, cex = 1.3)
dev.off()

png('EE95_mua_b_Agg.png', res = 300, units = 'in', height = 7, width = 7)
barplot(height = EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Upper 5th Percentile of Flow', col = ColPlots_Agg, border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,300))
arrows(seq(0.5,length(EEs95_b_mua_05[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs95_b_mua_05[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEs95_b_mua_05[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1095_Agg])/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1095_Agg])/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)
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
barplot(height = EEsot_b_mua_m[1:nrow(ParamRanges)]/max(EEsot_b_mua_95[1:nrow(ParamRanges)]), ylab = 'Normalized Mean Abs. Val. of Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for 5th-95th Percentile Flows', col = ColPlots, border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,300))
arrows(seq(0.5,nrow(ParamRanges)-0.5,1), EEsot_b_mua_05[1:nrow(ParamRanges)]/max(EEsot_b_mua_95[1:nrow(ParamRanges)]), seq(0.5,nrow(ParamRanges)-0.5,1), EEsot_b_mua_95[1:nrow(ParamRanges)]/max(EEsot_b_mua_95[1:nrow(ParamRanges)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(RanksMuaot_b[Top10ot,2]/max(EEsot_b_mua_95[1:nrow(ParamRanges)]),RanksMuaot_b[Top10ot,2]/max(EEsot_b_mua_95[1:nrow(ParamRanges)])), col = 'black', lwd = 2)
legend('topright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos, cex = 1.3)
dev.off()

png('EEot_mua_b_Agg.png', res = 300, units = 'in', height = 7, width = 7)
barplot(height = EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]/max(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for 5th-95th Percentile Flows', col = ColPlots_Agg, border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,300))
arrows(seq(0.5,length(EEsot_b_mua_05[-which(names(EEsot_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsot_b_mua_05[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]/max(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsot_b_mua_05[-which(names(EEsot_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]/max(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10ot_Agg])/max(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10ot_Agg])/max(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)
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
barplot(height = EEsTN05_b_mua_m[1:nrow(ParamRanges)]/max(EEsTN05_b_mua_95[1:nrow(ParamRanges)]), ylab = 'Normalized Mean Abs. Val. of Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Lower 5th Quantile of TN', col = ColPlots, border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,300))
arrows(seq(0.5,nrow(ParamRanges)-0.5,1), EEsTN05_b_mua_05[1:nrow(ParamRanges)]/max(EEsTN05_b_mua_95[1:nrow(ParamRanges)]), seq(0.5,nrow(ParamRanges)-0.5,1), EEsTN05_b_mua_95[1:nrow(ParamRanges)]/max(EEsTN05_b_mua_95[1:nrow(ParamRanges)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(RanksMuaTN05_b[Top10TN05,2]/max(EEsTN05_b_mua_95[1:nrow(ParamRanges)]),RanksMuaTN05_b[Top10TN05,2]/max(EEsTN05_b_mua_95[1:nrow(ParamRanges)])), col = 'black', lwd = 2)
legend('topright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos, cex = 1.3)
dev.off()

png('EETN05_mua_b_Agg.png', res = 300, units = 'in', height = 7, width = 7)
barplot(height = EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Lower 5th Quantile of TN', col = ColPlots_Agg, border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,300))
arrows(seq(0.5,length(EEsTN05_b_mua_05[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTN05_b_mua_05[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsTN05_b_mua_05[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TN05_Agg])/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TN05_Agg])/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)
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
barplot(height = EEsTN95_b_mua_m[1:nrow(ParamRanges)]/max(EEsTN95_b_mua_95[1:nrow(ParamRanges)]), ylab = 'Normalized Mean Abs. Val. of Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Upper 5th Quantile of TN', col = ColPlots, border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,300))
arrows(seq(0.5,nrow(ParamRanges)-0.5,1), EEsTN95_b_mua_05[1:nrow(ParamRanges)]/max(EEsTN95_b_mua_95[1:nrow(ParamRanges)]), seq(0.5,nrow(ParamRanges)-0.5,1), EEsTN95_b_mua_95[1:nrow(ParamRanges)]/max(EEsTN95_b_mua_95[1:nrow(ParamRanges)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(RanksMuaTN95_b[Top10TN95,2]/max(EEsTN95_b_mua_95[1:nrow(ParamRanges)]),RanksMuaTN95_b[Top10TN95,2]/max(EEsTN95_b_mua_95[1:nrow(ParamRanges)])), col = 'black', lwd = 2)
legend('topright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos, cex = 1.3)
dev.off()

png('EETN95_mua_b_Agg.png', res = 300, units = 'in', height = 7, width = 7)
barplot(height = EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Upper 5th Quantile of TN', col = ColPlots_Agg, border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,300))
arrows(seq(0.5,length(EEsTN95_b_mua_05[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTN95_b_mua_05[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsTN95_b_mua_05[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TN95_Agg])/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TN95_Agg])/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)
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
barplot(height = EEsTNMed_b_mua_m[1:nrow(ParamRanges)]/max(EEsTNMed_b_mua_95[1:nrow(ParamRanges)]), ylab = 'Normalized Mean Abs. Val. of Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Mean of TN', col = ColPlots, border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,300))
arrows(seq(0.5,nrow(ParamRanges)-0.5,1), EEsTNMed_b_mua_05[1:nrow(ParamRanges)]/max(EEsTNMed_b_mua_95[1:nrow(ParamRanges)]), seq(0.5,nrow(ParamRanges)-0.5,1), EEsTNMed_b_mua_95[1:nrow(ParamRanges)]/max(EEsTNMed_b_mua_95[1:nrow(ParamRanges)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(RanksMuaTNMed_b[Top10TNMed,2]/max(EEsTNMed_b_mua_95[1:nrow(ParamRanges)]),RanksMuaTNMed_b[Top10TNMed,2]/max(EEsTNMed_b_mua_95[1:nrow(ParamRanges)])), col = 'black', lwd = 2)
legend('topright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos, cex = 1.3)
dev.off()

png('EETNMed_mua_b_Agg.png', res = 300, units = 'in', height = 7, width = 7)
barplot(height = EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of Elementary Effect', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Mean of TN', col = ColPlots_Agg, border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,300))
arrows(seq(0.5,length(EEsTNMed_b_mua_05[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTNMed_b_mua_05[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsTNMed_b_mua_05[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TNMed_Agg])/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TNMed_Agg])/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)
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

# Paper figures----
offset = function(x){
  if ((x>=0.3) & (x < 0.8)){
    x = x + (x - 0.3)*0.13
  }else if (x >= 0.8) {
    x = x + (x - 0.3)*0.08
  }
  return(x)
}
png('Panel_mua_b_Agg_Paper.png', res = 300, units = 'in', height = 8, width = 12)
layout(rbind(c(1,2,3), c(4,5,6)))
#Streamflow 05
barplot(height = EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Lower 5th Percentile of Flow', col = ColPlots_Agg_Paper[order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEs05_b_mua_05[-which(names(EEs05_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs05_b_mua_05[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEs05_b_mua_05[-which(names(EEs05_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1005_Agg])/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1005_Agg])/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)
#legend('topright', legend = c('Hillslope: GW', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Soil: #8', 'Soil: Comp. #8', 'Soil: #9', 'Soil: Comp. #9', 'Veg: Trees', 'Veg: Grass', 'Buildings', 'Zone: Atm.'), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), cex = 1, ncol = 2)
legend('topright', legend = c('Hillslope: GW', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Soil: Riparian', 'Soil: Other', 'Veg: Trees', 'Veg: Grass', 'Buildings', 'Zone: Atm.'), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), cex = 1.1, ncol = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEs05_b_mua_m)[-which(names(EEs05_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMua05_b_Agg$EE05_b[Top1005_Agg]]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}
#Streamflow mid
par(xpd=FALSE)
barplot(height = EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][order(names(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for 5th - 95th Percentile of Flow', col = ColPlots_Agg_Paper[order(names(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEsot_b_mua_05[-which(names(EEsot_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsot_b_mua_05[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][order(names(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsot_b_mua_05[-which(names(EEsot_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][order(names(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10ot_Agg])/max(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10ot_Agg])/max(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEsot_b_mua_m)[-which(names(EEsot_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][order(names(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][order(names(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaot_b_Agg$EEot_b[Top10ot_Agg]]/max(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][order(names(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][order(names(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][order(names(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][order(names(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}

#Streamflow 95
par(xpd=FALSE)
barplot(height = EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Upper 95th Percentile of Flow', col = ColPlots_Agg_Paper[order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEs95_b_mua_05[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs95_b_mua_05[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEs95_b_mua_05[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1095_Agg])/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1095_Agg])/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEs95_b_mua_m)[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMua95_b_Agg$EE95_b[Top1095_Agg]]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}

#TN 05
par(xpd=FALSE)
barplot(height = EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Lower 5th Percentile of TN', col = ColPlots_Agg_Paper[order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEsTN05_b_mua_05[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTN05_b_mua_05[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsTN05_b_mua_05[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TN05_Agg])/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TN05_Agg])/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEsTN05_b_mua_m)[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaTN05_b_Agg$EETN05_b[Top10TN05_Agg]]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}

#TN mid
par(xpd=FALSE)
barplot(height = EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Mean of TN', col = ColPlots_Agg_Paper[order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEsTNMed_b_mua_05[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTNMed_b_mua_05[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsTNMed_b_mua_05[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TNMed_Agg])/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TNMed_Agg])/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEsTNMed_b_mua_m)[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaTNMed_b_Agg$EETNMed_b[Top10TNMed_Agg]]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}

#TN 95
par(xpd=FALSE)
barplot(height = EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Upper 95th Percentile of TN', col = ColPlots_Agg_Paper[order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEsTN95_b_mua_05[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTN95_b_mua_05[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsTN95_b_mua_05[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TN95_Agg])/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TN95_Agg])/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEsTN95_b_mua_m)[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaTN95_b_Agg$EETN95_b[Top10TN95_Agg]]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}
dev.off()

#Hillslope Plots for mua----
# Normalized EE value - not used----
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
    plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,15), pch = 15, xlab = 'Hillslope ID', ylab = 'Parameter', col = colFun(RanksMua95_h[[h]][RanksMua95_h[[h]]$Param == SortRanksMua_b[j],2] / max(RanksMua95_h[[h]][,2])), axes = FALSE)
    par(new=TRUE)
  }
}
rm(h,j)
axis(side = 1, at = seq(1,14,1), labels = TRUE)
axis(side = 2, at = seq(1,14,1), labels = FALSE)
dev.off()

# Rank order of EEs----
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
rm(h,j)
par(new=FALSE)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,14,1), labels = SortRanksMua_b, las = 1)
#legend('top', title = expression(bold('Parameter Sensitivity Rank')), legend = c('1 - 14', '15 - 29', '30 - 44', '45 - 60', '61 - 101'), horiz = TRUE, pch = 15, col = colFun(seq(0,60,15)), inset = -0.1, xpd = TRUE)
legend('right', title = expression(bold('Rank')), legend = c('1 - 14', '15 - 29', '30 - 44', '45 - 60', '61 - 101'), pch = 15, col = colFun(seq(0,60,15)), inset = -0.3, xpd = TRUE)
dev.off()

# Same but dropping using only the aggregated variables----
#  Normalized EE value - not used----
colPal = colorRampPalette(colors = rev(c('red', 'orange', 'yellow', 'green', 'blue')))
scaleRange = c(0, 1)
scaleBy = 0.1
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)

#Make a grid of the parameters in top 10% for hillslopes
SortRanksMua_b_Agg = unique(c(RanksMua_b_Agg, RanksMuaTN_b_Agg))[c(1,9,10,4,12,13,2,7,15,3,6,8,11,14,5)]
png('HillNormalizedTop12_Agg.png', res = 300, height = 6, width = 6, units = 'in')
for (h in 1:length(uhills)){
  #Loop over the top 10% ranks for basin
  for (j in 1:length(SortRanksMua_b_Agg)){
    plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg)), pch = 15, xlab = 'Hillslope ID', ylab = 'Parameter', col = colFun(RanksMua95_h_Agg[[h]][RanksMua95_h_Agg[[h]]$Param == SortRanksMua_b_Agg[j], 2] / max(RanksMua95_h_Agg[[h]][,2])), axes = FALSE)
    par(new=TRUE)
  }
}
rm(h,j)
axis(side = 1, at = seq(1,14,1), labels = TRUE)
axis(side = 2, at = seq(1,14,1), labels = FALSE)
dev.off()

#  Rank order----
#colPal = colorRampPalette(colors = rev(c('red', 'orange', 'gray', 'green', 'blue')))
colPal = colorRampPalette(colors = scico(n = 4, palette = 'nuuk'))
scaleRange = c(0, 45)
scaleBy = 15
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)
Ranks_Agg=NULL
LabelNames = c('H: GW Loss Coef.', 'Z: Wind Speed', 'Riparian Soil (S8+S108): m', 'Other Soil (S9+S109): m', 
               'Other Soil (S9+S109): Ksat', 'Other Soil (S9+S109): Poro.', 'S9: Sat. to GW Coef.', 'S9: Soil Thickness', 
               'S9: Air Entry Pres.', 'S109: Sat. to GW Coef.', 'Tree: Max Stomatal Cond.', 'Tree: Rainwater Capacity',
               'Tree: Stomatal Fraction', 'Tree: Day Leaf On', 'L4: Septic Water Load')
png('HillRankTop12_Agg.png', res = 300, height = 6, width = 6, units = 'in')
par(mar = c(2,11.5,0,4.5), mgp = c(1,1,0))
for (h in 1:length(uhills)){
  #Loop over the top 10% ranks for basin
  for (j in 1:length(SortRanksMua_b_Agg)){
    if(j == 1){
      if(h == 1){
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMua95_h_Agg[[h]]$Param == SortRanksMua_b_Agg[j])), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(0.5,0.5,14.5,14.5), y = c(0.5,21.5,21.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua95_h_Agg[[h]]$Param == SortRanksMua_b_Agg[j])), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMua95_h_Agg[[h]]$Param == SortRanksMua_b_Agg[j])), axes = FALSE, cex.lab = 1.5)
      }
    }else{
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua95_h_Agg[[h]]$Param == SortRanksMua_b_Agg[j])), axes = FALSE)
    }
    par(new=TRUE)
    Ranks_Agg = c(Ranks_Agg,which(RanksMua95_h_Agg[[h]]$Param == SortRanksMua_b_Agg[j]))
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
#axis(side = 2, at = seq(1,length(SortRanksMua_b_Agg),1), labels = SortRanksMua_b_Agg, las = 1)
axis(side = 2, at = seq(1,length(SortRanksMua_b_Agg),1), labels = LabelNames, las = 1)
#legend('top', title = expression(bold('Parameter Sensitivity Rank')), legend = c('1 - 14', '15 - 29', '30 - 44', '45 - 60', '61 - 101'), horiz = TRUE, pch = 15, col = colFun(seq(0,60,15)), inset = -0.1, xpd = TRUE)
legend('right', title = expression(bold('Rank')), legend = c('1 - 15', '16 - 30', '31 - 45', '>45'), pch = 15, col = colFun(seq(0,45,15)), inset = -0.3, xpd = TRUE)
box(which = 'figure')
dev.off()

#Hillslope Plots for mua with all parameters with EEs > 95% of 10% threshold----
SortRanksMua_b_Agg = unique(c(ParamSelect_b_Agg, ParamSelectTN_b_Agg))[c(1,2,16,17,13,14,12,18,4,3,19,20,5,6,7,9,10,11,15,21,8)]
LabelNames = c('H: GW Loss Coef.', 'Z: Wind Speed', 'Z: Avg. Temp. Coef.', 'Z: Atm. Trans. Coef.', 
               'Riparian Soil (S8+S108): m', 'Other Soil (S9+S109): m', 'Other Soil (S9+S109): Ksat', 'Other Soil (S9+S109): Poro.', 'S9: Sat. to GW Coef.', 'S9: Soil Thickness', 
               'S9: Pore Size', 'S9: Air Entry Pres.', 'S109: Soil Thickness', 'S109: Air Entry Pres.', 'S109: Sat. to GW Coef.', 
               'Tree: Max Stomatal Cond.', 'Tree: Stomatal Fraction', 'Tree: Rainwater Capacity', 'Tree: Day Leaf On', 'Tree: Leaf Cuticular Cond.', 'L4: Septic Water Load')
SortRanksMua_b_Agg_paper = rev(SortRanksMua_b_Agg[c(1,21,5:10,12,11,15,13,14,16:20,2:4)])
LabelNames_paper = rev(LabelNames[c(1,21,5:10,12,11,15,13,14,16:20,2:4)])
SortRanksMua_h_Agg = unique(c(ParamSelect_b_Agg, ParamSelectTN_b_Agg, ParamSelect_h_Agg, ParamSelectTN_h_Agg))[c(1,2,16,17,24,28,30,13,29,22,23,33,14,12,18,3,4,19,20,5,6,7,31,9,10,11,15,21,25,26,27,35,32,34,8)]
LabelNames_h = c('H: GW Loss Coef.', 'Z: Wind Speed', 'Z: Avg. Temp. Coef.', 'Z: Atm. Trans. Coef. 2', 'Z: Atm. Trans. Coef. 1', 'Z: Clear Sky Trans.', 
               'Riparian Soil (S8+S108): Ksat','Riparian Soil (S8+S108): m', 'S8: Air Entry Pres.', 'S8: Soil Thickness', 'S8: Sat. to GW Coef.', 'S108: Sat. to GW Coef.',
               'Other Soil (S9+S109): m', 'Other Soil (S9+S109): Ksat', 'Other Soil (S9+S109): Poro.', 'S9: Soil Thickness', 'S9: Sat. to GW Coef.', 
               'S9: Pore Size', 'S9: Air Entry Pres.', 'S109: Soil Thickness', 'S109: Air Entry Pres.', 'S109: Sat. to GW Coef.', 'S109: Pore Size', 
               'Tree: Max Stomatal Cond.', 'Tree: Stomatal Fraction', 'Tree: Rainwater Capacity', 'Tree: Day Leaf On', 'Tree: Leaf Cuticular Cond.', 'Tree: Day Leaf Off', 'Tree: Days Leaves Expand', 'Tree: Days Leaves Fall', 'Grass: Rainwater Capacity', 
               'L3: Percent Impervious', 'L4: Percent Impervious', 'L4: Septic Water Load')
SortRanksMua_h_Agg_paper = rev(SortRanksMua_h_Agg[c(1,33:35,7:12,14,13,15:26,28,27,29:32,2:6)])
LabelNames_h_paper = rev(LabelNames_h[c(1,33:35,7:12,14,13,15:26,28,27,29:32,2:6)])

# Heatmap parameters----
HeatParams05s_Thresh = sort(unique(c(RanksMua05_h_Agg[[1]]$Param[RanksMua05_h_Agg[[1]]$EE05_h >= 0.01],
                      RanksMua05_h_Agg[[2]]$Param[RanksMua05_h_Agg[[2]]$EE05_h >= 0.01],
                      RanksMua05_h_Agg[[3]]$Param[RanksMua05_h_Agg[[3]]$EE05_h >= 0.01],
                      RanksMua05_h_Agg[[4]]$Param[RanksMua05_h_Agg[[4]]$EE05_h >= 0.01],
                      RanksMua05_h_Agg[[5]]$Param[RanksMua05_h_Agg[[5]]$EE05_h >= 0.01],
                      RanksMua05_h_Agg[[6]]$Param[RanksMua05_h_Agg[[6]]$EE05_h >= 0.01],
                      RanksMua05_h_Agg[[7]]$Param[RanksMua05_h_Agg[[7]]$EE05_h >= 0.01],
                      RanksMua05_h_Agg[[8]]$Param[RanksMua05_h_Agg[[8]]$EE05_h >= 0.01],
                      RanksMua05_h_Agg[[9]]$Param[RanksMua05_h_Agg[[9]]$EE05_h >= 0.01],
                      RanksMua05_h_Agg[[10]]$Param[RanksMua05_h_Agg[[10]]$EE05_h >= 0.01],
                      RanksMua05_h_Agg[[11]]$Param[RanksMua05_h_Agg[[11]]$EE05_h >= 0.01],
                      RanksMua05_h_Agg[[12]]$Param[RanksMua05_h_Agg[[12]]$EE05_h >= 0.01],
                      RanksMua05_h_Agg[[13]]$Param[RanksMua05_h_Agg[[13]]$EE05_h >= 0.01],
                      RanksMua05_h_Agg[[14]]$Param[RanksMua05_h_Agg[[14]]$EE05_h >= 0.01])))
HeatParamsots_Thresh = sort(unique(c(RanksMuaot_h_Agg[[1]]$Param[RanksMuaot_h_Agg[[1]]$EEot_h >= 0.01],
                                     RanksMuaot_h_Agg[[2]]$Param[RanksMuaot_h_Agg[[2]]$EEot_h >= 0.01],
                                     RanksMuaot_h_Agg[[3]]$Param[RanksMuaot_h_Agg[[3]]$EEot_h >= 0.01],
                                     RanksMuaot_h_Agg[[4]]$Param[RanksMuaot_h_Agg[[4]]$EEot_h >= 0.01],
                                     RanksMuaot_h_Agg[[5]]$Param[RanksMuaot_h_Agg[[5]]$EEot_h >= 0.01],
                                     RanksMuaot_h_Agg[[6]]$Param[RanksMuaot_h_Agg[[6]]$EEot_h >= 0.01],
                                     RanksMuaot_h_Agg[[7]]$Param[RanksMuaot_h_Agg[[7]]$EEot_h >= 0.01],
                                     RanksMuaot_h_Agg[[8]]$Param[RanksMuaot_h_Agg[[8]]$EEot_h >= 0.01],
                                     RanksMuaot_h_Agg[[9]]$Param[RanksMuaot_h_Agg[[9]]$EEot_h >= 0.01],
                                     RanksMuaot_h_Agg[[10]]$Param[RanksMuaot_h_Agg[[10]]$EEot_h >= 0.01],
                                     RanksMuaot_h_Agg[[11]]$Param[RanksMuaot_h_Agg[[11]]$EEot_h >= 0.01],
                                     RanksMuaot_h_Agg[[12]]$Param[RanksMuaot_h_Agg[[12]]$EEot_h >= 0.01],
                                     RanksMuaot_h_Agg[[13]]$Param[RanksMuaot_h_Agg[[13]]$EEot_h >= 0.01],
                                     RanksMuaot_h_Agg[[14]]$Param[RanksMuaot_h_Agg[[14]]$EEot_h >= 0.01])))
HeatParams95s_Thresh = sort(unique(c(RanksMua95_h_Agg[[1]]$Param[RanksMua95_h_Agg[[1]]$EE95_h >= 0.01],
                                     RanksMua95_h_Agg[[2]]$Param[RanksMua95_h_Agg[[2]]$EE95_h >= 0.01],
                                     RanksMua95_h_Agg[[3]]$Param[RanksMua95_h_Agg[[3]]$EE95_h >= 0.01],
                                     RanksMua95_h_Agg[[4]]$Param[RanksMua95_h_Agg[[4]]$EE95_h >= 0.01],
                                     RanksMua95_h_Agg[[5]]$Param[RanksMua95_h_Agg[[5]]$EE95_h >= 0.01],
                                     RanksMua95_h_Agg[[6]]$Param[RanksMua95_h_Agg[[6]]$EE95_h >= 0.01],
                                     RanksMua95_h_Agg[[7]]$Param[RanksMua95_h_Agg[[7]]$EE95_h >= 0.01],
                                     RanksMua95_h_Agg[[8]]$Param[RanksMua95_h_Agg[[8]]$EE95_h >= 0.01],
                                     RanksMua95_h_Agg[[9]]$Param[RanksMua95_h_Agg[[9]]$EE95_h >= 0.01],
                                     RanksMua95_h_Agg[[10]]$Param[RanksMua95_h_Agg[[10]]$EE95_h >= 0.01],
                                     RanksMua95_h_Agg[[11]]$Param[RanksMua95_h_Agg[[11]]$EE95_h >= 0.01],
                                     RanksMua95_h_Agg[[12]]$Param[RanksMua95_h_Agg[[12]]$EE95_h >= 0.01],
                                     RanksMua95_h_Agg[[13]]$Param[RanksMua95_h_Agg[[13]]$EE95_h >= 0.01],
                                     RanksMua95_h_Agg[[14]]$Param[RanksMua95_h_Agg[[14]]$EE95_h >= 0.01])))
HeatParamsTN05_Thresh = sort(unique(c(RanksMuaTN05_h_Agg[[1]]$Param[RanksMuaTN05_h_Agg[[1]]$EETN05_h >= 0.01],
                                     RanksMuaTN05_h_Agg[[2]]$Param[RanksMuaTN05_h_Agg[[2]]$EETN05_h >= 0.01],
                                     RanksMuaTN05_h_Agg[[3]]$Param[RanksMuaTN05_h_Agg[[3]]$EETN05_h >= 0.01],
                                     RanksMuaTN05_h_Agg[[4]]$Param[RanksMuaTN05_h_Agg[[4]]$EETN05_h >= 0.01],
                                     RanksMuaTN05_h_Agg[[5]]$Param[RanksMuaTN05_h_Agg[[5]]$EETN05_h >= 0.01],
                                     RanksMuaTN05_h_Agg[[6]]$Param[RanksMuaTN05_h_Agg[[6]]$EETN05_h >= 0.01],
                                     RanksMuaTN05_h_Agg[[7]]$Param[RanksMuaTN05_h_Agg[[7]]$EETN05_h >= 0.01],
                                     RanksMuaTN05_h_Agg[[8]]$Param[RanksMuaTN05_h_Agg[[8]]$EETN05_h >= 0.01],
                                     RanksMuaTN05_h_Agg[[9]]$Param[RanksMuaTN05_h_Agg[[9]]$EETN05_h >= 0.01],
                                     RanksMuaTN05_h_Agg[[10]]$Param[RanksMuaTN05_h_Agg[[10]]$EETN05_h >= 0.01],
                                     RanksMuaTN05_h_Agg[[11]]$Param[RanksMuaTN05_h_Agg[[11]]$EETN05_h >= 0.01],
                                     RanksMuaTN05_h_Agg[[12]]$Param[RanksMuaTN05_h_Agg[[12]]$EETN05_h >= 0.01],
                                     RanksMuaTN05_h_Agg[[13]]$Param[RanksMuaTN05_h_Agg[[13]]$EETN05_h >= 0.01],
                                     RanksMuaTN05_h_Agg[[14]]$Param[RanksMuaTN05_h_Agg[[14]]$EETN05_h >= 0.01])))
HeatParamsTNMed_Thresh = sort(unique(c(RanksMuaTNMed_h_Agg[[1]]$Param[RanksMuaTNMed_h_Agg[[1]]$EETNMed_h >= 0.01],
                                     RanksMuaTNMed_h_Agg[[2]]$Param[RanksMuaTNMed_h_Agg[[2]]$EETNMed_h >= 0.01],
                                     RanksMuaTNMed_h_Agg[[3]]$Param[RanksMuaTNMed_h_Agg[[3]]$EETNMed_h >= 0.01],
                                     RanksMuaTNMed_h_Agg[[4]]$Param[RanksMuaTNMed_h_Agg[[4]]$EETNMed_h >= 0.01],
                                     RanksMuaTNMed_h_Agg[[5]]$Param[RanksMuaTNMed_h_Agg[[5]]$EETNMed_h >= 0.01],
                                     RanksMuaTNMed_h_Agg[[6]]$Param[RanksMuaTNMed_h_Agg[[6]]$EETNMed_h >= 0.01],
                                     RanksMuaTNMed_h_Agg[[7]]$Param[RanksMuaTNMed_h_Agg[[7]]$EETNMed_h >= 0.01],
                                     RanksMuaTNMed_h_Agg[[8]]$Param[RanksMuaTNMed_h_Agg[[8]]$EETNMed_h >= 0.01],
                                     RanksMuaTNMed_h_Agg[[9]]$Param[RanksMuaTNMed_h_Agg[[9]]$EETNMed_h >= 0.01],
                                     RanksMuaTNMed_h_Agg[[10]]$Param[RanksMuaTNMed_h_Agg[[10]]$EETNMed_h >= 0.01],
                                     RanksMuaTNMed_h_Agg[[11]]$Param[RanksMuaTNMed_h_Agg[[11]]$EETNMed_h >= 0.01],
                                     RanksMuaTNMed_h_Agg[[12]]$Param[RanksMuaTNMed_h_Agg[[12]]$EETNMed_h >= 0.01],
                                     RanksMuaTNMed_h_Agg[[13]]$Param[RanksMuaTNMed_h_Agg[[13]]$EETNMed_h >= 0.01],
                                     RanksMuaTNMed_h_Agg[[14]]$Param[RanksMuaTNMed_h_Agg[[14]]$EETNMed_h >= 0.01])))
HeatParamsTN95_Thresh = sort(unique(c(RanksMuaTN95_h_Agg[[1]]$Param[RanksMuaTN95_h_Agg[[1]]$EETN95_h >= 0.01],
                                     RanksMuaTN95_h_Agg[[2]]$Param[RanksMuaTN95_h_Agg[[2]]$EETN95_h >= 0.01],
                                     RanksMuaTN95_h_Agg[[3]]$Param[RanksMuaTN95_h_Agg[[3]]$EETN95_h >= 0.01],
                                     RanksMuaTN95_h_Agg[[4]]$Param[RanksMuaTN95_h_Agg[[4]]$EETN95_h >= 0.01],
                                     RanksMuaTN95_h_Agg[[5]]$Param[RanksMuaTN95_h_Agg[[5]]$EETN95_h >= 0.01],
                                     RanksMuaTN95_h_Agg[[6]]$Param[RanksMuaTN95_h_Agg[[6]]$EETN95_h >= 0.01],
                                     RanksMuaTN95_h_Agg[[7]]$Param[RanksMuaTN95_h_Agg[[7]]$EETN95_h >= 0.01],
                                     RanksMuaTN95_h_Agg[[8]]$Param[RanksMuaTN95_h_Agg[[8]]$EETN95_h >= 0.01],
                                     RanksMuaTN95_h_Agg[[9]]$Param[RanksMuaTN95_h_Agg[[9]]$EETN95_h >= 0.01],
                                     RanksMuaTN95_h_Agg[[10]]$Param[RanksMuaTN95_h_Agg[[10]]$EETN95_h >= 0.01],
                                     RanksMuaTN95_h_Agg[[11]]$Param[RanksMuaTN95_h_Agg[[11]]$EETN95_h >= 0.01],
                                     RanksMuaTN95_h_Agg[[12]]$Param[RanksMuaTN95_h_Agg[[12]]$EETN95_h >= 0.01],
                                     RanksMuaTN95_h_Agg[[13]]$Param[RanksMuaTN95_h_Agg[[13]]$EETN95_h >= 0.01],
                                     RanksMuaTN95_h_Agg[[14]]$Param[RanksMuaTN95_h_Agg[[14]]$EETN95_h >= 0.01])))
HeatLabels05s = c('H', 'L3', 'L4', 'S108', 'S109', 'S8', 'S9', 'Tree', 'Grass', 'Z')
HeatLabels95s = c('H', 'L3', 'L4', 'S108', 'S109', 'S8', 'S9', 'Tree', 'Grass', 'Z')
colPal = colorRampPalette(colors = scico(n = 5, palette = 'batlow'))
scaleRange = c(1, 81)
scaleBy = 20
Pal = rev(colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1))
png('HillHeatRank95_05s_Agg_paper.png', res = 300, height = 7, width = 7, units = 'in')
par(mar = c(2,4,2,4.5), mgp = c(1,1,0))
for (h in 1:length(uhills)){
  #Loop over the top 10% ranks for basin
  for (j in 1:length(HeatParams05s_Thresh)){
    if(j == 1){
      if(h == 1){
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(HeatParams05s_Thresh)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMua05_h_Agg[[h]]$Param == HeatParams05s_Thresh[j])), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(0.5,0.5,14.5,14.5), y = c(0.5,97,97,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(HeatParams05s_Thresh)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua05_h_Agg[[h]]$Param == HeatParams05s_Thresh[j])), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(HeatParams05s_Thresh)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMua05_h_Agg[[h]]$Param == HeatParams05s_Thresh[j])), axes = FALSE, cex.lab = 1.5)
      }
    }else{
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(HeatParams05s_Thresh)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua05_h_Agg[[h]]$Param == HeatParams05s_Thresh[j])), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = c(0,1.5,2.5,4.5,12.5,20.5,27.5,40.5,67.5,87.5,96.5), labels = FALSE, las = 1)
axis(side = 2, at = c(1,2,3.5,11,17,23,32,56,77,92), labels = HeatLabels05s, las = 1, tick = 0)
legend('right', title = expression(bold('Rank')), legend = c('1 - 20', '21 - 40', '41 - 60', '61 - 80', '81 - 96'), pch = 15, col = colFun(seq(1,101,20)), inset = -0.15, xpd = TRUE)
lines(c(8.5,8.5), c(0,105), col = 'white')
par(xpd = TRUE)
text(x = 4.5, y = 98.5, expression(bold('   More\nForested')))
text(x = 11.5, y = 98.5, expression(bold('     More\nImpervious')))
par(xpd=FALSE)
box(which = 'figure')
dev.off()

png('HillHeatRank95_95s_Agg_paper.png', res = 300, height = 7, width = 7, units = 'in')
par(mar = c(2,4,2,4.5), mgp = c(1,1,0))
for (h in 1:length(uhills)){
  #Loop over the top 10% ranks for basin
  for (j in 1:length(HeatParams95s_Thresh)){
    if(j == 1){
      if(h == 1){
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(HeatParams95s_Thresh)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMua95_h_Agg[[h]]$Param == HeatParams95s_Thresh[j])), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(0.5,0.5,14.5,14.5), y = c(0.5,103,103,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(HeatParams95s_Thresh)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua95_h_Agg[[h]]$Param == HeatParams95s_Thresh[j])), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(HeatParams95s_Thresh)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMua95_h_Agg[[h]]$Param == HeatParams95s_Thresh[j])), axes = FALSE, cex.lab = 1.5)
      }
    }else{
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(HeatParams95s_Thresh)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua95_h_Agg[[h]]$Param == HeatParams95s_Thresh[j])), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = c(0,1.5,2.5,4.5,13.5,22.5,31.5,46.5,73.5,92.5,102.5), labels = FALSE, las = 1)
axis(side = 2, at = c(1,2,3.5,9,17,28,36,58,82,97), labels = HeatLabels95s, las = 1, tick = 0)
legend('right', title = expression(bold('Rank')), legend = c('1 - 20', '21 - 40', '41 - 60', '61 - 80', '81 - 96'), pch = 15, col = colFun(seq(1,101,20)), inset = -0.15, xpd = TRUE)
lines(c(8.5,8.5), c(0,105), col = 'white')
par(xpd = TRUE)
text(x = 4.5, y = 104, expression(bold('   More\nForested')))
text(x = 11.5, y = 104, expression(bold('     More\nImpervious')))
par(xpd=FALSE)
box(which = 'figure')
dev.off()

# Aggregated Rank order----
Ranks05_Agg = Ranksot_Agg = Ranks95_Agg = RanksTN05_Agg = RanksTNMed_Agg = RanksTN95_Agg = NULL
#  In or not in the selected parameters to calibrate aggregated over all 6 metrics----
scaleRange = c(0,1)
scaleBy = 1
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)
png('HillRankTop1295_All_Aggh_paper.png', res = 300, height = 7, width = 7, units = 'in')
par(mar = c(2,12.5,2,4.5), mgp = c(1,1,0))
for (h in 0:length(uhills)){
  #Loop over the top 10% ranks
  for (j in 1:length(SortRanksMua_h_Agg_paper)){
    if (h == 0){
      #Basin
      if (j == 1){
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(ifelse(SortRanksMua_h_Agg_paper[j] %in% unique(c(ParamSelect_b_Agg, ParamSelectTN_b_Agg)), 1, 0)), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(-0.5,-0.5,14.5,14.5), y = c(0.5,35.5,35.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(ifelse(SortRanksMua_h_Agg_paper[j] %in% unique(c(ParamSelect_b_Agg, ParamSelectTN_b_Agg)), 1, 0)), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(ifelse(SortRanksMua_h_Agg_paper[j] %in% unique(c(ParamSelect_b_Agg, ParamSelectTN_b_Agg)), 1, 0)), axes = FALSE)
      }
    }else{
      #Hillslopes
      plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(ifelse(SortRanksMua_h_Agg_paper[j] %in% unique(c(ph10[[h]], phTN10[[h]])), 1, 0)), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = 0, labels = 'B', pos = 0.5)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_h_Agg_paper),1), labels = LabelNames_h_paper, las = 1)
legend('right', title = expression(bold('Selected for\nCalibration?')), legend = c('Yes', 'No'), pch = 15, col = colFun(c(1,0)), inset = -0.23, xpd = TRUE, bty = 'n')
lines(c(-0.5,15), c(34.5,34.5), col = 'white')
lines(c(-0.5,15), c(33.5,33.5), col = 'white')
lines(c(-0.5,15), c(31.5,31.5), col = 'white')
lines(c(-0.5,15), c(25.5,25.5), col = 'white')
lines(c(-0.5,15), c(14.5,14.5), col = 'white')
lines(c(-0.5,15), c(6.5,6.5), col = 'white')
lines(c(-0.5,15), c(5.5,5.5), col = 'white')
lines(c(8.5,8.5), c(0,40), col = 'white')
lines(c(0.5,0.5), c(0,40), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(-1,8),y = c(3,6,10.5,20,27,32.5,34,35), col = colos_paper[c(5,4,4,2,2,3,3,1)], pch = c(16,17,16,17,16,15,18,16), axes=FALSE, xlab = '', ylab = '', xlim = c(-1,15), ylim = c(0,length(SortRanksMua_h_Agg_paper)))
text(x = 4.5, y = 36.5, expression(bold('   More\nForested')))
text(x = 11.5, y = 36.5, expression(bold('     More\nImpervious')))
text(x = 0, y = 37, expression(bold('Basin')), srt = 90)
par(xpd=FALSE)
box(which = 'figure')
dev.off()

#  Streamflow 5th %-ile----
#colPal = colorRampPalette(colors = rev(c('red', 'orange', 'gray', 'green', 'blue')))
colPal = colorRampPalette(colors = scico(n = 4, palette = 'nuuk'))
scaleRange = c(1, 34)
scaleBy = 11
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)
png('HillRankTop1295_05s_Agg.png', res = 300, height = 6, width = 6, units = 'in')
par(mar = c(1,11,0,5))
for (h in 1:length(uhills)){
  #Loop over the top 10% ranks for basin
  for (j in 1:length(SortRanksMua_b_Agg)){
    if(j == 1){
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg)), pch = 15, xlab = 'Hillslope ID', ylab = 'Parameter', col = colFun(which(RanksMua05_h_Agg[[h]]$Param == SortRanksMua_b_Agg[j])), axes = FALSE, cex.lab = 1.5)
    }else{
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua05_h_Agg[[h]]$Param == SortRanksMua_b_Agg[j])), axes = FALSE)
    }
    par(new=TRUE)
    Ranks05_Agg = c(Ranks05_Agg,which(RanksMua05_h_Agg[[h]]$Param == SortRanksMua_b_Agg[j]))
  }
}
rm(h,j)
par(new=FALSE)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_b_Agg),1), labels = LabelNames, las = 1)
#legend('top', title = expression(bold('Parameter Sensitivity Rank')), legend = c('1 - 14', '15 - 29', '30 - 44', '45 - 60', '61 - 101'), horiz = TRUE, pch = 15, col = colFun(seq(0,60,15)), inset = -0.1, xpd = TRUE)
legend('right', title = expression(bold('Rank')), legend = c('1 - 11', '12 - 22', '23 - 33', '>33'), pch = 15, col = colFun(seq(1,34,11)), inset = -0.3, xpd = TRUE)
dev.off()

scaleRange = c(1, 64)
scaleBy = 21
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)
png('HillRankTop1295_05s_Agg_paper.png', res = 300, height = 6, width = 6, units = 'in')
par(mar = c(2,11.5,2,4.5), mgp = c(1,1,0))
for (h in 1:length(uhills)){
  #Loop over the top 10% ranks for basin
  for (j in 1:length(SortRanksMua_b_Agg_paper)){
    if(j == 1){
      if(h == 1){
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMua05_h_Agg[[h]]$Param == SortRanksMua_b_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(0.5,0.5,14.5,14.5), y = c(0.5,21.5,21.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua05_h_Agg[[h]]$Param == SortRanksMua_b_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMua05_h_Agg[[h]]$Param == SortRanksMua_b_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }
    }else{
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua05_h_Agg[[h]]$Param == SortRanksMua_b_Agg_paper[j])), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_b_Agg_paper),1), labels = LabelNames_paper, las = 1)
legend('right', title = expression(bold('Rank')), legend = c('1 - 21', '22 - 42', '43 - 63', '>63'), pch = 15, col = colFun(seq(1,64,21)), inset = -0.3, xpd = TRUE)
lines(c(0,15), c(20.5,20.5), col = 'white')
lines(c(0,15), c(19.5,19.5), col = 'white')
lines(c(0,15), c(18.5,18.5), col = 'white')
lines(c(0,15), c(8.5,8.5), col = 'white')
lines(c(0,15), c(3.5,3.5), col = 'white')
lines(c(8.5,8.5), c(0,30), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(15,6),y = c(2,6,15,19,20,21), col = colos_paper[c(5,4,2,2,3,1)], pch = c(16,16,17,16,15,16), axes=FALSE, xlab = '', ylab = '', xlim = c(0,15), ylim = c(0,21))
text(x = 4.5, y = 22, expression(bold('   More\nForested')))
text(x = 11.5, y = 22, expression(bold('     More\nImpervious')))
par(xpd=FALSE)
box(which = 'figure')
dev.off()

scaleRange = c(1, 34)
scaleBy = 11
Pal = rev(colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1))
png('HillRankTop1295_05s_Aggh_paper.png', res = 300, height = 7, width = 7, units = 'in')
par(mar = c(2,12.5,2,4.5), mgp = c(1,1,0))
for (h in 0:length(uhills)){
  #Loop over the top 10% ranks
  for (j in 1:length(SortRanksMua_h_Agg_paper)){
    if (h == 0){
      #Basin
      if (j == 1){
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMua05_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(-0.5,-0.5,14.5,14.5), y = c(0.5,35.5,35.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua05_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua05_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE)
      }
    }else{
      #Hillslopes
      plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua05_h_Agg[[h]]$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = 0, labels = 'B', pos = 0.5)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_h_Agg_paper),1), labels = LabelNames_h_paper, las = 1)
legend('right', title = expression(bold('Rank')), legend = c('1 - 11', '12 - 22', '23 - 33', '>33'), pch = 15, col = colFun(seq(1,34,11)), inset = -0.2, xpd = TRUE)
lines(c(-0.5,15), c(34.5,34.5), col = 'white')
lines(c(-0.5,15), c(33.5,33.5), col = 'white')
lines(c(-0.5,15), c(31.5,31.5), col = 'white')
lines(c(-0.5,15), c(25.5,25.5), col = 'white')
lines(c(-0.5,15), c(14.5,14.5), col = 'white')
lines(c(-0.5,15), c(6.5,6.5), col = 'white')
lines(c(-0.5,15), c(5.5,5.5), col = 'white')
lines(c(8.5,8.5), c(0,40), col = 'white')
lines(c(0.5,0.5), c(0,40), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(-1,8),y = c(3,6,10.5,20,27,32.5,34,35), col = colos_paper[c(5,4,4,2,2,3,3,1)], pch = c(16,17,16,17,16,15,18,16), axes=FALSE, xlab = '', ylab = '', xlim = c(-1,15), ylim = c(0,length(SortRanksMua_h_Agg_paper)))
text(x = 4.5, y = 36.5, expression(bold('   More\nForested')))
text(x = 11.5, y = 36.5, expression(bold('     More\nImpervious')))
text(x = 0, y = 37, expression(bold('Basin')), srt = 90)
text(x = 17, y = 23, expression(bold('      Metric:\n  Streamflow\n   Lower 5th\n   Percentile')))
par(xpd=FALSE)
box(which = 'figure')
dev.off()

#  Streamflow 5th-95th %-ile----
scaleRange = c(1, 34)
scaleBy = 11
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)
png('HillRankTop1295_ots_Agg.png', res = 300, height = 6, width = 6, units = 'in')
par(mar = c(1,11,0,5))
for (h in 1:length(uhills)){
  #Loop over the top 10% ranks for basin
  for (j in 1:length(SortRanksMua_b_Agg)){
    if(j == 1){
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg)), pch = 15, xlab = 'Hillslope ID', ylab = 'Parameter', col = colFun(which(RanksMuaot_h_Agg[[h]]$Param == SortRanksMua_b_Agg[j])), axes = FALSE, cex.lab = 1.5)
    }else{
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaot_h_Agg[[h]]$Param == SortRanksMua_b_Agg[j])), axes = FALSE)
    }
    par(new=TRUE)
    Ranksot_Agg = c(Ranksot_Agg,which(RanksMuaot_h_Agg[[h]]$Param == SortRanksMua_b_Agg[j]))
  }
}
rm(h,j)
par(new=FALSE)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_b_Agg),1), labels = LabelNames, las = 1)
#legend('top', title = expression(bold('Parameter Sensitivity Rank')), legend = c('1 - 14', '15 - 29', '30 - 44', '45 - 60', '61 - 101'), horiz = TRUE, pch = 15, col = colFun(seq(0,60,15)), inset = -0.1, xpd = TRUE)
legend('right', title = expression(bold('Rank')), legend = c('1 - 11', '12 - 22', '23 - 33', '>33'), pch = 15, col = colFun(seq(1,34,11)), inset = -0.3, xpd = TRUE)
dev.off()

scaleRange = c(1, 64)
scaleBy = 21
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)
png('HillRankTop1295_ots_Agg_paper.png', res = 300, height = 6, width = 6, units = 'in')
par(mar = c(2,11.5,2,4.5), mgp = c(1,1,0))
for (h in 1:length(uhills)){
  #Loop over the top 10% ranks for basin
  for (j in 1:length(SortRanksMua_b_Agg_paper)){
    if(j == 1){
      if(h == 1){
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMuaot_h_Agg[[h]]$Param == SortRanksMua_b_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(0.5,0.5,14.5,14.5), y = c(0.5,21.5,21.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaot_h_Agg[[h]]$Param == SortRanksMua_b_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMuaot_h_Agg[[h]]$Param == SortRanksMua_b_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }
    }else{
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaot_h_Agg[[h]]$Param == SortRanksMua_b_Agg_paper[j])), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_b_Agg_paper),1), labels = LabelNames_paper, las = 1)
legend('right', title = expression(bold('Rank')), legend = c('1 - 21', '22 - 42', '43 - 63', '>63'), pch = 15, col = colFun(seq(1,64,21)), inset = -0.3, xpd = TRUE)
lines(c(0,15), c(20.5,20.5), col = 'white')
lines(c(0,15), c(19.5,19.5), col = 'white')
lines(c(0,15), c(18.5,18.5), col = 'white')
lines(c(0,15), c(8.5,8.5), col = 'white')
lines(c(0,15), c(3.5,3.5), col = 'white')
lines(c(8.5,8.5), c(0,30), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(15,6),y = c(2,6,15,19,20,21), col = colos_paper[c(5,4,2,2,3,1)], pch = c(16,16,17,16,15,16), axes=FALSE, xlab = '', ylab = '', xlim = c(0,15), ylim = c(0,21))
text(x = 4.5, y = 22, expression(bold('   More\nForested')))
text(x = 11.5, y = 22, expression(bold('     More\nImpervious')))
par(xpd=FALSE)
box(which = 'figure')
dev.off()

scaleRange = c(1, 34)
scaleBy = 11
Pal = rev(colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1))
png('HillRankTop1295_ots_Aggh_paper.png', res = 300, height = 7, width = 7, units = 'in')
par(mar = c(2,12.5,2,4.5), mgp = c(1,1,0))
for (h in 0:length(uhills)){
  #Loop over the top 10% ranks
  for (j in 1:length(SortRanksMua_h_Agg_paper)){
    if (h == 0){
      #Basin
      if (j == 1){
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMuaot_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(-0.5,-0.5,14.5,14.5), y = c(0.5,35.5,35.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaot_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaot_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE)
      }
    }else{
      #Hillslopes
      plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaot_h_Agg[[h]]$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = 0, labels = 'B', pos = 0.5)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_h_Agg_paper),1), labels = LabelNames_h_paper, las = 1)
legend('right', title = expression(bold('Rank')), legend = c('1 - 11', '12 - 22', '23 - 33', '>33'), pch = 15, col = colFun(seq(1,34,11)), inset = -0.2, xpd = TRUE)
lines(c(-0.5,15), c(34.5,34.5), col = 'white')
lines(c(-0.5,15), c(33.5,33.5), col = 'white')
lines(c(-0.5,15), c(31.5,31.5), col = 'white')
lines(c(-0.5,15), c(25.5,25.5), col = 'white')
lines(c(-0.5,15), c(14.5,14.5), col = 'white')
lines(c(-0.5,15), c(6.5,6.5), col = 'white')
lines(c(-0.5,15), c(5.5,5.5), col = 'white')
lines(c(8.5,8.5), c(0,40), col = 'white')
lines(c(0.5,0.5), c(0,40), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(-1,8),y = c(3,6,10.5,20,27,32.5,34,35), col = colos_paper[c(5,4,4,2,2,3,3,1)], pch = c(16,17,16,17,16,15,18,16), axes=FALSE, xlab = '', ylab = '', xlim = c(-1,15), ylim = c(0,length(SortRanksMua_h_Agg_paper)))
text(x = 4.5, y = 36.5, expression(bold('   More\nForested')))
text(x = 11.5, y = 36.5, expression(bold('     More\nImpervious')))
text(x = 0, y = 37, expression(bold('Basin')), srt = 90)
text(x = 17, y = 23, expression(bold('    Metric:\nStreamflow\n   5th-95th\n Percentile')))
par(xpd=FALSE)
box(which = 'figure')
dev.off()

#  Streamflow 95th %-ile----
scaleRange = c(1, 34)
scaleBy = 11
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)
png('HillRankTop1295_95s_Agg.png', res = 300, height = 6, width = 6, units = 'in')
par(mar = c(1,11,0,5))
for (h in 1:length(uhills)){
  #Loop over the top 10% ranks for basin
  for (j in 1:length(SortRanksMua_b_Agg)){
    if(j == 1){
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg)), pch = 15, xlab = 'Hillslope ID', ylab = 'Parameter', col = colFun(which(RanksMua95_h_Agg[[h]]$Param == SortRanksMua_b_Agg[j])), axes = FALSE, cex.lab = 1.5)
    }else{
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua95_h_Agg[[h]]$Param == SortRanksMua_b_Agg[j])), axes = FALSE)
    }
    par(new=TRUE)
    Ranks95_Agg = c(Ranks95_Agg,which(RanksMua95_h_Agg[[h]]$Param == SortRanksMua_b_Agg[j]))
  }
}
rm(h,j)
par(new=FALSE)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_b_Agg),1), labels = LabelNames, las = 1)
#legend('top', title = expression(bold('Parameter Sensitivity Rank')), legend = c('1 - 14', '15 - 29', '30 - 44', '45 - 60', '61 - 101'), horiz = TRUE, pch = 15, col = colFun(seq(0,60,15)), inset = -0.1, xpd = TRUE)
legend('right', title = expression(bold('Rank')), legend = c('1 - 11', '12 - 22', '23 - 33', '>33'), pch = 15, col = colFun(seq(1,34,11)), inset = -0.3, xpd = TRUE)
dev.off()

scaleRange = c(1, 64)
scaleBy = 21
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)
png('HillRankTop1295_95s_Agg_paper.png', res = 300, height = 6, width = 6, units = 'in')
par(mar = c(2,11.5,2,4.5), mgp = c(1,1,0))
for (h in 1:length(uhills)){
  #Loop over the top 10% ranks for basin
  for (j in 1:length(SortRanksMua_b_Agg_paper)){
    if(j == 1){
      if(h == 1){
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMua95_h_Agg[[h]]$Param == SortRanksMua_b_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(0.5,0.5,14.5,14.5), y = c(0.5,21.5,21.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua95_h_Agg[[h]]$Param == SortRanksMua_b_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMua95_h_Agg[[h]]$Param == SortRanksMua_b_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }
    }else{
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua95_h_Agg[[h]]$Param == SortRanksMua_b_Agg_paper[j])), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
#axis(side = 2, at = seq(1,length(SortRanksMua_b_Agg_paper),1), labels = SortRanksMua_b_Agg_paper, las = 1)
axis(side = 2, at = seq(1,length(SortRanksMua_b_Agg_paper),1), labels = LabelNames_paper, las = 1)
#legend('top', title = expression(bold('Parameter Sensitivity Rank')), legend = c('1 - 14', '15 - 29', '30 - 44', '45 - 60', '61 - 101'), horiz = TRUE, pch = 15, col = colFun(seq(0,60,15)), inset = -0.1, xpd = TRUE)
legend('right', title = expression(bold('Rank')), legend = c('1 - 21', '22 - 42', '43 - 63', '>63'), pch = 15, col = colFun(seq(1,64,21)), inset = -0.3, xpd = TRUE)
lines(c(0,15), c(20.5,20.5), col = 'white')
lines(c(0,15), c(19.5,19.5), col = 'white')
lines(c(0,15), c(18.5,18.5), col = 'white')
lines(c(0,15), c(8.5,8.5), col = 'white')
lines(c(0,15), c(3.5,3.5), col = 'white')
lines(c(8.5,8.5), c(0,30), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(15,6),y = c(2,6,15,19,20,21), col = colos_paper[c(5,4,2,2,3,1)], pch = c(16,16,17,16,15,16), axes=FALSE, xlab = '', ylab = '', xlim = c(0,15), ylim = c(0,21))
text(x = 4.5, y = 22, expression(bold('   More\nForested')))
text(x = 11.5, y = 22, expression(bold('     More\nImpervious')))
par(xpd=FALSE)
box(which = 'figure')
dev.off()

scaleRange = c(1, 34)
scaleBy = 11
Pal = rev(colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1))
png('HillRankTop1295_95s_Aggh_paper.png', res = 300, height = 7, width = 7, units = 'in')
par(mar = c(2,12.5,2,4.5), mgp = c(1,1,0))
for (h in 0:length(uhills)){
  #Loop over the top 10% ranks
  for (j in 1:length(SortRanksMua_h_Agg_paper)){
    if (h == 0){
      #Basin
      if (j == 1){
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMua95_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(-0.5,-0.5,14.5,14.5), y = c(0.5,35.5,35.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua95_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua95_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE)
      }
    }else{
      #Hillslopes
      plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua95_h_Agg[[h]]$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = 0, labels = 'B', pos = 0.5)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_h_Agg_paper),1), labels = LabelNames_h_paper, las = 1)
legend('right', title = expression(bold('Rank')), legend = c('1 - 11', '12 - 22', '23 - 33', '>33'), pch = 15, col = colFun(seq(1,34,11)), inset = -0.2, xpd = TRUE)
lines(c(-0.5,15), c(34.5,34.5), col = 'white')
lines(c(-0.5,15), c(33.5,33.5), col = 'white')
lines(c(-0.5,15), c(31.5,31.5), col = 'white')
lines(c(-0.5,15), c(25.5,25.5), col = 'white')
lines(c(-0.5,15), c(14.5,14.5), col = 'white')
lines(c(-0.5,15), c(6.5,6.5), col = 'white')
lines(c(-0.5,15), c(5.5,5.5), col = 'white')
lines(c(8.5,8.5), c(0,40), col = 'white')
lines(c(0.5,0.5), c(0,40), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(-1,8),y = c(3,6,10.5,20,27,32.5,34,35), col = colos_paper[c(5,4,4,2,2,3,3,1)], pch = c(16,17,16,17,16,15,18,16), axes=FALSE, xlab = '', ylab = '', xlim = c(-1,15), ylim = c(0,length(SortRanksMua_h_Agg_paper)))
text(x = 4.5, y = 36.5, expression(bold('   More\nForested')))
text(x = 11.5, y = 36.5, expression(bold('     More\nImpervious')))
text(x = 0, y = 37, expression(bold('Basin')), srt = 90)
text(x = 17, y = 23, expression(bold('      Metric:\n  Streamflow\n   Upper 5th\n   Percentile')))
par(xpd=FALSE)
box(which = 'figure')
dev.off()

#  TN 5th %-ile----
scaleRange = c(1, 34)
scaleBy = 11
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)
png('HillRankTop1295_TN05_Agg.png', res = 300, height = 6, width = 6, units = 'in')
par(mar = c(1,11,0,5))
for (h in 1:length(uhills)){
  #Loop over the top 10% ranks for basin
  for (j in 1:length(SortRanksMua_b_Agg)){
    if(j == 1){
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg)), pch = 15, xlab = 'Hillslope ID', ylab = 'Parameter', col = colFun(which(RanksMuaTN05_h_Agg[[h]]$Param == SortRanksMua_b_Agg[j])), axes = FALSE, cex.lab = 1.5)
    }else{
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaTN05_h_Agg[[h]]$Param == SortRanksMua_b_Agg[j])), axes = FALSE)
    }
    par(new=TRUE)
    RanksTN05_Agg = c(RanksTN05_Agg,which(RanksMuaTN05_h_Agg[[h]]$Param == SortRanksMua_b_Agg[j]))
  }
}
rm(h,j)
par(new=FALSE)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_b_Agg),1), labels = LabelNames, las = 1)
#legend('top', title = expression(bold('Parameter Sensitivity Rank')), legend = c('1 - 14', '15 - 29', '30 - 44', '45 - 60', '61 - 101'), horiz = TRUE, pch = 15, col = colFun(seq(0,60,15)), inset = -0.1, xpd = TRUE)
legend('right', title = expression(bold('Rank')), legend = c('1 - 11', '12 - 22', '23 - 33', '>33'), pch = 15, col = colFun(seq(1,34,11)), inset = -0.3, xpd = TRUE)
dev.off()

scaleRange = c(1, 64)
scaleBy = 21
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)
png('HillRankTop1295_TN05_Agg_paper.png', res = 300, height = 6, width = 6, units = 'in')
par(mar = c(2,11.5,2,4.5), mgp = c(1,1,0))
for (h in 1:length(uhills)){
  #Loop over the top 10% ranks for basin
  for (j in 1:length(SortRanksMua_b_Agg_paper)){
    if(j == 1){
      if(h == 1){
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMuaTN05_h_Agg[[h]]$Param == SortRanksMua_b_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(0.5,0.5,14.5,14.5), y = c(0.5,21.5,21.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaTN05_h_Agg[[h]]$Param == SortRanksMua_b_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMuaTN05_h_Agg[[h]]$Param == SortRanksMua_b_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }
    }else{
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaTN05_h_Agg[[h]]$Param == SortRanksMua_b_Agg_paper[j])), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_b_Agg_paper),1), labels = LabelNames_paper, las = 1)
legend('right', title = expression(bold('Rank')), legend = c('1 - 21', '22 - 42', '43 - 63', '>63'), pch = 15, col = colFun(seq(1,64,21)), inset = -0.3, xpd = TRUE)
lines(c(0,15), c(20.5,20.5), col = 'white')
lines(c(0,15), c(19.5,19.5), col = 'white')
lines(c(0,15), c(18.5,18.5), col = 'white')
lines(c(0,15), c(8.5,8.5), col = 'white')
lines(c(0,15), c(3.5,3.5), col = 'white')
lines(c(8.5,8.5), c(0,30), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(15,6),y = c(2,6,15,19,20,21), col = colos_paper[c(5,4,2,2,3,1)], pch = c(16,16,17,16,15,16), axes=FALSE, xlab = '', ylab = '', xlim = c(0,15), ylim = c(0,21))
text(x = 4.5, y = 22, expression(bold('   More\nForested')))
text(x = 11.5, y = 22, expression(bold('     More\nImpervious')))
par(xpd=FALSE)
box(which = 'figure')
dev.off()

scaleRange = c(1, 34)
scaleBy = 11
Pal = rev(colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1))
png('HillRankTop1295_TN05s_Aggh_paper.png', res = 300, height = 7, width = 7, units = 'in')
par(mar = c(2,12.5,2,4.5), mgp = c(1,1,0))
for (h in 0:length(uhills)){
  #Loop over the top 10% ranks
  for (j in 1:length(SortRanksMua_h_Agg_paper)){
    if (h == 0){
      #Basin
      if (j == 1){
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMuaTN05_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(-0.5,-0.5,14.5,14.5), y = c(0.5,35.5,35.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaTN05_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaTN05_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE)
      }
    }else{
      #Hillslopes
      plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaTN05_h_Agg[[h]]$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = 0, labels = 'B', pos = 0.5)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_h_Agg_paper),1), labels = LabelNames_h_paper, las = 1)
legend('right', title = expression(bold('Rank')), legend = c('1 - 11', '12 - 22', '23 - 33', '>33'), pch = 15, col = colFun(seq(1,34,11)), inset = -0.2, xpd = TRUE)
lines(c(-0.5,15), c(34.5,34.5), col = 'white')
lines(c(-0.5,15), c(33.5,33.5), col = 'white')
lines(c(-0.5,15), c(31.5,31.5), col = 'white')
lines(c(-0.5,15), c(25.5,25.5), col = 'white')
lines(c(-0.5,15), c(14.5,14.5), col = 'white')
lines(c(-0.5,15), c(6.5,6.5), col = 'white')
lines(c(-0.5,15), c(5.5,5.5), col = 'white')
lines(c(8.5,8.5), c(0,40), col = 'white')
lines(c(0.5,0.5), c(0,40), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(-1,8),y = c(3,6,10.5,20,27,32.5,34,35), col = colos_paper[c(5,4,4,2,2,3,3,1)], pch = c(16,17,16,17,16,15,18,16), axes=FALSE, xlab = '', ylab = '', xlim = c(-1,15), ylim = c(0,length(SortRanksMua_h_Agg_paper)))
text(x = 4.5, y = 36.5, expression(bold('   More\nForested')))
text(x = 11.5, y = 36.5, expression(bold('     More\nImpervious')))
text(x = 0, y = 37, expression(bold('Basin')), srt = 90)
text(x = 17, y = 23, expression(bold('  Metric: TN\n   Lower 5th\n   Percentile')))
par(xpd=FALSE)
box(which = 'figure')
dev.off()

#  TN 5th-95th %-ile----
scaleRange = c(1, 34)
scaleBy = 11
png('HillRankTop1295_TNMed_Agg.png', res = 300, height = 6, width = 6, units = 'in')
par(mar = c(1,11,0,5))
for (h in 1:length(uhills)){
  #Loop over the top 10% ranks for basin
  for (j in 1:length(SortRanksMua_b_Agg)){
    if(j == 1){
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg)), pch = 15, xlab = 'Hillslope ID', ylab = 'Parameter', col = colFun(which(RanksMuaTNMed_h_Agg[[h]]$Param == SortRanksMua_b_Agg[j])), axes = FALSE, cex.lab = 1.5)
    }else{
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaTNMed_h_Agg[[h]]$Param == SortRanksMua_b_Agg[j])), axes = FALSE)
    }
    par(new=TRUE)
    RanksTNMed_Agg = c(RanksTNMed_Agg,which(RanksMuaTNMed_h_Agg[[h]]$Param == SortRanksMua_b_Agg[j]))
  }
}
rm(h,j)
par(new=FALSE)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_b_Agg),1), labels = LabelNames, las = 1)
#legend('top', title = expression(bold('Parameter Sensitivity Rank')), legend = c('1 - 14', '15 - 29', '30 - 44', '45 - 60', '61 - 101'), horiz = TRUE, pch = 15, col = colFun(seq(0,60,15)), inset = -0.1, xpd = TRUE)
legend('right', title = expression(bold('Rank')), legend = c('1 - 11', '12 - 22', '23 - 33', '>33'), pch = 15, col = colFun(seq(1,34,11)), inset = -0.3, xpd = TRUE)
dev.off()

scaleRange = c(1, 64)
scaleBy = 21
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)
png('HillRankTop1295_TNMed_Agg_paper.png', res = 300, height = 6, width = 6, units = 'in')
par(mar = c(2,11.5,2,4.5), mgp = c(1,1,0))
for (h in 1:length(uhills)){
  #Loop over the top 10% ranks for basin
  for (j in 1:length(SortRanksMua_b_Agg_paper)){
    if(j == 1){
      if(h == 1){
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMuaTNMed_h_Agg[[h]]$Param == SortRanksMua_b_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(0.5,0.5,14.5,14.5), y = c(0.5,21.5,21.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaTNMed_h_Agg[[h]]$Param == SortRanksMua_b_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMuaTNMed_h_Agg[[h]]$Param == SortRanksMua_b_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }
    }else{
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaTNMed_h_Agg[[h]]$Param == SortRanksMua_b_Agg_paper[j])), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_b_Agg_paper),1), labels = LabelNames_paper, las = 1)
legend('right', title = expression(bold('Rank')), legend = c('1 - 21', '22 - 42', '43 - 63', '>63'), pch = 15, col = colFun(seq(1,64,21)), inset = -0.3, xpd = TRUE)
lines(c(0,15), c(20.5,20.5), col = 'white')
lines(c(0,15), c(19.5,19.5), col = 'white')
lines(c(0,15), c(18.5,18.5), col = 'white')
lines(c(0,15), c(8.5,8.5), col = 'white')
lines(c(0,15), c(3.5,3.5), col = 'white')
lines(c(8.5,8.5), c(0,30), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(15,6),y = c(2,6,15,19,20,21), col = colos_paper[c(5,4,2,2,3,1)], pch = c(16,16,17,16,15,16), axes=FALSE, xlab = '', ylab = '', xlim = c(0,15), ylim = c(0,21))
text(x = 4.5, y = 22, expression(bold('   More\nForested')))
text(x = 11.5, y = 22, expression(bold('     More\nImpervious')))
par(xpd=FALSE)
box(which = 'figure')
dev.off()

scaleRange = c(1, 34)
scaleBy = 11
Pal = rev(colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1))
png('HillRankTop1295_TNMeds_Aggh_paper.png', res = 300, height = 7, width = 7, units = 'in')
par(mar = c(2,12.5,2,4.5), mgp = c(1,1,0))
for (h in 0:length(uhills)){
  #Loop over the top 10% ranks
  for (j in 1:length(SortRanksMua_h_Agg_paper)){
    if (h == 0){
      #Basin
      if (j == 1){
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMuaTNMed_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(-0.5,-0.5,14.5,14.5), y = c(0.5,35.5,35.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaTNMed_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaTNMed_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE)
      }
    }else{
      #Hillslopes
      plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaTNMed_h_Agg[[h]]$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = 0, labels = 'B', pos = 0.5)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_h_Agg_paper),1), labels = LabelNames_h_paper, las = 1)
legend('right', title = expression(bold('Rank')), legend = c('1 - 11', '12 - 22', '23 - 33', '>33'), pch = 15, col = colFun(seq(1,34,11)), inset = -0.2, xpd = TRUE)
lines(c(-0.5,15), c(34.5,34.5), col = 'white')
lines(c(-0.5,15), c(33.5,33.5), col = 'white')
lines(c(-0.5,15), c(31.5,31.5), col = 'white')
lines(c(-0.5,15), c(25.5,25.5), col = 'white')
lines(c(-0.5,15), c(14.5,14.5), col = 'white')
lines(c(-0.5,15), c(6.5,6.5), col = 'white')
lines(c(-0.5,15), c(5.5,5.5), col = 'white')
lines(c(8.5,8.5), c(0,40), col = 'white')
lines(c(0.5,0.5), c(0,40), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(-1,8),y = c(3,6,10.5,20,27,32.5,34,35), col = colos_paper[c(5,4,4,2,2,3,3,1)], pch = c(16,17,16,17,16,15,18,16), axes=FALSE, xlab = '', ylab = '', xlim = c(-1,15), ylim = c(0,length(SortRanksMua_h_Agg_paper)))
text(x = 4.5, y = 36.5, expression(bold('   More\nForested')))
text(x = 11.5, y = 36.5, expression(bold('     More\nImpervious')))
text(x = 0, y = 37, expression(bold('Basin')), srt = 90)
text(x = 17, y = 23, expression(bold('  Metric: TN\n    Median\n   Percentile')))
par(xpd=FALSE)
box(which = 'figure')
dev.off()

#  TN 95th %-ile----
scaleRange = c(1, 34)
scaleBy = 11
png('HillRankTop1295_TN95_Agg.png', res = 300, height = 6, width = 6, units = 'in')
par(mar = c(1,11,0,5))
for (h in 1:length(uhills)){
  #Loop over the top 10% ranks for basin
  for (j in 1:length(SortRanksMua_b_Agg)){
    if(j == 1){
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg)), pch = 15, xlab = 'Hillslope ID', ylab = 'Parameter', col = colFun(which(RanksMuaTN95_h_Agg[[h]]$Param == SortRanksMua_b_Agg[j])), axes = FALSE, cex.lab = 1.5)
    }else{
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaTN95_h_Agg[[h]]$Param == SortRanksMua_b_Agg[j])), axes = FALSE)
    }
    par(new=TRUE)
    RanksTN95_Agg = c(RanksTN95_Agg,which(RanksMuaTN95_h_Agg[[h]]$Param == SortRanksMua_b_Agg[j]))
  }
}
rm(h,j)
par(new=FALSE)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_b_Agg),1), labels = SortRanksMua_b_Agg, las = 1)
#legend('top', title = expression(bold('Parameter Sensitivity Rank')), legend = c('1 - 14', '15 - 29', '30 - 44', '45 - 60', '61 - 101'), horiz = TRUE, pch = 15, col = colFun(seq(0,60,15)), inset = -0.1, xpd = TRUE)
legend('right', title = expression(bold('Rank')), legend = c('1 - 11', '12 - 22', '23 - 33', '>33'), pch = 15, col = colFun(seq(1,34,11)), inset = -0.3, xpd = TRUE)
dev.off()

scaleRange = c(1, 64)
scaleBy = 21
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)
png('HillRankTop1295_TN95_Agg_paper.png', res = 300, height = 6, width = 6, units = 'in')
par(mar = c(2,11.5,2,4.5), mgp = c(1,1,0))
for (h in 1:length(uhills)){
  #Loop over the top 10% ranks for basin
  for (j in 1:length(SortRanksMua_b_Agg_paper)){
    if(j == 1){
      if(h == 1){
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMuaTN95_h_Agg[[h]]$Param == SortRanksMua_b_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(0.5,0.5,14.5,14.5), y = c(0.5,21.5,21.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaTN95_h_Agg[[h]]$Param == SortRanksMua_b_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMuaTN95_h_Agg[[h]]$Param == SortRanksMua_b_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }
    }else{
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_b_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaTN95_h_Agg[[h]]$Param == SortRanksMua_b_Agg_paper[j])), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_b_Agg_paper),1), labels = LabelNames_paper, las = 1)
legend('right', title = expression(bold('Rank')), legend = c('1 - 21', '22 - 42', '43 - 63', '>63'), pch = 15, col = colFun(seq(1,64,21)), inset = -0.3, xpd = TRUE)
lines(c(0,15), c(20.5,20.5), col = 'white')
lines(c(0,15), c(19.5,19.5), col = 'white')
lines(c(0,15), c(18.5,18.5), col = 'white')
lines(c(0,15), c(8.5,8.5), col = 'white')
lines(c(0,15), c(3.5,3.5), col = 'white')
lines(c(8.5,8.5), c(0,30), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(15,6),y = c(2,6,15,19,20,21), col = colos_paper[c(5,4,2,2,3,1)], pch = c(16,16,17,16,15,16), axes=FALSE, xlab = '', ylab = '', xlim = c(0,15), ylim = c(0,21))
text(x = 4.5, y = 22, expression(bold('   More\nForested')))
text(x = 11.5, y = 22, expression(bold('     More\nImpervious')))
par(xpd=FALSE)
box(which = 'figure')
dev.off()

scaleRange = c(1, 34)
scaleBy = 11
Pal = rev(colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1))
png('HillRankTop1295_TN95s_Aggh_paper.png', res = 300, height = 7, width = 7, units = 'in')
par(mar = c(2,12.5,2,4.5), mgp = c(1,1,0))
for (h in 0:length(uhills)){
  #Loop over the top 10% ranks
  for (j in 1:length(SortRanksMua_h_Agg_paper)){
    if (h == 0){
      #Basin
      if (j == 1){
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMuaTN95_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(-0.5,-0.5,14.5,14.5), y = c(0.5,35.5,35.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaTN95_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaTN95_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE)
      }
    }else{
      #Hillslopes
      plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaTN95_h_Agg[[h]]$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = 0, labels = 'B', pos = 0.5)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_h_Agg_paper),1), labels = LabelNames_h_paper, las = 1)
legend('right', title = expression(bold('Rank')), legend = c('1 - 11', '12 - 22', '23 - 33', '>33'), pch = 15, col = colFun(seq(1,34,11)), inset = -0.2, xpd = TRUE)
lines(c(-0.5,15), c(34.5,34.5), col = 'white')
lines(c(-0.5,15), c(33.5,33.5), col = 'white')
lines(c(-0.5,15), c(31.5,31.5), col = 'white')
lines(c(-0.5,15), c(25.5,25.5), col = 'white')
lines(c(-0.5,15), c(14.5,14.5), col = 'white')
lines(c(-0.5,15), c(6.5,6.5), col = 'white')
lines(c(-0.5,15), c(5.5,5.5), col = 'white')
lines(c(8.5,8.5), c(0,40), col = 'white')
lines(c(0.5,0.5), c(0,40), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(-1,8),y = c(3,6,10.5,20,27,32.5,34,35), col = colos_paper[c(5,4,4,2,2,3,3,1)], pch = c(16,17,16,17,16,15,18,16), axes=FALSE, xlab = '', ylab = '', xlim = c(-1,15), ylim = c(0,length(SortRanksMua_h_Agg_paper)))
text(x = 4.5, y = 36.5, expression(bold('   More\nForested')))
text(x = 11.5, y = 36.5, expression(bold('     More\nImpervious')))
text(x = 0, y = 37, expression(bold('Basin')), srt = 90)
text(x = 17, y = 23, expression(bold('  Metric: TN\n  Upper 5th \n   Percentile')))
par(xpd=FALSE)
box(which = 'figure')
dev.off()

#  Streamflow 5th %-ile - hillslope params top 10%----
png('HillRankTop1295_05s_h_Agg.png', res = 300, height = 7, width = 7, units = 'in')
par(mar = c(1,14,0,5))
for (h in 1:length(uhills)){
  #Loop over the top 10% ranks for basin
  for (j in 1:length(SortRanksMua_h_Agg)){
    if(j == 1){
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_h_Agg)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMua05_h_Agg[[h]]$Param == SortRanksMua_h_Agg[j])), axes = FALSE, cex.lab = 1.5)
    }else{
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_h_Agg)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua05_h_Agg[[h]]$Param == SortRanksMua_h_Agg[j])), axes = FALSE)
    }
    par(new=TRUE)
    Ranks05_Agg = c(Ranks05_Agg,which(RanksMua05_h_Agg[[h]]$Param == SortRanksMua_h_Agg[j]))
  }
}
rm(h,j)
par(new=FALSE)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_h_Agg),1), labels = SortRanksMua_h_Agg, las = 1)
#legend('top', title = expression(bold('Parameter Sensitivity Rank')), legend = c('1 - 14', '15 - 29', '30 - 44', '45 - 60', '61 - 101'), horiz = TRUE, pch = 15, col = colFun(seq(0,60,15)), inset = -0.1, xpd = TRUE)
legend('right', title = expression(bold('Rank')), legend = c('1 - 14', '15 - 29', '30 - 44', '45 - 60', '61 - 101'), pch = 15, col = colFun(seq(0,60,15)), inset = -0.3, xpd = TRUE)
dev.off()

#  Streamflow 5th-95th %-ile - hillslope params top 10%----
png('HillRankTop1295_ots_h_Agg.png', res = 300, height = 7, width = 7, units = 'in')
par(mar = c(1,14,0,5))
for (h in 1:length(uhills)){
  #Loop over the top 10% ranks for basin
  for (j in 1:length(SortRanksMua_h_Agg)){
    if(j == 1){
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_h_Agg)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMuaot_h_Agg[[h]]$Param == SortRanksMua_h_Agg[j])), axes = FALSE, cex.lab = 1.5)
    }else{
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_h_Agg)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaot_h_Agg[[h]]$Param == SortRanksMua_h_Agg[j])), axes = FALSE)
    }
    par(new=TRUE)
    Ranksot_Agg = c(Ranksot_Agg,which(RanksMuaot_h_Agg[[h]]$Param == SortRanksMua_h_Agg[j]))
  }
}
rm(h,j)
par(new=FALSE)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_h_Agg),1), labels = SortRanksMua_h_Agg, las = 1)
#legend('top', title = expression(bold('Parameter Sensitivity Rank')), legend = c('1 - 14', '15 - 29', '30 - 44', '45 - 60', '61 - 101'), horiz = TRUE, pch = 15, col = colFun(seq(0,60,15)), inset = -0.1, xpd = TRUE)
legend('right', title = expression(bold('Rank')), legend = c('1 - 14', '15 - 29', '30 - 44', '45 - 60', '61 - 101'), pch = 15, col = colFun(seq(0,60,15)), inset = -0.3, xpd = TRUE)
dev.off()

#  Streamflow 95th %-ile - hillslope params top 10%----
png('HillRankTop1295_95s_h_Agg.png', res = 300, height = 7, width = 7, units = 'in')
par(mar = c(1,14,0,5))
for (h in 1:length(uhills)){
  #Loop over the top 10% ranks for basin
  for (j in 1:length(SortRanksMua_h_Agg)){
    if(j == 1){
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_h_Agg)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMua95_h_Agg[[h]]$Param == SortRanksMua_h_Agg[j])), axes = FALSE, cex.lab = 1.5)
    }else{
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_h_Agg)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua95_h_Agg[[h]]$Param == SortRanksMua_h_Agg[j])), axes = FALSE)
    }
    par(new=TRUE)
    Ranks95_Agg = c(Ranks95_Agg,which(RanksMua95_h_Agg[[h]]$Param == SortRanksMua_h_Agg[j]))
  }
}
rm(h,j)
par(new=FALSE)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_h_Agg),1), labels = SortRanksMua_h_Agg, las = 1)
#legend('top', title = expression(bold('Parameter Sensitivity Rank')), legend = c('1 - 14', '15 - 29', '30 - 44', '45 - 60', '61 - 101'), horiz = TRUE, pch = 15, col = colFun(seq(0,60,15)), inset = -0.1, xpd = TRUE)
legend('right', title = expression(bold('Rank')), legend = c('1 - 14', '15 - 29', '30 - 44', '45 - 60', '61 - 101'), pch = 15, col = colFun(seq(0,60,15)), inset = -0.3, xpd = TRUE)
dev.off()

#  TN 5th %-ile - hillslope params top 10%----
png('HillRankTop1295_TN05_h_Agg.png', res = 300, height = 7, width = 7, units = 'in')
par(mar = c(1,14,0,5))
for (h in 1:length(uhills)){
  #Loop over the top 10% ranks for basin
  for (j in 1:length(SortRanksMua_h_Agg)){
    if(j == 1){
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_h_Agg)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMuaTN05_h_Agg[[h]]$Param == SortRanksMua_h_Agg[j])), axes = FALSE, cex.lab = 1.5)
    }else{
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_h_Agg)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaTN05_h_Agg[[h]]$Param == SortRanksMua_h_Agg[j])), axes = FALSE)
    }
    par(new=TRUE)
    RanksTN05_Agg = c(RanksTN05_Agg,which(RanksMuaTN05_h_Agg[[h]]$Param == SortRanksMua_h_Agg[j]))
  }
}
rm(h,j)
par(new=FALSE)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_h_Agg),1), labels = SortRanksMua_h_Agg, las = 1)
#legend('top', title = expression(bold('Parameter Sensitivity Rank')), legend = c('1 - 14', '15 - 29', '30 - 44', '45 - 60', '61 - 101'), horiz = TRUE, pch = 15, col = colFun(seq(0,60,15)), inset = -0.1, xpd = TRUE)
legend('right', title = expression(bold('Rank')), legend = c('1 - 14', '15 - 29', '30 - 44', '45 - 60', '61 - 101'), pch = 15, col = colFun(seq(0,60,15)), inset = -0.3, xpd = TRUE)
dev.off()

#  TN 5th-95th %-ile - hillslope params top 10%----
png('HillRankTop1295_TNMed_h_Agg.png', res = 300, height = 7, width = 7, units = 'in')
par(mar = c(1,14,0,5))
for (h in 1:length(uhills)){
  #Loop over the top 10% ranks for basin
  for (j in 1:length(SortRanksMua_h_Agg)){
    if(j == 1){
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_h_Agg)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMuaTNMed_h_Agg[[h]]$Param == SortRanksMua_h_Agg[j])), axes = FALSE, cex.lab = 1.5)
    }else{
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_h_Agg)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaTNMed_h_Agg[[h]]$Param == SortRanksMua_h_Agg[j])), axes = FALSE)
    }
    par(new=TRUE)
    RanksTNMed_Agg = c(RanksTNMed_Agg,which(RanksMuaTNMed_h_Agg[[h]]$Param == SortRanksMua_h_Agg[j]))
  }
}
rm(h,j)
par(new=FALSE)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_h_Agg),1), labels = SortRanksMua_h_Agg, las = 1)
#legend('top', title = expression(bold('Parameter Sensitivity Rank')), legend = c('1 - 14', '15 - 29', '30 - 44', '45 - 60', '61 - 101'), horiz = TRUE, pch = 15, col = colFun(seq(0,60,15)), inset = -0.1, xpd = TRUE)
legend('right', title = expression(bold('Rank')), legend = c('1 - 14', '15 - 29', '30 - 44', '45 - 60', '61 - 101'), pch = 15, col = colFun(seq(0,60,15)), inset = -0.3, xpd = TRUE)
dev.off()

#  TN 95th %-ile - hillslope params top 10%----
png('HillRankTop1295_TN95_h_Agg.png', res = 300, height = 7, width = 7, units = 'in')
par(mar = c(1,14,0,5))
for (h in 1:length(uhills)){
  #Loop over the top 10% ranks for basin
  for (j in 1:length(SortRanksMua_h_Agg)){
    if(j == 1){
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_h_Agg)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMuaTN95_h_Agg[[h]]$Param == SortRanksMua_h_Agg[j])), axes = FALSE, cex.lab = 1.5)
    }else{
      plot(x = h, y = j, xlim = c(0, 15), ylim = c(0,length(SortRanksMua_h_Agg)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaTN95_h_Agg[[h]]$Param == SortRanksMua_h_Agg[j])), axes = FALSE)
    }
    par(new=TRUE)
    RanksTN95_Agg = c(RanksTN95_Agg,which(RanksMuaTN95_h_Agg[[h]]$Param == SortRanksMua_h_Agg[j]))
  }
}
rm(h,j)
par(new=FALSE)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_h_Agg),1), labels = SortRanksMua_h_Agg, las = 1)
#legend('top', title = expression(bold('Parameter Sensitivity Rank')), legend = c('1 - 14', '15 - 29', '30 - 44', '45 - 60', '61 - 101'), horiz = TRUE, pch = 15, col = colFun(seq(0,60,15)), inset = -0.1, xpd = TRUE)
legend('right', title = expression(bold('Rank')), legend = c('1 - 14', '15 - 29', '30 - 44', '45 - 60', '61 - 101'), pch = 15, col = colFun(seq(0,60,15)), inset = -0.3, xpd = TRUE)
dev.off()

#Fixme: Show SA metrics for the basin and for each hillslope - ranks, and maps for hillslope----

#Compare the parameters affected by multipliers after aggregating constrained parameters to see if they are different in sensitivity----
#Using m as an example for now because it's in the top 12
#Streamflow 5%----
png('BasinKsatDecayEEs.png', res = 300, width = 3, height = 5, units = 'in')
barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs05_b_mua_m), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
        names.arg = c('S9', 'S109', 'S8', 'S108', 'S8 and S108', 'S9 and S109'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'm: Decay of Ksat with increasing Sat. Def.', xlab = 'Soils')
arrows(seq(0.5,6.5,1), 
       EEs05_b_mua_05[c(grep(names(EEs05_b_mua_05), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs05_b_mua_05), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
       seq(0.5,6.5,1), 
       EEs05_b_mua_95[c(grep(names(EEs05_b_mua_95), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs05_b_mua_95), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
       length=0.05, angle=90, code=3)
dev.off()

png('BasinKsatEEs.png', res = 300, width = 3, height = 5, units = 'in')
par(mar=c(6,4,3,0.5))
barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
        names.arg = c('S9 Ksat', 'S9 vKsat', 'S109 Ksat', 'S109 vKsat', 'S8 Ksat', 'S8 vKsat', 'S108 Ksat', 'S108 vKsat', 'S8 and S108', 'S9 and S109'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Hydraulic Conductivity', xlab = '', las = 2)
axis(side = 1, at = 5, labels = 'Soils', line = 4, cex.axis = 1.2, tick = FALSE)
arrows(seq(0.5,9.5,1), 
       EEs05_b_mua_05[grep(names(EEs05_b_mua_05), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
       seq(0.5,9.5,1), 
       EEs05_b_mua_95[grep(names(EEs05_b_mua_95), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
       length=0.05, angle=90, code=3)
dev.off()

png('BasinSatToGWEEs.png', res = 300, width = 3, height = 5, units = 'in')
barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Sat. to GW Coef.', xlab = 'Soils')
arrows(seq(0.5,6.5,1), 
       EEs05_b_mua_05[c(grep(names(EEs05_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
       seq(0.5,6.5,1), 
       EEs05_b_mua_95[c(grep(names(EEs05_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
       length=0.05, angle=90, code=3)
dev.off()

png('BasinAirEntryPresEEs.png', res = 300, width = 3, height = 5, units = 'in')
barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Air Entry Pressure', xlab = 'Soils')
arrows(seq(0.5,6.5,1), 
       EEs05_b_mua_05[c(grep(names(EEs05_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
       seq(0.5,6.5,1), 
       EEs05_b_mua_95[c(grep(names(EEs05_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
       length=0.05, angle=90, code=3)
dev.off()

png('BasinPoreSizeEEs.png', res = 300, width = 3, height = 5, units = 'in')
barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Pore Size', xlab = 'Soils')
arrows(seq(0.5,6.5,1), 
       EEs05_b_mua_05[c(grep(names(EEs05_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
       seq(0.5,6.5,1), 
       EEs05_b_mua_95[c(grep(names(EEs05_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
       length=0.05, angle=90, code=3)
dev.off()

png('BasinSoilDepthEEs.png', res = 300, width = 3, height = 5, units = 'in')
barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = 'active', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Soil Thickness', xlab = 'Soils', las = 1)
arrows(seq(0.5,4.5,1), 
       EEs05_b_mua_05[grep(names(EEs05_b_mua_05), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
       seq(0.5,4.5,1), 
       EEs05_b_mua_95[grep(names(EEs05_b_mua_95), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
       length=0.05, angle=90, code=3)
dev.off()

png('BasinSnowMeltEEs.png', res = 300, width = 3, height = 5, units = 'in')
barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.001), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Snow Melt Temp. Coef.', xlab = 'Soils')
arrows(seq(0.5,4.5,1), 
       EEs05_b_mua_05[grep(names(EEs05_b_mua_05), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
       seq(0.5,4.5,1), 
       EEs05_b_mua_95[grep(names(EEs05_b_mua_95), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
       length=0.05, angle=90, code=3)
dev.off()

png('BasinSnowEnergyEEs.png', res = 300, width = 3, height = 5, units = 'in')
barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.001), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Max Snow Energy Deficit', xlab = 'Soils')
arrows(seq(0.5,4.5,1), 
       EEs05_b_mua_05[grep(names(EEs05_b_mua_05), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
       seq(0.5,4.5,1), 
       EEs05_b_mua_95[grep(names(EEs05_b_mua_95), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
       length=0.05, angle=90, code=3)
dev.off()

png('BasinVegSLAEEs.png', res = 300, width = 3, height = 5, units = 'in')
barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = 'sla', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
        names.arg = c('Tree', 'Grass'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Specific Leaf Area', xlab = 'Vegetation')
arrows(seq(0.5,4.5,1), 
       EEs05_b_mua_05[grep(names(EEs05_b_mua_05), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
       seq(0.5,4.5,1), 
       EEs05_b_mua_95[grep(names(EEs05_b_mua_95), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_95) %in% ColsAggregated)]), 
       length=0.05, angle=90, code=3)
dev.off()

#  panel----
png('BasinAllMultEEs_05s.png', res = 300, width = 9, height = 9, units = 'in')
opar=par
layout(rbind(c(1,2,3), c(4,5,6), c(7,8,9)))
barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs05_b_mua_m), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs05_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108', 'S8 and S108', 'S9 and S109'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'm: Ksat Decay with Sat. Def.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,5.5,1), 
       EEs05_b_mua_05[c(grep(names(EEs05_b_mua_05), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs05_b_mua_05), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs05_b_mua_95), 
       seq(0.5,5.5,1), 
       EEs05_b_mua_95[c(grep(names(EEs05_b_mua_95), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs05_b_mua_95), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs05_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Sat. to GW Coef.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs05_b_mua_05[c(grep(names(EEs05_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs05_b_mua_95[c(grep(names(EEs05_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
       length=0.05, angle=90, code=3)

par(mar=c(6,4,3,0.5))
barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
        names.arg = c('S9 Ksat', 'S9 vKsat', 'S109 Ksat', 'S109 vKsat', 'S8 Ksat', 'S8 vKsat', 'S108 Ksat', 'S108 vKsat', 'S8 and S108', 'S9 and S109'), 
        ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Hydraulic Conductivity', xlab = '', las = 2, cex.main=1.5)
axis(side = 1, at = 5, labels = 'Soils', line = 4, cex.axis = 1.2, tick = FALSE)
arrows(seq(0.5,9.5,1), 
       EEs05_b_mua_05[grep(names(EEs05_b_mua_05), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95), 
       seq(0.5,9.5,1), 
       EEs05_b_mua_95[grep(names(EEs05_b_mua_95), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95), 
       length=0.05, angle=90, code=3)

par(opar)
barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Air Entry Pressure', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs05_b_mua_05[c(grep(names(EEs05_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs05_b_mua_95[c(grep(names(EEs05_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Pore Size', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs05_b_mua_05[c(grep(names(EEs05_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs05_b_mua_95[c(grep(names(EEs05_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = 'active', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Soil Thickness', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs05_b_mua_05[grep(names(EEs05_b_mua_05), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs05_b_mua_95[grep(names(EEs05_b_mua_95), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.001), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Snow Melt Temp. Coef.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs05_b_mua_05[grep(names(EEs05_b_mua_05), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs05_b_mua_95[grep(names(EEs05_b_mua_95), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.001), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Max Snow Energy Deficit', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs05_b_mua_05[grep(names(EEs05_b_mua_05), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs05_b_mua_95[grep(names(EEs05_b_mua_95), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = 'sla', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
        names.arg = c('Tree', 'Grass'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Specific Leaf Area', xlab = 'Vegetation', cex.main=1.5)
arrows(seq(0.5,1.5,1), 
       EEs05_b_mua_05[grep(names(EEs05_b_mua_05), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95), 
       seq(0.5,1.5,1), 
       EEs05_b_mua_95[grep(names(EEs05_b_mua_95), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95), 
       length=0.05, angle=90, code=3)
dev.off()

#Streamflow 5-95----
#  panel----
png('BasinAllMultEEs_ots.png', res = 300, width = 9, height = 9, units = 'in')
opar=par
layout(rbind(c(1,2,3), c(4,5,6), c(7,8,9)))
barplot(height = EEsot_b_mua_m[c(grep(names(EEsot_b_mua_m), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEsot_b_mua_m), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEsot_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108', 'S8 and S108', 'S9 and S109'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'm: Ksat Decay with Sat. Def.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,5.5,1), 
       EEsot_b_mua_05[c(grep(names(EEsot_b_mua_05), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEsot_b_mua_05), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEsot_b_mua_95), 
       seq(0.5,5.5,1), 
       EEsot_b_mua_95[c(grep(names(EEsot_b_mua_95), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEsot_b_mua_95), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEsot_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsot_b_mua_m[c(grep(names(EEsot_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Sat. to GW Coef.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsot_b_mua_05[c(grep(names(EEsot_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsot_b_mua_95[c(grep(names(EEsot_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_b_mua_95), 
       length=0.05, angle=90, code=3)

par(mar=c(6,4,3,0.5))
barplot(height = EEsot_b_mua_m[c(grep(names(EEsot_b_mua_m), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_b_mua_95), 
        names.arg = c('S9 Ksat', 'S9 vKsat', 'S109 Ksat', 'S109 vKsat', 'S8 Ksat', 'S8 vKsat', 'S108 Ksat', 'S108 vKsat', 'S8 and S108', 'S9 and S109'), 
        ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Hydraulic Conductivity', xlab = '', las = 2, cex.main=1.5)
axis(side = 1, at = 5, labels = 'Soils', line = 4, cex.axis = 1.2, tick = FALSE)
arrows(seq(0.5,9.5,1), 
       EEsot_b_mua_05[grep(names(EEsot_b_mua_05), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_b_mua_95), 
       seq(0.5,9.5,1), 
       EEsot_b_mua_95[grep(names(EEsot_b_mua_95), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_b_mua_95), 
       length=0.05, angle=90, code=3)

par(opar)
barplot(height = EEsot_b_mua_m[c(grep(names(EEsot_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Air Entry Pressure', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsot_b_mua_05[c(grep(names(EEsot_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsot_b_mua_95[c(grep(names(EEsot_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsot_b_mua_m[c(grep(names(EEsot_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Pore Size', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsot_b_mua_05[grep(names(EEsot_b_mua_05), pattern = 'pore', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsot_b_mua_95[grep(names(EEsot_b_mua_95), pattern = 'pore', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsot_b_mua_m[c(grep(names(EEsot_b_mua_m), pattern = 'active', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Soil Thickness', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsot_b_mua_05[grep(names(EEsot_b_mua_05), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsot_b_mua_95[grep(names(EEsot_b_mua_95), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsot_b_mua_m[c(grep(names(EEsot_b_mua_m), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.001), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Snow Melt Temp. Coef.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsot_b_mua_05[grep(names(EEsot_b_mua_05), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsot_b_mua_95[grep(names(EEsot_b_mua_95), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsot_b_mua_m[c(grep(names(EEsot_b_mua_m), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.001), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Max Snow Energy Deficit', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsot_b_mua_05[grep(names(EEsot_b_mua_05), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsot_b_mua_95[grep(names(EEsot_b_mua_95), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsot_b_mua_m[c(grep(names(EEsot_b_mua_m), pattern = 'sla', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_b_mua_95), 
        names.arg = c('Tree', 'Grass'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Specific Leaf Area', xlab = 'Vegetation', cex.main=1.5)
arrows(seq(0.5,1.5,1), 
       EEsot_b_mua_05[grep(names(EEsot_b_mua_05), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_b_mua_95), 
       seq(0.5,1.5,1), 
       EEsot_b_mua_95[grep(names(EEsot_b_mua_95), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_b_mua_95), 
       length=0.05, angle=90, code=3)
dev.off()

#Streamflow 95%----
#  panel----
png('BasinAllMultEEs_95s.png', res = 300, width = 9, height = 9, units = 'in')
opar=par
layout(rbind(c(1,2,3), c(4,5,6), c(7,8,9)))
barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs95_b_mua_m), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108', 'S8 and S108', 'S9 and S109'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'm: Ksat Decay with Sat. Def.', xlab = 'Soils', las=2, cex.main=1.5)
arrows(seq(0.5,5.5,1), 
       EEs95_b_mua_05[c(grep(names(EEs95_b_mua_05), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs95_b_mua_05), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs95_b_mua_95), 
       seq(0.5,5.5,1), 
       EEs95_b_mua_95[c(grep(names(EEs95_b_mua_95), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs95_b_mua_95), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Sat. to GW Coef.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs95_b_mua_05[c(grep(names(EEs95_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs95_b_mua_95[c(grep(names(EEs95_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)

par(mar=c(6,4,3,0.5))
barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
        names.arg = c('S9 Ksat', 'S9 vKsat', 'S109 Ksat', 'S109 vKsat', 'S8 Ksat', 'S8 vKsat', 'S108 Ksat', 'S108 vKsat', 'S8 and S108', 'S9 and S109'), 
        ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Hydraulic Conductivity', xlab = '', las = 2, cex.main=1.5)
axis(side = 1, at = 5, labels = 'Soils', line = 4, cex.axis = 1.2, tick = FALSE)
arrows(seq(0.5,9.5,1), 
       EEs95_b_mua_05[grep(names(EEs95_b_mua_05), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       seq(0.5,9.5,1), 
       EEs95_b_mua_95[grep(names(EEs95_b_mua_95), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)

par(opar)
barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Air Entry Pressure', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs95_b_mua_05[c(grep(names(EEs95_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs95_b_mua_95[c(grep(names(EEs95_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Pore Size', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs95_b_mua_05[grep(names(EEs95_b_mua_05), pattern = 'pore', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs95_b_mua_95[grep(names(EEs95_b_mua_95), pattern = 'pore', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = 'active', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Soil Thickness', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs95_b_mua_05[grep(names(EEs95_b_mua_05), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs95_b_mua_95[grep(names(EEs95_b_mua_95), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.005), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Snow Melt Temp. Coef.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs95_b_mua_05[grep(names(EEs95_b_mua_05), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs95_b_mua_95[grep(names(EEs95_b_mua_95), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.005), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Max Snow Energy Deficit', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs95_b_mua_05[grep(names(EEs95_b_mua_05), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs95_b_mua_95[grep(names(EEs95_b_mua_95), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = 'sla', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
        names.arg = c('Tree', 'Grass'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Specific Leaf Area', xlab = 'Vegetation', cex.main=1.5)
arrows(seq(0.5,1.5,1), 
       EEs95_b_mua_05[grep(names(EEs95_b_mua_05), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       seq(0.5,1.5,1), 
       EEs95_b_mua_95[grep(names(EEs95_b_mua_95), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)
dev.off()

#Compute the correlation of parameters that have non-zero EEs----
#Squared norm (L2) of a vector
norm_vec <- function(x){
  sqrt(sum(x^2))
}
#Matrix for the similarity measure
#Determine number of columns based on number of non-zero EEs
CorCols = length(which(RanksMua05_b$EE05_b != 0))
CosPhi05_b = CosPhi95_b = CosPhiot_b = matrix(NA, nrow = CorCols, ncol = CorCols)
for (i in 1:CorCols){
  for (j in 1:CorCols){
    CosPhi05_b[i,j] = abs(t(EEs05_b[,which((colnames(EEs05_b) %in% ParamRanges$NumberedParams))][,which(muaEEs05_b != 0)][,i]) %*% EEs05_b[,which((colnames(EEs05_b) %in% ParamRanges$NumberedParams))][,which(muaEEs05_b != 0)][,j])/norm_vec(EEs05_b[,which((colnames(EEs05_b) %in% ParamRanges$NumberedParams))][,which(muaEEs05_b != 0)][,i])/norm_vec(EEs05_b[,which((colnames(EEs05_b) %in% ParamRanges$NumberedParams))][,which(muaEEs05_b != 0)][,j])
    CosPhi95_b[i,j] = abs(t(EEs95_b[,which((colnames(EEs95_b) %in% ParamRanges$NumberedParams))][,which(muaEEs95_b != 0)][,i]) %*% EEs95_b[,which((colnames(EEs95_b) %in% ParamRanges$NumberedParams))][,which(muaEEs95_b != 0)][,j])/norm_vec(EEs95_b[,which((colnames(EEs95_b) %in% ParamRanges$NumberedParams))][,which(muaEEs95_b != 0)][,i])/norm_vec(EEs95_b[,which((colnames(EEs95_b) %in% ParamRanges$NumberedParams))][,which(muaEEs95_b != 0)][,j])
    CosPhiot_b[i,j] = abs(t(EEsot_b[,which((colnames(EEsot_b) %in% ParamRanges$NumberedParams))][,which(muaEEsot_b != 0)][,i]) %*% EEsot_b[,which((colnames(EEsot_b) %in% ParamRanges$NumberedParams))][,which(muaEEsot_b != 0)][,j])/norm_vec(EEsot_b[,which((colnames(EEsot_b) %in% ParamRanges$NumberedParams))][,which(muaEEsot_b != 0)][,i])/norm_vec(EEsot_b[,which((colnames(EEsot_b) %in% ParamRanges$NumberedParams))][,which(muaEEsot_b != 0)][,j])
  }
}
rm(i,j)

colnames(CosPhi05_b) = colnames(OrigParams[which(muaEEs05_b != 0)])
colnames(CosPhi95_b) = colnames(OrigParams[which(muaEEs95_b != 0)])
colnames(CosPhiot_b) = colnames(OrigParams[which(muaEEsot_b != 0)])
rownames(CosPhi05_b) = colnames(OrigParams[which(muaEEs05_b != 0)])
rownames(CosPhi95_b) = colnames(OrigParams[which(muaEEs95_b != 0)])
rownames(CosPhiot_b) = colnames(OrigParams[which(muaEEsot_b != 0)])

#Heatmaps----
png('ParamCorrelations_EE05.png', res = 300, height = 8, width = 8, units = 'in')
heatmap(x = CosPhi05_b, na.rm = TRUE, symm = TRUE, revC = TRUE)
dev.off()

png('ParamCorrelations_EE95.png', res = 300, height = 8, width = 8, units = 'in')
heatmap(x = CosPhi95_b, na.rm = TRUE, symm = TRUE, revC = TRUE)
dev.off()

png('ParamCorrelations_EEot.png', res = 300, height = 8, width = 8, units = 'in')
heatmap(x = CosPhiot_b, na.rm = TRUE, symm = TRUE, revC = TRUE)
dev.off()

# Aggregated----
CorCols_Agg = length(which(RanksMua05_b_Agg$EE05_b != 0))
CosPhi05_b_Agg = CosPhi95_b_Agg = CosPhiot_b_Agg = matrix(NA, nrow = CorCols_Agg, ncol = CorCols_Agg)
#Save the length of the norm for use in selecting total number of parameters
NormLen05_b_Agg = NormLen95_b_Agg = NormLenot_b_Agg = vector('numeric', length = CorCols_Agg)

for (i in 1:CorCols_Agg){
  for (j in 1:CorCols_Agg){
    CosPhi05_b_Agg[i,j] = abs(t(EEs05_b[,which((!(colnames(EEs05_b) %in% ColsAggregated)) & (EEs05_b_mua_m != 0))][,i]) %*% EEs05_b[,which((!(colnames(EEs05_b) %in% ColsAggregated)) & (EEs05_b_mua_m != 0))][,j])/norm_vec(EEs05_b[,which((!(colnames(EEs05_b) %in% ColsAggregated)) & (EEs05_b_mua_m != 0))][,i])/norm_vec(EEs05_b[,which((!(colnames(EEs05_b) %in% ColsAggregated)) & (EEs05_b_mua_m != 0))][,j])
    CosPhi95_b_Agg[i,j] = abs(t(EEs95_b[,which((!(colnames(EEs95_b) %in% ColsAggregated)) & (EEs95_b_mua_m != 0))][,i]) %*% EEs95_b[,which((!(colnames(EEs95_b) %in% ColsAggregated)) & (EEs95_b_mua_m != 0))][,j])/norm_vec(EEs95_b[,which((!(colnames(EEs95_b) %in% ColsAggregated)) & (EEs95_b_mua_m != 0))][,i])/norm_vec(EEs95_b[,which((!(colnames(EEs95_b) %in% ColsAggregated)) & (EEs95_b_mua_m != 0))][,j])
    CosPhiot_b_Agg[i,j] = abs(t(EEsot_b[,which((!(colnames(EEsot_b) %in% ColsAggregated)) & (EEsot_b_mua_m != 0))][,i]) %*% EEsot_b[,which((!(colnames(EEsot_b) %in% ColsAggregated)) & (EEsot_b_mua_m != 0))][,j])/norm_vec(EEsot_b[,which((!(colnames(EEsot_b) %in% ColsAggregated)) & (EEsot_b_mua_m != 0))][,i])/norm_vec(EEsot_b[,which((!(colnames(EEsot_b) %in% ColsAggregated)) & (EEsot_b_mua_m != 0))][,j])
  }
  NormLen05_b_Agg[i] = norm_vec(EEs05_b[,which((!(colnames(EEs05_b) %in% ColsAggregated)) & (EEs05_b_mua_m != 0))][,i])
  NormLen95_b_Agg[i] = norm_vec(EEs95_b[,which((!(colnames(EEs95_b) %in% ColsAggregated)) & (EEs95_b_mua_m != 0))][,i])
  NormLenot_b_Agg[i] = norm_vec(EEsot_b[,which((!(colnames(EEsot_b) %in% ColsAggregated)) & (EEsot_b_mua_m != 0))][,i])
}
rm(i,j)

colnames(CosPhi05_b_Agg) = colnames(EEs05_b[,which((!(colnames(EEs05_b) %in% ColsAggregated)) & (EEs05_b_mua_m != 0))])
colnames(CosPhi95_b_Agg) = colnames(EEs95_b[,which((!(colnames(EEs95_b) %in% ColsAggregated)) & (EEs95_b_mua_m != 0))])
colnames(CosPhiot_b_Agg) = colnames(EEsot_b[,which((!(colnames(EEsot_b) %in% ColsAggregated)) & (EEsot_b_mua_m != 0))])
rownames(CosPhi05_b_Agg) = colnames(EEs05_b[,which((!(colnames(EEs05_b) %in% ColsAggregated)) & (EEs05_b_mua_m != 0))])
rownames(CosPhi95_b_Agg) = colnames(EEs95_b[,which((!(colnames(EEs95_b) %in% ColsAggregated)) & (EEs95_b_mua_m != 0))])
rownames(CosPhiot_b_Agg) = colnames(EEsot_b[,which((!(colnames(EEsot_b) %in% ColsAggregated)) & (EEsot_b_mua_m != 0))])

#Get the parameters whose lengths are within 2.5% percentage of the maximum vector length. These will be evaluated for clustering
ParamsCluster = sort(unique(c(colnames(EEs05_b[,which((!(colnames(EEs05_b) %in% ColsAggregated)) & (EEs05_b_mua_m != 0))][,which(NormLen05_b_Agg >= max(NormLen05_b_Agg)*.025)]),
              colnames(EEs95_b[,which((!(colnames(EEs95_b) %in% ColsAggregated)) & (EEs95_b_mua_m != 0))][,which(NormLen95_b_Agg >= max(NormLen95_b_Agg)*.025)]),
              colnames(EEsot_b[,which((!(colnames(EEsot_b) %in% ColsAggregated)) & (EEsot_b_mua_m != 0))][,which(NormLenot_b_Agg >= max(NormLenot_b_Agg)*.025)]))))

png('ParamCorrelations_EE05_Agg.png', res = 300, height = 8, width = 8, units = 'in')
heatmap(x = CosPhi05_b_Agg[which(rownames(CosPhi05_b_Agg) %in% ParamsCluster),which(colnames(CosPhi05_b_Agg) %in% ParamsCluster)], symm = TRUE, revC = TRUE)
dev.off()

png('ParamCorrelations_EE95_Agg.png', res = 300, height = 8, width = 8, units = 'in')
heatmap(x = CosPhi95_b_Agg[which(rownames(CosPhi95_b_Agg) %in% ParamsCluster),which(colnames(CosPhi95_b_Agg) %in% ParamsCluster)], symm = TRUE, revC = TRUE)
dev.off()

png('ParamCorrelations_EEot_Agg.png', res = 300, height = 8, width = 8, units = 'in')
heatmap(x = CosPhiot_b_Agg[which(rownames(CosPhiot_b_Agg) %in% ParamsCluster),which(colnames(CosPhiot_b_Agg) %in% ParamsCluster)], symm = TRUE, revC = TRUE)
dev.off()

#Fixme: Evaluate total number of selected parameters as a function of correlation----
#Fixme: Make a 3D plot of total parameters selected as function of threshold and correlation----

#Evaluate correlations for the selected parameters----
RankSelParams_Agg = sort(unique(c(ParamSelect_b_Agg, ParamSelectTN_b_Agg)))
RankSelParams_h_Agg = sort(unique(c(ParamSelect_b_Agg, ParamSelectTN_b_Agg, ParamSelect_h_Agg, ParamSelectTN_h_Agg)))
RankSelParams_h_Agg910 = sort(ParamSelect_h_Agg910)

# Loop over the variables and get the parameters correlated by a certain amount
CorParams = CorParams_h = NULL
corr = 0.5
for (i in 1:length(RankSelParams_Agg)){
  CorParams = unique(c(CorParams, 
                       names(CosPhi05_b_Agg[,which(colnames(CosPhi05_b_Agg) %in% RankSelParams_Agg)][,i][which(CosPhi05_b_Agg[,which(colnames(CosPhi05_b_Agg) %in% RankSelParams_Agg)][,i] > corr)][!(names(CosPhi05_b_Agg[,which(colnames(CosPhi05_b_Agg) %in% RankSelParams_Agg)][,i][which(CosPhi05_b_Agg[,which(colnames(CosPhi05_b_Agg) %in% RankSelParams_Agg)][,i] > corr)]) %in% RankSelParams_Agg)]),
                       names(CosPhi95_b_Agg[,which(colnames(CosPhi95_b_Agg) %in% RankSelParams_Agg)][,i][which(CosPhi95_b_Agg[,which(colnames(CosPhi95_b_Agg) %in% RankSelParams_Agg)][,i] > corr)][!(names(CosPhi95_b_Agg[,which(colnames(CosPhi95_b_Agg) %in% RankSelParams_Agg)][,i][which(CosPhi95_b_Agg[,which(colnames(CosPhi95_b_Agg) %in% RankSelParams_Agg)][,i] > corr)]) %in% RankSelParams_Agg)]),
                       names(CosPhiot_b_Agg[,which(colnames(CosPhiot_b_Agg) %in% RankSelParams_Agg)][,i][which(CosPhiot_b_Agg[,which(colnames(CosPhiot_b_Agg) %in% RankSelParams_Agg)][,i] > corr)][!(names(CosPhiot_b_Agg[,which(colnames(CosPhiot_b_Agg) %in% RankSelParams_Agg)][,i][which(CosPhiot_b_Agg[,which(colnames(CosPhiot_b_Agg) %in% RankSelParams_Agg)][,i] > corr)]) %in% RankSelParams_Agg)])))
}
rm(i)
RankCorSelParams_Agg = unique(c(CorParams, RankSelParams_Agg))

for (i in 1:length(RankSelParams_h_Agg)){
  CorParams_h = unique(c(CorParams_h, 
                       names(CosPhi05_b_Agg[,which(colnames(CosPhi05_b_Agg) %in% RankSelParams_h_Agg)][,i][which(CosPhi05_b_Agg[,which(colnames(CosPhi05_b_Agg) %in% RankSelParams_h_Agg)][,i] > corr)][!(names(CosPhi05_b_Agg[,which(colnames(CosPhi05_b_Agg) %in% RankSelParams_h_Agg)][,i][which(CosPhi05_b_Agg[,which(colnames(CosPhi05_b_Agg) %in% RankSelParams_h_Agg)][,i] > corr)]) %in% RankSelParams_h_Agg)]),
                       names(CosPhi95_b_Agg[,which(colnames(CosPhi95_b_Agg) %in% RankSelParams_h_Agg)][,i][which(CosPhi95_b_Agg[,which(colnames(CosPhi95_b_Agg) %in% RankSelParams_h_Agg)][,i] > corr)][!(names(CosPhi95_b_Agg[,which(colnames(CosPhi95_b_Agg) %in% RankSelParams_h_Agg)][,i][which(CosPhi95_b_Agg[,which(colnames(CosPhi95_b_Agg) %in% RankSelParams_h_Agg)][,i] > corr)]) %in% RankSelParams_h_Agg)]),
                       names(CosPhiot_b_Agg[,which(colnames(CosPhiot_b_Agg) %in% RankSelParams_h_Agg)][,i][which(CosPhiot_b_Agg[,which(colnames(CosPhiot_b_Agg) %in% RankSelParams_h_Agg)][,i] > corr)][!(names(CosPhiot_b_Agg[,which(colnames(CosPhiot_b_Agg) %in% RankSelParams_h_Agg)][,i][which(CosPhiot_b_Agg[,which(colnames(CosPhiot_b_Agg) %in% RankSelParams_h_Agg)][,i] > corr)]) %in% RankSelParams_h_Agg)])))
}
rm(i)
RankCorSelParams_h_Agg = unique(c(CorParams_h, RankSelParams_h_Agg))

# Make plot of the number of variables as a function of the correlation----
xcor = seq(0.5,1,0.01)
NumParamsCor = vector('numeric', length(xcor))
EEsum = vector('numeric', length(xcor))
for (ci in 1:length(xcor)){
  CorParams_plt = NULL
  for (i in 1:length(RankSelParams_Agg)){
    CorParams_plt = unique(c(CorParams_plt, 
                         names(CosPhi05_b_Agg[,which(colnames(CosPhi05_b_Agg) %in% RankSelParams_Agg)][,i][which(CosPhi05_b_Agg[,which(colnames(CosPhi05_b_Agg) %in% RankSelParams_Agg)][,i] > xcor[ci])][!(names(CosPhi05_b_Agg[,which(colnames(CosPhi05_b_Agg) %in% RankSelParams_Agg)][,i][which(CosPhi05_b_Agg[,which(colnames(CosPhi05_b_Agg) %in% RankSelParams_Agg)][,i] > xcor[ci])]) %in% RankSelParams_Agg)]),
                         names(CosPhi95_b_Agg[,which(colnames(CosPhi95_b_Agg) %in% RankSelParams_Agg)][,i][which(CosPhi95_b_Agg[,which(colnames(CosPhi95_b_Agg) %in% RankSelParams_Agg)][,i] > xcor[ci])][!(names(CosPhi95_b_Agg[,which(colnames(CosPhi95_b_Agg) %in% RankSelParams_Agg)][,i][which(CosPhi95_b_Agg[,which(colnames(CosPhi95_b_Agg) %in% RankSelParams_Agg)][,i] > xcor[ci])]) %in% RankSelParams_Agg)]),
                         names(CosPhiot_b_Agg[,which(colnames(CosPhiot_b_Agg) %in% RankSelParams_Agg)][,i][which(CosPhiot_b_Agg[,which(colnames(CosPhiot_b_Agg) %in% RankSelParams_Agg)][,i] > xcor[ci])][!(names(CosPhiot_b_Agg[,which(colnames(CosPhiot_b_Agg) %in% RankSelParams_Agg)][,i][which(CosPhiot_b_Agg[,which(colnames(CosPhiot_b_Agg) %in% RankSelParams_Agg)][,i] > xcor[ci])]) %in% RankSelParams_Agg)])))
  }
  NumParamsCor[ci] = length(unique(c(CorParams_plt, RankSelParams_Agg)))
  EEsum[ci] = sum(EEs95_b_mua_m[names(EEs95_b_mua_m) %in% unique(c(CorParams_plt, RankSelParams_Agg))])
}
rm(ci, CorParams_plt, i)

png('TotalParamsCorr_Agg.png', res = 300, units = 'in', width = 5, height = 5)
plot(x = xcor, y = NumParamsCor, type = 'l', xlab = 'Similarity Index Cutoff', ylab = 'Number of Parameters Selected', ylim = c(0,100), xlim = c(0.5,1), main = 'Threshold = 10%')
dev.off()

png('TotalParamsCorr_CumulativeEE_Agg.png', res = 300, units = 'in', width = 5, height = 5)
plot(x = xcor, y = EEsum, type = 'l', xlab = 'Similarity Index Cutoff', ylab = 'Sum of Normalized Elementary Effects', ylim = c(0,1000), xlim = c(0.5,1), main = 'Threshold = 10%')
dev.off()

#Perform heirarchical clustering for discovering variable clusters----
#Inspired by blog post: https://uc-r.github.io/hc_clustering
# methods to assess
ClustMethods <- c( "average", "single", "complete", "ward")
names(ClustMethods) <- c( "average", "single", "complete", "ward")

# All variables----
df = (EEs05_b[,which((!(colnames(EEs05_b) %in% ColsAggregated)) & (EEs05_b_mua_m != 0))])
#Seems to be very odd clusters when they are scaled, but good clusters when they are not scaled
# for (i in 1:ncol(df)){
#   #Get column mean and sd to normalize
#   df[,i] = (df[,i] - colMeans(x = df)[i])/sd(df[,i])
# }
# rm(i)

# function to compute coefficient
ac <- function(x) {
  agnes(t(df), method = x)$ac
}

map_dbl(ClustMethods, ac)

pltree(agnes(t(df), method = 'ward'), cex = 0.6, hang = -1, main = "Dendrogram of agnes", )
pltree(agnes(t(df), method = 'complete'), cex = 0.6, hang = -1, main = "Dendrogram of agnes", )

# Only the ParamsCluster----
#  Streamflow 5th %-ile----
df = (EEs05_b[,which((colnames(EEs05_b) %in% ParamsCluster))])
for (i in 1:ncol(df)){
  #Get column mean and sd to normalize
  df[,i] = (df[,i] - colMeans(x = df)[i])/sd(df[,i])
}
rm(i)

# function to compute coefficient
ac <- function(x) {
  agnes(t(df), method = x)$ac
}

map_dbl(ClustMethods, ac)

d05 = agnes(t(df), method = 'ward')
for (k in seq(15,35,5)){
  png(paste0('d05_',k,'.png'), res = 300, units = 'in', width = 7, height = 5)
  #pltree(agnes(t(df), method = 'ward'), cex = 0.6, hang = -1, main = "Dendrogram of agnes")
  plot(as.hclust(agnes(t(df), method = 'ward')), cex = 0.6, hang = -1)
  rect.hclust(as.hclust(agnes(t(df), method = 'ward')), k = k, border = rainbow(45))
  axis(side = 1, at = seq(.8,50.8,1), labels = d05$order.lab, tick = 0, line = -7.95, las = 2, cex.axis = 0.6)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEs05_b_mua_95[which(names(EEs05_b_mua_m) %in% d05$order.lab)][match(d05$order.lab, names(EEs05_b_mua_m)[which(names(EEs05_b_mua_m) %in% d05$order.lab)])]), 0), tick = 0, line = -1, cex.axis = 0.3, col.axis = 'blue')
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsot_b_mua_95[which(names(EEsot_b_mua_m) %in% d05$order.lab)][match(d05$order.lab, names(EEsot_b_mua_m)[which(names(EEsot_b_mua_m) %in% d05$order.lab)])]), 0), tick = 0, line = -0.8, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEs95_b_mua_95[which(names(EEs95_b_mua_m) %in% d05$order.lab)][match(d05$order.lab, names(EEs95_b_mua_m)[which(names(EEs95_b_mua_m) %in% d05$order.lab)])]), 0), tick = 0, line = -0.6, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTN05_b_mua_95[which(names(EEsTN05_b_mua_m) %in% d05$order.lab)][match(d05$order.lab, names(EEsTN05_b_mua_m)[which(names(EEsTN05_b_mua_m) %in% d05$order.lab)])]), 0), tick = 0, line = -0.4, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTNMed_b_mua_95[which(names(EEsTNMed_b_mua_m) %in% d05$order.lab)][match(d05$order.lab, names(EEsTNMed_b_mua_m)[which(names(EEsTNMed_b_mua_m) %in% d05$order.lab)])]), 0), tick = 0, line = -0.2, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTN95_b_mua_95[which(names(EEsTN95_b_mua_m) %in% d05$order.lab)][match(d05$order.lab, names(EEsTN95_b_mua_m)[which(names(EEsTN95_b_mua_m) %in% d05$order.lab)])]), 0), tick = 0, line = 0, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1)[d05$order.lab %in% RankSelParams_h_Agg], labels = d05$order.lab[d05$order.lab %in% RankSelParams_h_Agg], tick = 0, line = -7.95, las = 2, cex.axis = 0.6, col.axis = 'red')
  dev.off()
}

#Somewhere from 12-17 clusters seems appropriate
fviz_nbclust(x = df, FUNcluster = hcut, method = 'wss', k.max = 35)
fviz_nbclust(x = df, FUNcluster = hcut, method = 'silhouette', k.max = 35)
gap_stat = clusGap(df, FUNcluster = hcut, K.max = 35, B = 100, d.power = 2, )
fviz_gap_stat(gap_stat)

sort(cutree(as.hclust(agnes(t(df), method = 'ward')), k = 30))
fviz_cluster(list(data = t(df), cluster = cutree(as.hclust(agnes(t(df), method = 'ward')), k = 35)))

#Panel plot with PCAs
fviz_cluster(list(data = t(df), cluster = cutree(as.hclust(agnes(t(df), method = 'ward')), k = 15)), stand = TRUE, show.clust.cent = FALSE, ellipse = TRUE, shape = 16, geom = 'point', axes = c(1,2))

#  Streamflow 5th-95th %-ile----
df = (EEsot_b[,which((colnames(EEsot_b) %in% ParamsCluster))])
for (i in 1:ncol(df)){
  #Get column mean and sd to normalize
  df[,i] = (df[,i] - colMeans(x = df)[i])/sd(df[,i])
}
rm(i)

# function to compute coefficient
ac <- function(x) {
  agnes(t(df), method = x)$ac
}

map_dbl(ClustMethods, ac)

pltree(agnes(t(df), method = 'ward'), cex = 0.6, hang = -1, main = "Dendrogram of agnes", )

dot = agnes(t(df), method = 'ward')
for (k in seq(15,35,5)){
  png(paste0('dot_',k,'.png'), res = 300, units = 'in', width = 7, height = 5)
  #pltree(agnes(t(df), method = 'ward'), cex = 0.6, hang = -1, main = "Dendrogram of agnes")
  plot(as.hclust(agnes(t(df), method = 'ward')), cex = 0.6, hang = -1)
  rect.hclust(as.hclust(agnes(t(df), method = 'ward')), k = k, border = rainbow(45))
  axis(side = 1, at = seq(.8,50.8,1), labels = dot$order.lab, tick = 0, line = -7.95, las = 2, cex.axis = 0.6)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEs05_b_mua_95[which(names(EEs05_b_mua_m) %in% dot$order.lab)][match(dot$order.lab, names(EEs05_b_mua_m)[which(names(EEs05_b_mua_m) %in% dot$order.lab)])]), 0), tick = 0, line = -1, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsot_b_mua_95[which(names(EEsot_b_mua_m) %in% dot$order.lab)][match(dot$order.lab, names(EEsot_b_mua_m)[which(names(EEsot_b_mua_m) %in% dot$order.lab)])]), 0), tick = 0, line = -0.8, cex.axis = 0.3, col.axis = 'blue')
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEs95_b_mua_95[which(names(EEs95_b_mua_m) %in% dot$order.lab)][match(dot$order.lab, names(EEs95_b_mua_m)[which(names(EEs95_b_mua_m) %in% dot$order.lab)])]), 0), tick = 0, line = -0.6, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTN05_b_mua_95[which(names(EEsTN05_b_mua_m) %in% dot$order.lab)][match(dot$order.lab, names(EEsTN05_b_mua_m)[which(names(EEsTN05_b_mua_m) %in% dot$order.lab)])]), 0), tick = 0, line = -0.4, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTNMed_b_mua_95[which(names(EEsTNMed_b_mua_m) %in% dot$order.lab)][match(dot$order.lab, names(EEsTNMed_b_mua_m)[which(names(EEsTNMed_b_mua_m) %in% dot$order.lab)])]), 0), tick = 0, line = -0.2, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTN95_b_mua_95[which(names(EEsTN95_b_mua_m) %in% dot$order.lab)][match(dot$order.lab, names(EEsTN95_b_mua_m)[which(names(EEsTN95_b_mua_m) %in% dot$order.lab)])]), 0), tick = 0, line = 0, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1)[dot$order.lab %in% RankSelParams_h_Agg], labels = dot$order.lab[dot$order.lab %in% RankSelParams_h_Agg], tick = 0, line = -7.95, las = 2, cex.axis = 0.6, col.axis = 'red')
  dev.off()
}

#Somewhere from 12-17 clusters seems appropriate
fviz_nbclust(x = df, FUNcluster = hcut, method = 'wss', k.max = 35)
fviz_nbclust(x = df, FUNcluster = hcut, method = 'silhouette', k.max = 35)
gap_stat = clusGap(df, FUNcluster = hcut, K.max = 35, B = 100, d.power = 2, )
fviz_gap_stat(gap_stat)

sort(cutree(as.hclust(agnes(t(df), method = 'ward')), k = 15))
plot(as.hclust(agnes(t(df), method = 'ward')), cex = 0.6, hang = -1)
rect.hclust(as.hclust(agnes(t(df), method = 'ward')), k = 15, border = 2:16)
fviz_cluster(list(data = t(df), cluster = cutree(as.hclust(agnes(t(df), method = 'ward')), k = 15)))

#Panel plot with PCAs
fviz_cluster(list(data = t(df), cluster = cutree(as.hclust(agnes(t(df), method = 'ward')), k = 15)), stand = TRUE, show.clust.cent = FALSE, ellipse = TRUE, shape = 16, geom = 'point', axes = c(1,2))

#  Streamflow 95th %-ile----
df = (EEs95_b[,which((colnames(EEs95_b) %in% ParamsCluster))])
for (i in 1:ncol(df)){
 #Get column mean and sd to normalize
 df[,i] = (df[,i] - colMeans(x = df)[i])/sd(df[,i])
}
rm(i)

# function to compute coefficient
ac <- function(x) {
  agnes(t(df), method = x)$ac
}

map_dbl(ClustMethods, ac)

pltree(agnes(t(df), method = 'ward'), cex = 0.6, hang = -1, main = "Dendrogram of agnes", )

d95 = agnes(t(df), method = 'ward')
for (k in seq(15,35,5)){
  png(paste0('d95_',k,'.png'), res = 300, units = 'in', width = 7, height = 5)
  #pltree(agnes(t(df), method = 'ward'), cex = 0.6, hang = -1, main = "Dendrogram of agnes")
  plot(as.hclust(agnes(t(df), method = 'ward')), cex = 0.6, hang = -1)
  rect.hclust(as.hclust(agnes(t(df), method = 'ward')), k = k, border = rainbow(45))
  axis(side = 1, at = seq(.8,50.8,1), labels = d95$order.lab, tick = 0, line = -7.95, las = 2, cex.axis = 0.6)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEs05_b_mua_95[which(names(EEs05_b_mua_m) %in% d95$order.lab)][match(d95$order.lab, names(EEs05_b_mua_m)[which(names(EEs05_b_mua_m) %in% d95$order.lab)])]), 0), tick = 0, line = -1, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsot_b_mua_95[which(names(EEsot_b_mua_m) %in% d95$order.lab)][match(d95$order.lab, names(EEsot_b_mua_m)[which(names(EEsot_b_mua_m) %in% d95$order.lab)])]), 0), tick = 0, line = -0.8, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEs95_b_mua_95[which(names(EEs95_b_mua_m) %in% d95$order.lab)][match(d95$order.lab, names(EEs95_b_mua_m)[which(names(EEs95_b_mua_m) %in% d95$order.lab)])]), 0), tick = 0, line = -0.6, cex.axis = 0.3, col.axis = 'blue')
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTN05_b_mua_95[which(names(EEsTN05_b_mua_m) %in% d95$order.lab)][match(d95$order.lab, names(EEsTN05_b_mua_m)[which(names(EEsTN05_b_mua_m) %in% d95$order.lab)])]), 0), tick = 0, line = -0.4, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTNMed_b_mua_95[which(names(EEsTNMed_b_mua_m) %in% d95$order.lab)][match(d95$order.lab, names(EEsTNMed_b_mua_m)[which(names(EEsTNMed_b_mua_m) %in% d95$order.lab)])]), 0), tick = 0, line = -0.2, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTN95_b_mua_95[which(names(EEsTN95_b_mua_m) %in% d95$order.lab)][match(d95$order.lab, names(EEsTN95_b_mua_m)[which(names(EEsTN95_b_mua_m) %in% d95$order.lab)])]), 0), tick = 0, line = 0, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1)[d95$order.lab %in% RankSelParams_h_Agg], labels = d95$order.lab[d95$order.lab %in% RankSelParams_h_Agg], tick = 0, line = -7.95, las = 2, cex.axis = 0.6, col.axis = 'red')
  dev.off()
}

#Somewhere from 12-17 clusters seems appropriate
fviz_nbclust(x = df, FUNcluster = hcut, method = 'wss', k.max = 35)
fviz_nbclust(x = df, FUNcluster = hcut, method = 'silhouette', k.max = 35)
gap_stat = clusGap(df, FUNcluster = hcut, K.max = 35, B = 100, d.power = 2, )
fviz_gap_stat(gap_stat)

sort(cutree(as.hclust(agnes(t(df), method = 'ward')), k = 15))
plot(as.hclust(agnes(t(df), method = 'ward')), cex = 0.6, hang = -1)
rect.hclust(as.hclust(agnes(t(df), method = 'ward')), k = 15, border = 2:16)
fviz_cluster(list(data = t(df), cluster = cutree(as.hclust(agnes(t(df), method = 'ward')), k = 15)))

#Panel plot with PCAs
fviz_cluster(list(data = t(df), cluster = cutree(as.hclust(agnes(t(df), method = 'ward')), k = 15)), stand = TRUE, show.clust.cent = FALSE, ellipse = TRUE, shape = 16, geom = 'point', axes = c(1,2))


#  TN 5th %-ile----
df = (EEsTN05_b[,which((colnames(EEs05_b) %in% ParamsCluster))])
for (i in 1:ncol(df)){
  #Get column mean and sd to normalize
  df[,i] = (df[,i] - colMeans(x = df)[i])/sd(df[,i])
}
rm(i)

map_dbl(ClustMethods, ac)

dTN05 = agnes(t(df), method = 'ward')
for (k in seq(15,35,5)){
  png(paste0('dTN05_',k,'.png'), res = 300, units = 'in', width = 7, height = 5)
  plot(as.hclust(agnes(t(df), method = 'ward')), cex = 0.6, hang = -1)
  rect.hclust(as.hclust(agnes(t(df), method = 'ward')), k = k, border = rainbow(45))
  axis(side = 1, at = seq(.8,50.8,1), labels = dTN05$order.lab, tick = 0, line = -7.95, las = 2, cex.axis = 0.6)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEs05_b_mua_95[which(names(EEs05_b_mua_m) %in% dTN05$order.lab)][match(dTN05$order.lab, names(EEs05_b_mua_m)[which(names(EEs05_b_mua_m) %in% dTN05$order.lab)])]), 0), tick = 0, line = -1, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsot_b_mua_95[which(names(EEsot_b_mua_m) %in% dTN05$order.lab)][match(dTN05$order.lab, names(EEsot_b_mua_m)[which(names(EEsot_b_mua_m) %in% dTN05$order.lab)])]), 0), tick = 0, line = -0.8, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEs95_b_mua_95[which(names(EEs95_b_mua_m) %in% dTN05$order.lab)][match(dTN05$order.lab, names(EEs95_b_mua_m)[which(names(EEs95_b_mua_m) %in% dTN05$order.lab)])]), 0), tick = 0, line = -0.6, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTN05_b_mua_95[which(names(EEsTN05_b_mua_m) %in% dTN05$order.lab)][match(dTN05$order.lab, names(EEsTN05_b_mua_m)[which(names(EEsTN05_b_mua_m) %in% dTN05$order.lab)])]), 0), tick = 0, line = -0.4, cex.axis = 0.3, col.axis = 'blue')
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTNMed_b_mua_95[which(names(EEsTNMed_b_mua_m) %in% dTN05$order.lab)][match(dTN05$order.lab, names(EEsTNMed_b_mua_m)[which(names(EEsTNMed_b_mua_m) %in% dTN05$order.lab)])]), 0), tick = 0, line = -0.2, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTN95_b_mua_95[which(names(EEsTN95_b_mua_m) %in% dTN05$order.lab)][match(dTN05$order.lab, names(EEsTN95_b_mua_m)[which(names(EEsTN95_b_mua_m) %in% dTN05$order.lab)])]), 0), tick = 0, line = 0, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1)[dTN05$order.lab %in% RankSelParams_h_Agg], labels = dTN05$order.lab[dTN05$order.lab %in% RankSelParams_h_Agg], tick = 0, line = -7.95, las = 2, cex.axis = 0.6, col.axis = 'red')
  dev.off()
}

#  TN 50th %-ile----
df = (EEsTNMed_b[,which((colnames(EEs05_b) %in% ParamsCluster))])
for (i in 1:ncol(df)){
  #Get column mean and sd to normalize
  df[,i] = (df[,i] - colMeans(x = df)[i])/sd(df[,i])
}
rm(i)

map_dbl(ClustMethods, ac)

dTNMed = agnes(t(df), method = 'ward')
for (k in seq(15,35,5)){
  png(paste0('dTNMed_',k,'.png'), res = 300, units = 'in', width = 7, height = 5)
  plot(as.hclust(agnes(t(df), method = 'ward')), cex = 0.6, hang = -1)
  rect.hclust(as.hclust(agnes(t(df), method = 'ward')), k = k, border = rainbow(45))
  axis(side = 1, at = seq(.8,50.8,1), labels = dTNMed$order.lab, tick = 0, line = -7.95, las = 2, cex.axis = 0.6)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEs05_b_mua_95[which(names(EEs05_b_mua_m) %in% dTNMed$order.lab)][match(dTNMed$order.lab, names(EEs05_b_mua_m)[which(names(EEs05_b_mua_m) %in% dTNMed$order.lab)])]), 0), tick = 0, line = -1, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsot_b_mua_95[which(names(EEsot_b_mua_m) %in% dTNMed$order.lab)][match(dTNMed$order.lab, names(EEsot_b_mua_m)[which(names(EEsot_b_mua_m) %in% dTNMed$order.lab)])]), 0), tick = 0, line = -0.8, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEs95_b_mua_95[which(names(EEs95_b_mua_m) %in% dTNMed$order.lab)][match(dTNMed$order.lab, names(EEs95_b_mua_m)[which(names(EEs95_b_mua_m) %in% dTNMed$order.lab)])]), 0), tick = 0, line = -0.6, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTN05_b_mua_95[which(names(EEsTN05_b_mua_m) %in% dTNMed$order.lab)][match(dTNMed$order.lab, names(EEsTN05_b_mua_m)[which(names(EEsTN05_b_mua_m) %in% dTNMed$order.lab)])]), 0), tick = 0, line = -0.4, cex.axis = 0.3, col.axis = 'blue')
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTNMed_b_mua_95[which(names(EEsTNMed_b_mua_m) %in% dTNMed$order.lab)][match(dTNMed$order.lab, names(EEsTNMed_b_mua_m)[which(names(EEsTNMed_b_mua_m) %in% dTNMed$order.lab)])]), 0), tick = 0, line = -0.2, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTN95_b_mua_95[which(names(EEsTN95_b_mua_m) %in% dTNMed$order.lab)][match(dTNMed$order.lab, names(EEsTN95_b_mua_m)[which(names(EEsTN95_b_mua_m) %in% dTNMed$order.lab)])]), 0), tick = 0, line = 0, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1)[dTNMed$order.lab %in% RankSelParams_h_Agg], labels = dTNMed$order.lab[dTNMed$order.lab %in% RankSelParams_h_Agg], tick = 0, line = -7.95, las = 2, cex.axis = 0.6, col.axis = 'red')
  dev.off()
}

#  TN 5th %-ile----
df = (EEsTN95_b[,which((colnames(EEs05_b) %in% ParamsCluster))])
for (i in 1:ncol(df)){
  #Get column mean and sd to normalize
  df[,i] = (df[,i] - colMeans(x = df)[i])/sd(df[,i])
}
rm(i)

map_dbl(ClustMethods, ac)

dTN95 = agnes(t(df), method = 'ward')
for (k in seq(15,35,5)){
  png(paste0('dTN95_',k,'.png'), res = 300, units = 'in', width = 7, height = 5)
  plot(as.hclust(agnes(t(df), method = 'ward')), cex = 0.6, hang = -1)
  rect.hclust(as.hclust(agnes(t(df), method = 'ward')), k = k, border = rainbow(45))
  axis(side = 1, at = seq(.8,50.8,1), labels = dTN95$order.lab, tick = 0, line = -7.95, las = 2, cex.axis = 0.6)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEs05_b_mua_95[which(names(EEs05_b_mua_m) %in% dTN95$order.lab)][match(dTN95$order.lab, names(EEs05_b_mua_m)[which(names(EEs05_b_mua_m) %in% dTN95$order.lab)])]), 0), tick = 0, line = -1, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsot_b_mua_95[which(names(EEsot_b_mua_m) %in% dTN95$order.lab)][match(dTN95$order.lab, names(EEsot_b_mua_m)[which(names(EEsot_b_mua_m) %in% dTN95$order.lab)])]), 0), tick = 0, line = -0.8, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEs95_b_mua_95[which(names(EEs95_b_mua_m) %in% dTN95$order.lab)][match(dTN95$order.lab, names(EEs95_b_mua_m)[which(names(EEs95_b_mua_m) %in% dTN95$order.lab)])]), 0), tick = 0, line = -0.6, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTN05_b_mua_95[which(names(EEsTN05_b_mua_m) %in% dTN95$order.lab)][match(dTN95$order.lab, names(EEsTN05_b_mua_m)[which(names(EEsTN05_b_mua_m) %in% dTN95$order.lab)])]), 0), tick = 0, line = -0.4, cex.axis = 0.3, col.axis = 'blue')
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTNMed_b_mua_95[which(names(EEsTNMed_b_mua_m) %in% dTN95$order.lab)][match(dTN95$order.lab, names(EEsTNMed_b_mua_m)[which(names(EEsTNMed_b_mua_m) %in% dTN95$order.lab)])]), 0), tick = 0, line = -0.2, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTN95_b_mua_95[which(names(EEsTN95_b_mua_m) %in% dTN95$order.lab)][match(dTN95$order.lab, names(EEsTN95_b_mua_m)[which(names(EEsTN95_b_mua_m) %in% dTN95$order.lab)])]), 0), tick = 0, line = 0, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1)[dTN95$order.lab %in% RankSelParams_h_Agg], labels = dTN95$order.lab[dTN95$order.lab %in% RankSelParams_h_Agg], tick = 0, line = -7.95, las = 2, cex.axis = 0.6, col.axis = 'red')
  dev.off()
}

#Save parameters to be used for calibration in a new file----
#Dendrograms suggest clustering in different ways for different metrics. Will need all variables if we want to parse out the important variables for each metric.
#Select top 35 ParamRanges[ParamRanges$NumberedParams %in% RankSelParams_h_Agg,1:4]
#And also disaggregate variables to add to that list.
RankSelParams_h_Disagg = c(RankSelParams_h_Agg[-which(RankSelParams_h_Agg %in% ColsAggregated_key)], ColsAggregated[which(ColsAggregated_key %in% RankSelParams_h_Agg[which(RankSelParams_h_Agg %in% colnames(EEs05_b_mua[,272:291]))])])
RankSelParams_h_Disagg910 = c(RankSelParams_h_Agg910[-which(RankSelParams_h_Agg910 %in% ColsAggregated_key)], ColsAggregated[which(ColsAggregated_key %in% RankSelParams_h_Agg910[which(RankSelParams_h_Agg910 %in% colnames(EEs05_b_mua[,272:291]))])])
#Also remove the landuse % impervious. That will not be considered uncertain because it will be assigned from land use maps
RankSelParams_h_Disagg = RankSelParams_h_Disagg[-grep(RankSelParams_h_Disagg, pattern = '.percent_impervious', fixed = TRUE)]
RankSelParams_h_Disagg910 = RankSelParams_h_Disagg910[-grep(RankSelParams_h_Disagg910, pattern = '.percent_impervious', fixed = TRUE)]

ParamRanges_Cal = ParamRanges[ParamRanges$NumberedParams %in% RankSelParams_h_Disagg,1:4]
ParamRanges_Cal910 = ParamRanges[ParamRanges$NumberedParams %in% RankSelParams_h_Disagg910,1:4]
#Edit several lower bounds that changed since SA run
ParamRanges_Cal$Lower[ParamRanges_Cal$NumberedParams == 's9_Ksat_0_v'] = 0.2
ParamRanges_Cal$Lower[ParamRanges_Cal$NumberedParams == 's109_Ksat_0_v'] = 0.05
ParamRanges_Cal$Lower[ParamRanges_Cal$NumberedParams == 's8_Ksat_0_v'] = 0.2
ParamRanges_Cal$Lower[ParamRanges_Cal$NumberedParams == 's108_Ksat_0_v'] = 0.05
ParamRanges_Cal$Lower[ParamRanges_Cal$NumberedParams == 'l4_septic_water_load'] = 0

ParamRanges_Cal910$Lower[ParamRanges_Cal910$NumberedParams == 's9_Ksat_0_v'] = 0.2
ParamRanges_Cal910$Lower[ParamRanges_Cal910$NumberedParams == 's109_Ksat_0_v'] = 0.05
ParamRanges_Cal910$Lower[ParamRanges_Cal910$NumberedParams == 's8_Ksat_0_v'] = 0.2
ParamRanges_Cal910$Lower[ParamRanges_Cal910$NumberedParams == 's108_Ksat_0_v'] = 0.05
ParamRanges_Cal910$Lower[ParamRanges_Cal910$NumberedParams == 'l4_septic_water_load'] = 0
#And edit the GW parameter bounds as well based on discussion with Laurence
ParamRanges_Cal910$Upper[ParamRanges_Cal910$NumberedParams == 'h_gw_loss_coeff'] = 0.3
ParamRanges_Cal910$Upper[ParamRanges_Cal910$NumberedParams == 's9_sat_to_gw_coeff'] = 0.3
ParamRanges_Cal910$Upper[ParamRanges_Cal910$NumberedParams == 's109_sat_to_gw_coeff'] = 0.3


#Add all of the parameters of the likelihood function as well
# (kurotsis) beta=-1: uniform, beta=0: Gaussian, beta=1: double exponential
# (skewness) xi=1: symmetric, xi<1: negatively skewed, xi>1: positively skewed
# (standard deviation when mean=0)
# (linear rate of change in standard deviation with mean)
# (lag-1 auto-correlation), phi_1=0: no auto-correlation, phi_1=1: perfect auto-correlation
# (mean bias factor)
LikelihoodParams = cbind(c('PL_beta', 'PL_xi', 'PL_sigma_0', 'PL_sigma_1', 'PL_phi_1', 'PL_mu_h', 
                           'PL_beta_TN', 'PL_xi_TN', 'PL_sigma_0_TN', 'PL_sigma_1_TN', 'PL_phi_1_TN', 'PL_mu_h_TN'), 
                         c('beta', 'xi', 'sigma_0', 'sigma_1', 'phi_1', 'mu_h', 'beta', 'xi', 'sigma_0', 'sigma_1', 'phi_1', 'mu_h'), 
                         c(-1, 0, 0, 0, 0, 0), c(1, 10, 1, 1, 1, 100))
colnames(LikelihoodParams) = colnames(ParamRanges_Cal)
ParamRanges_Cal_Likes = rbind(ParamRanges_Cal, LikelihoodParams)

options(scipen = 999)
write.csv(ParamRanges_Cal, file = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs_Calibration\\BaismanCalibrationParameterProblemFile.csv", row.names = FALSE)
write.csv(ParamRanges_Cal_Likes, file = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs_Calibration\\BaismanCalibrationParameterProblemFile_LikelihoodParams.csv", row.names = FALSE)

write.csv(ParamRanges_Cal910, file = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs_Calibration/BaismanCalibrationParameterProblemFile_NewGWBounds.csv", row.names = FALSE)
options(scipen = 0)

#Pre-2020 Diagnosis of problems with EEs----
# for (i in 1:cols){
#   ind = i+(1+cols)*(t-1)
#   parm = which((OrigParams[ind+1,] - OrigParams[ind,]) != 0)
#   if(parm == 65){
#     print(i)
#     break
#   }
# }
# rm(i, ind, parm)
# 
# #For all of these, the input parameter did not change when it was supposed to (it did change in the original input file)
# #(3365 and 3366) for v102_epc.frootlitr_fcel is NaN because delta and change in values are 0
# #(4913 and 4914) for v102_epc.frootlitr_fcel is NaN
# #(5837 and 5838) for "s8_Ksat_0_v" is NaN
# #(644 and 645) for "s109_Ksat_0_v" is NaN
# #(5388 and 5389) for "s109_Ksat_0_v" is NaN
# 
# for (i in 1:r){
#   j = which(Deltas[r,] == min(abs(Deltas)))
#   if (length(j) > 0){
#     print(j)
#   }
# }
# rm(i,j)
# 
# #Resample values for the NaN replicates
# #644:
# set.seed(644)
# runif(n = 1, min = 0.1, max = 0.36767)
# #3365:
# set.seed(3365)
# runif(n = 1, min = 0.4, max = 0.5)
# #4913:
# set.seed(4913)
# runif(n = 1, min = 0.4, max = 0.5)
# #5388
# set.seed(5388)
# runif(n = 1, min = 0.1, max = .909091)
# #5837
# #set manually to 0.4 because both Ksat0 and Ksat0v needed adjustment from lower bound

#Determine the practical significance of the largest EEs----
#Get all of the indices for which a particular parameter changed.
which((OrigParams[2:nrow(OrigParams),"h_gw_loss_coeff"] - OrigParams[1:(nrow(OrigParams)-1),"h_gw_loss_coeff"]) != 0)[-which(which((OrigParams[2:nrow(OrigParams),"h_gw_loss_coeff"] - OrigParams[1:(nrow(OrigParams)-1),"h_gw_loss_coeff"]) != 0) %in% seq(272,10880,272))]

#Check that EEs match (they do)
#Basin
max(abs((apply(X = abs(apply(X = BasinSF05[which((OrigParams[2:nrow(OrigParams),"h_gw_loss_coeff"] - OrigParams[1:(nrow(OrigParams)-1),"h_gw_loss_coeff"]) != 0)[-which(which((OrigParams[2:nrow(OrigParams),"h_gw_loss_coeff"] - OrigParams[1:(nrow(OrigParams)-1),"h_gw_loss_coeff"]) != 0) %in% seq(272,10880,272))]+1,], MARGIN = 1, FUN = "-", obs05$Flow)), MARGIN = 2, FUN = sum) - apply(X = abs(apply(X = BasinSF05[which((OrigParams[2:nrow(OrigParams),"h_gw_loss_coeff"] - OrigParams[1:(nrow(OrigParams)-1),"h_gw_loss_coeff"]) != 0)[-which(which((OrigParams[2:nrow(OrigParams),"h_gw_loss_coeff"] - OrigParams[1:(nrow(OrigParams)-1),"h_gw_loss_coeff"]) != 0) %in% seq(272,10880,272))],], MARGIN = 1, FUN = "-", obs05$Flow)), MARGIN = 2, FUN = sum))/Deltas[,"h_gw_loss_coeff"] - EEs05_b$h_gw_loss_coeff))
#Hillslope
max(abs((apply(X = abs(apply(X = HillSF05[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams),"h_gw_loss_coeff"] - OrigParams[1:(nrow(OrigParams)-1),"h_gw_loss_coeff"]) != 0)[-which(which((OrigParams[2:nrow(OrigParams),"h_gw_loss_coeff"] - OrigParams[1:(nrow(OrigParams)-1),"h_gw_loss_coeff"]) != 0) %in% seq(272,10880,272))]+1,], MARGIN = 1, FUN = "-", as.numeric(MedHills05[which(MedHills05[,1] == 9),-1]))), MARGIN = 2, FUN = sum) - apply(X = abs(apply(X = HillSF05[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams),"h_gw_loss_coeff"] - OrigParams[1:(nrow(OrigParams)-1),"h_gw_loss_coeff"]) != 0)[-which(which((OrigParams[2:nrow(OrigParams),"h_gw_loss_coeff"] - OrigParams[1:(nrow(OrigParams)-1),"h_gw_loss_coeff"]) != 0) %in% seq(272,10880,272))],], MARGIN = 1, FUN = "-", as.numeric(MedHills05[which(MedHills05[,1] == 9),-1]))), MARGIN = 2, FUN = sum))/Deltas[,"h_gw_loss_coeff"] - EEs05_h[EEs05_h$HillID == 9, "h_gw_loss_coeff"]))
max(abs((apply(X = abs(apply(X = HillSF95[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams),"h_gw_loss_coeff"] - OrigParams[1:(nrow(OrigParams)-1),"h_gw_loss_coeff"]) != 0)[-which(which((OrigParams[2:nrow(OrigParams),"h_gw_loss_coeff"] - OrigParams[1:(nrow(OrigParams)-1),"h_gw_loss_coeff"]) != 0) %in% seq(272,10880,272))]+1,], MARGIN = 1, FUN = "-", as.numeric(MedHills95[which(MedHills95[,1] == 9),-1]))), MARGIN = 2, FUN = sum) - apply(X = abs(apply(X = HillSF95[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams),"h_gw_loss_coeff"] - OrigParams[1:(nrow(OrigParams)-1),"h_gw_loss_coeff"]) != 0)[-which(which((OrigParams[2:nrow(OrigParams),"h_gw_loss_coeff"] - OrigParams[1:(nrow(OrigParams)-1),"h_gw_loss_coeff"]) != 0) %in% seq(272,10880,272))],], MARGIN = 1, FUN = "-", as.numeric(MedHills95[which(MedHills95[,1] == 9),-1]))), MARGIN = 2, FUN = sum))/Deltas[,"h_gw_loss_coeff"] - EEs95_h[EEs95_h$HillID == 9, "h_gw_loss_coeff"]))
max(abs((apply(X = abs(apply(X = HillSFot[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams),"h_gw_loss_coeff"] - OrigParams[1:(nrow(OrigParams)-1),"h_gw_loss_coeff"]) != 0)[-which(which((OrigParams[2:nrow(OrigParams),"h_gw_loss_coeff"] - OrigParams[1:(nrow(OrigParams)-1),"h_gw_loss_coeff"]) != 0) %in% seq(272,10880,272))]+1,], MARGIN = 1, FUN = "-", as.numeric(MedHillsot[which(MedHillsot[,1] == 9),-1]))), MARGIN = 2, FUN = sum) - apply(X = abs(apply(X = HillSFot[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams),"h_gw_loss_coeff"] - OrigParams[1:(nrow(OrigParams)-1),"h_gw_loss_coeff"]) != 0)[-which(which((OrigParams[2:nrow(OrigParams),"h_gw_loss_coeff"] - OrigParams[1:(nrow(OrigParams)-1),"h_gw_loss_coeff"]) != 0) %in% seq(272,10880,272))],], MARGIN = 1, FUN = "-", as.numeric(MedHillsot[which(MedHillsot[,1] == 9),-1]))), MARGIN = 2, FUN = sum))/Deltas[,"h_gw_loss_coeff"] - EEsot_h[EEsot_h$HillID == 9, "h_gw_loss_coeff"]))

# Try metrics to plot practical significance - none seem useful and not using any----
#abs(SAE2 - SAE1)/n(timeseries length)/(max-min flow in timeseries) = average daily difference/(max - min)
#hist(abs((apply(X = abs(apply(X = HillSF05[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams),"h_gw_loss_coeff"] - OrigParams[1:(nrow(OrigParams)-1),"h_gw_loss_coeff"]) != 0)[-which(which((OrigParams[2:nrow(OrigParams),"h_gw_loss_coeff"] - OrigParams[1:(nrow(OrigParams)-1),"h_gw_loss_coeff"]) != 0) %in% seq(272,10880,272))]+1,], MARGIN = 1, FUN = "-", as.numeric(MedHills05[which(MedHills05[,1] == 9),-1]))), MARGIN = 2, FUN = sum) - apply(X = abs(apply(X = HillSF05[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams),"h_gw_loss_coeff"] - OrigParams[1:(nrow(OrigParams)-1),"h_gw_loss_coeff"]) != 0)[-which(which((OrigParams[2:nrow(OrigParams),"h_gw_loss_coeff"] - OrigParams[1:(nrow(OrigParams)-1),"h_gw_loss_coeff"]) != 0) %in% seq(272,10880,272))],], MARGIN = 1, FUN = "-", as.numeric(MedHills05[which(MedHills05[,1] == 9),-1]))), MARGIN = 2, FUN = sum)))/length(obs05$Date)/(max(obs05$Flow) - min(obs05$Flow)), breaks = 10)
for (i in 1:length(ParamSelect_h_Agg910)){
  if (!(i %in% c(10,11))){
    #abs(SAE2 - SAE1)/n(timeseries length) = average daily difference
    #hist(abs((apply(X = abs(apply(X = HillSF05[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] - OrigParams[1:(nrow(OrigParams)-1), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0)[-which(which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] - OrigParams[1:(nrow(OrigParams)-1), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0) %in% seq(272,10880,272))]+1,], MARGIN = 1, FUN = "-", as.numeric(MedHills05[which(MedHills05[,1] == 9),-1]))), MARGIN = 2, FUN = sum) - apply(X = abs(apply(X = HillSF05[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] - OrigParams[1:(nrow(OrigParams)-1),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0)[-which(which((OrigParams[2:nrow(OrigParams),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] - OrigParams[1:(nrow(OrigParams)-1),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0) %in% seq(272,10880,272))],], MARGIN = 1, FUN = "-", as.numeric(MedHills05[which(MedHills05[,1] == 9),-1]))), MARGIN = 2, FUN = sum)))/length(obs05$Date), breaks = 10, xlab = 'average daily difference', main = ParamSelect_h_Agg910[i])
    #hist(abs((apply(X = abs(apply(X = HillSFot[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] - OrigParams[1:(nrow(OrigParams)-1), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0)[-which(which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] - OrigParams[1:(nrow(OrigParams)-1), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0) %in% seq(272,10880,272))]+1,], MARGIN = 1, FUN = "-", as.numeric(MedHillsot[which(MedHillsot[,1] == 9),-1]))), MARGIN = 2, FUN = sum) - apply(X = abs(apply(X = HillSFot[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] - OrigParams[1:(nrow(OrigParams)-1),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0)[-which(which((OrigParams[2:nrow(OrigParams),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] - OrigParams[1:(nrow(OrigParams)-1),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0) %in% seq(272,10880,272))],], MARGIN = 1, FUN = "-", as.numeric(MedHillsot[which(MedHillsot[,1] == 9),-1]))), MARGIN = 2, FUN = sum)))/length(obsot$Date), breaks = 10, xlab = 'average daily difference', main = ParamSelect_h_Agg910[i])
    #hist(abs((apply(X = abs(apply(X = HillSF95[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] - OrigParams[1:(nrow(OrigParams)-1), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0)[-which(which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] - OrigParams[1:(nrow(OrigParams)-1), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0) %in% seq(272,10880,272))]+1,], MARGIN = 1, FUN = "-", as.numeric(MedHills95[which(MedHills95[,1] == 9),-1]))), MARGIN = 2, FUN = sum) - apply(X = abs(apply(X = HillSF95[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] - OrigParams[1:(nrow(OrigParams)-1),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0)[-which(which((OrigParams[2:nrow(OrigParams),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] - OrigParams[1:(nrow(OrigParams)-1),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0) %in% seq(272,10880,272))],], MARGIN = 1, FUN = "-", as.numeric(MedHills95[which(MedHills95[,1] == 9),-1]))), MARGIN = 2, FUN = sum)))/length(obs95$Date), breaks = 10, xlab = 'average daily difference', main = ParamSelect_h_Agg910[i])
    #abs(SAE2 - SAE1)/n(timeseries length)/(max-min flow in timeseries) = average daily difference/(max - min)
    #hist(abs((apply(X = abs(apply(X = HillSF05[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] - OrigParams[1:(nrow(OrigParams)-1), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0)[-which(which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] - OrigParams[1:(nrow(OrigParams)-1), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0) %in% seq(272,10880,272))]+1,], MARGIN = 1, FUN = "-", as.numeric(MedHills05[which(MedHills05[,1] == 9),-1]))), MARGIN = 2, FUN = sum) - apply(X = abs(apply(X = HillSF05[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] - OrigParams[1:(nrow(OrigParams)-1),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0)[-which(which((OrigParams[2:nrow(OrigParams),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] - OrigParams[1:(nrow(OrigParams)-1),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0) %in% seq(272,10880,272))],], MARGIN = 1, FUN = "-", as.numeric(MedHills05[which(MedHills05[,1] == 9),-1]))), MARGIN = 2, FUN = sum)))/length(obs05$Date)/(max(as.numeric(MedHills05[which(MedHills05[,1] == 9),-1])) - min(as.numeric(MedHills05[which(MedHills05[,1] == 9),-1]))), breaks = 10, xlab = 'average daily difference/(max - min flow)', main = ParamSelect_h_Agg910[i])
    #hist(abs((apply(X = abs(apply(X = HillSFot[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] - OrigParams[1:(nrow(OrigParams)-1), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0)[-which(which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] - OrigParams[1:(nrow(OrigParams)-1), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0) %in% seq(272,10880,272))]+1,], MARGIN = 1, FUN = "-", as.numeric(MedHillsot[which(MedHillsot[,1] == 9),-1]))), MARGIN = 2, FUN = sum) - apply(X = abs(apply(X = HillSFot[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] - OrigParams[1:(nrow(OrigParams)-1),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0)[-which(which((OrigParams[2:nrow(OrigParams),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] - OrigParams[1:(nrow(OrigParams)-1),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0) %in% seq(272,10880,272))],], MARGIN = 1, FUN = "-", as.numeric(MedHillsot[which(MedHillsot[,1] == 9),-1]))), MARGIN = 2, FUN = sum)))/length(obsot$Date)/(max(as.numeric(MedHillsot[which(MedHillsot[,1] == 9),-1])) - min(as.numeric(MedHillsot[which(MedHillsot[,1] == 9),-1]))), breaks = 10, xlab = 'average daily difference/(max - min flow)', main = ParamSelect_h_Agg910[i])
    #hist(abs((apply(X = abs(apply(X = HillSF95[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] - OrigParams[1:(nrow(OrigParams)-1), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0)[-which(which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] - OrigParams[1:(nrow(OrigParams)-1), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0) %in% seq(272,10880,272))]+1,], MARGIN = 1, FUN = "-", as.numeric(MedHills95[which(MedHills95[,1] == 9),-1]))), MARGIN = 2, FUN = sum) - apply(X = abs(apply(X = HillSF95[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] - OrigParams[1:(nrow(OrigParams)-1),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0)[-which(which((OrigParams[2:nrow(OrigParams),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] - OrigParams[1:(nrow(OrigParams)-1),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0) %in% seq(272,10880,272))],], MARGIN = 1, FUN = "-", as.numeric(MedHills95[which(MedHills95[,1] == 9),-1]))), MARGIN = 2, FUN = sum)))/length(obs95$Date)/(max(as.numeric(MedHills95[which(MedHills95[,1] == 9),-1])) - min(as.numeric(MedHills95[which(MedHills95[,1] == 9),-1]))), breaks = 10, xlab = 'average daily difference/(max - min flow)', main = ParamSelect_h_Agg910[i])
    #abs(SAE2 - SAE1)/(median(SAE2, SAE1)) = 
    hist(abs((apply(X = abs(apply(X = HillSF05[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] 
                                                                          - OrigParams[1:(nrow(OrigParams)-1), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0)[-which(which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] 
                                                                                                                                                                                            - OrigParams[1:(nrow(OrigParams)-1), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0) %in% seq(272,10880,272))]+1,], 
                                  MARGIN = 1, FUN = "-", as.numeric(MedHills05[which(MedHills05[,1] == 9),-1]))), 
                    MARGIN = 2, FUN = sum) 
              - apply(X = abs(apply(X = HillSF05[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] 
                                                                            - OrigParams[1:(nrow(OrigParams)-1),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0)[-which(which((OrigParams[2:nrow(OrigParams),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] 
                                                                                                                                                                                             - OrigParams[1:(nrow(OrigParams)-1),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0) %in% seq(272,10880,272))],], 
                                    MARGIN = 1, FUN = "-", as.numeric(MedHills05[which(MedHills05[,1] == 9),-1]))), 
                      MARGIN = 2, FUN = sum)))/
           apply(X = rbind(apply(X = abs(apply(X = HillSF05[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] 
                                                                              - OrigParams[1:(nrow(OrigParams)-1), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0)[-which(which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] 
                                                                                                                                                                                                - OrigParams[1:(nrow(OrigParams)-1), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0) %in% seq(272,10880,272))]+1,], 
                                      MARGIN = 1, FUN = "-", as.numeric(MedHills05[which(MedHills05[,1] == 9),-1]))), 
                        MARGIN = 2, FUN = sum), apply(X = abs(apply(X = HillSF05[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] 
                                                                                                            - OrigParams[1:(nrow(OrigParams)-1),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0)[-which(which((OrigParams[2:nrow(OrigParams),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] 
                                                                                                                                                                                                                             - OrigParams[1:(nrow(OrigParams)-1),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0) %in% seq(272,10880,272))],], 
                                                                    MARGIN = 1, FUN = "-", as.numeric(MedHills05[which(MedHills05[,1] == 9),-1]))), 
                                                      MARGIN = 2, FUN = sum)), MARGIN = 2, FUN = mean), 
         breaks = 10, xlab = 'average daily difference', main = ParamSelect_h_Agg910[i])
    hist(abs((apply(X = abs(apply(X = HillSF95[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] 
                                                                          - OrigParams[1:(nrow(OrigParams)-1), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0)[-which(which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] 
                                                                                                                                                                                            - OrigParams[1:(nrow(OrigParams)-1), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0) %in% seq(272,10880,272))]+1,], 
                                  MARGIN = 1, FUN = "-", as.numeric(MedHills95[which(MedHills95[,1] == 9),-1]))), 
                    MARGIN = 2, FUN = sum) 
              - apply(X = abs(apply(X = HillSF95[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] 
                                                                            - OrigParams[1:(nrow(OrigParams)-1),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0)[-which(which((OrigParams[2:nrow(OrigParams),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] 
                                                                                                                                                                                             - OrigParams[1:(nrow(OrigParams)-1),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0) %in% seq(272,10880,272))],], 
                                    MARGIN = 1, FUN = "-", as.numeric(MedHills95[which(MedHills95[,1] == 9),-1]))), 
                      MARGIN = 2, FUN = sum)))/
           apply(X = rbind(apply(X = abs(apply(X = HillSF95[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] 
                                                                                       - OrigParams[1:(nrow(OrigParams)-1), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0)[-which(which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] 
                                                                                                                                                                                                         - OrigParams[1:(nrow(OrigParams)-1), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0) %in% seq(272,10880,272))]+1,], 
                                               MARGIN = 1, FUN = "-", as.numeric(MedHills95[which(MedHills95[,1] == 9),-1]))), 
                                 MARGIN = 2, FUN = sum), apply(X = abs(apply(X = HillSF95[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] 
                                                                                                                     - OrigParams[1:(nrow(OrigParams)-1),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0)[-which(which((OrigParams[2:nrow(OrigParams),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] 
                                                                                                                                                                                                                                      - OrigParams[1:(nrow(OrigParams)-1),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0) %in% seq(272,10880,272))],], 
                                                                             MARGIN = 1, FUN = "-", as.numeric(MedHills95[which(MedHills95[,1] == 9),-1]))), 
                                                               MARGIN = 2, FUN = sum)), MARGIN = 2, FUN = mean), 
         breaks = 10, xlab = 'average daily difference', main = ParamSelect_h_Agg910[i])
    hist(abs((apply(X = abs(apply(X = HillSFot[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] 
                                                                          - OrigParams[1:(nrow(OrigParams)-1), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0)[-which(which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] 
                                                                                                                                                                                            - OrigParams[1:(nrow(OrigParams)-1), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0) %in% seq(272,10880,272))]+1,], 
                                  MARGIN = 1, FUN = "-", as.numeric(MedHillsot[which(MedHillsot[,1] == 9),-1]))), 
                    MARGIN = 2, FUN = sum) 
              - apply(X = abs(apply(X = HillSFot[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] 
                                                                            - OrigParams[1:(nrow(OrigParams)-1),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0)[-which(which((OrigParams[2:nrow(OrigParams),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] 
                                                                                                                                                                                             - OrigParams[1:(nrow(OrigParams)-1),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0) %in% seq(272,10880,272))],], 
                                    MARGIN = 1, FUN = "-", as.numeric(MedHillsot[which(MedHillsot[,1] == 9),-1]))), 
                      MARGIN = 2, FUN = sum)))/
           apply(X = rbind(apply(X = abs(apply(X = HillSFot[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] 
                                                                                       - OrigParams[1:(nrow(OrigParams)-1), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0)[-which(which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] 
                                                                                                                                                                                                         - OrigParams[1:(nrow(OrigParams)-1), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0) %in% seq(272,10880,272))]+1,], 
                                               MARGIN = 1, FUN = "-", as.numeric(MedHillsot[which(MedHillsot[,1] == 9),-1]))), 
                                 MARGIN = 2, FUN = sum), apply(X = abs(apply(X = HillSFot[(9-1)*nrow(BasinSF)+which((OrigParams[2:nrow(OrigParams), which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] 
                                                                                                                     - OrigParams[1:(nrow(OrigParams)-1),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0)[-which(which((OrigParams[2:nrow(OrigParams),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])] 
                                                                                                                                                                                                                                      - OrigParams[1:(nrow(OrigParams)-1),which(colnames(OrigParams) == ParamSelect_h_Agg910[i])]) != 0) %in% seq(272,10880,272))],], 
                                                                             MARGIN = 1, FUN = "-", as.numeric(MedHillsot[which(MedHillsot[,1] == 9),-1]))), 
                                                               MARGIN = 2, FUN = sum)), MARGIN = 2, FUN = mean), 
         breaks = 10, xlab = 'average daily difference', main = ParamSelect_h_Agg910[i])
  }
}
