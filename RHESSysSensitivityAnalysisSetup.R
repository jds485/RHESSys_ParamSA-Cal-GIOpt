#Script for setting up the calculation of Morris EEs from output streamflow and TN data

#Set directories----
#Color functions - from JDS github repo: Geothermal_ESDA
dir_Main = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR"
dir_ColFuns = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\Hydrology\\USGSGauges"

#Load libraries----
library(sp)
library(rgdal)
library(vroom)

#Load functions----
source(paste0(dir_ColFuns, '\\ColorFunctions.R'))

#Load Morris parameter files and ranges----
InputParams = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs\\MorrisSampleLocs\\MorrisSamples_AfterProcessing_EditToRerun5SA.csv", stringsAsFactors = FALSE)
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
setwd(dir_Main)
BasinSF = vroom(file = 'SAResults_BasinStreamflow_p4_Reordered_Add5.txt', delim = '\t', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
HillSF = vroom(file = 'SAResults_HillStreamflow_p6_Reordered_Add5.txt', delim = '\t', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)

#Load data from before Add5 for EE calculation
BasinSF_pre = vroom(file = 'SAResults_BasinStreamflow_p4_Reordered.txt', delim = '\t', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
HillSF_pre = vroom(file = 'SAResults_HillStreamflow_p6_Reordered.txt', delim = '\t', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)

#Process for only the 5 replicates needed
BasinSF_pre = BasinSF_pre[BasinSF_pre$Replicate %in% c(645,3366,4914,5389,5838),]
HillSF_pre = HillSF_pre[HillSF_pre$Replicate %in% c(645,3366,4914,5389,5838),]

#Save pre dataset for computing likelihoods----
#Columns are Date followed by Replicate#
tBasinSF = t(BasinSF_pre)
tBasinSF = tBasinSF[-1,]
tBasinSF = cbind(rownames(tBasinSF), tBasinSF)
colnames(tBasinSF) = c('Date', paste0('Replicate', c(645,3366,4914,5389,5838)))
write.table(tBasinSF, file = 'SAResults_BasinStreamflow_p4_Reordered_Add5_Likes_pre.txt', row.names = FALSE, col.names = TRUE, sep = '\t')
rm(tBasinSF)

#Load WRTDS TN data----
BasinTN05 = vroom(file = 'SAResults_BasinTN05_p3_All_Reordered_Add5.txt', delim = '\t', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
BasinTNMed = vroom(file = 'SAResults_BasinTNMed_p3_All_Reordered_Add5.txt', delim = '\t', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
BasinTN95 = vroom(file = 'SAResults_BasinTN95_p3_All_Reordered_Add5.txt', delim = '\t', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
HillTN05 = vroom(file = 'SAResults_HillTN05_p3_All_Reordered_Add5.txt', delim = '\t', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
HillTNMed = vroom(file = 'SAResults_HillTNMed_p3_All_Reordered_Add5.txt', delim = '\t', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
HillTN95 = vroom(file = 'SAResults_HillTN95_p3_All_Reordered_Add5.txt', delim = '\t', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)

#Load data from before Add5 for EE calculation
BasinTN05_pre = vroom(file = 'SAResults_BasinTN05_p3_All_Reordered.txt', delim = '\t', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
BasinTNMed_pre = vroom(file = 'SAResults_BasinTNMed_p3_All_Reordered.txt', delim = '\t', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
BasinTN95_pre = vroom(file = 'SAResults_BasinTN95_p3_All_Reordered.txt', delim = '\t', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
HillTN05_pre = vroom(file = 'SAResults_HillTN05_p3_All_Reordered.txt', delim = '\t', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
HillTNMed_pre = vroom(file = 'SAResults_HillTNMed_p3_All_Reordered.txt', delim = '\t', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
HillTN95_pre = vroom(file = 'SAResults_HillTN95_p3_All_Reordered.txt', delim = '\t', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)

#Process for only the 5 replicates needed
BasinTN05_pre = BasinTN05_pre[BasinTN05_pre$Replicate %in% c(645,3366,4914,5389,5838),]
BasinTNMed_pre = BasinTNMed_pre[BasinTNMed_pre$Replicate %in% c(645,3366,4914,5389,5838),]
BasinTN95_pre = BasinTN95_pre[BasinTN95_pre$Replicate %in% c(645,3366,4914,5389,5838),]
HillTN05_pre = HillTN05_pre[HillTN05_pre$Replicate %in% c(645,3366,4914,5389,5838),]
HillTNMed_pre = HillTNMed_pre[HillTNMed_pre$Replicate %in% c(645,3366,4914,5389,5838),]
HillTN95_pre = HillTN95_pre[HillTN95_pre$Replicate %in% c(645,3366,4914,5389,5838),]

# Remove observations earlier than 10/01/2004 (SA timeperiod start)----
HillSF = HillSF[, c(1, 2, which(as.Date(colnames(HillSF[,-c(1,2)])) >= as.Date('2004-10-01'))+2)]
HillTN05 = HillTN05[, c(1, 2, which(as.Date(colnames(HillTN05[,-c(1,2)])) >= as.Date('2004-10-01'))+2)]
HillTNMed = HillTNMed[, c(1, 2, which(as.Date(colnames(HillTNMed[,-c(1,2)])) >= as.Date('2004-10-01'))+2)]
HillTN95 = HillTN95[, c(1, 2, which(as.Date(colnames(HillTN95[,-c(1,2)])) >= as.Date('2004-10-01'))+2)]
HillSF_pre = HillSF_pre[, c(1, 2, which(as.Date(colnames(HillSF_pre[,-c(1,2)])) >= as.Date('2004-10-01'))+2)]
HillTN05_pre = HillTN05_pre[, c(1, 2, which(as.Date(colnames(HillTN05_pre[,-c(1,2)])) >= as.Date('2004-10-01'))+2)]
HillTNMed_pre = HillTNMed_pre[, c(1, 2, which(as.Date(colnames(HillTNMed_pre[,-c(1,2)])) >= as.Date('2004-10-01'))+2)]
HillTN95_pre = HillTN95_pre[, c(1, 2, which(as.Date(colnames(HillTN95_pre[,-c(1,2)])) >= as.Date('2004-10-01'))+2)]

BasinSF = BasinSF[, c(1, which(as.Date(colnames(BasinSF[,-1])) >= as.Date('2004-10-01'))+1)]
BasinTN05 = BasinTN05[, c(1, which(as.Date(colnames(BasinTN05[,-1])) >= as.Date('2004-10-01'))+1)]
BasinTNMed = BasinTNMed[, c(1, which(as.Date(colnames(BasinTNMed[,-1])) >= as.Date('2004-10-01'))+1)]
BasinTN95 = BasinTN95[, c(1, which(as.Date(colnames(BasinTN95[,-1])) >= as.Date('2004-10-01'))+1)]
BasinSF_pre = BasinSF_pre[, c(1, which(as.Date(colnames(BasinSF_pre[,-1])) >= as.Date('2004-10-01'))+1)]
BasinTN05_pre = BasinTN05_pre[, c(1, which(as.Date(colnames(BasinTN05_pre[,-1])) >= as.Date('2004-10-01'))+1)]
BasinTNMed_pre = BasinTNMed_pre[, c(1, which(as.Date(colnames(BasinTNMed_pre[,-1])) >= as.Date('2004-10-01'))+1)]
BasinTN95_pre = BasinTN95_pre[, c(1, which(as.Date(colnames(BasinTN95_pre[,-1])) >= as.Date('2004-10-01'))+1)]

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

#saving global lower 5th variables here for comparison
obs05 = obs05g = obs[obs$Flow <= q05,]
obs95 = obs[obs$Flow >= q95,]
days05 = days05g = as.Date(obs05$Date)
days95 = as.Date(obs95$Date)
obsot = obsotg = obs[-which(as.Date(obs$Date) %in% c(days05, days95)),]
daysot = daysotg = as.Date(obsot$Date)

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
HillSF05_pre = HillSF_pre[,-c(1,2)][,which(as.Date(colnames(HillSF_pre[1,-c(1,2)])) %in% days05)]
HillSF95_pre = HillSF_pre[,-c(1,2)][,which(as.Date(colnames(HillSF_pre[1,-c(1,2)])) %in% days95)]
HillSFot_pre = HillSF_pre[,-c(1,2)][,which(as.Date(colnames(HillSF_pre[1,-c(1,2)])) %in% daysot)]

BasinSF05 = BasinSF[,-1][,which(as.Date(colnames(BasinSF[1,-1])) %in% days05)]
BasinSF95 = BasinSF[,-1][,which(as.Date(colnames(BasinSF[1,-1])) %in% days95)]
BasinSFot = BasinSF[,-1][,which(as.Date(colnames(BasinSF[1,-1])) %in% daysot)]
BasinSF05_pre = BasinSF_pre[,-1][,which(as.Date(colnames(BasinSF_pre[1,-1])) %in% days05)]
BasinSF95_pre = BasinSF_pre[,-1][,which(as.Date(colnames(BasinSF_pre[1,-1])) %in% days95)]
BasinSFot_pre = BasinSF_pre[,-1][,which(as.Date(colnames(BasinSF_pre[1,-1])) %in% daysot)]

#global 5th
BasinSF05g = BasinSF[,-1][,which(as.Date(colnames(BasinSF[1,-1])) %in% days05g)]
BasinSFotg = BasinSF[,-1][,which(as.Date(colnames(BasinSF[1,-1])) %in% daysotg)]
HillSF05g = HillSF[,-c(1,2)][,which(as.Date(colnames(HillSF[1,-c(1,2)])) %in% days05g)]
HillSFotg = HillSF[,-c(1,2)][,which(as.Date(colnames(HillSF[1,-c(1,2)])) %in% daysotg)]
BasinSF05g_pre = BasinSF_pre[,-1][,which(as.Date(colnames(BasinSF_pre[1,-1])) %in% days05g)]
BasinSFotg_pre = BasinSF_pre[,-1][,which(as.Date(colnames(BasinSF_pre[1,-1])) %in% daysotg)]
HillSF05g_pre = HillSF_pre[,-c(1,2)][,which(as.Date(colnames(HillSF_pre[1,-c(1,2)])) %in% days05g)]
HillSFotg_pre = HillSF_pre[,-c(1,2)][,which(as.Date(colnames(HillSF_pre[1,-c(1,2)])) %in% daysotg)]

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
#MedHills05 = MedHills[,c(1, which(as.Date(colnames(MedHills)[-1]) %in% as.Date(days05)))]
#MedHills95 = MedHills[,c(1, which(as.Date(colnames(MedHills)[-1]) %in% as.Date(days95)))]
#MedHillsot = MedHills[,c(1, which(as.Date(colnames(MedHills)[-1]) %in% as.Date(daysot)))]
#MedHills05g = MedHills[,c(1, which(as.Date(colnames(MedHills)[-1]) %in% as.Date(days05g)))]
#MedHillsotg = MedHills[,c(1, which(as.Date(colnames(MedHills)[-1]) %in% as.Date(daysotg)))]
#This is the fixed method
MedHills05 = MedHills[,c(1, 1+which(as.Date(colnames(MedHills)[-1]) %in% as.Date(days05)))]
MedHills95 = MedHills[,c(1, 1+which(as.Date(colnames(MedHills)[-1]) %in% as.Date(days95)))]
MedHillsot = MedHills[,c(1, 1+which(as.Date(colnames(MedHills)[-1]) %in% as.Date(daysot)))]
MedHills05g = MedHills[,c(1, 1+which(as.Date(colnames(MedHills)[-1]) %in% as.Date(days05g)))]
MedHillsotg = MedHills[,c(1, 1+which(as.Date(colnames(MedHills)[-1]) %in% as.Date(daysotg)))]
rm(MedHills)

#Plot simulated prior flows and observed flows----
png('SimObsFlows.png', res = 300, width = 5, height = 5, units = 'in')
matplot(x = as.numeric(as.Date(colnames(BasinSF)[-1])), y = t(as.matrix(unique(BasinSF[,-1]))), 
        type = 'l', col = adjustcolor('black', alpha=0.01), ylim = c(0,40), xlab = 'Time', ylab = 'Streamflow (cfs)', lwd = 0.01)
par(new=TRUE)
plot(x = as.numeric(as.Date(colnames(BasinSF)[-1])), y = obs$Flow, 
     type = 'l', col = 'red', ylim = c(0,40), ann = FALSE, axes = FALSE, lwd = 0.01)
legend('topleft', legend = c('Simulated from SA Replicates', 'Observed'), col = c('black', 'red'), pch = NA, lwd = 0.01)
dev.off()

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
BasinTN05_pre = BasinTN05_pre[,c(1,which(as.Date(colnames(BasinTN05_pre[,-1])) %in% as.Date(obsTN$Date))+1)]
BasinTNMed_pre = BasinTNMed_pre[,c(1,which(as.Date(colnames(BasinTNMed_pre[,-1])) %in% as.Date(obsTN$Date))+1)]
BasinTN95_pre = BasinTN95_pre[,c(1,which(as.Date(colnames(BasinTN95_pre[,-1])) %in% as.Date(obsTN$Date))+1)]

HillTN05 = HillTN05[,c(1,2,which(as.Date(colnames(HillTN05[,-c(1,2)])) %in% as.Date(obsTN$Date))+2)]
HillTNMed = HillTNMed[,c(1,2,which(as.Date(colnames(HillTNMed[,-c(1,2)])) %in% as.Date(obsTN$Date))+2)]
HillTN95 = HillTN95[,c(1,2,which(as.Date(colnames(HillTN95[,-c(1,2)])) %in% as.Date(obsTN$Date))+2)]
HillTN05_pre = HillTN05_pre[,c(1,2,which(as.Date(colnames(HillTN05_pre[,-c(1,2)])) %in% as.Date(obsTN$Date))+2)]
HillTNMed_pre = HillTNMed_pre[,c(1,2,which(as.Date(colnames(HillTNMed_pre[,-c(1,2)])) %in% as.Date(obsTN$Date))+2)]
HillTN95_pre = HillTN95_pre[,c(1,2,which(as.Date(colnames(HillTN95_pre[,-c(1,2)])) %in% as.Date(obsTN$Date))+2)]

#Create the median hillslope TNs that will be used as a reference point to compute Morris EEs----
MedTN05Hills = MedTNMedHills = MedTN95Hills = matrix(NA, nrow = length(uhills), ncol = ncol(HillTN05)-1)
for (h in 1:length(uhills)){
  MedTN05Hills[h,] = c(uhills[h], apply(X = HillTN05[which(HillTN05$HillID == uhills[h]),-c(1,2)], MARGIN = 2, FUN = median))
  MedTNMedHills[h,] = c(uhills[h], apply(X = HillTNMed[which(HillTNMed$HillID == uhills[h]),-c(1,2)], MARGIN = 2, FUN = median))
  MedTN95Hills[h,] = c(uhills[h], apply(X = HillTN95[which(HillTN95$HillID == uhills[h]),-c(1,2)], MARGIN = 2, FUN = median))
}
rm(h)

#Load likelihood of each SA replicate----
#Likes20 = read.csv(file = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Likelihood\\CorrectedLikelihood\\SA_Params_logLNoBC_Baisman_Flow_SQL_max1000_20samps.csv", check.names = FALSE, stringsAsFactors = FALSE)
#Likes40 = read.csv(file = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Likelihood\\CorrectedLikelihood\\40InitialLHS\\SA_Params_logLNoBC40_Baisman_Flow_SQL_max1000_20samps.csv", check.names = FALSE, stringsAsFactors = FALSE)
Likes200 = read.csv(file = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Likelihood\\CorrectedLikelihood\\200InitialLHS\\SA_Params_logLNoBC_Baisman_Flow_SQL_max1000_200samps.csv", check.names = FALSE, stringsAsFactors = FALSE)
Likes200_pre = read.csv(file = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Likelihood\\CorrectedLikelihood\\200InitialLHS\\SA_Params_logLNoBC_Baisman_Flow_SQL_max1000_200samps_pre.csv", check.names = FALSE, stringsAsFactors = FALSE)

#200 initial LHS better due to amount of unconverged points (< 0.5%).
#rm(Likes20, Likes40)

#Plot of calibration metric correlations
png('MetricScatterplot.png', res = 300, width = 7, height = 7, units = 'in')
psych::pairs.panels(Likes200[,c(8,9,13,17,21)], smooth = FALSE, scale = TRUE, 
                    density = FALSE, ellipses = FALSE, digits = 2, method = 'pearson',pch = 16, 
                    jiggle = FALSE, stars = FALSE, smoother = TRUE, breaks = 20, rug = FALSE, hist.col = 'gray')
dev.off()

png('MetricScatterplot_Spearman.png', res = 300, width = 7, height = 7, units = 'in')
psych::pairs.panels(Likes200[,c(8,9,13,17,21)], smooth = FALSE, scale = TRUE, 
                    density = FALSE, ellipses = FALSE, digits = 2, method = 'spearman',pch = 16, 
                    jiggle = FALSE, stars = FALSE, smoother = TRUE, breaks = 20, rug = FALSE, hist.col = 'gray')
dev.off()

#Add decision metrics to plot
SAE05g = apply(X = apply(X = apply(X = BasinSF05g, MARGIN = 1, FUN = "-", obs05g$Flow), MARGIN = 2, FUN = abs), MARGIN = 2, FUN = sum)
SAEotg = apply(X = apply(X = apply(X = BasinSFotg, MARGIN = 1, FUN = "-", obsotg$Flow), MARGIN = 2, FUN = abs), MARGIN = 2, FUN = sum)
SAE95 = apply(X = apply(X = apply(X = BasinSF95, MARGIN = 1, FUN = "-", obs95$Flow), MARGIN = 2, FUN = abs), MARGIN = 2, FUN = sum)
SAETN05 = apply(X = apply(X = apply(X = BasinTN05[,-1], MARGIN = 1, FUN = "-", obsTN$TN), MARGIN = 2, FUN = abs), MARGIN = 2, FUN = sum)
SAETNMed = apply(X = apply(X = apply(X = BasinTNMed[,-1], MARGIN = 1, FUN = "-", obsTN$TN), MARGIN = 2, FUN = abs), MARGIN = 2, FUN = sum)
SAETN95 = apply(X = apply(X = apply(X = BasinTN95[,-1], MARGIN = 1, FUN = "-", obsTN$TN), MARGIN = 2, FUN = abs), MARGIN = 2, FUN = sum)

#Plot of calibration metric correlations
png('MetricScatterplot_CalDec_Pearson.png', res = 300, width = 7, height = 7, units = 'in')
psych::pairs.panels(cbind(Likes200[,c(8,13,17,21)], SAE05g, SAEotg, SAE95, SAETN05, SAETNMed, SAETN95), smooth = FALSE, scale = TRUE, 
                    density = FALSE, ellipses = FALSE, digits = 2, method = 'pearson',pch = 16, 
                    jiggle = FALSE, stars = FALSE, smoother = TRUE, breaks = 20, rug = FALSE, hist.col = 'gray', cex.cor = 0.8)
dev.off()

png('MetricScatterplot_CalDec_Spearman.png', res = 300, width = 7, height = 7, units = 'in')
psych::pairs.panels(cbind(Likes200[,c(8,13,17,21)], SAE05g, SAEotg, SAE95, SAETN05, SAETNMed, SAETN95), 
                    smooth = FALSE, scale = FALSE, density = FALSE, ellipses = FALSE, digits = 2, method = 'spearman',pch = 16, 
                    stars = FALSE, smoother = TRUE, breaks = 20, rug = FALSE, hist.col = 'gray', jiggle = FALSE, cex.cor = 0.8)
dev.off()

#Save input data for Rivanna run----
#save.image(file = "C:/Users/js4yd/OneDrive - University of Virginia/BES_Data/BES_Data/RHESSysFiles/BR&POBR/EEs_All_Setup.RData")
#All data
save.image(file = "C:/Users/js4yd/OneDrive - University of Virginia/BES_Data/BES_Data/RHESSysFiles/BR&POBR/EEs_All_Setup_paper.RData")
#Only basin
rm(HillSF, HillSF_pre, HillSF05, HillSF05_pre, HillSF05g, HillSF05g_pre, HillSF95, HillSF95_pre, HillSFot, HillSFot_pre, HillSFotg, HillSFotg_pre, HillTN05, HillTN05_pre, HillTN95, HillTN95_pre, HillTNMed, HillTNMed_pre, MedHills05, MedHills05g, MedHills95, MedHillsot, MedHillsotg, MedTN05Hills, MedTN95Hills, MedTNMedHills)
save.image(file = "C:/Users/js4yd/OneDrive - University of Virginia/BES_Data/BES_Data/RHESSysFiles/BR&POBR/EEs_All_Setup_paperb.RData")
#Only hillslope - first half
load("C:/Users/js4yd/OneDrive - University of Virginia/BES_Data/BES_Data/RHESSysFiles/BR&POBR/EEs_All_Setup_paper.RData")
rm(BasinSF05, BasinSF05_pre, BasinSF05g, BasinSF05g_pre, BasinSF95, BasinSF95_pre, BasinSFot, BasinSFot_pre, BasinSFotg, BasinSFotg_pre, BasinTN05, BasinTN05_pre, BasinTN95, BasinTN95_pre, BasinTNMed, BasinTNMed_pre, HillSFotg, HillSFotg_pre, HillTN05, HillTN05_pre, HillTN95, HillTN95_pre, HillTNMed, HillTNMed_pre, MedHillsotg, MedTN05Hills, MedTN95Hills, MedTNMedHills)
save.image(file = "C:/Users/js4yd/OneDrive - University of Virginia/BES_Data/BES_Data/RHESSysFiles/BR&POBR/EEs_All_Setup_paperh1.RData")
#Only hillslope - second half
load("C:/Users/js4yd/OneDrive - University of Virginia/BES_Data/BES_Data/RHESSysFiles/BR&POBR/EEs_All_Setup_paper.RData")
rm(BasinSF05, BasinSF05_pre, BasinSF05g, BasinSF05g_pre, BasinSF95, BasinSF95_pre, BasinSFot, BasinSFot_pre, BasinSFotg, BasinSFotg_pre, BasinTN05, BasinTN05_pre, BasinTN95, BasinTN95_pre, BasinTNMed, BasinTNMed_pre, HillSF05g, HillSF05g_pre, HillSF95, HillSF95_pre, HillSFot, HillSFot_pre, MedHills05, MedHills05g, MedHills95, MedHillsot)
save.image(file = "C:/Users/js4yd/OneDrive - University of Virginia/BES_Data/BES_Data/RHESSysFiles/BR&POBR/EEs_All_Setup_paperh2.RData")

#Now compute EEs on Rivanna with these saved datasets----