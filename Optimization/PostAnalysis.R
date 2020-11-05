#Plotting Pareto Front for Synthetic Parameter Optimization

#Load Libraries----
library(plot3D)
library(rgl)
library(plot3Drgl)
library(vroom)

#Color functions - from JDS github repo: Geothermal_ESDA----
dir_ColFuns = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\Hydrology\\USGSGauges"
setwd(dir_ColFuns)
source('ColorFunctions.R')

#Load land cover for max GI computation----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptSynTruth")
LULC = read.csv('lulcFrac30m_maxGI.csv', stringsAsFactors = FALSE)

#Compute streamflow conversion factor----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation910")
world = read.csv('worldfile.csv', stringsAsFactors = FALSE)
res = 30
#Taking the unique patch IDs because strata can exist in more than one patch.
Area.basin = length(unique(world$patchID))*res^2
#Multiplier conversion for basin streamflow (mm/d)*conversion_b -> cfs
conversion_b = Area.basin/1000/(.3048^3)/24/3600

#Load synthetic observed timeseries----
ObsQNoError = read.table("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\SyntheticDataCreation\\Q0NoError.txt", header = TRUE, sep = '\t', stringsAsFactors = FALSE)
ObsQ = read.table("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\SyntheticDataCreation\\Q0.txt", header = TRUE, sep = '\t', stringsAsFactors = FALSE)
Resid = ObsQ$Flow - ObsQNoError$streamflow

#Compute the 5th and 95th %-iles for use later - using the calibration timeperiod
q05_cal = quantile(x = ObsQ$Flow, probs = 0.05)
q95_cal = quantile(x = ObsQ$Flow, probs = 0.95)

#Load 100% GI objectives data----
Q_b100 = vroom('Run401_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
#Make a new Date column
Q_b100$Date = as.Date(paste0(Q_b100$year, '-', Q_b100$month, '-', Q_b100$day))
#Trim off spin-up years
Q_b100 = Q_b100[which(as.Date(Q_b100$Date) >= as.Date('2004-10-01')),]
#Convert simulated streamflow to cfs units
Q_b100$streamflow = round(Q_b100$streamflow*conversion_b, 6)
#Make an evaporation + transpiration column
Q_b100$ET = Q_b100$evap + Q_b100$trans
#Retain only streamflow, sat def, detention storage, ET, and Date columns for space
Q_b100 = as.data.frame(Q_b100[,c('Date', 'streamflow', 'sat_def', 'detention_store', 'ET')])
#Add residuals
Q_b100$streamflow = Q_b100$streamflow + Resid
#Set negative flow to 0
Q_b100$streamflow[Q_b100$streamflow < 0] = 0
#Compute objectives
Sum95_b0 = sum(ObsQ$Flow[ObsQ$Flow >= q95_cal])
Sum95_b100 = sum(Q_b100$streamflow[which(Q_b100$streamflow >= q95_cal)])
Sum05_b0 = sum(ObsQ$Flow[ObsQ$Flow <= q05_cal])
Sum05_b100 = sum(Q_b100$streamflow[which(Q_b100$streamflow <= q05_cal)])
O1_b100 = (Sum95_b0 - Sum95_b100)/Sum95_b0
O2_b100 = (Sum05_b0 - Sum05_b100)/Sum05_b0
O3_b100 = sum(LULC$lulc13)/9

####Synthetic Optimization####
# Load Reference Set----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptSynTruth")
DVO = read.table('GInolic.referenceDVO', header = FALSE, sep = ' ')
colnames(DVO) = c('H9d','H9m','H9u','H10d','H10m','H10u', 'Flooding', 'LowFlow', 'NumTrees')

# Fixme: Load reference set over time----
#RefTime = read.table('GInolic_S7.runset', header = FALSE, sep = ' ')
#colnames(RefTime) = c('H9d','H9m','H9u','H10d','H10m','H10u', 'Flooding', 'LowFlow', 'NumTrees')

# Load convergence metrics----
ConvMetrics = read.table('GInolic_S7.metrics', header = FALSE, sep = ' ')
colnames(ConvMetrics) = c('Hypervolume', 'GenerationalDistance', 'InvertedGenerationalDistance', 'Spacing', 'EpsilonIndicator', 'MaximumParetoFrontError')

# Load Runtime Metrics (NFE, operator probability)----
Metrics0 = read.table('GInolic_S7_M0.data', header = FALSE, sep = ' ')
Metrics1 = read.table('GInolic_S7_M1.data', header = FALSE, sep = ' ')
colnames(Metrics0) = colnames(Metrics1) = c('NFE', 'ElapsedTime', 'SBX', 'DE', 'PCX', 'SPX', 'UNDX', 'UM', 'Improvements', 'Restarts', 'PopulationSize', 'ArchiveSize', 'MutationIndex', 'HelpRequests')

# Scatterplot matrix----
#Interesting behavior of where trees are planted with the objectives that are being met. mid 1st, then down, then up for flooding.
plot(DVO)

# Plot Pareto front----
scaleRange = c(0,18000)
scaleBy = 2000
Pal = rev(rainbow((scaleRange[2] - scaleRange[1])/scaleBy))

png('ParetoSyn_ColTrees.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO$Flooding*100, -DVO$LowFlow*100, col = colFun(DVO$NumTrees),
     xlim = c(0, 50), ylim=c(-100, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Reduction (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to Synthetic Truth', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
#Add max GI point
par(new=TRUE)
plot(x = O1_b100*100, y = O2_b100*100, pch = 17, col = colFun(O3_b100),xlim = c(0, 50), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16', 'Max GI'), col = colFun(c(seq(0,18000,2000), O3_b100)), pch = c(rep(16,9),17), cex=1.1)
#Ideal point reference marker
#par(new=TRUE)
#plot(50, 0, pch = '|', col = 'red',xlim = c(0, 50), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
#par(new=TRUE)
#plot(50, 0, pch = 'X', col = 'red',xlim = c(0, 50), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
dev.off()

png('ParetoSyn_ColTrees3D.png', res = 300, units = 'in', width = 6, height = 6)
points3D(x = -DVO$Flooding, y = -DVO$LowFlow, z = DVO$NumTrees,
     xlim = c(-1,1), ylim=c(-1,1), zlim = c(0,18000),
     xlab = 'Flooding', ylab = 'Low Flow', zlab = 'Number of Trees', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to Synthetic Truth', 
     breaks = seq(2000,18000,2000), bty = 'g', axes=TRUE)
dev.off()

png('ParetoSyn_ColTrees3D.png', res = 300, units = 'in', width = 6, height = 6)
scatter3D(x = -DVO$Flooding, y = -DVO$LowFlow, z = DVO$NumTrees,
         xlim = c(-1,1), ylim=c(-1,1), zlim = c(0,18000),
         xlab = 'Flooding', ylab = 'Low Flow', zlab = 'Number of Trees', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to Synthetic Truth', 
         breaks = seq(0,18000,2000), bty = 'g', axes=TRUE, ticktype = "detailed", font=2, adj=0.5)

plot3Drgl::scatter3Drgl(x = -DVO$Flooding, y = -DVO$LowFlow, z = DVO$NumTrees, 
              xlim = c(0,1), ylim=c(-1,1), zlim = c(0,17000), 
              xlab = 'Flooding', ylab = 'Low Flow', zlab = 'Number of Trees', 
              cex.axis = 1.5, cex.lab = 1.5, main = 'Pareto Front:Reforestation Optimization to Synthetic Truth', surface = FALSE)
dev.off()


# Runtime metrics plots----
png('ParetoSyn_TimevsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0$NFE, Metrics0$ElapsedTime, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Time', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to Synthetic Truth', pch = 16,
     xlim = c(0,80000), ylim = c(0,300000))
par(new=TRUE)
plot(Metrics1$NFE, Metrics1$ElapsedTime, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,300000))
legend('bottomright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoSyn_SBXvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0$NFE, Metrics0$SBX, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'SBX Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to Synthetic Truth', pch = 16,
     xlim = c(0,80000), ylim = c(0,1))
par(new=TRUE)
plot(Metrics1$NFE, Metrics1$SBX, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1))
legend('topright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoSyn_DEvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0$NFE, Metrics0$DE, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'DE Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to Synthetic Truth', pch = 16,
     xlim = c(0,80000), ylim = c(0,1))
par(new=TRUE)
plot(Metrics1$NFE, Metrics1$DE, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1))
legend('topright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoSyn_PCXvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0$NFE, Metrics0$PCX, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'PCX Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to Synthetic Truth', pch = 16,
     xlim = c(0,80000), ylim = c(0,1))
par(new=TRUE)
plot(Metrics1$NFE, Metrics1$PCX, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1))
legend('topright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoSyn_SPXvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0$NFE, Metrics0$SPX, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'SPX Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to Synthetic Truth', pch = 16,
     xlim = c(0,80000), ylim = c(0,1))
par(new=TRUE)
plot(Metrics1$NFE, Metrics1$SPX, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1))
legend('topright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoSyn_UNDXvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0$NFE, Metrics0$UNDX, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'UNDX Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to Synthetic Truth', pch = 16,
     xlim = c(0,80000), ylim = c(0,1))
par(new=TRUE)
plot(Metrics1$NFE, Metrics1$UNDX, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1))
legend('topright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoSyn_UMvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0$NFE, Metrics0$UM, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'UM Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to Synthetic Truth', pch = 16,
     xlim = c(0,80000), ylim = c(0,1))
par(new=TRUE)
plot(Metrics1$NFE, Metrics1$UM, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1))
legend('topright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoSyn_ImpvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0$NFE, Metrics0$Improvements, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Improvements', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to Synthetic Truth', pch = 16,
     xlim = c(0,80000), ylim = c(0,1200))
par(new=TRUE)
plot(Metrics1$NFE, Metrics1$Improvements, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1200))
legend('topleft', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoSyn_ResvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0$NFE, Metrics0$Restarts, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Restarts', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to Synthetic Truth', pch = 16,
     xlim = c(0,80000), ylim = c(-1,1))
par(new=TRUE)
plot(Metrics1$NFE, Metrics1$Restarts, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(-1,1))
legend('topleft', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoSyn_PopSizevsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0$NFE, Metrics0$PopulationSize, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Pop Size', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to Synthetic Truth', pch = 16,
     xlim = c(0,80000), ylim = c(0,1000))
par(new=TRUE)
plot(Metrics1$NFE, Metrics1$PopulationSize, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1000))
legend('topleft', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoSyn_ArchiveSizevsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0$NFE, Metrics0$ArchiveSize, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Archive Size', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to Synthetic Truth', pch = 16,
     xlim = c(0,80000), ylim=c(0,200))
par(new=TRUE)
plot(Metrics1$NFE, Metrics1$ArchiveSize, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,200))
legend('topleft', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoSyn_PopArchRatvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0$NFE, Metrics0$PopulationSize/Metrics0$ArchiveSize, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Population/Archive Ratio', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to Synthetic Truth', pch = 16,
     xlim = c(0,80000), ylim=c(0,10))
par(new=TRUE)
plot(Metrics1$NFE, Metrics1$PopulationSize/Metrics1$ArchiveSize, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,10))
legend('topleft', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

#  All operators----
pltcols = rainbow(6)
png('ParetoSyn_OpProbsvsNFE.png', res = 300, units = 'in', width = 12, height = 6)
layout(rbind(c(1,2)))
plot(Metrics0$NFE, Metrics0$SBX, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Operator Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Master 0', pch = 16,
     xlim = c(0,80000), ylim = c(0,1), col = pltcols[1])
par(new=TRUE)
plot(Metrics0$NFE, Metrics0$DE, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1), col = pltcols[2])
par(new=TRUE)
plot(Metrics0$NFE, Metrics0$PCX, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1), col = pltcols[3])
par(new=TRUE)
plot(Metrics0$NFE, Metrics0$SPX, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1), col = pltcols[4])
par(new=TRUE)
plot(Metrics0$NFE, Metrics0$UNDX, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1), col = pltcols[5])
par(new=TRUE)
plot(Metrics0$NFE, Metrics0$UM, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1), col = pltcols[6])

plot(Metrics1$NFE, Metrics1$SBX, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Operator Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Master 1', pch = 16,
     xlim = c(0,80000), ylim = c(0,1), col = pltcols[1])
par(new=TRUE)
plot(Metrics1$NFE, Metrics1$DE, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1), col = pltcols[2])
par(new=TRUE)
plot(Metrics1$NFE, Metrics1$PCX, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1), col = pltcols[3])
par(new=TRUE)
plot(Metrics1$NFE, Metrics1$SPX, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1), col = pltcols[4])
par(new=TRUE)
plot(Metrics1$NFE, Metrics1$UNDX, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1), col = pltcols[5])
par(new=TRUE)
plot(Metrics1$NFE, Metrics1$UM, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1), col = pltcols[6])
legend('topleft', legend = colnames(Metrics0)[3:8], col=pltcols, pch=NA, lty = 1, lwd = 2)
dev.off()

# Plot convergence metrics----
png('ParetoSyn_HypervolumevsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(c(Metrics0$NFE,74000), ConvMetrics$Hypervolume, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Hypervolume', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to Synthetic Truth', pch = 16,
     xlim = c(0,80000), ylim = c(0,1))
dev.off()

png('ParetoSyn_GDvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(c(Metrics0$NFE,74000), ConvMetrics$GenerationalDistance, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Generational Distance', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to Synthetic Truth', pch = 16,
     xlim = c(0,80000))
dev.off()

png('ParetoSyn_InvGDvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(c(Metrics0$NFE,74000), ConvMetrics$InvertedGenerationalDistance, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Inverted Generational Distance', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to Synthetic Truth', pch = 16,
     xlim = c(0,80000))
dev.off()

png('ParetoSyn_SpacingvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(c(Metrics0$NFE,74000), ConvMetrics$Spacing, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Spacing', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to Synthetic Truth', pch = 16,
     xlim = c(0,80000))
dev.off()

png('ParetoSyn_EIvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(c(Metrics0$NFE,74000), ConvMetrics$EpsilonIndicator, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Epsilon Indicator', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to Synthetic Truth', pch = 16,
     xlim = c(0,80000))
dev.off()

png('ParetoSyn_MaxErrvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(c(Metrics0$NFE,74000), ConvMetrics$MaximumParetoFrontError, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Max Pareto Front Error', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to Synthetic Truth', pch = 16,
     xlim = c(0,80000))
dev.off()

#  All convergence metrics----
png('ParetoSyn_ConvMetricsvsNFE.png', res = 300, units = 'in', width = 7, height = 6)
layout(rbind(c(1,2,3), c(4,5,6)))
plot(c(Metrics0$NFE, 74000), ConvMetrics$Hypervolume, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Hypervolume', pch = 16,
     xlim = c(0,80000), ylim = c(0,1))
plot(c(Metrics0$NFE, 74000), ConvMetrics$GenerationalDistance, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Generational Distance', pch = 16,
     xlim = c(0,80000))
plot(c(Metrics0$NFE, 74000), ConvMetrics$InvertedGenerationalDistance, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Inverted GD', pch = 16,
     xlim = c(0,80000))
plot(c(Metrics0$NFE, 74000), ConvMetrics$Spacing, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Spacing', pch = 16,
     xlim = c(0,80000))
plot(c(Metrics0$NFE, 74000), ConvMetrics$EpsilonIndicator, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Epsilon Indicator', pch = 16,
     xlim = c(0,80000))
plot(c(Metrics0$NFE, 74000), ConvMetrics$MaximumParetoFrontError, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Max Pareto Front Error', pch = 16,
     xlim = c(0,80000))
dev.off()

####MAP Optimization#########
# Load Reference Set----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptMAP")
DVO_MAP = read.table('GInolic.referenceDVO', header = FALSE, sep = ' ')
colnames(DVO_MAP) = c('H9d','H9m','H9u','H10d','H10m','H10u', 'Flooding', 'LowFlow', 'NumTrees')

# Load convergence metrics----
ConvMetrics_MAP = read.table('GInolic_S8.metrics', header = FALSE, sep = ' ')
colnames(ConvMetrics_MAP) = c('Hypervolume', 'GenerationalDistance', 'InvertedGenerationalDistance', 'Spacing', 'EpsilonIndicator', 'MaximumParetoFrontError')

# Load Runtime Metrics (NFE, operator probability)----
Metrics0_MAP = read.table('GInolic_S8_M0.data', header = FALSE, sep = ' ')
Metrics1_MAP = read.table('GInolic_S8_M1.data', header = FALSE, sep = ' ')
colnames(Metrics0_MAP) = colnames(Metrics1_MAP) = c('NFE', 'ElapsedTime', 'SBX', 'DE', 'PCX', 'SPX', 'UNDX', 'UM', 'Improvements', 'Restarts', 'PopulationSize', 'ArchiveSize', 'MutationIndex', 'HelpRequests')

# Scatterplot matrix----
#Interesting behavior of where trees are planted with the objectives that are being met. mid 1st, then up, then down for flooding.
plot(DVO_MAP)

# Plot Pareto front----
scaleRange = c(0,18000)
scaleBy = 2000
Pal = rev(rainbow((scaleRange[2] - scaleRange[1])/scaleBy))

png('ParetoMAP_ColTrees.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO_MAP$Flooding*100, -DVO_MAP$LowFlow*100, col = colFun(DVO_MAP$NumTrees),
     xlim = c(0, 50), ylim=c(-100, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Reduction (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MAP', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
#Add max GI point
par(new=TRUE)
plot(x = O1_b100*100, y = O2_b100*100, pch = 17, col = colFun(O3_b100),xlim = c(0, 50), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16', 'Max GI'), col = colFun(c(seq(0,18000,2000), O3_b100)), pch = c(rep(16,9),17), cex=1.1)
#Ideal point reference marker
#par(new=TRUE)
#plot(50, 0, pch = '|', col = 'red',xlim = c(0, 50), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
#par(new=TRUE)
#plot(50, 0, pch = 'X', col = 'red',xlim = c(0, 50), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
dev.off()

png('ParetoMAP_ColTrees3D.png', res = 300, units = 'in', width = 6, height = 6)
points3D(x = -DVO_MAP$Flooding, y = -DVO_MAP$LowFlow, z = DVO_MAP$NumTrees,
         xlim = c(-1,1), ylim=c(-1,1), zlim = c(0,18000),
         xlab = 'Flooding', ylab = 'Low Flow', zlab = 'Number of Trees', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MAP', 
         breaks = seq(2000,18000,2000), bty = 'g', axes=TRUE)
dev.off()

png('ParetoMAP_ColTrees3D.png', res = 300, units = 'in', width = 6, height = 6)
scatter3D(x = -DVO_MAP$Flooding, y = -DVO_MAP$LowFlow, z = DVO_MAP$NumTrees,
          xlim = c(-1,1), ylim=c(-1,1), zlim = c(0,18000),
          xlab = 'Flooding', ylab = 'Low Flow', zlab = 'Number of Trees', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MAP', 
          breaks = seq(0,18000,2000), bty = 'g', axes=TRUE, ticktype = "detailed", font=2, adj=0.5)

plot3Drgl::scatter3Drgl(x = -DVO_MAP$Flooding, y = -DVO_MAP$LowFlow, z = DVO_MAP$NumTrees, 
                        xlim = c(0,1), ylim=c(-1,1), zlim = c(0,17000), 
                        xlab = 'Flooding', ylab = 'Low Flow', zlab = 'Number of Trees', 
                        cex.axis = 1.5, cex.lab = 1.5, main = 'Pareto Front:Reforestation Optimization to MAP', surface = FALSE)
dev.off()


# Runtime metrics plots----
png('ParetoMAP_TimevsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MAP$NFE, Metrics0_MAP$ElapsedTime, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Time', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MAP', pch = 16,
     xlim = c(0,80000), ylim = c(0,300000))
par(new=TRUE)
plot(Metrics1_MAP$NFE, Metrics1_MAP$ElapsedTime, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,300000))
legend('bottomright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMAP_SBXvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MAP$NFE, Metrics0_MAP$SBX, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'SBX Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MAP', pch = 16,
     xlim = c(0,80000), ylim = c(0,1))
par(new=TRUE)
plot(Metrics1_MAP$NFE, Metrics1_MAP$SBX, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1))
legend('topright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMAP_DEvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MAP$NFE, Metrics0_MAP$DE, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'DE Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MAP', pch = 16,
     xlim = c(0,80000), ylim = c(0,1))
par(new=TRUE)
plot(Metrics1_MAP$NFE, Metrics1_MAP$DE, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1))
legend('topright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMAP_PCXvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MAP$NFE, Metrics0_MAP$PCX, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'PCX Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MAP', pch = 16,
     xlim = c(0,80000), ylim = c(0,1))
par(new=TRUE)
plot(Metrics1_MAP$NFE, Metrics1_MAP$PCX, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1))
legend('topright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMAP_SPXvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MAP$NFE, Metrics0_MAP$SPX, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'SPX Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MAP', pch = 16,
     xlim = c(0,80000), ylim = c(0,1))
par(new=TRUE)
plot(Metrics1_MAP$NFE, Metrics1_MAP$SPX, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1))
legend('topright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMAP_UNDXvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MAP$NFE, Metrics0_MAP$UNDX, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'UNDX Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MAP', pch = 16,
     xlim = c(0,80000), ylim = c(0,1))
par(new=TRUE)
plot(Metrics1_MAP$NFE, Metrics1_MAP$UNDX, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1))
legend('topright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMAP_UMvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MAP$NFE, Metrics0_MAP$UM, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'UM Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MAP', pch = 16,
     xlim = c(0,80000), ylim = c(0,1))
par(new=TRUE)
plot(Metrics1_MAP$NFE, Metrics1_MAP$UM, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1))
legend('topright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMAP_ImpvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MAP$NFE, Metrics0_MAP$Improvements, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Improvements', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MAP', pch = 16,
     xlim = c(0,80000), ylim = c(0,1200))
par(new=TRUE)
plot(Metrics1_MAP$NFE, Metrics1_MAP$Improvements, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1200))
legend('topleft', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMAP_ResvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MAP$NFE, Metrics0_MAP$Restarts, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Restarts', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MAP', pch = 16,
     xlim = c(0,80000), ylim = c(-1,1))
par(new=TRUE)
plot(Metrics1_MAP$NFE, Metrics1_MAP$Restarts, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(-1,1))
legend('topleft', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMAP_PopSizevsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MAP$NFE, Metrics0_MAP$PopulationSize, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Pop Size', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MAP', pch = 16,
     xlim = c(0,80000), ylim = c(0,1000))
par(new=TRUE)
plot(Metrics1_MAP$NFE, Metrics1_MAP$PopulationSize, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1000))
legend('topleft', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMAP_ArchiveSizevsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MAP$NFE, Metrics0_MAP$ArchiveSize, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Archive Size', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MAP', pch = 16,
     xlim = c(0,80000), ylim=c(0,200))
par(new=TRUE)
plot(Metrics1_MAP$NFE, Metrics1_MAP$ArchiveSize, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,200))
legend('topleft', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMAP_PopArchRatvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MAP$NFE, Metrics0_MAP$PopulationSize/Metrics0_MAP$ArchiveSize, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Population/Archive Ratio', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MAP', pch = 16,
     xlim = c(0,80000), ylim=c(0,10))
par(new=TRUE)
plot(Metrics1_MAP$NFE, Metrics1_MAP$PopulationSize/Metrics1_MAP$ArchiveSize, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,10))
legend('topleft', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

#  All operators----
pltcols = rainbow(6)
png('ParetoMAP_OpProbsvsNFE.png', res = 300, units = 'in', width = 12, height = 6)
layout(rbind(c(1,2)))
plot(Metrics0_MAP$NFE, Metrics0_MAP$SBX, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Operator Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Master 0', pch = 16,
     xlim = c(0,80000), ylim = c(0,1), col = pltcols[1])
par(new=TRUE)
plot(Metrics0_MAP$NFE, Metrics0_MAP$DE, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1), col = pltcols[2])
par(new=TRUE)
plot(Metrics0_MAP$NFE, Metrics0_MAP$PCX, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1), col = pltcols[3])
par(new=TRUE)
plot(Metrics0_MAP$NFE, Metrics0_MAP$SPX, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1), col = pltcols[4])
par(new=TRUE)
plot(Metrics0_MAP$NFE, Metrics0_MAP$UNDX, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1), col = pltcols[5])
par(new=TRUE)
plot(Metrics0_MAP$NFE, Metrics0_MAP$UM, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1), col = pltcols[6])

plot(Metrics1_MAP$NFE, Metrics1_MAP$SBX, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Operator Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Master 1', pch = 16,
     xlim = c(0,80000), ylim = c(0,1), col = pltcols[1])
par(new=TRUE)
plot(Metrics1_MAP$NFE, Metrics1_MAP$DE, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1), col = pltcols[2])
par(new=TRUE)
plot(Metrics1_MAP$NFE, Metrics1_MAP$PCX, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1), col = pltcols[3])
par(new=TRUE)
plot(Metrics1_MAP$NFE, Metrics1_MAP$SPX, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1), col = pltcols[4])
par(new=TRUE)
plot(Metrics1_MAP$NFE, Metrics1_MAP$UNDX, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1), col = pltcols[5])
par(new=TRUE)
plot(Metrics1_MAP$NFE, Metrics1_MAP$UM, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,80000), ylim = c(0,1), col = pltcols[6])
legend('topleft', legend = colnames(Metrics0_MAP)[3:8], col=pltcols, pch=NA, lty = 1, lwd = 2)
dev.off()

# Plot convergence metrics----
png('ParetoMAP_HypervolumevsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(c(Metrics0_MAP$NFE,74000), ConvMetrics_MAP$Hypervolume, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Hypervolume', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MAP', pch = 16,
     xlim = c(0,80000), ylim = c(0,1))
dev.off()

png('ParetoMAP_GDvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(c(Metrics0_MAP$NFE,74000), ConvMetrics_MAP$GenerationalDistance, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Generational Distance', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MAP', pch = 16,
     xlim = c(0,80000))
dev.off()

png('ParetoMAP_InvGDvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(c(Metrics0_MAP$NFE,74000), ConvMetrics_MAP$InvertedGenerationalDistance, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Inverted Generational Distance', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MAP', pch = 16,
     xlim = c(0,80000))
dev.off()

png('ParetoMAP_SpacingvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(c(Metrics0_MAP$NFE,74000), ConvMetrics_MAP$Spacing, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Spacing', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MAP', pch = 16,
     xlim = c(0,80000))
dev.off()

png('ParetoMAP_EIvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(c(Metrics0_MAP$NFE,74000), ConvMetrics_MAP$EpsilonIndicator, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Epsilon Indicator', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MAP', pch = 16,
     xlim = c(0,80000))
dev.off()

png('ParetoMAP_MaxErrvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(c(Metrics0_MAP$NFE,74000), ConvMetrics_MAP$MaximumParetoFrontError, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Max Pareto Front Error', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MAP', pch = 16,
     xlim = c(0,80000))
dev.off()

#  All convergence metrics----
png('ParetoMAP_ConvMetricsvsNFE.png', res = 300, units = 'in', width = 7, height = 6)
layout(rbind(c(1,2,3), c(4,5,6)))
plot(c(Metrics0_MAP$NFE, 74000), ConvMetrics_MAP$Hypervolume, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Hypervolume', pch = 16,
     xlim = c(0,80000), ylim = c(0,1))
plot(c(Metrics0_MAP$NFE, 74000), ConvMetrics_MAP$GenerationalDistance, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Generational Distance', pch = 16,
     xlim = c(0,80000))
plot(c(Metrics0_MAP$NFE, 74000), ConvMetrics_MAP$InvertedGenerationalDistance, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Inverted GD', pch = 16,
     xlim = c(0,80000))
plot(c(Metrics0_MAP$NFE, 74000), ConvMetrics_MAP$Spacing, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Spacing', pch = 16,
     xlim = c(0,80000))
plot(c(Metrics0_MAP$NFE, 74000), ConvMetrics_MAP$EpsilonIndicator, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Epsilon Indicator', pch = 16,
     xlim = c(0,80000))
plot(c(Metrics0_MAP$NFE, 74000), ConvMetrics_MAP$MaximumParetoFrontError, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Max Pareto Front Error', pch = 16,
     xlim = c(0,80000))
dev.off()

####MORO Optimization####