#Plotting Pareto Front for Synthetic Parameter Optimization

#Load Libraries----
library(plot3D)
library(rgl)
library(plot3Drgl)
library(vroom)
library(magick)
library(sp)
library(rgdal)
library(GISTools)
library(scico)

#Color functions - from JDS github repo: Geothermal_ESDA----
dir_ColFuns = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\Hydrology\\USGSGauges"
setwd(dir_ColFuns)
source('ColorFunctions.R')

#Pareto front movie functions----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\Code\\ParetoFrontMovie")
source("dataClean.R")

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

#With ID numbers
DVO_IDs = read.table('GInolic_IDs.referenceDVO', header = TRUE, sep = '\t')

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
     xlim = c(0, 60), ylim=c(-100, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to Synthetic Truth', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
#Add max GI point
par(new=TRUE)
plot(x = O1_b100*100, y = O2_b100*100, pch = 17, col = colFun(O3_b100),xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16', 'Max GI'), col = colFun(c(seq(0,18000,2000), O3_b100)), pch = c(rep(16,9),17), cex=1.1)
#Ideal point reference marker
#par(new=TRUE)
#plot(60, 0, pch = '|', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
#par(new=TRUE)
#plot(60, 0, pch = 'X', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
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
dev.off()

#Interactive
plot3Drgl::scatter3Drgl(x = -DVO$Flooding, y = -DVO$LowFlow, z = DVO$NumTrees, 
                        xlim = c(0,1), ylim=c(-1,1), zlim = c(0,17000), 
                        xlab = 'Flooding', ylab = 'Low Flow', zlab = 'Number of Trees', 
                        cex.axis = 1.5, cex.lab = 1.5, main = 'Pareto Front:Reforestation Optimization to Synthetic Truth', surface = FALSE)

#  Thresholds for flooding and low flow----
png('ParetoSyn_ColTrees_TargetThresh.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO$Flooding*100, -DVO$LowFlow*100, col = colFun(DVO$NumTrees),
     xlim = c(0, 60), ylim=c(-100, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to Synthetic Truth', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
lines(c(-110,110), c(-20,-20), col = 'red')
lines(c(20,20), c(-120,120), col = 'red')
#Add max GI point
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16'), col = colFun(c(seq(0,18000,2000))), pch = c(rep(16,9)), cex=1.1)
#Ideal point reference marker
#par(new=TRUE)
#plot(60, 0, pch = '|', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
#par(new=TRUE)
#plot(60, 0, pch = 'X', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
dev.off()

#Number of trees for the solutions that meet this objective
TreesThreshSyn = DVO_IDs[which((DVO_IDs$Flooding <= -0.2) & (DVO_IDs$LowFlow <= 0.2)),]

#  Pareto front movie----
#   Load Pareto front over time----
#ParetoEvo = read.table('GInolic_S7.runset', header = FALSE, sep = ' ')
#colnames(ParetoEvo) = c('H9d','H9m','H9u','H10d','H10m','H10u', 'Flooding', 'LowFlow', 'NumTrees')
#Read in and clean data
ParetoEvo = cleanData(fname = 'GInolic_S7.runset', separator = '#', objNames = c('Flooding', 'LowFlow', 'NumTrees'), numObjs = 3, numDVs = 6)

#Test of 3D movie
#makeParetoMovie(fname = 'GInolic_S7.runset', datSep = '#', mnmx = rep('min',3), objNames = c('Flooding', 'Low Flow', 'Num. Trees'),
#                objs = c(7,8,9,9,9,9), idealAdd = TRUE, colN = 10, colPal = rainbow(10), 
#                aniName='ParetoFrontSyn.mp4', aniInt = 0.05, ptMain='Pareto Front: Reforestation Optimization to Synthetic Truth')

#   Loop over all fronts----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptSynTruth\\ParetoEvo")
for (i in 1:length(unique(ParetoEvo$PtNo))){
        png(paste0('ParetoSyn_ColTrees_', unique(ParetoEvo$PtNo)[i], '.png'), res = 300, units = 'in', width = 6, height = 6)
        plot(-ParetoEvo$Flooding[ParetoEvo$PtNo == unique(ParetoEvo$PtNo)[i]]*100, -ParetoEvo$LowFlow[ParetoEvo$PtNo == unique(ParetoEvo$PtNo)[i]]*100, col = colFun(ParetoEvo$NumTrees[ParetoEvo$PtNo == unique(ParetoEvo$PtNo)[i]]),
             xlim = c(0, 60), ylim=c(-100, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = paste0('NFE = ', 200*unique(ParetoEvo$PtNo)[i], ': Reforestation Optimization to Synthetic Truth'), pch = 16)
        lines(c(-110,110), c(0,0))
        lines(c(-0,0), c(-120,120))
        #Add max GI point
        par(new=TRUE)
        plot(x = O1_b100*100, y = O2_b100*100, pch = 17, col = colFun(O3_b100),xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
        legend('bottomleft', title = 'Num. Trees (thous.)', 
               legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16', 'Max GI'), col = colFun(c(seq(0,18000,2000), O3_b100)), pch = c(rep(16,9),17), cex=1.1)
        dev.off()
}
rm(i)

#   Make animation----
for (i in 0:((length(unique(ParetoEvo$PtNo)))/5)){
        if (i >= 1){
                imgSyn = c(imgSyn, image_scale(image_read(paste('ParetoSyn_ColTrees_', i*5, '.png', sep='')), "750x750"))
        }else{
                imgSyn = image_scale(image_read(paste('ParetoSyn_ColTrees_', i+1, '.png', sep='')), "750x750")
        }
}
rm(i)

Animate = image_animate(image = imgSyn, fps = 1, dispose = 'previous', loop = 1)
image_write(image = Animate, format = 'gif', path = "ParetoSyn_ColTrees_All.gif")

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

#Load reference set transformed to synthetic true parameters
DVO_MAPSyn = read.table('DVO_c_MAPSyn.txt', header = TRUE, sep = '\t')
#Make sure Objective 3 is same
max(abs(DVO_MAPSyn$NumTrees - DVO_MAPSyn$NumTreesSyn))

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

plot(DVO_MAPSyn[,-c(7:11)])

#Histogram of difference in objective values from MAP to synthetic
png('ReEvalObjChangePct.png', res=200, width = 10, height = 5, units='in')
layout(rbind(c(1,2)))
hist(abs(DVO_MAPSyn$Flooding - DVO_MAPSyn$FloodingSyn)*100, breaks = seq(0,0.3,0.015)*100, xlab = 'Change from Synthetic (%)', main = 'MAP Flooding Objective', cex.lab=1.5, cex.axis=1.5, col = 'black', border = 'gray')
lines(c(1.5,1.5), c(0,500), col = 'red', lwd=2)
hist(abs(DVO_MAPSyn$LowFlow - DVO_MAPSyn$LowFlowSyn)*100, breaks = seq(0,0.4,0.015)*100, xlab = 'Change from Synthetic (%)', main = 'MAP Low Flow Objective', cex.lab=1.5, cex.axis=1.5, col = 'black', border = 'gray')
lines(c(1.5,1.5), c(0,30), col = 'red', lwd=2)
dev.off()

# Plot Pareto front----
scaleRange = c(0,18000)
scaleBy = 2000
Pal = rev(rainbow((scaleRange[2] - scaleRange[1])/scaleBy))

png('ParetoMAP_ColTrees.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO_MAP$Flooding*100, -DVO_MAP$LowFlow*100, col = colFun(DVO_MAP$NumTrees),
     xlim = c(0, 60), ylim=c(-100, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MAP', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
#Add max GI point
par(new=TRUE)
plot(x = O1_b100*100, y = O2_b100*100, pch = 17, col = colFun(O3_b100),xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16', 'Max GI'), col = colFun(c(seq(0,18000,2000), O3_b100)), pch = c(rep(16,9),17), cex=1.1)
#Ideal point reference marker
#par(new=TRUE)
#plot(60, 0, pch = '|', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
#par(new=TRUE)
#plot(60, 0, pch = 'X', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
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
dev.off()

#Interactive
plot3Drgl::scatter3Drgl(x = -DVO_MAP$Flooding, y = -DVO_MAP$LowFlow, z = DVO_MAP$NumTrees, 
                        xlim = c(0,1), ylim=c(-1,1), zlim = c(0,17000), 
                        xlab = 'Flooding', ylab = 'Low Flow', zlab = 'Number of Trees', 
                        cex.axis = 1.5, cex.lab = 1.5, main = 'Pareto Front:Reforestation Optimization to MAP', surface = FALSE)

#  Reevaluated on Synthetic Truth----
png('ParetoMAP_ColTrees_SynReEval.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO_MAPSyn$FloodingSyn*100, -DVO_MAPSyn$LowFlowSyn*100, col = colFun(DVO_MAPSyn$NumTreesSyn),
     xlim = c(0, 60), ylim=c(-100, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MAP on Synthetic Parameters', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
#Add max GI point
par(new=TRUE)
plot(x = O1_b100*100, y = O2_b100*100, pch = 17, col = colFun(O3_b100),xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16', 'Max GI'), col = colFun(c(seq(0,18000,2000), O3_b100)), pch = c(rep(16,9),17), cex=1.1)
#Ideal point reference marker
#par(new=TRUE)
#plot(60, 0, pch = '|', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
#par(new=TRUE)
#plot(60, 0, pch = 'X', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
dev.off()

#  Thresholds for flooding and low flow----
png('ParetoMAP_ColTrees_TargetThresh.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO_MAP$Flooding*100, -DVO_MAP$LowFlow*100, col = colFun(DVO_MAP$NumTrees),
     xlim = c(0, 60), ylim=c(-100, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MAP', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
lines(c(-110,110), c(-20,-20), col = 'red')
lines(c(20,20), c(-120,120), col = 'red')
#Add max GI point
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16'), col = colFun(c(seq(0,18000,2000))), pch = c(rep(16,9)), cex=1.1)
#Ideal point reference marker
#par(new=TRUE)
#plot(60, 0, pch = '|', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
#par(new=TRUE)
#plot(60, 0, pch = 'X', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
dev.off()

#Synthetic parameter set
png('ParetoMAPSyn_ColTrees_TargetThresh.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO_MAPSyn$FloodingSyn*100, -DVO_MAPSyn$LowFlowSyn*100, col = colFun(DVO_MAPSyn$NumTreesSyn),
     xlim = c(0, 60), ylim=c(-100, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MAP', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
lines(c(-110,110), c(-20,-20), col = 'red')
lines(c(20,20), c(-120,120), col = 'red')
#Add max GI point
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16'), col = colFun(c(seq(0,18000,2000))), pch = c(rep(16,9)), cex=1.1)
#Ideal point reference marker
#par(new=TRUE)
#plot(60, 0, pch = '|', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
#par(new=TRUE)
#plot(60, 0, pch = 'X', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
dev.off()

#Number of trees for the solutions that meet this objective in its space
TreesThreshMAP = DVO_MAPSyn[which((DVO_MAPSyn$Flooding <= -0.2) & (DVO_MAPSyn$LowFlow <= 0.2)),]

#  Pareto front movie----
#   Load Pareto front over time----
#Read in and clean data
ParetoEvo_MAP = cleanData(fname = 'GInolic_S8.runset', separator = '#', objNames = c('Flooding', 'LowFlow', 'NumTrees'), numObjs = 3, numDVs = 6)

#   Loop over all fronts----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptMAP\\ParetoEvo")
for (i in 1:length(unique(ParetoEvo_MAP$PtNo))){
        png(paste0('ParetoMAP_ColTrees_', unique(ParetoEvo_MAP$PtNo)[i], '.png'), res = 300, units = 'in', width = 6, height = 6)
        plot(-ParetoEvo_MAP$Flooding[ParetoEvo_MAP$PtNo == unique(ParetoEvo_MAP$PtNo)[i]]*100, -ParetoEvo_MAP$LowFlow[ParetoEvo_MAP$PtNo == unique(ParetoEvo_MAP$PtNo)[i]]*100, col = colFun(ParetoEvo_MAP$NumTrees[ParetoEvo_MAP$PtNo == unique(ParetoEvo_MAP$PtNo)[i]]),
             xlim = c(0, 60), ylim=c(-100, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = paste0('NFE = ', 200*unique(ParetoEvo_MAP$PtNo)[i], ': Reforestation Optimization to MAP'), pch = 16)
        lines(c(-110,110), c(0,0))
        lines(c(-0,0), c(-120,120))
        #Add max GI point
        par(new=TRUE)
        plot(x = O1_b100*100, y = O2_b100*100, pch = 17, col = colFun(O3_b100),xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
        legend('bottomleft', title = 'Num. Trees (thous.)', 
               legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16', 'Max GI'), col = colFun(c(seq(0,18000,2000), O3_b100)), pch = c(rep(16,9),17), cex=1.1)
        dev.off()
}
rm(i)

#   Make animation----
for (i in 0:((length(unique(ParetoEvo_MAP$PtNo))+1)/5)){
        if (i == (length(unique(ParetoEvo_MAP$PtNo))+1)/5){
                img = c(img, image_scale(image_read(paste('ParetoMAP_ColTrees_', i*5-1, '.png', sep='')), "750x750"))
        }else if (i >= 1){
                img = c(img, image_scale(image_read(paste('ParetoMAP_ColTrees_', i*5, '.png', sep='')), "750x750"))
        }else{
                img = image_scale(image_read(paste('ParetoMAP_ColTrees_', i+1, '.png', sep='')), "750x750")
        }
}
rm(i)

AnimateMAP = image_animate(image = img, fps = 1, dispose = 'previous', loop = 1)
image_write(image = AnimateMAP, format = 'gif', path = "ParetoMAP_ColTrees_All.gif")

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
# Load Reference Set----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptMORO")
DVO_MORO = read.table('GInolic.referenceDVO', header = FALSE, sep = ' ')
colnames(DVO_MORO) = c('H9d','H9m','H9u','H10d','H10m','H10u', 'Flooding', 'LowFlow', 'NumTrees')

#Load reference set transformed to synthetic true parameters
DVO_MOROSyn = read.table('DVO_c_MOROSyn.txt', header = TRUE, sep = '\t')
#Make sure Objective 3 is same
max(abs(DVO_MOROSyn$NumTrees - DVO_MOROSyn$NumTreesSyn))

# Load convergence metrics----
ConvMetrics_MORO = read.table('GInolic_S9.metrics', header = FALSE, sep = ' ')
colnames(ConvMetrics_MORO) = c('Hypervolume', 'GenerationalDistance', 'InvertedGenerationalDistance', 'Spacing', 'EpsilonIndicator', 'MaximumParetoFrontError')

# Load Runtime Metrics (NFE, operator probability)----
Metrics0_MORO = read.table('GInolic_S9_M0.data', header = FALSE, sep = ' ')
Metrics1_MORO = read.table('GInolic_S9_M1.data', header = FALSE, sep = ' ')
colnames(Metrics0_MORO) = colnames(Metrics1_MORO) = c('NFE', 'ElapsedTime', 'SBX', 'DE', 'PCX', 'SPX', 'UNDX', 'UM', 'Improvements', 'Restarts', 'PopulationSize', 'ArchiveSize', 'MutationIndex', 'HelpRequests')

# Scatterplot matrix----
#Interesting behavior of where trees are planted with the objectives that are being met. mid 1st, then up, then down for flooding.
plot(DVO_MORO)

plot(DVO_MOROSyn[,-c(7:11)])

#Histogram of difference in objective values from MORO to synthetic
png('ReEvalObjChangePct.png', res=200, width = 10, height = 5, units='in')
layout(rbind(c(1,2)))
hist(abs(DVO_MOROSyn$Flooding - DVO_MOROSyn$FloodingSyn)*100, breaks = seq(0,0.3,0.015)*100, xlab = 'Change from Synthetic (%)', main = 'MORO Flooding Objective', cex.lab=1.5, cex.axis=1.5, col = 'black', border = 'gray')
lines(c(1.5,1.5), c(0,50), col = 'red', lwd=2)
hist(abs(DVO_MOROSyn$LowFlow - DVO_MOROSyn$LowFlowSyn)*100, breaks = seq(0,0.4,0.015)*100, xlab = 'Change from Synthetic (%)', main = 'MORO Low Flow Objective', cex.lab=1.5, cex.axis=1.5, col = 'black', border = 'gray')
lines(c(1.5,1.5), c(0,30), col = 'red', lwd=2)
dev.off()

# Plot Pareto front----
scaleRange = c(0,18000)
scaleBy = 2000
Pal = rev(rainbow((scaleRange[2] - scaleRange[1])/scaleBy))

png('ParetoMORO_ColTrees.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO_MORO$Flooding*100, -DVO_MORO$LowFlow*100, col = colFun(DVO_MORO$NumTrees),
     xlim = c(0, 60), ylim=c(-100, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MORO', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
#Add max GI point
par(new=TRUE)
plot(x = O1_b100*100, y = O2_b100*100, pch = 17, col = colFun(O3_b100),xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16', 'Max GI'), col = colFun(c(seq(0,18000,2000), O3_b100)), pch = c(rep(16,9),17), cex=1.1)
#Ideal point reference marker
#par(new=TRUE)
#plot(60, 0, pch = '|', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
#par(new=TRUE)
#plot(60, 0, pch = 'X', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
dev.off()

png('ParetoMORO_ColTrees3D.png', res = 300, units = 'in', width = 6, height = 6)
points3D(x = -DVO_MORO$Flooding, y = -DVO_MORO$LowFlow, z = DVO_MORO$NumTrees,
         xlim = c(-1,1), ylim=c(-1,1), zlim = c(0,18000),
         xlab = 'Flooding', ylab = 'Low Flow', zlab = 'Number of Trees', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MORO', 
         breaks = seq(2000,18000,2000), bty = 'g', axes=TRUE)
dev.off()

png('ParetoMORO_ColTrees3D.png', res = 300, units = 'in', width = 6, height = 6)
scatter3D(x = -DVO_MORO$Flooding, y = -DVO_MORO$LowFlow, z = DVO_MORO$NumTrees,
          xlim = c(-1,1), ylim=c(-1,1), zlim = c(0,18000),
          xlab = 'Flooding', ylab = 'Low Flow', zlab = 'Number of Trees', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MORO', 
          breaks = seq(0,18000,2000), bty = 'g', axes=TRUE, ticktype = "detailed", font=2, adj=0.5)
dev.off()

#Interactive
plot3Drgl::scatter3Drgl(x = -DVO_MORO$Flooding, y = -DVO_MORO$LowFlow, z = DVO_MORO$NumTrees, 
                        xlim = c(0,1), ylim=c(-1,1), zlim = c(0,17000), 
                        xlab = 'Flooding', ylab = 'Low Flow', zlab = 'Number of Trees', 
                        cex.axis = 1.5, cex.lab = 1.5, main = 'Pareto Front:Reforestation Optimization to MORO', surface = FALSE)

#  Reevaluated on Synthetic Truth----
png('ParetoMORO_ColTrees_SynReEval.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO_MOROSyn$FloodingSyn*100, -DVO_MOROSyn$LowFlowSyn*100, col = colFun(DVO_MOROSyn$NumTreesSyn),
     xlim = c(0, 60), ylim=c(-100, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MORO on Synthetic Parameters', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
#Add max GI point
par(new=TRUE)
plot(x = O1_b100*100, y = O2_b100*100, pch = 17, col = colFun(O3_b100),xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16', 'Max GI'), col = colFun(c(seq(0,18000,2000), O3_b100)), pch = c(rep(16,9),17), cex=1.1)
#Ideal point reference marker
#par(new=TRUE)
#plot(60, 0, pch = '|', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
#par(new=TRUE)
#plot(60, 0, pch = 'X', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
dev.off()

#  Thresholds for flooding and low flow----
png('ParetoMORO_ColTrees_TargetThresh.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO_MORO$Flooding*100, -DVO_MORO$LowFlow*100, col = colFun(DVO_MORO$NumTrees),
     xlim = c(0, 60), ylim=c(-100, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MORO', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
lines(c(-110,110), c(-20,-20), col = 'red')
lines(c(20,20), c(-120,120), col = 'red')
#Add max GI point
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16'), col = colFun(c(seq(0,18000,2000))), pch = c(rep(16,9)), cex=1.1)
#Ideal point reference marker
#par(new=TRUE)
#plot(60, 0, pch = '|', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
#par(new=TRUE)
#plot(60, 0, pch = 'X', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
dev.off()

#Synthetic parameter set
png('ParetoMOROSyn_ColTrees_TargetThresh.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO_MOROSyn$FloodingSyn*100, -DVO_MOROSyn$LowFlowSyn*100, col = colFun(DVO_MOROSyn$NumTreesSyn),
     xlim = c(0, 60), ylim=c(-100, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MORO', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
lines(c(-110,110), c(-20,-20), col = 'red')
lines(c(20,20), c(-120,120), col = 'red')
#Add max GI point
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16'), col = colFun(c(seq(0,18000,2000))), pch = c(rep(16,9)), cex=1.1)
#Ideal point reference marker
#par(new=TRUE)
#plot(60, 0, pch = '|', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
#par(new=TRUE)
#plot(60, 0, pch = 'X', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
dev.off()

#Number of trees for the solutions that meet this objective
TreesThreshMORO = DVO_MOROSyn[which((DVO_MORO$Flooding <= -0.2) & (DVO_MORO$LowFlow <= 0.2)),]

#  Pareto front movie----
#   Load Pareto front over time----
#Read in and clean data
ParetoEvo_MORO = cleanData(fname = 'GInolic_S9.runset', separator = '#', objNames = c('Flooding', 'LowFlow', 'NumTrees'), numObjs = 3, numDVs = 6)

#   Loop over all fronts----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptMORO\\ParetoEvo")
for (i in 1:length(unique(ParetoEvo_MORO$PtNo))){
        png(paste0('ParetoMORO_ColTrees_', unique(ParetoEvo_MORO$PtNo)[i], '.png'), res = 300, units = 'in', width = 6, height = 6)
        plot(-ParetoEvo_MORO$Flooding[ParetoEvo_MORO$PtNo == unique(ParetoEvo_MORO$PtNo)[i]]*100, -ParetoEvo_MORO$LowFlow[ParetoEvo_MORO$PtNo == unique(ParetoEvo_MORO$PtNo)[i]]*100, col = colFun(ParetoEvo_MORO$NumTrees[ParetoEvo_MORO$PtNo == unique(ParetoEvo_MORO$PtNo)[i]]),
             xlim = c(0, 60), ylim=c(-100, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = paste0('NFE = ', 200*unique(ParetoEvo_MORO$PtNo)[i], ': Reforestation Optimization to MORO'), pch = 16)
        lines(c(-110,110), c(0,0))
        lines(c(-0,0), c(-120,120))
        #Add max GI point
        par(new=TRUE)
        plot(x = O1_b100*100, y = O2_b100*100, pch = 17, col = colFun(O3_b100),xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
        legend('bottomleft', title = 'Num. Trees (thous.)', 
               legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16', 'Max GI'), col = colFun(c(seq(0,18000,2000), O3_b100)), pch = c(rep(16,9),17), cex=1.1)
        dev.off()
}
rm(i)

#   Make animation----
for (i in 0:((length(unique(ParetoEvo_MORO$PtNo))+1)/2)){
        if (i == (length(unique(ParetoEvo_MORO$PtNo))+1)/2){
                imgMORO = c(imgMORO, image_scale(image_read(paste('ParetoMORO_ColTrees_', i*2-1, '.png', sep='')), "750x750"))
        }else if (i >= 1){
                imgMORO = c(imgMORO, image_scale(image_read(paste('ParetoMORO_ColTrees_', i*2, '.png', sep='')), "750x750"))
        }else{
                imgMORO = image_scale(image_read(paste('ParetoMORO_ColTrees_', i+1, '.png', sep='')), "750x750")
        }
}
rm(i)

AnimateMORO = image_animate(image = imgMORO, fps = 1, dispose = 'previous', loop = 1)
image_write(image = AnimateMORO, format = 'gif', path = "ParetoMORO_ColTrees_All.gif")

# Runtime metrics plots----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptMORO")
png('ParetoMORO_TimevsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MORO$NFE, Metrics0_MORO$ElapsedTime, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Time', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MORO', pch = 16,
     xlim = c(0,10000), ylim = c(0,300000))
par(new=TRUE)
plot(Metrics1_MORO$NFE, Metrics1_MORO$ElapsedTime, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,300000))
legend('bottomright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMORO_SBXvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MORO$NFE, Metrics0_MORO$SBX, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'SBX Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MORO', pch = 16,
     xlim = c(0,10000), ylim = c(0,1))
par(new=TRUE)
plot(Metrics1_MORO$NFE, Metrics1_MORO$SBX, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1))
legend('topright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMORO_DEvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MORO$NFE, Metrics0_MORO$DE, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'DE Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MORO', pch = 16,
     xlim = c(0,10000), ylim = c(0,1))
par(new=TRUE)
plot(Metrics1_MORO$NFE, Metrics1_MORO$DE, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1))
legend('topright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMORO_PCXvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MORO$NFE, Metrics0_MORO$PCX, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'PCX Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MORO', pch = 16,
     xlim = c(0,10000), ylim = c(0,1))
par(new=TRUE)
plot(Metrics1_MORO$NFE, Metrics1_MORO$PCX, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1))
legend('topright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMORO_SPXvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MORO$NFE, Metrics0_MORO$SPX, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'SPX Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MORO', pch = 16,
     xlim = c(0,10000), ylim = c(0,1))
par(new=TRUE)
plot(Metrics1_MORO$NFE, Metrics1_MORO$SPX, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1))
legend('topright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMORO_UNDXvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MORO$NFE, Metrics0_MORO$UNDX, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'UNDX Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MORO', pch = 16,
     xlim = c(0,10000), ylim = c(0,1))
par(new=TRUE)
plot(Metrics1_MORO$NFE, Metrics1_MORO$UNDX, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1))
legend('topright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMORO_UMvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MORO$NFE, Metrics0_MORO$UM, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'UM Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MORO', pch = 16,
     xlim = c(0,10000), ylim = c(0,1))
par(new=TRUE)
plot(Metrics1_MORO$NFE, Metrics1_MORO$UM, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1))
legend('topright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMORO_ImpvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MORO$NFE, Metrics0_MORO$Improvements, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Improvements', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MORO', pch = 16,
     xlim = c(0,10000), ylim = c(0,1200))
par(new=TRUE)
plot(Metrics1_MORO$NFE, Metrics1_MORO$Improvements, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1200))
legend('topleft', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMORO_ResvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MORO$NFE, Metrics0_MORO$Restarts, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Restarts', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MORO', pch = 16,
     xlim = c(0,10000), ylim = c(-1,1))
par(new=TRUE)
plot(Metrics1_MORO$NFE, Metrics1_MORO$Restarts, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(-1,1))
legend('topleft', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMORO_PopSizevsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MORO$NFE, Metrics0_MORO$PopulationSize, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Pop Size', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MORO', pch = 16,
     xlim = c(0,10000), ylim = c(0,1000))
par(new=TRUE)
plot(Metrics1_MORO$NFE, Metrics1_MORO$PopulationSize, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1000))
legend('topleft', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMORO_ArchiveSizevsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MORO$NFE, Metrics0_MORO$ArchiveSize, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Archive Size', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MORO', pch = 16,
     xlim = c(0,10000), ylim=c(0,200))
par(new=TRUE)
plot(Metrics1_MORO$NFE, Metrics1_MORO$ArchiveSize, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,200))
legend('topleft', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMORO_PopArchRatvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MORO$NFE, Metrics0_MORO$PopulationSize/Metrics0_MORO$ArchiveSize, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Population/Archive Ratio', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MORO', pch = 16,
     xlim = c(0,10000), ylim=c(0,10))
par(new=TRUE)
plot(Metrics1_MORO$NFE, Metrics1_MORO$PopulationSize/Metrics1_MORO$ArchiveSize, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,10))
legend('topleft', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

#  All operators----
pltcols = rainbow(6)
png('ParetoMORO_OpProbsvsNFE.png', res = 300, units = 'in', width = 12, height = 6)
layout(rbind(c(1,2)))
plot(Metrics0_MORO$NFE, Metrics0_MORO$SBX, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Operator Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Master 0', pch = 16,
     xlim = c(0,10000), ylim = c(0,1), col = pltcols[1])
par(new=TRUE)
plot(Metrics0_MORO$NFE, Metrics0_MORO$DE, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1), col = pltcols[2])
par(new=TRUE)
plot(Metrics0_MORO$NFE, Metrics0_MORO$PCX, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1), col = pltcols[3])
par(new=TRUE)
plot(Metrics0_MORO$NFE, Metrics0_MORO$SPX, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1), col = pltcols[4])
par(new=TRUE)
plot(Metrics0_MORO$NFE, Metrics0_MORO$UNDX, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1), col = pltcols[5])
par(new=TRUE)
plot(Metrics0_MORO$NFE, Metrics0_MORO$UM, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1), col = pltcols[6])

plot(Metrics1_MORO$NFE, Metrics1_MORO$SBX, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Operator Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Master 1', pch = 16,
     xlim = c(0,10000), ylim = c(0,1), col = pltcols[1])
par(new=TRUE)
plot(Metrics1_MORO$NFE, Metrics1_MORO$DE, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1), col = pltcols[2])
par(new=TRUE)
plot(Metrics1_MORO$NFE, Metrics1_MORO$PCX, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1), col = pltcols[3])
par(new=TRUE)
plot(Metrics1_MORO$NFE, Metrics1_MORO$SPX, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1), col = pltcols[4])
par(new=TRUE)
plot(Metrics1_MORO$NFE, Metrics1_MORO$UNDX, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1), col = pltcols[5])
par(new=TRUE)
plot(Metrics1_MORO$NFE, Metrics1_MORO$UM, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1), col = pltcols[6])
legend('topleft', legend = colnames(Metrics0_MORO)[3:8], col=pltcols, pch=NA, lty = 1, lwd = 2)
dev.off()

# Plot convergence metrics----
png('ParetoMORO_HypervolumevsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(c(Metrics0_MORO$NFE,9800), ConvMetrics_MORO$Hypervolume, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Hypervolume', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MORO', pch = 16,
     xlim = c(0,10000), ylim = c(0,1))
dev.off()

png('ParetoMORO_GDvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(c(Metrics0_MORO$NFE,9800), ConvMetrics_MORO$GenerationalDistance, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Generational Distance', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MORO', pch = 16,
     xlim = c(0,10000))
dev.off()

png('ParetoMORO_InvGDvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(c(Metrics0_MORO$NFE,9800), ConvMetrics_MORO$InvertedGenerationalDistance, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Inverted Generational Distance', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MORO', pch = 16,
     xlim = c(0,10000))
dev.off()

png('ParetoMORO_SpacingvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(c(Metrics0_MORO$NFE,9800), ConvMetrics_MORO$Spacing, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Spacing', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MORO', pch = 16,
     xlim = c(0,10000))
dev.off()

png('ParetoMORO_EIvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(c(Metrics0_MORO$NFE,9800), ConvMetrics_MORO$EpsilonIndicator, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Epsilon Indicator', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MORO', pch = 16,
     xlim = c(0,10000))
dev.off()

png('ParetoMORO_MaxErrvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(c(Metrics0_MORO$NFE,9800), ConvMetrics_MORO$MaximumParetoFrontError, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Max Pareto Front Error', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MORO', pch = 16,
     xlim = c(0,10000))
dev.off()

#  All convergence metrics----
png('ParetoMORO_ConvMetricsvsNFE.png', res = 300, units = 'in', width = 7, height = 6)
layout(rbind(c(1,2,3), c(4,5,6)))
plot(c(Metrics0_MORO$NFE, 9800), ConvMetrics_MORO$Hypervolume, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Hypervolume', pch = 16,
     xlim = c(0,10000), ylim = c(0,1))
plot(c(Metrics0_MORO$NFE, 9800), ConvMetrics_MORO$GenerationalDistance, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Generational Distance', pch = 16,
     xlim = c(0,10000))
plot(c(Metrics0_MORO$NFE, 9800), ConvMetrics_MORO$InvertedGenerationalDistance, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Inverted GD', pch = 16,
     xlim = c(0,10000))
plot(c(Metrics0_MORO$NFE, 9800), ConvMetrics_MORO$Spacing, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Spacing', pch = 16,
     xlim = c(0,10000))
plot(c(Metrics0_MORO$NFE, 9800), ConvMetrics_MORO$EpsilonIndicator, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Epsilon Indicator', pch = 16,
     xlim = c(0,10000))
plot(c(Metrics0_MORO$NFE, 9800), ConvMetrics_MORO$MaximumParetoFrontError, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Max Pareto Front Error', pch = 16,
     xlim = c(0,10000))
dev.off()

####MinMax Optimization####
# Load Reference Set----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptMinMax")
DVO_MMO = read.table('GI.referenceDVO', header = FALSE, sep = ' ')
colnames(DVO_MMO) = c('H9d','H9m','H9u','H10d','H10m','H10u', 'Flooding', 'LowFlow', 'NumTrees')

#Load reference set transformed to synthetic true parameters
DVO_MMOSyn = read.table('DVO_c_MOROMinMaxSyn.txt', header = TRUE, sep = '\t')
#Make sure Objective 3 is same
max(abs(DVO_MMOSyn$NumTrees - DVO_MMOSyn$NumTreesSyn))

# Load convergence metrics----
ConvMetrics_MMO = read.table('GI_S9.metrics', header = FALSE, sep = ' ')
colnames(ConvMetrics_MMO) = c('Hypervolume', 'GenerationalDistance', 'InvertedGenerationalDistance', 'Spacing', 'EpsilonIndicator', 'MaximumParetoFrontError')

# Load Runtime Metrics (NFE, operator probability)----
Metrics0_MMO = read.table('GI_S9_M0.data', header = FALSE, sep = ' ')
Metrics1_MMO = read.table('GI_S9_M1.data', header = FALSE, sep = ' ')
colnames(Metrics0_MMO) = colnames(Metrics1_MMO) = c('NFE', 'ElapsedTime', 'SBX', 'DE', 'PCX', 'SPX', 'UNDX', 'UM', 'Improvements', 'Restarts', 'PopulationSize', 'ArchiveSize', 'MutationIndex', 'HelpRequests')

# Scatterplot matrix----
#Interesting behavior of where trees are planted with the objectives that are being met. mid 1st, then up, then down for flooding.
plot(DVO_MMO)

plot(DVO_MMOSyn[,-c(7:11)])

#Histogram of difference in objective values from MMO to synthetic
png('ReEvalObjChangePct.png', res=200, width = 10, height = 5, units='in')
layout(rbind(c(1,2)))
hist(abs(DVO_MMOSyn$Flooding - DVO_MMOSyn$FloodingSyn)*100, breaks = seq(0,0.3,0.015)*100, xlab = 'Change from Synthetic (%)', main = 'MMO Flooding Objective', cex.lab=1.5, cex.axis=1.5, col = 'black', border = 'gray')
lines(c(1.5,1.5), c(0,50), col = 'red', lwd=2)
hist(abs(DVO_MMOSyn$LowFlow - DVO_MMOSyn$LowFlowSyn)*100, breaks = seq(0,0.4,0.015)*100, xlab = 'Change from Synthetic (%)', main = 'MMO Low Flow Objective', cex.lab=1.5, cex.axis=1.5, col = 'black', border = 'gray')
lines(c(1.5,1.5), c(0,30), col = 'red', lwd=2)
dev.off()

# Plot Pareto front----
scaleRange = c(0,18000)
scaleBy = 2000
Pal = rev(rainbow((scaleRange[2] - scaleRange[1])/scaleBy))

png('ParetoMMO_ColTrees.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO_MMO$Flooding*100, -DVO_MMO$LowFlow*100, col = colFun(DVO_MMO$NumTrees),
     xlim = c(0, 60), ylim=c(-120, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MMO', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-130,130))
#Add max GI point
par(new=TRUE)
plot(x = O1_b100*100, y = O2_b100*100, pch = 17, col = colFun(O3_b100),xlim = c(0, 60), ylim=c(-120, 10), axes=FALSE, xlab = '', ylab='')
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16', 'Max GI'), col = colFun(c(seq(0,18000,2000), O3_b100)), pch = c(rep(16,9),17), cex=1.1)
#Ideal point reference marker
#par(new=TRUE)
#plot(60, 0, pch = '|', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
#par(new=TRUE)
#plot(60, 0, pch = 'X', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
dev.off()

png('ParetoMMO_ColTrees3D.png', res = 300, units = 'in', width = 6, height = 6)
points3D(x = -DVO_MMO$Flooding, y = -DVO_MMO$LowFlow, z = DVO_MMO$NumTrees,
         xlim = c(-1,1), ylim=c(-1.2,1.2), zlim = c(0,18000),
         xlab = 'Flooding', ylab = 'Low Flow', zlab = 'Number of Trees', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MMO', 
         breaks = seq(2000,18000,2000), bty = 'g', axes=TRUE)
dev.off()

png('ParetoMMO_ColTrees3D.png', res = 300, units = 'in', width = 6, height = 6)
scatter3D(x = -DVO_MMO$Flooding, y = -DVO_MMO$LowFlow, z = DVO_MMO$NumTrees,
          xlim = c(-1,1), ylim=c(-1.2,1.2), zlim = c(0,18000),
          xlab = 'Flooding', ylab = 'Low Flow', zlab = 'Number of Trees', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MMO', 
          breaks = seq(0,18000,2000), bty = 'g', axes=TRUE, ticktype = "detailed", font=2, adj=0.5)
dev.off()

#Interactive
plot3Drgl::scatter3Drgl(x = -DVO_MMO$Flooding, y = -DVO_MMO$LowFlow, z = DVO_MMO$NumTrees, 
                        xlim = c(0,1), ylim=c(-1.2,1.2), zlim = c(0,18000), 
                        xlab = 'Flooding', ylab = 'Low Flow', zlab = 'Number of Trees', 
                        cex.axis = 1.5, cex.lab = 1.5, main = 'Pareto Front:Reforestation Optimization to MMO', surface = FALSE)

#  Reevaluated on Synthetic Truth----
png('ParetoMMO_ColTrees_SynReEval.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO_MMOSyn$FloodingSyn*100, -DVO_MMOSyn$LowFlowSyn*100, col = colFun(DVO_MMOSyn$NumTreesSyn),
     xlim = c(0, 60), ylim=c(-100, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MMO on Synthetic Parameters', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-130,130))
#Add max GI point
par(new=TRUE)
plot(x = O1_b100*100, y = O2_b100*100, pch = 17, col = colFun(O3_b100),xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16', 'Max GI'), col = colFun(c(seq(0,18000,2000), O3_b100)), pch = c(rep(16,9),17), cex=1.1)
#Ideal point reference marker
#par(new=TRUE)
#plot(60, 0, pch = '|', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
#par(new=TRUE)
#plot(60, 0, pch = 'X', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
dev.off()

png('ParetoMMO_ColTrees_SynReEval_SameScale.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO_MMOSyn$FloodingSyn*100, -DVO_MMOSyn$LowFlowSyn*100, col = colFun(DVO_MMOSyn$NumTreesSyn),
     xlim = c(0, 60), ylim=c(-120, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MMO on Synthetic Parameters', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-130,130))
#Add max GI point
par(new=TRUE)
plot(x = O1_b100*100, y = O2_b100*100, pch = 17, col = colFun(O3_b100),xlim = c(0, 60), ylim=c(-120, 10), axes=FALSE, xlab = '', ylab='')
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16', 'Max GI'), col = colFun(c(seq(0,18000,2000), O3_b100)), pch = c(rep(16,9),17), cex=1.1)
#Ideal point reference marker
#par(new=TRUE)
#plot(60, 0, pch = '|', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
#par(new=TRUE)
#plot(60, 0, pch = 'X', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
dev.off()

#  Thresholds for flooding and low flow----
png('ParetoMMO_ColTrees_TargetThresh.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO_MMO$Flooding*100, -DVO_MMO$LowFlow*100, col = colFun(DVO_MMO$NumTrees),
     xlim = c(0, 60), ylim=c(-120, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MMO', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-130,130))
lines(c(-110,110), c(-20,-20), col = 'red')
lines(c(20,20), c(-130,130), col = 'red')
#Add max GI point
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16'), col = colFun(c(seq(0,18000,2000))), pch = c(rep(16,9)), cex=1.1)
#Ideal point reference marker
#par(new=TRUE)
#plot(60, 0, pch = '|', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
#par(new=TRUE)
#plot(60, 0, pch = 'X', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
dev.off()

#Synthetic parameter set
png('ParetoMMOSyn_ColTrees_TargetThresh.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO_MMOSyn$FloodingSyn*100, -DVO_MMOSyn$LowFlowSyn*100, col = colFun(DVO_MMOSyn$NumTreesSyn),
     xlim = c(0, 60), ylim=c(-100, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MMO', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
lines(c(-110,110), c(-20,-20), col = 'red')
lines(c(20,20), c(-120,120), col = 'red')
#Add max GI point
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16'), col = colFun(c(seq(0,18000,2000))), pch = c(rep(16,9)), cex=1.1)
#Ideal point reference marker
#par(new=TRUE)
#plot(60, 0, pch = '|', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
#par(new=TRUE)
#plot(60, 0, pch = 'X', col = 'red',xlim = c(0, 60), ylim=c(-100, 10), axes=FALSE, xlab = '', ylab='')
dev.off()

#Number of trees for the solutions that meet this objective
TreesThreshMMO = DVO_MMOSyn[which((DVO_MMO$Flooding <= -0.2) & (DVO_MMO$LowFlow <= 0.2)),]

#  Pareto front movie----
#   Load Pareto front over time----
#Read in and clean data
ParetoEvo_MMO = cleanData(fname = 'GI_S9.runset', separator = '#', objNames = c('Flooding', 'LowFlow', 'NumTrees'), numObjs = 3, numDVs = 6)

#   Loop over all fronts----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptMinMax\\ParetoEvo")
for (i in 1:length(unique(ParetoEvo_MMO$PtNo))){
        png(paste0('ParetoMMO_ColTrees_', unique(ParetoEvo_MMO$PtNo)[i], '.png'), res = 300, units = 'in', width = 6, height = 6)
        plot(-ParetoEvo_MMO$Flooding[ParetoEvo_MMO$PtNo == unique(ParetoEvo_MMO$PtNo)[i]]*100, -ParetoEvo_MMO$LowFlow[ParetoEvo_MMO$PtNo == unique(ParetoEvo_MMO$PtNo)[i]]*100, col = colFun(ParetoEvo_MMO$NumTrees[ParetoEvo_MMO$PtNo == unique(ParetoEvo_MMO$PtNo)[i]]),
             xlim = c(0, 60), ylim=c(-120, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = paste0('NFE = ', 200*unique(ParetoEvo_MMO$PtNo)[i], ': Reforestation Optimization to MMO'), pch = 16)
        lines(c(-110,110), c(0,0))
        lines(c(-0,0), c(-130,130))
        #Add max GI point
        par(new=TRUE)
        plot(x = O1_b100*100, y = O2_b100*100, pch = 17, col = colFun(O3_b100),xlim = c(0, 60), ylim=c(-120, 10), axes=FALSE, xlab = '', ylab='')
        legend('bottomleft', title = 'Num. Trees (thous.)', 
               legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16', 'Max GI'), col = colFun(c(seq(0,18000,2000), O3_b100)), pch = c(rep(16,9),17), cex=1.1)
        dev.off()
}
rm(i)

#   Make animation----
for (i in 0:((length(unique(ParetoEvo_MMO$PtNo))+1)/2)){
        if (i == (length(unique(ParetoEvo_MMO$PtNo))+1)/2){
                imgMMO = c(imgMMO, image_scale(image_read(paste('ParetoMMO_ColTrees_', i*2-1, '.png', sep='')), "750x750"))
        }else if (i >= 1){
                imgMMO = c(imgMMO, image_scale(image_read(paste('ParetoMMO_ColTrees_', i*2, '.png', sep='')), "750x750"))
        }else{
                imgMMO = image_scale(image_read(paste('ParetoMMO_ColTrees_', i+1, '.png', sep='')), "750x750")
        }
}
rm(i)

AnimateMMO = image_animate(image = imgMMO, fps = 1, dispose = 'previous', loop = 1)
image_write(image = AnimateMMO, format = 'gif', path = "ParetoMMO_ColTrees_All.gif")

# Runtime metrics plots----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptMinMax")
png('ParetoMMO_TimevsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MMO$NFE, Metrics0_MMO$ElapsedTime, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Time', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MMO', pch = 16,
     xlim = c(0,10000), ylim = c(0,300000))
par(new=TRUE)
plot(Metrics1_MMO$NFE, Metrics1_MMO$ElapsedTime, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,300000))
legend('bottomright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMMO_SBXvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MMO$NFE, Metrics0_MMO$SBX, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'SBX Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MMO', pch = 16,
     xlim = c(0,10000), ylim = c(0,1))
par(new=TRUE)
plot(Metrics1_MMO$NFE, Metrics1_MMO$SBX, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1))
legend('topright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMMO_DEvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MMO$NFE, Metrics0_MMO$DE, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'DE Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MMO', pch = 16,
     xlim = c(0,10000), ylim = c(0,1))
par(new=TRUE)
plot(Metrics1_MMO$NFE, Metrics1_MMO$DE, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1))
legend('topright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMMO_PCXvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MMO$NFE, Metrics0_MMO$PCX, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'PCX Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MMO', pch = 16,
     xlim = c(0,10000), ylim = c(0,1))
par(new=TRUE)
plot(Metrics1_MMO$NFE, Metrics1_MMO$PCX, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1))
legend('topright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMMO_SPXvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MMO$NFE, Metrics0_MMO$SPX, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'SPX Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MMO', pch = 16,
     xlim = c(0,10000), ylim = c(0,1))
par(new=TRUE)
plot(Metrics1_MMO$NFE, Metrics1_MMO$SPX, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1))
legend('topright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMMO_UNDXvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MMO$NFE, Metrics0_MMO$UNDX, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'UNDX Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MMO', pch = 16,
     xlim = c(0,10000), ylim = c(0,1))
par(new=TRUE)
plot(Metrics1_MMO$NFE, Metrics1_MMO$UNDX, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1))
legend('topright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMMO_UMvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MMO$NFE, Metrics0_MMO$UM, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'UM Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MMO', pch = 16,
     xlim = c(0,10000), ylim = c(0,1))
par(new=TRUE)
plot(Metrics1_MMO$NFE, Metrics1_MMO$UM, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1))
legend('topright', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMMO_ImpvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MMO$NFE, Metrics0_MMO$Improvements, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Improvements', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MMO', pch = 16,
     xlim = c(0,10000), ylim = c(0,1200))
par(new=TRUE)
plot(Metrics1_MMO$NFE, Metrics1_MMO$Improvements, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1200))
legend('topleft', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMMO_ResvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MMO$NFE, Metrics0_MMO$Restarts, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Restarts', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MMO', pch = 16,
     xlim = c(0,10000), ylim = c(-1,1))
par(new=TRUE)
plot(Metrics1_MMO$NFE, Metrics1_MMO$Restarts, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(-1,1))
legend('topleft', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMMO_PopSizevsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MMO$NFE, Metrics0_MMO$PopulationSize, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Pop Size', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MMO', pch = 16,
     xlim = c(0,10000), ylim = c(0,1000))
par(new=TRUE)
plot(Metrics1_MMO$NFE, Metrics1_MMO$PopulationSize, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1000))
legend('topleft', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMMO_ArchiveSizevsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MMO$NFE, Metrics0_MMO$ArchiveSize, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Archive Size', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MMO', pch = 16,
     xlim = c(0,10000), ylim=c(0,200))
par(new=TRUE)
plot(Metrics1_MMO$NFE, Metrics1_MMO$ArchiveSize, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,200))
legend('topleft', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

png('ParetoMMO_PopArchRatvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(Metrics0_MMO$NFE, Metrics0_MMO$PopulationSize/Metrics0_MMO$ArchiveSize, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Population/Archive Ratio', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MMO', pch = 16,
     xlim = c(0,10000), ylim=c(0,10))
par(new=TRUE)
plot(Metrics1_MMO$NFE, Metrics1_MMO$PopulationSize/Metrics1_MMO$ArchiveSize, type = 'l', lwd = 2, col = 'red',
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,10))
legend('topleft', legend = c('M0', 'M1'), col=c('black', 'red'), pch=NA, lty = 1, lwd = 2)
dev.off()

#  All operators----
pltcols = rainbow(6)
png('ParetoMMO_OpProbsvsNFE.png', res = 300, units = 'in', width = 12, height = 6)
layout(rbind(c(1,2)))
plot(Metrics0_MMO$NFE, Metrics0_MMO$SBX, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Operator Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Master 0', pch = 16,
     xlim = c(0,10000), ylim = c(0,1), col = pltcols[1])
par(new=TRUE)
plot(Metrics0_MMO$NFE, Metrics0_MMO$DE, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1), col = pltcols[2])
par(new=TRUE)
plot(Metrics0_MMO$NFE, Metrics0_MMO$PCX, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1), col = pltcols[3])
par(new=TRUE)
plot(Metrics0_MMO$NFE, Metrics0_MMO$SPX, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1), col = pltcols[4])
par(new=TRUE)
plot(Metrics0_MMO$NFE, Metrics0_MMO$UNDX, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1), col = pltcols[5])
par(new=TRUE)
plot(Metrics0_MMO$NFE, Metrics0_MMO$UM, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1), col = pltcols[6])

plot(Metrics1_MMO$NFE, Metrics1_MMO$SBX, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Operator Prob', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Master 1', pch = 16,
     xlim = c(0,10000), ylim = c(0,1), col = pltcols[1])
par(new=TRUE)
plot(Metrics1_MMO$NFE, Metrics1_MMO$DE, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1), col = pltcols[2])
par(new=TRUE)
plot(Metrics1_MMO$NFE, Metrics1_MMO$PCX, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1), col = pltcols[3])
par(new=TRUE)
plot(Metrics1_MMO$NFE, Metrics1_MMO$SPX, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1), col = pltcols[4])
par(new=TRUE)
plot(Metrics1_MMO$NFE, Metrics1_MMO$UNDX, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1), col = pltcols[5])
par(new=TRUE)
plot(Metrics1_MMO$NFE, Metrics1_MMO$UM, type = 'l', lwd = 2,
     xlab = '', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = '', pch = 16, axes = FALSE,
     xlim = c(0,10000), ylim = c(0,1), col = pltcols[6])
legend('topleft', legend = colnames(Metrics0_MMO)[3:8], col=pltcols, pch=NA, lty = 1, lwd = 2)
dev.off()

# Plot convergence metrics----
png('ParetoMMO_HypervolumevsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(c(Metrics0_MMO$NFE,9800), ConvMetrics_MMO$Hypervolume, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Hypervolume', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MMO', pch = 16,
     xlim = c(0,10000), ylim = c(0,1))
dev.off()

png('ParetoMMO_GDvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(c(Metrics0_MMO$NFE,9800), ConvMetrics_MMO$GenerationalDistance, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Generational Distance', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MMO', pch = 16,
     xlim = c(0,10000))
dev.off()

png('ParetoMMO_InvGDvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(c(Metrics0_MMO$NFE,9800), ConvMetrics_MMO$InvertedGenerationalDistance, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Inverted Generational Distance', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MMO', pch = 16,
     xlim = c(0,10000))
dev.off()

png('ParetoMMO_SpacingvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(c(Metrics0_MMO$NFE,9800), ConvMetrics_MMO$Spacing, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Spacing', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MMO', pch = 16,
     xlim = c(0,10000))
dev.off()

png('ParetoMMO_EIvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(c(Metrics0_MMO$NFE,9800), ConvMetrics_MMO$EpsilonIndicator, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Epsilon Indicator', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MMO', pch = 16,
     xlim = c(0,10000))
dev.off()

png('ParetoMMO_MaxErrvsNFE.png', res = 300, units = 'in', width = 6, height = 6)
plot(c(Metrics0_MMO$NFE,9800), ConvMetrics_MMO$MaximumParetoFrontError, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = 'Max Pareto Front Error', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Reforestation Optimization to MMO', pch = 16,
     xlim = c(0,10000))
dev.off()

#  All convergence metrics----
png('ParetoMMO_ConvMetricsvsNFE.png', res = 300, units = 'in', width = 7, height = 6)
layout(rbind(c(1,2,3), c(4,5,6)))
plot(c(Metrics0_MMO$NFE, 9800), ConvMetrics_MMO$Hypervolume, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Hypervolume', pch = 16,
     xlim = c(0,10000), ylim = c(0,1))
plot(c(Metrics0_MMO$NFE, 9800), ConvMetrics_MMO$GenerationalDistance, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Generational Distance', pch = 16,
     xlim = c(0,10000))
plot(c(Metrics0_MMO$NFE, 9800), ConvMetrics_MMO$InvertedGenerationalDistance, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Inverted GD', pch = 16,
     xlim = c(0,10000))
plot(c(Metrics0_MMO$NFE, 9800), ConvMetrics_MMO$Spacing, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Spacing', pch = 16,
     xlim = c(0,10000))
plot(c(Metrics0_MMO$NFE, 9800), ConvMetrics_MMO$EpsilonIndicator, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Epsilon Indicator', pch = 16,
     xlim = c(0,10000))
plot(c(Metrics0_MMO$NFE, 9800), ConvMetrics_MMO$MaximumParetoFrontError, type = 'l', lwd = 2,
     xlab = 'NFE', ylab = '', cex.axis = 1.5, cex.lab = 1.5, 
     main = 'Max Pareto Front Error', pch = 16,
     xlim = c(0,10000))
dev.off()

#Plot number of trees in the thresholds----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12")
colors_plt = scico(3, palette = 'hawaii')
png('NumTreesTargetThresh.png', res = 300, units = 'in', width = 6, height = 6)
hist(TreesThreshSyn$NumTrees, col = 'black', border = 'black', breaks = seq(3500,10500,1000), ylim = c(0,10), ylab='Number of Compromise Solutions', xlab = 'Number of Trees', main='', cex.lab=1.5,cex.axis=1.5)
par(lwd=3)
hist(TreesThreshMAP$NumTrees, border = alpha.col(colors_plt[1],alpha = 0.5), breaks = seq(3500,10500,1000), add = TRUE)
hist(TreesThreshMORO$NumTrees, border = alpha.col(colors_plt[2], alpha = 0.5), breaks = seq(3500,10500,1000), add = TRUE, lwd = 2)
hist(TreesThreshMMO$NumTrees, border = alpha.col(colors_plt[3], alpha = 1), breaks = seq(3500,10500,1000), add = TRUE, lwd = 2)
legend('topright', legend = c('Synthetic', 'MAP', 'MORO', 'MinMax'), col = c('black', alpha.col(colors_plt[1:2], alpha = 0.5), colors_plt[3]), pch = 15)
dev.off()

pdf('NumTreesTargetThresh.pdf', width = 6, height = 6)
hist(TreesThreshSyn$NumTrees, col = 'black', border = 'black', breaks = seq(3500,10500,1000), ylim = c(0,10), ylab='Number of Compromise Solutions', xlab = 'Number of Trees', main = '', cex.lab=1.5,cex.axis=1.5)
par(lwd=3)
hist(TreesThreshMAP$NumTrees, border = alpha.col(colors_plt[1],alpha = 0.5), breaks = seq(3500,10500,1000), add = TRUE)
hist(TreesThreshMORO$NumTrees, border = alpha.col(colors_plt[2], alpha = 0.5), breaks = seq(3500,10500,1000), add = TRUE, lwd = 2)
hist(TreesThreshMMO$NumTrees, border = alpha.col(colors_plt[3], alpha = 1), breaks = seq(3500,10500,1000), add = TRUE, lwd = 2)
legend('topright', legend = c('Synthetic', 'MAP', 'MORO', 'MinMax'), col = c('black', alpha.col(colors_plt[1:2], alpha = 0.5), colors_plt[3]), pch = 15)
dev.off()
par(lwd=1)

#Reevaluation - all optimizations ----
png('ReEvalObjChangeAbsPct_AllOpts.png', res=200, width = 10, height = 5, units='in')
layout(rbind(c(1,2)))
#Flooding
hist(abs(DVO_MAPSyn$Flooding - DVO_MAPSyn$FloodingSyn)*100, breaks = seq(0,0.1,0.015)*100, xlab = 'Absolute Change from Synthetic (%)', main = 'Flooding Objective', cex.lab=1.5, cex.axis=1.5, 
     col = alpha.col(colors_plt[1],alpha = 0.5), border = 'gray', ylim = c(0,100))
hist(abs(DVO_MOROSyn$Flooding - DVO_MOROSyn$FloodingSyn)*100, breaks = seq(0,0.1,0.015)*100, 
     col = alpha.col(colors_plt[2], alpha = 0.5), border = 'gray', add = TRUE)
hist(abs(DVO_MMOSyn$Flooding - DVO_MMOSyn$FloodingSyn)*100, breaks = seq(0,0.1,0.015)*100, 
     col = alpha.col(colors_plt[3], alpha = 0.5), border = 'gray', add = TRUE)
#Low Flow
hist(abs(DVO_MAPSyn$LowFlow - DVO_MAPSyn$LowFlowSyn)*100, breaks = seq(0,0.4,0.015)*100, xlab = 'Absolute Change from Synthetic (%)', main = 'Low Flow Objective', cex.lab=1.5, cex.axis=1.5, 
     col = alpha.col(colors_plt[1],alpha = 0.5), border = 'gray', ylim = c(0,50), ylab = '')
hist(abs(DVO_MOROSyn$LowFlow - DVO_MOROSyn$LowFlowSyn)*100, breaks = seq(0,0.4,0.015)*100, 
     col = alpha.col(colors_plt[2], alpha = 0.5), border = 'gray', add = TRUE)
hist(abs(DVO_MMOSyn$LowFlow - DVO_MMOSyn$LowFlowSyn)*100, breaks = seq(0,0.4,0.015)*100, 
     col = alpha.col(colors_plt[3], alpha = 0.5), border = 'gray', add = TRUE)
legend('topright', legend = c('MAP', 'MORO', 'MinMax'), col = alpha.col(colors_plt, alpha = 0.5), pch = 15)
dev.off()

png('ReEvalObjChangePct_AllOpts.png', res=200, width = 10, height = 5, units='in')
layout(rbind(c(1,2)))
#Flooding
hist((-DVO_MAPSyn$Flooding - -DVO_MAPSyn$FloodingSyn)*100, breaks = seq(-0.075,0.075,0.015)*100, xlab = 'Change from Synthetic (%)', main = 'Flooding Objective', cex.lab=1.5, cex.axis=1.5, 
     col = alpha.col(colors_plt[1],alpha = 0.5), border = 'gray', ylim = c(0,100))
hist((-DVO_MOROSyn$Flooding - -DVO_MOROSyn$FloodingSyn)*100, breaks = seq(-0.075,0.075,0.015)*100, 
     col = alpha.col(colors_plt[2], alpha = 0.5), border = 'gray', add = TRUE)
hist((-DVO_MMOSyn$Flooding - -DVO_MMOSyn$FloodingSyn)*100, breaks = seq(-0.075,0.075,0.015)*100, 
     col = alpha.col(colors_plt[3], alpha = 0.5), border = 'gray', add = TRUE)
#Low Flow
hist((-DVO_MAPSyn$LowFlow - -DVO_MAPSyn$LowFlowSyn)*100, breaks = seq(-0.405,0.405,0.015)*100, xlab = 'Change from Synthetic (%)', main = 'Low Flow Objective', cex.lab=1.5, cex.axis=1.5, 
     col = alpha.col(colors_plt[1],alpha = 0.5), border = 'gray', ylim = c(0,50), ylab = '')
hist((-DVO_MOROSyn$LowFlow - -DVO_MOROSyn$LowFlowSyn)*100, breaks = seq(-0.405,0.405,0.015)*100, 
     col = alpha.col(colors_plt[2], alpha = 0.5), border = 'gray', add = TRUE)
hist((-DVO_MMOSyn$LowFlow - -DVO_MMOSyn$LowFlowSyn)*100, breaks = seq(-0.405,0.405,0.015)*100, 
     col = alpha.col(colors_plt[3], alpha = 0.5), border = 'gray', add = TRUE)
legend('topright', legend = c('MAP', 'MORO', 'MinMax'), col = alpha.col(colors_plt, alpha = 0.5), pch = 15)
dev.off()

pdf('ReEvalObjChangePct_AllOpts.pdf', width = 10, height = 5)
layout(rbind(c(1,2)))
#Flooding
hist((-DVO_MAPSyn$Flooding - -DVO_MAPSyn$FloodingSyn)*100, breaks = seq(-0.075,0.075,0.015)*100, xlab = 'Change from Synthetic (%)', main = 'Flooding Objective', cex.lab=1.5, cex.axis=1.5, 
     col = alpha.col(colors_plt[1],alpha = 0.5), border = 'gray', ylim = c(0,100))
hist((-DVO_MOROSyn$Flooding - -DVO_MOROSyn$FloodingSyn)*100, breaks = seq(-0.075,0.075,0.015)*100, 
     col = alpha.col(colors_plt[2], alpha = 0.5), border = 'gray', add = TRUE)
hist((-DVO_MMOSyn$Flooding - -DVO_MMOSyn$FloodingSyn)*100, breaks = seq(-0.075,0.075,0.015)*100, 
     col = alpha.col(colors_plt[3], alpha = 0.5), border = 'gray', add = TRUE)
#Low Flow
hist((-DVO_MAPSyn$LowFlow - -DVO_MAPSyn$LowFlowSyn)*100, breaks = seq(-0.405,0.405,0.015)*100, xlab = 'Change from Synthetic (%)', main = 'Low Flow Objective', cex.lab=1.5, cex.axis=1.5, 
     col = alpha.col(colors_plt[1],alpha = 0.5), border = 'gray', ylim = c(0,50), ylab = '')
hist((-DVO_MOROSyn$LowFlow - -DVO_MOROSyn$LowFlowSyn)*100, breaks = seq(-0.405,0.405,0.015)*100, 
     col = alpha.col(colors_plt[2], alpha = 0.5), border = 'gray', add = TRUE)
hist((-DVO_MMOSyn$LowFlow - -DVO_MMOSyn$LowFlowSyn)*100, breaks = seq(-0.405,0.405,0.015)*100, 
     col = alpha.col(colors_plt[3], alpha = 0.5), border = 'gray', add = TRUE)
legend('topright', legend = c('MAP', 'MORO', 'MinMax'), col = alpha.col(colors_plt, alpha = 0.5), pch = 15)
dev.off()



#Decision variables vs. objectives----
png('DecisionVarsVsObjectives_AllOpts.png', res=600, width = 6, height = 12, units='in')
layout(rbind(c(1,7,13), c(2,8,14), c(3,9,15), c(4,10,16), c(5,11,17), c(6,12,18)))
#Flooding
for(i in 1:6){
  #rearrange plotting order
  if(i == 2){
    i = 4
  }else if (i == 3){
    i = 2
  }else if (i == 4){
    i = 5
  }else if (i == 5){
    i = 3
  }
  hillslope_num = strsplit(colnames(DVO)[i], split = 'd|u|m')[[1]]
  location = strsplit(colnames(DVO)[i], split = hillslope_num)[[1]][2]
  if(location == 'd'){
    loc_name = 'Downslope'
  }else if(location == 'm'){
    loc_name = 'Midslope'
  }else if(location == 'u'){
    loc_name = 'Upslope'
  }
  if(i == 6){
          xlab_i = 'Flooding Reduction'
          par(mar = c(4, 4, 0.5, 0.5))
  }else{
          xlab_i = ''
          par(mar = c(1, 4, 0.5, 0.5))
  }
  scatter.smooth(-DVO$Flooding*100, DVO[,i], pch = NA, lpars = list(lwd=2), ylim = c(0,1), xlim = c(0,60),
               xlab = xlab_i, ylab = paste(hillslope_num, loc_name, 'Proportion'), axes = FALSE, cex.lab = 1.5)
  box()
  axis(side = 2, at = seq(0,1,0.2), labels = seq(0,1,0.2))
  if (i == 6){
          axis(side = 1, at = seq(0,60,10), labels = seq(0,60,10))
  }else{
          axis(side = 1, at = seq(0,60,10), labels = NA)
  }
  par(new=TRUE)
  scatter.smooth(-DVO_MAPSyn$FloodingSyn*100, DVO_MAPSyn[,i], pch = NA, lpars = list(lwd=2, col='red'), 
               ylim = c(0,1), xlim = c(0,60), xlab = '', ylab = '', axes=FALSE)
  par(new=TRUE)
  scatter.smooth(-DVO_MOROSyn$FloodingSyn*100, DVO_MOROSyn[,i], pch = NA, lpars = list(lwd=2, col='blue'), 
               ylim = c(0,1), xlim = c(0,60), xlab = '', ylab = '', axes=FALSE)
  par(new=TRUE)
  scatter.smooth(-DVO_MMOSyn$FloodingSyn*100, DVO_MMOSyn[,i], pch = NA, lpars = list(lwd=2, col='green'), 
               ylim = c(0,1), xlim = c(0,60), xlab = '', ylab = '', axes=FALSE)
  if(i==1){
    legend('topleft', legend = c('Synthetic', 'MAP', 'MORO', 'MinMax'), 
           col = c('black', alpha.col('red',0.5), alpha.col('blue',0.5), alpha.col('green',0.5)), 
           pch = NA, lty = 1)
  }
}
#Low Flow
for(i in 1:6){
        #rearrange plotting order
        if(i == 2){
                i = 4
        }else if (i == 3){
                i = 2
        }else if (i == 4){
                i = 5
        }else if (i == 5){
                i = 3
        }
        hillslope_num = strsplit(colnames(DVO)[i], split = 'd|u|m')[[1]]
        location = strsplit(colnames(DVO)[i], split = hillslope_num)[[1]][2]
        if(location == 'd'){
                loc_name = 'Downslope'
        }else if(location == 'm'){
                loc_name = 'Midslope'
        }else if(location == 'u'){
                loc_name = 'Upslope'
        }
        if(i == 6){
          xlab_i = 'Low Flow Change'
          par(mar = c(4, 4, 0.5, 0.5))
        }else{
          xlab_i = ''
          par(mar = c(1, 4, 0.5, 0.5))
        }
        scatter.smooth(-DVO$LowFlow*100, DVO[,i], pch = NA, lpars = list(lwd=2), ylim = c(0,1), xlim = c(-100,10),
                       xlab = xlab_i, ylab = '', axes = FALSE, cex.lab = 1.5)
        box()
        axis(side = 2, at = seq(0,1,0.2), labels = seq(0,1,0.2))
        if (i == 6){
                axis(side = 1, at = seq(-100,0,20), labels = seq(-100,0,20))
        }else{
                axis(side = 1, at = seq(-100,0,20), labels = NA)
        }
        par(new=TRUE)
        scatter.smooth(-DVO_MAPSyn$LowFlowSyn*100, DVO_MAPSyn[,i], pch = NA, lpars = list(lwd=2, col='red'), 
                       ylim = c(0,1), xlim = c(-100,10), xlab = '', ylab = '', axes=FALSE)
        par(new=TRUE)
        scatter.smooth(-DVO_MOROSyn$LowFlowSyn*100, DVO_MOROSyn[,i], pch = NA, lpars = list(lwd=2, col='blue'), 
                       ylim = c(0,1), xlim = c(-100,10), xlab = '', ylab = '', axes=FALSE)
        par(new=TRUE)
        scatter.smooth(-DVO_MMOSyn$LowFlowSyn*100, DVO_MMOSyn[,i], pch = NA, lpars = list(lwd=2, col='green'), 
                       ylim = c(0,1), xlim = c(-100,10), xlab = '', ylab = '', axes=FALSE)
}
#NumTrees
for(i in 1:6){
        #rearrange plotting order
        if(i == 2){
                i = 4
        }else if (i == 3){
                i = 2
        }else if (i == 4){
                i = 5
        }else if (i == 5){
                i = 3
        }
        hillslope_num = strsplit(colnames(DVO)[i], split = 'd|u|m')[[1]]
        location = strsplit(colnames(DVO)[i], split = hillslope_num)[[1]][2]
        if(location == 'd'){
                loc_name = 'Downslope'
        }else if(location == 'm'){
                loc_name = 'Midslope'
        }else if(location == 'u'){
                loc_name = 'Upslope'
        }
        if(i == 6){
                xlab_i = 'Number of Trees'
                par(mar = c(4, 4, 0.5, 0.5))
        }else{
                xlab_i = ''
                par(mar = c(1, 4, 0.5, 0.5))
        }
        scatter.smooth(DVO$NumTrees, DVO[,i], pch = NA, lpars = list(lwd=2), ylim = c(0,1), xlim = c(0,18000),
                       xlab = xlab_i, ylab = '', axes= FALSE, cex.lab = 1.5)
        box()
        axis(side = 2, at = seq(0,1,0.2), labels = seq(0,1,0.2))
        if (i == 6){
          axis(side = 1, at = seq(0,20000,5000), labels = seq(0,20000,5000))
        }else{
          axis(side = 1, at = seq(0,20000,5000), labels = NA)
        }
        par(new=TRUE)
        scatter.smooth(DVO_MAPSyn$NumTreesSyn, DVO_MAPSyn[,i], pch = NA, lpars = list(lwd=2, col='red'), 
                       ylim = c(0,1), xlim = c(0,18000), xlab = '', ylab = '', axes=FALSE)
        par(new=TRUE)
        scatter.smooth(DVO_MOROSyn$NumTreesSyn, DVO_MOROSyn[,i], pch = NA, lpars = list(lwd=2, col='blue'), 
                       ylim = c(0,1), xlim = c(0,18000), xlab = '', ylab = '', axes=FALSE)
        par(new=TRUE)
        scatter.smooth(DVO_MMOSyn$NumTreesSyn, DVO_MMOSyn[,i], pch = NA, lpars = list(lwd=2, col='green'), 
                       ylim = c(0,1), xlim = c(0,18000), xlab = '', ylab = '', axes=FALSE)
        
}
dev.off()

# combining H9 and H10----
png('DecisionVarsVsObjectives_AllOpts_AggHillslopes_NoTrees.png', res=600, width = 6, height = 4, units='in')
layout(rbind(c(1,2,3), c(4,5,6)))
span = 0.3
#Flooding
for(i in 1:3){
        #get plotting variable
        hillslope_num = strsplit(colnames(DVO)[i], split = 'd|u|m')[[1]]
        location = strsplit(colnames(DVO)[i], split = hillslope_num)[[1]][2]
        if(location == 'd'){
                loc_name = 'Downslope'
        }else if(location == 'm'){
                loc_name = 'Midslope'
        }else if(location == 'u'){
                loc_name = 'Upslope'
        }
        if(i == 1){
                ylab_i = 'Flooding Reduction'
                par(mar = c(2.5, 4.1, 0.5, 0.75))
        }else{
                ylab_i = ''
                par(mar = c(2.5, 2.1, 0.5, 0.75))
        }
        scatter.smooth(y = -DVO$Flooding*100, x = rowSums(DVO[,c(i, i+3)]), pch = NA, lpars = list(lwd=2), xlim = c(0,2), ylim = c(0,60),
                       ylab = ylab_i, xlab = '', axes = FALSE, cex.lab = 1.1, span = span)
        box()
        axis(side = 1, at = seq(0,2,0.5), labels = NA)
        if (i == 1){
                axis(side = 2, at = seq(0,60,10), labels = seq(0,60,10))
        }else{
                axis(side = 2, at = seq(0,60,10), labels = NA)
        }
        par(new=TRUE)
        scatter.smooth(y = -DVO_MAPSyn$FloodingSyn*100, x = rowSums(DVO_MAPSyn[,c(i, i+3)]), pch = NA, lpars = list(lwd=2, col=colors_plt[1]), 
                       xlim = c(0,2), ylim = c(0,60), xlab = '', ylab = '', axes=FALSE, span = span)
        par(new=TRUE)
        scatter.smooth(y = -DVO_MOROSyn$FloodingSyn*100, x = rowSums(DVO_MOROSyn[,c(i, i+3)]), pch = NA, lpars = list(lwd=2, col=colors_plt[2]), 
                       xlim = c(0,2), ylim = c(0,60), xlab = '', ylab = '', axes=FALSE, span = span)
        par(new=TRUE)
        scatter.smooth(y = -DVO_MMOSyn$FloodingSyn*100, x = rowSums(DVO_MMOSyn[,c(i, i+3)]), pch = NA, lpars = list(lwd=2, col=colors_plt[3]), 
                       xlim = c(0,2), ylim = c(0,60), xlab = '', ylab = '', axes=FALSE, span = span)
        if(i==1){
                legend('bottomright', legend = c('Synthetic', 'MAP', 'MORO', 'MinMax'), 
                       col = c('black', colors_plt), 
                       pch = NA, lty = 1)
        }
}
#Low Flow
for(i in 1:3){
        hillslope_num = strsplit(colnames(DVO)[i], split = 'd|u|m')[[1]]
        location = strsplit(colnames(DVO)[i], split = hillslope_num)[[1]][2]
        if(location == 'd'){
                loc_name = 'Downslope'
        }else if(location == 'm'){
                loc_name = 'Midslope'
        }else if(location == 'u'){
                loc_name = 'Upslope'
        }
        if(i == 1){
                ylab_i = 'Low Flow Change'
                par(mar = c(4, 4.1, 0.5, 0.75))
        }else{
                xlab_i = ''
                par(mar = c(4, 2.1, 0.5, 0.75))
        }
        scatter.smooth(y = -DVO$LowFlow*100, x = rowSums(DVO[,c(i, i+3)]), pch = NA, lpars = list(lwd=2), xlim = c(0,2), ylim = c(-100,10),
                       ylab = ylab_i, xlab = paste('Sum', loc_name, 'Proportion'), axes = FALSE, cex.lab = 1.1, span = span)
        box()
        axis(side = 1, at = seq(0,2,0.5), labels = seq(0,2,0.5))
        if (i == 1){
                axis(side = 2, at = seq(-100,0,20), labels = seq(-100,0,20))
        }else{
                axis(side = 2, at = seq(-100,0,20), labels = NA)
        }
        par(new=TRUE)
        scatter.smooth(y = -DVO_MAPSyn$LowFlowSyn*100, x = rowSums(DVO_MAPSyn[,c(i, i+3)]), pch = NA, lpars = list(lwd=2, col=colors_plt[1]), 
                       xlim = c(0,2), ylim = c(-100,10), xlab = '', ylab = '', axes=FALSE, span = span)
        par(new=TRUE)
        scatter.smooth(y = -DVO_MOROSyn$LowFlowSyn*100, x = rowSums(DVO_MOROSyn[,c(i, i+3)]), pch = NA, lpars = list(lwd=2, col=colors_plt[2]), 
                       xlim = c(0,2), ylim = c(-100,10), xlab = '', ylab = '', axes=FALSE, span = span)
        par(new=TRUE)
        scatter.smooth(y = -DVO_MMOSyn$LowFlowSyn*100, x = rowSums(DVO_MMOSyn[,c(i, i+3)]), pch = NA, lpars = list(lwd=2, col=colors_plt[3]), 
                       xlim = c(0,2), ylim = c(-100,10), xlab = '', ylab = '', axes=FALSE, span = span)
}
# #NumTrees
# for(i in 1:3){
#         hillslope_num = strsplit(colnames(DVO)[i], split = 'd|u|m')[[1]]
#         location = strsplit(colnames(DVO)[i], split = hillslope_num)[[1]][2]
#         if(location == 'd'){
#                 loc_name = 'Downslope'
#         }else if(location == 'm'){
#                 loc_name = 'Midslope'
#         }else if(location == 'u'){
#                 loc_name = 'Upslope'
#         }
#         if(i == 3){
#                 xlab_i = 'Number of Trees'
#                 par(mar = c(4, 4.1, 0.5, 0.5))
#         }else{
#                 xlab_i = ''
#                 par(mar = c(2.5, 4.1, 2, 0.5))
#                 if(i == 1){
#                         par(mar = c(1, 4.1, 3.5, 0.5))
#                 }
#         }
#         scatter.smooth(DVO$NumTrees, rowSums(DVO[,c(i, i+3)]), pch = NA, lpars = list(lwd=2), ylim = c(0,2), xlim = c(0,18000),
#                        xlab = xlab_i, ylab = '', axes= FALSE, cex.lab = 1.5)
#         box()
#         axis(side = 2, at = seq(0,2,0.5), labels = NA)
#         if (i == 3){
#                 axis(side = 1, at = seq(0,20000,5000), labels = seq(0,20000,5000))
#         }else{
#                 axis(side = 1, at = seq(0,20000,5000), labels = NA)
#         }
#         par(new=TRUE)
#         scatter.smooth(DVO_MAPSyn$NumTreesSyn, rowSums(DVO_MAPSyn[,c(i, i+3)]), pch = NA, lpars = list(lwd=2, col=colors_plt[1]), 
#                        ylim = c(0,2), xlim = c(0,18000), xlab = '', ylab = '', axes=FALSE)
#         par(new=TRUE)
#         scatter.smooth(DVO_MOROSyn$NumTreesSyn, rowSums(DVO_MOROSyn[,c(i, i+3)]), pch = NA, lpars = list(lwd=2, col=colors_plt[2]), 
#                        ylim = c(0,2), xlim = c(0,18000), xlab = '', ylab = '', axes=FALSE)
#         par(new=TRUE)
#         scatter.smooth(DVO_MMOSyn$NumTreesSyn, rowSums(DVO_MMOSyn[,c(i, i+3)]), pch = NA, lpars = list(lwd=2, col=colors_plt[3]), 
#                        ylim = c(0,2), xlim = c(0,18000), xlab = '', ylab = '', axes=FALSE)
#         
# }
dev.off()

# pdf----
pdf('DecisionVarsVsObjectives_AllOpts_AggHillslopes.pdf', width = 6, height = 6)
layout(rbind(c(1,4,7), c(2,5,8), c(3,6,9)))
#Flooding
for(i in 1:3){
        #get plotting variable
        hillslope_num = strsplit(colnames(DVO)[i], split = 'd|u|m')[[1]]
        location = strsplit(colnames(DVO)[i], split = hillslope_num)[[1]][2]
        if(location == 'd'){
                loc_name = 'Downslope'
        }else if(location == 'm'){
                loc_name = 'Midslope'
        }else if(location == 'u'){
                loc_name = 'Upslope'
        }
        if(i == 3){
                xlab_i = 'Flooding Reduction'
                par(mar = c(4, 4.1, 0.5, 0.5))
        }else{
                xlab_i = ''
                par(mar = c(2.5, 4.1, 2, 0.5))
                if(i == 1){
                        par(mar = c(1, 4.1, 3.5, 0.5))
                }
        }
        scatter.smooth(-DVO$Flooding*100, rowSums(DVO[,c(i, i+3)]), pch = NA, lpars = list(lwd=2), ylim = c(0,2), xlim = c(0,60),
                       xlab = xlab_i, ylab = paste('Sum', loc_name, 'Proportion'), axes = FALSE, cex.lab = 1.2)
        box()
        axis(side = 2, at = seq(0,2,0.5), labels = seq(0,2,0.5))
        if (i == 3){
                axis(side = 1, at = seq(0,60,10), labels = seq(0,60,10))
        }else{
                axis(side = 1, at = seq(0,60,10), labels = NA)
        }
        par(new=TRUE)
        scatter.smooth(-DVO_MAPSyn$FloodingSyn*100, rowSums(DVO_MAPSyn[,c(i, i+3)]), pch = NA, lpars = list(lwd=2, col=colors_plt[1]), 
                       ylim = c(0,2), xlim = c(0,60), xlab = '', ylab = '', axes=FALSE)
        par(new=TRUE)
        scatter.smooth(-DVO_MOROSyn$FloodingSyn*100, rowSums(DVO_MOROSyn[,c(i, i+3)]), pch = NA, lpars = list(lwd=2, col=colors_plt[2]), 
                       ylim = c(0,2), xlim = c(0,60), xlab = '', ylab = '', axes=FALSE)
        par(new=TRUE)
        scatter.smooth(-DVO_MMOSyn$FloodingSyn*100, rowSums(DVO_MMOSyn[,c(i, i+3)]), pch = NA, lpars = list(lwd=2, col=colors_plt[3]), 
                       ylim = c(0,2), xlim = c(0,60), xlab = '', ylab = '', axes=FALSE)
        if(i==1){
                legend('topleft', legend = c('Synthetic', 'MAP', 'MORO', 'MinMax'), 
                       col = c('black', colors_plt), 
                       pch = NA, lty = 1)
        }
}
#Low Flow
for(i in 1:3){
        hillslope_num = strsplit(colnames(DVO)[i], split = 'd|u|m')[[1]]
        location = strsplit(colnames(DVO)[i], split = hillslope_num)[[1]][2]
        if(location == 'd'){
                loc_name = 'Downslope'
        }else if(location == 'm'){
                loc_name = 'Midslope'
        }else if(location == 'u'){
                loc_name = 'Upslope'
        }
        if(i == 3){
                xlab_i = 'Low Flow Change'
                par(mar = c(4, 4.1, 0.5, 0.5))
        }else{
                xlab_i = ''
                par(mar = c(2.5, 4.1, 2, 0.5))
                if(i == 1){
                        par(mar = c(1, 4.1, 3.5, 0.5))
                }
        }
        scatter.smooth(-DVO$LowFlow*100, rowSums(DVO[,c(i, i+3)]), pch = NA, lpars = list(lwd=2), ylim = c(0,2), xlim = c(-100,10),
                       xlab = xlab_i, ylab = '', axes = FALSE, cex.lab = 1.5)
        box()
        axis(side = 2, at = seq(0,2,0.5), labels = NA)
        if (i == 3){
                axis(side = 1, at = seq(-100,0,20), labels = seq(-100,0,20))
        }else{
                axis(side = 1, at = seq(-100,0,20), labels = NA)
        }
        par(new=TRUE)
        scatter.smooth(-DVO_MAPSyn$LowFlowSyn*100, rowSums(DVO_MAPSyn[,c(i, i+3)]), pch = NA, lpars = list(lwd=2, col=colors_plt[1]), 
                       ylim = c(0,2), xlim = c(-100,10), xlab = '', ylab = '', axes=FALSE)
        par(new=TRUE)
        scatter.smooth(-DVO_MOROSyn$LowFlowSyn*100, rowSums(DVO_MOROSyn[,c(i, i+3)]), pch = NA, lpars = list(lwd=2, col=colors_plt[2]), 
                       ylim = c(0,2), xlim = c(-100,10), xlab = '', ylab = '', axes=FALSE)
        par(new=TRUE)
        scatter.smooth(-DVO_MMOSyn$LowFlowSyn*100, rowSums(DVO_MMOSyn[,c(i, i+3)]), pch = NA, lpars = list(lwd=2, col=colors_plt[3]), 
                       ylim = c(0,2), xlim = c(-100,10), xlab = '', ylab = '', axes=FALSE)
}
#NumTrees
for(i in 1:3){
        hillslope_num = strsplit(colnames(DVO)[i], split = 'd|u|m')[[1]]
        location = strsplit(colnames(DVO)[i], split = hillslope_num)[[1]][2]
        if(location == 'd'){
                loc_name = 'Downslope'
        }else if(location == 'm'){
                loc_name = 'Midslope'
        }else if(location == 'u'){
                loc_name = 'Upslope'
        }
        if(i == 3){
                xlab_i = 'Number of Trees'
                par(mar = c(4, 4.1, 0.5, 0.5))
        }else{
                xlab_i = ''
                par(mar = c(2.5, 4.1, 2, 0.5))
                if(i == 1){
                        par(mar = c(1, 4.1, 3.5, 0.5))
                }
        }
        scatter.smooth(DVO$NumTrees, rowSums(DVO[,c(i, i+3)]), pch = NA, lpars = list(lwd=2), ylim = c(0,2), xlim = c(0,18000),
                       xlab = xlab_i, ylab = '', axes= FALSE, cex.lab = 1.5)
        box()
        axis(side = 2, at = seq(0,2,0.5), labels = NA)
        if (i == 3){
                axis(side = 1, at = seq(0,20000,5000), labels = seq(0,20000,5000))
        }else{
                axis(side = 1, at = seq(0,20000,5000), labels = NA)
        }
        par(new=TRUE)
        scatter.smooth(DVO_MAPSyn$NumTreesSyn, rowSums(DVO_MAPSyn[,c(i, i+3)]), pch = NA, lpars = list(lwd=2, col=colors_plt[1]), 
                       ylim = c(0,2), xlim = c(0,18000), xlab = '', ylab = '', axes=FALSE)
        par(new=TRUE)
        scatter.smooth(DVO_MOROSyn$NumTreesSyn, rowSums(DVO_MOROSyn[,c(i, i+3)]), pch = NA, lpars = list(lwd=2, col=colors_plt[2]), 
                       ylim = c(0,2), xlim = c(0,18000), xlab = '', ylab = '', axes=FALSE)
        par(new=TRUE)
        scatter.smooth(DVO_MMOSyn$NumTreesSyn, rowSums(DVO_MMOSyn[,c(i, i+3)]), pch = NA, lpars = list(lwd=2, col=colors_plt[3]), 
                       ylim = c(0,2), xlim = c(0,18000), xlab = '', ylab = '', axes=FALSE)
        
}
dev.off()


#All Pareto fronts----
scaleRange = c(0,18000)
scaleBy = 3000
Pal = rev(scico(palette = 'batlow', n = (scaleRange[2] - scaleRange[1])/scaleBy))

png('ParetoAll_ColTrees.png', res = 600, units = 'in', width = 12, height = 6)
layout(rbind(c(1,2,3,4), c(5,6,7,8)))
plot(-DVO$Flooding*100, -DVO$LowFlow*100, col = colFun(DVO$NumTrees),
     xlim = c(0, 60), ylim=c(-120, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Synthetic True Optimization', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-130,130))
lines(c(-110,110), c(-20,-20), col = 'gray', lty = 2)
lines(c(20,20), c(-130,120), col = 'gray', lty = 2)
#Add max GI point
par(new=TRUE)
plot(x = O1_b100*100, y = O2_b100*100, pch = 17, col = colFun(O3_b100),xlim = c(0, 60), ylim=c(-120, 10), axes=FALSE, xlab = '', ylab='')
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <3', '3 - <6', '6 - <9', '9 - <12', '12 - <15', '>15', 'Max GI'), col = colFun(c(seq(0,18000,3000), O3_b100)), pch = c(rep(16,6),17), cex = 0.7)
par(xpd=TRUE)
text(x = -15, y = 40, 'A', cex = 2)
par(xpd=FALSE)

plot(-DVO_MAP$Flooding*100, -DVO_MAP$LowFlow*100, col = colFun(DVO_MAP$NumTrees),
     xlim = c(0, 60), ylim=c(-120, 10), xlab = 'Flooding Reduction (%)', ylab = '', cex.axis = 1.5, cex.lab = 1.5, main = 'MAP Optimization', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-130,120))
lines(c(-110,110), c(-20,-20), col = 'gray', lty = 2)
lines(c(20,20), c(-130,120), col = 'gray', lty = 2)
par(xpd=TRUE)
text(x = -15, y = 40, 'B', cex = 2)
par(xpd=FALSE)

plot(-DVO_MORO$Flooding*100, -DVO_MORO$LowFlow*100, col = colFun(DVO_MORO$NumTrees),
     xlim = c(0, 60), ylim=c(-120, 10), xlab = 'Flooding Reduction (%)', ylab = '', cex.axis = 1.5, cex.lab = 1.5, main = 'MORO Optimization', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-130,120))
lines(c(-110,110), c(-20,-20), col = 'gray', lty = 2)
lines(c(20,20), c(-130,120), col = 'gray', lty = 2)
par(xpd=TRUE)
text(x = -15, y = 40, 'C', cex = 2)
par(xpd=FALSE)

plot(-DVO_MMO$Flooding*100, -DVO_MMO$LowFlow*100, col = colFun(DVO_MMO$NumTrees),
     xlim = c(0, 60), ylim=c(-120, 10), xlab = 'Flooding Reduction (%)', ylab = '', cex.axis = 1.5, cex.lab = 1.5, main = 'Min-Max Optimization', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-130,130))
lines(c(-110,110), c(-20,-20), col = 'gray', lty = 2)
lines(c(20,20), c(-130,120), col = 'gray', lty = 2)
par(xpd=TRUE)
text(x = -15, y = 40, 'D', cex = 2)
par(xpd=FALSE)

#Histogram
hist(TreesThreshSyn$NumTrees, col = 'black', border = 'black', breaks = seq(3500,10500,1000), ylim = c(0,10), ylab='Number of Compromise Solutions', xlab = 'Number of Trees', main='', cex.lab=1.5,cex.axis=1.5)
par(lwd=3)
hist(TreesThreshMAP$NumTrees, border = alpha.col(colors_plt[1],alpha = 1), breaks = seq(3500,10500,1000), add = TRUE)
par(lwd=2)
hist(TreesThreshMORO$NumTrees, border = alpha.col(colors_plt[2], alpha = 1), breaks = seq(3500,10500,1000), add = TRUE, lwd = 2)
hist(TreesThreshMMO$NumTrees, border = alpha.col(colors_plt[3], alpha = 1), breaks = seq(3500,10500,1000), add = TRUE, lwd = 2)
legend('topright', legend = c('Synthetic', 'MAP', 'MORO', 'MinMax'), col = c('black', alpha.col(colors_plt[1:2], alpha = 1), colors_plt[3]), pch = 15)
par(lwd=1)
par(xpd=TRUE)
text(x = 3000, y = 12.3, 'E', cex = 2)
par(xpd=FALSE)

#Reevaluation
plot(-DVO_MAPSyn$FloodingSyn*100, -DVO_MAPSyn$LowFlowSyn*100, col = colFun(DVO_MAPSyn$NumTreesSyn),
     xlim = c(0, 60), ylim=c(-120, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = '', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-130,120))
lines(c(-110,110), c(-20,-20), col = 'gray', lty = 2)
lines(c(20,20), c(-130,120), col = 'gray', lty = 2)
#Add max GI point
par(new=TRUE)
plot(x = O1_b100*100, y = O2_b100*100, pch = 17, col = colFun(O3_b100),xlim = c(0, 60), ylim=c(-120, 10), axes=FALSE, xlab = '', ylab='')
par(xpd=TRUE)
text(x = -15, y = 40, 'F', cex = 2)
par(xpd=FALSE)

plot(-DVO_MOROSyn$FloodingSyn*100, -DVO_MOROSyn$LowFlowSyn*100, col = colFun(DVO_MOROSyn$NumTreesSyn),
     xlim = c(0, 60), ylim=c(-120, 10), xlab = 'Flooding Reduction (%)', ylab = '', cex.axis = 1.5, cex.lab = 1.5, main = 'Synthetic Evaluation of Solutions', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-130,120))
lines(c(-110,110), c(-20,-20), col = 'gray', lty = 2)
lines(c(20,20), c(-130,120), col = 'gray', lty = 2)
#Add max GI point
par(new=TRUE)
plot(x = O1_b100*100, y = O2_b100*100, pch = 17, col = colFun(O3_b100),xlim = c(0, 60), ylim=c(-120, 10), axes=FALSE, xlab = '', ylab='')
par(xpd=TRUE)
text(x = -15, y = 40, 'G', cex = 2)
par(xpd=FALSE)

plot(-DVO_MMOSyn$FloodingSyn*100, -DVO_MMOSyn$LowFlowSyn*100, col = colFun(DVO_MMOSyn$NumTreesSyn),
     xlim = c(0, 60), ylim=c(-120, 10), xlab = 'Flooding Reduction (%)', ylab = '', cex.axis = 1.5, cex.lab = 1.5, main = '', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-130,130))
lines(c(-110,110), c(-20,-20), col = 'gray', lty = 2)
lines(c(20,20), c(-130,120), col = 'gray', lty = 2)
#Add max GI point
par(new=TRUE)
plot(x = O1_b100*100, y = O2_b100*100, pch = 17, col = colFun(O3_b100),xlim = c(0, 60), ylim=c(-120, 10), axes=FALSE, xlab = '', ylab='')
par(xpd=TRUE)
text(x = -15, y = 40, 'H', cex = 2)
par(xpd=FALSE)
dev.off()


pdf('ParetoAll_ColTrees.pdf', width = 12, height = 6)
layout(rbind(c(1,2,3,4), c(5,6,7,8)))
plot(-DVO$Flooding*100, -DVO$LowFlow*100, col = colFun(DVO$NumTrees),
     xlim = c(0, 60), ylim=c(-120, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Synthetic True Optimization', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-130,130))
lines(c(-110,110), c(-20,-20), col = 'gray', lty = 2)
lines(c(20,20), c(-130,120), col = 'gray', lty = 2)
#Add max GI point
par(new=TRUE)
plot(x = O1_b100*100, y = O2_b100*100, pch = 17, col = colFun(O3_b100),xlim = c(0, 60), ylim=c(-120, 10), axes=FALSE, xlab = '', ylab='')
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <3', '3 - <6', '6 - <9', '9 - <12', '12 - <15', '>15', 'Max GI'), col = colFun(c(seq(0,18000,3000), O3_b100)), pch = c(rep(16,6),17), cex = 0.7)
par(xpd=TRUE)
text(x = -15, y = 40, 'A', cex = 2)
par(xpd=FALSE)

plot(-DVO_MAP$Flooding*100, -DVO_MAP$LowFlow*100, col = colFun(DVO_MAP$NumTrees),
     xlim = c(0, 60), ylim=c(-120, 10), xlab = 'Flooding Reduction (%)', ylab = '', cex.axis = 1.5, cex.lab = 1.5, main = 'MAP Optimization', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-130,120))
lines(c(-110,110), c(-20,-20), col = 'gray', lty = 2)
lines(c(20,20), c(-130,120), col = 'gray', lty = 2)
par(xpd=TRUE)
text(x = -15, y = 40, 'B', cex = 2)
par(xpd=FALSE)

plot(-DVO_MORO$Flooding*100, -DVO_MORO$LowFlow*100, col = colFun(DVO_MORO$NumTrees),
     xlim = c(0, 60), ylim=c(-120, 10), xlab = 'Flooding Reduction (%)', ylab = '', cex.axis = 1.5, cex.lab = 1.5, main = 'MORO Optimization', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-130,120))
lines(c(-110,110), c(-20,-20), col = 'gray', lty = 2)
lines(c(20,20), c(-130,120), col = 'gray', lty = 2)
par(xpd=TRUE)
text(x = -15, y = 40, 'C', cex = 2)
par(xpd=FALSE)

plot(-DVO_MMO$Flooding*100, -DVO_MMO$LowFlow*100, col = colFun(DVO_MMO$NumTrees),
     xlim = c(0, 60), ylim=c(-120, 10), xlab = 'Flooding Reduction (%)', ylab = '', cex.axis = 1.5, cex.lab = 1.5, main = 'Min-Max Optimization', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-130,130))
lines(c(-110,110), c(-20,-20), col = 'gray', lty = 2)
lines(c(20,20), c(-130,120), col = 'gray', lty = 2)
par(xpd=TRUE)
text(x = -15, y = 40, 'D', cex = 2)
par(xpd=FALSE)

#Histogram
hist(TreesThreshSyn$NumTrees, col = 'black', border = 'black', breaks = seq(3500,10500,1000), ylim = c(0,10), ylab='Number of Compromise Solutions', xlab = 'Number of Trees', main='', cex.lab=1.5,cex.axis=1.5)
par(lwd=3)
hist(TreesThreshMAP$NumTrees, border = alpha.col(colors_plt[1],alpha = 1), breaks = seq(3500,10500,1000), add = TRUE)
par(lwd=2)
hist(TreesThreshMORO$NumTrees, border = alpha.col(colors_plt[2], alpha = 1), breaks = seq(3500,10500,1000), add = TRUE, lwd = 2)
hist(TreesThreshMMO$NumTrees, border = alpha.col(colors_plt[3], alpha = 1), breaks = seq(3500,10500,1000), add = TRUE, lwd = 2)
legend('topright', legend = c('Synthetic', 'MAP', 'MORO', 'MinMax'), col = c('black', alpha.col(colors_plt[1:2], alpha = 1), colors_plt[3]), pch = 15)
par(lwd=1)
par(xpd=TRUE)
text(x = 3000, y = 12.3, 'E', cex = 2)
par(xpd=FALSE)

#Reevaluation
plot(-DVO_MAPSyn$FloodingSyn*100, -DVO_MAPSyn$LowFlowSyn*100, col = colFun(DVO_MAPSyn$NumTreesSyn),
     xlim = c(0, 60), ylim=c(-120, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = '', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-130,120))
lines(c(-110,110), c(-20,-20), col = 'gray', lty = 2)
lines(c(20,20), c(-130,120), col = 'gray', lty = 2)
#Add max GI point
par(new=TRUE)
plot(x = O1_b100*100, y = O2_b100*100, pch = 17, col = colFun(O3_b100),xlim = c(0, 60), ylim=c(-120, 10), axes=FALSE, xlab = '', ylab='')
par(xpd=TRUE)
text(x = -15, y = 40, 'F', cex = 2)
par(xpd=FALSE)

plot(-DVO_MOROSyn$FloodingSyn*100, -DVO_MOROSyn$LowFlowSyn*100, col = colFun(DVO_MOROSyn$NumTreesSyn),
     xlim = c(0, 60), ylim=c(-120, 10), xlab = 'Flooding Reduction (%)', ylab = '', cex.axis = 1.5, cex.lab = 1.5, main = 'Synthetic Evaluation of Solutions', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-130,120))
lines(c(-110,110), c(-20,-20), col = 'gray', lty = 2)
lines(c(20,20), c(-130,120), col = 'gray', lty = 2)
#Add max GI point
par(new=TRUE)
plot(x = O1_b100*100, y = O2_b100*100, pch = 17, col = colFun(O3_b100),xlim = c(0, 60), ylim=c(-120, 10), axes=FALSE, xlab = '', ylab='')
par(xpd=TRUE)
text(x = -15, y = 40, 'G', cex = 2)
par(xpd=FALSE)

plot(-DVO_MMOSyn$FloodingSyn*100, -DVO_MMOSyn$LowFlowSyn*100, col = colFun(DVO_MMOSyn$NumTreesSyn),
     xlim = c(0, 60), ylim=c(-120, 10), xlab = 'Flooding Reduction (%)', ylab = '', cex.axis = 1.5, cex.lab = 1.5, main = '', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-130,130))
lines(c(-110,110), c(-20,-20), col = 'gray', lty = 2)
lines(c(20,20), c(-130,120), col = 'gray', lty = 2)
#Add max GI point
par(new=TRUE)
plot(x = O1_b100*100, y = O2_b100*100, pch = 17, col = colFun(O3_b100),xlim = c(0, 60), ylim=c(-120, 10), axes=FALSE, xlab = '', ylab='')
par(xpd=TRUE)
text(x = -15, y = 40, 'H', cex = 2)
par(xpd=FALSE)
dev.off()



#Average compromise decisions
png('MeanCompromiseDecisions.png', res = 600, width = 6, height = 6, units = 'in')
layout(rbind(c(1,2), c(3,4)))
scaleRange = c(0,0.8)
scaleBy = 0.2
Pal = rev(scico(palette = 'imola', begin = 0.4, n = (scaleRange[2] - scaleRange[1])/scaleBy))

par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(colMeans(TreesThreshSyn)[l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topright', title = expression(bold('Mean Proportion')), legend = c(paste(seq(0,0.6,0.2), '-', seq(0.2,0.8,0.2))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15, cex = 0.6)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.695, y = 39.479, 'WGS84')
text(x = -76.705, y = 39.4925, 'Synthetic\nCompromise', cex = 1.2)
box(which = 'figure', lwd = 2)
par(xpd=TRUE)
text(x = -76.716, y = 39.494, 'A', cex = 2)
par(xpd=FALSE)

plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(colMeans(TreesThreshMAP)[l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.695, y = 39.479, 'WGS84')
text(x = -76.705, y = 39.4925, 'MAP\nCompromise', cex = 1.2)
box(which = 'figure', lwd = 2)
par(xpd=TRUE)
text(x = -76.716, y = 39.494, 'B', cex = 2)
par(xpd=FALSE)

plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(colMeans(TreesThreshMORO)[l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.695, y = 39.479, 'WGS84')
text(x = -76.705, y = 39.4925, 'MORO\nCompromise', cex = 1.2)
box(which = 'figure', lwd = 2)
par(xpd=TRUE)
text(x = -76.716, y = 39.494, 'C', cex = 2)
par(xpd=FALSE)

plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(colMeans(TreesThreshMMO)[l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.695, y = 39.479, 'WGS84')
text(x = -76.705, y = 39.4925, 'Min-Max\nCompromise', cex = 1.2)
box(which = 'figure', lwd = 2)
par(xpd=TRUE)
text(x = -76.716, y = 39.494, 'D', cex = 2)
par(xpd=FALSE)
dev.off()

# pdf----
pdf('MeanCompromiseDecisions.pdf', width = 6, height = 6)
layout(rbind(c(1,2), c(3,4)))
scaleRange = c(0,0.8)
scaleBy = 0.2
Pal = scico(palette = 'imola', begin = 0.4, n = (scaleRange[2] - scaleRange[1])/scaleBy)

par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(colMeans(TreesThreshSyn)[l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topright', title = expression(bold('Mean Proportion')), legend = c(paste(seq(0,0.6,0.2), '-', seq(0.2,0.8,0.2))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15, cex = 0.6)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.695, y = 39.479, 'WGS84')
text(x = -76.705, y = 39.4925, 'Synthetic\nCompromise', cex = 1.2)
box(which = 'figure', lwd = 2)
par(xpd=TRUE)
text(x = -76.716, y = 39.494, 'A', cex = 2)
par(xpd=FALSE)

plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(colMeans(TreesThreshMAP)[l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.695, y = 39.479, 'WGS84')
text(x = -76.705, y = 39.4925, 'MAP\nCompromise', cex = 1.2)
box(which = 'figure', lwd = 2)
par(xpd=TRUE)
text(x = -76.716, y = 39.494, 'B', cex = 2)
par(xpd=FALSE)

plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(colMeans(TreesThreshMORO)[l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.695, y = 39.479, 'WGS84')
text(x = -76.705, y = 39.4925, 'MORO\nCompromise', cex = 1.2)
box(which = 'figure', lwd = 2)
par(xpd=TRUE)
text(x = -76.716, y = 39.494, 'C', cex = 2)
par(xpd=FALSE)

plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(colMeans(TreesThreshMMO)[l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.695, y = 39.479, 'WGS84')
text(x = -76.705, y = 39.4925, 'Min-Max\nCompromise', cex = 1.2)
box(which = 'figure', lwd = 2)
par(xpd=TRUE)
text(x = -76.716, y = 39.494, 'D', cex = 2)
par(xpd=FALSE)
dev.off()


#Select the 3 policies to compare for each optimization----
# Synthetic----
BestFloodSyn = DVO_IDs[DVO_IDs$Flooding == min(DVO_IDs$Flooding),]
BestLowFlowSyn = DVO_IDs[DVO_IDs$NumTrees >= 1000,][which(DVO_IDs$LowFlow[DVO_IDs$NumTrees >= 1000] == min(DVO_IDs$LowFlow[DVO_IDs$NumTrees >= 1000])),]
#Compute compromise as the solution closest to the ideal point in all 3 objectives (minimum Euclidean distance after scaling to common scale)
TreesThreshSyn$FloodingScaled = (TreesThreshSyn$Flooding - mean(TreesThreshSyn$Flooding)) / sd(TreesThreshSyn$Flooding)
TreesThreshSyn$LowFlowScaled = (TreesThreshSyn$LowFlow - mean(TreesThreshSyn$LowFlow)) / sd(TreesThreshSyn$LowFlow)
TreesThreshSyn$NumTreesScaled = (TreesThreshSyn$NumTrees - mean(TreesThreshSyn$NumTrees)) / sd(TreesThreshSyn$NumTrees)

FloodIdealSynScaled = (-1 - mean(TreesThreshSyn$Flooding)) / sd(TreesThreshSyn$Flooding)
LowFlowIdealSynScaled = (-1 - mean(TreesThreshSyn$LowFlow)) / sd(TreesThreshSyn$LowFlow)
NumTreesIdealSynScaled = (0 - mean(TreesThreshSyn$NumTrees)) / sd(TreesThreshSyn$NumTrees)

TreesThreshSyn$distIdeal = 0
for (i in 1:nrow(TreesThreshSyn)){
        TreesThreshSyn$distIdeal[i] = sqrt((TreesThreshSyn$FloodingScaled[i] - FloodIdealSynScaled)^2 + (TreesThreshSyn$LowFlowScaled[i] - LowFlowIdealSynScaled)^2 + (TreesThreshSyn$NumTreesScaled[i] - NumTreesIdealSynScaled)^2)
}

CompromiseSyn = TreesThreshSyn[which(TreesThreshSyn$distIdeal == min(TreesThreshSyn$distIdeal)),]
CompromiseSynMinTrees = TreesThreshSyn[which(TreesThreshSyn$NumTrees == min(TreesThreshSyn$NumTrees)),]

# MAP----
BestFloodMAP = DVO_MAPSyn[DVO_MAPSyn$Flooding == min(DVO_MAPSyn$Flooding),]
BestLowFlowMAP = DVO_MAPSyn[DVO_MAPSyn$NumTrees >= 1000,][which(DVO_MAPSyn$LowFlow[DVO_MAPSyn$NumTrees >= 1000] == min(DVO_MAPSyn$LowFlow[DVO_MAPSyn$NumTrees >= 1000])),]
#Compute compromise as the solution closest to the ideal point in all 3 objectives (minimum Euclidean distance after scaling to common scale)
TreesThreshMAP$FloodingScaled = (TreesThreshMAP$Flooding - mean(TreesThreshMAP$Flooding)) / sd(TreesThreshMAP$Flooding)
TreesThreshMAP$LowFlowScaled = (TreesThreshMAP$LowFlow - mean(TreesThreshMAP$LowFlow)) / sd(TreesThreshMAP$LowFlow)
TreesThreshMAP$NumTreesScaled = (TreesThreshMAP$NumTrees - mean(TreesThreshMAP$NumTrees)) / sd(TreesThreshMAP$NumTrees)

FloodIdealMAPScaled = (-1 - mean(TreesThreshMAP$Flooding)) / sd(TreesThreshMAP$Flooding)
LowFlowIdealMAPScaled = (-1 - mean(TreesThreshMAP$LowFlow)) / sd(TreesThreshMAP$LowFlow)
NumTreesIdealMAPScaled = (0 - mean(TreesThreshMAP$NumTrees)) / sd(TreesThreshMAP$NumTrees)

TreesThreshMAP$distIdeal = 0
for (i in 1:nrow(TreesThreshMAP)){
        TreesThreshMAP$distIdeal[i] = sqrt((TreesThreshMAP$FloodingScaled[i] - FloodIdealMAPScaled)^2 + (TreesThreshMAP$LowFlowScaled[i] - LowFlowIdealMAPScaled)^2 + (TreesThreshMAP$NumTreesScaled[i] - NumTreesIdealMAPScaled)^2)
}

CompromiseMAP = TreesThreshMAP[which(TreesThreshMAP$distIdeal == min(TreesThreshMAP$distIdeal)),]
CompromiseMAPMinTrees = TreesThreshMAP[which(TreesThreshMAP$NumTrees == min(TreesThreshMAP$NumTrees)),]

# MORO----
BestFloodMORO = DVO_MOROSyn[DVO_MOROSyn$Flooding == min(DVO_MOROSyn$Flooding),]
BestLowFlowMORO = DVO_MOROSyn[DVO_MOROSyn$NumTrees >= 1000,][which(DVO_MOROSyn$LowFlow[DVO_MOROSyn$NumTrees >= 1000] == min(DVO_MOROSyn$LowFlow[DVO_MOROSyn$NumTrees >= 1000])),]
#Compute compromise as the solution closest to the ideal point in all 3 objectives (minimum Euclidean distance after scaling to common scale)
TreesThreshMORO$FloodingScaled = (TreesThreshMORO$Flooding - mean(TreesThreshMORO$Flooding)) / sd(TreesThreshMORO$Flooding)
TreesThreshMORO$LowFlowScaled = (TreesThreshMORO$LowFlow - mean(TreesThreshMORO$LowFlow)) / sd(TreesThreshMORO$LowFlow)
TreesThreshMORO$NumTreesScaled = (TreesThreshMORO$NumTrees - mean(TreesThreshMORO$NumTrees)) / sd(TreesThreshMORO$NumTrees)

FloodIdealMOROScaled = (-1 - mean(TreesThreshMORO$Flooding)) / sd(TreesThreshMORO$Flooding)
LowFlowIdealMOROScaled = (-1 - mean(TreesThreshMORO$LowFlow)) / sd(TreesThreshMORO$LowFlow)
NumTreesIdealMOROScaled = (0 - mean(TreesThreshMORO$NumTrees)) / sd(TreesThreshMORO$NumTrees)

TreesThreshMORO$distIdeal = 0
for (i in 1:nrow(TreesThreshMORO)){
        TreesThreshMORO$distIdeal[i] = sqrt((TreesThreshMORO$FloodingScaled[i] - FloodIdealMOROScaled)^2 + (TreesThreshMORO$LowFlowScaled[i] - LowFlowIdealMOROScaled)^2 + (TreesThreshMORO$NumTreesScaled[i] - NumTreesIdealMOROScaled)^2)
}

CompromiseMORO = TreesThreshMORO[which(TreesThreshMORO$distIdeal == min(TreesThreshMORO$distIdeal)),]
CompromiseMOROMinTrees = TreesThreshMORO[which(TreesThreshMORO$NumTrees == min(TreesThreshMORO$NumTrees)),]

# MMO----
BestFloodMMO = DVO_MMOSyn[DVO_MMOSyn$Flooding == min(DVO_MMOSyn$Flooding),]
BestLowFlowMMO = DVO_MMOSyn[DVO_MMOSyn$NumTrees >= 1000,][which(DVO_MMOSyn$LowFlow[DVO_MMOSyn$NumTrees >= 1000] == min(DVO_MMOSyn$LowFlow[DVO_MMOSyn$NumTrees >= 1000])),]
#Compute compromise as the solution closest to the ideal point in all 3 objectives (minimum Euclidean distance after scaling to common scale)
TreesThreshMMO$FloodingScaled = (TreesThreshMMO$Flooding - mean(TreesThreshMMO$Flooding)) / sd(TreesThreshMMO$Flooding)
TreesThreshMMO$LowFlowScaled = (TreesThreshMMO$LowFlow - mean(TreesThreshMMO$LowFlow)) / sd(TreesThreshMMO$LowFlow)
TreesThreshMMO$NumTreesScaled = (TreesThreshMMO$NumTrees - mean(TreesThreshMMO$NumTrees)) / sd(TreesThreshMMO$NumTrees)

FloodIdealMMOScaled = (-1 - mean(TreesThreshMMO$Flooding)) / sd(TreesThreshMMO$Flooding)
LowFlowIdealMMOScaled = (-1 - mean(TreesThreshMMO$LowFlow)) / sd(TreesThreshMMO$LowFlow)
NumTreesIdealMMOScaled = (0 - mean(TreesThreshMMO$NumTrees)) / sd(TreesThreshMMO$NumTrees)

TreesThreshMMO$distIdeal = 0
for (i in 1:nrow(TreesThreshMMO)){
        TreesThreshMMO$distIdeal[i] = sqrt((TreesThreshMMO$FloodingScaled[i] - FloodIdealMMOScaled)^2 + (TreesThreshMMO$LowFlowScaled[i] - LowFlowIdealMMOScaled)^2 + (TreesThreshMMO$NumTreesScaled[i] - NumTreesIdealMMOScaled)^2)
}

CompromiseMMO = TreesThreshMMO[which(TreesThreshMMO$distIdeal == min(TreesThreshMMO$distIdeal)),]
CompromiseMMOMinTrees = TreesThreshMMO[which(TreesThreshMMO$NumTrees == min(TreesThreshMMO$NumTrees)),]

#Plot the Pareto front with thresholds and selected solution----
scaleRange = c(0,18000)
scaleBy = 2000
Pal = rev(rainbow((scaleRange[2] - scaleRange[1])/scaleBy))

# Syn----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptSynTruth")
png('ParetoSyn_ColTrees_TargetThresh_CompSol.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO$Flooding*100, -DVO$LowFlow*100, col = colFun(DVO$NumTrees),
     xlim = c(0, 60), ylim=c(-100, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to Synthetic Truth', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
lines(c(-110,110), c(-20,-20), col = 'red')
lines(c(20,20), c(-120,120), col = 'red')
#Add compromise solution
par(new=TRUE)
plot(-CompromiseSyn$Flooding*100, -CompromiseSyn$LowFlow*100, col = colFun(CompromiseSyn$NumTrees),
     xlim = c(0, 60), ylim=c(-100, 10), xlab = '', ylab = '', axes=FALSE, pch = 15)
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16', 'Compromise Sol.'), col = colFun(c(seq(0,16000,2000), CompromiseSyn$NumTrees)), pch = c(rep(16,9),15), cex=1.1)
dev.off()

png('ParetoSyn_ColTrees_TargetThresh_CompSolMinTrees.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO$Flooding*100, -DVO$LowFlow*100, col = colFun(DVO$NumTrees),
     xlim = c(0, 60), ylim=c(-100, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to Synthetic Truth', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
lines(c(-110,110), c(-20,-20), col = 'red')
lines(c(20,20), c(-120,120), col = 'red')
#Add compromise solution
par(new=TRUE)
plot(-CompromiseSynMinTrees$Flooding*100, -CompromiseSynMinTrees$LowFlow*100, col = colFun(CompromiseSynMinTrees$NumTrees),
     xlim = c(0, 60), ylim=c(-100, 10), xlab = '', ylab = '', axes=FALSE, pch = 15)
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16', 'Compromise Sol.'), col = colFun(c(seq(0,16000,2000), CompromiseSyn$NumTrees)), pch = c(rep(16,9),15), cex=1.1)
dev.off()

# MAP----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptMAP")
png('ParetoMAP_ColTrees_TargetThresh_CompSol.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO_MAP$Flooding*100, -DVO_MAP$LowFlow*100, col = colFun(DVO_MAP$NumTrees),
     xlim = c(0, 60), ylim=c(-100, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MAP', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
lines(c(-110,110), c(-20,-20), col = 'red')
lines(c(20,20), c(-120,120), col = 'red')
#Add compromise solution
par(new=TRUE)
plot(-CompromiseMAP$Flooding*100, -CompromiseMAP$LowFlow*100, col = colFun(CompromiseMAP$NumTrees),
     xlim = c(0, 60), ylim=c(-100, 10), xlab = '', ylab = '', axes=FALSE, pch = 15)
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16', 'Compromise Sol.'), col = colFun(c(seq(0,16000,2000), CompromiseMAP$NumTrees)), pch = c(rep(16,9),15), cex=1.1)
dev.off()

png('ParetoMAP_ColTrees_TargetThresh_CompSolMinTrees.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO_MAP$Flooding*100, -DVO_MAP$LowFlow*100, col = colFun(DVO_MAP$NumTrees),
     xlim = c(0, 60), ylim=c(-100, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MAP', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
lines(c(-110,110), c(-20,-20), col = 'red')
lines(c(20,20), c(-120,120), col = 'red')
#Add compromise solution
par(new=TRUE)
plot(-CompromiseMAPMinTrees$Flooding*100, -CompromiseMAPMinTrees$LowFlow*100, col = colFun(CompromiseMAPMinTrees$NumTrees),
     xlim = c(0, 60), ylim=c(-100, 10), xlab = '', ylab = '', axes=FALSE, pch = 15)
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16', 'Compromise Sol.'), col = colFun(c(seq(0,16000,2000), CompromiseMAPMinTrees$NumTrees)), pch = c(rep(16,9),15), cex=1.1)
dev.off()

# MORO----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptMORO")
png('ParetoMORO_ColTrees_TargetThresh_CompSol.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO_MORO$Flooding*100, -DVO_MORO$LowFlow*100, col = colFun(DVO_MORO$NumTrees),
     xlim = c(0, 60), ylim=c(-100, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MORO', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
lines(c(-110,110), c(-20,-20), col = 'red')
lines(c(20,20), c(-120,120), col = 'red')
#Add compromise solution
par(new=TRUE)
plot(-CompromiseMORO$Flooding*100, -CompromiseMORO$LowFlow*100, col = colFun(CompromiseMORO$NumTrees),
     xlim = c(0, 60), ylim=c(-100, 10), xlab = '', ylab = '', axes=FALSE, pch = 15)
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16', 'Compromise Sol.'), col = colFun(c(seq(0,16000,2000), CompromiseMORO$NumTrees)), pch = c(rep(16,9),15), cex=1.1)
dev.off()

png('ParetoMORO_ColTrees_TargetThresh_CompSolMinTrees.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO_MORO$Flooding*100, -DVO_MORO$LowFlow*100, col = colFun(DVO_MORO$NumTrees),
     xlim = c(0, 60), ylim=c(-100, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MORO', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
lines(c(-110,110), c(-20,-20), col = 'red')
lines(c(20,20), c(-120,120), col = 'red')
#Add compromise solution
par(new=TRUE)
plot(-CompromiseMOROMinTrees$Flooding*100, -CompromiseMOROMinTrees$LowFlow*100, col = colFun(CompromiseMOROMinTrees$NumTrees),
     xlim = c(0, 60), ylim=c(-100, 10), xlab = '', ylab = '', axes=FALSE, pch = 15)
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16', 'Compromise Sol.'), col = colFun(c(seq(0,16000,2000), CompromiseMOROMinTrees$NumTrees)), pch = c(rep(16,9),15), cex=1.1)
dev.off()

# MMO----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptMinMax")
png('ParetoMMO_ColTrees_TargetThresh_CompSol.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO_MMO$Flooding*100, -DVO_MMO$LowFlow*100, col = colFun(DVO_MMO$NumTrees),
     xlim = c(0, 60), ylim=c(-120, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MMO', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-130,130))
lines(c(-110,110), c(-20,-20), col = 'red')
lines(c(20,20), c(-130,130), col = 'red')
#Add compromise solution
par(new=TRUE)
plot(-CompromiseMMO$Flooding*100, -CompromiseMMO$LowFlow*100, col = colFun(CompromiseMMO$NumTrees),
     xlim = c(0, 60), ylim=c(-120, 10), xlab = '', ylab = '', axes=FALSE, pch = 15)
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16', 'Compromise Sol.'), col = colFun(c(seq(0,16000,2000), CompromiseMMO$NumTrees)), pch = c(rep(16,9),15), cex=1.1)
dev.off()

png('ParetoMMO_ColTrees_TargetThresh_CompSolMinTrees.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO_MMO$Flooding*100, -DVO_MMO$LowFlow*100, col = colFun(DVO_MMO$NumTrees),
     xlim = c(0, 60), ylim=c(-120, 10), xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Reforestation Optimization to MMO', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-130,130))
lines(c(-110,110), c(-20,-20), col = 'red')
lines(c(20,20), c(-130,130), col = 'red')
#Add compromise solution
par(new=TRUE)
plot(-CompromiseMMOMinTrees$Flooding*100, -CompromiseMMOMinTrees$LowFlow*100, col = colFun(CompromiseMMOMinTrees$NumTrees),
     xlim = c(0, 60), ylim=c(-120, 10), xlab = '', ylab = '', axes=FALSE, pch = 15)
legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16', 'Compromise Sol.'), col = colFun(c(seq(0,16000,2000), CompromiseMMOMinTrees$NumTrees)), pch = c(rep(16,9),15), cex=1.1)
dev.off()

# Re-evaluated in Synthetic Space----
# I don't know where these objective values came from. They are slightly different
# than the values computed above, except for trees. I'm commenting out.
# BestFloodMAP$FloodingSyn = -0.48622513845799
# BestFloodMAP$LowFlowSyn = 0.955338717060994
# BestFloodMAP$NumTreesSyn = 17335
# 
# BestLowFlowMAP$FloodingSyn = -0.0371680966407472
# BestLowFlowMAP$LowFlowSyn = 0.0109226024934508
# BestLowFlowMAP$NumTreesSyn = 1073
# 
# CompromiseMAPMinTrees$FloodingSyn = -0.155504665219056
# CompromiseMAPMinTrees$LowFlowSyn = 0.18100878735067
# CompromiseMAPMinTrees$NumTreesSyn = 3967

setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12")
ylims = c(-120,10)
png('ParetoSyn_ColTrees_MAP+MORO+MMOsols.png', res = 300, units = 'in', width = 6, height = 6)
plot(-DVO$Flooding*100, -DVO$LowFlow*100, col = colFun(DVO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Solution Degradation', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
#3 MAP solutions
par(new=TRUE)
plot(-BestFloodMAP$Flooding*100, -BestFloodMAP$LowFlow*100, col = colFun(BestFloodMAP$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
par(new=TRUE)
plot(-BestFloodMAP$FloodingSyn*100, -BestFloodMAP$LowFlowSyn*100, col = colFun(BestFloodMAP$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
lines(x = c(-BestFloodMAP$Flooding*100, -BestFloodMAP$FloodingSyn*100), y = c(-BestFloodMAP$LowFlow*100, -BestFloodMAP$LowFlowSyn*100), lwd = 2)

par(new=TRUE)
plot(-BestLowFlowMAP$Flooding*100, -BestLowFlowMAP$LowFlow*100, col = colFun(BestLowFlowMAP$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
par(new=TRUE)
plot(-BestLowFlowMAP$FloodingSyn*100, -BestLowFlowMAP$LowFlowSyn*100, col = colFun(BestLowFlowMAP$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
lines(x = c(-BestLowFlowMAP$Flooding*100, -BestLowFlowMAP$FloodingSyn*100), y = c(-BestLowFlowMAP$LowFlow*100, -BestLowFlowMAP$LowFlowSyn*100), lwd = 2)

par(new=TRUE)
plot(-CompromiseMAPMinTrees$Flooding*100, -CompromiseMAPMinTrees$LowFlow*100, col = colFun(CompromiseMAPMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
par(new=TRUE)
plot(-CompromiseMAPMinTrees$FloodingSyn*100, -CompromiseMAPMinTrees$LowFlowSyn*100, col = colFun(CompromiseMAPMinTrees$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
lines(x = c(-CompromiseMAPMinTrees$Flooding*100, -CompromiseMAPMinTrees$FloodingSyn*100), y = c(-CompromiseMAPMinTrees$LowFlow*100, -CompromiseMAPMinTrees$LowFlowSyn*100), lwd = 2)

#3 MORO solutions
par(new=TRUE)
plot(-BestFloodMORO$Flooding*100, -BestFloodMORO$LowFlow*100, col = colFun(BestFloodMORO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
par(new=TRUE)
plot(-BestFloodMORO$FloodingSyn*100, -BestFloodMORO$LowFlowSyn*100, col = colFun(BestFloodMORO$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
lines(x = c(-BestFloodMORO$Flooding*100, -BestFloodMORO$FloodingSyn*100), y = c(-BestFloodMORO$LowFlow*100, -BestFloodMORO$LowFlowSyn*100), lwd = 2)

par(new=TRUE)
plot(-BestLowFlowMORO$Flooding*100, -BestLowFlowMORO$LowFlow*100, col = colFun(BestLowFlowMORO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
par(new=TRUE)
plot(-BestLowFlowMORO$FloodingSyn*100, -BestLowFlowMORO$LowFlowSyn*100, col = colFun(BestLowFlowMORO$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
lines(x = c(-BestLowFlowMORO$Flooding*100, -BestLowFlowMORO$FloodingSyn*100), y = c(-BestLowFlowMORO$LowFlow*100, -BestLowFlowMORO$LowFlowSyn*100), lwd = 2)

par(new=TRUE)
plot(-CompromiseMOROMinTrees$Flooding*100, -CompromiseMOROMinTrees$LowFlow*100, col = colFun(CompromiseMOROMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
par(new=TRUE)
plot(-CompromiseMOROMinTrees$FloodingSyn*100, -CompromiseMOROMinTrees$LowFlowSyn*100, col = colFun(CompromiseMOROMinTrees$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
lines(x = c(-CompromiseMOROMinTrees$Flooding*100, -CompromiseMOROMinTrees$FloodingSyn*100), y = c(-CompromiseMOROMinTrees$LowFlow*100, -CompromiseMOROMinTrees$LowFlowSyn*100), lwd = 2)

#3 MMO solutions
par(new=TRUE)
plot(-BestFloodMMO$Flooding*100, -BestFloodMMO$LowFlow*100, col = colFun(BestFloodMMO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
par(new=TRUE)
plot(-BestFloodMMO$FloodingSyn*100, -BestFloodMMO$LowFlowSyn*100, col = colFun(BestFloodMMO$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
lines(x = c(-BestFloodMMO$Flooding*100, -BestFloodMMO$FloodingSyn*100), y = c(-BestFloodMMO$LowFlow*100, -BestFloodMMO$LowFlowSyn*100), lwd = 2)

par(new=TRUE)
plot(-BestLowFlowMMO$Flooding*100, -BestLowFlowMMO$LowFlow*100, col = colFun(BestLowFlowMMO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
par(new=TRUE)
plot(-BestLowFlowMMO$FloodingSyn*100, -BestLowFlowMMO$LowFlowSyn*100, col = colFun(BestLowFlowMMO$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
lines(x = c(-BestLowFlowMMO$Flooding*100, -BestLowFlowMMO$FloodingSyn*100), y = c(-BestLowFlowMMO$LowFlow*100, -BestLowFlowMMO$LowFlowSyn*100), lwd = 2)

par(new=TRUE)
plot(-CompromiseMOROMinTrees$Flooding*100, -CompromiseMOROMinTrees$LowFlow*100, col = colFun(CompromiseMOROMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
par(new=TRUE)
plot(-CompromiseMOROMinTrees$FloodingSyn*100, -CompromiseMOROMinTrees$LowFlowSyn*100, col = colFun(CompromiseMOROMinTrees$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
lines(x = c(-CompromiseMOROMinTrees$Flooding*100, -CompromiseMOROMinTrees$FloodingSyn*100), y = c(-CompromiseMOROMinTrees$LowFlow*100, -CompromiseMOROMinTrees$LowFlowSyn*100), lwd = 2)


lines(c(-110,110), c(-20,-20), col = 'red')
lines(c(20,20), c(-120,120), col = 'red')

legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16'), col = colFun(c(seq(0,16000,2000))), pch = c(rep(16,9)), cex=1.1)
legend('topright', title = 'Optimization', 
       legend = c('Synthetic', 'MAP', 'MORO', 'MinMax'), pch = c(16,15,17,18), cex=1.1)
dev.off()

#Just lines
png('ParetoSyn_ColTrees_MAP+MORO+MMOsols_lines.png', res = 300, units = 'in', width = 6, height = 6)
plot(-CompromiseSynMinTrees$Flooding*100, -CompromiseSynMinTrees$LowFlow*100, col = colFun(CompromiseSynMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Solution Degradation', pch = 16)
par(new=TRUE)
plot(-BestLowFlowSyn$Flooding*100, -BestLowFlowSyn$LowFlow*100, col = colFun(BestLowFlowSyn$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', main = '', axes = FALSE, pch = 16)
par(new=TRUE)
plot(-BestFloodSyn$Flooding*100, -BestFloodSyn$LowFlow*100, col = colFun(BestFloodSyn$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', main = '', axes = FALSE, pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
#3 MAP solutions
par(new=TRUE)
plot(-BestFloodMAP$Flooding*100, -BestFloodMAP$LowFlow*100, col = colFun(BestFloodMAP$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
par(new=TRUE)
plot(-BestFloodMAP$FloodingSyn*100, -BestFloodMAP$LowFlowSyn*100, col = colFun(BestFloodMAP$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
lines(x = c(-BestFloodMAP$Flooding*100, -BestFloodMAP$FloodingSyn*100), y = c(-BestFloodMAP$LowFlow*100, -BestFloodMAP$LowFlowSyn*100), lwd = 2)

par(new=TRUE)
plot(-BestLowFlowMAP$Flooding*100, -BestLowFlowMAP$LowFlow*100, col = colFun(BestLowFlowMAP$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
par(new=TRUE)
plot(-BestLowFlowMAP$FloodingSyn*100, -BestLowFlowMAP$LowFlowSyn*100, col = colFun(BestLowFlowMAP$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
lines(x = c(-BestLowFlowMAP$Flooding*100, -BestLowFlowMAP$FloodingSyn*100), y = c(-BestLowFlowMAP$LowFlow*100, -BestLowFlowMAP$LowFlowSyn*100), lwd = 2)

par(new=TRUE)
plot(-CompromiseMAPMinTrees$Flooding*100, -CompromiseMAPMinTrees$LowFlow*100, col = colFun(CompromiseMAPMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
par(new=TRUE)
plot(-CompromiseMAPMinTrees$FloodingSyn*100, -CompromiseMAPMinTrees$LowFlowSyn*100, col = colFun(CompromiseMAPMinTrees$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
lines(x = c(-CompromiseMAPMinTrees$Flooding*100, -CompromiseMAPMinTrees$FloodingSyn*100), y = c(-CompromiseMAPMinTrees$LowFlow*100, -CompromiseMAPMinTrees$LowFlowSyn*100), lwd = 2)

#3 MORO solutions
par(new=TRUE)
plot(-BestFloodMORO$Flooding*100, -BestFloodMORO$LowFlow*100, col = colFun(BestFloodMORO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
par(new=TRUE)
plot(-BestFloodMORO$FloodingSyn*100, -BestFloodMORO$LowFlowSyn*100, col = colFun(BestFloodMORO$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
lines(x = c(-BestFloodMORO$Flooding*100, -BestFloodMORO$FloodingSyn*100), y = c(-BestFloodMORO$LowFlow*100, -BestFloodMORO$LowFlowSyn*100), lwd = 2)

par(new=TRUE)
plot(-BestLowFlowMORO$Flooding*100, -BestLowFlowMORO$LowFlow*100, col = colFun(BestLowFlowMORO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
par(new=TRUE)
plot(-BestLowFlowMORO$FloodingSyn*100, -BestLowFlowMORO$LowFlowSyn*100, col = colFun(BestLowFlowMORO$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
lines(x = c(-BestLowFlowMORO$Flooding*100, -BestLowFlowMORO$FloodingSyn*100), y = c(-BestLowFlowMORO$LowFlow*100, -BestLowFlowMORO$LowFlowSyn*100), lwd = 2)

par(new=TRUE)
plot(-CompromiseMOROMinTrees$Flooding*100, -CompromiseMOROMinTrees$LowFlow*100, col = colFun(CompromiseMOROMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
par(new=TRUE)
plot(-CompromiseMOROMinTrees$FloodingSyn*100, -CompromiseMOROMinTrees$LowFlowSyn*100, col = colFun(CompromiseMOROMinTrees$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
lines(x = c(-CompromiseMOROMinTrees$Flooding*100, -CompromiseMOROMinTrees$FloodingSyn*100), y = c(-CompromiseMOROMinTrees$LowFlow*100, -CompromiseMOROMinTrees$LowFlowSyn*100), lwd = 2)

#3 MMO solutions
par(new=TRUE)
plot(-BestFloodMMO$Flooding*100, -BestFloodMMO$LowFlow*100, col = colFun(BestFloodMMO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
par(new=TRUE)
plot(-BestFloodMMO$FloodingSyn*100, -BestFloodMMO$LowFlowSyn*100, col = colFun(BestFloodMMO$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
lines(x = c(-BestFloodMMO$Flooding*100, -BestFloodMMO$FloodingSyn*100), y = c(-BestFloodMMO$LowFlow*100, -BestFloodMMO$LowFlowSyn*100), lwd = 2)

par(new=TRUE)
plot(-BestLowFlowMMO$Flooding*100, -BestLowFlowMMO$LowFlow*100, col = colFun(BestLowFlowMMO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
par(new=TRUE)
plot(-BestLowFlowMMO$FloodingSyn*100, -BestLowFlowMMO$LowFlowSyn*100, col = colFun(BestLowFlowMMO$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
lines(x = c(-BestLowFlowMMO$Flooding*100, -BestLowFlowMMO$FloodingSyn*100), y = c(-BestLowFlowMMO$LowFlow*100, -BestLowFlowMMO$LowFlowSyn*100), lwd = 2)

par(new=TRUE)
plot(-CompromiseMOROMinTrees$Flooding*100, -CompromiseMOROMinTrees$LowFlow*100, col = colFun(CompromiseMOROMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
par(new=TRUE)
plot(-CompromiseMOROMinTrees$FloodingSyn*100, -CompromiseMOROMinTrees$LowFlowSyn*100, col = colFun(CompromiseMOROMinTrees$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
lines(x = c(-CompromiseMOROMinTrees$Flooding*100, -CompromiseMOROMinTrees$FloodingSyn*100), y = c(-CompromiseMOROMinTrees$LowFlow*100, -CompromiseMOROMinTrees$LowFlowSyn*100), lwd = 2)

legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16'), col = colFun(c(seq(0,16000,2000))), pch = c(rep(16,9)), cex=1.1)
legend('topright', title = 'Optimization', 
       legend = c('Synthetic', 'MAP', 'MORO', 'MinMax'), pch = c(16,15,17,18), cex=1.1)
dev.off()

#Lines with compromise thresholds
png('ParetoSyn_ColTrees_MAP+MORO+MMOsols_linesThresh.png', res = 300, units = 'in', width = 6, height = 6)
plot(-CompromiseSynMinTrees$Flooding*100, -CompromiseSynMinTrees$LowFlow*100, col = colFun(CompromiseSynMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Solution Degradation', pch = 16)
par(new=TRUE)
plot(-BestLowFlowSyn$Flooding*100, -BestLowFlowSyn$LowFlow*100, col = colFun(BestLowFlowSyn$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', main = '', axes = FALSE, pch = 16)
par(new=TRUE)
plot(-BestFloodSyn$Flooding*100, -BestFloodSyn$LowFlow*100, col = colFun(BestFloodSyn$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', main = '', axes = FALSE, pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
#3 MAP solutions
par(new=TRUE)
plot(-BestFloodMAP$Flooding*100, -BestFloodMAP$LowFlow*100, col = colFun(BestFloodMAP$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
par(new=TRUE)
plot(-BestFloodMAP$FloodingSyn*100, -BestFloodMAP$LowFlowSyn*100, col = colFun(BestFloodMAP$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
lines(x = c(-BestFloodMAP$Flooding*100, -BestFloodMAP$FloodingSyn*100), y = c(-BestFloodMAP$LowFlow*100, -BestFloodMAP$LowFlowSyn*100), lwd = 2)

par(new=TRUE)
plot(-BestLowFlowMAP$Flooding*100, -BestLowFlowMAP$LowFlow*100, col = colFun(BestLowFlowMAP$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
par(new=TRUE)
plot(-BestLowFlowMAP$FloodingSyn*100, -BestLowFlowMAP$LowFlowSyn*100, col = colFun(BestLowFlowMAP$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
lines(x = c(-BestLowFlowMAP$Flooding*100, -BestLowFlowMAP$FloodingSyn*100), y = c(-BestLowFlowMAP$LowFlow*100, -BestLowFlowMAP$LowFlowSyn*100), lwd = 2)

par(new=TRUE)
plot(-CompromiseMAPMinTrees$Flooding*100, -CompromiseMAPMinTrees$LowFlow*100, col = colFun(CompromiseMAPMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
par(new=TRUE)
plot(-CompromiseMAPMinTrees$FloodingSyn*100, -CompromiseMAPMinTrees$LowFlowSyn*100, col = colFun(CompromiseMAPMinTrees$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
lines(x = c(-CompromiseMAPMinTrees$Flooding*100, -CompromiseMAPMinTrees$FloodingSyn*100), y = c(-CompromiseMAPMinTrees$LowFlow*100, -CompromiseMAPMinTrees$LowFlowSyn*100), lwd = 2)

#3 MORO solutions
par(new=TRUE)
plot(-BestFloodMORO$Flooding*100, -BestFloodMORO$LowFlow*100, col = colFun(BestFloodMORO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
par(new=TRUE)
plot(-BestFloodMORO$FloodingSyn*100, -BestFloodMORO$LowFlowSyn*100, col = colFun(BestFloodMORO$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
lines(x = c(-BestFloodMORO$Flooding*100, -BestFloodMORO$FloodingSyn*100), y = c(-BestFloodMORO$LowFlow*100, -BestFloodMORO$LowFlowSyn*100), lwd = 2)

par(new=TRUE)
plot(-BestLowFlowMORO$Flooding*100, -BestLowFlowMORO$LowFlow*100, col = colFun(BestLowFlowMORO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
par(new=TRUE)
plot(-BestLowFlowMORO$FloodingSyn*100, -BestLowFlowMORO$LowFlowSyn*100, col = colFun(BestLowFlowMORO$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
lines(x = c(-BestLowFlowMORO$Flooding*100, -BestLowFlowMORO$FloodingSyn*100), y = c(-BestLowFlowMORO$LowFlow*100, -BestLowFlowMORO$LowFlowSyn*100), lwd = 2)

par(new=TRUE)
plot(-CompromiseMOROMinTrees$Flooding*100, -CompromiseMOROMinTrees$LowFlow*100, col = colFun(CompromiseMOROMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
par(new=TRUE)
plot(-CompromiseMOROMinTrees$FloodingSyn*100, -CompromiseMOROMinTrees$LowFlowSyn*100, col = colFun(CompromiseMOROMinTrees$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
lines(x = c(-CompromiseMOROMinTrees$Flooding*100, -CompromiseMOROMinTrees$FloodingSyn*100), y = c(-CompromiseMOROMinTrees$LowFlow*100, -CompromiseMOROMinTrees$LowFlowSyn*100), lwd = 2)

#3 MMO solutions
par(new=TRUE)
plot(-BestFloodMMO$Flooding*100, -BestFloodMMO$LowFlow*100, col = colFun(BestFloodMMO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
par(new=TRUE)
plot(-BestFloodMMO$FloodingSyn*100, -BestFloodMMO$LowFlowSyn*100, col = colFun(BestFloodMMO$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
lines(x = c(-BestFloodMMO$Flooding*100, -BestFloodMMO$FloodingSyn*100), y = c(-BestFloodMMO$LowFlow*100, -BestFloodMMO$LowFlowSyn*100), lwd = 2)

par(new=TRUE)
plot(-BestLowFlowMMO$Flooding*100, -BestLowFlowMMO$LowFlow*100, col = colFun(BestLowFlowMMO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
par(new=TRUE)
plot(-BestLowFlowMMO$FloodingSyn*100, -BestLowFlowMMO$LowFlowSyn*100, col = colFun(BestLowFlowMMO$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
lines(x = c(-BestLowFlowMMO$Flooding*100, -BestLowFlowMMO$FloodingSyn*100), y = c(-BestLowFlowMMO$LowFlow*100, -BestLowFlowMMO$LowFlowSyn*100), lwd = 2)

par(new=TRUE)
plot(-CompromiseMOROMinTrees$Flooding*100, -CompromiseMOROMinTrees$LowFlow*100, col = colFun(CompromiseMOROMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
par(new=TRUE)
plot(-CompromiseMOROMinTrees$FloodingSyn*100, -CompromiseMOROMinTrees$LowFlowSyn*100, col = colFun(CompromiseMOROMinTrees$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
lines(x = c(-CompromiseMOROMinTrees$Flooding*100, -CompromiseMOROMinTrees$FloodingSyn*100), y = c(-CompromiseMOROMinTrees$LowFlow*100, -CompromiseMOROMinTrees$LowFlowSyn*100), lwd = 2)

lines(c(-110,110), c(-20,-20), col = 'red')
lines(c(20,20), c(-130,130), col = 'red')

legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <2', '2 - <4', '4 - <6', '6 - <8', '8 - <10', '10 - <12', '12 - <14', '14 - <16', '>16'), col = colFun(c(seq(0,16000,2000))), pch = c(rep(16,9)), cex=1.1)
legend('topright', title = 'Optimization', 
       legend = c('Synthetic', 'MAP', 'MORO', 'MinMax'), pch = c(16,15,17,18), cex=1.1)
dev.off()

#Lines with only compromise sols
png('ParetoSyn_ColTrees_MAP+MORO+MMOsols_linesThresh_CompSolOnly.png', res = 300, units = 'in', width = 6, height = 6)
plot(-CompromiseSynMinTrees$Flooding*100, -CompromiseSynMinTrees$LowFlow*100, col = colFun(CompromiseSynMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Solution Degradation', pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
#MAP solution
par(new=TRUE)
plot(-CompromiseMAPMinTrees$Flooding*100, -CompromiseMAPMinTrees$LowFlow*100, col = colFun(CompromiseMAPMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
par(new=TRUE)
plot(-CompromiseMAPMinTrees$FloodingSyn*100, -CompromiseMAPMinTrees$LowFlowSyn*100, col = colFun(CompromiseMAPMinTrees$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
lines(x = c(-CompromiseMAPMinTrees$Flooding*100, -CompromiseMAPMinTrees$FloodingSyn*100), y = c(-CompromiseMAPMinTrees$LowFlow*100, -CompromiseMAPMinTrees$LowFlowSyn*100), lwd = 2)

#MORO solution
par(new=TRUE)
plot(-CompromiseMOROMinTrees$Flooding*100, -CompromiseMOROMinTrees$LowFlow*100, col = colFun(CompromiseMOROMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
par(new=TRUE)
plot(-CompromiseMOROMinTrees$FloodingSyn*100, -CompromiseMOROMinTrees$LowFlowSyn*100, col = colFun(CompromiseMOROMinTrees$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
lines(x = c(-CompromiseMOROMinTrees$Flooding*100, -CompromiseMOROMinTrees$FloodingSyn*100), y = c(-CompromiseMOROMinTrees$LowFlow*100, -CompromiseMOROMinTrees$LowFlowSyn*100), lwd = 2)

#MMO solution
par(new=TRUE)
plot(-CompromiseMOROMinTrees$Flooding*100, -CompromiseMOROMinTrees$LowFlow*100, col = colFun(CompromiseMOROMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
par(new=TRUE)
plot(-CompromiseMOROMinTrees$FloodingSyn*100, -CompromiseMOROMinTrees$LowFlowSyn*100, col = colFun(CompromiseMOROMinTrees$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
lines(x = c(-CompromiseMOROMinTrees$Flooding*100, -CompromiseMOROMinTrees$FloodingSyn*100), y = c(-CompromiseMOROMinTrees$LowFlow*100, -CompromiseMOROMinTrees$LowFlowSyn*100), lwd = 2)

lines(c(-110,110), c(-20,-20), col = 'red')
lines(c(20,20), c(-120,120), col = 'red')

legend('topright', title = 'Optimization', 
       legend = c('Synthetic', 'MAP', 'MORO', 'MinMax'), pch = c(16,15,17,18), cex=1.1)
dev.off()


#Plot maps of the trees added for selected policies----
#worldfile cells
Cells = read.csv('worldfile.csv', stringsAsFactors = FALSE)
coordinates(Cells) = c('patchX', 'patchY')
proj4string(Cells) = CRS('+init=epsg:26918')

#Change all data to degrees
CellsWGS = spTransform(Cells, CRSobj = CRS('+init=epsg:4326'))

#Add MaxGI info
MaxGI = read.csv('MaxGI30m910.csv', stringsAsFactors = FALSE)
CellsWGS$MaxGI = 0
for(p in 1:nrow(MaxGI)){
        CellsWGS$MaxGI[CellsWGS$patchID == MaxGI[p,1]] = MaxGI[p,2]
}
rm(p)

# Up, Mid, Down defined for individual hillslopes
Upslope_h = Midslope_h = Downslope_h = matrix(0, nrow = length(unique(CellsWGS$hillID)), ncol = 2)
for (h in 9:10){
        Upslope_h[h-8,] = c(h, max(CellsWGS$patchZ[which((CellsWGS$MaxGI > 0) & (CellsWGS$hillID == h))])+1)
        Midslope_h[h-8,] = c(h, as.numeric(quantile(CellsWGS$patchZ[which((duplicated(CellsWGS$patchID) == FALSE) & (CellsWGS$hillID == h) & (CellsWGS$MaxGI > 0))], probs = c(0.6667))))
        Downslope_h[h-8,] = c(h, as.numeric(quantile(CellsWGS$patchZ[which((duplicated(CellsWGS$patchID) == FALSE) & (CellsWGS$hillID == h) & (CellsWGS$MaxGI > 0))], probs = c(0.3333))))
}
rm(h)

CellsWGS$Up = CellsWGS$Mid = CellsWGS$Down = 0
CellsWGS$Down[which((CellsWGS$patchZ <= Downslope_h[1,2]) & (CellsWGS$hillID == 9))] = 9
CellsWGS$Mid[which((CellsWGS$patchZ <= Midslope_h[1,2]) & (CellsWGS$patchZ > Downslope_h[1,2]) & (CellsWGS$hillID == 9))] = 9
CellsWGS$Up[which((CellsWGS$patchZ <= Upslope_h[1,2]) & (CellsWGS$patchZ > Midslope_h[1,2]) & (CellsWGS$hillID == 9))] = 9
CellsWGS$Down[which((CellsWGS$patchZ <= Downslope_h[2,2]) & (CellsWGS$hillID == 10))] = 10
CellsWGS$Mid[which((CellsWGS$patchZ <= Midslope_h[2,2]) & (CellsWGS$patchZ > Downslope_h[2,2]) & (CellsWGS$hillID == 10))] = 10
CellsWGS$Up[which((CellsWGS$patchZ <= Upslope_h[2,2]) & (CellsWGS$patchZ > Midslope_h[2,2]) & (CellsWGS$hillID == 10))] = 10

scaleRange = c(0,1)
scaleBy = 0.2
Pal = rev(rainbow((scaleRange[2] - scaleRange[1])/scaleBy))

# Synthetic----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptSynTruth")
#  Best Flooding----
png('BestFloodingMapSyn.png', res = 300, units = 'in', width = 6, height = 6)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
  #For hillslope boundaries
  #plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
  #plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
  for (l in 1:3){
    #Decision variable boundaries - hillslopes - difficult to get to look right
    #if (h == 9){
    #  plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
    #}else{
    #  plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'red', add = TRUE, lwd=7, pch = 22, cex = 1.2)
    #}
    #Decision variable boundaries - hillslopes - difficult to get to look right
    if (l == 1){
      plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
    }else if (l == 2){
      plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, lwd=7, pch = 22, cex = 1.2)
    }else{
      plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'darkorange', add = TRUE, lwd=7, pch = 22, cex = 1.2)
    }
    plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
    plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(BestFloodSyn[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
  }
}
rm(h)
legend('topleft', title = expression(bold('GI Proportion')), legend = c(paste(seq(0,0.8,0.2), '-', seq(0.2,1,0.2))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
#Hillslope legend
#legend('topright', title = 'Decision Variable Area Outlines', legend = c('Hillslope 9', 'Hillslope 10'), col = c('gray', 'red'), pch = 22, pt.cex = 2, pt.lwd = 5)
#Up, Mid, Down legend
legend('topright', title = 'Decision Variable Area Outlines', legend = c('Downslope', 'Midslope', 'Upslope'), col = c('gray', 'black', 'darkorange'), pch = 22, pt.cex = 2, pt.lwd = 5)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

png('BestFloodingMapSyn_HillOutlines.png', res = 300, units = 'in', width = 6, height = 5)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(BestFloodSyn[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('GI Proportion')), legend = c(paste(seq(0,0.8,0.2), '-', seq(0.2,1,0.2))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

#  Best Low Flow----
png('BestLowFlowMapSyn_HillOutlines.png', res = 300, units = 'in', width = 6, height = 5)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(BestLowFlowSyn[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('GI Proportion')), legend = c(paste(seq(0,0.8,0.2), '-', seq(0.2,1,0.2))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

#  Compromise----
png('CompromiseMapSyn_HillOutlines.png', res = 300, units = 'in', width = 6, height = 5)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(CompromiseSyn[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('GI Proportion')), legend = c(paste(seq(0,0.8,0.2), '-', seq(0.2,1,0.2))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

png('CompromiseMapSynMinTrees_HillOutlines.png', res = 300, units = 'in', width = 6, height = 5)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(CompromiseSynMinTrees[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('GI Proportion')), legend = c(paste(seq(0,0.8,0.2), '-', seq(0.2,1,0.2))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

#Average compromise map----
png('CompromiseMapSynMinTrees_HillOutlines_Mean.png', res = 300, units = 'in', width = 6, height = 5)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(colMeans(TreesThreshSyn)[l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('Mean Proportion')), legend = c(paste(seq(0,0.8,0.2), '-', seq(0.2,1,0.2))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

#SD compromise map----
scaleRange = c(0,0.4)
scaleBy = 0.1
Pal = rev(gray.colors((scaleRange[2] - scaleRange[1])/scaleBy))

png('CompromiseMapSynMinTrees_HillOutlines_SD.png', res = 300, units = 'in', width = 6, height = 5)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(apply(TreesThreshSyn,2,sd)[l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('SD Proportion')), legend = c(paste(seq(0,0.3,0.1), '-', seq(0.1,0.4,0.1))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

# MAP----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptMAP")
scaleRange = c(0,1)
scaleBy = 0.2
Pal = rev(rainbow((scaleRange[2] - scaleRange[1])/scaleBy))

#  Best Flooding----
png('BestFloodingMapMAP_HillOutlines.png', res = 300, units = 'in', width = 6, height = 5)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(BestFloodMAP[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('GI Proportion')), legend = c(paste(seq(0,0.8,0.2), '-', seq(0.2,1,0.2))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

#  Best Low Flow----
png('BestLowFlowMapMAP_HillOutlines.png', res = 300, units = 'in', width = 6, height = 5)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(BestLowFlowMAP[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('GI Proportion')), legend = c(paste(seq(0,0.8,0.2), '-', seq(0.2,1,0.2))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

#  Compromise----
png('CompromiseMapMAP_HillOutlines.png', res = 300, units = 'in', width = 6, height = 5)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(CompromiseMAP[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('GI Proportion')), legend = c(paste(seq(0,0.8,0.2), '-', seq(0.2,1,0.2))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

png('CompromiseMapMAPMinTrees_HillOutlines.png', res = 300, units = 'in', width = 6, height = 5)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(CompromiseMAPMinTrees[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('GI Proportion')), legend = c(paste(seq(0,0.8,0.2), '-', seq(0.2,1,0.2))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

#Average compromise map----
png('CompromiseMapMAPMinTrees_HillOutlines_Mean.png', res = 300, units = 'in', width = 6, height = 5)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(colMeans(TreesThreshMAP)[l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('Mean Proportion')), legend = c(paste(seq(0,0.8,0.2), '-', seq(0.2,1,0.2))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

#SD compromise map----
scaleRange = c(0,0.4)
scaleBy = 0.1
Pal = rev(gray.colors((scaleRange[2] - scaleRange[1])/scaleBy))

png('CompromiseMapMAPMinTrees_HillOutlines_SD.png', res = 300, units = 'in', width = 6, height = 5)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(apply(TreesThreshMAP,2,sd)[l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('SD Proportion')), legend = c(paste(seq(0,0.3,0.1), '-', seq(0.1,0.4,0.1))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

# MORO----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptMORO")
scaleRange = c(0,1)
scaleBy = 0.2
Pal = rev(rainbow((scaleRange[2] - scaleRange[1])/scaleBy))

#  Best Flooding----
png('BestFloodingMapMORO_HillOutlines.png', res = 300, units = 'in', width = 6, height = 5)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(BestFloodMORO[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('GI Proportion')), legend = c(paste(seq(0,0.8,0.2), '-', seq(0.2,1,0.2))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

#  Best Low Flow----
png('BestLowFlowMapMORO_HillOutlines.png', res = 300, units = 'in', width = 6, height = 5)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(BestLowFlowMORO[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('GI Proportion')), legend = c(paste(seq(0,0.8,0.2), '-', seq(0.2,1,0.2))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

#  Compromise----
png('BestCompromiseMapMORO_HillOutlines.png', res = 300, units = 'in', width = 6, height = 5)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(CompromiseMORO[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('GI Proportion')), legend = c(paste(seq(0,0.8,0.2), '-', seq(0.2,1,0.2))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

png('BestCompromiseMapMOROMinTrees_HillOutlines.png', res = 300, units = 'in', width = 6, height = 5)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(CompromiseMOROMinTrees[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('GI Proportion')), legend = c(paste(seq(0,0.8,0.2), '-', seq(0.2,1,0.2))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

#Average compromise map----
png('BestCompromiseMapMOROMinTrees_HillOutlines_Mean.png', res = 300, units = 'in', width = 6, height = 5)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(colMeans(TreesThreshMORO)[l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('Mean Proportion')), legend = c(paste(seq(0,0.8,0.2), '-', seq(0.2,1,0.2))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

#SD compromise map----
scaleRange = c(0,0.4)
scaleBy = 0.1
Pal = rev(gray.colors((scaleRange[2] - scaleRange[1])/scaleBy))

png('BestCompromiseMapMOROMinTrees_HillOutlines_SD.png', res = 300, units = 'in', width = 6, height = 5)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(apply(TreesThreshMORO,2,sd)[l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('SD Proportion')), legend = c(paste(seq(0,0.3,0.1), '-', seq(0.1,0.4,0.1))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

# MMO----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptMinMax")
scaleRange = c(0,1)
scaleBy = 0.2
Pal = rev(rainbow((scaleRange[2] - scaleRange[1])/scaleBy))

#  Best Flooding----
png('BestFloodingMapMMO_HillOutlines.png', res = 300, units = 'in', width = 6, height = 5)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(BestFloodMMO[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('GI Proportion')), legend = c(paste(seq(0,0.8,0.2), '-', seq(0.2,1,0.2))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

#  Best Low Flow----
png('BestLowFlowMapMMO_HillOutlines.png', res = 300, units = 'in', width = 6, height = 5)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(BestLowFlowMMO[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('GI Proportion')), legend = c(paste(seq(0,0.8,0.2), '-', seq(0.2,1,0.2))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

#  Compromise----
png('BestCompromiseMapMMO_HillOutlines.png', res = 300, units = 'in', width = 6, height = 5)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(CompromiseMMO[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('GI Proportion')), legend = c(paste(seq(0,0.8,0.2), '-', seq(0.2,1,0.2))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

png('BestCompromiseMapMMOMinTrees_HillOutlines.png', res = 300, units = 'in', width = 6, height = 5)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(CompromiseMMOMinTrees[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('GI Proportion')), legend = c(paste(seq(0,0.8,0.2), '-', seq(0.2,1,0.2))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

#Average compromise map----
png('BestCompromiseMapMMOMinTrees_HillOutlines_Mean.png', res = 300, units = 'in', width = 6, height = 5)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(colMeans(TreesThreshMMO)[l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('Mean Proportion')), legend = c(paste(seq(0,0.8,0.2), '-', seq(0.2,1,0.2))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

#SD compromise map----
scaleRange = c(0,0.4)
scaleBy = 0.1
Pal = rev(gray.colors((scaleRange[2] - scaleRange[1])/scaleBy))

png('BestCompromiseMapMMOMinTrees_HillOutlines_SD.png', res = 300, units = 'in', width = 6, height = 5)
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(apply(TreesThreshMMO,2,sd)[l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('SD Proportion')), legend = c(paste(seq(0,0.3,0.1), '-', seq(0.1,0.4,0.1))), col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()

# paper graphic----
setwd("C:/Users/js4yd/OneDrive - University of Virginia/BES_Data/BES_Data/RHESSysFiles/BR&POBR/SyntheticHill11+12")
png('ParetoSyn_ColTrees_MAP+MORO+MMOsols_linesThresh_arrows.png', res = 300, units = 'in', width = 6, height = 6)
scaleRange = c(0,18000)
scaleBy = 3000
Pal = rev(scico(palette = 'batlow', n = (scaleRange[2] - scaleRange[1])/scaleBy))
plot(-CompromiseSynMinTrees$Flooding*100, -CompromiseSynMinTrees$LowFlow*100, col = colFun(CompromiseSynMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 1.5, cex.lab = 1.5, main = 'Solution Degradation', pch = 16)
par(new=TRUE)
plot(-BestLowFlowSyn$Flooding*100, -BestLowFlowSyn$LowFlow*100, col = colFun(BestLowFlowSyn$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', main = '', axes = FALSE, pch = 16)
par(new=TRUE)
plot(-BestFloodSyn$Flooding*100, -BestFloodSyn$LowFlow*100, col = colFun(BestFloodSyn$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', main = '', axes = FALSE, pch = 16)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
lines(c(20,20), c(-120,120), lty = 2, col = 'gray')
lines(c(-120,120), c(-20,-20), lty = 2, col = 'gray')
#3 MAP solutions
par(new=TRUE)
plot(-BestFloodMAP$Flooding*100, -BestFloodMAP$LowFlow*100, col = colFun(BestFloodMAP$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
par(new=TRUE)
plot(-BestFloodMAP$FloodingSyn*100, -BestFloodMAP$LowFlowSyn*100, col = colFun(BestFloodMAP$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
arrows(x0 = -BestFloodMAP$Flooding*100, y0 = -BestFloodMAP$LowFlow*100, x1 = -BestFloodMAP$FloodingSyn*100, y1 = -BestFloodMAP$LowFlowSyn*100, lwd = 2, length = 0.05)

par(new=TRUE)
plot(-BestLowFlowMAP$Flooding*100, -BestLowFlowMAP$LowFlow*100, col = colFun(BestLowFlowMAP$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
par(new=TRUE)
plot(-BestLowFlowMAP$FloodingSyn*100, -BestLowFlowMAP$LowFlowSyn*100, col = colFun(BestLowFlowMAP$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
arrows(x0 = -BestLowFlowMAP$Flooding*100, y0 = -BestLowFlowMAP$LowFlow*100, x1 = -BestLowFlowMAP$FloodingSyn*100, y1 = -BestLowFlowMAP$LowFlowSyn*100, lwd = 2, length = 0.05)

par(new=TRUE)
plot(-CompromiseMAPMinTrees$Flooding*100, -CompromiseMAPMinTrees$LowFlow*100, col = colFun(CompromiseMAPMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
par(new=TRUE)
plot(-CompromiseMAPMinTrees$FloodingSyn*100, -CompromiseMAPMinTrees$LowFlowSyn*100, col = colFun(CompromiseMAPMinTrees$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15)
arrows(x0 = -CompromiseMAPMinTrees$Flooding*100, y0 = -CompromiseMAPMinTrees$LowFlow*100, x1 = -CompromiseMAPMinTrees$FloodingSyn*100, y1 = -CompromiseMAPMinTrees$LowFlowSyn*100, lwd = 2, length = 0.05)

#3 MORO solutions
par(new=TRUE)
plot(-BestFloodMORO$Flooding*100, -BestFloodMORO$LowFlow*100, col = colFun(BestFloodMORO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
par(new=TRUE)
plot(-BestFloodMORO$FloodingSyn*100, -BestFloodMORO$LowFlowSyn*100, col = colFun(BestFloodMORO$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
arrows(x0 = -BestFloodMORO$Flooding*100, y0 = -BestFloodMORO$LowFlow*100, x1 = -BestFloodMORO$FloodingSyn*100, y1 = -BestFloodMORO$LowFlowSyn*100, lwd = 2, length = 0.05)

par(new=TRUE)
plot(-BestLowFlowMORO$Flooding*100, -BestLowFlowMORO$LowFlow*100, col = colFun(BestLowFlowMORO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
par(new=TRUE)
plot(-BestLowFlowMORO$FloodingSyn*100, -BestLowFlowMORO$LowFlowSyn*100, col = colFun(BestLowFlowMORO$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
arrows(x0 = -BestLowFlowMORO$Flooding*100, y0 = -BestLowFlowMORO$LowFlow*100, x1 = -BestLowFlowMORO$FloodingSyn*100, y1 = -BestLowFlowMORO$LowFlowSyn*100, lwd = 2, length = 0.05)

par(new=TRUE)
plot(-CompromiseMOROMinTrees$Flooding*100, -CompromiseMOROMinTrees$LowFlow*100, col = colFun(CompromiseMOROMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
par(new=TRUE)
plot(-CompromiseMOROMinTrees$FloodingSyn*100, -CompromiseMOROMinTrees$LowFlowSyn*100, col = colFun(CompromiseMOROMinTrees$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17)
arrows(x0 = -CompromiseMOROMinTrees$Flooding*100, y0 = -CompromiseMOROMinTrees$LowFlow*100, x1 = -CompromiseMOROMinTrees$FloodingSyn*100, y1 = -CompromiseMOROMinTrees$LowFlowSyn*100, lwd = 2, length = 0.05)

#3 MMO solutions
par(new=TRUE)
plot(-BestFloodMMO$Flooding*100, -BestFloodMMO$LowFlow*100, col = colFun(BestFloodMMO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
par(new=TRUE)
plot(-BestFloodMMO$FloodingSyn*100, -BestFloodMMO$LowFlowSyn*100, col = colFun(BestFloodMMO$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
arrows(x0 = -BestFloodMMO$Flooding*100, y0 = -BestFloodMMO$LowFlow*100, x1 = -BestFloodMMO$FloodingSyn*100, y1 = -BestFloodMMO$LowFlowSyn*100, lwd = 2, length = 0.05)

par(new=TRUE)
plot(-BestLowFlowMMO$Flooding*100, -BestLowFlowMMO$LowFlow*100, col = colFun(BestLowFlowMMO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
par(new=TRUE)
plot(-BestLowFlowMMO$FloodingSyn*100, -BestLowFlowMMO$LowFlowSyn*100, col = colFun(BestLowFlowMMO$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
arrows(x0 = -BestLowFlowMMO$Flooding*100, y0 = -BestLowFlowMMO$LowFlow*100, x1 = -BestLowFlowMMO$FloodingSyn*100, y1 = -BestLowFlowMMO$LowFlowSyn*100, lwd = 2, length = 0.05)

par(new=TRUE)
plot(-CompromiseMMOMinTrees$Flooding*100, -CompromiseMMOMinTrees$LowFlow*100, col = colFun(CompromiseMMOMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
par(new=TRUE)
plot(-CompromiseMMOMinTrees$FloodingSyn*100, -CompromiseMMOMinTrees$LowFlowSyn*100, col = colFun(CompromiseMMOMinTrees$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18)
arrows(x0 = -CompromiseMMOMinTrees$Flooding*100, y0 = -CompromiseMMOMinTrees$LowFlow*100, x1 = -CompromiseMMOMinTrees$FloodingSyn*100, y1 = -CompromiseMMOMinTrees$LowFlowSyn*100, lwd = 2, length = 0.05)

legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <3', '3 - <6', '6 - <9', '9 - <12', '12 - <15', '>15'), col = colFun(c(seq(0,16000,3000))), pch = c(rep(16,9)), cex=0.7)
legend('topright', title = 'Optimization', 
       legend = c('Synthetic', 'MAP', 'MORO', 'MinMax'), pch = c(16,15,17,18), cex=0.7)
dev.off()

# paper panel----
png('ParetoSolDegradation_CompromiseTreeMaps.png', res = 600, units = 'in', width = 6, height = 4)
layout(rbind(c(1,1,1,2,2,2), c(3,3,4,4,5,5)))

scaleRange = c(0,18000)
scaleBy = 3000
Pal = rev(scico(palette = 'batlow', n = (scaleRange[2] - scaleRange[1])/scaleBy))

par(mar = c(4.1,4.1,3,1))

plot(-CompromiseSynMinTrees$Flooding*100, -CompromiseSynMinTrees$LowFlow*100, col = colFun(CompromiseSynMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 0.7, cex.lab = 1, 
     main = 'Solution Degradation', pch = 16, cex = 0.7)
par(new=TRUE)
plot(-BestLowFlowSyn$Flooding*100, -BestLowFlowSyn$LowFlow*100, col = colFun(BestLowFlowSyn$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', main = '', axes = FALSE, pch = 16, cex = 0.7)
par(new=TRUE)
plot(-BestFloodSyn$Flooding*100, -BestFloodSyn$LowFlow*100, col = colFun(BestFloodSyn$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', main = '', axes = FALSE, pch = 16, cex = 0.7)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
lines(c(20,20), c(-120,120), lty = 2, col = 'gray')
lines(c(-120,120), c(-20,-20), lty = 2, col = 'gray')
#3 MAP solutions
par(new=TRUE)
plot(-BestFloodMAP$Flooding*100, -BestFloodMAP$LowFlow*100, col = colFun(BestFloodMAP$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15, cex = 0.7)
par(new=TRUE)
plot(-BestFloodMAP$FloodingSyn*100, -BestFloodMAP$LowFlowSyn*100, col = colFun(BestFloodMAP$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15, cex = 0.7)
arrows(x0 = -BestFloodMAP$Flooding*100, y0 = -BestFloodMAP$LowFlow*100, x1 = -BestFloodMAP$FloodingSyn*100, 
       y1 = -BestFloodMAP$LowFlowSyn*100, lwd = 0.5, length = 0.025)

par(new=TRUE)
plot(-BestLowFlowMAP$Flooding*100, -BestLowFlowMAP$LowFlow*100, col = colFun(BestLowFlowMAP$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15, cex = 0.7)
par(new=TRUE)
plot(-BestLowFlowMAP$FloodingSyn*100, -BestLowFlowMAP$LowFlowSyn*100, col = colFun(BestLowFlowMAP$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15, cex = 0.7)
arrows(x0 = -BestLowFlowMAP$Flooding*100, y0 = -BestLowFlowMAP$LowFlow*100, x1 = -BestLowFlowMAP$FloodingSyn*100, 
       y1 = -BestLowFlowMAP$LowFlowSyn*100, lwd = 0.5, length = 0.025)

par(new=TRUE)
plot(-CompromiseMAPMinTrees$Flooding*100, -CompromiseMAPMinTrees$LowFlow*100, col = colFun(CompromiseMAPMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15, cex = 0.7)
par(new=TRUE)
plot(-CompromiseMAPMinTrees$FloodingSyn*100, -CompromiseMAPMinTrees$LowFlowSyn*100, col = colFun(CompromiseMAPMinTrees$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15, cex = 0.7)
arrows(x0 = -CompromiseMAPMinTrees$Flooding*100, y0 = -CompromiseMAPMinTrees$LowFlow*100, 
       x1 = -CompromiseMAPMinTrees$FloodingSyn*100, y1 = -CompromiseMAPMinTrees$LowFlowSyn*100, lwd = 0.5, length = 0.025)

#3 MORO solutions
par(new=TRUE)
plot(-BestFloodMORO$Flooding*100, -BestFloodMORO$LowFlow*100, col = colFun(BestFloodMORO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17, cex = 0.7)
par(new=TRUE)
plot(-BestFloodMORO$FloodingSyn*100, -BestFloodMORO$LowFlowSyn*100, col = colFun(BestFloodMORO$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17, cex = 0.7)
arrows(x0 = -BestFloodMORO$Flooding*100, y0 = -BestFloodMORO$LowFlow*100, x1 = -BestFloodMORO$FloodingSyn*100, 
       y1 = -BestFloodMORO$LowFlowSyn*100, lwd = 0.5, length = 0.025)

par(new=TRUE)
plot(-BestLowFlowMORO$Flooding*100, -BestLowFlowMORO$LowFlow*100, col = colFun(BestLowFlowMORO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17, cex = 0.7)
par(new=TRUE)
plot(-BestLowFlowMORO$FloodingSyn*100, -BestLowFlowMORO$LowFlowSyn*100, col = colFun(BestLowFlowMORO$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17, cex = 0.7)
arrows(x0 = -BestLowFlowMORO$Flooding*100, y0 = -BestLowFlowMORO$LowFlow*100, x1 = -BestLowFlowMORO$FloodingSyn*100, 
       y1 = -BestLowFlowMORO$LowFlowSyn*100, lwd = 0.5, length = 0.025)

par(new=TRUE)
plot(-CompromiseMOROMinTrees$Flooding*100, -CompromiseMOROMinTrees$LowFlow*100, col = colFun(CompromiseMOROMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17, cex = 0.7)
par(new=TRUE)
plot(-CompromiseMOROMinTrees$FloodingSyn*100, -CompromiseMOROMinTrees$LowFlowSyn*100, col = colFun(CompromiseMOROMinTrees$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17, cex = 0.7)
arrows(x0 = -CompromiseMOROMinTrees$Flooding*100, y0 = -CompromiseMOROMinTrees$LowFlow*100, 
       x1 = -CompromiseMOROMinTrees$FloodingSyn*100, y1 = -CompromiseMOROMinTrees$LowFlowSyn*100, lwd = 0.5, length = 0.025)

#3 MMO solutions
par(new=TRUE)
plot(-BestFloodMMO$Flooding*100, -BestFloodMMO$LowFlow*100, col = colFun(BestFloodMMO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18, cex = 0.7)
par(new=TRUE)
plot(-BestFloodMMO$FloodingSyn*100, -BestFloodMMO$LowFlowSyn*100, col = colFun(BestFloodMMO$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18, cex = 0.7)
arrows(x0 = -BestFloodMMO$Flooding*100, y0 = -BestFloodMMO$LowFlow*100, x1 = -BestFloodMMO$FloodingSyn*100, 
       y1 = -BestFloodMMO$LowFlowSyn*100, lwd = 0.5, length = 0.025)

par(new=TRUE)
plot(-BestLowFlowMMO$Flooding*100, -BestLowFlowMMO$LowFlow*100, col = colFun(BestLowFlowMMO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18, cex = 0.7)
par(new=TRUE)
plot(-BestLowFlowMMO$FloodingSyn*100, -BestLowFlowMMO$LowFlowSyn*100, col = colFun(BestLowFlowMMO$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18, cex = 0.7)
arrows(x0 = -BestLowFlowMMO$Flooding*100, y0 = -BestLowFlowMMO$LowFlow*100, x1 = -BestLowFlowMMO$FloodingSyn*100, 
       y1 = -BestLowFlowMMO$LowFlowSyn*100, lwd = 0.5, length = 0.025)

par(new=TRUE)
plot(-CompromiseMMOMinTrees$Flooding*100, -CompromiseMMOMinTrees$LowFlow*100, col = colFun(CompromiseMMOMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18, cex = 0.7)
par(new=TRUE)
plot(-CompromiseMMOMinTrees$FloodingSyn*100, -CompromiseMMOMinTrees$LowFlowSyn*100, col = colFun(CompromiseMMOMinTrees$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18, cex = 0.7)
arrows(x0 = -CompromiseMMOMinTrees$Flooding*100, y0 = -CompromiseMMOMinTrees$LowFlow*100, 
       x1 = -CompromiseMMOMinTrees$FloodingSyn*100, y1 = -CompromiseMMOMinTrees$LowFlowSyn*100, lwd = 0.5, length = 0.025)

legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <3', '3 - <6', '6 - <9', '9 - <12', '12 - <15', '>15'), col = colFun(c(seq(0,16000,3000))), pch = c(rep(16,9)), cex=0.5)
legend('topright', title = 'Optimization', 
       legend = c('Synthetic', 'MAP', 'MORO', 'MinMax'), pch = c(16,15,17,18), cex=0.7)
box(which = 'figure', lwd = 2)

scaleRange = c(0,1)
scaleBy = 0.2
Pal = rev(scico(palette = 'oslo', n = (scaleRange[2] - scaleRange[1])/scaleBy))

#Syn
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(CompromiseSynMinTrees[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('GI Proportion')), 
       legend = c(paste(seq(0,0.8,0.2), '-', seq(0.2,1,0.2))), 
       col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15, cex = 0.7)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.482, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
text(x = -76.696, y = 39.4903, 'Compromise:')
text(x = -76.695, y = 39.489, 'Synthetic')

#MAP
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(CompromiseMAPMinTrees[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
box(which = 'figure', lwd = 2)
text(x = -76.698, y = 39.493, 'Compromise:')
text(x = -76.695, y = 39.4915, 'MAP')

#MORO
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(CompromiseMOROMinTrees[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
box(which = 'figure', lwd = 2)
text(x = -76.698, y = 39.493, 'Compromise:')
text(x = -76.695, y = 39.4915, 'MORO')

#MMO
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(CompromiseMMOMinTrees[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
box(which = 'figure', lwd = 2)
text(x = -76.698, y = 39.493, 'Compromise:')
text(x = -76.695, y = 39.4915, 'MMO')
dev.off()

#  pdf ----
pdf('ParetoSolDegradation_CompromiseTreeMaps.pdf', width = 6, height = 4)
layout(rbind(c(1,1,1,2,2,2), c(3,3,4,4,5,5)))

scaleRange = c(0,18000)
scaleBy = 3000
Pal = rev(scico(palette = 'batlow', n = (scaleRange[2] - scaleRange[1])/scaleBy))

par(mar = c(4.1,4.1,3,1))

plot(-CompromiseSynMinTrees$Flooding*100, -CompromiseSynMinTrees$LowFlow*100, col = colFun(CompromiseSynMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = 'Flooding Reduction (%)', ylab = 'Low Flow Change (%)', cex.axis = 0.7, cex.lab = 1, 
     main = 'Solution Degradation', pch = 16, cex = 0.7)
par(new=TRUE)
plot(-BestLowFlowSyn$Flooding*100, -BestLowFlowSyn$LowFlow*100, col = colFun(BestLowFlowSyn$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', main = '', axes = FALSE, pch = 16, cex = 0.7)
par(new=TRUE)
plot(-BestFloodSyn$Flooding*100, -BestFloodSyn$LowFlow*100, col = colFun(BestFloodSyn$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', main = '', axes = FALSE, pch = 16, cex = 0.7)
lines(c(-110,110), c(0,0))
lines(c(-0,0), c(-120,120))
lines(c(20,20), c(-120,120), lty = 2, col = 'gray')
lines(c(-120,120), c(-20,-20), lty = 2, col = 'gray')
#3 MAP solutions
par(new=TRUE)
plot(-BestFloodMAP$Flooding*100, -BestFloodMAP$LowFlow*100, col = colFun(BestFloodMAP$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15, cex = 0.7)
par(new=TRUE)
plot(-BestFloodMAP$FloodingSyn*100, -BestFloodMAP$LowFlowSyn*100, col = colFun(BestFloodMAP$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15, cex = 0.7)
arrows(x0 = -BestFloodMAP$Flooding*100, y0 = -BestFloodMAP$LowFlow*100, x1 = -BestFloodMAP$FloodingSyn*100, 
       y1 = -BestFloodMAP$LowFlowSyn*100, lwd = 0.5, length = 0.025)

par(new=TRUE)
plot(-BestLowFlowMAP$Flooding*100, -BestLowFlowMAP$LowFlow*100, col = colFun(BestLowFlowMAP$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15, cex = 0.7)
par(new=TRUE)
plot(-BestLowFlowMAP$FloodingSyn*100, -BestLowFlowMAP$LowFlowSyn*100, col = colFun(BestLowFlowMAP$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15, cex = 0.7)
arrows(x0 = -BestLowFlowMAP$Flooding*100, y0 = -BestLowFlowMAP$LowFlow*100, x1 = -BestLowFlowMAP$FloodingSyn*100, 
       y1 = -BestLowFlowMAP$LowFlowSyn*100, lwd = 0.5, length = 0.025)

par(new=TRUE)
plot(-CompromiseMAPMinTrees$Flooding*100, -CompromiseMAPMinTrees$LowFlow*100, col = colFun(CompromiseMAPMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15, cex = 0.7)
par(new=TRUE)
plot(-CompromiseMAPMinTrees$FloodingSyn*100, -CompromiseMAPMinTrees$LowFlowSyn*100, col = colFun(CompromiseMAPMinTrees$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 15, cex = 0.7)
arrows(x0 = -CompromiseMAPMinTrees$Flooding*100, y0 = -CompromiseMAPMinTrees$LowFlow*100, 
       x1 = -CompromiseMAPMinTrees$FloodingSyn*100, y1 = -CompromiseMAPMinTrees$LowFlowSyn*100, lwd = 0.5, length = 0.025)

#3 MORO solutions
par(new=TRUE)
plot(-BestFloodMORO$Flooding*100, -BestFloodMORO$LowFlow*100, col = colFun(BestFloodMORO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17, cex = 0.7)
par(new=TRUE)
plot(-BestFloodMORO$FloodingSyn*100, -BestFloodMORO$LowFlowSyn*100, col = colFun(BestFloodMORO$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17, cex = 0.7)
arrows(x0 = -BestFloodMORO$Flooding*100, y0 = -BestFloodMORO$LowFlow*100, x1 = -BestFloodMORO$FloodingSyn*100, 
       y1 = -BestFloodMORO$LowFlowSyn*100, lwd = 0.5, length = 0.025)

par(new=TRUE)
plot(-BestLowFlowMORO$Flooding*100, -BestLowFlowMORO$LowFlow*100, col = colFun(BestLowFlowMORO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17, cex = 0.7)
par(new=TRUE)
plot(-BestLowFlowMORO$FloodingSyn*100, -BestLowFlowMORO$LowFlowSyn*100, col = colFun(BestLowFlowMORO$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17, cex = 0.7)
arrows(x0 = -BestLowFlowMORO$Flooding*100, y0 = -BestLowFlowMORO$LowFlow*100, x1 = -BestLowFlowMORO$FloodingSyn*100, 
       y1 = -BestLowFlowMORO$LowFlowSyn*100, lwd = 0.5, length = 0.025)

par(new=TRUE)
plot(-CompromiseMOROMinTrees$Flooding*100, -CompromiseMOROMinTrees$LowFlow*100, col = colFun(CompromiseMOROMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17, cex = 0.7)
par(new=TRUE)
plot(-CompromiseMOROMinTrees$FloodingSyn*100, -CompromiseMOROMinTrees$LowFlowSyn*100, col = colFun(CompromiseMOROMinTrees$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 17, cex = 0.7)
arrows(x0 = -CompromiseMOROMinTrees$Flooding*100, y0 = -CompromiseMOROMinTrees$LowFlow*100, 
       x1 = -CompromiseMOROMinTrees$FloodingSyn*100, y1 = -CompromiseMOROMinTrees$LowFlowSyn*100, lwd = 0.5, length = 0.025)

#3 MMO solutions
par(new=TRUE)
plot(-BestFloodMMO$Flooding*100, -BestFloodMMO$LowFlow*100, col = colFun(BestFloodMMO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18, cex = 0.7)
par(new=TRUE)
plot(-BestFloodMMO$FloodingSyn*100, -BestFloodMMO$LowFlowSyn*100, col = colFun(BestFloodMMO$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18, cex = 0.7)
arrows(x0 = -BestFloodMMO$Flooding*100, y0 = -BestFloodMMO$LowFlow*100, x1 = -BestFloodMMO$FloodingSyn*100, 
       y1 = -BestFloodMMO$LowFlowSyn*100, lwd = 0.5, length = 0.025)

par(new=TRUE)
plot(-BestLowFlowMMO$Flooding*100, -BestLowFlowMMO$LowFlow*100, col = colFun(BestLowFlowMMO$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18, cex = 0.7)
par(new=TRUE)
plot(-BestLowFlowMMO$FloodingSyn*100, -BestLowFlowMMO$LowFlowSyn*100, col = colFun(BestLowFlowMMO$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18, cex = 0.7)
arrows(x0 = -BestLowFlowMMO$Flooding*100, y0 = -BestLowFlowMMO$LowFlow*100, x1 = -BestLowFlowMMO$FloodingSyn*100, 
       y1 = -BestLowFlowMMO$LowFlowSyn*100, lwd = 0.5, length = 0.025)

par(new=TRUE)
plot(-CompromiseMMOMinTrees$Flooding*100, -CompromiseMMOMinTrees$LowFlow*100, col = colFun(CompromiseMMOMinTrees$NumTrees),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18, cex = 0.7)
par(new=TRUE)
plot(-CompromiseMMOMinTrees$FloodingSyn*100, -CompromiseMMOMinTrees$LowFlowSyn*100, col = colFun(CompromiseMMOMinTrees$NumTreesSyn),
     xlim = c(0, 60), ylim = ylims, xlab = '', ylab = '', axes=FALSE, pch = 18, cex = 0.7)
arrows(x0 = -CompromiseMMOMinTrees$Flooding*100, y0 = -CompromiseMMOMinTrees$LowFlow*100, 
       x1 = -CompromiseMMOMinTrees$FloodingSyn*100, y1 = -CompromiseMMOMinTrees$LowFlowSyn*100, lwd = 0.5, length = 0.025)

legend('bottomleft', title = 'Num. Trees (thous.)', 
       legend = c('0 - <3', '3 - <6', '6 - <9', '9 - <12', '12 - <15', '>15'), col = colFun(c(seq(0,16000,3000))), pch = c(rep(16,9)), cex=0.5)
legend('topright', title = 'Optimization', 
       legend = c('Synthetic', 'MAP', 'MORO', 'MinMax'), pch = c(16,15,17,18), cex=0.7)
box(which = 'figure', lwd = 2)

scaleRange = c(0,1)
scaleBy = 0.2
Pal = rev(scico(palette = 'oslo', n = (scaleRange[2] - scaleRange[1])/scaleBy))

#Syn
par(mar= c(2.5,2.5,1,1))
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(CompromiseSynMinTrees[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
legend('topleft', title = expression(bold('GI Proportion')), 
       legend = c(paste(seq(0,0.8,0.2), '-', seq(0.2,1,0.2))), 
       col = c(colFun(seq(scaleRange[1],scaleRange[2],scaleBy))), pch = 15, cex = 0.7)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.482, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.694, y = 39.480, 'WGS84')
box(which = 'figure', lwd = 2)
text(x = -76.696, y = 39.4903, 'Compromise:')
text(x = -76.695, y = 39.489, 'Synthetic')

#MAP
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(CompromiseMAPMinTrees[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
box(which = 'figure', lwd = 2)
text(x = -76.698, y = 39.493, 'Compromise:')
text(x = -76.695, y = 39.4915, 'MAP')

#MORO
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(CompromiseMOROMinTrees[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
box(which = 'figure', lwd = 2)
text(x = -76.698, y = 39.493, 'Compromise:')
text(x = -76.695, y = 39.4915, 'MORO')

#MMO
plot(CellsWGS, col = 'black', pch = 15, lwd = 0, cex=1.2)
for (h in 9:10){
        #For hillslope boundaries
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'gray', add = TRUE, lwd=7, pch = 22, cex = 1.2)
        plot(CellsWGS[which((CellsWGS$hillID == h)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
        for (l in 1:3){
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = 'black', add = TRUE, pch = 15, cex = 1.2)
                plot(CellsWGS[which((CellsWGS$hillID == h) & (CellsWGS@data[,116+l] == h) & (CellsWGS$MaxGI > 0)),], col = colFun(CompromiseMMOMinTrees[1,l+3*(h-9)]), add = TRUE, pch = 15, cex = 1.2)
        }
}
rm(h)
degAxis(side = 1, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 1, at = seq(-77,-76,.01))
degAxis(side = 3, at = seq(-77,-76,.005), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.005))
degAxis(side = 4, at = seq(39.45, 40,.005), labels = FALSE)
north.arrow(xb = -76.694, yb = 39.481, len = .0005, lab = 'N', tcol = 'black', col='black')
box(which = 'figure', lwd = 2)
text(x = -76.698, y = 39.493, 'Compromise:')
text(x = -76.695, y = 39.4915, 'MMO')
dev.off()


#Plot timeseries of flow, ET, and saturation deficit for the selected policies----
# Synthetic----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptSynTruth")
#  Best Flooding----
#Load timeseries
Q_SynFlood = vroom('BestFloodingSyn_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
#Make a new Date column
Q_SynFlood$Date = as.Date(paste0(Q_SynFlood$year, '-', Q_SynFlood$month, '-', Q_SynFlood$day))
#Trim off spin-up years
Q_SynFlood = Q_SynFlood[which(as.Date(Q_SynFlood$Date) >= as.Date('2004-10-01')),]
#Convert simulated streamflow to cfs units
Q_SynFlood$streamflow = round(Q_SynFlood$streamflow*conversion_b, 6)
#Make an evaporation + transpiration column
Q_SynFlood$ET = Q_SynFlood$evap + Q_SynFlood$trans
#Retain only streamflow, sat def, detention storage, ET, and Date columns for space
Q_SynFlood = as.data.frame(Q_SynFlood[,c('Date', 'streamflow', 'sat_def', 'ET')])

#Add residuals
Q_SynFlood$streamflowResid = Q_SynFlood$streamflow + Resid

png(paste0('Q_BestFloodSyn.png'), res = 300, height = 5, width=5, units = 'in')
plot(x = as.Date(Q_SynFlood$Date), y = Q_SynFlood$streamflowResid, xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
lines(x = c(0,100000), y = c(q05_cal,q05_cal), col = 'blue', lty = 2, lwd = 2)
lines(x = c(0,100000), y = c(q95_cal,q95_cal), col = 'red', lty = 2, lwd = 2)
dev.off()

png(paste0('SatDef_BestFloodSyn.png'), res = 300, height = 5, width=5, units = 'in')
plot(x = as.Date(Q_SynFlood$Date), y = Q_SynFlood$sat_def, xlab = 'Year', ylab = 'Saturation Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,5000,50), labels = TRUE, cex.axis = 1.5)
dev.off()

#  Best Low Flow----
#Load timeseries
Q_SynLowFlow = vroom('BestLowFlowSyn_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
#Make a new Date column
Q_SynLowFlow$Date = as.Date(paste0(Q_SynLowFlow$year, '-', Q_SynLowFlow$month, '-', Q_SynLowFlow$day))
#Trim off spin-up years
Q_SynLowFlow = Q_SynLowFlow[which(as.Date(Q_SynLowFlow$Date) >= as.Date('2004-10-01')),]
#Convert simulated streamflow to cfs units
Q_SynLowFlow$streamflow = round(Q_SynLowFlow$streamflow*conversion_b, 6)
#Make an evaporation + transpiration column
Q_SynLowFlow$ET = Q_SynLowFlow$evap + Q_SynLowFlow$trans
#Retain only streamflow, sat def, detention storage, ET, and Date columns for space
Q_SynLowFlow = as.data.frame(Q_SynLowFlow[,c('Date', 'streamflow', 'sat_def', 'ET')])

#Add residuals
Q_SynLowFlow$streamflowResid = Q_SynLowFlow$streamflow + Resid

png(paste0('Q_BestLowFlowSyn.png'), res = 300, height = 5, width=5, units = 'in')
plot(x = as.Date(Q_SynLowFlow$Date), y = Q_SynLowFlow$streamflowResid, xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
lines(x = c(0,100000), y = c(q05_cal,q05_cal), col = 'blue', lty = 2, lwd = 2)
lines(x = c(0,100000), y = c(q95_cal,q95_cal), col = 'red', lty = 2, lwd = 2)
dev.off()

png(paste0('SatDef_BestLowFlowSyn.png'), res = 300, height = 5, width=5, units = 'in')
plot(x = as.Date(Q_SynLowFlow$Date), y = Q_SynLowFlow$sat_def, xlab = 'Year', ylab = 'Saturation Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,5000,50), labels = TRUE, cex.axis = 1.5)
dev.off()

#  Compromise----
#Load timeseries
Q_SynComp = vroom('Compromise_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
#Make a new Date column
Q_SynComp$Date = as.Date(paste0(Q_SynComp$year, '-', Q_SynComp$month, '-', Q_SynComp$day))
#Trim off spin-up years
Q_SynComp = Q_SynComp[which(as.Date(Q_SynComp$Date) >= as.Date('2004-10-01')),]
#Convert simulated streamflow to cfs units
Q_SynComp$streamflow = round(Q_SynComp$streamflow*conversion_b, 6)
#Make an evaporation + transpiration column
Q_SynComp$ET = Q_SynComp$evap + Q_SynComp$trans
#Retain only streamflow, sat def, detention storage, ET, and Date columns for space
Q_SynComp = as.data.frame(Q_SynComp[,c('Date', 'streamflow', 'sat_def', 'ET')])

#Add residuals
Q_SynComp$streamflowResid = Q_SynComp$streamflow + Resid

png(paste0('Q_CompromiseSyn.png'), res = 300, height = 5, width=5, units = 'in')
plot(x = as.Date(Q_SynComp$Date), y = Q_SynComp$streamflowResid, xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
lines(x = c(0,100000), y = c(q05_cal,q05_cal), col = 'blue', lty = 2, lwd = 2)
lines(x = c(0,100000), y = c(q95_cal,q95_cal), col = 'red', lty = 2, lwd = 2)
dev.off()

png(paste0('SatDef_CompromiseSyn.png'), res = 300, height = 5, width=5, units = 'in')
plot(x = as.Date(Q_SynComp$Date), y = Q_SynComp$sat_def, xlab = 'Year', ylab = 'Saturation Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,5000,50), labels = TRUE, cex.axis = 1.5)
dev.off()

#  Combine all 3----
png(paste0('Q_AllSyn.png'), res = 300, height = 5, width=5, units = 'in')
plot(x = as.Date(Q_SynLowFlow$Date), y = Q_SynLowFlow$streamflowResid, xlab = '', ylab = '', type = 'l', axes=FALSE, cex.lab = 1.5, ylim = c(0,12), col = 'red')
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,5000,2), labels = TRUE, cex.axis = 1.5)
par(new=TRUE)
plot(x = as.Date(Q_SynComp$Date), y = Q_SynComp$streamflowResid, xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5, ylim = c(0,12))
par(new=TRUE)
plot(x = as.Date(Q_SynFlood$Date), y = Q_SynFlood$streamflowResid, xlab = '', ylab = '', type = 'l', axes=FALSE, cex.lab = 1.5, ylim = c(0,12), col = 'blue')
dev.off()

png(paste0('SatDef_AllSyn.png'), res = 300, height = 5, width=5, units = 'in')
plot(x = as.Date(Q_SynComp$Date), y = Q_SynComp$sat_def, xlab = 'Year', ylab = 'Saturation Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5, ylim = c(1700,2300))
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,5000,50), labels = TRUE, cex.axis = 1.5)
par(new=TRUE)
plot(x = as.Date(Q_SynFlood$Date), y = Q_SynFlood$sat_def, xlab = '', ylab = '', type = 'l', axes=FALSE, cex.lab = 1.5, ylim = c(1700,2300), col = 'blue')
par(new=TRUE)
plot(x = as.Date(Q_SynLowFlow$Date), y = Q_SynLowFlow$sat_def, xlab = '', ylab = '', type = 'l', axes=FALSE, cex.lab = 1.5, ylim = c(1700,2300), col = 'red')
dev.off()

# # MAP----
# #  Best Flooding----
# 
# #  Best Low Flow----
# 
# #  Compromise----
# 
# # MORO----
# setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptMORO")
# #  Best Flooding----
# #Load timeseries
# Q_MOROFlood1 = vroom('Run8346_P1_M1_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
# Q_MOROFlood2 = vroom('Run8346_P2_M1_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
# Q_MOROFlood3 = vroom('Run8346_P3_M1_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
# Q_MOROFlood4 = vroom('Run8346_P4_M1_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
# Q_MOROFlood5 = vroom('Run8346_P5_M1_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
# Q_MOROFlood6 = vroom('Run8346_P6_M1_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
# Q_MOROFlood7 = vroom('Run8346_P7_M1_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
# Q_MOROFlood8 = vroom('Run8346_P8_M1_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
# Q_MOROFlood9 = vroom('Run8346_P9_M1_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
# 
# #Make a new Date column
# Q_MOROFlood1$Date = as.Date(paste0(Q_MOROFlood1$year, '-', Q_MOROFlood1$month, '-', Q_MOROFlood1$day))
# Q_MOROFlood2$Date = as.Date(paste0(Q_MOROFlood2$year, '-', Q_MOROFlood2$month, '-', Q_MOROFlood2$day))
# Q_MOROFlood3$Date = as.Date(paste0(Q_MOROFlood3$year, '-', Q_MOROFlood3$month, '-', Q_MOROFlood3$day))
# Q_MOROFlood4$Date = as.Date(paste0(Q_MOROFlood4$year, '-', Q_MOROFlood4$month, '-', Q_MOROFlood4$day))
# Q_MOROFlood5$Date = as.Date(paste0(Q_MOROFlood5$year, '-', Q_MOROFlood5$month, '-', Q_MOROFlood5$day))
# Q_MOROFlood6$Date = as.Date(paste0(Q_MOROFlood6$year, '-', Q_MOROFlood6$month, '-', Q_MOROFlood6$day))
# Q_MOROFlood7$Date = as.Date(paste0(Q_MOROFlood7$year, '-', Q_MOROFlood7$month, '-', Q_MOROFlood7$day))
# Q_MOROFlood8$Date = as.Date(paste0(Q_MOROFlood8$year, '-', Q_MOROFlood8$month, '-', Q_MOROFlood8$day))
# Q_MOROFlood9$Date = as.Date(paste0(Q_MOROFlood9$year, '-', Q_MOROFlood9$month, '-', Q_MOROFlood9$day))
# 
# #Trim off spin-up years
# Q_MOROFlood1 = Q_MOROFlood1[which(as.Date(Q_MOROFlood1$Date) >= as.Date('2004-10-01')),]
# Q_MOROFlood2 = Q_MOROFlood2[which(as.Date(Q_MOROFlood2$Date) >= as.Date('2004-10-01')),]
# Q_MOROFlood3 = Q_MOROFlood3[which(as.Date(Q_MOROFlood3$Date) >= as.Date('2004-10-01')),]
# Q_MOROFlood4 = Q_MOROFlood4[which(as.Date(Q_MOROFlood4$Date) >= as.Date('2004-10-01')),]
# Q_MOROFlood5 = Q_MOROFlood5[which(as.Date(Q_MOROFlood5$Date) >= as.Date('2004-10-01')),]
# Q_MOROFlood6 = Q_MOROFlood6[which(as.Date(Q_MOROFlood6$Date) >= as.Date('2004-10-01')),]
# Q_MOROFlood7 = Q_MOROFlood7[which(as.Date(Q_MOROFlood7$Date) >= as.Date('2004-10-01')),]
# Q_MOROFlood8 = Q_MOROFlood8[which(as.Date(Q_MOROFlood8$Date) >= as.Date('2004-10-01')),]
# Q_MOROFlood9 = Q_MOROFlood9[which(as.Date(Q_MOROFlood9$Date) >= as.Date('2004-10-01')),]
# 
# #Convert simulated streamflow to cfs units
# Q_MOROFlood1$streamflow = round(Q_MOROFlood1$streamflow*conversion_b, 6)
# Q_MOROFlood2$streamflow = round(Q_MOROFlood2$streamflow*conversion_b, 6)
# Q_MOROFlood3$streamflow = round(Q_MOROFlood3$streamflow*conversion_b, 6)
# Q_MOROFlood4$streamflow = round(Q_MOROFlood4$streamflow*conversion_b, 6)
# Q_MOROFlood5$streamflow = round(Q_MOROFlood5$streamflow*conversion_b, 6)
# Q_MOROFlood6$streamflow = round(Q_MOROFlood6$streamflow*conversion_b, 6)
# Q_MOROFlood7$streamflow = round(Q_MOROFlood7$streamflow*conversion_b, 6)
# Q_MOROFlood8$streamflow = round(Q_MOROFlood8$streamflow*conversion_b, 6)
# Q_MOROFlood9$streamflow = round(Q_MOROFlood9$streamflow*conversion_b, 6)
# 
# #Make an evaporation + transpiration column
# Q_MOROFlood1$ET = Q_MOROFlood1$evap + Q_MOROFlood1$trans
# Q_MOROFlood2$ET = Q_MOROFlood2$evap + Q_MOROFlood2$trans
# Q_MOROFlood3$ET = Q_MOROFlood3$evap + Q_MOROFlood3$trans
# Q_MOROFlood4$ET = Q_MOROFlood4$evap + Q_MOROFlood4$trans
# Q_MOROFlood5$ET = Q_MOROFlood5$evap + Q_MOROFlood5$trans
# Q_MOROFlood6$ET = Q_MOROFlood6$evap + Q_MOROFlood6$trans
# Q_MOROFlood7$ET = Q_MOROFlood7$evap + Q_MOROFlood7$trans
# Q_MOROFlood8$ET = Q_MOROFlood8$evap + Q_MOROFlood8$trans
# Q_MOROFlood9$ET = Q_MOROFlood9$evap + Q_MOROFlood9$trans
# 
# #Retain only streamflow, sat def, detention storage, ET, and Date columns for space
# Q_MOROFlood1 = as.data.frame(Q_MOROFlood1[,c('Date', 'streamflow', 'sat_def', 'ET')])
# Q_MOROFlood2 = as.data.frame(Q_MOROFlood2[,c('Date', 'streamflow', 'sat_def', 'ET')])
# Q_MOROFlood3 = as.data.frame(Q_MOROFlood3[,c('Date', 'streamflow', 'sat_def', 'ET')])
# Q_MOROFlood4 = as.data.frame(Q_MOROFlood4[,c('Date', 'streamflow', 'sat_def', 'ET')])
# Q_MOROFlood5 = as.data.frame(Q_MOROFlood5[,c('Date', 'streamflow', 'sat_def', 'ET')])
# Q_MOROFlood6 = as.data.frame(Q_MOROFlood6[,c('Date', 'streamflow', 'sat_def', 'ET')])
# Q_MOROFlood7 = as.data.frame(Q_MOROFlood7[,c('Date', 'streamflow', 'sat_def', 'ET')])
# Q_MOROFlood8 = as.data.frame(Q_MOROFlood8[,c('Date', 'streamflow', 'sat_def', 'ET')])
# Q_MOROFlood9 = as.data.frame(Q_MOROFlood9[,c('Date', 'streamflow', 'sat_def', 'ET')])
# 
# #Fixme: Add residuals for each of the 9 simulations
# #Q_MOROFlood$streamflowResid = Q_MOROFlood$streamflow + Resid
# 
# png(paste0('Q_BestFloodSyn.png'), res = 300, height = 5, width=5, units = 'in')
# plot(x = as.Date(Q_MOROFlood$Date), y = Q_MOROFlood$streamflowResid, xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
# box()
# axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
# axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
# #Add quantiles
# lines(x = c(0,100000), y = c(q05_cal,q05_cal), col = 'blue', lty = 2, lwd = 2)
# lines(x = c(0,100000), y = c(q95_cal,q95_cal), col = 'red', lty = 2, lwd = 2)
# dev.off()
# 
# png(paste0('SatDef_BestFloodSyn.png'), res = 300, height = 5, width=5, units = 'in')
# plot(x = as.Date(Q_MOROFlood$Date), y = Q_MOROFlood$sat_def, xlab = 'Year', ylab = 'Saturation Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
# box()
# axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
# axis(2, at = seq(0,5000,50), labels = TRUE, cex.axis = 1.5)
# dev.off()
# 
# #  Best Low Flow----
# #Load timeseries
# Q_MOROLowFlow = vroom('BestLowFlowSyn_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
# #Make a new Date column
# Q_MOROLowFlow$Date = as.Date(paste0(Q_MOROLowFlow$year, '-', Q_MOROLowFlow$month, '-', Q_MOROLowFlow$day))
# #Trim off spin-up years
# Q_MOROLowFlow = Q_MOROLowFlow[which(as.Date(Q_MOROLowFlow$Date) >= as.Date('2004-10-01')),]
# #Convert simulated streamflow to cfs units
# Q_MOROLowFlow$streamflow = round(Q_MOROLowFlow$streamflow*conversion_b, 6)
# #Make an evaporation + transpiration column
# Q_MOROLowFlow$ET = Q_MOROLowFlow$evap + Q_MOROLowFlow$trans
# #Retain only streamflow, sat def, detention storage, ET, and Date columns for space
# Q_MOROLowFlow = as.data.frame(Q_MOROLowFlow[,c('Date', 'streamflow', 'sat_def', 'ET')])
# 
# #Add residuals
# Q_MOROLowFlow$streamflowResid = Q_MOROLowFlow$streamflow + Resid
# 
# png(paste0('Q_BestLowFlowSyn.png'), res = 300, height = 5, width=5, units = 'in')
# plot(x = as.Date(Q_MOROLowFlow$Date), y = Q_MOROLowFlow$streamflowResid, xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
# box()
# axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
# axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
# #Add quantiles
# lines(x = c(0,100000), y = c(q05_cal,q05_cal), col = 'blue', lty = 2, lwd = 2)
# lines(x = c(0,100000), y = c(q95_cal,q95_cal), col = 'red', lty = 2, lwd = 2)
# dev.off()
# 
# png(paste0('SatDef_BestLowFlowSyn.png'), res = 300, height = 5, width=5, units = 'in')
# plot(x = as.Date(Q_MOROLowFlow$Date), y = Q_MOROLowFlow$sat_def, xlab = 'Year', ylab = 'Saturation Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
# box()
# axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
# axis(2, at = seq(0,5000,50), labels = TRUE, cex.axis = 1.5)
# dev.off()
# 
# #  Compromise----
# #Load timeseries
# Q_MOROComp = vroom('Compromise_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
# #Make a new Date column
# Q_MOROComp$Date = as.Date(paste0(Q_MOROComp$year, '-', Q_MOROComp$month, '-', Q_MOROComp$day))
# #Trim off spin-up years
# Q_MOROComp = Q_MOROComp[which(as.Date(Q_MOROComp$Date) >= as.Date('2004-10-01')),]
# #Convert simulated streamflow to cfs units
# Q_MOROComp$streamflow = round(Q_MOROComp$streamflow*conversion_b, 6)
# #Make an evaporation + transpiration column
# Q_MOROComp$ET = Q_MOROComp$evap + Q_MOROComp$trans
# #Retain only streamflow, sat def, detention storage, ET, and Date columns for space
# Q_MOROComp = as.data.frame(Q_MOROComp[,c('Date', 'streamflow', 'sat_def', 'ET')])
# 
# #Add residuals
# Q_MOROComp$streamflowResid = Q_MOROComp$streamflow + Resid
# 
# png(paste0('Q_CompromiseSyn.png'), res = 300, height = 5, width=5, units = 'in')
# plot(x = as.Date(Q_MOROComp$Date), y = Q_MOROComp$streamflowResid, xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
# box()
# axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
# axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
# #Add quantiles
# lines(x = c(0,100000), y = c(q05_cal,q05_cal), col = 'blue', lty = 2, lwd = 2)
# lines(x = c(0,100000), y = c(q95_cal,q95_cal), col = 'red', lty = 2, lwd = 2)
# dev.off()
# 
# png(paste0('SatDef_CompromiseSyn.png'), res = 300, height = 5, width=5, units = 'in')
# plot(x = as.Date(Q_MOROComp$Date), y = Q_MOROComp$sat_def, xlab = 'Year', ylab = 'Saturation Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
# box()
# axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
# axis(2, at = seq(0,5000,50), labels = TRUE, cex.axis = 1.5)
# dev.off()
# 
# #  Combine all 3----
# png(paste0('Q_AllSyn.png'), res = 300, height = 5, width=5, units = 'in')
# plot(x = as.Date(Q_MOROLowFlow$Date), y = Q_MOROLowFlow$streamflowResid, xlab = '', ylab = '', type = 'l', axes=FALSE, cex.lab = 1.5, ylim = c(0,12), col = 'red')
# box()
# axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
# axis(2, at = seq(0,5000,2), labels = TRUE, cex.axis = 1.5)
# par(new=TRUE)
# plot(x = as.Date(Q_MOROComp$Date), y = Q_MOROComp$streamflowResid, xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5, ylim = c(0,12))
# par(new=TRUE)
# plot(x = as.Date(Q_MOROFlood$Date), y = Q_MOROFlood$streamflowResid, xlab = '', ylab = '', type = 'l', axes=FALSE, cex.lab = 1.5, ylim = c(0,12), col = 'blue')
# dev.off()
# 
# png(paste0('SatDef_AllSyn.png'), res = 300, height = 5, width=5, units = 'in')
# plot(x = as.Date(Q_MOROComp$Date), y = Q_MOROComp$sat_def, xlab = 'Year', ylab = 'Saturation Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5, ylim = c(1700,2300))
# box()
# axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
# axis(2, at = seq(0,5000,50), labels = TRUE, cex.axis = 1.5)
# par(new=TRUE)
# plot(x = as.Date(Q_MOROFlood$Date), y = Q_MOROFlood$sat_def, xlab = '', ylab = '', type = 'l', axes=FALSE, cex.lab = 1.5, ylim = c(1700,2300), col = 'blue')
# par(new=TRUE)
# plot(x = as.Date(Q_MOROLowFlow$Date), y = Q_MOROLowFlow$sat_def, xlab = '', ylab = '', type = 'l', axes=FALSE, cex.lab = 1.5, ylim = c(1700,2300), col = 'red')
# dev.off()
# # MMO----
# setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptMinMax")
# #  Best Flooding----
# #Load timeseries
# Q_MOROFlood1 = vroom('Run8277_P1_M0_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
# Q_MOROFlood2 = vroom('Run8277_P2_M0_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
# Q_MOROFlood3 = vroom('Run8277_P3_M0_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
# Q_MOROFlood4 = vroom('Run8277_P4_M0_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
# Q_MOROFlood5 = vroom('Run8277_P5_M0_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
# Q_MOROFlood6 = vroom('Run8277_P6_M0_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
# Q_MOROFlood7 = vroom('Run8277_P7_M0_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
# Q_MOROFlood8 = vroom('Run8277_P8_M0_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
# Q_MOROFlood9 = vroom('Run8277_P9_M0_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
# 
# #Make a new Date column
# Q_MOROFlood1$Date = as.Date(paste0(Q_MOROFlood1$year, '-', Q_MOROFlood1$month, '-', Q_MOROFlood1$day))
# Q_MOROFlood2$Date = as.Date(paste0(Q_MOROFlood2$year, '-', Q_MOROFlood2$month, '-', Q_MOROFlood2$day))
# Q_MOROFlood3$Date = as.Date(paste0(Q_MOROFlood3$year, '-', Q_MOROFlood3$month, '-', Q_MOROFlood3$day))
# Q_MOROFlood4$Date = as.Date(paste0(Q_MOROFlood4$year, '-', Q_MOROFlood4$month, '-', Q_MOROFlood4$day))
# Q_MOROFlood5$Date = as.Date(paste0(Q_MOROFlood5$year, '-', Q_MOROFlood5$month, '-', Q_MOROFlood5$day))
# Q_MOROFlood6$Date = as.Date(paste0(Q_MOROFlood6$year, '-', Q_MOROFlood6$month, '-', Q_MOROFlood6$day))
# Q_MOROFlood7$Date = as.Date(paste0(Q_MOROFlood7$year, '-', Q_MOROFlood7$month, '-', Q_MOROFlood7$day))
# Q_MOROFlood8$Date = as.Date(paste0(Q_MOROFlood8$year, '-', Q_MOROFlood8$month, '-', Q_MOROFlood8$day))
# Q_MOROFlood9$Date = as.Date(paste0(Q_MOROFlood9$year, '-', Q_MOROFlood9$month, '-', Q_MOROFlood9$day))
# 
# #Trim off spin-up years
# Q_MOROFlood1 = Q_MOROFlood1[which(as.Date(Q_MOROFlood1$Date) >= as.Date('2004-10-01')),]
# Q_MOROFlood2 = Q_MOROFlood2[which(as.Date(Q_MOROFlood2$Date) >= as.Date('2004-10-01')),]
# Q_MOROFlood3 = Q_MOROFlood3[which(as.Date(Q_MOROFlood3$Date) >= as.Date('2004-10-01')),]
# Q_MOROFlood4 = Q_MOROFlood4[which(as.Date(Q_MOROFlood4$Date) >= as.Date('2004-10-01')),]
# Q_MOROFlood5 = Q_MOROFlood5[which(as.Date(Q_MOROFlood5$Date) >= as.Date('2004-10-01')),]
# Q_MOROFlood6 = Q_MOROFlood6[which(as.Date(Q_MOROFlood6$Date) >= as.Date('2004-10-01')),]
# Q_MOROFlood7 = Q_MOROFlood7[which(as.Date(Q_MOROFlood7$Date) >= as.Date('2004-10-01')),]
# Q_MOROFlood8 = Q_MOROFlood8[which(as.Date(Q_MOROFlood8$Date) >= as.Date('2004-10-01')),]
# Q_MOROFlood9 = Q_MOROFlood9[which(as.Date(Q_MOROFlood9$Date) >= as.Date('2004-10-01')),]
# 
# #Convert simulated streamflow to cfs units
# Q_MOROFlood1$streamflow = round(Q_MOROFlood1$streamflow*conversion_b, 6)
# Q_MOROFlood2$streamflow = round(Q_MOROFlood2$streamflow*conversion_b, 6)
# Q_MOROFlood3$streamflow = round(Q_MOROFlood3$streamflow*conversion_b, 6)
# Q_MOROFlood4$streamflow = round(Q_MOROFlood4$streamflow*conversion_b, 6)
# Q_MOROFlood5$streamflow = round(Q_MOROFlood5$streamflow*conversion_b, 6)
# Q_MOROFlood6$streamflow = round(Q_MOROFlood6$streamflow*conversion_b, 6)
# Q_MOROFlood7$streamflow = round(Q_MOROFlood7$streamflow*conversion_b, 6)
# Q_MOROFlood8$streamflow = round(Q_MOROFlood8$streamflow*conversion_b, 6)
# Q_MOROFlood9$streamflow = round(Q_MOROFlood9$streamflow*conversion_b, 6)
# 
# #Make an evaporation + transpiration column
# Q_MOROFlood1$ET = Q_MOROFlood1$evap + Q_MOROFlood1$trans
# Q_MOROFlood2$ET = Q_MOROFlood2$evap + Q_MOROFlood2$trans
# Q_MOROFlood3$ET = Q_MOROFlood3$evap + Q_MOROFlood3$trans
# Q_MOROFlood4$ET = Q_MOROFlood4$evap + Q_MOROFlood4$trans
# Q_MOROFlood5$ET = Q_MOROFlood5$evap + Q_MOROFlood5$trans
# Q_MOROFlood6$ET = Q_MOROFlood6$evap + Q_MOROFlood6$trans
# Q_MOROFlood7$ET = Q_MOROFlood7$evap + Q_MOROFlood7$trans
# Q_MOROFlood8$ET = Q_MOROFlood8$evap + Q_MOROFlood8$trans
# Q_MOROFlood9$ET = Q_MOROFlood9$evap + Q_MOROFlood9$trans
# 
# #Retain only streamflow, sat def, detention storage, ET, and Date columns for space
# Q_MOROFlood1 = as.data.frame(Q_MOROFlood1[,c('Date', 'streamflow', 'sat_def', 'ET')])
# Q_MOROFlood2 = as.data.frame(Q_MOROFlood2[,c('Date', 'streamflow', 'sat_def', 'ET')])
# Q_MOROFlood3 = as.data.frame(Q_MOROFlood3[,c('Date', 'streamflow', 'sat_def', 'ET')])
# Q_MOROFlood4 = as.data.frame(Q_MOROFlood4[,c('Date', 'streamflow', 'sat_def', 'ET')])
# Q_MOROFlood5 = as.data.frame(Q_MOROFlood5[,c('Date', 'streamflow', 'sat_def', 'ET')])
# Q_MOROFlood6 = as.data.frame(Q_MOROFlood6[,c('Date', 'streamflow', 'sat_def', 'ET')])
# Q_MOROFlood7 = as.data.frame(Q_MOROFlood7[,c('Date', 'streamflow', 'sat_def', 'ET')])
# Q_MOROFlood8 = as.data.frame(Q_MOROFlood8[,c('Date', 'streamflow', 'sat_def', 'ET')])
# Q_MOROFlood9 = as.data.frame(Q_MOROFlood9[,c('Date', 'streamflow', 'sat_def', 'ET')])
# 
# #Fixme: Add residuals for each of the 9 simulations
# #Q_MOROFlood$streamflowResid = Q_MOROFlood$streamflow + Resid
# 
# png(paste0('Q_BestFloodSyn.png'), res = 300, height = 5, width=5, units = 'in')
# plot(x = as.Date(Q_MOROFlood$Date), y = Q_MOROFlood$streamflowResid, xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
# box()
# axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
# axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
# #Add quantiles
# lines(x = c(0,100000), y = c(q05_cal,q05_cal), col = 'blue', lty = 2, lwd = 2)
# lines(x = c(0,100000), y = c(q95_cal,q95_cal), col = 'red', lty = 2, lwd = 2)
# dev.off()
# 
# png(paste0('SatDef_BestFloodSyn.png'), res = 300, height = 5, width=5, units = 'in')
# plot(x = as.Date(Q_MOROFlood$Date), y = Q_MOROFlood$sat_def, xlab = 'Year', ylab = 'Saturation Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
# box()
# axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
# axis(2, at = seq(0,5000,50), labels = TRUE, cex.axis = 1.5)
# dev.off()
# 
# #  Best Low Flow----
# #Load timeseries
# Q_MOROLowFlow = vroom('BestLowFlowSyn_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
# #Make a new Date column
# Q_MOROLowFlow$Date = as.Date(paste0(Q_MOROLowFlow$year, '-', Q_MOROLowFlow$month, '-', Q_MOROLowFlow$day))
# #Trim off spin-up years
# Q_MOROLowFlow = Q_MOROLowFlow[which(as.Date(Q_MOROLowFlow$Date) >= as.Date('2004-10-01')),]
# #Convert simulated streamflow to cfs units
# Q_MOROLowFlow$streamflow = round(Q_MOROLowFlow$streamflow*conversion_b, 6)
# #Make an evaporation + transpiration column
# Q_MOROLowFlow$ET = Q_MOROLowFlow$evap + Q_MOROLowFlow$trans
# #Retain only streamflow, sat def, detention storage, ET, and Date columns for space
# Q_MOROLowFlow = as.data.frame(Q_MOROLowFlow[,c('Date', 'streamflow', 'sat_def', 'ET')])
# 
# #Add residuals
# Q_MOROLowFlow$streamflowResid = Q_MOROLowFlow$streamflow + Resid
# 
# png(paste0('Q_BestLowFlowSyn.png'), res = 300, height = 5, width=5, units = 'in')
# plot(x = as.Date(Q_MOROLowFlow$Date), y = Q_MOROLowFlow$streamflowResid, xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
# box()
# axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
# axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
# #Add quantiles
# lines(x = c(0,100000), y = c(q05_cal,q05_cal), col = 'blue', lty = 2, lwd = 2)
# lines(x = c(0,100000), y = c(q95_cal,q95_cal), col = 'red', lty = 2, lwd = 2)
# dev.off()
# 
# png(paste0('SatDef_BestLowFlowSyn.png'), res = 300, height = 5, width=5, units = 'in')
# plot(x = as.Date(Q_MOROLowFlow$Date), y = Q_MOROLowFlow$sat_def, xlab = 'Year', ylab = 'Saturation Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
# box()
# axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
# axis(2, at = seq(0,5000,50), labels = TRUE, cex.axis = 1.5)
# dev.off()
# 
# #  Compromise----
# #Load timeseries
# Q_MOROComp = vroom('Compromise_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
# #Make a new Date column
# Q_MOROComp$Date = as.Date(paste0(Q_MOROComp$year, '-', Q_MOROComp$month, '-', Q_MOROComp$day))
# #Trim off spin-up years
# Q_MOROComp = Q_MOROComp[which(as.Date(Q_MOROComp$Date) >= as.Date('2004-10-01')),]
# #Convert simulated streamflow to cfs units
# Q_MOROComp$streamflow = round(Q_MOROComp$streamflow*conversion_b, 6)
# #Make an evaporation + transpiration column
# Q_MOROComp$ET = Q_MOROComp$evap + Q_MOROComp$trans
# #Retain only streamflow, sat def, detention storage, ET, and Date columns for space
# Q_MOROComp = as.data.frame(Q_MOROComp[,c('Date', 'streamflow', 'sat_def', 'ET')])
# 
# #Add residuals
# Q_MOROComp$streamflowResid = Q_MOROComp$streamflow + Resid
# 
# png(paste0('Q_CompromiseSyn.png'), res = 300, height = 5, width=5, units = 'in')
# plot(x = as.Date(Q_MOROComp$Date), y = Q_MOROComp$streamflowResid, xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
# box()
# axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
# axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
# #Add quantiles
# lines(x = c(0,100000), y = c(q05_cal,q05_cal), col = 'blue', lty = 2, lwd = 2)
# lines(x = c(0,100000), y = c(q95_cal,q95_cal), col = 'red', lty = 2, lwd = 2)
# dev.off()
# 
# png(paste0('SatDef_CompromiseSyn.png'), res = 300, height = 5, width=5, units = 'in')
# plot(x = as.Date(Q_MOROComp$Date), y = Q_MOROComp$sat_def, xlab = 'Year', ylab = 'Saturation Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
# box()
# axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
# axis(2, at = seq(0,5000,50), labels = TRUE, cex.axis = 1.5)
# dev.off()
# 
# #  Combine all 3----
# png(paste0('Q_AllSyn.png'), res = 300, height = 5, width=5, units = 'in')
# plot(x = as.Date(Q_MOROLowFlow$Date), y = Q_MOROLowFlow$streamflowResid, xlab = '', ylab = '', type = 'l', axes=FALSE, cex.lab = 1.5, ylim = c(0,12), col = 'red')
# box()
# axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
# axis(2, at = seq(0,5000,2), labels = TRUE, cex.axis = 1.5)
# par(new=TRUE)
# plot(x = as.Date(Q_MOROComp$Date), y = Q_MOROComp$streamflowResid, xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5, ylim = c(0,12))
# par(new=TRUE)
# plot(x = as.Date(Q_MOROFlood$Date), y = Q_MOROFlood$streamflowResid, xlab = '', ylab = '', type = 'l', axes=FALSE, cex.lab = 1.5, ylim = c(0,12), col = 'blue')
# dev.off()
# 
# png(paste0('SatDef_AllSyn.png'), res = 300, height = 5, width=5, units = 'in')
# plot(x = as.Date(Q_MOROComp$Date), y = Q_MOROComp$sat_def, xlab = 'Year', ylab = 'Saturation Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5, ylim = c(1700,2300))
# box()
# axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
# axis(2, at = seq(0,5000,50), labels = TRUE, cex.axis = 1.5)
# par(new=TRUE)
# plot(x = as.Date(Q_MOROFlood$Date), y = Q_MOROFlood$sat_def, xlab = '', ylab = '', type = 'l', axes=FALSE, cex.lab = 1.5, ylim = c(1700,2300), col = 'blue')
# par(new=TRUE)
# plot(x = as.Date(Q_MOROLowFlow$Date), y = Q_MOROLowFlow$sat_def, xlab = '', ylab = '', type = 'l', axes=FALSE, cex.lab = 1.5, ylim = c(1700,2300), col = 'red')
# dev.off()