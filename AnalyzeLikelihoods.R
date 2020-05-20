setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR")

library(GISTools)
library(rgdal)

source('ColorFunctions.R')

#Read the worldfile. This is used to extract the area of basin and hillslopes----
#Patch resolution, m
res = 30

world = read.csv("C:\\Users\\js4yd\\Documents\\BaismanSA\\RHESSysRuns\\Run0\\worldfiles\\worldfile.csv", stringsAsFactors = FALSE)

#Taking the unique patch IDs because strata can exist in more than one patch.
Area.basin = length(unique(world$patchID))*res^2

#Get hillslope areas and conversion factor for streamflow in hillslopes
uhills = unique(world$hillID)
Area.Hills = matrix(NA, nrow = length(uhills), ncol = 2)
for (h in 1:length(uhills)){
  Area.Hills[h,1] = h
  #some patches have multiple strata, so their area cannot be counted from the count of cells.
  Area.Hills[h,2] = length(which(world[which(duplicated(world$patchID) == FALSE),]$hillID == h))*res^2
}
rm(h)

# Make the worldfile a spatial dataframe to get a map. Plot information in the worldfile on the maps----
coordinates(world) = c('patchX', 'patchY')
proj4string(world) = CRS('+init=epsg:26918')
#Change to degrees
world=spTransform(world, CRSobj = CRS('+init=epsg:4326'))

cols = rainbow(n = length(uhills))

# Baisman Map----
#Fixme: add stream to this map (white?)
png('hillslopeMap.png', res = 300, height = 6, width = 6, units ='in')
par(mar= c(2.5,2.5,1,1))
plot(world, col = 'white')
for (h in 1:length(uhills)){
  plot(world[world$hillID == uhills[h],], pch = 22, add = TRUE, lwd=10)
  plot(world[world$hillID == uhills[h],], col = cols[h], pch = 15, add = TRUE)
}
legend('bottomright', title = expression(bold('Hillslope')), legend=seq(1,length(uhills),1), fill = cols, border = 'black', ncol = 2)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()
rm(h)

#Load the observed streamflow record----
obs = read.table("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\obs\\BaismanStreamflow_Cal.txt", header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, sep = '\t')
#Remove observations later than 9/30/2010 (SA timeperiod end)
obs = obs[as.Date(obs$Date) <= as.Date(colnames(BasinSF)[ncol(BasinSF)]),]
#Remove observations earlier than 10/01/2004 (SA timeperiod start)
obs = obs[as.Date(obs$Date) >= as.Date('2004-10-01'),]

#Load the observed TN record----
obsTN = read.table("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\obs\\TN_Cal.txt", header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, sep = '\t')
#Remove observations later than 9/30/2010 (SA timeperiod end)
obsTN = obsTN[as.Date(obsTN$Date) <= as.Date(colnames(BasinSF)[ncol(BasinSF)]),]
#Remove observations earlier than 10/01/2004 (SA timeperiod start)
obsTN = obsTN[as.Date(obsTN$Date) >= as.Date('2004-10-01'),]
#Remove all NA days
obsTN = obsTN[!is.na(obsTN$TN),]

#Read in the replicate information (reordered to match Morris scheme)----
Likes = read.csv(file = 'SA_Params_logL_Baisman_Flow_Reordered.csv', check.names = FALSE, stringsAsFactors = FALSE)
LikesTN = read.csv(file = 'SA_Params_logL_Baisman_TN_Reordered.csv', check.names = FALSE, stringsAsFactors = FALSE)

#Load in the streamflow data for the basin and hillslopes----
BasinSF = read.table(file = 'SAResults_BasinStreamflow_p4_Reordered.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillSF = read.table(file = 'SAResults_HillStreamflow_p6_Reordered.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
BasinTNMed = read.table(file = 'SAResults_BasinTNMed_p3_All_Reordered.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillTNMed = read.table(file = 'SAResults_HillTNMed_p3_All_Reordered.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)

BasinSF = BasinSF[, c(1, which(as.Date(colnames(BasinSF[,-1])) >= as.Date('2004-10-01'))+1)]
HillSF = HillSF[, c(1, 2, which(as.Date(colnames(HillSF[,-c(1,2)])) >= as.Date('2004-10-01'))+2)]
BasinTNMed = BasinTNMed[, c(1, which(as.Date(colnames(BasinTNMed[,-1])) >= as.Date('2004-10-01'))+1)]
HillTNMed = HillTNMed[, c(1, 2, which(as.Date(colnames(HillTNMed[,-c(1,2)])) >= as.Date('2004-10-01'))+2)]

#Select only the unique streamflow and TN records.----
#Non-unique records exist because some parameters had no affect on output
UniqueReps = which(duplicated(x = BasinSF[,-1]) == FALSE)
BasinSF = BasinSF[UniqueReps,]
Likes = Likes[UniqueReps,]
LikesTN = LikesTN[UniqueReps,]
HillSF = HillSF[which(HillSF$Replicate %in% UniqueReps),]
BasinTNMed = BasinTNMed[UniqueReps,]
HillTNMed = HillTNMed[which(HillTNMed$Replicate %in% UniqueReps),]

#Make a new file for the multiplication of the likelihood functions (which is summing the log likelihoods)----
LikesAll = cbind(Likes, LikesTN)
LikesAll$logLAll = Likes$logL + LikesTN$logL

#Plots for Likelihoods----
#log-likelihoods vs. scale----
png('ColorLikeScale.png', res = 300, width = 10, height = 10, units = 'in')
layout(rbind(c(1,2), c(3,4)))
plot(Likes$logL, Likes$sigma_0, xlab = 'Log Likelihood', ylab = 'Sigma_0', pch = 16, main = 'Streamflow', cex = 0.5)
plot(Likes$logL, Likes$sigma_1, xlab = 'Log Likelihood', ylab = 'Sigma_1', pch = 16, main = 'Streamflow', cex = 0.5)

plot(x = LikesTN$logL, y = LikesTN$sigma_0, xlab = 'Log Likelihood', ylab = 'Sigma_0', pch = 16, main = 'TN', cex = 0.5)
plot(x = LikesTN$logL, y = LikesTN$sigma_1, xlab = 'Log Likelihood', ylab = 'Sigma_1', pch = 16, main = 'TN', cex = 0.5)

dev.off()

#Histograms of the parameters for each of flow and TN----
#-1 < beta   < 1, beta=-1: uniform, beta=0: Gaussian, beta=1: double exponential
#0  < xi    <= 10, xi=1: symmetric, xi<1: negatively skewed, xi>1: positively skewed
#0 <= sigma_0 <= 1
#0 <= sigma_1 <= 1
#0 <= phi_1    < 1, phi_1=0: no auto-correlation, phi_1=1: perfect auto-correlation
#0 <= mu_h    <= 100

png('HistsLikeParams.png', res = 300, width = 10, height = 10, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
hist(Likes$beta, breaks = 100, freq = TRUE, main = 'Kurtosis (Beta)', xlab = '', xlim = c(-1,1))
hist(Likes$xi, breaks = 100, freq = TRUE, main = 'Skewness (Xi)', xlab = '', xlim = c(0,10))
hist(Likes$sigma_0, breaks = 100, freq = TRUE, main = 'Standard Deviation when Mean = 0 (sigma_0)', xlab = '', xlim = c(0,1))
hist(Likes$sigma_1, breaks = 100, freq = TRUE, main = 'Linear Change in Std. Dev. with Mean (sigma_1)', xlab = '', xlim = c(0,1))
hist(Likes$phi_1, breaks = 100, freq = TRUE, main = 'Lag 1 Autocorrelation (phi_1)', xlab = '', xlim = c(0,1))
hist(Likes$mu_h, breaks = 100, freq = TRUE, main = 'Mean Bias Factor (muh)', xlab = '', xlim = c(0,100))
dev.off()

png('HistsLikeTNParams.png', res = 300, width = 10, height = 10, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
hist(LikesTN$beta, breaks = 100, freq = TRUE, main = 'Kurtosis for TN (Beta)', xlab = '', xlim = c(-1,1))
hist(LikesTN$xi, breaks = 100, freq = TRUE, main = 'Skewness for TN (Xi)', xlab = '', xlim = c(0,10))
hist(LikesTN$sigma_0, breaks = 100, freq = TRUE, main = 'Standard Deviation when Mean = 0 \n for TN (sigma_0)', xlab = '', xlim = c(0,1))
hist(LikesTN$sigma_1, breaks = 100, freq = TRUE, main = 'Linear Change in Std. Dev. with Mean \n for TN (sigma_1)', xlab = '', xlim = c(0,1))
hist(LikesTN$phi_1, breaks = 100, freq = TRUE, main = 'Lag 1 Autocorrelation for TN (phi_1)', xlab = '', xlim = c(0,1))
hist(LikesTN$mu_h, breaks = 100, freq = TRUE, main = 'Mean Bias Factor for TN (muh)', xlab = '', xlim = c(0,100))
dev.off()


#eCDF plot to see if there is a significant change in the log-likelihood at a certain percentile
png('eCDFsLikes.png', res = 300, width = 9, height = 3, units = 'in')
layout(rbind(c(1,2,3)))
plot(ecdf(Likes$logL), main = 'Streamflow', xlab = 'Log Likelihood')
plot(ecdf(LikesTN$logL), main = 'TN', xlab = 'Log Likelihood')
plot(ecdf(LikesAll$logLAll), main = 'Streamflow + TN', xlab = 'Log Likelihood')
dev.off()

#Gather the top 1% of the log-likelihoods for timeseries plotting----
q1 = quantile(x = LikesAll$logLAll, probs = .99)
SelLikes = LikesAll[LikesAll$logLAll >= q1,]

#Using these replicates, plot the basin and hillslope streamflow and TN graphs----
#Basin:
#Replicates on y axis
#Time on x axis
#Color by streamflow mean?
#Color by the percentile of streamflow for that day?
#one plot and color by streamflow variability for the date?
Means = apply(X = BasinSF[which(BasinSF$Replicate %in% SelLikes$Replicate),-1], MARGIN = 2, FUN = mean)
SDs = apply(X = BasinSF[which(BasinSF$Replicate %in% SelLikes$Replicate),-1], MARGIN = 2, FUN = sd)
SDsTN = apply(X = BasinTNMed[which(BasinTNMed$Replicate %in% SelLikes$Replicate),-1], MARGIN = 2, FUN = sd)
MeansTN = apply(X = BasinTNMed[which(BasinTNMed$Replicate %in% SelLikes$Replicate),-1], MARGIN = 2, FUN = mean)

png('SDMostLikey1p.png', res = 300, width = 6, height = 10, units = 'in')
layout(c(1,2))
plot(x = as.Date(colnames(BasinSF[-1])), y = SDs, pch = 16, cex = 0.5, ylim = c(0,10), ylab = 'Standard Deviation of Flow (cfs)', xlab = 'Year', main = 'Standard Deviation of Flow \n Most Likely 1% of Replicates', type = 'o', cex.axis=1.5, cex.lab=1.5,cex.main=1.5)
plot(x = as.Date(colnames(BasinTNMed[-1])), y = SDsTN, pch = 16, cex = 0.5, ylim = c(0,.5), ylab = 'Standard Deviation of TN (mg/L)', xlab = 'Year', main = 'Standard Deviation of TN \n Most Likely 1% of Replicates', type = 'o', cex.axis=1.5, cex.lab=1.5,cex.main=1.5)
dev.off()

png('MeanMostLikey1p.png', res = 300, width = 6, height = 10, units = 'in')
layout(c(1,2))
plot(x = as.Date(colnames(BasinSF[-1])), y = Means, pch = 16, cex = 0.5, ylim = c(0,20), ylab = 'Mean of Flow (cfs)', xlab = 'Year', main = 'Mean of Flow \n Most Likely 1% of Replicates', type = 'o', cex.axis=1.5, cex.lab=1.5,cex.main=1.5)
plot(x = as.Date(colnames(BasinTNMed[-1])), y = MeansTN, pch = 16, cex = 0.5, ylim = c(0,3), ylab = 'Mean of TN (mg/L)', xlab = 'Year', main = 'Mean of TN \n Most Likely 1% of Replicates', type = 'o', cex.axis=1.5, cex.lab=1.5,cex.main=1.5)
dev.off()

png('RepsObsMostLikey1p.png', res = 300, width = 6, height = 10, units = 'in')
layout(c(1,2))
plot(x = as.Date(colnames(BasinSF[-1])), y = obs$Flow, pch = 16, cex = 0.5, ylim = c(0,30), ylab = 'Flow (cfs)', xlab = 'Year', main = 'Basin Outlet Streamflow \n Most Likely 1% of Replicates', type = 'l', cex.axis=1.5, cex.lab=1.5,cex.main=1.5, col = 'red')
par(new = TRUE)
matplot(x = as.Date(colnames(BasinSF[-1])), y = t(BasinSF[which(BasinSF$Replicate %in% SelLikes$Replicate),-1]), type = 'l', ylim = c(0,30), ylab = '', xlab = '', main = '', cex.axis=1.5, cex.lab=1.5,cex.main=1.5, col = adjustcolor('gray', alpha.f = 0.1), axes=FALSE)
par(new = TRUE)
plot(x = as.Date(colnames(BasinSF[-1])), y = obs$Flow, pch = 16, cex = 0.5, ylim = c(0,30), ylab = '', xlab = '', main = '', type = 'l', cex.axis=1.5, cex.lab=1.5,cex.main=1.5, col = 'red', axes=FALSE)

plot(x = as.Date(colnames(BasinTNMed[-1])), y = obsTN$TN, pch = 16, cex = 0.5, ylim = c(0,3), ylab = 'Mean of TN (mg/L)', xlab = 'Year', main = 'Mean of TN \n Most Likely 1% of Replicates', type = 'p', cex.axis=1.5, cex.lab=1.5,cex.main=1.5, col='red')
par(new = TRUE)
matplot(x = as.Date(colnames(BasinTNMed[-1])), y = t(BasinTNMed[which(BasinTNMed$Replicate %in% SelLikes$Replicate),-1]), type = 'l', ylim = c(0,3), ylab = '', xlab = '', main = '', cex.axis=1.5, cex.lab=1.5,cex.main=1.5, col = adjustcolor('gray', alpha.f = 0.1), axes=FALSE)
par(new = TRUE)
plot(x = as.Date(colnames(BasinTNMed[-1])), y = obsTN$TN, pch = 16, cex = 0.5, ylim = c(0,3), ylab = '', xlab = '', main = '', type = 'p', cex.axis=1.5, cex.lab=1.5,cex.main=1.5, col='red', axes=FALSE)
dev.off()

png('CVMostLikey1p.png', res = 300, width = 6, height = 10, units = 'in')
layout(c(1,2))
plot(x = as.Date(colnames(BasinSF[-1])), y = SDs/Means, pch = 16, cex = 0.5, ylim = c(0,1), ylab = 'CV of Flow (-)', xlab = 'Year', main = 'CV of Flow \n Most Likely 1% of Replicates', type = 'o', cex.axis=1.5, cex.lab=1.5,cex.main=1.5)
plot(x = as.Date(colnames(BasinTNMed[-1])), y = SDsTN/MeansTN, pch = 16, cex = 0.5, ylim = c(0,.5), ylab = 'CV of TN (-)', xlab = 'Year', main = 'CV of TN \n Most Likely 1% of Replicates', type = 'o', cex.axis=1.5, cex.lab=1.5,cex.main=1.5)
dev.off()

#for (i in 1:(ncol(BasinSF)-1)){
#  plot(x = as.Date(colnames(BasinSF[-1])), y = test, ncol(BasinSF)-1), pch = 15, col = rainbow(4000))
#  par(new=TRUE)
#}

#Maybe multiply to get flux of nitrogen, and plot that spatially as a result to show differences in variability across the top 100 scenarios

#Hillslope Plots----
MeansHill = SDsHill = MeansTNHill = SDsTNHill = matrix(NA, nrow = length(unique(HillSF$HillID)), ncol = ncol(BasinSF)-1)
for (i in 1:length(unique(HillSF$HillID))){
  MeansHill[i,] = apply(X = HillSF[which((HillSF$Replicate %in% SelLikes$Replicate) & (HillSF$HillID == i)),-c(1,2)], MARGIN = 2, FUN = mean)
  SDsHill[i,] = apply(X = HillSF[which((HillSF$Replicate %in% SelLikes$Replicate) & (HillSF$HillID == i)),-c(1,2)], MARGIN = 2, FUN = sd)
  SDsTNHill[i,] = apply(X = HillTNMed[which((HillSF$Replicate %in% SelLikes$Replicate) & (HillSF$HillID == i)),-c(1,2)], MARGIN = 2, FUN = sd)
  MeansTNHill[i,] = apply(X = HillTNMed[which((HillSF$Replicate %in% SelLikes$Replicate) & (HillSF$HillID == i)),-c(1,2)], MARGIN = 2, FUN = mean)
}
rm(i)

png('SDMostLikey1p_Hill.png', res = 300, width = 10, height = 14, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12), c(13,14)))
for (i in 1:length(unique(HillSF$HillID))){
  plot(x = as.Date(colnames(BasinSF[-1])), y = SDsHill[i,], pch = 16, cex = 0.5, ylab = 'Standard Deviation of Flow (cfs)', xlab = 'Year', main = paste0('Hillslope ', i, ' Standard Deviation of Flow \n Most Likely 1% of Replicates'), type = 'o', cex.axis=1.5, cex.lab=1.5,cex.main=1.5, col = cols[i])
}
dev.off()

png('SDMostLikey1p_Hill_sameY.png', res = 300, width = 10, height = 14, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12), c(13,14)))
for (i in 1:length(unique(HillSF$HillID))){
  plot(x = as.Date(colnames(BasinSF[-1])), y = SDsHill[i,], pch = 16, cex = 0.5, ylim = c(0,5), ylab = 'Standard Deviation of Flow (cfs)', xlab = 'Year', main = paste0('Hillslope ', i, ' Standard Deviation of Flow \n Most Likely 1% of Replicates'), type = 'o', cex.axis=1.5, cex.lab=1.5,cex.main=1.5, col = cols[i])
}
dev.off()

png('SDMostLikey1p_HillTN.png', res = 300, width = 10, height = 14, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12), c(13,14)))
for (i in 1:length(unique(HillSF$HillID))){
  plot(x = as.Date(colnames(BasinSF[-1])), y = SDsTNHill[i,], pch = 16, cex = 0.5, ylim = c(0,5), ylab = 'Standard Deviation of TN (mg/L)', xlab = 'Year', main = paste0('Hillslope ', i, ' Standard Deviation of TN \n Most Likely 1% of Replicates'), type = 'o', cex.axis=1.5, cex.lab=1.5,cex.main=1.5, col = cols[i])
}
dev.off()

#Hillslope Maps for Upper 5th %-ile variability across most likely replicates, and TN loads mean and sd spatially----
#Get the upper 5th percentile flow, and the TN load from each replicate and take the mean and sd
#kg/d loading
HillTNLoad = cbind(HillSF[,c(1,2)], HillSF[,-c(1,2)]*HillTNMed[,-c(1,2)]*1000/(100)^3*(2.54)^3*(12^3)/1000000*3600*24)
HillLikes95 = HillLikesTN = HillLikesNormTN = matrix(NA, nrow = length(unique(HillSF$HillID)), ncol = 1+nrow(SelLikes))
Means95Hill = SDs95Hill = MeansTNLoadsHill = SDsTNLoadsHill = MeansTNLoadsHillNorm = SDsTNLoadsHillNorm = matrix(NA, nrow = length(unique(HillSF$HillID)), ncol = 2)
for (i in 1:length(unique(HillSF$HillID))){
  Up5Flows = apply(X = HillSF[which((HillSF$Replicate %in% SelLikes$Replicate) & (HillSF$HillID == i)),-c(1,2)], MARGIN = 1, FUN = quantile, probs = 0.95)
  #Dividing by 6 for number of years
  TNLoads = apply(X = HillTNLoad[which((HillTNLoad$Replicate %in% SelLikes$Replicate) & (HillTNLoad$HillID == i)),-c(1,2)], MARGIN = 1, FUN = sum)/6
  #Dividing by area in km^2
  TNLoadsNormalized = apply(X = HillTNLoad[which((HillTNLoad$Replicate %in% SelLikes$Replicate) & (HillTNLoad$HillID == i)),-c(1,2)], MARGIN = 1, FUN = sum)/6/Area.Hills[i,2]*1000^2
  HillLikes95[i,] = c(i, Up5Flows)
  HillLikesTN[i,] = c(i, TNLoads)
  HillLikesNormTN[i,] = c(i, TNLoadsNormalized)
  Means95Hill[i,] = c(i, mean(Up5Flows))
  SDs95Hill[i,] = c(i, sd(Up5Flows))
  SDsTNLoadsHill[i,] = c(i, sd(TNLoads))
  MeansTNLoadsHill[i,] = c(i, mean(TNLoads))
  SDsTNLoadsHillNorm[i,] = c(i, sd(TNLoadsNormalized))
  MeansTNLoadsHillNorm[i,] = c(i, mean(TNLoadsNormalized))
}
rm(i, Up5Flows, TNLoads)
colnames(HillLikes95) = colnames(HillLikesTN) = colnames(HillLikesNormTN) = c('HillID', SelLikes$Replicate)
colnames(Means95Hill) = colnames(MeansTNLoadsHill) = c('HillID', 'Mean')
colnames(SDs95Hill) = colnames(SDsTNLoadsHill) = c('HillID', 'SD')

#Map of hillslope mean Upper 5th Percentile Flow----
#colPal = colorRampPalette(colors = rev(c('red', 'orange', 'yellow', 'green', 'blue')))
#scaleRange = c(0, 0.5)
#scaleBy = .1
#Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)

colPal = colorRampPalette(colors = rev(c('red', 'orange', 'yellow', 'green', 'blue')))
scaleRange = c(0.1, 0.9)
scaleBy = 0.1
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)

png('HillslopeMean95FlowMap.png', res = 300, height = 6, width = 6, units ='in')
par(mar= c(2.5,2.5,1,1))
plot(world, col = 'white')
for (h in 1:length(uhills)){
  plot(world[world$hillID == uhills[h],], pch = 22, add = TRUE, lwd=10)
  plot(world[world$hillID == uhills[h],], col = colFun(Data = Means95Hill[uhills[h],2]), pch = 15, add = TRUE)
}
legend('bottomright', title = expression(bold(paste('95th Percentile Flow (cfs)'))), legend=c(paste0(seq(scaleRange[1], scaleRange[2], scaleBy), ' - < ', seq(scaleRange[1]+scaleBy, scaleRange[2]+scaleBy, scaleBy))), fill = colFun(seq(scaleRange[1], scaleRange[2], scaleBy)), border = 'black', ncol = 2)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()
rm(h)

#Map of hillslope SD Upper 5th Percentile Flow----
colPal = colorRampPalette(colors = c('skyblue', 'blue', 'darkblue'))
scaleRange = c(0, 0.2)
scaleBy = .05
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)

png('HillslopeSD95FlowMap.png', res = 300, height = 6, width = 6, units ='in')
par(mar= c(2.5,2.5,1,1))
plot(world, col = 'white')
for (h in 1:length(uhills)){
  plot(world[world$hillID == uhills[h],], pch = 22, add = TRUE, lwd=10)
  plot(world[world$hillID == uhills[h],], col = colFun(Data = SDs95Hill[uhills[h],2]), pch = 15, add = TRUE)
}
legend('bottomright', title = expression(bold('SD 95th Percentile Flow (cfs)')), legend=c('< 0.05', '0.05 - < 0.1', '0.1 - < 0.15', '0.15 - < 0.2', '0.2 - < 0.25'), fill = colFun(seq(0,0.2,0.05)), border = 'black', ncol = 2)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()
rm(h)


#Map of hillslope mean TN load----
colPal = colorRampPalette(colors = rev(c('red', 'orange', 'yellow', 'green', 'blue')))
scaleRange = c(0, 400)
scaleBy = 100
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)

png('HillslopeMeanTNLoadsMap.png', res = 300, height = 6, width = 6, units ='in')
par(mar= c(2.5,2.5,1,1))
plot(world, col = 'white')
for (h in 1:length(uhills)){
  plot(world[world$hillID == uhills[h],], pch = 22, add = TRUE, lwd=10)
  plot(world[world$hillID == uhills[h],], col = colFun(Data = MeansTNLoadsHill[uhills[h],2]), pch = 15, add = TRUE)
}
legend('bottomright', title = expression(bold('TN Load (kg/yr)')), legend=c('0 - < 100', '100 - < 200', '200 - < 300', '300 - < 400', '400 - < 500'), fill = colFun(seq(0,400,100)), border = 'black', ncol = 2)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()
rm(h)

#Map of hillslope SD TN load----
colPal = colorRampPalette(colors = c('skyblue', 'blue', 'darkblue'))
scaleRange = c(0, 150)
scaleBy = 30
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)

png('HillslopeSDTNLoadsMap.png', res = 300, height = 6, width = 6, units ='in')
par(mar= c(2.5,2.5,1,1))
plot(world, col = 'white')
for (h in 1:length(uhills)){
  plot(world[world$hillID == uhills[h],], pch = 22, add = TRUE, lwd=10)
  plot(world[world$hillID == uhills[h],], col = colFun(Data = SDsTNLoadsHill[uhills[h],2]), pch = 15, add = TRUE)
}
legend('bottomright', title = expression(bold('SD TN Load (kg/yr)')), legend=c('0 - < 30', '30 - < 60', '60 - < 90', '90 - < 120', '120 - < 150', '150 - < 180'), fill = colFun(seq(0,180,30)), border = 'black', ncol = 2)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()
rm(h)

#Map of hillslope mean normalized TN load----
colPal = colorRampPalette(colors = rev(c('red', 'orange', 'yellow', 'green', 'blue')))
scaleRange = c(400, 1600)
scaleBy = 400
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)

png('HillslopeMeanTNLoadsNormMap.png', res = 300, height = 6, width = 6, units ='in')
par(mar= c(2.5,2.5,1,1))
plot(world, col = 'white')
for (h in 1:length(uhills)){
  plot(world[world$hillID == uhills[h],], pch = 22, add = TRUE, lwd=10)
  plot(world[world$hillID == uhills[h],], col = colFun(Data = MeansTNLoadsHillNorm[MeansTNLoadsHillNorm[,1] == uhills[h],2]), pch = 15, add = TRUE)
}
legend('bottomright', title = expression(bold(paste('TN Load (kg/yr/km'^2,')'))), legend=c('400 - < 800', '800 - < 1200', '1200 - < 1600', '1600 - < 1655'), fill = colFun(seq(400,2000,400)), border = 'black', ncol = 2)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()
rm(h)

#Map of hillslope CV normalized TN load----

#SD colors
#colPal = colorRampPalette(colors = c('skyblue', 'blue', 'darkblue'))
#scaleRange = c(150, 650)
#scaleBy = 100
#Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)

#CV plot
colPal = colorRampPalette(colors = c('skyblue', 'blue', 'darkblue'))
scaleRange = c(0, .8)
scaleBy = .2
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)

png('HillslopeCVTNLoadsNormMap.png', res = 300, height = 6, width = 6, units ='in')
par(mar= c(2.5,2.5,1,1))
plot(world, col = 'white')
for (h in 1:length(uhills)){
  plot(world[world$hillID == uhills[h],], pch = 22, add = TRUE, lwd=10)
  plot(world[world$hillID == uhills[h],], col = colFun(Data = SDsTNLoadsHillNorm[SDsTNLoadsHillNorm[,1] == uhills[h],2]/MeansTNLoadsHillNorm[MeansTNLoadsHillNorm[,1] == uhills[h],2]), pch = 15, add = TRUE)
}
legend('bottomright', title = expression(bold(paste('CV TN Load (-)'))), legend=c('0 - < 0.2', '0.2 - < 0.4', '0.4 - < 0.6', '0.6 - < 0.8'), fill = colFun(seq(0,0.8,0.2)), border = 'black', ncol = 2)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()
rm(h)

#Side by side Flow and TN plots for Hillslope Replicates: MLE and 1 other high-likelihood replicate----
#Flow Maximum Likelihood Map
colPal = colorRampPalette(colors = rev(c('red', 'orange', 'yellow', 'green', 'blue')))
scaleRange = c(0.1, 0.9)
scaleBy = 0.1
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)

png('HillslopeMaxLike95Map.png', res = 300, height = 6, width = 6, units ='in')
par(mar= c(2.5,2.5,1,1))
plot(world, col = 'white')
for (h in 1:length(uhills)){
  plot(world[world$hillID == uhills[h],], pch = 22, add = TRUE, lwd=10)
  plot(world[world$hillID == uhills[h],], col = colFun(Data = HillLikes95[HillLikes95[,1] == uhills[h], which(colnames(HillLikes95) == LikesAll$Replicate[which(LikesAll$logLAll == max(LikesAll$logLAll))])]), pch = 15, add = TRUE)
}
legend('bottomright', title = expression(bold(paste('95th Percentile Flow (cfs)'))), legend=c(paste0(seq(0.1,0.9,0.1), ' - < ', seq(0.2,1,0.1))), fill = colFun(seq(0.1,0.9,0.1)), border = 'black', ncol = 2)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()
rm(h)

#1535
png('HillslopeCompare153595Map.png', res = 300, height = 6, width = 6, units ='in')
par(mar= c(2.5,2.5,1,1))
plot(world, col = 'white')
for (h in 1:length(uhills)){
  plot(world[world$hillID == uhills[h],], pch = 22, add = TRUE, lwd=10)
  plot(world[world$hillID == uhills[h],], col = colFun(Data = HillLikes95[HillLikes95[,1] == uhills[h], which(colnames(HillLikes95) == 1535)]), pch = 15, add = TRUE)
}
legend('bottomright', title = expression(bold(paste('95th Percentile Flow (cfs)'))), legend=c(paste0(seq(0.1,0.9,0.1), ' - < ', seq(0.2,1,0.1))), fill = colFun(seq(0.1,0.9,0.1)), border = 'black', ncol = 2)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()
rm(h)

#TN for MLE
colPal = colorRampPalette(colors = rev(c('red', 'orange', 'yellow', 'green', 'blue')))
scaleRange = c(0.1, 0.9)
scaleBy = 0.1
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)

colPal = colorRampPalette(colors = rev(c('red', 'orange', 'yellow', 'green', 'blue')))
scaleRange = c(400, 1600)
scaleBy = 400
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)

png('HillslopeMaxLikeTNMap.png', res = 300, height = 6, width = 6, units ='in')
par(mar= c(2.5,2.5,1,1))
plot(world, col = 'white')
for (h in 1:length(uhills)){
  plot(world[world$hillID == uhills[h],], pch = 22, add = TRUE, lwd=10)
  plot(world[world$hillID == uhills[h],], col = colFun(Data = HillLikesNormTN[HillLikesNormTN[,1] == uhills[h], which(colnames(HillLikesNormTN) == LikesAll$Replicate[which(LikesAll$logLAll == max(LikesAll$logLAll))])]), pch = 15, add = TRUE)
}
legend('bottomright', title = expression(bold(paste('TN Load (kg/yr/km'^2,')'))), legend=c(paste0(seq(0.1,0.9,0.1), ' - < ', seq(0.2,1,0.1))), fill = colFun(seq(0.1,0.9,0.1)), border = 'black', ncol = 2)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()
rm(h)

#1535
png('HillslopeCompare1535TNMap.png', res = 300, height = 6, width = 6, units ='in')
par(mar= c(2.5,2.5,1,1))
plot(world, col = 'white')
for (h in 1:length(uhills)){
  plot(world[world$hillID == uhills[h],], pch = 22, add = TRUE, lwd=10)
  plot(world[world$hillID == uhills[h],], col = colFun(Data = HillLikesNormTN[HillLikesNormTN[,1] == uhills[h], which(colnames(HillLikesNormTN) == 1535)]), pch = 15, add = TRUE)
}
legend('bottomright', title = expression(bold(paste('TN Load (kg/yr/km'^2,')'))), legend=c(paste0(seq(0.1,0.9,0.1), ' - < ', seq(0.2,1,0.1))), fill = colFun(seq(0.1,0.9,0.1)), border = 'black', ncol = 2)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()
rm(h)

#Make a plot of the weighted prediction based on the likelihoods of the replicates----
LikesAll$weights = (LikesAll$logLAll-min(LikesAll$logLAll))/sum(LikesAll$logLAll-min(LikesAll$logLAll))

#Use the distribution of weights as an empirical distribution

#Parallel axis plot of the parameters on x-axis and likelihood coloring----
InputParams = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs\\MorrisSampleLocs\\MorrisSamples_AfterProcessing.csv", stringsAsFactors = FALSE)

#Remove all of the parameters with _orig. They were not modified
InputParams = InputParams[-grep(x = colnames(InputParams), pattern = '_orig', fixed = TRUE)]
#Get the number of parameters
cols = ncol(InputParams)

#Export csv of parameters and likelihood----
LikesParams = cbind(SelLikes$logL, InputParams[SelLikes$Replicate, which(colnames(InputParams) %in% SortRanksMua_b)])
write.csv(LikesParams, 'LikelihoodParamsParAxis.csv', row.names = FALSE)
