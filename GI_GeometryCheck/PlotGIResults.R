#Script to plot results from GI random seed analysis

library(vroom)
library(sp)
library(rgdal)

setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation")

numReps = 100

#Compute streamflow conversion factor----
world = read.csv('worldfile.csv', stringsAsFactors = FALSE)
res = 30
#Taking the unique patch IDs because strata can exist in more than one patch.
Area.basin = length(unique(world$patchID))*res^2
#Multiplier conversion for basin streamflow (mm/d)*conversion_b -> cfs
conversion_b = Area.basin/1000/(.3048^3)/24/3600

#Get hillslope areas and conversion factor for streamflow in hillslopes
uhills = unique(world$hillID)
Area.Hills = matrix(NA, nrow = length(uhills), ncol = 2)
#Multiplier conversion for streamflow (mm/d)*conversion_b -> cfs
conversion_h = matrix(NA, nrow = length(uhills), ncol = 2)
for (h in 1:length(uhills)){
  Area.Hills[h,1] = h
  conversion_h[h,1] = h
  #some patches have multiple strata, so their area cannot be counted from the count of cells.
  Area.Hills[h,2] = length(which(world[which(duplicated(world$patchID) == FALSE),]$hillID == h))*res^2
  conversion_h[h,2] = Area.Hills[h,2]/1000/(.3048^3)/24/3600
}
rm(h)

#Make spatial dataframe from worldfile----
coordinates(world) = c('patchX', 'patchY')
proj4string(world) = CRS('+init=epsg:26918')
#Change to degrees
world=spTransform(world, CRSobj = CRS('+init=epsg:4326'))

#Load the observed streamflow to compare the parameter set to it
ObsQ = read.table("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\obs\\BaismanStreamflow_Feb2020Revised_Cal_p.txt", header = TRUE, sep = '\t', stringsAsFactors = FALSE)
#Load streamflow likelihood parameters to plot error bars
ErrParams = read.csv('Params_logLQ_Run192_Ch9.csv')

#Load the original data and trim to only the variables being compared----
#Read in simulated basin streamflow
SimB = vroom('Run192_Ch9_basin.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)

#Make a new Date column
SimB$Date = as.Date(paste0(SimB$year, '-', SimB$month, '-', SimB$day))

#Trim off spin-up years
SimB = SimB[which(as.Date(SimB$Date) >= as.Date('2004-10-01')),]

#Convert simulated streamflow to cfs units
SimB$streamflow = round(SimB$streamflow*conversion_b, 6)

#Make an evaporation + transpiration column
SimB$ET = SimB$evap + SimB$trans

#Retain only streamflow, sat def, detention storage, ET, and Date columns for space
SimB = as.data.frame(SimB[,c('Date', 'streamflow', 'sat_def', 'detention_store', 'ET')])

# Hillslope----
SimH = vroom('Run192_Ch9_hillslope.daily', delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)

#Make a new date column
SimH$Date = as.Date(paste0(SimH$year, '-', SimH$month, '-', SimH$day))

#Trim off spin-up years
SimH = SimH[which(as.Date(SimH$Date) >= as.Date('2004-10-01')),]

#Make an evaporation + transpiration column
SimH$ET = SimH$evap + SimH$trans

#Convert streamflow
for (h in 1:nrow(Area.Hills)){
  SimH$streamflow[SimH$hillID == h] = SimH$streamflow[SimH$hillID == h]*conversion_h[which(conversion_h[,1] == h),2]
}

#Retain only streamflow, sat def, detention storage, ET, and Date columns for space
SimH = as.data.frame(SimH[,c('hillID', 'Date', 'streamflow', 'sat_def', 'detention_store', 'ET')])

#Load the GI data----
Q_b = read.table('FlowGI_c.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE)
SatDef_b = read.table('SatDefGI_c.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE)
DetStore_b = read.table('DetStoreGI_c.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE)
ET_b = read.table('ETGI_c.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE)

Q_h = read.table('FlowGI_c_h.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
SatDef_h = read.table('SatDefGI_c_h.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
DetStore_h = read.table('DetStoreGI_c_h.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
ET_h = read.table('ETGI_c_h.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)

#Plots - Basin ----
# Compare observed and simulated streamflow----
png(paste0('Q_ObsVsSim_b.png'), res = 300, height = 5, width=5, units = 'in')
plot(x = as.Date(ObsQ$Date), y = ObsQ$Flow, col = 'black', xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
par(new=TRUE)
plot(x = as.Date(Q_b$Date), y = SimB$streamflow, col = 'red', xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()

#Simulate random error and plot as error bars around the mean
rsep = function(n, beta, xi, s0, s1, p, mu){
  #perform n draws from the skew exponential power density function (AR1 and heteroskedastic) with the definied parameters
  
}
  
#Lambda from Python estimated value
Qlambda = 0.03891046118748475
#Inverse Box Cox Transform the simulated error
(SimQ['streamflow']*Qlambda + 1)^(1/Qlambda) - 0.001

#Get the mean and 5th and 95th quantile estimates


# Streamflow----
png(paste0('Q_MedGI_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_b$Date), y = Q_b[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = Q_b[,-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(Q_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('Q_MedGI_2007_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_b$Date[which((Q_b$Date >= '2007-01-01') & (Q_b$Date <= '2007-12-31'))]), y = Q_b[which((Q_b$Date >= '2007-01-01') & (Q_b$Date <= '2007-12-31')),-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = Q_b[which((Q_b$Date >= '2007-01-01') & (Q_b$Date <= '2007-12-31')),-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(Q_b$Date[which((Q_b$Date >= '2007-01-01') & (Q_b$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdQ = apply(X = Q_b[,-1], MARGIN = 1, FUN = sd)
meanQ = apply(X = Q_b[,-1], MARGIN = 1, FUN = mean)
CVQ = sdQ/meanQ

png(paste0('Q_CVQ_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_b$Date), y = CVQ, xlab = 'Year', ylab = 'CV Streamflow (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('Q_sdQ_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_b$Date), y = sdQ, xlab = 'Year', ylab = 'Std. Dev. Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
dev.off()

#Compare to without GI
png(paste0('Q_GI+NoGI_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_b$Date), y = Q_b[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
par(new=TRUE)
plot(x = as.Date(Q_b$Date), y = SimB$streamflow, col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()

#Check that all GI streamflows are <= without GI
for (i in 1:100){
  if (!all(Q_b[,1+i] < SimB$streamflow)){
    print(paste('i = ', i, length(which(Q_b[,1+i] >= SimB$streamflow))))
  }
}

#Plots that compare reduction in streamflow
png(paste0('Q_MinusGI_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_b$Date), y = Q_b[,-1] - SimB$streamflow, col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,-50,-0.01), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = Q_b[,-1] - SimB$streamflow, MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(Q_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('Q_MinusGI_2007_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_b$Date[which((Q_b$Date >= '2007-01-01') & (Q_b$Date <= '2007-12-31'))]), y = Q_b[which((Q_b$Date >= '2007-01-01') & (Q_b$Date <= '2007-12-31')),-1] - SimB$streamflow[which((Q_b$Date >= '2007-01-01') & (Q_b$Date <= '2007-12-31'))], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,-50,-0.01), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = Q_b[which((Q_b$Date >= '2007-01-01') & (Q_b$Date <= '2007-12-31')),-1] - SimB$streamflow[which((Q_b$Date >= '2007-01-01') & (Q_b$Date <= '2007-12-31'))], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(Q_b$Date[which((Q_b$Date >= '2007-01-01') & (Q_b$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdQ_MinGI = apply(X = Q_b[,-1] - SimB$streamflow, MARGIN = 1, FUN = sd)
meanQ_MinGI = apply(X = Q_b[,-1] - SimB$streamflow, MARGIN = 1, FUN = mean)
CVQ_MinGI = sdQ_MinGI/meanQ_MinGI

png(paste0('Q_MinusGI_CVQ_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_b$Date), y = CVQ_MinGI, xlab = 'Year', ylab = 'CV Streamflow (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(-1,1,0.1), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('Q_MinusGI_sdQ_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_b$Date), y = sdQ_MinGI, xlab = 'Year', ylab = 'Std. Dev. Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
dev.off()


# Sat Def----
png(paste0('SatDef_MedGI_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_b$Date), y = SatDef_b[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Sat. Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,5000,20), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = SatDef_b[,-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(SatDef_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('SatDef_MedGI_2007_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_b$Date[which((SatDef_b$Date >= '2007-01-01') & (SatDef_b$Date <= '2007-12-31'))]), y = SatDef_b[which((SatDef_b$Date >= '2007-01-01') & (SatDef_b$Date <= '2007-12-31')),-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Sat. Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,5000,20), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = SatDef_b[which((SatDef_b$Date >= '2007-01-01') & (SatDef_b$Date <= '2007-12-31')),-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(SatDef_b$Date[which((SatDef_b$Date >= '2007-01-01') & (SatDef_b$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdSatDef = apply(X = SatDef_b[,-1], MARGIN = 1, FUN = sd)
meanSatDef = apply(X = SatDef_b[,-1], MARGIN = 1, FUN = mean)
CVSatDef = sdSatDef/meanSatDef

png(paste0('SatDef_CV_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_b$Date), y = CVSatDef, xlab = 'Year', ylab = 'CV Sat. Def. (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.0001), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('SatDef_sd_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_b$Date), y = sdSatDef, xlab = 'Year', ylab = 'Std. Dev. Sat. Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,10,0.1), labels = TRUE, cex.axis = 1.5)
dev.off()

#Compare to without GI
png(paste0('SatDef_GI+NoGI_b.png'), res = 300, height = 5, width=5, units = 'in')
plot(x = as.Date(SatDef_b$Date), y = c(max(SatDef_b[,-1]), min(SatDef_b[,-1]), max(SimB$sat_def), min(SimB$sat_def), rep(median(SimB$sat_def), nrow(SatDef_b)-4)), col = 'white', xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i")
par(new=TRUE)
matplot(x = as.Date(SatDef_b$Date), y = SatDef_b[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Sat. Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5, xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,5000,20), labels = TRUE, cex.axis = 1.5)
par(new=TRUE)
plot(x = as.Date(SatDef_b$Date), y = SimB$sat_def, col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()

#Check that all GI sat def are >= without GI
for (i in 1:100){
  if (!all(SatDef_b[,1+i] > SimB$sat_def)){
    print(paste('i = ', i, length(which(SatDef_b[,1+i] <= SimB$sat_def))))
  }
}

#Plots that compare reduction in streamflow
png(paste0('SatDef_MinusGI_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_b$Date), y = SatDef_b[,-1] - SimB$sat_def, col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Sat. Deficit: GI - No GI (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,500,5), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = SatDef_b[,-1] - SimB$sat_def, MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(SatDef_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('SatDef_MinusGI_2007_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_b$Date[which((SatDef_b$Date >= '2007-01-01') & (SatDef_b$Date <= '2007-12-31'))]), y = SatDef_b[which((SatDef_b$Date >= '2007-01-01') & (SatDef_b$Date <= '2007-12-31')),-1] - SimB$sat_def[which((SatDef_b$Date >= '2007-01-01') & (SatDef_b$Date <= '2007-12-31'))], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Sat. Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,500,5), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = SatDef_b[which((SatDef_b$Date >= '2007-01-01') & (SatDef_b$Date <= '2007-12-31')),-1] - SimB$sat_def[which((SatDef_b$Date >= '2007-01-01') & (SatDef_b$Date <= '2007-12-31'))], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(SatDef_b$Date[which((SatDef_b$Date >= '2007-01-01') & (SatDef_b$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdSatDef_MinGI = apply(X = SatDef_b[,-1] - SimB$sat_def, MARGIN = 1, FUN = sd)
meanSatDef_MinGI = apply(X = SatDef_b[,-1] - SimB$sat_def, MARGIN = 1, FUN = mean)
CVSatDef_MinGI = sdSatDef_MinGI/meanSatDef_MinGI

png(paste0('SatDef_MinusGI_CV_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_b$Date), y = CVSatDef_MinGI, xlab = 'Year', ylab = 'CV Sat. Def. (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(-1,1,0.01), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('SatDef_MinusGI_sd_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_b$Date), y = sdSatDef_MinGI, xlab = 'Year', ylab = 'Std. Dev. Sat. Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,5,0.1), labels = TRUE, cex.axis = 1.5)
dev.off()


# Detention Storage - Same----
png(paste0('DetStore_MedGI_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(DetStore_b$Date), y = DetStore_b[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Detention Storage (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,0.001), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = DetStore_b[,-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(DetStore_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('DetStore_MedGI_2007_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(DetStore_b$Date[which((DetStore_b$Date >= '2007-01-01') & (DetStore_b$Date <= '2007-12-31'))]), y = DetStore_b[which((DetStore_b$Date >= '2007-01-01') & (DetStore_b$Date <= '2007-12-31')),-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Detention Storage (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,0.001), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = DetStore_b[which((DetStore_b$Date >= '2007-01-01') & (DetStore_b$Date <= '2007-12-31')),-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(DetStore_b$Date[which((DetStore_b$Date >= '2007-01-01') & (DetStore_b$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdDetStore = apply(X = DetStore_b[,-1], MARGIN = 1, FUN = sd)
meanDetStore = apply(X = DetStore_b[,-1], MARGIN = 1, FUN = mean)
CVDetStore = sdDetStore/meanDetStore

png(paste0('DetStore_CV_b.png'), res = 300, height = 5, width=5, units = 'in')
plot(x = as.Date(DetStore_b$Date), y = CVDetStore, xlab = 'Year', ylab = 'CV Detention Storage (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,1000,0.0001), labels = TRUE, cex.axis = 1.5)
dev.off()


# ET----
png(paste0('ET_MedGI_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_b$Date), y = ET_b[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = ET_b[,-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(ET_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('ET_MedGI_2007_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_b$Date[which((ET_b$Date >= '2007-01-01') & (ET_b$Date <= '2007-12-31'))]), y = ET_b[which((ET_b$Date >= '2007-01-01') & (ET_b$Date <= '2007-12-31')),-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = ET_b[which((ET_b$Date >= '2007-01-01') & (ET_b$Date <= '2007-12-31')),-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(ET_b$Date[which((ET_b$Date >= '2007-01-01') & (ET_b$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdET = apply(X = ET_b[,-1], MARGIN = 1, FUN = sd)
meanET = apply(X = ET_b[,-1], MARGIN = 1, FUN = mean)
CVET = sdET/meanET

png(paste0('ET_CV_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_b$Date), y = CVET, xlab = 'Year', ylab = 'CV ET (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('ET_sd_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_b$Date), y = sdET, xlab = 'Year', ylab = 'Std. Dev. ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
dev.off()

#Compare to without GI
png(paste0('ET_GI+NoGI_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_b$Date), y = ET_b[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
par(new=TRUE)
plot(x = as.Date(ET_b$Date), y = SimB$ET, col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()

#Check that all GI streamflows are <= without GI
for (i in 1:100){
  if (!all(ET_b[,1+i] > SimB$ET)){
    print(paste('i = ', i, length(which(ET_b[,1+i] <= SimB$ET))))
  }
}
#Peak ET > with GI, but off peak can be less

#Plots that compare reduction in streamflow
png(paste0('ET_MinusGI_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_b$Date), y = ET_b[,-1] - SimB$ET, col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,-50,-0.1), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = ET_b[,-1] - SimB$ET, MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(ET_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('ET_MinusGI_2007_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_b$Date[which((ET_b$Date >= '2007-01-01') & (ET_b$Date <= '2007-12-31'))]), y = ET_b[which((ET_b$Date >= '2007-01-01') & (ET_b$Date <= '2007-12-31')),-1] - SimB$ET[which((ET_b$Date >= '2007-01-01') & (ET_b$Date <= '2007-12-31'))], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,-50,-0.01), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = ET_b[which((ET_b$Date >= '2007-01-01') & (ET_b$Date <= '2007-12-31')),-1] - SimB$ET[which((ET_b$Date >= '2007-01-01') & (ET_b$Date <= '2007-12-31'))], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(ET_b$Date[which((ET_b$Date >= '2007-01-01') & (ET_b$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdET_MinGI = apply(X = ET_b[,-1] - SimB$ET, MARGIN = 1, FUN = sd)
meanET_MinGI = apply(X = ET_b[,-1] - SimB$ET, MARGIN = 1, FUN = mean)
CVET_MinGI = sdET_MinGI/meanET_MinGI

png(paste0('ET_MinusGI_CV_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_b$Date), y = CVET_MinGI, xlab = 'Year', ylab = 'CV ET (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(-2000,2000,10), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('ET_MinusGI_sd_b.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_b$Date), y = sdET_MinGI, xlab = 'Year', ylab = 'Std. Dev. ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,10,0.01), labels = TRUE, cex.axis = 1.5)
dev.off()


#Plots - Hillslope----
# Streamflow----
png(paste0('Q_MedGI_h.png'), res = 300, height = 10, width=10, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(Q_b$Date), y = t(Q_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,.1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = Q_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(Q_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h, quants, qlty)

png(paste0('Q_MedGI_2007_h.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(Q_b$Date), y = t(Q_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,.1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = Q_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(Q_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h, quants, qlty)

#Make a plot of the sd in streamflow for each day
sdQ_h = meanQ_h = CVQ_h = matrix(0, nrow = nrow(Area.Hills), ncol = length(Q_b$Date))
for (h in 1:nrow(sdQ_h)){
  sdQ_h[h,] = apply(X = Q_h[which(Q_h$HillID == h),-c(1,2)], MARGIN = 2, FUN = sd)
  meanQ_h[h,] = apply(X = Q_h[which(Q_h$HillID == h),-c(1,2)], MARGIN = 2, FUN = mean)
  CVQ_h[h,] = sdQ_h[h,]/meanQ_h[h,]
}
rm(h)

png(paste0('Q_CVQ_h.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(Q_b$Date), y = CVQ_h[h,],
          main = paste0('Hillslope ', uhills[h], ' CV Streamflow'), type = 'l', xlab = 'Year', ylab = 'CV Streamflow (1)',
          col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,1,.01), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('Q_CVQ_h_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  plot(x = as.Date(Q_b$Date), y = CVQ_h[h,],
       main = paste0('Hillslope ', uhills[h], ' CV Streamflow'), type = 'l', xlab = 'Year', ylab = 'CV Streamflow (1)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,1,.01), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('Q_sdQ_h.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(Q_b$Date), y = sdQ_h[h,],
       main = paste0('Hillslope ', uhills[h], ' SD Streamflow'), type = 'l', xlab = 'Year', ylab = 'Std. Dev. Streamflow (cfs)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('Q_sdQ_h_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  plot(x = as.Date(Q_b$Date), y = sdQ_h[h,],
       main = paste0('Hillslope ', uhills[h], ' SD Streamflow'), type = 'l', xlab = 'Year', ylab = 'Std. Dev. Streamflow (cfs)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)


#Compare to without GI
png(paste0('Q_GI+NoGI_h.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(Q_b$Date), y = t(Q_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,.1), labels = TRUE, cex.axis = 1.5)
  par(new=TRUE)
  plot(x = as.Date(Q_b$Date), y = SimH$streamflow[SimH$hillID == h], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, 
       lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Compare to without GI
png(paste0('Q_GI+NoGI_h_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(Q_b$Date), y = t(Q_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,.1), labels = TRUE, cex.axis = 1.5)
  par(new=TRUE)
  plot(x = as.Date(Q_b$Date), y = SimH$streamflow[SimH$hillID == h], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, 
       lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Plots that compare reduction in streamflow
png(paste0('Q_MinusGI_h.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(Q_b$Date), y = t(Q_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(1,-50,-.01), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(Q_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(Q_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('Q_MinusGI_h_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(Q_b$Date), y = t(Q_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,-50,-.1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(Q_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(Q_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('Q_MinusGI_2007_h.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(Q_b$Date), y = t(Q_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(1,-50,-.01), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(Q_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(Q_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('Q_MinusGI_2007_h_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(Q_b$Date), y = t(Q_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(1,-50,-.01), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(Q_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(Q_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

#Make a plot of the sd in streamflow for each day
sdQ_MinGI_h = meanQ_MinGI_h = CVQ_MinGI_h = matrix(0, nrow = nrow(Area.Hills), ncol = length(Q_b$Date))
for (h in 1:nrow(sdQ_MinGI_h)){
  sdQ_MinGI_h[h,] = apply(X = t(Q_h[which(Q_h$HillID == h),-c(1,2)]) - SimH$streamflow[SimH$hillID == h], MARGIN = 1, FUN = sd)
  meanQ_MinGI_h[h,] = apply(X = t(Q_h[which(Q_h$HillID == h),-c(1,2)]) - SimH$streamflow[SimH$hillID == h], MARGIN = 1, FUN = mean)
  CVQ_MinGI_h[h,] = sdQ_MinGI_h[h,]/meanQ_MinGI_h[h,]
}
rm(h)

png(paste0('Q_MinusGI_CVQ_h.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(Q_b$Date), y = CVQ_MinGI_h[h,], xlab = 'Year', ylab = 'CV Streamflow (1)', type = 'l', axes=FALSE, cex.lab = 1.5,
       main = paste0('Hillslope ', uhills[h], ' CV GI - No GI Streamflow'))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-1000,1000,1), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('Q_MinusGI_sdQ_h.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(Q_b$Date), y = sdQ_MinGI_h[h,], xlab = 'Year', ylab = 'Std. Dev. Streamflow (1)', type = 'l', axes=FALSE, cex.lab = 1.5,
       main = paste0('Hillslope ', uhills[h], ' SD GI - No GI Streamflow'))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-1000,1000,0.001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

# Sat Def----
png(paste0('SatDef_MedGI_h.png'), res = 300, height = 10, width=10, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(SatDef_b$Date), y = t(SatDef_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,5000,100), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = SatDef_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(SatDef_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h, quants, qlty)

png(paste0('SatDef_MedGI_2007_h.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(SatDef_b$Date), y = t(SatDef_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,5000,100), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = SatDef_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(SatDef_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h, quants, qlty)

#Make a plot of the sd in streamflow for each day
sdSatDef_h = meanSatDef_h = CVSatDef_h = matrix(0, nrow = nrow(Area.Hills), ncol = length(SatDef_b$Date))
for (h in 1:nrow(sdSatDef_h)){
  sdSatDef_h[h,] = apply(X = SatDef_h[which(SatDef_h$HillID == h),-c(1,2)], MARGIN = 2, FUN = sd)
  meanSatDef_h[h,] = apply(X = SatDef_h[which(SatDef_h$HillID == h),-c(1,2)], MARGIN = 2, FUN = mean)
  CVSatDef_h[h,] = sdSatDef_h[h,]/meanSatDef_h[h,]
}
rm(h)

png(paste0('SatDef_CV_h.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(SatDef_b$Date), y = CVSatDef_h[h,],
       main = paste0('Hillslope ', uhills[h], ' CV Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'CV Sat. Def. (1)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,10,.001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('SatDef_CV_h_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  plot(x = as.Date(SatDef_b$Date), y = CVSatDef_h[h,],
       main = paste0('Hillslope ', uhills[h], ' CV Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'CV Sat. Def. (1)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,1,.001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('SatDef_sd_h.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(SatDef_b$Date), y = sdSatDef_h[h,],
       main = paste0('Hillslope ', uhills[h], ' SD Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Std. Dev. Sat. Def. (mm)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,100,1), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('SatDef_sd_h_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  plot(x = as.Date(SatDef_b$Date), y = sdSatDef_h[h,],
       main = paste0('Hillslope ', uhills[h], ' SD Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Std. Dev. Sat. Def. (mm)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,1000,1), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)


#Compare to without GI
png(paste0('SatDef_GI+NoGI_h.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(SatDef_b$Date), y = c(max(t(SatDef_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)])), min(t(SatDef_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)])), max(SimH$sat_def[SimH$hillID == h]), min(SimH$sat_def[SimH$hillID == h]), rep(median(SimH$sat_def[SimH$hillID == h]), nrow(SatDef_b)-4)), col = 'white', xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i")
  par(new=TRUE)
  matplot(x = as.Date(SatDef_b$Date), y = t(SatDef_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,5000,100), labels = TRUE, cex.axis = 1.5)
  par(new=TRUE)
  plot(x = as.Date(SatDef_b$Date), y = SimH$sat_def[SimH$hillID == h], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, 
       lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Compare to without GI
png(paste0('SatDef_GI+NoGI_h_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(SatDef_b$Date), y = t(SatDef_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,5000,100), labels = TRUE, cex.axis = 1.5)
  par(new=TRUE)
  plot(x = as.Date(SatDef_b$Date), y = SimH$sat_def[SimH$hillID == h], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, 
       lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Plots that compare reduction in streamflow
png(paste0('SatDef_MinusGI_h.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(SatDef_b$Date), y = t(SatDef_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-500,500,10), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(SatDef_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(SatDef_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('SatDef_MinusGI_h_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(SatDef_b$Date), y = t(SatDef_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-500,500,0.001), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(SatDef_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(SatDef_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('SatDef_MinusGI_2007_h.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(SatDef_b$Date), y = t(SatDef_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-500,500,10), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(SatDef_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(SatDef_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('SatDef_MinusGI_2007_h_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(SatDef_b$Date), y = t(SatDef_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-500,500,.001), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(SatDef_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(SatDef_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

#Make a plot of the sd in streamflow for each day
sdSatDef_MinGI_h = meanSatDef_MinGI_h = CVSatDef_MinGI_h = matrix(0, nrow = nrow(Area.Hills), ncol = length(SatDef_b$Date))
for (h in 1:nrow(sdSatDef_MinGI_h)){
  sdSatDef_MinGI_h[h,] = apply(X = t(SatDef_h[which(SatDef_h$HillID == h),-c(1,2)]) - SimH$sat_def[SimH$hillID == h], MARGIN = 1, FUN = sd)
  meanSatDef_MinGI_h[h,] = apply(X = t(SatDef_h[which(SatDef_h$HillID == h),-c(1,2)]) - SimH$sat_def[SimH$hillID == h], MARGIN = 1, FUN = mean)
  CVSatDef_MinGI_h[h,] = sdSatDef_MinGI_h[h,]/meanSatDef_MinGI_h[h,]
}
rm(h)

png(paste0('SatDef_MinusGI_CV_h.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(SatDef_b$Date), y = CVSatDef_MinGI_h[h,], xlab = 'Year', ylab = 'CV Sat. Def. (1)', type = 'l', axes=FALSE, cex.lab = 1.5,
       main = paste0('Hillslope ', uhills[h], ' CV GI - No GI Sat. Def.'))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-1000,1000,0.01), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('SatDef_MinusGI_sd_h.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(SatDef_b$Date), y = sdSatDef_MinGI_h[h,], xlab = 'Year', ylab = 'Std. Dev. Sat. Def. (1)', type = 'l', axes=FALSE, cex.lab = 1.5,
       main = paste0('Hillslope ', uhills[h], ' SD GI - No GI Sat. Def.'))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-1000,1000,1), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

# ET----
png(paste0('ET_MedGI_h.png'), res = 300, height = 10, width=10, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(ET_b$Date), y = t(ET_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = ET_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(ET_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h, quants, qlty)

png(paste0('ET_MedGI_2007_h.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(ET_b$Date), y = t(ET_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = ET_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(ET_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h, quants, qlty)

#Make a plot of the sd in streamflow for each day
sdET_h = meanET_h = CVET_h = matrix(0, nrow = nrow(Area.Hills), ncol = length(ET_b$Date))
for (h in 1:nrow(sdET_h)){
  sdET_h[h,] = apply(X = ET_h[which(ET_h$HillID == h),-c(1,2)], MARGIN = 2, FUN = sd)
  meanET_h[h,] = apply(X = ET_h[which(ET_h$HillID == h),-c(1,2)], MARGIN = 2, FUN = mean)
  CVET_h[h,] = sdET_h[h,]/meanET_h[h,]
}
rm(h)

png(paste0('ET_CV_h.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(ET_b$Date), y = CVET_h[h,],
       main = paste0('Hillslope ', uhills[h], ' CV ET'), type = 'l', xlab = 'Year', ylab = 'CV ET (1)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,10,.01), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('ET_CV_h_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  plot(x = as.Date(ET_b$Date), y = CVET_h[h,],
       main = paste0('Hillslope ', uhills[h], ' CV ET'), type = 'l', xlab = 'Year', ylab = 'CV ET (1)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,1,.0001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('ET_sd_h.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(ET_b$Date), y = sdET_h[h,],
       main = paste0('Hillslope ', uhills[h], ' SD ET'), type = 'l', xlab = 'Year', ylab = 'Std. Dev. ET (mm)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,100,0.01), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('ET_sd_h_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  plot(x = as.Date(ET_b$Date), y = sdET_h[h,],
       main = paste0('Hillslope ', uhills[h], ' SD ET'), type = 'l', xlab = 'Year', ylab = 'Std. Dev. ET (mm)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,1,.0001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)


#Compare to without GI
png(paste0('ET_GI+NoGI_h.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(ET_b$Date), y = t(ET_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,10,1), labels = TRUE, cex.axis = 1.5)
  par(new=TRUE)
  plot(x = as.Date(ET_b$Date), y = SimH$ET[SimH$hillID == h], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, 
       lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Compare to without GI
png(paste0('ET_GI+NoGI_h_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(ET_b$Date), y = t(ET_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,1), labels = TRUE, cex.axis = 1.5)
  par(new=TRUE)
  plot(x = as.Date(ET_b$Date), y = SimH$ET[SimH$hillID == h], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, 
       lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Plots that compare reduction in streamflow
png(paste0('ET_MinusGI_h.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(ET_b$Date), y = t(ET_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-50,50,0.2), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(ET_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(ET_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('ET_MinusGI_h_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(ET_b$Date), y = t(ET_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-50,50,0.1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(ET_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(ET_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('ET_MinusGI_2007_h.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(ET_b$Date), y = t(ET_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-50,50,1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(ET_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(ET_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('ET_MinusGI_2007_h_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(ET_b$Date), y = t(ET_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-50,50,.1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(ET_h[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(ET_b$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

#Make a plot of the sd in streamflow for each day
sdET_MinGI_h = meanET_MinGI_h = CVET_MinGI_h = matrix(0, nrow = nrow(Area.Hills), ncol = length(ET_b$Date))
for (h in 1:nrow(sdET_MinGI_h)){
  sdET_MinGI_h[h,] = apply(X = t(ET_h[which(ET_h$HillID == h),-c(1,2)]) - SimH$ET[SimH$hillID == h], MARGIN = 1, FUN = sd)
  meanET_MinGI_h[h,] = apply(X = t(ET_h[which(ET_h$HillID == h),-c(1,2)]) - SimH$ET[SimH$hillID == h], MARGIN = 1, FUN = mean)
  CVET_MinGI_h[h,] = sdET_MinGI_h[h,]/meanET_MinGI_h[h,]
}
rm(h)

png(paste0('ET_MinusGI_CV_h.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(ET_b$Date), y = CVET_MinGI_h[h,], xlab = 'Year', ylab = 'CV ET (1)', type = 'l', axes=FALSE, cex.lab = 1.5,
       main = paste0('Hillslope ', uhills[h], ' CV GI - No GI ET'))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-100,100,1), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('ET_MinusGI_sd_h.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(ET_b$Date), y = sdET_MinGI_h[h,], xlab = 'Year', ylab = 'Std. Dev. ET (1)', type = 'l', axes=FALSE, cex.lab = 1.5,
       main = paste0('Hillslope ', uhills[h], ' SD GI - No GI ET'))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,100,.01), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

##Maps----
##  Make the worldfile a spatial dataframe to get a map. Plot information in the worldfile on the maps----
#coordinates(world) = c('patchX', 'patchY')
#proj4string(world) = CRS('+init=epsg:26918')
##Change to degrees
#world=spTransform(world, CRSobj = CRS('+init=epsg:4326'))
#
#cols = rainbow(n = length(uhills))
#
##Fixme: add stream to this map (white?)
#png('hillslopeMap.png', res = 300, height = 6, width = 6, units ='in')
#par(mar= c(2.5,2.5,1,1))
#plot(world, col = 'white')
#for (h in 1:length(uhills)){
#  plot(world[world$hillID == uhills[h],], pch = 22, add = TRUE, lwd=10)
#  plot(world[world$hillID == uhills[h],], col = cols[h], pch = 15, add = TRUE)
#}
#legend('bottomright', title = expression(bold('Hillslope')), legend=seq(1,length(uhills),1), fill = cols, border = 'black', ncol = 2)
#degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
#degAxis(side = 1, at = seq(-76.7,-76,.02))
#degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
#degAxis(side = 2, at = seq(39.45, 40,.01))
#degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
#north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
#text(x = -76.712, y = 39.467, 'WGS84')
#box(which = 'figure', lwd = 2)
#dev.off()
#rm(h)