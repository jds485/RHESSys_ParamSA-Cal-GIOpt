#Script to plot results from GI random seed analysis

library(vroom)
library(sp)
library(rgdal)

setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation")

#Number of random seeds run with each GI allocation
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

#Load the observed streamflow to compare the parameter set to it----
ObsQ = read.table("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\obs\\BaismanStreamflow_Feb2020Revised_Cal_p.txt", header = TRUE, sep = '\t', stringsAsFactors = FALSE)
#Compute the 5th and 95th %-iles for use later - using the calibration timeperiod
obsQuants = read.table("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\obs\\BaismanStreamflow_Feb2020Revised_Cal.txt", header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, sep = '\t')
#Remove observations later than 9/30/2013 (calibration timeperiod end)
obsQuants = obsQuants[as.Date(obsQuants$Date) <= as.Date('2013-09-30'),]

q05_cal = quantile(x = obsQuants$Flow, probs = 0.05)
q95_cal = quantile(x = obsQuants$Flow, probs = 0.95)

Year = unique(format(as.Date(obsQuants$Date), '%Y'))
for (i in 1:14){
  plot(x = as.Date(paste0(Year[i+1], '-01-01')), y = quantile(x = obsQuants$Flow[as.Date(obsQuants$Date) <= as.Date(paste0(Year[i+1], '-09-30'))], probs = 0.05), xlim = c(as.Date('1999-01-01'), as.Date('2014-01-01')), ylim = c(0,5), xlab = 'Year', ylab = 'Streamflow (cfs)')
  par(new = TRUE)
  plot(x = as.Date(paste0(Year[i+1], '-01-01')), y = quantile(x = obsQuants$Flow[as.Date(obsQuants$Date) <= as.Date(paste0(Year[i+1], '-09-30'))], probs = 0.95), xlim = c(as.Date('1999-01-01'), as.Date('2014-01-01')), ylim = c(0,5), col = 'red', axes = FALSE, xlab = '', ylab = '')
  par(new=T)
}
rm(i)

#Load streamflow likelihood parameters to plot error bars----
ErrParams = read.csv('Params_logLQ_Run192_Ch9.csv')

#Load the simulated data and trim to only the variables being compared----
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

#Compute quantiles----
q05_sim = quantile(x = SimB$streamflow, probs = 0.05)
q95_sim = quantile(x = SimB$streamflow, probs = 0.95)

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
rm(h)

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

#Upslope only
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation_Up")
Q_bu = read.table('FlowGI_c.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE)
SatDef_bu = read.table('SatDefGI_c.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE)
DetStore_bu = read.table('DetStoreGI_c.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE)
ET_bu = read.table('ETGI_c.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE)

Q_hu = read.table('FlowGI_c_h.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
SatDef_hu = read.table('SatDefGI_c_h.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
DetStore_hu = read.table('DetStoreGI_c_h.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
ET_hu = read.table('ETGI_c_h.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)

#Midslope only
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation_Mid")
Q_bm = read.table('FlowGI_c.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE)
SatDef_bm = read.table('SatDefGI_c.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE)
DetStore_bm = read.table('DetStoreGI_c.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE)
ET_bm = read.table('ETGI_c.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE)

Q_hm = read.table('FlowGI_c_h.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
SatDef_hm = read.table('SatDefGI_c_h.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
DetStore_hm = read.table('DetStoreGI_c_h.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
ET_hm = read.table('ETGI_c_h.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)

#Downslope only
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation_Down")
Q_bd = read.table('FlowGI_c.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE)
SatDef_bd = read.table('SatDefGI_c.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE)
DetStore_bd = read.table('DetStoreGI_c.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE)
ET_bd = read.table('ETGI_c.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE)

Q_hd = read.table('FlowGI_c_h.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
SatDef_hd = read.table('SatDefGI_c_h.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
DetStore_hd = read.table('DetStoreGI_c_h.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
ET_hd = read.table('ETGI_c_h.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)

#Plots - Basin ----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation")
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

#Get the mean and 5th and 95th quantile estimates for the timeseries

#Plot as a gray box for 5th - 95th, observations in black line, simulation in red line


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


#  Sum flow greater than 95th%-ile historical flows----
Sum95_b = apply(X = Q_b[,-1][which(Q_b[,-1] >= q95_cal),], MARGIN = 2, FUN = sum, na.rm=TRUE)
#  Sum flow less than 5th%-ile historical flows----
Sum05_b = apply(X = Q_b[,-1][which(Q_b[,-1] <= q05_cal),], MARGIN = 2, FUN = sum, na.rm=TRUE)
#  Sum reduction in flow for all historical flows----
SumAll_b = apply(X = Q_b[,-1] - SimB$streamflow, MARGIN = 2, FUN = sum)

#  Sum flow greater than 95th%-ile flows for this parameter without GI----
Sum95_b_sim = apply(X = Q_b[,-1][which(Q_b[,-1] >= q95_sim),], MARGIN = 2, FUN = sum, na.rm=TRUE)
#  Sum flow less than 5th%-ile flowsfor this parameter without GI----
Sum05_b_sim = apply(X = Q_b[,-1][which(Q_b[,-1] <= q05_sim),], MARGIN = 2, FUN = sum, na.rm=TRUE)
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

#Plots - Basin Upslope ----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation_Up")
# Streamflow----
png(paste0('Q_MedGI_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bu$Date), y = Q_bu[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = Q_bu[,-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(Q_bu$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('Q_MedGI_2007_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bu$Date[which((Q_bu$Date >= '2007-01-01') & (Q_bu$Date <= '2007-12-31'))]), y = Q_bu[which((Q_bu$Date >= '2007-01-01') & (Q_bu$Date <= '2007-12-31')),-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = Q_bu[which((Q_bu$Date >= '2007-01-01') & (Q_bu$Date <= '2007-12-31')),-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(Q_bu$Date[which((Q_bu$Date >= '2007-01-01') & (Q_bu$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdQ_bu = apply(X = Q_bu[,-1], MARGIN = 1, FUN = sd)
meanQ_bu = apply(X = Q_bu[,-1], MARGIN = 1, FUN = mean)
CVQ_bu = sdQ_bu/meanQ_bu

png(paste0('Q_CVQ_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bu$Date), y = CVQ_bu, xlab = 'Year', ylab = 'CV Streamflow (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('Q_sdQ_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bu$Date), y = sdQ_bu, xlab = 'Year', ylab = 'Std. Dev. Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
dev.off()

#Compare to without GI
png(paste0('Q_GI+NoGI_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bu$Date), y = Q_bu[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
par(new=TRUE)
plot(x = as.Date(Q_bu$Date), y = SimB$streamflow, col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()

#Check that all GI streamflows are <= without GI
for (i in 1:100){
  if (!all(Q_bu[,1+i] < SimB$streamflow)){
    print(paste('i = ', i, length(which(Q_bu[,1+i] >= SimB$streamflow))))
  }
}

#Plots that compare reduction in streamflow
png(paste0('Q_MinusGI_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bu$Date), y = Q_bu[,-1] - SimB$streamflow, col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,-50,-0.01), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = Q_bu[,-1] - SimB$streamflow, MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(Q_bu$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('Q_MinusGI_2007_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bu$Date[which((Q_bu$Date >= '2007-01-01') & (Q_bu$Date <= '2007-12-31'))]), y = Q_bu[which((Q_bu$Date >= '2007-01-01') & (Q_bu$Date <= '2007-12-31')),-1] - SimB$streamflow[which((Q_bu$Date >= '2007-01-01') & (Q_bu$Date <= '2007-12-31'))], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,-50,-0.01), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = Q_bu[which((Q_bu$Date >= '2007-01-01') & (Q_bu$Date <= '2007-12-31')),-1] - SimB$streamflow[which((Q_bu$Date >= '2007-01-01') & (Q_bu$Date <= '2007-12-31'))], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(Q_bu$Date[which((Q_bu$Date >= '2007-01-01') & (Q_bu$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdQ_MinGI_bu = apply(X = Q_bu[,-1] - SimB$streamflow, MARGIN = 1, FUN = sd)
meanQ_MinGI_bu = apply(X = Q_bu[,-1] - SimB$streamflow, MARGIN = 1, FUN = mean)
CVQ_MinGI_bu = sdQ_MinGI_bu/meanQ_MinGI_bu

png(paste0('Q_MinusGI_CVQ_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bu$Date), y = CVQ_MinGI_bu, xlab = 'Year', ylab = 'CV Streamflow (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(-1,1,0.1), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('Q_MinusGI_sdQ_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bu$Date), y = sdQ_MinGI_bu, xlab = 'Year', ylab = 'Std. Dev. Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
dev.off()


#  Sum flow greater than 95th%-ile historical flows----
Sum95_bu = apply(X = Q_bu[,-1][which(Q_bu[,-1] >= q95_cal),], MARGIN = 2, FUN = sum, na.rm=TRUE)
#  Sum flow less than 5th%-ile historical flows----
Sum05_bu = apply(X = Q_bu[,-1][which(Q_bu[,-1] <= q05_cal),], MARGIN = 2, FUN = sum, na.rm=TRUE)
#  Sum reduction in flow for all historical flows----
SumAll_bu = apply(X = Q_bu[,-1] - SimB$streamflow, MARGIN = 2, FUN = sum)
#  Sum flow greater than 95th%-ile flows for this parameter without GI----
Sum95_bu_sim = apply(X = Q_bu[,-1][which(Q_bu[,-1] >= q95_sim),], MARGIN = 2, FUN = sum, na.rm=TRUE)
#  Sum flow less than 5th%-ile flowsfor this parameter without GI----
Sum05_bu_sim = apply(X = Q_bu[,-1][which(Q_bu[,-1] <= q05_sim),], MARGIN = 2, FUN = sum, na.rm=TRUE)
# Sat Def----
png(paste0('SatDef_MedGI_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_bu$Date), y = SatDef_bu[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Sat. Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,5000,20), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = SatDef_bu[,-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(SatDef_bu$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('SatDef_MedGI_2007_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_bu$Date[which((SatDef_bu$Date >= '2007-01-01') & (SatDef_bu$Date <= '2007-12-31'))]), y = SatDef_bu[which((SatDef_bu$Date >= '2007-01-01') & (SatDef_bu$Date <= '2007-12-31')),-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Sat. Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,5000,20), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = SatDef_bu[which((SatDef_bu$Date >= '2007-01-01') & (SatDef_bu$Date <= '2007-12-31')),-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(SatDef_bu$Date[which((SatDef_bu$Date >= '2007-01-01') & (SatDef_bu$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdSatDef_bu = apply(X = SatDef_bu[,-1], MARGIN = 1, FUN = sd)
meanSatDef_bu = apply(X = SatDef_bu[,-1], MARGIN = 1, FUN = mean)
CVSatDef_bu = sdSatDef_bu/meanSatDef_bu

png(paste0('SatDef_CV_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_bu$Date), y = CVSatDef_bu, xlab = 'Year', ylab = 'CV Sat. Def. (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.0001), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('SatDef_sd_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_bu$Date), y = sdSatDef_bu, xlab = 'Year', ylab = 'Std. Dev. Sat. Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,10,0.1), labels = TRUE, cex.axis = 1.5)
dev.off()

#Compare to without GI
png(paste0('SatDef_GI+NoGI_bu.png'), res = 300, height = 5, width=5, units = 'in')
plot(x = as.Date(SatDef_bu$Date), y = c(max(SatDef_bu[,-1]), min(SatDef_bu[,-1]), max(SimB$sat_def), min(SimB$sat_def), rep(median(SimB$sat_def), nrow(SatDef_bu)-4)), col = 'white', xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i")
par(new=TRUE)
matplot(x = as.Date(SatDef_bu$Date), y = SatDef_bu[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Sat. Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5, xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,5000,20), labels = TRUE, cex.axis = 1.5)
par(new=TRUE)
plot(x = as.Date(SatDef_bu$Date), y = SimB$sat_def, col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()

#Check that all GI sat def are >= without GI
for (i in 1:100){
  if (!all(SatDef_bu[,1+i] > SimB$sat_def)){
    print(paste('i = ', i, length(which(SatDef_bu[,1+i] <= SimB$sat_def))))
  }
}

#Plots that compare reduction in streamflow
png(paste0('SatDef_MinusGI_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_bu$Date), y = SatDef_bu[,-1] - SimB$sat_def, col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Sat. Deficit: GI - No GI (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,500,5), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = SatDef_bu[,-1] - SimB$sat_def, MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(SatDef_bu$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('SatDef_MinusGI_2007_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_bu$Date[which((SatDef_bu$Date >= '2007-01-01') & (SatDef_bu$Date <= '2007-12-31'))]), y = SatDef_bu[which((SatDef_bu$Date >= '2007-01-01') & (SatDef_bu$Date <= '2007-12-31')),-1] - SimB$sat_def[which((SatDef_bu$Date >= '2007-01-01') & (SatDef_bu$Date <= '2007-12-31'))], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Sat. Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,500,5), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = SatDef_bu[which((SatDef_bu$Date >= '2007-01-01') & (SatDef_bu$Date <= '2007-12-31')),-1] - SimB$sat_def[which((SatDef_bu$Date >= '2007-01-01') & (SatDef_bu$Date <= '2007-12-31'))], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(SatDef_bu$Date[which((SatDef_bu$Date >= '2007-01-01') & (SatDef_bu$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdSatDef_MinGI_bu = apply(X = SatDef_bu[,-1] - SimB$sat_def, MARGIN = 1, FUN = sd)
meanSatDef_MinGI_bu = apply(X = SatDef_bu[,-1] - SimB$sat_def, MARGIN = 1, FUN = mean)
CVSatDef_MinGI_bu = sdSatDef_MinGI_bu/meanSatDef_MinGI_bu

png(paste0('SatDef_MinusGI_CV_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_bu$Date), y = CVSatDef_MinGI_bu, xlab = 'Year', ylab = 'CV Sat. Def. (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(-1,1,0.01), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('SatDef_MinusGI_sd_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_bu$Date), y = sdSatDef_MinGI_bu, xlab = 'Year', ylab = 'Std. Dev. Sat. Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,5,0.1), labels = TRUE, cex.axis = 1.5)
dev.off()


# Detention Storage - Same----
sdDetStore_bu = apply(X = DetStore_bu[,-1], MARGIN = 1, FUN = sd)
meanDetStore_bu = apply(X = DetStore_bu[,-1], MARGIN = 1, FUN = mean)
CVDetStore_bu = sdDetStore_bu/meanDetStore_bu

# ET----
png(paste0('ET_MedGI_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bu$Date), y = ET_bu[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = ET_bu[,-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(ET_bu$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('ET_MedGI_2007_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bu$Date[which((ET_bu$Date >= '2007-01-01') & (ET_bu$Date <= '2007-12-31'))]), y = ET_bu[which((ET_bu$Date >= '2007-01-01') & (ET_bu$Date <= '2007-12-31')),-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = ET_bu[which((ET_bu$Date >= '2007-01-01') & (ET_bu$Date <= '2007-12-31')),-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(ET_bu$Date[which((ET_bu$Date >= '2007-01-01') & (ET_bu$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdET_bu = apply(X = ET_bu[,-1], MARGIN = 1, FUN = sd)
meanET_bu = apply(X = ET_bu[,-1], MARGIN = 1, FUN = mean)
CVET_bu = sdET_bu/meanET_bu

png(paste0('ET_CV_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bu$Date), y = CVET_bu, xlab = 'Year', ylab = 'CV ET (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('ET_sd_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bu$Date), y = sdET_bu, xlab = 'Year', ylab = 'Std. Dev. ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
dev.off()

#Compare to without GI
png(paste0('ET_GI+NoGI_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bu$Date), y = ET_bu[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
par(new=TRUE)
plot(x = as.Date(ET_bu$Date), y = SimB$ET, col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()

#Check that all GI streamflows are <= without GI
for (i in 1:100){
  if (!all(ET_bu[,1+i] > SimB$ET)){
    print(paste('i = ', i, length(which(ET_bu[,1+i] <= SimB$ET))))
  }
}
#Peak ET > with GI, but off peak can be less

#Plots that compare reduction in streamflow
png(paste0('ET_MinusGI_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bu$Date), y = ET_bu[,-1] - SimB$ET, col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,-50,-0.1), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = ET_bu[,-1] - SimB$ET, MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(ET_bu$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('ET_MinusGI_2007_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bu$Date[which((ET_bu$Date >= '2007-01-01') & (ET_bu$Date <= '2007-12-31'))]), y = ET_bu[which((ET_bu$Date >= '2007-01-01') & (ET_bu$Date <= '2007-12-31')),-1] - SimB$ET[which((ET_bu$Date >= '2007-01-01') & (ET_bu$Date <= '2007-12-31'))], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,-50,-0.01), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = ET_bu[which((ET_bu$Date >= '2007-01-01') & (ET_bu$Date <= '2007-12-31')),-1] - SimB$ET[which((ET_bu$Date >= '2007-01-01') & (ET_bu$Date <= '2007-12-31'))], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(ET_bu$Date[which((ET_bu$Date >= '2007-01-01') & (ET_bu$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdET_MinGI_bu = apply(X = ET_bu[,-1] - SimB$ET, MARGIN = 1, FUN = sd)
meanET_MinGI_bu = apply(X = ET_bu[,-1] - SimB$ET, MARGIN = 1, FUN = mean)
CVET_MinGI_bu = sdET_MinGI_bu/meanET_MinGI_bu

png(paste0('ET_MinusGI_CV_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bu$Date), y = CVET_MinGI_bu, xlab = 'Year', ylab = 'CV ET (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(-2000,2000,10), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('ET_MinusGI_sd_bu.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bu$Date), y = sdET_MinGI_bu, xlab = 'Year', ylab = 'Std. Dev. ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,10,0.01), labels = TRUE, cex.axis = 1.5)
dev.off()


#Plots - Hillslope Upslope----
# Streamflow----
png(paste0('Q_MedGI_hu.png'), res = 300, height = 10, width=10, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(Q_bu$Date), y = t(Q_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,.1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = Q_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(Q_bu$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h, quants, qlty)

png(paste0('Q_MedGI_2007_hu.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(Q_bu$Date), y = t(Q_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,.1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = Q_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(Q_bu$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h, quants, qlty)

#Make a plot of the sd in streamflow for each day
sdQ_hu = meanQ_hu = CVQ_hu = matrix(0, nrow = nrow(Area.Hills), ncol = length(Q_bu$Date))
for (h in 1:nrow(sdQ_hu)){
  sdQ_hu[h,] = apply(X = Q_hu[which(Q_hu$HillID == h),-c(1,2)], MARGIN = 2, FUN = sd)
  meanQ_hu[h,] = apply(X = Q_hu[which(Q_hu$HillID == h),-c(1,2)], MARGIN = 2, FUN = mean)
  CVQ_hu[h,] = sdQ_hu[h,]/meanQ_hu[h,]
}
rm(h)

png(paste0('Q_CVQ_hu.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(Q_bu$Date), y = CVQ_hu[h,],
       main = paste0('Hillslope ', uhills[h], ' CV Streamflow'), type = 'l', xlab = 'Year', ylab = 'CV Streamflow (1)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,1,.01), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('Q_CVQ_hu_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  plot(x = as.Date(Q_bu$Date), y = CVQ_hu[h,],
       main = paste0('Hillslope ', uhills[h], ' CV Streamflow'), type = 'l', xlab = 'Year', ylab = 'CV Streamflow (1)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,1,.01), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('Q_sdQ_hu.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(Q_bu$Date), y = sdQ_hu[h,],
       main = paste0('Hillslope ', uhills[h], ' SD Streamflow'), type = 'l', xlab = 'Year', ylab = 'Std. Dev. Streamflow (cfs)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('Q_sdQ_hu_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  plot(x = as.Date(Q_bu$Date), y = sdQ_hu[h,],
       main = paste0('Hillslope ', uhills[h], ' SD Streamflow'), type = 'l', xlab = 'Year', ylab = 'Std. Dev. Streamflow (cfs)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)


#Compare to without GI
png(paste0('Q_GI+NoGI_hu.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(Q_bu$Date), y = t(Q_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,.1), labels = TRUE, cex.axis = 1.5)
  par(new=TRUE)
  plot(x = as.Date(Q_bu$Date), y = SimH$streamflow[SimH$hillID == h], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, 
       lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Compare to without GI
png(paste0('Q_GI+NoGI_hu_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(Q_bu$Date), y = t(Q_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,.1), labels = TRUE, cex.axis = 1.5)
  par(new=TRUE)
  plot(x = as.Date(Q_bu$Date), y = SimH$streamflow[SimH$hillID == h], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, 
       lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Plots that compare reduction in streamflow
png(paste0('Q_MinusGI_hu.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(Q_bu$Date), y = t(Q_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(1,-50,-.01), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(Q_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(Q_bu$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('Q_MinusGI_hu_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(Q_bu$Date), y = t(Q_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,-50,-.1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(Q_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(Q_bu$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('Q_MinusGI_2007_hu.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(Q_bu$Date), y = t(Q_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(1,-50,-.01), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(Q_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(Q_bu$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('Q_MinusGI_2007_hu_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(Q_bu$Date), y = t(Q_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(1,-50,-.01), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(Q_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(Q_bu$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

#Make a plot of the sd in streamflow for each day
sdQ_MinGI_hu = meanQ_MinGI_hu = CVQ_MinGI_hu = matrix(0, nrow = nrow(Area.Hills), ncol = length(Q_bu$Date))
for (h in 1:nrow(sdQ_MinGI_hu)){
  sdQ_MinGI_hu[h,] = apply(X = t(Q_hu[which(Q_hu$HillID == h),-c(1,2)]) - SimH$streamflow[SimH$hillID == h], MARGIN = 1, FUN = sd)
  meanQ_MinGI_hu[h,] = apply(X = t(Q_hu[which(Q_hu$HillID == h),-c(1,2)]) - SimH$streamflow[SimH$hillID == h], MARGIN = 1, FUN = mean)
  CVQ_MinGI_hu[h,] = sdQ_MinGI_hu[h,]/meanQ_MinGI_hu[h,]
}
rm(h)

png(paste0('Q_MinusGI_CVQ_hu.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(Q_bu$Date), y = CVQ_MinGI_hu[h,], xlab = 'Year', ylab = 'CV Streamflow (1)', type = 'l', axes=FALSE, cex.lab = 1.5,
       main = paste0('Hillslope ', uhills[h], ' CV GI - No GI Streamflow'))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-1000,1000,1), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('Q_MinusGI_sdQ_hu.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(Q_bu$Date), y = sdQ_MinGI_hu[h,], xlab = 'Year', ylab = 'Std. Dev. Streamflow (1)', type = 'l', axes=FALSE, cex.lab = 1.5,
       main = paste0('Hillslope ', uhills[h], ' SD GI - No GI Streamflow'))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-1000,1000,0.001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

# Sat Def----
png(paste0('SatDef_MedGI_hu.png'), res = 300, height = 10, width=10, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(SatDef_bu$Date), y = t(SatDef_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,5000,100), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = SatDef_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(SatDef_bu$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h, quants, qlty)

png(paste0('SatDef_MedGI_2007_hu.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(SatDef_bu$Date), y = t(SatDef_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,5000,100), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = SatDef_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(SatDef_bu$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h, quants, qlty)

#Make a plot of the sd in streamflow for each day
sdSatDef_hu = meanSatDef_hu = CVSatDef_hu = matrix(0, nrow = nrow(Area.Hills), ncol = length(SatDef_bu$Date))
for (h in 1:nrow(sdSatDef_hu)){
  sdSatDef_hu[h,] = apply(X = SatDef_hu[which(SatDef_hu$HillID == h),-c(1,2)], MARGIN = 2, FUN = sd)
  meanSatDef_hu[h,] = apply(X = SatDef_hu[which(SatDef_hu$HillID == h),-c(1,2)], MARGIN = 2, FUN = mean)
  CVSatDef_hu[h,] = sdSatDef_hu[h,]/meanSatDef_hu[h,]
}
rm(h)

png(paste0('SatDef_CV_hu.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(SatDef_bu$Date), y = CVSatDef_hu[h,],
       main = paste0('Hillslope ', uhills[h], ' CV Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'CV Sat. Def. (1)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,10,.001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('SatDef_CV_hu_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  plot(x = as.Date(SatDef_bu$Date), y = CVSatDef_hu[h,],
       main = paste0('Hillslope ', uhills[h], ' CV Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'CV Sat. Def. (1)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,1,.001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('SatDef_sd_hu.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(SatDef_bu$Date), y = sdSatDef_hu[h,],
       main = paste0('Hillslope ', uhills[h], ' SD Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Std. Dev. Sat. Def. (mm)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,100,1), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('SatDef_sd_hu_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  plot(x = as.Date(SatDef_bu$Date), y = sdSatDef_hu[h,],
       main = paste0('Hillslope ', uhills[h], ' SD Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Std. Dev. Sat. Def. (mm)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,1000,1), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)


#Compare to without GI
png(paste0('SatDef_GI+NoGI_hu.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(SatDef_bu$Date), y = c(max(t(SatDef_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)])), min(t(SatDef_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)])), max(SimH$sat_def[SimH$hillID == h]), min(SimH$sat_def[SimH$hillID == h]), rep(median(SimH$sat_def[SimH$hillID == h]), nrow(SatDef_bu)-4)), col = 'white', xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i")
  par(new=TRUE)
  matplot(x = as.Date(SatDef_bu$Date), y = t(SatDef_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,5000,100), labels = TRUE, cex.axis = 1.5)
  par(new=TRUE)
  plot(x = as.Date(SatDef_bu$Date), y = SimH$sat_def[SimH$hillID == h], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, 
       lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Compare to without GI
png(paste0('SatDef_GI+NoGI_hu_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(SatDef_bu$Date), y = t(SatDef_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,5000,100), labels = TRUE, cex.axis = 1.5)
  par(new=TRUE)
  plot(x = as.Date(SatDef_bu$Date), y = SimH$sat_def[SimH$hillID == h], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, 
       lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Plots that compare reduction in streamflow
png(paste0('SatDef_MinusGI_hu.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(SatDef_bu$Date), y = t(SatDef_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-500,500,10), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(SatDef_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(SatDef_bu$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('SatDef_MinusGI_hu_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(SatDef_bu$Date), y = t(SatDef_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-500,500,0.001), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(SatDef_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(SatDef_bu$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('SatDef_MinusGI_2007_hu.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(SatDef_bu$Date), y = t(SatDef_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-500,500,10), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(SatDef_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(SatDef_bu$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('SatDef_MinusGI_2007_hu_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(SatDef_bu$Date), y = t(SatDef_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-500,500,.001), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(SatDef_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(SatDef_bu$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

#Make a plot of the sd in streamflow for each day
sdSatDef_MinGI_hu = meanSatDef_MinGI_hu = CVSatDef_MinGI_hu = matrix(0, nrow = nrow(Area.Hills), ncol = length(SatDef_bu$Date))
for (h in 1:nrow(sdSatDef_MinGI_hu)){
  sdSatDef_MinGI_hu[h,] = apply(X = t(SatDef_hu[which(SatDef_hu$HillID == h),-c(1,2)]) - SimH$sat_def[SimH$hillID == h], MARGIN = 1, FUN = sd)
  meanSatDef_MinGI_hu[h,] = apply(X = t(SatDef_hu[which(SatDef_hu$HillID == h),-c(1,2)]) - SimH$sat_def[SimH$hillID == h], MARGIN = 1, FUN = mean)
  CVSatDef_MinGI_hu[h,] = sdSatDef_MinGI_hu[h,]/meanSatDef_MinGI_hu[h,]
}
rm(h)

png(paste0('SatDef_MinusGI_CV_hu.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(SatDef_bu$Date), y = CVSatDef_MinGI_hu[h,], xlab = 'Year', ylab = 'CV Sat. Def. (1)', type = 'l', axes=FALSE, cex.lab = 1.5,
       main = paste0('Hillslope ', uhills[h], ' CV GI - No GI Sat. Def.'))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-1000,1000,0.01), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('SatDef_MinusGI_sd_hu.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(SatDef_bu$Date), y = sdSatDef_MinGI_hu[h,], xlab = 'Year', ylab = 'Std. Dev. Sat. Def. (1)', type = 'l', axes=FALSE, cex.lab = 1.5,
       main = paste0('Hillslope ', uhills[h], ' SD GI - No GI Sat. Def.'))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-1000,1000,1), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

# ET----
png(paste0('ET_MedGI_hu.png'), res = 300, height = 10, width=10, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(ET_bu$Date), y = t(ET_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = ET_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(ET_bu$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h, quants, qlty)

png(paste0('ET_MedGI_2007_hu.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(ET_bu$Date), y = t(ET_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = ET_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(ET_bu$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h, quants, qlty)

#Make a plot of the sd in streamflow for each day
sdET_hu = meanET_hu = CVET_hu = matrix(0, nrow = nrow(Area.Hills), ncol = length(ET_bu$Date))
for (h in 1:nrow(sdET_hu)){
  sdET_hu[h,] = apply(X = ET_hu[which(ET_hu$HillID == h),-c(1,2)], MARGIN = 2, FUN = sd)
  meanET_hu[h,] = apply(X = ET_hu[which(ET_hu$HillID == h),-c(1,2)], MARGIN = 2, FUN = mean)
  CVET_hu[h,] = sdET_hu[h,]/meanET_hu[h,]
}
rm(h)

png(paste0('ET_CV_hu.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(ET_bu$Date), y = CVET_hu[h,],
       main = paste0('Hillslope ', uhills[h], ' CV ET'), type = 'l', xlab = 'Year', ylab = 'CV ET (1)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,10,.01), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('ET_CV_hu_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  plot(x = as.Date(ET_bu$Date), y = CVET_hu[h,],
       main = paste0('Hillslope ', uhills[h], ' CV ET'), type = 'l', xlab = 'Year', ylab = 'CV ET (1)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,1,.0001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('ET_sd_hu.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(ET_bu$Date), y = sdET_hu[h,],
       main = paste0('Hillslope ', uhills[h], ' SD ET'), type = 'l', xlab = 'Year', ylab = 'Std. Dev. ET (mm)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,100,0.01), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('ET_sd_hu_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  plot(x = as.Date(ET_bu$Date), y = sdET_hu[h,],
       main = paste0('Hillslope ', uhills[h], ' SD ET'), type = 'l', xlab = 'Year', ylab = 'Std. Dev. ET (mm)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,1,.0001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)


#Compare to without GI
png(paste0('ET_GI+NoGI_hu.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(ET_bu$Date), y = t(ET_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,10,1), labels = TRUE, cex.axis = 1.5)
  par(new=TRUE)
  plot(x = as.Date(ET_bu$Date), y = SimH$ET[SimH$hillID == h], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, 
       lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Compare to without GI
png(paste0('ET_GI+NoGI_hu_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(ET_bu$Date), y = t(ET_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,1), labels = TRUE, cex.axis = 1.5)
  par(new=TRUE)
  plot(x = as.Date(ET_bu$Date), y = SimH$ET[SimH$hillID == h], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, 
       lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Plots that compare reduction in streamflow
png(paste0('ET_MinusGI_hu.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(ET_bu$Date), y = t(ET_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-50,50,0.2), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(ET_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(ET_bu$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('ET_MinusGI_hu_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(ET_bu$Date), y = t(ET_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-50,50,0.1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(ET_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(ET_bu$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('ET_MinusGI_2007_hu.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(ET_bu$Date), y = t(ET_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-50,50,1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(ET_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(ET_bu$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('ET_MinusGI_2007_hu_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(ET_bu$Date), y = t(ET_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-50,50,.1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(ET_hu[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(ET_bu$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

#Make a plot of the sd in streamflow for each day
sdET_MinGI_hu = meanET_MinGI_hu = CVET_MinGI_hu = matrix(0, nrow = nrow(Area.Hills), ncol = length(ET_bu$Date))
for (h in 1:nrow(sdET_MinGI_hu)){
  sdET_MinGI_hu[h,] = apply(X = t(ET_hu[which(ET_hu$HillID == h),-c(1,2)]) - SimH$ET[SimH$hillID == h], MARGIN = 1, FUN = sd)
  meanET_MinGI_hu[h,] = apply(X = t(ET_hu[which(ET_hu$HillID == h),-c(1,2)]) - SimH$ET[SimH$hillID == h], MARGIN = 1, FUN = mean)
  CVET_MinGI_hu[h,] = sdET_MinGI_hu[h,]/meanET_MinGI_hu[h,]
}
rm(h)

png(paste0('ET_MinusGI_CV_hu.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(ET_bu$Date), y = CVET_MinGI_hu[h,], xlab = 'Year', ylab = 'CV ET (1)', type = 'l', axes=FALSE, cex.lab = 1.5,
       main = paste0('Hillslope ', uhills[h], ' CV GI - No GI ET'))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-100,100,1), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('ET_MinusGI_sd_hu.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(ET_bu$Date), y = sdET_MinGI_hu[h,], xlab = 'Year', ylab = 'Std. Dev. ET (1)', type = 'l', axes=FALSE, cex.lab = 1.5,
       main = paste0('Hillslope ', uhills[h], ' SD GI - No GI ET'))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,100,.01), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)


#Plots - Basin Midslope ----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation_Mid")
# Streamflow----
png(paste0('Q_MedGI_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bm$Date), y = Q_bm[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = Q_bm[,-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(Q_bm$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('Q_MedGI_2007_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bm$Date[which((Q_bm$Date >= '2007-01-01') & (Q_bm$Date <= '2007-12-31'))]), y = Q_bm[which((Q_bm$Date >= '2007-01-01') & (Q_bm$Date <= '2007-12-31')),-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = Q_bm[which((Q_bm$Date >= '2007-01-01') & (Q_bm$Date <= '2007-12-31')),-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(Q_bm$Date[which((Q_bm$Date >= '2007-01-01') & (Q_bm$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdQ_bm = apply(X = Q_bm[,-1], MARGIN = 1, FUN = sd)
meanQ_bm = apply(X = Q_bm[,-1], MARGIN = 1, FUN = mean)
CVQ_bm = sdQ_bm/meanQ_bm

png(paste0('Q_CVQ_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bm$Date), y = CVQ_bm, xlab = 'Year', ylab = 'CV Streamflow (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('Q_sdQ_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bm$Date), y = sdQ_bm, xlab = 'Year', ylab = 'Std. Dev. Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
dev.off()

#Compare to without GI
png(paste0('Q_GI+NoGI_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bm$Date), y = Q_bm[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
par(new=TRUE)
plot(x = as.Date(Q_bm$Date), y = SimB$streamflow, col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()

#Check that all GI streamflows are <= without GI
for (i in 1:100){
  if (!all(Q_bm[,1+i] < SimB$streamflow)){
    print(paste('i = ', i, length(which(Q_bm[,1+i] >= SimB$streamflow))))
  }
}

#Plots that compare reduction in streamflow
png(paste0('Q_MinusGI_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bm$Date), y = Q_bm[,-1] - SimB$streamflow, col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,-50,-0.01), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = Q_bm[,-1] - SimB$streamflow, MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(Q_bm$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('Q_MinusGI_2007_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bm$Date[which((Q_bm$Date >= '2007-01-01') & (Q_bm$Date <= '2007-12-31'))]), y = Q_bm[which((Q_bm$Date >= '2007-01-01') & (Q_bm$Date <= '2007-12-31')),-1] - SimB$streamflow[which((Q_bm$Date >= '2007-01-01') & (Q_bm$Date <= '2007-12-31'))], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,-50,-0.01), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = Q_bm[which((Q_bm$Date >= '2007-01-01') & (Q_bm$Date <= '2007-12-31')),-1] - SimB$streamflow[which((Q_bm$Date >= '2007-01-01') & (Q_bm$Date <= '2007-12-31'))], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(Q_bm$Date[which((Q_bm$Date >= '2007-01-01') & (Q_bm$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdQ_MinGI_bm = apply(X = Q_bm[,-1] - SimB$streamflow, MARGIN = 1, FUN = sd)
meanQ_MinGI_bm = apply(X = Q_bm[,-1] - SimB$streamflow, MARGIN = 1, FUN = mean)
CVQ_MinGI_bm = sdQ_MinGI_bm/meanQ_MinGI_bm

png(paste0('Q_MinusGI_CVQ_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bm$Date), y = CVQ_MinGI_bm, xlab = 'Year', ylab = 'CV Streamflow (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(-1,1,0.1), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('Q_MinusGI_sdQ_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bm$Date), y = sdQ_MinGI_bm, xlab = 'Year', ylab = 'Std. Dev. Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
dev.off()


#  Sum flow greater than 95th%-ile historical flows----
Sum95_bm = apply(X = Q_bm[,-1][which(Q_bm[,-1] >= q95_cal),], MARGIN = 2, FUN = sum, na.rm=TRUE)
#  Sum flow less than 5th%-ile historical flows----
Sum05_bm = apply(X = Q_bm[,-1][which(Q_bm[,-1] <= q05_cal),], MARGIN = 2, FUN = sum, na.rm=TRUE)
#  Sum reduction in flow for all historical flows----
SumAll_bm = apply(X = Q_bm[,-1] - SimB$streamflow, MARGIN = 2, FUN = sum)
# Sat Def----
png(paste0('SatDef_MedGI_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_bm$Date), y = SatDef_bm[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Sat. Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,5000,20), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = SatDef_bm[,-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(SatDef_bm$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('SatDef_MedGI_2007_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_bm$Date[which((SatDef_bm$Date >= '2007-01-01') & (SatDef_bm$Date <= '2007-12-31'))]), y = SatDef_bm[which((SatDef_bm$Date >= '2007-01-01') & (SatDef_bm$Date <= '2007-12-31')),-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Sat. Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,5000,20), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = SatDef_bm[which((SatDef_bm$Date >= '2007-01-01') & (SatDef_bm$Date <= '2007-12-31')),-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(SatDef_bm$Date[which((SatDef_bm$Date >= '2007-01-01') & (SatDef_bm$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdSatDef_bm = apply(X = SatDef_bm[,-1], MARGIN = 1, FUN = sd)
meanSatDef_bm = apply(X = SatDef_bm[,-1], MARGIN = 1, FUN = mean)
CVSatDef_bm = sdSatDef_bm/meanSatDef_bm

png(paste0('SatDef_CV_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_bm$Date), y = CVSatDef_bm, xlab = 'Year', ylab = 'CV Sat. Def. (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.0001), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('SatDef_sd_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_bm$Date), y = sdSatDef_bm, xlab = 'Year', ylab = 'Std. Dev. Sat. Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,10,0.1), labels = TRUE, cex.axis = 1.5)
dev.off()

#Compare to without GI
png(paste0('SatDef_GI+NoGI_bm.png'), res = 300, height = 5, width=5, units = 'in')
plot(x = as.Date(SatDef_bm$Date), y = c(max(SatDef_bm[,-1]), min(SatDef_bm[,-1]), max(SimB$sat_def), min(SimB$sat_def), rep(median(SimB$sat_def), nrow(SatDef_bm)-4)), col = 'white', xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i")
par(new=TRUE)
matplot(x = as.Date(SatDef_bm$Date), y = SatDef_bm[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Sat. Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5, xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,5000,20), labels = TRUE, cex.axis = 1.5)
par(new=TRUE)
plot(x = as.Date(SatDef_bm$Date), y = SimB$sat_def, col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()

#Check that all GI sat def are >= without GI
for (i in 1:100){
  if (!all(SatDef_bm[,1+i] > SimB$sat_def)){
    print(paste('i = ', i, length(which(SatDef_bm[,1+i] <= SimB$sat_def))))
  }
}

#Plots that compare reduction in streamflow
png(paste0('SatDef_MinusGI_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_bm$Date), y = SatDef_bm[,-1] - SimB$sat_def, col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Sat. Deficit: GI - No GI (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,500,5), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = SatDef_bm[,-1] - SimB$sat_def, MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(SatDef_bm$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('SatDef_MinusGI_2007_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_bm$Date[which((SatDef_bm$Date >= '2007-01-01') & (SatDef_bm$Date <= '2007-12-31'))]), y = SatDef_bm[which((SatDef_bm$Date >= '2007-01-01') & (SatDef_bm$Date <= '2007-12-31')),-1] - SimB$sat_def[which((SatDef_bm$Date >= '2007-01-01') & (SatDef_bm$Date <= '2007-12-31'))], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Sat. Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,500,5), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = SatDef_bm[which((SatDef_bm$Date >= '2007-01-01') & (SatDef_bm$Date <= '2007-12-31')),-1] - SimB$sat_def[which((SatDef_bm$Date >= '2007-01-01') & (SatDef_bm$Date <= '2007-12-31'))], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(SatDef_bm$Date[which((SatDef_bm$Date >= '2007-01-01') & (SatDef_bm$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdSatDef_MinGI_bm = apply(X = SatDef_bm[,-1] - SimB$sat_def, MARGIN = 1, FUN = sd)
meanSatDef_MinGI_bm = apply(X = SatDef_bm[,-1] - SimB$sat_def, MARGIN = 1, FUN = mean)
CVSatDef_MinGI_bm = sdSatDef_MinGI_bm/meanSatDef_MinGI_bm

png(paste0('SatDef_MinusGI_CV_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_bm$Date), y = CVSatDef_MinGI_bm, xlab = 'Year', ylab = 'CV Sat. Def. (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(-1,1,0.01), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('SatDef_MinusGI_sd_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_bm$Date), y = sdSatDef_MinGI_bm, xlab = 'Year', ylab = 'Std. Dev. Sat. Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,5,0.1), labels = TRUE, cex.axis = 1.5)
dev.off()


# Detention Storage - Same----
sdDetStore_bm = apply(X = DetStore_bm[,-1], MARGIN = 1, FUN = sd)
meanDetStore_bm = apply(X = DetStore_bm[,-1], MARGIN = 1, FUN = mean)
CVDetStore_bm = sdDetStore_bm/meanDetStore_bm

# ET----
png(paste0('ET_MedGI_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bm$Date), y = ET_bm[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = ET_bm[,-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(ET_bm$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('ET_MedGI_2007_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bm$Date[which((ET_bm$Date >= '2007-01-01') & (ET_bm$Date <= '2007-12-31'))]), y = ET_bm[which((ET_bm$Date >= '2007-01-01') & (ET_bm$Date <= '2007-12-31')),-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = ET_bm[which((ET_bm$Date >= '2007-01-01') & (ET_bm$Date <= '2007-12-31')),-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(ET_bm$Date[which((ET_bm$Date >= '2007-01-01') & (ET_bm$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdET_bm = apply(X = ET_bm[,-1], MARGIN = 1, FUN = sd)
meanET_bm = apply(X = ET_bm[,-1], MARGIN = 1, FUN = mean)
CVET_bm = sdET_bm/meanET_bm

png(paste0('ET_CV_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bm$Date), y = CVET_bm, xlab = 'Year', ylab = 'CV ET (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('ET_sd_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bm$Date), y = sdET_bm, xlab = 'Year', ylab = 'Std. Dev. ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
dev.off()

#Compare to without GI
png(paste0('ET_GI+NoGI_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bm$Date), y = ET_bm[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
par(new=TRUE)
plot(x = as.Date(ET_bm$Date), y = SimB$ET, col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()

#Check that all GI streamflows are <= without GI
for (i in 1:100){
  if (!all(ET_bm[,1+i] > SimB$ET)){
    print(paste('i = ', i, length(which(ET_bm[,1+i] <= SimB$ET))))
  }
}
#Peak ET > with GI, but off peak can be less

#Plots that compare reduction in streamflow
png(paste0('ET_MinusGI_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bm$Date), y = ET_bm[,-1] - SimB$ET, col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,-50,-0.1), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = ET_bm[,-1] - SimB$ET, MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(ET_bm$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('ET_MinusGI_2007_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bm$Date[which((ET_bm$Date >= '2007-01-01') & (ET_bm$Date <= '2007-12-31'))]), y = ET_bm[which((ET_bm$Date >= '2007-01-01') & (ET_bm$Date <= '2007-12-31')),-1] - SimB$ET[which((ET_bm$Date >= '2007-01-01') & (ET_bm$Date <= '2007-12-31'))], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,-50,-0.01), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = ET_bm[which((ET_bm$Date >= '2007-01-01') & (ET_bm$Date <= '2007-12-31')),-1] - SimB$ET[which((ET_bm$Date >= '2007-01-01') & (ET_bm$Date <= '2007-12-31'))], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(ET_bm$Date[which((ET_bm$Date >= '2007-01-01') & (ET_bm$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdET_MinGI_bm = apply(X = ET_bm[,-1] - SimB$ET, MARGIN = 1, FUN = sd)
meanET_MinGI_bm = apply(X = ET_bm[,-1] - SimB$ET, MARGIN = 1, FUN = mean)
CVET_MinGI_bm = sdET_MinGI_bm/meanET_MinGI_bm

png(paste0('ET_MinusGI_CV_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bm$Date), y = CVET_MinGI_bm, xlab = 'Year', ylab = 'CV ET (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(-2000,2000,10), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('ET_MinusGI_sd_bm.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bm$Date), y = sdET_MinGI_bm, xlab = 'Year', ylab = 'Std. Dev. ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,10,0.01), labels = TRUE, cex.axis = 1.5)
dev.off()


#Plots - Hillslope Midslope----
# Streamflow----
png(paste0('Q_MedGI_hm.png'), res = 300, height = 10, width=10, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(Q_bm$Date), y = t(Q_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,.1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = Q_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(Q_bm$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h, quants, qlty)

png(paste0('Q_MedGI_2007_hm.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(Q_bm$Date), y = t(Q_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,.1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = Q_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(Q_bm$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h, quants, qlty)

#Make a plot of the sd in streamflow for each day
sdQ_hm = meanQ_hm = CVQ_hm = matrix(0, nrow = nrow(Area.Hills), ncol = length(Q_bm$Date))
for (h in 1:nrow(sdQ_hm)){
  sdQ_hm[h,] = apply(X = Q_hm[which(Q_hm$HillID == h),-c(1,2)], MARGIN = 2, FUN = sd)
  meanQ_hm[h,] = apply(X = Q_hm[which(Q_hm$HillID == h),-c(1,2)], MARGIN = 2, FUN = mean)
  CVQ_hm[h,] = sdQ_hm[h,]/meanQ_hm[h,]
}
rm(h)

png(paste0('Q_CVQ_hm.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(Q_bm$Date), y = CVQ_hm[h,],
       main = paste0('Hillslope ', uhills[h], ' CV Streamflow'), type = 'l', xlab = 'Year', ylab = 'CV Streamflow (1)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,1,.01), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('Q_CVQ_hm_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  plot(x = as.Date(Q_bm$Date), y = CVQ_hm[h,],
       main = paste0('Hillslope ', uhills[h], ' CV Streamflow'), type = 'l', xlab = 'Year', ylab = 'CV Streamflow (1)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,1,.01), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('Q_sdQ_hm.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(Q_bm$Date), y = sdQ_hm[h,],
       main = paste0('Hillslope ', uhills[h], ' SD Streamflow'), type = 'l', xlab = 'Year', ylab = 'Std. Dev. Streamflow (cfs)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('Q_sdQ_hm_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  plot(x = as.Date(Q_bm$Date), y = sdQ_hm[h,],
       main = paste0('Hillslope ', uhills[h], ' SD Streamflow'), type = 'l', xlab = 'Year', ylab = 'Std. Dev. Streamflow (cfs)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)


#Compare to without GI
png(paste0('Q_GI+NoGI_hm.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(Q_bm$Date), y = t(Q_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,.1), labels = TRUE, cex.axis = 1.5)
  par(new=TRUE)
  plot(x = as.Date(Q_bm$Date), y = SimH$streamflow[SimH$hillID == h], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, 
       lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Compare to without GI
png(paste0('Q_GI+NoGI_hm_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(Q_bm$Date), y = t(Q_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,.1), labels = TRUE, cex.axis = 1.5)
  par(new=TRUE)
  plot(x = as.Date(Q_bm$Date), y = SimH$streamflow[SimH$hillID == h], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, 
       lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Plots that compare reduction in streamflow
png(paste0('Q_MinusGI_hm.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(Q_bm$Date), y = t(Q_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(1,-50,-.01), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(Q_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(Q_bm$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('Q_MinusGI_hm_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(Q_bm$Date), y = t(Q_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,-50,-.1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(Q_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(Q_bm$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('Q_MinusGI_2007_hm.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(Q_bm$Date), y = t(Q_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(1,-50,-.01), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(Q_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(Q_bm$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('Q_MinusGI_2007_hm_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(Q_bm$Date), y = t(Q_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(1,-50,-.01), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(Q_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(Q_bm$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

#Make a plot of the sd in streamflow for each day
sdQ_MinGI_hm = meanQ_MinGI_hm = CVQ_MinGI_hm = matrix(0, nrow = nrow(Area.Hills), ncol = length(Q_bm$Date))
for (h in 1:nrow(sdQ_MinGI_hm)){
  sdQ_MinGI_hm[h,] = apply(X = t(Q_hm[which(Q_hm$HillID == h),-c(1,2)]) - SimH$streamflow[SimH$hillID == h], MARGIN = 1, FUN = sd)
  meanQ_MinGI_hm[h,] = apply(X = t(Q_hm[which(Q_hm$HillID == h),-c(1,2)]) - SimH$streamflow[SimH$hillID == h], MARGIN = 1, FUN = mean)
  CVQ_MinGI_hm[h,] = sdQ_MinGI_hm[h,]/meanQ_MinGI_hm[h,]
}
rm(h)

png(paste0('Q_MinusGI_CVQ_hm.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(Q_bm$Date), y = CVQ_MinGI_hm[h,], xlab = 'Year', ylab = 'CV Streamflow (1)', type = 'l', axes=FALSE, cex.lab = 1.5,
       main = paste0('Hillslope ', uhills[h], ' CV GI - No GI Streamflow'))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-1000,1000,1), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('Q_MinusGI_sdQ_hm.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(Q_bm$Date), y = sdQ_MinGI_hm[h,], xlab = 'Year', ylab = 'Std. Dev. Streamflow (1)', type = 'l', axes=FALSE, cex.lab = 1.5,
       main = paste0('Hillslope ', uhills[h], ' SD GI - No GI Streamflow'))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-1000,1000,0.001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

# Sat Def----
png(paste0('SatDef_MedGI_hm.png'), res = 300, height = 10, width=10, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(SatDef_bm$Date), y = t(SatDef_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,5000,100), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = SatDef_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(SatDef_bm$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h, quants, qlty)

png(paste0('SatDef_MedGI_2007_hm.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(SatDef_bm$Date), y = t(SatDef_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,5000,100), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = SatDef_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(SatDef_bm$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h, quants, qlty)

#Make a plot of the sd in streamflow for each day
sdSatDef_hm = meanSatDef_hm = CVSatDef_hm = matrix(0, nrow = nrow(Area.Hills), ncol = length(SatDef_bm$Date))
for (h in 1:nrow(sdSatDef_hm)){
  sdSatDef_hm[h,] = apply(X = SatDef_hm[which(SatDef_hm$HillID == h),-c(1,2)], MARGIN = 2, FUN = sd)
  meanSatDef_hm[h,] = apply(X = SatDef_hm[which(SatDef_hm$HillID == h),-c(1,2)], MARGIN = 2, FUN = mean)
  CVSatDef_hm[h,] = sdSatDef_hm[h,]/meanSatDef_hm[h,]
}
rm(h)

png(paste0('SatDef_CV_hm.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(SatDef_bm$Date), y = CVSatDef_hm[h,],
       main = paste0('Hillslope ', uhills[h], ' CV Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'CV Sat. Def. (1)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,10,.001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('SatDef_CV_hm_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  plot(x = as.Date(SatDef_bm$Date), y = CVSatDef_hm[h,],
       main = paste0('Hillslope ', uhills[h], ' CV Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'CV Sat. Def. (1)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,1,.001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('SatDef_sd_hm.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(SatDef_bm$Date), y = sdSatDef_hm[h,],
       main = paste0('Hillslope ', uhills[h], ' SD Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Std. Dev. Sat. Def. (mm)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,100,1), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('SatDef_sd_hm_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  plot(x = as.Date(SatDef_bm$Date), y = sdSatDef_hm[h,],
       main = paste0('Hillslope ', uhills[h], ' SD Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Std. Dev. Sat. Def. (mm)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,1000,1), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)


#Compare to without GI
png(paste0('SatDef_GI+NoGI_hm.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(SatDef_bm$Date), y = c(max(t(SatDef_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)])), min(t(SatDef_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)])), max(SimH$sat_def[SimH$hillID == h]), min(SimH$sat_def[SimH$hillID == h]), rep(median(SimH$sat_def[SimH$hillID == h]), nrow(SatDef_bm)-4)), col = 'white', xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i")
  par(new=TRUE)
  matplot(x = as.Date(SatDef_bm$Date), y = t(SatDef_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,5000,100), labels = TRUE, cex.axis = 1.5)
  par(new=TRUE)
  plot(x = as.Date(SatDef_bm$Date), y = SimH$sat_def[SimH$hillID == h], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, 
       lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Compare to without GI
png(paste0('SatDef_GI+NoGI_hm_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(SatDef_bm$Date), y = t(SatDef_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,5000,100), labels = TRUE, cex.axis = 1.5)
  par(new=TRUE)
  plot(x = as.Date(SatDef_bm$Date), y = SimH$sat_def[SimH$hillID == h], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, 
       lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Plots that compare reduction in streamflow
png(paste0('SatDef_MinusGI_hm.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(SatDef_bm$Date), y = t(SatDef_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-500,500,10), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(SatDef_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(SatDef_bm$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('SatDef_MinusGI_hm_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(SatDef_bm$Date), y = t(SatDef_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-500,500,0.001), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(SatDef_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(SatDef_bm$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('SatDef_MinusGI_2007_hm.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(SatDef_bm$Date), y = t(SatDef_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-500,500,10), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(SatDef_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(SatDef_bm$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('SatDef_MinusGI_2007_hm_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(SatDef_bm$Date), y = t(SatDef_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-500,500,.001), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(SatDef_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(SatDef_bm$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

#Make a plot of the sd in streamflow for each day
sdSatDef_MinGI_hm = meanSatDef_MinGI_hm = CVSatDef_MinGI_hm = matrix(0, nrow = nrow(Area.Hills), ncol = length(SatDef_bm$Date))
for (h in 1:nrow(sdSatDef_MinGI_hm)){
  sdSatDef_MinGI_hm[h,] = apply(X = t(SatDef_hm[which(SatDef_hm$HillID == h),-c(1,2)]) - SimH$sat_def[SimH$hillID == h], MARGIN = 1, FUN = sd)
  meanSatDef_MinGI_hm[h,] = apply(X = t(SatDef_hm[which(SatDef_hm$HillID == h),-c(1,2)]) - SimH$sat_def[SimH$hillID == h], MARGIN = 1, FUN = mean)
  CVSatDef_MinGI_hm[h,] = sdSatDef_MinGI_hm[h,]/meanSatDef_MinGI_hm[h,]
}
rm(h)

png(paste0('SatDef_MinusGI_CV_hm.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(SatDef_bm$Date), y = CVSatDef_MinGI_hm[h,], xlab = 'Year', ylab = 'CV Sat. Def. (1)', type = 'l', axes=FALSE, cex.lab = 1.5,
       main = paste0('Hillslope ', uhills[h], ' CV GI - No GI Sat. Def.'))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-1000,1000,0.01), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('SatDef_MinusGI_sd_hm.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(SatDef_bm$Date), y = sdSatDef_MinGI_hm[h,], xlab = 'Year', ylab = 'Std. Dev. Sat. Def. (1)', type = 'l', axes=FALSE, cex.lab = 1.5,
       main = paste0('Hillslope ', uhills[h], ' SD GI - No GI Sat. Def.'))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-1000,1000,1), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

# ET----
png(paste0('ET_MedGI_hm.png'), res = 300, height = 10, width=10, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(ET_bm$Date), y = t(ET_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = ET_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(ET_bm$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h, quants, qlty)

png(paste0('ET_MedGI_2007_hm.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(ET_bm$Date), y = t(ET_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = ET_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(ET_bm$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h, quants, qlty)

#Make a plot of the sd in streamflow for each day
sdET_hm = meanET_hm = CVET_hm = matrix(0, nrow = nrow(Area.Hills), ncol = length(ET_bm$Date))
for (h in 1:nrow(sdET_hm)){
  sdET_hm[h,] = apply(X = ET_hm[which(ET_hm$HillID == h),-c(1,2)], MARGIN = 2, FUN = sd)
  meanET_hm[h,] = apply(X = ET_hm[which(ET_hm$HillID == h),-c(1,2)], MARGIN = 2, FUN = mean)
  CVET_hm[h,] = sdET_hm[h,]/meanET_hm[h,]
}
rm(h)

png(paste0('ET_CV_hm.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(ET_bm$Date), y = CVET_hm[h,],
       main = paste0('Hillslope ', uhills[h], ' CV ET'), type = 'l', xlab = 'Year', ylab = 'CV ET (1)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,10,.01), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('ET_CV_hm_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  plot(x = as.Date(ET_bm$Date), y = CVET_hm[h,],
       main = paste0('Hillslope ', uhills[h], ' CV ET'), type = 'l', xlab = 'Year', ylab = 'CV ET (1)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,1,.0001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('ET_sd_hm.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(ET_bm$Date), y = sdET_hm[h,],
       main = paste0('Hillslope ', uhills[h], ' SD ET'), type = 'l', xlab = 'Year', ylab = 'Std. Dev. ET (mm)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,100,0.01), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('ET_sd_hm_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  plot(x = as.Date(ET_bm$Date), y = sdET_hm[h,],
       main = paste0('Hillslope ', uhills[h], ' SD ET'), type = 'l', xlab = 'Year', ylab = 'Std. Dev. ET (mm)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,1,.0001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)


#Compare to without GI
png(paste0('ET_GI+NoGI_hm.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(ET_bm$Date), y = t(ET_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,10,1), labels = TRUE, cex.axis = 1.5)
  par(new=TRUE)
  plot(x = as.Date(ET_bm$Date), y = SimH$ET[SimH$hillID == h], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, 
       lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Compare to without GI
png(paste0('ET_GI+NoGI_hm_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(ET_bm$Date), y = t(ET_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,1), labels = TRUE, cex.axis = 1.5)
  par(new=TRUE)
  plot(x = as.Date(ET_bm$Date), y = SimH$ET[SimH$hillID == h], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, 
       lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Plots that compare reduction in streamflow
png(paste0('ET_MinusGI_hm.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(ET_bm$Date), y = t(ET_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-50,50,0.2), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(ET_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(ET_bm$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('ET_MinusGI_hm_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(ET_bm$Date), y = t(ET_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-50,50,0.1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(ET_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(ET_bm$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('ET_MinusGI_2007_hm.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(ET_bm$Date), y = t(ET_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-50,50,1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(ET_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(ET_bm$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('ET_MinusGI_2007_hm_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(ET_bm$Date), y = t(ET_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-50,50,.1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(ET_hm[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(ET_bm$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

#Make a plot of the sd in streamflow for each day
sdET_MinGI_hm = meanET_MinGI_hm = CVET_MinGI_hm = matrix(0, nrow = nrow(Area.Hills), ncol = length(ET_bm$Date))
for (h in 1:nrow(sdET_MinGI_hm)){
  sdET_MinGI_hm[h,] = apply(X = t(ET_hm[which(ET_hm$HillID == h),-c(1,2)]) - SimH$ET[SimH$hillID == h], MARGIN = 1, FUN = sd)
  meanET_MinGI_hm[h,] = apply(X = t(ET_hm[which(ET_hm$HillID == h),-c(1,2)]) - SimH$ET[SimH$hillID == h], MARGIN = 1, FUN = mean)
  CVET_MinGI_hm[h,] = sdET_MinGI_hm[h,]/meanET_MinGI_hm[h,]
}
rm(h)

png(paste0('ET_MinusGI_CV_hm.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(ET_bm$Date), y = CVET_MinGI_hm[h,], xlab = 'Year', ylab = 'CV ET (1)', type = 'l', axes=FALSE, cex.lab = 1.5,
       main = paste0('Hillslope ', uhills[h], ' CV GI - No GI ET'))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-100,100,1), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('ET_MinusGI_sd_hm.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(ET_bm$Date), y = sdET_MinGI_hm[h,], xlab = 'Year', ylab = 'Std. Dev. ET (1)', type = 'l', axes=FALSE, cex.lab = 1.5,
       main = paste0('Hillslope ', uhills[h], ' SD GI - No GI ET'))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,100,.01), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)


#Plots - Basin Downslope ----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation_Down")
# Streamflow----
png(paste0('Q_MedGI_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bd$Date), y = Q_bd[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = Q_bd[,-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(Q_bd$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('Q_MedGI_2007_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bd$Date[which((Q_bd$Date >= '2007-01-01') & (Q_bd$Date <= '2007-12-31'))]), y = Q_bd[which((Q_bd$Date >= '2007-01-01') & (Q_bd$Date <= '2007-12-31')),-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = Q_bd[which((Q_bd$Date >= '2007-01-01') & (Q_bd$Date <= '2007-12-31')),-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(Q_bd$Date[which((Q_bd$Date >= '2007-01-01') & (Q_bd$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdQ_bd = apply(X = Q_bd[,-1], MARGIN = 1, FUN = sd)
meanQ_bd = apply(X = Q_bd[,-1], MARGIN = 1, FUN = mean)
CVQ_bd = sdQ_bd/meanQ_bd

png(paste0('Q_CVQ_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bd$Date), y = CVQ_bd, xlab = 'Year', ylab = 'CV Streamflow (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('Q_sdQ_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bd$Date), y = sdQ_bd, xlab = 'Year', ylab = 'Std. Dev. Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
dev.off()

#Compare to without GI
png(paste0('Q_GI+NoGI_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bd$Date), y = Q_bd[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
par(new=TRUE)
plot(x = as.Date(Q_bd$Date), y = SimB$streamflow, col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()

#Check that all GI streamflows are <= without GI
for (i in 1:100){
  if (!all(Q_bd[,1+i] < SimB$streamflow)){
    print(paste('i = ', i, length(which(Q_bd[,1+i] >= SimB$streamflow))))
  }
}

#Plots that compare reduction in streamflow
png(paste0('Q_MinusGI_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bd$Date), y = Q_bd[,-1] - SimB$streamflow, col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,-50,-0.01), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = Q_bd[,-1] - SimB$streamflow, MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(Q_bd$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('Q_MinusGI_2007_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bd$Date[which((Q_bd$Date >= '2007-01-01') & (Q_bd$Date <= '2007-12-31'))]), y = Q_bd[which((Q_bd$Date >= '2007-01-01') & (Q_bd$Date <= '2007-12-31')),-1] - SimB$streamflow[which((Q_bd$Date >= '2007-01-01') & (Q_bd$Date <= '2007-12-31'))], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,-50,-0.01), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = Q_bd[which((Q_bd$Date >= '2007-01-01') & (Q_bd$Date <= '2007-12-31')),-1] - SimB$streamflow[which((Q_bd$Date >= '2007-01-01') & (Q_bd$Date <= '2007-12-31'))], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(Q_bd$Date[which((Q_bd$Date >= '2007-01-01') & (Q_bd$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdQ_MinGI_bd = apply(X = Q_bd[,-1] - SimB$streamflow, MARGIN = 1, FUN = sd)
meanQ_MinGI_bd = apply(X = Q_bd[,-1] - SimB$streamflow, MARGIN = 1, FUN = mean)
CVQ_MinGI_bd = sdQ_MinGI_bd/meanQ_MinGI_bd

png(paste0('Q_MinusGI_CVQ_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bd$Date), y = CVQ_MinGI_bd, xlab = 'Year', ylab = 'CV Streamflow (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(-1,1,0.1), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('Q_MinusGI_sdQ_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bd$Date), y = sdQ_MinGI_bd, xlab = 'Year', ylab = 'Std. Dev. Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
dev.off()


#  Sum flow greater than 95th%-ile historical flows----
Sum95_bd = apply(X = Q_bd[,-1][which(Q_bd[,-1] >= q95_cal),], MARGIN = 2, FUN = sum, na.rm=TRUE)
#  Sum flow less than 5th%-ile historical flows----
Sum05_bd = apply(X = Q_bd[,-1][which(Q_bd[,-1] <= q05_cal),], MARGIN = 2, FUN = sum, na.rm=TRUE)
#  Sum reduction in flow for all historical flows----
SumAll_bd = apply(X = Q_bd[,-1] - SimB$streamflow, MARGIN = 2, FUN = sum)
# Sat Def----
png(paste0('SatDef_MedGI_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_bd$Date), y = SatDef_bd[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Sat. Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,5000,20), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = SatDef_bd[,-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(SatDef_bd$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('SatDef_MedGI_2007_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_bd$Date[which((SatDef_bd$Date >= '2007-01-01') & (SatDef_bd$Date <= '2007-12-31'))]), y = SatDef_bd[which((SatDef_bd$Date >= '2007-01-01') & (SatDef_bd$Date <= '2007-12-31')),-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Sat. Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,5000,20), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = SatDef_bd[which((SatDef_bd$Date >= '2007-01-01') & (SatDef_bd$Date <= '2007-12-31')),-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(SatDef_bd$Date[which((SatDef_bd$Date >= '2007-01-01') & (SatDef_bd$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdSatDef_bd = apply(X = SatDef_bd[,-1], MARGIN = 1, FUN = sd)
meanSatDef_bd = apply(X = SatDef_bd[,-1], MARGIN = 1, FUN = mean)
CVSatDef_bd = sdSatDef_bd/meanSatDef_bd

png(paste0('SatDef_CV_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_bd$Date), y = CVSatDef_bd, xlab = 'Year', ylab = 'CV Sat. Def. (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.0001), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('SatDef_sd_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_bd$Date), y = sdSatDef_bd, xlab = 'Year', ylab = 'Std. Dev. Sat. Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,10,0.1), labels = TRUE, cex.axis = 1.5)
dev.off()

#Compare to without GI
png(paste0('SatDef_GI+NoGI_bd.png'), res = 300, height = 5, width=5, units = 'in')
plot(x = as.Date(SatDef_bd$Date), y = c(max(SatDef_bd[,-1]), min(SatDef_bd[,-1]), max(SimB$sat_def), min(SimB$sat_def), rep(median(SimB$sat_def), nrow(SatDef_bd)-4)), col = 'white', xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i")
par(new=TRUE)
matplot(x = as.Date(SatDef_bd$Date), y = SatDef_bd[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Sat. Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5, xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,5000,20), labels = TRUE, cex.axis = 1.5)
par(new=TRUE)
plot(x = as.Date(SatDef_bd$Date), y = SimB$sat_def, col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()

#Check that all GI sat def are >= without GI
for (i in 1:100){
  if (!all(SatDef_bd[,1+i] > SimB$sat_def)){
    print(paste('i = ', i, length(which(SatDef_bd[,1+i] <= SimB$sat_def))))
  }
}

#Plots that compare reduction in streamflow
png(paste0('SatDef_MinusGI_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_bd$Date), y = SatDef_bd[,-1] - SimB$sat_def, col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Sat. Deficit: GI - No GI (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,500,5), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = SatDef_bd[,-1] - SimB$sat_def, MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(SatDef_bd$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('SatDef_MinusGI_2007_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_bd$Date[which((SatDef_bd$Date >= '2007-01-01') & (SatDef_bd$Date <= '2007-12-31'))]), y = SatDef_bd[which((SatDef_bd$Date >= '2007-01-01') & (SatDef_bd$Date <= '2007-12-31')),-1] - SimB$sat_def[which((SatDef_bd$Date >= '2007-01-01') & (SatDef_bd$Date <= '2007-12-31'))], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Sat. Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,500,5), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = SatDef_bd[which((SatDef_bd$Date >= '2007-01-01') & (SatDef_bd$Date <= '2007-12-31')),-1] - SimB$sat_def[which((SatDef_bd$Date >= '2007-01-01') & (SatDef_bd$Date <= '2007-12-31'))], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(SatDef_bd$Date[which((SatDef_bd$Date >= '2007-01-01') & (SatDef_bd$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdSatDef_MinGI_bd = apply(X = SatDef_bd[,-1] - SimB$sat_def, MARGIN = 1, FUN = sd)
meanSatDef_MinGI_bd = apply(X = SatDef_bd[,-1] - SimB$sat_def, MARGIN = 1, FUN = mean)
CVSatDef_MinGI_bd = sdSatDef_MinGI_bd/meanSatDef_MinGI_bd

png(paste0('SatDef_MinusGI_CV_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_bd$Date), y = CVSatDef_MinGI_bd, xlab = 'Year', ylab = 'CV Sat. Def. (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(-1,1,0.01), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('SatDef_MinusGI_sd_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(SatDef_bd$Date), y = sdSatDef_MinGI_bd, xlab = 'Year', ylab = 'Std. Dev. Sat. Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,5,0.1), labels = TRUE, cex.axis = 1.5)
dev.off()


# Detention Storage - Same----
sdDetStore_bd = apply(X = DetStore_bd[,-1], MARGIN = 1, FUN = sd)
meanDetStore_bd = apply(X = DetStore_bd[,-1], MARGIN = 1, FUN = mean)
CVDetStore_bd = sdDetStore_bd/meanDetStore_bd

# ET----
png(paste0('ET_MedGI_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bd$Date), y = ET_bd[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = ET_bd[,-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(ET_bd$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('ET_MedGI_2007_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bd$Date[which((ET_bd$Date >= '2007-01-01') & (ET_bd$Date <= '2007-12-31'))]), y = ET_bd[which((ET_bd$Date >= '2007-01-01') & (ET_bd$Date <= '2007-12-31')),-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = ET_bd[which((ET_bd$Date >= '2007-01-01') & (ET_bd$Date <= '2007-12-31')),-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(ET_bd$Date[which((ET_bd$Date >= '2007-01-01') & (ET_bd$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdET_bd = apply(X = ET_bd[,-1], MARGIN = 1, FUN = sd)
meanET_bd = apply(X = ET_bd[,-1], MARGIN = 1, FUN = mean)
CVET_bd = sdET_bd/meanET_bd

png(paste0('ET_CV_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bd$Date), y = CVET_bd, xlab = 'Year', ylab = 'CV ET (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('ET_sd_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bd$Date), y = sdET_bd, xlab = 'Year', ylab = 'Std. Dev. ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
dev.off()

#Compare to without GI
png(paste0('ET_GI+NoGI_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bd$Date), y = ET_bd[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
par(new=TRUE)
plot(x = as.Date(ET_bd$Date), y = SimB$ET, col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()

#Check that all GI streamflows are <= without GI
for (i in 1:100){
  if (!all(ET_bd[,1+i] > SimB$ET)){
    print(paste('i = ', i, length(which(ET_bd[,1+i] <= SimB$ET))))
  }
}
#Peak ET > with GI, but off peak can be less

#Plots that compare reduction in streamflow
png(paste0('ET_MinusGI_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bd$Date), y = ET_bd[,-1] - SimB$ET, col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,-50,-0.1), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = ET_bd[,-1] - SimB$ET, MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(ET_bd$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

png(paste0('ET_MinusGI_2007_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bd$Date[which((ET_bd$Date >= '2007-01-01') & (ET_bd$Date <= '2007-12-31'))]), y = ET_bd[which((ET_bd$Date >= '2007-01-01') & (ET_bd$Date <= '2007-12-31')),-1] - SimB$ET[which((ET_bd$Date >= '2007-01-01') & (ET_bd$Date <= '2007-12-31'))], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,-50,-0.01), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = ET_bd[which((ET_bd$Date >= '2007-01-01') & (ET_bd$Date <= '2007-12-31')),-1] - SimB$ET[which((ET_bd$Date >= '2007-01-01') & (ET_bd$Date <= '2007-12-31'))], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(ET_bd$Date[which((ET_bd$Date >= '2007-01-01') & (ET_bd$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()
rm(quants, qlty)

#Make a plot of the sd in streamflow for each day
sdET_MinGI_bd = apply(X = ET_bd[,-1] - SimB$ET, MARGIN = 1, FUN = sd)
meanET_MinGI_bd = apply(X = ET_bd[,-1] - SimB$ET, MARGIN = 1, FUN = mean)
CVET_MinGI_bd = sdET_MinGI_bd/meanET_MinGI_bd

png(paste0('ET_MinusGI_CV_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bd$Date), y = CVET_MinGI_bd, xlab = 'Year', ylab = 'CV ET (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(-2000,2000,10), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('ET_MinusGI_sd_bd.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(ET_bd$Date), y = sdET_MinGI_bd, xlab = 'Year', ylab = 'Std. Dev. ET (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,10,0.01), labels = TRUE, cex.axis = 1.5)
dev.off()


#Plots - Hillslope Downslope----
# Streamflow----
png(paste0('Q_MedGI_hd.png'), res = 300, height = 10, width=10, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(Q_bd$Date), y = t(Q_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,.1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = Q_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(Q_bd$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h, quants, qlty)

png(paste0('Q_MedGI_2007_hd.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(Q_bd$Date), y = t(Q_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,.1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = Q_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(Q_bd$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h, quants, qlty)

#Make a plot of the sd in streamflow for each day
sdQ_hd = meanQ_hd = CVQ_hd = matrix(0, nrow = nrow(Area.Hills), ncol = length(Q_bd$Date))
for (h in 1:nrow(sdQ_hd)){
  sdQ_hd[h,] = apply(X = Q_hd[which(Q_hd$HillID == h),-c(1,2)], MARGIN = 2, FUN = sd)
  meanQ_hd[h,] = apply(X = Q_hd[which(Q_hd$HillID == h),-c(1,2)], MARGIN = 2, FUN = mean)
  CVQ_hd[h,] = sdQ_hd[h,]/meanQ_hd[h,]
}
rm(h)

png(paste0('Q_CVQ_hd.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(Q_bd$Date), y = CVQ_hd[h,],
       main = paste0('Hillslope ', uhills[h], ' CV Streamflow'), type = 'l', xlab = 'Year', ylab = 'CV Streamflow (1)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,1,.01), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('Q_CVQ_hd_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  plot(x = as.Date(Q_bd$Date), y = CVQ_hd[h,],
       main = paste0('Hillslope ', uhills[h], ' CV Streamflow'), type = 'l', xlab = 'Year', ylab = 'CV Streamflow (1)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,1,.01), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('Q_sdQ_hd.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(Q_bd$Date), y = sdQ_hd[h,],
       main = paste0('Hillslope ', uhills[h], ' SD Streamflow'), type = 'l', xlab = 'Year', ylab = 'Std. Dev. Streamflow (cfs)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('Q_sdQ_hd_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  plot(x = as.Date(Q_bd$Date), y = sdQ_hd[h,],
       main = paste0('Hillslope ', uhills[h], ' SD Streamflow'), type = 'l', xlab = 'Year', ylab = 'Std. Dev. Streamflow (cfs)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)


#Compare to without GI
png(paste0('Q_GI+NoGI_hd.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(Q_bd$Date), y = t(Q_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,.1), labels = TRUE, cex.axis = 1.5)
  par(new=TRUE)
  plot(x = as.Date(Q_bd$Date), y = SimH$streamflow[SimH$hillID == h], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, 
       lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Compare to without GI
png(paste0('Q_GI+NoGI_hd_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(Q_bd$Date), y = t(Q_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,.1), labels = TRUE, cex.axis = 1.5)
  par(new=TRUE)
  plot(x = as.Date(Q_bd$Date), y = SimH$streamflow[SimH$hillID == h], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, 
       lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Plots that compare reduction in streamflow
png(paste0('Q_MinusGI_hd.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(Q_bd$Date), y = t(Q_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(1,-50,-.01), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(Q_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(Q_bd$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('Q_MinusGI_hd_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(Q_bd$Date), y = t(Q_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,-50,-.1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(Q_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(Q_bd$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('Q_MinusGI_2007_hd.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(Q_bd$Date), y = t(Q_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(1,-50,-.01), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(Q_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(Q_bd$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('Q_MinusGI_2007_hd_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(Q_bd$Date), y = t(Q_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(1,-50,-.01), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(Q_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$streamflow[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(Q_bd$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

#Make a plot of the sd in streamflow for each day
sdQ_MinGI_hd = meanQ_MinGI_hd = CVQ_MinGI_hd = matrix(0, nrow = nrow(Area.Hills), ncol = length(Q_bd$Date))
for (h in 1:nrow(sdQ_MinGI_hd)){
  sdQ_MinGI_hd[h,] = apply(X = t(Q_hd[which(Q_hd$HillID == h),-c(1,2)]) - SimH$streamflow[SimH$hillID == h], MARGIN = 1, FUN = sd)
  meanQ_MinGI_hd[h,] = apply(X = t(Q_hd[which(Q_hd$HillID == h),-c(1,2)]) - SimH$streamflow[SimH$hillID == h], MARGIN = 1, FUN = mean)
  CVQ_MinGI_hd[h,] = sdQ_MinGI_hd[h,]/meanQ_MinGI_hd[h,]
}
rm(h)

png(paste0('Q_MinusGI_CVQ_hd.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(Q_bd$Date), y = CVQ_MinGI_hd[h,], xlab = 'Year', ylab = 'CV Streamflow (1)', type = 'l', axes=FALSE, cex.lab = 1.5,
       main = paste0('Hillslope ', uhills[h], ' CV GI - No GI Streamflow'))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-1000,1000,1), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('Q_MinusGI_sdQ_hd.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(Q_bd$Date), y = sdQ_MinGI_hd[h,], xlab = 'Year', ylab = 'Std. Dev. Streamflow (1)', type = 'l', axes=FALSE, cex.lab = 1.5,
       main = paste0('Hillslope ', uhills[h], ' SD GI - No GI Streamflow'))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-1000,1000,0.001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

# Sat Def----
png(paste0('SatDef_MedGI_hd.png'), res = 300, height = 10, width=10, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(SatDef_bd$Date), y = t(SatDef_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,5000,100), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = SatDef_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(SatDef_bd$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h, quants, qlty)

png(paste0('SatDef_MedGI_2007_hd.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(SatDef_bd$Date), y = t(SatDef_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,5000,100), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = SatDef_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(SatDef_bd$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h, quants, qlty)

#Make a plot of the sd in streamflow for each day
sdSatDef_hd = meanSatDef_hd = CVSatDef_hd = matrix(0, nrow = nrow(Area.Hills), ncol = length(SatDef_bd$Date))
for (h in 1:nrow(sdSatDef_hd)){
  sdSatDef_hd[h,] = apply(X = SatDef_hd[which(SatDef_hd$HillID == h),-c(1,2)], MARGIN = 2, FUN = sd)
  meanSatDef_hd[h,] = apply(X = SatDef_hd[which(SatDef_hd$HillID == h),-c(1,2)], MARGIN = 2, FUN = mean)
  CVSatDef_hd[h,] = sdSatDef_hd[h,]/meanSatDef_hd[h,]
}
rm(h)

png(paste0('SatDef_CV_hd.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(SatDef_bd$Date), y = CVSatDef_hd[h,],
       main = paste0('Hillslope ', uhills[h], ' CV Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'CV Sat. Def. (1)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,10,.001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('SatDef_CV_hd_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  plot(x = as.Date(SatDef_bd$Date), y = CVSatDef_hd[h,],
       main = paste0('Hillslope ', uhills[h], ' CV Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'CV Sat. Def. (1)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,1,.001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('SatDef_sd_hd.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(SatDef_bd$Date), y = sdSatDef_hd[h,],
       main = paste0('Hillslope ', uhills[h], ' SD Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Std. Dev. Sat. Def. (mm)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,100,1), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('SatDef_sd_hd_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  plot(x = as.Date(SatDef_bd$Date), y = sdSatDef_hd[h,],
       main = paste0('Hillslope ', uhills[h], ' SD Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Std. Dev. Sat. Def. (mm)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,1000,1), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)


#Compare to without GI
png(paste0('SatDef_GI+NoGI_hd.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(SatDef_bd$Date), y = c(max(t(SatDef_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)])), min(t(SatDef_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)])), max(SimH$sat_def[SimH$hillID == h]), min(SimH$sat_def[SimH$hillID == h]), rep(median(SimH$sat_def[SimH$hillID == h]), nrow(SatDef_bd)-4)), col = 'white', xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i")
  par(new=TRUE)
  matplot(x = as.Date(SatDef_bd$Date), y = t(SatDef_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,5000,100), labels = TRUE, cex.axis = 1.5)
  par(new=TRUE)
  plot(x = as.Date(SatDef_bd$Date), y = SimH$sat_def[SimH$hillID == h], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, 
       lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Compare to without GI
png(paste0('SatDef_GI+NoGI_hd_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(SatDef_bd$Date), y = t(SatDef_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,5000,100), labels = TRUE, cex.axis = 1.5)
  par(new=TRUE)
  plot(x = as.Date(SatDef_bd$Date), y = SimH$sat_def[SimH$hillID == h], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, 
       lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Plots that compare reduction in streamflow
png(paste0('SatDef_MinusGI_hd.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(SatDef_bd$Date), y = t(SatDef_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-500,500,10), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(SatDef_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(SatDef_bd$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('SatDef_MinusGI_hd_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(SatDef_bd$Date), y = t(SatDef_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-500,500,0.001), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(SatDef_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(SatDef_bd$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('SatDef_MinusGI_2007_hd.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(SatDef_bd$Date), y = t(SatDef_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-500,500,10), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(SatDef_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(SatDef_bd$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('SatDef_MinusGI_2007_hd_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(SatDef_bd$Date), y = t(SatDef_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI Sat. Def.'), type = 'l', xlab = 'Year', ylab = 'Sat. Def. (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-500,500,.001), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(SatDef_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$sat_def[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(SatDef_bd$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

#Make a plot of the sd in streamflow for each day
sdSatDef_MinGI_hd = meanSatDef_MinGI_hd = CVSatDef_MinGI_hd = matrix(0, nrow = nrow(Area.Hills), ncol = length(SatDef_bd$Date))
for (h in 1:nrow(sdSatDef_MinGI_hd)){
  sdSatDef_MinGI_hd[h,] = apply(X = t(SatDef_hd[which(SatDef_hd$HillID == h),-c(1,2)]) - SimH$sat_def[SimH$hillID == h], MARGIN = 1, FUN = sd)
  meanSatDef_MinGI_hd[h,] = apply(X = t(SatDef_hd[which(SatDef_hd$HillID == h),-c(1,2)]) - SimH$sat_def[SimH$hillID == h], MARGIN = 1, FUN = mean)
  CVSatDef_MinGI_hd[h,] = sdSatDef_MinGI_hd[h,]/meanSatDef_MinGI_hd[h,]
}
rm(h)

png(paste0('SatDef_MinusGI_CV_hd.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(SatDef_bd$Date), y = CVSatDef_MinGI_hd[h,], xlab = 'Year', ylab = 'CV Sat. Def. (1)', type = 'l', axes=FALSE, cex.lab = 1.5,
       main = paste0('Hillslope ', uhills[h], ' CV GI - No GI Sat. Def.'))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-1000,1000,0.01), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('SatDef_MinusGI_sd_hd.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(SatDef_bd$Date), y = sdSatDef_MinGI_hd[h,], xlab = 'Year', ylab = 'Std. Dev. Sat. Def. (1)', type = 'l', axes=FALSE, cex.lab = 1.5,
       main = paste0('Hillslope ', uhills[h], ' SD GI - No GI Sat. Def.'))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-1000,1000,1), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

# ET----
png(paste0('ET_MedGI_hd.png'), res = 300, height = 10, width=10, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(ET_bd$Date), y = t(ET_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = ET_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(ET_bd$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h, quants, qlty)

png(paste0('ET_MedGI_2007_hd.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(ET_bd$Date), y = t(ET_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = ET_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(ET_bd$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h, quants, qlty)

#Make a plot of the sd in streamflow for each day
sdET_hd = meanET_hd = CVET_hd = matrix(0, nrow = nrow(Area.Hills), ncol = length(ET_bd$Date))
for (h in 1:nrow(sdET_hd)){
  sdET_hd[h,] = apply(X = ET_hd[which(ET_hd$HillID == h),-c(1,2)], MARGIN = 2, FUN = sd)
  meanET_hd[h,] = apply(X = ET_hd[which(ET_hd$HillID == h),-c(1,2)], MARGIN = 2, FUN = mean)
  CVET_hd[h,] = sdET_hd[h,]/meanET_hd[h,]
}
rm(h)

png(paste0('ET_CV_hd.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(ET_bd$Date), y = CVET_hd[h,],
       main = paste0('Hillslope ', uhills[h], ' CV ET'), type = 'l', xlab = 'Year', ylab = 'CV ET (1)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,10,.01), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('ET_CV_hd_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  plot(x = as.Date(ET_bd$Date), y = CVET_hd[h,],
       main = paste0('Hillslope ', uhills[h], ' CV ET'), type = 'l', xlab = 'Year', ylab = 'CV ET (1)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,1,.0001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('ET_sd_hd.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(ET_bd$Date), y = sdET_hd[h,],
       main = paste0('Hillslope ', uhills[h], ' SD ET'), type = 'l', xlab = 'Year', ylab = 'Std. Dev. ET (mm)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,100,0.01), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('ET_sd_hd_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  plot(x = as.Date(ET_bd$Date), y = sdET_hd[h,],
       main = paste0('Hillslope ', uhills[h], ' SD ET'), type = 'l', xlab = 'Year', ylab = 'Std. Dev. ET (mm)',
       col = 'black', axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,1,.0001), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)


#Compare to without GI
png(paste0('ET_GI+NoGI_hd.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(ET_bd$Date), y = t(ET_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,10,1), labels = TRUE, cex.axis = 1.5)
  par(new=TRUE)
  plot(x = as.Date(ET_bd$Date), y = SimH$ET[SimH$hillID == h], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, 
       lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Compare to without GI
png(paste0('ET_GI+NoGI_hd_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(ET_bd$Date), y = t(ET_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,1), labels = TRUE, cex.axis = 1.5)
  par(new=TRUE)
  plot(x = as.Date(ET_bd$Date), y = SimH$ET[SimH$hillID == h], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, 
       lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Plots that compare reduction in streamflow
png(paste0('ET_MinusGI_hd.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(ET_bd$Date), y = t(ET_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-50,50,0.2), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(ET_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(ET_bd$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('ET_MinusGI_hd_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(ET_bd$Date), y = t(ET_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-50,50,0.1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(ET_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(ET_bd$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('ET_MinusGI_2007_hd.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  matplot(x = as.Date(ET_bd$Date), y = t(ET_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-50,50,1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(ET_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(ET_bd$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

png(paste0('ET_MinusGI_2007_hd_UnalteredSlopes.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8)))
par(mar = c(3,3,3,0.5))
for (h in 1:8){
  matplot(x = as.Date(ET_bd$Date), y = t(ET_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h],
          main = paste0('Hillslope ', uhills[h], ' GI - No GI ET'), type = 'l', xlab = 'Year', ylab = 'ET (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5, xlim = c(as.Date('2007-01-01'), as.Date('2007-12-31')))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-50,50,.1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = t(ET_hd[seq(1+(uhills[h]-1)*numReps, numReps+(uhills[h]-1)*numReps, 1), -c(1,2)]) - SimH$ET[SimH$hillID == h], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(ET_bd$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  lines(x = c(as.Date('2004-01-01'), as.Date('2014-01-01')), y = c(0,0), col = 'blue', type = 'l')
}
dev.off()
rm(h, quants, qlty)

#Make a plot of the sd in streamflow for each day
sdET_MinGI_hd = meanET_MinGI_hd = CVET_MinGI_hd = matrix(0, nrow = nrow(Area.Hills), ncol = length(ET_bd$Date))
for (h in 1:nrow(sdET_MinGI_hd)){
  sdET_MinGI_hd[h,] = apply(X = t(ET_hd[which(ET_hd$HillID == h),-c(1,2)]) - SimH$ET[SimH$hillID == h], MARGIN = 1, FUN = sd)
  meanET_MinGI_hd[h,] = apply(X = t(ET_hd[which(ET_hd$HillID == h),-c(1,2)]) - SimH$ET[SimH$hillID == h], MARGIN = 1, FUN = mean)
  CVET_MinGI_hd[h,] = sdET_MinGI_hd[h,]/meanET_MinGI_hd[h,]
}
rm(h)

png(paste0('ET_MinusGI_CV_hd.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(ET_bd$Date), y = CVET_MinGI_hd[h,], xlab = 'Year', ylab = 'CV ET (1)', type = 'l', axes=FALSE, cex.lab = 1.5,
       main = paste0('Hillslope ', uhills[h], ' CV GI - No GI ET'))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(-100,100,1), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)

png(paste0('ET_MinusGI_sd_hd.png'), res = 300, height = 5, width=5, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6)))
par(mar = c(3,3,3,0.5))
for (h in 9:length(uhills)){
  plot(x = as.Date(ET_bd$Date), y = sdET_MinGI_hd[h,], xlab = 'Year', ylab = 'Std. Dev. ET (1)', type = 'l', axes=FALSE, cex.lab = 1.5,
       main = paste0('Hillslope ', uhills[h], ' SD GI - No GI ET'))
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,100,.01), labels = TRUE, cex.axis = 1.5)
}
dev.off()
rm(h)


#Compare streamflow reduction for up, mid, downslope, and sum vs. using all at once----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation")

Area_d = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation_Down\\lulcFrac30m_GI.csv", stringsAsFactors = FALSE)
Area_m = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation_Mid\\lulcFrac30m_GI.csv", stringsAsFactors = FALSE)
Area_u = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation_Up\\lulcFrac30m_GI.csv", stringsAsFactors = FALSE)

Area_bd = sum(Area_d$lulc13)/1000000
Area_bm = sum(Area_m$lulc13)/1000000
Area_bu = sum(Area_u$lulc13)/1000000

#Plots that compare reduction in streamflow, normalized by area converted to GI
png(paste0('Q_MinusGI_all_area.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bd$Date), y = (Q_bd[,-1] - SimB$streamflow)/Area_bd, col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)/GI Area (km2)', type = 'l', axes=FALSE, cex.lab = 1.5, xlim = c(as.Date("2004-01-01"), as.Date("2014-06-30")), ylim = c(-5, 1), xaxs = 'i', yaxs = 'i')
par(new = TRUE)
matplot(x = as.Date(Q_bm$Date), y = (Q_bm[,-1] - SimB$streamflow)/Area_bm, col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = '', type = 'l', axes=FALSE, cex.lab = 1.5, xlim=as.Date(par('usr')[c(1,2)], origin = as.Date('1970-01-01')), ylim=par('usr')[c(3,4)], xaxs = 'i', yaxs = 'i')
par(new = TRUE)
matplot(x = as.Date(Q_bu$Date), y = (Q_bu[,-1] - SimB$streamflow)/Area_bu, col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = '', type = 'l', axes=FALSE, cex.lab = 1.5, xlim=as.Date(par('usr')[c(1,2)], origin = as.Date('1970-01-01')), ylim=par('usr')[c(3,4)], xaxs = 'i', yaxs = 'i')
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,-50,-0.01), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants_d = apply(X = (Q_bd[,-1] - SimB$streamflow)/Area_bd, MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
quants_m = apply(X = (Q_bm[,-1] - SimB$streamflow)/Area_bm, MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
quants_u = apply(X = (Q_bu[,-1] - SimB$streamflow)/Area_bu, MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(Q_bd$Date), y = quants_d[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=as.Date(par('usr')[c(1,2)], origin = as.Date('1970-01-01')), ylim=par('usr')[c(3,4)], xaxs = 'i', yaxs = 'i')
par(new=TRUE)
plot(x = as.Date(Q_bm$Date), y = quants_m[3,], col = 'green', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=as.Date(par('usr')[c(1,2)], origin = as.Date('1970-01-01')), ylim=par('usr')[c(3,4)], xaxs = 'i', yaxs = 'i')
par(new=TRUE)
plot(x = as.Date(Q_bu$Date), y = quants_u[3,], col = 'blue', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=as.Date(par('usr')[c(1,2)], origin = as.Date('1970-01-01')), ylim=par('usr')[c(3,4)], xaxs = 'i', yaxs = 'i')
dev.off()
rm(quants_d, quants_m, quants_u, qlty)

png(paste0('Q_MinusGI_all_sum.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Q_bd$Date), y = (Q_bd[,-1] - SimB$streamflow) + (Q_bm[,-1] - SimB$streamflow) + (Q_bu[,-1] - SimB$streamflow), col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5, xlim = c(as.Date("2004-01-01"), as.Date("2014-06-30")), ylim = c(-0.3, 0.1), xaxs = 'i', yaxs = 'i')
par(new = TRUE)
matplot(x = as.Date(Q_b$Date), y = (Q_b[,-1] - SimB$streamflow), col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = '', type = 'l', axes=FALSE, cex.lab = 1.5, xlim=as.Date(par('usr')[c(1,2)], origin = as.Date('1970-01-01')), ylim=par('usr')[c(3,4)], xaxs = 'i', yaxs = 'i')
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,-50,-0.01), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants_d = apply(X = (Q_bd[,-1] - SimB$streamflow) + (Q_bm[,-1] - SimB$streamflow) + (Q_bu[,-1] - SimB$streamflow), MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
quants = apply(X = (Q_b[,-1] - SimB$streamflow), MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(Q_bd$Date), y = quants_d[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=as.Date(par('usr')[c(1,2)], origin = as.Date('1970-01-01')), ylim=par('usr')[c(3,4)], xaxs = 'i', yaxs = 'i')
par(new=TRUE)
plot(x = as.Date(Q_b$Date), y = quants[3,], col = 'green', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=as.Date(par('usr')[c(1,2)], origin = as.Date('1970-01-01')), ylim=par('usr')[c(3,4)], xaxs = 'i', yaxs = 'i')
dev.off()
rm(quants_d, quants, qlty)

##Maps----
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
#Land Cover in Suburban Hillslopes----
# length(unique(world$patchID[which((world$hillID %in% c(11,12)) & (world$patchLandID == 1))]))/length(unique(world$patchID[which(world$hillID %in% c(11,12))]))
# [1] 0.1432665
# > length(unique(world$patchID[which((world$hillID %in% c(10,9)) & (world$patchLandID == 1))]))/length(unique(world$patchID[which(world$hillID %in% c(10,9))]))
# [1] 0.1317114
# > length(unique(world$patchID[which((world$hillID %in% c(13,14)) & (world$patchLandID == 1))]))/length(unique(world$patchID[which(world$hillID %in% c(13,14))]))
# [1] 0.1424936
# > length(unique(world$patchID[which(world$patchLandID == 1)]))/length(unique(world$patchID))
# [1] 0.08989547
# > length(unique(world$patchID[which(world$patchLandID == 2)]))/length(unique(world$patchID))
# [1] 0.7437863
# > length(unique(world$patchID[which((world$hillID %in% c(13,14)) & (world$patchLandID == 2))]))/length(unique(world$patchID[which(world$hillID %in% c(13,14))]))
# [1] 0.5839695
# > length(unique(world$patchID[which((world$hillID %in% c(10,9)) & (world$patchLandID == 2))]))/length(unique(world$patchID[which(world$hillID %in% c(10,9))]))
# [1] 0.6627517
# > length(unique(world$patchID[which((world$hillID %in% c(11,12)) & (world$patchLandID == 2))]))/length(unique(world$patchID[which(world$hillID %in% c(11,12))]))
# [1] 0.5315186
# > length(unique(world$patchID[which(world$patchLandID == 3)]))/length(unique(world$patchID))
# [1] 0.01718931
# > length(unique(world$patchID[which((world$hillID %in% c(11,12)) & (world$patchLandID == 3))]))/length(unique(world$patchID[which(world$hillID %in% c(11,12))]))
# [1] 0.0243553
# > length(unique(world$patchID[which((world$hillID %in% c(10,9)) & (world$patchLandID == 3))]))/length(unique(world$patchID[which(world$hillID %in% c(10,9))]))
# [1] 0.02432886
# > length(unique(world$patchID[which((world$hillID %in% c(13,14)) & (world$patchLandID == 3))]))/length(unique(world$patchID[which(world$hillID %in% c(13,14))]))
# [1] 0.03180662
# > length(unique(world$patchID[which(world$patchLandID == 4)]))/length(unique(world$patchID))
# [1] 0.1491289
# > length(unique(world$patchID[which((world$hillID %in% c(11,12)) & (world$patchLandID == 4))]))/length(unique(world$patchID[which(world$hillID %in% c(11,12))]))
# [1] 0.3008596
# > length(unique(world$patchID[which((world$hillID %in% c(10,9)) & (world$patchLandID == 4))]))/length(unique(world$patchID[which(world$hillID %in% c(10,9))]))
# [1] 0.1812081
# > length(unique(world$patchID[which((world$hillID %in% c(13,14)) & (world$patchLandID == 4))]))/length(unique(world$patchID[which(world$hillID %in% c(13,14))]))
# [1] 0.2417303
