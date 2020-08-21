#Script to compile streamflows into a matrix and plot

library(vroom)

#Compute streamflow conversion factor----
world = read.csv('/scratch/js4yd/GI_RandomSeedEval/RHESSys_Baisman30m_g74/worldfiles/worldfile.csv', stringsAsFactors = FALSE)
res = 30
#Taking the unique patch IDs because strata can exist in more than one patch.
Area.basin = length(unique(world$patchID))*res^2
rm(world, res)
#Multiplier conversion for basin streamflow (mm/d)*conversion_b -> cfs
conversion_b = Area.basin/1000/(.3048^3)/24/3600

#Aggregate into one file to matplot
setwd('/scratch/js4yd/GI_RandomSeedEval/RHESSysRuns/Run1/RHESSys_Baisman30m_g74/output')
#Read in simulated basin streamflow
Q1 = vroom(paste0(getwd(), '/Run1_basin.daily'), delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
#Make a new Date column
Q1$Date = as.Date(paste0(Q1$year, '-', Q1$month, '-', Q1$day))
#Retain only streamflow and Date columns for space
Q1 = as.data.frame(Q1[,c('Date','streamflow')])
#Trim off spin-up years
Q1 = Q1[which(as.Date(Q1$Date) >= as.Date('2004-10-01')),]
Qmat = matrix(NA, nrow = nrow(Q1), ncol = 101)
for (i in 1:100){
  setwd(paste0('/scratch/js4yd/GI_RandomSeedEval/RHESSysRuns/Run', i, '/RHESSys_Baisman30m_g74/output'))
  #Read in simulated basin streamflow
  Q = vroom(paste0(getwd(), '/Run', i, '_basin.daily'), delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
  
  #Make a new Date column
  Q$Date = as.Date(paste0(Q$year, '-', Q$month, '-', Q$day))
  
  #Retain only streamflow and Date columns for space
  Q = as.data.frame(Q[,c('Date','streamflow')])
  
  #Trim off spin-up years
  Q = Q[which(as.Date(Q$Date) >= as.Date('2004-10-01')),]
  
  #Convert simulated streamflow to cfs units
  Q$streamflow = round(Q$streamflow*conversion_b, 6)
  
  #Write streamflow to file for loading in Python likelihood function
  options(scipen = 999)
  write.table(Q, file = paste0(getwd(), '/Q.txt'), col.names = TRUE, row.names = FALSE, sep = '\t')
  options(scipen = 0)
  
  #Add to Qmat
  Qmat[,i+1] = Q$streamflow
}

#Convert to dataframe
Qmat = as.data.frame(Qmat)
colnames(Qmat) = c('Date', paste0('Run', seq(1,100,1)))

#Add dates
Qmat$Date = as.Date(Q1$Date)

#Save files
setwd('/scratch/js4yd/GI_RandomSeedEval')
write.csv(Qmat, 'FlowGI_c.csv', row.names=FALSE)

#plots
png(paste0('GIstreamflowRepsBasin_Med.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Qmat$Date), y = Qmat[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = Qmat[,-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(Qmat$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()

png(paste0('GIstreamflowRepsBasin_2007_Med.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Qmat$Date[which((Qmat$Date >= '2007-01-01') & (Qmat$Date <= '2007-12-31'))]), y = Qmat[which((Qmat$Date >= '2007-01-01') & (Qmat$Date <= '2007-12-31')),-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = Qmat[which((Qmat$Date >= '2007-01-01') & (Qmat$Date <= '2007-12-31')),-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(Qmat$Date[which((Qmat$Date >= '2007-01-01') & (Qmat$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()

#Make a plot of the sd in streamflow for each day
sdQ = apply(X = Qmat[,-1], MARGIN = 1, FUN = sd)
meanQ = apply(X = Qmat[,-1], MARGIN = 1, FUN = mean)
CVQ = sdQ/meanQ

png(paste0('GIstreamflowRepsBasin_CVQ.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Qmat$Date), y = CVQ, xlab = 'Year', ylab = 'CV Streamflow (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('GIstreamflowRepsBasin_sdQ.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Qmat$Date), y = sdQ, xlab = 'Year', ylab = 'Std. Dev. Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
dev.off()

#Compare to without GI
#Load pre-GI data
Q_NoGI = read.table('/scratch/js4yd/GI_RandomSeedEval/output_preGI/Q.txt', sep = '\t', header = TRUE, stringsAsFactors=FALSE)

png(paste0('GIstreamflowRepsBasin_Med_NoGI.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Qmat$Date), y = Qmat[,-1], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = Qmat[,-1], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(Qmat$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
par(new=TRUE)
plot(x = as.Date(Qmat$Date), y = Q_NoGI[,-1], col = 'green', xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()

#Check that all GI streamflows are <= without GI
for (i in 1:100){
  if (!all(Qmat[,1+i] < Q_NoGI$streamflow)){
    print(paste('i = ', i, length(which(Qmat[,1+i] >= Q_NoGI$streamflow))))
  }
}

#Plots that compare reduction in streamflow
png(paste0('GIstreamflowRepsBasin_MedMinGI.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Qmat$Date), y = Qmat[,-1] - Q_NoGI$streamflow, col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,-50,-0.001), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = Qmat[,-1] - Q_NoGI$streamflow, MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(Qmat$Date), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()

png(paste0('GIstreamflowRepsBasin_2007_MedMinGI.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Qmat$Date[which((Qmat$Date >= '2007-01-01') & (Qmat$Date <= '2007-12-31'))]), y = Qmat[which((Qmat$Date >= '2007-01-01') & (Qmat$Date <= '2007-12-31')),-1] - Q_NoGI$streamflow[which((Qmat$Date >= '2007-01-01') & (Qmat$Date <= '2007-12-31'))], col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(1,-50,-0.001), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = Qmat[which((Qmat$Date >= '2007-01-01') & (Qmat$Date <= '2007-12-31')),-1] - Q_NoGI$streamflow[which((Qmat$Date >= '2007-01-01') & (Qmat$Date <= '2007-12-31'))], MARGIN = 1, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(Qmat$Date[which((Qmat$Date >= '2007-01-01') & (Qmat$Date <= '2007-12-31'))]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()

#Make a plot of the sd in streamflow for each day
sdQ_MinGI = apply(X = Qmat[,-1] - Q_NoGI$streamflow, MARGIN = 1, FUN = sd)
meanQ_MinGI = apply(X = Qmat[,-1] - Q_NoGI$streamflow, MARGIN = 1, FUN = mean)
CVQ_MinGI = sdQ_MinGI/meanQ_MinGI

png(paste0('GIstreamflowRepsBasin_CVQMinGI.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Qmat$Date), y = CVQ_MinGI, xlab = 'Year', ylab = 'CV Streamflow (1)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(-1,1,0.1), labels = TRUE, cex.axis = 1.5)
dev.off()

png(paste0('GIstreamflowRepsBasin_sdQMinGI.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(Qmat$Date), y = sdQ_MinGI, xlab = 'Year', ylab = 'Std. Dev. Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,.1,0.001), labels = TRUE, cex.axis = 1.5)
dev.off()



#Maps----
#  Make the worldfile a spatial dataframe to get a map. Plot information in the worldfile on the maps----
coordinates(world) = c('patchX', 'patchY')
proj4string(world) = CRS('+init=epsg:26918')
#Change to degrees
world=spTransform(world, CRSobj = CRS('+init=epsg:4326'))

cols = rainbow(n = length(uhills))

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