#Load libraries----
library(BayesianTools)
library(parallel)
library(foreach)
library(iterators)
library(doParallel)
library(rlist)
library(EGRET)
library(vroom)
library(pracma)
library(coda)
library(bayesplot)
library(gridExtra)
library(fGarch)
library(factoextra)
library(NbClust)
library(MASS)

#Load SEP code----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\Code\\GenLikelihood_Zach")
source('GL_maineqs.R')
#Load matplot with dates on x-axis allowed
source('C:\\Users\\js4yd\\OneDrive - University of Virginia\\EnGauge\\EnGauge\\matplotDates.R')
#Load edited functions
source('C:\\Users\\js4yd\\OneDrive - University of Virginia\\RHESSys_ParameterSA\\Calibration\\marginalPlot_trueVals.R')

#Load synthetic parameter set----
SynParams = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\Bais910_Syn_AfterProcessing.csv")

#Set directories----
#dir_Ch39 = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration\\Ch39"
dir_Ch10 = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration\\Ch10"
dir_Ch10_1 = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration\\Ch10-1"
dir_Ch10_2 = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration\\Ch10-2"
dir_Ch10_3 = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration\\Ch10-3"

dir_sCh10 = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration\\SynCh10"
dir_sCh10n = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration\\SynCh10\\NewCR"
dir_sCh10_1 = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration\\SynCh10-1"
dir_sCh10_1n = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration\\SynCh10-1\\NewCR"
dir_sCh10_2 = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration\\SynCh10-2"
dir_sCh10_2n = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration\\SynCh10-2\\NewCR"
dir_sCh10_3 = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration\\SynCh10-3"
dir_sCh10_3n = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration\\SynCh10-3\\NewCR"

#39 Chains----
#setwd(dir_Ch39)
#load(file = paste0(dir_Ch39, '/OutputWorkspace.RData'), verbose = FALSE)
#rm(cl, Info, startValues, i, tic_Script)

# Plot and summarize output----
#Fixme: this requires cores to be setup to run in parallel, with all of the exported variables.
#summary(out_Parext)

#print(out_Parext_Restart$settings$runtime)
#summary(out_Parext_Restart)
#print(out_Parext_Restart_1$settings$runtime)
#summary(out_Parext_Restart_1)

#Useful to see the prior and posterior on same plot.
#PriorSample = prior$sampler(n = 50000)
#png('marginalplot_39_1-16.png', res = 300, units = 'in', width = 7, height = 7)
#marginalPlot(x = out_Parext$chain[,1:16], prior = PriorSample[,1:16])
#dev.off()
#png('marginalplot_39_17-32.png', res = 300, units = 'in', width = 7, height = 7)
#marginalPlot(x = out_Parext$chain[,17:32], prior = PriorSample[,17:32])
#dev.off()
#png('marginalplot_39_33-42.png', res = 300, units = 'in', width = 7, height = 7)
#marginalPlot(x = out_Parext$chain[,33:42], prior = PriorSample[,33:42])
#dev.off()

#Only for last 50 chain steps
#png('marginalplot_39_1-16_l50.png', res = 300, units = 'in', width = 7, height = 7)
#marginalPlot(x = out_Parext$chain[51:100,1:16], prior = PriorSample[,1:16])
#dev.off()
#png('marginalplot_39_17-32_l50.png', res = 300, units = 'in', width = 7, height = 7)
#marginalPlot(x = out_Parext$chain[51:100,17:32], prior = PriorSample[,17:32])
#dev.off()
#png('marginalplot_39_33-42_l50.png', res = 300, units = 'in', width = 7, height = 7)
#marginalPlot(x = out_Parext$chain[51:100,33:42], prior = PriorSample[,33:42])
#dev.off()

#scatterplot matrix (mcmc_pairs in bayesplot is alternative)
#Fixme: Not sure how is best to view this scatterplot matrix because there are too many parameters to view in one grid.
# use whichParameters() for converged sets, and for the most sensitive parameters
#png('corplot_39.png', res = 300, units = 'in', width = 14, height = 14)
#correlationPlot(mat = out_Parext, scaleCorText = TRUE)
#dev.off()

#png('pairs_39.png', res = 300, units = 'in', width = 10, height = 10)
#mcmc_pairs(out_Parext$chain[,1:10])
#dev.off()

#Trace plots with posterior density for all parameter values - chains should look similar, not get stuck
# png('traceplot_39_1-4.png', res = 300, units = 'in', width = 7, height = 7)
# plot(out_Parext$chain[,1:4], smooth = FALSE)
# dev.off()
# png('traceplot_39_5-8.png', res = 300, units = 'in', width = 7, height = 7)
# plot(out_Parext$chain[,5:8], smooth = FALSE)
# dev.off()
# png('traceplot_39_9-12.png', res = 300, units = 'in', width = 7, height = 7)
# plot(out_Parext$chain[,9:12], smooth = FALSE)
# dev.off()
# png('traceplot_39_13-16.png', res = 300, units = 'in', width = 7, height = 7)
# plot(out_Parext$chain[,13:16], smooth = FALSE)
# dev.off()
# png('traceplot_39_17-20.png', res = 300, units = 'in', width = 7, height = 7)
# plot(out_Parext$chain[,17:20], smooth = FALSE)
# dev.off()
# png('traceplot_39_21-24.png', res = 300, units = 'in', width = 7, height = 7)
# plot(out_Parext$chain[,21:24], smooth = FALSE)
# dev.off()
# png('traceplot_39_25-28.png', res = 300, units = 'in', width = 7, height = 7)
# plot(out_Parext$chain[,25:28], smooth = FALSE)
# dev.off()
# png('traceplot_39_29-32.png', res = 300, units = 'in', width = 7, height = 7)
# plot(out_Parext$chain[,29:32], smooth = FALSE)
# dev.off()
# png('traceplot_39_33-36.png', res = 300, units = 'in', width = 7, height = 7)
# plot(out_Parext$chain[,33:36], smooth = FALSE)
# dev.off()
# png('traceplot_39_37-40.png', res = 300, units = 'in', width = 7, height = 7)
# plot(out_Parext$chain[,37:40], smooth = FALSE)
# dev.off()
# png('traceplot_39_41-42.png', res = 300, units = 'in', width = 7, height = 7)
# plot(out_Parext$chain[,41:42], smooth = FALSE)
# dev.off()

#mcmc_trace in bayesplot as alternative traceplots
# png('traceplot_39_1-9_b.png', res = 300, units = 'in', width = 7, height = 7)
# mcmc_trace(out_Parext$chain[,1:9])
# dev.off()
# png('traceplot_39_10-18_b.png', res = 300, units = 'in', width = 7, height = 7)
# mcmc_trace(out_Parext$chain[,10:18])
# dev.off()
# png('traceplot_39_19-27_b.png', res = 300, units = 'in', width = 7, height = 7)
# mcmc_trace(out_Parext$chain[,19:27])
# dev.off()
# png('traceplot_39_28-36_b.png', res = 300, units = 'in', width = 7, height = 7)
# mcmc_trace(out_Parext$chain[,28:36])
# dev.off()
# png('traceplot_39_veg_b.png', res = 300, units = 'in', width = 7, height = 7)
# mcmc_trace(out_Parext$chain[,34:42])
# dev.off()

#Fixme: This requires cores to be setup to run in parallel
#png('gelmanplot_30.png', res = 300, units = 'in', width = 7, height = 7)
#gelmanDiagnostics(sampler = out_Parext, plot = TRUE)
#dev.off()
#png('diagnosticplot_30.png', res = 300, units = 'in', width = 7, height = 7)
#plotDiagnostic(out = out_Parext)
#dev.off()

#stopCluster(cl)

# Run convergence diagnostics----
#R-hat - Gelman-Rubin within and between chain variance for all variables - target less than 1.05 for all
#gelman.diag(out_Parext$chain[,1:42])
#Autocorrelation of chain for each parameter should drop quickly. This informs the amound of values to skip in MCMC
# png('AutocorrPlot_39_1-9.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,1:9], lag.max = 10)
# dev.off()
# png('AutocorrPlot_39_10-18.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,10:18], lag.max = 10)
# dev.off()
# png('AutocorrPlot_39_19-27.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,19:27], lag.max = 10)
# dev.off()
# png('AutocorrPlot_39_28-36.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,28:36], lag.max = 10)
# dev.off()
# png('AutocorrPlot_39_veg.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,34:42], lag.max = 10)
# dev.off()

#Last 50 steps - function doesn't like this
# png('AutocorrPlot_39_1-9_l50.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,1:9], lag.max = 10)
# dev.off()
# png('AutocorrPlot_39_10-18_l50.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,10:18], lag.max = 10)
# dev.off()
# png('AutocorrPlot_39_19-27_l50.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,19:27], lag.max = 10)
# dev.off()
# png('AutocorrPlot_39_28-36_l50.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,28:36], lag.max = 10)
# dev.off()
# png('AutocorrPlot_39_veg_l50.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,34:42], lag.max = 10)
# dev.off()


#Stationarity of chain - Geweke
#Computed for individual chains
# png('GewekePlot_39_1-9.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Parext$chain[,1:9])
# dev.off()
# png('GewekePlot_39_10-18.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Parext$chain[,10:18])
# dev.off()
# png('GewekePlot_39_19-27.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Parext$chain[,19:27])
# dev.off()
# png('GewekePlot_39_28-36.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Parext$chain[,28:36])
# dev.off()
# png('GewekePlot_39_veg.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Parext$chain[,34:42])
# dev.off()

#need at least 50 chain steps for this
#gelman.plot(x, bin.width = 10, max.bins = 50, confidence = 0.95, transform = FALSE, autoburnin=TRUE, auto.layout = TRUE, ask, col, lty, xlab, ylab, type, ...)

#Parallel coordinate plot - including all parameters is a bit much.
# png('parcoord_39.png', res = 300, units = 'in', width = 10, height = 5)
# mcmc_parcoord(outParext$chain, transform = function(x) {(x - min(x)) / (max(x)-min(x))})
# dev.off()

#Density overlay (mcmc_dens_overlay) - all chains should have similar densities for each variable
# png('densOverlay_39.png', res = 300, units = 'in', width = 10, height = 10)
# mcmc_dens_overlay(out_Parext$chain)
# dev.off()

#Rejection Rate
#Reject_Ch39 = coda::rejectionRate(out_Parext$chain)

#Highest Posterior Density Interval
#HPDs_Ch39 = coda::HPDinterval(out_Parext$chain)
#HPDs_Ch39_bt = BayesianTools::getCredibleIntervals(out_Parext$chain[[1]])

#Effective sample size for each parameter should be similar
#EffSize_Ch39 = coda::effectiveSize(out_Parext$chain)

# Accept/Reject rates for each chain----
#AR = read.table('AcceptReject_c.txt', header=TRUE)
#ARrates = vector('numeric', length = ncol(AR))
#for (i in 1:ncol(AR)){
#  ARrates[i] = sum(AR[,i])/nrow(AR)
#}
#png('AcceptRejectHist.png', res = 200, height=5, width=5, units='in')
#hist(ARrates, xlim=c(0,1))
#dev.off()

#Fixme: Check for runs of 0s in chains

# Likelihoods----
#LQ = read.csv('LikeParamsQ_c.csv', stringsAsFactors = FALSE)
#LTN = read.csv('LikeParamsTN_c.csv', stringsAsFactors = FALSE)
#Read observation timeseries
#obsQ = read.table("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\obs\\BaismanStreamflow_Feb2020Revised_Cal.txt", stringsAsFactors = FALSE, sep = '\t', header=TRUE)
#obsQ = obsQ[which((as.Date(obsQ$Date) >= as.Date('2004-10-01')) & (as.Date(obsQ$Date) <= as.Date('2013-09-30'))),]
#obsTN = read.table("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\obs\\TN_Feb2020Revised_Cal.txt", stringsAsFactors = FALSE, sep = '\t', header=TRUE)

#Fixme: histogram from chains, not from csv files.
#hist(LQ$logL/nrow(obsQ))

#Fixme: Parameter histograms for likelihoods from AnalyzeLikelihoods script

#10 Chains----
# Random seed----
set.seed(2077)
# Load Files----
setwd(dir_Ch10)
load(file = paste0(dir_Ch10, '/OutputWorkspace-10Ch.RData'), verbose = FALSE)
rm(cl, Info, i, arg, startValues, tic_Script)
out_Ch10_s100 = out_Parext
rm(out_Parext)
load(file = paste0(dir_Ch10, '/OutputWorkspace-10Ch_r.RData'), verbose = FALSE)
for (i in 1:length(out_Parext_Restart$chain)){
  colnames(out_Parext_Restart$chain[[i]]) = colnames(temp$chain[[1]])
}
rm(cl, Info, i, arg, temp)
out_Ch10_s200 = out_Parext_Restart
rm(out_Parext_Restart)
load(file = paste0(dir_Ch10, '/OutputWorkspace-10Ch_s300.RData'), verbose = FALSE)
rm(cl, Info, i, arg, temp)
out_Ch10_s300 = out_Parext_Restart
rm(out_Parext_Restart)
load(file = paste0(dir_Ch10, '/OutputWorkspace-10Ch_s400.RData'), verbose = FALSE)
rm(cl, Info, i, arg, temp)
out_Ch10_s400 = out_Parext_Restart
rm(out_Parext_Restart)
load(file = paste0(dir_Ch10, '/OutputWorkspace-10Ch_s500.RData'), verbose = FALSE)
rm(cl, Info, i, arg, temp)
out_Ch10_s500 = out_Parext_Restart
rm(out_Parext_Restart)
load(file = paste0(dir_Ch10, '/OutputWorkspace-10Ch_s600.RData'), verbose = FALSE)
rm(cl, Info, i, arg, temp)
out_Ch10_s600 = out_Parext_Restart
rm(out_Parext_Restart)

#  Remove the burnin from all greater than 500----
out_Ch10_s500$burnrm = out_Ch10_s500$chain[201:500,]
class(out_Ch10_s500$burnrm) = 'mcmc.list'
out_Ch10_s600$burnrm = out_Ch10_s600$chain[201:600,]
class(out_Ch10_s600$burnrm) = 'mcmc.list'

# Plot and summarize output----
#Fixme: this requires cores to be setup to run in parallel, with all of the exported variables.
#summary(out_Parext)

#  Marginals - Useful to see the prior and posterior on same plot.----
PriorSample = prior$sampler(n = 50000)
png('marginalplot_10_atm_s100.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s100$chain[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s100.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s100$chain[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s100.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s100$chain[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s100.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s100$chain[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s100.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s100$chain[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s200$chain[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s200$chain[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s200$chain[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s200$chain[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s100.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s200$chain[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s300.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s300$chain[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s300.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s300$chain[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s300.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s300$chain[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s300.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s300$chain[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s300.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s300$chain[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s400$chain[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s400$chain[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s400$chain[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s400$chain[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s400$chain[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s500.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s500$chain[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s500.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s500$chain[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s500.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s500$chain[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s500.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s500$chain[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s500.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s500$chain[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s600.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s600$chain[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s600.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s600$chain[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s600.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s600$chain[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s600.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s600$chain[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s600.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s600$chain[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

#Only for last 50 chain steps
png('marginalplot_10_atm_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s600$chain[551:600,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s600$chain[551:600,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s600$chain[551:600,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s600$chain[551:600,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s600$chain[551:600,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

#Removing the first 200 (burnin)
png('marginalplot_10_atm_s500_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s500$burnrm[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s500_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s500$burnrm[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s500_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s500$burnrm[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s500_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s500$burnrm[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s500_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s500$burnrm[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s600_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s600$burnrm[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s600_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s600$burnrm[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s600_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s600$burnrm[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s600_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s600$burnrm[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s600_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_s600$burnrm[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()


#  scatterplot matrix----
#Not sure how is best to view this scatterplot matrix because there are too many parameters to view in one grid.
png('corplot_10_atm_s500.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_Ch10_s500$burnrm, scaleCorText = TRUE, whichParameters = c(2:6, 1, 33))
dev.off()
png('corplot_10_s8GW_s500.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_Ch10_s500$burnrm, scaleCorText = TRUE, whichParameters = c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26, 1, 33))
dev.off()
png('corplot_10_s9GW_s500.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_Ch10_s500$burnrm, scaleCorText = TRUE, whichParameters = c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21, 10, 12, 18, 20, 1, 33))
dev.off()
png('corplot_10_veg_s500.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_Ch10_s500$burnrm, scaleCorText = TRUE, whichParameters = c(34:42, 1,33))
dev.off()
#More converged parameters
png('corplot_10_conv_s500.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_Ch10_s500$burnrm, scaleCorText = TRUE, whichParameters = c())
dev.off()
#Most sensitive parameters
png('corplot_10_sens_s500.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_Ch10_s500$burnrm, scaleCorText = TRUE, whichParameters = c(34:42, 1,33))
dev.off()

png('corplot_10_atm_s600.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_Ch10_s600$burnrm, scaleCorText = TRUE, whichParameters = c(2:6, 1, 33))
dev.off()
png('corplot_10_s8GW_s600.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_Ch10_s600$burnrm, scaleCorText = TRUE, whichParameters = c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26, 1, 33))
dev.off()
png('corplot_10_s9GW_s600.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_Ch10_s600$burnrm, scaleCorText = TRUE, whichParameters = c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21, 10, 12, 18, 20, 1, 33))
dev.off()
png('corplot_10_veg_s600.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_Ch10_s600$burnrm, scaleCorText = TRUE, whichParameters = c(34:42, 1,33))
dev.off()
#More converged parameters
png('corplot_10_conv_s600.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_Ch10_s600$burnrm, scaleCorText = TRUE, whichParameters = c())
dev.off()
#Most sensitive parameters
png('corplot_10_sens_s600.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_Ch10_s600$burnrm, scaleCorText = TRUE, whichParameters = c(34:42, 1,33))
dev.off()

#mcmc_pairs in bayesplot is alternative method
#png('pairs_10.png', res = 300, units = 'in', width = 10, height = 10)
#mcmc_pairs(out_Ch10_s100$chain[,1:10])
#dev.off()

#  Trace plots with posterior density for all parameter values----
# chains should look similar, not get stuck
png('traceplot_10_1-4.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s100$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s100$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s100$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s100$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s100$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s100$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s100$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s100$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s100$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s100$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s100$chain[,41:42], smooth = FALSE)
dev.off()

png('traceplot_10_1-4_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s200$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s200$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s200$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s200$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s200$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s200$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s200$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s200$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s200$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s200$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s200$chain[,41:42], smooth = FALSE)
dev.off()

png('traceplot_10_1-4_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s300$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s300$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s300$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s300$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s300$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s300$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s300$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s300$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s300$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s300$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s300$chain[,41:42], smooth = FALSE)
dev.off()

png('traceplot_10_1-4_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s400$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s400$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s400$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s400$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s400$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s400$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s400$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s400$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s400$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s400$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s400$chain[,41:42], smooth = FALSE)
dev.off()

png('traceplot_10_1-4_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s500$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s500$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s500$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s500$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s500$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s500$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s500$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s500$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s500$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s500$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s500$chain[,41:42], smooth = FALSE)
dev.off()

png('traceplot_10_1-4_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s600$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s600$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s600$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s600$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s600$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s600$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s600$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s600$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s600$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s600$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_s600$chain[,41:42], smooth = FALSE)
dev.off()

#mcmc_trace in bayesplot as alternative traceplots
# png('traceplot_10_1-9_b.png', res = 300, units = 'in', width = 7, height = 7)
# mcmc_trace(out_Ch10_s100$chain[,1:9])
# dev.off()
# png('traceplot_10_10-18_b.png', res = 300, units = 'in', width = 7, height = 7)
# mcmc_trace(out_Ch10_s100$chain[,10:18])
# dev.off()
# png('traceplot_10_19-27_b.png', res = 300, units = 'in', width = 7, height = 7)
# mcmc_trace(out_Ch10_s100$chain[,19:27])
# dev.off()
# png('traceplot_10_28-36_b.png', res = 300, units = 'in', width = 7, height = 7)
# mcmc_trace(out_Ch10_s100$chain[,28:36])
# dev.off()
# png('traceplot_10_veg_b.png', res = 300, units = 'in', width = 7, height = 7)
# mcmc_trace(out_Ch10_s100$chain[,34:42])
# dev.off()

#  Gelman Diagnostics----
#Fixme: This requires cores to be setup to run in parallel
#png('gelmanplot_10.png', res = 300, units = 'in', width = 7, height = 7)
#gelmanDiagnostics(sampler = out_Ch10_s100, plot = TRUE)
#dev.off()
#png('diagnosticplot_10.png', res = 300, units = 'in', width = 7, height = 7)
#plotDiagnostic(out = out_Ch10_s100)
#dev.off()

#stopCluster(cl)

# Run convergence diagnostics----
#R-hat - Gelman-Rubin within and between chain variance for all variables - target less than 1.05 for all
#gelman.diag(out_Ch10_s100$chain[,1:42])
#Autocorrelation of chain for each parameter should drop quickly. This informs the amount of values to skip in MCMC
#Fixme: this function plots individual chains. Would be good to average the autocorrelation for each chain and plot with error bars.
png('AutocorrPlot_10_1-9.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s100$chain[,1:9], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_10-18.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s100$chain[,10:18], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_19-27.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s100$chain[,19:27], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_28-36.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s100$chain[,28:36], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_veg.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s100$chain[,34:42], lag.max = 10, ask = FALSE)
dev.off()

png('AutocorrPlot_10_1-9_s200.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s200$chain[,1:9], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_10-18_s200.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s200$chain[,10:18], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_19-27_s200.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s200$chain[,19:27], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_28-36_s200.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s200$chain[,28:36], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_veg_s200.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s200$chain[,34:42], lag.max = 10, ask = FALSE)
dev.off()

png('AutocorrPlot_10_1-9_s300.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s300$chain[,1:9], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_10-18_s300.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s300$chain[,10:18], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_19-27_s300.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s300$chain[,19:27], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_28-36_s300.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s300$chain[,28:36], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_veg_s300.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s300$chain[,34:42], lag.max = 10, ask = FALSE)
dev.off()

png('AutocorrPlot_10_1-9_s400.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s400$chain[,1:9], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_10-18_s400.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s400$chain[,10:18], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_19-27_s400.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s400$chain[,19:27], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_28-36_s400.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s400$chain[,28:36], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_veg_s400.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s400$chain[,34:42], lag.max = 10, ask = FALSE)
dev.off()

png('AutocorrPlot_10_1-9_s500.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s500$chain[,1:9], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_10-18_s500.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s500$chain[,10:18], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_19-27_s500.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s500$chain[,19:27], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_28-36_s500.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s500$chain[,28:36], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_veg_s500.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s500$chain[,34:42], lag.max = 10, ask = FALSE)
dev.off()

png('AutocorrPlot_10_1-9_s600.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s600$chain[,1:9], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_10-18_s600.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s600$chain[,10:18], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_19-27_s600.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s600$chain[,19:27], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_28-36_s600.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s600$chain[,28:36], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_veg_s600.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s600$chain[,34:42], lag.max = 10, ask = FALSE)
dev.off()

#Burnin removed
png('AutocorrPlot_10_1-9_s500.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s500$burnrm[,1:9], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_10-18_s500.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s500$burnrm[,10:18], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_19-27_s500.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s500$burnrm[,19:27], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_28-36_s500.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s500$burnrm[,28:36], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_veg_s500.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s500$burnrm[,34:42], lag.max = 10, ask = FALSE)
dev.off()

png('AutocorrPlot_10_1-9_s600.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s600$burnrm[,1:9], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_10-18_s600.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s600$burnrm[,10:18], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_19-27_s600.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s600$burnrm[,19:27], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_28-36_s600.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s600$burnrm[,28:36], lag.max = 10, ask = FALSE)
dev.off()
png('AutocorrPlot_10_veg_s600.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Ch10_s600$burnrm[,34:42], lag.max = 10, ask = FALSE)
dev.off()

#Stationarity of chain - Geweke
#Fixme: This is computed for individual chains. code to loop in first figure
# png('GewekePlot_10_1-9.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Ch10_s100$chain[,1:9])
# coda::geweke.plot(as.mcmc(out_Ch10_s300$chain[[1]][150:300,1:9]))
# dev.off()
# png('GewekePlot_10_10-18.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Ch10_s100$chain[,10:18])
# dev.off()
# png('GewekePlot_10_19-27.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Ch10_s100$chain[,19:27])
# dev.off()
# png('GewekePlot_10_28-36.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Ch10_s100$chain[,28:36])
# dev.off()
# png('GewekePlot_10_veg.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Ch10_s100$chain[,34:42])
# dev.off()

#need at least 50 chain steps for this
gelman.plot(x = out_Ch10_s400$chain[,-c(43,44,45)], autoburnin = TRUE, ylim = c(0,2), ask=FALSE)
gelman.plot(x = out_Ch10_s500$chain[,-c(43,44,45)], autoburnin = TRUE, ylim = c(0,2), ask=FALSE)
gelman.plot(x = out_Ch10_s600$chain[,-c(43,44,45)], autoburnin = TRUE, ylim = c(0,2), ask=FALSE)
#Burning removed
gelman.plot(x = out_Ch10_s500$burnrm[,-c(43,44,45)], autoburnin = TRUE, ylim = c(0,2), ask=FALSE)
gelman.plot(x = out_Ch10_s600$burnrm[,-c(43,44,45)], autoburnin = TRUE, ylim = c(0,2), ask=FALSE)

#Parallel coordinate plot - including all parameters is a bit much.
#png('parcoord_10.png', res = 300, units = 'in', width = 10, height = 5)
#mcmc_parcoord(outParext$chain, transform = function(x) {(x - min(x)) / (max(x)-min(x))})
#dev.off()

#Density overlay (mcmc_dens_overlay) - all chains should have similar densities for each variable
png('densOverlay_10_s100.png', res = 300, units = 'in', width = 10, height = 10)
mcmc_dens_overlay(out_Ch10_s100$chain)
dev.off()
png('densOverlay_10_s200.png', res = 300, units = 'in', width = 10, height = 10)
mcmc_dens_overlay(out_Ch10_s200$chain)
dev.off()
png('densOverlay_10_s300.png', res = 300, units = 'in', width = 10, height = 10)
mcmc_dens_overlay(out_Ch10_s300$chain)
dev.off()
png('densOverlay_10_s400.png', res = 300, units = 'in', width = 10, height = 10)
mcmc_dens_overlay(out_Ch10_s400$chain)
dev.off()
png('densOverlay_10_s500.png', res = 300, units = 'in', width = 10, height = 10)
mcmc_dens_overlay(out_Ch10_s500$chain)
dev.off()
png('densOverlay_10_s600.png', res = 300, units = 'in', width = 10, height = 10)
mcmc_dens_overlay(out_Ch10_s600$chain)
dev.off()
#Burnin Removed
png('densOverlay_10_s500b.png', res = 300, units = 'in', width = 10, height = 10)
mcmc_dens_overlay(out_Ch10_s500$burnrm)
dev.off()
png('densOverlay_10_s600b.png', res = 300, units = 'in', width = 10, height = 10)
mcmc_dens_overlay(out_Ch10_s600$burnrm)
dev.off()

#Rejection Rate
Reject_Ch10_s100 = coda::rejectionRate(out_Ch10_s100$chain)
Reject_Ch10_s200 = coda::rejectionRate(out_Ch10_s200$chain)
Reject_Ch10_s300 = coda::rejectionRate(out_Ch10_s300$chain)
Reject_Ch10_s400 = coda::rejectionRate(out_Ch10_s400$chain)
Reject_Ch10_s500 = coda::rejectionRate(out_Ch10_s500$chain)
Reject_Ch10_s600 = coda::rejectionRate(out_Ch10_s600$chain)
#Burnin Removed
Reject_Ch10_s500r = coda::rejectionRate(out_Ch10_s500$chain)
Reject_Ch10_s600r = coda::rejectionRate(out_Ch10_s600$chain)

#Highest Posterior Density Interval
HPDs_Ch10 = coda::HPDinterval(out_Ch10_s600$chain)
HPDs_Ch10_bt = BayesianTools::getCredibleIntervals(out_Ch10_s600$chain[[1]])

#Effective sample size for each parameter should be similar
EffSize_Ch10 = coda::effectiveSize(out_Ch10_s600$chain)

# Accept/Reject rates for each chain----
AR_Ch10_s100 = read.table('AcceptReject_c.txt', header=TRUE)
AR_Ch10_s200 = read.table('AcceptReject_c_s200.txt', header=TRUE)
AR_Ch10_s300 = read.table('AcceptReject_c_s300.txt', header=TRUE)
AR_Ch10_s400 = read.table('AcceptReject_c_s400.txt', header=TRUE)
AR_Ch10_s500 = read.table('AcceptReject_c_s500.txt', header=TRUE)
AR_Ch10_s600 = read.table('AcceptReject_c_s600.txt', header=TRUE)
#Join into one
AR_Ch10 = rbind(AR_Ch10_s100, AR_Ch10_s200, AR_Ch10_s300, AR_Ch10_s400, AR_Ch10_s500, AR_Ch10_s600)
rm(AR_Ch10_s100, AR_Ch10_s200, AR_Ch10_s300, AR_Ch10_s400, AR_Ch10_s500, AR_Ch10_s600)

ARrates_Ch10 = vector('numeric', length = ncol(AR_Ch10))
for (i in 1:ncol(AR_Ch10)){
  ARrates_Ch10[i] = sum(AR_Ch10[,i])/nrow(AR_Ch10)
}

png('AcceptRejectHist.png', res = 200, height=5, width=5, units='in')
hist(ARrates_Ch10, xlim=c(0,1))
dev.off()

#10 Chains - Ch1----
# Random seed----
set.seed(2078)
# Load Files----
setwd(dir_Ch10_1)
load(file = paste0(dir_Ch10_1, '/OutputWorkspace-10Ch-1.RData'), verbose = FALSE)
out_Ch10_1_s100 = out_Parext
rm(cl, Info, i, arg, startValues, tic_Script, out_Parext)
load(file = paste0(dir_Ch10_1, '/OutputWorkspace-10Ch-1_r.RData'), verbose = FALSE)
for (i in 1:length(out_Parext_Restart$chain)){
  colnames(out_Parext_Restart$chain[[i]]) = colnames(temp$chain[[1]])
}
rm(cl, Info, i, arg, temp)
out_Ch10_1_s200 = out_Parext_Restart
rm(out_Parext_Restart)
load(file = paste0(dir_Ch10_1, '/OutputWorkspace-10Ch-1_s300.RData'), verbose = FALSE)
rm(cl, Info, i, arg, temp)
out_Ch10_1_s300 = out_Parext_Restart
rm(out_Parext_Restart)
load(file = paste0(dir_Ch10_1, '/OutputWorkspace-10Ch-1_s400.RData'), verbose = FALSE)
rm(cl, Info, i, arg, temp)
out_Ch10_1_s400 = out_Parext_Restart
rm(out_Parext_Restart)
load(file = paste0(dir_Ch10_1, '/OutputWorkspace-10Ch-1_s500.RData'), verbose = FALSE)
rm(cl, Info, i, arg, temp)
out_Ch10_1_s500 = out_Parext_Restart
rm(out_Parext_Restart)
load(file = paste0(dir_Ch10_1, '/OutputWorkspace-10Ch-1_s600.RData'), verbose = FALSE)
rm(cl, Info, i, arg, temp)
out_Ch10_1_s600 = out_Parext_Restart
rm(out_Parext_Restart)

#  Remove the burnin from all greater than 500----
out_Ch10_1_s500$burnrm = out_Ch10_1_s500$chain[201:500,]
class(out_Ch10_1_s500$burnrm) = 'mcmc.list'
out_Ch10_1_s600$burnrm = out_Ch10_1_s600$chain[201:600,]
class(out_Ch10_1_s600$burnrm) = 'mcmc.list'

# Plot and summarize output----
#Fixme: this requires cores to be setup to run in parallel, with all of the exported variables.
#summary(out_Parext)

#  Marginals - Useful to see the prior and posterior on same plot----
PriorSample = prior$sampler(n = 50000)
png('marginalplot_10_atm_s100.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s100$chain[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s100.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s100$chain[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s100.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s100$chain[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s100.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s100$chain[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s100.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s100$chain[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s200$chain[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s200$chain[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s200$chain[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s200$chain[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s100.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s200$chain[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s300.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s300$chain[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s300.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s300$chain[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s300.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s300$chain[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s300.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s300$chain[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s300.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s300$chain[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s400$chain[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s400$chain[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s400$chain[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s400$chain[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s400$chain[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s500.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s500$chain[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s500.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s500$chain[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s500.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s500$chain[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s500.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s500$chain[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s500.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s500$chain[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s600.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s600$chain[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s600.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s600$chain[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s600.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s600$chain[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s600.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s600$chain[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s600.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s600$chain[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

#Only for last 50 chain steps
png('marginalplot_10_atm_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s600$chain[551:600,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s600$chain[551:600,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s600$chain[551:600,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s600$chain[551:600,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s600$chain[551:600,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

#Removing the first 200 (burnin)
png('marginalplot_10_atm_s500_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s500$burnrm[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s500_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s500$burnrm[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s500_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s500$burnrm[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s500_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s500$burnrm[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s500_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s500$burnrm[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s600_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s600$burnrm[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s600_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s600$burnrm[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s600_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s600$burnrm[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s600_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s600$burnrm[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s600_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_1_s600$burnrm[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

#  scatterplot matrix----
#Not sure how is best to view this scatterplot matrix because there are too many parameters to view in one grid.
#png('corplot_10.png', res = 300, units = 'in', width = 14, height = 14)
#correlationPlot(mat = out_Parext, scaleCorText = TRUE)
#dev.off()

#png('pairs_10.png', res = 300, units = 'in', width = 10, height = 10)
#mcmc_pairs(out_Parext$chain[,1:10])
#dev.off()

#  Trace plots with posterior density for all parameter values----
# chains should look similar, not get stuck
png('traceplot_10_1-4.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s100$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s100$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s100$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s100$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s100$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s100$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s100$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s100$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s100$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s100$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s100$chain[,41:42], smooth = FALSE)
dev.off()

png('traceplot_10_1-4_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s200$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s200$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s200$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s200$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s200$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s200$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s200$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s200$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s200$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s200$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s200$chain[,41:42], smooth = FALSE)
dev.off()

png('traceplot_10_1-4_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s300$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s300$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s300$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s300$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s300$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s300$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s300$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s300$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s300$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s300$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s300$chain[,41:42], smooth = FALSE)
dev.off()

png('traceplot_10_1-4_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s400$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s400$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s400$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s400$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s400$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s400$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s400$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s400$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s400$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s400$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s400$chain[,41:42], smooth = FALSE)
dev.off()

png('traceplot_10_1-4_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s500$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s500$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s500$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s500$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s500$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s500$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s500$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s500$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s500$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s500$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s500$chain[,41:42], smooth = FALSE)
dev.off()

png('traceplot_10_1-4_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s600$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s600$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s600$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s600$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s600$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s600$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s600$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s600$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s600$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s600$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_1_s600$chain[,41:42], smooth = FALSE)
dev.off()

#  Gelman Diagnostics----
#Fixme: This requires cores to be setup to run in parallel
#png('gelmanplot_10.png', res = 300, units = 'in', width = 7, height = 7)
#gelmanDiagnostics(sampler = out_Parext, plot = TRUE)
#dev.off()
#png('diagnosticplot_10.png', res = 300, units = 'in', width = 7, height = 7)
#plotDiagnostic(out = out_Parext)
#dev.off()

#stopCluster(cl)

# Run convergence diagnostics----
#R-hat - Gelman-Rubin within and between chain variance for all variables - target less than 1.05 for all
#gelman.diag(out_Parext$chain[,1:42])
#Autocorrelation of chain for each parameter should drop quickly. This informs the amound of values to skip in MCMC
png('AutocorrPlot_10_1-9.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,1:9], lag.max = 10)
dev.off()
png('AutocorrPlot_10_10-18.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,10:18], lag.max = 10)
dev.off()
png('AutocorrPlot_10_19-27.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,19:27], lag.max = 10)
dev.off()
png('AutocorrPlot_10_28-36.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,28:36], lag.max = 10)
dev.off()
png('AutocorrPlot_10_veg.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,34:42], lag.max = 10)
dev.off()


#Stationarity of chain - Geweke
#Fixme: This is computed for individual chains
# png('GewekePlot_10_1-9.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Parext$chain[,1:9])
# dev.off()
# png('GewekePlot_10_10-18.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Parext$chain[,10:18])
# dev.off()
# png('GewekePlot_10_19-27.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Parext$chain[,19:27])
# dev.off()
# png('GewekePlot_10_28-36.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Parext$chain[,28:36])
# dev.off()
# png('GewekePlot_10_veg.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Parext$chain[,34:42])
# dev.off()

#need at least 50 chain steps for this
#gelman.plot(x, bin.width = 10, max.bins = 50, confidence = 0.95, transform = FALSE, autoburnin=TRUE, auto.layout = TRUE, ask, col, lty, xlab, ylab, type, ...)

#Parallel coordinate plot - including all parameters is a bit much.
png('parcoord_10.png', res = 300, units = 'in', width = 10, height = 5)
mcmc_parcoord(outParext$chain, transform = function(x) {(x - min(x)) / (max(x)-min(x))})
dev.off()

#Density overlay (mcmc_dens_overlay) - all chains should have similar densities for each variable
png('densOverlay_10.png', res = 300, units = 'in', width = 10, height = 10)
mcmc_dens_overlay(out_Parext$chain)
dev.off()

#Rejection Rate
Reject_Ch10 = coda::rejectionRate(out_Parext$chain)

#Highest Posterior Density Interval
HPDs_Ch10 = coda::HPDinterval(out_Parext$chain)
HPDs_Ch10_bt = BayesianTools::getCredibleIntervals(out_Parext$chain[[1]])

#Effective sample size for each parameter should be similar
EffSize_Ch10 = coda::effectiveSize(out_Parext$chain)

# Accept/Reject rates for each chain----
AR_Ch10_1_s100 = read.table('AcceptReject_c.txt', header=TRUE)
AR_Ch10_1_s200 = read.table('AcceptReject_c_s200.txt', header=TRUE)
AR_Ch10_1_s300 = read.table('AcceptReject_c_s300.txt', header=TRUE)
AR_Ch10_1_s400 = read.table('AcceptReject_c_s400.txt', header=TRUE)
AR_Ch10_1_s500 = read.table('AcceptReject_c_s500.txt', header=TRUE)
AR_Ch10_1_s600 = read.table('AcceptReject_c_s600.txt', header=TRUE)
#Join into one
AR_Ch10_1 = rbind(AR_Ch10_1_s100, AR_Ch10_1_s200, AR_Ch10_1_s300, AR_Ch10_1_s400, AR_Ch10_1_s500, AR_Ch10_1_s600)
rm(AR_Ch10_1_s100, AR_Ch10_1_s200, AR_Ch10_1_s300, AR_Ch10_1_s400, AR_Ch10_1_s500, AR_Ch10_1_s600)

ARrates_Ch10_1 = vector('numeric', length = ncol(AR_Ch10_1))
for (i in 1:ncol(AR_Ch10_1)){
  ARrates_Ch10_1[i] = sum(AR_Ch10_1[,i])/nrow(AR_Ch10_1)
}

png('AcceptRejectHist.png', res = 200, height=5, width=5, units='in')
hist(ARrates_Ch10_1, xlim=c(0,1))
dev.off()

#10 Chains - Ch2----
# Random seed----
set.seed(2079)
# Load Files----
setwd(dir_Ch10_2)
load(file = paste0(dir_Ch10_2, '/OutputWorkspace-10Ch-2.RData'), verbose = FALSE)
out_Ch10_2_s100 = out_Parext
rm(cl, Info, i, arg, startValues, tic_Script, out_Parext)
load(file = paste0(dir_Ch10_2, '/OutputWorkspace-10Ch-2_r.RData'), verbose = FALSE)
for (i in 1:length(out_Parext_Restart$chain)){
  colnames(out_Parext_Restart$chain[[i]]) = colnames(temp$chain[[1]])
}
out_Ch10_2_s200 = out_Parext_Restart
rm(cl, Info, i, arg, temp, out_Parext_Restart)
load(file = paste0(dir_Ch10_2, '/OutputWorkspace-10Ch-2_s300.RData'), verbose = FALSE)
out_Ch10_2_s300 = out_Parext_Restart
rm(cl, Info, i, arg, temp, out_Parext_Restart)
load(file = paste0(dir_Ch10_2, '/OutputWorkspace-10Ch-2_s400.RData'), verbose = FALSE)
rm(cl, Info, i, arg, temp)
out_Ch10_2_s400 = out_Parext_Restart
rm(out_Parext_Restart)
load(file = paste0(dir_Ch10_2, '/OutputWorkspace-10Ch-2_s500.RData'), verbose = FALSE)
rm(cl, Info, i, arg, temp)
out_Ch10_2_s500 = out_Parext_Restart
rm(out_Parext_Restart)
load(file = paste0(dir_Ch10_2, '/OutputWorkspace-10Ch-2_s600.RData'), verbose = FALSE)
rm(cl, Info, i, arg, temp)
out_Ch10_2_s600 = out_Parext_Restart
rm(out_Parext_Restart)

#  Remove the burnin from all greater than 500----
out_Ch10_2_s500$burnrm = out_Ch10_2_s500$chain[201:500,]
class(out_Ch10_2_s500$burnrm) = 'mcmc.list'
out_Ch10_2_s600$burnrm = out_Ch10_2_s600$chain[201:600,]
class(out_Ch10_2_s600$burnrm) = 'mcmc.list'

# Plot and summarize output----
#Fixme: this requires cores to be setup to run in parallel, with all of the exported variables.
#summary(out_Parext)

#  Marginals - Useful to see the prior and posterior on same plot----
PriorSample = prior$sampler(n = 50000)
png('marginalplot_10_atm_s100.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s100$chain[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s100.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s100$chain[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s100.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s100$chain[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s100.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s100$chain[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s100.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s100$chain[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s200$chain[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s200$chain[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s200$chain[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s200$chain[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s100.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s200$chain[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s300.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s300$chain[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s300.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s300$chain[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s300.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s300$chain[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s300.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s300$chain[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s300.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s300$chain[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s400$chain[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s400$chain[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s400$chain[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s400$chain[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s400$chain[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s500.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s500$chain[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s500.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s500$chain[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s500.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s500$chain[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s500.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s500$chain[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s500.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s500$chain[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s600.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s600$chain[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s600.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s600$chain[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s600.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s600$chain[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s600.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s600$chain[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s600.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s600$chain[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

#Only for last 50 chain steps
png('marginalplot_10_atm_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s600$chain[551:600,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s600$chain[551:600,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s600$chain[551:600,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s600$chain[551:600,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s600$chain[551:600,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

#Removing the first 200 (burnin)
png('marginalplot_10_atm_s500_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s500$burnrm[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s500_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s500$burnrm[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s500_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s500$burnrm[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s500_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s500$burnrm[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s500_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s500$burnrm[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s600_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s600$burnrm[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s600_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s600$burnrm[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s600_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s600$burnrm[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s600_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s600$burnrm[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s600_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_2_s600$burnrm[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

#  scatterplot matrix----
#Not sure how is best to view this scatterplot matrix because there are too many parameters to view in one grid.
#png('corplot_10.png', res = 300, units = 'in', width = 14, height = 14)
#correlationPlot(mat = out_Parext, scaleCorText = TRUE)
#dev.off()

#png('pairs_10.png', res = 300, units = 'in', width = 10, height = 10)
#mcmc_pairs(out_Parext$chain[,1:10])
#dev.off()

#  Trace plots with posterior density for all parameter values----
png('traceplot_10_1-4.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s100$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s100$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s100$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s100$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s100$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s100$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s100$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s100$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s100$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s100$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s100$chain[,41:42], smooth = FALSE)
dev.off()

png('traceplot_10_1-4_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s200$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s200$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s200$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s200$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s200$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s200$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s200$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s200$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s200$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s200$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s200$chain[,41:42], smooth = FALSE)
dev.off()

png('traceplot_10_1-4_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s300$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s300$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s300$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s300$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s300$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s300$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s300$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s300$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s300$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s300$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s300$chain[,41:42], smooth = FALSE)
dev.off()

png('traceplot_10_1-4_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s400$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s400$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s400$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s400$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s400$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s400$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s400$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s400$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s400$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s400$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s400$chain[,41:42], smooth = FALSE)
dev.off()

png('traceplot_10_1-4_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s500$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s500$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s500$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s500$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s500$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s500$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s500$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s500$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s500$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s500$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s500$chain[,41:42], smooth = FALSE)
dev.off()

png('traceplot_10_1-4_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s600$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s600$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s600$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s600$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s600$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s600$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s600$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s600$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s600$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s600$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_2_s600$chain[,41:42], smooth = FALSE)
dev.off()

#  Gelman Diagnostics----
#Fixme: This requires cores to be setup to run in parallel
#png('gelmanplot_10.png', res = 300, units = 'in', width = 7, height = 7)
#gelmanDiagnostics(sampler = out_Parext, plot = TRUE)
#dev.off()
#png('diagnosticplot_10.png', res = 300, units = 'in', width = 7, height = 7)
#plotDiagnostic(out = out_Parext)
#dev.off()

#stopCluster(cl)

# Run convergence diagnostics----
#R-hat - Gelman-Rubin within and between chain variance for all variables - target less than 1.05 for all
#gelman.diag(out_Parext$chain[,1:42])
#Autocorrelation of chain for each parameter should drop quickly. This informs the amound of values to skip in MCMC
png('AutocorrPlot_10_1-9.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,1:9], lag.max = 10)
dev.off()
png('AutocorrPlot_10_10-18.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,10:18], lag.max = 10)
dev.off()
png('AutocorrPlot_10_19-27.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,19:27], lag.max = 10)
dev.off()
png('AutocorrPlot_10_28-36.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,28:36], lag.max = 10)
dev.off()
png('AutocorrPlot_10_veg.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,34:42], lag.max = 10)
dev.off()

#Last 50 steps - function doesn't like this
# png('AutocorrPlot_10_1-9_l50.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,1:9], lag.max = 10)
# dev.off()
# png('AutocorrPlot_10_10-18_l50.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,10:18], lag.max = 10)
# dev.off()
# png('AutocorrPlot_10_19-27_l50.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,19:27], lag.max = 10)
# dev.off()
# png('AutocorrPlot_10_28-36_l50.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,28:36], lag.max = 10)
# dev.off()
# png('AutocorrPlot_10_veg_l50.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,34:42], lag.max = 10)
# dev.off()


#Stationarity of chain - Geweke
#Fixme: This is computed for individual chains
# png('GewekePlot_10_1-9.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Parext$chain[,1:9])
# dev.off()
# png('GewekePlot_10_10-18.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Parext$chain[,10:18])
# dev.off()
# png('GewekePlot_10_19-27.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Parext$chain[,19:27])
# dev.off()
# png('GewekePlot_10_28-36.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Parext$chain[,28:36])
# dev.off()
# png('GewekePlot_10_veg.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Parext$chain[,34:42])
# dev.off()

#need at least 50 chain steps for this
#gelman.plot(x, bin.width = 10, max.bins = 50, confidence = 0.95, transform = FALSE, autoburnin=TRUE, auto.layout = TRUE, ask, col, lty, xlab, ylab, type, ...)

#Parallel coordinate plot - including all parameters is a bit much.
png('parcoord_10.png', res = 300, units = 'in', width = 10, height = 5)
mcmc_parcoord(outParext$chain, transform = function(x) {(x - min(x)) / (max(x)-min(x))})
dev.off()

#Density overlay (mcmc_dens_overlay) - all chains should have similar densities for each variable
png('densOverlay_10.png', res = 300, units = 'in', width = 10, height = 10)
mcmc_dens_overlay(out_Parext$chain)
dev.off()

#Rejection Rate
Reject_Ch10 = coda::rejectionRate(out_Parext$chain)

#Highest Posterior Density Interval
HPDs_Ch10 = coda::HPDinterval(out_Parext$chain)
HPDs_Ch10_bt = BayesianTools::getCredibleIntervals(out_Parext$chain[[1]])

#Effective sample size for each parameter should be similar
EffSize_Ch10 = coda::effectiveSize(out_Parext$chain)

# Accept/Reject rates for each chain----
AR_Ch10_2_s100 = read.table('AcceptReject_c.txt', header=TRUE)
AR_Ch10_2_s200 = read.table('AcceptReject_c_s200.txt', header=TRUE)
AR_Ch10_2_s300 = read.table('AcceptReject_c_s300.txt', header=TRUE)
AR_Ch10_2_s400 = read.table('AcceptReject_c_s400.txt', header=TRUE)
AR_Ch10_2_s500 = read.table('AcceptReject_c_s500.txt', header=TRUE)
AR_Ch10_2_s600 = read.table('AcceptReject_c_s600.txt', header=TRUE)
#Join into one
AR_Ch10_2 = rbind(AR_Ch10_2_s100, AR_Ch10_2_s200, AR_Ch10_2_s300, AR_Ch10_2_s400, AR_Ch10_2_s500, AR_Ch10_2_s600)
rm(AR_Ch10_2_s100, AR_Ch10_2_s200, AR_Ch10_2_s300, AR_Ch10_2_s400, AR_Ch10_2_s500, AR_Ch10_2_s600)

ARrates_Ch10_2 = vector('numeric', length = ncol(AR_Ch10_2))
for (i in 1:ncol(AR_Ch10_2)){
  ARrates_Ch10_2[i] = sum(AR_Ch10_2[,i])/nrow(AR_Ch10_2)
}

png('AcceptRejectHist.png', res = 200, height=5, width=5, units='in')
hist(ARrates_Ch10_2, xlim=c(0,1))
dev.off()


#10 Chains - Ch3----
# Random seed----
set.seed(2080)
# Load Files----
setwd(dir_Ch10_3)
load(file = paste0(dir_Ch10_3, '/OutputWorkspace-10Ch-3.RData'), verbose = FALSE)
out_Ch10_3_s100 = out_Parext
rm(cl, Info, i, arg, startValues, tic_Script, out_Parext)
load(file = paste0(dir_Ch10_3, '/OutputWorkspace-10Ch-3_r.RData'), verbose = FALSE)
for (i in 1:length(out_Parext_Restart$chain)){
  colnames(out_Parext_Restart$chain[[i]]) = colnames(temp$chain[[1]])
}
out_Ch10_3_s200 = out_Parext_Restart
rm(cl, Info, i, arg, temp, out_Parext_Restart)
load(file = paste0(dir_Ch10_3, '/OutputWorkspace-10Ch-3_s300.RData'), verbose = FALSE)
out_Ch10_3_s300 = out_Parext_Restart
rm(cl, Info, i, arg, temp, out_Parext_Restart)
load(file = paste0(dir_Ch10_3, '/OutputWorkspace-10Ch-3_s400.RData'), verbose = FALSE)
rm(cl, Info, i, arg, temp)
out_Ch10_3_s400 = out_Parext_Restart
rm(out_Parext_Restart)
load(file = paste0(dir_Ch10_3, '/OutputWorkspace-10Ch-3_s500.RData'), verbose = FALSE)
rm(cl, Info, i, arg, temp)
out_Ch10_3_s500 = out_Parext_Restart
rm(out_Parext_Restart)
load(file = paste0(dir_Ch10_3, '/OutputWorkspace-10Ch-3_s600.RData'), verbose = FALSE)
rm(cl, Info, i, arg, temp)
out_Ch10_3_s600 = out_Parext_Restart
rm(out_Parext_Restart)

#  Remove the burnin from all greater than 500----
out_Ch10_3_s500$burnrm = out_Ch10_3_s500$chain[201:500,]
class(out_Ch10_3_s500$burnrm) = 'mcmc.list'
out_Ch10_3_s600$burnrm = out_Ch10_3_s600$chain[201:600,]
class(out_Ch10_3_s600$burnrm) = 'mcmc.list'

# Plot and summarize output----
#Fixme: this requires cores to be setup to run in parallel, with all of the exported variables.
#summary(out_Parext)

#  Marginals - Useful to see the prior and posterior on same plot----
PriorSample = prior$sampler(n = 50000)
png('marginalplot_10_atm_s100.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s100$chain[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s100.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s100$chain[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s100.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s100$chain[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s100.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s100$chain[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s100.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s100$chain[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s200$chain[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s200$chain[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s200$chain[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s200$chain[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s100.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s200$chain[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s300.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s300$chain[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s300.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s300$chain[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s300.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s300$chain[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s300.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s300$chain[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s300.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s300$chain[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s400$chain[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s400$chain[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s400$chain[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s400$chain[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s400$chain[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s500.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s500$chain[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s500.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s500$chain[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s500.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s500$chain[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s500.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s500$chain[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s500.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s500$chain[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s600.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s600$chain[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s600.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s600$chain[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s600.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s600$chain[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s600.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s600$chain[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s600.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s600$chain[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

#Only for last 50 chain steps
png('marginalplot_10_atm_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s600$chain[551:600,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s600$chain[551:600,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s600$chain[551:600,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s600$chain[551:600,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s600$chain[551:600,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

#Removing the first 200 (burnin)
png('marginalplot_10_atm_s500_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s500$burnrm[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s500_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s500$burnrm[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s500_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s500$burnrm[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s500_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s500$burnrm[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s500_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s500$burnrm[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s600_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s600$burnrm[,2:6], prior = PriorSample[,2:6], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s8_s600_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s600$burnrm[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], prior = PriorSample[,c(23, 28, 29, 32, 24, 25, 30, 27, 31, 26)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s600_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s600$burnrm[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], prior = PriorSample[,c(7, 14, 15, 22, 8, 9, 16, 17, 11, 13, 19, 21)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9GW_s600_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s600$burnrm[,c(1,10,12,33,18,20)], prior = PriorSample[,c(1,10,12,33,18,20)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s600_brm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Ch10_3_s600$burnrm[,34:42], prior = PriorSample[,34:42], singlePanel = FALSE)
dev.off()

#  scatterplot matrix----
#Not sure how is best to view this scatterplot matrix because there are too many parameters to view in one grid.
#png('corplot_10.png', res = 300, units = 'in', width = 14, height = 14)
#correlationPlot(mat = out_Parext, scaleCorText = TRUE)
#dev.off()

#png('pairs_10.png', res = 300, units = 'in', width = 10, height = 10)
#mcmc_pairs(out_Parext$chain[,1:10])
#dev.off()

#  Trace plots with posterior density for all parameter values----
png('traceplot_10_1-4.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s100$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s100$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s100$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s100$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s100$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s100$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s100$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s100$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s100$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s100$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s100$chain[,41:42], smooth = FALSE)
dev.off()

png('traceplot_10_1-4_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s200$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s200$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s200$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s200$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s200$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s200$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s200$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s200$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s200$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s200$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42_s200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s200$chain[,41:42], smooth = FALSE)
dev.off()

png('traceplot_10_1-4_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s300$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s300$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s300$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s300$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s300$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s300$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s300$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s300$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s300$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s300$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42_s300.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s300$chain[,41:42], smooth = FALSE)
dev.off()

png('traceplot_10_1-4_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s400$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s400$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s400$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s400$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s400$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s400$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s400$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s400$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s400$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s400$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42_s400.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s400$chain[,41:42], smooth = FALSE)
dev.off()

png('traceplot_10_1-4_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s500$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s500$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s500$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s500$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s500$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s500$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s500$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s500$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s500$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s500$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42_s500.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s500$chain[,41:42], smooth = FALSE)
dev.off()

png('traceplot_10_1-4_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s600$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s600$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s600$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s600$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s600$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s600$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s600$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s600$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s600$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s600$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42_s600.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Ch10_3_s600$chain[,41:42], smooth = FALSE)
dev.off()

#  Gelman Diagnostics----
#Fixme: This requires cores to be setup to run in parallel
#png('gelmanplot_10.png', res = 300, units = 'in', width = 7, height = 7)
#gelmanDiagnostics(sampler = out_Parext, plot = TRUE)
#dev.off()
#png('diagnosticplot_10.png', res = 300, units = 'in', width = 7, height = 7)
#plotDiagnostic(out = out_Parext)
#dev.off()

#stopCluster(cl)

# Run convergence diagnostics----
#R-hat - Gelman-Rubin within and between chain variance for all variables - target less than 1.05 for all
#gelman.diag(out_Parext$chain[,1:42])
#Autocorrelation of chain for each parameter should drop quickly. This informs the amound of values to skip in MCMC
png('AutocorrPlot_10_1-9.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,1:9], lag.max = 10)
dev.off()
png('AutocorrPlot_10_10-18.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,10:18], lag.max = 10)
dev.off()
png('AutocorrPlot_10_19-27.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,19:27], lag.max = 10)
dev.off()
png('AutocorrPlot_10_28-36.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,28:36], lag.max = 10)
dev.off()
png('AutocorrPlot_10_veg.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,34:42], lag.max = 10)
dev.off()

#Last 50 steps - function doesn't like this
# png('AutocorrPlot_10_1-9_l50.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,1:9], lag.max = 10)
# dev.off()
# png('AutocorrPlot_10_10-18_l50.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,10:18], lag.max = 10)
# dev.off()
# png('AutocorrPlot_10_19-27_l50.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,19:27], lag.max = 10)
# dev.off()
# png('AutocorrPlot_10_28-36_l50.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,28:36], lag.max = 10)
# dev.off()
# png('AutocorrPlot_10_veg_l50.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,34:42], lag.max = 10)
# dev.off()


#Stationarity of chain - Geweke
#Fixme: This is computed for individual chains
# png('GewekePlot_10_1-9.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Parext$chain[,1:9])
# dev.off()
# png('GewekePlot_10_10-18.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Parext$chain[,10:18])
# dev.off()
# png('GewekePlot_10_19-27.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Parext$chain[,19:27])
# dev.off()
# png('GewekePlot_10_28-36.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Parext$chain[,28:36])
# dev.off()
# png('GewekePlot_10_veg.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Parext$chain[,34:42])
# dev.off()

#need at least 50 chain steps for this
#gelman.plot(x, bin.width = 10, max.bins = 50, confidence = 0.95, transform = FALSE, autoburnin=TRUE, auto.layout = TRUE, ask, col, lty, xlab, ylab, type, ...)

#Parallel coordinate plot - including all parameters is a bit much.
png('parcoord_10.png', res = 300, units = 'in', width = 10, height = 5)
mcmc_parcoord(outParext$chain, transform = function(x) {(x - min(x)) / (max(x)-min(x))})
dev.off()

#Density overlay (mcmc_dens_overlay) - all chains should have similar densities for each variable
png('densOverlay_10.png', res = 300, units = 'in', width = 10, height = 10)
mcmc_dens_overlay(out_Parext$chain)
dev.off()

#Rejection Rate
Reject_Ch10 = coda::rejectionRate(out_Parext$chain)

#Highest Posterior Density Interval
HPDs_Ch10 = coda::HPDinterval(out_Parext$chain)
HPDs_Ch10_bt = BayesianTools::getCredibleIntervals(out_Parext$chain[[1]])

#Effective sample size for each parameter should be similar
EffSize_Ch10 = coda::effectiveSize(out_Parext$chain)

# Accept/Reject rates for each chain----
AR_Ch10_3_s100 = read.table('AcceptReject_c.txt', header=TRUE)
AR_Ch10_3_s200 = read.table('AcceptReject_c_s200.txt', header=TRUE)
AR_Ch10_3_s300 = read.table('AcceptReject_c_s300.txt', header=TRUE)
AR_Ch10_3_s400 = read.table('AcceptReject_c_s400.txt', header=TRUE)
AR_Ch10_3_s500 = read.table('AcceptReject_c_s500.txt', header=TRUE)
AR_Ch10_3_s600 = read.table('AcceptReject_c_s600.txt', header=TRUE)
#Join into one
AR_Ch10_3 = rbind(AR_Ch10_3_s100, AR_Ch10_3_s200, AR_Ch10_3_s300, AR_Ch10_3_s400, AR_Ch10_3_s500, AR_Ch10_3_s600)
rm(AR_Ch10_3_s100, AR_Ch10_3_s200, AR_Ch10_3_s300, AR_Ch10_3_s400, AR_Ch10_3_s500, AR_Ch10_3_s600)

ARrates_Ch10_3 = vector('numeric', length = ncol(AR_Ch10_3))
for (i in 1:ncol(AR_Ch10_3)){
  ARrates_Ch10_3[i] = sum(AR_Ch10_3[,i])/nrow(AR_Ch10_3)
}

png('AcceptRejectHist.png', res = 200, height=5, width=5, units='in')
hist(ARrates_Ch10_3, xlim=c(0,1))
dev.off()
#########Synthetic Hillslope 9+10################ 
#10 Chains----
# Random seed----
set.seed(7256)
# Load Files----
setwd(dir_sCh10)
load(file = paste0(dir_sCh10, '/OutputWorkspace-10Ch_s400.RData'), verbose = FALSE)
rm(cl, Info, i, arg, startValues, tic_Script)
out_sCh10_s400 = out_Parext
rm(out_Parext)

#  Remove the burnin----
#out_Ch10_s500$burnrm = out_Ch10_s500$chain[201:500,]
#class(out_Ch10_s500$burnrm) = 'mcmc.list'

# Plot and summarize output----
#Fixme: this requires cores to be setup to run in parallel, with all of the exported variables.
#summary(out_Parext)

#  Marginals - Useful to see the prior and posterior on same plot.----
PriorSample = prior$sampler(n = 50000)
png('marginalplot_10_atm_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_sCh10_s400$chain[,c(12,2)], prior = PriorSample[,c(12,2)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_sCh10_s400$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], prior = PriorSample[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_sCh10_s400$chain[,c(1, 16:19)], prior = PriorSample[,c(1, 16:19)], singlePanel = FALSE)
dev.off()


#Only for last 50 chain steps

#Removing the first 200 (burnin)

#  scatterplot matrix----
#Not sure how is best to view this scatterplot matrix because there are too many parameters to view in one grid.
png('corplot_10_atm_s400.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_sCh10_s400$chain, scaleCorText = TRUE, whichParameters = c(12,2))
dev.off()
png('corplot_10_veg_s400.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_sCh10_s400$chain, scaleCorText = TRUE, whichParameters = c(34:42, 1,33))
dev.off()
#More converged parameters
png('corplot_10_conv_s400.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_sCh10_s400$chain, scaleCorText = TRUE, whichParameters = c())
dev.off()
#Most sensitive parameters
png('corplot_10_sens_s400.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_sCh10_s400$chain, scaleCorText = TRUE, whichParameters = c(34:42, 1,33))
dev.off()

#mcmc_pairs in bayesplot is alternative method
#png('pairs_10.png', res = 300, units = 'in', width = 10, height = 10)
#mcmc_pairs(out_sCh10_s400$chain[,1:10])
#dev.off()

#  Trace plots with posterior density for all parameter values----
# chains should look similar, not get stuck
png('traceplot_10_1-4.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_s400$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_s400$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_s400$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_s400$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-19.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_s400$chain[,17:19], smooth = FALSE)
dev.off()

#mcmc_trace in bayesplot as alternative traceplots
# png('traceplot_10_1-9_b.png', res = 300, units = 'in', width = 7, height = 7)
# mcmc_trace(out_sCh10_s400$chain[,1:9])
# dev.off()


#  Gelman Diagnostics----
#Fixme: This requires cores to be setup to run in parallel
#png('gelmanplot_10.png', res = 300, units = 'in', width = 7, height = 7)
#gelmanDiagnostics(sampler = out_sCh10_s400, plot = TRUE)
#dev.off()
#png('diagnosticplot_10.png', res = 300, units = 'in', width = 7, height = 7)
#plotDiagnostic(out = out_sCh10_s400)
#dev.off()

#stopCluster(cl)

# Run convergence diagnostics----
#R-hat - Gelman-Rubin within and between chain variance for all variables - target less than 1.05 for all
#gelman.diag(out_sCh10_s400$chain[,1:42])
#Autocorrelation of chain for each parameter should drop quickly. This informs the amount of values to skip in MCMC
#Fixme: this function plots individual chains. Would be good to average the autocorrelation for each chain and plot with error bars.
png('AutocorrPlot_10_1-9.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_sCh10_s400$chain[,1:9], lag.max = 100, ask = FALSE)
dev.off()
png('AutocorrPlot_10_10-18.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_sCh10_s400$chain[,10:18], lag.max = 100, ask = FALSE)
dev.off()
png('AutocorrPlot_10_19.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_sCh10_s400$chain[,19], lag.max = 100, ask = FALSE)
dev.off()

#Burnin removed


#Stationarity of chain - Geweke
#Fixme: This is computed for individual chains. code to loop in first figure
# png('GewekePlot_10_1-9.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_sCh10_s400$chain[,1:9])
# coda::geweke.plot(as.mcmc(out_Ch10_s300$chain[[1]][150:300,1:9]))
# dev.off()
# png('GewekePlot_10_10-18.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_sCh10_s400$chain[,10:18])
# dev.off()
# png('GewekePlot_10_19-27.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_sCh10_s400$chain[,19:27])
# dev.off()
# png('GewekePlot_10_28-36.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_sCh10_s400$chain[,28:36])
# dev.off()
# png('GewekePlot_10_veg.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_sCh10_s400$chain[,34:42])
# dev.off()

#need at least 50 chain steps for this
gelman.plot(x = out_sCh10_s400$chain[,-c(20,21,22)], autoburnin = TRUE, ylim = c(0,10))

#Burning removed

#Parallel coordinate plot - including all parameters is a bit much.
#png('parcoord_10.png', res = 300, units = 'in', width = 10, height = 5)
#mcmc_parcoord(outParext$chain, transform = function(x) {(x - min(x)) / (max(x)-min(x))})
#dev.off()

#Density overlay (mcmc_dens_overlay) - all chains should have similar densities for each variable
png('densOverlay_10_s400.png', res = 300, units = 'in', width = 10, height = 10)
mcmc_dens_overlay(out_sCh10_s400$chain)
dev.off()

#Burnin Removed

#Rejection Rate
Reject_sCh10_s400 = coda::rejectionRate(out_sCh10_s400$chain)
#Burnin Removed

#Highest Posterior Density Interval
HPDs_Ch10 = coda::HPDinterval(out_Ch10_s600$chain)
HPDs_Ch10_bt = BayesianTools::getCredibleIntervals(out_Ch10_s600$chain[[1]])

#Effective sample size for each parameter should be similar
EffSize_sCh10 = coda::effectiveSize(out_sCh10_s400$chain)

# Accept/Reject rates for each chain----
AR_Ch10_s400 = read.table('AcceptReject_c_s400.txt', header=TRUE)

#Join into one
#AR_Ch10 = rbind(AR_Ch10_s400, AR_Ch10_s200, AR_Ch10_s300, AR_Ch10_s400, AR_Ch10_s500, AR_Ch10_s600)
#rm(AR_Ch10_s400, AR_Ch10_s200, AR_Ch10_s300, AR_Ch10_s400, AR_Ch10_s500, AR_Ch10_s600)

ARrates_Ch10 = vector('numeric', length = ncol(AR_Ch10))
for (i in 1:ncol(AR_Ch10)){
  ARrates_Ch10[i] = sum(AR_Ch10[,i])/nrow(AR_Ch10)
}

png('AcceptRejectHist.png', res = 200, height=5, width=5, units='in')
hist(ARrates_Ch10, xlim=c(0,1))
dev.off()

#10 Chains, New CR Probabilities----
# Random seed----
set.seed(7256)
# Load Files----
setwd(dir_sCh10n)
load(file = paste0(dir_sCh10n, '/OutputWorkspace-10Ch_s400.RData'), verbose = FALSE)
rm(cl, Info, i, arg, startValues, tic_Script)
out_sCh10_s400n = out_Parext
rm(out_Parext)
load(file = paste0(dir_sCh10n, '/OutputWorkspace-10Ch_s1200.RData'), verbose = FALSE)
rm(cl, Info, i, arg, startValues, tic_Script)
out_sCh10_s1200n = out_Parext_Restart
rm(out_Parext_Restart, temp)

#  Remove the burnin----
#out_Ch10_s500$burnrm = out_Ch10_s500$chain[201:500,]
#class(out_Ch10_s500$burnrm) = 'mcmc.list'

# Plot and summarize output----
#Fixme: this requires cores to be setup to run in parallel, with all of the exported variables.
#summary(out_Parext)

#  Marginals - Useful to see the prior and posterior on same plot.----
PriorSample = prior$sampler(n = 50000)
png('marginalplot_10_atm_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_sCh10_s400n$chain[,c(12,2)], prior = PriorSample[,c(12,2)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_sCh10_s400n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], prior = PriorSample[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s400.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_sCh10_s400n$chain[,c(1, 16:19)], prior = PriorSample[,c(1, 16:19)], singlePanel = FALSE)
dev.off()

png('marginalplot_10_atm_s1200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_sCh10_s1200n$chain[,c(12,2)], prior = PriorSample[,c(12,2)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s1200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_sCh10_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], prior = PriorSample[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s1200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_sCh10_s1200n$chain[,c(1, 16:19)], prior = PriorSample[,c(1, 16:19)], singlePanel = FALSE)
dev.off()

#Only for last 50 chain steps

#Removing the first 200 (burnin)
png('marginalplot_10_atm_s1200_burnrm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_sCh10_s1200n$chain[-seq(1,200,1),c(12,2)], prior = PriorSample[,c(12,2)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s1200_burnrm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_sCh10_s1200n$chain[-seq(1,200,1),c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], prior = PriorSample[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s1200_burnrm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_sCh10_s1200n$chain[-seq(1,200,1),c(1, 16:19)], prior = PriorSample[,c(1, 16:19)], singlePanel = FALSE)
dev.off()

#  scatterplot matrix----
#Not sure how is best to view this scatterplot matrix because there are too many parameters to view in one grid.
png('corplot_10_atm_s1200.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_sCh10_s1200n$chain, scaleCorText = TRUE, whichParameters = c(12,2))
dev.off()
png('corplot_10_29_s1200.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_sCh10_s1200n$chain, scaleCorText = TRUE, whichParameters = c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14))
dev.off()
png('corplot_10_veg_s1200.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_sCh10_s1200n$chain, scaleCorText = TRUE, whichParameters = c(1, 16:19))
dev.off()
#Most sensitive parameters
#png('corplot_10_sens_s1200.png', res = 300, units = 'in', width = 14, height = 14)
#correlationPlot(mat = out_sCh10_s1200n$chain, scaleCorText = TRUE, whichParameters = c(34:42, 1,33))
#dev.off()

#mcmc_pairs in bayesplot is alternative method
#png('pairs_10.png', res = 300, units = 'in', width = 10, height = 10)
#mcmc_pairs(out_sCh10_s400n$chain[,1:10])
#dev.off()

#  Trace plots with posterior density for all parameter values----
# chains should look similar, not get stuck
png('traceplot_10_1-4.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_s400n$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_s400n$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_s400n$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_s400n$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-19.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_s400n$chain[,17:19], smooth = FALSE)
dev.off()

png('traceplot_10_1-4_s1200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_s1200n$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8_s1200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_s1200n$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12_s1200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_s1200n$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16_s1200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_s1200n$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-19_s1200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_s1200n$chain[,17:19], smooth = FALSE)
dev.off()

#mcmc_trace in bayesplot as alternative traceplots
# png('traceplot_10_1-9_b.png', res = 300, units = 'in', width = 7, height = 7)
# mcmc_trace(out_sCh10_s400n$chain[,1:9])
# dev.off()

#  Gelman Diagnostics----
#Fixme: This requires cores to be setup to run in parallel
#png('gelmanplot_10.png', res = 300, units = 'in', width = 7, height = 7)
#gelmanDiagnostics(sampler = out_sCh10_s400n, plot = TRUE)
#dev.off()
#png('diagnosticplot_10.png', res = 300, units = 'in', width = 7, height = 7)
#plotDiagnostic(out = out_sCh10_s400n)
#dev.off()

#stopCluster(cl)

# Run convergence diagnostics----
#R-hat - Gelman-Rubin within and between chain variance for all variables - target less than 1.05 for all
#gelman.diag(out_sCh10_s400n$chain[,1:42])
#Autocorrelation of chain for each parameter should drop quickly. This informs the amount of values to skip in MCMC
#Fixme: this function plots individual chains. Would be good to average the autocorrelation for each chain and plot with error bars.
png('AutocorrPlot_10_1-9.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_sCh10_s400n$chain[,1:9], lag.max = 100, ask = FALSE)
dev.off()
png('AutocorrPlot_10_10-18.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_sCh10_s400n$chain[,10:18], lag.max = 100, ask = FALSE)
dev.off()
png('AutocorrPlot_10_19.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_sCh10_s400n$chain[,19], lag.max = 100, ask = FALSE)
dev.off()

png('AutocorrPlot_10_1-9_s1200.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_sCh10_s1200n$chain[,1:9], lag.max = 100, ask = FALSE)
dev.off()
png('AutocorrPlot_10_10-18_s1200.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_sCh10_s1200n$chain[,10:18], lag.max = 100, ask = FALSE)
dev.off()
png('AutocorrPlot_10_19_s1200.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_sCh10_s1200n$chain[,19], lag.max = 100, ask = FALSE)
dev.off()

#Burnin removed

#Stationarity of chain - Geweke
#Fixme: This is computed for individual chains. code to loop in first figure
# png('GewekePlot_10_1-9.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_sCh10_s400n$chain[,1:9])
# coda::geweke.plot(as.mcmc(out_Ch10_s300$chain[[1]][150:300,1:9]))
# dev.off()
# png('GewekePlot_10_10-18.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_sCh10_s400n$chain[,10:18])
# dev.off()
# png('GewekePlot_10_19-27.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_sCh10_s400n$chain[,19:27])
# dev.off()
# png('GewekePlot_10_28-36.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_sCh10_s400n$chain[,28:36])
# dev.off()
# png('GewekePlot_10_veg.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_sCh10_s400n$chain[,34:42])
# dev.off()

#need at least 50 chain steps for this
gelman.plot(x = out_sCh10_s400n$chain[,-c(20,21,22)], autoburnin = TRUE, ylim = c(0,10))

gelman.plot(x = out_sCh10_s1200n$chain[,-c(20,21,22)], autoburnin = TRUE, ylim = c(0,10))

#Burning removed

#Parallel coordinate plot - including all parameters is a bit much.
#png('parcoord_10.png', res = 300, units = 'in', width = 10, height = 5)
#mcmc_parcoord(outParext$chain, transform = function(x) {(x - min(x)) / (max(x)-min(x))})
#dev.off()

#Density overlay (mcmc_dens_overlay) - all chains should have similar densities for each variable
png('densOverlay_10_s400.png', res = 300, units = 'in', width = 10, height = 10)
mcmc_dens_overlay(out_sCh10_s400n$chain)
dev.off()

png('densOverlay_10_s1200.png', res = 300, units = 'in', width = 10, height = 10)
mcmc_dens_overlay(out_sCh10_s1200n$chain)
dev.off()

#Burnin Removed

#Rejection Rate
Reject_sCh10_s400 = coda::rejectionRate(out_sCh10_s400n$chain)
Reject_sCh10_s1200 = coda::rejectionRate(out_sCh10_s1200n$chain)
#Burnin Removed

#Highest Posterior Density Interval
HPDs_Ch10 = coda::HPDinterval(out_sCh10_s1200n$chain)
HPDs_Ch10_bt = BayesianTools::getCredibleIntervals(out_sCh10_s1200n$chain[[1]])

#Effective sample size for each parameter should be similar
EffSize_sCh10 = coda::effectiveSize(out_sCh10_s400n$chain)
EffSize_sCh10_s1200 = coda::effectiveSize(out_sCh10_s1200n$chain)

# Accept/Reject rates for each chain----
#AR_Ch10_s400 = read.table('AcceptReject_c_s400.txt', header=TRUE)

#Join into one
#AR_Ch10 = rbind(AR_Ch10_s400, AR_Ch10_s200, AR_Ch10_s300, AR_Ch10_s400, AR_Ch10_s500, AR_Ch10_s600)
#rm(AR_Ch10_s400, AR_Ch10_s200, AR_Ch10_s300, AR_Ch10_s400, AR_Ch10_s500, AR_Ch10_s600)

#ARrates_Ch10 = vector('numeric', length = ncol(AR_Ch10))
#for (i in 1:ncol(AR_Ch10)){
#  ARrates_Ch10[i] = sum(AR_Ch10[,i])/nrow(AR_Ch10)
#}

#png('AcceptRejectHist.png', res = 200, height=5, width=5, units='in')
#hist(ARrates_Ch10, xlim=c(0,1))
#dev.off()

#10 Chains - 1, New CR Probabilities----
# Random seed----
set.seed(7257)
# Load Files----
setwd(dir_sCh10_1n)
load(file = paste0(dir_sCh10_1n, '/OutputWorkspace-10Ch-1_s1200.RData'), verbose = FALSE)
rm(cl, Info, i, arg, startValues, tic_Script)
out_sCh10_1_s1200n = out_Parext_Restart
rm(out_Parext_Restart, temp)

#  Remove the burnin----
#out_Ch10_s500$burnrm = out_Ch10_s500$chain[201:500,]
#class(out_Ch10_s500$burnrm) = 'mcmc.list'

# Plot and summarize output----
#Fixme: this requires cores to be setup to run in parallel, with all of the exported variables.
#summary(out_Parext)

#  Marginals - Useful to see the prior and posterior on same plot.----
PriorSample = prior$sampler(n = 50000)
png('marginalplot_10_atm_s1200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_sCh10_1_s1200n$chain[,c(12,2)], prior = PriorSample[,c(12,2)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s1200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_sCh10_1_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], prior = PriorSample[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s1200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_sCh10_1_s1200n$chain[,c(1, 16:19)], prior = PriorSample[,c(1, 16:19)], singlePanel = FALSE)
dev.off()

#Only for last 50 chain steps

#Removing the first 200 (burnin)

#  scatterplot matrix----
#Not sure how is best to view this scatterplot matrix because there are too many parameters to view in one grid.
png('corplot_10_atm_s1200.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_sCh10_1_s1200n$chain, scaleCorText = TRUE, whichParameters = c(12,2))
dev.off()
png('corplot_10_29_s1200.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_sCh10_1_s1200n$chain, scaleCorText = TRUE, whichParameters = c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14))
dev.off()
png('corplot_10_veg_s1200.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_sCh10_1_s1200n$chain, scaleCorText = TRUE, whichParameters = c(1, 16:19))
dev.off()
#Most sensitive parameters
#png('corplot_10_sens_s1200.png', res = 300, units = 'in', width = 14, height = 14)
#correlationPlot(mat = out_sCh10_1_s1200n$chain, scaleCorText = TRUE, whichParameters = c(34:42, 1,33))
#dev.off()

#mcmc_pairs in bayesplot is alternative method

#  Trace plots with posterior density for all parameter values----
# chains should look similar, not get stuck
png('traceplot_10_1-4_s1200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_1_s1200n$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8_s1200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_1_s1200n$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12_s1200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_1_s1200n$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16_s1200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_1_s1200n$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-19_s1200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_1_s1200n$chain[,17:19], smooth = FALSE)
dev.off()

#mcmc_trace in bayesplot as alternative traceplots

#  Gelman Diagnostics----

# Run convergence diagnostics----
#R-hat - Gelman-Rubin within and between chain variance for all variables - target less than 1.05 for all
#gelman.diag(out_sCh10_1_s400n$chain[,1:42])
#Autocorrelation of chain for each parameter should drop quickly. This informs the amount of values to skip in MCMC
#Fixme: this function plots individual chains. Would be good to average the autocorrelation for each chain and plot with error bars.
png('AutocorrPlot_10_1-9_s1200.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_sCh10_1_s1200n$chain[,1:9], lag.max = 100, ask = FALSE)
dev.off()
png('AutocorrPlot_10_10-18_s1200.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_sCh10_1_s1200n$chain[,10:18], lag.max = 100, ask = FALSE)
dev.off()
png('AutocorrPlot_10_19_s1200.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_sCh10_1_s1200n$chain[,19], lag.max = 100, ask = FALSE)
dev.off()

#Burnin removed

#Stationarity of chain - Geweke

#need at least 50 chain steps for this
gelman.plot(x = out_sCh10_1_s1200n$chain[,-c(20,21,22)], autoburnin = TRUE, ylim = c(0,10))

#Burning removed

#Parallel coordinate plot - including all parameters is a bit much.
#png('parcoord_10.png', res = 300, units = 'in', width = 10, height = 5)
#mcmc_parcoord(outParext$chain, transform = function(x) {(x - min(x)) / (max(x)-min(x))})
#dev.off()

#Density overlay (mcmc_dens_overlay) - all chains should have similar densities for each variable
png('densOverlay_10_s1200.png', res = 300, units = 'in', width = 10, height = 10)
mcmc_dens_overlay(out_sCh10_1_s1200n$chain)
dev.off()

#Burnin Removed

#Rejection Rate
Reject_sCh10_1_s1200 = coda::rejectionRate(out_sCh10_1_s1200n$chain)
#Burnin Removed

#Highest Posterior Density Interval
HPDs_Ch10_1 = coda::HPDinterval(out_sCh10_1_s1200n$chain)
HPDs_Ch10_1_bt = BayesianTools::getCredibleIntervals(out_sCh10_1_s1200n$chain[[1]])

#Effective sample size for each parameter should be similar
EffSize_sCh10_1_s1200 = coda::effectiveSize(out_sCh10_1_s1200n$chain)

# Accept/Reject rates for each chain----

#10 Chains - 2, New CR Probabilities----
# Random seed----
set.seed(7258)
# Load Files----
setwd(dir_sCh10_2n)
load(file = paste0(dir_sCh10_2n, '/OutputWorkspace-10Ch-2_s1200.RData'), verbose = FALSE)
rm(cl, Info, i, arg, startValues, tic_Script)
out_sCh10_2_s1200n = out_Parext_Restart
rm(out_Parext_Restart, temp)

#  Remove the burnin----

# Plot and summarize output----
#Fixme: this requires cores to be setup to run in parallel, with all of the exported variables.
#summary(out_Parext)

#  Marginals - Useful to see the prior and posterior on same plot.----
PriorSample = prior$sampler(n = 50000)
png('marginalplot_10_atm_s1200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_sCh10_2_s1200n$chain[,c(12,2)], prior = PriorSample[,c(12,2)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s1200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_sCh10_2_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], prior = PriorSample[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s1200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_sCh10_2_s1200n$chain[,c(1, 16:19)], prior = PriorSample[,c(1, 16:19)], singlePanel = FALSE)
dev.off()

#Only for last 50 chain steps

#Removing the first 200 (burnin)

#  scatterplot matrix----
#Not sure how is best to view this scatterplot matrix because there are too many parameters to view in one grid.
png('corplot_10_atm_s1200.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_sCh10_2_s1200n$chain, scaleCorText = TRUE, whichParameters = c(12,2))
dev.off()
png('corplot_10_29_s1200.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_sCh10_2_s1200n$chain, scaleCorText = TRUE, whichParameters = c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14))
dev.off()
png('corplot_10_veg_s1200.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_sCh10_2_s1200n$chain, scaleCorText = TRUE, whichParameters = c(1, 16:19))
dev.off()
#Most sensitive parameters
#png('corplot_10_sens_s1200.png', res = 300, units = 'in', width = 14, height = 14)
#correlationPlot(mat = out_sCh10_2_s1200n$chain, scaleCorText = TRUE, whichParameters = c(34:42, 1,33))
#dev.off()

#mcmc_pairs in bayesplot is alternative method

#  Trace plots with posterior density for all parameter values----
# chains should look similar, not get stuck
png('traceplot_10_1-4_s1200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_2_s1200n$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8_s1200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_2_s1200n$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12_s1200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_2_s1200n$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16_s1200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_2_s1200n$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-19_s1200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_2_s1200n$chain[,17:19], smooth = FALSE)
dev.off()

#mcmc_trace in bayesplot as alternative traceplots

#  Gelman Diagnostics----

# Run convergence diagnostics----
#R-hat - Gelman-Rubin within and between chain variance for all variables - target less than 1.05 for all
#gelman.diag(out_sCh10_2_s400n$chain[,1:42])
#Autocorrelation of chain for each parameter should drop quickly. This informs the amount of values to skip in MCMC
#Fixme: this function plots individual chains. Would be good to average the autocorrelation for each chain and plot with error bars.
png('AutocorrPlot_10_1-9_s1200.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_sCh10_2_s1200n$chain[,1:9], lag.max = 100, ask = FALSE)
dev.off()
png('AutocorrPlot_10_10-18_s1200.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_sCh10_2_s1200n$chain[,10:18], lag.max = 100, ask = FALSE)
dev.off()
png('AutocorrPlot_10_19_s1200.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_sCh10_2_s1200n$chain[,19], lag.max = 100, ask = FALSE)
dev.off()

#Burnin removed

#Stationarity of chain - Geweke

#need at least 50 chain steps for this
gelman.plot(x = out_sCh10_2_s1200n$chain[,-c(20,21,22)], autoburnin = TRUE, ylim = c(0,10))

#Burning removed

#Parallel coordinate plot - including all parameters is a bit much.
#png('parcoord_10.png', res = 300, units = 'in', width = 10, height = 5)
#mcmc_parcoord(outParext$chain, transform = function(x) {(x - min(x)) / (max(x)-min(x))})
#dev.off()

#Density overlay (mcmc_dens_overlay) - all chains should have similar densities for each variable
png('densOverlay_10_s1200.png', res = 300, units = 'in', width = 10, height = 10)
mcmc_dens_overlay(out_sCh10_2_s1200n$chain)
dev.off()

#Burnin Removed

#Rejection Rate
Reject_sCh10_2_s1200 = coda::rejectionRate(out_sCh10_2_s1200n$chain)
#Burnin Removed

#Highest Posterior Density Interval
HPDs_Ch10_2 = coda::HPDinterval(out_sCh10_2_s1200n$chain)
HPDs_Ch10_2_bt = BayesianTools::getCredibleIntervals(out_sCh10_2_s1200n$chain[[1]])

#Effective sample size for each parameter should be similar
EffSize_sCh10_2_s1200 = coda::effectiveSize(out_sCh10_2_s1200n$chain)

# Accept/Reject rates for each chain----

#10 Chains - 3 New CR Probabilities----
# Random seed----
set.seed(7259)
# Load Files----
setwd(dir_sCh10_3n)
load(file = paste0(dir_sCh10_3n, '/OutputWorkspace-10Ch-3_s1200.RData'), verbose = FALSE)
rm(cl, Info, i, arg, startValues, tic_Script)
out_sCh10_3_s1200n = out_Parext_Restart
rm(out_Parext_Restart, temp)

#  Remove the burnin----

# Plot and summarize output----
#Fixme: this requires cores to be setup to run in parallel, with all of the exported variables.
#summary(out_Parext)

#  Marginals - Useful to see the prior and posterior on same plot.----
PriorSample = prior$sampler(n = 50000)
png('marginalplot_10_atm_s1200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_sCh10_3_s1200n$chain[,c(12,2)], prior = PriorSample[,c(12,2)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_s9_s1200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_sCh10_3_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], prior = PriorSample[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], singlePanel = FALSE)
dev.off()
png('marginalplot_10_veg_s1200.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_sCh10_3_s1200n$chain[,c(1, 16:19)], prior = PriorSample[,c(1, 16:19)], singlePanel = FALSE)
dev.off()

#Only for last 50 chain steps

#Removing the first 200 (burnin)

#  scatterplot matrix----
#Not sure how is best to view this scatterplot matrix because there are too many parameters to view in one grid.
png('corplot_10_atm_s1200.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_sCh10_3_s1200n$chain, scaleCorText = TRUE, whichParameters = c(12,2))
dev.off()
png('corplot_10_29_s1200.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_sCh10_3_s1200n$chain, scaleCorText = TRUE, whichParameters = c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14))
dev.off()
png('corplot_10_veg_s1200.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_sCh10_3_s1200n$chain, scaleCorText = TRUE, whichParameters = c(1, 16:19))
dev.off()
#Most sensitive parameters
#png('corplot_10_sens_s1200.png', res = 300, units = 'in', width = 14, height = 14)
#correlationPlot(mat = out_sCh10_3_s1200n$chain, scaleCorText = TRUE, whichParameters = c(34:42, 1,33))
#dev.off()

#mcmc_pairs in bayesplot is alternative method

#  Trace plots with posterior density for all parameter values----
# chains should look similar, not get stuck
png('traceplot_10_1-4_s1200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_3_s1200n$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8_s1200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_3_s1200n$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12_s1200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_3_s1200n$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16_s1200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_3_s1200n$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-19_s1200.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_sCh10_3_s1200n$chain[,17:19], smooth = FALSE)
dev.off()

#mcmc_trace in bayesplot as alternative traceplots

#  Gelman Diagnostics----

# Run convergence diagnostics----
#R-hat - Gelman-Rubin within and between chain variance for all variables - target less than 1.05 for all
#gelman.diag(out_sCh10_3_s400n$chain[,1:42])
#Autocorrelation of chain for each parameter should drop quickly. This informs the amount of values to skip in MCMC
#Fixme: this function plots individual chains. Would be good to average the autocorrelation for each chain and plot with error bars.
png('AutocorrPlot_10_1-9_s1200.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_sCh10_3_s1200n$chain[,1:9], lag.max = 100, ask = FALSE)
dev.off()
png('AutocorrPlot_10_10-18_s1200.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_sCh10_3_s1200n$chain[,10:18], lag.max = 100, ask = FALSE)
dev.off()
png('AutocorrPlot_10_19_s1200.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_sCh10_3_s1200n$chain[,19], lag.max = 100, ask = FALSE)
dev.off()

#Burnin removed

#Stationarity of chain - Geweke

#need at least 50 chain steps for this
gelman.plot(x = out_sCh10_3_s1200n$chain[,-c(20,21,22)], autoburnin = TRUE, ylim = c(0,10))

#Burning removed

#Parallel coordinate plot - including all parameters is a bit much.
#png('parcoord_10.png', res = 300, units = 'in', width = 10, height = 5)
#mcmc_parcoord(outParext$chain, transform = function(x) {(x - min(x)) / (max(x)-min(x))})
#dev.off()

#Density overlay (mcmc_dens_overlay) - all chains should have similar densities for each variable
png('densOverlay_10_s1200.png', res = 300, units = 'in', width = 10, height = 10)
mcmc_dens_overlay(out_sCh10_3_s1200n$chain)
dev.off()

#Burnin Removed

#Rejection Rate
Reject_sCh10_3_s1200 = coda::rejectionRate(out_sCh10_3_s1200n$chain)
#Burnin Removed

#Highest Posterior Density Interval
HPDs_Ch10_3 = coda::HPDinterval(out_sCh10_3_s1200n$chain)
HPDs_Ch10_3_bt = BayesianTools::getCredibleIntervals(out_sCh10_3_s1200n$chain[[1]])

#Effective sample size for each parameter should be similar
EffSize_sCh10_3_s1200 = coda::effectiveSize(out_sCh10_3_s1200n$chain)

# Accept/Reject rates for each chain----

#Combined chain marginals----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration")
png('marginalplot_10_atm_s1200_c.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(12,2)], out_sCh10_2_s1200n$chain[,c(12,2)], out_sCh10_1_s1200n$chain[,c(12,2)], out_sCh10_s1200n$chain[,c(12,2)]), prior = PriorSample[,c(12,2)], singlePanel = FALSE, trueVals = SynParams[c(12,2)])
dev.off()
png('marginalplot_10_s9_s1200_c.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_2_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_1_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)]), prior = PriorSample[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], singlePanel = FALSE, trueVals = SynParams[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)])
dev.off()
png('marginalplot_10_veg_s1200_c.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(1, 16:19)], out_sCh10_2_s1200n$chain[,c(1, 16:19)], out_sCh10_1_s1200n$chain[,c(1, 16:19)], out_sCh10_s1200n$chain[,c(1, 16:19)]), prior = PriorSample[,c(1, 16:19)], singlePanel = FALSE, trueVals = SynParams[c(1, 16:19)])
dev.off()

#Compute MAP----
out_sCh10_s1200nc = as.data.frame(mergeChains(out_sCh10_s1200n$chain))
out_sCh10_1_s1200nc = as.data.frame(mergeChains(out_sCh10_1_s1200n$chain))
out_sCh10_2_s1200nc = as.data.frame(mergeChains(out_sCh10_2_s1200n$chain))
out_sCh10_3_s1200nc = as.data.frame(mergeChains(out_sCh10_3_s1200n$chain))

max(out_sCh10_s1200nc$LP)
max(out_sCh10_1_s1200nc$LP)
max(out_sCh10_2_s1200nc$LP)
max(out_sCh10_3_s1200nc$LP)

#10Ch-3, Chain2, replicate 997
which(out_sCh10_3_s1200nc$LP == max(out_sCh10_3_s1200nc$LP))[1]-1200

#Extract the maximum likelihood value----
#Returns same value as above approach for MAP.
#Loop over datasets and chains
MaxLikes = matrix(NA, nrow = 40, ncol = ncol(out_sCh10_1_s1200n$chain[[1]])+3)
for (ch in 1:10){
  MaxLikes[ch,1] = 0
  MaxLikes[ch,2] = ch
  MaxLikes[ch,3] = which(out_sCh10_s1200n$chain[[ch]][,20] == max(out_sCh10_s1200n$chain[[ch]][,20]))[1]
  MaxLikes[ch,-c(1,2,3)] = out_sCh10_s1200n$chain[[ch]][which(out_sCh10_s1200n$chain[[ch]][,20] == max(out_sCh10_s1200n$chain[[ch]][,20]))[1],]
}
for (ch in 11:20){
  MaxLikes[ch,1] = 1
  MaxLikes[ch,2] = ch-10
  MaxLikes[ch,3] = which(out_sCh10_1_s1200n$chain[[ch-10]][,20] == max(out_sCh10_1_s1200n$chain[[ch-10]][,20]))[1]
  MaxLikes[ch,-c(1,2,3)] = out_sCh10_1_s1200n$chain[[ch-10]][which(out_sCh10_1_s1200n$chain[[ch-10]][,20] == max(out_sCh10_1_s1200n$chain[[ch-10]][,20]))[1],]
}
for (ch in 21:30){
  MaxLikes[ch,1] = 2
  MaxLikes[ch,2] = ch-20
  MaxLikes[ch,3] = which(out_sCh10_2_s1200n$chain[[ch-20]][,20] == max(out_sCh10_2_s1200n$chain[[ch-20]][,20]))[1]
  MaxLikes[ch,-c(1,2,3)] = out_sCh10_2_s1200n$chain[[ch-20]][which(out_sCh10_2_s1200n$chain[[ch-20]][,20] == max(out_sCh10_2_s1200n$chain[[ch-20]][,20]))[1],]
}
for (ch in 31:40){
  MaxLikes[ch,1] = 3
  MaxLikes[ch,2] = ch-30
  MaxLikes[ch,3] = which(out_sCh10_3_s1200n$chain[[ch-30]][,20] == max(out_sCh10_3_s1200n$chain[[ch-30]][,20]))[1]
  MaxLikes[ch,-c(1,2,3)] = out_sCh10_3_s1200n$chain[[ch-30]][which(out_sCh10_3_s1200n$chain[[ch-30]][,20] == max(out_sCh10_3_s1200n$chain[[ch-30]][,20]))[1],]
}

#10Ch-3, Chain2, replicate 997
MaxLikes[which(MaxLikes[,23] == max(MaxLikes[,23])),]

#Select the 19 parameter sets to use for MORO optimization----
#Load the files with LNSE and other efficiency metrics - THESE ARE NOT MCMC CHAINS!----
Eff_Ch10 = read.csv("C:/Users/js4yd/OneDrive - University of Virginia/BES_Data/BES_Data/RHESSysFiles/BR&POBR/Calibration/SynCh10/NewCR/ParamsLikes_c_s400.csv", stringsAsFactors = FALSE)
Eff_Ch10 = rbind(Eff_Ch10, read.csv("C:/Users/js4yd/OneDrive - University of Virginia/BES_Data/BES_Data/RHESSysFiles/BR&POBR/Calibration/SynCh10/NewCR/ParamsLikes_c_s800.csv", stringsAsFactors = FALSE))
Eff_Ch10 = rbind(Eff_Ch10, read.csv("C:/Users/js4yd/OneDrive - University of Virginia/BES_Data/BES_Data/RHESSysFiles/BR&POBR/Calibration/SynCh10/NewCR/ParamsLikes_c_s1200.csv", stringsAsFactors = FALSE))
Eff_Ch10_1 = read.csv("C:/Users/js4yd/OneDrive - University of Virginia/BES_Data/BES_Data/RHESSysFiles/BR&POBR/Calibration/SynCh10-1/NewCR/ParamsLikes_c_s400.csv", stringsAsFactors = FALSE)
Eff_Ch10_1 = rbind(Eff_Ch10_1, read.csv("C:/Users/js4yd/OneDrive - University of Virginia/BES_Data/BES_Data/RHESSysFiles/BR&POBR/Calibration/SynCh10-1/NewCR/ParamsLikes_c_s800.csv", stringsAsFactors = FALSE))
Eff_Ch10_1 = rbind(Eff_Ch10_1, read.csv("C:/Users/js4yd/OneDrive - University of Virginia/BES_Data/BES_Data/RHESSysFiles/BR&POBR/Calibration/SynCh10-1/NewCR/ParamsLikes_c_s1200.csv", stringsAsFactors = FALSE))
Eff_Ch10_2 = read.csv("C:/Users/js4yd/OneDrive - University of Virginia/BES_Data/BES_Data/RHESSysFiles/BR&POBR/Calibration/SynCh10-2/NewCR/ParamsLikes_c_s400.csv", stringsAsFactors = FALSE)
Eff_Ch10_2 = rbind(Eff_Ch10_2, read.csv("C:/Users/js4yd/OneDrive - University of Virginia/BES_Data/BES_Data/RHESSysFiles/BR&POBR/Calibration/SynCh10-2/NewCR/ParamsLikes_c_s800.csv", stringsAsFactors = FALSE))
Eff_Ch10_2 = rbind(Eff_Ch10_2, read.csv("C:/Users/js4yd/OneDrive - University of Virginia/BES_Data/BES_Data/RHESSysFiles/BR&POBR/Calibration/SynCh10-2/NewCR/ParamsLikes_c_s1200.csv", stringsAsFactors = FALSE))
Eff_Ch10_3 = read.csv("C:/Users/js4yd/OneDrive - University of Virginia/BES_Data/BES_Data/RHESSysFiles/BR&POBR/Calibration/SynCh10-3/NewCR/ParamsLikes_c_s400.csv", stringsAsFactors = FALSE)
Eff_Ch10_3 = rbind(Eff_Ch10_3, read.csv("C:/Users/js4yd/OneDrive - University of Virginia/BES_Data/BES_Data/RHESSysFiles/BR&POBR/Calibration/SynCh10-3/NewCR/ParamsLikes_c_s800.csv", stringsAsFactors = FALSE))
Eff_Ch10_3 = rbind(Eff_Ch10_3, read.csv("C:/Users/js4yd/OneDrive - University of Virginia/BES_Data/BES_Data/RHESSysFiles/BR&POBR/Calibration/SynCh10-3/NewCR/ParamsLikes_c_s1200.csv", stringsAsFactors = FALSE))

#Join the metrics to the MCMC chains----
#Add columns to chain datasets
out_sCh10_s1200nc$ID = out_sCh10_s1200nc$Dataset = out_sCh10_s1200nc$Replicate = out_sCh10_s1200nc$Chain = NA
out_sCh10_s1200nc$beta = out_sCh10_s1200nc$xi = out_sCh10_s1200nc$sigma_0 = out_sCh10_s1200nc$sigma_1 = out_sCh10_s1200nc$phi_1 = out_sCh10_s1200nc$mu_h = NA
out_sCh10_s1200nc$SSEd = out_sCh10_s1200nc$SSEw = out_sCh10_s1200nc$SSEm = out_sCh10_s1200nc$SSEa = NA
out_sCh10_s1200nc$NSEd = out_sCh10_s1200nc$NSEw = out_sCh10_s1200nc$NSEm = out_sCh10_s1200nc$NSEa = NA
out_sCh10_s1200nc$LNSEd = out_sCh10_s1200nc$LNSEw = out_sCh10_s1200nc$LNSEm = out_sCh10_s1200nc$LNSEa = out_sCh10_s1200nc$pBias = NA
out_sCh10_s1200nc$success = NA
colnames(out_sCh10_s1200nc)[23:46] = c('ID','Dataset','Replicate','Chain','beta','xi','sigma_0','sigma_1','phi_1','mu_h','SSEd','SSEw','SSEm','SSEa','NSEd','NSEw','NSEm','NSEa','LNSEd','LNSEw','LNSEm','LNSEa','pBias','success')

out_sCh10_1_s1200nc$ID = out_sCh10_1_s1200nc$Dataset = out_sCh10_1_s1200nc$Replicate = out_sCh10_1_s1200nc$Chain = NA
out_sCh10_1_s1200nc$beta = out_sCh10_1_s1200nc$xi = out_sCh10_1_s1200nc$sigma_0 = out_sCh10_1_s1200nc$sigma_1 = out_sCh10_1_s1200nc$phi_1 = out_sCh10_1_s1200nc$mu_h = NA
out_sCh10_1_s1200nc$SSEd = out_sCh10_1_s1200nc$SSEw = out_sCh10_1_s1200nc$SSEm = out_sCh10_1_s1200nc$SSEa = NA
out_sCh10_1_s1200nc$NSEd = out_sCh10_1_s1200nc$NSEw = out_sCh10_1_s1200nc$NSEm = out_sCh10_1_s1200nc$NSEa = NA
out_sCh10_1_s1200nc$LNSEd = out_sCh10_1_s1200nc$LNSEw = out_sCh10_1_s1200nc$LNSEm = out_sCh10_1_s1200nc$LNSEa = out_sCh10_1_s1200nc$pBias = NA
out_sCh10_1_s1200nc$success = NA
colnames(out_sCh10_1_s1200nc)[23:46] = c('ID','Dataset','Replicate','Chain','beta','xi','sigma_0','sigma_1','phi_1','mu_h','SSEd','SSEw','SSEm','SSEa','NSEd','NSEw','NSEm','NSEa','LNSEd','LNSEw','LNSEm','LNSEa','pBias','success')

out_sCh10_2_s1200nc$ID = out_sCh10_2_s1200nc$Dataset = out_sCh10_2_s1200nc$Replicate = out_sCh10_2_s1200nc$Chain = NA
out_sCh10_2_s1200nc$beta = out_sCh10_2_s1200nc$xi = out_sCh10_2_s1200nc$sigma_0 = out_sCh10_2_s1200nc$sigma_1 = out_sCh10_2_s1200nc$phi_1 = out_sCh10_2_s1200nc$mu_h = NA
out_sCh10_2_s1200nc$SSEd = out_sCh10_2_s1200nc$SSEw = out_sCh10_2_s1200nc$SSEm = out_sCh10_2_s1200nc$SSEa = NA
out_sCh10_2_s1200nc$NSEd = out_sCh10_2_s1200nc$NSEw = out_sCh10_2_s1200nc$NSEm = out_sCh10_2_s1200nc$NSEa = NA
out_sCh10_2_s1200nc$LNSEd = out_sCh10_2_s1200nc$LNSEw = out_sCh10_2_s1200nc$LNSEm = out_sCh10_2_s1200nc$LNSEa = out_sCh10_2_s1200nc$pBias = NA
out_sCh10_2_s1200nc$success = NA
colnames(out_sCh10_2_s1200nc)[23:46] = c('ID','Dataset','Replicate','Chain','beta','xi','sigma_0','sigma_1','phi_1','mu_h','SSEd','SSEw','SSEm','SSEa','NSEd','NSEw','NSEm','NSEa','LNSEd','LNSEw','LNSEm','LNSEa','pBias','success')

out_sCh10_3_s1200nc$ID = out_sCh10_3_s1200nc$Dataset = out_sCh10_3_s1200nc$Replicate = out_sCh10_3_s1200nc$Chain = NA
out_sCh10_3_s1200nc$beta = out_sCh10_3_s1200nc$xi = out_sCh10_3_s1200nc$sigma_0 = out_sCh10_3_s1200nc$sigma_1 = out_sCh10_3_s1200nc$phi_1 = out_sCh10_3_s1200nc$mu_h = NA
out_sCh10_3_s1200nc$SSEd = out_sCh10_3_s1200nc$SSEw = out_sCh10_3_s1200nc$SSEm = out_sCh10_3_s1200nc$SSEa = NA
out_sCh10_3_s1200nc$NSEd = out_sCh10_3_s1200nc$NSEw = out_sCh10_3_s1200nc$NSEm = out_sCh10_3_s1200nc$NSEa = NA
out_sCh10_3_s1200nc$LNSEd = out_sCh10_3_s1200nc$LNSEw = out_sCh10_3_s1200nc$LNSEm = out_sCh10_3_s1200nc$LNSEa = out_sCh10_3_s1200nc$pBias = NA
out_sCh10_3_s1200nc$success = NA
colnames(out_sCh10_3_s1200nc)[23:46] = c('ID','Dataset','Replicate','Chain','beta','xi','sigma_0','sigma_1','phi_1','mu_h','SSEd','SSEw','SSEm','SSEa','NSEd','NSEw','NSEm','NSEa','LNSEd','LNSEw','LNSEm','LNSEa','pBias','success')

#Loop over replicates
lchain = nrow(out_sCh10_s1200n$chain[[1]])
nchains = length(out_sCh10_s1200n$chain)
for (i in 1:lchain){
  #Order of out_sCh10 files is Chain 1 replicate 1-1200, Chains 2 replicate 1-1200, etc.
  #Order of metrics file is Replicate 1 Chain 1-10, Replicate 2 Chain 1-10, etc.
  if (i == 1){
    #This will always be accepted. Add metrics info to out files
    for (j in 1:nchains){
      out_sCh10_s1200nc[i+lchain*(j-1),23:46] = Eff_Ch10[j,c(1,2,3,4,26:31,33:46)]
      out_sCh10_1_s1200nc[i+lchain*(j-1),23:46] = Eff_Ch10_1[j,c(1,2,3,4,26:31,33:46)]
      out_sCh10_2_s1200nc[i+lchain*(j-1),23:46] = Eff_Ch10_2[j,c(1,2,3,4,26:31,33:46)]
      out_sCh10_3_s1200nc[i+lchain*(j-1),23:46] = Eff_Ch10_3[j,c(1,2,3,4,26:31,33:46)]
    }
  }else{
    #Loop over chains, and check if the value changed.
    for (j in 1:nchains){
      #10Ch
      if (any(out_sCh10_s1200n$chain[[j]][i,1:20] != out_sCh10_s1200n$chain[[j]][i-1,1:20])){
        #If it did, get the corresponding value in the metrics file
        out_sCh10_s1200nc[i+lchain*(j-1),23:46] = Eff_Ch10[j+nchains*(i-1),c(1,2,3,4,26:31,33:46)]
      }else{
        #If it didn't, copy the current metric info
        out_sCh10_s1200nc[i+lchain*(j-1),23:46] = out_sCh10_s1200nc[i+lchain*(j-1)-1,23:46]
      }
      #10Ch-1
      if (any(out_sCh10_1_s1200n$chain[[j]][i,1:20] != out_sCh10_1_s1200n$chain[[j]][i-1,1:20])){
        #If it did, get the corresponding value in the metrics file
        out_sCh10_1_s1200nc[i+lchain*(j-1),23:46] = Eff_Ch10_1[j+nchains*(i-1),c(1,2,3,4,26:31,33:46)]
      }else{
        #If it didn't, copy the current metric info
        out_sCh10_1_s1200nc[i+lchain*(j-1),23:46] = out_sCh10_1_s1200nc[i+lchain*(j-1)-1,23:46]
      }
      #10Ch-2
      if (any(out_sCh10_2_s1200n$chain[[j]][i,1:20] != out_sCh10_2_s1200n$chain[[j]][i-1,1:20])){
        #If it did, get the corresponding value in the metrics file
        out_sCh10_2_s1200nc[i+lchain*(j-1),23:46] = Eff_Ch10_2[j+nchains*(i-1),c(1,2,3,4,26:31,33:46)]
      }else{
        #If it didn't, copy the current metric info
        out_sCh10_2_s1200nc[i+lchain*(j-1),23:46] = out_sCh10_2_s1200nc[i+lchain*(j-1)-1,23:46]
      }
      #10Ch-3
      if (any(out_sCh10_3_s1200n$chain[[j]][i,1:20] != out_sCh10_3_s1200n$chain[[j]][i-1,1:20])){
        #If it did, get the corresponding value in the metrics file
        out_sCh10_3_s1200nc[i+lchain*(j-1),23:46] = Eff_Ch10_3[j+nchains*(i-1),c(1,2,3,4,26:31,33:46)]
      }else{
        #If it didn't, copy the current metric info
        out_sCh10_3_s1200nc[i+lchain*(j-1),23:46] = out_sCh10_3_s1200nc[i+lchain*(j-1)-1,23:46]
      }
    }
  }
}

#Normalize the parameters to 0-1----
for (i in 1:nrow(ParamRanges)){
  out_sCh10_s1200nc[,i] = (out_sCh10_s1200nc[,i] - ParamRanges$Lower[ParamRanges$NumberedParams == colnames(out_sCh10_s1200nc)[i]])/(ParamRanges$Upper[ParamRanges$NumberedParams == colnames(out_sCh10_s1200nc)[i]] - ParamRanges$Lower[ParamRanges$NumberedParams == colnames(out_sCh10_s1200nc)[i]])
  out_sCh10_1_s1200nc[,i] = (out_sCh10_1_s1200nc[,i] - ParamRanges$Lower[ParamRanges$NumberedParams == colnames(out_sCh10_1_s1200nc)[i]])/(ParamRanges$Upper[ParamRanges$NumberedParams == colnames(out_sCh10_1_s1200nc)[i]] - ParamRanges$Lower[ParamRanges$NumberedParams == colnames(out_sCh10_1_s1200nc)[i]])
  out_sCh10_2_s1200nc[,i] = (out_sCh10_2_s1200nc[,i] - ParamRanges$Lower[ParamRanges$NumberedParams == colnames(out_sCh10_2_s1200nc)[i]])/(ParamRanges$Upper[ParamRanges$NumberedParams == colnames(out_sCh10_2_s1200nc)[i]] - ParamRanges$Lower[ParamRanges$NumberedParams == colnames(out_sCh10_2_s1200nc)[i]])
  out_sCh10_3_s1200nc[,i] = (out_sCh10_3_s1200nc[,i] - ParamRanges$Lower[ParamRanges$NumberedParams == colnames(out_sCh10_3_s1200nc)[i]])/(ParamRanges$Upper[ParamRanges$NumberedParams == colnames(out_sCh10_3_s1200nc)[i]] - ParamRanges$Lower[ParamRanges$NumberedParams == colnames(out_sCh10_3_s1200nc)[i]])
  #Eff_Ch10[,4+i] = (Eff_Ch10[,4+i] - ParamRanges$Lower[ParamRanges$NumberedParams == colnames(Eff_Ch10)[4+i]])/(ParamRanges$Upper[ParamRanges$NumberedParams == colnames(Eff_Ch10)[4+i]] - ParamRanges$Lower[ParamRanges$NumberedParams == colnames(Eff_Ch10)[4+i]])
  #Eff_Ch10_1[,4+i] = (Eff_Ch10_1[,4+i] - ParamRanges$Lower[ParamRanges$NumberedParams == colnames(Eff_Ch10_1)[4+i]])/(ParamRanges$Upper[ParamRanges$NumberedParams == colnames(Eff_Ch10_1)[4+i]] - ParamRanges$Lower[ParamRanges$NumberedParams == colnames(Eff_Ch10_1)[4+i]])
  #Eff_Ch10_2[,4+i] = (Eff_Ch10_2[,4+i] - ParamRanges$Lower[ParamRanges$NumberedParams == colnames(Eff_Ch10_2)[4+i]])/(ParamRanges$Upper[ParamRanges$NumberedParams == colnames(Eff_Ch10_2)[4+i]] - ParamRanges$Lower[ParamRanges$NumberedParams == colnames(Eff_Ch10_2)[4+i]])
  #Eff_Ch10_3[,4+i] = (Eff_Ch10_3[,4+i] - ParamRanges$Lower[ParamRanges$NumberedParams == colnames(Eff_Ch10_3)[4+i]])/(ParamRanges$Upper[ParamRanges$NumberedParams == colnames(Eff_Ch10_3)[4+i]] - ParamRanges$Lower[ParamRanges$NumberedParams == colnames(Eff_Ch10_3)[4+i]])
}

#Remove burnin of 200----
out_sCh10_s1200ncb = out_sCh10_s1200nc[c(1:1000,1201:2200,2401:3400,3601:4600,4801:5800,6001:7000,7201:8200,8401:9400,9601:10600,10801:11800),]
out_sCh10_1_s1200ncb = out_sCh10_1_s1200nc[c(1:1000,1201:2200,2401:3400,3601:4600,4801:5800,6001:7000,7201:8200,8401:9400,9601:10600,10801:11800),]
out_sCh10_2_s1200ncb = out_sCh10_2_s1200nc[c(1:1000,1201:2200,2401:3400,3601:4600,4801:5800,6001:7000,7201:8200,8401:9400,9601:10600,10801:11800),]
out_sCh10_3_s1200ncb = out_sCh10_3_s1200nc[c(1:1000,1201:2200,2401:3400,3601:4600,4801:5800,6001:7000,7201:8200,8401:9400,9601:10600,10801:11800),]

#Select only the unique points----
out_sCh10_s1200ncu = unique(out_sCh10_s1200nc)
out_sCh10_1_s1200ncu = unique(out_sCh10_1_s1200nc)
out_sCh10_2_s1200ncu = unique(out_sCh10_2_s1200nc)
out_sCh10_3_s1200ncu = unique(out_sCh10_3_s1200nc)
#burnin
out_sCh10_s1200ncbu = unique(out_sCh10_s1200ncb)
out_sCh10_1_s1200ncbu = unique(out_sCh10_1_s1200ncb)
out_sCh10_2_s1200ncbu = unique(out_sCh10_2_s1200ncb)
out_sCh10_3_s1200ncbu = unique(out_sCh10_3_s1200ncb)

#Just 4.7% were accepted
out_sCh10u = rbind(out_sCh10_s1200ncu, out_sCh10_1_s1200ncu, out_sCh10_2_s1200ncu, out_sCh10_3_s1200ncu)
out_sCh10bu = rbind(out_sCh10_s1200ncbu, out_sCh10_1_s1200ncbu, out_sCh10_2_s1200ncbu, out_sCh10_3_s1200ncbu)

#Make a dataset of only the points with LNSE > 0.7
#Just 14.4% are less than 0.7, so we're not cutting much of the tails off, and 0.7 is very acceptable performance.
out_sCh10uL = out_sCh10u[out_sCh10u$LNSEd > 0.7,]
out_sCh10buL = out_sCh10bu[out_sCh10bu$LNSEd > 0.7,]

#Burnin of 200 steps would remove some higher LNSEd points as well as all of the LNSEd < 0.7
#Using non-burned in portion of chains

#Create a scale normalized dataset for clustering (compare with 0-1 nromalized)----
out_sCh10suL = out_sCh10uL
out_sCh10suL[,1:19] = t(apply(X = t(apply(X = out_sCh10uL[,1:19], MARGIN = 1, FUN = "-", colMeans(out_sCh10uL[,1:19]))), MARGIN = 1, FUN = "/", colSds(out_sCh10uL[,1:19])))

#Test clustering into 1 - 40 clusters----
# Elbow method
#0-1 normalization
fviz_nbclust(x = out_sCh10uL[,1:19], FUNcluster = kmeans, method = "wss", nstart = 100, k.max = 40) +
  geom_vline(xintercept = 9, linetype = 2, colour='red') +
  geom_vline(xintercept = 19, linetype = 2, colour='red') +
  labs(subtitle = "Elbow method") + coord_cartesian(xlim = c(1, 40), ylim = c(0,2000))
#Scale normalization
fviz_nbclust(x = out_sCh10suL[,1:19], FUNcluster = kmeans, method = "wss", nstart = 100, k.max = 40) +
  geom_vline(xintercept = 9, linetype = 2, colour='red') +
  geom_vline(xintercept = 19, linetype = 2, colour='red') +
  labs(subtitle = "Elbow method") + coord_cartesian(xlim = c(1, 40))

# Silhouette method - optimal of 39
#0-1 normalization
fviz_nbclust(x = out_sCh10uL[,1:19], FUNcluster = kmeans, method = "silhouette", nstart = 100, k.max = 40) +
  geom_vline(xintercept = 9, linetype = 2, colour='red') +
  geom_vline(xintercept = 19, linetype = 2, colour='red') +
  labs(subtitle = "Silhouette method") + coord_cartesian(xlim = c(1, 40))
#Scale normalization
fviz_nbclust(x = out_sCh10suL[,1:19], FUNcluster = kmeans, method = "silhouette", nstart = 100, k.max = 40) +
  geom_vline(xintercept = 9, linetype = 2, colour='red') +
  geom_vline(xintercept = 19, linetype = 2, colour='red') +
  labs(subtitle = "Silhouette method") + coord_cartesian(xlim = c(1, 40))

# Gap statistic - optimal of 14
#0-1 normalization
fviz_nbclust(x = out_sCh10uL[,1:19], FUNcluster = kmeans, method = "gap_stat", iter.max = 100, nstart = 100, k.max = 40, nboot = 100) + 
  geom_vline(xintercept = 9, linetype = 2, colour='red') +
  geom_vline(xintercept = 19, linetype = 2, colour='red') +
  labs(subtitle = "Gap statistic method") + coord_cartesian(xlim = c(1, 40))
#Scale normalization
fviz_nbclust(x = out_sCh10suL[,1:19], FUNcluster = kmeans, method = "gap_stat", iter.max = 100, nstart = 100, k.max = 40, nboot = 5) + 
  geom_vline(xintercept = 9, linetype = 2, colour='red') +
  geom_vline(xintercept = 19, linetype = 2, colour='red') +
  labs(subtitle = "Gap statistic method") + coord_cartesian(xlim = c(1, 40))

#Cluster Metrics - Optimal of 4? That doesn't make sense to me.
nbclust_out <- NbClust(data = out_sCh10uL[,1:19], distance = "euclidean", min.nc = 2, max.nc = 40, method = "kmeans")
# create a dataframe of the optimal number of clusters
nbclust_plot <- data.frame(clusters = nbclust_out$Best.nc[1, ])
# select only indices which select between 2 and 40 clusters
nbclust_plot <- subset(nbclust_plot, clusters >= 2 & clusters <= 40)
# create plot
ggplot(nbclust_plot) + aes(x = clusters) +
  geom_histogram(bins = 40L, fill = "#0c4c8a") +
  labs(x = "Number of clusters", y = "Frequency among all indices", title = "Optimal number of clusters") +
  theme_minimal()

#Evaluate 9 vs. 19 clusters. 19 seems more reasonable from above plots----
#Also doing 8 and 18 + MAP
Clust9 = kmeans(x = out_sCh10uL[,1:19], centers = 9, iter.max = 1000, nstart = 100, trace = FALSE)
Clust19 = kmeans(x = out_sCh10uL[,1:19], centers = 19, iter.max = 1000, nstart = 100, trace = FALSE)
Clust8 = kmeans(x = out_sCh10uL[,1:19], centers = 8, iter.max = 1000, nstart = 100, trace = FALSE)
Clust18 = kmeans(x = out_sCh10uL[,1:19], centers = 18, iter.max = 1000, nstart = 100, trace = FALSE)
#Scale normalization
Clust9s = kmeans(x = out_sCh10suL[,1:19], centers = 9, iter.max = 1000, nstart = 100, trace = FALSE)
Clust19s = kmeans(x = out_sCh10suL[,1:19], centers = 19, iter.max = 1000, nstart = 100, trace = FALSE)
Clust8s = kmeans(x = out_sCh10suL[,1:19], centers = 8, iter.max = 1000, nstart = 100, trace = FALSE)
Clust18s = kmeans(x = out_sCh10suL[,1:19], centers = 18, iter.max = 1000, nstart = 100, trace = FALSE)

# Gather the most likely point from each cluster----
MaxClust8 = as.data.frame(matrix(NA, nrow = 8, ncol = ncol(out_sCh10uL)))
MaxClust8s = as.data.frame(matrix(NA, nrow = 8, ncol = ncol(out_sCh10suL)))
for (i in 1:8){
  MaxClust8[i,] = out_sCh10uL[Clust8$cluster == i,][which(out_sCh10uL$LP[Clust8$cluster == i] == max(out_sCh10uL$LP[Clust8$cluster == i])),]
  MaxClust8s[i,] = out_sCh10suL[Clust8s$cluster == i,][which(out_sCh10suL$LP[Clust8s$cluster == i] == max(out_sCh10suL$LP[Clust8s$cluster == i])),]
}
colnames(MaxClust8) = colnames(out_sCh10uL)
colnames(MaxClust8s) = colnames(out_sCh10suL)

#Points in clusters do not all come from the same chain, which is good.
hist(out_sCh10uL$Chain[Clust8$cluster == 1])
hist(out_sCh10uL$Chain[Clust8$cluster == 2])
hist(out_sCh10uL$Chain[Clust8$cluster == 3])
hist(out_sCh10uL$Chain[Clust8$cluster == 4])
hist(out_sCh10uL$Chain[Clust8$cluster == 5])
hist(out_sCh10uL$Chain[Clust8$cluster == 6])
hist(out_sCh10uL$Chain[Clust8$cluster == 7])
hist(out_sCh10uL$Chain[Clust8$cluster == 8])

hist(out_sCh10suL$Chain[Clust8s$cluster == 1])
hist(out_sCh10suL$Chain[Clust8s$cluster == 2])
hist(out_sCh10suL$Chain[Clust8s$cluster == 3])
hist(out_sCh10suL$Chain[Clust8s$cluster == 4])
hist(out_sCh10suL$Chain[Clust8s$cluster == 5])
hist(out_sCh10suL$Chain[Clust8s$cluster == 6])
hist(out_sCh10suL$Chain[Clust8s$cluster == 7])
hist(out_sCh10suL$Chain[Clust8s$cluster == 8])

MaxClust9 = as.data.frame(matrix(NA, nrow = 9, ncol = ncol(out_sCh10uL)))
MaxClust9s = as.data.frame(matrix(NA, nrow = 9, ncol = ncol(out_sCh10suL)))
for (i in 1:9){
  MaxClust9[i,] = out_sCh10uL[Clust9$cluster == i,][which(out_sCh10uL$LP[Clust9$cluster == i] == max(out_sCh10uL$LP[Clust9$cluster == i])),]
  MaxClust9s[i,] = out_sCh10suL[Clust9s$cluster == i,][which(out_sCh10suL$LP[Clust9s$cluster == i] == max(out_sCh10suL$LP[Clust9s$cluster == i])),]
}
colnames(MaxClust9) = colnames(out_sCh10uL)
colnames(MaxClust9s) = colnames(out_sCh10suL)

#Points in clusters do not all come from the same chain, which is good.
hist(out_sCh10uL$Chain[Clust9$cluster == 1])
hist(out_sCh10uL$Chain[Clust9$cluster == 2])
hist(out_sCh10uL$Chain[Clust9$cluster == 3])
hist(out_sCh10uL$Chain[Clust9$cluster == 4])
hist(out_sCh10uL$Chain[Clust9$cluster == 5])
hist(out_sCh10uL$Chain[Clust9$cluster == 6])
hist(out_sCh10uL$Chain[Clust9$cluster == 7])
hist(out_sCh10uL$Chain[Clust9$cluster == 8])
hist(out_sCh10uL$Chain[Clust9$cluster == 9])

hist(out_sCh10suL$Chain[Clust9s$cluster == 1])
hist(out_sCh10suL$Chain[Clust9s$cluster == 2])
hist(out_sCh10suL$Chain[Clust9s$cluster == 3])
hist(out_sCh10suL$Chain[Clust9s$cluster == 4])
hist(out_sCh10suL$Chain[Clust9s$cluster == 5])
hist(out_sCh10suL$Chain[Clust9s$cluster == 6])
hist(out_sCh10suL$Chain[Clust9s$cluster == 7])
hist(out_sCh10suL$Chain[Clust9s$cluster == 8])
hist(out_sCh10suL$Chain[Clust9s$cluster == 9])

MaxClust18 = as.data.frame(matrix(NA, nrow = 18, ncol = ncol(out_sCh10uL)))
MaxClust18s = as.data.frame(matrix(NA, nrow = 18, ncol = ncol(out_sCh10suL)))
for (i in 1:18){
  MaxClust18[i,] = out_sCh10uL[Clust18$cluster == i,][which(out_sCh10uL$LP[Clust18$cluster == i] == max(out_sCh10uL$LP[Clust18$cluster == i])),]
  MaxClust18s[i,] = out_sCh10suL[Clust18s$cluster == i,][which(out_sCh10suL$LP[Clust18s$cluster == i] == max(out_sCh10suL$LP[Clust18s$cluster == i])),]
}
colnames(MaxClust18) = colnames(out_sCh10uL)
colnames(MaxClust18s) = colnames(out_sCh10suL)

#Points in clusters do not all come from the same chain, which is good.
hist(out_sCh10uL$Chain[Clust18$cluster == 1])
hist(out_sCh10uL$Chain[Clust18$cluster == 2])
hist(out_sCh10uL$Chain[Clust18$cluster == 3])
hist(out_sCh10uL$Chain[Clust18$cluster == 4])
hist(out_sCh10uL$Chain[Clust18$cluster == 5])
hist(out_sCh10uL$Chain[Clust18$cluster == 6])
hist(out_sCh10uL$Chain[Clust18$cluster == 7])
hist(out_sCh10uL$Chain[Clust18$cluster == 8])
hist(out_sCh10uL$Chain[Clust18$cluster == 9])
hist(out_sCh10uL$Chain[Clust18$cluster == 10])
hist(out_sCh10uL$Chain[Clust18$cluster == 11])
hist(out_sCh10uL$Chain[Clust18$cluster == 12])
hist(out_sCh10uL$Chain[Clust18$cluster == 13])
hist(out_sCh10uL$Chain[Clust18$cluster == 14])
hist(out_sCh10uL$Chain[Clust18$cluster == 15])
hist(out_sCh10uL$Chain[Clust18$cluster == 16])
hist(out_sCh10uL$Chain[Clust18$cluster == 17])
hist(out_sCh10uL$Chain[Clust18$cluster == 18])

hist(out_sCh10suL$Chain[Clust18s$cluster == 1])
hist(out_sCh10suL$Chain[Clust18s$cluster == 2])
hist(out_sCh10suL$Chain[Clust18s$cluster == 3])
hist(out_sCh10suL$Chain[Clust18s$cluster == 4])
hist(out_sCh10suL$Chain[Clust18s$cluster == 5])
hist(out_sCh10suL$Chain[Clust18s$cluster == 6])
hist(out_sCh10suL$Chain[Clust18s$cluster == 7])
hist(out_sCh10suL$Chain[Clust18s$cluster == 8])
hist(out_sCh10suL$Chain[Clust18s$cluster == 9])
hist(out_sCh10suL$Chain[Clust18s$cluster == 10])
hist(out_sCh10suL$Chain[Clust18s$cluster == 11])
hist(out_sCh10suL$Chain[Clust18s$cluster == 12])
hist(out_sCh10suL$Chain[Clust18s$cluster == 13])
hist(out_sCh10suL$Chain[Clust18s$cluster == 14])
hist(out_sCh10suL$Chain[Clust18s$cluster == 15])
hist(out_sCh10suL$Chain[Clust18s$cluster == 16])
hist(out_sCh10suL$Chain[Clust18s$cluster == 17])
hist(out_sCh10suL$Chain[Clust18s$cluster == 18])

MaxClust19 = as.data.frame(matrix(NA, nrow = 19, ncol = ncol(out_sCh10uL)))
MaxClust19s = as.data.frame(matrix(NA, nrow = 19, ncol = ncol(out_sCh10suL)))
for (i in 1:19){
  MaxClust19[i,] = out_sCh10uL[Clust19$cluster == i,][which(out_sCh10uL$LP[Clust19$cluster == i] == max(out_sCh10uL$LP[Clust19$cluster == i])),]
  MaxClust19s[i,] = out_sCh10suL[Clust19s$cluster == i,][which(out_sCh10suL$LP[Clust19s$cluster == i] == max(out_sCh10suL$LP[Clust19s$cluster == i])),]
}
colnames(MaxClust19) = colnames(out_sCh10uL)
colnames(MaxClust19s) = colnames(out_sCh10suL)

hist(out_sCh10uL$Chain[Clust19$cluster == 1])
hist(out_sCh10uL$Chain[Clust19$cluster == 2])
hist(out_sCh10uL$Chain[Clust19$cluster == 3])
hist(out_sCh10uL$Chain[Clust19$cluster == 4])
hist(out_sCh10uL$Chain[Clust19$cluster == 5])
hist(out_sCh10uL$Chain[Clust19$cluster == 6])
hist(out_sCh10uL$Chain[Clust19$cluster == 7])
hist(out_sCh10uL$Chain[Clust19$cluster == 8])
hist(out_sCh10uL$Chain[Clust19$cluster == 9])
hist(out_sCh10uL$Chain[Clust19$cluster == 10])
hist(out_sCh10uL$Chain[Clust19$cluster == 11])
hist(out_sCh10uL$Chain[Clust19$cluster == 12])
hist(out_sCh10uL$Chain[Clust19$cluster == 13])
hist(out_sCh10uL$Chain[Clust19$cluster == 14])
hist(out_sCh10uL$Chain[Clust19$cluster == 15])
hist(out_sCh10uL$Chain[Clust19$cluster == 16])
hist(out_sCh10uL$Chain[Clust19$cluster == 17])
hist(out_sCh10uL$Chain[Clust19$cluster == 18])
hist(out_sCh10uL$Chain[Clust19$cluster == 19])

hist(out_sCh10suL$Chain[Clust19s$cluster == 1])
hist(out_sCh10suL$Chain[Clust19s$cluster == 2])
hist(out_sCh10suL$Chain[Clust19s$cluster == 3])
hist(out_sCh10suL$Chain[Clust19s$cluster == 4])
hist(out_sCh10suL$Chain[Clust19s$cluster == 5])
hist(out_sCh10suL$Chain[Clust19s$cluster == 6])
hist(out_sCh10suL$Chain[Clust19s$cluster == 7])
hist(out_sCh10suL$Chain[Clust19s$cluster == 8])
hist(out_sCh10suL$Chain[Clust19s$cluster == 9])
hist(out_sCh10suL$Chain[Clust19s$cluster == 10])
hist(out_sCh10suL$Chain[Clust19s$cluster == 11])
hist(out_sCh10suL$Chain[Clust19s$cluster == 12])
hist(out_sCh10suL$Chain[Clust19s$cluster == 13])
hist(out_sCh10suL$Chain[Clust19s$cluster == 14])
hist(out_sCh10suL$Chain[Clust19s$cluster == 15])
hist(out_sCh10suL$Chain[Clust19s$cluster == 16])
hist(out_sCh10suL$Chain[Clust19s$cluster == 17])
hist(out_sCh10suL$Chain[Clust19s$cluster == 18])
hist(out_sCh10suL$Chain[Clust19s$cluster == 19])

# Gather the point closest to the center of each cluster (that's how they're clustered, so probably better)----
#  0-1 normalization----
CenPtClust8 = as.data.frame(matrix(NA, nrow = 8, ncol = ncol(out_sCh10uL)))
for (i in 1:8){
  #Calculate Euclidean distance to the center vector
  dists = t(apply(X = out_sCh10uL[Clust8$cluster == i,1:19], MARGIN = 1, FUN = '-', Clust8$centers[i,]))
  #nth root of sum of squared differences
  dists = dists^2
  dists = apply(X = dists, MARGIN = 1, FUN = sum)
  dists = dists^(1/8)
  #Take the point with the smallest distance to the center
  CenPtClust8[i,] = out_sCh10uL[Clust8$cluster == i,][which(dists == min(dists)),]
  
  #If this cluster has the MAP, compute the distance rank of the MAP
  if (i == which(MaxClust8$LP == max(MaxClust8$LP))){
    distMAP = dists[which(out_sCh10uL$LP[Clust8$cluster == i] == max(out_sCh10uL$LP[Clust8$cluster == i]))]
    DistRankMAP8 = which(sort(dists) == distMAP)
  }
}
colnames(CenPtClust8) = colnames(out_sCh10uL)

CenPtClust9 = as.data.frame(matrix(NA, nrow = 9, ncol = ncol(out_sCh10uL)))
for (i in 1:9){
  #Calculate Euclidean distance to the center vector
  dists = t(apply(X = out_sCh10uL[Clust9$cluster == i,1:19], MARGIN = 1, FUN = '-', Clust9$centers[i,]))
  #nth root of sum of squared differences
  dists = dists^2
  dists = apply(X = dists, MARGIN = 1, FUN = sum)
  dists = dists^(1/9)
  #Take the point with the smallest distance to the center
  CenPtClust9[i,] = out_sCh10uL[Clust9$cluster == i,][which(dists == min(dists)),]
  
  #If this cluster has the MAP, compute the distance rank of the MAP
  if (i == which(MaxClust9$LP == max(MaxClust9$LP))){
    distMAP = dists[which(out_sCh10uL$LP[Clust9$cluster == i] == max(out_sCh10uL$LP[Clust9$cluster == i]))]
    DistRankMAP9 = which(sort(dists) == distMAP)
  }
}
colnames(CenPtClust9) = colnames(out_sCh10uL)

CenPtClust18 = as.data.frame(matrix(NA, nrow = 18, ncol = ncol(out_sCh10uL)))
for (i in 1:18){
  #Calculate Euclidean distance to the center vector
  dists = t(apply(X = out_sCh10uL[Clust18$cluster == i,1:19], MARGIN = 1, FUN = '-', Clust18$centers[i,]))
  #nth root of sum of squared differences
  dists = dists^2
  dists = apply(X = dists, MARGIN = 1, FUN = sum)
  dists = dists^(1/18)
  #Take the point with the smallest distance to the center
  CenPtClust18[i,] = out_sCh10uL[Clust18$cluster == i,][which(dists == min(dists)),]
  
  #If this cluster has the MAP, compute the distance rank of the MAP
  if (i == which(MaxClust18$LP == max(MaxClust18$LP))){
    distMAP = dists[which(out_sCh10uL$LP[Clust18$cluster == i] == max(out_sCh10uL$LP[Clust18$cluster == i]))]
    DistRankMAP18 = which(sort(dists) == distMAP)
  }
}
colnames(CenPtClust18) = colnames(out_sCh10uL)

CenPtClust19 = as.data.frame(matrix(NA, nrow = 19, ncol = ncol(out_sCh10uL)))
for (i in 1:19){
  #Calculate Euclidean distance to the center vector
  dists = t(apply(X = out_sCh10uL[Clust19$cluster == i,1:19], MARGIN = 1, FUN = '-', Clust19$centers[i,]))
  #nth root of sum of squared differences
  dists = dists^2
  dists = apply(X = dists, MARGIN = 1, FUN = sum)
  dists = dists^(1/19)
  #Take the point with the smallest distance to the center
  CenPtClust19[i,] = out_sCh10uL[Clust19$cluster == i,][which(dists == min(dists)),]
  
  #If this cluster has the MAP, compute the distance rank of the MAP
  if (i == which(MaxClust19$LP == max(MaxClust19$LP))){
    distMAP = dists[which(out_sCh10uL$LP[Clust19$cluster == i] == max(out_sCh10uL$LP[Clust19$cluster == i]))]
    DistRankMAP19 = which(sort(dists) == distMAP)
  }
}
colnames(CenPtClust19) = colnames(out_sCh10uL)

#  Scale normalization----
CenPtClust8s = as.data.frame(matrix(NA, nrow = 8, ncol = ncol(out_sCh10suL)))
for (i in 1:8){
  #Calculate Euclidean distance to the center vector
  dists = t(apply(X = out_sCh10suL[Clust8s$cluster == i,1:19], MARGIN = 1, FUN = '-', Clust8s$centers[i,]))
  #nth root of sum of squared differences
  dists = dists^2
  dists = apply(X = dists, MARGIN = 1, FUN = sum)
  dists = dists^(1/8)
  #Take the point with the smallest distance to the center
  CenPtClust8s[i,] = out_sCh10suL[Clust8s$cluster == i,][which(dists == min(dists)),]
  
  #If this cluster has the MAP, compute the distance rank of the MAP
  if (i == which(MaxClust8s$LP == max(MaxClust8s$LP))){
    distMAP = dists[which(out_sCh10suL$LP[Clust8s$cluster == i] == max(out_sCh10suL$LP[Clust8s$cluster == i]))]
    DistRankMAP8s = which(sort(dists) == distMAP)
  }
}
colnames(CenPtClust8s) = colnames(out_sCh10suL)

CenPtClust9s = as.data.frame(matrix(NA, nrow = 9, ncol = ncol(out_sCh10suL)))
for (i in 1:9){
  #Calculate Euclidean distance to the center vector
  dists = t(apply(X = out_sCh10suL[Clust9s$cluster == i,1:19], MARGIN = 1, FUN = '-', Clust9s$centers[i,]))
  #nth root of sum of squared differences
  dists = dists^2
  dists = apply(X = dists, MARGIN = 1, FUN = sum)
  dists = dists^(1/9)
  #Take the point with the smallest distance to the center
  CenPtClust9s[i,] = out_sCh10suL[Clust9s$cluster == i,][which(dists == min(dists)),]
  
  #If this cluster has the MAP, compute the distance rank of the MAP
  if (i == which(MaxClust9s$LP == max(MaxClust9s$LP))){
    distMAP = dists[which(out_sCh10suL$LP[Clust9s$cluster == i] == max(out_sCh10suL$LP[Clust9s$cluster == i]))]
    DistRankMAP9s = which(sort(dists) == distMAP)
  }
}
colnames(CenPtClust9s) = colnames(out_sCh10suL)

CenPtClust18s = as.data.frame(matrix(NA, nrow = 18, ncol = ncol(out_sCh10suL)))
for (i in 1:18){
  #Calculate Euclidean distance to the center vector
  dists = t(apply(X = out_sCh10suL[Clust18s$cluster == i,1:19], MARGIN = 1, FUN = '-', Clust18s$centers[i,]))
  #nth root of sum of squared differences
  dists = dists^2
  dists = apply(X = dists, MARGIN = 1, FUN = sum)
  dists = dists^(1/18)
  #Take the point with the smallest distance to the center
  CenPtClust18s[i,] = out_sCh10suL[Clust18s$cluster == i,][which(dists == min(dists)),]
  
  #If this cluster has the MAP, compute the distance rank of the MAP
  if (i == which(MaxClust18s$LP == max(MaxClust18s$LP))){
    distMAP = dists[which(out_sCh10suL$LP[Clust18s$cluster == i] == max(out_sCh10suL$LP[Clust18s$cluster == i]))]
    DistRankMAP18s = which(sort(dists) == distMAP)
  }
}
colnames(CenPtClust18s) = colnames(out_sCh10suL)

CenPtClust19s = as.data.frame(matrix(NA, nrow = 19, ncol = ncol(out_sCh10suL)))
for (i in 1:19){
  #Calculate Euclidean distance to the center vector
  dists = t(apply(X = out_sCh10suL[Clust19s$cluster == i,1:19], MARGIN = 1, FUN = '-', Clust19s$centers[i,]))
  #nth root of sum of squared differences
  dists = dists^2
  dists = apply(X = dists, MARGIN = 1, FUN = sum)
  dists = dists^(1/19)
  #Take the point with the smallest distance to the center
  CenPtClust19s[i,] = out_sCh10suL[Clust19s$cluster == i,][which(dists == min(dists)),]
  
  #If this cluster has the MAP, compute the distance rank of the MAP
  if (i == which(MaxClust19s$LP == max(MaxClust19s$LP))){
    distMAP = dists[which(out_sCh10suL$LP[Clust19s$cluster == i] == max(out_sCh10suL$LP[Clust19s$cluster == i]))]
    DistRankMAP19s = which(sort(dists) == distMAP)
  }
}
colnames(CenPtClust19s) = colnames(out_sCh10suL)

# Parallel axis plot of points----
#Rescale values to 0-1 range for plotting
Clust8sp = t(apply(X = t(apply(X = Clust8s$centers[,1:19], MARGIN = 1, FUN = "*", colSds(out_sCh10uL[,1:19]))), MARGIN = 1, FUN = "+", colMeans(out_sCh10uL[,1:19])))
Clust9sp = t(apply(X = t(apply(X = Clust9s$centers[,1:19], MARGIN = 1, FUN = "*", colSds(out_sCh10uL[,1:19]))), MARGIN = 1, FUN = "+", colMeans(out_sCh10uL[,1:19])))
Clust18sp = t(apply(X = t(apply(X = Clust18s$centers[,1:19], MARGIN = 1, FUN = "*", colSds(out_sCh10uL[,1:19]))), MARGIN = 1, FUN = "+", colMeans(out_sCh10uL[,1:19])))
Clust19sp = t(apply(X = t(apply(X = Clust19s$centers[,1:19], MARGIN = 1, FUN = "*", colSds(out_sCh10uL[,1:19]))), MARGIN = 1, FUN = "+", colMeans(out_sCh10uL[,1:19])))

CenPtClust8sp = t(apply(X = t(apply(X = CenPtClust8s[,1:19], MARGIN = 1, FUN = "*", colSds(out_sCh10uL[,1:19]))), MARGIN = 1, FUN = "+", colMeans(out_sCh10uL[,1:19])))
CenPtClust9sp = t(apply(X = t(apply(X = CenPtClust9s[,1:19], MARGIN = 1, FUN = "*", colSds(out_sCh10uL[,1:19]))), MARGIN = 1, FUN = "+", colMeans(out_sCh10uL[,1:19])))
CenPtClust18sp = t(apply(X = t(apply(X = CenPtClust18s[,1:19], MARGIN = 1, FUN = "*", colSds(out_sCh10uL[,1:19]))), MARGIN = 1, FUN = "+", colMeans(out_sCh10uL[,1:19])))
CenPtClust19sp = t(apply(X = t(apply(X = CenPtClust19s[,1:19], MARGIN = 1, FUN = "*", colSds(out_sCh10uL[,1:19]))), MARGIN = 1, FUN = "+", colMeans(out_sCh10uL[,1:19])))

MaxClust8sp = t(apply(X = t(apply(X = MaxClust8s[,1:19], MARGIN = 1, FUN = "*", colSds(out_sCh10uL[,1:19]))), MARGIN = 1, FUN = "+", colMeans(out_sCh10uL[,1:19])))
MaxClust9sp = t(apply(X = t(apply(X = MaxClust9s[,1:19], MARGIN = 1, FUN = "*", colSds(out_sCh10uL[,1:19]))), MARGIN = 1, FUN = "+", colMeans(out_sCh10uL[,1:19])))
MaxClust18sp = t(apply(X = t(apply(X = MaxClust18s[,1:19], MARGIN = 1, FUN = "*", colSds(out_sCh10uL[,1:19]))), MARGIN = 1, FUN = "+", colMeans(out_sCh10uL[,1:19])))
MaxClust19sp = t(apply(X = t(apply(X = MaxClust19s[,1:19], MARGIN = 1, FUN = "*", colSds(out_sCh10uL[,1:19]))), MARGIN = 1, FUN = "+", colMeans(out_sCh10uL[,1:19])))

#  All cluster centers----
parcoord(x = rbind(rep(0,19), rep(1,19), Clust8$centers[,1:19]), col = c('black', 'black', rainbow(8)))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust9$centers[,1:19]), col = c('black', 'black', rainbow(9)))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18$centers[,1:19]), col = c('black', 'black', rainbow(18)))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19$centers[,1:19]), col = c('black', 'black', rainbow(19)))

parcoord(x = rbind(rep(0,19), rep(1,19), Clust8sp[,1:19]), col = c('black', 'black', rainbow(8)))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust9sp[,1:19]), col = c('black', 'black', rainbow(9)))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18sp[,1:19]), col = c('black', 'black', rainbow(18)))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19sp[,1:19]), col = c('black', 'black', rainbow(19)))

#  All points closest to center----
parcoord(x = rbind(rep(0,19), rep(1,19), CenPtClust8[,1:19]), col = c('black', 'black', rainbow(8)))
parcoord(x = rbind(rep(0,19), rep(1,19), CenPtClust9[,1:19]), col = c('black', 'black', rainbow(9)))
parcoord(x = rbind(rep(0,19), rep(1,19), CenPtClust18[,1:19]), col = c('black', 'black', rainbow(18)))
parcoord(x = rbind(rep(0,19), rep(1,19), CenPtClust19[,1:19]), col = c('black', 'black', rainbow(19)))

parcoord(x = rbind(rep(0,19), rep(1,19), CenPtClust8sp[,1:19]), col = c('black', 'black', rainbow(8)))
parcoord(x = rbind(rep(0,19), rep(1,19), CenPtClust9sp[,1:19]), col = c('black', 'black', rainbow(9)))
parcoord(x = rbind(rep(0,19), rep(1,19), CenPtClust18sp[,1:19]), col = c('black', 'black', rainbow(18)))
parcoord(x = rbind(rep(0,19), rep(1,19), CenPtClust19sp[,1:19]), col = c('black', 'black', rainbow(19)))

#  All most likely points in clusters----
parcoord(x = rbind(rep(0,19), rep(1,19), MaxClust8[,1:19]), col = c('black', 'black', rainbow(8)))
parcoord(x = rbind(rep(0,19), rep(1,19), MaxClust9[,1:19]), col = c('black', 'black', rainbow(9)))
parcoord(x = rbind(rep(0,19), rep(1,19), MaxClust18[,1:19]), col = c('black', 'black', rainbow(18)))
parcoord(x = rbind(rep(0,19), rep(1,19), MaxClust19[,1:19]), col = c('black', 'black', rainbow(19)))

parcoord(x = rbind(rep(0,19), rep(1,19), MaxClust8sp[,1:19]), col = c('black', 'black', rainbow(8)))
parcoord(x = rbind(rep(0,19), rep(1,19), MaxClust9sp[,1:19]), col = c('black', 'black', rainbow(9)))
parcoord(x = rbind(rep(0,19), rep(1,19), MaxClust18sp[,1:19]), col = c('black', 'black', rainbow(18)))
parcoord(x = rbind(rep(0,19), rep(1,19), MaxClust19sp[,1:19]), col = c('black', 'black', rainbow(19)))

# Plot the center point, max point, and cluster centers on parallel axis plot----
#  8 clusters----
parcoord(x = rbind(rep(0,19), rep(1,19), Clust8$centers[1,1:19], CenPtClust8[1,1:19], MaxClust8[1,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust8$centers[2,1:19], CenPtClust8[2,1:19], MaxClust8[2,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust8$centers[3,1:19], CenPtClust8[3,1:19], MaxClust8[3,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust8$centers[4,1:19], CenPtClust8[4,1:19], MaxClust8[4,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust8$centers[5,1:19], CenPtClust8[5,1:19], MaxClust8[5,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust8$centers[6,1:19], CenPtClust8[6,1:19], MaxClust8[6,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust8$centers[7,1:19], CenPtClust8[7,1:19], MaxClust8[7,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust8$centers[8,1:19], CenPtClust8[8,1:19], MaxClust8[8,1:19]), col = c('black', 'black','red', 'green', 'blue'))

parcoord(x = rbind(rep(0,19), rep(1,19), Clust8sp[1,1:19], CenPtClust8sp[1,1:19], MaxClust8sp[1,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust8sp[2,1:19], CenPtClust8sp[2,1:19], MaxClust8sp[2,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust8sp[3,1:19], CenPtClust8sp[3,1:19], MaxClust8sp[3,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust8sp[4,1:19], CenPtClust8sp[4,1:19], MaxClust8sp[4,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust8sp[5,1:19], CenPtClust8sp[5,1:19], MaxClust8sp[5,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust8sp[6,1:19], CenPtClust8sp[6,1:19], MaxClust8sp[6,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust8sp[7,1:19], CenPtClust8sp[7,1:19], MaxClust8sp[7,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust8sp[8,1:19], CenPtClust8sp[8,1:19], MaxClust8sp[8,1:19]), col = c('black', 'black','red', 'green', 'blue'))

#  9 clusters----
parcoord(x = rbind(rep(0,19), rep(1,19), Clust9$centers[1,1:19], CenPtClust9[1,1:19], MaxClust9[1,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust9$centers[2,1:19], CenPtClust9[2,1:19], MaxClust9[2,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust9$centers[3,1:19], CenPtClust9[3,1:19], MaxClust9[3,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust9$centers[4,1:19], CenPtClust9[4,1:19], MaxClust9[4,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust9$centers[5,1:19], CenPtClust9[5,1:19], MaxClust9[5,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust9$centers[6,1:19], CenPtClust9[6,1:19], MaxClust9[6,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust9$centers[7,1:19], CenPtClust9[7,1:19], MaxClust9[7,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust9$centers[8,1:19], CenPtClust9[8,1:19], MaxClust9[8,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust9$centers[9,1:19], CenPtClust9[9,1:19], MaxClust9[9,1:19]), col = c('black', 'black','red', 'green', 'blue'))

parcoord(x = rbind(rep(0,19), rep(1,19), Clust9sp[1,1:19], CenPtClust9sp[1,1:19], MaxClust9sp[1,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust9sp[2,1:19], CenPtClust9sp[2,1:19], MaxClust9sp[2,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust9sp[3,1:19], CenPtClust9sp[3,1:19], MaxClust9sp[3,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust9sp[4,1:19], CenPtClust9sp[4,1:19], MaxClust9sp[4,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust9sp[5,1:19], CenPtClust9sp[5,1:19], MaxClust9sp[5,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust9sp[6,1:19], CenPtClust9sp[6,1:19], MaxClust9sp[6,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust9sp[7,1:19], CenPtClust9sp[7,1:19], MaxClust9sp[7,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust9sp[8,1:19], CenPtClust9sp[8,1:19], MaxClust9sp[8,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust9sp[9,1:19], CenPtClust9sp[9,1:19], MaxClust9sp[9,1:19]), col = c('black', 'black','red', 'green', 'blue'))

#18 clusters
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18$centers[1,1:19], CenPtClust18[1,1:19], MaxClust18[1,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18$centers[2,1:19], CenPtClust18[2,1:19], MaxClust18[2,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18$centers[3,1:19], CenPtClust18[3,1:19], MaxClust18[3,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18$centers[4,1:19], CenPtClust18[4,1:19], MaxClust18[4,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18$centers[5,1:19], CenPtClust18[5,1:19], MaxClust18[5,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18$centers[6,1:19], CenPtClust18[6,1:19], MaxClust18[6,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18$centers[7,1:19], CenPtClust18[7,1:19], MaxClust18[7,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18$centers[8,1:19], CenPtClust18[8,1:19], MaxClust18[8,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18$centers[9,1:19], CenPtClust18[9,1:19], MaxClust18[9,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18$centers[10,1:19], CenPtClust18[10,1:19], MaxClust18[10,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18$centers[11,1:19], CenPtClust18[11,1:19], MaxClust18[11,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18$centers[12,1:19], CenPtClust18[12,1:19], MaxClust18[12,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18$centers[13,1:19], CenPtClust18[13,1:19], MaxClust18[13,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18$centers[14,1:19], CenPtClust18[14,1:19], MaxClust18[14,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18$centers[15,1:19], CenPtClust18[15,1:19], MaxClust18[15,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18$centers[16,1:19], CenPtClust18[16,1:19], MaxClust18[16,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18$centers[17,1:19], CenPtClust18[17,1:19], MaxClust18[17,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18$centers[18,1:19], CenPtClust18[18,1:19], MaxClust18[18,1:19]), col = c('black', 'black','red', 'green', 'blue'))

parcoord(x = rbind(rep(0,19), rep(1,19), Clust18sp[1,1:19], CenPtClust18sp[1,1:19], MaxClust18sp[1,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18sp[2,1:19], CenPtClust18sp[2,1:19], MaxClust18sp[2,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18sp[3,1:19], CenPtClust18sp[3,1:19], MaxClust18sp[3,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18sp[4,1:19], CenPtClust18sp[4,1:19], MaxClust18sp[4,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18sp[5,1:19], CenPtClust18sp[5,1:19], MaxClust18sp[5,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18sp[6,1:19], CenPtClust18sp[6,1:19], MaxClust18sp[6,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18sp[7,1:19], CenPtClust18sp[7,1:19], MaxClust18sp[7,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18sp[8,1:19], CenPtClust18sp[8,1:19], MaxClust18sp[8,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18sp[9,1:19], CenPtClust18sp[9,1:19], MaxClust18sp[9,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18sp[10,1:19], CenPtClust18sp[10,1:19], MaxClust18sp[10,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18sp[11,1:19], CenPtClust18sp[11,1:19], MaxClust18sp[11,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18sp[12,1:19], CenPtClust18sp[12,1:19], MaxClust18sp[12,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18sp[13,1:19], CenPtClust18sp[13,1:19], MaxClust18sp[13,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18sp[14,1:19], CenPtClust18sp[14,1:19], MaxClust18sp[14,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18sp[15,1:19], CenPtClust18sp[15,1:19], MaxClust18sp[15,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18sp[16,1:19], CenPtClust18sp[16,1:19], MaxClust18sp[16,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18sp[17,1:19], CenPtClust18sp[17,1:19], MaxClust18sp[17,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust18sp[18,1:19], CenPtClust18sp[18,1:19], MaxClust18sp[18,1:19]), col = c('black', 'black','red', 'green', 'blue'))

#19 clusters
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19$centers[1,1:19], CenPtClust19[1,1:19], MaxClust19[1,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19$centers[2,1:19], CenPtClust19[2,1:19], MaxClust19[2,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19$centers[3,1:19], CenPtClust19[3,1:19], MaxClust19[3,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19$centers[4,1:19], CenPtClust19[4,1:19], MaxClust19[4,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19$centers[5,1:19], CenPtClust19[5,1:19], MaxClust19[5,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19$centers[6,1:19], CenPtClust19[6,1:19], MaxClust19[6,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19$centers[7,1:19], CenPtClust19[7,1:19], MaxClust19[7,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19$centers[8,1:19], CenPtClust19[8,1:19], MaxClust19[8,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19$centers[9,1:19], CenPtClust19[9,1:19], MaxClust19[9,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19$centers[10,1:19], CenPtClust19[10,1:19], MaxClust19[10,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19$centers[11,1:19], CenPtClust19[11,1:19], MaxClust19[11,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19$centers[12,1:19], CenPtClust19[12,1:19], MaxClust19[12,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19$centers[13,1:19], CenPtClust19[13,1:19], MaxClust19[13,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19$centers[14,1:19], CenPtClust19[14,1:19], MaxClust19[14,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19$centers[15,1:19], CenPtClust19[15,1:19], MaxClust19[15,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19$centers[16,1:19], CenPtClust19[16,1:19], MaxClust19[16,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19$centers[17,1:19], CenPtClust19[17,1:19], MaxClust19[17,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19$centers[18,1:19], CenPtClust19[18,1:19], MaxClust19[18,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19$centers[19,1:19], CenPtClust19[19,1:19], MaxClust19[19,1:19]), col = c('black', 'black','red', 'green', 'blue'))

parcoord(x = rbind(rep(0,19), rep(1,19), Clust19sp[1,1:19], CenPtClust19sp[1,1:19], MaxClust19sp[1,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19sp[2,1:19], CenPtClust19sp[2,1:19], MaxClust19sp[2,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19sp[3,1:19], CenPtClust19sp[3,1:19], MaxClust19sp[3,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19sp[4,1:19], CenPtClust19sp[4,1:19], MaxClust19sp[4,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19sp[5,1:19], CenPtClust19sp[5,1:19], MaxClust19sp[5,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19sp[6,1:19], CenPtClust19sp[6,1:19], MaxClust19sp[6,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19sp[7,1:19], CenPtClust19sp[7,1:19], MaxClust19sp[7,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19sp[8,1:19], CenPtClust19sp[8,1:19], MaxClust19sp[8,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19sp[9,1:19], CenPtClust19sp[9,1:19], MaxClust19sp[9,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19sp[10,1:19], CenPtClust19sp[10,1:19], MaxClust19sp[10,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19sp[11,1:19], CenPtClust19sp[11,1:19], MaxClust19sp[11,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19sp[12,1:19], CenPtClust19sp[12,1:19], MaxClust19sp[12,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19sp[13,1:19], CenPtClust19sp[13,1:19], MaxClust19sp[13,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19sp[14,1:19], CenPtClust19sp[14,1:19], MaxClust19sp[14,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19sp[15,1:19], CenPtClust19sp[15,1:19], MaxClust19sp[15,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19sp[16,1:19], CenPtClust19sp[16,1:19], MaxClust19sp[16,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19sp[17,1:19], CenPtClust19sp[17,1:19], MaxClust19sp[17,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19sp[18,1:19], CenPtClust19sp[18,1:19], MaxClust19sp[18,1:19]), col = c('black', 'black','red', 'green', 'blue'))
parcoord(x = rbind(rep(0,19), rep(1,19), Clust19sp[19,1:19], CenPtClust19sp[19,1:19], MaxClust19sp[19,1:19]), col = c('black', 'black','red', 'green', 'blue'))

#Plot the center points and the MAP on the marginal distributions----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration")
# Atm Params----
#Synthetic
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(12,2)], out_sCh10_2_s1200n$chain[,c(12,2)], out_sCh10_1_s1200n$chain[,c(12,2)], out_sCh10_s1200n$chain[,c(12,2)]), prior = PriorSample[,c(12,2)], singlePanel = FALSE, trueVals = SynParams[c(12,2)])
#MAP
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(12,2)], out_sCh10_2_s1200n$chain[,c(12,2)], out_sCh10_1_s1200n$chain[,c(12,2)], out_sCh10_s1200n$chain[,c(12,2)]), prior = PriorSample[,c(12,2)], singlePanel = FALSE, trueVals = matrix(MaxLikes[32,c(15,5)], ncol = 2))
#All Centers
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(12,2)], out_sCh10_2_s1200n$chain[,c(12,2)], out_sCh10_1_s1200n$chain[,c(12,2)], out_sCh10_s1200n$chain[,c(12,2)]), prior = PriorSample[,c(12,2)], singlePanel = FALSE, trueVals = t(apply(X = t(apply(X = CenPtClust8[,c(12,2)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(12,2)] - ParamRanges$Lower[c(12,2)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(12,2)])))
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(12,2)], out_sCh10_2_s1200n$chain[,c(12,2)], out_sCh10_1_s1200n$chain[,c(12,2)], out_sCh10_s1200n$chain[,c(12,2)]), prior = PriorSample[,c(12,2)], singlePanel = FALSE, trueVals = t(apply(X = t(apply(X = CenPtClust9[,c(12,2)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(12,2)] - ParamRanges$Lower[c(12,2)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(12,2)])))
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(12,2)], out_sCh10_2_s1200n$chain[,c(12,2)], out_sCh10_1_s1200n$chain[,c(12,2)], out_sCh10_s1200n$chain[,c(12,2)]), prior = PriorSample[,c(12,2)], singlePanel = FALSE, trueVals = t(apply(X = t(apply(X = CenPtClust18[,c(12,2)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(12,2)] - ParamRanges$Lower[c(12,2)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(12,2)])))
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(12,2)], out_sCh10_2_s1200n$chain[,c(12,2)], out_sCh10_1_s1200n$chain[,c(12,2)], out_sCh10_s1200n$chain[,c(12,2)]), prior = PriorSample[,c(12,2)], singlePanel = FALSE, trueVals = t(apply(X = t(apply(X = CenPtClust19[,c(12,2)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(12,2)] - ParamRanges$Lower[c(12,2)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(12,2)])))
#Scale normalization
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(12,2)], out_sCh10_2_s1200n$chain[,c(12,2)], out_sCh10_1_s1200n$chain[,c(12,2)], out_sCh10_s1200n$chain[,c(12,2)]), prior = PriorSample[,c(12,2)], singlePanel = FALSE, trueVals = t(apply(X = t(apply(X = CenPtClust8sp[,c(12,2)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(12,2)] - ParamRanges$Lower[c(12,2)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(12,2)])))
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(12,2)], out_sCh10_2_s1200n$chain[,c(12,2)], out_sCh10_1_s1200n$chain[,c(12,2)], out_sCh10_s1200n$chain[,c(12,2)]), prior = PriorSample[,c(12,2)], singlePanel = FALSE, trueVals = t(apply(X = t(apply(X = CenPtClust9sp[,c(12,2)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(12,2)] - ParamRanges$Lower[c(12,2)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(12,2)])))
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(12,2)], out_sCh10_2_s1200n$chain[,c(12,2)], out_sCh10_1_s1200n$chain[,c(12,2)], out_sCh10_s1200n$chain[,c(12,2)]), prior = PriorSample[,c(12,2)], singlePanel = FALSE, trueVals = t(apply(X = t(apply(X = CenPtClust18sp[,c(12,2)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(12,2)] - ParamRanges$Lower[c(12,2)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(12,2)])))
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(12,2)], out_sCh10_2_s1200n$chain[,c(12,2)], out_sCh10_1_s1200n$chain[,c(12,2)], out_sCh10_s1200n$chain[,c(12,2)]), prior = PriorSample[,c(12,2)], singlePanel = FALSE, trueVals = t(apply(X = t(apply(X = CenPtClust19sp[,c(12,2)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(12,2)] - ParamRanges$Lower[c(12,2)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(12,2)])))

#Combined
png('OptSamplesMarginalPlot_atm.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(12,2)], out_sCh10_2_s1200n$chain[,c(12,2)], out_sCh10_1_s1200n$chain[,c(12,2)], out_sCh10_s1200n$chain[,c(12,2)]), prior = PriorSample[,c(12,2)], singlePanel = FALSE, 
             trueVals = t(apply(X = t(apply(X = CenPtClust8sp[,c(12,2)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(12,2)] - ParamRanges$Lower[c(12,2)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(12,2)])),
             SynVals = SynParams[c(12,2)],
             MAPVals = matrix(MaxLikes[32,c(15,5)], ncol = 2),
             settings = list(col = c('gray',NA)))
dev.off()

png('OptSamplesMarginalPlot_atm_NoSamps.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(12,2)], out_sCh10_2_s1200n$chain[,c(12,2)], out_sCh10_1_s1200n$chain[,c(12,2)], out_sCh10_s1200n$chain[,c(12,2)]), prior = PriorSample[,c(12,2)], singlePanel = FALSE)
dev.off()

png('OptSamplesMarginalPlot_atm_NoSyn.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(12,2)], out_sCh10_2_s1200n$chain[,c(12,2)], out_sCh10_1_s1200n$chain[,c(12,2)], out_sCh10_s1200n$chain[,c(12,2)]), prior = PriorSample[,c(12,2)], singlePanel = FALSE, 
             trueVals = t(apply(X = t(apply(X = CenPtClust8sp[,c(12,2)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(12,2)] - ParamRanges$Lower[c(12,2)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(12,2)])),
             SynVals = NULL,
             MAPVals = matrix(MaxLikes[32,c(15,5)], ncol = 2))
dev.off()

#s9 Parameters----
#Synthetic
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_2_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_1_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)]), prior = PriorSample[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], singlePanel = FALSE, trueVals = SynParams[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)])
#MAP
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_2_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_1_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)]), prior = PriorSample[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], singlePanel = FALSE, trueVals = matrix(MaxLikes[32,(c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)+3)], ncol = 12))
#All Centers
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_2_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_1_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)]), prior = PriorSample[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], singlePanel = FALSE, trueVals = t(apply(X = t(apply(X = CenPtClust8[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)] - ParamRanges$Lower[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)])))
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_2_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_1_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)]), prior = PriorSample[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], singlePanel = FALSE, trueVals = t(apply(X = t(apply(X = CenPtClust9[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)] - ParamRanges$Lower[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)])))
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_2_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_1_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)]), prior = PriorSample[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], singlePanel = FALSE, trueVals = t(apply(X = t(apply(X = CenPtClust18[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)] - ParamRanges$Lower[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)])))
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_2_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_1_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)]), prior = PriorSample[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], singlePanel = FALSE, trueVals = t(apply(X = t(apply(X = CenPtClust19[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)] - ParamRanges$Lower[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)])))
#Scale normalization
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_2_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_1_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)]), prior = PriorSample[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], singlePanel = FALSE, trueVals = t(apply(X = t(apply(X = CenPtClust8sp[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)] - ParamRanges$Lower[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)])))
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_2_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_1_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)]), prior = PriorSample[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], singlePanel = FALSE, trueVals = t(apply(X = t(apply(X = CenPtClust9sp[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)] - ParamRanges$Lower[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)])))
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_2_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_1_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)]), prior = PriorSample[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], singlePanel = FALSE, trueVals = t(apply(X = t(apply(X = CenPtClust18sp[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)] - ParamRanges$Lower[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)])))
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_2_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_1_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)]), prior = PriorSample[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], singlePanel = FALSE, trueVals = t(apply(X = t(apply(X = CenPtClust19sp[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)] - ParamRanges$Lower[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)])))

png('OptSamplesMarginalPlot_s9.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_2_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_1_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], out_sCh10_s1200n$chain[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)]), prior = PriorSample[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], singlePanel = FALSE, 
             trueVals = t(apply(X = t(apply(X = CenPtClust8sp[,c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)] - ParamRanges$Lower[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)])),
             SynVals = SynParams[c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)],
             MAPVals = matrix(MaxLikes[32,(c(3, 8, 9, 15, 4, 5, 10, 11, 6, 7, 13, 14)+3)], ncol = 12),
             settings = list(col = c('gray',NA)))
dev.off()

png('OptSamplesMarginalPlot_s9_Sel_NoSyn.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(4,13)], out_sCh10_2_s1200n$chain[,c(4,13)], out_sCh10_1_s1200n$chain[,c(4,13)], out_sCh10_s1200n$chain[,c(4,13)]), prior = PriorSample[,c(4,13)], singlePanel = FALSE, 
             trueVals = t(apply(X = t(apply(X = CenPtClust8sp[,c(4,13)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(4,13)] - ParamRanges$Lower[c(4,13)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(4,13)])),
             SynVals = NULL,
             MAPVals = matrix(MaxLikes[32,c(7,16)], ncol = 2))
dev.off()

png('OptSamplesMarginalPlot_s9_Sel_NoSamps.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(4,13)], out_sCh10_2_s1200n$chain[,c(4,13)], out_sCh10_1_s1200n$chain[,c(4,13)], out_sCh10_s1200n$chain[,c(4,13)]), prior = PriorSample[,c(4,13)], singlePanel = FALSE, 
             trueVals = NULL,
             SynVals = NULL,
             MAPVals = NULL)
dev.off()

#Veg Params----
#Synthetic
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(1, 16:19)], out_sCh10_2_s1200n$chain[,c(1, 16:19)], out_sCh10_1_s1200n$chain[,c(1, 16:19)], out_sCh10_s1200n$chain[,c(1, 16:19)]), prior = PriorSample[,c(1, 16:19)], singlePanel = FALSE, trueVals = SynParams[c(1, 16:19)])
#MAP
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(1, 16:19)], out_sCh10_2_s1200n$chain[,c(1, 16:19)], out_sCh10_1_s1200n$chain[,c(1, 16:19)], out_sCh10_s1200n$chain[,c(1, 16:19)]), prior = PriorSample[,c(1, 16:19)], singlePanel = FALSE, trueVals = matrix(MaxLikes[32,(c(1, 16:19)+3)], ncol = 5))
#All Centers
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(1, 16:19)], out_sCh10_2_s1200n$chain[,c(1, 16:19)], out_sCh10_1_s1200n$chain[,c(1, 16:19)], out_sCh10_s1200n$chain[,c(1, 16:19)]), prior = PriorSample[,c(1, 16:19)], singlePanel = FALSE, trueVals = t(apply(X = t(apply(X = CenPtClust8[,c(1, 16:19)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(1, 16:19)] - ParamRanges$Lower[c(1, 16:19)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(1, 16:19)])))
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(1, 16:19)], out_sCh10_2_s1200n$chain[,c(1, 16:19)], out_sCh10_1_s1200n$chain[,c(1, 16:19)], out_sCh10_s1200n$chain[,c(1, 16:19)]), prior = PriorSample[,c(1, 16:19)], singlePanel = FALSE, trueVals = t(apply(X = t(apply(X = CenPtClust9[,c(1, 16:19)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(1, 16:19)] - ParamRanges$Lower[c(1, 16:19)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(1, 16:19)])))
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(1, 16:19)], out_sCh10_2_s1200n$chain[,c(1, 16:19)], out_sCh10_1_s1200n$chain[,c(1, 16:19)], out_sCh10_s1200n$chain[,c(1, 16:19)]), prior = PriorSample[,c(1, 16:19)], singlePanel = FALSE, trueVals = t(apply(X = t(apply(X = CenPtClust18[,c(1, 16:19)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(1, 16:19)] - ParamRanges$Lower[c(1, 16:19)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(1, 16:19)])))
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(1, 16:19)], out_sCh10_2_s1200n$chain[,c(1, 16:19)], out_sCh10_1_s1200n$chain[,c(1, 16:19)], out_sCh10_s1200n$chain[,c(1, 16:19)]), prior = PriorSample[,c(1, 16:19)], singlePanel = FALSE, trueVals = t(apply(X = t(apply(X = CenPtClust19[,c(1, 16:19)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(1, 16:19)] - ParamRanges$Lower[c(1, 16:19)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(1, 16:19)])))
#Scale normalization
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(1, 16:19)], out_sCh10_2_s1200n$chain[,c(1, 16:19)], out_sCh10_1_s1200n$chain[,c(1, 16:19)], out_sCh10_s1200n$chain[,c(1, 16:19)]), prior = PriorSample[,c(1, 16:19)], singlePanel = FALSE, trueVals = t(apply(X = t(apply(X = CenPtClust8sp[,c(1, 16:19)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(1, 16:19)] - ParamRanges$Lower[c(1, 16:19)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(1, 16:19)])))
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(1, 16:19)], out_sCh10_2_s1200n$chain[,c(1, 16:19)], out_sCh10_1_s1200n$chain[,c(1, 16:19)], out_sCh10_s1200n$chain[,c(1, 16:19)]), prior = PriorSample[,c(1, 16:19)], singlePanel = FALSE, trueVals = t(apply(X = t(apply(X = CenPtClust9sp[,c(1, 16:19)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(1, 16:19)] - ParamRanges$Lower[c(1, 16:19)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(1, 16:19)])))
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(1, 16:19)], out_sCh10_2_s1200n$chain[,c(1, 16:19)], out_sCh10_1_s1200n$chain[,c(1, 16:19)], out_sCh10_s1200n$chain[,c(1, 16:19)]), prior = PriorSample[,c(1, 16:19)], singlePanel = FALSE, trueVals = t(apply(X = t(apply(X = CenPtClust18sp[,c(1, 16:19)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(1, 16:19)] - ParamRanges$Lower[c(1, 16:19)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(1, 16:19)])))
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(1, 16:19)], out_sCh10_2_s1200n$chain[,c(1, 16:19)], out_sCh10_1_s1200n$chain[,c(1, 16:19)], out_sCh10_s1200n$chain[,c(1, 16:19)]), prior = PriorSample[,c(1, 16:19)], singlePanel = FALSE, trueVals = t(apply(X = t(apply(X = CenPtClust19sp[,c(1, 16:19)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(1, 16:19)] - ParamRanges$Lower[c(1, 16:19)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(1, 16:19)])))

png('OptSamplesMarginalPlot_veg.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(1, 16:19)], out_sCh10_2_s1200n$chain[,c(1, 16:19)], out_sCh10_1_s1200n$chain[,c(1, 16:19)], out_sCh10_s1200n$chain[,c(1, 16:19)]), prior = PriorSample[,c(1, 16:19)], singlePanel = FALSE, 
             trueVals = t(apply(X = t(apply(X = CenPtClust8sp[,c(1, 16:19)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(1, 16:19)] - ParamRanges$Lower[c(1, 16:19)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(1, 16:19)])),
             SynVals = SynParams[c(1, 16:19)],
             MAPVals = matrix(MaxLikes[32,(c(1, 16:19)+3)], ncol = 5),
             settings = list(col = c('gray',NA)))
dev.off()

#All params on one plot----
key_colnames = c('H: GW Loss Coef.', 'Z: Windspeed', 
                 'Su: Ksat', 'Su: Soil Thickness', 'Su: m', 'Su: Air Entry Pres.', 'Su: Sat. to GW Coef.', 'Su: vKsat',
                 'Sd: Ksat', 'Sd: Soil Thickness', 'Sd: m', 'Sd: Pore Size', 'Sd: Air Entry Pres.', 
                 'Sd: Sat. to GW Coef.', 'Sd: vKsat', 
                 'L: Septic Water Load', 'V: Tree Max. Stomatal Cond.', 'V: Tree Stomatal Fraction', 
                 'V: Tree Rainwater Capacity', "LP", "LL", "LPr")

col_order = c(16, 1, 9:11, 13:15, 12, 3:8, 2, 17:22)

for (i in 1:length(out_sCh10_s1200n$chain)){
  colnames(out_sCh10_3_s1200n$chain[[i]]) = key_colnames
  colnames(out_sCh10_2_s1200n$chain[[i]]) = key_colnames
  colnames(out_sCh10_1_s1200n$chain[[i]]) = key_colnames
  colnames(out_sCh10_s1200n$chain[[i]]) = key_colnames
  #rearrange for plotting
  out_sCh10_3_s1200n$chain[[i]] = out_sCh10_3_s1200n$chain[[i]][,col_order]
  out_sCh10_2_s1200n$chain[[i]] = out_sCh10_2_s1200n$chain[[i]][,col_order]
  out_sCh10_1_s1200n$chain[[i]] = out_sCh10_1_s1200n$chain[[i]][,col_order]
  out_sCh10_s1200n$chain[[i]] = out_sCh10_s1200n$chain[[i]][,col_order]
}

png('OptSamplesMarginalPlot_all-1.png', res = 600, units = 'in', width = 8, height = 8)
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(1:16)], out_sCh10_2_s1200n$chain[,c(1:16)], out_sCh10_1_s1200n$chain[,c(1:16)], out_sCh10_s1200n$chain[,c(1:16)]), prior = PriorSample[,col_order[1:16]], singlePanel = FALSE, 
             trueVals = t(apply(X = t(apply(X = CenPtClust8sp[,col_order[1:16]], MARGIN = 1, FUN = "*", (ParamRanges$Upper[col_order[1:16]] - ParamRanges$Lower[col_order[1:16]]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[col_order[1:16]])),
             SynVals = SynParams[col_order[1:16]],
             MAPVals = matrix(MaxLikes[32,(col_order[1:16]+3)], ncol = 16),
             settings = list(col = c('gray',NA)))
dev.off()
png('OptSamplesMarginalPlot_all-2.png', res = 600, units = 'in', width = 8, height = 8)
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(5:19)], out_sCh10_2_s1200n$chain[,c(5:19)], out_sCh10_1_s1200n$chain[,c(5:19)], out_sCh10_s1200n$chain[,c(5:19)]), prior = PriorSample[,col_order[5:19]], singlePanel = FALSE, 
             trueVals = t(apply(X = t(apply(X = CenPtClust8sp[,col_order[5:19]], MARGIN = 1, FUN = "*", (ParamRanges$Upper[col_order[5:19]] - ParamRanges$Lower[col_order[5:19]]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[col_order[5:19]])),
             SynVals = SynParams[col_order[5:19]],
             MAPVals = matrix(MaxLikes[32,(col_order[5:19]+3)], ncol = 15),
             settings = list(col = c('gray',NA)))
dev.off()

#with parameter set labels
png('OptSamplesMarginalPlot_all-1_params.png', res = 600, units = 'in', width = 8, height = 8)
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(1:16)], out_sCh10_2_s1200n$chain[,c(1:16)], out_sCh10_1_s1200n$chain[,c(1:16)], out_sCh10_s1200n$chain[,c(1:16)]), prior = PriorSample[,col_order[1:16]], singlePanel = FALSE, 
             trueVals = t(apply(X = t(apply(X = CenPtClust8sp[,col_order[1:16]], MARGIN = 1, FUN = "*", (ParamRanges$Upper[col_order[1:16]] - ParamRanges$Lower[col_order[1:16]]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[col_order[1:16]])),
             SynVals = SynParams[col_order[1:16]],
             MAPVals = matrix(MaxLikes[32,(col_order[1:16]+3)], ncol = 16),
             settings = list(col = c('gray',NA)),
             paramText = TRUE)
dev.off()
png('OptSamplesMarginalPlot_all-2_params.png', res = 600, units = 'in', width = 8, height = 8)
marginalPlot(x = c(out_sCh10_3_s1200n$chain[,c(17:19)], out_sCh10_2_s1200n$chain[,c(17:19)], out_sCh10_1_s1200n$chain[,c(17:19)], out_sCh10_s1200n$chain[,c(17:19)]), prior = PriorSample[,c(17:19)], singlePanel = FALSE, 
             trueVals = t(apply(X = t(apply(X = CenPtClust8sp[,c(17:19)], MARGIN = 1, FUN = "*", (ParamRanges$Upper[c(17:19)] - ParamRanges$Lower[c(17:19)]))), MARGIN = 1, FUN = "+", ParamRanges$Lower[c(17:19)])),
             SynVals = SynParams[c(17:19)],
             MAPVals = matrix(MaxLikes[32,(c(17:19)+3)], ncol = 3),
             settings = list(col = c('gray',NA)),
             paramText = TRUE)
dev.off()

#with likelihood params----
Likes_sCh10_400 = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration/SynCh10/NewCR/ParamsLikes_c_s400.csv", stringsAsFactors = FALSE)
Likes_sCh10_800 = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration/SynCh10/NewCR/ParamsLikes_c_s800.csv", stringsAsFactors = FALSE)
Likes_sCh10_1200 = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration/SynCh10/NewCR/ParamsLikes_c_s1200.csv", stringsAsFactors = FALSE)
Likes_sCh10 = rbind(Likes_sCh10_400, Likes_sCh10_800, Likes_sCh10_1200)

Likes_sCh10_1_400 = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration/SynCh10-1/NewCR/ParamsLikes_c_s400.csv", stringsAsFactors = FALSE)
Likes_sCh10_1_800 = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration/SynCh10-1/NewCR/ParamsLikes_c_s800.csv", stringsAsFactors = FALSE)
Likes_sCh10_1_1200 = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration/SynCh10-1/NewCR/ParamsLikes_c_s1200.csv", stringsAsFactors = FALSE)
Likes_sCh10_1 = rbind(Likes_sCh10_1_400, Likes_sCh10_1_800, Likes_sCh10_1_1200)

Likes_sCh10_2_400 = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration/SynCh10-2/NewCR/ParamsLikes_c_s400.csv", stringsAsFactors = FALSE)
Likes_sCh10_2_800 = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration/SynCh10-2/NewCR/ParamsLikes_c_s800.csv", stringsAsFactors = FALSE)
Likes_sCh10_2_1200 = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration/SynCh10-2/NewCR/ParamsLikes_c_s1200.csv", stringsAsFactors = FALSE)
Likes_sCh10_2 = rbind(Likes_sCh10_2_400, Likes_sCh10_2_800, Likes_sCh10_2_1200)

Likes_sCh10_3_400 = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration/SynCh10-3/NewCR/ParamsLikes_c_s400.csv", stringsAsFactors = FALSE)
Likes_sCh10_3_800 = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration/SynCh10-3/NewCR/ParamsLikes_c_s800.csv", stringsAsFactors = FALSE)
Likes_sCh10_3_1200 = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration/SynCh10-3/NewCR/ParamsLikes_c_s1200.csv", stringsAsFactors = FALSE)
Likes_sCh10_3 = rbind(Likes_sCh10_3_400, Likes_sCh10_3_800, Likes_sCh10_3_1200)

Likes = rbind(Likes_sCh10, Likes_sCh10_1, Likes_sCh10_2, Likes_sCh10_3)

png('HistsLikeParams.png', res = 300, width = 10, height = 10, units = 'in')
par(mar = c(2,4.1,3,1))
layout(rbind(c(1,2), c(3,4), c(5,6)))
hist(Likes$beta_Q, breaks = 50, freq = TRUE, main = 'Kurtosis (Beta)', xlab = '', xlim = c(-1,7), ylab = '')
lines(x = c(0,0), y = c(0,100000), lwd = 2, lty = 3)
axis(side = 1, at = 0, tick = TRUE, labels = NA, lwd.ticks = 2)
for(i in 1:nrow(CenPtClust8s)){
  lines(x = c(CenPtClust8s$beta[i],CenPtClust8s$beta[i]), y = c(0,100000), lwd = 2, lty = 3, col = 'skyblue')
}
lines(x = c(Likes[which(Likes$ID == 'DCh10-3_R997_C2'),]$beta,Likes[which(Likes$ID == 'DCh10-3_R997_C2'),]$beta), 
      y = c(0,100000), lwd = 2, lty = 4, col = 'blue')
axis(side = 1, at = 1, tick = TRUE, labels = NA, lwd.ticks = 2, col.ticks = 'blue')

hist(Likes$xi_Q, breaks = 50, freq = TRUE, main = 'Skewness (Xi)', xlab = '', xlim = c(0,5), ylab = '')
lines(x = c(1,1), y = c(0,100000), lwd = 2, lty = 3)
axis(side = 1, at = 1, tick = TRUE, labels = NA, lwd.ticks = 2)
for(i in 1:nrow(CenPtClust8s)){
  lines(x = c(CenPtClust8s$xi[i],CenPtClust8s$xi[i]), y = c(0,100000), lwd = 2, lty = 3, col = 'skyblue')
}
lines(x = c(Likes[which(Likes$ID == 'DCh10-3_R997_C2'),]$xi,Likes[which(Likes$ID == 'DCh10-3_R997_C2'),]$xi),
      y = c(0,100000), lwd = 2, lty = 4, col = 'blue')
axis(side = 1, at = 1, tick = TRUE, labels = NA, lwd.ticks = 2, col.ticks = 'blue')

hist(Likes$sigma_0_Q, breaks = 50, freq = TRUE, main = 'Standard Deviation when Mean = 0 (sigma_0)', xlab = '', xlim = c(0,1))
lines(x = c(0.05,0.05), y = c(0,100000), lwd = 2, lty = 3)
axis(side = 1, at = 0.05, tick = TRUE, labels = NA, lwd.ticks = 2)
for(i in 1:nrow(CenPtClust8s)){
  lines(x = c(CenPtClust8s$sigma_0[i],CenPtClust8s$sigma_0[i]), y = c(0,100000), lwd = 2, lty = 3, col = 'skyblue')
}
lines(x = c(Likes[which(Likes$ID == 'DCh10-3_R997_C2'),]$sigma_0,Likes[which(Likes$ID == 'DCh10-3_R997_C2'),]$sigma_0), 
      y = c(0,100000), lwd = 2, lty = 4, col = 'blue')
axis(side = 1, at = 1, tick = TRUE, labels = NA, lwd.ticks = 2, col.ticks = 'blue')

hist(Likes$sigma_1_Q, breaks = 50, freq = TRUE, main = 'Linear Change in Std. Dev. with Mean (sigma_1)', xlab = '', xlim = c(0,1), ylab = '')
lines(x = c(0,0), y = c(0,100000), lwd = 2, lty = 3)
axis(side = 1, at = 0, tick = TRUE, labels = NA, lwd.ticks = 2)
for(i in 1:nrow(CenPtClust8s)){
  lines(x = c(CenPtClust8s$sigma_1[i],CenPtClust8s$sigma_1[i]), y = c(0,100000), lwd = 2, lty = 3, col = 'skyblue')
}
lines(x = c(Likes[which(Likes$ID == 'DCh10-3_R997_C2'),]$sigma_1,Likes[which(Likes$ID == 'DCh10-3_R997_C2'),]$sigma_1), 
      y = c(0,100000), lwd = 2, lty = 4, col = 'blue')
axis(side = 1, at = 1, tick = TRUE, labels = NA, lwd.ticks = 2, col.ticks = 'blue')

hist(Likes$phi_1_Q, breaks = 50, freq = TRUE, main = 'Lag 1 Autocorrelation (phi_1)', xlab = '', xlim = c(0,1), ylab = '')
lines(x = c(0.7,0.7), y = c(0,100000), lwd = 2, lty = 3)
axis(side = 1, at = 0.7, tick = TRUE, labels = NA, lwd.ticks = 2)
for(i in 1:nrow(CenPtClust8s)){
  lines(x = c(CenPtClust8s$phi_1[i],CenPtClust8s$phi_1[i]), y = c(0,100000), lwd = 2, lty = 3, col = 'skyblue')
}
lines(x = c(Likes[which(Likes$ID == 'DCh10-3_R997_C2'),]$phi_1,Likes[which(Likes$ID == 'DCh10-3_R997_C2'),]$phi_1), 
      y = c(0,100000), lwd = 2, lty = 4, col = 'blue')
axis(side = 1, at = 1, tick = TRUE, labels = NA, lwd.ticks = 2, col.ticks = 'blue')

hist(Likes$mu_h_Q, breaks = 50, freq = TRUE, main = 'Mean Bias Factor (muh)', xlab = '', xlim = c(0,1), ylab = '')
lines(x = c(0,0), y = c(0,100000), lwd = 2, lty = 3)
axis(side = 1, at = 0, tick = TRUE, labels = NA, lwd.ticks = 2)
for(i in 1:nrow(CenPtClust8s)){
  lines(x = c(CenPtClust8s$mu_h[i],CenPtClust8s$mu_h[i]), y = c(0,100000), lwd = 2, lty = 3, col = 'skyblue')
}
lines(x = c(Likes[which(Likes$ID == 'DCh10-3_R997_C2'),]$mu_h,Likes[which(Likes$ID == 'DCh10-3_R997_C2'),]$mu_h), 
      y = c(0,100000), lwd = 2, lty = 4, col = 'blue')
axis(side = 1, at = 1, tick = TRUE, labels = NA, lwd.ticks = 2, col.ticks = 'blue')

dev.off()


#Create weights for each of the selected parameters based on their likelihoods----
CenPtClust8s$weight = CenPtClust8s$LP/sum(CenPtClust8s$LP, MaxLikes[32,23])
MaxLikeWeight = MaxLikes[32,23]/sum(CenPtClust8s$LP, MaxLikes[32,23])

#MAP is parameter set 1, and then sets 2-9 are as ordered in CenPtClust8s$ID
#"DCh10-1_R600_C5"  "DCh10-2_R630_C10" "DCh10_R695_C5"    "DCh10-2_R773_C7"  "DCh10-3_R1199_C8" "DCh10-1_R363_C7"  "DCh10_R263_C1"   "DCh10-1_R364_C5"

#Posterior Predictive Checks----
#Generate data from the parameter joint posterior distribution
# compare to the calibrated observations (X and yRep), and/or to future values (XTilde and yTilde)
# The true distribution of y should be similar to that of yRep.
#  Look at density overlay of y and yRep for each of the parameter sets

# Create a prediction function
#createPredictions <- function(par){
  # set the parameters that are not calibrated on default values 
#  x = refPars$best
#  x[parSel] = par
#  predicted <- VSEM(x[1:11], PAR) # replace here VSEM with your model 
#  return(predicted[,1] * 1000)
#}

# Create an error function
#createError <- function(mean, par){
#  return(rnorm(length(mean), mean = mean, sd = par[7]))
#}

# plot prior predictive distribution and prior predictive simulations
#plotTimeSeriesResults(sampler = out, model = createPredictions, observed = obs[,1],
#                      error = createError, prior = TRUE, main = "Prior predictive")

# plot posterior predictive distribution and posterior predictive simulations
#plotTimeSeriesResults(sampler = out, model = createPredictions, observed = obs[,1],
#                      error = createError, main = "Posterior predictive")

#% of observations within %-level prediction intervals
#autocorrelation for time series data 
#Bayesian p-values for evaluating model fit over the entire posterior distribution

#Credible intervals as barplots and as areas (mcmc_areas and mcmc_intervals)

#Plot timeseries Overlays----
#Synthetic with error - used for calibration
TestQ =  read.table("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\SyntheticDataCreation\\QSyn910_1.txt", header = TRUE, sep = '\t')
#No error - what we hope RHESSys looks like
SynObs = read.table("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\SyntheticDataCreation\\Q_NoErr_1.txt", header = TRUE, sep = '\t')

#Load Likelihoods----
Likes = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration/SynCh10/LikeParamsQ_c_s400.csv", stringsAsFactors = FALSE)

#Load simulated streamflow----
SimQ = read.table("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration/SynCh10/Q_c_s400.txt", stringsAsFactors = FALSE, sep = '\t', header = TRUE)
#Trim flows to only the top likelihoods
SimQ_topL = SimQ[,which(colnames(SimQ) %in% paste0('Run',Likes$Replicate[Likes$logL > 5000], '_Ch', Likes$Chain[Likes$logL > 5000]))]

#Plot the simulated flows
matplotDates(x = as.Date(SynObs$Date), y = SimQ_topL, type = 'l', col = gray(level = 0.7, alpha = 0.2), ylim = c(0.1,15), xlab = 'Year', ylab = 'Streamflow (cfs)', xlim = c(as.Date('2004-01-01'), as.Date('2014-01-01')), log = 'y')
par(new = TRUE)
#Plot median of those flows
plot(x = as.Date(SynObs$Date), y = apply(SimQ_topL, MARGIN = 1, FUN = median), type = 'l', col = 'red', ylim = c(0.1,15), axes = FALSE, xlab = '', ylab = '', xlim = c(as.Date('2004-01-01'), as.Date('2014-01-01')), log = 'y')
par(new = TRUE)
#Plot observed flows
plot(x = as.Date(SynObs$Date), y = SynObs$streamflow, type = 'l', ylim = c(0.1,15), axes = FALSE, xlab = '', ylab = '', xlim = c(as.Date('2004-01-01'), as.Date('2014-01-01')), log = 'y')
par(new=TRUE)
#Plot what was used for calibration timeseries with error to previous
plot(x = as.Date(SynObs$Date), y = TestQ$Flow, type = 'l', col = 'blue', ylim = c(0.1,15), axes = FALSE, xlab = '', ylab = '', xlim = c(as.Date('2004-01-01'), as.Date('2014-01-01')), log = 'y')

#Plot median vs. observed
plot(x = SynObs$streamflow, y = apply(SimQ_topL, MARGIN = 1, FUN = median), pch = 16, xlim = c(0,15), ylim = c(0,15))
lines(x = c(0,15), y = c(0,15), col = 'red')

#Plot mean vs. observed
plot(x = SynObs$streamflow, y = apply(SimQ_topL, MARGIN = 1, FUN = mean), pch = 16, xlim = c(0,15), ylim = c(0,15))
lines(x = c(0,15), y = c(0,15), col = 'red')


#Plot log-likelihood for these flows
png('LNSE.png', res = 200, height = 7, width = 7, units = 'in')
layout(rbind(c(1,2), c(3,4)))
hist(Likes$LNSEd[Likes$logL > 5000], xlim = c(0.75,1), main = 'LNSEd', xlab = 'LNSE')
hist(Likes$LNSEw[Likes$logL > 5000], xlim = c(0.75,1), main = 'LNSEw', xlab = 'LNSE')
hist(Likes$LNSEm[Likes$logL > 5000], xlim = c(0.75,1), main = 'LNSEm', xlab = 'LNSE')
hist(Likes$LNSEa[Likes$logL > 5000], xlim = c(0.75,1), main = 'LNSEa', xlab = 'LNSE')
dev.off()