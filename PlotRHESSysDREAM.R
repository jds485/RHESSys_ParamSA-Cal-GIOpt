tic_Script_plot = Sys.time()

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

#Set directories----
dir_Ch39 = "C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration\\Ch39"
dir_Ch10 = "C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration\\Ch10"
dir_Ch10_1 = "C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration\\Ch10-1"
dir_Ch10_2 = "C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration\\Ch10-2"
dir_Ch10_3 = "C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Calibration\\Ch10-3"

#Random seed----
set.seed(2077)

#39 Chains----
setwd(dir_Ch39)
load(file = paste0(dir_Ch39, '/OutputWorkspace.RData'), verbose = FALSE)
rm(cl, Info, startValues, i, tic_Script)

# Plot and summarize output----
#Fixme: this requires cores to be setup to run in parallel, with all of the exported variables.
#summary(out_Parext)

#print(out_Parext_Restart$settings$runtime)
#summary(out_Parext_Restart)
#print(out_Parext_Restart_1$settings$runtime)
#summary(out_Parext_Restart_1)

#Useful to see the prior and posterior on same plot.
PriorSample = prior$sampler(n = 50000)
png('marginalplot_39_1-16.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[,1:16], prior = PriorSample[,1:16])
dev.off()
png('marginalplot_39_17-32.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[,17:32], prior = PriorSample[,17:32])
dev.off()
png('marginalplot_39_33-42.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[,33:42], prior = PriorSample[,33:42])
dev.off()

#Only for last 50 chain steps
png('marginalplot_39_1-16_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[51:100,1:16], prior = PriorSample[,1:16])
dev.off()
png('marginalplot_39_17-32_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[51:100,17:32], prior = PriorSample[,17:32])
dev.off()
png('marginalplot_39_33-42_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[51:100,33:42], prior = PriorSample[,33:42])
dev.off()

#scatterplot matrix (mcmc_pairs in bayesplot is alternative)
#Not sure how is best to view this scatterplot matrix because there are too many parameters to view in one grid.
#png('corplot_39.png', res = 300, units = 'in', width = 14, height = 14)
#correlationPlot(mat = out_Parext, scaleCorText = TRUE)
#dev.off()

#png('pairs_39.png', res = 300, units = 'in', width = 10, height = 10)
#mcmc_pairs(out_Parext$chain[,1:10])
#dev.off()

#Trace plots with posterior density for all parameter values - chains should look similar, not get stuck
png('traceplot_39_1-4.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_39_5-8.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_39_9-12.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_39_13-16.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_39_17-20.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_39_21-24.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_39_25-28.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_39_29-32.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_39_33-36.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_39_37-40.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_39_41-42.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,41:42], smooth = FALSE)
dev.off()

#mcmc_trace in bayesplot as alternative traceplots
png('traceplot_39_1-9_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,1:9])
dev.off()
png('traceplot_39_10-18_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,10:18])
dev.off()
png('traceplot_39_19-27_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,19:27])
dev.off()
png('traceplot_39_28-36_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,28:36])
dev.off()
png('traceplot_39_37-42_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,37:42])
dev.off()

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
png('AutocorrPlot_39_1-9.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,1:9], lag.max = 10)
dev.off()
png('AutocorrPlot_39_10-18.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,10:18], lag.max = 10)
dev.off()
png('AutocorrPlot_39_19-27.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,19:27], lag.max = 10)
dev.off()
png('AutocorrPlot_39_28-36.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,28:36], lag.max = 10)
dev.off()
png('AutocorrPlot_39_37-42.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,37:42], lag.max = 10)
dev.off()

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
# png('AutocorrPlot_39_37-42_l50.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,37:42], lag.max = 10)
# dev.off()


#Stationarity of chain - Geweke
#Computed for individual chains
png('GewekePlot_39_1-9.png', res = 300, units = 'in', width = 7, height = 7)
coda::geweke.plot(out_Parext$chain[,1:9])
dev.off()
png('GewekePlot_39_10-18.png', res = 300, units = 'in', width = 7, height = 7)
coda::geweke.plot(out_Parext$chain[,10:18])
dev.off()
png('GewekePlot_39_19-27.png', res = 300, units = 'in', width = 7, height = 7)
coda::geweke.plot(out_Parext$chain[,19:27])
dev.off()
png('GewekePlot_39_28-36.png', res = 300, units = 'in', width = 7, height = 7)
coda::geweke.plot(out_Parext$chain[,28:36])
dev.off()
png('GewekePlot_39_37-42.png', res = 300, units = 'in', width = 7, height = 7)
coda::geweke.plot(out_Parext$chain[,37:42])
dev.off()

#need at least 50 chain steps for this
#gelman.plot(x, bin.width = 10, max.bins = 50, confidence = 0.95, transform = FALSE, autoburnin=TRUE, auto.layout = TRUE, ask, col, lty, xlab, ylab, type, ...)

#Parallel coordinate plot - including all parameters is a bit much.
png('parcoord_39.png', res = 300, units = 'in', width = 10, height = 5)
mcmc_parcoord(outParext$chain, transform = function(x) {(x - min(x)) / (max(x)-min(x))})
dev.off()

#Density overlay (mcmc_dens_overlay) - all chains should have similar densities for each variable
png('densOverlay_39.png', res = 300, units = 'in', width = 10, height = 10)
mcmc_dens_overlay(out_Parext$chain)
dev.off()

#Rejection Rate
Reject_Ch39 = coda::rejectionRate(out_Parext$chain)

#Highest Posterior Density Interval
HPDs_Ch39 = coda::HPDinterval(out_Parext$chain)
HPDs_Ch39_bt = BayesianTools::getCredibleIntervals(out_Parext$chain[[1]])

#Effective sample size for each parameter should be similar
EffSize_Ch39 = coda::effectiveSize(out_Parext$chain)

# Accept/Reject rates for each chain----
AR = read.table('AcceptReject_c.txt', header=TRUE)
ARrates = vector('numeric', length = ncol(AR))
for (i in 1:ncol(AR)){
  ARrates[i] = sum(AR[,i])/nrow(AR)
}
png('AcceptRejectHist.png', res = 200, height=5, width=5, units='in')
hist(ARrates, xlim=c(0,1))
dev.off()

#Fixme: Check for runs of 0s in chains

#Likelihoods----
LQ = read.csv('LikeParamsQ_c.csv', stringsAsFactors = FALSE)
LTN = read.csv('LikeParamsTN_c.csv', stringsAsFactors = FALSE)
#Read observation timeseries
obsQ = read.table("C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\obs\\BaismanStreamflow_Feb2020Revised_Cal.txt", stringsAsFactors = FALSE, sep = '\t', header=TRUE)
obsQ = obsQ[which((as.Date(obsQ$Date) >= as.Date('2004-10-01')) & (as.Date(obsQ$Date) <= as.Date('2013-09-30'))),]
obsTN = read.table("C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\obs\\TN_Feb2020Revised_Cal.txt", stringsAsFactors = FALSE, sep = '\t', header=TRUE)

#Fixme: histogram from chains, not from csv files.
hist(LQ$logL/nrow(obsQ))

#Fixme: Parameter histograms for likelihoods from AnalyzeLikelihoods script

#10 Chains----
setwd(dir_Ch10)
load(file = paste0(dir_Ch10, '/OutputWorkspace-10Ch.RData'), verbose = FALSE)
rm(cl, Info, startValues, i, tic_Script)

# Plot and summarize output----
#Fixme: this requires cores to be setup to run in parallel, with all of the exported variables.
#summary(out_Parext)

#print(out_Parext_Restart$settings$runtime)
#summary(out_Parext_Restart)
#print(out_Parext_Restart_1$settings$runtime)
#summary(out_Parext_Restart_1)

#Useful to see the prior and posterior on same plot.
PriorSample = prior$sampler(n = 50000)
png('marginalplot_10_1-16.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[,1:16], prior = PriorSample[,1:16])
dev.off()
png('marginalplot_10_17-32.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[,17:32], prior = PriorSample[,17:32])
dev.off()
png('marginalplot_10_33-42.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[,33:42], prior = PriorSample[,33:42])
dev.off()

#Only for last 50 chain steps
png('marginalplot_10_1-16_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[51:100,1:16], prior = PriorSample[,1:16])
dev.off()
png('marginalplot_10_17-32_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[51:100,17:32], prior = PriorSample[,17:32])
dev.off()
png('marginalplot_10_33-42_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[51:100,33:42], prior = PriorSample[,33:42])
dev.off()

#scatterplot matrix (mcmc_pairs in bayesplot is alternative)
#Not sure how is best to view this scatterplot matrix because there are too many parameters to view in one grid.
#png('corplot_10.png', res = 300, units = 'in', width = 14, height = 14)
#correlationPlot(mat = out_Parext, scaleCorText = TRUE)
#dev.off()

#png('pairs_10.png', res = 300, units = 'in', width = 10, height = 10)
#mcmc_pairs(out_Parext$chain[,1:10])
#dev.off()

#Trace plots with posterior density for all parameter values - chains should look similar, not get stuck
png('traceplot_10_1-4.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,41:42], smooth = FALSE)
dev.off()

#mcmc_trace in bayesplot as alternative traceplots
png('traceplot_10_1-9_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,1:9])
dev.off()
png('traceplot_10_10-18_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,10:18])
dev.off()
png('traceplot_10_19-27_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,19:27])
dev.off()
png('traceplot_10_28-36_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,28:36])
dev.off()
png('traceplot_10_37-42_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,37:42])
dev.off()

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
png('AutocorrPlot_10_37-42.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,37:42], lag.max = 10)
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
# png('AutocorrPlot_10_37-42_l50.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,37:42], lag.max = 10)
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
# png('GewekePlot_10_37-42.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Parext$chain[,37:42])
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
AR_Ch10 = read.table('AcceptReject_c.txt', header=TRUE)
ARrates_Ch10 = vector('numeric', length = ncol(AR_Ch10))
for (i in 1:ncol(AR_Ch10)){
  ARrates_Ch10[i] = sum(AR_Ch10[,i])/nrow(AR_Ch10)
}
png('AcceptRejectHist.png', res = 200, height=5, width=5, units='in')
hist(ARrates_Ch10, xlim=c(0,1))
dev.off()

#Fixme: Check for runs of 0s in chains


#10 Chains - Ch1----
setwd(dir_Ch10_1)
load(file = paste0(dir_Ch10_1, '/OutputWorkspace_10Ch-1.RData'), verbose = FALSE)
rm(cl, Info, startValues, i, tic_Script)

# Plot and summarize output----
#Fixme: this requires cores to be setup to run in parallel, with all of the exported variables.
#summary(out_Parext)

#print(out_Parext_Restart$settings$runtime)
#summary(out_Parext_Restart)
#print(out_Parext_Restart_1$settings$runtime)
#summary(out_Parext_Restart_1)

#Useful to see the prior and posterior on same plot.
PriorSample = prior$sampler(n = 50000)
png('marginalplot_10_1-16.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[,1:16], prior = PriorSample[,1:16])
dev.off()
png('marginalplot_10_17-32.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[,17:32], prior = PriorSample[,17:32])
dev.off()
png('marginalplot_10_33-42.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[,33:42], prior = PriorSample[,33:42])
dev.off()

#Only for last 50 chain steps
png('marginalplot_10_1-16_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[51:100,1:16], prior = PriorSample[,1:16])
dev.off()
png('marginalplot_10_17-32_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[51:100,17:32], prior = PriorSample[,17:32])
dev.off()
png('marginalplot_10_33-42_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[51:100,33:42], prior = PriorSample[,33:42])
dev.off()

#scatterplot matrix (mcmc_pairs in bayesplot is alternative)
#Not sure how is best to view this scatterplot matrix because there are too many parameters to view in one grid.
#png('corplot_10.png', res = 300, units = 'in', width = 14, height = 14)
#correlationPlot(mat = out_Parext, scaleCorText = TRUE)
#dev.off()

#png('pairs_10.png', res = 300, units = 'in', width = 10, height = 10)
#mcmc_pairs(out_Parext$chain[,1:10])
#dev.off()

#Trace plots with posterior density for all parameter values - chains should look similar, not get stuck
png('traceplot_10_1-4.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,41:42], smooth = FALSE)
dev.off()

#mcmc_trace in bayesplot as alternative traceplots
png('traceplot_10_1-9_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,1:9])
dev.off()
png('traceplot_10_10-18_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,10:18])
dev.off()
png('traceplot_10_19-27_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,19:27])
dev.off()
png('traceplot_10_28-36_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,28:36])
dev.off()
png('traceplot_10_37-42_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,37:42])
dev.off()

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
png('AutocorrPlot_10_37-42.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,37:42], lag.max = 10)
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
# png('AutocorrPlot_10_37-42_l50.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,37:42], lag.max = 10)
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
# png('GewekePlot_10_37-42.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Parext$chain[,37:42])
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
AR_Ch10 = read.table('AcceptReject_c.txt', header=TRUE)
ARrates_Ch10 = vector('numeric', length = ncol(AR))
for (i in 1:ncol(AR_Ch10)){
  ARrates_Ch10[i] = sum(AR_Ch10[,i])/nrow(AR_Ch10)
}
png('AcceptRejectHist.png', res = 200, height=5, width=5, units='in')
hist(ARrates_Ch10, xlim=c(0,1))
dev.off()

#Fixme: Check for runs of 0s in chains


#10 Chains----
setwd(dir_Ch10)
load(file = paste0(dir_Ch39, '/OutputWorkspace.RData'), verbose = FALSE)
rm(cl, Info, startValues, i, tic_Script)

# Plot and summarize output----
#Fixme: this requires cores to be setup to run in parallel, with all of the exported variables.
#summary(out_Parext)

#print(out_Parext_Restart$settings$runtime)
#summary(out_Parext_Restart)
#print(out_Parext_Restart_1$settings$runtime)
#summary(out_Parext_Restart_1)

#Useful to see the prior and posterior on same plot.
PriorSample = prior$sampler(n = 50000)
png('marginalplot_10_1-16.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[,1:16], prior = PriorSample[,1:16])
dev.off()
png('marginalplot_10_17-32.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[,17:32], prior = PriorSample[,17:32])
dev.off()
png('marginalplot_10_33-42.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[,33:42], prior = PriorSample[,33:42])
dev.off()

#Only for last 50 chain steps
png('marginalplot_10_1-16_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[51:100,1:16], prior = PriorSample[,1:16])
dev.off()
png('marginalplot_10_17-32_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[51:100,17:32], prior = PriorSample[,17:32])
dev.off()
png('marginalplot_10_33-42_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[51:100,33:42], prior = PriorSample[,33:42])
dev.off()

#scatterplot matrix (mcmc_pairs in bayesplot is alternative)
#Not sure how is best to view this scatterplot matrix because there are too many parameters to view in one grid.
#png('corplot_10.png', res = 300, units = 'in', width = 14, height = 14)
#correlationPlot(mat = out_Parext, scaleCorText = TRUE)
#dev.off()

#png('pairs_10.png', res = 300, units = 'in', width = 10, height = 10)
#mcmc_pairs(out_Parext$chain[,1:10])
#dev.off()

#Trace plots with posterior density for all parameter values - chains should look similar, not get stuck
png('traceplot_10_1-4.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,41:42], smooth = FALSE)
dev.off()

#mcmc_trace in bayesplot as alternative traceplots
png('traceplot_10_1-9_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,1:9])
dev.off()
png('traceplot_10_10-18_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,10:18])
dev.off()
png('traceplot_10_19-27_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,19:27])
dev.off()
png('traceplot_10_28-36_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,28:36])
dev.off()
png('traceplot_10_37-42_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,37:42])
dev.off()

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
png('AutocorrPlot_10_37-42.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,37:42], lag.max = 10)
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
# png('AutocorrPlot_10_37-42_l50.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,37:42], lag.max = 10)
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
# png('GewekePlot_10_37-42.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Parext$chain[,37:42])
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
AR_Ch10 = read.table('AcceptReject_c.txt', header=TRUE)
ARrates_Ch10 = vector('numeric', length = ncol(AR))
for (i in 1:ncol(AR_Ch10)){
  ARrates_Ch10[i] = sum(AR_Ch10[,i])/nrow(AR_Ch10)
}
png('AcceptRejectHist.png', res = 200, height=5, width=5, units='in')
hist(ARrates_Ch10, xlim=c(0,1))
dev.off()

#Fixme: Check for runs of 0s in chains


#10 Chains----
setwd(dir_Ch10)
load(file = paste0(dir_Ch39, '/OutputWorkspace.RData'), verbose = FALSE)
rm(cl, Info, startValues, i, tic_Script)

# Plot and summarize output----
#Fixme: this requires cores to be setup to run in parallel, with all of the exported variables.
#summary(out_Parext)

#print(out_Parext_Restart$settings$runtime)
#summary(out_Parext_Restart)
#print(out_Parext_Restart_1$settings$runtime)
#summary(out_Parext_Restart_1)

#Useful to see the prior and posterior on same plot.
PriorSample = prior$sampler(n = 50000)
png('marginalplot_10_1-16.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[,1:16], prior = PriorSample[,1:16])
dev.off()
png('marginalplot_10_17-32.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[,17:32], prior = PriorSample[,17:32])
dev.off()
png('marginalplot_10_33-42.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[,33:42], prior = PriorSample[,33:42])
dev.off()

#Only for last 50 chain steps
png('marginalplot_10_1-16_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[51:100,1:16], prior = PriorSample[,1:16])
dev.off()
png('marginalplot_10_17-32_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[51:100,17:32], prior = PriorSample[,17:32])
dev.off()
png('marginalplot_10_33-42_l50.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[51:100,33:42], prior = PriorSample[,33:42])
dev.off()

#scatterplot matrix (mcmc_pairs in bayesplot is alternative)
#Not sure how is best to view this scatterplot matrix because there are too many parameters to view in one grid.
#png('corplot_10.png', res = 300, units = 'in', width = 14, height = 14)
#correlationPlot(mat = out_Parext, scaleCorText = TRUE)
#dev.off()

#png('pairs_10.png', res = 300, units = 'in', width = 10, height = 10)
#mcmc_pairs(out_Parext$chain[,1:10])
#dev.off()

#Trace plots with posterior density for all parameter values - chains should look similar, not get stuck
png('traceplot_10_1-4.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,1:4], smooth = FALSE)
dev.off()
png('traceplot_10_5-8.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,5:8], smooth = FALSE)
dev.off()
png('traceplot_10_9-12.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,9:12], smooth = FALSE)
dev.off()
png('traceplot_10_13-16.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,13:16], smooth = FALSE)
dev.off()
png('traceplot_10_17-20.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,17:20], smooth = FALSE)
dev.off()
png('traceplot_10_21-24.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,21:24], smooth = FALSE)
dev.off()
png('traceplot_10_25-28.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,25:28], smooth = FALSE)
dev.off()
png('traceplot_10_29-32.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,29:32], smooth = FALSE)
dev.off()
png('traceplot_10_33-36.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,33:36], smooth = FALSE)
dev.off()
png('traceplot_10_37-40.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,37:40], smooth = FALSE)
dev.off()
png('traceplot_10_41-42.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,41:42], smooth = FALSE)
dev.off()

#mcmc_trace in bayesplot as alternative traceplots
png('traceplot_10_1-9_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,1:9])
dev.off()
png('traceplot_10_10-18_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,10:18])
dev.off()
png('traceplot_10_19-27_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,19:27])
dev.off()
png('traceplot_10_28-36_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,28:36])
dev.off()
png('traceplot_10_37-42_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(out_Parext$chain[,37:42])
dev.off()

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
png('AutocorrPlot_10_37-42.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,37:42], lag.max = 10)
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
# png('AutocorrPlot_10_37-42_l50.png', res = 300, units = 'in', width = 7, height = 7)
# coda::autocorr.plot(out_Parext$chain[,37:42], lag.max = 10)
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
# png('GewekePlot_10_37-42.png', res = 300, units = 'in', width = 7, height = 7)
# coda::geweke.plot(out_Parext$chain[,37:42])
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
AR_Ch10 = read.table('AcceptReject_c.txt', header=TRUE)
ARrates_Ch10 = vector('numeric', length = ncol(AR))
for (i in 1:ncol(AR_Ch10)){
  ARrates_Ch10[i] = sum(AR_Ch10[,i])/nrow(AR_Ch10)
}
png('AcceptRejectHist.png', res = 200, height=5, width=5, units='in')
hist(ARrates_Ch10, xlim=c(0,1))
dev.off()

#Fixme: Check for runs of 0s in chains


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