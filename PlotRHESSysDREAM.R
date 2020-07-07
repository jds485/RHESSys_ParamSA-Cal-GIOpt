tic_Script = Sys.time()
#install.packages("/home/js4yd/BayesianTools/BayesianTools", type = 'source', repos = NULL )
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

load(file = 'OutputWorkspace.RData', verbose = FALSE)

#Plot and summarize output----
#Copy the IterNum.txt file because plots and summary info will run the likelihood function again. Want to continue using the old IterNum for that to avoid overwriting output files. But also want to save the end-of-chain IterNum.
file.copy(from = paste0(arg[19], '/IterNum.txt'), to = paste0(arg[19], '/IterFinalNum_ChainRun.txt'))

#Fixme: this requires cores to be setup to run in parallel, with all of the exported variables.
#summary(out_Parext)

#print(out_Parext_Restart$settings$runtime)
#summary(out_Parext_Restart)
#print(out_Parext_Restart_1$settings$runtime)
#summary(out_Parext_Restart_1)

#Useful to see the prior and posterior on same plot.
png('marginalplot_30.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext, prior = TRUE)
dev.off()
png('marginalplot_39_1-16.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[,1:16], prior = TRUE)
dev.off()
png('marginalplot_39_17-32.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[,17:32], prior = TRUE)
dev.off()
png('marginalplot_39_33-42.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext$chain[,33:42], prior = TRUE)
dev.off()

#scatterplot matrix (mcmc_pairs in bayesplot is alternative)
#Not sure how is best to view this scatterplot matrix because there are too many parameters to view in one grid.
png('corplot_30.png', res = 300, units = 'in', width = 14, height = 14)
correlationPlot(mat = out_Parext, scaleCorText = FALSE)
dev.off()

png('pairs_30.png', res = 300, units = 'in', width = 10, height = 10)
mcmc_pairs(out_Parext$chain[,1:10])
dev.off()


#Trace plots with posterior density for all parameter values - chains should look similar, not get stuck
png('traceplot_30_1-4.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,1:4])
dev.off()
png('traceplot_30_5-8.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,5:8])
dev.off()
png('traceplot_30_9-12.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,9:12])
dev.off()
png('traceplot_30_13-16.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,13:16])
dev.off()
png('traceplot_30_17-20.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,17:20])
dev.off()
png('traceplot_30_21-24.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,21:24])
dev.off()
png('traceplot_30_25-28.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,25:28])
dev.off()
png('traceplot_30_29-32.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,29:32])
dev.off()
png('traceplot_30_33-36.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,33:36])
dev.off()
png('traceplot_30_37-40.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,37:40])
dev.off()
png('traceplot_30_41-42.png', res = 300, units = 'in', width = 7, height = 7)
plot(out_Parext$chain[,41:42])
dev.off()

#mcmc_trace in bayesplot as alternative traceplots
png('traceplot_30_1-9_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(outParext$chain[,1:9])
dev.off()
png('traceplot_30_10-18_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(outParext$chain[,10:18])
dev.off()
png('traceplot_30_19-27_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(outParext$chain[,19:27])
dev.off()
png('traceplot_30_28-36_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(outParext$chain[,28:36])
dev.off()
png('traceplot_30_37-42_b.png', res = 300, units = 'in', width = 7, height = 7)
mcmc_trace(outParext$chain[,37:42])
dev.off()

#Fixme: This requires cores to be setup to run in parallel
#png('gelmanplot_30.png', res = 300, units = 'in', width = 7, height = 7)
#gelmanDiagnostics(sampler = out_Parext, plot = TRUE)
#dev.off()
#png('diagnosticplot_30.png', res = 300, units = 'in', width = 7, height = 7)
#plotDiagnostic(out = out_Parext)
#dev.off()


#stopCluster(cl)

#Run convergence diagnostics----
#R-hat - Gelman-Rubin within and between chain variance for all variables - target less than 1.05 for all
#gelman.diag(out_Parext$chain[,1:42])
#Autocorrelation of chain for each parameter should drop quickly. This informs the amound of values to skip in MCMC
png('AutocorrPlot_20_1-9.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,1:9], lag.max = 5)
dev.off()
png('AutocorrPlot_20_10-18.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,10:18], lag.max = 5)
dev.off()
png('AutocorrPlot_20_19-27.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,19:27], lag.max = 5)
dev.off()
png('AutocorrPlot_20_28-36.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,28:36], lag.max = 5)
dev.off()
png('AutocorrPlot_20_37-42.png', res = 300, units = 'in', width = 7, height = 7)
coda::autocorr.plot(out_Parext$chain[,37:42], lag.max = 5)
dev.off()

#Stationarity of chain - Geweke
#Computed for individual chains
png('GewekePlot_20_1-9.png', res = 300, units = 'in', width = 7, height = 7)
coda::geweke.plot(out_Parext$chain[,1:9])
dev.off()
png('GewekePlot_20_10-18.png', res = 300, units = 'in', width = 7, height = 7)
coda::geweke.plot(out_Parext$chain[,10:18])
dev.off()
png('GewekePlot_20_19-27.png', res = 300, units = 'in', width = 7, height = 7)
coda::geweke.plot(out_Parext$chain[,19:27])
dev.off()
png('GewekePlot_20_28-36.png', res = 300, units = 'in', width = 7, height = 7)
coda::geweke.plot(out_Parext$chain[,28:36])
dev.off()
png('GewekePlot_20_37-42.png', res = 300, units = 'in', width = 7, height = 7)
coda::geweke.plot(out_Parext$chain[,37:42])
dev.off()

#need at least 50 chain steps for this
#gelman.plot(x, bin.width = 10, max.bins = 50, confidence = 0.95, transform = FALSE, autoburnin=TRUE, auto.layout = TRUE, ask, col, lty, xlab, ylab, type, ...)

#Effective sample size for each parameter should be similar
EffSize = coda::effectiveSize(out_Parext$chain)

#Parallel coordinate plot - including all parameters is a bit much.
png('parcoord_30.png', res = 300, units = 'in', width = 10, height = 5)
mcmc_parcoord(outParext$chain, transform = function(x) {(x - min(x)) / (max(x)-min(x))})
dev.off()

#Density overlay (mcmc_dens_overlay) - all chains should have similar densities for each variable
png('densOverlay_30.png', res = 300, units = 'in', width = 10, height = 10)
mcmc_dens_overlay(out_Parext$chain)
dev.off()

#Rejection Rate
Reject = coda::rejectionRate(out_Parext$chain)

#Highest Posterior Density Interval
HPDs = coda::HPDinterval(out_Parext$chain)
BayesianTools::getCredibleIntervals(out_Parext$chain[[1]])


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