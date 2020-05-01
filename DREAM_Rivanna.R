#Setting up the use of DREAM(ZS) for MCMC on Rivanna

#Load libraries----
library(BayesianTools)
library(parallel)
library(coda)

#Set seed and save the session information----
set.seed(42120)
Info = sessionInfo()

#Load in the problem file that defines parameter names, bounds and best values----
ParamRanges = read.csv('C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs\\MorrisSampleLocs\\BaismanCalibrationParameterProblemFile.csv', stringsAsFactors = FALSE)
#Fixme: This will also need to have the likelihood function hyperparameters included in it.
#Fixme: This will have to be trimmed to include only those variables that will be used in calibration.

#Load the matrix of starting values to be used for the chains---- 
#Initially this is from the SA. For restarts of DREAM, this is the reported last point in the chain.
startValues = read.table(file = 'BaismanChainStarts.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE)

#Define the multivariate prior----
prior = createUniformPrior(lower = ParamRanges$Lower, upper = ParamRanges$Upper, best = NULL)

#Define the likelihood function----
#Likelihood is a function that should return the sum of the log likelihood values
#Make a function to call GIS2RHESSys, RHESSys, and WRTDS / ECM models at once----
#Fixme: end date will be different for calibration. Also change tecfile
par = paste0("rhessys5.20.0.develop -st 1999 11 15 1 -ed 2010 10 1 1 -b -h -newcaprise -gwtoriparian -capMax 0.01 -slowDrain -t tecfiles/tec_daily_Cal.txt -w worldfiles/worldfile -whdr worldfiles/worldfile.hdr -r flows/subflow.txt flows/surfflow.txt -pre output/BaismanRun", i, " -s 1 1 1 -sv 1 1 -gw 1 1 -svalt 1 1 -vgsen 1 1 1 -snowTs 1 -snowEs 1 -capr 0.001")
likelihood = function(par){
  #Check that suggested parameter values are within their constraints
  
  #If they are not, then need to have the algorithm suggest a new location, and make sure DREAM knows the final parameters used
  
  # Create here a string with what you would write to call the model from the command line
  systemCall <- par
  
  # this runs the executable file
  system(systemCall)
  
  #Get the model output and compute the residuals from observed streamflow and nitrogen
  
  #Compute the likelihood
  
}

#Create a bayesian setup variable to define the problem----
BayesianSetup = createBayesianSetup(likelihood = likelihood, prior = prior, priorSampler = NULL,
                                    names = ParamRanges$NumberedParams, lower = NULL, upper = NULL, best = NULL,
                                    catchDuplicates = TRUE,
                                    parallelOptions = list(variables = , 
                                                           packages = , dlls = ))


#Run in parallel----
cl <- makeCluster(2)

runParallel<- function(parList){
  
  parSapply(cl, parList, mymodel)
  
}

runParallel(c(1,2))

parModel <- generateParallelExecuter(mymodel)


out = runMCMC(bayesianSetup = BayesianSetup,
        sampler = "DREAMzs", 
        settings = list(iterations = 100, gamma= NULL, eps = 0, e = 0.05, parallel = NULL, Z = NULL, ZupdateFrequency = 10, pSnooker = 0.1, DEpairs = 2, startValue = startValues, currentChain = 1, nrChains = 1, runtime = 0,
                        nCR = 3, pCRupdate = TRUE, updateInterval = 10,
                        #burnin must be greater than adaptation.
                        burnin= 0, adaptation = 0.2, thin = 1,
                        consoleUpdates = 10, message = TRUE))

#Default DREAMzs settings
settings = list(iterations = 10000, nCR = 3, gamma= NULL, eps = 0, e = 0.05, pCRupdate = FALSE, updateInterval = 10, burnin= 0, thin = 1, adaptation = 0.2, parallel = NULL, Z = NULL, ZupdateFrequency = 10, pSnooker = 0.1, DEpairs = 2, consoleUpdates = 10, startValue = NULL, currentChain = 1, message = FALSE, nrChains = 1, runtime = 0)

#Run convergence diagnostics----
#From VSEM package:
plot(out)
summary(out)
marginalPlot(out)
gelmanDiagnostics(out)

#R-hat - Gelman-Rubin within and between chain variance for all variables - target less than 1.05 for all
gelman.diag(chain)
#Autocorrelation of chain for each parameter should drop quickly. This informs the amound of values to skip in MCMC

#Stationarity of chain - Geweke

#Effective sample size for each parameter should be similar

#Parallel coordinate plot (mcmc_parcoord in bayesplot)

#scatterplot matrix (mcmc_pairs in bayesplot)

#Trace plots for all parameter values (mcmc_trace) - chains should look similar, not get stuck

#Density overlay (mcmc_dens_overlay) - all chains should have similar densities

#Posterior Predictive Checks----
#Generate data from the parameter joint posterior distribution
# compare to the calibrated observations (X and yRep), and/or to future values (XTilde and yTilde)
# The true distribution of y should be similar to that of yRep.
#  Look at density overlay of y and yRep for each of the parameter sets

# Create a prediction function
createPredictions <- function(par){
  # set the parameters that are not calibrated on default values 
  x = refPars$best
  x[parSel] = par
  predicted <- VSEM(x[1:11], PAR) # replace here VSEM with your model 
  return(predicted[,1] * 1000)
}

# Create an error function
createError <- function(mean, par){
  return(rnorm(length(mean), mean = mean, sd = par[7]))
}

# plot prior predictive distribution and prior predictive simulations
plotTimeSeriesResults(sampler = out, model = createPredictions, observed = obs[,1],
                      error = createError, prior = TRUE, main = "Prior predictive")

# plot posterior predictive distribution and posterior predictive simulations
plotTimeSeriesResults(sampler = out, model = createPredictions, observed = obs[,1],
                      error = createError, main = "Posterior predictive")


#% of observations within %-level prediction intervals
#autocorrelation for time series data 
#Bayesian p-values for evaluating model fit over the entire posterior distribution

#Credible intervals as barplots and as areas (mcmc_areas and mcmc_intervals)
