#Script to lean some of the functionality of the BayesianTools R package
#Includes serial and parallel examples.
#External parallelization not demonstrated here.
#There are many additional plots and diagnostics available in the coda and bayesplot packages. Users encouraged to try them.
#Example from: https://cran.r-project.org/web/packages/BayesianTools/vignettes/BayesianTools.html#example

#Load libraries----
library(BayesianTools)
library(parallel)
library(bayesplot)
library(coda)

#Setup problem: Quadratic Equation----
set.seed(2759)
sampleSize = 30
x <- (-(sampleSize-1)/2):((sampleSize-1)/2)
y <-  1 * x + 1*x^2 + rnorm(n=sampleSize,mean=0,sd=10)
plot(x,y, main="Test Data")

#Likelihood definition----
likelihood1 <- function(param){
  #Intercept, linear, quadratic terms
  pred = param[1] + param[2]*x + param[3] * x^2
  #Standard deviation parameter
  singlelikelihoods = dnorm(y, mean = pred, sd = 1/(param[4]^2), log = T)
  return(sum(singlelikelihoods))  
}

#Setup problem and define the uniform prior----
setUp1 <- createBayesianSetup(likelihood1, lower = c(-5,-5,-5,0.01), upper = c(5,5,5,30))

#MCMC setup for DREAMzs algorithm - Serial Run----
#Iterations per chain = (iterations - burnin/thin). 1400 with 5 chains
#Number of chains specified by startValue = 5 (5 random draws from prior)
settings = list(iterations = 10000, gamma= NULL, eps = 0, e = 0.05, parallel = NULL, 
                Z = NULL, ZupdateFrequency = 10, pSnooker = 0.1, DEpairs = 3,
                nCR = 3, pCRupdate = TRUE, updateInterval = 10,
                #burnin must be greater than adaptation.
                burnin = 3000, adaptation = 2000, thin = 1, message = FALSE, startValue = 5)

out1 <- runMCMC(bayesianSetup = setUp1, sampler = "DREAMzs", settings = settings)

# Summary information and plots----
summary(out1)

#Traceplot
tracePlot(out1, smooth=FALSE)
#Marginal prior and posterior
marginalPlot(x = out1, prior = TRUE)
#Scatterplot matrix of parameters
correlationPlot(out1)
#Potential scale reduction factor
gelmanDiagnostics(out1, plot=T)
#Density overlay
mcmc_dens_overlay(out1$chain, color_chains = TRUE)

#Serial run with half the iterations----
settings_5000 = list(iterations = 5000, gamma= NULL, eps = 0, e = 0.05, parallel = NULL, 
                Z = NULL, ZupdateFrequency = 10, pSnooker = 0.1, DEpairs = 3,
                nCR = 3, pCRupdate = TRUE, updateInterval = 10,
                #burnin must be greater than adaptation.
                burnin = 3000, adaptation = 2000, thin = 1, message = FALSE, startValue = 5)

out1_5000 <- runMCMC(bayesianSetup = setUp1, sampler = "DREAMzs", settings = settings_5000)

# Summary information and plots----
summary(out1_5000)

#Traceplot
tracePlot(out1_5000, smooth=FALSE)
#Marginal prior and posterior
marginalPlot(x = out1_5000, prior = TRUE)
#Scatterplot matrix of parameters
correlationPlot(out1_5000)
#Potential scale reduction factor
gelmanDiagnostics(out1_5000, plot=T)
#Density overlay
mcmc_dens_overlay(out1$chain, color_chains = TRUE)

#Serial run with double the iterations----
settings_20000 = list(iterations = 20000, gamma= NULL, eps = 0, e = 0.05, parallel = NULL, 
                     Z = NULL, ZupdateFrequency = 10, pSnooker = 0.1, DEpairs = 3,
                     nCR = 3, pCRupdate = TRUE, updateInterval = 10,
                     #burnin must be greater than adaptation.
                     burnin = 3000, adaptation = 2000, thin = 1, message = FALSE, startValue = 5)

out1_20000 <- runMCMC(bayesianSetup = setUp1, sampler = "DREAMzs", settings = settings_20000)

# Summary information and plots----
summary(out1_20000)

#Traceplot
tracePlot(out1_20000, smooth=FALSE)
#Marginal prior and posterior
marginalPlot(x = out1_20000, prior = TRUE)
#Scatterplot matrix of parameters
correlationPlot(out1_20000)
#Potential scale reduction factor
gelmanDiagnostics(out1_20000, plot=T)
#Density overlay
mcmc_dens_overlay(out1$chain, color_chains = TRUE)

#Parallel run----
#For parallel test on 5 cores
setUp1_par <- createBayesianSetup(likelihood1, lower = c(-5,-5,-5,0.01), upper = c(5,5,5,30), parallel = 5, parallelOptions = list(packages=list('BayesianTools'), variables=list('x','y'), dlls=NULL))

#Note: setting the seed equal to the seed for serial does not result in identical runs. There must be a parallel random seed option that's different than serial.
settings_par = list(iterations = 10000, gamma= NULL, eps = 0, e = 0.05, parallel = NULL, Z = NULL, ZupdateFrequency = 10, pSnooker = 0.1, DEpairs = 3,
                    nCR = 3, pCRupdate = TRUE, updateInterval = 10,
                    #burnin must be greater than adaptation.
                    burnin = 3000, adaptation = 2000, thin = 1, message = FALSE, startValue = 5)

out1_par <- runMCMC(bayesianSetup = setUp1_par, sampler = "DREAMzs", settings = settings_par)

# Summary information and plots----
#Note that summary requires that the parallel cores are still open
summary(out1_par)

#Traceplot
tracePlot(out1_par, smooth=FALSE)
#Marginal prior and posterior
marginalPlot(x = out1_par, prior = TRUE)
#Scatterplot matrix of parameters
correlationPlot(out1_par)
#Potential scale reduction factor
gelmanDiagnostics(out1_par, plot=T)

stopParallel(setUp1_par)
