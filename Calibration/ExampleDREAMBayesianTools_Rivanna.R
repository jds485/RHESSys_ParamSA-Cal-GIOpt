#install.packages("/home/js4yd/BayesianTools/BayesianTools", type = 'source', repos = NULL )
library(BayesianTools)
library(parallel)
library(foreach)
library(doParallel)
library(rlist)

#Example from: https://cran.r-project.org/web/packages/BayesianTools/vignettes/BayesianTools.html#example
#External parallelization----
#This likelihood function must be able to do parallel allocation, and accept a matrix of chains as rows, parameters as columns, and return a vector of log-likelihoods, one element per chain.
### Create cluster with n cores
cl <- parallel::makeCluster(3)
registerDoParallel(cl)

set.seed(2759)
sampleSize = 30
x <- (-(sampleSize-1)/2):((sampleSize-1)/2)
y <-  1 * x + 1*x^2 + rnorm(n=sampleSize,mean=0,sd=10)

## Definition of the likelihood which will be calculated in parallel. Instead of the parApply function, we could also define a costly parallelized likelihood
likelihood_externalTest <- function(param){
  #Check to see if there are any files of the format test_#.txt in the working directory
  #setwd("/sfs/lustre/bahamut/scratch/js4yd/TestBayesianToolsDREAMzs")
  fs = list.files()
  if (length(grep(fs,pattern = 'IterNum',ignore.case = FALSE,fixed = TRUE,value = FALSE)) == 0){
    num = 1
  }else{
    num = read.csv(file = 'IterNum.txt', header = TRUE)$x[1] + 1
  }
  write.csv(x = num, file = 'IterNum.txt', row.names = FALSE)
  rm(fs)
  #print(paste('test', num))
  #print(param)
  #print(nrow(param))
  #print(ifelse(is.null(nrow(param)), 1, nrow(param)))
  
  #Write a file with the suggested parameter sets
  #write.table(x = matrix(param, nrow = ifelse(is.null(nrow(param)), 1, nrow(param)), ncol = ifelse(is.null(nrow(param)), length(param), ncol(param))), file = paste0('test_', num,'.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)
  write.csv(x = matrix(param, nrow = ifelse(is.null(nrow(param)), 1, nrow(param)), ncol = ifelse(is.null(nrow(param)), length(param), ncol(param))), file = paste0('Chain_', num,'_AfterProcessing.csv'), row.names = FALSE)
  od = getwd()
  
  #Compute log likelihoods in parallel
  lls = foreach::foreach(i=1:ifelse(is.null(nrow(param)), 1, nrow(param)), .combine = c, .inorder = TRUE, .export = c('x', 'y')) %dopar%{
    setwd("/sfs/lustre/bahamut/scratch/js4yd/TestBayesianToolsDREAMzs/TempComputeDirectory")
    if (is.null(nrow(param))){
      pred = param[1] + param[2]*x + param[3] * x^2
      singlelikelihoods = dnorm(y, mean = pred, sd = 1/(param[4]^2), log = T)
    }else{
      pred = param[i,1] + param[i,2]*x + param[i,3] * x^2
      singlelikelihoods = dnorm(y, mean = pred, sd = 1/(param[i,4]^2), log = T)
    }
    #Used to ensure that only the essential variables were showing up on these cores.
    #Files = ls()
    #save(Files, file = paste0('Iter_', i, '.RData'))
    #This is reurned to lls
    sum(singlelikelihoods)
  }
  
  #Used to ensure that only essential variables were showing up in likelihood function.
  #print(ls())
  
  setwd(od)
  return(lls)  
}

## export functions, dlls, libraries
#parallel::clusterEvalQ(cl = cl, library(BayesianTools))
#parallel::clusterExport(cl, varlist = list(likelihood))

## create BayesianSetup
setup_Parext = createBayesianSetup(likelihood_externalTest, lower = c(-5,-5,-5,0.01), upper = c(5,5,5,30), parallel = 'external', parallelOptions = list(packages=list('BayesianTools', 'foreach', 'doParallel'), variables=list('x','y','likelihood_externalTest'), dlls=NULL), names = c('p1', 'p2', 'p3', 'p4'))
#bayesianSetup = setup_Parext
## For this case we want to parallelize the internal chains, therefore we create a n row matrix with startValues, if you parallelize a model in the likelihood, do not set a n*row Matrix for startValue
settings_Parext = list(iterations = 10000, gamma= NULL, eps = 0, e = 0.05, parallel = NULL, Z = NULL, ZupdateFrequency = 1, pSnooker = 0.1, DEpairs = 2,
                       nCR = 2, pCRupdate = TRUE, updateInterval = 2,
                       #burnin must be greater than adaptation.
                       burnin = 2000, adaptation = 2000, thin = 1, message = FALSE, startValue = 3)
#settings = settings_Parext

## runMCMC
set.seed(7377)
setwd("/sfs/lustre/bahamut/scratch/js4yd/TestBayesianToolsDREAMzs")
#parallel::clusterEvalQ(cl = cl, library(BayesianTools))
#parallel::clusterEvalQ(cl = cl, library(foreach))
#parallel::clusterEvalQ(cl = cl, library(doParallel))
#parallel::clusterExport(cl = cl, varlist = c('DREAMzs', 'AdaptpCR', 'generateCRvalues', 'x', 'y', 'likelihood_externalTest'))
out_Parext <- runMCMC(settings = settings_Parext, bayesianSetup = setup_Parext, sampler = "DREAMzs")

#Researting a DREAM run from output----
load(file = 'CurrentChain.RData', verbose = FALSE)
class(temp) = c('mcmcSampler', 'bayesianOutput')

#Change the settings, if needed.
#No adaptation and burnin needed for the restart.
temp$settings$burnin = 0
temp$settings$adaptation = 0
temp$settings$iterations = 1000

out_Parext_Restart <- runMCMC(bayesianSetup = temp, sampler = "DREAMzs")

#Researting a restarted DREAM run from output----
temp_1 = temp
load(file = 'CurrentChain_Restart_1.RData', verbose = FALSE)
class(temp) = c('mcmcSampler', 'bayesianOutput')

#Change the settings, if needed.
#No adaptation and burnin needed for the restart.
temp$settings$burnin = 0
temp$settings$adaptation = 0
temp$settings$iterations = 1000

#Join the chains from the previous runs to this run
for (i in 1:length(temp_1$chain)){
  temp$chain[[i]] = rbind(temp_1$chain[[i]], temp$chain[[i]])
}

out_Parext_Restart_1 <- runMCMC(bayesianSetup = temp, sampler = "DREAMzs")

#Rename the IterNum.txt file because plots and summary info will run the likelihood function again, thus making a new IterNum.txt file.
file.rename(from = "/sfs/lustre/bahamut/scratch/js4yd/TestBayesianToolsDREAMzs/IterNum.txt", to = "/sfs/lustre/bahamut/scratch/js4yd/TestBayesianToolsDREAMzs/IterFinalNum_ChainRun.txt")

print(out_Parext$settings$runtime)
summary(out_Parext)
print(out_Parext_Restart$settings$runtime)
summary(out_Parext_Restart)
print(out_Parext_Restart_1$settings$runtime)
summary(out_Parext_Restart_1)

png('diagnosticplot_30.png', res = 300, units = 'in', width = 7, height = 7)
plotDiagnostic(out = out_Parext_Restart_1)
dev.off()
png('marginalplot_30.png', res = 300, units = 'in', width = 7, height = 7)
marginalPlot(x = out_Parext_Restart_1, prior = TRUE)
dev.off()
png('gelmanplot_30.png', res = 300, units = 'in', width = 7, height = 7)
gelmanDiagnostics(sampler = out_Parext_Restart_1, plot = TRUE)
dev.off()
png('corplot_30.png', res = 300, units = 'in', width = 7, height = 7)
correlationPlot(mat = out_Parext_Restart_1, scaleCorText = FALSE)
dev.off()
png('traceplot_30.png', res = 300, units = 'in', width = 5, height = 7)
plot(out_Parext_Restart_1)
dev.off()

#Delete the new IterNum.txt file
file.remove("/sfs/lustre/bahamut/scratch/js4yd/TestBayesianToolsDREAMzs/IterNum.txt")

#Functions (including summary) assume connection to external parallel cores will still be available.
stopCluster(cl)

#Save list of chains
for (i in 1:length(out_Parext_Restart_1$chain)){
  out_Parext_Restart_1$chain[[i]] = signif(out_Parext_Restart_1$chain[[i]],6)
}

list.save(out_Parext_Restart$chain, 'OutputChains_Restart_1.yaml')
#Save entire workspace info
save.image(file = 'OutputWorkspace_Restart.RData', safe = FALSE)