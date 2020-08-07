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

#Load results of completed DREAM run
arg = commandArgs(T)
load(arg[18], verbose=FALSE)
temp = out_Parext_Restart
#Remove unnecessary variables
rm(out_Parext_Restart, arg, cl, i, Info, likelihood_external, setup_Parext)

#Read in command line calls----
#1 - number of cores to make, equal to number of chains
#2 - R random seed to run the MCMC
#3 - WRTDS script path (R script)
#4 - Observed streamflow record path (.txt file, tab separated)
#5 - Observed TN record path (.txt file, tab separated)
#6 - Start Date for comparison - first day after spinup is complete (e.g., '2004-10-01')
#7 - End Date for comparison (e.g., '2013-09-30')
#8 - worldfile location (csv file)
#9 - patch resolution (m)
#10 - WRTDS TabInt file (txt file, tab separated with headers)
#11 - WRTDS TabYear file (txt file, tab separated with headers)
#12 - WRTDS TabLogQ file (txt file, tab separated with headers)
#13 - WRTDS TabLogQ2 file (txt file, tab separated with headers)
#14 - WRTDS TabSinYear file (txt file, tab separated with headers)
#15 - WRTDS TabCosYear file (txt file, tab separated with headers)
#16 - WRTDS TabLogErr file (txt file, tab separated with headers)
#17 - File that describes the parameter names and bounds (.csv file with headers)
#18 - Data file from previous run (CurrentChain.RData)
#19 - Full path to working directory for chain runs (RHESSysRuns)
#20 - iterations, Need at least one more iteration than the burnin to report a value to the output chain
#21 - eps, normal noise to the proposal update.
#22 - e, multiplier to gamma in proposal update 
#23 - ZupdateFrequency, Z is not updated until after burnin if the update frequency is greater than 1. Making equal to updateInterval.
#24 - pSnooker, chance of a snooker update as a proportion
#25 - DEpairs, Number of chains used to compute the DE proposal update. In papers, randomly choose 1, 2, or 3 to use.
#26 - nCR, nCR >=2 recommended in papers
#27 - updateInterval, must be greater than or equal to nCR.
#28 - burnin, must be greater than or equal to adaptation, or all of the adaptation steps must be manually removed before summarizing chains.
#29 - adaptation, should be about 20 % of the total NFE. Doing this manually.
#30 - thin, thin the chain, or set to 1 to allow for post-solver manual thinning
#31 - path to container image file '/share/resources/containers/singularity/rhessys/rhessys_v3.img'
#32 - path to def files '/scratch/js4yd/Baisman30mDREAMzs-10Ch/RHESSys_Baisman30m_g74/defs/'
#33 - round tolerance for def files '10'
#34 - 'BaismanCalibrationParameterProblemFile.csv'
#35 - RHESSys project name 'RHESSys_Baisman30m_g74'
#36 - full path to project directory '/scratch/js4yd/Baisman30mDREAMzs-10Ch/RHESSys_Baisman30m_g74/'
#37 - path to RHESSys exe file '/scratch/js4yd/RHESSysEastCoast_Optimized/rhessys5.20.0.develop_optimsize'
#38 - path to the likelihood functions '/scratch/js4yd/Baisman30mDREAMzs-10Ch/LikelihoodFun/'
#39 - suffix for the output file information '-10Ch_s###'
#40 - Initial random seed for Python '1'
#41 - Initial random seed for flow likelihood '1020'
#42 - Initial random seed for TN likelihood '2010'

arg = commandArgs(T)

#Load in the modified WRTDS code----
source(arg[3])
#Remove unnecessary functions
rm(estCrossVal, estSurfaces, FillTableNAs, modelEstimation, plotContours, run_WRTDS, runSurvReg, surfaceIndex)

#Process observation timeseries to specified date range----
#Load the observed streamflow record and trim to necessary days
obs = read.table(arg[4], header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, sep = '\t')
#Remove observations later than 9/30/2013 (Cal timeperiod end)
obs = obs[as.Date(obs$Date) <= as.Date(arg[7]),]
#Remove observations earlier than 10/01/2004 (Cal timeperiod start)
obs = obs[as.Date(obs$Date) >= as.Date(arg[6]),]
#Write file for use in likelihood function
options(scipen = 999)
write.table(x = obs, file = paste0(strsplit(arg[4], split = '.txt')[[1]], '_p.txt'), sep = '\t', row.names = FALSE, col.names = TRUE, fileEncoding = 'UTF-8')
options(scipen = 0)
nQ = nrow(obs)

#Load the observed TN record
obsTN = read.table(arg[5], header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, sep = '\t')
#Remove observations later than 9/30/2013 (Cal timeperiod end)
obsTN = obsTN[as.Date(obsTN$Date) <= as.Date(arg[7]),]
#Remove observations earlier than 10/01/2004 (Cal timeperiod start)
obsTN = obsTN[as.Date(obsTN$Date) >= as.Date(arg[6]),]
#Remove all NA days - doing this in Python and R later.
#obsTN = obsTN[!is.na(obsTN$TN),]
#Write file for use in likelihood function
options(scipen = 999)
write.table(x = obsTN, file = paste0(strsplit(arg[5], split = '.txt')[[1]], '_p.txt'), sep = '\t', row.names = FALSE, col.names = TRUE, fileEncoding = 'UTF-8')
options(scipen = 0)
nTN = nrow(obsTN[!is.na(obsTN$TN),])

#Compute streamflow conversion factor----
world = read.csv(arg[8], stringsAsFactors = FALSE)
res = as.numeric(arg[9])
#Taking the unique patch IDs because strata can exist in more than one patch.
Area.basin = length(unique(world$patchID))*res^2
rm(world, res)
#Multiplier conversion for basin streamflow (mm/d)*conversion_b -> cfs
conversion_b = Area.basin/1000/(.3048^3)/24/3600
rm(Area.basin)

#Load WRTDS tables----
TabInt = as.matrix(read.table(file = arg[10], sep = '\t', header = TRUE, check.names = FALSE))
TabYear = as.matrix(read.table(file = arg[11], sep = '\t', header = TRUE, check.names = FALSE))
TabLogQ = as.matrix(read.table(file = arg[12], sep = '\t', header = TRUE, check.names = FALSE))
TabLogQ2 = as.matrix(read.table(file = arg[13], sep = '\t', header = TRUE, check.names = FALSE))
TabSinYear = as.matrix(read.table(file = arg[14], sep = '\t', header = TRUE, check.names = FALSE))
TabCosYear = as.matrix(read.table(file = arg[15], sep = '\t', header = TRUE, check.names = FALSE))
TabLogErr = as.matrix(read.table(file = arg[16], sep = '\t', header = TRUE, check.names = FALSE))

rowt = as.numeric(rownames(TabInt))
colt = as.numeric(colnames(TabInt))

#Load in the problem file that defines parameter names and bounds----
#Fixme: This could have the likelihood function hyperparameters included in it.
#Loaded in above
#ParamRanges = read.csv(arg[17], stringsAsFactors = FALSE)

#Define the Multivariate Prior----
#Loaded in above.
#prior = createUniformPrior(lower = ParamRanges$Lower, upper = ParamRanges$Upper, best = NULL)

#Define the Likelihood with External Parallelization----
#This likelihood function implements parallel allocation (foreach loop); accepts a matrix of chains as rows, parameters as columns; and returns a vector of log-likelihoods, one element per chain.

#Function has several steps:
#0 - Determine the current step in the chain
#1 - check that params are within their constraints, and adjust if needed
#---In parallel---
#2 - update def files with those params and make directories for each chain's RHESSys run
#3 - Run RHESSys 
#4 - Run WRTDS model to compute TN 
#5 - Compute likelihoods
likelihood_external <- function(param){
  od = getwd()
  
  #Check to see if there are any files of the format IterNum.txt in the working directory
  setwd(arg[19])
  if (length(grep(list.files(),pattern = 'IterNum',ignore.case = FALSE,fixed = TRUE,value = FALSE)) == 0){
    num = 1
  }else{
    num = read.csv(file = 'IterNum.txt', header = TRUE)$x[1] + 1
  }
  write.csv(x = num, file = 'IterNum.txt', row.names = FALSE)
  
  #Write a file with the suggested parameter values as columns, chains as rows
  #Parameter column names are not passed in with the call. Add manually.
  param_cols = c('h_gw_loss_coeff', 'z_sea_level_clear_sky_trans', 'z_temcf', 'z_wind', 'z_trans_coeff1', 'z_trans_coeff2', 's9_Ksat_0', 's9_active_zone_z', 's9_m', 's9_pore_size_index', 's9_porosity_0', 's9_psi_air_entry', 's9_sat_to_gw_coeff', 's9_Ksat_0_v', 's109_Ksat_0', 's109_active_zone_z', 's109_m', 's109_pore_size_index', 's109_porosity_0', 's109_psi_air_entry', 's109_sat_to_gw_coeff', 's109_Ksat_0_v', 's8_Ksat_0', 's8_active_zone_z', 's8_m', 's8_psi_air_entry', 's8_sat_to_gw_coeff', 's8_Ksat_0_v', 's108_Ksat_0', 's108_m', 's108_sat_to_gw_coeff', 's108_Ksat_0_v', 'l4_septic_water_load', 'v102_epc.day_leafoff', 'v102_epc.day_leafon', 'v102_epc.gl_c', 'v102_epc.gl_smax', 'v102_epc.ndays_expand', 'v102_epc.ndays_litfall', 'v102_lai_stomatal_fraction', 'v102_specific_rain_capacity', 'v3_specific_rain_capacity')
  write.table(x = matrix(param, nrow = ifelse(is.null(nrow(param)), 1, nrow(param)), ncol = ifelse(is.null(nrow(param)), length(param), ncol(param))), file = paste0('Chain_', num,'.txt'), sep = '\t', row.names = FALSE, col.names = param_cols)
  
  #Check that suggested parameter values are within their constraints,
  #and if they are not, then they are adjusted to be within their constraints.
  #This calls python script that checks bounds and returns a file with those bounds adjusted
  #arguments 
  #1: working directory
  #2: random seed - should be different for each step in chain. All chains processed at once in this script
  #3: directory of def files
  #4: round tolerance <= 10
  #5: problem file name with extension
  #6: chain parameter sample text file name without extension (e.g., 'BaismanChainStarts', 'Chain_1' where 1 is chain iteration)
  
  #Random seed for Python
  seed = as.numeric(arg[40]) + num
  system(paste0("singularity exec ", arg[31], " python ", arg[19], "DREAM_ParameterBoundChecks.py '", arg[19], "' '", seed, "' '", arg[32], "' '", arg[33], "' '", arg[34], "' 'Chain_", num,"'"), intern = TRUE)
  rm(seed)
  
  print(paste0('Files set up for chain step num ', num, '. Running lls loop over all chains.'))
  
  #Used to make sure the essential variables were available
  #num, od, param, param_cols
  #print(ls())
  
  tic_parLoop = Sys.time()
  
  # Run RHESSys, compute TN, and compute likelihoods in parallel
  #Fixme: export, maybe a no export on param?
  lls = foreach::foreach(i=1:ifelse(is.null(nrow(param)), 1, nrow(param)), .combine = c, .inorder = TRUE) %dopar%{
    setwd(arg[19])
    
    #Create the .out file that will be used for system2 commands
    sout = paste0(getwd(), '/output/Run', num, '_Ch', i, '.out')
    file.create(sout)
    
    #Files = c(ls(".GlobalEnv"), ls())
    #save(Files, file = paste0('PreIter', num, '_Ch', i, '.RData'))
    
    #All parameters have been checked and adjusted, if needed. Now make and update the RHESSys def files with those values in Chain_<num>_AfterProcessing.csv.
    #This function will also make directories in which to run RHESSys
    
    #1: name of the chain parameter file (e.g., 'Chain_1_AfterProcessing.csv')
    #2: RHESSysRuns directory for saving replicates "$BASEDIR"/RHESSysRuns
    #3: def file directory "$BASEDIR"/"$RHESSysNAME"/defs
    #4: grass GIS folder name "$RHESSysNAME"
    #5: name of the file that has parameter names and lower and upper bounds (BaismanCalibrationParameterProblemFile.csv)
    #6: rounding tolerance (10)
    #7: current chain iteration
    #8: chain number
    
    sysout = system2('singularity', args=c('exec', arg[31], 'python', paste0(arg[19], 'MakeDefs_fn_Chains.py'), paste0('Chain_', num,'_AfterProcessing.csv'), arg[19], arg[32], arg[35], arg[34], arg[33], as.character(num), as.character(i)), stdout = TRUE, stderr = TRUE)
    
    cat(sysout, file=sout, append=TRUE, sep="\n")
    rm(sysout)
    
    #Change into ith directory that was just made and copy in files needed to run RHESSys
    # full path to the project location;
    setwd(paste0(arg[19], "Run", num,"_Ch", i))
    
    #Copy folders and files to ./"$RHESSysNAME"
    file.copy(from = paste0(arg[36], "worldfiles"), to = paste0('./', arg[35]), recursive = TRUE)
    file.copy(from = paste0(arg[36], "tecfiles"), to = paste0('./', arg[35]), recursive = TRUE)
    file.copy(from = paste0(arg[36], "output"), to = paste0('./', arg[35]), recursive = TRUE)
    file.copy(from = paste0(arg[36], "flows"), to = paste0('./', arg[35]), recursive = TRUE)
    file.copy(from = paste0(arg[36], "clim"), to = paste0('./', arg[35]), recursive = TRUE)
    
    setwd(paste0(arg[19], "Run", num,"_Ch", i, "/", arg[35]))
    
    #Run RHESSys
    sysout = system2(arg[37], args=c('-st', '1999', '11', '15', '1', '-ed', '2013', '10', '1', '1', '-b', '-h', '-newcaprise', '-gwtoriparian', '-capMax', '0.01', '-slowDrain', '-t', 'tecfiles/tec_daily_cal.txt', '-w', 'worldfiles/worldfile', '-whdr', 'worldfiles/worldfile.hdr', '-r', 'flows/subflow.txt', 'flows/surfflow.txt', '-pre', paste0('output/Run', num, '_Ch', i), '-s', '1', '1', '1', '-sv', '1', '1', '-gw', '1', '1', '-svalt', '1', '1', '-vgsen', '1', '1', '1', '-snowTs', '1', '-snowEs', '1', '-capr', '0.001'), stdout = TRUE, stderr = TRUE)
    
    cat(sysout, file=sout, append=TRUE, sep="\n")
    rm(sysout)
    
    #Remove the flows, tecfiles, defs, and clim folders.
    unlink(paste0(arg[19], "Run", num,"_Ch", i, "/", arg[35], "/flows"), recursive = TRUE)
    unlink(paste0(arg[19], "Run", num,"_Ch", i, "/", arg[35], "/clim"), recursive = TRUE)
    unlink(paste0(arg[19], "Run", num,"_Ch", i, "/", arg[35], "/tecfiles"), recursive = TRUE)
    unlink(paste0(arg[19], "Run", num,"_Ch", i, "/", arg[35], "/defs"), recursive = TRUE)
    #Remove some un-needed output
    file.remove(paste0(arg[19], "Run", num,"_Ch", i, "/", arg[35], "/output/Run", num,"_Ch", i, "_basin.hourly"))
    file.remove(paste0(arg[19], "Run", num,"_Ch", i, "/", arg[35], "/output/Run", num,"_Ch", i, "_basin.monthly"))
    file.remove(paste0(arg[19], "Run", num,"_Ch", i, "/", arg[35], "/output/Run", num,"_Ch", i, "_basin.yearly"))
    file.remove(paste0(arg[19], "Run", num,"_Ch", i, "/", arg[35], "/output/Run", num,"_Ch", i, "_hillslope.hourly"))
    file.remove(paste0(arg[19], "Run", num,"_Ch", i, "/", arg[35], "/output/Run", num,"_Ch", i, "_hillslope.monthly"))
    file.remove(paste0(arg[19], "Run", num,"_Ch", i, "/", arg[35], "/output/Run", num,"_Ch", i, "_hillslope.yearly"))
    
    #Read in simulated basin streamflow
    Q = vroom(paste0(getwd(), '/output/Run', num, '_Ch', i, '_basin.daily'), delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
    
    #Make a new Date column
    Q$Date = as.Date(paste0(Q$year, '-', Q$month, '-', Q$day))
    
    #Retain only streamflow and Date columns for space
    Q = as.data.frame(Q[,c('Date','streamflow')])
    
    #Trim off spin-up years
    Q = Q[which(as.Date(Q$Date) >= as.Date(arg[6])),]
    
    #Convert simulated streamflow to cfs units
    Q$streamflow = round(Q$streamflow*conversion_b, 6)
    
    #Write streamflow to file for loading in Python likelihood function
    options(scipen = 999)
    write.table(Q, file = paste0(getwd(), '/output/Q.txt'), col.names = TRUE, row.names = FALSE, sep = '\t')
    options(scipen = 0)
    
    #Compute TN using WRTDS
    TN = matrix(NA, nrow = nrow(Q), ncol = 3)
    for (qi in 1:nrow(TN)){
      TN[qi,] = predictWRTDS(Flow = Q$streamflow[qi], Date = Q$Date[qi], rowt = rowt, colt = colt, TabInt = TabInt, TabYear = TabYear, TabLogQ = TabLogQ, TabSinYear = TabSinYear, TabCosYear = TabCosYear, TabLogErr = TabLogErr, TabLogQ2 = TabLogQ2)
    }
    rm(qi)
    
    #Assign columns to TN
    TN = data.frame(Date = Q$Date, TN = round(TN[,2], 6))
    
    #Write TN to file for loading in Python likelihood function
    options(scipen = 999)
    write.table(TN, file = paste0(getwd(), '/output/TN.txt'), col.names = TRUE, row.names = FALSE, sep = '\t')
    options(scipen = 0)
    
    rm(Q,TN)
    
    #Compute likelihood for Q
    seedLQ = as.numeric(arg[41])+i+as.numeric(arg[1])*(num-1)
    sysout = system2('python', args=c(paste0(arg[38], 'Flow_MLEfits_Cal.py'), as.character(num), as.character(i), as.character(seedLQ), arg[38], arg[43], arg[45]), stdout = TRUE, stderr = TRUE)
    rm(seedLQ)
    
    cat(sysout, file=sout, append=TRUE, sep="\n")
    rm(sysout)
    
    #Compute likelihood for TN
    seedLTN = as.numeric(arg[42])+i+as.numeric(arg[1])*(num-1)
    sysout = system2('python', args=c(paste0(arg[38], 'TN_MLEfits_Cal.py'), as.character(num), as.character(i), as.character(seedLTN), arg[38], arg[44], arg[46]), stdout = TRUE, stderr = TRUE)
    rm(seedLTN)
    
    cat(sysout, file=sout, append=TRUE, sep="\n")
    rm(sysout, sout)
    
    #Read in the written csv files and sum the likelihoods
    Q_l = read.csv(paste0(getwd(), '/Params_logLQ_Run', num, '_Ch', i, '.csv'), stringsAsFactors=FALSE)
    TN_l = read.csv(paste0(getwd(), '/Params_logLTN_Run', num, '_Ch', i, '.csv'), stringsAsFactors=FALSE)
    
    likesum = Q_l$logL/nQ + TN_l$logL/nTN
    rm(Q_l, TN_l)
    
    #Used to ensure that only the essential variables were showing up on these cores.
    #Files = c(ls(), sapply(search(), ls))
    #save(Files, file = paste0('Iter', num, '_Ch', i, '.RData'))
    
    likesum
  }
  toc_parLoop = Sys.time()
  print('lls Loop Time')
  print(toc_parLoop - tic_parLoop)
  
  #Used to ensure that only essential variables were showing up in likelihood function.
  #lls, num, od, params, param_cols
  #print(ls())
  
  #Reset working directory
  setwd(od)
  
  #Return vector of log likelihoods
  return(lls)  
}

### Create cluster with n cores----
cl <- makeCluster(as.numeric(arg[1]))
registerDoParallel(cl)

#Send conversion, obs timeseries, and tables to global environment of each core
parallel::clusterExport(cl = cl, varlist = c('conversion_b', 'TabInt', 'TabYear', 'TabLogQ', 'TabLogQ2', 'TabSinYear', 'TabCosYear', 'TabLogErr', 'predictWRTDS', 'rowt', 'colt', 'nQ', 'nTN', 'likelihood_external', 'arg'))

rm(conversion_b, obs, obsTN, TabInt, TabYear, TabLogQ, TabLogQ2, TabSinYear, TabCosYear, TabLogErr, predictWRTDS, rowt, colt, nQ, nTN)

## export libraries to the computing cores----
parallel::clusterEvalQ(cl = cl, library(BayesianTools, logical.return=TRUE))
parallel::clusterEvalQ(cl = cl, library(foreach, logical.return=TRUE))
parallel::clusterEvalQ(cl = cl, library(iterators, logical.return=TRUE))
parallel::clusterEvalQ(cl = cl, library(doParallel, logical.return=TRUE))
parallel::clusterEvalQ(cl = cl, library(rlist, logical.return=TRUE))
parallel::clusterEvalQ(cl = cl, library(vroom, logical.return=TRUE))
parallel::clusterEvalQ(cl = cl, library(pracma, logical.return=TRUE))
#returning full list of loaded packages on last load
parallel::clusterEvalQ(cl = cl, library(EGRET, logical.return=FALSE))

## create BayesianSetup----
setup_Parext = createBayesianSetup(likelihood = likelihood_external, prior = prior, lower = NULL, upper = NULL, parallel = 'external', names = ParamRanges$NumberedParams)

#Load previous run data if the run didn't finish----
#Would have to load this and all previous runs an join the results together.
#load(arg[18], verbose=FALSE)
#class(temp) = c('mcmcSampler', 'bayesianOutput')

#Researting a restarted DREAM run from output----
#temp_1 = temp
#load(file = 'CurrentChain_Restart_1.RData', verbose = FALSE)
#class(temp) = c('mcmcSampler', 'bayesianOutput')

#Join the chains from the previous runs to this run
#for (i in 1:length(temp_1$chain)){
#  temp$chain[[i]] = rbind(temp_1$chain[[i]], temp$chain[[i]])
#}

#Assign setup_Parext to replace old setup (needed because the likelihood function was edited since the previous run)
temp$setup = setup_Parext

#Assign new settings
temp$settings$iterations = as.numeric(arg[20])
temp$settings$eps = as.numeric(arg[21])
temp$settings$e = as.numeric(arg[22])
temp$settings$ZupdateFrequency = as.numeric(arg[23])
temp$settings$pSnooker = as.numeric(arg[24])
temp$settings$DEpairs = as.numeric(arg[25])
temp$settings$nCR = as.numeric(arg[26])
temp$settings$updateInterval = as.numeric(arg[27])
temp$settings$burnin = as.numeric(arg[28])
temp$settings$adaptation = as.numeric(arg[29])
temp$settings$thin = as.numeric(arg[30])

#Trim the archive be removing all randomly initialized states.
#Not needed for restart of restart.

## runMCMC----
set.seed(as.numeric(arg[2]))
setwd(arg[19])
out_Parext_Restart <- runMCMC(bayesianSetup = temp, sampler = "DREAMzs")

print('DREAM complete. Saving output')

# Save list of chains----
for (i in 1:length(out_Parext_Restart$chain)){
  out_Parext_Restart$chain[[i]] = signif(out_Parext_Restart$chain[[i]],6)
}
list.save(out_Parext_Restart$chain, paste0('OutputChains', arg[39], '.yaml'))

# Save session information----
Info = sessionInfo()
# Save entire workspace info----
save.image(file = paste0('OutputWorkspace', arg[39], '.RData'), safe = FALSE)

#Copy the IterNum.txt file because plots and summary info will run the likelihood function again. Want to continue using the old IterNum for that to avoid overwriting output files. But also want to save the end-of-chain IterNum.
file.copy(from = paste0(arg[19], '/IterNum.txt'), to = paste0(arg[19], '/IterFinalNum_ChainRun', arg[39], '.txt'))

print(out_Parext_Restart$settings$runtime)

stopCluster(cl)