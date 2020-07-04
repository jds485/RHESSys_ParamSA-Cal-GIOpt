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

#Read in command line calls----
#Fixme: Add likelihood function directories to arg
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
#18 - File with chain starting locations (.csv file with headers)
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

arg = commandArgs(T)

#Load in the modified WRTDS code----
source(arg[3])
#Remove unnecessary functions
rm(estCrossVal, estSurfaces, FillTableNAs, modelEstimation, plotContours, run_WRTDS, runSurvReg, surfaceIndex)

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

#Compute streamflow conversion factor
world = read.csv(arg[8], stringsAsFactors = FALSE)
res = as.numeric(arg[9])
#Taking the unique patch IDs because strata can exist in more than one patch.
Area.basin = length(unique(world$patchID))*res^2
rm(world, res)
#Multiplier conversion for basin streamflow (mm/d)*conversion_b -> cfs
conversion_b = Area.basin/1000/(.3048^3)/24/3600
rm(Area.basin)

#Load WRTDS tables
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
ParamRanges = read.csv(arg[17], stringsAsFactors = FALSE)

#Load the matrix of starting values to be used for the chains---- 
#Initially this is from the SA. This file should be the processed output from DREAM_ParameterBoundChecks_ChainStarts.py 
#For restarts of DREAM, this is not needed because the output from DREAM knows the last point in the chain, which is the starting point for restarts.
startValues = as.matrix(read.csv(file = arg[18], stringsAsFactors = FALSE))
#startValues = as.matrix(read.csv(file = '/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/BaismanChainStarts_AfterProcessing_Top3.csv', stringsAsFactors = FALSE))

#Define the Multivariate Prior----
prior = createUniformPrior(lower = ParamRanges$Lower, upper = ParamRanges$Upper, best = NULL)

#Define the Likelihood with External Parallelization----
#This likelihood function implements parallel allocation (foreach loop); accepts a matrix of chains as rows, parameters as columns; and returns a vector of log-likelihoods, one element per chain.

#Function has several steps:
#0 - Determine the current step in the chain
#1 - check that params are within their constraints, and adjust if needed
#---In parallel---
#2 - update def files with those params and make directories for each chain's RHESSys run
#3 - call GIS2RHESSys operations 
#4 - Run RHESSys 
#5 - Run WRTDS ECM models 
#6 - Compute likelihoods
likelihood_external <- function(param){
  od = getwd()
  
  #Check to see if there are any files of the format IterNum.txt in the working directory
  #Fixme: Directory
  setwd("/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/")
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
  seed = 75843 + num
  #Fixme: Directories
  system(paste0("singularity exec /share/resources/containers/singularity/rhessys/rhessys_v3.img python /scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/DREAM_ParameterBoundChecks.py '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/' '", seed, "' '/scratch/js4yd/Baisman30mDREAMzs/RHESSys_Baisman30m_g74/defs/' '10' 'BaismanCalibrationParameterProblemFile.csv' 'Chain_", num,"'"), intern = TRUE)
  rm(seed)
  
  print(paste0('Files set up for chain step num ', num, '. Running lls loop over all chains.'))
  
  #Used to make sure the essential variables were available
  #num, od, param, param_cols
  #print(ls())
  
  tic_parLoop = Sys.time()
  
  # Run RHESSys, compute TN, and compute likelihoods in parallel
  #Fixme: export, maybe a no export on param?
  lls = foreach::foreach(i=1:ifelse(is.null(nrow(param)), 1, nrow(param)), .combine = c, .inorder = TRUE) %dopar%{
    setwd("/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/")
    
    #Create the .out file that will be used for system2 commands
    #The path must not have sfs because it will run in Singularity
    sout = paste0('/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/output/Run', num, '_Ch', i, '.out')
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
    
    sysout = system2('singularity', args=c('exec', '/share/resources/containers/singularity/rhessys/rhessys_v3.img', 'python', '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/MakeDefs_fn_Chains.py', paste0('Chain_', num,'_AfterProcessing.csv'), '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/', '/scratch/js4yd/Baisman30mDREAMzs/RHESSys_Baisman30m_g74/defs/', 'RHESSys_Baisman30m_g74', 'BaismanCalibrationParameterProblemFile.csv', '10', as.character(num), as.character(i)), stdout = TRUE, stderr = TRUE)
    
    cat(sysout, file=sout, append=TRUE, sep="\n")
    rm(sysout)
    
    #Change into ith directory that was just made and copy in files needed to run RHESSys
    # full path to the project location;
    setwd(paste0("/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i))
    
    #Make directories that RHESSys needs and copy only the required files into them
    dir.create('./grass_dataset')
    file.copy(from = "/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/grass_dataset/g74_RHESSys_Baisman30m_g74", to = './grass_dataset', recursive = TRUE)
  
    dir.create('./GIS2RHESSys')
    dir.create('./GIS2RHESSys/libraries')
    file.copy(from = "/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/GIS2RHESSys/lulcCollectionEC_Cal.csv", to = './GIS2RHESSys')
    file.copy(from = "/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/GIS2RHESSys/soilCollection_Cal.csv", to = './GIS2RHESSys')
    file.copy(from = "/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/GIS2RHESSys/vegCollection_modified_Cal.csv", to = './GIS2RHESSys')
    #The vegetation file needs to be modified by the def file data
    #Run the vegetation modification script
    sysout = system2('singularity', args=c('exec', '/share/resources/containers/singularity/rhessys/rhessys_v3.img', 'python', '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/ModifyVeg.py', paste0('/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run', num, '_Ch', i, '/GIS2RHESSys'), paste0('/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run', num, '_Ch', i, '/RHESSys_Baisman30m_g74'), paste0('/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run', num, '_Ch', i, '/RHESSys_Baisman30m_g74/defs/'), 'vegCollection_modified_Cal.csv'), stdout = TRUE, stderr = TRUE)
    
    cat(sysout, file=sout, append=TRUE, sep="\n")
    rm(sysout)
    
    file.copy(from = "/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/GIS2RHESSys/libraries/g2w_cf_RHESSysEC.R", to = './GIS2RHESSys/libraries')
    file.copy(from = "/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/GIS2RHESSys/libraries/LIB_RHESSys_writeTable2World.R", to = './GIS2RHESSys/libraries')
    
    dir.create('./Date_analysis')
    file.copy(from = "/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/Date_analysis/climate_extension.R", to = './Date_analysis')
    file.copy(from = "/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/Date_analysis/LIB_dailytimeseries3.R", to = './Date_analysis')
    file.copy(from = "/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/Date_analysis/LIB_misc.r", to = './Date_analysis')
    
    #Copy folders and files to ./"$RHESSysNAME"
    file.copy(from = "/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/RHESSys_Baisman30m_g74/worldfiles", to = './RHESSys_Baisman30m_g74', recursive = TRUE)
    file.copy(from = "/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/RHESSys_Baisman30m_g74/tecfiles", to = './RHESSys_Baisman30m_g74', recursive = TRUE)
    file.copy(from = "/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/RHESSys_Baisman30m_g74/output", to = './RHESSys_Baisman30m_g74', recursive = TRUE)
    file.copy(from = "/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/RHESSys_Baisman30m_g74/flows", to = './RHESSys_Baisman30m_g74', recursive = TRUE)
    file.copy(from = "/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/RHESSys_Baisman30m_g74/clim", to = './RHESSys_Baisman30m_g74', recursive = TRUE)
    file.copy(from = "/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/RHESSys_Baisman30m_g74/soil_cat_mukey.csv", to = './RHESSys_Baisman30m_g74')
    file.copy(from = "/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/RHESSys_Baisman30m_g74/lulcFrac30m.csv", to = './RHESSys_Baisman30m_g74')
    
    #Now update the worldfile
    # set paths for RHESSys input files
    # 1 = Yes, output this file; 0 = No, do not output this file
    #Using > instead of >> here to clobber the old contents and start anew.
    system(paste0("echo outputWorldfile \"/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/worldfiles/worldfile.csv\" 1 > '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo outputWorldfileHDR \"/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/worldfiles/worldfile.hdr\" 0 >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo outputDefs \"/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/defs\" 0 >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo outputSurfFlow \"/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/flows/surfflow.txt\" 0 >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo outputSubFlow \"/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/flows/subflow.txt\" 0 >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    
    # set climate station ID and file name
    system(paste0("echo stationID 101 >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo stationFile \"clim/Cal_Feb2020Revised.base\" >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    
    # the following maps that must be provided with syntax:
    system(paste0("echo basinMap basin >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo hillslopeMap hill >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo zoneMAP zone_cluster >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo patchMAP patch >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo soilidMAP soil_texture >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo xMAP xmap >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo yMAP ymap >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo demMAP dem >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo slopeMap slope >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo aspectMAP aspect >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo twiMAP wetness_index >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo whorizonMAP west_180 >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo ehorizonMAP east_000 >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo isohyetMAP isohyet >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo rowMap rowmap >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo colMap colmap >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo drainMap drain >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    # ... impervious and its breakdown
    system(paste0("echo impFracMAP impFrac >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo roofMAP roofFrac >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo drivewayMAP drivewayFrac >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo pavedRoadFracMAP pavedroadFrac >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    # ... forest vegetations
    system(paste0("echo forestFracMAP forestFrac >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo tree1StratumID tree1StratumID >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo tree1FFrac tree1FFrac >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo tree1LAI tree1LAI >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    # ... shrub vegetation
    system(paste0("echo shrubFracMAP shrubFrac >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    #echo shrub1StratumID shrub1StratumID >> "$templateFile"
    #echo shrub1FFrac shrub1FFrac >> "$templateFile"
    #echo shrub1LAI shrub1LAI >> "$templateFile"
    # ... crop vegetation
    system(paste0("echo cropFracMAP cropFrac >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    #echo crop1StratumID crop1StratumID >> "$templateFile"
    #echo crop1FFrac crop1FFrac >> "$templateFile"
    #echo crop1LAI crop1LAI >> "$templateFile"
    # ... lawn/pasture vegetation
    system(paste0("echo grassFracMAP lawnFrac >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo grass1StratumID grass1StratumID >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo grass1FFrac grass1FFrac >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo grass1LAI grass1LAI >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    # ... modeling stream-grids
    system(paste0("echo streamMap str >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    # The following maps are optional; User can comment out the lines that do not apply using "#" up front.
    system(paste0("echo streamFullExtension strExt >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    #echo unpavedroadMap NA >> "$templateFile"
    system(paste0("echo riparianMAP riparian_hands >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    #echo sewercoverMAP sewercover >> "$templateFile"
    system(paste0("echo septicMAP septic >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    #echo pipecoverMAP NA >> "$templateFile"
    system(paste0("echo stormdrainMAP roadExit >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo compactedsoilMAP compactedsoil >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))
    system(paste0("echo additionalSurfaceDrainMAP addsurfdrain >> '/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt'"))

    #Run g2w script
    sysout = system2('singularity', args=c('exec', '/share/resources/containers/singularity/rhessys/rhessys_v3.img', 'grass74', paste0('/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run', num,'_Ch', i, '/grass_dataset/g74_RHESSys_Baisman30m_g74/PERMANENT'), '--exec', 'Rscript', paste0('/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run', num,'_Ch', i, '/GIS2RHESSys/libraries/g2w_cf_RHESSysEC.R'), paste0('/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run', num,'_Ch', i), paste0('/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run', num,'_Ch', i, '/RHESSys_Baisman30m_g74/vegCollection_modified_Cal.csv'), paste0('/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run', num,'_Ch', i, '/GIS2RHESSys/soilCollection_Cal.csv'), paste0('/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run', num,'_Ch', i, '/GIS2RHESSys/lulcCollectionEC_Cal.csv'), paste0('/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run', num,'_Ch', i, '/RHESSys_Baisman30m_g74/g2w_template.txt')), stdout = TRUE, stderr = TRUE)
    
    cat(sysout, file=sout, append=TRUE, sep="\n")
    rm(sysout)
      
    #Run to create worldfile
    sysout = system2('singularity', args=c('exec', '/share/resources/containers/singularity/rhessys/rhessys_v3.img', 'Rscript', paste0('/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run', num,'_Ch', i, '/GIS2RHESSys/libraries/LIB_RHESSys_writeTable2World.R'), 'NA', paste0('/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run', num,'_Ch', i, '/RHESSys_Baisman30m_g74/worldfiles/worldfile.csv'), paste0('/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run', num,'_Ch', i, '/RHESSys_Baisman30m_g74/worldfiles/worldfile')), stdout = TRUE, stderr = TRUE)
    
    cat(sysout, file=sout, append=TRUE, sep="\n")
    rm(sysout)
    
    #Delete the GRASS directory and code libraries for space concerns
    unlink(paste0("/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/grass_dataset"), recursive=TRUE)
    unlink(paste0("/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/Date_analysis"), recursive=TRUE)
    unlink(paste0("/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/GIS2RHESSys"), recursive=TRUE)
    file.remove(paste0("/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/lulcFrac30m.csv"))
    file.remove(paste0("/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/soil_cat_mukey.csv"))
    file.remove(paste0("/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/vegCollection_modified_Cal.csv"))
    file.remove(paste0("/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/g2w_template.txt"))
    
    setwd(paste0("/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74"))
    
    #Run RHESSys
    sysout = system2('/sfs/lustre/bahamut/scratch/js4yd/RHESSysEastCoast_Optimized/rhessys5.20.0.develop_optimsize', args=c('-st', '1999', '11', '15', '1', '-ed', '2013', '10', '1', '1', '-b', '-h', '-newcaprise', '-gwtoriparian', '-capMax', '0.01', '-slowDrain', '-t', 'tecfiles/tec_daily_cal.txt', '-w', 'worldfiles/worldfile', '-whdr', 'worldfiles/worldfile.hdr', '-r', 'flows/subflow.txt', 'flows/surfflow.txt', '-pre', paste0('output/Run', num, '_Ch', i), '-s', '1', '1', '1', '-sv', '1', '1', '-gw', '1', '1', '-svalt', '1', '1', '-vgsen', '1', '1', '1', '-snowTs', '1', '-snowEs', '1', '-capr', '0.001'), stdout = TRUE, stderr = TRUE)
    
    cat(sysout, file=sout, append=TRUE, sep="\n")
    rm(sysout)
    
    #Remove the flows, tecfiles, defs, and clim folders.
    unlink(paste0("/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/flows"), recursive = TRUE)
    unlink(paste0("/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/clim"), recursive = TRUE)
    unlink(paste0("/scratch/js4yd/Baisman30mDREAMzs/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/tecfiles"), recursive = TRUE)
    unlink(paste0("/scratch/js4yd/Baisman30mDREAMzs-10Ch-3/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/defs"), recursive = TRUE)
    #Remove some un-needed output
    file.remove(paste0("/scratch/js4yd/Baisman30mDREAMzs-10Ch-3/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/output/Run", num,"_Ch", i, "_basin.hourly"))
    file.remove(paste0("/scratch/js4yd/Baisman30mDREAMzs-10Ch-3/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/output/Run", num,"_Ch", i, "_basin.monthly"))
    file.remove(paste0("/scratch/js4yd/Baisman30mDREAMzs-10Ch-3/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/output/Run", num,"_Ch", i, "_basin.yearly"))
    file.remove(paste0("/scratch/js4yd/Baisman30mDREAMzs-10Ch-3/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/output/Run", num,"_Ch", i, "_hillslope.hourly"))
    file.remove(paste0("/scratch/js4yd/Baisman30mDREAMzs-10Ch-3/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/output/Run", num,"_Ch", i, "_hillslope.monthly"))
    file.remove(paste0("/scratch/js4yd/Baisman30mDREAMzs-10Ch-3/RHESSysRuns/Run", num,"_Ch", i, "/RHESSys_Baisman30m_g74/output/Run", num,"_Ch", i, "_hillslope.yearly"))
    
    #Read in simulated basin streamflow
    Q = vroom(paste0(getwd(), '/output/Run', num, '_Ch', i, '_basin.daily'), delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
    
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
    seedLQ = 518+i+39*(num-1)
    #system(paste0("python /sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/LikelihoodFun/Flow_MLEfits_Cal.py '", num, "' '", i, "' '", seedLQ, "'"), intern=TRUE)
    sysout = system2('python', args=c('/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/LikelihoodFun/Flow_MLEfits_Cal.py', as.character(num), as.character(i), as.character(seedLQ)), stdout = TRUE, stderr = TRUE)
    rm(seedLQ)
    
    cat(sysout, file=sout, append=TRUE, sep="\n")
    rm(sysout)
    
    #Compute likelihood for TN
    seedLTN = 185+i+39*(num-1)
    #system(paste0("python /sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/LikelihoodFun/TN_MLEfits_Cal.py '", num, "' '", i, "' '", seedLTN, "'"), intern=TRUE)
    sysout = system2('python', args=c('/sfs/lustre/bahamut/scratch/js4yd/Baisman30mDREAMzs/LikelihoodFun/TN_MLEfits_Cal.py', as.character(num), as.character(i), as.character(seedLTN)), stdout = TRUE, stderr = TRUE)
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
parallel::clusterExport(cl = cl, varlist = c('conversion_b', 'TabInt', 'TabYear', 'TabLogQ', 'TabLogQ2', 'TabSinYear', 'TabCosYear', 'TabLogErr', 'predictWRTDS', 'rowt', 'colt', 'nQ', 'nTN', 'likelihood_external'))

rm(conversion_b, obs, obsTN, TabInt, TabYear, TabLogQ, TabLogQ2, TabSinYear, TabCosYear, TabLogErr, predictWRTDS, rowt, colt, nQ, nTN)

## export libraries to the computing cores----
parallel::clusterEvalQ(cl = cl, library(BayesianTools))
parallel::clusterEvalQ(cl = cl, library(foreach))
parallel::clusterEvalQ(cl = cl, library(iterators))
parallel::clusterEvalQ(cl = cl, library(doParallel))
parallel::clusterEvalQ(cl = cl, library(rlist))
parallel::clusterEvalQ(cl = cl, library(vroom))
parallel::clusterEvalQ(cl = cl, library(pracma))
parallel::clusterEvalQ(cl = cl, library(EGRET))

## create BayesianSetup----
setup_Parext = createBayesianSetup(likelihood = likelihood_external, prior = prior, lower = NULL, upper = NULL, parallel = 'external', names = ParamRanges$NumberedParams)

#Create settings----
## For this case we want to parallelize the internal chains, therefore we create a n row matrix with startValues, if you parallelize a model in the likelihood, do not set a n*row Matrix for startValue
settings_Parext = list(iterations = as.numeric(arg[20]), #Need at least one more iteration than the burnin to report a value to the output chain
                        gamma= NULL, #Not used in code
                        eps = as.numeric(arg[21]), #Not adding normal noise to the proposal update.
                        e = as.numeric(arg[22]), #Multiplying gamma by 1.05 in proposal update 
                        parallel = NULL, #Specified in setup
                        Z = NULL, #Computed in code. Supply upon manual restart.
                        ZupdateFrequency = as.numeric(arg[23]), #Z is not updated until after burnin if the update frequency is greater than 1. Making equal to updateInterval.
                        pSnooker = as.numeric(arg[24]), #10% chance of a snooker update
                        DEpairs = as.numeric(arg[25]), #Number of chains used to compute the DE proposal update. In papers, randomly choose 1, 2, or 3 to use.
                        nCR = as.numeric(arg[26]), #Update about 12 variables each time. nCR >=2 recommended in papers.
                        pCRupdate = TRUE, #Use crossover updates
                        updateInterval = as.numeric(arg[27]), #Must be greater than or equal to nCR. Best to be smaller.
                        burnin = as.numeric(arg[28]), #burnin must be greater than or equal to adaptation, or all of the adaptation steps must be manually removed before summarizing chains.
                        adaptation = as.numeric(arg[29]), #Adaptation should be about 20 % of the total NFE. Doing this manually.
                        thin = as.numeric(arg[30]), #Thin = 1 to allow for post-solver manual thinning
                        message = FALSE, #Messages do not print to .out file.
                        startValue = startValues) #Use the specified starting values for each chain.

## runMCMC----
set.seed(as.numeric(arg[2]))
setwd(arg[19])
out_Parext <- runMCMC(settings = settings_Parext, bayesianSetup = setup_Parext, sampler = "DREAMzs")

print('DREAM complete. Saving output')

# Save list of chains----
for (i in 1:length(out_Parext$chain)){
  out_Parext$chain[[i]] = signif(out_Parext$chain[[i]],6)
}
list.save(out_Parext$chain, 'OutputChains.yaml')

# Save session information----
Info = sessionInfo()
# Save entire workspace info----
save.image(file = 'OutputWorkspace.RData', safe = FALSE)

#Researting a DREAM run from output----
#load(file = 'CurrentChain.RData', verbose = FALSE)
#class(temp) = c('mcmcSampler', 'bayesianOutput')

#Change the settings, if needed.
#No adaptation and burnin needed for the restart.
#temp$settings$burnin = 0
#temp$settings$adaptation = 0
#temp$settings$iterations = 1000

#out_Parext_Restart <- runMCMC(bayesianSetup = temp, sampler = "DREAMzs")

#Researting a restarted DREAM run from output----
#temp_1 = temp
#load(file = 'CurrentChain_Restart_1.RData', verbose = FALSE)
#class(temp) = c('mcmcSampler', 'bayesianOutput')

#Change the settings, if needed.
#No adaptation and burnin needed for the restart.
#temp$settings$burnin = 0
#temp$settings$adaptation = 0
#temp$settings$iterations = 1000

#Join the chains from the previous runs to this run
#for (i in 1:length(temp_1$chain)){
#  temp$chain[[i]] = rbind(temp_1$chain[[i]], temp$chain[[i]])
#}

#out_Parext_Restart_1 <- runMCMC(bayesianSetup = temp, sampler = "DREAMzs")

#Copy the IterNum.txt file because plots and summary info will run the likelihood function again. Want to continue using the old IterNum for that to avoid overwriting output files. But also want to save the end-of-chain IterNum.
file.copy(from = paste0(arg[19], '/IterNum.txt'), to = paste0(arg[19], '/IterFinalNum_ChainRun.txt'))

print(out_Parext$settings$runtime)

stopCluster(cl)