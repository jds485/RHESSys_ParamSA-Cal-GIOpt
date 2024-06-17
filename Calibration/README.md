# Readme for the Calibration Directory
The code in this directory was used to select parameters to calibrate (based on results in the SensitivityAnalysis directory), calibrate those parameters using the DREAMzs algorithm in a modified version of the BayesianTools R package, and plot resulting summary metrics. The provided DREAMzsCal-Workflow.pdf describes how to use these files.

## Files in Use Order
1.	workflows_Singularity_Baisman30m_Rivanna_GI.sh: Script to allow adding green infrastructure to the original Baisman Run RHESSys model setup.
2.	ModifyVeg.py: Script to automatically modify RHESSys vegetation definition files during calibration based on proposed parameter values for tree species.
3.	RHESSysParamSelection.R: Used to select parameters for calibration based on their Elementary Effects.
4.	AnalyzeLikelihoods.R: Script to evaluate the likelihood of parameter sets and to select the MCMC chain starting locations based on those likelihoods.
5.	DREAM_ParameterBoundChecks_ChainStarts....py: Script to check and adjust (if needed) the parameter constraints for the MCMC chain starting locations.
6.	RunChainStartsCheck.sh: Script used to run the previous file.
7.	DREAM_ParameterBoundChecks....py: Script to check and adjust (if needed) the parameter constraints for the MCMC chain proposal locations.
8.	MakeDefs_fn_Chains.py: Script to make RHESSys def files for the MCMC chain runs with the calibration parameters.
9.	Flow_MLEFits_Cal.py, TN_MLEFits_Cal.py: Scripts used during calibration for maximum likelihood estimation of the likelihood function parameters for flow and TN. These are found in the LikelihoodData directory in this repository.
10.	GenCRFiles_DREAMrestart.R: Script to generate the CR files for a restart of the DREAM calibration (i.e., to continue the Markov chain from where it left off).
11.	RHESSysDREAM_NoG2W_..._arg....R: Scripts to setup and run the DREAM MCMC calibration. arg indicates that command line arguments have been added and passed to the likelihood function. The scripts without \_r\_ or \_rr\_ are the initial MCMC runs. With r it is the first restart, and rr is every subsequent restart of the MCMC chains.
12.	RunRHESSysDREAMCal-10Ch_..._arg....sh: Shell scripts to run the previous files.
13.	JoinDREAM....R: Script to join the output from the completed DREAM run into fewer files.
14.	RunJoinDREAM.sh: Shell script to run the previous file.
15.	MoveDREAMFiles....sh: Script to move DREAM output files to permanent storage directories.
16.	PlotRHESSysDREAM.R: Script to plot and analyze DREAM output.
17.	marginalPlot_trueVals.R: Script that contains a modified marginal distribution function for the [BayesianTools R package](https://github.com/florianhartig/BayesianTools).

## Other Files
1. The Example_Learning directory contains simple example scripts used while learning how to use the BayesianTools package.
2. The FullRegion directory contains scripts from an earlier simulation that used the entire Baisman Run watershed instead of two of its hillslopes (hillslopes 9 and 10)
