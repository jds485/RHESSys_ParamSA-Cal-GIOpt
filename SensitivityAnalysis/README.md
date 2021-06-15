# Readme for SensitivityAnalysis Directory
This readme provides a step-by-step set of instructions that describe how to set up, run, and analyze results for a Morris sensitivity analysis of a Regional HydroEcological Simulation System (RHESSys) model. The instructions are intended to be general, such that any project may rely on this step-by-step process. That said, the instructions are based on the steps needed for the cited Smith et al. study, and modifications to code and/or to the steps may be needed for other studies. Guidelines are provided where code edits may be required.

## Data Repository for Smith et al.
The code depends on a data repository located on [HydroShare](add link here). 

## Running the Code
A description of how to run the code is provided in the MorrisSA-Workflow PDF. For the Smith et al. study, data and code referenced in the workflow are in the data repository.

## File Descriptions in Use Order
The files used in the Smith et al. study are briefly described below. The repository has several additional files that were used in testing or for error checking.

1.	**workflows_Singularity_Baisman30m_Rivanna_FromScratch.sh**: used to run GIS2RHESSys preprocessor to generate the necessary input files to RHESSys.
2.	**MorrisSampleCreation.sh**: Shell script that calls scripts that generate Morris samples and corrects them to match constraints on their values.
3.	**MorrisSamplingLocations_BeforeProcessing.R**: Script that generates Morris sample trajectories.
4.	**MorrisSampling.py**: Script that corrects parameter values to meet their constraints.
5.	**Sum3CheckFun.py**: Script that provides a function for MorrisSampling.py.
6.	**MorrisSamplingDiagnostics.R**: Script that makes a plot of parameter correlations before and after constraint processing has been completed.
7.	**RunArray.sh (and variant names)**: Shell script to update the def files with MakeDefs.py, update the vegetation collection file with ModifyVeg.py, run GIS2RHESSys to ONLY update the worldfile, and run the RHESSys model.
8.	**MakeDefs_fn.py**: This function updates the def files with the values used for the Morris replicate.
9.	**ModifyVeg.py**: Modifying the def files for vegetation is not enough to start RHESSys because the GIS2RHESSys preprocessor needs to know the values of the vegetation parameters in order to assign patch-specific values to the worldfile. This script updates the vegCollection csv file to match the def parameter values used for the Morris replicate so that GIS2RHESSys can do that update.
10.	**RenameOutputFiles.sh**: renames the .out files so that their index number match the Python index numbers instead of the job array numbers.
11.	**SummarizeTimeseries.R**: Script that processes all of the SA replicates into basin and hillslope files.
12.	**CheckOutput.R**: Contains functions that are used to check output files for errors and missing runs.
13.	**RenameOutputFiles.sh**: script to rename .out files to match the Python index number instead of the SLURM job array index number.
14.	**WRTDS_modifiedFunctions.R**: Modifications to functions in the EGRET package to make interpolation tables of parameters estimated from WRTDS regressions.
15.	**ExtractTN.sh**: shell script to run a job array with the TNFileExtraction.R script
16.	**TNFileExtraction.R**: Runs WRTDS regression for each of the SA replicates and writes results to one txt file per replicate for each of basin and hillslope results.
17.	**ErrCheckTN.sh**: shell script to run ErrCheckTNFileExtraction.R
18.	**ErrCheckTNFileExtraction.R**: Checks the .out files created during ExtractTN for any errors.
19.	**JoinTN_Basin.sh, JoinTN_Hill05.sh, JoinTN_HillMed.sh, JoinTN_Hill95.sh**: shell scripts that run the TNFileJoining scripts. 
20.	**TNFileJoining_Basin.R, TNFileJoining_Hill05.R, TNFileJoining_HillMed.R, TNFileJoining_Hill95.R**: scripts to compile all of the TN SA replicates into basin or hillslope files.
21.	**(DEPRECATED) ReorderingSAresults.R**: For the initial studies, a code error caused the replicates to be ordered incorrectly (by name instead of number). This script was used to reorder the files to the correct trajectory order. It should no longer be needed.
22.	**likelihood.py**: script containing the skewed exponential power likelihood function.
23.	**Flow_MLEFits.py, TN_MLEFits.py**: Scripts for maximum likelihood estimation of the likelihood function parameters for flow and TN.
24.	**Flow_MLEFits.sh, TN_MLEFits.sh**: Shell scripts used to run parallel estimation of the likelihood function parameters using MPI.
25.	**collectParams.py**: Script to gather the results of the likelihood parameter estimating into one file for flow, and one file for TN.
26.	**Plot_logL_v_SSE.py**: Script to make figures of the log likelihood versus sum of squared errors.
27.	**RHESSysSensitvityAnalysisSetup.R**: Used to set up datasets for elementary effect computations.
28.	**ExtractSA_AllMetrics_[b, h1, or h2].sh**: shell script to run a job array over each of the Morris trajectories for basin or hillslopes. Hillslope data processing was split into 2 due to data size limitations.
29.	**ExtractSAtrajectories_AllMetrics_[b, h1, or h2].R**: Computes elementary effects for all parameters for each trajectory.
30.	**JoinSA_AllMetrics.sh**: shell script that runs the JoinSAtrajectories_AllMetrics.R script.
31.	**JoinSAtrajectories_AllMetrics.R**: script to compile all of the elementary effects into one file for each of the SA metrics.
32.	**RHESSysSensitvityAnalysis.R**: Used to process the results of the EE computation for each variable at the basin and hillslope scale.
