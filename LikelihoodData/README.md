# Readme for the LikelihoodData Directory
Scripts in this directory were used to obtain MLE estimates of the generalized normal distribution (skewed exponential power) for the SensitivityAnalysis and Calibration studies. Some of these scripts will try to load intermediate files that are generated from running the SensitivityAnalysis code (but that will not work unless you have access to those results). These files are several GB in size, so are not provided here or currently hosted elsewhere (location for these files is to be determined).

## Files
1. likelihood.py: Script containing the skewed exponential power likelihood function.
2. errorModel.py: Script used to test which likelihood parameter bounds should be selected and how many random Latin Hypercube samples would be needed to obtain converged MLE estimates.
3. Flow_MLEFits.py, TN_MLEFits.py: Scripts for maximum likelihood estimation of the likelihood function parameters for flow and TN. NoBC in the file name indicates that no Box Cox transform was used. Cal in the filename indicates the file was used for the Calibration directory. m and w at the end of the filename indicate that the likelihood corresponds to monthly and weekly data, as opposed to daily data. Add5 in the filename was used for the SensitivityAnalysis study for 5 replicates that originally did not run properly.
4. Flow_MLEFits.sh, TN_MLEFits.sh: Shell scripts used to run parallel estimation of the likelihood function parameters using MPI.
5. collectParams.py: Script to gather the results of the likelihood parameter estimating into one file for flow, and one file for TN. Add5 in the filename was used for the SensitivityAnalysis study for 5 replicates that originally did not run properly.
6. Plot_logL_v_SSE.py: Script to make figures of the log likelihood versus sum of squared errors.
7. GL_....R: R implementation of the generalized likelihood function. Used in the optimization case study.
