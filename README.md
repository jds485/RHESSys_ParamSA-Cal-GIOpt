# Readme for RHESSys_ParamSA-Cal-GIOpt[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11894592.svg)](https://doi.org/10.5281/zenodo.11894592)
This repository contains code that was used to run the RHESSys ecohydrological model and complete a Morris global sensitivity analysis of model parameters, Bayesian DREAMzs calibration of model parameters, and multi-objective spatial optimization of Green Infrastructure that is robust to the uncertainty in model parameter values. The repository consists of the following directories, each with their own README file:


## Sensitivity Analysis
Contains code for a Morris sensitivity analysis of RHESSys model parameters.

This code was used for the manuscript:

Smith, J.D., L. Lin, J.D. Quinn, and L.E. Band. Guidance on evaluating parametric model uncertainty at decision-relevant scales. Hydrology and Earth System Sciences. https://doi.org/10.5194/hess-26-2519-2022.

Credit: The WRTDS_ModifiedFunctions.R were originally from USGS functions in the [EGRET](https://github.com/USGS-R/EGRET) package. They are modified according to [GitHub issue #251](https://github.com/USGS-R/EGRET/issues/251)


## LikelihoodData
Contains code for the generalized normal distribution (skew exponential power) used for the sensitiivty analysis, calibration, and optimization studies. 

Credit: The likelihood.py script contains a modified version of the generalizedLikelihoodFunction function found within the [spotpy](https://github.com/thouska/spotpy) repository’s [likelihoods.py script from March 7th, 2018](https://github.com/thouska/spotpy/blob/3862cd2e6e0881c7a78d081a5b42e4094d359a45/spotpy/likelihoods.py).

•	[Julianne Quinn](https://github.com/julianneq) modified this function to allow for unequally spaced samples, as is the case for water quality data in this study. Measurement error sections were commented out. The bias computation was corrected according to closed GitHub issue #245. The returned likelihood was negated for minimization.

•	[Jared Smith](https://github.com/jds485) corrected the computation of residual errors according to closed GitHub issue #257, and added options to remove seasonality in residuals.

•	[Zachary Brodeur](https://github.com/zpb4/) provided the generalized likelihood equations in R.


## Calibration
Contains code for running and interpreting DREAMzs Bayesian calibration of a RHESSys hydrological model using the [BayesianTools](https://github.com/florianhartig/BayesianTools) R package. The calibrated model is used in the robust optimization study.


## GI_GeometryCheck
Contains code to obtain the maximum possible amount of green infrastructure that could be added to each patch (grid cell) within a RHESSys model. This is used to inform the decision variables and constraints in the optimization study.


## Optimization
Contains code for the multi-objective robust optimization of green infrastructure using the Borg multi-objective evolutionary algorithm.
