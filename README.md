**Readme for RHESSys_ParamSA+Cal+GIOpt**
This repository contains code that was used to run the RHESSys ecohydrological model and complete a global sensitivity analysis of model parameters, Bayesian calibration of model parameters, and multi-objective spatial optimization of Green Infrastructure that is robust to the uncertainty in model parameter values. The repository consists of the following directories, each with their own README file:

**Sensitivity Analysis**
Contains code for a Morris sensitivity analysis of RHESSys model parameters.

Corresponds to the manuscript:
Smith, J.D., L. Lin, J.D. Quinn, and L.E. Band. (in prep.). Guidance on evaluating model parameter uncertainty at decision-relevant scales. To be submitted to Hydrology and Earth Systems Science.

**LikelihoodData**
Contains code for the generalized normal distribution (skew exponential power) used for the sensitiivty analysis and calibration studies.

**Calibration**
Contains code for DREAMzs Bayesian calibration using the BayesianTools R package. The calibrated model is used in the robust optimization study.

**GI_GeometryCheck**
Contains code to obtain the maximum possible amount of green infrastructure that could be added to each patch (grid cell) within a RHESSys model. This is used to inform the decision variables and constraints in the optimization study.

**Optimization**
Contains code for the optimization of green infrastructure using the Borg multi-objective evolutionary algorithm.