# Readme for the Optimization Directory - In Progress

## parallel Directory
Contains sub-directories for the three green infrastructure optimizations: MAP, MORO, and Synthetic. These were completed using the multi-master implementation of the Borg evolutionary algorithm. Users interested in this algorithm must [request permission](http://borgmoea.org/#contact) to obtain it and use it, and must also [write to Pat Reed](https://www.engineering.cornell.edu/faculty-directory/patrick-michael-reed) to request permission to access the multi-master repository that was used for this project. Within the borg-moea repository, the passNFE branch was used for this project.

This directory also contains scripts used to process the resulting Pareto fronts using the [MOEA framework Demo file, version 2.12](http://moeaframework.org/) and [Matthew Woodruff and Jon Herman's pareto.py script](https://github.com/matthewjwoodruff/pareto.py/blob/master/pareto.py). The processingSteps.txt file describes how to use these scripts. Some of these scripts were modified from [Julianne Quinn's Lake_Problem_DPS repository](https://github.com/julianneq/Lake_Problem_DPS/tree/master/Optimization).

## Other Files
1. RunRHESSys_Synthetic.sh: Script to run the RHESSys executable with the synthetic true parameter values. The generated data serve as the synthetic observed data for this study.
2. CalcObjs....R: Scripts to calculate the objectives within the optimization using the RHESSys model output.
3. MatchParetoIDs.R: Places the NFE ID number of the Borg optimization run onto the Pareto fronts. This is used to obtain RHESSys streamflow files for the Pareto front.
4. PostAnalysis.R: Makes plots and computes metrics for the optimization runs.
5. MoveOptFiles....sh: Scripts to move the generated optimization files to permanent storage.
