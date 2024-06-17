# Readme for the Optimization Directory

## parallel Directory
Contains sub-directories for the four green infrastructure optimizations: MAP, MORO, MinMax and Synthetic. These were completed using the multi-master implementation of the Borg evolutionary algorithm. Users interested in this algorithm must [request permission](http://borgmoea.org/#contact) to obtain it and use it, and must also [write to Pat Reed](https://www.engineering.cornell.edu/faculty-directory/patrick-michael-reed) to request permission to access the multi-master repository that was used for this project. Within the borg-moea repository, the passNFE branch was used for this project.

This directory also contains scripts used to process the resulting Pareto fronts using [Matthew Woodruff and Jon Herman's pareto.py script](https://github.com/matthewjwoodruff/pareto.py/blob/master/pareto.py) (which we provide in parallel\ParetoAnalysis\CombinedReferenceSet for convenience), and the [MOEA framework Demo file, version 2.13](http://moeaframework.org/) (available in the [archive](http://moeaframework.org/archive.html)). The processingSteps.txt file describes how to use these scripts. Some of these scripts were modified from [Julianne Quinn's Lake_Problem_DPS repository](https://github.com/julianneq/Lake_Problem_DPS/tree/master/Optimization).

## Files within parallel/[MAP, MORO, MinMax, Synthetic]
1. mainParallel...cpp: C++ script that defined the optimization problem for the Borg algorithm.
2. makefile: file used to create an exe file from the cpp script.
3. mainParallel...sh: Slurm script used to run the optimization.
4. RunArray....sh: Slurm script used to run RHESSys and compute results within the optimization and return objective values for candidate optimization solutions. ReEvalSyn in the file name indicates that the script was used to evaluate Pareto solutions on the synthetic true parameter set.
5. CalcObjs....R: Scripts to calculate the objectives within the optimization using the RHESSys model output. Syn in the filename indicates that file was used to evaluate Pareto solutions on synthetic true parameter values.
6. JoinOpt....R: Scripts to join results of the optimization into more useful file formats.
7. RunJoinOpt....sh: Scripts to run the previous files.
8. MoveOpt....sh: Scripts to move files from compute directories into storage directories.
9. concatenate_runref.sh: optimization-specific settings for the file located in the parallel/ParetoAnalysis directory.
10. find_runtime_refSet.sh: optimization-specific settings for the file located in the parallel/ParetoAnalysis directory.

## Files within parallel/ParetoAnalysis 
1. refer to the processingSteps.txt file
2. ./CombinedReferenceSet: directory to combine results from all optimization runs into a single Pareto front showing dominated and non-dominated solutions.

## Files within Optimization/
11. MatchParetoIDs.R: Places the NFE ID number of the Borg optimization run onto the Pareto fronts. This is used to obtain RHESSys streamflow files for the Pareto front.
12. PostAnalysis.R: Makes plots and computes metrics for the optimization runs.
13. FindMinMaxParams.R: Finds the parameter set that contributed the MinMax Pareto solution for each objective.
