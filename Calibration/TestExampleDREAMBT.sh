#!/bin/bash
#SBATCH -D /scratch/js4yd/TestBayesianToolsDREAMzs/		# working directory
#SBATCH -o /scratch/js4yd/TestBayesianToolsDREAMzs/TestExampleDREAMBT.out   # Name of the output file (eg. myMPI.oJobID)
#SBATCH -N 1            					# Total number of nodes to request (up to 45)
#SBATCH --cpus-per-task 3           		# Number of processors per node (up to 20)
#SBATCH -p standard           				# Queue name "parallel"
#SBATCH -A quinnlab       					# allocation name
#SBATCH -t 00:10:00       					# Run time (hh:mm:ss) - up to 36 hours
#SBATCH --mail-user=js4yd@virginia.edu     # address for email notification
#SBATCH --mail-type=ALL 					# email at Begin and End of job

module purge
module load gcc/7.1.0 openmpi/3.1.4 R/3.5.3
Rscript ExampleDREAMBayesianTools_Rivanna.R                     