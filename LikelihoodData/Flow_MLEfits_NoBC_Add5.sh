#!/bin/bash
#SBATCH -D /scratch/js4yd/LikelihoodAnalysis_CorrBias200/		# working directory
#SBATCH -o /scratch/js4yd/LikelihoodAnalysis_CorrBias200/Flow_MLEfits_NoBC200pre.out   # Name of the output file (eg. myMPI.oJobID)
#SBATCH -N 2            					# Total number of nodes to request (up to 45)
#SBATCH --ntasks-per-node 3           		# Number of processors per node (up to 20)
#SBATCH -p parallel           				# Queue name "parallel"
#SBATCH -A quinnlab       					# allocation name
#SBATCH -t 00:20:00       					# Run time (hh:mm:ss) - up to 36 hours
#SBATCH --mail-user=js4yd@virginia.edu     # address for email notification
#SBATCH --mail-type=ALL 					# email at Begin and End of job

module purge
module load gcc/7.1.0 openmpi/3.1.4 python/3.6.6 mpi4py
mpirun python Flow_MLEfits_NoBC_Add5.py                      