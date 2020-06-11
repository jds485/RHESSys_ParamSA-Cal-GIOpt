#!/bin/bash
#SBATCH -D /scratch/js4yd/LikelihoodAnalysis/		# working directory
#SBATCH -o /scratch/js4yd/LikelihoodAnalysis/TN_MLEfits.out   # Name of the output file (eg. myMPI.oJobID)
#SBATCH -N 15            					# Total number of nodes to request (up to 45)
#SBATCH --ntasks-per-node 20           		# Number of processors per node (up to 20)
#SBATCH -p parallel           				# Queue name "parallel"
#SBATCH -A quinnlab       					# allocation name
#SBATCH -t 02:00:00       					# Run time (hh:mm:ss) - up to 36 hours
#SBATCH --mail-user=js4yd@virginia.edu     # address for email notification
#SBATCH --mail-type=ALL 					# email at Begin and End of job

module purge
module load gcc/7.1.0 openmpi/3.1.4 python/3.6.6 mpi4py
mpirun python TN_MLEfits.py                        