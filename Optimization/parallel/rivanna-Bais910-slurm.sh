#!/bin/bash
#SBATCH -D /scratch/ss9vz/LakeComo/parallel
#SBATCH -o /scratch/ss9vz/LakeComo/parallel/job.%j.%N.out   # Name of the output file (eg. myMPI.oJobID)
#SBATCH -N 20            # Total number of nodes to request (up to 120)
#SBATCH --ntasks-per-node 20           # Number of processors per node (up to 20)
#SBATCH -p parallel           # Queue name "parallel"
#SBATCH -A quinnlab       # allocation name
#SBATCH -t 24:00:00        # Run time (hh:mm:ss) - up to 36 hours
#SBATCH --mail-user=ss9vz@virginia.edu              # address for email notification
#SBATCH --mail-type=ALL                  # email at Begin and End of job

#load openmpi module
module load gcc openmpi

# Your commands go here
# arguments are <seed> <NFE> <islands>
mpirun ./LakeComo 1 100000 2
