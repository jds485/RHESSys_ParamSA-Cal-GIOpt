#!/bin/bash
#SBATCH -D /scratch/jdq6nn/Bais910Hill30mOptimizeGI_MAP/    # Working directory
#SBATCH -o /scratch/jdq6nn/Bais910Hill30mOptimizeGI_MAP/RunMapMMOptGIw.out   # Name of the output file
#SBATCH -N 45                                              # Total number of nodes to request
#SBATCH --ntasks-per-node 20                               # Number of processors per node
#SBATCH -p parallel                                        # Queue name "parallel"
#SBATCH -A quinnlab                                        # allocation name
#SBATCH -t 3-00:00:00                                      # Run time (d-hh:mm:ss)
#SBATCH --mail-user=jdq6nn@virginia.edu                     # address for email notification
#SBATCH --mail-type=ALL                                    # email at Begin and End of job

module purge
module load gcc/7.1.0 openmpi/3.1.4

cd /scratch/jdq6nn/borg-moea_workerEditsMAP/Borg-1.8/

# arguments are hard coded because that made the .exe run faster. 
#1: random seed - 7
#2: Borg archive print frequency (NFE per print) - 200
#3: Borg number of islands (1 master per island) - 2
#4: Borg maximum runtime in hours (for the entire run = 2.98 days ~= 71 hrs) - 71.4
mpirun ./mainParallelmm_worker.exe
