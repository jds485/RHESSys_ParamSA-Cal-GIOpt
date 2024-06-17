#!/bin/bash
#SBATCH -D /scratch/js4yd/Bais910Hill30mOptimizeGI_Syn/    # Working directory
#SBATCH -o /scratch/js4yd/Bais910Hill30mOptimizeGI_Syn/RunSepRunObjs.out   # Name of the output file
#SBATCH -N 1                                              # Total number of nodes to request
#SBATCH --ntasks 1                               # Number of processors per node
#SBATCH -p standard                                        # Queue name "parallel"
#SBATCH -A quinnlab                                        # allocation name
#SBATCH -t 00:05:00                                      # Run time (d-hh:mm:ss)
#SBATCH --mail-user=js4yd@virginia.edu                     # address for email notification
#SBATCH --mail-type=ALL                                    # email at Begin and End of job

module purge
module load gcc/7.1.0 openmpi/3.1.4 python/2.7.16

python separateRuntimeObjs.py
python separateRuntimeSets.py