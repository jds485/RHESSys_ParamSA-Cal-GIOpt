#!/bin/bash
#SBATCH -D /scratch/js4yd/GI_RandomSeedEval/
#SBATCH -o /scratch/js4yd/GI_RandomSeedEval/Compareh.out
#SBATCH --ntasks=1                              # Number of tasks per serial job (must be 1)
#SBATCH -p standard           									# Queue partition name "standard"
#SBATCH -A quinnlab       											# allocation name
#SBATCH -t 00:10:00       											# Run time per serial job (hh:mm:ss) - up to 36 hours
#SBATCH --mail-user=js4yd@virginia.edu          # address for email notification
#SBATCH --mail-type=ALL                  		    # email at Begin and End of job

module purge
module load gcc/7.1.0 openmpi/3.1.4 R/3.5.3

Rscript /sfs/lustre/bahamut/scratch/js4yd/GI_RandomSeedEval/CompareGIStreamflows_h.R