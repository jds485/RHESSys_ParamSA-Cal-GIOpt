#!/bin/bash
#SBATCH -D /scratch/js4yd/GI_RandomSeedEval910/
#SBATCH -o /scratch/js4yd/GI_RandomSeedEval910/Compareb20.out
#SBATCH --ntasks=1                              # Number of tasks per serial job (must be 1)
#SBATCH -p standard           									# Queue partition name "standard"
#SBATCH -A quinnlab       											# allocation name
#SBATCH -t 00:10:00       											# Run time per serial job (hh:mm:ss) - up to 36 hours
#SBATCH --mail-user=js4yd@virginia.edu          # address for email notification
#SBATCH --mail-type=ALL                  		    # email at Begin and End of job

module purge
module load gcc/7.1.0 openmpi/3.1.4 R/3.5.3

#Arguments:
#1: patch resolution
#2: main working directory
#3: project directory name
#4: Number of replicates
#5: Starting date
#6: Replicate starting number
#7: File end name
Rscript /sfs/lustre/bahamut/scratch/js4yd/GI_RandomSeedEval910/CompareGIStreamflows.R '30' '/scratch/js4yd/GI_RandomSeedEval910/' 'RHESSys_Baisman30m_g74' '100' '2004-10-01' '1' '20'