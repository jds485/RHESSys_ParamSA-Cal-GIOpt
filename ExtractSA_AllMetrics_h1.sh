#!/bin/bash
#SBATCH -D /scratch/js4yd/MorrisSA/SAmetrics/
#SBATCH -o /scratch/js4yd/MorrisSA/SAmetrics/output/Run_1h%a.out
#SBATCH --ntasks=1                              # Number of tasks per serial job (must be 1)
#SBATCH -p standard           									# Queue partition name "standard"
#SBATCH -A quinnlab       											# allocation name
#SBATCH -t 00:40:00       											# Run time per serial job (hh:mm:ss) - up to 36 hours
#SBATCH --mail-user=js4yd@virginia.edu          # address for email notification
#SBATCH --mail-type=ALL                  		    # email at Begin and End of job

module load gcc/7.1.0 openmpi/3.1.4 R/3.5.3

#Output and input file directories
OUTDIR='/scratch/js4yd/MorrisSA/SAmetrics/SAdata'
INDIR='/scratch/js4yd/MorrisSA/SAmetrics'

Rscript "$INDIR"/ExtractSAtrajectories_AllMetrics_h1.R "$SLURM_ARRAY_TASK_ID" "$OUTDIR" "/scratch/js4yd/MorrisSA/SAmetrics/EEs_All_Setup_paperh1.RData"
