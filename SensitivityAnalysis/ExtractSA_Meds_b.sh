#!/bin/bash
#SBATCH -D /scratch/js4yd/MorrisSA/SAmetrics/
#SBATCH -o /scratch/js4yd/MorrisSA/SAmetrics/output/Run_1bm%a.out
#SBATCH --ntasks=1                              # Number of tasks per serial job (must be 1)
#SBATCH -p standard           									# Queue partition name "standard"
#SBATCH -A quinnlab       											# allocation name
#SBATCH -t 00:10:00       											# Run time per serial job (hh:mm:ss) - up to 36 hours
#SBATCH --mail-user=js4yd@virginia.edu          # address for email notification
#SBATCH --mail-type=ALL                  		    # email at Begin and End of job

module load gcc/7.1.0 openmpi/3.1.4 R/3.5.3

#Output and input file directories
OUTDIR='/scratch/js4yd/MorrisSA/SAmetrics/SAdata'
INDIR='/scratch/js4yd/MorrisSA/SAmetrics'

Rscript "$INDIR"/ExtractSAtrajectories_Meds_b.R "$SLURM_ARRAY_TASK_ID" "$OUTDIR" "/scratch/js4yd/MorrisSA/SAmetrics/EEs_All_Setup_paperb.RData" "/scratch/js4yd/MorrisSA/SAmetrics/EEs_Meds_Setup_b.RData"