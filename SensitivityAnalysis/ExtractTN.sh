#!/bin/bash
#SBATCH -D /scratch/js4yd/MorrisSA/TNprocessing/
#SBATCH -o /scratch/js4yd/MorrisSA/TNprocessing/output/Run_%a.out
#SBATCH --ntasks=1                              # Number of tasks per serial job (must be 1)
#SBATCH -p standard           									# Queue partition name "standard"
#SBATCH -A quinnlab       											# allocation name
#SBATCH -t 01:00:00       											# Run time per serial job (hh:mm:ss) - up to 36 hours
#SBATCH --mail-user=js4yd@virginia.edu          # address for email notification
#SBATCH --mail-type=ALL                  		    # email at Begin and End of job

module load gcc/7.1.0 openmpi/3.1.4 R/3.5.3

#Output and input file directories
OUTDIR='/scratch/js4yd/MorrisSA/TNprocessing/TNdata'
INDIR='/scratch/js4yd/MorrisSA/TNprocessing'

#arguments are: 
#1. The array job ID
#2. INDIR above
#3. OUTDIR above
#4. text file of the basin streamflows found in INDIR
#5. text file of the hillslope streamflows found in INDIR 
#6-11. Interpolation table names for WRTDS. Intercept, year, log flow, sin year, cos year, log error. 
Rscript "$INDIR"/TNFileExtraction.R "$SLURM_ARRAY_TASK_ID" "$INDIR" "$OUTDIR" 'SAResults_BasinStreamflow_p4_t.txt' 'SAResults_HillStreamflow_p6_t.txt' 'TabIntMod4_p5.txt' 'TabYearMod4_p4.txt' 'TabLogQMod4_p4.txt' 'TabSinYearMod4_p4.txt' 'TabCosYearMod4_p4.txt' 'TabLogErrMod4_p5.txt' 
