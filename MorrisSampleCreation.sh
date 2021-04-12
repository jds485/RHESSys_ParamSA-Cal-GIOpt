module purge
module load gcc/7.1.0 openmpi/3.1.4 R/3.5.3 singularity

#Make Morris sampling locations using R
Rscript /scratch/js4yd/MorrisSA/RHESSysRuns/MorrisSamplingLocations_BeforeProcessing.R '/scratch/js4yd/MorrisSA/RHESSysRuns/' 'BaismanMorrisSamplingProblemFile_Full.csv' '8154' '40' '100' '50' '/home/js4yd/R/x86_64-pc-linux-gnu-library/3.5'

#Execute MorrisSampling.py to adjust these locations to meet parameter constraints. This is completed using the version of Python in the singularity container
singularity exec /share/resources/containers/singularity/rhessys/rhessys_v3.img python /scratch/js4yd/MorrisSA/RHESSysRuns/MorrisSampling.py '/scratch/js4yd/MorrisSA/RHESSysRuns' '1349' '40' '10' 'BaismanMorrisSamplingProblemFile_Full.csv'

#Run sampling diagnostics script to get figure of parameter correlations pre and post processing.
Rscript /scratch/js4yd/MorrisSA/RHESSysRuns/MorrisSamplingDiagnostics.R '/scratch/js4yd/MorrisSA/RHESSysRuns/'