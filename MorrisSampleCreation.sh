module load singularity gcc/7.1.0 openmpi/3.1.4 R/3.5.3

#Make Morris sampling locations using R
Rscript /scratch/js4yd/MorrisSA/RHESSysRuns/MorrisSamplingLocations_BeforeProcessing.R

#Execute MorrisSampling.py to adjust these locations to meet parameter constraints. This is completed using the version of Python in the singularity container
singularity exec /share/resources/containers/singularity/rhessys/rhessys_v3.img python /scratch/js4yd/MorrisSA/RHESSysRuns/MorrisSampling.py