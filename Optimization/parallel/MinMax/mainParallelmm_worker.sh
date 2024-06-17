#!/bin/bash
#SBATCH -D /scratch/js4yd/Bais910Hill30mOptGI_MOROMinMax/    # Working directory
#SBATCH -o /scratch/js4yd/Bais910Hill30mOptGI_MOROMinMax/RunMoroMinMaxMMOptGIw.out   # Name of the output file
#SBATCH --ntasks=92                                        #number of MPI tasks
#SBATCH --cpus-per-task=10                                 #number of cores per task
#SBATCH -p parallel                                        # Queue name "parallel"
#SBATCH -A quinnlab                                        # allocation name
#SBATCH -t 3-00:00:00                                      # Run time (d-hh:mm:ss)
#SBATCH --mail-user=js4yd@virginia.edu                     # address for email notification
#SBATCH --mail-type=ALL                                    # email at Begin and End of job

module purge
module load gcc/7.1.0 openmpi/3.1.4

cd /scratch/js4yd/borg-moea_workerEditsMORO/Borg-1.8/

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

# arguments are hard coded because that made the .exe run faster. 
#1: random seed - 9
#2: Borg archive print frequency (NFE per print) - 200
#3: Borg number of islands (1 master per island) - 2
#4: Borg maximum runtime in hours (for the entire run = 2.98 days ~= 71 hrs) - 71.4
srun ./mainParallelmm_worker.exe
