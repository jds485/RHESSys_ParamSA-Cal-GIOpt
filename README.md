**Workflow for Morris Sampling, Processing, and Running**

**Steps:**

**1.	Create Main Directory**
    This directory will have all of the files needed to run the Morris analysis, GIS2RHESSys, and RHESSys

**2.	Move Data and Code to Directory**
    •	Date analysis GitHub
    •	GIS2RHESSys GitHub
        o	Make any changes to the vegetation, soil, and LULC csv files in this directory. These must contain the same exact parameters as the def files that will be used. Values of the parameters in these files are not important, as they are all updated by the scripts later.
    •	ssurgo extraction GitHub
    •	raw_data folder with Roads, DEM, LULC, Soils, etc. required for your model in GIS2RHESSys and ssurgo extraction
    •	workflows_fromScratch.sh for running GIS2RHESSys to obtain the initial basemap conditions for RHESSys. The output from this script is used for every RHESSys run.
    •	RunArray.sh for running SA replicates
    •	Rename output files shell script for moving and renaming the output files.

**3.	Run GIS2RHESSys in Singularity container using the workflows_fromScratch.sh**
    Before running this, check that in your home directory’s .grass7 directory that there is no rc file. This rc file tells GRASS where the main GRASS directory is on the computer. If it exists, delete it as long as you know that’s okay for your other work that relies on GRASS. A new rc file will be created when GRASS is run. If you cannot delete it, I do not know enough about GRASS to suggest an alternative method. Seek IT support, or use another computer/username. Similarly, you will not be able to run other GRASS processes than this project’s until this analysis has completed.

    Start an interactive Singularity session, and use sh workflows_fromScratch.sh to obtain the files. This should be run in the main directory

**4.	Add RHESSys input files to the created _g74 directory**
    •	tecfiles directory with the tecfile in it.
        o	Note that the end date must be the day after the last day for which you want data!
    •	clim folder with climate station base, rain, tmin, tmax, etc. files
        o	Note that the dates in these timeseries must match the tecfile start and end date
    •	defs folder with all of the def files needed for RHESSys to run
        o	For all parameters that will remain constant, these def files must have the desired value specified for those parameters. Other parameters that are changing will be updated by the scripts.
**5.	Create Directory to do Morris SA runs**
    In that folder place the following:
    •	output folder – empty
    •	MorrisSampling.py
    •	MorrisSampling_BeforeProcessing.R
    •	Morris problem file
    •	MorrisSampleCreation.sh
    •	MakeDefs_fn.py
    •	ModifyVeg.py

**6.	Run MorrisSampleCreation.sh**
    This will create the Morris sampling locations from the specified parameter bounds using the R script, and post-process them to ensure that they are within the constraints of the parameters using the MorrisSampling.py script. The output is a file of the Morris sampling trajectories for each of the SA replicates.

**7.	Make a permanent storage directory for the results**
    On some systems, the directory in which code is run is not recommended as permanent storage (e.g. it is purged every 90 days). So, results are moved to a permanent storage directory once they complete.

**8.	Move or Downloaded RHESSysEC to a Directory**
    This can be any directory, and you must specify where to find this directory in shell scripts

**9.	Compile RHESSysEC**
    Use “make” to compile the executable for RHESSysEC in the directory.
    There will be warnings when this is compiled. These do not affect the code. Errors are all within print statements.

**10.	Run an array of replicates using the RunArray.sh script**
    The number of replicates that can be run at once depends on your computer.

**11.	After all replicates are complete, run the RenameOutputFiles.sh**
    This script renames all of the output files from 1 index to 0 index to match the Python indices for the replicates. It then moves them to the specified output directory.

**12.	Go to analysis of sensitivity results**