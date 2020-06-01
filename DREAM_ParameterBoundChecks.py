# -*- coding: utf-8 -*-
"""
Created on Sun May 31 14:13:07 2020

@author: Jared D. Smith, js4yd@virginia.edu

File for checking sum and inequality constraints for calibration of RHESSys.

Developed in Python 2.7.15. It may work in other versions.
"""
import os
import pandas as pd
import sys
import re
import numpy as np
import random as rd
import string

#sys.argv contains: 
#0: unused - script call info
#1: working directory
#2: random seed - should be different for each chain and each step in chain
#3: number of trajectories
#4: round tolerance <= 10
#5: problem file name

#%% Set working directory
#os.chdir('/scratch/js4yd/MorrisSA/RHESSysRuns')
#os.chdir('C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs_Calibration\\')
os.chdir(sys.argv[1])

#Import function
#Fixme: may need more functions here
from Sum3CheckFun import FullSum3Check

#Set the random seed for randomly generated values
rd.seed(int(sys.argv[2]))

#Set rounding precision as number of decimal places. Using 10 for lack of a sigfig function in Py 2.7.x
#Note: There are still statements of '%.10f' for writing def files, and '%.10f' and '%.7f' for comparing values
#so rounding to more than 10 decimal places will cut to 10 in written files.
#This can result in sums of variables not equal to exactly 1. Best to use <= 10 decimal places for now.
#Also note that round() and pandas .round() give different results when placeholder 0s are needed.
#Oddly, both do not add 0s. Rather, round() adds 0.0000000025, and .round adds 0.0000000099.
#roundTol = 10
roundTol = int(sys.argv[4])

#%% Load problem file containing the bounds of the parameters
#ProbFile = pd.read_csv('BaismanCalibrationParameterProblemFile.csv')
ProbFile = pd.read_csv(sys.argv[5])

#Check that the lower bounds are all less than the upper bounds
if not all(ProbFile.iloc[:,2] < ProbFile.iloc[:,3]):
    sys.exit('PyERROR1: For parameters, all lower bounds are not less than the upper bounds')

#%% Load the def file default values that will be used for variables not in the ProbFile.
soil_loam = pd.read_table(sys.argv[3]+'/soil_loam.def', delim_whitespace=True, header=None, dtype = 'object')
soil_cloam = pd.read_table(sys.argv[3]+'/soil_loam_compact.def', delim_whitespace=True, header=None, dtype = 'object')
soil_sloam = pd.read_table(sys.argv[3]+'/soil_silt_loam.def', delim_whitespace=True, header=None, dtype = 'object')
soil_csloam = pd.read_table(sys.argv[3]+'/soil_silt_loam_compact.def', delim_whitespace=True, header=None, dtype = 'object')
land_grass = pd.read_table(sys.argv[3]+'/landuse_grass.def', delim_whitespace=True, header=None, dtype = 'object')
land_undev = pd.read_table(sys.argv[3]+'/landuse_undeveloped.def', delim_whitespace=True, header=None, dtype = 'object')
land_urban = pd.read_table(sys.argv[3]+'/landuse_urban.def', delim_whitespace=True, header=None, dtype = 'object')
land_septic = pd.read_table(sys.argv[3]+'/landuse_urbanSeptic.def', delim_whitespace=True, header=None, dtype = 'object')
veg_Tree = pd.read_table(sys.argv[3]+'/stratum_deciduousBES.def', delim_whitespace=True, header=None, dtype = 'object')
veg_grass = pd.read_table(sys.argv[3]+'/stratum_grass.def', delim_whitespace=True, header=None, dtype = 'object')
veg_NonVeg = pd.read_table(sys.argv[3]+'/stratum_nonveg.def', delim_whitespace=True, header=None, dtype = 'object')

#Check lower bounds for variables. 
#First check the ProbFile for variable names.
#If both exist, check the ProbFile bounds
#If both don't exist, check the def file values
if (len(np.where(ProbFile.iloc[:,0] == 's108_Ksat_0_v')[0]) != 0) & (len(np.where(ProbFile.iloc[:,0] == 's108_Ksat_0')[0]) != 0):
    if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's108_Ksat_0_v'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's108_Ksat_0'].index[0],2]):
        sys.exit('PyERROR2: Lower bound of soil 108 vertical Ksat is not less than the lower bound of soil 108 Ksat. Either increase lower bound of Ksat or decrease lower bound of vertical Ksat. The change must be greater than rounding tolerance.')
elif (len(np.where(ProbFile.iloc[:,0] == 's108_Ksat_0_v')[0]) == 0) & (len(np.where(ProbFile.iloc[:,0] == 's108_Ksat_0')[0]) == 0):
    if not (soil_csloam.iloc[soil_csloam.iloc[:,1][soil_csloam.iloc[:,1] == 'Ksat_0_v'].index[0],0] < soil_csloam.iloc[soil_csloam.iloc[:,1][soil_csloam.iloc[:,1] == 'Ksat_0'].index[0],0]):
        sys.exit('PyERROR2a: soil 108 vertical Ksat is not less than soil 108 Ksat. Either increase Ksat or decrease vertical Ksat. The change must be greater than rounding tolerance.')

if (len(np.where(ProbFile.iloc[:,0] == 's8_Ksat_0_v')[0]) != 0) & (len(np.where(ProbFile.iloc[:,0] == 's8_Ksat_0')[0]) != 0):
    if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's8_Ksat_0_v'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's8_Ksat_0'].index[0],2]):
        sys.exit('PyERROR3: Lower bound of soil 8 vertical Ksat is not less than the lower bound of soil 8 Ksat. Either increase lower bound of Ksat or decrease lower bound of vertical Ksat. The change must be greater than rounding tolerance.')
elif (len(np.where(ProbFile.iloc[:,0] == 's8_Ksat_0_v')[0]) == 0) & (len(np.where(ProbFile.iloc[:,0] == 's8_Ksat_0')[0]) == 0):
    if not (soil_sloam.iloc[soil_sloam.iloc[:,1][soil_sloam.iloc[:,1] == 'Ksat_0_v'].index[0],0] < soil_sloam.iloc[soil_sloam.iloc[:,1][soil_sloam.iloc[:,1] == 'Ksat_0'].index[0],0]):
        sys.exit('PyERROR3a: soil 8 vertical Ksat is not less than soil 8 Ksat. Either increase Ksat or decrease vertical Ksat. The change must be greater than rounding tolerance.')
        
if (len(np.where(ProbFile.iloc[:,0] == 's109_Ksat_0_v')[0]) != 0) & (len(np.where(ProbFile.iloc[:,0] == 's109_Ksat_0_v')[0]) != 0):
    if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's109_Ksat_0_v'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's109_Ksat_0'].index[0],2]):
        sys.exit('PyERROR4: Lower bound of soil 109 vertical Ksat is not less than the lower bound of soil 109 Ksat. Either increase lower bound of Ksat or decrease lower bound of vertical Ksat. The change must be greater than rounding tolerance.')
elif (len(np.where(ProbFile.iloc[:,0] == 's109_Ksat_0_v')[0]) == 0) & (len(np.where(ProbFile.iloc[:,0] == 's109_Ksat_0')[0]) == 0):
    if not (soil_cloam.iloc[soil_cloam.iloc[:,1][soil_cloam.iloc[:,1] == 'Ksat_0_v'].index[0],0] < soil_cloam.iloc[soil_cloam.iloc[:,1][soil_cloam.iloc[:,1] == 'Ksat_0'].index[0],0]):
        sys.exit('PyERROR4a: soil 109 vertical Ksat is not less than soil 109 Ksat. Either increase Ksat or decrease vertical Ksat. The change must be greater than rounding tolerance.')

if (len(np.where(ProbFile.iloc[:,0] == 's9_Ksat_0_v')[0]) != 0) & (len(np.where(ProbFile.iloc[:,0] == 's9_Ksat_0')[0]) != 0):
    if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's9_Ksat_0_v'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's9_Ksat_0'].index[0],2]):
        sys.exit('PyERROR5: Lower bound of soil 9 vertical Ksat is not less than the lower bound of soil 9 Ksat. Either increase lower bound of Ksat or decrease lower bound of vertical Ksat. The change must be greater than rounding tolerance.')
elif (len(np.where(ProbFile.iloc[:,0] == 's9_Ksat_0_v')[0]) == 0) & (len(np.where(ProbFile.iloc[:,0] == 's9_Ksat_0')[0]) == 0):
    if not (soil_loam.iloc[soil_loam.iloc[:,1][soil_loam.iloc[:,1] == 'Ksat_0_v'].index[0],0] < soil_loam.iloc[soil_loam.iloc[:,1][soil_loam.iloc[:,1] == 'Ksat_0'].index[0],0]):
        sys.exit('PyERROR5a: soil 9 vertical Ksat is not less than soil 9 Ksat. Either increase Ksat or decrease vertical Ksat. The change must be greater than rounding tolerance.')

if (len(np.where(ProbFile.iloc[:,0] == 's109_m')[0]) != 0) & (len(np.where(ProbFile.iloc[:,0] == 's9_m')[0]) != 0):
    if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's109_m'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's9_m'].index[0],2]):
        sys.exit('PyERROR6: Lower bound of soil 109 m is not less than the lower bound of soil 9 m. Either increase lower bound of s9 m or decrease lower bound of s109 m. The change must be greater than rounding tolerance.')
elif (len(np.where(ProbFile.iloc[:,0] == 's109_m')[0]) == 0) & (len(np.where(ProbFile.iloc[:,0] == 's9_m')[0]) == 0):
    if not (soil_cloam.iloc[soil_cloam.iloc[:,1][soil_cloam.iloc[:,1] == 'm'].index[0],0] < soil_loam.iloc[soil_loam.iloc[:,1][soil_loam.iloc[:,1] == 'm'].index[0],0]):
        sys.exit('PyERROR6a: soil 109 m is not less than soil 9 m in def file. Either increase soil 9 m or decrease soil 109 m. The change must be greater than rounding tolerance.')

if (len(np.where(ProbFile.iloc[:,0] == 's108_m')[0]) != 0) & (len(np.where(ProbFile.iloc[:,0] == 's8_m')[0]) != 0):
    if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's108_m'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's8_m'].index[0],2]):
        sys.exit('PyERROR7: Lower bound of soil 108 m is not less than the lower bound of soil 8 m. Either increase lower bound of s8 m or decrease lower bound of s108 m. The change must be greater than rounding tolerance.')
elif (len(np.where(ProbFile.iloc[:,0] == 's108_m')[0]) == 0) & (len(np.where(ProbFile.iloc[:,0] == 's8_m')[0]) == 0):
    if not (soil_csloam.iloc[soil_csloam.iloc[:,1][soil_csloam.iloc[:,1] == 'm'].index[0],0] < soil_sloam.iloc[soil_sloam.iloc[:,1][soil_sloam.iloc[:,1] == 'm'].index[0],0]):
        sys.exit('PyERROR7a: soil 108 m is not less than soil 8 m in def file. Either increase soil 8 m or decrease soil 108 m. The change must be greater than rounding tolerance.')

if (len(np.where(ProbFile.iloc[:,0] == 's109_porosity_0')[0]) != 0) & (len(np.where(ProbFile.iloc[:,0] == 's9_porosity_0')[0]) != 0):
    if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's109_porosity_0'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's9_porosity_0'].index[0],2]):
        sys.exit('PyERROR8: Lower bound of soil 109 porosity_0 is not less than the lower bound of soil 9 porosity_0. Either increase lower bound of s9 porosity_0 or decrease lower bound of s109 porosity_0. The change must be greater than rounding tolerance.')
elif (len(np.where(ProbFile.iloc[:,0] == 's109_porosity_0')[0]) == 0) & (len(np.where(ProbFile.iloc[:,0] == 's9_porosity_0')[0]) == 0):
    if not (soil_cloam.iloc[soil_cloam.iloc[:,1][soil_cloam.iloc[:,1] == 'porosity_0'].index[0],0] < soil_loam.iloc[soil_loam.iloc[:,1][soil_loam.iloc[:,1] == 'porosity_0'].index[0],0]):
        sys.exit('PyERROR8a: soil 109 porosity_0 is not less than soil 9 porosity_0 in def file. Either increase soil 9 porosity_0 or decrease soil 109 porosity_0. The change must be greater than rounding tolerance.')

if (len(np.where(ProbFile.iloc[:,0] == 's108_porosity_0')[0]) != 0) & (len(np.where(ProbFile.iloc[:,0] == 's8_porosity_0')[0]) != 0):
    if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's108_porosity_0'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's8_porosity_0'].index[0],2]):
        sys.exit('PyERROR9: Lower bound of soil 108 porosity_0 is not less than the lower bound of soil 8 porosity_0. Either increase lower bound of s8 porosity_0 or decrease lower bound of s108 porosity_0. The change must be greater than rounding tolerance.')
elif (len(np.where(ProbFile.iloc[:,0] == 's108_porosity_0')[0]) == 0) & (len(np.where(ProbFile.iloc[:,0] == 's8_porosity_0')[0]) == 0):
    if not (soil_csloam.iloc[soil_csloam.iloc[:,1][soil_csloam.iloc[:,1] == 'porosity_0'].index[0],0] < soil_sloam.iloc[soil_sloam.iloc[:,1][soil_sloam.iloc[:,1] == 'porosity_0'].index[0],0]):
        sys.exit('PyERROR9a: soil 108 porosity_0 is not less than soil 8 porosity_0 in def file. Either increase soil 8 porosity_0 or decrease soil 108 porosity_0. The change must be greater than rounding tolerance.')

if (len(np.where(ProbFile.iloc[:,0] == 's109_Ksat_0')[0]) != 0) & (len(np.where(ProbFile.iloc[:,0] == 's9_Ksat_0')[0]) != 0):
    if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's109_Ksat_0'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's9_Ksat_0'].index[0],2]):
        sys.exit('PyERROR10: Lower bound of soil 109 Ksat_0 is not less than the lower bound of soil 9 Ksat_0. Either increase lower bound of s9 Ksat_0 or decrease lower bound of s109 Ksat_0. The change must be greater than rounding tolerance.')
elif (len(np.where(ProbFile.iloc[:,0] == 's109_Ksat_0')[0]) == 0) & (len(np.where(ProbFile.iloc[:,0] == 's9_Ksat_0')[0]) == 0):
    if not (soil_cloam.iloc[soil_cloam.iloc[:,1][soil_cloam.iloc[:,1] == 'Ksat_0'].index[0],0] < soil_loam.iloc[soil_loam.iloc[:,1][soil_loam.iloc[:,1] == 'Ksat_0'].index[0],0]):
        sys.exit('PyERROR10a: soil 109 Ksat_0 is not less than soil 9 Ksat_0 in def file. Either increase soil 9 Ksat_0 or decrease soil 109 Ksat_0. The change must be greater than rounding tolerance.')

if (len(np.where(ProbFile.iloc[:,0] == 's108_Ksat_0')[0]) != 0) & (len(np.where(ProbFile.iloc[:,0] == 's8_Ksat_0')[0]) != 0):
    if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's108_Ksat_0'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's8_Ksat_0'].index[0],2]):
        sys.exit('PyERROR11: Lower bound of soil 108 Ksat_0 is not less than the lower bound of soil 8 Ksat_0. Either increase lower bound of s8 Ksat_0 or decrease lower bound of s108 Ksat_0. The change must be greater than rounding tolerance.')
elif (len(np.where(ProbFile.iloc[:,0] == 's108_Ksat_0')[0]) == 0) & (len(np.where(ProbFile.iloc[:,0] == 's8_Ksat_0')[0]) == 0):
    if not (soil_csloam.iloc[soil_csloam.iloc[:,1][soil_csloam.iloc[:,1] == 'Ksat_0'].index[0],0] < soil_sloam.iloc[soil_sloam.iloc[:,1][soil_sloam.iloc[:,1] == 'Ksat_0'].index[0],0]):
        sys.exit('PyERROR11a: soil 108 Ksat_0 is not less than soil 8 Ksat_0 in def file. Either increase soil 8 Ksat_0 or decrease soil 108 Ksat_0. The change must be greater than rounding tolerance.')

if (len(np.where(ProbFile.iloc[:,0] == 's109_Ksat_0_v')[0]) != 0) & (len(np.where(ProbFile.iloc[:,0] == 's9_Ksat_0_v')[0]) != 0):
    if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's109_Ksat_0_v'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's9_Ksat_0_v'].index[0],2]):
        sys.exit('PyERROR12: Lower bound of soil 109 Ksat_0_v is not less than the lower bound of soil 9 Ksat_0_v. Either increase lower bound of s9 Ksat_0_v or decrease lower bound of s109 Ksat_0_v. The change must be greater than rounding tolerance.')
elif (len(np.where(ProbFile.iloc[:,0] == 's109_Ksat_0_v')[0]) == 0) & (len(np.where(ProbFile.iloc[:,0] == 's9_Ksat_0_v')[0]) == 0):
    if not (soil_cloam.iloc[soil_cloam.iloc[:,1][soil_cloam.iloc[:,1] == 'Ksat_0_v'].index[0],0] < soil_loam.iloc[soil_loam.iloc[:,1][soil_loam.iloc[:,1] == 'Ksat_0_v'].index[0],0]):
        sys.exit('PyERROR12a: soil 109 Ksat_0_v is not less than soil 9 Ksat_0_v in def file. Either increase soil 9 Ksat_0_v or decrease soil 109 Ksat_0_v. The change must be greater than rounding tolerance.')

if (len(np.where(ProbFile.iloc[:,0] == 's108_Ksat_0_v')[0]) != 0) & (len(np.where(ProbFile.iloc[:,0] == 's8_Ksat_0_v')[0]) != 0):
    if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's108_Ksat_0_v'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's8_Ksat_0_v'].index[0],2]):
        sys.exit('PyERROR13: Lower bound of soil 108 Ksat_0_v is not less than the lower bound of soil 8 Ksat_0_v. Either increase lower bound of s8 Ksat_0_v or decrease lower bound of s108 Ksat_0_v. The change must be greater than rounding tolerance.')
elif (len(np.where(ProbFile.iloc[:,0] == 's108_Ksat_0_v')[0]) == 0) & (len(np.where(ProbFile.iloc[:,0] == 's8_Ksat_0_v')[0]) == 0):
    if not (soil_csloam.iloc[soil_csloam.iloc[:,1][soil_csloam.iloc[:,1] == 'Ksat_0_v'].index[0],0] < soil_sloam.iloc[soil_sloam.iloc[:,1][soil_sloam.iloc[:,1] == 'Ksat_0_v'].index[0],0]):
        sys.exit('PyERROR13a: soil 108 Ksat_0_v is not less than soil 8 Ksat_0_v in def file. Either increase soil 8 Ksat_0_v or decrease soil 108 Ksat_0_v. The change must be greater than rounding tolerance.')

if (len(np.where(ProbFile.iloc[:,0] == 'v102_epc.topt')[0]) != 0) & (len(np.where(ProbFile.iloc[:,0] == 'v102_epc.tmax')[0]) != 0):
    if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 'v102_epc.topt'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 'v102_epc.tmax'].index[0],2]):
        sys.exit('PyERROR14: Lower bound of veg 102 epc.topt is not less than the lower bound of veg 102 epc.tmax. Either increase lower bound of epc.tmax or decrease lower bound of epc.topt. The change must be greater than rounding tolerance.')
elif (len(np.where(ProbFile.iloc[:,0] == 'v102_epc.topt')[0]) == 0) & (len(np.where(ProbFile.iloc[:,0] == 'v102_epc.tmax')[0]) == 0):
    if not (veg_Tree.iloc[veg_Tree.iloc[:,1][veg_Tree.iloc[:,1] == 'epc.topt'].index[0],0] < veg_Tree.iloc[veg_Tree.iloc[:,1][veg_Tree.iloc[:,1] == 'epc.tmax'].index[0],0]):
        sys.exit('PyERROR14a: veg 102 epc.topt is not less than veg 102 epc.tmax in def file. Either increase epc.tmax or decrease epc.topt. The change must be greater than rounding tolerance.')

if (len(np.where(ProbFile.iloc[:,0] == 'v3_epc.topt')[0]) != 0) & (len(np.where(ProbFile.iloc[:,0] == 'v3_epc.tmax')[0]) != 0):
    if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 'v3_epc.topt'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 'v3_epc.tmax'].index[0],2]):
        sys.exit('PyERROR15: Lower bound of veg 3 epc.topt is not less than the lower bound of veg 3 epc.tmax. Either increase lower bound of epc.tmax or decrease lower bound of epc.topt. The change must be greater than rounding tolerance.')
elif (len(np.where(ProbFile.iloc[:,0] == 'v3_epc.topt')[0]) == 0) & (len(np.where(ProbFile.iloc[:,0] == 'v3_epc.tmax')[0]) == 0):
    if not (veg_grass.iloc[veg_grass.iloc[:,1][veg_grass.iloc[:,1] == 'epc.topt'].index[0],0] < veg_grass.iloc[veg_grass.iloc[:,1][veg_grass.iloc[:,1] == 'epc.tmax'].index[0],0]):
        sys.exit('PyERROR15a: veg 3 epc.topt is not less than veg 3 epc.tmax in def file. Either increase epc.tmax or decrease epc.topt. The change must be greater than rounding tolerance.')

if (len(np.where(ProbFile.iloc[:,0] == 'v102_epc.leaf_cn')[0]) != 0) & (len(np.where(ProbFile.iloc[:,0] == 'v102_epc.leaflitr_cn')[0]) != 0):
    if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 'v102_epc.leaf_cn'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 'v102_epc.leaflitr_cn'].index[0],2]):
        sys.exit('PyERROR16: Lower bound of veg 102 epc.leaf_cn is not less than the lower bound of veg 102 epc.leaflitr_cn. Either increase lower bound of epc.leaflitr_cn or decrease lower bound of epc.leaf_cn. The change must be greater than rounding tolerance.')
elif (len(np.where(ProbFile.iloc[:,0] == 'v102_epc.leaf_cn')[0]) == 0) & (len(np.where(ProbFile.iloc[:,0] == 'v102_epc.leaflitr_cn')[0]) == 0):
    if not (veg_Tree.iloc[veg_Tree.iloc[:,1][veg_Tree.iloc[:,1] == 'epc.leaf_cn'].index[0],0] < veg_Tree.iloc[veg_Tree.iloc[:,1][veg_Tree.iloc[:,1] == 'epc.leaflitr_cn'].index[0],0]):
        sys.exit('PyERROR16a: veg 102 epc.leaf_cn is not less than veg 102 epc.leaflitr_cn in def file. Either increase epc.leaflitr_cn or decrease epc.leaf_cn. The change must be greater than rounding tolerance.')

if (len(np.where(ProbFile.iloc[:,0] == 'v3_epc.leaf_cn')[0]) != 0) & (len(np.where(ProbFile.iloc[:,0] == 'v3_epc.leaflitr_cn')[0]) != 0):
    if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 'v3_epc.leaf_cn'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 'v3_epc.leaflitr_cn'].index[0],2]):
        sys.exit('PyERROR17: Lower bound of veg 3 epc.leaf_cn is not less than the lower bound of veg 3 epc.leaflitr_cn. Either increase lower bound of epc.leaflitr_cn or decrease lower bound of epc.leaf_cn. The change must be greater than rounding tolerance.')
elif (len(np.where(ProbFile.iloc[:,0] == 'v3_epc.leaf_cn')[0]) == 0) & (len(np.where(ProbFile.iloc[:,0] == 'v3_epc.leaflitr_cn')[0]) == 0):
    if not (veg_grass.iloc[veg_grass.iloc[:,1][veg_grass.iloc[:,1] == 'epc.leaf_cn'].index[0],0] < veg_grass.iloc[veg_grass.iloc[:,1][veg_grass.iloc[:,1] == 'epc.leaflitr_cn'].index[0],0]):
        sys.exit('PyERROR17a: veg 3 epc.leaf_cn is not less than veg 3 epc.leaflitr_cn in def file. Either increase epc.leaflitr_cn or decrease epc.leaf_cn. The change must be greater than rounding tolerance.')
    
#%%Load in R generated file of proposed chain starting locations. These must be checked for constraints.
Chains_df = pd.read_table('BaismanChainStarts.txt', delim_whitespace=True, header='infer', dtype = 'object')

#%%Check the parameter constraints for soils and vegetation
#%% Soil: sand + silt + clay = 1
#Fixme: commenting out because not used in this calibration. Should fix when all or some are not considered in calibration, and also get rid of trajectory based checks.
#Chains_df = FullSum3Check(Col1='_silt', Col2='_sand', Col3='_clay', MorrisSample_df=Chains_df, N=N, ProbFile=ProbFile, regexNm='silt$', roundTol=roundTol)

#Check sums
#Fixme: can these autodetect parameter names to compare?
#if any((Chains_df.loc[:, 's8_silt'] + Chains_df.loc[:, 's8_sand'] + Chains_df.loc[:, 's8_clay']).round(roundTol) != 1.0000):
#    sys.exit('PyERROR18: Sum of sand + silt + clay != 1 for s8')
#if any((Chains_df.loc[:, 's108_silt'] + Chains_df.loc[:, 's108_sand'] + Chains_df.loc[:, 's108_clay']).round(roundTol) != 1.0000):
#    sys.exit('PyERROR19: Sum of sand + silt + clay != 1 for s108')
#if any((Chains_df.loc[:, 's9_silt'] + Chains_df.loc[:, 's9_sand'] + Chains_df.loc[:, 's9_clay']).round(roundTol) != 1.0000):
#    sys.exit('PyERROR20: Sum of sand + silt + clay != 1 for s9')
#if any((Chains_df.loc[:, 's109_silt'] + Chains_df.loc[:, 's109_sand'] + Chains_df.loc[:, 's109_clay']).round(roundTol) != 1.0000):
#    sys.exit('PyERROR21: Sum of sand + silt + clay != 1 for s109')

#%%
#4 parameters for compacted soil must be <= values for uncompacted soil
#m, porosity_0, Ksat_0, Ksat_0_v

#Additionally, values for Ksat_0_v must be less than Ksat_0 for each soil type

#These items are accomplished in the following sections

#%% Order is important for Ksat and vKsat. 
#Must first adjust Ksat of compacted to be less than Ksat of uncompacted.
#If it is not greater, simulate a random value less than Ksatu for Ksatc
for i in range(len(Chains_df.filter(regex='_Ksat_0$').columns)):
    #Find the indicator for the first soil type
    #string = Get all columns containing _Ksat_0
    ind1 = re.split(string=Chains_df.filter(regex='_Ksat_0$').columns[i], pattern='_')[0]
    
    #Only work on the columns that have a 0, and find the other corresponding index
    if len(re.split(string=ind1, pattern='0')) == 2:
        #Fixme: in Python 3, str.join and remove sep.
        ind2 = string.join(re.split(string=ind1, pattern='10'), sep='')
        #Use the indicator to extract the Ksat compacted, and Ksat uncompacted columns
        mc = Chains_df.loc[:, ind1 + '_Ksat_0']
        mu = Chains_df.loc[:, ind2 + '_Ksat_0']
        
        #Save the original column
        Chains_df.loc[:, 'orig_' + ind1 + '_Ksat_0_orig'] = mc
        
        #Go through the trajectories to check the condition.
        compare = mc > mu
    
        #Get the indices for which this is true
        inds = compare[compare == True].index
    
        #Get the indices of the chain starting points. 
        #Changes in these variables will only happen between those points.
        trajChange = range(len(Chains_df))
    
        #Loop over the number of chains to replace values in the series
        for j in range(len(trajChange)):
            #Get the chain index if the constraint is violated
            indsTrajPos = inds[(inds == trajChange[j])]
    
            if len(indsTrajPos) > 0:
                #There are values to be adjusted
                #Change all mc to a random value
                Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0'].index[0],2], mu[indsTrajPos].iloc[0])]*len(mc[indsTrajPos])
                mc[indsTrajPos] = Val
                del Val
            del indsTrajPos
    
        #Save the new column
        Chains_df.loc[:, ind1 + '_Ksat_0'] = mc

del compare, i, ind1, ind2, j, mu, mc, trajChange, inds

#Check sums
if sum(Chains_df.loc[:,'s109_Ksat_0'] > Chains_df.loc[:,'s9_Ksat_0']) != 0:
    sys.exit('PyERROR22: s109 Ksat not < s9 Ksat')
if sum(Chains_df.loc[:,'s108_Ksat_0'] > Chains_df.loc[:,'s8_Ksat_0']) != 0:
    sys.exit('PyERROR23: s108 Ksat not < s8 Ksat')
    
#%% Next make sure that vertical Ksat < Ksat for each soil
#Ksat_0_v < Ksat_0
#If Ksat_0 is not greater, simulate a random value less than Ksat_0 for Ksat_0_v
for i in range(len(Chains_df.filter(regex='Ksat_0_v$').columns)):
    #Find the indicator for the soil type
    #string = Get all columns containing Ksat_0_v
    ind = re.split(string=Chains_df.filter(regex='Ksat_0_v$').columns[i], pattern='_')[0]
    #Use the indicator to extract the Ksat_0, and Ksat_0_v columns
    Ksat = Chains_df.loc[:, ind + '_Ksat_0']
    vKsat = Chains_df.loc[:, ind + '_Ksat_0_v']
    
    #Save the original column.
    Chains_df.loc[:, 'orig_' + ind + '_Ksat_0_v_orig'] = vKsat
    
    #Go through the trajectories to check the condition.
    compare = vKsat > Ksat
    
    #Get the indices for which this is true
    inds = compare[compare == True].index
    
    #Get the indices of the Morris trajectory starting points. 
    #Changes in these variables will only happen between those points
    trajChange = range((len(ProbFile.iloc[:,0])+1), (len(ProbFile.iloc[:,0])+1)*(N+1), (len(ProbFile.iloc[:,0])+1))
    
    #Loop over the number of trajectories to replace values in the series
    for j in range(len(trajChange)):
        #Get indices for which the constraint is violated that are in this trajectory
        if j == 0:
            indsTrajPos = inds[(inds < trajChange[j])]
        else:
            indsTrajPos = inds[(inds >= trajChange[j-1]) & (inds < trajChange[j])]

        if len(indsTrajPos) > 0:
            #There are values to be adjusted.
            #Must search for more than 1 change point per series because of the changes to Ksat compacted above.
            
            #Check if all of the indices have exactly the same value.
            if (len(Ksat[indsTrajPos].unique()) == 1) & (len(vKsat[indsTrajPos].unique()) == 1):
                #No change in this trajectory for the selected indices
                #Change all vKsat to a random value
                Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_Ksat_0_v'].index[0],2], Ksat[indsTrajPos].iloc[0])]*len(vKsat[indsTrajPos])
                vKsat[indsTrajPos] = Val
            else:
                #determine all of the change indices for each
                for k in range(len(vKsat[indsTrajPos].unique())):
                    #Find the first instance of the unique value and record it
                    if k == 0:
                        #Start new inex array
                        indvKsatChange = vKsat[indsTrajPos][vKsat[indsTrajPos] == vKsat[indsTrajPos].unique()[k]].index[0]
                    else:
                        #append to list
                        indvKsatChange = np.append(indvKsatChange, vKsat[indsTrajPos][vKsat[indsTrajPos] == vKsat[indsTrajPos].unique()[k]].index[0])
                del k
                for k in range(len(Ksat[indsTrajPos].unique())):
                    #Find the first instance of the unique value and record it
                    if k == 0:
                        #Start new inex array
                        indKsatChange = Ksat[indsTrajPos][Ksat[indsTrajPos] == Ksat[indsTrajPos].unique()[k]].index[0]
                    else:
                        #append to list
                        indKsatChange = np.append(indKsatChange, Ksat[indsTrajPos][Ksat[indsTrajPos] == Ksat[indsTrajPos].unique()[k]].index[0])
                del k
                
                #Join into one list, which is sorted by the unique command by default.
                IndsAll = np.unique(np.append(indKsatChange, indvKsatChange))
                
                #Start looping through the IndsAll indices, and replace as needed.
                for k in range(len(IndsAll)):
                    if k == (len(IndsAll)-1):
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_Ksat_0_v'].index[0],2], Ksat[indsTrajPos[(indsTrajPos >= IndsAll[k])]].iloc[0])]*len(vKsat[indsTrajPos[(indsTrajPos >= IndsAll[k])]])
                        vKsat[indsTrajPos[(indsTrajPos >= IndsAll[k])]] = Val
                    else: 
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_Ksat_0_v'].index[0],2], Ksat[indsTrajPos[(indsTrajPos >= IndsAll[k]) & (indsTrajPos < IndsAll[k+1])]].iloc[0])]*len(vKsat[indsTrajPos[(indsTrajPos >= IndsAll[k]) & (indsTrajPos < IndsAll[k+1])]])
                        vKsat[indsTrajPos[(indsTrajPos >= IndsAll[k]) & (indsTrajPos < IndsAll[k+1])]] = Val
                
                del indvKsatChange, indKsatChange, k, IndsAll
            del Val
        del indsTrajPos
    
    #Save the new column
    Chains_df.loc[:, ind + '_Ksat_0_v'] = vKsat

del compare, i, ind, j, vKsat, Ksat, trajChange, inds

#Check sums
if sum(Chains_df.loc[:,'s109_Ksat_0_v'] > Chains_df.loc[:,'s109_Ksat_0']) != 0:
    sys.exit('PyERROR24: s109 vKsat not < s109 Ksat')
if sum(Chains_df.loc[:,'s9_Ksat_0_v'] > Chains_df.loc[:,'s9_Ksat_0']) != 0:
    sys.exit('PyERROR25: s9 vKsat not < s9 Ksat')
if sum(Chains_df.loc[:,'s108_Ksat_0_v'] > Chains_df.loc[:,'s108_Ksat_0']) != 0:
    sys.exit('PyERROR26: s108 vKsat not < s108 Ksat')
if sum(Chains_df.loc[:,'s8_Ksat_0_v'] > Chains_df.loc[:,'s8_Ksat_0']) != 0:
    sys.exit('PyERROR27: s8 vKsat not < s8 Ksat')

#%%
#Finally, make sure that vKsat compacted < vKsat uncompacted
#If it is not greater, simulate a random value less than Ksatu for Ksatc
for i in range(len(Chains_df.filter(regex='_Ksat_0_v$').columns)):
    #Find the indicator for the first soil type
    #string = Get all columns containing Ksat_0_v
    ind1 = re.split(string=Chains_df.filter(regex='_Ksat_0_v$').columns[i], pattern='_')[0]
    
    #Only work on the columns that have a 0, and find the other corresponding index
    if len(re.split(string=ind1, pattern='0')) == 2:
        ind2 = string.join(re.split(string=ind1, pattern='10'), sep='')
        #Use the indicator to extract the m compacted, and m uncompacted columns
        mc = Chains_df.loc[:, ind1 + '_Ksat_0_v']
        mu = Chains_df.loc[:, ind2 + '_Ksat_0_v']
        
        #No need to save original column here. It's already been saved earlier.
        
        #Go through the trajectories to check the condition.
        compare = mc > mu
    
        #Get the indices for which this is true
        inds = compare[compare == True].index
    
        #Get the indices of the Morris trajectory starting points. 
        #Changes in these variables will only happen between those points.
        trajChange = range((len(ProbFile.iloc[:,0])+1), (len(ProbFile.iloc[:,0])+1)*(N+1), (len(ProbFile.iloc[:,0])+1))
    
        #Loop over the number of trajectories to replace values in the series
        for j in range(len(trajChange)):
            #Get indices for which the constraint is violated that are in this trajectory
            if j == 0:
                indsTrajPos = inds[(inds < trajChange[j])]
            else:
                indsTrajPos = inds[(inds >= trajChange[j-1]) & (inds < trajChange[j])]
    
            if len(indsTrajPos) > 0:
                #There are values to be adjusted
                #Check if all of the indices have exactly the same value.
                if (len(mu[indsTrajPos].unique()) == 1) & (len(mc[indsTrajPos].unique()) == 1):
                    #No change in this trajectory for the selected indices
                    #Change all mc to a random value
                    Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0_v'].index[0],2], mu[indsTrajPos].iloc[0])]*len(mc[indsTrajPos])
                    mc[indsTrajPos] = Val
                else:
                    #determine all of the indices for each
                    for k in range(len(mc[indsTrajPos].unique())):
                        #Find the first instance of the unique value and record it
                        if k == 0:
                            #Start new inex array
                            indvKsatChange = mc[indsTrajPos][mc[indsTrajPos] == mc[indsTrajPos].unique()[k]].index[0]
                        else:
                            #append to list
                            indvKsatChange = np.append(indvKsatChange, mc[indsTrajPos][mc[indsTrajPos] == mc[indsTrajPos].unique()[k]].index[0])
                    del k
                    for k in range(len(mu[indsTrajPos].unique())):
                        #Find the first instance of the unique value and record it
                        if k == 0:
                            #Start new inex array
                            indKsatChange = mu[indsTrajPos][mu[indsTrajPos] == mu[indsTrajPos].unique()[k]].index[0]
                        else:
                            #append to list
                            indKsatChange = np.append(indKsatChange, mu[indsTrajPos][mu[indsTrajPos] == mu[indsTrajPos].unique()[k]].index[0])
                    del k
                    
                    #Join into one list, which is sorted by the unique command by default.
                    IndsAll = np.unique(np.append(indKsatChange, indvKsatChange))
                    
                    #Start looping through the IndsAll indices, and replace as needed.
                    for k in range(len(IndsAll)):
                        if k == (len(IndsAll)-1):
                            Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0_v'].index[0],2], mu[indsTrajPos[(indsTrajPos >= IndsAll[k])]].iloc[0])]*len(mc[indsTrajPos[(indsTrajPos >= IndsAll[k])]])
                            mc[indsTrajPos[(indsTrajPos >= IndsAll[k])]] = Val
                        else: 
                            Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0_v'].index[0],2], mu[indsTrajPos[(indsTrajPos >= IndsAll[k]) & (indsTrajPos < IndsAll[k+1])]].iloc[0])]*len(mc[indsTrajPos[(indsTrajPos >= IndsAll[k]) & (indsTrajPos < IndsAll[k+1])]])
                            mc[indsTrajPos[(indsTrajPos >= IndsAll[k]) & (indsTrajPos < IndsAll[k+1])]] = Val
                    
                    del indvKsatChange, indKsatChange, k, IndsAll
                del Val
            del indsTrajPos

del compare, i, ind1, ind2, j, mu, mc, trajChange, inds

#Check sums
if sum(Chains_df.loc[:,'s109_Ksat_0_v'] > Chains_df.loc[:,'s9_Ksat_0_v']) != 0:
    sys.exit('PyERROR28: s109 vKsat not < s9 Ksat')
if sum(Chains_df.loc[:,'s108_Ksat_0_v'] > Chains_df.loc[:,'s8_Ksat_0_v']) != 0:
    sys.exit('PyERROR29: s108 vKsat not < s8 Ksat')

#%%
#m uncompacted >= m compacted for each soil type:
#If it is not greater, simulate a random value less than mu for mc
for i in range(len(Chains_df.filter(regex='_m$').columns)):
    #Find the indicator for the first soil type
    #string = Get all columns containing m
    ind1 = re.split(string=Chains_df.filter(regex='_m$').columns[i], pattern='_')[0]
    
    #Only work on the columns that have a 0, and find the other corresponding index
    if len(re.split(string=ind1, pattern='0')) == 2:
        ind2 = string.join(re.split(string=ind1, pattern='10'), sep='')
        #Use the indicator to extract the m compacted, and m uncompacted columns
        mc = Chains_df.loc[:, ind1 + '_m']
        mu = Chains_df.loc[:, ind2 + '_m']
        
        #Save the original column
        Chains_df.loc[:, 'orig_' + ind1 + '_m_orig'] = mc
        
        #Go through the trajectories to check the condition.
        compare = mc > mu
    
        #Get the indices for which this is true
        inds = compare[compare == True].index
    
        #Get the indices of the Morris trajectory starting points. 
        #Changes in these variables will only happen between those points.
        trajChange = range((len(ProbFile.iloc[:,0])+1), (len(ProbFile.iloc[:,0])+1)*(N+1), (len(ProbFile.iloc[:,0])+1))
    
        #Loop over the number of trajectories to replace values in the series
        for j in range(len(trajChange)):
            #Get indices for which the constraint is violated that are in this trajectory
            if j == 0:
                indsTrajPos = inds[(inds < trajChange[j])]
            else:
                indsTrajPos = inds[(inds >= trajChange[j-1]) & (inds < trajChange[j])]
    
            if len(indsTrajPos) > 0:
                #There are values to be adjusted
                #Check if all of the indices have exactly the same value.
                if (len(mu[indsTrajPos].unique()) == 1) & (len(mc[indsTrajPos].unique()) == 1):
                    #No change in this trajectory for the selected indices
                    #Change all mc to a random value
                    Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_m'].index[0],2], mu[indsTrajPos].iloc[0])]*len(mc[indsTrajPos])
                    mc[indsTrajPos] = Val
                elif (len(mu[indsTrajPos].unique()) == 1) | (len(mc[indsTrajPos].unique()) == 1):
                    #There is one change in this trajectory for one, but not the other variable
                    if (len(mu[indsTrajPos].unique()) == 1):
                        #mc changes
                        indmcChange = mc[indsTrajPos][mc[indsTrajPos] != mc[indsTrajPos].iloc[0]].index[0]
                        
                        #Only 1 change in the trajectory. Change to random value less than mu and greater than lower bound
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_m'].index[0],2], mu[indsTrajPos].iloc[0])]*len(mc[indsTrajPos[indsTrajPos < indmcChange]])
                        mc[indsTrajPos[indsTrajPos < indmcChange]] = Val
                    
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_m'].index[0],2], mu[indsTrajPos].iloc[0])]*len(mc[indsTrajPos[indsTrajPos >= indmcChange]])
                        mc[indsTrajPos[indsTrajPos >= indmcChange]] = Val
                        
                        del indmcChange
                    else:
                        #mu changes
                        indmuChange = mu[indsTrajPos][mu[indsTrajPos] != mu[indsTrajPos].iloc[0]].index[0]
                        
                        #Only 1 change in the trajectory. Change to random value less than mu and greater than lower bound
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_m'].index[0],2], mu[indsTrajPos[indsTrajPos < indmuChange]].iloc[0])]*len(mc[indsTrajPos[indsTrajPos < indmuChange]])
                        mc[indsTrajPos[indsTrajPos < indmuChange]] = Val
                    
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_m'].index[0],2], mu[indsTrajPos[indsTrajPos >= indmuChange]].iloc[0])]*len(mc[indsTrajPos[indsTrajPos >= indmuChange]])
                        mc[indsTrajPos[indsTrajPos >= indmuChange]] = Val
                        
                        del indmuChange
                else:
                    #within these positions, mc and mu change each exactly once. Find when those happen.
                    indmcChange = mc[indsTrajPos][mc[indsTrajPos] != mc[indsTrajPos].iloc[0]].index[0]
                    indmuChange = mu[indsTrajPos][mu[indsTrajPos] != mu[indsTrajPos].iloc[0]].index[0]
                    if indmuChange == indmcChange:
                        #Only 1 change in the trajectory. Change to random value less than mu and greater than lower bound
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_m'].index[0],2], mu[indsTrajPos[indsTrajPos < indmuChange]].iloc[0])]*len(mc[indsTrajPos[indsTrajPos < indmuChange]])
                        mc[indsTrajPos[indsTrajPos < indmuChange]] = Val
                    
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_m'].index[0],2], mu[indsTrajPos[indsTrajPos >= indmuChange]].iloc[0])]*len(mc[indsTrajPos[indsTrajPos >= indmuChange]])
                        mc[indsTrajPos[indsTrajPos >= indmuChange]] = Val
                    elif indmuChange > indmcChange:
                        #Work from mc index first
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_m'].index[0],2], mu[indsTrajPos[indsTrajPos < indmcChange]].iloc[0])]*len(mc[indsTrajPos[indsTrajPos < indmcChange]])
                        mc[indsTrajPos[indsTrajPos < indmcChange]] = Val
                    
                        #Now check the indices between indmcChange and indmuChange
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_m'].index[0],2], mu[indsTrajPos[(indsTrajPos < indmuChange) & (indsTrajPos >= indmcChange)]].iloc[0])]*len(mc[indsTrajPos[(indsTrajPos < indmuChange) & (indsTrajPos >= indmcChange)]])
                        mc[indsTrajPos[(indsTrajPos < indmuChange) & (indsTrajPos >= indmcChange)]] = Val
                    
                        #Now check the indices greater than or equal to indmuChange
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_m'].index[0],2], mu[indsTrajPos[(indsTrajPos >= indmuChange)]].iloc[0])]*len(mc[indsTrajPos[(indsTrajPos >= indmuChange)]])
                        mc[indsTrajPos[(indsTrajPos >= indmuChange)]] = Val
                    else:
                        #Work from mu index first
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_m'].index[0],2], mu[indsTrajPos[indsTrajPos < indmuChange]].iloc[0])]*len(mc[indsTrajPos[indsTrajPos < indmuChange]])
                        mc[indsTrajPos[indsTrajPos < indmuChange]] = Val
                    
                        #Now check the indices between indmuChange and indmcChange
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_m'].index[0],2], mu[indsTrajPos[(indsTrajPos < indmcChange) & (indsTrajPos >= indmuChange)]].iloc[0])]*len(mc[indsTrajPos[(indsTrajPos < indmcChange) & (indsTrajPos >= indmuChange)]])
                        mc[indsTrajPos[(indsTrajPos < indmcChange) & (indsTrajPos >= indmuChange)]] = Val
                    
                        #Now check the indices greater than or equal to indmcChange
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_m'].index[0],2], mu[indsTrajPos[(indsTrajPos >= indmcChange)]].iloc[0])]*len(mc[indsTrajPos[(indsTrajPos >= indmcChange)]])
                        mc[indsTrajPos[(indsTrajPos >= indmcChange)]] = Val
                    
                    del indmcChange, indmuChange
                del Val
            del indsTrajPos
        #Save the new column
        Chains_df.loc[:, ind1 + '_m'] = mc

del compare, i, ind1, ind2, j, mu, mc, trajChange, inds

#Check sums
if sum(Chains_df.loc[:,'s109_m'] > Chains_df.loc[:,'s9_m']) != 0:
    sys.exit('PyERROR30: s109 m not < s9 m')
if sum(Chains_df.loc[:,'s108_m'] > Chains_df.loc[:,'s8_m']) != 0:
    sys.exit('PyERROR31: s108 m not < s8 m')
    
#%% Porosity
#porosity uncompacted >= porosity compacted for each soil type:
#If it is not greater, simulate a random value less than pu for pc
for i in range(len(Chains_df.filter(regex='_porosity_0$').columns)):
    #Find the indicator for the first soil type
    #string = Get all columns containing porosity_0
    ind1 = re.split(string=Chains_df.filter(regex='_porosity_0$').columns[i], pattern='_')[0]
    
    #Only work on the columns that have a 0, and find the other corresponding index
    if len(re.split(string=ind1, pattern='0')) == 2:
        ind2 = string.join(re.split(string=ind1, pattern='10'), sep='')
        #Use the indicator to extract the m compacted, and m uncompacted columns
        pc = Chains_df.loc[:, ind1 + '_porosity_0']
        pu = Chains_df.loc[:, ind2 + '_porosity_0']
        
        #Save the original column
        Chains_df.loc[:, 'orig_' + ind1 + '_porosity_0_orig'] = pc
        
        #Go through the trajectories to check the condition.
        compare = pc > pu
    
        #Get the indices for which this is true
        inds = compare[compare == True].index
    
        #Get the indices of the Morris trajectory starting points. 
        #Changes in these variables will only happen between those points.
        trajChange = range((len(ProbFile.iloc[:,0])+1), (len(ProbFile.iloc[:,0])+1)*(N+1), (len(ProbFile.iloc[:,0])+1))
    
        #Loop over the number of trajectories to replace values in the series
        for j in range(len(trajChange)):
            #Get indices for which the constraint is violated that are in this trajectory
            if j == 0:
                indsTrajPos = inds[(inds < trajChange[j])]
            else:
                indsTrajPos = inds[(inds >= trajChange[j-1]) & (inds < trajChange[j])]
    
            if len(indsTrajPos) > 0:
                #There are values to be adjusted
                #Check if all of the indices have exactly the same value.
                if (len(pu[indsTrajPos].unique()) == 1) & (len(pc[indsTrajPos].unique()) == 1):
                    #No change in this trajectory for the selected indices
                    #Change all pc to a random value
                    Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_porosity_0'].index[0],2], pu[indsTrajPos].iloc[0])]*len(pc[indsTrajPos])
                    pc[indsTrajPos] = Val
                elif (len(pu[indsTrajPos].unique()) == 1) | (len(pc[indsTrajPos].unique()) == 1):
                    #There is one change in this trajectory for one, but not the other variable
                    if (len(pu[indsTrajPos].unique()) == 1):
                        #pc changes
                        indpcChange = pc[indsTrajPos][pc[indsTrajPos] != pc[indsTrajPos].iloc[0]].index[0]
                        
                        #Only 1 change in the trajectory. Change to random value less than pu and greater than lower bound
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_porosity_0'].index[0],2], pu[indsTrajPos].iloc[0])]*len(pc[indsTrajPos[indsTrajPos < indpcChange]])
                        pc[indsTrajPos[indsTrajPos < indpcChange]] = Val
                    
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_porosity_0'].index[0],2], pu[indsTrajPos].iloc[0])]*len(pc[indsTrajPos[indsTrajPos >= indpcChange]])
                        pc[indsTrajPos[indsTrajPos >= indpcChange]] = Val
                        
                        del indpcChange
                    else:
                        #pu changes
                        indpuChange = pu[indsTrajPos][pu[indsTrajPos] != pu[indsTrajPos].iloc[0]].index[0]
                        
                        #Only 1 change in the trajectory. Change to random value less than pu and greater than lower bound
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_porosity_0'].index[0],2], pu[indsTrajPos[indsTrajPos < indpuChange]].iloc[0])]*len(pc[indsTrajPos[indsTrajPos < indpuChange]])
                        pc[indsTrajPos[indsTrajPos < indpuChange]] = Val
                    
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_porosity_0'].index[0],2], pu[indsTrajPos[indsTrajPos >= indpuChange]].iloc[0])]*len(pc[indsTrajPos[indsTrajPos >= indpuChange]])
                        pc[indsTrajPos[indsTrajPos >= indpuChange]] = Val
                        
                        del indpuChange
                else:
                    #within these positions, pc and pu change each exactly once. Find when those happen.
                    indpcChange = pc[indsTrajPos][pc[indsTrajPos] != pc[indsTrajPos].iloc[0]].index[0]
                    indpuChange = pu[indsTrajPos][pu[indsTrajPos] != pu[indsTrajPos].iloc[0]].index[0]
                    if indpuChange == indpcChange:
                        #Only 1 change in the trajectory. Change to random value less than pu and greater than lower bound
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_porosity_0'].index[0],2], pu[indsTrajPos[indsTrajPos < indpuChange]].iloc[0])]*len(pc[indsTrajPos[indsTrajPos < indpuChange]])
                        pc[indsTrajPos[indsTrajPos < indpuChange]] = Val
                    
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_porosity_0'].index[0],2], pu[indsTrajPos[indsTrajPos >= indpuChange]].iloc[0])]*len(pc[indsTrajPos[indsTrajPos >= indpuChange]])
                        pc[indsTrajPos[indsTrajPos >= indpuChange]] = Val
                    elif indpuChange > indpcChange:
                        #Work from pc index first
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_porosity_0'].index[0],2], pu[indsTrajPos[indsTrajPos < indpcChange]].iloc[0])]*len(pc[indsTrajPos[indsTrajPos < indpcChange]])
                        pc[indsTrajPos[indsTrajPos < indpcChange]] = Val
                    
                        #Now check the indices between indpcChange and indpuChange
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_porosity_0'].index[0],2], pu[indsTrajPos[(indsTrajPos < indpuChange) & (indsTrajPos >= indpcChange)]].iloc[0])]*len(pc[indsTrajPos[(indsTrajPos < indpuChange) & (indsTrajPos >= indpcChange)]])
                        pc[indsTrajPos[(indsTrajPos < indpuChange) & (indsTrajPos >= indpcChange)]] = Val
                    
                        #Now check the indices greater than or equal to indpuChange
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_porosity_0'].index[0],2], pu[indsTrajPos[(indsTrajPos >= indpuChange)]].iloc[0])]*len(pc[indsTrajPos[(indsTrajPos >= indpuChange)]])
                        pc[indsTrajPos[(indsTrajPos >= indpuChange)]] = Val
                    else:
                        #Work from pu index first
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_porosity_0'].index[0],2], pu[indsTrajPos[indsTrajPos < indpuChange]].iloc[0])]*len(pc[indsTrajPos[indsTrajPos < indpuChange]])
                        pc[indsTrajPos[indsTrajPos < indpuChange]] = Val
                    
                        #Now check the indices between indpuChange and indpcChange
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_porosity_0'].index[0],2], pu[indsTrajPos[(indsTrajPos < indpcChange) & (indsTrajPos >= indpuChange)]].iloc[0])]*len(pc[indsTrajPos[(indsTrajPos < indpcChange) & (indsTrajPos >= indpuChange)]])
                        pc[indsTrajPos[(indsTrajPos < indpcChange) & (indsTrajPos >= indpuChange)]] = Val
                    
                        #Now check the indices greater than or equal to indpcChange
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_porosity_0'].index[0],2], pu[indsTrajPos[(indsTrajPos >= indpcChange)]].iloc[0])]*len(pc[indsTrajPos[(indsTrajPos >= indpcChange)]])
                        pc[indsTrajPos[(indsTrajPos >= indpcChange)]] = Val
                    
                    del indpcChange, indpuChange
                del Val
            del indsTrajPos
        #Save the new column
        Chains_df.loc[:, ind1 + '_porosity_0'] = pc

del compare, i, ind1, ind2, j, pu, pc, trajChange, inds

#Check sums
if sum(Chains_df.loc[:,'s109_porosity_0'] > Chains_df.loc[:,'s9_porosity_0']) != 0:
    sys.exit('PyERROR32: s109 porosity_0 not < s9 porosity_0')
if sum(Chains_df.loc[:,'s108_porosity_0'] > Chains_df.loc[:,'s8_porosity_0']) != 0:
    sys.exit('PyERROR33: s108 porosity_0 not < s8 porosity_0')

#%% Vegetation

#%% K_absorptance + K_reflectance + K_transmittance = 1 - round check
#The only vegetation for SA that is changed is #102 - trees. Other two are 1 - values assigned randomly.
#Fixme: For the calibration will need a different check because all vars could be adjusted.
Chains_df = FullSum3Check(Col1='_K_absorptance', Col2='_K_reflectance', Col3='_K_transmittance', MorrisSample_df=Chains_df, N=N, ProbFile=ProbFile, regexNm='102_K_absorptance$', roundTol=roundTol)

#Check sums
if any((Chains_df.loc[:, 'v102_K_absorptance'] + Chains_df.loc[:, 'v102_K_reflectance'] + Chains_df.loc[:, 'v102_K_transmittance']).round(roundTol) != 1.0000):
    sys.exit('PyERROR34: Sum of K absorptance, reflectance, transmittance != 1 for v102')

#%% PAR_absorptance + PAR_reflectance + PAR_transmittance = 1 - round check
#The only venetation for SA that is changed is #102 - trees. Other two are 1 - values assigned randomly.
#Fixme: For the calibration will need a different check because all vars could be adjusted.
Chains_df = FullSum3Check(Col1='_PAR_absorptance', Col2='_PAR_reflectance', Col3='_PAR_transmittance', MorrisSample_df=Chains_df, N=N, ProbFile=ProbFile, regexNm='102_PAR_absorptance$', roundTol=roundTol)

#Check sums
if any((Chains_df.loc[:, 'v102_PAR_absorptance'] + Chains_df.loc[:, 'v102_PAR_reflectance'] + Chains_df.loc[:, 'v102_PAR_transmittance']).round(roundTol) != 1.0000):
    sys.exit('PyERROR35: Sum of PAR absorptance, reflectance, transmittance != 1 for v102')

#%% epc.frootlitr_fcel + epc.frootlitr_flab + epc.frootlitr_flig = 1 - round check
#The only venetation for SA that is changed is #102 - trees and #3 - grass. Other is not random.
Chains_df = FullSum3Check(Col1='_epc.frootlitr_fcel', Col2='_epc.frootlitr_flab', Col3='_epc.frootlitr_flig', MorrisSample_df=Chains_df, N=N, ProbFile=ProbFile, regexNm='epc.frootlitr_fcel$', roundTol=roundTol)

#Check sums
if any((Chains_df.loc[:, 'v102_epc.frootlitr_fcel'] + Chains_df.loc[:, 'v102_epc.frootlitr_flab'] + Chains_df.loc[:, 'v102_epc.frootlitr_flig']).round(roundTol) != 1.0000):
    sys.exit('PyERROR36: Sum of frootlitr_fcel, flab, flig != 1 for v102')
if any((Chains_df.loc[:, 'v3_epc.frootlitr_fcel'] + Chains_df.loc[:, 'v3_epc.frootlitr_flab'] + Chains_df.loc[:, 'v3_epc.frootlitr_flig']).round(roundTol) != 1.0000):
    sys.exit('PyERROR37: Sum of frootlitr_fcel, flab, flig != 1 for v3')
    
#%% epc.leaflitr_fcel + epc.leaflitr_flab + epc.leaflitr_flig = 1 - round check
#The only venetation for SA that is changed is #102 - trees and #3 - grass. Other is not random.
Chains_df = FullSum3Check(Col1='_epc.leaflitr_fcel', Col2='_epc.leaflitr_flab', Col3='_epc.leaflitr_flig', MorrisSample_df=Chains_df, N=N, ProbFile=ProbFile, regexNm='epc.leaflitr_fcel$', roundTol=roundTol)

#Check sums
if any((Chains_df.loc[:, 'v102_epc.leaflitr_fcel'] + Chains_df.loc[:, 'v102_epc.leaflitr_flab'] + Chains_df.loc[:, 'v102_epc.leaflitr_flig']).round(roundTol) != 1.0000):
    sys.exit('PyERROR38: Sum of leaflitr_fcel, flab, flig != 1 for v102')
if any((Chains_df.loc[:, 'v3_epc.leaflitr_fcel'] + Chains_df.loc[:, 'v3_epc.leaflitr_flab'] + Chains_df.loc[:, 'v3_epc.leaflitr_flig']).round(roundTol) != 1.0000):
    sys.exit('PyERROR39: Sum of leaflitr_fcel, flab, flig != 1 for v3')
#%% epc.topt <= epc.tmax
#If epc.tmax is not greater, simulate a random value less than epc.tmax for epc.topt
for i in range(len(Chains_df.filter(regex='epc.topt$').columns)):
    #Find the indicator for the soil type
    #string = Get all columns containing epc.tmax
    ind = re.split(string=Chains_df.filter(regex='epc.topt$').columns[i], pattern='_')[0]
    #Use the indicator to extract the epc.topt, and epc.tmax columns
    Ksat = Chains_df.loc[:, ind + '_epc.tmax']
    vKsat = Chains_df.loc[:, ind + '_epc.topt']
    
    #Save the original column.
    Chains_df.loc[:, 'orig_' + ind + '_epc.topt_orig'] = vKsat
    
    #Go through the trajectories to check the condition.
    compare = vKsat > Ksat
    
    #Get the indices for which this is true
    inds = compare[compare == True].index
    
    #Get the indices of the Morris trajectory starting points. 
    #Changes in these variables will only happen between those points
    trajChange = range((len(ProbFile.iloc[:,0])+1), (len(ProbFile.iloc[:,0])+1)*(N+1), (len(ProbFile.iloc[:,0])+1))
    
    #Loop over the number of trajectories to replace values in the series
    for j in range(len(trajChange)):
        #Get indices for which the constraint is violated that are in this trajectory
        if j == 0:
            indsTrajPos = inds[(inds < trajChange[j])]
        else:
            indsTrajPos = inds[(inds >= trajChange[j-1]) & (inds < trajChange[j])]

        if len(indsTrajPos) > 0:
            #There are values to be adjusted.
            #Must search for more than 1 change point per series because of the changes to Ksat compacted above.
            
            #Check if all of the indices have exactly the same value.
            if (len(Ksat[indsTrajPos].unique()) == 1) & (len(vKsat[indsTrajPos].unique()) == 1):
                #No change in this trajectory for the selected indices
                #Change all vKsat to a random value
                Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.topt'].index[0],2], Ksat[indsTrajPos].iloc[0])]*len(vKsat[indsTrajPos])
                vKsat[indsTrajPos] = Val
            else:
                #determine all of the indices for each
                for k in range(len(vKsat[indsTrajPos].unique())):
                    #Find the first instance of the unique value and record it
                    if k == 0:
                        #Start new inex array
                        indvKsatChange = vKsat[indsTrajPos][vKsat[indsTrajPos] == vKsat[indsTrajPos].unique()[k]].index[0]
                    else:
                        #append to list
                        indvKsatChange = np.append(indvKsatChange, vKsat[indsTrajPos][vKsat[indsTrajPos] == vKsat[indsTrajPos].unique()[k]].index[0])
                del k
                for k in range(len(Ksat[indsTrajPos].unique())):
                    #Find the first instance of the unique value and record it
                    if k == 0:
                        #Start new inex array
                        indKsatChange = Ksat[indsTrajPos][Ksat[indsTrajPos] == Ksat[indsTrajPos].unique()[k]].index[0]
                    else:
                        #append to list
                        indKsatChange = np.append(indKsatChange, Ksat[indsTrajPos][Ksat[indsTrajPos] == Ksat[indsTrajPos].unique()[k]].index[0])
                del k
                
                #Join into one list, which is sorted by the unique command by default.
                IndsAll = np.unique(np.append(indKsatChange, indvKsatChange))
                
                #Start looping through the IndsAll indices, and replace as needed.
                for k in range(len(IndsAll)):
                    if k == (len(IndsAll)-1):
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.topt'].index[0],2], Ksat[indsTrajPos[(indsTrajPos >= IndsAll[k])]].iloc[0])]*len(vKsat[indsTrajPos[(indsTrajPos >= IndsAll[k])]])
                        vKsat[indsTrajPos[(indsTrajPos >= IndsAll[k])]] = Val
                    else: 
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.topt'].index[0],2], Ksat[indsTrajPos[(indsTrajPos >= IndsAll[k]) & (indsTrajPos < IndsAll[k+1])]].iloc[0])]*len(vKsat[indsTrajPos[(indsTrajPos >= IndsAll[k]) & (indsTrajPos < IndsAll[k+1])]])
                        vKsat[indsTrajPos[(indsTrajPos >= IndsAll[k]) & (indsTrajPos < IndsAll[k+1])]] = Val
                
                del indvKsatChange, indKsatChange, k, IndsAll
            del Val
        del indsTrajPos
    #Save the new column
    Chains_df.loc[:, ind + '_epc.topt'] = vKsat

del compare, i, ind, j, vKsat, Ksat, trajChange, inds

#Check sums
if sum(Chains_df.loc[:,'v102_epc.topt'] > Chains_df.loc[:,'v102_epc.tmax']) != 0:
    sys.exit('PyERROR40: topt not < tmax for v102')
if sum(Chains_df.loc[:,'v3_epc.topt'] > Chains_df.loc[:,'v3_epc.tmax']) != 0:
    sys.exit('PyERROR41: topt not < tmax for v3')

#%% Leaf litter CN >= leaf CN
#If epc.leaflitr_cn is not greater, simulate a random value less than epc.leaflitr_cn for epc.leaf_cn
for i in range(len(Chains_df.filter(regex='epc.leaf_cn$').columns)):
    #Find the indicator for the soil type
    #string = Get all columns containing epc.leaflitr_cn
    ind = re.split(string=Chains_df.filter(regex='epc.leaf_cn$').columns[i], pattern='_')[0]
    #Use the indicator to extract the epc.leaf_cn, and epc.leaflitr_cn columns
    Ksat = Chains_df.loc[:, ind + '_epc.leaflitr_cn']
    vKsat = Chains_df.loc[:, ind + '_epc.leaf_cn']
    
    #Save the original column.
    Chains_df.loc[:, 'orig_' + ind + '_epc.leaf_cn_orig'] = vKsat
    
    #Go through the trajectories to check the condition.
    compare = vKsat > Ksat
    
    #Get the indices for which this is true
    inds = compare[compare == True].index
    
    #Get the indices of the Morris trajectory starting points. 
    #Changes in these variables will only happen between those points
    trajChange = range((len(ProbFile.iloc[:,0])+1), (len(ProbFile.iloc[:,0])+1)*(N+1), (len(ProbFile.iloc[:,0])+1))
    
    #Loop over the number of trajectories to replace values in the series
    for j in range(len(trajChange)):
        #Get indices for which the constraint is violated that are in this trajectory
        if j == 0:
            indsTrajPos = inds[(inds < trajChange[j])]
        else:
            indsTrajPos = inds[(inds >= trajChange[j-1]) & (inds < trajChange[j])]

        if len(indsTrajPos) > 0:
            #There are values to be adjusted.
            #Must search for more than 1 change point per series because of the changes to Ksat compacted above.
            
            #Check if all of the indices have exactly the same value.
            if (len(Ksat[indsTrajPos].unique()) == 1) & (len(vKsat[indsTrajPos].unique()) == 1):
                #No change in this trajectory for the selected indices
                #Change all vKsat to a random value
                Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaf_cn'].index[0],2], Ksat[indsTrajPos].iloc[0])]*len(vKsat[indsTrajPos])
                vKsat[indsTrajPos] = Val
            else:
                #determine all of the indices for each
                for k in range(len(vKsat[indsTrajPos].unique())):
                    #Find the first instance of the unique value and record it
                    if k == 0:
                        #Start new inex array
                        indvKsatChange = vKsat[indsTrajPos][vKsat[indsTrajPos] == vKsat[indsTrajPos].unique()[k]].index[0]
                    else:
                        #append to list
                        indvKsatChange = np.append(indvKsatChange, vKsat[indsTrajPos][vKsat[indsTrajPos] == vKsat[indsTrajPos].unique()[k]].index[0])
                del k
                for k in range(len(Ksat[indsTrajPos].unique())):
                    #Find the first instance of the unique value and record it
                    if k == 0:
                        #Start new inex array
                        indKsatChange = Ksat[indsTrajPos][Ksat[indsTrajPos] == Ksat[indsTrajPos].unique()[k]].index[0]
                    else:
                        #append to list
                        indKsatChange = np.append(indKsatChange, Ksat[indsTrajPos][Ksat[indsTrajPos] == Ksat[indsTrajPos].unique()[k]].index[0])
                del k
                
                #Join into one list, which is sorted by the unique command by default.
                IndsAll = np.unique(np.append(indKsatChange, indvKsatChange))
                
                #Start looping through the IndsAll indices, and replace as needed.
                for k in range(len(IndsAll)):
                    if k == (len(IndsAll)-1):
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaf_cn'].index[0],2], Ksat[indsTrajPos[(indsTrajPos >= IndsAll[k])]].iloc[0])]*len(vKsat[indsTrajPos[(indsTrajPos >= IndsAll[k])]])
                        vKsat[indsTrajPos[(indsTrajPos >= IndsAll[k])]] = Val
                    else: 
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaf_cn'].index[0],2], Ksat[indsTrajPos[(indsTrajPos >= IndsAll[k]) & (indsTrajPos < IndsAll[k+1])]].iloc[0])]*len(vKsat[indsTrajPos[(indsTrajPos >= IndsAll[k]) & (indsTrajPos < IndsAll[k+1])]])
                        vKsat[indsTrajPos[(indsTrajPos >= IndsAll[k]) & (indsTrajPos < IndsAll[k+1])]] = Val
                
                del indvKsatChange, indKsatChange, k, IndsAll
            del Val
        del indsTrajPos
    #Save the new column
    Chains_df.loc[:, ind + '_epc.leaf_cn'] = vKsat

del compare, i, ind, j, vKsat, Ksat, trajChange, inds

#Check sums
if sum(Chains_df.loc[:,'v102_epc.leaf_cn'] > Chains_df.loc[:,'v102_epc.leaflitr_cn']) != 0:
    sys.exit('PyERROR42: leaf_cn not < leaflitr_cn for v102')
if sum(Chains_df.loc[:,'v3_epc.leaf_cn'] > Chains_df.loc[:,'v3_epc.leaflitr_cn']) != 0:
    sys.exit('PyERROR43: leaf_cn not < leaflitr_cn for v3')

#%% Check sums for all parameters once more to make sure nothing odd happened
if (any((Chains_df.loc[:, 's8_silt'] + Chains_df.loc[:, 's8_sand'] + Chains_df.loc[:, 's8_clay']).round(roundTol) != 1.0000) | \
any((Chains_df.loc[:, 's108_silt'] + Chains_df.loc[:, 's108_sand'] + Chains_df.loc[:, 's108_clay']).round(roundTol) != 1.0000) | \
any((Chains_df.loc[:, 's9_silt'] + Chains_df.loc[:, 's9_sand'] + Chains_df.loc[:, 's9_clay']).round(roundTol) != 1.0000) | \
any((Chains_df.loc[:, 's109_silt'] + Chains_df.loc[:, 's109_sand'] + Chains_df.loc[:, 's109_clay']).round(roundTol) != 1.0000) | \
any((Chains_df.loc[:, 'v102_K_absorptance'] + Chains_df.loc[:, 'v102_K_reflectance'] + Chains_df.loc[:, 'v102_K_transmittance']).round(roundTol) != 1.0000) | \
any((Chains_df.loc[:, 'v102_PAR_absorptance'] + Chains_df.loc[:, 'v102_PAR_reflectance'] + Chains_df.loc[:, 'v102_PAR_transmittance']).round(roundTol) != 1.0000) | \
any((Chains_df.loc[:, 'v102_epc.frootlitr_fcel'] + Chains_df.loc[:, 'v102_epc.frootlitr_flab'] + Chains_df.loc[:, 'v102_epc.frootlitr_flig']).round(roundTol) != 1.0000) | \
any((Chains_df.loc[:, 'v3_epc.frootlitr_fcel'] + Chains_df.loc[:, 'v3_epc.frootlitr_flab'] + Chains_df.loc[:, 'v3_epc.frootlitr_flig']).round(roundTol) != 1.0000) | \
any((Chains_df.loc[:, 'v102_epc.leaflitr_fcel'] + Chains_df.loc[:, 'v102_epc.leaflitr_flab'] + Chains_df.loc[:, 'v102_epc.leaflitr_flig']).round(roundTol) != 1.0000) | \
any((Chains_df.loc[:, 'v3_epc.leaflitr_fcel'] + Chains_df.loc[:, 'v3_epc.leaflitr_flab'] + Chains_df.loc[:, 'v3_epc.leaflitr_flig']).round(roundTol) != 1.0000)):
    sys.exit('PyERROR44: One of the sums used to be correct, but after other function manipulations is now incorrect. This is a function bug that should be reported.')

if (sum(Chains_df.loc[:,'s109_Ksat_0'] > Chains_df.loc[:,'s9_Ksat_0']) + \
sum(Chains_df.loc[:,'s108_Ksat_0'] > Chains_df.loc[:,'s8_Ksat_0']) + \
sum(Chains_df.loc[:,'s109_Ksat_0_v'] > Chains_df.loc[:,'s109_Ksat_0']) + \
sum(Chains_df.loc[:,'s9_Ksat_0_v'] > Chains_df.loc[:,'s9_Ksat_0']) + \
sum(Chains_df.loc[:,'s108_Ksat_0_v'] > Chains_df.loc[:,'s108_Ksat_0']) + \
sum(Chains_df.loc[:,'s8_Ksat_0_v'] > Chains_df.loc[:,'s8_Ksat_0']) + \
sum(Chains_df.loc[:,'s109_Ksat_0_v'] > Chains_df.loc[:,'s9_Ksat_0_v']) + \
sum(Chains_df.loc[:,'s108_Ksat_0_v'] > Chains_df.loc[:,'s8_Ksat_0_v']) + \
sum(Chains_df.loc[:,'s109_m'] > Chains_df.loc[:,'s9_m']) + \
sum(Chains_df.loc[:,'s108_m'] > Chains_df.loc[:,'s8_m']) + \
sum(Chains_df.loc[:,'s108_porosity_0'] > Chains_df.loc[:,'s8_porosity_0']) + \
sum(Chains_df.loc[:,'s109_porosity_0'] > Chains_df.loc[:,'s9_porosity_0']) + \
sum(Chains_df.loc[:,'v102_epc.topt'] > Chains_df.loc[:,'v102_epc.tmax']) + \
sum(Chains_df.loc[:,'v3_epc.topt'] > Chains_df.loc[:,'v3_epc.tmax']) + \
sum(Chains_df.loc[:,'v102_epc.leaf_cn'] > Chains_df.loc[:,'v102_epc.leaflitr_cn']) + \
sum(Chains_df.loc[:,'v3_epc.leaf_cn'] > Chains_df.loc[:,'v3_epc.leaflitr_cn'])) != 0:
    sys.exit('PyERROR45: One of the inequalities used to be correct, but after other function manipulations is now incorrect. This is a function bug that should be reported.')

#%% All constraints have passed. Now make sure that the parameter that was supposed to change actually changed.
#Original file
OrigMorrisSample_df = pd.read_csv('MorrisSamples_BeforeProcessing.csv')

#Find all of the instances in which a parameter does not change in the AfterProcessing.csv file
ListMissing = []
ColsMissing = []
for i in range(len(Chains_df.iloc[:,0])-1):
    #Check that there is a difference from the current row to the next row.
    #Absolute value because some changes have sum constraints such that a difference will always be 0
    if sum(abs(Chains_df.iloc[i,:len(OrigMorrisSample_df.columns)] - Chains_df.iloc[i+1,:len(OrigMorrisSample_df.columns)])) == 0:
        #record i+1 because that is the row that should have changed
        ListMissing.append(i+1)
        #Find which parameter should have changed - get the name
        #OrigMorrisSample_df.iloc[i,np.where(((OrigMorrisSample_df.iloc[i,:] - OrigMorrisSample_df.iloc[i+1,:]) != 0).values == True)[0][0]]
        ColsMissing.append(OrigMorrisSample_df.columns[np.where(((OrigMorrisSample_df.iloc[i,:] - OrigMorrisSample_df.iloc[i+1,:]) != 0).values == True)[0][0]])

if len(ListMissing) > 0:
    sys.exit('PyERROR46: Some of the variables that were supposed to change in the Morris trajectory did not change.')
else:
    #Write the resulting Chains_df to a csv file
    Chains_df.round(roundTol).to_csv('MorrisSamples_AfterProcessing.csv', index = False)  