# -*- coding: utf-8 -*-
"""
Created on Sun Nov  3 18:18:27 2019

@author: jsmif
"""

import os
import pandas as pd
import sys
import numpy as np
import string

#%% Set working directory
os.chdir('/scratch/js4yd/MorrisSA/RHESSysRuns')

#%% Set parameters
roundTol = 10

#%% Read the MorrisSample_df and problem file with bounds of parameters
ProbFile = pd.read_csv('BaismanMorrisSamplingProblemFile_Full.csv')
MorrisSample_df = pd.read_csv('MorrisSamples_AfterProcessing.csv')

#%% Make the definition files from the Morris sample locations and place them in new directories by replicate ID
#Load original def files. Use object dtype because the parameters each have different types.
basin = pd.read_table('/scratch/js4yd/MorrisSA/RHESSys_Baisman30m_g74/defs/basin_basin.def', delim_whitespace=True, header=None, dtype = 'object')
zone = pd.read_table('/scratch/js4yd/MorrisSA/RHESSys_Baisman30m_g74/defs/zone_zone.def', delim_whitespace=True, header=None, dtype = 'object')
hill = pd.read_table('/scratch/js4yd/MorrisSA/RHESSys_Baisman30m_g74/defs/hillslope_hillslope.def', delim_whitespace=True, header=None, dtype = 'object')
soil_loam = pd.read_table('/scratch/js4yd/MorrisSA/RHESSys_Baisman30m_g74/defs/soil_loam.def', delim_whitespace=True, header=None, dtype = 'object')
soil_cloam = pd.read_table('/scratch/js4yd/MorrisSA/RHESSys_Baisman30m_g74/defs/soil_loam_compact.def', delim_whitespace=True, header=None, dtype = 'object')
soil_sloam = pd.read_table('/scratch/js4yd/MorrisSA/RHESSys_Baisman30m_g74/defs/soil_silt_loam.def', delim_whitespace=True, header=None, dtype = 'object')
soil_csloam = pd.read_table('/scratch/js4yd/MorrisSA/RHESSys_Baisman30m_g74/defs/soil_silt_loam_compact.def', delim_whitespace=True, header=None, dtype = 'object')
land_grass = pd.read_table('/scratch/js4yd/MorrisSA/RHESSys_Baisman30m_g74/defs/landuse_grass.def', delim_whitespace=True, header=None, dtype = 'object')
land_undev = pd.read_table('/scratch/js4yd/MorrisSA/RHESSys_Baisman30m_g74/defs/landuse_undeveloped.def', delim_whitespace=True, header=None, dtype = 'object')
land_urban = pd.read_table('/scratch/js4yd/MorrisSA/RHESSys_Baisman30m_g74/defs/landuse_urban.def', delim_whitespace=True, header=None, dtype = 'object')
land_septic = pd.read_table('/scratch/js4yd/MorrisSA/RHESSys_Baisman30m_g74/defs/landuse_urbanSeptic.def', delim_whitespace=True, header=None, dtype = 'object')
veg_Tree = pd.read_table('/scratch/js4yd/MorrisSA/RHESSys_Baisman30m_g74/defs/stratum_deciduousBES.def', delim_whitespace=True, header=None, dtype = 'object')
veg_grass = pd.read_table('/scratch/js4yd/MorrisSA/RHESSys_Baisman30m_g74/defs/stratum_grass.def', delim_whitespace=True, header=None, dtype = 'object')
veg_NonVeg = pd.read_table('/scratch/js4yd/MorrisSA/RHESSys_Baisman30m_g74/defs/stratum_nonveg.def', delim_whitespace=True, header=None, dtype = 'object')

#for i in range(len(MorrisSample_df.iloc[:,0])):
for i in [int(sys.argv[1])]:
    #Save original wd
    od = os.getcwd()
    #Make new directory for the replicate. This is the directory where code to run GIS2RHESSys pre-processing will be
    os.mkdir(od+'/Run'+str(i))
    #Make the directory in which the RHESSys simulation will be run
    os.mkdir(od+'/Run'+str(i)+'/RHESSys_Baisman30m_g74')
    #Make new directory for the def files in that folder
    os.mkdir(od+'/Run'+str(i)+'/RHESSys_Baisman30m_g74'+'/defs')
    #Change into that directory
    os.chdir(od+'/Run'+str(i)+'/RHESSys_Baisman30m_g74'+'/defs')
    
    #%% Zone
    #Edit the zone file with the variables generated
    #Make a deep copy of the zone file so that the original zone file remains unchanged
    zc = zone.copy(deep=True)
    #Loop over the MorrisSample_df data for z_. These variables were random and should be changed
    IndZ = []
    for z in range(len(MorrisSample_df.columns)):
        #If the z is in the first position, it's a zone file. Same concept used for other defs below.
        if string.find(MorrisSample_df.columns[z], 'z') == 0:
            #Record the index. This variable should be used for zones
            IndZ.append(z)
    del z
    #Now loop over these columns and extract the ProbFile variable name that matches the def file name
    for z in range(len(IndZ)):
        defName = ProbFile.iloc[:,1][ProbFile.iloc[:,0] == MorrisSample_df.columns[IndZ[z]]].values[0]
        #Find the defName in the def file and replace it with the ith Morris row
        #Fixme: reporting roundTol decimal places here to avoid scientific notation, and avoid loss of precision in atm trans lapse rate
        zc.iloc[:,0][zc.iloc[:,1] == defName] = '%.10f' % MorrisSample_df.iloc[i,IndZ[z]].round(roundTol)
    
    #Check that all of the columns that are different from the original zone file correspond to the ones in the MorrisSample dataset
    checkCols = [(string.split(s=ck, sep='z_'))[1] for ck in MorrisSample_df.columns[IndZ]]
    if not all(checkCols == zone.iloc[zone.index[((zone.iloc[:,0] != zc.iloc[:,0]) == True)],1].values):
        sys.exit('Zone def file not constructed with values correctly for Replicate = %s' % str(i))
    del ck, checkCols
    
    #Check that the replaced values are the same as the ones in the MorrisSample.
    #Fixme: need to guarantee that the same columns are being compared between the two.
    #If def files have parameters in the same order as listed in the MorrisSample_df, it's guaranteed.
    checkVals = [float(ck) for ck in (zc.iloc[zc.index[((zone.iloc[:,0] != zc.iloc[:,0]) == True)],0].values)]
    if not all(MorrisSample_df.iloc[i, IndZ].values.round(roundTol) == checkVals):
        sys.exit('Zone def file not constructed with values correctly for Replicate = %s' % str(i))
    
    #Write new zone file
    zc.to_csv('zone_zone.def', sep=' ', index=False, header=False, encoding='utf-8')
    del zc, IndZ, defName, z, ck, checkVals
    #%% Hillslope
    #Edit the hillslope file with the variables generated
    #Make a deep copy of the hillslope file so that the original hillslope file remains unchanged
    hc = hill.copy(deep=True)
    #Loop over the MorrisSample_df data for h_. These variables were random and should be changed
    IndH = []
    for h in range(len(MorrisSample_df.columns)):
        if string.find(MorrisSample_df.columns[h], 'h') == 0:
            #Record the index. This variable should be used for hillslopes
            IndH.append(h)
    del h
    #Now loop over these columns and extract the ProbFile variable name that matches the def file name
    for h in range(len(IndH)):
        defName = ProbFile.iloc[:,1][ProbFile.iloc[:,0] == MorrisSample_df.columns[IndH[h]]].values[0]
        #Find the defName in the def file and replace it with the ith Morris row
        #Fixme: reporting roundTol decimal places here to avoid scientific notation, and avoid loss of precision in atm trans lapse rate
        hc.iloc[:,0][hc.iloc[:,1] == defName] = '%.10f' % MorrisSample_df.iloc[i,IndH[h]].round(roundTol)
    
    #Fixme: see above for guarantee of columns in same order
    checkCols = [(string.split(s=ck, sep='h_'))[1] for ck in MorrisSample_df.columns[IndH]]
    if not all(checkCols == hill.iloc[hill.index[((hill.iloc[:,0] != hc.iloc[:,0]) == True)],1].values):
        sys.exit('Hill def file not constructed with values correctly for Replicate = %s' % str(i))
    del ck, checkCols
    
    #Check that the replaced values are the same as the ones in the MorrisSample.
    #Fixme: need to guarantee that the same columns are being compared between the two.
    #If def files have parameters in the same order as listed in the MorrisSample_df, it's guaranteed.
    checkVals = [float(ck) for ck in (hc.iloc[hc.index[((hill.iloc[:,0] != hc.iloc[:,0]) == True)],0].values)]
    if not all(MorrisSample_df.iloc[i, IndH].values.round(roundTol) == checkVals):
        sys.exit('Hill def file not constructed with values correctly for Replicate = %s' % str(i))
    
    #Write new hillslope file
    hc.to_csv('hillslope_hillslope.def', sep=' ', index=False, header=False, encoding='utf-8')
    del hc, IndH, defName, h, ck, checkVals
    #%% Land Use
    #Load land use files
    #l1 - grass
    #Edit the land_grass file with the variables generated
    #Make a deep copy of the land_grass file so that the original land_grass file remains unchanged
    l1c = land_grass.copy(deep=True)
    #Loop over the MorrisSample_df data for l_. These variables were random and should be changed
    IndL1 = []
    for l in range(len(MorrisSample_df.columns)):
        if string.find(MorrisSample_df.columns[l], 'l1') == 0:
            #Record the index. This variable should be used for land_grass
            IndL1.append(l)
    del l
    #Now loop over these columns and extract the ProbFile variable name that matches the def file name
    for l in range(len(IndL1)):
        defName = ProbFile.iloc[:,1][ProbFile.iloc[:,0] == MorrisSample_df.columns[IndL1[l]]].values[0]
        #Find the defName in the def file and replace it with the ith Morris row
        #Fixme: reporting roundTol decimal places here to avoid scientific notation, and avoid loss of precision in atm trans lapse rate
        l1c.iloc[:,0][l1c.iloc[:,1] == defName] = '%.10f' % MorrisSample_df.iloc[i,IndL1[l]].round(roundTol)
    
    #Fixme: see above for guarantee of columns in same order
    checkCols = [(string.split(s=ck, sep='l1_'))[1] for ck in MorrisSample_df.columns[IndL1]]
    if not all(checkCols == land_grass.iloc[land_grass.index[((land_grass.iloc[:,0] != l1c.iloc[:,0]) == True)],1].values):
        sys.exit('Grass land use def file not constructed with values correctly for Replicate = %s' % str(i))
    del ck, checkCols
    
    #Check that the replaced values are the same as the ones in the MorrisSample.
    #Fixme: need to guarantee that the same columns are being compared between the two.
    #If def files have parameters in the same order as listed in the MorrisSample_df, it's guaranteed.
    checkVals = [float(ck) for ck in (l1c.iloc[l1c.index[((land_grass.iloc[:,0] != l1c.iloc[:,0]) == True)],0].values)]
    if not all(MorrisSample_df.iloc[i, IndL1].values.round(roundTol) == checkVals):
        sys.exit('Grass land use def file not constructed with values correctly for Replicate = %s' % str(i))
    
    #Write new land_grass file
    l1c.to_csv('landuse_grass.def', sep=' ', index=False, header=False, encoding='utf-8')
    del l1c, IndL1, defName, l, ck, checkVals
    
    #l2 - undeveloped
    #Edit the land file with the variables generated
    #Make a deep copy of the land file so that the original land file remains unchanged
    l2c = land_undev.copy(deep=True)
    #Loop over the MorrisSample_df data for l_. These variables were random and should be changed
    IndL2 = []
    for l in range(len(MorrisSample_df.columns)):
        if string.find(MorrisSample_df.columns[l], 'l2') == 0:
            #Record the index. This variable should be used
            IndL2.append(l)
    del l
    #Now loop over these columns and extract the ProbFile variable name that matches the def file name
    for l in range(len(IndL2)):
        defName = ProbFile.iloc[:,1][ProbFile.iloc[:,0] == MorrisSample_df.columns[IndL2[l]]].values[0]
        #Find the defName in the def file and replace it with the ith Morris row
        #Fixme: reporting roundTol decimal places here to avoid scientific notation, and avoid loss of precision in atm trans lapse rate
        l2c.iloc[:,0][l2c.iloc[:,1] == defName] = '%.10f' % MorrisSample_df.iloc[i,IndL2[l]].round(roundTol)

    #Fixme: see above for guarantee of columns in same order
    checkCols = [(string.split(s=ck, sep='l2_'))[1] for ck in MorrisSample_df.columns[IndL2]]
    if not all(checkCols == land_undev.iloc[land_undev.index[((land_undev.iloc[:,0] != l2c.iloc[:,0]) == True)],1].values):
        sys.exit('Undeveloped land use def file not constructed with values correctly for Replicate = %s' % str(i))
    del ck, checkCols
    
    #Check that the replaced values are the same as the ones in the MorrisSample.
    #Fixme: need to guarantee that the same columns are being compared between the two.
    #If def files have parameters in the same order as listed in the MorrisSample_df, it's guaranteed.
    checkVals = [float(ck) for ck in (l2c.iloc[l2c.index[((land_undev.iloc[:,0] != l2c.iloc[:,0]) == True)],0].values)]
    if not all(MorrisSample_df.iloc[i, IndL2].values.round(roundTol) == checkVals):
        sys.exit('Undeveloped land use def file not constructed with values correctly for Replicate = %s' % str(i))
    
    #Write new file
    l2c.to_csv('landuse_undeveloped.def', sep=' ', index=False, header=False, encoding='utf-8')
    del l2c, IndL2, defName, l, ck, checkVals
    
    #l3 - urban
    #Edit the land file with the variables generated
    #Make a deep copy of the land file so that the original land file remains unchanged
    l3c = land_urban.copy(deep=True)
    #Loop over the MorrisSample_df data for l_. These variables were random and should be changed
    IndL3 = []
    for l in range(len(MorrisSample_df.columns)):
        if string.find(MorrisSample_df.columns[l], 'l3') == 0:
            #Record the index. This variable should be used
            IndL3.append(l)
    del l
    #Now loop over these columns and extract the ProbFile variable name that matches the def file name
    for l in range(len(IndL3)):
        defName = ProbFile.iloc[:,1][ProbFile.iloc[:,0] == MorrisSample_df.columns[IndL3[l]]].values[0]
        #Find the defName in the def file and replace it with the ith Morris row
        #Fixme: reporting roundTol decimal places here to avoid scientific notation, and avoid loss of precision in atm trans lapse rate
        l3c.iloc[:,0][l3c.iloc[:,1] == defName] = '%.10f' % MorrisSample_df.iloc[i,IndL3[l]].round(roundTol)

    #Fixme: see above for guarantee of columns in same order
    checkCols = [(string.split(s=ck, sep='l3_'))[1] for ck in MorrisSample_df.columns[IndL3]]
    if not all(checkCols == land_urban.iloc[land_urban.index[((land_urban.iloc[:,0] != l3c.iloc[:,0]) == True)],1].values):
        sys.exit('Urban land use def file not constructed with values correctly for Replicate = %s' % str(i))
    del ck, checkCols
    
    #Check that the replaced values are the same as the ones in the MorrisSample.
    #Fixme: need to guarantee that the same columns are being compared between the two.
    #If def files have parameters in the same order as listed in the MorrisSample_df, it's guaranteed.
    checkVals = [float(ck) for ck in (l3c.iloc[l3c.index[((land_urban.iloc[:,0] != l3c.iloc[:,0]) == True)],0].values)]
    if not all(MorrisSample_df.iloc[i, IndL3].values.round(roundTol) == checkVals):
        sys.exit('Urban land use def file not constructed with values correctly for Replicate = %s' % str(i))

    #Write new file
    l3c.to_csv('landuse_urban.def', sep=' ', index=False, header=False, encoding='utf-8')
    del l3c, IndL3, defName, l, ck, checkVals
    
    #l4 - urban septic
    #Edit the land file with the variables generated
    #Make a deep copy of the file so that the original file remains unchanged
    l4c = land_septic.copy(deep=True)
    #Loop over the MorrisSample_df data for l_. These variables were random and should be changed
    IndL4 = []
    for l in range(len(MorrisSample_df.columns)):
        if string.find(MorrisSample_df.columns[l], 'l4') == 0:
            #Record the index. This variable should be used
            IndL4.append(l)
    del l
    #Now loop over these columns and extract the ProbFile variable name that matches the def file name
    for l in range(len(IndL4)):
        defName = ProbFile.iloc[:,1][ProbFile.iloc[:,0] == MorrisSample_df.columns[IndL4[l]]].values[0]
        #Find the defName in the def file and replace it with the ith Morris row
        #Fixme: reporting roundTol decimal places here to avoid scientific notation, and avoid loss of precision in atm trans lapse rate
        l4c.iloc[:,0][l4c.iloc[:,1] == defName] = '%.10f' % MorrisSample_df.iloc[i,IndL4[l]].round(roundTol)

    #Fixme: see above for guarantee of columns in same order
    checkCols = [(string.split(s=ck, sep='l4_'))[1] for ck in MorrisSample_df.columns[IndL4]]
    if not all(checkCols == land_septic.iloc[land_septic.index[((land_septic.iloc[:,0] != l4c.iloc[:,0]) == True)],1].values):
        sys.exit('Urban septic land use def file not constructed with values correctly for Replicate = %s' % str(i))
    del ck, checkCols
    
    #Check that the replaced values are the same as the ones in the MorrisSample.
    #Fixme: need to guarantee that the same columns are being compared between the two.
    #If def files have parameters in the same order as listed in the MorrisSample_df, it's guaranteed.
    checkVals = [float(ck) for ck in (l4c.iloc[l4c.index[((land_septic.iloc[:,0] != l4c.iloc[:,0]) == True)],0].values)]
    if not all(MorrisSample_df.iloc[i, IndL4].values.round(roundTol) == checkVals):
        sys.exit('Urban septic land use def file not constructed with values correctly for Replicate = %s' % str(i))

    #Write new file
    l4c.to_csv('landuse_urbanSeptic.def', sep=' ', index=False, header=False, encoding='utf-8')
    del l4c, IndL4, defName, l, ck, checkVals
    
    #%% Soils
    #s9 - loam
    #Edit the land file with the variables generated
    #Make a deep copy of the file so that the original file remains unchanged
    s9 = soil_loam.copy(deep=True)
    #Loop over the MorrisSample_df data for l_. These variables were random and should be changed
    IndS9 = []
    for s in range(len(MorrisSample_df.columns)):
        if string.find(MorrisSample_df.columns[s], 's9') == 0:
            #Record the index. This variable should be used
            IndS9.append(s)
    del s
    #Now loop over these columns and extract the ProbFile variable name that matches the def file name
    for s in range(len(IndS9)):
        defName = ProbFile.iloc[:,1][ProbFile.iloc[:,0] == MorrisSample_df.columns[IndS9[s]]].values[0]
        #Find the defName in the def file and replace it with the ith Morris row
        #Fixme: reporting roundTol decimal places here to avoid scientific notation, and avoid loss of precision in atm trans lapse rate
        s9.iloc[:,0][s9.iloc[:,1] == defName] = '%.10f' % MorrisSample_df.iloc[i,IndS9[s]].round(roundTol)

    #Sorting alphabetically to check columns are the same
    checkCols = sorted([(string.split(s=ck, sep='s9_'))[1] for ck in MorrisSample_df.columns[IndS9]])
    if not (checkCols == sorted(soil_loam.iloc[soil_loam.index[((soil_loam.iloc[:,0] != s9.iloc[:,0]) == True)],1].values.tolist())):
        sys.exit('Soil loam def file not constructed with values correctly for Replicate = %s' % str(i))
    del ck, checkCols
    
    #Check that the replaced values are the same as the ones in the MorrisSample.
    #Fixme: need to guarantee that the same columns are being compared between the two.
    #For now, just checking that the sorted lists are the same
    checkVals = sorted([str('%.10f' % round(ck,roundTol)) for ck in MorrisSample_df.iloc[i,IndS9]])
    checkVals2 = sorted([str(ck) for ck in s9.iloc[s9.index[((soil_loam.iloc[:,0] != s9.iloc[:,0]) == True)],0].values])
    if not (checkVals2 == checkVals):
        sys.exit('Soil loam def file not constructed with values correctly for Replicate = %s' % str(i))
    
    #Make edits to other variables that depend on the generated variables
    #Fixme: This substitution should technically happen before rounding in the step above. Practically, it shouldn't matter.
    #m_z = m/porosity_0
    s9.loc[:,0][s9.loc[:,1] == 'm_z'] = '%.10f' % (float(s9.loc[:,0][s9.loc[:,1] == 'm'].values[0])/float(s9.loc[:,0][s9.loc[:,1] == 'porosity_0'].values[0]))
    #soil_depth = active_zone_z
    s9.loc[:,0][s9.loc[:,1] == 'soil_depth'] = '%.10f' % s9.loc[:,0][s9.loc[:,1] == 'active_zone_z']
    
    #Write new file
    s9.to_csv('soil_loam.def', sep=' ', index=False, header=False, encoding='utf-8')
    del s9, IndS9, defName, s, ck, checkVals, checkVals2
        
    #s109 - compacted loam
    #Edit the land file with the variables generated
    #Make a deep copy of the file so that the original file remains unchanged
    s109 = soil_cloam.copy(deep=True)
    #Loop over the MorrisSample_df data for l_. These variables were random and should be changed
    Inds109 = []
    for s in range(len(MorrisSample_df.columns)):
        if string.find(MorrisSample_df.columns[s], 's109') == 0:
            #Record the index. This variable should be used
            Inds109.append(s)
    del s
    #Now loop over these columns and extract the ProbFile variable name that matches the def file name
    for s in range(len(Inds109)):
        defName = ProbFile.iloc[:,1][ProbFile.iloc[:,0] == MorrisSample_df.columns[Inds109[s]]].values[0]
        #Find the defName in the def file and replace it with the ith Morris row
        #Fixme: reporting roundTol decimal places here to avoid scientific notation, and avoid loss of precision in atm trans lapse rate
        s109.iloc[:,0][s109.iloc[:,1] == defName] = '%.10f' % MorrisSample_df.iloc[i,Inds109[s]].round(roundTol)

    #Sorting alphabetically to check columns are the same
    checkCols = sorted([(string.split(s=ck, sep='s109_'))[1] for ck in MorrisSample_df.columns[Inds109]])
    if not (checkCols == sorted(soil_cloam.iloc[soil_cloam.index[((soil_cloam.iloc[:,0] != s109.iloc[:,0]) == True)],1].values.tolist())):
        sys.exit('Soil compact loam def file not constructed with values correctly for Replicate = %s' % str(i))
    del ck, checkCols
    
    #Check that the replaced values are the same as the ones in the MorrisSample.
    #Fixme: need to guarantee that the same columns are being compared between the two.
    #For now, just checking that the sorted lists are the same
    checkVals = sorted([str('%.10f' % round(ck,roundTol)) for ck in MorrisSample_df.iloc[i,Inds109]])
    checkVals2 = sorted([str(ck) for ck in s109.iloc[s109.index[((soil_cloam.iloc[:,0] != s109.iloc[:,0]) == True)],0].values])
    if not (checkVals2 == checkVals):
        sys.exit('Soil compact loam def file not constructed with values correctly for Replicate = %s' % str(i))
    
    #Make edits to other variables that depend on the generated variables
    #Fixme: This substitution should technically happen before rounding in the step above. Practically, it shouldn't matter.
    #m_z = m/porosity_0
    s109.loc[:,0][s109.loc[:,1] == 'm_z'] = '%.10f' % (float(s109.loc[:,0][s109.loc[:,1] == 'm'].values[0])/float(s109.loc[:,0][s109.loc[:,1] == 'porosity_0'].values[0]))
    #soil_depth = active_zone_z
    s109.loc[:,0][s109.loc[:,1] == 'soil_depth'] = '%.10f' % s109.loc[:,0][s109.loc[:,1] == 'active_zone_z']
    
    #Write new file
    s109.to_csv('soil_loam_compact.def', sep=' ', index=False, header=False, encoding='utf-8')
    del s109, Inds109, defName, s, ck, checkVals, checkVals2
    
    #s8 - silty loam
    #Edit the land file with the variables generated
    #Make a deep copy of the file so that the original file remains unchanged
    s8 = soil_sloam.copy(deep=True)
    #Loop over the MorrisSample_df data for l_. These variables were random and should be changed
    Inds8 = []
    for s in range(len(MorrisSample_df.columns)):
        if string.find(MorrisSample_df.columns[s], 's8') == 0:
            #Record the index. This variable should be used
            Inds8.append(s)
    del s
    #Now loop over these columns and extract the ProbFile variable name that matches the def file name
    for s in range(len(Inds8)):
        defName = ProbFile.iloc[:,1][ProbFile.iloc[:,0] == MorrisSample_df.columns[Inds8[s]]].values[0]
        #Find the defName in the def file and replace it with the ith Morris row
        #Fixme: reporting roundTol decimal places here to avoid scientific notation, and avoid loss of precision in atm trans lapse rate
        s8.iloc[:,0][s8.iloc[:,1] == defName] = '%.10f' % MorrisSample_df.iloc[i,Inds8[s]].round(roundTol)

    #Sorting alphabetically to check columns are the same
    checkCols = sorted([(string.split(s=ck, sep='s8_'))[1] for ck in MorrisSample_df.columns[Inds8]])
    if not (checkCols == sorted(soil_sloam.iloc[soil_sloam.index[((soil_sloam.iloc[:,0] != s8.iloc[:,0]) == True)],1].values.tolist())):
        sys.exit('Soil silty loam def file not constructed with values correctly for Replicate = %s' % str(i))
    del ck, checkCols
    
    #Check that the replaced values are the same as the ones in the MorrisSample.
    #Fixme: need to guarantee that the same columns are being compared between the two.
    #For now, just checking that the sorted lists are the same
    checkVals = sorted([str('%.10f' % round(ck,roundTol)) for ck in MorrisSample_df.iloc[i,Inds8]])
    checkVals2 = sorted([str(ck) for ck in s8.iloc[s8.index[((soil_sloam.iloc[:,0] != s8.iloc[:,0]) == True)],0].values])
    if not (checkVals2 == checkVals):
        sys.exit('Soil silty loam def file not constructed with values correctly for Replicate = %s' % str(i))
    
    #Make edits to other variables that depend on the generated variables
    #Fixme: This substitution should technically happen before rounding in the step above. Practically, it shouldn't matter.
    #m_z = m/porosity_0
    s8.loc[:,0][s8.loc[:,1] == 'm_z'] = '%.10f' % (float(s8.loc[:,0][s8.loc[:,1] == 'm'].values[0])/float(s8.loc[:,0][s8.loc[:,1] == 'porosity_0'].values[0]))
    #soil_depth = active_zone_z
    s8.loc[:,0][s8.loc[:,1] == 'soil_depth'] = '%.10f' % s8.loc[:,0][s8.loc[:,1] == 'active_zone_z']
    
    #Write new file
    s8.to_csv('soil_silt_loam.def', sep=' ', index=False, header=False, encoding='utf-8')
    del s8, Inds8, defName, s, ck, checkVals, checkVals2

    #s108 - compacted silty loam
    #Edit the land file with the variables generated
    #Make a deep copy of the file so that the original file remains unchanged
    s108 = soil_csloam.copy(deep=True)
    #Loop over the MorrisSample_df data for l_. These variables were random and should be changed
    Inds108 = []
    for s in range(len(MorrisSample_df.columns)):
        if string.find(MorrisSample_df.columns[s], 's108') == 0:
            #Record the index. This variable should be used
            Inds108.append(s)
    del s
    #Now loop over these columns and extract the ProbFile variable name that matches the def file name
    for s in range(len(Inds108)):
        defName = ProbFile.iloc[:,1][ProbFile.iloc[:,0] == MorrisSample_df.columns[Inds108[s]]].values[0]
        #Find the defName in the def file and replace it with the ith Morris row
        #Fixme: reporting roundTol decimal places here to avoid scientific notation, and avoid loss of precision in atm trans lapse rate
        s108.iloc[:,0][s108.iloc[:,1] == defName] = '%.10f' % MorrisSample_df.iloc[i,Inds108[s]].round(roundTol)

    #Sorting alphabetically to check columns are the same
    checkCols = sorted([(string.split(s=ck, sep='s108_'))[1] for ck in MorrisSample_df.columns[Inds108]])
    if not (checkCols == sorted(soil_csloam.iloc[soil_csloam.index[((soil_csloam.iloc[:,0] != s108.iloc[:,0]) == True)],1].values.tolist())):
        sys.exit('Soil compact silty loam def file not constructed with values correctly for Replicate = %s' % str(i))
    del ck, checkCols
    
    #Check that the replaced values are the same as the ones in the MorrisSample.
    #Fixme: need to guarantee that the same columns are being compared between the two.
    #For now, just checking that the sorted lists are the same
    checkVals = sorted([str('%.10f' % round(ck,roundTol)) for ck in MorrisSample_df.iloc[i,Inds108]])
    checkVals2 = sorted([str(ck) for ck in s108.iloc[s108.index[((soil_csloam.iloc[:,0] != s108.iloc[:,0]) == True)],0].values])
    if not (checkVals2 == checkVals):
        sys.exit('Soil compact silty loam def file not constructed with values correctly for Replicate = %s' % str(i))
    
    #Make edits to other variables that depend on the generated variables
    #Fixme: This substitution should technically happen before rounding in the step above. Practically, it shouldn't matter.
    #m_z = m/porosity_0
    s108.loc[:,0][s108.loc[:,1] == 'm_z'] = '%.10f' % (float(s108.loc[:,0][s108.loc[:,1] == 'm'].values[0])/float(s108.loc[:,0][s108.loc[:,1] == 'porosity_0'].values[0]))
    #soil_depth = active_zone_z
    s108.loc[:,0][s108.loc[:,1] == 'soil_depth'] = '%.10f' % s108.loc[:,0][s108.loc[:,1] == 'active_zone_z']
    
    #Write new file
    s108.to_csv('soil_silt_loam_compact.def', sep=' ', index=False, header=False, encoding='utf-8')
    del s108, Inds108, defName, s, ck, checkVals, checkVals2

    
    #%%Vegetation
    #v102 - BES deciduous tree
    #Edit the land file with the variables generated
    #Make a deep copy of the file so that the original file remains unchanged
    v102 = veg_Tree.copy(deep=True)
    #Loop over the MorrisSample_df data for l_. These variables were random and should be changed
    Indv102 = []
    for v in range(len(MorrisSample_df.columns)):
        if string.find(MorrisSample_df.columns[v], 'v102') == 0:
            #Record the index. This variable should be used
            Indv102.append(v)
    del v
    #Now loop over these columns and extract the ProbFile variable name that matches the def file name
    for v in range(len(Indv102)):
        defName = ProbFile.iloc[:,1][ProbFile.iloc[:,0] == MorrisSample_df.columns[Indv102[v]]].values[0]
        #Find the defName in the def file and replace it with the ith Morris row
        #Fixme: reporting roundTol decimal places here to avoid scientific notation, and avoid loss of precision in atm trans lapse rate
        v102.iloc[:,0][v102.iloc[:,1] == defName] = '%.10f' % MorrisSample_df.iloc[i,Indv102[v]].round(roundTol)

    #Sorting alphabetically to check columns are the same
    checkCols = sorted([(string.split(s=ck, sep='v102_'))[1] for ck in MorrisSample_df.columns[Indv102]])
    if not (checkCols == sorted(veg_Tree.iloc[veg_Tree.index[((veg_Tree.iloc[:,0] != v102.iloc[:,0]) == True)],1].values.tolist())):
        sys.exit('Vegetation tree def file not constructed with values correctly for Replicate = %s' % str(i))
    del ck, checkCols
    
    #Check that the replaced values are the same as the ones in the MorrisSample.
    #Fixme: need to guarantee that the same columns are being compared between the two.
    #Fixme: Long integer with decimals is causing an issue for comparing here. It exceeds Python's comparison limits
    #For now, just checking that the sorted lists are the same manually on one
    checkVals = sorted([str('%.7f' % round(ck,roundTol)) for ck in MorrisSample_df.iloc[i,Indv102]])
    checkVals2 = sorted([str('%.7f' % round(float(ck),roundTol)) for ck in v102.iloc[v102.index[((veg_Tree.iloc[:,0] != v102.iloc[:,0]) == True)],0].values])
    if not (checkVals2 == checkVals):
        sys.exit('Vegetation tree def file not constructed with values correctly for Replicate = %s' % str(i))

    #Make edits to other variables that depend on the generated variables
    #Fixme: This substitution should technically happen before rounding in the step above. Practically, it shouldn't matter.
    #epc.deadwood_flig = 1 - epc.deadwood_fcel for trees only - round check
    v102.loc[:,0][v102.loc[:,1] == 'epc.deadwood_flig'] = '%.10f' % (1. - float(v102.loc[:,0][v102.loc[:,1] == 'epc.deadwood_fcel']))
    
    #Ensure that the sum of these two variables = 1 because rounding has now been completed.
    if (float('%.10f' % (float(v102.loc[:,0][v102.loc[:,1] == 'epc.deadwood_flig']) + float(v102.loc[:,0][v102.loc[:,1] == 'epc.deadwood_fcel']))) != 1.0):
        sys.exit('Vegetation tree epc.deadwood_flig + epc.deadwood_fcel != 1 for Replicate = %s' % str(i))
    
    #Write new file
    v102.to_csv('stratum_deciduousBES.def', sep=' ', index=False, header=False, encoding='utf-8')
    #Note: not deleting 102 because it is needed for grasses
    del Indv102, defName, v, ck, checkVals, checkVals2

    #v3 - grass
    #Edit the land file with the variables generated
    #Make a deep copy of the file so that the original file remains unchanged
    v3 = veg_grass.copy(deep=True)
    #Loop over the MorrisSample_df data for l_. These variables were random and should be changed
    Indv3 = []
    for v in range(len(MorrisSample_df.columns)):
        if string.find(MorrisSample_df.columns[v], 'v3') == 0:
            #Record the index. This variable should be used
            Indv3.append(v)
    del v
    #Now loop over these columns and extract the ProbFile variable name that matches the def file name
    for v in range(len(Indv3)):
        defName = ProbFile.iloc[:,1][ProbFile.iloc[:,0] == MorrisSample_df.columns[Indv3[v]]].values[0]
        #Find the defName in the def file and replace it with the ith Morris row
        #Fixme: reporting roundTol decimal places here to avoid scientific notation, and avoid loss of precision in atm trans lapse rate
        v3.iloc[:,0][v3.iloc[:,1] == defName] = '%.10f' % MorrisSample_df.iloc[i,Indv3[v]].round(roundTol)

    #Sorting alphabetically to check columns are the same
    checkCols = sorted([(string.split(s=ck, sep='v3_'))[1] for ck in MorrisSample_df.columns[Indv3]])
    if not (checkCols == sorted(veg_grass.iloc[veg_grass.index[((veg_grass.iloc[:,0] != v3.iloc[:,0]) == True)],1].values.tolist())):
        sys.exit('Vegetation grass def file not constructed with values correctly for Replicate = %s' % str(i))
    del ck, checkCols
    
    #Check that the replaced values are the same as the ones in the MorrisSample.
    #Fixme: need to guarantee that the same columns are being compared between the two.
    #Fixme: Long integer with decimals is causing an issue for comparing here. It exceeds Python's comparison limits
    #For now, just checking that the sorted lists are the same manually on one
    checkVals = sorted([str('%.7f' % round(ck,roundTol)) for ck in MorrisSample_df.iloc[i,Indv3]])
    checkVals2 = sorted([str('%.7f' % round(float(ck),roundTol)) for ck in v3.iloc[v3.index[((veg_grass.iloc[:,0] != v3.iloc[:,0]) == True)],0].values])
    if not (checkVals2 == checkVals):
        sys.exit('Vegetation grass def file not constructed with values correctly for Replicate = %s' % str(i))

    #Make edits to other variables that depend on the generated variables
    #Fixme: This substitution should technically happen before rounding in the step above. Practically, it shouldn't matter.
    #epc.ndays_expand = value for tree
    v3.loc[:,0][v3.loc[:,1] == 'epc.ndays_expand'] = v102.loc[:,0][v102.loc[:,1] == 'epc.ndays_expand']
    #epc.ndays_litfall = value for tree
    v3.loc[:,0][v3.loc[:,1] == 'epc.ndays_litfall'] = v102.loc[:,0][v102.loc[:,1] == 'epc.ndays_litfall']
    #epc.gs_threshold_on = value for tree
    v3.loc[:,0][v3.loc[:,1] == 'epc.gs_threshold_on'] = v102.loc[:,0][v102.loc[:,1] == 'epc.gs_threshold_on']
    #epc.gs_threshold_off = value for tree
    v3.loc[:,0][v3.loc[:,1] == 'epc.gs_threshold_off'] = v102.loc[:,0][v102.loc[:,1] == 'epc.gs_threshold_off']
    #epc.day_leafoff = value for tree
    v3.loc[:,0][v3.loc[:,1] == 'epc.day_leafoff'] = v102.loc[:,0][v102.loc[:,1] == 'epc.day_leafoff']
    #epc.day_leafon = value for tree
    v3.loc[:,0][v3.loc[:,1] == 'epc.day_leafon'] = v102.loc[:,0][v102.loc[:,1] == 'epc.day_leafon']
    #epc.min_percent_leafg = Same as epc.leaf_turnover for grass
    v3.loc[:,0][v3.loc[:,1] == 'epc.min_percent_leafg'] = v3.loc[:,0][v3.loc[:,1] == 'epc.leaf_turnover']
    #PAR_transmittance = 1 - PAR_absorptance – PAR_reflectance - round check
    v3.loc[:,0][v3.loc[:,1] == 'PAR_transmittance'] = '%.10f' % (1. - float(v3.loc[:,0][v3.loc[:,1] == 'PAR_absorptance']) - float(v3.loc[:,0][v3.loc[:,1] == 'PAR_reflectance']))
    #K_reflectance = 1 - K_absorptance - K_transmittance - round check
    v3.loc[:,0][v3.loc[:,1] == 'K_reflectance'] = '%.10f' % (1. - float(v3.loc[:,0][v3.loc[:,1] == 'K_absorptance']) - float(v3.loc[:,0][v3.loc[:,1] == 'K_transmittance']))
    
    #Ensure that the sum of these variables = 1 because rounding has now been completed.
    if (float('%.10f' % (float(v3.loc[:,0][v3.loc[:,1] == 'K_reflectance']) + float(v3.loc[:,0][v3.loc[:,1] == 'K_absorptance']) + float(v3.loc[:,0][v3.loc[:,1] == 'K_transmittance']))) != 1.0):
        sys.exit('Vegetation tree K reflectance + K absorptance + K transmittance != 1 for Replicate = %s' % str(i))

    if (float('%.10f' % (float(v3.loc[:,0][v3.loc[:,1] == 'PAR_reflectance']) + float(v3.loc[:,0][v3.loc[:,1] == 'PAR_absorptance']) + float(v3.loc[:,0][v3.loc[:,1] == 'PAR_transmittance']))) != 1.0):
        sys.exit('Vegetation tree PAR reflectance + PAR absorptance + PAR transmittance != 1 for Replicate = %s' % str(i))
        
    #Write new file
    v3.to_csv('stratum_grass.def', sep=' ', index=False, header=False, encoding='utf-8')
    del v3, v102, Indv3, defName, v, ck, checkVals, checkVals2
    
    #v4 - non-vegetation   
    #Edit the land file with the variables generated
    #Make a deep copy of the file so that the original file remains unchanged
    v4 = veg_NonVeg.copy(deep=True)
    #Loop over the MorrisSample_df data for l_. These variables were random and should be changed
    Indv4 = []
    for v in range(len(MorrisSample_df.columns)):
        if string.find(MorrisSample_df.columns[v], 'v4') == 0:
            #Record the index. This variable should be used
            Indv4.append(v)
    del v
    #Now loop over these columns and extract the ProbFile variable name that matches the def file name
    for v in range(len(Indv4)):
        defName = ProbFile.iloc[:,1][ProbFile.iloc[:,0] == MorrisSample_df.columns[Indv4[v]]].values[0]
        #Find the defName in the def file and replace it with the ith Morris row
        #Fixme: reporting roundTol decimal places here to avoid scientific notation, and avoid loss of precision in atm trans lapse rate
        v4.iloc[:,0][v4.iloc[:,1] == defName] = '%.10f' % MorrisSample_df.iloc[i,Indv4[v]].round(roundTol)

    #Sorting alphabetically to check columns are the same
    checkCols = sorted([(string.split(s=ck, sep='v4_'))[1] for ck in MorrisSample_df.columns[Indv4]])
    if not (checkCols == sorted(veg_NonVeg.iloc[veg_NonVeg.index[((veg_NonVeg.iloc[:,0] != v4.iloc[:,0]) == True)],1].values.tolist())):
        sys.exit('Vegetation nonveg def file not constructed with values correctly for Replicate = %s' % str(i))
    del ck, checkCols
    
    #Check that the replaced values are the same as the ones in the MorrisSample.
    #Fixme: need to guarantee that the same columns are being compared between the two.
    #Fixme: Long integer with decimals is causing an issue for comparing here. It exceeds Python's comparison limits
    #For now, just checking that the sorted lists are the same manually on one
    checkVals = sorted([str('%.7f' % round(ck,roundTol)) for ck in MorrisSample_df.iloc[i,Indv4]])
    checkVals2 = sorted([str('%.7f' % round(float(ck),roundTol)) for ck in v4.iloc[v4.index[((veg_NonVeg.iloc[:,0] != v4.iloc[:,0]) == True)],0].values])
    if not (checkVals2 == checkVals):
        sys.exit('Vegetation nonveg def file not constructed with values correctly for Replicate = %s' % str(i))
    
    #Make edits to other variables that depend on the generated variables
    #Fixme: This substitution should technically happen before rounding in the step above. Practically, it shouldn't matter.
    #K_absorptance = 1- K_reflectance- K_transmittance - round check
    v4.loc[:,0][v4.loc[:,1] == 'K_absorptance'] = '%.10f' % (1. - float(v4.loc[:,0][v4.loc[:,1] == 'K_reflectance']) - float(v4.loc[:,0][v4.loc[:,1] == 'K_transmittance']))
    
    #Ensure that the sum of these three variables = 1 because rounding has now been completed.
    if (float('%.10f' % (float(v4.loc[:,0][v4.loc[:,1] == 'K_reflectance']) + float(v4.loc[:,0][v4.loc[:,1] == 'K_absorptance']) + float(v4.loc[:,0][v4.loc[:,1] == 'K_transmittance']))) != 1.0):
        sys.exit('Vegetation nonveg K reflectance + K absorptance + K transmittance != 1 for Replicate = %s' % str(i))
    
    #Write new file
    v4.to_csv('stratum_nonveg.def', sep=' ', index=False, header=False, encoding='utf-8')
    del v4, Indv4, defName, v, ck, checkVals, checkVals2
    
    #Copy the basin def file, too
    basin.to_csv('basin_basin.def', sep=' ', index=False, header=False, encoding='utf-8')
    
    #Change back to od
    os.chdir(od)