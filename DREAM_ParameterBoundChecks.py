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
#2: random seed - should be different for each step in chain. All chains processed at once in this script
#3: directory of def files
#4: round tolerance <= 10
#5: problem file name with extension
#6: chain parameter sample text file name without extension (e.g., 'BaismanChainStarts', 'Chain_1' where 1 is chain iteration)

#%% Set working directory
#os.chdir('/scratch/js4yd/MorrisSA/RHESSysRuns')
#os.chdir('C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs_Calibration\\')
os.chdir(sys.argv[1])

#Import function
#Fixme: may need more functions here
#Fixme: not used in calibration
#from Sum3CheckFun import FullSum3Check

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
#Lower bounds and upper bounds have already been checked for this file.
    
#%%Load in R generated file of proposed chain starting locations. These must be checked for constraints.
Chains_df = pd.read_csv(sys.argv[6]+'.txt', delim_whitespace=True, header='infer')

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
        #Fixme: not useful for calibration? Here and elsewhere in script.
        #Chains_df.loc[:, 'orig_' + ind1 + '_Ksat_0_orig'] = mc
        
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
    #Chains_df.loc[:, 'orig_' + ind + '_Ksat_0_v_orig'] = vKsat
    
    #Go through the trajectories to check the condition.
    compare = vKsat > Ksat
    
    #Get the indices for which this is true
    inds = compare[compare == True].index
    
    #Get the indices of the Morris trajectory starting points. 
    #Changes in these variables will only happen between those points
    trajChange = range(len(Chains_df))
    
    #Loop over the number of trajectories to replace values in the series
    for j in range(len(trajChange)):
        indsTrajPos = inds[(inds == trajChange[j])]
        
        if len(indsTrajPos) > 0:
            #There are values to be adjusted.
            #Change all vKsat to a random value
            Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_Ksat_0_v'].index[0],2], Ksat[indsTrajPos].iloc[0])]*len(vKsat[indsTrajPos])
            vKsat[indsTrajPos] = Val
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
        trajChange = range(len(Chains_df))
    
        #Loop over the number of trajectories to replace values in the series
        for j in range(len(trajChange)):
            indsTrajPos = inds[(inds == trajChange[j])]
            
            if len(indsTrajPos) > 0:
                #There are values to be adjusted
                #Change all mc to a random value
                Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0_v'].index[0],2], mu[indsTrajPos].iloc[0])]*len(mc[indsTrajPos])
                mc[indsTrajPos] = Val
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
        #Chains_df.loc[:, 'orig_' + ind1 + '_m_orig'] = mc
        
        #Go through the trajectories to check the condition.
        compare = mc > mu
    
        #Get the indices for which this is true
        inds = compare[compare == True].index
    
        #Get the indices of the Morris trajectory starting points. 
        #Changes in these variables will only happen between those points.
        trajChange = range(len(Chains_df))
    
        #Loop over the number of trajectories to replace values in the series
        for j in range(len(trajChange)):
            indsTrajPos = inds[(inds == trajChange[j])]
            
            if len(indsTrajPos) > 0:
                #There are values to be adjusted
                #Change all mc to a random value
                Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_m'].index[0],2], mu[indsTrajPos].iloc[0])]*len(mc[indsTrajPos])
                mc[indsTrajPos] = Val
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
        #Chains_df.loc[:, 'orig_' + ind1 + '_porosity_0_orig'] = pc
        
        #Go through the trajectories to check the condition.
        compare = pc > pu
    
        #Get the indices for which this is true
        inds = compare[compare == True].index
    
        #Get the indices of the Morris trajectory starting points. 
        #Changes in these variables will only happen between those points.
        trajChange = range(len(Chains_df))
    
        #Loop over the number of trajectories to replace values in the series
        for j in range(len(trajChange)):
            indsTrajPos = inds[(inds == trajChange[j])]
            
            if len(indsTrajPos) > 0:
                #There are values to be adjusted
                #Change all pc to a random value
                Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_porosity_0'].index[0],2], pu[indsTrajPos].iloc[0])]*len(pc[indsTrajPos])
                pc[indsTrajPos] = Val
                del Val
            del indsTrajPos
        #Save the new column
        Chains_df.loc[:, ind1 + '_porosity_0'] = pc

del compare, i, ind1, ind2, j, pu, pc, trajChange, inds

#Check sums
if sum(Chains_df.loc[:,'s109_porosity_0'] > Chains_df.loc[:,'s9_porosity_0']) != 0:
    sys.exit('PyERROR32: s109 porosity_0 not < s9 porosity_0')
#Fixme: commenting out for now because it's not in calibration parameter set.
#if sum(Chains_df.loc[:,'s108_porosity_0'] > Chains_df.loc[:,'s8_porosity_0']) != 0:
#    sys.exit('PyERROR33: s108 porosity_0 not < s8 porosity_0')

#%% Vegetation

#%% K_absorptance + K_reflectance + K_transmittance = 1 - round check
#The only vegetation for SA that is changed is #102 - trees. Other two are 1 - values assigned randomly.
#Fixme: For the calibration will need a different check because all vars could be adjusted.
#Fixme: not in calibration
#Chains_df = FullSum3Check(Col1='_K_absorptance', Col2='_K_reflectance', Col3='_K_transmittance', MorrisSample_df=Chains_df, N=N, ProbFile=ProbFile, regexNm='102_K_absorptance$', roundTol=roundTol)

#Check sums
#if any((Chains_df.loc[:, 'v102_K_absorptance'] + Chains_df.loc[:, 'v102_K_reflectance'] + Chains_df.loc[:, 'v102_K_transmittance']).round(roundTol) != 1.0000):
#    sys.exit('PyERROR34: Sum of K absorptance, reflectance, transmittance != 1 for v102')

#%% PAR_absorptance + PAR_reflectance + PAR_transmittance = 1 - round check
#The only venetation for SA that is changed is #102 - trees. Other two are 1 - values assigned randomly.
#Fixme: For the calibration will need a different check because all vars could be adjusted.
#Fixme: not in calibration
#Chains_df = FullSum3Check(Col1='_PAR_absorptance', Col2='_PAR_reflectance', Col3='_PAR_transmittance', MorrisSample_df=Chains_df, N=N, ProbFile=ProbFile, regexNm='102_PAR_absorptance$', roundTol=roundTol)

#Check sums
#if any((Chains_df.loc[:, 'v102_PAR_absorptance'] + Chains_df.loc[:, 'v102_PAR_reflectance'] + Chains_df.loc[:, 'v102_PAR_transmittance']).round(roundTol) != 1.0000):
#    sys.exit('PyERROR35: Sum of PAR absorptance, reflectance, transmittance != 1 for v102')

#%% epc.frootlitr_fcel + epc.frootlitr_flab + epc.frootlitr_flig = 1 - round check
#The only venetation for SA that is changed is #102 - trees and #3 - grass. Other is not random.
#Fixme: not in calibration
#Chains_df = FullSum3Check(Col1='_epc.frootlitr_fcel', Col2='_epc.frootlitr_flab', Col3='_epc.frootlitr_flig', MorrisSample_df=Chains_df, N=N, ProbFile=ProbFile, regexNm='epc.frootlitr_fcel$', roundTol=roundTol)

#Check sums
#if any((Chains_df.loc[:, 'v102_epc.frootlitr_fcel'] + Chains_df.loc[:, 'v102_epc.frootlitr_flab'] + Chains_df.loc[:, 'v102_epc.frootlitr_flig']).round(roundTol) != 1.0000):
#    sys.exit('PyERROR36: Sum of frootlitr_fcel, flab, flig != 1 for v102')
#if any((Chains_df.loc[:, 'v3_epc.frootlitr_fcel'] + Chains_df.loc[:, 'v3_epc.frootlitr_flab'] + Chains_df.loc[:, 'v3_epc.frootlitr_flig']).round(roundTol) != 1.0000):
#    sys.exit('PyERROR37: Sum of frootlitr_fcel, flab, flig != 1 for v3')
    
#%% epc.leaflitr_fcel + epc.leaflitr_flab + epc.leaflitr_flig = 1 - round check
#The only venetation for SA that is changed is #102 - trees and #3 - grass. Other is not random.
#Fixme: not in calibration
#Chains_df = FullSum3Check(Col1='_epc.leaflitr_fcel', Col2='_epc.leaflitr_flab', Col3='_epc.leaflitr_flig', MorrisSample_df=Chains_df, N=N, ProbFile=ProbFile, regexNm='epc.leaflitr_fcel$', roundTol=roundTol)

#Check sums
#if any((Chains_df.loc[:, 'v102_epc.leaflitr_fcel'] + Chains_df.loc[:, 'v102_epc.leaflitr_flab'] + Chains_df.loc[:, 'v102_epc.leaflitr_flig']).round(roundTol) != 1.0000):
#    sys.exit('PyERROR38: Sum of leaflitr_fcel, flab, flig != 1 for v102')
#if any((Chains_df.loc[:, 'v3_epc.leaflitr_fcel'] + Chains_df.loc[:, 'v3_epc.leaflitr_flab'] + Chains_df.loc[:, 'v3_epc.leaflitr_flig']).round(roundTol) != 1.0000):
#    sys.exit('PyERROR39: Sum of leaflitr_fcel, flab, flig != 1 for v3')
#%% epc.topt <= epc.tmax
#If epc.tmax is not greater, simulate a random value less than epc.tmax for epc.topt
#Fixme: not in calibration
#for i in range(len(Chains_df.filter(regex='epc.topt$').columns)):
#    #Find the indicator for the soil type
#    #string = Get all columns containing epc.tmax
#    ind = re.split(string=Chains_df.filter(regex='epc.topt$').columns[i], pattern='_')[0]
#    #Use the indicator to extract the epc.topt, and epc.tmax columns
#    Ksat = Chains_df.loc[:, ind + '_epc.tmax']
#    vKsat = Chains_df.loc[:, ind + '_epc.topt']
#    
#    #Save the original column.
#    Chains_df.loc[:, 'orig_' + ind + '_epc.topt_orig'] = vKsat
#    
#    #Go through the trajectories to check the condition.
#    compare = vKsat > Ksat
#    
#    #Get the indices for which this is true
#    inds = compare[compare == True].index
#    
#    #Get the indices of the Morris trajectory starting points. 
#    #Changes in these variables will only happen between those points
#    trajChange = range(len(Chains_df))
#    
#    #Loop over the number of trajectories to replace values in the series
#    for j in range(len(trajChange)):
#        indsTrajPos = inds[(inds == trajChange[j])]
#        
#        if len(indsTrajPos) > 0:
#            #There are values to be adjusted.
#            #Change all vKsat to a random value
#            Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.topt'].index[0],2], Ksat[indsTrajPos].iloc[0])]*len(vKsat[indsTrajPos])
#            vKsat[indsTrajPos] = Val
#            del Val
#        del indsTrajPos
#    #Save the new column
#    Chains_df.loc[:, ind + '_epc.topt'] = vKsat
#
#del compare, i, ind, j, vKsat, Ksat, trajChange, inds
#
##Check sums
#if sum(Chains_df.loc[:,'v102_epc.topt'] > Chains_df.loc[:,'v102_epc.tmax']) != 0:
#    sys.exit('PyERROR40: topt not < tmax for v102')
#if sum(Chains_df.loc[:,'v3_epc.topt'] > Chains_df.loc[:,'v3_epc.tmax']) != 0:
#    sys.exit('PyERROR41: topt not < tmax for v3')

#%% Leaf litter CN >= leaf CN
#Fixme: not in calibration
#If epc.leaflitr_cn is not greater, simulate a random value less than epc.leaflitr_cn for epc.leaf_cn
#for i in range(len(Chains_df.filter(regex='epc.leaf_cn$').columns)):
#    #Find the indicator for the soil type
#    #string = Get all columns containing epc.leaflitr_cn
#    ind = re.split(string=Chains_df.filter(regex='epc.leaf_cn$').columns[i], pattern='_')[0]
#    #Use the indicator to extract the epc.leaf_cn, and epc.leaflitr_cn columns
#    Ksat = Chains_df.loc[:, ind + '_epc.leaflitr_cn']
#    vKsat = Chains_df.loc[:, ind + '_epc.leaf_cn']
#    
#    #Save the original column.
#    Chains_df.loc[:, 'orig_' + ind + '_epc.leaf_cn_orig'] = vKsat
#    
#    #Go through the trajectories to check the condition.
#    compare = vKsat > Ksat
#    
#    #Get the indices for which this is true
#    inds = compare[compare == True].index
#    
#    #Get the indices of the Morris trajectory starting points. 
#    #Changes in these variables will only happen between those points
#    trajChange = range(len(Chains_df))
#    
#    #Loop over the number of trajectories to replace values in the series
#    for j in range(len(trajChange)):
#        indsTrajPos = inds[(inds == trajChange[j])]
#        
#        if len(indsTrajPos) > 0:
#            #Change all vKsat to a random value
#            Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaf_cn'].index[0],2], Ksat[indsTrajPos].iloc[0])]*len(vKsat[indsTrajPos])
#            vKsat[indsTrajPos] = Val
#            del Val
#        del indsTrajPos
#    #Save the new column
#    Chains_df.loc[:, ind + '_epc.leaf_cn'] = vKsat
#
#del compare, i, ind, j, vKsat, Ksat, trajChange, inds
#
##Check sums
#if sum(Chains_df.loc[:,'v102_epc.leaf_cn'] > Chains_df.loc[:,'v102_epc.leaflitr_cn']) != 0:
#    sys.exit('PyERROR42: leaf_cn not < leaflitr_cn for v102')
#if sum(Chains_df.loc[:,'v3_epc.leaf_cn'] > Chains_df.loc[:,'v3_epc.leaflitr_cn']) != 0:
#    sys.exit('PyERROR43: leaf_cn not < leaflitr_cn for v3')

#%% Check sums for all parameters once more to make sure nothing odd happened
#Fixme: not in calibration
#if (any((Chains_df.loc[:, 's8_silt'] + Chains_df.loc[:, 's8_sand'] + Chains_df.loc[:, 's8_clay']).round(roundTol) != 1.0000) | \
#any((Chains_df.loc[:, 's108_silt'] + Chains_df.loc[:, 's108_sand'] + Chains_df.loc[:, 's108_clay']).round(roundTol) != 1.0000) | \
#any((Chains_df.loc[:, 's9_silt'] + Chains_df.loc[:, 's9_sand'] + Chains_df.loc[:, 's9_clay']).round(roundTol) != 1.0000) | \
#any((Chains_df.loc[:, 's109_silt'] + Chains_df.loc[:, 's109_sand'] + Chains_df.loc[:, 's109_clay']).round(roundTol) != 1.0000) | \
#any((Chains_df.loc[:, 'v102_K_absorptance'] + Chains_df.loc[:, 'v102_K_reflectance'] + Chains_df.loc[:, 'v102_K_transmittance']).round(roundTol) != 1.0000) | \
#any((Chains_df.loc[:, 'v102_PAR_absorptance'] + Chains_df.loc[:, 'v102_PAR_reflectance'] + Chains_df.loc[:, 'v102_PAR_transmittance']).round(roundTol) != 1.0000) | \
#any((Chains_df.loc[:, 'v102_epc.frootlitr_fcel'] + Chains_df.loc[:, 'v102_epc.frootlitr_flab'] + Chains_df.loc[:, 'v102_epc.frootlitr_flig']).round(roundTol) != 1.0000) | \
#any((Chains_df.loc[:, 'v3_epc.frootlitr_fcel'] + Chains_df.loc[:, 'v3_epc.frootlitr_flab'] + Chains_df.loc[:, 'v3_epc.frootlitr_flig']).round(roundTol) != 1.0000) | \
#any((Chains_df.loc[:, 'v102_epc.leaflitr_fcel'] + Chains_df.loc[:, 'v102_epc.leaflitr_flab'] + Chains_df.loc[:, 'v102_epc.leaflitr_flig']).round(roundTol) != 1.0000) | \
#any((Chains_df.loc[:, 'v3_epc.leaflitr_fcel'] + Chains_df.loc[:, 'v3_epc.leaflitr_flab'] + Chains_df.loc[:, 'v3_epc.leaflitr_flig']).round(roundTol) != 1.0000)):
#    sys.exit('PyERROR44: One of the sums used to be correct, but after other function manipulations is now incorrect. This is a function bug that should be reported.')

#Fixme: not in calibration. Add these to sum below.
#sum(Chains_df.loc[:,'s108_porosity_0'] > Chains_df.loc[:,'s8_porosity_0']) + \
#sum(Chains_df.loc[:,'v102_epc.topt'] > Chains_df.loc[:,'v102_epc.tmax']) + \
#sum(Chains_df.loc[:,'v3_epc.topt'] > Chains_df.loc[:,'v3_epc.tmax']) + \
#sum(Chains_df.loc[:,'v102_epc.leaf_cn'] > Chains_df.loc[:,'v102_epc.leaflitr_cn']) + \
#sum(Chains_df.loc[:,'v3_epc.leaf_cn'] > Chains_df.loc[:,'v3_epc.leaflitr_cn'])
    
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
sum(Chains_df.loc[:,'s109_porosity_0'] > Chains_df.loc[:,'s9_porosity_0'])) != 0:
    sys.exit('PyERROR45: One of the inequalities used to be correct, but after other function manipulations is now incorrect. This is a function bug that should be reported.')

#%% All constraints have passed.
#Write the resulting Chains_df to a csv file
Chains_df.round(roundTol).to_csv(sys.argv[6] + '_AfterProcessing.csv', index = False)  