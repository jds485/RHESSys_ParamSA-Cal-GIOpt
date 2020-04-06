# -*- coding: utf-8 -*-
"""
Created on Fri Oct 25 15:17:24 2019

@author: Jared D. Smith, js4yd@virginia.edu
Script for checking that the output locations from Morris SA samples 
are valid within the model parameter constraints.

Developed in Python 2.7.15. It may work in other versions.
"""

#Currently not using SALib for SA.
#import SALib
#from SALib import sample
#from SALib.sample import morris
#from SALib.util import read_param_file
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
#2: random seed
#3: number of trajectories
#4: round tolerance <= 10
#5: problem file name

#%% Set working directory
#os.chdir('/scratch/js4yd/MorrisSA/RHESSysRuns')
os.chdir(sys.argv[1])

#Import function
from Sum3CheckFun import FullSum3Check

#Set the random seed for randomly generated values
#rd.seed(1349)
rd.seed(int(sys.argv[2]))

#%% Set Morris sampling parameters
#number of sampling trajectories
#N = 40
N = int(sys.argv[3])

#p to determine the grid levels for SALib 
#num_levels = 100
#Set rounding precision as number of decimal places. Using 10 for lack of a sigfig function in Py 2.7.x
#Note: There are still statements of '%.10f' for writing def files, and '%.10f' and '%.7f' for comparing values
#so rounding to more than 10 decimal places will cut to 10 in written files.
#This can result in sums of variables not equal to exactly 1. Best to use <= 10 decimal places for now.
#Also note that round() and pandas .round() give different results when placeholder 0s are needed.
#Oddly, both do not add 0s. Rather, round() adds 0.0000000025, and .round adds 0.0000000099.
#roundTol = 10
roundTol = int(sys.argv[4])

#%% Load problem file for SALib sampling - NOTE - Morris in SALib is currently 
#not generating sampling locations correctly. Using R to generate the model 
#run locations instead.

#ProbFile = pd.read_csv('BaismanMorrisSamplingProblemFile_Full_Feb2020.csv')
ProbFile = pd.read_csv(sys.argv[5])

#Check that the lower bounds are all less than the upper bounds
if not all(ProbFile.iloc[:,2] < ProbFile.iloc[:,3]):
    sys.exit('PyERROR1: For parameters, all lower bounds are not less than the upper bounds')

#Check lower bounds for variables that are set in code
if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's108_Ksat_0_v'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's108_Ksat_0'].index[0],2]):
    sys.exit('PyERROR2: Lower bound of soil 108 vertical Ksat is not less than the lower bound of soil 108 Ksat. Either increase lower bound of Ksat or decrease lower bound of vertical Ksat. The change must be greater than rounding tolerance.')

if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's8_Ksat_0_v'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's8_Ksat_0'].index[0],2]):
    sys.exit('PyERROR3: Lower bound of soil 8 vertical Ksat is not less than the lower bound of soil 8 Ksat. Either increase lower bound of Ksat or decrease lower bound of vertical Ksat. The change must be greater than rounding tolerance.')

if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's109_Ksat_0_v'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's109_Ksat_0'].index[0],2]):
    sys.exit('PyERROR4: Lower bound of soil 109 vertical Ksat is not less than the lower bound of soil 109 Ksat. Either increase lower bound of Ksat or decrease lower bound of vertical Ksat. The change must be greater than rounding tolerance.')

if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's9_Ksat_0_v'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's9_Ksat_0'].index[0],2]):
    sys.exit('PyERROR5: Lower bound of soil 9 vertical Ksat is not less than the lower bound of soil 9 Ksat. Either increase lower bound of Ksat or decrease lower bound of vertical Ksat. The change must be greater than rounding tolerance.')

if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's109_m'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's9_m'].index[0],2]):
    sys.exit('PyERROR6: Lower bound of soil 109 m is not less than the lower bound of soil 9 m. Either increase lower bound of s9 m or decrease lower bound of s109 m. The change must be greater than rounding tolerance.')

if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's108_m'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's8_m'].index[0],2]):
    sys.exit('PyERROR7: Lower bound of soil 108 m is not less than the lower bound of soil 8 m. Either increase lower bound of s8 m or decrease lower bound of s108 m. The change must be greater than rounding tolerance.')

if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's109_porosity_0'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's9_porosity_0'].index[0],2]):
    sys.exit('PyERROR8: Lower bound of soil 109 porosity_0 is not less than the lower bound of soil 9 porosity_0. Either increase lower bound of s9 porosity_0 or decrease lower bound of s109 porosity_0. The change must be greater than rounding tolerance.')

if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's108_porosity_0'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's8_porosity_0'].index[0],2]):
    sys.exit('PyERROR9: Lower bound of soil 108 porosity_0 is not less than the lower bound of soil 8 porosity_0. Either increase lower bound of s8 porosity_0 or decrease lower bound of s108 porosity_0. The change must be greater than rounding tolerance.')

if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's109_Ksat_0'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's9_Ksat_0'].index[0],2]):
    sys.exit('PyERROR10: Lower bound of soil 109 Ksat_0 is not less than the lower bound of soil 9 Ksat_0. Either increase lower bound of s9 Ksat_0 or decrease lower bound of s109 Ksat_0. The change must be greater than rounding tolerance.')

if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's108_Ksat_0'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's8_Ksat_0'].index[0],2]):
    sys.exit('PyERROR11: Lower bound of soil 108 Ksat_0 is not less than the lower bound of soil 8 Ksat_0. Either increase lower bound of s8 Ksat_0 or decrease lower bound of s108 Ksat_0. The change must be greater than rounding tolerance.')

if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's109_Ksat_0_v'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's8_Ksat_0_v'].index[0],2]):
    sys.exit('PyERROR12: Lower bound of soil 109 Ksat_0_v is not less than the lower bound of soil 9 Ksat_0_v. Either increase lower bound of s8 Ksat_0_v or decrease lower bound of s108 Ksat_0_v. The change must be greater than rounding tolerance.')

if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's108_Ksat_0_v'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 's8_Ksat_0_v'].index[0],2]):
    sys.exit('PyERROR13: Lower bound of soil 108 Ksat_0_v is not less than the lower bound of soil 8 Ksat_0_v. Either increase lower bound of s8 Ksat_0_v or decrease lower bound of s108 Ksat_0_v. The change must be greater than rounding tolerance.')

if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 'v102_epc.topt'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 'v102_epc.tmax'].index[0],2]):
    sys.exit('PyERROR14: Lower bound of veg 102 epc.topt is not less than the lower bound of veg 102 epc.tmax. Either increase lower bound of epc.tmax or decrease lower bound of epc.topt. The change must be greater than rounding tolerance.')

if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 'v3_epc.topt'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 'v3_epc.tmax'].index[0],2]):
    sys.exit('PyERROR15: Lower bound of veg 3 epc.topt is not less than the lower bound of veg 3 epc.tmax. Either increase lower bound of epc.tmax or decrease lower bound of epc.topt. The change must be greater than rounding tolerance.')

if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 'v102_epc.leaf_cn'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 'v102_epc.leaflitr_cn'].index[0],2]):
    sys.exit('PyERROR16: Lower bound of veg 102 epc.leaf_cn is not less than the lower bound of veg 102 epc.leaflitr_cn. Either increase lower bound of epc.leaflitr_cn or decrease lower bound of epc.leaf_cn. The change must be greater than rounding tolerance.')

if not (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 'v3_epc.leaf_cn'].index[0],2] < ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == 'v3_epc.leaflitr_cn'].index[0],2]):
    sys.exit('PyERROR17: Lower bound of veg 3 epc.leaf_cn is not less than the lower bound of veg 3 epc.leaflitr_cn. Either increase lower bound of epc.leaflitr_cn or decrease lower bound of epc.leaf_cn. The change must be greater than rounding tolerance.')
    
#Save that file into a new file with no headers
#ProbFile.drop('DefParameter', axis = 1).to_csv('BaismanMorrisSamplingProblemFile_ForSA.csv', header=False, index=False) 

#use the SALib function to read that file
#SAProbFile = read_param_file('BaismanMorrisSamplingProblemFile_ForSA.csv')

#use the SALib function to sample Morris locations
#MorrisSample = SALib.sample.morris.sample(problem=SAProbFile, N = N, num_levels=num_levels, optimal_trajectories=None, local_optimization=True)

#Make this a pandas dataframe
#MorrisSample_df = pd.DataFrame(MorrisSample)
#MorrisSample_df.columns = ProbFile.iloc[:,0]

#Load in R file of proposed sampling locations. These must be checked for constraints
MorrisSample_df = pd.read_csv('MorrisSamples_BeforeProcessing.csv')

#%%Check the parameter constraints for soils and vegetation
#Fixme: should also check that the change in any one parameter is within the 
#step change of the Morris delta on that parameter

#%% Soil: sand + silt + clay = 1

MorrisSample_df = FullSum3Check(Col1='_silt', Col2='_sand', Col3='_clay', MorrisSample_df=MorrisSample_df, N=N, ProbFile=ProbFile, regexNm='silt$', roundTol=roundTol)

#Check sums
#Fixme: can these autodetect parameter names to compare?
if any((MorrisSample_df.loc[:, 's8_silt'] + MorrisSample_df.loc[:, 's8_sand'] + MorrisSample_df.loc[:, 's8_clay']).round(roundTol) != 1.0000):
    sys.exit('PyERROR18: Sum of sand + silt + clay != 1 for s8')
if any((MorrisSample_df.loc[:, 's108_silt'] + MorrisSample_df.loc[:, 's108_sand'] + MorrisSample_df.loc[:, 's108_clay']).round(roundTol) != 1.0000):
    sys.exit('PyERROR19: Sum of sand + silt + clay != 1 for s108')
if any((MorrisSample_df.loc[:, 's9_silt'] + MorrisSample_df.loc[:, 's9_sand'] + MorrisSample_df.loc[:, 's9_clay']).round(roundTol) != 1.0000):
    sys.exit('PyERROR20: Sum of sand + silt + clay != 1 for s9')
if any((MorrisSample_df.loc[:, 's109_silt'] + MorrisSample_df.loc[:, 's109_sand'] + MorrisSample_df.loc[:, 's109_clay']).round(roundTol) != 1.0000):
    sys.exit('PyERROR21: Sum of sand + silt + clay != 1 for s109')

#%%
#4 parameters for compacted soil must be <= values for uncompacted soil
#m, porosity_0, Ksat_0, Ksat_0_v

#Additionally, values for Ksat_0_v must be less than Ksat_0 for each soil type

#These items are accomplished in the following sections

#%% Order is important for Ksat and vKsat. 
#Must first adjust Ksat of compacted to be less than Ksat of uncompacted.
#If it is not greater, simulate a random value less than Ksatu for Ksatc
for i in range(len(MorrisSample_df.filter(regex='_Ksat_0$').columns)):
    #Find the indicator for the first soil type
    #string = Get all columns containing _Ksat_0
    ind1 = re.split(string=MorrisSample_df.filter(regex='_Ksat_0$').columns[i], pattern='_')[0]
    
    #Only work on the columns that have a 0, and find the other corresponding index
    if len(re.split(string=ind1, pattern='0')) == 2:
        ind2 = string.join(re.split(string=ind1, pattern='10'), sep='')
        #Use the indicator to extract the Ksat compacted, and Ksat uncompacted columns
        mc = MorrisSample_df.loc[:, ind1 + '_Ksat_0']
        mu = MorrisSample_df.loc[:, ind2 + '_Ksat_0']
        
        #Save the original column
        MorrisSample_df.loc[:, 'orig_' + ind1 + '_Ksat_0_orig'] = mc
        
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
                    Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0'].index[0],2], mu[indsTrajPos].iloc[0])]*len(mc[indsTrajPos])
                    mc[indsTrajPos] = Val
                elif (len(mu[indsTrajPos].unique()) == 1) | (len(mc[indsTrajPos].unique()) == 1):
                    #There is one change in this trajectory for one, but not the other variable
                    if (len(mu[indsTrajPos].unique()) == 1):
                        #mc changes
                        indmcChange = mc[indsTrajPos][mc[indsTrajPos] != mc[indsTrajPos].iloc[0]].index[0]
                        
                        #Only 1 change in the trajectory. Change to random value less than mu and greater than lower bound
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0'].index[0],2], mu[indsTrajPos].iloc[0])]*len(mc[indsTrajPos[indsTrajPos < indmcChange]])
                        mc[indsTrajPos[indsTrajPos < indmcChange]] = Val
                    
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0'].index[0],2], mu[indsTrajPos].iloc[0])]*len(mc[indsTrajPos[indsTrajPos >= indmcChange]])
                        mc[indsTrajPos[indsTrajPos >= indmcChange]] = Val
                        
                        del indmcChange
                    else:
                        #mu changes
                        indmuChange = mu[indsTrajPos][mu[indsTrajPos] != mu[indsTrajPos].iloc[0]].index[0]
                        
                        #Only 1 change in the trajectory. Change to random value less than mu and greater than lower bound
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0'].index[0],2], mu[indsTrajPos[indsTrajPos < indmuChange]].iloc[0])]*len(mc[indsTrajPos[indsTrajPos < indmuChange]])
                        mc[indsTrajPos[indsTrajPos < indmuChange]] = Val
                    
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0'].index[0],2], mu[indsTrajPos[indsTrajPos >= indmuChange]].iloc[0])]*len(mc[indsTrajPos[indsTrajPos >= indmuChange]])
                        mc[indsTrajPos[indsTrajPos >= indmuChange]] = Val
                        
                        del indmuChange
                else:
                    #within these positions, mc and mu change each exactly once. Find when those happen.
                    indmcChange = mc[indsTrajPos][mc[indsTrajPos] != mc[indsTrajPos].iloc[0]].index[0]
                    indmuChange = mu[indsTrajPos][mu[indsTrajPos] != mu[indsTrajPos].iloc[0]].index[0]
                    if indmuChange == indmcChange:
                        #Only 1 change in the trajectory. Change to random value less than mu and greater than lower bound
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0'].index[0],2], mu[indsTrajPos[indsTrajPos < indmuChange]].iloc[0])]*len(mc[indsTrajPos[indsTrajPos < indmuChange]])
                        mc[indsTrajPos[indsTrajPos < indmuChange]] = Val
                    
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0'].index[0],2], mu[indsTrajPos[indsTrajPos >= indmuChange]].iloc[0])]*len(mc[indsTrajPos[indsTrajPos >= indmuChange]])
                        mc[indsTrajPos[indsTrajPos >= indmuChange]] = Val
                    elif indmuChange > indmcChange:
                        #Work from mc index first
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0'].index[0],2], mu[indsTrajPos[indsTrajPos < indmcChange]].iloc[0])]*len(mc[indsTrajPos[indsTrajPos < indmcChange]])
                        mc[indsTrajPos[indsTrajPos < indmcChange]] = Val
                    
                        #Now check the indices between indmcChange and indmuChange
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0'].index[0],2], mu[indsTrajPos[(indsTrajPos < indmuChange) & (indsTrajPos >= indmcChange)]].iloc[0])]*len(mc[indsTrajPos[(indsTrajPos < indmuChange) & (indsTrajPos >= indmcChange)]])
                        mc[indsTrajPos[(indsTrajPos < indmuChange) & (indsTrajPos >= indmcChange)]] = Val
                    
                        #Now check the indices greater than or equal to indmuChange
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0'].index[0],2], mu[indsTrajPos[(indsTrajPos >= indmuChange)]].iloc[0])]*len(mc[indsTrajPos[(indsTrajPos >= indmuChange)]])
                        mc[indsTrajPos[(indsTrajPos >= indmuChange)]] = Val
                    else:
                        #Work from mu index first
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0'].index[0],2], mu[indsTrajPos[indsTrajPos < indmuChange]].iloc[0])]*len(mc[indsTrajPos[indsTrajPos < indmuChange]])
                        mc[indsTrajPos[indsTrajPos < indmuChange]] = Val
                    
                        #Now check the indices between indmuChange and indmcChange
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0'].index[0],2], mu[indsTrajPos[(indsTrajPos < indmcChange) & (indsTrajPos >= indmuChange)]].iloc[0])]*len(mc[indsTrajPos[(indsTrajPos < indmcChange) & (indsTrajPos >= indmuChange)]])
                        mc[indsTrajPos[(indsTrajPos < indmcChange) & (indsTrajPos >= indmuChange)]] = Val
                    
                        #Now check the indices greater than or equal to indmcChange
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0'].index[0],2], mu[indsTrajPos[(indsTrajPos >= indmcChange)]].iloc[0])]*len(mc[indsTrajPos[(indsTrajPos >= indmcChange)]])
                        mc[indsTrajPos[(indsTrajPos >= indmcChange)]] = Val
                        
                    del indmcChange, indmuChange
                del Val
            del indsTrajPos
    
        #Save the new column
        MorrisSample_df.loc[:, ind1 + '_Ksat_0'] = mc

del compare, i, ind1, ind2, j, mu, mc, trajChange, inds

#Check sums
if sum(MorrisSample_df.loc[:,'s109_Ksat_0'] > MorrisSample_df.loc[:,'s9_Ksat_0']) != 0:
    sys.exit('PyERROR22: s109 Ksat not < s9 Ksat')
if sum(MorrisSample_df.loc[:,'s108_Ksat_0'] > MorrisSample_df.loc[:,'s8_Ksat_0']) != 0:
    sys.exit('PyERROR23: s108 Ksat not < s8 Ksat')
    
#%% Next make sure that vertical Ksat < Ksat for each soil
#Ksat_0_v < Ksat_0
#If Ksat_0 is not greater, simulate a random value less than Ksat_0 for Ksat_0_v
for i in range(len(MorrisSample_df.filter(regex='Ksat_0_v$').columns)):
    #Find the indicator for the soil type
    #string = Get all columns containing Ksat_0_v
    ind = re.split(string=MorrisSample_df.filter(regex='Ksat_0_v$').columns[i], pattern='_')[0]
    #Use the indicator to extract the Ksat_0, and Ksat_0_v columns
    Ksat = MorrisSample_df.loc[:, ind + '_Ksat_0']
    vKsat = MorrisSample_df.loc[:, ind + '_Ksat_0_v']
    
    #Save the original column.
    MorrisSample_df.loc[:, 'orig_' + ind + '_Ksat_0_v_orig'] = vKsat
    
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
    MorrisSample_df.loc[:, ind + '_Ksat_0_v'] = vKsat

del compare, i, ind, j, vKsat, Ksat, trajChange, inds

#Check sums
if sum(MorrisSample_df.loc[:,'s109_Ksat_0_v'] > MorrisSample_df.loc[:,'s109_Ksat_0']) != 0:
    sys.exit('PyERROR24: s109 vKsat not < s109 Ksat')
if sum(MorrisSample_df.loc[:,'s9_Ksat_0_v'] > MorrisSample_df.loc[:,'s9_Ksat_0']) != 0:
    sys.exit('PyERROR25: s9 vKsat not < s9 Ksat')
if sum(MorrisSample_df.loc[:,'s108_Ksat_0_v'] > MorrisSample_df.loc[:,'s108_Ksat_0']) != 0:
    sys.exit('PyERROR26: s108 vKsat not < s108 Ksat')
if sum(MorrisSample_df.loc[:,'s8_Ksat_0_v'] > MorrisSample_df.loc[:,'s8_Ksat_0']) != 0:
    sys.exit('PyERROR27: s8 vKsat not < s8 Ksat')

#%%
#Finally, make sure that vKsat compacted < vKsat uncompacted
#If it is not greater, simulate a random value less than Ksatu for Ksatc
for i in range(len(MorrisSample_df.filter(regex='_Ksat_0_v$').columns)):
    #Find the indicator for the first soil type
    #string = Get all columns containing Ksat_0_v
    ind1 = re.split(string=MorrisSample_df.filter(regex='_Ksat_0_v$').columns[i], pattern='_')[0]
    
    #Only work on the columns that have a 0, and find the other corresponding index
    if len(re.split(string=ind1, pattern='0')) == 2:
        ind2 = string.join(re.split(string=ind1, pattern='10'), sep='')
        #Use the indicator to extract the m compacted, and m uncompacted columns
        mc = MorrisSample_df.loc[:, ind1 + '_Ksat_0_v']
        mu = MorrisSample_df.loc[:, ind2 + '_Ksat_0_v']
        
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
if sum(MorrisSample_df.loc[:,'s109_Ksat_0_v'] > MorrisSample_df.loc[:,'s9_Ksat_0_v']) != 0:
    sys.exit('PyERROR28: s109 vKsat not < s9 Ksat')
if sum(MorrisSample_df.loc[:,'s108_Ksat_0_v'] > MorrisSample_df.loc[:,'s8_Ksat_0_v']) != 0:
    sys.exit('PyERROR29: s108 vKsat not < s8 Ksat')

#%%
#m uncompacted >= m compacted for each soil type:
#If it is not greater, simulate a random value less than mu for mc
for i in range(len(MorrisSample_df.filter(regex='_m$').columns)):
    #Find the indicator for the first soil type
    #string = Get all columns containing m
    ind1 = re.split(string=MorrisSample_df.filter(regex='_m$').columns[i], pattern='_')[0]
    
    #Only work on the columns that have a 0, and find the other corresponding index
    if len(re.split(string=ind1, pattern='0')) == 2:
        ind2 = string.join(re.split(string=ind1, pattern='10'), sep='')
        #Use the indicator to extract the m compacted, and m uncompacted columns
        mc = MorrisSample_df.loc[:, ind1 + '_m']
        mu = MorrisSample_df.loc[:, ind2 + '_m']
        
        #Save the original column
        MorrisSample_df.loc[:, 'orig_' + ind1 + '_m_orig'] = mc
        
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
        MorrisSample_df.loc[:, ind1 + '_m'] = mc

del compare, i, ind1, ind2, j, mu, mc, trajChange, inds

#Check sums
if sum(MorrisSample_df.loc[:,'s109_m'] > MorrisSample_df.loc[:,'s9_m']) != 0:
    sys.exit('PyERROR30: s109 m not < s9 m')
if sum(MorrisSample_df.loc[:,'s108_m'] > MorrisSample_df.loc[:,'s8_m']) != 0:
    sys.exit('PyERROR31: s108 m not < s8 m')
    
#%% Porosity
#porosity uncompacted >= porosity compacted for each soil type:
#If it is not greater, simulate a random value less than pu for pc
for i in range(len(MorrisSample_df.filter(regex='_porosity_0$').columns)):
    #Find the indicator for the first soil type
    #string = Get all columns containing porosity_0
    ind1 = re.split(string=MorrisSample_df.filter(regex='_porosity_0$').columns[i], pattern='_')[0]
    
    #Only work on the columns that have a 0, and find the other corresponding index
    if len(re.split(string=ind1, pattern='0')) == 2:
        ind2 = string.join(re.split(string=ind1, pattern='10'), sep='')
        #Use the indicator to extract the m compacted, and m uncompacted columns
        pc = MorrisSample_df.loc[:, ind1 + '_porosity_0']
        pu = MorrisSample_df.loc[:, ind2 + '_porosity_0']
        
        #Save the original column
        MorrisSample_df.loc[:, 'orig_' + ind1 + '_porosity_0_orig'] = pc
        
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
        MorrisSample_df.loc[:, ind1 + '_porosity_0'] = pc

del compare, i, ind1, ind2, j, pu, pc, trajChange, inds

#Check sums
if sum(MorrisSample_df.loc[:,'s109_porosity_0'] > MorrisSample_df.loc[:,'s9_porosity_0']) != 0:
    sys.exit('PyERROR32: s109 porosity_0 not < s9 porosity_0')
if sum(MorrisSample_df.loc[:,'s108_porosity_0'] > MorrisSample_df.loc[:,'s8_porosity_0']) != 0:
    sys.exit('PyERROR33: s108 porosity_0 not < s8 porosity_0')

#%% Vegetation

#%% K_absorptance + K_reflectance + K_transmittance = 1 - round check
#The only vegetation for SA that is changed is #102 - trees. Other two are 1 - values assigned randomly.
#Fixme: For the calibration will need a different check because all vars could be adjusted.
MorrisSample_df = FullSum3Check(Col1='_K_absorptance', Col2='_K_reflectance', Col3='_K_transmittance', MorrisSample_df=MorrisSample_df, N=N, ProbFile=ProbFile, regexNm='102_K_absorptance$', roundTol=roundTol)

#Check sums
if any((MorrisSample_df.loc[:, 'v102_K_absorptance'] + MorrisSample_df.loc[:, 'v102_K_reflectance'] + MorrisSample_df.loc[:, 'v102_K_transmittance']).round(roundTol) != 1.0000):
    sys.exit('PyERROR34: Sum of K absorptance, reflectance, transmittance != 1 for v102')

#%% PAR_absorptance + PAR_reflectance + PAR_transmittance = 1 - round check
#The only venetation for SA that is changed is #102 - trees. Other two are 1 - values assigned randomly.
#Fixme: For the calibration will need a different check because all vars could be adjusted.
MorrisSample_df = FullSum3Check(Col1='_PAR_absorptance', Col2='_PAR_reflectance', Col3='_PAR_transmittance', MorrisSample_df=MorrisSample_df, N=N, ProbFile=ProbFile, regexNm='102_PAR_absorptance$', roundTol=roundTol)

#Check sums
if any((MorrisSample_df.loc[:, 'v102_PAR_absorptance'] + MorrisSample_df.loc[:, 'v102_PAR_reflectance'] + MorrisSample_df.loc[:, 'v102_PAR_transmittance']).round(roundTol) != 1.0000):
    sys.exit('PyERROR35: Sum of PAR absorptance, reflectance, transmittance != 1 for v102')

#%% epc.frootlitr_fcel + epc.frootlitr_flab + epc.frootlitr_flig = 1 - round check
#The only venetation for SA that is changed is #102 - trees and #3 - grass. Other is not random.
MorrisSample_df = FullSum3Check(Col1='_epc.frootlitr_fcel', Col2='_epc.frootlitr_flab', Col3='_epc.frootlitr_flig', MorrisSample_df=MorrisSample_df, N=N, ProbFile=ProbFile, regexNm='epc.frootlitr_fcel$', roundTol=roundTol)

#Check sums
if any((MorrisSample_df.loc[:, 'v102_epc.frootlitr_fcel'] + MorrisSample_df.loc[:, 'v102_epc.frootlitr_flab'] + MorrisSample_df.loc[:, 'v102_epc.frootlitr_flig']).round(roundTol) != 1.0000):
    sys.exit('PyERROR36: Sum of frootlitr_fcel, flab, flig != 1 for v102')
if any((MorrisSample_df.loc[:, 'v3_epc.frootlitr_fcel'] + MorrisSample_df.loc[:, 'v3_epc.frootlitr_flab'] + MorrisSample_df.loc[:, 'v3_epc.frootlitr_flig']).round(roundTol) != 1.0000):
    sys.exit('PyERROR37: Sum of frootlitr_fcel, flab, flig != 1 for v3')
    
#%% epc.leaflitr_fcel + epc.leaflitr_flab + epc.leaflitr_flig = 1 - round check
#The only venetation for SA that is changed is #102 - trees and #3 - grass. Other is not random.
MorrisSample_df = FullSum3Check(Col1='_epc.leaflitr_fcel', Col2='_epc.leaflitr_flab', Col3='_epc.leaflitr_flig', MorrisSample_df=MorrisSample_df, N=N, ProbFile=ProbFile, regexNm='epc.leaflitr_fcel$', roundTol=roundTol)

#Check sums
if any((MorrisSample_df.loc[:, 'v102_epc.leaflitr_fcel'] + MorrisSample_df.loc[:, 'v102_epc.leaflitr_flab'] + MorrisSample_df.loc[:, 'v102_epc.leaflitr_flig']).round(roundTol) != 1.0000):
    sys.exit('PyERROR38: Sum of leaflitr_fcel, flab, flig != 1 for v102')
if any((MorrisSample_df.loc[:, 'v3_epc.leaflitr_fcel'] + MorrisSample_df.loc[:, 'v3_epc.leaflitr_flab'] + MorrisSample_df.loc[:, 'v3_epc.leaflitr_flig']).round(roundTol) != 1.0000):
    sys.exit('PyERROR39: Sum of leaflitr_fcel, flab, flig != 1 for v3')
#%% epc.topt <= epc.tmax
#If epc.tmax is not greater, simulate a random value less than epc.tmax for epc.topt
for i in range(len(MorrisSample_df.filter(regex='epc.topt$').columns)):
    #Find the indicator for the soil type
    #string = Get all columns containing epc.tmax
    ind = re.split(string=MorrisSample_df.filter(regex='epc.topt$').columns[i], pattern='_')[0]
    #Use the indicator to extract the epc.topt, and epc.tmax columns
    Ksat = MorrisSample_df.loc[:, ind + '_epc.tmax']
    vKsat = MorrisSample_df.loc[:, ind + '_epc.topt']
    
    #Save the original column.
    MorrisSample_df.loc[:, 'orig_' + ind + '_epc.topt_orig'] = vKsat
    
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
    MorrisSample_df.loc[:, ind + '_epc.topt'] = vKsat

del compare, i, ind, j, vKsat, Ksat, trajChange, inds

#Check sums
if sum(MorrisSample_df.loc[:,'v102_epc.topt'] > MorrisSample_df.loc[:,'v102_epc.tmax']) != 0:
    sys.exit('PyERROR40: topt not < tmax for v102')
if sum(MorrisSample_df.loc[:,'v3_epc.topt'] > MorrisSample_df.loc[:,'v3_epc.tmax']) != 0:
    sys.exit('PyERROR41: topt not < tmax for v3')

#%% Leaf litter CN >= leaf CN
#If epc.leaflitr_cn is not greater, simulate a random value less than epc.leaflitr_cn for epc.leaf_cn
for i in range(len(MorrisSample_df.filter(regex='epc.leaf_cn$').columns)):
    #Find the indicator for the soil type
    #string = Get all columns containing epc.leaflitr_cn
    ind = re.split(string=MorrisSample_df.filter(regex='epc.leaf_cn$').columns[i], pattern='_')[0]
    #Use the indicator to extract the epc.leaf_cn, and epc.leaflitr_cn columns
    Ksat = MorrisSample_df.loc[:, ind + '_epc.leaflitr_cn']
    vKsat = MorrisSample_df.loc[:, ind + '_epc.leaf_cn']
    
    #Save the original column.
    MorrisSample_df.loc[:, 'orig_' + ind + '_epc.leaf_cn_orig'] = vKsat
    
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
    MorrisSample_df.loc[:, ind + '_epc.leaf_cn'] = vKsat

del compare, i, ind, j, vKsat, Ksat, trajChange, inds

#Check sums
if sum(MorrisSample_df.loc[:,'v102_epc.leaf_cn'] > MorrisSample_df.loc[:,'v102_epc.leaflitr_cn']) != 0:
    sys.exit('PyERROR42: leaf_cn not < leaflitr_cn for v102')
if sum(MorrisSample_df.loc[:,'v3_epc.leaf_cn'] > MorrisSample_df.loc[:,'v3_epc.leaflitr_cn']) != 0:
    sys.exit('PyERROR43: leaf_cn not < leaflitr_cn for v3')

#%% Check sums for all parameters once more to make sure nothing odd happened
if (any((MorrisSample_df.loc[:, 's8_silt'] + MorrisSample_df.loc[:, 's8_sand'] + MorrisSample_df.loc[:, 's8_clay']).round(roundTol) != 1.0000) | \
any((MorrisSample_df.loc[:, 's108_silt'] + MorrisSample_df.loc[:, 's108_sand'] + MorrisSample_df.loc[:, 's108_clay']).round(roundTol) != 1.0000) | \
any((MorrisSample_df.loc[:, 's9_silt'] + MorrisSample_df.loc[:, 's9_sand'] + MorrisSample_df.loc[:, 's9_clay']).round(roundTol) != 1.0000) | \
any((MorrisSample_df.loc[:, 's109_silt'] + MorrisSample_df.loc[:, 's109_sand'] + MorrisSample_df.loc[:, 's109_clay']).round(roundTol) != 1.0000) | \
any((MorrisSample_df.loc[:, 'v102_K_absorptance'] + MorrisSample_df.loc[:, 'v102_K_reflectance'] + MorrisSample_df.loc[:, 'v102_K_transmittance']).round(roundTol) != 1.0000) | \
any((MorrisSample_df.loc[:, 'v102_PAR_absorptance'] + MorrisSample_df.loc[:, 'v102_PAR_reflectance'] + MorrisSample_df.loc[:, 'v102_PAR_transmittance']).round(roundTol) != 1.0000) | \
any((MorrisSample_df.loc[:, 'v102_epc.frootlitr_fcel'] + MorrisSample_df.loc[:, 'v102_epc.frootlitr_flab'] + MorrisSample_df.loc[:, 'v102_epc.frootlitr_flig']).round(roundTol) != 1.0000) | \
any((MorrisSample_df.loc[:, 'v3_epc.frootlitr_fcel'] + MorrisSample_df.loc[:, 'v3_epc.frootlitr_flab'] + MorrisSample_df.loc[:, 'v3_epc.frootlitr_flig']).round(roundTol) != 1.0000) | \
any((MorrisSample_df.loc[:, 'v102_epc.leaflitr_fcel'] + MorrisSample_df.loc[:, 'v102_epc.leaflitr_flab'] + MorrisSample_df.loc[:, 'v102_epc.leaflitr_flig']).round(roundTol) != 1.0000) | \
any((MorrisSample_df.loc[:, 'v3_epc.leaflitr_fcel'] + MorrisSample_df.loc[:, 'v3_epc.leaflitr_flab'] + MorrisSample_df.loc[:, 'v3_epc.leaflitr_flig']).round(roundTol) != 1.0000)):
    sys.exit('PyERROR44: One of the sums used to be correct, but after other function manipulations is now incorrect. This is a function bug that should be reported.')

if (sum(MorrisSample_df.loc[:,'s109_Ksat_0'] > MorrisSample_df.loc[:,'s9_Ksat_0']) + \
sum(MorrisSample_df.loc[:,'s108_Ksat_0'] > MorrisSample_df.loc[:,'s8_Ksat_0']) + \
sum(MorrisSample_df.loc[:,'s109_Ksat_0_v'] > MorrisSample_df.loc[:,'s109_Ksat_0']) + \
sum(MorrisSample_df.loc[:,'s9_Ksat_0_v'] > MorrisSample_df.loc[:,'s9_Ksat_0']) + \
sum(MorrisSample_df.loc[:,'s108_Ksat_0_v'] > MorrisSample_df.loc[:,'s108_Ksat_0']) + \
sum(MorrisSample_df.loc[:,'s8_Ksat_0_v'] > MorrisSample_df.loc[:,'s8_Ksat_0']) + \
sum(MorrisSample_df.loc[:,'s109_Ksat_0_v'] > MorrisSample_df.loc[:,'s9_Ksat_0_v']) + \
sum(MorrisSample_df.loc[:,'s108_Ksat_0_v'] > MorrisSample_df.loc[:,'s8_Ksat_0_v']) + \
sum(MorrisSample_df.loc[:,'s109_m'] > MorrisSample_df.loc[:,'s9_m']) + \
sum(MorrisSample_df.loc[:,'s108_m'] > MorrisSample_df.loc[:,'s8_m']) + \
sum(MorrisSample_df.loc[:,'s108_porosity_0'] > MorrisSample_df.loc[:,'s8_porosity_0']) + \
sum(MorrisSample_df.loc[:,'s109_porosity_0'] > MorrisSample_df.loc[:,'s9_porosity_0']) + \
sum(MorrisSample_df.loc[:,'v102_epc.topt'] > MorrisSample_df.loc[:,'v102_epc.tmax']) + \
sum(MorrisSample_df.loc[:,'v3_epc.topt'] > MorrisSample_df.loc[:,'v3_epc.tmax']) + \
sum(MorrisSample_df.loc[:,'v102_epc.topt'] > MorrisSample_df.loc[:,'v102_epc.tmax']) + \
sum(MorrisSample_df.loc[:,'v3_epc.topt'] > MorrisSample_df.loc[:,'v3_epc.tmax']) + \
sum(MorrisSample_df.loc[:,'v102_epc.leaf_cn'] > MorrisSample_df.loc[:,'v102_epc.leaflitr_cn']) + \
sum(MorrisSample_df.loc[:,'v3_epc.leaf_cn'] > MorrisSample_df.loc[:,'v3_epc.leaflitr_cn'])) != 0:
    sys.exit('PyERROR45: One of the inequalities used to be correct, but after other function manipulations is now incorrect. This is a function bug that should be reported.')

#%% All constraints have passed. Now make sure that the parameter that was supposed to change actually changed.
#Original file
OrigMorrisSample_df = pd.read_csv('MorrisSamples_BeforeProcessing.csv')

#Find all of the instances in which a parameter does not change in the AfterProcessing.csv file
ListMissing = []
ColsMissing = []
for i in range(len(MorrisSample_df.iloc[:,0])-1):
    #Check that there is a difference from the current row to the next row.
    #Absolute value because some changes have sum constraints such that a difference will always be 0
    if sum(abs(MorrisSample_df.iloc[i,:len(OrigMorrisSample_df.columns)] - MorrisSample_df.iloc[i+1,:len(OrigMorrisSample_df.columns)])) == 0:
        #record i+1 because that is the row that should have changed
        ListMissing.append(i+1)
        #Find which parameter should have changed - get the name
        #OrigMorrisSample_df.iloc[i,np.where(((OrigMorrisSample_df.iloc[i,:] - OrigMorrisSample_df.iloc[i+1,:]) != 0).values == True)[0][0]]
        ColsMissing.append(OrigMorrisSample_df.columns[np.where(((OrigMorrisSample_df.iloc[i,:] - OrigMorrisSample_df.iloc[i+1,:]) != 0).values == True)[0][0]])

if len(ListMissing) > 0:
    sys.exit('PyERROR46: Some of the variables that were supposed to change in the Morris trajectory did not change.')
else:
    #Write the resulting MorrisSample_df to a csv file
    MorrisSample_df.round(roundTol).to_csv('MorrisSamples_AfterProcessing.csv', index = False)  