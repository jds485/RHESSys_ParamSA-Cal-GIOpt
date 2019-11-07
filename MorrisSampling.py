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

#%% Set working directory
os.chdir('C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs\\MorrisSampleLocs')

#Set the random seed for randomly generated values
rd.seed(1349)

#%% Set Morris sampling parameters
#number of sampling trajectories
N = 40
#p to determine the grid levels for SALib 
#num_levels = 100
#Set rounding precision as number of decimal places. Using 10 for lack of a sigfig function in Py 2.7.x
#Note: There are still statements of '%.10f' for writing def files, and '%.10f' and '%.7f' for comparing values
#so rounding to more than 10 decimal places will cut to 10 in written files.
#This can result in sums of variables not equal to exactly 1. Best to use <= 10 decimal places for now.
#Also note that round() and pandas .round() give different results when placeholder 0s are needed.
#Oddly, both do not add 0s. Rather, round() adds 0.0000000025, and .round adds 0.0000000099.
roundTol = 10

#%% Load problem file for SALib sampling - NOTE - Morris in SALib is currently 
#not generating sampling locations correctly. Using R to generate the model 
#run locations instead.

ProbFile = pd.read_csv('BaismanMorrisSamplingProblemFile_Full.csv')

#Check that the lower bounds are all less than the upper bounds
if not all(ProbFile.iloc[:,2] < ProbFile.iloc[:,3]):
    sys.exit('For parameters, all lower bounds are not less than the upper bounds')

#Save that file into a new file with no headers
ProbFile.drop('DefParameter', axis = 1).to_csv('BaismanMorrisSamplingProblemFile_ForSA.csv', header=False, index=False) 

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

#%% Soil:

#Fixme: Tried to make a function to do this, but I'm confused by the handling of dataframe by Python functions.
#It seems that even the function requires a deep copy to be specified.
#Can and should compare results of this versus the explicit and unaesthetic current method
#test = Sum3Check(Col1='silt', Col2='sand', Col3='clay', MorrisSample=MorrisSample_df, Prob=ProbFile, regExCol='silt')
#%%
#silt + sand + clay = 1
#loop over the columns containing silt (+ sand + clay) to check the relationship is statisfied
#Fixme: This loop is slow because it has to loop through the entire database once per unique soil class.
#Could the i for loop be dropped and all soil classes done in the row iteration loop? 
for i in range(len(MorrisSample_df.filter(regex='silt$').columns)):
    #Find the indicator for the soil type
    #string = Get all columns containing silt
    ind = re.split(string=MorrisSample_df.filter(regex='silt$').columns[i], pattern='_')[0]
    #Use the indicator to extract the silt, sand, and clay columns
    silt = MorrisSample_df.loc[:, ind + '_silt']
    sand = MorrisSample_df.loc[:, ind + '_sand']
    clay = MorrisSample_df.loc[:, ind + '_clay']
    
    #Save the original column
    MorrisSample_df.loc[:, 'orig_' + ind + '_silt_orig'] = silt
    MorrisSample_df.loc[:, 'orig_' + ind + '_sand_orig'] = sand
    MorrisSample_df.loc[:, 'orig_' + ind + '_clay_orig'] = clay
    
    #Loop through the indices to reassign the sand silt and clay values - should remain within the delta step change
    for j in range(len(silt)):
        #Compute the remainder from 1
        delta = 1 - (sand[j] + silt[j] + clay[j])
        
        #Need to add delta/3 to each parameter.
        #Check that the redistribution of values is within each parameter's range
        #Add delta/3 
        #Check if this will make sand greater than upper bound on sand
        if ((sand[j] + delta/3) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_sand'].index[0],3])):
            #Set sand to its maximum value and distribute the remaining delta to clay and silt
            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_sand'].index[0],3] - sand[j])
            sand[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_sand'].index[0],3]
            
            #delta must be positive, so no need to check upper bounds on silt and clay by adding delta/2
            #Check if this will make silt greater than upper bound on silt
            if ((silt[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_silt'].index[0],3])):
                #Set silt to its maximum value and distribute the remaining delta to clay
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_silt'].index[0],3] - silt[j])
                silt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_silt'].index[0],3]
                
                #Check if this will make clay greater than upper bound on clay
                if ((clay[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_clay'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of clay, silt, and sand cannot be 1 for Replicate = %s' % str(j))
                else:
                    clay[j] += delta
            elif ((clay[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_clay'].index[0],3])):
                #Set clay to its maximum value and distribute the remaining delta to silt
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_clay'].index[0],3] - clay[j])
                clay[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_clay'].index[0],3]
                
                #Check if this will make silt greater than upper bound on silt
                if ((silt[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_silt'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of clay, silt, and sand cannot be 1 for Replicate = %s' % str(j))
                else:
                    silt[j] += delta
            else:
                #Clay and silt both not exceeding their max. Add delta/2
                clay[j] += delta/2
                silt[j] += delta/2
            
        elif ((sand[j] + delta/3) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_sand'].index[0],2])):        
            #Set sand to its minimum value and distribute the remaining delta to clay and silt
            delta += (sand[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_sand'].index[0],2])
            sand[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_sand'].index[0],2]
            
            #delta must be negative, so no need to check lower bounds on silt and clay by adding delta/2
            #Check if this will make silt less than lower bound on silt
            if ((silt[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_silt'].index[0],2])):
                #Set silt to its minimum value and distribute the remaining delta to clay
                delta += (silt[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_silt'].index[0],2])
                silt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_silt'].index[0],2]
                
                #Check if this will make clay less than lower bound on clay
                if ((clay[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_clay'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of clay, silt, and sand cannot be 1 for Replicate = %s' % str(j))
                else:
                    clay[j] += delta
            elif ((clay[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_clay'].index[0],2])):
                #Set clay to its minimum value and distribute the remaining delta to silt
                delta += (clay[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_clay'].index[0],2])
                clay[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_clay'].index[0],2]
                
                #Check if this will make silt less than lower bound on silt
                if ((silt[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_silt'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of clay, silt, and sand cannot be 1 for Replicate = %s' % str(j))
                else:
                    silt[j] += delta
            else:
                #Clay and silt both not less than their min. Add delta/2 (which is subtraction)
                clay[j] += delta/2
                silt[j] += delta/2
        elif ((silt[j] + delta/3) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_silt'].index[0],3])):
            #Set silt to its maximum value and distribute the remaining delta to clay and silt
            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_silt'].index[0],3] - silt[j])
            silt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_silt'].index[0],3]
            
            #delta must be positive, so no need to check upper bounds on sand and clay by adding delta/2
            if ((sand[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_sand'].index[0],3])):
                #Set sand to its maximum value and distribute the remaining delta to clay
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_sand'].index[0],3] - sand[j])
                sand[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_sand'].index[0],3]
                
                #Check if this will make clay greater than upper bound on clay
                if ((clay[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_clay'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of clay, silt, and sand cannot be 1 for Replicate = %s' % str(j))
                else:
                    clay[j] += delta
            elif ((clay[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_clay'].index[0],3])):
                #Set clay to its maximum value and distribute the remaining delta to sand
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_clay'].index[0],3] - clay[j])
                clay[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_clay'].index[0],3]
                
                #Check if this will make sand greater than upper bound on sand
                if ((sand[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_sand'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of clay, silt, and sand cannot be 1 for Replicate = %s' % str(j))
                else:
                    sand[j] += delta
            else:
                #Clay and sand both not exceeding their max. Add delta/2
                clay[j] += delta/2
                sand[j] += delta/2
            
        elif ((silt[j] + delta/3) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_silt'].index[0],2])):        
            #Set silt to its minimum value and distribute the remaining delta to clay and silt
            delta += (silt[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_silt'].index[0],2])
            silt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_silt'].index[0],2]
            
            #delta must be negative, so no need to check lower bounds on sand and clay by adding delta/2
            #Check if this will make sand less than lower bound on sand
            if ((sand[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_sand'].index[0],2])):
                #Set sand to its minimum value and distribute the remaining delta to clay
                delta += (sand[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_sand'].index[0],2])
                sand[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_sand'].index[0],2]
                
                #Check if this will make clay less than lower bound on clay
                if ((clay[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_clay'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of clay, silt, and sand cannot be 1 for Replicate = %s' % str(j))
                else:
                    clay[j] += delta
            elif ((clay[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_clay'].index[0],2])):
                #Set clay to its minimum value and distribute the remaining delta to sand
                delta += (clay[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_clay'].index[0],2])
                clay[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_clay'].index[0],2]
                
                #Check if this will make sand less than lower bound on sand
                if ((sand[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_sand'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of clay, silt, and sand cannot be 1 for Replicate = %s' % str(j))
                else:
                    sand[j] += delta
            else:
                #Clay and sand both not less than their min. Add delta/2 (which is subtraction)
                clay[j] += delta/2
                sand[j] += delta/2
        elif ((clay[j] + delta/3) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_clay'].index[0],3])):
            #Set clay to its maximum value and distribute the remaining delta to sand and silt
            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_clay'].index[0],3] - clay[j])
            clay[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_clay'].index[0],3]
            
            #delta must be positive, so no need to check upper bounds on sand and silt by adding delta/2
            #Check if this will make sand greater than upper bound on sand
            if ((sand[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_sand'].index[0],3])):
                #Set sand to its maximum value and distribute the remaining delta to silt
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_sand'].index[0],3] - sand[j])
                sand[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_sand'].index[0],3]
                
                #Check if this will make silt greater than upper bound on silt
                if ((silt[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_silt'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of clay, silt, and sand cannot be 1 for Replicate = %s' % str(j))
                else:
                    silt[j] += delta
            elif ((silt[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_silt'].index[0],3])):
                #Set silt to its maximum value and distribute the remaining delta to sand
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_silt'].index[0],3] - silt[j])
                silt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_silt'].index[0],3]
                
                #Check if this will make sand greater than upper bound on sand
                if ((sand[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_sand'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of clay, silt, and sand cannot be 1 for Replicate = %s' % str(j))
                else:
                    sand[j] += delta
            else:
                #silt and sand both not exceeding their max. Add delta/2
                silt[j] += delta/2
                sand[j] += delta/2
            
        elif ((clay[j] + delta/3) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_clay'].index[0],2])):        
            #Set clay to its minimum value and distribute the remaining delta to sand and silt
            delta += (clay[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_clay'].index[0],2])
            clay[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_clay'].index[0],2]
            
            #delta must be negative, so no need to check lower bounds on sand and silt by adding delta/2
            #Check if this will make sand less than lower bound on sand
            if ((sand[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_sand'].index[0],2])):
                #Set sand to its minimum value and distribute the remaining delta to silt
                delta += (sand[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_sand'].index[0],2])
                sand[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_sand'].index[0],2]
                
                #Check if this will make silt less than lower bound on silt
                if ((silt[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_silt'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of clay, silt, and sand cannot be 1 for Replicate = %s' % str(j))
                else:
                    silt[j] += delta
            elif ((silt[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_silt'].index[0],2])):
                #Set silt to its minimum value and distribute the remaining delta to sand
                delta += (silt[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_silt'].index[0],2])
                silt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_silt'].index[0],2]
                
                #Check if this will make sand less than lower bound on sand
                if ((sand[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_sand'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of clay, silt, and sand cannot be 1 for Replicate = %s' % str(j))
                else:
                    sand[j] += delta
            else:
                #silt and sand both not less than their min. Add delta/2 (which is subtraction)
                silt[j] += delta/2
                sand[j] += delta/2
        else:
            #sand, silt, and clay within their bounds. Add delta/3
            sand[j] += delta/3
            silt[j] += delta/3
            clay[j] += delta/3
        
        #The resulting values could sum to a number other than exactly 1, but be within thousandths of 1. 
        #So, round all values and ensure they sum to exactly 1
        sand[j] = round(sand[j],roundTol) 
        silt[j] = round(silt[j],roundTol)
        clay[j] = round(clay[j],roundTol)
        f = round(1 - (sand[j] + silt[j] + clay[j]),roundTol)
        if f > 0:
            #Check for being less than upper bound. Add to first of sand, silt, clay
            if (sand[j] + f) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_sand'].index[0],3]):
                sand[j] += f
            elif (silt[j] + f) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_silt'].index[0],3]):
                silt[j] += f
            elif (clay[j] + f) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_clay'].index[0],3]):
                clay[j] += f
            else:
                sys.exit('Sum of clay + silt + sand != 1, and cannot be rounded to 1 for Replicate = %s' % str(j))
        elif f < 0:
            #Check for being greater than lower bound. Add to first of sand, silt, clay
            if (sand[j] + f) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_sand'].index[0],2]):
                sand[j] += f
            elif (silt[j] + f) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_silt'].index[0],2]):
                silt[j] += f
            elif (clay[j] + f) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_clay'].index[0],2]):
                clay[j] += f
            else:
                sys.exit('Sum of clay + silt + sand != 1, and cannot be rounded to 1 for Replicate = %s' % str(j))    
    
    #Save the new column of values to track what was changed.
    MorrisSample_df.loc[:, ind + '_silt'] = silt
    MorrisSample_df.loc[:, ind + '_sand'] = sand
    MorrisSample_df.loc[:, ind + '_clay'] = clay

del i, j, clay, silt, sand, delta, ind, f

#Check sums
if any((MorrisSample_df.loc[:, 's8_silt'] + MorrisSample_df.loc[:, 's8_sand'] + MorrisSample_df.loc[:, 's8_clay']).round(roundTol) != 1.0000):
    sys.exit('Sum of sand + silt + clay != 1 for s8')
if any((MorrisSample_df.loc[:, 's108_silt'] + MorrisSample_df.loc[:, 's108_sand'] + MorrisSample_df.loc[:, 's108_clay']).round(roundTol) != 1.0000):
    sys.exit('Sum of sand + silt + clay != 1 for s108')
if any((MorrisSample_df.loc[:, 's9_silt'] + MorrisSample_df.loc[:, 's9_sand'] + MorrisSample_df.loc[:, 's9_clay']).round(roundTol) != 1.0000):
    sys.exit('Sum of sand + silt + clay != 1 for s9')
if any((MorrisSample_df.loc[:, 's109_silt'] + MorrisSample_df.loc[:, 's109_sand'] + MorrisSample_df.loc[:, 's109_clay']).round(roundTol) != 1.0000):
    sys.exit('Sum of sand + silt + clay != 1 for s109')

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
    #string = Get all columns containing m
    ind1 = re.split(string=MorrisSample_df.filter(regex='_Ksat_0$').columns[i], pattern='_')[0]
    
    #Only work on the columns that have a 0, and find the other corresponding index
    if len(re.split(string=ind1, pattern='0')) == 2:
        ind2 = string.join(re.split(string=ind1, pattern='10'), sep='')
        #Use the indicator to extract the m compacted, and m uncompacted columns
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
    sys.exit('s109 Ksat not < s9 Ksat')
if sum(MorrisSample_df.loc[:,'s108_Ksat_0'] > MorrisSample_df.loc[:,'s8_Ksat_0']):
    sys.exit('s108 Ksat not < s8 Ksat')
    
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
    sys.exit('s109 vKsat not < s109 Ksat')
if sum(MorrisSample_df.loc[:,'s9_Ksat_0_v'] > MorrisSample_df.loc[:,'s9_Ksat_0']) != 0:
    sys.exit('s9 vKsat not < s9 Ksat')
if sum(MorrisSample_df.loc[:,'s108_Ksat_0_v'] > MorrisSample_df.loc[:,'s108_Ksat_0']) != 0:
    sys.exit('s108 vKsat not < s108 Ksat')
if sum(MorrisSample_df.loc[:,'s8_Ksat_0_v'] > MorrisSample_df.loc[:,'s8_Ksat_0']) != 0:
    sys.exit('s8 vKsat not < s8 Ksat')

#%%
#Finally, make sure that vKsat compacted < vKsat uncompacted
#If it is not greater, simulate a random value less than Ksatu for Ksatc
for i in range(len(MorrisSample_df.filter(regex='_Ksat_0_v$').columns)):
    #Find the indicator for the first soil type
    #string = Get all columns containing m
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
                elif (len(mu[indsTrajPos].unique()) == 1) | (len(mc[indsTrajPos].unique()) == 1):
                    #There is one change in this trajectory for one, but not the other variable
                    if (len(mu[indsTrajPos].unique()) == 1):
                        #mc changes
                        indmcChange = mc[indsTrajPos][mc[indsTrajPos] != mc[indsTrajPos].iloc[0]].index[0]
                        
                        #Only 1 change in the trajectory. Change to random value less than mu and greater than lower bound
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0_v'].index[0],2], mu[indsTrajPos].iloc[0])]*len(mc[indsTrajPos[indsTrajPos < indmcChange]])
                        mc[indsTrajPos[indsTrajPos < indmcChange]] = Val
                    
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0_v'].index[0],2], mu[indsTrajPos].iloc[0])]*len(mc[indsTrajPos[indsTrajPos >= indmcChange]])
                        mc[indsTrajPos[indsTrajPos >= indmcChange]] = Val
                        
                        del indmcChange
                    else:
                        #mu changes
                        indmuChange = mu[indsTrajPos][mu[indsTrajPos] != mu[indsTrajPos].iloc[0]].index[0]
                        
                        #Only 1 change in the trajectory. Change to random value less than mu and greater than lower bound
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0_v'].index[0],2], mu[indsTrajPos[indsTrajPos < indmuChange]].iloc[0])]*len(mc[indsTrajPos[indsTrajPos < indmuChange]])
                        mc[indsTrajPos[indsTrajPos < indmuChange]] = Val
                    
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0_v'].index[0],2], mu[indsTrajPos[indsTrajPos >= indmuChange]].iloc[0])]*len(mc[indsTrajPos[indsTrajPos >= indmuChange]])
                        mc[indsTrajPos[indsTrajPos >= indmuChange]] = Val
                        
                        del indmuChange
                else:
                    #within these positions, mc and mu change each exactly once. Find when those happen.
                    indmcChange = mc[indsTrajPos][mc[indsTrajPos] != mc[indsTrajPos].iloc[0]].index[0]
                    indmuChange = mu[indsTrajPos][mu[indsTrajPos] != mu[indsTrajPos].iloc[0]].index[0]
                    if indmuChange == indmcChange:
                        #Only 1 change in the trajectory. Change to random value less than mu and greater than lower bound
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0_v'].index[0],2], mu[indsTrajPos[indsTrajPos < indmuChange]].iloc[0])]*len(mc[indsTrajPos[indsTrajPos < indmuChange]])
                        mc[indsTrajPos[indsTrajPos < indmuChange]] = Val
                    
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0_v'].index[0],2], mu[indsTrajPos[indsTrajPos >= indmuChange]].iloc[0])]*len(mc[indsTrajPos[indsTrajPos >= indmuChange]])
                        mc[indsTrajPos[indsTrajPos >= indmuChange]] = Val
                    elif indmuChange > indmcChange:
                        #Work from mc index first
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0_v'].index[0],2], mu[indsTrajPos[indsTrajPos < indmcChange]].iloc[0])]*len(mc[indsTrajPos[indsTrajPos < indmcChange]])
                        mc[indsTrajPos[indsTrajPos < indmcChange]] = Val
                    
                        #Now check the indices between indmcChange and indmuChange
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0_v'].index[0],2], mu[indsTrajPos[(indsTrajPos < indmuChange) & (indsTrajPos >= indmcChange)]].iloc[0])]*len(mc[indsTrajPos[(indsTrajPos < indmuChange) & (indsTrajPos >= indmcChange)]])
                        mc[indsTrajPos[(indsTrajPos < indmuChange) & (indsTrajPos >= indmcChange)]] = Val
                    
                        #Now check the indices greater than or equal to indmuChange
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0_v'].index[0],2], mu[indsTrajPos[(indsTrajPos >= indmuChange)]].iloc[0])]*len(mc[indsTrajPos[(indsTrajPos >= indmuChange)]])
                        mc[indsTrajPos[(indsTrajPos >= indmuChange)]] = Val
                    else:
                        #Work from mu index first
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0_v'].index[0],2], mu[indsTrajPos[indsTrajPos < indmuChange]].iloc[0])]*len(mc[indsTrajPos[indsTrajPos < indmuChange]])
                        mc[indsTrajPos[indsTrajPos < indmuChange]] = Val
                    
                        #Now check the indices between indmuChange and indmcChange
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0_v'].index[0],2], mu[indsTrajPos[(indsTrajPos < indmcChange) & (indsTrajPos >= indmuChange)]].iloc[0])]*len(mc[indsTrajPos[(indsTrajPos < indmcChange) & (indsTrajPos >= indmuChange)]])
                        mc[indsTrajPos[(indsTrajPos < indmcChange) & (indsTrajPos >= indmuChange)]] = Val
                    
                        #Now check the indices greater than or equal to indmcChange
                        Val = [rd.uniform(ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind1+'_Ksat_0_v'].index[0],2], mu[indsTrajPos[(indsTrajPos >= indmcChange)]].iloc[0])]*len(mc[indsTrajPos[(indsTrajPos >= indmcChange)]])
                        mc[indsTrajPos[(indsTrajPos >= indmcChange)]] = Val
                    
                    del indmcChange, indmuChange
                del Val
            del indsTrajPos
        #Save the new column
        MorrisSample_df.loc[:, ind1 + '_Ksat_0_v'] = mc

del compare, i, ind1, ind2, j, mu, mc, trajChange, inds

if sum(MorrisSample_df.loc[:,'s109_Ksat_0_v'] > MorrisSample_df.loc[:,'s9_Ksat_0_v']) != 0:
    sys.exit('s109 vKsat not < s9 Ksat')
if sum(MorrisSample_df.loc[:,'s108_Ksat_0_v'] > MorrisSample_df.loc[:,'s8_Ksat_0_v']) != 0:
    sys.exit('s108 vKsat not < s8 Ksat')

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

if sum(MorrisSample_df.loc[:,'s109_m'] > MorrisSample_df.loc[:,'s9_m']) != 0:
    sys.exit('s109 m not < s9 m')
if sum(MorrisSample_df.loc[:,'s108_m'] > MorrisSample_df.loc[:,'s8_m']) != 0:
    sys.exit('s108 m not < s8 m')
    
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

if sum(MorrisSample_df.loc[:,'s109_porosity_0'] > MorrisSample_df.loc[:,'s9_porosity_0']) != 0:
    sys.exit('s109 porosity_0 not < s9 porosity_0')
if sum(MorrisSample_df.loc[:,'s108_porosity_0'] > MorrisSample_df.loc[:,'s8_porosity_0']) != 0:
    sys.exit('s108 porosity_0 not < s8 porosity_0')

#%% Vegetation
#%% K_absorptance + K_reflectance + K_transmittance = 1 - round check
#The only venetation for SA that is changed is #102 - trees. Other two are 1 - values assigned randomly.
#Fixme: For the calibration will need a different check because all vars could be adjusted.
#Fixme: This loop is slow because it has to loop through the entire database once per unique soil class.
#Could the i for loop be dropped and all soil classes done in the row iteration loop? 
for i in range(len(MorrisSample_df.filter(regex='102_K_absorptance$').columns)):
    #Find the indicator for the soil type
    #string = Get all columns containing Ka
    ind = re.split(string=MorrisSample_df.filter(regex='K_absorptance$').columns[i], pattern='_')[0]
    #Use the indicator to extract the Ka, Kr, and Kt columns
    Ka = MorrisSample_df.loc[:, ind + '_K_absorptance']
    Kr = MorrisSample_df.loc[:, ind + '_K_reflectance']
    Kt = MorrisSample_df.loc[:, ind + '_K_transmittance']
    
    #Save the original column
    MorrisSample_df.loc[:, 'orig_' + ind + '_K_absorptance_orig'] = Ka
    MorrisSample_df.loc[:, 'orig_' + ind + '_K_reflectance_orig'] = Kr
    MorrisSample_df.loc[:, 'orig_' + ind + '_K_transmittance_orig'] = Kt
    
    #Loop through the indices to reassign the Kr Ka and Kt values - should remain within the delta step change
    for j in range(len(Ka)):
        #Compute the remainder from 1
        delta = 1 - (Kr[j] + Ka[j] + Kt[j])
        
        #Need to add delta/3 to each parameter.
        #Check that the redistribution of values is within each parameter's range
        #Add delta/3 
        #Check if this will make Kr greater than upper bound on Kr
        if ((Kr[j] + delta/3) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_reflectance'].index[0],3])):
            #Set Kr to its maximum value and distribute the remaining delta to Kt and Ka
            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_reflectance'].index[0],3] - Kr[j])
            Kr[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_reflectance'].index[0],3]
            
            #delta must be positive, so no need to check upper bounds on Ka and Kt by adding delta/2
            #Check if this will make Ka greater than upper bound on Ka
            if ((Ka[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_absorptance'].index[0],3])):
                #Set Ka to its maximum value and distribute the remaining delta to Kt
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_absorptance'].index[0],3] - Ka[j])
                Ka[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_absorptance'].index[0],3]
                
                #Check if this will make Kt greater than upper bound on Kt
                if ((Kt[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_transmittance'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kt[j] += delta
            elif ((Kt[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_transmittance'].index[0],3])):
                #Set Kt to its maximum value and distribute the remaining delta to Ka
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_transmittance'].index[0],3] - Kt[j])
                Kt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_transmittance'].index[0],3]
                
                #Check if this will make Ka greater than upper bound on Ka
                if ((Ka[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_absorptance'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Ka[j] += delta
            else:
                #Kt and Ka both not exceeding their max. Add delta/2
                Kt[j] += delta/2
                Ka[j] += delta/2
            
        elif ((Kr[j] + delta/3) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_reflectance'].index[0],2])):        
            #Set Kr to its minimum value and distribute the remaining delta to Kt and Ka
            delta += (Kr[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_reflectance'].index[0],2])
            Kr[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_reflectance'].index[0],2]
            
            #delta must be negative, so no need to check lower bounds on Ka and Kt by adding delta/2
            #Check if this will make Ka less than lower bound on Ka
            if ((Ka[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_absorptance'].index[0],2])):
                #Set Ka to its minimum value and distribute the remaining delta to Kt
                delta += (Ka[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_absorptance'].index[0],2])
                Ka[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_absorptance'].index[0],2]
                
                #Check if this will make Kt less than lower bound on Kt
                if ((Kt[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_transmittance'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kt[j] += delta
            elif ((Kt[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_transmittance'].index[0],2])):
                #Set Kt to its minimum value and distribute the remaining delta to Ka
                delta += (Kt[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_transmittance'].index[0],2])
                Kt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_transmittance'].index[0],2]
                
                #Check if this will make Ka less than lower bound on Ka
                if ((Ka[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_absorptance'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Ka[j] += delta
            else:
                #Kt and Ka both not less than their min. Add delta/2 (which is subtraction)
                Kt[j] += delta/2
                Ka[j] += delta/2
        elif ((Ka[j] + delta/3) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_absorptance'].index[0],3])):
            #Set Ka to its maximum value and distribute the remaining delta to Kt and Ka
            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_absorptance'].index[0],3] - Ka[j])
            Ka[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_absorptance'].index[0],3]
            
            #delta must be positive, so no need to check upper bounds on Kr and Kt by adding delta/2
            if ((Kr[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_reflectance'].index[0],3])):
                #Set Kr to its maximum value and distribute the remaining delta to Kt
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_reflectance'].index[0],3] - Kr[j])
                Kr[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_reflectance'].index[0],3]
                
                #Check if this will make Kt greater than upper bound on Kt
                if ((Kt[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_transmittance'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kt[j] += delta
            elif ((Kt[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_transmittance'].index[0],3])):
                #Set Kt to its maximum value and distribute the remaining delta to Kr
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_transmittance'].index[0],3] - Kt[j])
                Kt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_transmittance'].index[0],3]
                
                #Check if this will make Kr greater than upper bound on Kr
                if ((Kr[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_reflectance'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kr[j] += delta
            else:
                #Kt and Kr both not exceeding their max. Add delta/2
                Kt[j] += delta/2
                Kr[j] += delta/2
            
        elif ((Ka[j] + delta/3) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_absorptance'].index[0],2])):        
            #Set Ka to its minimum value and distribute the remaining delta to Kt and Ka
            delta += (Ka[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_absorptance'].index[0],2])
            Ka[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_absorptance'].index[0],2]
            
            #delta must be negative, so no need to check lower bounds on Kr and Kt by adding delta/2
            #Check if this will make Kr less than lower bound on Kr
            if ((Kr[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_reflectance'].index[0],2])):
                #Set Kr to its minimum value and distribute the remaining delta to Kt
                delta += (Kr[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_reflectance'].index[0],2])
                Kr[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_reflectance'].index[0],2]
                
                #Check if this will make Kt less than lower bound on Kt
                if ((Kt[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_transmittance'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kt[j] += delta
            elif ((Kt[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_transmittance'].index[0],2])):
                #Set Kt to its minimum value and distribute the remaining delta to Kr
                delta += (Kt[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_transmittance'].index[0],2])
                Kt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_transmittance'].index[0],2]
                
                #Check if this will make Kr less than lower bound on Kr
                if ((Kr[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_reflectance'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kr[j] += delta
            else:
                #Kt and Kr both not less than their min. Add delta/2 (which is subtraction)
                Kt[j] += delta/2
                Kr[j] += delta/2
        elif ((Kt[j] + delta/3) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_transmittance'].index[0],3])):
            #Set Kt to its maximum value and distribute the remaining delta to Kr and Ka
            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_transmittance'].index[0],3] - Kt[j])
            Kt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_transmittance'].index[0],3]
            
            #delta must be positive, so no need to check upper bounds on Kr and Ka by adding delta/2
            #Check if this will make Kr greater than upper bound on Kr
            if ((Kr[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_reflectance'].index[0],3])):
                #Set Kr to its maximum value and distribute the remaining delta to Ka
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_reflectance'].index[0],3] - Kr[j])
                Kr[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_reflectance'].index[0],3]
                
                #Check if this will make Ka greater than upper bound on Ka
                if ((Ka[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_absorptance'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Ka[j] += delta
            elif ((Ka[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_absorptance'].index[0],3])):
                #Set Ka to its maximum value and distribute the remaining delta to Kr
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_absorptance'].index[0],3] - Ka[j])
                Ka[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_absorptance'].index[0],3]
                
                #Check if this will make Kr greater than upper bound on Kr
                if ((Kr[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_reflectance'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kr[j] += delta
            else:
                #Ka and Kr both not exceeding their max. Add delta/2
                Ka[j] += delta/2
                Kr[j] += delta/2
            
        elif ((Kt[j] + delta/3) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_transmittance'].index[0],2])):        
            #Set Kt to its minimum value and distribute the remaining delta to Kr and Ka
            delta += (Kt[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_transmittance'].index[0],2])
            Kt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_transmittance'].index[0],2]
            
            #delta must be negative, so no need to check lower bounds on Kr and Ka by adding delta/2
            #Check if this will make Kr less than lower bound on Kr
            if ((Kr[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_reflectance'].index[0],2])):
                #Set Kr to its minimum value and distribute the remaining delta to Ka
                delta += (Kr[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_reflectance'].index[0],2])
                Kr[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_reflectance'].index[0],2]
                
                #Check if this will make Ka less than lower bound on Ka
                if ((Ka[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_absorptance'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Ka[j] += delta
            elif ((Ka[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_absorptance'].index[0],2])):
                #Set Ka to its minimum value and distribute the remaining delta to Kr
                delta += (Ka[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_absorptance'].index[0],2])
                Ka[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_absorptance'].index[0],2]
                
                #Check if this will make Kr less than lower bound on Kr
                if ((Kr[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_reflectance'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kr[j] += delta
            else:
                #Ka and Kr both not less than their min. Add delta/2 (which is subtraction)
                Ka[j] += delta/2
                Kr[j] += delta/2
        else:
            #Kr, Ka, and Kt within their bounds. Add delta/3
            Kr[j] += delta/3
            Ka[j] += delta/3
            Kt[j] += delta/3
        
        #The resulting values could sum to a number other than exactly 1, but be within thouKrths of 1. 
        #So, round all values and ensure they sum to exactly 1
        Kr[j] = round(Kr[j],roundTol) 
        Ka[j] = round(Ka[j],roundTol)
        Kt[j] = round(Kt[j],roundTol)
        f = round(1 - (Kr[j] + Ka[j] + Kt[j]),roundTol)
        if f > 0:
            #Check for being less than upper bound. Add to first of Kr, Ka, Kt
            if (Kr[j] + f) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_reflectance'].index[0],3]):
                Kr[j] += f
            elif (Ka[j] + f) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_absorptance'].index[0],3]):
                Ka[j] += f
            elif (Kt[j] + f) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_transmittance'].index[0],3]):
                Kt[j] += f
            else:
                sys.exit('Sum of Kt + Ka + Kr != 1, and cannot be rounded to 1 for Replicate = %s' % str(j))
        elif f < 0:
            #Check for being greater than lower bound. Add to first of Kr, Ka, Kt
            if (Kr[j] + f) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_reflectance'].index[0],2]):
                Kr[j] += f
            elif (Ka[j] + f) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_absorptance'].index[0],2]):
                Ka[j] += f
            elif (Kt[j] + f) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_K_transmittance'].index[0],2]):
                Kt[j] += f
            else:
                sys.exit('Sum of Kt + Ka + Kr != 1, and cannot be rounded to 1 for Replicate = %s' % str(j))    
    
    #Save the new column of values to track what was changed.
    MorrisSample_df.loc[:, ind + '_K_absorptance'] = Ka
    MorrisSample_df.loc[:, ind + '_K_reflectance'] = Kr
    MorrisSample_df.loc[:, ind + '_K_transmittance'] = Kt

del i, j, Kt, Ka, Kr, delta, ind, f

#Check sums
if any((MorrisSample_df.loc[:, 'v102_K_absorptance'] + MorrisSample_df.loc[:, 'v102_K_reflectance'] + MorrisSample_df.loc[:, 'v102_K_transmittance']).round(roundTol) != 1.0000):
    sys.exit('Sum of K absorptance, reflectance, transmittance != 1 for v102')

#%% PAR_absorptance + PAR_reflectance + PAR_transmittance = 1 - round check
#The only venetation for SA that is changed is #102 - trees. Other two are 1 - values assigned randomly.
#Fixme: For the calibration will need a different check because all vars could be adjusted.
#Fixme: This loop is slow because it has to loop through the entire database once per unique soil class.
#Could the i for loop be dropped and all soil classes done in the row iteration loop? 
for i in range(len(MorrisSample_df.filter(regex='102_PAR_absorptance$').columns)):
    #Find the indicator for the soil type
    #string = Get all columns containing Ka
    ind = re.split(string=MorrisSample_df.filter(regex='PAR_absorptance$').columns[i], pattern='_')[0]
    #Use the indicator to extract the Ka, Kr, and Kt columns
    Ka = MorrisSample_df.loc[:, ind + '_PAR_absorptance']
    Kr = MorrisSample_df.loc[:, ind + '_PAR_reflectance']
    Kt = MorrisSample_df.loc[:, ind + '_PAR_transmittance']
    
    #Save the original column
    MorrisSample_df.loc[:, 'orig_' + ind + '_PAR_absorptance_orig'] = Ka
    MorrisSample_df.loc[:, 'orig_' + ind + '_PAR_reflectance_orig'] = Kr
    MorrisSample_df.loc[:, 'orig_' + ind + '_PAR_transmittance_orig'] = Kt
    
    #Loop through the indices to reassign the Kr Ka and Kt values - should remain within the delta step change
    for j in range(len(Ka)):
        #Compute the remainder from 1
        delta = 1 - (Kr[j] + Ka[j] + Kt[j])
        
        #Need to add delta/3 to each parameter.
        #Check that the redistribution of values is within each parameter's range
        #Add delta/3 
        #Check if this will make Kr greater than upper bound on Kr
        if ((Kr[j] + delta/3) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_reflectance'].index[0],3])):
            #Set Kr to its maximum value and distribute the remaining delta to Kt and Ka
            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_reflectance'].index[0],3] - Kr[j])
            Kr[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_reflectance'].index[0],3]
            
            #delta must be positive, so no need to check upper bounds on Ka and Kt by adding delta/2
            #Check if this will make Ka greater than upper bound on Ka
            if ((Ka[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_absorptance'].index[0],3])):
                #Set Ka to its maximum value and distribute the remaining delta to Kt
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_absorptance'].index[0],3] - Ka[j])
                Ka[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_absorptance'].index[0],3]
                
                #Check if this will make Kt greater than upper bound on Kt
                if ((Kt[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_transmittance'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kt[j] += delta
            elif ((Kt[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_transmittance'].index[0],3])):
                #Set Kt to its maximum value and distribute the remaining delta to Ka
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_transmittance'].index[0],3] - Kt[j])
                Kt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_transmittance'].index[0],3]
                
                #Check if this will make Ka greater than upper bound on Ka
                if ((Ka[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_absorptance'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Ka[j] += delta
            else:
                #Kt and Ka both not exceeding their max. Add delta/2
                Kt[j] += delta/2
                Ka[j] += delta/2
            
        elif ((Kr[j] + delta/3) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_reflectance'].index[0],2])):        
            #Set Kr to its minimum value and distribute the remaining delta to Kt and Ka
            delta += (Kr[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_reflectance'].index[0],2])
            Kr[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_reflectance'].index[0],2]
            
            #delta must be negative, so no need to check lower bounds on Ka and Kt by adding delta/2
            #Check if this will make Ka less than lower bound on Ka
            if ((Ka[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_absorptance'].index[0],2])):
                #Set Ka to its minimum value and distribute the remaining delta to Kt
                delta += (Ka[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_absorptance'].index[0],2])
                Ka[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_absorptance'].index[0],2]
                
                #Check if this will make Kt less than lower bound on Kt
                if ((Kt[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_transmittance'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kt[j] += delta
            elif ((Kt[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_transmittance'].index[0],2])):
                #Set Kt to its minimum value and distribute the remaining delta to Ka
                delta += (Kt[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_transmittance'].index[0],2])
                Kt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_transmittance'].index[0],2]
                
                #Check if this will make Ka less than lower bound on Ka
                if ((Ka[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_absorptance'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Ka[j] += delta
            else:
                #Kt and Ka both not less than their min. Add delta/2 (which is subtraction)
                Kt[j] += delta/2
                Ka[j] += delta/2
        elif ((Ka[j] + delta/3) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_absorptance'].index[0],3])):
            #Set Ka to its maximum value and distribute the remaining delta to Kt and Ka
            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_absorptance'].index[0],3] - Ka[j])
            Ka[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_absorptance'].index[0],3]
            
            #delta must be positive, so no need to check upper bounds on Kr and Kt by adding delta/2
            if ((Kr[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_reflectance'].index[0],3])):
                #Set Kr to its maximum value and distribute the remaining delta to Kt
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_reflectance'].index[0],3] - Kr[j])
                Kr[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_reflectance'].index[0],3]
                
                #Check if this will make Kt greater than upper bound on Kt
                if ((Kt[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_transmittance'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kt[j] += delta
            elif ((Kt[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_transmittance'].index[0],3])):
                #Set Kt to its maximum value and distribute the remaining delta to Kr
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_transmittance'].index[0],3] - Kt[j])
                Kt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_transmittance'].index[0],3]
                
                #Check if this will make Kr greater than upper bound on Kr
                if ((Kr[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_reflectance'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kr[j] += delta
            else:
                #Kt and Kr both not exceeding their max. Add delta/2
                Kt[j] += delta/2
                Kr[j] += delta/2
            
        elif ((Ka[j] + delta/3) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_absorptance'].index[0],2])):        
            #Set Ka to its minimum value and distribute the remaining delta to Kt and Ka
            delta += (Ka[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_absorptance'].index[0],2])
            Ka[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_absorptance'].index[0],2]
            
            #delta must be negative, so no need to check lower bounds on Kr and Kt by adding delta/2
            #Check if this will make Kr less than lower bound on Kr
            if ((Kr[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_reflectance'].index[0],2])):
                #Set Kr to its minimum value and distribute the remaining delta to Kt
                delta += (Kr[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_reflectance'].index[0],2])
                Kr[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_reflectance'].index[0],2]
                
                #Check if this will make Kt less than lower bound on Kt
                if ((Kt[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_transmittance'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kt[j] += delta
            elif ((Kt[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_transmittance'].index[0],2])):
                #Set Kt to its minimum value and distribute the remaining delta to Kr
                delta += (Kt[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_transmittance'].index[0],2])
                Kt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_transmittance'].index[0],2]
                
                #Check if this will make Kr less than lower bound on Kr
                if ((Kr[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_reflectance'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kr[j] += delta
            else:
                #Kt and Kr both not less than their min. Add delta/2 (which is subtraction)
                Kt[j] += delta/2
                Kr[j] += delta/2
        elif ((Kt[j] + delta/3) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_transmittance'].index[0],3])):
            #Set Kt to its maximum value and distribute the remaining delta to Kr and Ka
            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_transmittance'].index[0],3] - Kt[j])
            Kt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_transmittance'].index[0],3]
            
            #delta must be positive, so no need to check upper bounds on Kr and Ka by adding delta/2
            #Check if this will make Kr greater than upper bound on Kr
            if ((Kr[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_reflectance'].index[0],3])):
                #Set Kr to its maximum value and distribute the remaining delta to Ka
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_reflectance'].index[0],3] - Kr[j])
                Kr[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_reflectance'].index[0],3]
                
                #Check if this will make Ka greater than upper bound on Ka
                if ((Ka[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_absorptance'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Ka[j] += delta
            elif ((Ka[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_absorptance'].index[0],3])):
                #Set Ka to its maximum value and distribute the remaining delta to Kr
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_absorptance'].index[0],3] - Ka[j])
                Ka[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_absorptance'].index[0],3]
                
                #Check if this will make Kr greater than upper bound on Kr
                if ((Kr[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_reflectance'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kr[j] += delta
            else:
                #Ka and Kr both not exceeding their max. Add delta/2
                Ka[j] += delta/2
                Kr[j] += delta/2
            
        elif ((Kt[j] + delta/3) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_transmittance'].index[0],2])):        
            #Set Kt to its minimum value and distribute the remaining delta to Kr and Ka
            delta += (Kt[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_transmittance'].index[0],2])
            Kt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_transmittance'].index[0],2]
            
            #delta must be negative, so no need to check lower bounds on Kr and Ka by adding delta/2
            #Check if this will make Kr less than lower bound on Kr
            if ((Kr[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_reflectance'].index[0],2])):
                #Set Kr to its minimum value and distribute the remaining delta to Ka
                delta += (Kr[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_reflectance'].index[0],2])
                Kr[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_reflectance'].index[0],2]
                
                #Check if this will make Ka less than lower bound on Ka
                if ((Ka[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_absorptance'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Ka[j] += delta
            elif ((Ka[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_absorptance'].index[0],2])):
                #Set Ka to its minimum value and distribute the remaining delta to Kr
                delta += (Ka[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_absorptance'].index[0],2])
                Ka[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_absorptance'].index[0],2]
                
                #Check if this will make Kr less than lower bound on Kr
                if ((Kr[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_reflectance'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kr[j] += delta
            else:
                #Ka and Kr both not less than their min. Add delta/2 (which is subtraction)
                Ka[j] += delta/2
                Kr[j] += delta/2
        else:
            #Kr, Ka, and Kt within their bounds. Add delta/3
            Kr[j] += delta/3
            Ka[j] += delta/3
            Kt[j] += delta/3
        
        #The resulting values could sum to a number other than exactly 1, but be within thouKrths of 1. 
        #So, round all values and ensure they sum to exactly 1
        Kr[j] = round(Kr[j],roundTol) 
        Ka[j] = round(Ka[j],roundTol)
        Kt[j] = round(Kt[j],roundTol)
        f = round(1 - (Kr[j] + Ka[j] + Kt[j]),roundTol)
        if f > 0:
            #Check for being less than upper bound. Add to first of Kr, Ka, Kt
            if (Kr[j] + f) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_reflectance'].index[0],3]):
                Kr[j] += f
            elif (Ka[j] + f) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_absorptance'].index[0],3]):
                Ka[j] += f
            elif (Kt[j] + f) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_transmittance'].index[0],3]):
                Kt[j] += f
            else:
                sys.exit('Sum of Kt + Ka + Kr != 1, and cannot be rounded to 1 for Replicate = %s' % str(j))
        elif f < 0:
            #Check for being greater than lower bound. Add to first of Kr, Ka, Kt
            if (Kr[j] + f) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_reflectance'].index[0],2]):
                Kr[j] += f
            elif (Ka[j] + f) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_absorptance'].index[0],2]):
                Ka[j] += f
            elif (Kt[j] + f) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_PAR_transmittance'].index[0],2]):
                Kt[j] += f
            else:
                sys.exit('Sum of Kt + Ka + Kr != 1, and cannot be rounded to 1 for Replicate = %s' % str(j))    
    
    #Save the new column of values to track what was changed.
    MorrisSample_df.loc[:, ind + '_PAR_absorptance'] = Ka
    MorrisSample_df.loc[:, ind + '_PAR_reflectance'] = Kr
    MorrisSample_df.loc[:, ind + '_PAR_transmittance'] = Kt

del i, j, Kt, Ka, Kr, delta, ind, f

#Check sums
if any((MorrisSample_df.loc[:, 'v102_PAR_absorptance'] + MorrisSample_df.loc[:, 'v102_PAR_reflectance'] + MorrisSample_df.loc[:, 'v102_PAR_transmittance']).round(roundTol) != 1.0000):
    sys.exit('Sum of PAR absorptance, reflectance, transmittance != 1 for v102')

#%% epc.frootlitr_fcel + epc.frootlitr_flab + epc.frootlitr_flig = 1 - round check
#The only venetation for SA that is changed is #102 - trees and #3 - grass. Other is not random.
#Fixme: This loop is slow because it has to loop through the entire database once per unique soil class.
#Could the i for loop be dropped and all soil classes done in the row iteration loop? 
for i in range(len(MorrisSample_df.filter(regex='epc.frootlitr_fcel$').columns)):
    #Find the indicator for the soil type
    #string = Get all columns containing Ka
    ind = re.split(string=MorrisSample_df.filter(regex='epc.frootlitr_fcel$').columns[i], pattern='_')[0]
    #Use the indicator to extract the Ka, Kr, and Kt columns
    Ka = MorrisSample_df.loc[:, ind + '_epc.frootlitr_fcel']
    Kr = MorrisSample_df.loc[:, ind + '_epc.frootlitr_flab']
    Kt = MorrisSample_df.loc[:, ind + '_epc.frootlitr_flig']
    
    #Save the original column
    MorrisSample_df.loc[:, 'orig_' + ind + '_epc.frootlitr_fcel_orig'] = Ka
    MorrisSample_df.loc[:, 'orig_' + ind + '_epc.frootlitr_flab_orig'] = Kr
    MorrisSample_df.loc[:, 'orig_' + ind + '_epc.frootlitr_flig_orig'] = Kt
    
    #Loop through the indices to reassign the Kr Ka and Kt values - should remain within the delta step change
    for j in range(len(Ka)):
        #Compute the remainder from 1
        delta = 1 - (Kr[j] + Ka[j] + Kt[j])
        
        #Need to add delta/3 to each parameter.
        #Check that the redistribution of values is within each parameter's range
        #Add delta/3 
        #Check if this will make Kr greater than upper bound on Kr
        if ((Kr[j] + delta/3) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3])):
            #Set Kr to its maximum value and distribute the remaining delta to Kt and Ka
            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3] - Kr[j])
            Kr[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3]
            
            #delta must be positive, so no need to check upper bounds on Ka and Kt by adding delta/2
            #Check if this will make Ka greater than upper bound on Ka
            if ((Ka[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3])):
                #Set Ka to its maximum value and distribute the remaining delta to Kt
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Ka[j])
                Ka[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3]
                
                #Check if this will make Kt greater than upper bound on Kt
                if ((Kt[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kt[j] += delta
            elif ((Kt[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3])):
                #Set Kt to its maximum value and distribute the remaining delta to Ka
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Kt[j])
                Kt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3]
                
                #Check if this will make Ka greater than upper bound on Ka
                if ((Ka[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Ka[j] += delta
            else:
                #Kt and Ka both not exceeding their max. Add delta/2
                Kt[j] += delta/2
                Ka[j] += delta/2
            
        elif ((Kr[j] + delta/3) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])):        
            #Set Kr to its minimum value and distribute the remaining delta to Kt and Ka
            delta += (Kr[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])
            Kr[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2]
            
            #delta must be negative, so no need to check lower bounds on Ka and Kt by adding delta/2
            #Check if this will make Ka less than lower bound on Ka
            if ((Ka[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])):
                #Set Ka to its minimum value and distribute the remaining delta to Kt
                delta += (Ka[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])
                Ka[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]
                
                #Check if this will make Kt less than lower bound on Kt
                if ((Kt[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kt[j] += delta
            elif ((Kt[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])):
                #Set Kt to its minimum value and distribute the remaining delta to Ka
                delta += (Kt[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])
                Kt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2]
                
                #Check if this will make Ka less than lower bound on Ka
                if ((Ka[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Ka[j] += delta
            else:
                #Kt and Ka both not less than their min. Add delta/2 (which is subtraction)
                Kt[j] += delta/2
                Ka[j] += delta/2
        elif ((Ka[j] + delta/3) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3])):
            #Set Ka to its maximum value and distribute the remaining delta to Kt and Ka
            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Ka[j])
            Ka[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3]
            
            #delta must be positive, so no need to check upper bounds on Kr and Kt by adding delta/2
            if ((Kr[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3])):
                #Set Kr to its maximum value and distribute the remaining delta to Kt
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3] - Kr[j])
                Kr[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3]
                
                #Check if this will make Kt greater than upper bound on Kt
                if ((Kt[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kt[j] += delta
            elif ((Kt[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3])):
                #Set Kt to its maximum value and distribute the remaining delta to Kr
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Kt[j])
                Kt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3]
                
                #Check if this will make Kr greater than upper bound on Kr
                if ((Kr[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kr[j] += delta
            else:
                #Kt and Kr both not exceeding their max. Add delta/2
                Kt[j] += delta/2
                Kr[j] += delta/2
            
        elif ((Ka[j] + delta/3) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])):        
            #Set Ka to its minimum value and distribute the remaining delta to Kt and Ka
            delta += (Ka[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])
            Ka[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]
            
            #delta must be negative, so no need to check lower bounds on Kr and Kt by adding delta/2
            #Check if this will make Kr less than lower bound on Kr
            if ((Kr[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])):
                #Set Kr to its minimum value and distribute the remaining delta to Kt
                delta += (Kr[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])
                Kr[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2]
                
                #Check if this will make Kt less than lower bound on Kt
                if ((Kt[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kt[j] += delta
            elif ((Kt[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])):
                #Set Kt to its minimum value and distribute the remaining delta to Kr
                delta += (Kt[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])
                Kt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2]
                
                #Check if this will make Kr less than lower bound on Kr
                if ((Kr[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kr[j] += delta
            else:
                #Kt and Kr both not less than their min. Add delta/2 (which is subtraction)
                Kt[j] += delta/2
                Kr[j] += delta/2
        elif ((Kt[j] + delta/3) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3])):
            #Set Kt to its maximum value and distribute the remaining delta to Kr and Ka
            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Kt[j])
            Kt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3]
            
            #delta must be positive, so no need to check upper bounds on Kr and Ka by adding delta/2
            #Check if this will make Kr greater than upper bound on Kr
            if ((Kr[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3])):
                #Set Kr to its maximum value and distribute the remaining delta to Ka
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3] - Kr[j])
                Kr[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3]
                
                #Check if this will make Ka greater than upper bound on Ka
                if ((Ka[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Ka[j] += delta
            elif ((Ka[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3])):
                #Set Ka to its maximum value and distribute the remaining delta to Kr
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Ka[j])
                Ka[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3]
                
                #Check if this will make Kr greater than upper bound on Kr
                if ((Kr[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kr[j] += delta
            else:
                #Ka and Kr both not exceeding their max. Add delta/2
                Ka[j] += delta/2
                Kr[j] += delta/2
            
        elif ((Kt[j] + delta/3) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])):        
            #Set Kt to its minimum value and distribute the remaining delta to Kr and Ka
            delta += (Kt[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])
            Kt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2]
            
            #delta must be negative, so no need to check lower bounds on Kr and Ka by adding delta/2
            #Check if this will make Kr less than lower bound on Kr
            if ((Kr[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])):
                #Set Kr to its minimum value and distribute the remaining delta to Ka
                delta += (Kr[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])
                Kr[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2]
                
                #Check if this will make Ka less than lower bound on Ka
                if ((Ka[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Ka[j] += delta
            elif ((Ka[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])):
                #Set Ka to its minimum value and distribute the remaining delta to Kr
                delta += (Ka[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])
                Ka[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]
                
                #Check if this will make Kr less than lower bound on Kr
                if ((Kr[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kr[j] += delta
            else:
                #Ka and Kr both not less than their min. Add delta/2 (which is subtraction)
                Ka[j] += delta/2
                Kr[j] += delta/2
        else:
            #Kr, Ka, and Kt within their bounds. Add delta/3
            Kr[j] += delta/3
            Ka[j] += delta/3
            Kt[j] += delta/3
        
        #The resulting values could sum to a number other than exactly 1, but be within thouKrths of 1. 
        #So, round all values and ensure they sum to exactly 1
        Kr[j] = round(Kr[j],roundTol) 
        Ka[j] = round(Ka[j],roundTol)
        Kt[j] = round(Kt[j],roundTol)
        f = round(1 - (Kr[j] + Ka[j] + Kt[j]),roundTol)
        if f > 0:
            #Check for being less than upper bound. Add to first of Kr, Ka, Kt
            if (Kr[j] + f) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3]):
                Kr[j] += f
            elif (Ka[j] + f) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3]):
                Ka[j] += f
            elif (Kt[j] + f) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3]):
                Kt[j] += f
            else:
                sys.exit('Sum of Kt + Ka + Kr != 1, and cannot be rounded to 1 for Replicate = %s' % str(j))
        elif f < 0:
            #Check for being greater than lower bound. Add to first of Kr, Ka, Kt
            if (Kr[j] + f) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2]):
                Kr[j] += f
            elif (Ka[j] + f) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]):
                Ka[j] += f
            elif (Kt[j] + f) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2]):
                Kt[j] += f
            else:
                sys.exit('Sum of Kt + Ka + Kr != 1, and cannot be rounded to 1 for Replicate = %s' % str(j))    
    
    #Save the new column of values to track what was changed.
    MorrisSample_df.loc[:, ind + '_epc.frootlitr_fcel'] = Ka
    MorrisSample_df.loc[:, ind + '_epc.frootlitr_flab'] = Kr
    MorrisSample_df.loc[:, ind + '_epc.frootlitr_flig'] = Kt

del i, j, Kt, Ka, Kr, delta, ind, f

#Check sums
if any((MorrisSample_df.loc[:, 'v102_epc.frootlitr_fcel'] + MorrisSample_df.loc[:, 'v102_epc.frootlitr_flab'] + MorrisSample_df.loc[:, 'v102_epc.frootlitr_flig']).round(roundTol) != 1.0000):
    sys.exit('Sum of frootlitr_fcel, flab, flig != 1 for v102')
if any((MorrisSample_df.loc[:, 'v3_epc.frootlitr_fcel'] + MorrisSample_df.loc[:, 'v3_epc.frootlitr_flab'] + MorrisSample_df.loc[:, 'v3_epc.frootlitr_flig']).round(roundTol) != 1.0000):
    sys.exit('Sum of frootlitr_fcel, flab, flig != 1 for v3')
    
#%% epc.leaflitr_fcel + epc.leaflitr_flab + epc.leaflitr_flig = 1 - round check
#The only venetation for SA that is changed is #102 - trees and #3 - grass. Other is not random.
#Fixme: This loop is slow because it has to loop through the entire database once per unique soil class.
#Could the i for loop be dropped and all soil classes done in the row iteration loop? 
for i in range(len(MorrisSample_df.filter(regex='epc.leaflitr_fcel$').columns)):
    #Find the indicator for the soil type
    #string = Get all columns containing Ka
    ind = re.split(string=MorrisSample_df.filter(regex='epc.leaflitr_fcel$').columns[i], pattern='_')[0]
    #Use the indicator to extract the Ka, Kr, and Kt columns
    Ka = MorrisSample_df.loc[:, ind + '_epc.leaflitr_fcel']
    Kr = MorrisSample_df.loc[:, ind + '_epc.leaflitr_flab']
    Kt = MorrisSample_df.loc[:, ind + '_epc.leaflitr_flig']
    
    #Save the original column
    MorrisSample_df.loc[:, 'orig_' + ind + '_epc.leaflitr_fcel_orig'] = Ka
    MorrisSample_df.loc[:, 'orig_' + ind + '_epc.leaflitr_flab_orig'] = Kr
    MorrisSample_df.loc[:, 'orig_' + ind + '_epc.leaflitr_flig_orig'] = Kt
    
    #Loop through the indices to reassign the Kr Ka and Kt values - should remain within the delta step change
    for j in range(len(Ka)):
        #Compute the remainder from 1
        delta = 1 - (Kr[j] + Ka[j] + Kt[j])
        
        #Need to add delta/3 to each parameter.
        #Check that the redistribution of values is within each parameter's range
        #Add delta/3 
        #Check if this will make Kr greater than upper bound on Kr
        if ((Kr[j] + delta/3) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flab'].index[0],3])):
            #Set Kr to its maximum value and distribute the remaining delta to Kt and Ka
            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flab'].index[0],3] - Kr[j])
            Kr[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flab'].index[0],3]
            
            #delta must be positive, so no need to check upper bounds on Ka and Kt by adding delta/2
            #Check if this will make Ka greater than upper bound on Ka
            if ((Ka[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_fcel'].index[0],3])):
                #Set Ka to its maximum value and distribute the remaining delta to Kt
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_fcel'].index[0],3] - Ka[j])
                Ka[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_fcel'].index[0],3]
                
                #Check if this will make Kt greater than upper bound on Kt
                if ((Kt[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flig'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kt[j] += delta
            elif ((Kt[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flig'].index[0],3])):
                #Set Kt to its maximum value and distribute the remaining delta to Ka
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flig'].index[0],3] - Kt[j])
                Kt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flig'].index[0],3]
                
                #Check if this will make Ka greater than upper bound on Ka
                if ((Ka[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_fcel'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Ka[j] += delta
            else:
                #Kt and Ka both not exceeding their max. Add delta/2
                Kt[j] += delta/2
                Ka[j] += delta/2
            
        elif ((Kr[j] + delta/3) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flab'].index[0],2])):        
            #Set Kr to its minimum value and distribute the remaining delta to Kt and Ka
            delta += (Kr[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flab'].index[0],2])
            Kr[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flab'].index[0],2]
            
            #delta must be negative, so no need to check lower bounds on Ka and Kt by adding delta/2
            #Check if this will make Ka less than lower bound on Ka
            if ((Ka[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_fcel'].index[0],2])):
                #Set Ka to its minimum value and distribute the remaining delta to Kt
                delta += (Ka[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_fcel'].index[0],2])
                Ka[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_fcel'].index[0],2]
                
                #Check if this will make Kt less than lower bound on Kt
                if ((Kt[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flig'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kt[j] += delta
            elif ((Kt[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flig'].index[0],2])):
                #Set Kt to its minimum value and distribute the remaining delta to Ka
                delta += (Kt[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flig'].index[0],2])
                Kt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flig'].index[0],2]
                
                #Check if this will make Ka less than lower bound on Ka
                if ((Ka[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_fcel'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Ka[j] += delta
            else:
                #Kt and Ka both not less than their min. Add delta/2 (which is subtraction)
                Kt[j] += delta/2
                Ka[j] += delta/2
        elif ((Ka[j] + delta/3) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_fcel'].index[0],3])):
            #Set Ka to its maximum value and distribute the remaining delta to Kt and Ka
            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_fcel'].index[0],3] - Ka[j])
            Ka[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_fcel'].index[0],3]
            
            #delta must be positive, so no need to check upper bounds on Kr and Kt by adding delta/2
            if ((Kr[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flab'].index[0],3])):
                #Set Kr to its maximum value and distribute the remaining delta to Kt
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flab'].index[0],3] - Kr[j])
                Kr[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flab'].index[0],3]
                
                #Check if this will make Kt greater than upper bound on Kt
                if ((Kt[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flig'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kt[j] += delta
            elif ((Kt[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flig'].index[0],3])):
                #Set Kt to its maximum value and distribute the remaining delta to Kr
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flig'].index[0],3] - Kt[j])
                Kt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flig'].index[0],3]
                
                #Check if this will make Kr greater than upper bound on Kr
                if ((Kr[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flab'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kr[j] += delta
            else:
                #Kt and Kr both not exceeding their max. Add delta/2
                Kt[j] += delta/2
                Kr[j] += delta/2
            
        elif ((Ka[j] + delta/3) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_fcel'].index[0],2])):        
            #Set Ka to its minimum value and distribute the remaining delta to Kt and Ka
            delta += (Ka[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_fcel'].index[0],2])
            Ka[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_fcel'].index[0],2]
            
            #delta must be negative, so no need to check lower bounds on Kr and Kt by adding delta/2
            #Check if this will make Kr less than lower bound on Kr
            if ((Kr[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flab'].index[0],2])):
                #Set Kr to its minimum value and distribute the remaining delta to Kt
                delta += (Kr[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flab'].index[0],2])
                Kr[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flab'].index[0],2]
                
                #Check if this will make Kt less than lower bound on Kt
                if ((Kt[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flig'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kt[j] += delta
            elif ((Kt[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flig'].index[0],2])):
                #Set Kt to its minimum value and distribute the remaining delta to Kr
                delta += (Kt[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flig'].index[0],2])
                Kt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flig'].index[0],2]
                
                #Check if this will make Kr less than lower bound on Kr
                if ((Kr[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flab'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kr[j] += delta
            else:
                #Kt and Kr both not less than their min. Add delta/2 (which is subtraction)
                Kt[j] += delta/2
                Kr[j] += delta/2
        elif ((Kt[j] + delta/3) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flig'].index[0],3])):
            #Set Kt to its maximum value and distribute the remaining delta to Kr and Ka
            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flig'].index[0],3] - Kt[j])
            Kt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flig'].index[0],3]
            
            #delta must be positive, so no need to check upper bounds on Kr and Ka by adding delta/2
            #Check if this will make Kr greater than upper bound on Kr
            if ((Kr[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flab'].index[0],3])):
                #Set Kr to its maximum value and distribute the remaining delta to Ka
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flab'].index[0],3] - Kr[j])
                Kr[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flab'].index[0],3]
                
                #Check if this will make Ka greater than upper bound on Ka
                if ((Ka[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_fcel'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Ka[j] += delta
            elif ((Ka[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_fcel'].index[0],3])):
                #Set Ka to its maximum value and distribute the remaining delta to Kr
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_fcel'].index[0],3] - Ka[j])
                Ka[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_fcel'].index[0],3]
                
                #Check if this will make Kr greater than upper bound on Kr
                if ((Kr[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flab'].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kr[j] += delta
            else:
                #Ka and Kr both not exceeding their max. Add delta/2
                Ka[j] += delta/2
                Kr[j] += delta/2
            
        elif ((Kt[j] + delta/3) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flig'].index[0],2])):        
            #Set Kt to its minimum value and distribute the remaining delta to Kr and Ka
            delta += (Kt[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flig'].index[0],2])
            Kt[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flig'].index[0],2]
            
            #delta must be negative, so no need to check lower bounds on Kr and Ka by adding delta/2
            #Check if this will make Kr less than lower bound on Kr
            if ((Kr[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flab'].index[0],2])):
                #Set Kr to its minimum value and distribute the remaining delta to Ka
                delta += (Kr[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flab'].index[0],2])
                Kr[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flab'].index[0],2]
                
                #Check if this will make Ka less than lower bound on Ka
                if ((Ka[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_fcel'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Ka[j] += delta
            elif ((Ka[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_fcel'].index[0],2])):
                #Set Ka to its minimum value and distribute the remaining delta to Kr
                delta += (Ka[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_fcel'].index[0],2])
                Ka[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_fcel'].index[0],2]
                
                #Check if this will make Kr less than lower bound on Kr
                if ((Kr[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flab'].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('The sum of Kt, Ka, and Kr cannot be 1 for Replicate = %s' % str(j))
                else:
                    Kr[j] += delta
            else:
                #Ka and Kr both not less than their min. Add delta/2 (which is subtraction)
                Ka[j] += delta/2
                Kr[j] += delta/2
        else:
            #Kr, Ka, and Kt within their bounds. Add delta/3
            Kr[j] += delta/3
            Ka[j] += delta/3
            Kt[j] += delta/3
        
        #The resulting values could sum to a number other than exactly 1, but be within thouKrths of 1. 
        #So, round all values and ensure they sum to exactly 1
        Kr[j] = round(Kr[j],roundTol) 
        Ka[j] = round(Ka[j],roundTol)
        Kt[j] = round(Kt[j],roundTol)
        f = round(1 - (Kr[j] + Ka[j] + Kt[j]),roundTol)
        if f > 0:
            #Check for being less than upper bound. Add to first of Kr, Ka, Kt
            if (Kr[j] + f) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flab'].index[0],3]):
                Kr[j] += f
            elif (Ka[j] + f) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_fcel'].index[0],3]):
                Ka[j] += f
            elif (Kt[j] + f) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flig'].index[0],3]):
                Kt[j] += f
            else:
                sys.exit('Sum of Kt + Ka + Kr != 1, and cannot be rounded to 1 for Replicate = %s' % str(j))
        elif f < 0:
            #Check for being greater than lower bound. Add to first of Kr, Ka, Kt
            if (Kr[j] + f) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flab'].index[0],2]):
                Kr[j] += f
            elif (Ka[j] + f) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_fcel'].index[0],2]):
                Ka[j] += f
            elif (Kt[j] + f) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.leaflitr_flig'].index[0],2]):
                Kt[j] += f
            else:
                sys.exit('Sum of Kt + Ka + Kr != 1, and cannot be rounded to 1 for Replicate = %s' % str(j))    
    
    #Save the new column of values to track what was changed.
    MorrisSample_df.loc[:, ind + '_epc.leaflitr_fcel'] = Ka
    MorrisSample_df.loc[:, ind + '_epc.leaflitr_flab'] = Kr
    MorrisSample_df.loc[:, ind + '_epc.leaflitr_flig'] = Kt

del i, j, Kt, Ka, Kr, delta, ind, f

#Check sums
if any((MorrisSample_df.loc[:, 'v102_epc.leaflitr_fcel'] + MorrisSample_df.loc[:, 'v102_epc.leaflitr_flab'] + MorrisSample_df.loc[:, 'v102_epc.leaflitr_flig']).round(roundTol) != 1.0000):
    sys.exit('Sum of leaflitr_fcel, flab, flig != 1 for v102')
if any((MorrisSample_df.loc[:, 'v3_epc.leaflitr_fcel'] + MorrisSample_df.loc[:, 'v3_epc.leaflitr_flab'] + MorrisSample_df.loc[:, 'v3_epc.leaflitr_flig']).round(roundTol) != 1.0000):
    sys.exit('Sum of leaflitr_fcel, flab, flig != 1 for v3')
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
    sys.exit('topt not < tmax for v102')
if sum(MorrisSample_df.loc[:,'v3_epc.topt'] > MorrisSample_df.loc[:,'v3_epc.tmax']) != 0:
    sys.exit('topt not < tmax for v3')

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
if sum(MorrisSample_df.loc[:,'v102_epc.leaf_cn'] > MorrisSample_df.loc[:,'v102_epc.leaflitr_cn']):
    sys.exit('leaf_cn not < leaflitr_cn for v102')
if sum(MorrisSample_df.loc[:,'v3_epc.leaf_cn'] > MorrisSample_df.loc[:,'v3_epc.leaflitr_cn']):
    sys.exit('leaf_cn not < leaflitr_cn for v3')

#%% Check all sums once more to make sure nothing odd happened
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
    sys.exit('One of the sums used to be correct, but after other function manipulations is now incorrect. This is a function bug that should be reported.')

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
    sys.exit('One of the inequalities used to be correct, but after other function manipulations is now incorrect. This is a function bug that should be reported.')

#%% Write the resulting MorrisSample_df to a csv file
MorrisSample_df.round(roundTol).to_csv('MorrisSamples_AfterProcessing.csv', index = False)

#%% Test loading that file in and that the tests pass

MorrisSample_test = pd.read_csv('MorrisSamples_AfterProcessing.csv')
if (any((MorrisSample_test.loc[:, 's8_silt'] + MorrisSample_test.loc[:, 's8_sand'] + MorrisSample_test.loc[:, 's8_clay']).round(roundTol) != 1.0000) | \
any((MorrisSample_test.loc[:, 's108_silt'] + MorrisSample_test.loc[:, 's108_sand'] + MorrisSample_test.loc[:, 's108_clay']).round(roundTol) != 1.0000) | \
any((MorrisSample_test.loc[:, 's9_silt'] + MorrisSample_test.loc[:, 's9_sand'] + MorrisSample_test.loc[:, 's9_clay']).round(roundTol) != 1.0000) | \
any((MorrisSample_test.loc[:, 's109_silt'] + MorrisSample_test.loc[:, 's109_sand'] + MorrisSample_test.loc[:, 's109_clay']).round(roundTol) != 1.0000) | \
any((MorrisSample_test.loc[:, 'v102_K_absorptance'] + MorrisSample_test.loc[:, 'v102_K_reflectance'] + MorrisSample_test.loc[:, 'v102_K_transmittance']).round(roundTol) != 1.0000) | \
any((MorrisSample_test.loc[:, 'v102_PAR_absorptance'] + MorrisSample_test.loc[:, 'v102_PAR_reflectance'] + MorrisSample_test.loc[:, 'v102_PAR_transmittance']).round(roundTol) != 1.0000) | \
any((MorrisSample_test.loc[:, 'v102_epc.frootlitr_fcel'] + MorrisSample_test.loc[:, 'v102_epc.frootlitr_flab'] + MorrisSample_test.loc[:, 'v102_epc.frootlitr_flig']).round(roundTol) != 1.0000) | \
any((MorrisSample_test.loc[:, 'v3_epc.frootlitr_fcel'] + MorrisSample_test.loc[:, 'v3_epc.frootlitr_flab'] + MorrisSample_test.loc[:, 'v3_epc.frootlitr_flig']).round(roundTol) != 1.0000) | \
any((MorrisSample_test.loc[:, 'v102_epc.leaflitr_fcel'] + MorrisSample_test.loc[:, 'v102_epc.leaflitr_flab'] + MorrisSample_test.loc[:, 'v102_epc.leaflitr_flig']).round(roundTol) != 1.0000) | \
any((MorrisSample_test.loc[:, 'v3_epc.leaflitr_fcel'] + MorrisSample_test.loc[:, 'v3_epc.leaflitr_flab'] + MorrisSample_test.loc[:, 'v3_epc.leaflitr_flig']).round(roundTol) != 1.0000)):
    sys.exit('Upon loading csv file, one of the sums that was correct when it saved is now incorrect. This is a function bug that should be reported.')

if (sum(MorrisSample_test.loc[:,'s109_Ksat_0'] > MorrisSample_test.loc[:,'s9_Ksat_0']) + \
sum(MorrisSample_test.loc[:,'s108_Ksat_0'] > MorrisSample_test.loc[:,'s8_Ksat_0']) + \
sum(MorrisSample_test.loc[:,'s109_Ksat_0_v'] > MorrisSample_test.loc[:,'s109_Ksat_0']) + \
sum(MorrisSample_test.loc[:,'s9_Ksat_0_v'] > MorrisSample_test.loc[:,'s9_Ksat_0']) + \
sum(MorrisSample_test.loc[:,'s108_Ksat_0_v'] > MorrisSample_test.loc[:,'s108_Ksat_0']) + \
sum(MorrisSample_test.loc[:,'s8_Ksat_0_v'] > MorrisSample_test.loc[:,'s8_Ksat_0']) + \
sum(MorrisSample_test.loc[:,'s109_Ksat_0_v'] > MorrisSample_test.loc[:,'s9_Ksat_0_v']) + \
sum(MorrisSample_test.loc[:,'s108_Ksat_0_v'] > MorrisSample_test.loc[:,'s8_Ksat_0_v']) + \
sum(MorrisSample_test.loc[:,'s109_m'] > MorrisSample_test.loc[:,'s9_m']) + \
sum(MorrisSample_test.loc[:,'s108_m'] > MorrisSample_test.loc[:,'s8_m']) + \
sum(MorrisSample_test.loc[:,'s108_porosity_0'] > MorrisSample_test.loc[:,'s8_porosity_0']) + \
sum(MorrisSample_test.loc[:,'s109_porosity_0'] > MorrisSample_test.loc[:,'s9_porosity_0']) + \
sum(MorrisSample_test.loc[:,'v102_epc.topt'] > MorrisSample_test.loc[:,'v102_epc.tmax']) + \
sum(MorrisSample_test.loc[:,'v3_epc.topt'] > MorrisSample_test.loc[:,'v3_epc.tmax']) + \
sum(MorrisSample_test.loc[:,'v102_epc.topt'] > MorrisSample_test.loc[:,'v102_epc.tmax']) + \
sum(MorrisSample_test.loc[:,'v3_epc.topt'] > MorrisSample_test.loc[:,'v3_epc.tmax']) + \
sum(MorrisSample_test.loc[:,'v102_epc.leaf_cn'] > MorrisSample_test.loc[:,'v102_epc.leaflitr_cn']) + \
sum(MorrisSample_test.loc[:,'v3_epc.leaf_cn'] > MorrisSample_test.loc[:,'v3_epc.leaflitr_cn'])) != 0:
    sys.exit('Upon loading csv file, one of the inequalities that was correct when it saved is now incorrect. This is a function bug that should be reported.')