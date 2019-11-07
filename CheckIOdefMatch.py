# -*- coding: utf-8 -*-
"""
Created on Tue Nov  5 10:50:01 2019

@author: jsmif
"""

#Checking that def files are the same as what were input
import pandas as pd
import os
import string
os.chdir('C:\Users\jsmif\OneDrive - University of Virginia\BES_Data\BES_Data\RHESSysFiles\BR&POBR\RHESSysFilePreparation\defs\MorrisSampleLocs\Run19\defs')

Files = os.listdir(os.getcwd())
OutFiles = os.listdir('C:\Users\jsmif\OneDrive - University of Virginia\BES_Data\BES_Data\RHESSysFiles\BR&POBR\RHESSysFilePreparation\defs\MorrisSampleLocs\Run19\output')
#Now loop over these files
for h in range(len(Files)):
    if string.split(Files[h], ".def")[0] != 'basin_basin':
        #Original def file
        fo = pd.read_table(Files[h], delim_whitespace=True, header=None)
        #RHESSys output def file index
        for k in range(len(OutFiles)):    
            if len(string.split(OutFiles[k], string.split(Files[h], ".def")[0])) == 2:
                f = pd.read_table("C:\Users\jsmif\OneDrive - University of Virginia\BES_Data\BES_Data\RHESSysFiles\BR&POBR\RHESSysFilePreparation\defs\MorrisSampleLocs\Run19\output\\"+OutFiles[k], delim_whitespace=True, header=None)
        del k
        #Loop over the rows (parameters) in these files and compare the values for the parameters
        for d in range(len(f.iloc[:,1])):
            #Find dth parameter's column in the fo file
            IndD = fo.iloc[:,1][fo.iloc[:,1] == f.iloc[d,1]].index
            #and replace it with the value in the def file
            try:
                float(fo.iloc[IndD,0]) != float(f.iloc[d,0])
                if float(fo.iloc[IndD,0]) != float(f.iloc[d,0]):
                    print(Files[h], f.iloc[d,1], f.iloc[d,0], fo.iloc[IndD,0])                
            except:
                #Fixme: strings have different line endings, which causes them to be different.
                if str(fo.iloc[IndD,0]) != str(f.iloc[d,0]):
                    print(Files[h], f.iloc[d,1], str(f.iloc[d,0]), str(fo.iloc[IndD,0]))