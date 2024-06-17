# -*- coding: utf-8 -*-
"""
Created on Mon Nov  4 09:24:36 2019

@author: jsmif
"""

print('Starting ModifyVeg.py')

#Script for modifying the vegCollection.csv file and moving it to the project directory

#sys.argv contains: 
#0: unused - script call info
#1: vegetation csv file starting location "$BASEDIR"/RHESSysRuns/Run"$i"/GIS2RHESSys 
#2: desired output file location '"$PROJDIR"/"$RHESSysNAME"'
#3: def file directory
#4: csv file name (e.g., 'vegCollection_modified.csv')

import pandas as pd
import sys
import os
import string
veg = pd.read_csv(sys.argv[1]+'/'+sys.argv[4], header=None)

#Modify each column using the def files
#Go to the def file directory
os.chdir(sys.argv[3])

#Grep all of the def files beginning with stratum_
Files = os.listdir(os.getcwd())
IndH = []
for h in range(len(Files)):
    if string.find(Files[h], 'stratum') == 0:
        #Record the index. This file should be used
        IndH.append(h)
del h
#Now loop over these files
for h in range(len(IndH)):
    f = pd.read_csv(Files[IndH[h]], delim_whitespace=True, header=None)
    
    #Find the column index (name, so use loc) in veg corresponding to the def file f
    IndV = veg.iloc[5,:][veg.iloc[5,:] == str(f.iloc[f.iloc[:,1][(f.iloc[:,1] == 'stratum_default_ID') == True].index,0].values[0])].index
    #Loop over the rows in these files and add those data to the vegetation csv file
    for d in range(len(f.iloc[:,1])):
        #Find dth parameter's column in the veg file
        IndD = veg.iloc[:,1][veg.iloc[:,1] == f.iloc[d,1]].index
        #and replace it with the value in the def file
        veg.loc[IndD,IndV] = f.iloc[d,0]

#Write a new vegetation csv file in the output location
veg.to_csv(sys.argv[2]+'/'+sys.argv[4], index=False, header=False)
print('End ModifyVeg.py')