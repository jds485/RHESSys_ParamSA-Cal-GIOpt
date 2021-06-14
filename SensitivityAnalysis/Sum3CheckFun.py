# -*- coding: utf-8 -*-
"""
Created on Sun Apr  5 15:48:17 2020

@author: jsmif
"""
import sys
import random as rd
import re
import numpy as np

def Sum3CheckFun(ProbFile, #File that specifies the parameters in column 0, and the "Lower" and "Upper" bounds as columns 2 and 3. Column 1 is the parameters without the def file ID numbers attached, but is not used in this script.
              Col1, Col2, Col3, #The 3 column names to be summed (flab, fcel, flig)
              Col1v, Col2v, Col3v, #The 3 variable vectors
              ind, #index of def file (e.g., v102)
              roundTol, #round tolerance
              j, #index of ChangeVec
              delta
              ):
    #Check if adding delta/3 will make Col1v greater than upper bound on Col1v
    if ((Col1v[j] + delta/3.) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3])):
        #If so, then check if the previous Col1v value is also equal to the upper bound.
        if Col1v[j-1] == ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3]:
            #Both are true, so don't change Col1v[j] first. Instead distribute delta/2 to Col2v and Col3v.
            #delta must be positive, so need to check upper bounds on Col2v and Col3v by adding delta/2
            if ((Col2v[j] + delta/2.) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3])):
                #Set Col2v to its maximum value and distribute the remaining delta to Col3v
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j])
                Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3]
                
                #Check if this will make Col3v greater than upper bound on Col3v
                if ((Col3v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3])):
                    #Set Col3v to its maximum value and distribute the remaining delta to Col1v.
                    delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j])
                    Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3]
                    
                    #Check if this will make Col1v greater than upper bound on Col1v
                    if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3])):
                        #Throw an error - the sum of these 3 values must be 1 at this point.
                        sys.exit('PyERROR1: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                    else:
                        #This must be different than upper bound if there are several ways to make Col2v + Col3v + Col1v = 1
                        #Check if Col1v[j] = upper bound of Col1v. 
                        if Col1v[j] == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3]):
                            #Throw an error - the sum of these 3 values requires max of all 3 variables.
                            sys.exit('PyERROR2: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                        else:
                            Col1v[j] += delta
                            if round(Col1v[j], roundTol) == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3]):
                                #Throw an error - the sum of these 3 values requires max of all 3 variables.
                                sys.exit('PyERROR3: The sum of Col3v, Col2v, and Col1v can only be 1 when all are at their max for Replicate = %s' % str(j))
                else:
                    Col3v[j] += delta
                    #delta has been fully distributed.
                    #Check if Col1v[j] = upper bound of Col1v. 
                    if Col1v[j] == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3]):
                        #Subtract a random amount from Col1v and add it to Col3v
                        #The amount must be a number between 0 and min((max(Col3v) - Col3v[j]), (Col1v[j] - min(Col1v)))
                        rndNum = rd.uniform(10.**(-roundTol), min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j]), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])))
                        Col3v[j] += rndNum
                        Col1v[j] -= rndNum
                        del rndNum
                    
            elif ((Col3v[j] + delta/2.) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3])):
                #Set Col3v to its maximum value and distribute the remaining delta to Col1v
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j])
                Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3]
                
                #Check if this will make Col2v greater than upper bound on Col2v
                if ((Col2v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3])):
                    #Set Col2v to its maximum value and distribute the remaining delta to Col1v.
                    delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j])
                    Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3]
                    
                    #Check if this will make Col1v greater than upper bound on Col1v
                    if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3])):
                        #Throw an error - the sum of these 3 values must be 1 at this point.
                        sys.exit('PyERROR4: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                    else:
                        #This must be different than upper bound if there are several ways to make Col2v + Col3v + Col1v = 1
                        #Check if Col1v[j] = upper bound of Col1v. 
                        if Col1v[j] == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3]):
                            #Throw an error - the sum of these 3 values requires max of all 3 variables.
                            sys.exit('PyERROR5: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                        else:
                            Col1v[j] += delta
                            if round(Col1v[j], roundTol) == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3]):
                                #Throw an error - the sum of these 3 values requires max of all 3 variables.
                                sys.exit('PyERROR6: The sum of Col3v, Col2v, and Col1v can only be 1 when all are at their max for Replicate = %s' % str(j))
                else:
                    Col2v[j] += delta
                    #delta has been fully distributed.
                    #Check if Col1v[j] = upper bound of Col1v. 
                    if Col1v[j] == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3]):
                        #Subtract a random amount from Col1v and add it to Col2v
                        #The amount must be a number between 0 and min((max(Col2v) - Col2v[j]), (Col1v[j] - min(Col1v)))
                        rndNum = rd.uniform(10.**(-roundTol), min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j]), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])))
                        Col2v[j] += rndNum
                        Col1v[j] -= rndNum
                        del rndNum
            else:
                #Col3v and Col2v both not exceeding their max. Add delta/2
                Col3v[j] += delta/2.
                Col2v[j] += delta/2.
                #delta has been fully distributed.
                #Check if Col1v[j] = upper bound of Col1v. 
                if Col1v[j] == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3]):
                    #Subtract a random amount from Col1v and add it to Col3v and Col2v
                    #The amount must be a number between roundTol and min((max(Col3v) - Col3v[j]),(max(Col2v) - Col2v[j]), (Col1v[j] - min(Col1v)))
                    rndNum = rd.uniform(10.**(-roundTol), min(min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j])), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])))
                    Col2v[j] += rndNum/2.
                    Col3v[j] += rndNum/2.
                    Col1v[j] -= rndNum
                    del rndNum
        else:
            #Set Col1v to upper bound. That is okay for Col1v to be different from previous Col1v value
            #Set Col1v to its maximum value and distribute the remaining delta to Col3v and Col2v
            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3] - Col1v[j])
            Col1v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3]
            
            #delta must be positive, so need to check upper bounds on Col2v and Col3v by adding delta/2
            if ((Col2v[j] + delta/2.) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3])):
                #Set Col2v to its maximum value and distribute the remaining delta to Col3v
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j])
                Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3]
                
                #Check if this will make Col3v greater than upper bound on Col3v
                if ((Col3v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('PyERROR7: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                else:
                    Col3v[j] += delta
            elif ((Col3v[j] + delta/2.) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3])):
                #Set Col3v to its maximum value and distribute the remaining delta to Col1v
                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j])
                Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3]
                
                #Check if this will make Col2v greater than upper bound on Col2v
                if ((Col2v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('PyERROR8: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                else:
                    Col2v[j] += delta
            else:
                #Col3v and Col2v both not exceeding their max. Add delta/2
                Col3v[j] += delta/2.
                Col2v[j] += delta/2.
            
    #Check if adding delta/3 will make Col1v less than lower bound on Col1v
    elif ((Col1v[j] + delta/3.) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])):
        #If so, then check if the previous Col1v value is also equal to the lower bound.
        if Col1v[j-1] == ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2]:
            #Both are true, so don't change Col1v[j] first. Instead distribute delta/2 to Col2v and Col3v.
            #delta must be negative, so need to check lower bounds on Col2v and Col3v by adding delta/2
            if ((Col2v[j] + delta/2.) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])):
                #Set Col2v to its minimum value and distribute the remaining delta to Col3v
                delta += (Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])
                Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]
                
                #Check if this will make Col3v less than lower bound on Col3v
                if ((Col3v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])):
                    #Set Col3v to its minimum value and distribute the remaining delta to Col1v.
                    delta += (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])
                    Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2]
                    
                    #Check if this will make Col1v less than lower bound on Col1v
                    if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])):
                        #Throw an error - the sum of these 3 values must be 1 at this point.
                        sys.exit('PyERROR9: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                    else:
                        #This must be different than lower bound if there are several ways to make Col2v + Col3v + Col1v = 1
                        #Check if Col1v[j] = lower bound of Col1v. 
                        if Col1v[j] == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2]):
                            #Throw an error - the sum of these 3 values requires min of all 3 variables.
                            sys.exit('PyERROR10: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                        else:
                            Col1v[j] += delta
                            if round(Col1v[j], roundTol) == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2]):
                                #Throw an error - the sum of these 3 values requires min of all 3 variables.
                                sys.exit('PyERROR11: The sum of Col3v, Col2v, and Col1v can only be 1 when all are at their min for Replicate = %s' % str(j))
                else:
                    Col3v[j] += delta
                    #delta has been fully distributed.
                    #Check if Col1v[j] = lower bound of Col1v. 
                    if Col1v[j] == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2]):
                        #Add a random amount to Col1v and subtract it from Col3v
                        #The amount must be a number between roundTol and min((Col3v[j] - min(Col3v)), (max(Col1v) - Col1v))
                        rndNum = rd.uniform(10.**(-roundTol), min((Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3] - Col1v[j])))
                        Col3v[j] -= rndNum
                        Col1v[j] += rndNum
                        del rndNum
                    
            elif ((Col3v[j] + delta/2.) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])):
                #Set Col3v to its minimum value and distribute the remaining delta to Col1v
                delta += (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])
                Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2]
                
                #Check if this will make Col2v less than lower bound on Col2v
                if ((Col2v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])):
                    #Set Col2v to its minimum value and distribute the remaining delta to Col1v.
                    delta += (Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])
                    Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]
                    
                    #Check if this will make Col1v less than lower bound on Col1v
                    if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])):
                        #Throw an error - the sum of these 3 values must be 1 at this point.
                        sys.exit('PyERROR12: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                    else:
                        #This must be different than lower bound if there are several ways to make Col2v + Col3v + Col1v = 1
                        #Check if Col1v[j] = lower bound of Col1v. 
                        if Col1v[j] == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2]):
                            #Throw an error - the sum of these 3 values requires min of all 3 variables.
                            sys.exit('PyERROR13: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                        else:
                            Col1v[j] += delta
                            if round(Col1v[j], roundTol) == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2]):
                                #Throw an error - the sum of these 3 values requires min of all 3 variables.
                                sys.exit('PyERROR14: The sum of Col3v, Col2v, and Col1v can only be 1 when all are at their min for Replicate = %s' % str(j))
                else:
                    Col2v[j] += delta
                    #delta has been fully distributed.
                    #Check if Col1v[j] = lower bound of Col1v. 
                    if Col1v[j] == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2]):
                        #Add a random amount to Col1v and subtract it from Col2v
                        #The amount must be a number between roundTol and min((Col2v[j] - min(Col2v)), (max(Col1v) - Col1v))
                        rndNum = rd.uniform(10.**(-roundTol), min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3] - Col1v[j])))
                        Col2v[j] -= rndNum
                        Col1v[j] += rndNum
                        del rndNum
            else:
                #Col3v and Col2v both not exceeding their max. Add delta/2
                Col3v[j] += delta/2.
                Col2v[j] += delta/2.
                #delta has been fully distributed.
                #Check if Col1v[j] = lower bound of Col1v. 
                if Col1v[j] == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2]):
                    #Add a random amount to Col1v and subtract it from Col2v
                    #The amount must be a number between roundTol and min((Col3v[j] - min(Col3v)), (Col2v[j] - min(Col2v)), (max(Col1v) - Col1v))
                    rndNum = rd.uniform(10.**(-roundTol), min(min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]), (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3] - Col1v[j])))
                    Col2v[j] -= rndNum/2.
                    Col3v[j] -= rndNum/2.
                    Col1v[j] += rndNum
                    del rndNum
        else:
            #Set Col1v to lower bound. That is okay for Col1v to be different from previous Col1v value
            #Set Col1v to its minimum value and distribute the remaining delta to Col3v and Col2v
            delta += (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])
            Col1v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2]
            
            #delta must be negative, so need to check lower bounds on Col2v and Col3v by adding delta/2
            if ((Col2v[j] + delta/2.) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])):
                #Set Col2v to its minimum value and distribute the remaining delta to Col3v
                delta += (Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])
                Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]
                
                #Check if this will make Col3v less than lower bound on Col3v
                if ((Col3v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('PyERROR15: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                else:
                    Col3v[j] += delta
            elif ((Col3v[j] + delta/2.) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])):
                #Set Col3v to its minimum value and distribute the remaining delta to Col1v
                delta += (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])
                Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2]
                
                #Check if this will make Col2v less than lower bound on Col2v
                if ((Col2v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])):
                    #Throw an error - the sum of these 3 values must be 1 at this point.
                    sys.exit('PyERROR16: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                else:
                    Col2v[j] += delta
            else:
                #Col3v and Col2v both not less than their min. Add delta/2
                Col3v[j] += delta/2.
                Col2v[j] += delta/2.
    
    else:
        #Changing Col1v is within bounds. Now need to check that adding delta/3 will not be equal to previous Col1v value
        if (round(Col1v[j] + delta/3., roundTol) == round(Col1v[j-1], roundTol)):
            #Don't change Col1v[j] first. Instead distribute delta/2 to Col2v and Col3v.
            if delta < 0:
                #Need to check lower bounds on Col2v and Col3v by adding delta/2
                if ((Col2v[j] + delta/2.) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])):
                    #Set Col2v to its minimum value and distribute the remaining delta to Col3v
                    delta += (Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])
                    Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]
                    
                    #Check if this will make Col3v less than lower bound on Col3v
                    if ((Col3v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])):
                        #Set Col3v to its minimum value and distribute the remaining delta to Col1v.
                        delta += (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])
                        Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2]
                        
                        #Check if this will make Col1v less than lower bound on Col1v
                        if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])):
                            #Throw an error - the sum of these 3 values must be 1 at this point.
                            sys.exit('PyERROR17: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                        else:
                            Col1v[j] += delta
                            #Check if Col1v[j] still = Col1v[j-1]. 
                            if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
                                #Subtract a random amount from Col1v and add evenly to Col2v and Col3v
                                #The amount must be a number between roundTol
                                rndNum = rd.uniform(10.**(-roundTol), min(min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j])), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])))
                                Col2v[j] += rndNum/2.
                                Col3v[j] += rndNum/2.
                                Col1v[j] -= rndNum
                                del rndNum
                    else:
                        Col3v[j] += delta
                        #delta has been fully distributed.
                        #Check if Col1v[j] = Col1v[j-1]
                        if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
                            #Subtract a random amount from Col1v and add it to Col3v
                            #The amount must be a number between roundTol and min((Col3v[j] - min(Col3v)), (max(Col1v) - Col1v))
                            rndNum = rd.uniform(10.**(-roundTol), min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j]), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])))
                            Col3v[j] += rndNum
                            Col1v[j] -= rndNum
                            del rndNum
                        
                elif ((Col3v[j] + delta/2.) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])):
                    #Set Col3v to its minimum value and distribute the remaining delta to Col2v
                    delta += (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])
                    Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2]
                    
                    #Check if this will make Col2v less than lower bound on Col2v
                    if ((Col2v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])):
                        #Set Col2v to its minimum value and distribute the remaining delta to Col1v.
                        delta += (Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])
                        Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]
                        
                        #Check if this will make Col1v less than lower bound on Col1v
                        if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])):
                            #Throw an error - the sum of these 3 values must be 1 at this point.
                            sys.exit('PyERROR18: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                        else:
                            Col1v[j] += delta
                            #Check if Col1v[j] still = Col1v[j-1]. 
                            if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
                                #Subtract a random amount from Col1v and add evenly to Col2v and Col3v
                                #The amount must be a number between roundTol
                                rndNum = rd.uniform(10.**(-roundTol), min(min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j])), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])))
                                Col2v[j] += rndNum/2.
                                Col3v[j] += rndNum/2.
                                Col1v[j] -= rndNum
                                del rndNum                                        
                    else:
                        Col2v[j] += delta
                        #delta has been fully distributed.
                        #Check if Col1v[j] = Col1v[j-1]
                        if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
                            #Subtract a random amount from Col1v and add it to Col2v
                            #The amount must be a number between roundTol and min((Col2v[j] - min(Col2v)), (max(Col1v) - Col1v))
                            rndNum = rd.uniform(10.**(-roundTol), min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j]), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])))
                            Col2v[j] += rndNum
                            Col1v[j] -= rndNum
                            del rndNum
                else:
                    #Col3v and Col2v both not exceeding their min. Add delta/2
                    Col3v[j] += delta/2.
                    Col2v[j] += delta/2.
                    #delta has been fully distributed.
                    #Check if Col1v[j] = Col1v[j-1]. 
                    if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
                        #Subtract a random amount from Col1v and add evenly to Col2v and Col3v
                        #The amount must be a number between roundTol
                        rndNum = rd.uniform(10.**(-roundTol), min(min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j])), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])))
                        Col2v[j] += rndNum/2.
                        Col3v[j] += rndNum/2.
                        Col1v[j] -= rndNum
                        del rndNum
            
            else:
                #Need to check upper bounds on Col2v and Col3v by adding delta/2
                if ((Col2v[j] + delta/2.) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3])):
                    #Set Col2v to its maximum value and distribute the remaining delta to Col3v
                    delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j])
                    Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3]
                    
                    #Check if this will make Col3v greater than upper bound on Col3v
                    if ((Col3v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3])):
                        #Set Col3v to its maximum value and distribute the remaining delta to Col1v.
                        delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j])
                        Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3]
                        
                        #Check if this will make Col1v greater than upper bound on Col1v
                        if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3])):
                            #Throw an error - the sum of these 3 values must be 1 at this point.
                            sys.exit('PyERROR19: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                        else:
                            Col1v[j] += delta
                            #Check if Col1v[j] still = Col1v[j-1]. 
                            if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
                                #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
                                #The amount must be a number between roundTol
                                rndNum = rd.uniform(10.**(-roundTol), min(min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]), (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3] - Col1v[j])))
                                Col2v[j] -= rndNum/2.
                                Col3v[j] -= rndNum/2.
                                Col1v[j] += rndNum
                                del rndNum
                    else:
                        Col3v[j] += delta
                        #delta has been fully distributed.
                        #Check if Col1v[j] = Col1v[j-1]
                        if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
                            #Add a random amount to Col1v and subtract from Col3v
                            #The amount must be a number between roundTol and min((Col3v[j] - min(Col3v)), (max(Col1v) - Col1v))
                            rndNum = rd.uniform(10.**(-roundTol), min((Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3] - Col1v[j])))
                            Col3v[j] -= rndNum
                            Col1v[j] += rndNum
                            del rndNum
                        
                elif ((Col3v[j] + delta/2.) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3])):
                    #Set Col3v to its maximum value and distribute the remaining delta to Col2v
                    delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j])
                    Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3]
                    
                    #Check if this will make Col2v greater than upper bound on Col2v
                    if ((Col2v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3])):
                        #Set Col2v to its maximum value and distribute the remaining delta to Col1v.
                        delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j])
                        Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3]
                        
                        #Check if this will make Col1v greater than upper bound on Col1v
                        if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3])):
                            #Throw an error - the sum of these 3 values must be 1 at this point.
                            sys.exit('PyERROR20: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                        else:
                            Col1v[j] += delta
                            #Check if Col1v[j] still = Col1v[j-1]. 
                            if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
                                #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
                                #The amount must be a number between roundTol
                                rndNum = rd.uniform(10.**(-roundTol), min(min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]), (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3] - Col1v[j])))
                                Col2v[j] -= rndNum/2.
                                Col3v[j] -= rndNum/2.
                                Col1v[j] += rndNum
                                del rndNum
                    else:
                        Col2v[j] += delta
                        #delta has been fully distributed.
                        #Check if Col1v[j] = Col1v[j-1]
                        if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
                            #Add a random amount to Col1v and subtract from Col2v
                            #The amount must be a number between roundTol and min((Col2v[j] - min(Col2v)), (max(Col1v) - Col1v))
                            rndNum = rd.uniform(10.**(-roundTol), min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3] - Col1v[j])))
                            Col2v[j] -= rndNum
                            Col1v[j] += rndNum
                            del rndNum
                else:
                    #Col3v and Col2v both not exceeding their min. Add delta/2
                    Col3v[j] += delta/2.
                    Col2v[j] += delta/2.
                    #delta has been fully distributed.
                    #Check if Col1v[j] = Col1v[j-1]. 
                    if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
                        #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
                        #The amount must be a number between roundTol
                        rndNum = rd.uniform(10.**(-roundTol), min(min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]), (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3] - Col1v[j])))
                        Col2v[j] -= rndNum/2.
                        Col3v[j] -= rndNum/2.
                        Col1v[j] += rndNum
                        del rndNum                    
        else:
            #Col1v + delta/3 is different than previous value. Make that change.
            delta3 = delta/3.
            Col1v[j] += delta3
            delta -= delta3
            
            #Check that adding delta/2=delta3 to Col2v and Col3v is within their bounds.
            if delta < 0:
                #Need to check lower bounds on Col2v and Col3v by adding delta/2=delta3
                if ((Col2v[j] + delta3) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])):
                    #Set Col2v to its minimum value and distribute the remaining delta to Col3v and Col1v
                    delta += (Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])
                    Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]
                    
                    #The delta here should be distributed to Col1v as well as Col3v.
                    #Check if adding delta3 + remainder to distribute to Col1v and Col3v will make Col3v less than lower bound on Col3v
                    if ((Col3v[j] + delta3 + (delta-delta3)/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])):
                        #Set Col3v to its minimum value and distribute the remaining delta to Col1v.
                        delta += (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])
                        Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2]
                        
                        #The remaining delta must be added to Col1v because Col2v and Col3v are at lower bounds. 
                        #Check if this will make Col1v less than lower bound on Col1v
                        if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])):
                            #Throw an error - the sum of these 3 values must be 1 at this point.
                            sys.exit('PyERROR21: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                        else:
                            Col1v[j] += delta
                            #Check if Col1v[j] still = Col1v[j-1]. 
                            if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
                                #Subtract a random amount from Col1v and add evenly to Col2v and Col3v
                                #The amount must be a number between roundTol
                                rndNum = rd.uniform(10.**(-roundTol), min(min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j])), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])))
                                Col2v[j] += rndNum/2.
                                Col3v[j] += rndNum/2.
                                Col1v[j] -= rndNum
                                del rndNum
                    else:
                        Col3v[j] += (delta3 + (delta-delta3)/2)
                        delta -= (delta3 + (delta-delta3)/2)
                        #This remaining delta should be distributed to Col1v, if possible. Otherwise to Col3v.
                        #First check if Col1v will exceed lower bound if delta is added.
                        if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])):
                            #If so, then check if the previous Col1v value is also equal to the lower bound.
                            if Col1v[j-1] == ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2]:
                                #Both are true, so don't change Col1v[j] first. Instead distribute delta to Col3v.
                                if ((Col3v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])):
                                    #Set Col3v to its minimum value and distribute the remaining delta to Col1v
                                    delta += (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])
                                    Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2]
                                    
                                    #Check if this will make Col1v less than lower bound on Col1v
                                    if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])):
                                        #Throw an error - the sum of these 3 values must be 1 at this point.
                                        sys.exit('PyERROR22: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                                    else:
                                        Col1v[j] += delta
                                        #Check if Col1v[j] = Col1v[j-1]. 
                                        if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
                                            #Subtract a random amount from Col1v and add evenly to Col2v and Col3v
                                            #The amount must be a number between roundTol
                                            rndNum = rd.uniform(10.**(-roundTol), min(min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j])), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])))
                                            Col2v[j] += rndNum/2.
                                            Col3v[j] += rndNum/2.
                                            Col1v[j] -= rndNum
                                            del rndNum
                                else:
                                    #Col3v is not less than its min. Add delta
                                    Col3v[j] += delta
                                    #Check if Col1v[j] = Col1v[j-1]. 
                                    if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
                                        #Subtract a random amount from Col1v and add evenly to Col3v
                                        #The amount must be a number between roundTol
                                        rndNum = rd.uniform(10.**(-roundTol), min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j]), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])))
                                        Col3v[j] += rndNum
                                        Col1v[j] -= rndNum
                                        del rndNum
                            else:
                                #Set Col1v to lower bound. That is okay for Col1v to be different from previous Col1v value
                                #Set Col1v to its minimum value and distribute the remaining delta to Col3v
                                delta += (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])
                                Col1v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2]
                                
                                #delta must be negative, so need to check lower bound on Col3v by adding delta
                                if ((Col3v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])):
                                    #Throw an error - the sum of these 3 values must be 1 at this point.
                                    sys.exit('PyERROR23: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                                else:
                                    #Col3v is not lass than its min. Add delta
                                    Col3v[j] += delta
                        else:
                            #Then check if Col1v in previous time step was equal to this value.
                            if round(Col1v[j] + delta, roundTol) == round(Col1v[j-1], roundTol):
                                #Don't change Col1v[j] first. Instead distribute delta to Col3v.
                                if ((Col3v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])):
                                    #Set Col3v to its minimum value and distribute the remaining delta to Col1v
                                    delta += (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])
                                    Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2]
                                    
                                    #Check if this will make Col1v less than lower bound on Col1v
                                    if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])):
                                        #Throw an error - the sum of these 3 values must be 1 at this point.
                                        sys.exit('PyERROR24: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                                    else:
                                        Col1v[j] += delta
                                        #Check if Col1v[j] = Col1v[j-1]. 
                                        if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
                                            #Subtract a random amount from Col1v and add evenly to Col2v and Col3v
                                            #The amount must be a number between roundTol
                                            rndNum = rd.uniform(10.**(-roundTol), min(min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j])), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])))
                                            Col2v[j] += rndNum/2.
                                            Col3v[j] += rndNum/2.
                                            Col1v[j] -= rndNum
                                            del rndNum
                                else:
                                    #Col3v is not less than its min. Add delta
                                    Col3v[j] += delta
                                    #Check if Col1v[j] = Col1v[j-1]. 
                                    if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
                                        #Subtract a random amount from Col1v and add evenly to Col3v
                                        #The amount must be a number between roundTol
                                        rndNum = rd.uniform(10.**(-roundTol), min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j]), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])))
                                        Col3v[j] += rndNum
                                        Col1v[j] -= rndNum
                                        del rndNum
                            else:
                                #Add delta to Col1v
                                Col1v[j] += delta
                                #Check if Col1v[j] = Col1v[j-1]. 
                                if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
                                    #Subtract a random amount from Col1v and add to Col3v
                                    #The amount must be a number between roundTol
                                    rndNum = rd.uniform(10.**(-roundTol), min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j]), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])))
                                    Col3v[j] += rndNum
                                    Col1v[j] -= rndNum
                                    del rndNum
                            
                #Need to check lower bounds on Col3v by adding delta/2=delta3
                elif ((Col3v[j] + delta/2.) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])):
                    #Set Col3v to its minimum value and distribute the remaining delta to Col2v and Col1v
                    delta += (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])
                    Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2]
                    
                    #The delta here should be distributed to Col1v as well as Col2v.
                    #Check if adding delta3 + remainder to distribute to Col1v and Col2v will make Col2v less than lower bound on Col2v
                    if ((Col2v[j] + delta3 + (delta-delta3)/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])):
                        #Set Col2v to its minimum value and distribute the remaining delta to Col1v.
                        delta += (Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])
                        Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]
                        
                        #The remaining delta must be added to Col1v because Col2v and Col3v are at lower bounds. 
                        #Check if this will make Col1v less than lower bound on Col1v
                        if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])):
                            #Throw an error - the sum of these 3 values must be 1 at this point.
                            sys.exit('PyERROR25: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                        else:
                            #This must be different than previous value if there are several ways to make Col2v + Col3v + Col1v = 1
                            Col1v[j] += delta
                            #Check if Col1v[j] still = Col1v[j-1]. 
                            if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
                                #Subtract a random amount from Col1v and add evenly to Col2v and Col3v
                                #The amount must be a number between roundTol
                                rndNum = rd.uniform(10.**(-roundTol), min(min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j])), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])))
                                Col2v[j] += rndNum/2.
                                Col3v[j] += rndNum/2.
                                Col1v[j] -= rndNum
                                del rndNum
                    else:
                        Col2v[j] += (delta3 + (delta-delta3)/2)
                        delta -= (delta3 + (delta-delta3)/2)
                        #This remaining delta should be distributed to Col1v, if possible. Otherwise to Col2v.
                        #First check if Col1v will exceed lower bound if delta is added.
                        if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])):
                            #If so, then check if the previous Col1v value is also equal to the lower bound.
                            if Col1v[j-1] == ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2]:
                                #Both are true, so don't change Col1v[j] first. Instead distribute delta to Col2v.
                                if ((Col2v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])):
                                    #Set Col2v to its minimum value and distribute the remaining delta to Col1v
                                    delta += (Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])
                                    Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]
                                    
                                    #Check if this will make Col1v less than lower bound on Col1v
                                    if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])):
                                        #Throw an error - the sum of these 3 values must be 1 at this point.
                                        sys.exit('PyERROR26: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                                    else:
                                        #This must be different than lower bound if there are several ways to make Col2v + Col3v + Col1v = 1
                                        Col1v[j] += delta
                                        #Check if Col1v[j] = Col1v[j-1]. 
                                        if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
                                            #Subtract a random amount from Col1v and add evenly to Col3v
                                            #The amount must be a number between roundTol
                                            rndNum = rd.uniform(10.**(-roundTol), min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j]), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])))
                                            Col2v[j] += rndNum
                                            Col1v[j] -= rndNum
                                            del rndNum
                                else:
                                    #Col2v is not less than its min. Add delta
                                    Col2v[j] += delta
                                    #Check if Col1v[j] = Col1v[j-1]. 
                                    if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
                                        #Subtract a random amount from Col1v and add evenly to Col3v
                                        #The amount must be a number between roundTol
                                        rndNum = rd.uniform(10.**(-roundTol), min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j]), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])))
                                        Col2v[j] += rndNum
                                        Col1v[j] -= rndNum
                                        del rndNum
                            else:
                                #Set Col1v to lower bound. That is okay for Col1v to be different from previous Col1v value
                                #Set Col1v to its minimum value and distribute the remaining delta to Col2v
                                delta += (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])
                                Col1v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2]
                                
                                #delta must be negative, so need to check lower bound on Col2v by adding delta
                                if ((Col2v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])):
                                    #Throw an error - the sum of these 3 values must be 1 at this point.
                                    sys.exit('PyERROR27: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                                else:
                                    #Col2v is not lass than its min. Add delta
                                    Col2v[j] += delta
                        else:
                            #Then check if Col1v in previous time step was equal to this value.
                            if round(Col1v[j] + delta, roundTol) == round(Col1v[j-1], roundTol):
                                #Don't change Col1v[j] first. Instead distribute delta to Col2v.
                                if ((Col2v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])):
                                    #Set Col2v to its minimum value and distribute the remaining delta to Col1v
                                    delta += (Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])
                                    Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]
                                    
                                    #Check if this will make Col1v less than lower bound on Col1v
                                    if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])):
                                        #Throw an error - the sum of these 3 values must be 1 at this point.
                                        sys.exit('PyERROR28: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                                    else:
                                        #This must be different than previous step
                                        Col1v[j] += delta
                                else:
                                    #Col2v is not less than its min. Add delta
                                    Col2v[j] += delta
                                    #Check if Col1v[j] = Col1v[j-1]. 
                                    if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
                                        #Subtract a random amount from Col1v and add evenly to Col3v
                                        #The amount must be a number between roundTol
                                        rndNum = rd.uniform(10.**(-roundTol), min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j]), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])))
                                        Col2v[j] += rndNum
                                        Col1v[j] -= rndNum
                                        del rndNum
                            else:
                                #Add delta to Col1v
                                Col1v[j] += delta
                                #Check if Col1v[j] = Col1v[j-1]. 
                                if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
                                    #Subtract a random amount from Col1v and add evenly to Col3v
                                    #The amount must be a number between roundTol
                                    rndNum = rd.uniform(10.**(-roundTol), min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j]), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])))
                                    Col2v[j] += rndNum
                                    Col1v[j] -= rndNum
                                    del rndNum
                else:
                    #Col3v and Col2v both not exceeding their min. Add delta/2
                    Col3v[j] += delta/2.
                    Col2v[j] += delta/2.
                    #The Col1v value in this scenario is definitely different than previous.
            
            else:
                #Need to check upper bounds on Col2v and Col3v by adding delta/2=delta3
                if ((Col2v[j] + delta3) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3])):
                    #Set Col2v to its maximum value and distribute the remaining delta to Col3v and Col1v
                    delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j])
                    Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3]
                    
                    #The delta here should be distributed to Col1v as well as Col3v.
                    #Check if adding delta3 + remainder to distribute to Col1v and Col3v will make Col3v greater than upper bound on Col3v
                    if ((Col3v[j] + delta3 + (delta-delta3)/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3])):
                        #Set Col3v to its minimum value and distribute the remaining delta to Col1v.
                        delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j])
                        Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3]
                        
                        #The remaining delta must be added to Col1v because Col2v and Col3v are at lower bounds. 
                        #Check if this will make Col1v less than lower bound on Col1v
                        if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3])):
                            #Throw an error - the sum of these 3 values must be 1 at this point.
                            sys.exit('PyERROR29: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                        else:
                            Col1v[j] += delta
                            #Check if Col1v[j] still = Col1v[j-1]. 
                            if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
                                #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
                                #The amount must be a number between roundTol
                                rndNum = rd.uniform(10.**(-roundTol), min(min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]), (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3] - Col1v[j])))
                                Col2v[j] -= rndNum/2.
                                Col3v[j] -= rndNum/2.
                                Col1v[j] += rndNum
                                del rndNum
                    else:
                        Col3v[j] += (delta3 + (delta-delta3)/2)
                        delta -= (delta3 + (delta-delta3)/2)
                        #This remaining delta should be distributed to Col1v, if possible. Otherwise to Col3v.
                        #First check if Col1v will exceed upper bound if delta is added.
                        if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3])):
                            #If so, then check if the previous Col1v value is also equal to the lower bound.
                            if Col1v[j-1] == ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3]:
                                #Both are true, so don't change Col1v[j] first. Instead distribute delta to Col3v.
                                if ((Col3v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3])):
                                    #Set Col3v to its minimum value and distribute the remaining delta to Col1v
                                    delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j])
                                    Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3]
                                    
                                    #Check if this will make Col1v less than lower bound on Col1v
                                    if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3])):
                                        #Throw an error - the sum of these 3 values must be 1 at this point.
                                        sys.exit('PyERROR30: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                                    else:
                                        Col1v[j] += delta
                                        #Check if Col1v[j] = Col1v[j-1]. 
                                        if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
                                            #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
                                            #The amount must be a number between roundTol
                                            rndNum = rd.uniform(10.**(-roundTol), min(min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]), (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3] - Col1v[j])))
                                            Col2v[j] -= rndNum/2.
                                            Col3v[j] -= rndNum/2.
                                            Col1v[j] += rndNum
                                            del rndNum
                                else:
                                    #Col3v is not greater than its max. Add delta
                                    Col3v[j] += delta
                                    #Check if Col1v[j] = Col1v[j-1]. 
                                    if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
                                        #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
                                        #The amount must be a number between roundTol
                                        rndNum = rd.uniform(10.**(-roundTol), min((Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3] - Col1v[j])))
                                        Col3v[j] -= rndNum
                                        Col1v[j] += rndNum
                                        del rndNum
                            else:
                                #Set Col1v to upper bound. That is okay for Col1v to be different from previous Col1v value
                                #Set Col1v to its maximum value and distribute the remaining delta to Col3v
                                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3] - Col1v[j])
                                Col1v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3]
                                
                                #delta must be negative, so need to check lower bound on Col3v by adding delta
                                if ((Col3v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3])):
                                    #Throw an error - the sum of these 3 values must be 1 at this point.
                                    sys.exit('PyERROR31: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                                else:
                                    #Col3v is not lass than its min. Add delta
                                    Col3v[j] += delta
                        else:
                            #Then check if Col1v in previous time step was equal to this value.
                            if round(Col1v[j] + delta, roundTol) == round(Col1v[j-1], roundTol):
                                #Don't change Col1v[j] first. Instead distribute delta to Col3v.
                                if ((Col3v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3])):
                                    #Set Col3v to its minimum value and distribute the remaining delta to Col1v
                                    delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j])
                                    Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3]
                                    
                                    #Check if this will make Col1v less than lower bound on Col1v
                                    if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3])):
                                        #Throw an error - the sum of these 3 values must be 1 at this point.
                                        sys.exit('PyERROR32: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                                    else:
                                        Col1v[j] += delta
                                        #Check if Col1v[j] = Col1v[j-1]. 
                                        if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
                                            #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
                                            #The amount must be a number between roundTol
                                            rndNum = rd.uniform(10.**(-roundTol), min(min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]), (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3] - Col1v[j])))
                                            Col2v[j] -= rndNum/2.
                                            Col3v[j] -= rndNum/2.
                                            Col1v[j] += rndNum
                                            del rndNum
                                else:
                                    #Col3v is not less than its min. Add delta
                                    Col3v[j] += delta
                                    #Check if Col1v[j] = Col1v[j-1]. 
                                    if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
                                        #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
                                        #The amount must be a number between roundTol
                                        rndNum = rd.uniform(10.**(-roundTol), min((Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3] - Col1v[j])))
                                        Col3v[j] -= rndNum
                                        Col1v[j] += rndNum
                                        del rndNum
                            else:
                                #Add delta to Col1v
                                Col1v[j] += delta
                                #Check if Col1v[j] = Col1v[j-1]. 
                                if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
                                    #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
                                    #The amount must be a number between roundTol
                                    rndNum = rd.uniform(10.**(-roundTol), min((Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3] - Col1v[j])))
                                    Col3v[j] -= rndNum
                                    Col1v[j] += rndNum
                                    del rndNum
                            
                #Need to check lower bounds on Col3v by adding delta/2=delta3
                elif ((Col3v[j] + delta/2.) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3])):
                    #Set Col3v to its minimum value and distribute the remaining delta to Col2v and Col1v
                    delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j])
                    Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3]
                    
                    #The delta here should be distributed to Col1v as well as Col2v.
                    #Check if adding delta3 + remainder to distribute to Col1v and Col2v will make Col2v less than lower bound on Col2v
                    if ((Col2v[j] + delta3 + (delta-delta3)/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3])):
                        #Set Col2v to its maximum value and distribute the remaining delta to Col1v.
                        delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j])
                        Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3]
                        
                        #The remaining delta must be added to Col1v because Col2v and Col3v are at upper bounds. 
                        #Check if this will make Col1v greater than upper bound on Col1v
                        if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3])):
                            #Throw an error - the sum of these 3 values must be 1 at this point.
                            sys.exit('PyERROR33: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                        else:
                            #This must be different than previous value if there are several ways to make Col2v + Col3v + Col1v = 1
                            Col1v[j] += delta
                            #Check if Col1v[j] = Col1v[j-1]. 
                            if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
                                #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
                                #The amount must be a number between roundTol
                                rndNum = rd.uniform(10.**(-roundTol), min(min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]), (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3] - Col1v[j])))
                                Col2v[j] -= rndNum/2.
                                Col3v[j] -= rndNum/2.
                                Col1v[j] += rndNum
                                del rndNum
                    else:
                        Col2v[j] += (delta3 + (delta-delta3)/2)
                        delta -= (delta3 + (delta-delta3)/2)
                        #This remaining delta should be distributed to Col1v, if possible. Otherwise to Col2v.
                        #First check if Col1v will exceed lower bound if delta is added.
                        if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3])):
                            #If so, then check if the previous Col1v value is also equal to the lower bound.
                            if Col1v[j-1] == ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3]:
                                #Both are true, so don't change Col1v[j] first. Instead distribute delta to Col2v.
                                if ((Col2v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3])):
                                    #Set Col2v to its minimum value and distribute the remaining delta to Col1v
                                    delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j])
                                    Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3]
                                    
                                    #Check if this will make Col1v less than lower bound on Col1v
                                    if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3])):
                                        #Throw an error - the sum of these 3 values must be 1 at this point.
                                        sys.exit('PyERROR34: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                                    else:
                                        #This must be different than lower bound if there are several ways to make Col2v + Col3v + Col1v = 1
                                        Col1v[j] += delta
                                        #Check if Col1v[j] = Col1v[j-1]. 
                                        if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
                                            #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
                                            #The amount must be a number between roundTol
                                            rndNum = rd.uniform(10.**(-roundTol), min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3] - Col1v[j])))
                                            Col2v[j] -= rndNum
                                            Col1v[j] += rndNum
                                            del rndNum
                                else:
                                    #Col2v is not less than its min. Add delta
                                    Col2v[j] += delta
                                    #Check if Col1v[j] = Col1v[j-1]. 
                                    if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
                                        #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
                                        #The amount must be a number between roundTol
                                        rndNum = rd.uniform(10.**(-roundTol), min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3] - Col1v[j])))
                                        Col2v[j] -= rndNum
                                        Col1v[j] += rndNum
                                        del rndNum
                            else:
                                #Set Col1v to lower bound. That is okay for Col1v to be different from previous Col1v value
                                #Set Col1v to its minimum value and distribute the remaining delta to Col2v
                                delta += (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3])
                                Col1v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3]
                                
                                #delta must be negative, so need to check lower bound on Col2v by adding delta
                                if ((Col2v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3])):
                                    #Throw an error - the sum of these 3 values must be 1 at this point.
                                    sys.exit('PyERROR35: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                                else:
                                    #Col2v is not lass than its min. Add delta
                                    Col2v[j] += delta
                        else:
                            #Then check if Col1v in previous time step was equal to this value.
                            if round(Col1v[j] + delta, roundTol) == round(Col1v[j-1], roundTol):
                                #Don't change Col1v[j] first. Instead distribute delta to Col2v.
                                if ((Col2v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3])):
                                    #Set Col2v to its minimum value and distribute the remaining delta to Col1v
                                    delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j])
                                    Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3]
                                    
                                    #Check if this will make Col1v less than lower bound on Col1v
                                    if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3])):
                                        #Throw an error - the sum of these 3 values must be 1 at this point.
                                        sys.exit('PyERROR36: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
                                    else:
                                        #This must be different than previous step
                                        Col1v[j] += delta
                                else:
                                    #Col2v is not less than its min. Add delta
                                    Col2v[j] += delta
                                    #Check if Col1v[j] = Col1v[j-1]. 
                                    if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
                                        #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
                                        #The amount must be a number between roundTol
                                        rndNum = rd.uniform(10.**(-roundTol), min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3] - Col1v[j])))
                                        Col2v[j] -= rndNum
                                        Col1v[j] += rndNum
                                        del rndNum
                            else:
                                #Add delta to Col1v
                                Col1v[j] += delta
                                #Check if Col1v[j] = Col1v[j-1]. 
                                if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
                                    #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
                                    #The amount must be a number between roundTol
                                    rndNum = rd.uniform(10.**(-roundTol), min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3] - Col1v[j])))
                                    Col2v[j] -= rndNum
                                    Col1v[j] += rndNum
                                    del rndNum
                else:
                    #Col3v and Col2v both not exceeding their min. Add delta/2
                    Col3v[j] += delta/2.
                    Col2v[j] += delta/2.
                    #The Col1v value in this scenario is definitely different than previous.   
            del delta3
            
    return Col1v, Col2v, Col3v

#Original Function:
#            #Check if adding delta/3 will make Col1v greater than upper bound on Col1v
#            if ((Col1v[j] + delta/3) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3])):
#                #If so, then check if the previous Col1v value is also equal to the upper bound.
#                if Col1v[j-1] == ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3]:
#                    #Both are true, so don't change Col1v[j] first. Instead distribute delta/2 to Col2v and Col3v.
#                    #delta must be positive, so need to check upper bounds on Col2v and Col3v by adding delta/2
#                    if ((Col2v[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3])):
#                        #Set Col2v to its maximum value and distribute the remaining delta to Col3v
#                        delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Col2v[j])
#                        Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3]
#                        
#                        #Check if this will make Col3v greater than upper bound on Col3v
#                        if ((Col3v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3])):
#                            #Set Col3v to its maximum value and distribute the remaining delta to Col1v.
#                            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Col3v[j])
#                            Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3]
#                            
#                            #Check if this will make Col1v greater than upper bound on Col1v
#                            if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3])):
#                                #Throw an error - the sum of these 3 values must be 1 at this point.
#                                sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                            else:
#                                #This must be different than upper bound if there are several ways to make Col2v + Col3v + Col1v = 1
#                                #Check if Col1v[j] = upper bound of Col1v. 
#                                if Col1v[j] == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3]):
#                                    #Throw an error - the sum of these 3 values requires max of all 3 variables.
#                                    sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                                else:
#                                    Col1v[j] += delta
#                                    if round(Col1v[j], roundTol) == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3]):
#                                        #Throw an error - the sum of these 3 values requires max of all 3 variables.
#                                        sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v can only be 1 when all are at their max for Replicate = %s' % str(j))
#                        else:
#                            Col3v[j] += delta
#                            #delta has been fully distributed.
#                            #Check if Col1v[j] = upper bound of Col1v. 
#                            if Col1v[j] == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3]):
#                                #Subtract a random amount from Col1v and add it to Col3v
#                                #The amount must be a number between 0 and min((max(Col3v) - Col3v[j]), (Col1v[j] - min(Col1v)))
#                                rndNum = rd.uniform(10.**(-roundTol), min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Col3v[j]), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])))
#                                Col3v[j] += rndNum
#                                Col1v[j] -= rndNum
#                                del rndNum
#                            
#                    elif ((Col3v[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3])):
#                        #Set Col3v to its maximum value and distribute the remaining delta to Col1v
#                        delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Col3v[j])
#                        Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3]
#                        
#                        #Check if this will make Col2v greater than upper bound on Col2v
#                        if ((Col2v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3])):
#                            #Set Col2v to its maximum value and distribute the remaining delta to Col1v.
#                            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Col2v[j])
#                            Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3]
#                            
#                            #Check if this will make Col1v greater than upper bound on Col1v
#                            if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3])):
#                                #Throw an error - the sum of these 3 values must be 1 at this point.
#                                sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                            else:
#                                #This must be different than upper bound if there are several ways to make Col2v + Col3v + Col1v = 1
#                                #Check if Col1v[j] = upper bound of Col1v. 
#                                if Col1v[j] == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3]):
#                                    #Throw an error - the sum of these 3 values requires max of all 3 variables.
#                                    sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                                else:
#                                    Col1v[j] += delta
#                                    if round(Col1v[j], roundTol) == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3]):
#                                        #Throw an error - the sum of these 3 values requires max of all 3 variables.
#                                        sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v can only be 1 when all are at their max for Replicate = %s' % str(j))
#                        else:
#                            Col2v[j] += delta
#                            #delta has been fully distributed.
#                            #Check if Col1v[j] = upper bound of Col1v. 
#                            if Col1v[j] == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3]):
#                                #Subtract a random amount from Col1v and add it to Col2v
#                                #The amount must be a number between 0 and min((max(Col2v) - Col2v[j]), (Col1v[j] - min(Col1v)))
#                                rndNum = rd.uniform(10.**(-roundTol), min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Col2v[j]), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])))
#                                Col2v[j] += rndNum
#                                Col1v[j] -= rndNum
#                                del rndNum
#                    else:
#                        #Col3v and Col2v both not exceeding their max. Add delta/2
#                        Col3v[j] += delta/2
#                        Col2v[j] += delta/2
#                        #delta has been fully distributed.
#                        #Check if Col1v[j] = upper bound of Col1v. 
#                        if Col1v[j] == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3]):
#                            #Subtract a random amount from Col1v and add it to Col3v and Col2v
#                            #The amount must be a number between roundTol and min((max(Col3v) - Col3v[j]),(max(Col2v) - Col2v[j]), (Col1v[j] - min(Col1v)))
#                            rndNum = rd.uniform(10.**(-roundTol), min(min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Col3v[j]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Col2v[j])), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])))
#                            Col2v[j] += rndNum/2
#                            Col3v[j] += rndNum/2
#                            Col1v[j] -= rndNum
#                            del rndNum
#                else:
#                    #Set Col1v to upper bound. That is okay for Col1v to be different from previous Col1v value
#                    #Set Col1v to its maximum value and distribute the remaining delta to Col3v and Col2v
#                    delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3] - Col1v[j])
#                    Col1v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3]
#                    
#                    #delta must be positive, so need to check upper bounds on Col2v and Col3v by adding delta/2
#                    if ((Col2v[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3])):
#                        #Set Col2v to its maximum value and distribute the remaining delta to Col3v
#                        delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Col2v[j])
#                        Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3]
#                        
#                        #Check if this will make Col3v greater than upper bound on Col3v
#                        if ((Col3v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3])):
#                            #Throw an error - the sum of these 3 values must be 1 at this point.
#                            sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                        else:
#                            Col3v[j] += delta
#                    elif ((Col3v[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3])):
#                        #Set Col3v to its maximum value and distribute the remaining delta to Col1v
#                        delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Col3v[j])
#                        Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3]
#                        
#                        #Check if this will make Col2v greater than upper bound on Col2v
#                        if ((Col2v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3])):
#                            #Throw an error - the sum of these 3 values must be 1 at this point.
#                            sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                        else:
#                            Col2v[j] += delta
#                    else:
#                        #Col3v and Col2v both not exceeding their max. Add delta/2
#                        Col3v[j] += delta/2
#                        Col2v[j] += delta/2
#                    
#            #Check if adding delta/3 will make Col1v less than lower bound on Col1v
#            elif ((Col1v[j] + delta/3) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])):
#                #If so, then check if the previous Col1v value is also equal to the lower bound.
#                if Col1v[j-1] == ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2]:
#                    #Both are true, so don't change Col1v[j] first. Instead distribute delta/2 to Col2v and Col3v.
#                    #delta must be negative, so need to check lower bounds on Col2v and Col3v by adding delta/2
#                    if ((Col2v[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])):
#                        #Set Col2v to its minimum value and distribute the remaining delta to Col3v
#                        delta += (Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])
#                        Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]
#                        
#                        #Check if this will make Col3v less than lower bound on Col3v
#                        if ((Col3v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])):
#                            #Set Col3v to its minimum value and distribute the remaining delta to Col1v.
#                            delta += (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])
#                            Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2]
#                            
#                            #Check if this will make Col1v less than lower bound on Col1v
#                            if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])):
#                                #Throw an error - the sum of these 3 values must be 1 at this point.
#                                sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                            else:
#                                #This must be different than lower bound if there are several ways to make Col2v + Col3v + Col1v = 1
#                                #Check if Col1v[j] = lower bound of Col1v. 
#                                if Col1v[j] == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2]):
#                                    #Throw an error - the sum of these 3 values requires min of all 3 variables.
#                                    sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                                else:
#                                    Col1v[j] += delta
#                                    if round(Col1v[j], roundTol) == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2]):
#                                        #Throw an error - the sum of these 3 values requires min of all 3 variables.
#                                        sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v can only be 1 when all are at their min for Replicate = %s' % str(j))
#                        else:
#                            Col3v[j] += delta
#                            #delta has been fully distributed.
#                            #Check if Col1v[j] = lower bound of Col1v. 
#                            if Col1v[j] == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2]):
#                                #Add a random amount to Col1v and subtract it from Col3v
#                                #The amount must be a number between roundTol and min((Col3v[j] - min(Col3v)), (max(Col1v) - Col1v))
#                                rndNum = rd.uniform(10.**(-roundTol), min((Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3] - Col1v[j])))
#                                Col3v[j] -= rndNum
#                                Col1v[j] += rndNum
#                                del rndNum
#                            
#                    elif ((Col3v[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])):
#                        #Set Col3v to its minimum value and distribute the remaining delta to Col1v
#                        delta += (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])
#                        Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2]
#                        
#                        #Check if this will make Col2v less than lower bound on Col2v
#                        if ((Col2v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])):
#                            #Set Col2v to its minimum value and distribute the remaining delta to Col1v.
#                            delta += (Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])
#                            Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]
#                            
#                            #Check if this will make Col1v less than lower bound on Col1v
#                            if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])):
#                                #Throw an error - the sum of these 3 values must be 1 at this point.
#                                sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                            else:
#                                #This must be different than lower bound if there are several ways to make Col2v + Col3v + Col1v = 1
#                                #Check if Col1v[j] = lower bound of Col1v. 
#                                if Col1v[j] == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2]):
#                                    #Throw an error - the sum of these 3 values requires min of all 3 variables.
#                                    sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                                else:
#                                    Col1v[j] += delta
#                                    if round(Col1v[j], roundTol) == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2]):
#                                        #Throw an error - the sum of these 3 values requires min of all 3 variables.
#                                        sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v can only be 1 when all are at their min for Replicate = %s' % str(j))
#                        else:
#                            Col2v[j] += delta
#                            #delta has been fully distributed.
#                            #Check if Col1v[j] = lower bound of Col1v. 
#                            if Col1v[j] == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2]):
#                                #Add a random amount to Col1v and subtract it from Col2v
#                                #The amount must be a number between roundTol and min((Col2v[j] - min(Col2v)), (max(Col1v) - Col1v))
#                                rndNum = rd.uniform(10.**(-roundTol), min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3] - Col1v[j])))
#                                Col2v[j] -= rndNum
#                                Col1v[j] += rndNum
#                                del rndNum
#                    else:
#                        #Col3v and Col2v both not exceeding their max. Add delta/2
#                        Col3v[j] += delta/2
#                        Col2v[j] += delta/2
#                        #delta has been fully distributed.
#                        #Check if Col1v[j] = lower bound of Col1v. 
#                        if Col1v[j] == (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2]):
#                            #Add a random amount to Col1v and subtract it from Col2v
#                            #The amount must be a number between roundTol and min((Col3v[j] - min(Col3v)), (Col2v[j] - min(Col2v)), (max(Col1v) - Col1v))
#                            rndNum = rd.uniform(10.**(-roundTol), min(min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]), (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3] - Col1v[j])))
#                            Col2v[j] -= rndNum/2
#                            Col3v[j] -= rndNum/2
#                            Col1v[j] += rndNum
#                            del rndNum
#                else:
#                    #Set Col1v to lower bound. That is okay for Col1v to be different from previous Col1v value
#                    #Set Col1v to its minimum value and distribute the remaining delta to Col3v and Col2v
#                    delta += (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])
#                    Col1v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2]
#                    
#                    #delta must be negative, so need to check lower bounds on Col2v and Col3v by adding delta/2
#                    if ((Col2v[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])):
#                        #Set Col2v to its minimum value and distribute the remaining delta to Col3v
#                        delta += (Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])
#                        Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]
#                        
#                        #Check if this will make Col3v less than lower bound on Col3v
#                        if ((Col3v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])):
#                            #Throw an error - the sum of these 3 values must be 1 at this point.
#                            sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                        else:
#                            Col3v[j] += delta
#                    elif ((Col3v[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])):
#                        #Set Col3v to its minimum value and distribute the remaining delta to Col1v
#                        delta += (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])
#                        Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2]
#                        
#                        #Check if this will make Col2v less than lower bound on Col2v
#                        if ((Col2v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])):
#                            #Throw an error - the sum of these 3 values must be 1 at this point.
#                            sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                        else:
#                            Col2v[j] += delta
#                    else:
#                        #Col3v and Col2v both not less than their min. Add delta/2
#                        Col3v[j] += delta/2
#                        Col2v[j] += delta/2
#            
#            else:
#                #Changing Col1v is within bounds. Now need to check that adding delta/3 will not be equal to previous Col1v value
#                if (round(Col1v[j] + delta/3, roundTol) == round(Col1v[j-1], roundTol)):
#                    #Don't change Col1v[j] first. Instead distribute delta/2 to Col2v and Col3v.
#                    if delta < 0:
#                        #Need to check lower bounds on Col2v and Col3v by adding delta/2
#                        if ((Col2v[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])):
#                            #Set Col2v to its minimum value and distribute the remaining delta to Col3v
#                            delta += (Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])
#                            Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]
#                            
#                            #Check if this will make Col3v less than lower bound on Col3v
#                            if ((Col3v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])):
#                                #Set Col3v to its minimum value and distribute the remaining delta to Col1v.
#                                delta += (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])
#                                Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2]
#                                
#                                #Check if this will make Col1v less than lower bound on Col1v
#                                if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])):
#                                    #Throw an error - the sum of these 3 values must be 1 at this point.
#                                    sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                                else:
#                                    Col1v[j] += delta
#                                    #Check if Col1v[j] still = Col1v[j-1]. 
#                                    if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
#                                        #Subtract a random amount from Col1v and add evenly to Col2v and Col3v
#                                        #The amount must be a number between roundTol
#                                        rndNum = rd.uniform(10.**(-roundTol), min(min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Col2v[j]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Col3v[j])), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])))
#                                        Col2v[j] += rndNum/2
#                                        Col3v[j] += rndNum/2
#                                        Col1v[j] -= rndNum
#                                        del rndNum
#                            else:
#                                Col3v[j] += delta
#                                #delta has been fully distributed.
#                                #Check if Col1v[j] = Col1v[j-1]
#                                if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
#                                    #Subtract a random amount from Col1v and add it to Col3v
#                                    #The amount must be a number between roundTol and min((Col3v[j] - min(Col3v)), (max(Col1v) - Col1v))
#                                    rndNum = rd.uniform(10.**(-roundTol), min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Col3v[j]), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])))
#                                    Col3v[j] += rndNum
#                                    Col1v[j] -= rndNum
#                                    del rndNum
#                                
#                        elif ((Col3v[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])):
#                            #Set Col3v to its minimum value and distribute the remaining delta to Col2v
#                            delta += (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])
#                            Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2]
#                            
#                            #Check if this will make Col2v less than lower bound on Col2v
#                            if ((Col2v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])):
#                                #Set Col2v to its minimum value and distribute the remaining delta to Col1v.
#                                delta += (Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])
#                                Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]
#                                
#                                #Check if this will make Col1v less than lower bound on Col1v
#                                if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])):
#                                    #Throw an error - the sum of these 3 values must be 1 at this point.
#                                    sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                                else:
#                                    Col1v[j] += delta
#                                    #Check if Col1v[j] still = Col1v[j-1]. 
#                                    if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
#                                        #Subtract a random amount from Col1v and add evenly to Col2v and Col3v
#                                        #The amount must be a number between roundTol
#                                        rndNum = rd.uniform(10.**(-roundTol), min(min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Col2v[j]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Col3v[j])), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])))
#                                        Col2v[j] += rndNum/2
#                                        Col3v[j] += rndNum/2
#                                        Col1v[j] -= rndNum
#                                        del rndNum                                        
#                            else:
#                                Col2v[j] += delta
#                                #delta has been fully distributed.
#                                #Check if Col1v[j] = Col1v[j-1]
#                                if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
#                                    #Subtract a random amount from Col1v and add it to Col2v
#                                    #The amount must be a number between roundTol and min((Col2v[j] - min(Col2v)), (max(Col1v) - Col1v))
#                                    rndNum = rd.uniform(10.**(-roundTol), min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Col2v[j]), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])))
#                                    Col2v[j] += rndNum
#                                    Col1v[j] -= rndNum
#                                    del rndNum
#                        else:
#                            #Col3v and Col2v both not exceeding their min. Add delta/2
#                            Col3v[j] += delta/2
#                            Col2v[j] += delta/2
#                            #delta has been fully distributed.
#                            #Check if Col1v[j] = Col1v[j-1]. 
#                            if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
#                                #Subtract a random amount from Col1v and add evenly to Col2v and Col3v
#                                #The amount must be a number between roundTol
#                                rndNum = rd.uniform(10.**(-roundTol), min(min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Col2v[j]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Col3v[j])), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])))
#                                Col2v[j] += rndNum/2
#                                Col3v[j] += rndNum/2
#                                Col1v[j] -= rndNum
#                                del rndNum
#                    
#                    else:
#                        #Need to check upper bounds on Col2v and Col3v by adding delta/2
#                        if ((Col2v[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3])):
#                            #Set Col2v to its maximum value and distribute the remaining delta to Col3v
#                            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Col2v[j])
#                            Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3]
#                            
#                            #Check if this will make Col3v greater than upper bound on Col3v
#                            if ((Col3v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3])):
#                                #Set Col3v to its maximum value and distribute the remaining delta to Col1v.
#                                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Col3v[j])
#                                Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3]
#                                
#                                #Check if this will make Col1v greater than upper bound on Col1v
#                                if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3])):
#                                    #Throw an error - the sum of these 3 values must be 1 at this point.
#                                    sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                                else:
#                                    Col1v[j] += delta
#                                    #Check if Col1v[j] still = Col1v[j-1]. 
#                                    if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
#                                        #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
#                                        #The amount must be a number between roundTol
#                                        rndNum = rd.uniform(10.**(-roundTol), min(min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]), (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3] - Col1v[j])))
#                                        Col2v[j] -= rndNum/2
#                                        Col3v[j] -= rndNum/2
#                                        Col1v[j] += rndNum
#                                        del rndNum
#                            else:
#                                Col3v[j] += delta
#                                #delta has been fully distributed.
#                                #Check if Col1v[j] = Col1v[j-1]
#                                if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
#                                    #Add a random amount to Col1v and subtract from Col3v
#                                    #The amount must be a number between roundTol and min((Col3v[j] - min(Col3v)), (max(Col1v) - Col1v))
#                                    rndNum = rd.uniform(10.**(-roundTol), min((Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3] - Col1v[j])))
#                                    Col3v[j] -= rndNum
#                                    Col1v[j] += rndNum
#                                    del rndNum
#                                
#                        elif ((Col3v[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3])):
#                            #Set Col3v to its maximum value and distribute the remaining delta to Col2v
#                            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Col3v[j])
#                            Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3]
#                            
#                            #Check if this will make Col2v greater than upper bound on Col2v
#                            if ((Col2v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3])):
#                                #Set Col2v to its maximum value and distribute the remaining delta to Col1v.
#                                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Col2v[j])
#                                Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3]
#                                
#                                #Check if this will make Col1v greater than upper bound on Col1v
#                                if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3])):
#                                    #Throw an error - the sum of these 3 values must be 1 at this point.
#                                    sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                                else:
#                                    Col1v[j] += delta
#                                    #Check if Col1v[j] still = Col1v[j-1]. 
#                                    if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
#                                        #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
#                                        #The amount must be a number between roundTol
#                                        rndNum = rd.uniform(10.**(-roundTol), min(min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]), (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3] - Col1v[j])))
#                                        Col2v[j] -= rndNum/2
#                                        Col3v[j] -= rndNum/2
#                                        Col1v[j] += rndNum
#                                        del rndNum
#                            else:
#                                Col2v[j] += delta
#                                #delta has been fully distributed.
#                                #Check if Col1v[j] = Col1v[j-1]
#                                if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
#                                    #Add a random amount to Col1v and subtract from Col2v
#                                    #The amount must be a number between roundTol and min((Col2v[j] - min(Col2v)), (max(Col1v) - Col1v))
#                                    rndNum = rd.uniform(10.**(-roundTol), min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3] - Col1v[j])))
#                                    Col2v[j] -= rndNum
#                                    Col1v[j] += rndNum
#                                    del rndNum
#                        else:
#                            #Col3v and Col2v both not exceeding their min. Add delta/2
#                            Col3v[j] += delta/2
#                            Col2v[j] += delta/2
#                            #delta has been fully distributed.
#                            #Check if Col1v[j] = Col1v[j-1]. 
#                            if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
#                                #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
#                                #The amount must be a number between roundTol
#                                rndNum = rd.uniform(10.**(-roundTol), min(min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]), (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3] - Col1v[j])))
#                                Col2v[j] -= rndNum/2
#                                Col3v[j] -= rndNum/2
#                                Col1v[j] += rndNum
#                                del rndNum                    
#                else:
#                    #Col1v + delta/3 is different than previous value. Make that change.
#                    delta3 = delta/3
#                    Col1v[j] += delta3
#                    delta -= delta3
#                    
#                    #Check that adding delta/2=delta3 to Col2v and Col3v is within their bounds.
#                    if delta < 0:
#                        #Need to check lower bounds on Col2v and Col3v by adding delta/2=delta3
#                        if ((Col2v[j] + delta3) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])):
#                            #Set Col2v to its minimum value and distribute the remaining delta to Col3v and Col1v
#                            delta += (Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])
#                            Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]
#                            
#                            #The delta here should be distributed to Col1v as well as Col3v.
#                            #Check if adding delta3 + remainder to distribute to Col1v and Col3v will make Col3v less than lower bound on Col3v
#                            if ((Col3v[j] + delta3 + (delta-delta3)/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])):
#                                #Set Col3v to its minimum value and distribute the remaining delta to Col1v.
#                                delta += (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])
#                                Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2]
#                                
#                                #The remaining delta must be added to Col1v because Col2v and Col3v are at lower bounds. 
#                                #Check if this will make Col1v less than lower bound on Col1v
#                                if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])):
#                                    #Throw an error - the sum of these 3 values must be 1 at this point.
#                                    sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                                else:
#                                    Col1v[j] += delta
#                                    #Check if Col1v[j] still = Col1v[j-1]. 
#                                    if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
#                                        #Subtract a random amount from Col1v and add evenly to Col2v and Col3v
#                                        #The amount must be a number between roundTol
#                                        rndNum = rd.uniform(10.**(-roundTol), min(min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Col2v[j]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Col3v[j])), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])))
#                                        Col2v[j] += rndNum/2
#                                        Col3v[j] += rndNum/2
#                                        Col1v[j] -= rndNum
#                                        del rndNum
#                            else:
#                                Col3v[j] += (delta3 + (delta-delta3)/2)
#                                delta -= (delta3 + (delta-delta3)/2)
#                                #This remaining delta should be distributed to Col1v, if possible. Otherwise to Col3v.
#                                #First check if Col1v will exceed lower bound if delta is added.
#                                if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])):
#                                    #If so, then check if the previous Col1v value is also equal to the lower bound.
#                                    if Col1v[j-1] == ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2]:
#                                        #Both are true, so don't change Col1v[j] first. Instead distribute delta to Col3v.
#                                        if ((Col3v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])):
#                                            #Set Col3v to its minimum value and distribute the remaining delta to Col1v
#                                            delta += (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])
#                                            Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2]
#                                            
#                                            #Check if this will make Col1v less than lower bound on Col1v
#                                            if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])):
#                                                #Throw an error - the sum of these 3 values must be 1 at this point.
#                                                sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                                            else:
#                                                Col1v[j] += delta
#                                                #Check if Col1v[j] = Col1v[j-1]. 
#                                                if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
#                                                    #Subtract a random amount from Col1v and add evenly to Col2v and Col3v
#                                                    #The amount must be a number between roundTol
#                                                    rndNum = rd.uniform(10.**(-roundTol), min(min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Col2v[j]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Col3v[j])), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])))
#                                                    Col2v[j] += rndNum/2
#                                                    Col3v[j] += rndNum/2
#                                                    Col1v[j] -= rndNum
#                                                    del rndNum
#                                        else:
#                                            #Col3v is not less than its min. Add delta
#                                            Col3v[j] += delta
#                                            #Check if Col1v[j] = Col1v[j-1]. 
#                                            if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
#                                                #Subtract a random amount from Col1v and add evenly to Col3v
#                                                #The amount must be a number between roundTol
#                                                rndNum = rd.uniform(10.**(-roundTol), min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Col3v[j]), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])))
#                                                Col3v[j] += rndNum
#                                                Col1v[j] -= rndNum
#                                                del rndNum
#                                    else:
#                                        #Set Col1v to lower bound. That is okay for Col1v to be different from previous Col1v value
#                                        #Set Col1v to its minimum value and distribute the remaining delta to Col3v
#                                        delta += (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])
#                                        Col1v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2]
#                                        
#                                        #delta must be negative, so need to check lower bound on Col3v by adding delta
#                                        if ((Col3v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])):
#                                            #Throw an error - the sum of these 3 values must be 1 at this point.
#                                            sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                                        else:
#                                            #Col3v is not lass than its min. Add delta
#                                            Col3v[j] += delta
#                                else:
#                                    #Then check if Col1v in previous time step was equal to this value.
#                                    if round(Col1v[j] + delta, roundTol) == round(Col1v[j-1], roundTol):
#                                        #Don't change Col1v[j] first. Instead distribute delta to Col3v.
#                                        if ((Col3v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])):
#                                            #Set Col3v to its minimum value and distribute the remaining delta to Col1v
#                                            delta += (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])
#                                            Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2]
#                                            
#                                            #Check if this will make Col1v less than lower bound on Col1v
#                                            if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])):
#                                                #Throw an error - the sum of these 3 values must be 1 at this point.
#                                                sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                                            else:
#                                                Col1v[j] += delta
#                                                #Check if Col1v[j] = Col1v[j-1]. 
#                                                if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
#                                                    #Subtract a random amount from Col1v and add evenly to Col2v and Col3v
#                                                    #The amount must be a number between roundTol
#                                                    rndNum = rd.uniform(10.**(-roundTol), min(min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Col2v[j]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Col3v[j])), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])))
#                                                    Col2v[j] += rndNum/2
#                                                    Col3v[j] += rndNum/2
#                                                    Col1v[j] -= rndNum
#                                                    del rndNum
#                                        else:
#                                            #Col3v is not less than its min. Add delta
#                                            Col3v[j] += delta
#                                            #Check if Col1v[j] = Col1v[j-1]. 
#                                            if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
#                                                #Subtract a random amount from Col1v and add evenly to Col3v
#                                                #The amount must be a number between roundTol
#                                                rndNum = rd.uniform(10.**(-roundTol), min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Col3v[j]), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])))
#                                                Col3v[j] += rndNum
#                                                Col1v[j] -= rndNum
#                                                del rndNum
#                                    else:
#                                        #Add delta to Col1v
#                                        Col1v[j] += delta
#                                        #Check if Col1v[j] = Col1v[j-1]. 
#                                        if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
#                                            #Subtract a random amount from Col1v and add to Col3v
#                                            #The amount must be a number between roundTol
#                                            rndNum = rd.uniform(10.**(-roundTol), min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Col3v[j]), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])))
#                                            Col3v[j] += rndNum
#                                            Col1v[j] -= rndNum
#                                            del rndNum
#                                    
#                        #Need to check lower bounds on Col3v by adding delta/2=delta3
#                        elif ((Col3v[j] + delta/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])):
#                            #Set Col3v to its minimum value and distribute the remaining delta to Col2v and Col1v
#                            delta += (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])
#                            Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2]
#                            
#                            #The delta here should be distributed to Col1v as well as Col2v.
#                            #Check if adding delta3 + remainder to distribute to Col1v and Col2v will make Col2v less than lower bound on Col2v
#                            if ((Col2v[j] + delta3 + (delta-delta3)/2) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])):
#                                #Set Col2v to its minimum value and distribute the remaining delta to Col1v.
#                                delta += (Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])
#                                Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]
#                                
#                                #The remaining delta must be added to Col1v because Col2v and Col3v are at lower bounds. 
#                                #Check if this will make Col1v less than lower bound on Col1v
#                                if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])):
#                                    #Throw an error - the sum of these 3 values must be 1 at this point.
#                                    sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                                else:
#                                    #This must be different than previous value if there are several ways to make Col2v + Col3v + Col1v = 1
#                                    Col1v[j] += delta
#                                    #Check if Col1v[j] still = Col1v[j-1]. 
#                                    if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
#                                        #Subtract a random amount from Col1v and add evenly to Col2v and Col3v
#                                        #The amount must be a number between roundTol
#                                        rndNum = rd.uniform(10.**(-roundTol), min(min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Col2v[j]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Col3v[j])), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])))
#                                        Col2v[j] += rndNum/2
#                                        Col3v[j] += rndNum/2
#                                        Col1v[j] -= rndNum
#                                        del rndNum
#                            else:
#                                Col2v[j] += (delta3 + (delta-delta3)/2)
#                                delta -= (delta3 + (delta-delta3)/2)
#                                #This remaining delta should be distributed to Col1v, if possible. Otherwise to Col2v.
#                                #First check if Col1v will exceed lower bound if delta is added.
#                                if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])):
#                                    #If so, then check if the previous Col1v value is also equal to the lower bound.
#                                    if Col1v[j-1] == ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2]:
#                                        #Both are true, so don't change Col1v[j] first. Instead distribute delta to Col2v.
#                                        if ((Col2v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])):
#                                            #Set Col2v to its minimum value and distribute the remaining delta to Col1v
#                                            delta += (Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])
#                                            Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]
#                                            
#                                            #Check if this will make Col1v less than lower bound on Col1v
#                                            if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])):
#                                                #Throw an error - the sum of these 3 values must be 1 at this point.
#                                                sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                                            else:
#                                                #This must be different than lower bound if there are several ways to make Col2v + Col3v + Col1v = 1
#                                                Col1v[j] += delta
#                                                #Check if Col1v[j] = Col1v[j-1]. 
#                                                if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
#                                                    #Subtract a random amount from Col1v and add evenly to Col3v
#                                                    #The amount must be a number between roundTol
#                                                    rndNum = rd.uniform(10.**(-roundTol), min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Col2v[j]), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])))
#                                                    Col2v[j] += rndNum
#                                                    Col1v[j] -= rndNum
#                                                    del rndNum
#                                        else:
#                                            #Col2v is not less than its min. Add delta
#                                            Col2v[j] += delta
#                                            #Check if Col1v[j] = Col1v[j-1]. 
#                                            if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
#                                                #Subtract a random amount from Col1v and add evenly to Col3v
#                                                #The amount must be a number between roundTol
#                                                rndNum = rd.uniform(10.**(-roundTol), min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Col2v[j]), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])))
#                                                Col2v[j] += rndNum
#                                                Col1v[j] -= rndNum
#                                                del rndNum
#                                    else:
#                                        #Set Col1v to lower bound. That is okay for Col1v to be different from previous Col1v value
#                                        #Set Col1v to its minimum value and distribute the remaining delta to Col2v
#                                        delta += (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])
#                                        Col1v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2]
#                                        
#                                        #delta must be negative, so need to check lower bound on Col2v by adding delta
#                                        if ((Col2v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])):
#                                            #Throw an error - the sum of these 3 values must be 1 at this point.
#                                            sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                                        else:
#                                            #Col2v is not lass than its min. Add delta
#                                            Col2v[j] += delta
#                                else:
#                                    #Then check if Col1v in previous time step was equal to this value.
#                                    if round(Col1v[j] + delta, roundTol) == round(Col1v[j-1], roundTol):
#                                        #Don't change Col1v[j] first. Instead distribute delta to Col2v.
#                                        if ((Col2v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])):
#                                            #Set Col2v to its minimum value and distribute the remaining delta to Col1v
#                                            delta += (Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2])
#                                            Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]
#                                            
#                                            #Check if this will make Col1v less than lower bound on Col1v
#                                            if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])):
#                                                #Throw an error - the sum of these 3 values must be 1 at this point.
#                                                sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                                            else:
#                                                #This must be different than previous step
#                                                Col1v[j] += delta
#                                        else:
#                                            #Col2v is not less than its min. Add delta
#                                            Col2v[j] += delta
#                                            #Check if Col1v[j] = Col1v[j-1]. 
#                                            if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
#                                                #Subtract a random amount from Col1v and add evenly to Col3v
#                                                #The amount must be a number between roundTol
#                                                rndNum = rd.uniform(10.**(-roundTol), min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Col2v[j]), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])))
#                                                Col2v[j] += rndNum
#                                                Col1v[j] -= rndNum
#                                                del rndNum
#                                    else:
#                                        #Add delta to Col1v
#                                        Col1v[j] += delta
#                                        #Check if Col1v[j] = Col1v[j-1]. 
#                                        if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
#                                            #Subtract a random amount from Col1v and add evenly to Col3v
#                                            #The amount must be a number between roundTol
#                                            rndNum = rd.uniform(10.**(-roundTol), min((ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Col2v[j]), (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],2])))
#                                            Col2v[j] += rndNum
#                                            Col1v[j] -= rndNum
#                                            del rndNum
#                        else:
#                            #Col3v and Col2v both not exceeding their min. Add delta/2
#                            Col3v[j] += delta/2
#                            Col2v[j] += delta/2
#                            #The Col1v value in this scenario is definitely different than previous.
#                    
#                    else:
#                        #Need to check upper bounds on Col2v and Col3v by adding delta/2=delta3
#                        if ((Col2v[j] + delta3) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3])):
#                            #Set Col2v to its maximum value and distribute the remaining delta to Col3v and Col1v
#                            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Col2v[j])
#                            Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3]
#                            
#                            #The delta here should be distributed to Col1v as well as Col3v.
#                            #Check if adding delta3 + remainder to distribute to Col1v and Col3v will make Col3v greater than upper bound on Col3v
#                            if ((Col3v[j] + delta3 + (delta-delta3)/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3])):
#                                #Set Col3v to its minimum value and distribute the remaining delta to Col1v.
#                                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Col3v[j])
#                                Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3]
#                                
#                                #The remaining delta must be added to Col1v because Col2v and Col3v are at lower bounds. 
#                                #Check if this will make Col1v less than lower bound on Col1v
#                                if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3])):
#                                    #Throw an error - the sum of these 3 values must be 1 at this point.
#                                    sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                                else:
#                                    Col1v[j] += delta
#                                    #Check if Col1v[j] still = Col1v[j-1]. 
#                                    if (round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol)):
#                                        #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
#                                        #The amount must be a number between roundTol
#                                        rndNum = rd.uniform(10.**(-roundTol), min(min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]), (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3] - Col1v[j])))
#                                        Col2v[j] -= rndNum/2
#                                        Col3v[j] -= rndNum/2
#                                        Col1v[j] += rndNum
#                                        del rndNum
#                            else:
#                                Col3v[j] += (delta3 + (delta-delta3)/2)
#                                delta -= (delta3 + (delta-delta3)/2)
#                                #This remaining delta should be distributed to Col1v, if possible. Otherwise to Col3v.
#                                #First check if Col1v will exceed upper bound if delta is added.
#                                if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3])):
#                                    #If so, then check if the previous Col1v value is also equal to the lower bound.
#                                    if Col1v[j-1] == ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3]:
#                                        #Both are true, so don't change Col1v[j] first. Instead distribute delta to Col3v.
#                                        if ((Col3v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3])):
#                                            #Set Col3v to its minimum value and distribute the remaining delta to Col1v
#                                            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Col3v[j])
#                                            Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3]
#                                            
#                                            #Check if this will make Col1v less than lower bound on Col1v
#                                            if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3])):
#                                                #Throw an error - the sum of these 3 values must be 1 at this point.
#                                                sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                                            else:
#                                                Col1v[j] += delta
#                                                #Check if Col1v[j] = Col1v[j-1]. 
#                                                if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
#                                                    #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
#                                                    #The amount must be a number between roundTol
#                                                    rndNum = rd.uniform(10.**(-roundTol), min(min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]), (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3] - Col1v[j])))
#                                                    Col2v[j] -= rndNum/2
#                                                    Col3v[j] -= rndNum/2
#                                                    Col1v[j] += rndNum
#                                                    del rndNum
#                                        else:
#                                            #Col3v is not greater than its max. Add delta
#                                            Col3v[j] += delta
#                                            #Check if Col1v[j] = Col1v[j-1]. 
#                                            if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
#                                                #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
#                                                #The amount must be a number between roundTol
#                                                rndNum = rd.uniform(10.**(-roundTol), min((Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3] - Col1v[j]))
#                                                Col3v[j] -= rndNum
#                                                Col1v[j] += rndNum
#                                                del rndNum
#                                    else:
#                                        #Set Col1v to upper bound. That is okay for Col1v to be different from previous Col1v value
#                                        #Set Col1v to its maximum value and distribute the remaining delta to Col3v
#                                        delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3] - Col1v[j])
#                                        Col1v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3]
#                                        
#                                        #delta must be negative, so need to check lower bound on Col3v by adding delta
#                                        if ((Col3v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3])):
#                                            #Throw an error - the sum of these 3 values must be 1 at this point.
#                                            sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                                        else:
#                                            #Col3v is not lass than its min. Add delta
#                                            Col3v[j] += delta
#                                else:
#                                    #Then check if Col1v in previous time step was equal to this value.
#                                    if round(Col1v[j] + delta, roundTol) == round(Col1v[j-1], roundTol):
#                                        #Don't change Col1v[j] first. Instead distribute delta to Col3v.
#                                        if ((Col3v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3])):
#                                            #Set Col3v to its minimum value and distribute the remaining delta to Col1v
#                                            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Col3v[j])
#                                            Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3]
#                                            
#                                            #Check if this will make Col1v less than lower bound on Col1v
#                                            if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3])):
#                                                #Throw an error - the sum of these 3 values must be 1 at this point.
#                                                sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                                            else:
#                                                Col1v[j] += delta
#                                                #Check if Col1v[j] = Col1v[j-1]. 
#                                                if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
#                                                    #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
#                                                    #The amount must be a number between roundTol
#                                                    rndNum = rd.uniform(10.**(-roundTol), min(min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]), (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3] - Col1v[j])))
#                                                    Col2v[j] -= rndNum/2
#                                                    Col3v[j] -= rndNum/2
#                                                    Col1v[j] += rndNum
#                                                    del rndNum
#                                        else:
#                                            #Col3v is not less than its min. Add delta
#                                            Col3v[j] += delta
#                                            #Check if Col1v[j] = Col1v[j-1]. 
#                                            if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
#                                                #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
#                                                #The amount must be a number between roundTol
#                                                rndNum = rd.uniform(10.**(-roundTol), min((Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3] - Col1v[j]))
#                                                Col3v[j] -= rndNum
#                                                Col1v[j] += rndNum
#                                                del rndNum
#                                    else:
#                                        #Add delta to Col1v
#                                        Col1v[j] += delta
#                                        #Check if Col1v[j] = Col1v[j-1]. 
#                                        if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
#                                            #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
#                                            #The amount must be a number between roundTol
#                                            rndNum = rd.uniform(10.**(-roundTol), min((Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3] - Col1v[j]))
#                                            Col3v[j] -= rndNum
#                                            Col1v[j] += rndNum
#                                            del rndNum
#                                    
#                        #Need to check lower bounds on Col3v by adding delta/2=delta3
#                        elif ((Col3v[j] + delta/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3])):
#                            #Set Col3v to its minimum value and distribute the remaining delta to Col2v and Col1v
#                            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3] - Col3v[j])
#                            Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],3]
#                            
#                            #The delta here should be distributed to Col1v as well as Col2v.
#                            #Check if adding delta3 + remainder to distribute to Col1v and Col2v will make Col2v less than lower bound on Col2v
#                            if ((Col2v[j] + delta3 + (delta-delta3)/2) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3])):
#                                #Set Col2v to its maximum value and distribute the remaining delta to Col1v.
#                                delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Col2v[j])
#                                Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3]
#                                
#                                #The remaining delta must be added to Col1v because Col2v and Col3v are at upper bounds. 
#                                #Check if this will make Col1v greater than upper bound on Col1v
#                                if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3])):
#                                    #Throw an error - the sum of these 3 values must be 1 at this point.
#                                    sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                                else:
#                                    #This must be different than previous value if there are several ways to make Col2v + Col3v + Col1v = 1
#                                    Col1v[j] += delta
#                                    #Check if Col1v[j] = Col1v[j-1]. 
#                                    if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
#                                        #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
#                                        #The amount must be a number between roundTol
#                                        rndNum = rd.uniform(10.**(-roundTol), min(min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]), (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flig'].index[0],2])), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3] - Col1v[j])))
#                                        Col2v[j] -= rndNum/2
#                                        Col3v[j] -= rndNum/2
#                                        Col1v[j] += rndNum
#                                        del rndNum
#                            else:
#                                Col2v[j] += (delta3 + (delta-delta3)/2)
#                                delta -= (delta3 + (delta-delta3)/2)
#                                #This remaining delta should be distributed to Col1v, if possible. Otherwise to Col2v.
#                                #First check if Col1v will exceed lower bound if delta is added.
#                                if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3])):
#                                    #If so, then check if the previous Col1v value is also equal to the lower bound.
#                                    if Col1v[j-1] == ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3]:
#                                        #Both are true, so don't change Col1v[j] first. Instead distribute delta to Col2v.
#                                        if ((Col2v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3])):
#                                            #Set Col2v to its minimum value and distribute the remaining delta to Col1v
#                                            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Col2v[j])
#                                            Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3]
#                                            
#                                            #Check if this will make Col1v less than lower bound on Col1v
#                                            if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3])):
#                                                #Throw an error - the sum of these 3 values must be 1 at this point.
#                                                sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                                            else:
#                                                #This must be different than lower bound if there are several ways to make Col2v + Col3v + Col1v = 1
#                                                Col1v[j] += delta
#                                                #Check if Col1v[j] = Col1v[j-1]. 
#                                                if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
#                                                    #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
#                                                    #The amount must be a number between roundTol
#                                                    rndNum = rd.uniform(10.**(-roundTol), min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3] - Col1v[j]))
#                                                    Col2v[j] -= rndNum
#                                                    Col1v[j] += rndNum
#                                                    del rndNum
#                                        else:
#                                            #Col2v is not less than its min. Add delta
#                                            Col2v[j] += delta
#                                            #Check if Col1v[j] = Col1v[j-1]. 
#                                            if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
#                                                #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
#                                                #The amount must be a number between roundTol
#                                                rndNum = rd.uniform(10.**(-roundTol), min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3] - Col1v[j]))
#                                                Col2v[j] -= rndNum
#                                                Col1v[j] += rndNum
#                                                del rndNum
#                                    else:
#                                        #Set Col1v to lower bound. That is okay for Col1v to be different from previous Col1v value
#                                        #Set Col1v to its minimum value and distribute the remaining delta to Col2v
#                                        delta += (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3])
#                                        Col1v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3]
#                                        
#                                        #delta must be negative, so need to check lower bound on Col2v by adding delta
#                                        if ((Col2v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3])):
#                                            #Throw an error - the sum of these 3 values must be 1 at this point.
#                                            sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                                        else:
#                                            #Col2v is not lass than its min. Add delta
#                                            Col2v[j] += delta
#                                else:
#                                    #Then check if Col1v in previous time step was equal to this value.
#                                    if round(Col1v[j] + delta, roundTol) == round(Col1v[j-1], roundTol):
#                                        #Don't change Col1v[j] first. Instead distribute delta to Col2v.
#                                        if ((Col2v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3])):
#                                            #Set Col2v to its minimum value and distribute the remaining delta to Col1v
#                                            delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3] - Col2v[j])
#                                            Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],3]
#                                            
#                                            #Check if this will make Col1v less than lower bound on Col1v
#                                            if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3])):
#                                                #Throw an error - the sum of these 3 values must be 1 at this point.
#                                                sys.exit('PyERROR: The sum of Col3v, Col2v, and Col1v cannot be 1 for Replicate = %s' % str(j))
#                                            else:
#                                                #This must be different than previous step
#                                                Col1v[j] += delta
#                                        else:
#                                            #Col2v is not less than its min. Add delta
#                                            Col2v[j] += delta
#                                            #Check if Col1v[j] = Col1v[j-1]. 
#                                            if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
#                                                #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
#                                                #The amount must be a number between roundTol
#                                                rndNum = rd.uniform(10.**(-roundTol), min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3] - Col1v[j]))
#                                                Col2v[j] -= rndNum
#                                                Col1v[j] += rndNum
#                                                del rndNum
#                                    else:
#                                        #Add delta to Col1v
#                                        Col1v[j] += delta
#                                        #Check if Col1v[j] = Col1v[j-1]. 
#                                        if round(Col1v[j], roundTol) == round(Col1v[j-1], roundTol):
#                                            #Add a random amount to Col1v and subtract evenly from Col2v and Col3v
#                                            #The amount must be a number between roundTol
#                                            rndNum = rd.uniform(10.**(-roundTol), min((Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_fcel'].index[0],2]), (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+'_epc.frootlitr_flab'].index[0],3] - Col1v[j]))
#                                            Col2v[j] -= rndNum
#                                            Col1v[j] += rndNum
#                                            del rndNum
#                        else:
#                            #Col3v and Col2v both not exceeding their min. Add delta/2
#                            Col3v[j] += delta/2
#                            Col2v[j] += delta/2
#                            #The Col1v value in this scenario is definitely different than previous.   
#                    del delta3
    

def FullSum3Check(MorrisSample_df, 
                  ProbFile, #File that specifies the parameters in column 0, and the "Lower" and "Upper" bounds as columns 2 and 3. Column 1 is the parameters without the def file ID numbers attached, but is not used in this script.
                  Col1, Col2, Col3, #The 3 column names to be summed (flab, fcel, flig)
                  regexNm, #regular expression
                  roundTol, #round tolerance
                  N #number of Morris trajectories
                  ):
    for i in range(len(MorrisSample_df.filter(regex=regexNm).columns)):
        #Find the indicator for the soil type
        #string = Get all columns containing Col1v
        ind = re.split(string=MorrisSample_df.filter(regex=regexNm).columns[i], pattern='_')[0]
        #Use the indicator to extract the Col1v, Col2v, and Col3v columns
        Col1v = MorrisSample_df.loc[:, ind + Col1]
        Col2v = MorrisSample_df.loc[:, ind + Col2]
        Col3v = MorrisSample_df.loc[:, ind + Col3]
        
        #Save the original column
        MorrisSample_df.loc[:, 'orig_' + ind + Col1 +'_orig'] = Col1v
        MorrisSample_df.loc[:, 'orig_' + ind + Col2 +'_orig'] = Col2v
        MorrisSample_df.loc[:, 'orig_' + ind + Col3 +'_orig'] = Col3v
        
        #Get the indices of the Morris trajectory starting points. 
        #Changes in these variables will only happen between those points
        trajChange = range((len(ProbFile.iloc[:,0])+1), (len(ProbFile.iloc[:,0])+1)*(N+1), (len(ProbFile.iloc[:,0])+1))
        
        #Get a list of indices at which these variables are supposed to change.
        Col1vChange = filter(lambda i: i not in trajChange, list(np.where((MorrisSample_df.iloc[0:(N*(max(ProbFile.index)+2)-1),np.where(MorrisSample_df.columns == 'orig_' + ind + Col1 +'_orig')[0][0]].values - MorrisSample_df.iloc[1:(N*(max(ProbFile.index)+2)),np.where(MorrisSample_df.columns == 'orig_' + ind + Col1 +'_orig')[0][0]].values) != 0)[0]+1))
        Col2vChange = filter(lambda i: i not in trajChange, list(np.where((MorrisSample_df.iloc[0:(N*(max(ProbFile.index)+2)-1),np.where(MorrisSample_df.columns == 'orig_' + ind + Col2 +'_orig')[0][0]].values - MorrisSample_df.iloc[1:(N*(max(ProbFile.index)+2)),np.where(MorrisSample_df.columns == 'orig_' + ind + Col2 +'_orig')[0][0]].values) != 0)[0]+1))
        Col3vChange = filter(lambda i: i not in trajChange, list(np.where((MorrisSample_df.iloc[0:(N*(max(ProbFile.index)+2)-1),np.where(MorrisSample_df.columns == 'orig_' + ind + Col3 +'_orig')[0][0]].values - MorrisSample_df.iloc[1:(N*(max(ProbFile.index)+2)),np.where(MorrisSample_df.columns == 'orig_' + ind + Col3 +'_orig')[0][0]].values) != 0)[0]+1))
        
        ChangeVec = np.unique(np.append(np.append(np.append(np.append(trajChange, Col1vChange), Col2vChange), Col3vChange), [0]))
        #Remove the last trajChange value because it's not needed for this loop.
        ChangeVec = np.delete(ChangeVec, len(ChangeVec)-1)
        
        #Make an index to count number of loop iterations
        ChangeInd = 0
        
        #Loop through the indices to reassign the Col2v Col1v and Col3v values - should remain within the delta step change
        for j in ChangeVec:
            #Compute the remainder from 1
            delta = 1 - (Col2v[j] + Col1v[j] + Col3v[j])
            
            #Check if j is a change in variables or in trajectory
            if j in Col1vChange:
                #
                Col1v, Col2v, Col3v = Sum3CheckFun(Col1=Col1, Col2=Col2, Col3=Col3, delta=delta, ind=ind, j=j, Col1v=Col1v, Col2v=Col2v, Col3v=Col3v, ProbFile=ProbFile, roundTol=roundTol)
                
            elif j in Col2vChange:
                #
                Col2v, Col1v, Col3v = Sum3CheckFun(Col1=Col2, Col2=Col1, Col3=Col3, delta=delta, ind=ind, j=j, Col1v=Col2v, Col2v=Col1v, Col3v=Col3v, ProbFile=ProbFile, roundTol=roundTol)
                
            elif j in Col3vChange:
                #
                Col3v, Col1v, Col2v = Sum3CheckFun(Col1=Col3, Col2=Col1, Col3=Col2, delta=delta, ind=ind, j=j, Col1v=Col3v, Col2v=Col1v, Col3v=Col2v, ProbFile=ProbFile, roundTol=roundTol)
                
            else:
                #Can change and not worry about previous values. 
                #Need to check if all 3 parameters will be within their bounds.
                if ((Col1v[j] + delta/3.) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3])):
                    #Set Col1v to upper bound and distribute remaining delta to Col2v and Col3v.
                    delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3] - Col1v[j])
                    Col1v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3]
                    
                    if ((Col2v[j] + delta/2.) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3])):
                        #Set Col2v to upper bound and distribute remaining delta to Col3v.
                        delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j])
                        Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3]
                        
                        if ((Col3v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3])):
                            #Throw an error - the sum of these 3 values must be 1 at this point.
                            sys.exit('PyERROR: The sum of Col3v, Col1v, and Col2v cannot be 1 for Replicate = %s' % str(j))
                        else:
                            Col3v[j] += delta
                            
                    elif ((Col3v[j] + delta/2.) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3])):
                        #Set Col3v to upper bound and distribute remaining delta to Col2v.
                        delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j])
                        Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3]
                        
                        if ((Col2v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3])):
                            #Throw an error - the sum of these 3 values must be 1 at this point.
                            sys.exit('PyERROR: The sum of Col3v, Col1v, and Col2v cannot be 1 for Replicate = %s' % str(j))
                        else:
                            Col2v[j] += delta
                            
                    else:
                        #Both within bounds.
                        Col2v[j] += delta/2.
                        Col3v[j] += delta/2.
                
                elif ((Col1v[j] + delta/3.) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])):
                    #Set Col1v to upper bound and distribute remaining delta to Col2v and Col3v.
                    delta += (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])
                    Col1v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2]
                    
                    if ((Col2v[j] + delta/2.) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])):
                        #Set Col2v to upper bound and distribute remaining delta to Col3v.
                        delta += (Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])
                        Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]
                        
                        if ((Col3v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])):
                            #Throw an error - the sum of these 3 values must be 1 at this point.
                            sys.exit('PyERROR: The sum of Col3v, Col1v, and Col2v cannot be 1 for Replicate = %s' % str(j))
                        else:
                            Col3v[j] += delta
                            
                    elif ((Col3v[j] + delta/2.) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])):
                        #Set Col3v to upper bound and distribute remaining delta to Col2v.
                        delta += (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])
                        Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2]
                        
                        if ((Col2v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])):
                            #Throw an error - the sum of these 3 values must be 1 at this point.
                            sys.exit('PyERROR: The sum of Col3v, Col1v, and Col2v cannot be 1 for Replicate = %s' % str(j))
                        else:
                            Col2v[j] += delta
                            
                    else:
                        #Both within bounds.
                        Col2v[j] += delta/2.
                        Col3v[j] += delta/2.
                
                elif ((Col2v[j] + delta/3.) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3])):
                    #Set Col2v to upper bound and distribute remaining delta to Col1v and Col3v.
                    delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j])
                    Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3]
                    
                    if ((Col1v[j] + delta/2.) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3])):
                        #Set Col1v to upper bound and distribute remaining delta to Col3v.
                        delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3] - Col1v[j])
                        Col1v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3]
                        
                        if ((Col3v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3])):
                            #Throw an error - the sum of these 3 values must be 1 at this point.
                            sys.exit('PyERROR: The sum of Col3v, Col1v, and Col2v cannot be 1 for Replicate = %s' % str(j))
                        else:
                            Col3v[j] += delta
                            
                    elif ((Col3v[j] + delta/2.) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3])):
                        #Set Col3v to upper bound and distribute remaining delta to Col2v.
                        delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j])
                        Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3]
                        
                        if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3])):
                            #Throw an error - the sum of these 3 values must be 1 at this point.
                            sys.exit('PyERROR: The sum of Col3v, Col1v, and Col2v cannot be 1 for Replicate = %s' % str(j))
                        else:
                            Col1v[j] += delta
                            
                    else:
                        #Both within bounds.
                        Col1v[j] += delta/2.
                        Col3v[j] += delta/2.
                
                elif ((Col2v[j] + delta/3.) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])):
                    #Set Col2v to upper bound and distribute remaining delta to Col2v and Col3v.
                    delta += (Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])
                    Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]
                    
                    if ((Col1v[j] + delta/2.) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])):
                        #Set Col1v to upper bound and distribute remaining delta to Col3v.
                        delta += (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])
                        Col1v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2]
                        
                        if ((Col3v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])):
                            #Throw an error - the sum of these 3 values must be 1 at this point.
                            sys.exit('PyERROR: The sum of Col3v, Col1v, and Col2v cannot be 1 for Replicate = %s' % str(j))
                        else:
                            Col3v[j] += delta
                            
                    elif ((Col3v[j] + delta/2.) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])):
                        #Set Col3v to upper bound and distribute remaining delta to Col1v.
                        delta += (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])
                        Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2]
                        
                        if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])):
                            #Throw an error - the sum of these 3 values must be 1 at this point.
                            sys.exit('PyERROR: The sum of Col3v, Col1v, and Col2v cannot be 1 for Replicate = %s' % str(j))
                        else:
                            Col1v[j] += delta
                            
                    else:
                        #Both within bounds.
                        Col1v[j] += delta/2.
                        Col3v[j] += delta/2.
                
                elif ((Col3v[j] + delta/3.) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3])):
                    #Set Col3v to upper bound and distribute remaining delta to Col1v and Col3v.
                    delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3] - Col3v[j])
                    Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3]
                    
                    if ((Col1v[j] + delta/2.) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3])):
                        #Set Col1v to upper bound and distribute remaining delta to Col3v.
                        delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3] - Col1v[j])
                        Col1v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3]
                        
                        if ((Col2v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3])):
                            #Throw an error - the sum of these 3 values must be 1 at this point.
                            sys.exit('PyERROR: The sum of Col3v, Col1v, and Col2v cannot be 1 for Replicate = %s' % str(j))
                        else:
                            Col2v[j] += delta
                            
                    elif ((Col2v[j] + delta/2.) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3])):
                        #Set Col2v to upper bound and distribute remaining delta to Col1v.
                        delta -= (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3] - Col2v[j])
                        Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3]
                        
                        if ((Col1v[j] + delta) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3])):
                            #Throw an error - the sum of these 3 values must be 1 at this point.
                            sys.exit('PyERROR: The sum of Col3v, Col1v, and Col2v cannot be 1 for Replicate = %s' % str(j))
                        else:
                            Col1v[j] += delta
                            
                    else:
                        #Both within bounds.
                        Col1v[j] += delta/2.
                        Col2v[j] += delta/2.
                
                elif ((Col3v[j] + delta/3.) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])):
                    #Set Col3v to upper bound and distribute remaining delta to Col2v and Col3v.
                    delta += (Col3v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2])
                    Col3v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2]
                    
                    if ((Col1v[j] + delta/2.) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])):
                        #Set Col1v to upper bound and distribute remaining delta to Col3v.
                        delta += (Col1v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])
                        Col1v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2]
                        
                        if ((Col2v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])):
                            #Throw an error - the sum of these 3 values must be 1 at this point.
                            sys.exit('PyERROR: The sum of Col3v, Col1v, and Col2v cannot be 1 for Replicate = %s' % str(j))
                        else:
                            Col2v[j] += delta
                            
                    elif ((Col2v[j] + delta/2.) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])):
                        #Set Col3v to upper bound and distribute remaining delta to Col1v.
                        delta += (Col2v[j] - ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2])
                        Col2v[j] = ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]
                        
                        if ((Col1v[j] + delta) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2])):
                            #Throw an error - the sum of these 3 values must be 1 at this point.
                            sys.exit('PyERROR: The sum of Col3v, Col1v, and Col2v cannot be 1 for Replicate = %s' % str(j))
                        else:
                            Col1v[j] += delta
                            
                    else:
                        #Both within bounds.
                        Col1v[j] += delta/2.
                        Col2v[j] += delta/2.
                else:
                    #can safely change all 3 and be within bounds
                    Col1v[j] += delta/3.
                    Col2v[j] += delta/3.
                    Col3v[j] += delta/3.
            
            #The resulting values could sum to a number other than exactly 1, but be within thouKrths of 1. 
            #So, round all values and ensure they sum to exactly 1
            Col2v[j] = round(Col2v[j],roundTol) 
            Col1v[j] = round(Col1v[j],roundTol)
            Col3v[j] = round(Col3v[j],roundTol)
            f = round(1. - (Col2v[j] + Col1v[j] + Col3v[j]),roundTol)
            if f > 0:
                #Check for being less than upper bound. Add to first of Kr, Ka, Kt
                if (Col2v[j] + f) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],3]):
                    Col2v[j] += f
                elif (Col1v[j] + f) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],3]):
                    Col1v[j] += f
                elif (Col3v[j] + f) < (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],3]):
                    Col3v[j] += f
                else:
                    sys.exit('PyERROR: Sum of Col2v + Col1v + Col3v != 1, and cannot be rounded to 1 for Replicate = %s' % str(j))
            elif f < 0:
                #Check for being greater than lower bound. Add to first of Kr, Ka, Kt
                if (Col2v[j] + f) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col2].index[0],2]):
                    Col2v[j] += f
                elif (Col1v[j] + f) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col1].index[0],2]):
                    Col1v[j] += f
                elif (Col3v[j] + f) > (ProbFile.iloc[ProbFile.iloc[:,0][ProbFile.iloc[:,0] == ind+Col3].index[0],2]):
                    Col3v[j] += f
                else:
                    sys.exit('PyERROR: Sum of Col2v + Col1v + Col3v != 1, and cannot be rounded to 1 for Replicate = %s' % str(j))
            
            #Change the values of all data in the vector up until the next trajectory change point.
            if j == ChangeVec[len(ChangeVec)-1]:
                Col1v[j:(len(Col1v))] = Col1v[j]
                Col2v[j:(len(Col2v))] = Col2v[j]
                Col3v[j:(len(Col3v))] = Col3v[j]        
            else:
                Col1v[j:(ChangeVec[ChangeInd+1])] = Col1v[j]
                Col2v[j:(ChangeVec[ChangeInd+1])] = Col2v[j]
                Col3v[j:(ChangeVec[ChangeInd+1])] = Col3v[j]
            
            ChangeInd += 1
        
        #Save the new column of values to track what was changed.
        MorrisSample_df.loc[:, ind + Col1] = Col1v
        MorrisSample_df.loc[:, ind + Col2] = Col2v
        MorrisSample_df.loc[:, ind + Col3] = Col3v
    
        del i, j, Col3v, Col2v, Col1v, delta, ind, f, ChangeInd, ChangeVec, Col1vChange, Col2vChange, Col3vChange
    return MorrisSample_df
