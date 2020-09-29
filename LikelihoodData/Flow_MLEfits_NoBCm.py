import pandas as pd
import numpy as np
from scipy import optimize as sciOpt
from scipy import stats as ss
from mpi4py import MPI
import math
import pyDOE
import sys
import os

#All commented out for now. Use of sys.argv is untested.
#sys.argv contains: 
#0: unused - script call info
#1: initial random seed
#2: Full path to streamflow observations .txt file
#3: Full path to simulated streamflow .txt file
#4: Number of initial locations for the multi-start MLE solver
#5: Prefix for the output file name
#6: directory of the likelihood function

#Change to function directory and load function
#owd = os.getcwd()
#os.chdir(sys.argv[6])
#os.chdir('C:\\Users\\js4yd\\OneDrive - University of Virginia\\RHESSys_ParameterSA\\LikelihoodData')
from likelihood import generalizedLikelihoodFunction
#os.chdir(owd)

# load flow observations
#TrueQ = pd.read_csv('C:\\Users\\js4yd\\Dropbox\\Jared-Julie-Share\\Data\\BaismanStreamflow_Cal.txt',delimiter='\t') #11-15-99 through 9-30-13
#TrueQ = pd.read_csv('C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\obs\\BaismanStreamflow_Feb2020Revised_Cal_p.txt',delimiter='\t') #11-15-99 through 9-30-13
TrueQ = pd.read_csv('Rep4246_p.txt',delimiter='\t') #11-15-99 through 9-30-13
#TrueQ = pd.read_csv(sys.argv[2],delimiter='\t') #11-15-99 through 9-30-13
TrueQ['Date'] = pd.to_datetime(TrueQ['Date'],format="%Y-%m-%d")

# load flow simulations
#Dec. 2019
#SimQ = pd.read_csv('SAResults_BasinStreamflow_p4.txt',delimiter='\t') #(11-15-99 through 9-30-10)
#SimQ = pd.read_csv('C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SAResults_BasinStreamflow_p4_Reordered_Add5_Likes.txt',delimiter='\t')
SimQ = pd.read_csv('SynBaisTestData.txt',delimiter='\t')
#SimQ = pd.read_csv('SAResults_BasinStreamflow_p4_Reordered_Add5_Likes.txt',delimiter='\t')
#SimQ = pd.read_csv(sys.argv[3],delimiter='\t')
SimQ['Date'] = pd.to_datetime(SimQ['Date'],format="%Y-%m-%d")
#Make a copy for use in computing metrics later
cSimQ = SimQ.copy(deep=True)
#Change index to water year (shift to Jan 1 of following year so leap years match)
cSimQ = cSimQ.set_index('Date')
cSimQ.index = cSimQ.index.shift(92, freq = 'D')

columns = SimQ.columns

# create data frame to store parameter estimates, likelihood and SSE
Qdf = pd.DataFrame(columns=['Replicate','beta','xi','sigma_0','sigma_1','phi_1','mu_h','logL','SSEd','SSEw','SSEm','SSEa','NSEd','NSEw','NSEm','NSEa','LNSEd','LNSEw','LNSEm','LNSEa','pBias','success','mess'])

#Make a deep copy of the true data to compute weekly, monthly, and annual info later
cTrueQ = TrueQ.copy(deep=True)
cTrueQ = cTrueQ.set_index('Date')
#Change index to water year (shift to Jan 1 of following year so leap years match)
cTrueQ.index = cTrueQ.index.shift(92, freq = 'D')
#Compute monthly and annual flows
cTrueQ['mFlow'] = cTrueQ['Flow'].resample('M').sum()
cTrueQ['aFlow'] = cTrueQ['Flow'].resample('A').sum()
mTrueQ = np.array(cTrueQ['mFlow'].iloc[np.where(np.isnan(cTrueQ['mFlow']) == False)[0]])
aTrueQ = np.array(cTrueQ['aFlow'].iloc[np.where(np.isnan(cTrueQ['aFlow']) == False)[0]])
#For weekly, need to loop and combine every 7 days, continuously
wTrueQ = np.zeros(len(aTrueQ)*52 + 1)
w = 0
for k in range(len(cTrueQ['Flow'])):
    if np.mod(k+1, 7) == 0:
        #Sum the last 6 days + this day
        wTrueQ[w] = cTrueQ['Flow'].iloc[(k-6):k+1].sum()
        w = w + 1

del w

data = np.array(mTrueQ)
tIndex = range(len(mTrueQ))
#month = TrueQ['Date'].dt.month

# Begin parallel simulation
comm = MPI.COMM_WORLD

# Get the number of processors and the rank of processors
rank = comm.rank
nprocs = comm.size

# Determine the chunk which each processor will neeed to do
count = int(math.floor((len(columns)-1)/nprocs))
remainder = (len(columns)-1) % nprocs

# Use the processor rank to determine the chunk of work each processor will do
if rank < remainder:
	start = rank*(count+1)
	stop = start + count + 1
else:
	start = remainder*(count+1) + (rank-remainder)*count
	stop = start + count

#Number of samples to take for the multi-start gradient descent algorithm
numsamps = 1000
#numsamps = int(sys.argv[4])
#Create dataframe to store successful parameter sets
#Qdf_success = pd.DataFrame(columns=['beta','xi','sigma_0','sigma_1','phi_1','mu_h','logL'])

for i in range(start,stop):
    #Compute the weekly, monthly, and annual timeseries from the daily timeseries
    cSimQ['mFlow'] = cSimQ['Replicate' + str(i+1)].resample('M').sum()
    cSimQ['aFlow'] = cSimQ['Replicate' + str(i+1)].resample('A').sum()
    mSimQ = np.array(cSimQ['mFlow'].iloc[np.where(np.isnan(cSimQ['mFlow']) == False)[0]])
    aSimQ = np.array(cSimQ['aFlow'].iloc[np.where(np.isnan(cSimQ['aFlow']) == False)[0]])
    #For weekly, need to loop and combine every 7 days, continuously
    wSimQ = np.zeros(len(aSimQ)*52 + 1)
    w = 0
    for k in range(len(cTrueQ['Flow'])):
        if np.mod(k+1, 7) == 0:
            #Sum the last 6 days + this day
            wSimQ[w] = cSimQ['Replicate' + str(i+1)].iloc[(k-6):k+1].sum()
            w = w + 1
    
    comparedata = np.array(mSimQ)
    
    #Compute metrics to append to dataframe:
    #SSE
    SSEd = np.sum((cTrueQ['Flow'] - cSimQ['Replicate' + str(i+1)])**2)
    SSEw = np.sum((wTrueQ - wSimQ)**2)
    SSEm = np.sum((mTrueQ - mSimQ)**2)
    SSEa = np.sum((aTrueQ - aSimQ)**2)
    #NSE
    NSEd = 1. - SSEd/np.sum((cTrueQ['Flow'] - np.mean(cTrueQ['Flow']))**2)
    NSEw = 1. - SSEw/np.sum((wTrueQ - np.mean(wTrueQ))**2)
    NSEm = 1. - SSEm/np.sum((mTrueQ - np.mean(mTrueQ))**2)
    NSEa = 1. - SSEa/np.sum((aTrueQ - np.mean(aTrueQ))**2)
    #LNSE
    LNSEd = 1. - np.sum((np.log(cTrueQ['Flow']) - np.log(cSimQ['Replicate' + str(i+1)]))**2)/np.sum((np.log(cTrueQ['Flow']) - np.mean(np.log(cTrueQ['Flow'])))**2)
    LNSEw = 1. - np.sum((np.log(wTrueQ) - np.log(wSimQ))**2)/np.sum((np.log(wTrueQ) - np.mean(np.log(wTrueQ)))**2)
    LNSEm = 1. - np.sum((np.log(mTrueQ) - np.log(mSimQ))**2)/np.sum((np.log(mTrueQ) - np.mean(np.log(mTrueQ)))**2)
    LNSEa = 1. - np.sum((np.log(aTrueQ) - np.log(aSimQ))**2)/np.sum((np.log(aTrueQ) - np.mean(np.log(aTrueQ)))**2)
    #Percent Bias
    pBias = 100. * (np.sum(cSimQ['Replicate' + str(i+1)] - cTrueQ['Flow']) / np.sum(cTrueQ['Flow']))

    ObjFunc = lambda params: generalizedLikelihoodFunction(data,comparedata,tIndex,params)
    #ObjFunc = lambda params: generalizedLikelihoodFunction(data,comparedata,tIndex,params,month)
    
    # find MLE fits for each simulation
    # params = [beta, xi, sigma_0, sigma_1, phi_1, mu_h]
    # initialize parameter estimates at beta=1 (double exponential), xi=0.5 (negatively skewed), 
    # sigma_0 = 0.1, sigma_1 = 0.1, phi_1 = 0.7 (high auto-correlation), mu_h = 0.0 (unbiased)
    #Make an LHS sample of the initial parameters to try for each replicate. Random seed is the index
    np.random.seed(seed=i+518)
    #np.random.seed(seed=i+int(sys.argv[1]))
    #paramsInit = pyDOE.lhs(n=6, criterion='m', iterations=1000, samples=numsamps)
    paramsInit = pyDOE.lhs(n=6, samples=numsamps)
    
    #Get all of the parameters into their expected ranges
    #Initial bounds were [-1,10], [0,10], same, same, same, [0,100]
    paramsInit[:,0] = paramsInit[:,0]*(7. + 0.99) - 0.99
    paramsInit[:,1] = paramsInit[:,1]*(5. - 0.01) + 0.01
    paramsInit[:,2] = paramsInit[:,2]*(1.-.000001)+.000001
    #3 is on [0,1]
    #4 is on [0,1]
    #5 is on [0,1]
    
    #Loop over the initial starting locations and select the most optimal parameter set
    for j in range(numsamps):
        optParams = sciOpt.minimize(ObjFunc, 
                                    paramsInit[j,:], 
                                    method='SLSQP', 
                                    bounds=[[-0.99,7.],[0.01,5.],[0.000001,1.],[0.,1.],[0.,1.],[0.,1.]],
                                    options={'maxiter': 1000, 'disp': False})
        if j == 0:
            #Save the optimal successful convergence and unsuccessful convergence
            OptChoice = optParams
            OptFailed = optParams
        elif (((optParams.fun < OptChoice.fun) & (optParams.success == True)) | (np.isnan(OptChoice.fun) & (np.isnan(optParams.fun) == False))):
            OptChoice = optParams
        elif (((optParams.fun < OptFailed.fun) & (optParams.success == False)) | (np.isnan(OptFailed.fun) & (np.isnan(optParams.fun) == False))):
            OptFailed = optParams

        #Used to see the distribution of parameter values with different starting locations
        #if (optParams.success == True):
        #    #Save parameter vector
        #    Qdf_success = Qdf_success.append({'beta': optParams.x[0], 'xi': optParams.x[1], 'sigma_0': optParams.x[2],
        #                'sigma_1': optParams.x[3], 'phi_1': optParams.x[4], 'mu_h': optParams.x[5],
        #                'logL': -optParams.fun}, ignore_index=True)
    
    #Check if there are any successes
    if OptChoice.success == True:
        #Assign the best parameter values to this ith replicate
        Qdf = Qdf.append({'Replicate': i+1, 'beta': OptChoice.x[0], 'xi': OptChoice.x[1], 'sigma_0': OptChoice.x[2],
                          'sigma_1': OptChoice.x[3], 'phi_1': OptChoice.x[4], 'mu_h': OptChoice.x[5],
                          'logL': -OptChoice.fun, 'SSEd': SSEd, 'SSEw': SSEw, 'SSEm': SSEm, 'SSEa': SSEa, 
                          'NSEd': NSEd, 'NSEw': NSEw, 'NSEm': NSEm, 'NSEa': NSEa, 
                          'LNSEd': LNSEd, 'LNSEw': LNSEw, 'LNSEm': LNSEm, 'LNSEa': LNSEa, 
                          'pBias': pBias,
                          'success': OptChoice.success, 'mess': OptChoice.message}, ignore_index=True)
    else:
        #No successful convergence. Use the OptFailed
        Qdf = Qdf.append({'Replicate': i+1, 'beta': OptFailed.x[0], 'xi': OptFailed.x[1], 'sigma_0': OptFailed.x[2],
                          'sigma_1': OptFailed.x[3], 'phi_1': OptFailed.x[4], 'mu_h': OptFailed.x[5],
                          'logL': -OptFailed.fun, 'SSEd': SSEd, 'SSEw': SSEw, 'SSEm': SSEm, 'SSEa': SSEa, 
                          'NSEd': NSEd, 'NSEw': NSEw, 'NSEm': NSEm, 'NSEa': NSEa, 
                          'LNSEd': LNSEd, 'LNSEw': LNSEw, 'LNSEm': LNSEm, 'LNSEa': LNSEa, 
                          'pBias': pBias,
                          'success': OptFailed.success, 'mess': OptFailed.message}, ignore_index=True)

# write data frame to file
Qdf.to_csv('SA_Params_logLmNoBC_Baisman_Flow_rank' + str(rank) + '.csv', index=False)
#Qdf.to_csv(sys.argv[5] + '_Flow_rank' + str(rank) + '.csv', index=False)