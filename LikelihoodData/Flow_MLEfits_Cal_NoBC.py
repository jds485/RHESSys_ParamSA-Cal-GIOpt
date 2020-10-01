import pandas as pd
import numpy as np
from scipy import optimize as sciOpt
#from mpi4py import MPI
import math
import pyDOE
from scipy import stats as ss
import sys
import os

print('Starting Flow_MLEfits.py')

#sys.argv contains: 
#0: unused - script call info
#1: step in chain, num
#2: chain number, i
#3: random seed - should be different for each step in chain. All chains processed at once in this script
#4: directory of the likelihood function
#5: path to the processed observation .txt file
#6: Number of initial locations for the multi-start MLE solver

#Change to function directory and load function
owd = os.getcwd()
os.chdir(sys.argv[4])
from likelihood import generalizedLikelihoodFunction
os.chdir(owd)

# load flow observations
#10-01-04 through 9-30-13
TrueQ = pd.read_csv(sys.argv[5],delimiter='\t')
TrueQ['Date'] = pd.to_datetime(TrueQ['Date'],format="%Y-%m-%d")

# load flow simulations
#(10-01-04 through 9-30-13)
SimQ = pd.read_csv(owd + '/output/Q.txt', delimiter='\t')
SimQ['Date'] = pd.to_datetime(SimQ['Date'],format="%Y-%m-%d")
#Make a copy for use in computing metrics later
cSimQ = SimQ.copy(deep=True)

# create data frame to store parameter estimates, likelihood, SSE, NSE, LNSE, pBias (daily, weekly, monthly, annually)
Qdf = pd.DataFrame(columns=['Replicate', 'Chain', 'beta','xi','sigma_0','sigma_1','phi_1','mu_h','logL','SSEd','SSEw','SSEm','SSEa','NSEd','NSEw','NSEm','NSEa','LNSEd','LNSEw','LNSEm','LNSEa','pBias','success','mess'])

data = np.array(TrueQ['Flow'])
tIndex = TrueQ['Flow'].index

#Number of samples to take for the multi-start gradient descent algorithm
numsamps = int(sys.argv[6])

comparedata = np.array(SimQ['streamflow'])

#Compute the weekly, monthly, and annual timeseries from the daily timeseries
#Make a deep copy of this to compute weekly, monthly, and annual info later
cTrueQ = TrueQ.copy(deep=True)
cTrueQ = cTrueQ.set_index('Date')
cSimQ = cSimQ.set_index('Date')
#Change index to water year (shift to Jan 1 of following year so leap years match)
cTrueQ.index = cTrueQ.index.shift(92, freq = 'D')
cSimQ.index = cSimQ.index.shift(92, freq = 'D')
#Compute monthly and annual flows
cTrueQ['mFlow'] = cTrueQ['Flow'].resample('M').sum()
cTrueQ['aFlow'] = cTrueQ['Flow'].resample('A').sum()
mTrueQ = np.array(cTrueQ['mFlow'].iloc[np.where(np.isnan(cTrueQ['mFlow']) == False)[0]])
aTrueQ = np.array(cTrueQ['aFlow'].iloc[np.where(np.isnan(cTrueQ['aFlow']) == False)[0]])
cSimQ['mFlow'] = cSimQ['streamflow'].resample('M').sum()
cSimQ['aFlow'] = cSimQ['streamflow'].resample('A').sum()
mSimQ = np.array(cSimQ['mFlow'].iloc[np.where(np.isnan(cSimQ['mFlow']) == False)[0]])
aSimQ = np.array(cSimQ['aFlow'].iloc[np.where(np.isnan(cSimQ['aFlow']) == False)[0]])
#For weekly, need to loop and combine every 7 days, continuously
wTrueQ = np.zeros(len(aTrueQ)*52 + 1)
wSimQ = np.zeros(len(aSimQ)*52 + 1)
w = 0
for i in range(len(cTrueQ['Flow'])):
    if np.mod(i+1, 7) == 0:
        #Sum the last 6 days + this day
        wTrueQ[w] = cTrueQ['Flow'].iloc[(i-6):i+1].sum()
        wSimQ[w] = cSimQ['streamflow'].iloc[(i-6):i+1].sum()
        w = w + 1

#Compute metrics to append to dataframe:
#SSE
SSEd = np.sum((cTrueQ['Flow'] - cSimQ['streamflow'])**2)
SSEw = np.sum((wTrueQ - wSimQ)**2)
SSEm = np.sum((mTrueQ - mSimQ)**2)
SSEa = np.sum((aTrueQ - aSimQ)**2)
#NSE
NSEd = 1. - SSEd/np.sum((cTrueQ['Flow'] - np.mean(cTrueQ['Flow']))**2)
NSEw = 1. - SSEw/np.sum((wTrueQ - np.mean(wTrueQ))**2)
NSEm = 1. - SSEm/np.sum((mTrueQ - np.mean(mTrueQ))**2)
NSEa = 1. - SSEa/np.sum((aTrueQ - np.mean(aTrueQ))**2)
#LNSE
LNSEd = 1. - np.sum((np.log(cTrueQ['Flow']) - np.log(cSimQ['streamflow']))**2)/np.sum((np.log(cTrueQ['Flow']) - np.mean(np.log(cTrueQ['Flow'])))**2)
LNSEw = 1. - np.sum((np.log(wTrueQ) - np.log(wSimQ))**2)/np.sum((np.log(wTrueQ) - np.mean(np.log(wTrueQ)))**2)
LNSEm = 1. - np.sum((np.log(mTrueQ) - np.log(mSimQ))**2)/np.sum((np.log(mTrueQ) - np.mean(np.log(mTrueQ)))**2)
LNSEa = 1. - np.sum((np.log(aTrueQ) - np.log(aSimQ))**2)/np.sum((np.log(aTrueQ) - np.mean(np.log(aTrueQ)))**2)
#Percent Bias
pBias = 100. * (np.sum(cSimQ['streamflow'] - cTrueQ['Flow']) / np.sum(cTrueQ['Flow']))

ObjFunc = lambda params: generalizedLikelihoodFunction(data,comparedata,tIndex,params)
# find MLE fits for each simulation
# params = [beta, xi, sigma_0, sigma_1, phi_1, mu_h]
# initialize parameter estimates at beta (shape), xi (skew) 
# sigma_0 (err), sigma_1 (err trend with flow), phi_1 (auto-correlation), mu_h (bias)
#Make an LHS sample of the initial parameters to try for each replicate. Random seed is the index
np.random.seed(seed=int(sys.argv[3]))
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

#Check if there are any successes
if OptChoice.success == True:
    #Assign the best parameter values to this ith replicate
    Qdf = Qdf.append({'Replicate': int(sys.argv[1]), 'Chain': int(sys.argv[2]), 'beta': OptChoice.x[0], 'xi': OptChoice.x[1], 'sigma_0': OptChoice.x[2],
					  'sigma_1': OptChoice.x[3], 'phi_1': OptChoice.x[4], 'mu_h': OptChoice.x[5],
					  'logL': -OptChoice.fun, 'SSEd': SSEd, 'SSEw': SSEw, 'SSEm': SSEm, 'SSEa': SSEa, 
					  'NSEd': NSEd, 'NSEw': NSEw, 'NSEm': NSEm, 'NSEa': NSEa, 
					  'LNSEd': LNSEd, 'LNSEw': LNSEw, 'LNSEm': LNSEm, 'LNSEa': LNSEa, 
					  'pBias': pBias,
					  'success': OptChoice.success, 'mess': OptChoice.message}, ignore_index=True)
else:
    #No successful convergence. Use the OptFailed
    Qdf = Qdf.append({'Replicate': int(sys.argv[1]), 'Chain': int(sys.argv[2]), 'beta': OptFailed.x[0], 'xi': OptFailed.x[1], 'sigma_0': OptFailed.x[2],
					  'sigma_1': OptFailed.x[3], 'phi_1': OptFailed.x[4], 'mu_h': OptFailed.x[5],
					  'logL': -OptFailed.fun, 'SSEd': SSEd, 'SSEw': SSEw, 'SSEm': SSEm, 'SSEa': SSEa, 
					  'NSEd': NSEd, 'NSEw': NSEw, 'NSEm': NSEm, 'NSEa': NSEa, 
					  'LNSEd': LNSEd, 'LNSEw': LNSEw, 'LNSEm': LNSEm, 'LNSEa': LNSEa, 
					  'pBias': pBias,
					  'success': OptFailed.success, 'mess': OptFailed.message}, ignore_index=True)

# write data frame to file
Qdf.to_csv('Params_logLQ_Run' + str(sys.argv[1]) + '_Ch' + str(sys.argv[2]) + '.csv', index=False)
print('End Flow_MLEfits.py')