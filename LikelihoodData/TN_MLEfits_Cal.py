import pandas as pd
import numpy as np
from scipy import optimize as sciOpt
#from mpi4py import MPI
import math
import pyDOE
import sys
import os

print('Starting TN_MLEfits.py')

#sys.argv contains: 
#0: unused - script call info
#1: step in chain, num
#2: chain number, i
#3: random seed - should be different for each step in chain. All chains processed at once in this script
#4: path to likelihood functions
#5: path to the processed observation .txt file
#6: Number of initial locations for the multi-start MLE solver

#Change to function directory and load function
owd = os.getcwd()
os.chdir(sys.argv[4])
from likelihood import generalizedLikelihoodFunction
os.chdir(owd)

# load TN observations
#10-01-04 through 09-30-13
TrueTN = pd.read_csv(sys.argv[5],delimiter='\t')
TrueTN['Date'] = pd.to_datetime(TrueTN['Date'],format="%Y-%m-%d")

# load TN simulations
#(10-01-04 through 9-30-13)
SimTN = pd.read_csv(owd + '/output/TN.txt',delimiter='\t')
SimTN['Date'] = pd.to_datetime(SimTN['Date'],format="%Y-%m-%d")

# create data frame to store parameter estimates, likelihood and SSE
TNdf = pd.DataFrame(columns=['Replicate', 'Chain', 'beta','xi','sigma_0','sigma_1','phi_1','mu_h','logL','SSE','success','mess'])

data = np.array(TrueTN['TN'].dropna())
tIndex = TrueTN['TN'].dropna().index

#Number of samples to take for the multi-start gradient descent algorithm
numsamps = int(sys.argv[6])

comparedata = np.array(SimTN['TN'].iloc[TrueTN['TN'].dropna().index])
ObjFunc = lambda params: generalizedLikelihoodFunction(data,comparedata,tIndex,params)

# find MLE fits for each simulation
# params = [beta, xi, sigma_0, sigma_1, phi_1, mu_h]
# initialize parameter estimates at beta=1 (double exponential), xi=0.5 (negatively skewed), 
# sigma_0 = 0.1, sigma_1 = 0.1, phi_1 = 0.7 (high auto-correlation), mu_h = 0.0 (unbiased)
#Make an LHS sample of the initial parameters to try for each replicate. Random seed is the index
np.random.seed(seed=int(sys.argv[3]))
paramsInit = pyDOE.lhs(n=6, samples=numsamps)

#Get all of the parameters into their expected ranges
#Initial bounds were [-1,2], [0,10], same, same, same, [0,100]
paramsInit[:,0] = paramsInit[:,0]*4. - 1.
paramsInit[:,1] = paramsInit[:,1]*5.
paramsInit[:,2] = paramsInit[:,2]*(1.-.000000001)+.000000001
#3 is on [0,1]
#4 is on [0,1]
paramsInit[:,5] = paramsInit[:,5]*(30.)

#Loop over the initial starting locations and select the most optimal parameter set
for j in range(numsamps):
    optParams = sciOpt.minimize(ObjFunc, 
                                paramsInit[j,:], 
                                method='SLSQP', 
                                bounds=[[-1,3],[0,5],[0.000000001,1],[0,1],[0,1],[0,30]],
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
    TNdf = TNdf.append({'Replicate': int(sys.argv[1]), 'Chain': int(sys.argv[2]), 'beta': OptChoice.x[0], 'xi': OptChoice.x[1], 'sigma_0': OptChoice.x[2],
                        'sigma_1': OptChoice.x[3], 'phi_1': OptChoice.x[4], 'mu_h': OptChoice.x[5],
                        'logL': -OptChoice.fun, 'SSE': np.sum((data-comparedata)**2), 'success': OptChoice.success,
                        'mess': OptChoice.message}, ignore_index=True)
else:
    #No successful convergence. Use the OptFailed
    TNdf = TNdf.append({'Replicate': int(sys.argv[1]), 'Chain': int(sys.argv[2]), 'beta': OptFailed.x[0], 'xi': OptFailed.x[1], 'sigma_0': OptFailed.x[2],
                        'sigma_1': OptFailed.x[3], 'phi_1': OptFailed.x[4], 'mu_h': OptFailed.x[5],
                        'logL': -OptFailed.fun, 'SSE': np.sum((data-comparedata)**2), 'success': OptFailed.success,
                        'mess': OptFailed.message}, ignore_index=True)
    
# write data frame to file
TNdf.to_csv('Params_logLTN_Run' + str(sys.argv[1]) + '_Ch' + str(sys.argv[2]) + '.csv', index=False)
print('End TN_MLEfits.py')