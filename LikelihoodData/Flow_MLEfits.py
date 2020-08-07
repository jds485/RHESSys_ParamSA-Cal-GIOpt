import pandas as pd
import numpy as np
from scipy import optimize as sciOpt
from scipy import stats as ss
from likelihood import generalizedLikelihoodFunction
from mpi4py import MPI
import math
import pyDOE
import sys

#All commented out for now. Use of sys.argv is untested.
#sys.argv contains: 
#0: unused - script call info
#1: initial random seed
#2: Full path to streamflow observations .txt file
#3: Full path to simulated streamflow .txt file
#4: Number of initial locations for the multi-start MLE solver
#5: Prefix for the output file name

# load flow observations
#TrueQ = pd.read_csv('C:\\Users\\js4yd\\Dropbox\\Jared-Julie-Share\\Data\\BaismanStreamflow_Cal.txt',delimiter='\t') #11-15-99 through 9-30-13
TrueQ = pd.read_csv('BaismanStreamflow_Cal.txt',delimiter='\t') #11-15-99 through 9-30-13
#TrueQ = pd.read_csv(sys.argv[2],delimiter='\t') #11-15-99 through 9-30-13
TrueQ['Date'] = pd.to_datetime(TrueQ['Date'],format="%Y-%m-%d")

# load flow simulations
#Dec. 2019
#SimQ = pd.read_csv('SAResults_BasinStreamflow_p4.txt',delimiter='\t') #(11-15-99 through 9-30-10)
#SimQ = pd.read_csv('C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SAResults_BasinStreamflow_p4_Reordered_Add5_Likes.txt',delimiter='\t')
SimQ = pd.read_csv('SAResults_BasinStreamflow_p4_Reordered_Add5_Likes.txt',delimiter='\t')
#SimQ = pd.read_csv(sys.argv[3],delimiter='\t')
SimQ['Date'] = pd.to_datetime(SimQ['Date'],format="%Y-%m-%d")

columns = SimQ.columns

# perform Box-Cox transformation of data - MLE lambda
TrueQ_BC, Qlambda = ss.boxcox(TrueQ['Flow']+0.001) #19 0-flow days; next lowest 0.01; add constant of 0.001
#Transform simulated data
for col in columns[1:]:
    SimQ[col] = ((SimQ[col]+0.001)**Qlambda-1)/Qlambda

# create data frame to store parameter estimates, likelihood and SSE
Qdf = pd.DataFrame(columns=['Replicate','beta','xi','sigma_0','sigma_1','phi_1','mu_h','logL','SSE','success','mess'])
data = np.array(TrueQ_BC[1782:3973])
tIndex = TrueQ['Flow'].iloc[1782:3973].index
#month = TrueQ['Date'].iloc[1782:3973].dt.month

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
numsamps = 20
#numsamps = int(sys.argv[4])
#Create dataframe to store successful parameter sets
#Qdf_success = pd.DataFrame(columns=['beta','xi','sigma_0','sigma_1','phi_1','mu_h','logL'])

for i in range(start,stop):
    comparedata = np.array(SimQ['Replicate' + str(i+1)].iloc[1782:])
    ObjFunc = lambda params: generalizedLikelihoodFunction(data,comparedata,tIndex,params)
    
    # find MLE fits for each simulation
    # params = [beta, xi, sigma_0, sigma_1, phi_1, mu_h]
    # initialize parameter estimates at beta=1 (double exponential), xi=0.5 (negatively skewed), 
    # sigma_0 = 0.1, sigma_1 = 0.1, phi_1 = 0.7 (high auto-correlation), mu_h = 0.0 (unbiased)
    #Make an LHS sample of the initial parameters to try for each replicate. Random seed is the index
    np.random.seed(seed=i+518)
	#np.random.seed(seed=i+int(sys.argv[1]))
    paramsInit = pyDOE.lhs(n=6, samples=numsamps)
    
    #Get all of the parameters into their expected ranges
    #Initial bounds were [-1,10], [0,10], same, same, same, [0,100]
    paramsInit[:,0] = paramsInit[:,0]*8. - 1.
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
                                    bounds=[[-1,7],[0,5],[0.000000001,1],[0,1],[0,1],[0,30]],
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
                          'logL': -OptChoice.fun, 'SSE': np.sum((data-comparedata)**2), 'success': OptChoice.success,
                          'mess': OptChoice.message}, ignore_index=True)
    else:
        #No successful convergence. Use the OptFailed
        Qdf = Qdf.append({'Replicate': i+1, 'beta': OptFailed.x[0], 'xi': OptFailed.x[1], 'sigma_0': OptFailed.x[2],
                          'sigma_1': OptFailed.x[3], 'phi_1': OptFailed.x[4], 'mu_h': OptFailed.x[5],
                          'logL': -OptFailed.fun, 'SSE': np.sum((data-comparedata)**2), 'success': OptFailed.success,
                          'mess': OptFailed.message}, ignore_index=True)

# write data frame to file
Qdf.to_csv('SA_Params_logL_Baisman_Flow_rank' + str(rank) + '.csv', index=False)
#Qdf.to_csv(sys.argv[5] + '_Flow_rank' + str(rank) + '.csv', index=False)