import pandas as pd
import numpy as np
from scipy import optimize as sciOpt
from likelihood import generalizedLikelihoodFunction
#from matplotlib import pyplot as plt
from mpi4py import MPI
import math
import pyDOE

# load TN observations
TrueTN = pd.read_csv('C:\\Users\\js4yd\\Dropbox\\Jared-Julie-Share\\Data\\TN_Cal.txt',delimiter='\t') #11-15-99 through 09-30-13
TrueTN['Date'] = pd.to_datetime(TrueTN['Date'],format="%Y-%m-%d")

# load TN simulations
#Dec. 2019
#SimTN = pd.read_csv('SAResults_BasinTNMed_p3.txt',delimiter='\t')
SimTN = pd.read_csv('C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SAResults_BasinTNMed_p3_All_Reordered_Add5_Likes.txt',delimiter='\t') #(11-15-99 through 9-30-10)
SimTN['Date'] = pd.to_datetime(SimTN['Date'],format="%Y-%m-%d")

columns = SimTN.columns

# create data frame to store parameter estimates, likelihood and SSE
TNdf = pd.DataFrame(columns=['Replicate','beta','xi','sigma_0','sigma_1','phi_1','mu_h','logL','SSE','success','mess'])
data = np.array(TrueTN['TN'].iloc[1782:3973].dropna())
tIndex = TrueTN['TN'].iloc[1782:3973].dropna().index

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
#Create dataframe to store successful parameter sets
TNdf_success = pd.DataFrame(columns=['beta','xi','sigma_0','sigma_1','phi_1','mu_h','logL'])

for i in range(start,stop):
    comparedata = np.array(SimTN['Replicate' + str(i+1)].iloc[TrueTN['TN'].iloc[1782:3973].dropna().index])
    ObjFunc = lambda params: generalizedLikelihoodFunction(data,comparedata,tIndex,params)
    
    # find MLE fits for each simulation
    # params = [beta, xi, sigma_0, sigma_1, phi_1, mu_h]
    # initialize parameter estimates at beta=1 (double exponential), xi=0.5 (negatively skewed), 
    # sigma_0 = 0.1, sigma_1 = 0.1, phi_1 = 0.7 (high auto-correlation), mu_h = 0.0 (unbiased)
    #Make an LHS sample of the initial parameters to try for each replicate. Random seed is the index
    np.random.seed(seed=i+185)
    paramsInit = pyDOE.lhs(n=6, samples=numsamps)
    
    #Get all of the parameters into their expected ranges
    #Fixme: try with different bounds that are adjusted based on SA run values.
    paramsInit[:,0] = paramsInit[:,0]*3. - 1.
    paramsInit[:,1] = paramsInit[:,1]*10.
    paramsInit[:,2] = paramsInit[:,2]*(1.-.000000001)+.000000001
    #3 is on [0,1]
    #4 is on [0,1]
    paramsInit[:,5] = paramsInit[:,5]*(100.)

    #Loop over the initial starting locations and select the most optimal parameter set
    for j in range(numsamps):
        optParams = sciOpt.minimize(ObjFunc, 
                                    paramsInit[j,:], 
                                    method='SLSQP', 
                                    bounds=[[-1,2],[0,10],[0.000000001,1],[0,1],[0,1],[0,100]],
                                    options={'maxiter': 1000, 'disp': False})
        if j == 0:
            #Save the optimal successful convergence and unsuccessful convergence
            OptChoice = optParams
            OptFailed = optParams
        elif ((optParams.fun < OptChoice.fun) & (optParams.success == True)):
            OptChoice = optParams
        elif ((optParams.fun < OptFailed.fun) & (optParams.success == False)):
            OptFailed = optParams
            
        #Used to see the distribution of parameter values with different starting locations
        if (optParams.success == True):
            #Save parameter vector
            TNdf_success = TNdf_success.append({'beta': optParams.x[0], 'xi': optParams.x[1], 'sigma_0': optParams.x[2],
                        'sigma_1': optParams.x[3], 'phi_1': optParams.x[4], 'mu_h': optParams.x[5],
                        'logL': -optParams.fun}, ignore_index=True)
    
    #Check if there are any successes
    if OptChoice.success == True:        
        #Assign the best parameter values to this ith replicate
        TNdf = TNdf.append({'Replicate': i+1, 'beta': OptChoice.x[0], 'xi': OptChoice.x[1], 'sigma_0': OptChoice.x[2],
                            'sigma_1': OptChoice.x[3], 'phi_1': OptChoice.x[4], 'mu_h': OptChoice.x[5],
                            'logL': -OptChoice.fun, 'SSE': np.sum((data-comparedata)**2), 'success': OptChoice.success,
                            'mess': OptChoice.message}, ignore_index=True)
    else:
        #No successful convergence. Use the OptFailed
        TNdf = TNdf.append({'Replicate': i+1, 'beta': OptFailed.x[0], 'xi': OptFailed.x[1], 'sigma_0': OptFailed.x[2],
                            'sigma_1': OptFailed.x[3], 'phi_1': OptFailed.x[4], 'mu_h': OptFailed.x[5],
                            'logL': -OptFailed.fun, 'SSE': np.sum((data-comparedata)**2), 'success': OptFailed.success,
                            'mess': OptFailed.message}, ignore_index=True)
    
# write data frame to file
TNdf.to_csv('SA_Params_logL_Baisman_TN_rank' + str(rank) + '.csv', index=False)

# sanity check: sort by -logL and SSE and compare ranks
# logLranks = np.argsort(TNdf['logL'])[::-1]
# SSEranks = np.argsort(TNdf['SSE'])
# plt.scatter(logLranks,SSEranks)
# plt.title('Spearman Correlation= ' + str(np.corrcoef(logLranks,SSEranks)[0,1]))
# plt.xlabel('Ranks by logL')
# plt.ylabel('Ranks by SSE')
# plt.savefig('TN_Ranks_logL_v_SSE.png')
# plt.clf()