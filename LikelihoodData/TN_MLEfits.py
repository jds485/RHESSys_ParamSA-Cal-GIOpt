import pandas as pd
import numpy as np
from scipy import optimize as sciOpt
from likelihood import generalizedLikelihoodFunction
#from matplotlib import pyplot as plt
from mpi4py import MPI
import math

# load TN observations
TrueTN = pd.read_csv('TN_Cal.txt',delimiter='\t') #11-15-99 through 09-30-13
TrueTN['Date'] = pd.to_datetime(TrueTN['Date'],format="%Y-%m-%d")

SimTN = pd.read_csv('SAResults_BasinTNMed_p3.txt',delimiter='\t') #(11-15-99 through 9-30-10)
SimTN['Date'] = pd.to_datetime(SimTN['Date'],format="%Y-%m-%d")

columns = SimTN.columns

# find MLE fits for each simulation
# params = [beta, xi, sigma_0, sigma_1, phi_1, mu_h]
# initialize parameter estimates at beta=1 (double exponential), xi=0.5 (negatively skewed), 
# sigma_0 = 0.1, sigma_1 = 0.1, phi_1 = 0.7 (high auto-correlation), mu_h = 0.0 (unbiased)
paramsInit = [1.0,0.5,0.1,0.1,0.7,0.0]

# create data frame to store parameter estimates, likelihood and SSE
TNdf = pd.DataFrame(columns=['Replicate','beta','xi','sigma_0','sigma_1','phi_1','mu_h','logL','SSE'])
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

for i in range(start,stop):
    comparedata = np.array(SimTN['Replicate' + str(i+1)].iloc[TrueTN['TN'].iloc[1782:3973].dropna().index])
    ObjFunc = lambda params: generalizedLikelihoodFunction(data,comparedata,tIndex,params)
    optParams = sciOpt.minimize(ObjFunc, paramsInit, method='TNC', bounds=[[-1,1],[0,10],[0.01,1],[0.01,1],[0,1],[0,100]])
    TNdf = TNdf.append({'Replicate': i+1, 'beta': optParams.x[0], 'xi': optParams.x[1], 'sigma_0': optParams.x[2],
                'sigma_1': optParams.x[3], 'phi_1': optParams.x[4], 'mu_h': optParams.x[5],
                'logL': -optParams.fun, 'SSE': np.sum((data-comparedata)**2)}, ignore_index=True)
    
# write data frame to file
TNdf.to_csv('SA_Params_logL_Baisman_TN_rank' + str(rank) + '.csv')

# sanity check: sort by -logL and SSE and compare ranks
# logLranks = np.argsort(TNdf['logL'])[::-1]
# SSEranks = np.argsort(TNdf['SSE'])
# plt.scatter(logLranks,SSEranks)
# plt.title('Spearman Correlation= ' + str(np.corrcoef(logLranks,SSEranks)[0,1]))
# plt.xlabel('Ranks by logL')
# plt.ylabel('Ranks by SSE')
# plt.savefig('TN_Ranks_logL_v_SSE.png')
# plt.clf()