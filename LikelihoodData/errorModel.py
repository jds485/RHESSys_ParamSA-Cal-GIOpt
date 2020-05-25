import numpy as np
import pandas as pd
from scipy.signal import periodogram
from matplotlib import pyplot as plt
from scipy import stats as ss
import statsmodels.api as sm

#Load the observed flow and TN, and convert date to the correct format
TrueQ = pd.read_csv('C:\\Users\\js4yd\\Dropbox\\Jared-Julie-Share\\Data\\BaismanStreamflow_Cal.txt',delimiter='\t') #11-15-99 through 9-30-13
TrueTN = pd.read_csv('C:\\Users\\js4yd\\Dropbox\\Jared-Julie-Share\\Data\\TN_Cal.txt',delimiter='\t') #11-15-99 through 09-30-13

TrueQ['Date'] = pd.to_datetime(TrueQ['Date'],format="%Y-%m-%d")
TrueTN['Date'] = pd.to_datetime(TrueTN['Date'],format="%Y-%m-%d")

#load simulated Q and TN from SA experiment (11-15-99 through 9-30-10)
#Dec. 2019:
#SimQ_used = pd.read_csv('C:\\Users\\js4yd\\OneDrive - University of Virginia\\RHESSys_ParameterSA\\LikelihoodData\\SAResults_BasinStreamflow_p4.txt',delimiter='\t')
#SimTN_used = pd.read_csv('C:\\Users\\js4yd\\OneDrive - University of Virginia\\RHESSys_ParameterSA\\LikelihoodData\\SAResults_BasinTNMed_p3.txt',delimiter='\t')
SimQ = pd.read_csv('C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SAResults_BasinStreamflow_p4_Reordered_Add5_Likes.txt',delimiter='\t')
SimTN = pd.read_csv('C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SAResults_BasinTNMed_p3_All_Reordered_Add5_Likes.txt',delimiter='\t')

SimQ['Date'] = pd.to_datetime(SimQ['Date'],format="%Y-%m-%d")
SimTN['Date'] = pd.to_datetime(SimTN['Date'],format="%Y-%m-%d")

columns = SimQ.columns
'''
#compute matrix of residuals (only consider 10-01-2004 through 09-30-2010)
Qresid = SimQ[columns[1:]].iloc[1782:]
for col in columns[1:]:
    Qresid[col] = Qresid[col] - TrueQ['Flow'].iloc[1782:3973]
    
TNresid = SimTN[columns[1:]].iloc[1782:]
for col in columns[1:]:
    TNresid[col] = TNresid[col] - TrueTN['TN'].iloc[1782:3973]
    
# look for seasonality in residuals
fs = 1.0 / (3600.0*24.0) # 1/day = 1/(3600*24) Hz
f, A = periodogram(Qresid[columns[1]],fs)

# convert f from Hz to period in days
T = (1/(3600*24*f))
plt.plot(T,A)
plt.plot([365,365],[0,np.max(A)]) # clear peak at 1 year - need to remove seasonality
'''
# perform Box-Cox transformation of data
TrueQ_BC, Qlambda = ss.boxcox(TrueQ['Flow']+0.001) #19 0-flow days; next lowest 0.01; add constant of 0.001
_, TNlambda = ss.boxcox(TrueTN['TN'].dropna()+0.001) # no 0 concentrations, but has to be 0 on no-flow days, so add 0.001 (flow and TN ~same mean)
TrueTN_BC = ((TrueTN['TN']+0.001)**TNlambda-1)/TNlambda

SimQ_BC = SimQ.copy()
for col in columns[1:]:
    SimQ_BC[col] = ((SimQ[col]+0.001)**Qlambda-1)/Qlambda
    
SimTN_BC = SimTN.copy()
for col in columns[1:]:
    SimTN_BC[col] = ((SimTN[col]+0.001)**TNlambda-1)/TNlambda

Qresid_BC = SimQ_BC[columns].iloc[1782:]
for col in columns[1:]:
    Qresid_BC[col] = SimQ_BC[col].iloc[1782:] - TrueQ_BC[1782:3973]
    
TNresid_BC = SimTN_BC[columns].iloc[1782:]
for col in columns[1:]:
    TNresid_BC[col] = SimTN_BC[col].iloc[1782:] - TrueTN_BC[1782:3973]

# look for seasonality in Q residuals
fs = 1.0 / (3600.0*24.0) # 1/day = 1/(3600*24) Hz
f, A = periodogram(Qresid_BC[columns[1]],fs)

# convert f from Hz to period in days
T = (1/(3600*24*f))
plt.plot(T,A)
plt.plot([365,365],[0,np.max(A)]) # clear peak at 1 year - need to remove seasonality

# remove seasonality
Qresid_BC['Year'] = Qresid_BC['Date'].dt.year
Qresid_BC['Month'] = Qresid_BC['Date'].dt.month
monthlyMeans = Qresid_BC.groupby('Month').mean()
monthlyStds = Qresid_BC.groupby('Month').std()

Qresid_BC_Z = Qresid_BC

for col in columns[1:]:
    Mu = np.array(monthlyMeans[col][Qresid_BC['Date'].dt.month])
    Sigma = np.array(monthlyStds[col][Qresid_BC['Date'].dt.month])
    Qresid_BC_Z[col] = (Qresid_BC[col] - Mu) / Sigma
    
# plot standardized residual time series
fig = plt.figure()
ax1 = fig.add_subplot(3,1,1)
ax1.plot(Qresid_BC_Z['Date'],Qresid_BC_Z[columns[1]])
ax1.set_ylabel('De-seasonalized, standardized\nBox-Cox-space residuals')
ax2 = fig.add_subplot(3,1,2)
ax2.hist(Qresid_BC_Z[columns[1]])
ax2.set_xlabel('De-seasonalized, standardized\nBox-Cox-space residuals')
ax2.set_ylabel('Frequency')
ax3 = fig.add_subplot(3,1,3)
sm.qqplot(Qresid_BC_Z[columns[1]],ss.norm,fit=True,line='45',ax=ax3)
ax3.set_xlabel('Theoretical Normal Quantiles')
fig.set_size_inches([6.4,9.2])
fig.subplots_adjust(wspace=0.3,hspace=0.3)
fig.savefig('Replicate1_BC_diagnostics.png')
fig.clf()

# see if seasonality has been removed
fs = 1.0 / (3600.0*24.0) # 1/day = 1/(3600*24) Hz
f, A = periodogram(Qresid_BC_Z[columns[1]],fs)

# convert f from Hz to period in days
T = (1/(3600*24*f))
plt.plot(T,A)
plt.plot([365,365],[0,np.max(A)]) # peak gone

# see if Qresid_BC_Z can be modeled by AR1
fig = plt.figure()
ax1 = fig.add_subplot(2,1,1)
sm.graphics.tsa.plot_acf(Qresid_BC_Z[columns[1]],ax=ax1)
ax1.set_xlim([0,7]) # 1 week

ax2 = fig.add_subplot(2,1,2)
sm.graphics.tsa.plot_pacf(Qresid_BC_Z[columns[1]],ax=ax2)
ax2.set_xlim([0,7]) # 1 week
ax2.set_ylim([-1,1])

fig.set_size_inches([9,7.25])
fig.savefig('BC-space_Qresid_ACF_PACF.png')
fig.clf()

# compute correlation of Box-Cox TN residuals with Box-Cox Q residuals



# repeat with log transformation
TrueQ_log = np.log(TrueQ['Flow']+0.001) #19 0-flow days; next lowest 0.01; add constant of 0.001
TrueTN_log = np.log(TrueTN['TN']+0.001) # no 0 concentrations, but has to be 0 on no-flow days, so add 0.001 (flow and TN ~same mean)

SimQ_log = SimQ.copy()
for col in columns[1:]:
    SimQ_log[col] = np.log(SimQ[col]+0.001)
    
SimTN_log = SimTN.copy()
for col in columns[1:]:
    SimTN_log[col] = np.log(SimTN[col]+0.001)

Qresid_log = SimQ_log[columns].iloc[1782:]
for col in columns[1:]:
    Qresid_log[col] = SimQ_log[col].iloc[1782:] - TrueQ_log[1782:3973]

# look for seasonality in residuals
fs = 1.0 / (3600.0*24.0) # 1/day = 1/(3600*24) Hz
f, A = periodogram(Qresid_log[columns[1]],fs)

# convert f from Hz to period in days
T = (1/(3600*24*f))
plt.plot(T,A)
plt.plot([365,365],[0,np.max(A)]) # clear peak at 1 year - need to remove seasonality

# remove seasonality
Qresid_log['Year'] = Qresid_log['Date'].dt.year
Qresid_log['Month'] = Qresid_log['Date'].dt.month
monthlyMeans = Qresid_log.groupby('Month').mean()
monthlyStds = Qresid_log.groupby('Month').std()

Qresid_log_Z = Qresid_log

for col in columns[1:]:
    Mu = np.array(monthlyMeans[col][Qresid_log['Date'].dt.month])
    Sigma = np.array(monthlyStds[col][Qresid_log['Date'].dt.month])
    Qresid_log_Z[col] = (Qresid_log[col] - Mu) / Sigma

# plot standardized residual time series
fig = plt.figure()
ax1 = fig.add_subplot(3,1,1)
ax1.plot(Qresid_log_Z['Date'],Qresid_log_Z[columns[1]])
ax1.set_ylabel('De-seasonalized, standardized\nlog-space residuals')
ax2 = fig.add_subplot(3,1,2)
ax2.hist(Qresid_log_Z[columns[1]])
ax2.set_xlabel('De-seasonalized, standardized\nlog-space residuals')
ax2.set_ylabel('Frequency')
ax3 = fig.add_subplot(3,1,3)
sm.qqplot(Qresid_log_Z[columns[1]],ss.norm,fit=True,line='45',ax=ax3)
ax3.set_xlabel('Theoretical Normal Quantiles')
fig.set_size_inches([6.4,9.2])
fig.subplots_adjust(wspace=0.3,hspace=0.3)
fig.savefig('Replicate1_log_diagnostics.png')
fig.clf()

# see if seasonality has been removed
fs = 1.0 / (3600.0*24.0) # 1/day = 1/(3600*24) Hz
f, A = periodogram(Qresid_log_Z[columns[1]],fs)

# convert f from Hz to period in days
T = (1/(3600*24*f))
plt.plot(T,A)
plt.plot([365,365],[0,np.max(A)]) # peak gone

# see if Qresid_log_Z can be modeled by AR1
fig = plt.figure()
ax1 = fig.add_subplot(2,1,1)
sm.graphics.tsa.plot_acf(Qresid_log_Z[columns[1]],ax=ax1)
ax1.set_xlim([0,7]) # 1 week

ax2 = fig.add_subplot(2,1,2)
sm.graphics.tsa.plot_pacf(Qresid_log_Z[columns[1]],ax=ax2)
ax2.set_xlim([0,7]) # 1 week
ax2.set_ylim([-1,1])

fig.set_size_inches([9,7.25])
fig.savefig('Log-space_Qresid_ACF_PACF.png')
fig.clf()
