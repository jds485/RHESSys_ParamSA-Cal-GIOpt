import pandas as pd
import numpy as np
from matplotlib import pyplot as plt

Qdf = pd.read_csv('SA_Params_logL_Baisman_Flow.csv')

logLranks = np.argsort(Qdf['logL'])[::-1]
SSEranks = np.argsort(Qdf['SSE'])
plt.scatter(logLranks,SSEranks)
plt.title('Spearman Correlation= ' + str(np.corrcoef(logLranks,SSEranks)[0,1]))
plt.xlabel('Ranks by logL (Flow)')
plt.ylabel('Ranks by SSE (Flow)')
plt.savefig('Flow_Ranks_logL_v_SSE.png')
plt.clf()

plt.scatter(-Qdf['logL'],Qdf['SSE'])
plt.title('Pearson Correlation= ' + str(np.corrcoef(-Qdf['logL'],Qdf['SSE'])[0,1]))
plt.xlabel('-logL (Flow)')
plt.ylabel('SSE (Flow)')
plt.savefig('Flow_logL_v_SSE.png')
plt.clf()

TNdf = pd.read_csv('SA_Params_logL_Baisman_TN.csv')

logLranks = np.argsort(TNdf['logL'])[::-1]
SSEranks = np.argsort(TNdf['SSE'])
plt.scatter(logLranks,SSEranks)
plt.title('Spearman Correlation= ' + str(np.corrcoef(logLranks,SSEranks)[0,1]))
plt.xlabel('Ranks by logL (TN)')
plt.ylabel('Ranks by SSE (TN)')
plt.savefig('TN_Ranks_logL_v_SSE.png')
plt.clf()

plt.scatter(-TNdf['logL'],TNdf['SSE'])
plt.title('Pearson Correlation= ' + str(np.corrcoef(-TNdf['logL'],TNdf['SSE'])[0,1]))
plt.xlabel('-logL (TN)')
plt.ylabel('SSE (TN)')
plt.savefig('TN_logL_v_SSE.png')
plt.clf()