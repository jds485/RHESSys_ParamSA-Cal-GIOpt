import pandas as pd

#Fixme: Could use 1 loop in this script instead of 2.
Qdf = pd.DataFrame(columns=['Replicate','beta','xi','sigma_0','sigma_1','phi_1','mu_h','logL','SSEdBC','SSEd','SSEw','SSEm','SSEa','NSEdBC','NSEd','NSEw','NSEm','NSEa','LNSEd','LNSEw','LNSEm','LNSEa','pBias','success','mess'])
nprocs = 5
for proc in range(nprocs):
	procParams = pd.read_csv('SA_Params_logLNoBC200pre_Baisman_Flow_rank' + str(proc) + '.csv')
	Qdf = pd.concat([Qdf,procParams], sort=False)

Qdf.to_csv('SA_Params_logLNoBC_Baisman_Flow_SQL_max1000_200samps_pre.csv', index=False)