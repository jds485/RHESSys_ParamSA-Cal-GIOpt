import pandas as pd

#Fixme: Could use 1 loop in this script instead of 2.
Qdf = pd.DataFrame(columns=['Replicate','beta','xi','sigma_0','sigma_1','phi_1','mu_h','logL','SSE'])
nprocs = 300
for proc in range(nprocs):
	procParams = pd.read_csv('SA_Params_logL_Baisman_Flow_rank' + str(proc) + '.csv')
	Qdf = pd.concat([Qdf,procParams])

Qdf.to_csv('SA_Params_logL_Baisman_Flow.csv')

TNdf = pd.DataFrame(columns=['Replicate','beta','xi','sigma_0','sigma_1','phi_1','mu_h','logL','SSE'])
nprocs = 300
for proc in range(nprocs):
	procParams = pd.read_csv('SA_Params_logL_Baisman_TN_rank' + str(proc) + '.csv')
	TNdf = pd.concat([TNdf,procParams])

TNdf.to_csv('SA_Params_logL_Baisman_TN.csv')