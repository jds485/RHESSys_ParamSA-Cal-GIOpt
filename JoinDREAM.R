#Script to join the DREAM output timeseries into single files.

#1: number of chains 
#2: number of replicates in the chain
#3: directory with RHESSysRuns ('/nv/vol288/quinnlab-value/js4yd/Baisman30mDREAMzs/RHESSysRuns')
#4: Extension to output ('/RHESSys_Baisman30m_g74')
#5: save directory ('/nv/vol288/quinnlab-value/js4yd/Baisman30mDREAMzs/')

arg = commandArgs(T)

nchain = as.numeric(arg[1])
nreps = as.numeric(arg[2])

#Make dataframes to store the compiled data
#Basin TN and streamflow
TN_i = read.table(paste0(arg[3], '/Run1_Ch1', arg[4], '/output/TN.txt'), header = TRUE)
TN = Q = as.data.frame(matrix(NA, nrow = nrow(TN_i), ncol = nreps*nchain+1))
colnames(TN)[1] = 'Date'
colnames(Q)[1] = 'Date'
TN[,1] = TN_i$Date
Q[,1] = TN_i$Date
rm(TN_i)
#Likelihood Parameters
Params_i = read.csv(paste0(arg[3], '/Run1_Ch1', arg[4], '/Params_logLTN_Run1_Ch1.csv'))
Params_Q = Params_TN = as.data.frame(matrix(NA, nrow = nreps*nchain, ncol = ncol(Params_i)))
colnames(Params_Q) = colnames(Params_TN) = colnames(Params_i)
rm(Params_i)
#Accept-Reject
AR = as.data.frame(matrix(NA, nrow = nreps-1, ncol = nchain))
colnames(AR) = paste0('Chain', seq(1,nchain,1))
  
for (num in 1:nreps){
  for (i in 1:nchain){
    #Basin streamflow and TN
    setwd(paste0(arg[3], '/Run', num, '_Ch', i, arg[4], '/output'))
    TN[, 1+nchain*(num-1)+i] = read.table('TN.txt', header = TRUE)[,2]
    Q[, 1+nchain*(num-1)+i] = read.table('Q.txt', header = TRUE)[,2]
    colnames(TN)[1+nchain*(num-1)+i] = paste0('Run', num, '_Ch', i) 
    colnames(Q)[1+nchain*(num-1)+i] = paste0('Run', num, '_Ch', i)
    
    #Likelihood
    Params_TN[nchain*(num-1)+i,] = read.csv(paste0(arg[3], '/Run', num, '_Ch', i, arg[4], '/Params_logLTN_', 'Run', num, '_Ch', i, '.csv'), stringsAsFactors = FALSE)
    Params_Q[nchain*(num-1)+i,] = read.csv(paste0(arg[3], '/Run', num, '_Ch', i, arg[4], '/Params_logLQ_', 'Run', num, '_Ch', i, '.csv'), stringsAsFactors = FALSE)
  }
  #Combine Accept-Reject info
  if (num > 1){
    AR[num-1,] = read.table(paste0(arg[3], '/AcceptRejectRun', num, '.txt'), header=FALSE)
  }
}

#Save compiled data
setwd(arg[5])
write.table(Q, 'Q_c.txt', row.names=FALSE, col.names=TRUE, sep='\t')
write.table(TN, 'TN_c.txt', row.names=FALSE, col.names=TRUE, sep='\t')
write.csv(Params_Q, 'LikeParamsQ_c.csv', row.names=FALSE)
write.csv(Params_TN, 'LikeParamsTN_c.csv', row.names=FALSE)
write.table(AR, 'AcceptReject_c.txt', row.names=FALSE, col.names=TRUE, sep='\t')
