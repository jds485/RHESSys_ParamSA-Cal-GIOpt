#Script to join the DREAM output timeseries into single files.

#1: number of chains 
#2: number of replicates in the chain
#3: directory with RHESSysRuns ('/nv/vol288/quinnlab-value/js4yd/Baisman30mDREAMzs/RHESSysRuns')
#4: Extension to output ('/RHESSys_Baisman30m_g74')
#5: save directory ('/nv/vol288/quinnlab-value/js4yd/Baisman30mDREAMzs/')
#6: starting value for the chain step index
#7: dataset name to append to column

arg = commandArgs(T)

nchain = as.numeric(arg[1])
nreps = as.numeric(arg[2])
startVal = as.numeric(arg[6])

#Make dataframes to store the compiled data
#Basin streamflow
Q_i = read.table(paste0(arg[3], '/Run', startVal, '_Ch1', arg[4], '/output/Q.txt'), header = TRUE)
Q = as.data.frame(matrix(NA, nrow = nrow(Q_i), ncol = nreps*nchain+1))
colnames(Q)[1] = 'Date'
Q[,1] = Q_i$Date
rm(Q_i)
#Likelihood Parameters
Params_i = read.csv(paste0(arg[3], '/Run', startVal, '_Ch1', arg[4], '/Params_logLQ_Run', startVal, '_Ch1.csv'))
Params_Q = as.data.frame(matrix(NA, nrow = nreps*nchain, ncol = ncol(Params_i)))
colnames(Params_Q) = colnames(Params_i)
rm(Params_i)
#Accept-Reject
if (startVal == 1){
  #First replicate is always accepted and no file is saved, so total files is nreps-1
  AR = as.data.frame(matrix(NA, nrow = nreps-1, ncol = nchain))
}else{
  AR = as.data.frame(matrix(NA, nrow = nreps, ncol = nchain))
}
colnames(AR) = paste0('Chain', seq(1,nchain,1))
#All Parameters
Params_i = read.csv(paste0(arg[3], '/Chain_', startVal, '_AfterProcessing.csv'), stringsAsFactors = FALSE)
Params = as.data.frame(matrix(NA, nrow = nreps*nchain, ncol = ncol(Params_i)+2))
colnames(Params) = c('Replicate', 'Chain', colnames(Params_i))
rm(Params_i)

#Begin loop to extract data  
for (num in 1:nreps){
  Ind = num+startVal-1
  for (i in 1:nchain){
    #Basin streamflow
    setwd(paste0(arg[3], '/Run', Ind, '_Ch', i, arg[4], '/output'))
    Q[, 1+nchain*(num-1)+i] = read.table('Q.txt', header = TRUE)[,2]
    colnames(Q)[1+nchain*(num-1)+i] = paste0('Run', Ind, '_Ch', i)
    
    #Likelihood
    Params_Q[nchain*(num-1)+i,] = read.csv(paste0(arg[3], '/Run', Ind, '_Ch', i, arg[4], '/Params_logLQ_', 'Run', Ind, '_Ch', i, '.csv'), stringsAsFactors = FALSE)
  }
  #Combine Accept-Reject info
  if (Ind > 1){
    if (startVal == 1){
      AR[num-1,] = read.table(paste0(arg[3], '/AcceptRejectRun', Ind, '.txt'), header=FALSE)
    }else{
      AR[num,] = read.table(paste0(arg[3], '/AcceptRejectRun', Ind, '.txt'), header=FALSE)
    }
  }
  
  #All Parameters
  Params[(1+(num-1)*nchain):(nchain+(num-1)*nchain),1] = Ind
  Params[(1+(num-1)*nchain):(nchain+(num-1)*nchain),2] = seq(1,nchain,1)
  Params[(1+(num-1)*nchain):(nchain+(num-1)*nchain),3:ncol(Params)] = read.csv(paste0(arg[3], '/Chain_', Ind, '_AfterProcessing.csv'), stringsAsFactors = FALSE)
}

#Save compiled data
setwd(arg[5])
write.table(Q, paste0('Q_c_s', Ind, '.txt'), row.names=FALSE, col.names=TRUE, sep='\t')
write.csv(Params_Q, paste0('LikeParamsQ_c_s', Ind, '.csv'), row.names=FALSE)
write.table(AR, paste0('AcceptReject_c_s', Ind, '.txt'), row.names=FALSE, col.names=TRUE, sep='\t')
write.csv(Params, paste0('Params_c_s', Ind, '.csv'), row.names=FALSE)

rm(Params_Q, Q, AR)

#Merge parameter and likelihood data to one csv file
Ind = nreps+startVal-1

#Make dataframes to store the compiled data
Params = read.csv(paste0('Params_c_s', Ind, '.csv'), stringsAsFactors = FALSE)
LikeQ = read.csv(paste0('LikeParamsQ_c_s', Ind, '.csv'), stringsAsFactors = FALSE)
ParamsLikes = as.data.frame(matrix(NA, nrow = nreps*nchain, ncol = 2+ncol(Params)+ncol(LikeQ)))
colnames(ParamsLikes) = c('ID', 'Dataset', colnames(Params), paste0(colnames(LikeQ), '_Q'))
#Add a dataset indicator
ParamsLikes$Dataset = arg[7]
ParamsLikes[,3:(ncol(Params)+2)] = Params
ParamsLikes[,(ncol(Params)+3):(ncol(Params)+ncol(LikeQ)+2)] = LikeQ

#Make a new global indicator = D<Dataset> + R<Replicate> + C<Chain>
ParamsLikes$ID = paste0('D', ParamsLikes$Dataset, '_R', ParamsLikes$Replicate, '_C', ParamsLikes$Chain)

#Check that the Chain and Replicate columns match for all 3 merged datasets
if (any(c(all(ParamsLikes$Replicate == ParamsLikes$Replicate_Q),
      all(ParamsLikes$Chain == ParamsLikes$Chain_Q)) == FALSE)){
  print('Some rows do not match. For loop join needed.')
  stop()      
}

#Save compiled data
setwd(arg[5])
write.csv(ParamsLikes, paste0('ParamsLikes_c_s', Ind, '.csv'), row.names=FALSE)