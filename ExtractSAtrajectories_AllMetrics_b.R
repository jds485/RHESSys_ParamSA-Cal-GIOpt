#For extracting trajectory EEs on Rivanna using a SLURM array

#Load in t from the SLURM job array ID number
arg = commandArgs(trailingOnly = TRUE)
t = as.numeric(arg[1])

#Load file containing the input data needed for the loop
load(arg[3])

#Create a storage matrix for the EEs for each parameter
EEs05_b = EEs05g_b = EEs95_b = EEsot_b = EEsotg_b = EEsLogL_b = EEsNSE_b = EEsLNSE_b = EEspBias_b = matrix(NA, nrow = 1, ncol = cols)
#Create a stoage matrix for the deltas
Deltas = matrix(NA, nrow = 1, ncol = cols)
#Store the column names. These are the dates
colnms = colnames(BasinSF[,-1])

#Loop over the trajectories
tic = Sys.time()
#Compute the EEs for all parameters in the trajectory
for (i in 1:cols){
  #Determine the base index. This is used for basin, and the first hillslope value
  ind = i+(1+cols)*(t-1)
  
  #Find the parameter column that was changed, before any processing was completed. 
  #This is the same column that will be used for the Deltas and EEs
  parm = which((OrigParams[ind+1,] - OrigParams[ind,]) != 0)
  #Computes the exact delta from the modified sampling file. Allows for positive and negative deltas.
  delta = InputParams[ind+1,parm] - InputParams[ind,parm]
  #Adjust delta to the range sampled for the parameter
  delta = delta/abs(ParamRanges$Upper[parm] - ParamRanges$Lower[parm])
  Deltas[1, parm] = delta  
  #Fixme: record deltas for every variable, not only the parameter that changed
  #delta for other parameters could be larger than the OAT parameter that was supposed to change
  
  #Compute the difference for highest 5th percentile, lowest 5th percentile and all other flows, as defined by the days from the observed streamflow record
  #SSE - not using because we don't think it's appropriate for Morris method
  #Fixme: compare SSE to sum of absolute errors. Is there theoretical proof for Morris?
  #diff05 = (sum((BasinSF05[ind+1,] - obs05$Flow)^2) - sum((BasinSF05[ind,] - obs05$Flow)^2))  
  #diff95 = (sum((BasinSF95[ind+1,] - obs95$Flow)^2) - sum((BasinSF95[ind,] - obs95$Flow)^2))
  #diffot = (sum((BasinSFot[ind+1,] - obsot$Flow)^2) - sum((BasinSFot[ind,] - obsot$Flow)^2))
  
  #Fixme: Compare to using the median flow instead of the observed flow for basin
  
  #Sum absolute errors
  #Check if the ind is one of the 5 that needs special treatment
  if (ind %in% c(645, 3366, 4914, 5389, 5838)){
    diff05 = (sum(abs((BasinSF05[ind+1,] - obs05$Flow))) - sum(abs((BasinSF05_pre[Likes200_pre$Replicate == ind,] - obs05$Flow))))
    diff95 = (sum(abs((BasinSF95[ind+1,] - obs95$Flow))) - sum(abs((BasinSF95_pre[Likes200_pre$Replicate == ind,] - obs95$Flow))))
    diffot = (sum(abs((BasinSFot[ind+1,] - obsot$Flow))) - sum(abs((BasinSFot_pre[Likes200_pre$Replicate == ind,] - obsot$Flow)))) 
    diff05g = (sum(abs((BasinSF05g[ind+1,] - obs05g$Flow))) - sum(abs((BasinSF05g_pre[Likes200_pre$Replicate == ind,] - obs05g$Flow))))
    diffotg = (sum(abs((BasinSFotg[ind+1,] - obsotg$Flow))) - sum(abs((BasinSFotg_pre[Likes200_pre$Replicate == ind,] - obsotg$Flow))))
    
    diffLL = Likes200$logL[ind+1] - Likes200_pre$logL[Likes200_pre$Replicate == ind]
    diffNSE = Likes200$NSEd[ind+1] - Likes200_pre$NSEd[Likes200_pre$Replicate == ind]
    diffLNSE = Likes200$LNSEd[ind+1] - Likes200_pre$LNSEd[Likes200_pre$Replicate == ind] 
    diffBias = Likes200$pBias[ind+1] - Likes200_pre$pBias[Likes200_pre$Replicate == ind]
  }else{
    diff05 = (sum(abs((BasinSF05[ind+1,] - obs05$Flow))) - sum(abs((BasinSF05[ind,] - obs05$Flow))))
    diff95 = (sum(abs((BasinSF95[ind+1,] - obs95$Flow))) - sum(abs((BasinSF95[ind,] - obs95$Flow))))
    diffot = (sum(abs((BasinSFot[ind+1,] - obsot$Flow))) - sum(abs((BasinSFot[ind,] - obsot$Flow)))) 
    diff05g = (sum(abs((BasinSF05g[ind+1,] - obs05g$Flow))) - sum(abs((BasinSF05g[ind,] - obs05g$Flow))))
    diffotg = (sum(abs((BasinSFotg[ind+1,] - obsotg$Flow))) - sum(abs((BasinSFotg[ind,] - obsotg$Flow))))
    
    diffLL = Likes200$logL[ind+1] - Likes200$logL[ind]
    diffNSE = Likes200$NSEd[ind+1] - Likes200$NSEd[ind]
    diffLNSE = Likes200$LNSEd[ind+1] - Likes200$LNSEd[ind] 
    diffBias = Likes200$pBias[ind+1] - Likes200$pBias[ind]
  }
  
  #Computes EEs with specified metric
  EEs05_b[1, parm] = diff05/delta
  EEs95_b[1, parm] = diff95/delta
  EEsot_b[1, parm] = diffot/delta
  EEs05g_b[1, parm] = diff05g/delta
  EEsotg_b[1, parm] = diffotg/delta
  EEsLogL_b[1, parm] = diffLL/delta
  EEsNSE_b[1, parm] = diffNSE/delta
  EEsLNSE_b[1, parm] = diffLNSE/delta
  EEspBias_b[1, parm] = diffBias/delta
}
toc = Sys.time()
#32.5 mins per trajectory
print(toc-tic)
rm(i, parm, ind, delta, diff05, diff95, diffot, diff05g, diffotg, diffLL, diffNSE, diffLNSE, diffBias)

#Save the trajectory data as text files to the specified output location
out = as.character(arg[2])

write.table(EEs05_b, file = paste0(out, '/EEs05_b_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
write.table(EEs95_b, file = paste0(out, '/EEs95_b_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
write.table(EEsot_b, file = paste0(out, '/EEsot_b_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
write.table(EEs05g_b, file = paste0(out, '/EEs05g_b_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
write.table(EEsotg_b, file = paste0(out, '/EEsotg_b_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
write.table(EEsLogL_b, file = paste0(out, '/EEsLogL_b_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
write.table(EEsNSE_b, file = paste0(out, '/EEsNSE_b_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE) 
write.table(EEsLNSE_b, file = paste0(out, '/EEsLNSE_b_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
write.table(EEspBias_b, file = paste0(out, '/EEspBias_b_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
write.table(Deltas, file = paste0(out, '/Deltas_b', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)

#save.image(file = paste0(out, '/EEs_b', t,'.RData'))