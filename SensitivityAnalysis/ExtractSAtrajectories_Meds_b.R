#For extracting trajectory EEs on Rivanna using a SLURM array

#Load in t from the SLURM job array ID number
arg = commandArgs(trailingOnly = TRUE)
t = as.numeric(arg[1])

#Load file containing the input data needed for the loop
load(arg[3])
load(arg[4])

#Create a storage matrix for the EEs for each parameter
EEs05gm_b = EEs95m_b = EEsotgm_b = matrix(NA, nrow = 1, ncol = cols)
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

  #Sum absolute median deviation
  #Check if the ind is one of the 5 that needs special treatment
  if (ind %in% c(645, 3366, 4914, 5389, 5838)){
    diff95m = (sum(abs((BasinSF95[ind+1,] - MedBasins95))) - sum(abs((BasinSF95_pre[Likes200_pre$Replicate == ind,] - MedBasins95))))
    diff05gm = (sum(abs((BasinSF05g[ind+1,] - MedBasins05g))) - sum(abs((BasinSF05g_pre[Likes200_pre$Replicate == ind,] - MedBasins05g))))
    diffotgm = (sum(abs((BasinSFotg[ind+1,] - MedBasinsotg))) - sum(abs((BasinSFotg_pre[Likes200_pre$Replicate == ind,] - MedBasinsotg))))    
  }else{
    diff95m = (sum(abs((BasinSF95[ind+1,] - MedBasins95))) - sum(abs((BasinSF95[ind,] - MedBasins95))))
    diff05gm = (sum(abs((BasinSF05g[ind+1,] - MedBasins05g))) - sum(abs((BasinSF05g[ind,] - MedBasins05g))))
    diffotgm = (sum(abs((BasinSFotg[ind+1,] - MedBasinsotg))) - sum(abs((BasinSFotg[ind,] - MedBasinsotg))))
  }
  
  #Computes EEs with specified metric
  EEs95m_b[1, parm] = diff95m/delta
  EEs05gm_b[1, parm] = diff05gm/delta
  EEsotgm_b[1, parm] = diffotgm/delta
}
toc = Sys.time()
#32.5 mins per trajectory
print(toc-tic)
rm(i, parm, ind, delta, diff95m, diff05gm, diffotgm)

#Save the trajectory data as text files to the specified output location
out = as.character(arg[2])

write.table(EEs95m_b, file = paste0(out, '/EEs95m_b_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
write.table(EEs05gm_b, file = paste0(out, '/EEs05gm_b_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
write.table(EEsotgm_b, file = paste0(out, '/EEsotgm_b_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)