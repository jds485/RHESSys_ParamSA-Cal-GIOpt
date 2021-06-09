#For extracting trajectory EEs on Rivanna using a SLURM array

#Load in t from the SLURM job array ID number
arg = commandArgs(trailingOnly = TRUE)
t = as.numeric(arg[1])

#Load file containing the input data needed for the loop
load(arg[3])

#Create a storage matrix for the EEs for each parameter
#Need an extra column for the hillslope ID
EEs05_h = EEs05g_h = EEs95_h = EEsot_h = matrix(NA, nrow = length(uhills), ncol = cols+1)
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
      
  #Hillslopes
  #Compare to median streamflow and TN
  diff05_h = diff05g_h = diff95_h = diffot_h = NA
  if (ind %in% c(645, 3366, 4914, 5389, 5838)){
    for (hi in 1:length(uhills)){
      #Get the indices for this hillslope
      IndsHill = ind+(uhills[hi]-1)*nrow(BasinSF)
      #SF
      diff05_h = (sum(abs((HillSF05[IndsHill+1,] - MedHills05[which(MedHills05[,1] == uhills[hi]),-1]))) - sum(abs((HillSF05_pre[which((HillSF_pre$Replicate == ind) & (HillSF_pre$HillID == uhills[hi])),] - MedHills05[which(MedHills05[,1] == uhills[hi]),-1]))))  
      diff95_h = (sum(abs((HillSF95[IndsHill+1,] - MedHills95[which(MedHills95[,1] == uhills[hi]),-1]))) - sum(abs((HillSF95_pre[which((HillSF_pre$Replicate == ind) & (HillSF_pre$HillID == uhills[hi])),] - MedHills95[which(MedHills95[,1] == uhills[hi]),-1]))))
      diffot_h = (sum(abs((HillSFot[IndsHill+1,] - MedHillsot[which(MedHillsot[,1] == uhills[hi]),-1]))) - sum(abs((HillSFot_pre[which((HillSF_pre$Replicate == ind) & (HillSF_pre$HillID == uhills[hi])),] - MedHillsot[which(MedHillsot[,1] == uhills[hi]),-1]))))
      diff05g_h = (sum(abs((HillSF05g[IndsHill+1,] - MedHills05g[which(MedHills05g[,1] == uhills[hi]),-1]))) - sum(abs((HillSF05g_pre[which((HillSF_pre$Replicate == ind) & (HillSF_pre$HillID == uhills[hi])),] - MedHills05g[which(MedHills05g[,1] == uhills[hi]),-1]))))
      
      EEs05_h[hi, c(1,parm+1)] = c(uhills[hi], diff05_h/delta)
      EEs95_h[hi, c(1,parm+1)] = c(uhills[hi], diff95_h/delta)
      EEsot_h[hi, c(1,parm+1)] = c(uhills[hi], diffot_h/delta)
      EEs05g_h[hi, c(1,parm+1)] = c(uhills[hi], diff05g_h/delta)
    }
  }else{
    for (hi in 1:length(uhills)){
      #Get the indices for this hillslope
      IndsHill = ind+(uhills[hi]-1)*nrow(BasinSF)
      #SF
      diff05_h = (sum(abs((HillSF05[IndsHill+1,] - MedHills05[which(MedHills05[,1] == uhills[hi]),-1]))) - sum(abs((HillSF05[IndsHill,] - MedHills05[which(MedHills05[,1] == uhills[hi]),-1]))))  
      diff95_h = (sum(abs((HillSF95[IndsHill+1,] - MedHills95[which(MedHills95[,1] == uhills[hi]),-1]))) - sum(abs((HillSF95[IndsHill,] - MedHills95[which(MedHills95[,1] == uhills[hi]),-1]))))
      diffot_h = (sum(abs((HillSFot[IndsHill+1,] - MedHillsot[which(MedHillsot[,1] == uhills[hi]),-1]))) - sum(abs((HillSFot[IndsHill,] - MedHillsot[which(MedHillsot[,1] == uhills[hi]),-1]))))
  	diff05g_h = (sum(abs((HillSF05g[IndsHill+1,] - MedHills05g[which(MedHills05g[,1] == uhills[hi]),-1]))) - sum(abs((HillSF05g[IndsHill,] - MedHills05g[which(MedHills05g[,1] == uhills[hi]),-1]))))
      
      EEs05_h[hi, c(1,parm+1)] = c(uhills[hi], diff05_h/delta)
      EEs95_h[hi, c(1,parm+1)] = c(uhills[hi], diff95_h/delta)
      EEsot_h[hi, c(1,parm+1)] = c(uhills[hi], diffot_h/delta)
      EEs05g_h[hi, c(1,parm+1)] = c(uhills[hi], diff05g_h/delta)
    }
  }
}
toc = Sys.time()
#32.5 mins per trajectory
print(toc-tic)
rm(i, hi, parm, ind, delta, diff05_h, diff05g_h, diff95_h, diffot_h, IndsHill, HillSF05, HillSF, HillSFot, HillSF95, HillSF05g, MedHillsot)

#Save the trajectory data as text files to the specified output location
out = as.character(arg[2])

write.table(EEs05_h, file = paste0(out, '/EEs05_h_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
write.table(EEs95_h, file = paste0(out, '/EEs95_h_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
write.table(EEsot_h, file = paste0(out, '/EEsot_h_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
write.table(EEs05g_h, file = paste0(out, '/EEs05g_h_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
write.table(Deltas, file = paste0(out, '/Deltas_1h', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)

#save.image(file = paste0(out, '/EEs_1h', t,'.RData'))