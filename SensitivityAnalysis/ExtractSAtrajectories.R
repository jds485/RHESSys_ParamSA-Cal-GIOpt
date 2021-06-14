#For extracting trajectory EEs on Rivanna using a SLURM array

#Load in t from the SLURM job array ID number
arg = commandArgs(trailingOnly = TRUE)
t = as.numeric(arg[1])

#Load file containing the input data needed for the loop
load(arg[3])

#Create a storage matrix for the EEs for each parameter
EEs05_b = EEs95_b = EEsot_b = EEsTN05_b = EEsTNMed_b = EEsTN95_b = matrix(NA, nrow = 1, ncol = cols)
#Need an extra column for the hillslope ID
EEs05_h = EEs95_h = EEsot_h = EEsTN05_h = EEsTNMed_h = EEsTN95_h = matrix(NA, nrow = length(uhills), ncol = cols+1)
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
  diff05 = (sum(abs((BasinSF05[ind+1,] - obs05$Flow))) - sum(abs((BasinSF05[ind,] - obs05$Flow))))  
  diff95 = (sum(abs((BasinSF95[ind+1,] - obs95$Flow))) - sum(abs((BasinSF95[ind,] - obs95$Flow))))
  diffot = (sum(abs((BasinSFot[ind+1,] - obsot$Flow))) - sum(abs((BasinSFot[ind,] - obsot$Flow))))
  
  #TN
  diffTN05 = (sum(abs((BasinTN05[ind+1,-1] - obsTN$TN))) - sum(abs((BasinTN05[ind,-1] - obsTN$TN))))  
  diffTNMed = (sum(abs((BasinTNMed[ind+1,-1] - obsTN$TN))) - sum(abs((BasinTNMed[ind,-1] - obsTN$TN))))
  diffTN95 = (sum(abs((BasinTN95[ind+1,-1] - obsTN$TN))) - sum(abs((BasinTN95[ind,-1] - obsTN$TN))))
  
  #Computes EEs with specified metric
  EEs05_b[1, parm] = diff05/delta
  EEs95_b[1, parm] = diff95/delta
  EEsot_b[1, parm] = diffot/delta
  
  EEsTN05_b[1, parm] = diffTN05/delta
  EEsTNMed_b[1, parm] = diffTNMed/delta
  EEsTN95_b[1, parm] = diffTN95/delta
  
  #Hillslopes
  #Compare to median streamflow and TN
  diff05_h = diff95_h = diffot_h = diffTN05_h = diffTNMed_h = diffTN95_h = NA
  for (hi in 1:length(uhills)){
    #Get the indices for this hillslope
    IndsHill = ind+(uhills[hi]-1)*nrow(BasinSF)
    #SF
    diff05_h = (sum(abs((HillSF05[IndsHill+1,] - MedHills05[which(MedHills05[,1] == uhills[hi]),-1]))) - sum(abs((HillSF05[IndsHill,] - MedHills05[which(MedHills05[,1] == uhills[hi]),-1]))))  
    diff95_h = (sum(abs((HillSF95[IndsHill+1,] - MedHills95[which(MedHills95[,1] == uhills[hi]),-1]))) - sum(abs((HillSF95[IndsHill,] - MedHills95[which(MedHills95[,1] == uhills[hi]),-1]))))
    diffot_h = (sum(abs((HillSFot[IndsHill+1,] - MedHillsot[which(MedHillsot[,1] == uhills[hi]),-1]))) - sum(abs((HillSFot[IndsHill,] - MedHillsot[which(MedHillsot[,1] == uhills[hi]),-1]))))
    
    #TN
    diffTN05_h = (sum(abs((HillTN05[IndsHill+1,-c(1,2)] - MedTN05Hills[which(MedTN05Hills[,1] == uhills[hi]),-1]))) - sum(abs((HillTN05[IndsHill,-c(1,2)] - MedTN05Hills[which(MedTN05Hills[,1] == uhills[hi]),-1]))))  
    diffTNMed_h = (sum(abs((HillTNMed[IndsHill+1,-c(1,2)] - MedTNMedHills[which(MedTNMedHills[,1] == uhills[hi]),-1]))) - sum(abs((HillTNMed[IndsHill,-c(1,2)] - MedTNMedHills[which(MedTNMedHills[,1] == uhills[hi]),-1]))))
    diffTN95_h = (sum(abs((HillTN95[IndsHill+1,-c(1,2)] - MedTN95Hills[which(MedTN95Hills[,1] == uhills[hi]),-1]))) - sum(abs((HillTN95[IndsHill,-c(1,2)] - MedTN95Hills[which(MedTN95Hills[,1] == uhills[hi]),-1]))))
    
    EEs05_h[hi, c(1,parm+1)] = c(uhills[hi], diff05_h/delta)
    EEs95_h[hi, c(1,parm+1)] = c(uhills[hi], diff95_h/delta)
    EEsot_h[hi, c(1,parm+1)] = c(uhills[hi], diffot_h/delta)
    
    EEsTN05_h[hi, c(1,parm+1)] = c(uhills[hi], diffTN05_h/delta)
    EEsTNMed_h[hi, c(1,parm+1)] = c(uhills[hi], diffTNMed_h/delta)
    EEsTN95_h[hi, c(1,parm+1)] = c(uhills[hi], diffTN95_h/delta)
  }
}
toc = Sys.time()
#32.5 mins per trajectory
print(toc-tic)
rm(i, hi, parm, ind, delta, diff05, diff05_h, diff95, diff95_h, diffot, diffot_h, diffTN05, diffTN05_h, diffTN95, diffTN95_h, diffTNMed, diffTNMed_h, IndsHill)

#Save the trajectory data as text files to the specified output location
out = as.character(arg[2])

write.table(EEs05_b, file = paste0(out, '/EEs05_b_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
write.table(EEs95_b, file = paste0(out, '/EEs95_b_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE) 
write.table(EEsot_b, file = paste0(out, '/EEsot_b_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
write.table(EEsTN05_b, file = paste0(out, '/EEsTN05_b_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE) 
write.table(EEsTNMed_b, file = paste0(out, '/EEsTNMed_b_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE) 
write.table(EEsTN95_b, file = paste0(out, '/EEsTN95_b_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
write.table(EEs05_h, file = paste0(out, '/EEs05_h_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
write.table(EEs95_h, file = paste0(out, '/EEs95_h_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
write.table(EEsot_h, file = paste0(out, '/EEsot_h_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
write.table(EEsTN05_h, file = paste0(out, '/EEsTN05_h_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE) 
write.table(EEsTNMed_h, file = paste0(out, '/EEsTNMed_h_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE) 
write.table(EEsTN95_h, file = paste0(out, '/EEsTN95_h_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
write.table(Deltas, file = paste0(out, '/Deltas_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)

save.image(file = paste0(out, '/EEs_', t,'.RData'))