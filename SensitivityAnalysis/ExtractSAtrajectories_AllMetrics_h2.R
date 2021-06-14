#For extracting trajectory EEs on Rivanna using a SLURM array

#Load in t from the SLURM job array ID number
arg = commandArgs(trailingOnly = TRUE)
t = as.numeric(arg[1])

#Load file containing the input data needed for the loop
load(arg[3])

#Create a storage matrix for the EEs for each parameter
#Need an extra column for the hillslope ID
EEsotg_h = EEsTN05_h = EEsTNMed_h = EEsTN95_h = matrix(NA, nrow = length(uhills), ncol = cols+1)
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
  diffotg_h = diffTN05_h = diffTNMed_h = diffTN95_h = NA
  if (ind %in% c(645, 3366, 4914, 5389, 5838)){
    for (hi in 1:length(uhills)){
      #Get the indices for this hillslope
      IndsHill = ind+(uhills[hi]-1)*nrow(BasinSF)
      diffotg_h = (sum(abs((HillSFotg[IndsHill+1,] - MedHillsotg[which(MedHillsotg[,1] == uhills[hi]),-1]))) - sum(abs((HillSFotg_pre[which((HillSF_pre$Replicate == ind) & (HillSF_pre$HillID == uhills[hi])),] - MedHillsotg[which(MedHillsotg[,1] == uhills[hi]),-1]))))
      
      #TN
      diffTN05_h = (sum(abs((HillTN05[IndsHill+1,-c(1,2)] - MedTN05Hills[which(MedTN05Hills[,1] == uhills[hi]),-1]))) - sum(abs((HillTN05_pre[which((HillSF_pre$Replicate == ind) & (HillSF_pre$HillID == uhills[hi])),-c(1,2)] - MedTN05Hills[which(MedTN05Hills[,1] == uhills[hi]),-1]))))  
      diffTNMed_h = (sum(abs((HillTNMed[IndsHill+1,-c(1,2)] - MedTNMedHills[which(MedTNMedHills[,1] == uhills[hi]),-1]))) - sum(abs((HillTNMed_pre[which((HillSF_pre$Replicate == ind) & (HillSF_pre$HillID == uhills[hi])),-c(1,2)] - MedTNMedHills[which(MedTNMedHills[,1] == uhills[hi]),-1]))))
      diffTN95_h = (sum(abs((HillTN95[IndsHill+1,-c(1,2)] - MedTN95Hills[which(MedTN95Hills[,1] == uhills[hi]),-1]))) - sum(abs((HillTN95_pre[which((HillSF_pre$Replicate == ind) & (HillSF_pre$HillID == uhills[hi])),-c(1,2)] - MedTN95Hills[which(MedTN95Hills[,1] == uhills[hi]),-1]))))
      
      EEsotg_h[hi, c(1,parm+1)] = c(uhills[hi], diffotg_h/delta)
      
      EEsTN05_h[hi, c(1,parm+1)] = c(uhills[hi], diffTN05_h/delta)
      EEsTNMed_h[hi, c(1,parm+1)] = c(uhills[hi], diffTNMed_h/delta)
      EEsTN95_h[hi, c(1,parm+1)] = c(uhills[hi], diffTN95_h/delta)
    }
  }else{
    for (hi in 1:length(uhills)){
      #Get the indices for this hillslope
      IndsHill = ind+(uhills[hi]-1)*nrow(BasinSF)
      diffotg_h = (sum(abs((HillSFotg[IndsHill+1,] - MedHillsotg[which(MedHillsotg[,1] == uhills[hi]),-1]))) - sum(abs((HillSFotg[IndsHill,] - MedHillsotg[which(MedHillsotg[,1] == uhills[hi]),-1]))))
      
      #TN
      diffTN05_h = (sum(abs((HillTN05[IndsHill+1,-c(1,2)] - MedTN05Hills[which(MedTN05Hills[,1] == uhills[hi]),-1]))) - sum(abs((HillTN05[IndsHill,-c(1,2)] - MedTN05Hills[which(MedTN05Hills[,1] == uhills[hi]),-1]))))  
      diffTNMed_h = (sum(abs((HillTNMed[IndsHill+1,-c(1,2)] - MedTNMedHills[which(MedTNMedHills[,1] == uhills[hi]),-1]))) - sum(abs((HillTNMed[IndsHill,-c(1,2)] - MedTNMedHills[which(MedTNMedHills[,1] == uhills[hi]),-1]))))
      diffTN95_h = (sum(abs((HillTN95[IndsHill+1,-c(1,2)] - MedTN95Hills[which(MedTN95Hills[,1] == uhills[hi]),-1]))) - sum(abs((HillTN95[IndsHill,-c(1,2)] - MedTN95Hills[which(MedTN95Hills[,1] == uhills[hi]),-1]))))
      
      EEsotg_h[hi, c(1,parm+1)] = c(uhills[hi], diffotg_h/delta)
      
      EEsTN05_h[hi, c(1,parm+1)] = c(uhills[hi], diffTN05_h/delta)
      EEsTNMed_h[hi, c(1,parm+1)] = c(uhills[hi], diffTNMed_h/delta)
      EEsTN95_h[hi, c(1,parm+1)] = c(uhills[hi], diffTN95_h/delta)
    }
  }
}
toc = Sys.time()
#32.5 mins per trajectory
print(toc-tic)
rm(i, hi, parm, ind, delta, diffotg_h, diffTN05_h, diffTN95_h, diffTNMed_h, IndsHill, HillSF, HillSFotg, HillTN05, HillTNMed, HillTN95, MedHillsotg)

#Save the trajectory data as text files to the specified output location
out = as.character(arg[2])

write.table(EEsotg_h, file = paste0(out, '/EEsotg_h_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
write.table(EEsTN05_h, file = paste0(out, '/EEsTN05_h_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE) 
write.table(EEsTNMed_h, file = paste0(out, '/EEsTNMed_h_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE) 
write.table(EEsTN95_h, file = paste0(out, '/EEsTN95_h_', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)
write.table(Deltas, file = paste0(out, '/Deltas_2h', t, '.txt'), sep = '\t', row.names = FALSE, col.names = FALSE)

#save.image(file = paste0(out, '/EEs_2h', t,'.RData'))