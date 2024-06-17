#Script to join the optimization output into single files.

library(stringi)
library(stringr)

#arguments are:
#1: directory with RHESSysRuns
#2: Extension to output ('/RHESSys_Baisman30m_g74')
#3: save directory
#4: original decision variables and objectives file with IDs for master and NFE
arg = commandArgs(T)

setwd(arg[1])

#Get list of all directories to search
#want all except output directory
fs = grep(x=list.files(), pattern='_Syn', value=TRUE)

#Make dataframes to store the compiled data
#read in decision vars and objectives saved in one file
DVO = read.table(arg[4], sep='\t', header=TRUE)
#Add columns for synthetic flood and low flow
DVO$FloodingSyn = NA
DVO$LowFlowSyn = NA
DVO$NumTreesSyn = NA

#Begin loop to extract data  
for (num in 1:length(fs)){
  #Row to replace
  n = as.numeric(str_split(str_split(fs[num], pattern='Run')[[1]][2], pattern='_')[[1]][1])
  
  #read in objectives
  DVOi = t(read.table(paste0(getwd(), '/', fs[num], arg[2], '/output/Run', n, '_Syn_Objs.txt'), sep='\t'))
  
  #Assign to original file  
  DVO$FloodingSyn[n] = DVOi[1,1]
  DVO$LowFlowSyn[n] = DVOi[1,2]
  DVO$NumTreesSyn[n] = DVOi[1,3]
}

#Save compiled data
setwd(arg[3])
write.table(DVO, paste0('DVO_c_MOROMinMaxSyn.txt'), row.names=FALSE, col.names=TRUE, sep='\t')