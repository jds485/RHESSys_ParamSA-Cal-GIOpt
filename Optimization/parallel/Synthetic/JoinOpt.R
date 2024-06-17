#Script to join the optimization output into single files.

library(stringi)
library(stringr)

#arguments are:
#1: directory with RHESSysRuns
#2: Extension to output ('/RHESSys_Baisman30m_g74')
#3: save directory

arg = commandArgs(T)

setwd(arg[1])

#Get list of all directories to search
#want all except output directory
fs = grep(x=list.files(), pattern='output', invert=TRUE, value=TRUE)
#Remove ModifyVeg.py
fs = fs[fs != 'ModifyVeg.py']

#Make dataframes to store the compiled data
#names of first file variables
n1 = str_split(str_split(fs[1], pattern='Run')[[1]][2], pattern='_')[[1]][1]
P1 = str_split(str_split(str_split(fs[1], pattern='Run')[[1]][2], pattern='_')[[1]][2], pattern='P')[[1]][2]
M1 = str_split(str_split(str_split(fs[1], pattern='Run')[[1]][2], pattern='_')[[1]][3], pattern='M')[[1]][2]

#decision vars and objectives saved in one file
DVO1 = read.table(paste0(getwd(), '/', fs[1], arg[2], '/n', n1, '_i', P1, '_m', M1, '.txt'), sep='\t')
DVO1 = cbind(DVO1, t(read.table(paste0(getwd(), '/', fs[1], arg[2], '/output/Run', n1, '_P', P1, '_M', M1, '_Objs.txt'), sep='\t')))
DVO1 = cbind(as.numeric(n1), as.numeric(P1), as.numeric(M1), DVO1)
DVO = as.data.frame(matrix(NA, nrow = length(fs), ncol = ncol(DVO1)))
colnames(DVO) = c('N', 'P', 'M', 'H9d', 'H9m', 'H9u', 'H10d', 'H10m', 'H10u', 'Flood', 'LowFlow', 'NumTrees')
DVO[1,] = DVO1
rm(DVO1, n1, P1, M1)

#Begin loop to extract data  
for (num in 2:length(fs)){
  n = str_split(str_split(fs[num], pattern='Run')[[1]][2], pattern='_')[[1]][1]
  P = str_split(str_split(str_split(fs[num], pattern='Run')[[1]][2], pattern='_')[[1]][2], pattern='P')[[1]][2]
  M = str_split(str_split(str_split(fs[num], pattern='Run')[[1]][2], pattern='_')[[1]][3], pattern='M')[[1]][2]
  
  #decision vars and objectives saved in one file
  DVOi = read.table(paste0(getwd(), '/', fs[num], arg[2], '/n', n, '_i', P, '_m', M, '.txt'), sep='\t')
  DVOi = cbind(DVOi, t(read.table(paste0(getwd(), '/', fs[num], arg[2], '/output/Run', n, '_P', P, '_M', M, '_Objs.txt'), sep='\t')))
  DVOi = cbind(as.numeric(n), as.numeric(P), as.numeric(M), DVOi)
  
  DVO[num,] = DVOi
}

#Save compiled data
setwd(arg[3])
write.table(DVO, paste0('DVO_c_Syn.txt'), row.names=FALSE, col.names=TRUE, sep='\t')