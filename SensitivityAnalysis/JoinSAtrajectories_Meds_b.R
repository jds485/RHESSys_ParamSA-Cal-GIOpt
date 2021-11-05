#Script to join SA trajectory data
#Fixme: change hard-coded numbers, if possible.

#Load command line arguments
arg = commandArgs(trailingOnly = TRUE)

setwd(arg[1])

OrigParams = read.csv(arg[2], stringsAsFactors = FALSE)

EEs05gm_b = EEs95m_b = EEsotgm_b = matrix(NA, nrow = 40, ncol = 271)

for (t in 1:40){
  EEs95m_b[t,] = as.numeric(scan(file = paste0(getwd(), '/EEs95m_b_', t, '.txt'), sep = '\t', what = 'numeric', quiet = TRUE))
  EEs05gm_b[t,] = as.numeric(scan(file = paste0(getwd(), '/EEs05gm_b_', t, '.txt'), sep = '\t', what = 'numeric', quiet = TRUE))
  EEsotgm_b[t,] = as.numeric(scan(file = paste0(getwd(), '/EEsotgm_b_', t, '.txt'), sep = '\t', what = 'numeric', quiet = TRUE))
}

#set column names
colnames(EEs05gm_b) = colnames(EEs95m_b) = colnames(EEsotgm_b) = colnames(OrigParams)


#Write files
setwd(arg[3])
write.table(EEs95m_b, file = paste0(getwd(), '/EEs95m_b_All.txt'), sep = '\t', row.names = FALSE, col.names = TRUE) 
write.table(EEs05gm_b, file = paste0(getwd(), '/EEs05gm_b_All.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)
write.table(EEsotgm_b, file = paste0(getwd(), '/EEsotgm_b_All.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)

save.image(file = paste0(getwd(), '/EEs_Meds_b.RData'))