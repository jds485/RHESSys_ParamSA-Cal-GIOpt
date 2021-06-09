#Script to join SA trajectory data
#Fixme: change hard-coded numbers, if possible.

#Load command line arguments
arg = commandArgs(trailingOnly = TRUE)

setwd(arg[1])

OrigParams = read.csv(arg[2], stringsAsFactors = FALSE)

Deltas = EEs05_b = EEs05g_b = EEs95_b = EEsot_b = EEsotg_b = EEsTN05_b = EEsTNMed_b = EEsTN95_b = EEsLogL_b = EEsNSE_b = EEsLNSE_b = EEspBias_b = matrix(NA, nrow = 40, ncol = 271)
EEs05_h = EEs05g_h = EEs95_h = EEsot_h = EEsotg_h = EEsTN05_h = EEsTNMed_h = EEsTN95_h = matrix(NA, nrow = 40*14, ncol = 272)

for (t in 1:40){
  EEs05_b[t,] = as.numeric(scan(file = paste0(getwd(), '/EEs05_b_', t, '.txt'), sep = '\t', what = 'numeric', quiet = TRUE))
  EEs95_b[t,] = as.numeric(scan(file = paste0(getwd(), '/EEs95_b_', t, '.txt'), sep = '\t', what = 'numeric', quiet = TRUE))
  EEsot_b[t,] = as.numeric(scan(file = paste0(getwd(), '/EEsot_b_', t, '.txt'), sep = '\t', what = 'numeric', quiet = TRUE))
  EEs05g_b[t,] = as.numeric(scan(file = paste0(getwd(), '/EEs05g_b_', t, '.txt'), sep = '\t', what = 'numeric', quiet = TRUE))
  EEsotg_b[t,] = as.numeric(scan(file = paste0(getwd(), '/EEsotg_b_', t, '.txt'), sep = '\t', what = 'numeric', quiet = TRUE))
  EEsLogL_b[t,] = as.numeric(scan(file = paste0(getwd(), '/EEsLogL_b_', t, '.txt'), sep = '\t', what = 'numeric', quiet = TRUE))
  EEsNSE_b[t,] = as.numeric(scan(file = paste0(getwd(), '/EEsNSE_b_', t, '.txt'), sep = '\t', what = 'numeric', quiet = TRUE))
  EEsLNSE_b[t,] = as.numeric(scan(file = paste0(getwd(), '/EEsLNSE_b_', t, '.txt'), sep = '\t', what = 'numeric', quiet = TRUE))
  EEspBias_b[t,] = as.numeric(scan(file = paste0(getwd(), '/EEspBias_b_', t, '.txt'), sep = '\t', what = 'numeric', quiet = TRUE))
  EEsTN05_b[t,] = as.numeric(scan(file = paste0(getwd(), '/EEsTN05_b_', t, '.txt'), sep = '\t', what = 'numeric', quiet = TRUE))
  EEsTNMed_b[t,] = as.numeric(scan(file = paste0(getwd(), '/EEsTNMed_b_', t, '.txt'), sep = '\t', what = 'numeric', quiet = TRUE))
  EEsTN95_b[t,] = as.numeric(scan(file = paste0(getwd(), '/EEsTN95_b_', t, '.txt'), sep = '\t', what = 'numeric', quiet = TRUE))
  Deltas[t,] = as.numeric(scan(file = paste0(getwd(), '/Deltas_', t, '.txt'), sep = '\t', what = 'numeric', quiet = TRUE))
  
  #Extracting in hillslope order for each trajectory
  EEs05_h[((t-1)*14+1):((t-1)*14+1+13),] = as.matrix(read.table(file = paste0(getwd(), '/EEs05_h_', t, '.txt'), sep = '\t', stringsAsFactors = FALSE, header = FALSE))
  EEs95_h[((t-1)*14+1):((t-1)*14+1+13),] = as.matrix(read.table(file = paste0(getwd(), '/EEs95_h_', t, '.txt'), sep = '\t', stringsAsFactors = FALSE, header = FALSE))
  EEsot_h[((t-1)*14+1):((t-1)*14+1+13),] = as.matrix(read.table(file = paste0(getwd(), '/EEsot_h_', t, '.txt'), sep = '\t', stringsAsFactors = FALSE, header = FALSE))
  EEs05g_h[((t-1)*14+1):((t-1)*14+1+13),] = as.matrix(read.table(file = paste0(getwd(), '/EEs05g_h_', t, '.txt'), sep = '\t', stringsAsFactors = FALSE, header = FALSE))
  EEsotg_h[((t-1)*14+1):((t-1)*14+1+13),] = as.matrix(read.table(file = paste0(getwd(), '/EEsotg_h_', t, '.txt'), sep = '\t', stringsAsFactors = FALSE, header = FALSE))
  EEsTN05_h[((t-1)*14+1):((t-1)*14+1+13),] = as.matrix(read.table(file = paste0(getwd(), '/EEsTN05_h_', t, '.txt'), sep = '\t', stringsAsFactors = FALSE, header = FALSE))
  EEsTNMed_h[((t-1)*14+1):((t-1)*14+1+13),] = as.matrix(read.table(file = paste0(getwd(), '/EEsTNMed_h_', t, '.txt'), sep = '\t', stringsAsFactors = FALSE, header = FALSE))
  EEsTN95_h[((t-1)*14+1):((t-1)*14+1+13),] = as.matrix(read.table(file = paste0(getwd(), '/EEsTN95_h_', t, '.txt'), sep = '\t', stringsAsFactors = FALSE, header = FALSE))
}

#set column names
colnames(Deltas) = colnames(EEs05_b) = colnames(EEs05g_b) = colnames(EEs95_b) = colnames(EEsot_b) = colnames(EEsotg_b) = colnames(EEsTN05_b) = colnames(EEsTNMed_b) = colnames(EEsTN95_b) = colnames(EEsLogL_b) = colnames(EEsNSE_b) = colnames(EEsLNSE_b) = colnames(EEspBias_b) = colnames(OrigParams)
colnames(EEs05_h) = colnames(EEs05g_h) = colnames(EEs95_h) = colnames(EEsot_h) = colnames(EEsotg_h) = colnames(EEsTN05_h) = colnames(EEsTNMed_h) = colnames(EEsTN95_h) = c('HillID', colnames(OrigParams))


#Write files
setwd(arg[3])
write.table(EEs05_b, file = paste0(getwd(), '/EEs05_b_All.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)
write.table(EEs95_b, file = paste0(getwd(), '/EEs95_b_All.txt'), sep = '\t', row.names = FALSE, col.names = TRUE) 
write.table(EEsot_b, file = paste0(getwd(), '/EEsot_b_All.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)
write.table(EEs05g_b, file = paste0(getwd(), '/EEs05g_b_All.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)
write.table(EEsotg_b, file = paste0(getwd(), '/EEsotg_b_All.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)
write.table(EEsLogL_b, file = paste0(getwd(), '/EEsLogL_b_All.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)
write.table(EEsNSE_b, file = paste0(getwd(), '/EEsNSE_b_All.txt'), sep = '\t', row.names = FALSE, col.names = TRUE) 
write.table(EEsLNSE_b, file = paste0(getwd(), '/EEsLNSE_b_All.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)
write.table(EEspBias_b, file = paste0(getwd(), '/EEspBias_b_All.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)
write.table(EEsTN05_b, file = paste0(getwd(), '/EEsTN05_b_All.txt'), sep = '\t', row.names = FALSE, col.names = TRUE) 
write.table(EEsTNMed_b, file = paste0(getwd(), '/EEsTNMed_b_All.txt'), sep = '\t', row.names = FALSE, col.names = TRUE) 
write.table(EEsTN95_b, file = paste0(getwd(), '/EEsTN95_b_All.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)
write.table(EEs05_h, file = paste0(getwd(), '/EEs05_h_All.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)
write.table(EEs95_h, file = paste0(getwd(), '/EEs95_h_All.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)
write.table(EEsot_h, file = paste0(getwd(), '/EEsot_h_All.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)
write.table(EEs05g_h, file = paste0(getwd(), '/EEs05g_h_All.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)
write.table(EEsotg_h, file = paste0(getwd(), '/EEsotg_h_All.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)
write.table(EEsTN05_h, file = paste0(getwd(), '/EEsTN05_h_All.txt'), sep = '\t', row.names = FALSE, col.names = TRUE) 
write.table(EEsTNMed_h, file = paste0(getwd(), '/EEsTNMed_h_All.txt'), sep = '\t', row.names = FALSE, col.names = TRUE) 
write.table(EEsTN95_h, file = paste0(getwd(), '/EEsTN95_h_All.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)
write.table(Deltas, file = paste0(getwd(), '/Deltas_All.txt'), sep = '\t', row.names = FALSE, col.names = TRUE)

save.image(file = paste0(getwd(), '/EEs_All.RData'))