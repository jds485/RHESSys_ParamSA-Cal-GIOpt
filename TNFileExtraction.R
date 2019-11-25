#TN extraction on Rivanna

#Get the arg[1] value, which is the date column number to use
d = arg[1]

#Load in the columns (arg[2] is directory of file)
setwd(arg[2])
BasinSF_cols = read.table(file = 'SAResults_BasinStreamflow_p4.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE, nrows = 1)

BasinSF = read.table(file = 'SAResults_HillStreamflow_p6.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE, colClasses = c('numeric', rep(NULL,ncol(BasinSF_cols)-2)))
HillSF = read.table(file = 'SAResults_HillStreamflow_p6.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)

#Loop over the date to fill in the new dataset
a = matrix(NA, nrow = 3, ncol = ncol(BasinSF)-1)
h = matrix(NA, nrow = 3, ncol = ncol(BasinSF)-1)
rowt = as.numeric(rownames(TabInt))
colt = as.numeric(colnames(TabInt))
dtest = as.character(colnames(BasinSF)[d+1])
a = apply(X = t(BasinSF[,d+1]), MARGIN = 2, FUN = predictWRTDS, Date = dtest, rowt = rowt, colt = colt)
h = apply(X = t(HillSF[,d+2]), MARGIN = 2, FUN = predictWRTDS, Date = dtest, rowt = rowt, colt = colt)
  
#Write to txt file
write.table(x = a, file = paste0(arg[3],'/a_', d, '.txt'), sep = '\t')
write.table(x = h, file = paste0(arg[3],'/h_', d, '.txt'), sep = '\t')

tic = Sys.time()
BasinSF = read.table(file = 'SAResults_HillStreamflow_p6.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
toc = Sys.time()
print(toc-tic)
