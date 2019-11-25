#TN extraction on Rivanna

library(EGRET)
library(survival)
library(pracma)

#Get the arg[1] value, which is the date column number to use
arg = commandArgs(trailingOnly = TRUE)
d = arg[1]

#Load in the modified code (arg[2] is directory of file)
setwd(arg[2])
source('WRTDS_modifiedFunctions.R')

#Load in the transposed streamflow data with rows as the dates
BasinSF = read.table(file = 'SAResults_BasinStreamflow_p4_t.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE, nrows = 1, skip=d+1, row.names = 1)
HillSF = read.table(file = 'SAResults_HillStreamflow_p6_t.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE, nrows = 1, skip=d+2, row.names = 1)

#Tables
TabInt = as.matrix(read.table(file = 'TabIntMod4_p5.txt', sep = '\t', header = TRUE, check.names = FALSE))
TabYear = as.matrix(read.table(file = 'TabYearMod4_p4.txt', sep = '\t', header = TRUE, check.names = FALSE))
TabLogQ = as.matrix(read.table(file = 'TabLogQMod4_p4.txt', sep = '\t', header = TRUE, check.names = FALSE))
TabSinYear = as.matrix(read.table(file = 'TabSinYearMod4_p4.txt', sep = '\t', header = TRUE, check.names = FALSE))
TabCosYear = as.matrix(read.table(file = 'TabCosYearMod4_p4.txt', sep = '\t', header = TRUE, check.names = FALSE))
TabLogErr = as.matrix(read.table(file = 'TabLogErrMod4_p5.txt', sep = '\t', header = TRUE, check.names = FALSE))

#Loop over the date to fill in the new dataset
a = matrix(NA, nrow = 3, ncol = ncol(BasinSF))
h = matrix(NA, nrow = 3, ncol = ncol(BasinSF))
rowt = as.numeric(rownames(TabInt))
colt = as.numeric(colnames(TabInt))
dtest = rownames(BasinSF)
a = apply(X = BasinSF, MARGIN = 2, FUN = predictWRTDS, Date = dtest, rowt = rowt, colt = colt)
h = apply(X = HillSF, MARGIN = 2, FUN = predictWRTDS, Date = dtest, rowt = rowt, colt = colt)

#Write to txt file (arg[3] is output directory)
write.table(x = a, file = paste0(arg[3],'/a_', d, '.txt'), sep = '\t', col.names = FALSE, row.names = FALSE)
write.table(x = h, file = paste0(arg[3],'/h_', d, '.txt'), sep = '\t', col.names = FALSE, row.names = FALSE)

# tic = Sys.time()
# d = 3900
# BasinSF = read.table(file = 'SAResults_BasinStreamflow_p4_t.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE, nrows = 1, skip=d+1, row.names = 1)
# rowt = as.numeric(rownames(TabInt))
# colt = as.numeric(colnames(TabInt))
# dtest = rownames(BasinSF)
# a = apply(X = BasinSF, MARGIN = 2, FUN = predictWRTDS, Date = dtest, rowt = rowt, colt = colt)
# HillSF = read.table(file = 'SAResults_HillStreamflow_p6_t.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE, nrows = 1, skip=d+2, row.names = 1)
# h = apply(X = HillSF, MARGIN = 2, FUN = predictWRTDS, Date = dtest, rowt = rowt, colt = colt)
# toc = Sys.time()
# print(toc-tic)