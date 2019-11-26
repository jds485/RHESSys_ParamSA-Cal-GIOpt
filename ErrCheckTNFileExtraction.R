#Compare the output files from TN extraction and search for errors

setwd('/scratch/js4yd/MorrisSA/TNprocessing/output')
ofs = list.files()
#Scan every file for "error" and report the index numebr if it's there
errs = as.data.frame(matrix(NA, nrow = length(ofs), ncol = 8))
colnames(errs) = c('ind', 'error', 'Emessage', 'Eline')
for (i in 1:length(ofs)){
  f = scan(file = ofs[i], what = 'character', sep = '`', fill = TRUE, quiet = TRUE)
  ind = as.numeric(strsplit(strsplit(ofs[i], split = 'Run_')[[1]][2], '.out')[[1]])
  
  #Check for error messages
  errs$ind[i] = ind
  #Search for error in the output file
  #Example: slurmstepd: error: *** JOB 7208446 ON udc-ba25-35c0 CANCELLED AT 2019-11-26T01:57:31 DUE TO TIME LIMIT ***
  if (length(grep(f, pattern = 'error', ignore.case = TRUE)) != 0){
    errs$error[i] = 1
    #See if there is 1 or more errors
    if (length(paste(grep(f, pattern = 'error', ignore.case = TRUE, value = TRUE))) == 1){
      #String message
      s = grep(f, pattern = 'error', ignore.case = TRUE, value = TRUE)
      #Line of error in .out file
      l = grep(f, pattern = 'error', ignore.case = TRUE)
    }else{
      s = paste(grep(f, pattern = 'error', ignore.case = TRUE, value = TRUE))[1]
      for (st in 1:(length(str_c(paste(grep(f, pattern = 'error', ignore.case = TRUE, value = TRUE)), sep = ', '))-1)){
        s = str_c(paste(s, grep(f, pattern = 'error', ignore.case = TRUE, value = TRUE)[st+1]), sep = ', ')
      }
      #Taking the first error line here. That's what matters
      l = grep(f, pattern = 'error', ignore.case = TRUE)[1]
    }
    errs$Emessage[i] = s
    errs$Eline[i] = l
  }
}
print("Error indices:")
print(errs$ind[!is.na(errs$error)])
print("Error Messages:")
print(errs$error[!is.na(errs$error)])

#Find missing runs by index (not Python index - subtract 1 for the Python index):
Missing = seq(1,3973,1)[-which(seq(1,3973,1) %in% errs$ind)]
if (length(Missing) > 0){
  print("Missing:")
  print(Missing)
}



#read.table method works only when files have data. Some .out file have no data, and this crashes.
# for (i in 1:length(ofs)){
#   f = read.table(file = ofs[i], header = FALSE, fill = TRUE, stringsAsFactors = FALSE, sep = '`')
#   ind = as.numeric(strsplit(strsplit(ofs[i], split = 'Run_')[[1]][2], '.out')[[1]])
#   
#   #Check for error messages
#   errs$ind[i] = ind
#   #Search for error in the output file
#   #Example: slurmstepd: error: *** JOB 7208446 ON udc-ba25-35c0 CANCELLED AT 2019-11-26T01:57:31 DUE TO TIME LIMIT ***
#   if (length(grep(f$V1, pattern = 'error', ignore.case = TRUE)) != 0){
#     errs$error[i] = 1
#     #See if there is 1 or more errors
#     if (length(paste(grep(f$V1, pattern = 'error', ignore.case = TRUE, value = TRUE))) == 1){
#       #String message
#       s = grep(f$V1, pattern = 'error', ignore.case = TRUE, value = TRUE)
#       #Line of error in .out file
#       l = grep(f$V1, pattern = 'error', ignore.case = TRUE)
#     }else{
#       s = paste(grep(f$V1, pattern = 'error', ignore.case = TRUE, value = TRUE))[1]
#       for (st in 1:(length(str_c(paste(grep(f$V1, pattern = 'error', ignore.case = TRUE, value = TRUE)), sep = ', '))-1)){
#         s = str_c(paste(s, grep(f$V1, pattern = 'error', ignore.case = TRUE, value = TRUE)[st+1]), sep = ', ')
#       }
#       #Taking the first error line here. That's what matters
#       l = grep(f$V1, pattern = 'error', ignore.case = TRUE)[1]
#     }
#     errs$Emessage[i] = s
#     errs$Eline[i] = l
#   }
# }