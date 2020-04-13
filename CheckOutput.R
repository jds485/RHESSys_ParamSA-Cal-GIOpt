#This script is used to check output files for errors, tracebacks, and missing runs.

library(stringi)
library(stringr)

#Fixme: this function could have an option to process in parallel, but it's so fast to run as-is that may not be necessary.
#Fixme: could speed up by using scan instead of read.table. Or a vroom package command?
CheckOutput = function(numReps, #the number of replicates run
                       PlusString, #The output file name string format used to indicate numbers were added. At least 2 underscores must be used.
                       PlusNum, #The number to be added to all output file names that have PlusString in the name
                       wd #working directory full path
                       ){
  setwd(wd)
  #Output files
  ofs = grep(list.files(), pattern = '.out', ignore.case = FALSE, fixed = TRUE, value = TRUE)
  #Create a matrix to store the time cost in seconds
  #Columns: ind, Pyind, time
  time = matrix(NA, nrow = length(ofs), ncol = 3)
  #Create a matrix to store the errorss
  #Columns: ind, Pyind, error indicator (0/1), traceback indicator (0/1), first error message, first traceback message, 
  #         line of first error message, line of first traceback message.
  errs = as.data.frame(matrix(NA, nrow = length(ofs), ncol = 8))
  colnames(errs) = c('ind', 'Pyind', 'error', 'traceback', 'Emessage', 'Tmessage', 'Eline', 'Tline')
  for (i in 1:length(ofs)){
    f = read.table(file = ofs[i], header = FALSE, fill = TRUE, stringsAsFactors = FALSE, sep = '`')
    #Find the original and Python index that should be used for this variable.
    #Note that there are 2 possible stringsplit results because job arrays could not be submitted with ID numbers more than 4 digits long.
    if (length(strsplit(ofs[i], split = '_')[[1]]) == 2){
      ind = as.numeric(strsplit(strsplit(ofs[i], split = 'Run_')[[1]][2], '.out')[[1]])
      Pyind = ind - 1
    }else{
      #Have to add PlusNum to the number
      Pyind = as.numeric(strsplit(strsplit(ofs[i], split = PlusString)[[1]][2], '.out')[[1]]) + PlusNum
      ind = Pyind + 1
    }
    
    #Check for error messages
    errs$ind[i] = ind
    errs$Pyind[i] = Pyind
    #Search for tracebacks in the output file - results when node fails and it re-runs the code. Obviously there will be directory creation errors because they were already made!
    #Search for PyERROR
    #slurmstepd: error: *** JOB 6847502 ON udc-aw29-24b CANCELLED AT 2019-11-07T02:09:10 DUE TO TIME LIMIT ***
    #slurmstepd: error: *** JOB 6847502 STEPD TERMINATED ON udc-aw29-24b AT 2019-11-07T02:10:11 DUE TO JOB NOT ENDING WITH SIGNALS ***
    if (length(grep(f$V1, pattern = 'error', ignore.case = TRUE)) != 0){
      errs$error[i] = 1
      if (length(paste(grep(f$V1, pattern = 'error', ignore.case = TRUE, value = TRUE))) == 1){
        s = grep(f$V1, pattern = 'error', ignore.case = TRUE, value = TRUE)
        l = grep(f$V1, pattern = 'error', ignore.case = TRUE)
      }else{
        s = paste(grep(f$V1, pattern = 'error', ignore.case = TRUE, value = TRUE))[1]
        for (st in 1:(length(str_c(paste(grep(f$V1, pattern = 'error', ignore.case = TRUE, value = TRUE)), sep = ', '))-1)){
          s = str_c(paste(s, grep(f$V1, pattern = 'error', ignore.case = TRUE, value = TRUE)[st+1]), sep = ', ')
        }
        #Taking the first error line here. That's what matters
        l = grep(f$V1, pattern = 'error', ignore.case = TRUE)[1]
      }
      errs$Emessage[i] = s
      errs$Eline[i] = l
    }
    if (length(grep(f$V1, pattern = 'traceback', ignore.case = TRUE)) != 0){
      errs$traceback[i] = 1
      if (length(paste(grep(f$V1, pattern = 'traceback', ignore.case = TRUE, value = TRUE))) == 1){
        s = grep(f$V1, pattern = 'traceback', ignore.case = TRUE, value = TRUE)
        l = grep(f$V1, pattern = 'traceback', ignore.case = TRUE)
      }else{
        s = paste(grep(f$V1, pattern = 'traceback', ignore.case = TRUE, value = TRUE))[1]
        for (st in 1:(length(str_c(paste(grep(f$V1, pattern = 'traceback', ignore.case = TRUE, value = TRUE)), sep = ', '))-1)){
          s = str_c(paste(s, grep(f$V1, pattern = 'traceback', ignore.case = TRUE, value = TRUE)[st+1]), sep = ', ')
        }
        l = paste(grep(f$V1, pattern = 'traceback', ignore.case = TRUE))[1]
      }
      errs$Tmessage[i] = s
      errs$Tline[i] = l
    }
    
    #add to the time cost if that data are available
    time[i,1] = ind
    time[i,2] = Pyind
    if (length(grep(f$V1, pattern = 'time cost =')) != 0){
      time[i,3] = as.numeric(strsplit(strsplit(grep(f$V1, pattern = 'time cost =', value = TRUE), split = '= ')[[1]][2], split = ' seconds')[[1]][1])
    }
  }
  rm(i, ind, Pyind, s, l, f, st)
  
  #Find missing runs by job array index (not Python index - subtract 1 for the Python index):
  Missing = seq(1,numReps,1)[-which(seq(1,numReps,1) %in% errs$ind)]
  if (length(Missing) > 0){
    print(paste('Missing run for job array index', Missing))
  }
  
  #Python Tracebacks - These happened due to node failure.
  Trace = errs$ind[which(errs$traceback == 1)]
  
  #Collect the error indices that are not tracebacks
  Errors = errs$ind[which((errs$error == 1) & (is.na(errs$traceback)))]
  
  return(list(time = time, errs = errs, Missing = Missing, Tracebacks = Trace, Errors = Errors))
}

CheckLines = function(LineInds){
  #Array of reruns
  s = paste0(as.character(LineInds[1]), sep=',')
  for (st in 1:(length(LineInds)-1)){
    if (st == (length(LineInds)-1)){
      s = str_c(s, paste0(as.character(LineInds[st+1]), sep=''))
    }else{
      s = str_c(s, paste0(as.character(LineInds[st+1]), sep=',')) 
    }
  }
  rm(st)
  
  return(s)
}
