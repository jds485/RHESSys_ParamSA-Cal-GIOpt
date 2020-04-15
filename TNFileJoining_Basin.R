#Combining the TN information into dataframes and writing txt file of the output
tic=Sys.time()

#Get the arg values
arg = commandArgs(trailingOnly = TRUE)

#Template for the column names
#Load in the transposed streamflow data with rows as the dates
setwd(arg[1])
BasinSF = read.table(file = arg[2], sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE, nrows = 1)
HillSF = read.table(file = arg[3], sep = '\t', stringsAsFactors = FALSE, check.names = FALSE, nrows = 2, skip=1, row.names = 1)

#Filenames from which data will be extracted
setwd(arg[4])
fs_b = grep(list.files(), pattern = 'a_', value = TRUE)

#Use temporary files to extract dataframes for storing basin and hillslope information
tempb = scan(file = paste0(getwd(), '/', fs_b[1]), sep = '\t', what = 'numeric', blank.lines.skip = TRUE, quiet = TRUE, nlines = 1)
BasinTN05 = BasinTN95 = BasinTNMed = matrix(NA, nrow = length(tempb), ncol = (1 + length(fs_b)))
rm(tempb)

toc=Sys.time()
print('Data setup time')
print(toc-tic)

tic=Sys.time()
#Loop over files and extract----
for (i in 1:(ncol(BasinSF)-1)){
  #Want the column index to match the file number that is read in
  ColInd_b = as.numeric(strsplit(strsplit(fs_b[i], split = 'a_')[[1]][2], '.txt')[[1]])
  #Get the basin file by number
  BasinTN05[,ColInd_b+1] = as.numeric(scan(file = paste0(getwd(), '/', fs_b[i]), sep = '\t', what = 'numeric', blank.lines.skip = TRUE, quiet = TRUE, nlines = 1))
  BasinTNMed[,ColInd_b+1] = as.numeric(scan(file = paste0(getwd(), '/', fs_b[i]), sep = '\t', what = 'numeric', blank.lines.skip = TRUE, quiet = TRUE, nlines = 1, skip = 1))
  BasinTN95[,ColInd_b+1] = as.numeric(scan(file = paste0(getwd(), '/', fs_b[i]), sep = '\t', what = 'numeric', blank.lines.skip = TRUE, quiet = TRUE, nlines = 1, skip = 2))
}

toc=Sys.time()
print('Loop time')
print(toc-tic)

tic=Sys.time()
#Add the 1st (and 2nd) columns for basin (and hillslope) (all the same as streamflow)
BasinTNMed[,1] = BasinTN05[,1] = BasinTN95[,1] = seq(1,ncol(HillSF)/length(unique(as.numeric(HillSF[2,]))),1)

#Add dates as column names
colnames(BasinTN05) = colnames(BasinTN95) = colnames(BasinTNMed) = colnames(BasinSF)

#Save TN timeseries----
#Save R data file
setwd(arg[1])
save.image(file = arg[5], safe = FALSE)

#tables
write.table(round(BasinTN05,3), file = arg[6], row.names = FALSE, sep = '\t')
write.table(round(BasinTNMed,3), file = arg[7], row.names = FALSE, sep = '\t')
write.table(round(BasinTN95,3), file = arg[8], row.names = FALSE, sep = '\t')

toc=Sys.time()
print('Write time')
print(toc-tic)