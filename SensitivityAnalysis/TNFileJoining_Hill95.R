#Combining the TN information into dataframes and writing txt file of the output
tic=Sys.time()

#Get the arg values
arg = commandArgs(trailingOnly = TRUE)

#Template for the column names
#Load in the transposed streamflow data with rows as the dates
setwd(arg[1])
BasinSF = read.table(file = arg[2], sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE, nrows = 1)

#Get the 1st and 2nd rows for hillslopes from the streamflow file. These will be the same in the TN files.
HillSF1 = as.numeric(scan(file = arg[3], sep = '\t', what = 'numeric', blank.lines.skip = TRUE, quiet = TRUE, nlines = 1, skip=1, row.names)[-1])
HillSF2 = as.numeric(scan(file = arg[3], sep = '\t', what = 'numeric', blank.lines.skip = TRUE, quiet = TRUE, nlines = 1, skip=2, row.names)[-1])


#Filenames from which data will be extracted
setwd(arg[4])
fs_h = grep(list.files(), pattern = 'h_', value = TRUE)

#Use temporary files to extract dataframes for storing basin and hillslope information
temph = scan(file = paste0(getwd(), '/', fs_h[1]), sep = '\t', what = 'numeric', blank.lines.skip = TRUE, quiet = TRUE, nlines = 1)
HillTNMed = matrix(NA, nrow = length(temph), ncol = (2 + length(fs_h)))
rm(temph)

toc=Sys.time()
print('Data setup time')
print(toc-tic)

tic=Sys.time()
#Loop over files and extract----
for (i in 1:(ncol(BasinSF)-1)){
  #Want the column index to match the file number that is read in
  #Get the hillslope file by number
  ColInd_h = as.numeric(strsplit(strsplit(fs_h[i], split = 'h_')[[1]][2], '.txt')[[1]])
  HillTNMed[,ColInd_h+2] = as.numeric(scan(file = paste0(getwd(), '/', fs_h[i]), sep = '\t', what = 'numeric', blank.lines.skip = TRUE, quiet = TRUE, nlines = 1, skip = 2))
}

toc=Sys.time()
print('Loop time')
print(toc-tic)

tic=Sys.time()
#Add the 1st and 2nd columns for hillslope (all the same as streamflow)
HillTNMed[,1] = HillSF1
HillTNMed[,2] = HillSF2

#Add dates as column names
colnames(HillTNMed) = c('Replicate', 'HillID', colnames(BasinSF)[-1])

#Save TN timeseries----
#Save R data file
setwd(arg[1])
save.image(file = arg[5], safe = FALSE)

#tables
write.table(round(HillTNMed,3), file = arg[6], row.names = FALSE, sep = '\t')


toc=Sys.time()
print('Write time')
print(toc-tic)