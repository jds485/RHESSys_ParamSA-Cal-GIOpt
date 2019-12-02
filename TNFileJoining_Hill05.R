#Combining the TN information into dataframes and writing txt file of the output
tic=Sys.time()
#Template for the column names
#Load in the transposed streamflow data with rows as the dates
setwd("/scratch/js4yd/MorrisSA/TNprocessing/")
BasinSF = read.table(file = 'DateColumnNames.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE, nrows = 1)

HillSF1 = as.numeric(scan(file = 'SAResults_HillStreamflow_p6_t.txt', sep = '\t', what = 'numeric', blank.lines.skip = TRUE, quiet = TRUE, nlines = 1, skip=1, row.names)[-1])
HillSF2 = as.numeric(scan(file = 'SAResults_HillStreamflow_p6_t.txt', sep = '\t', what = 'numeric', blank.lines.skip = TRUE, quiet = TRUE, nlines = 1, skip=2, row.names)[-1])


#Filenames - separate list for basin and hillslope files
setwd('/scratch/js4yd/MorrisSA/TNprocessing/TNdata/')
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
  HillTNMed[,ColInd_h+2] = as.numeric(scan(file = paste0(getwd(), '/', fs_h[i]), sep = '\t', what = 'numeric', blank.lines.skip = TRUE, quiet = TRUE, nlines = 1))
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
setwd('/scratch/js4yd/MorrisSA/TNprocessing/')
save.image(file = "TNSAreps_Hill05_All.RData", safe = FALSE)

#tables
write.table(round(HillTNMed,3), file = 'SAResults_HillTN05_p3_All.txt', row.names = FALSE, sep = '\t')


toc=Sys.time()
print('Write time')
print(toc-tic)