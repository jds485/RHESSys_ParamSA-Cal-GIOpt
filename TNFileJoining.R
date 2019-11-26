#Combining the TN information into dataframes and writing txt file of the output

#Template for the column names
#Load in the transposed streamflow data with rows as the dates
setwd("/scratch/js4yd/MorrisSA/TNprocessing/")
BasinSF = read.table(file = 'DateColumnNames.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE, nrows = 1)
HillSF = read.table(file = 'SAResults_HillStreamflow_p6_t.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE, nrows = 2, skip=1, row.names = 1)

#Filenames - separate list for basin and hillslope files
setwd('/scratch/js4yd/MorrisSA/TNprocessing/TNdata/')
fs_b = grep(list.files(), pattern = 'a_', value = TRUE)
fs_h = grep(list.files(), pattern = 'h_', value = TRUE)

#Use temporary files to extract dataframes for storing basin and hillslope information
tempb = read.table(paste0(getwd(), '/', fs_b[1]), stringsAsFactors = FALSE, sep = '\t', nrows = 1)
temph = read.table(paste0(getwd(), '/', fs_h[1]), stringsAsFactors = FALSE, sep = '\t', nrows = 1)
BasinTN05 = BasinTN95 = BasinTNMed = matrix(NA, nrow = ncol(tempb), ncol = (1 + length(fs_b)))
HillTN05 = HillTN95 = HillTNMed = matrix(NA, nrow = ncol(temph), ncol = (2 + length(fs_b)))
rm(temph, tempb)

#Loop over files and extract----
for (i in 1:ncol(BasinSF)){
  #Get the basin file by number
  BasinTN05[,i+1] = scan(file = paste0(getwd(), '/', fs_b[i]), sep = '\t', what = 'numeric', blank.lines.skip = TRUE, quiet = TRUE, nlines = 1)
  BasinTNMed[,i+1] = scan(file = paste0(getwd(), '/', fs_b[i]), sep = '\t', what = 'numeric', blank.lines.skip = TRUE, quiet = TRUE, nlines = 1, skip = 1)
  BasinTN95[,i+1] = scan(file = paste0(getwd(), '/', fs_b[i]), sep = '\t', what = 'numeric', blank.lines.skip = TRUE, quiet = TRUE, nlines = 1, skip = 2)
  
  #Get the hillslope file by number
  HillTN05[,i+2] = scan(file = paste0(getwd(), '/', fs_h[i]), sep = '\t', what = 'numeric', blank.lines.skip = TRUE, quiet = TRUE, nlines = 1)
  HillTNMed[,i+2] = scan(file = paste0(getwd(), '/', fs_h[i]), sep = '\t', what = 'numeric', blank.lines.skip = TRUE, quiet = TRUE, nlines = 1, skip = 1)
  HillTN95[,i+2] = scan(file = paste0(getwd(), '/', fs_h[i]), sep = '\t', what = 'numeric', blank.lines.skip = TRUE, quiet = TRUE, nlines = 1, skip = 2)
}

#Add the 1st (and 2nd) columns for basin (and hillslope) (all the same as streamflow)
BasinTNMed[,1] = BasinTN05[,1] = BasinTN95[,1] = seq(1,length(fs_b),1)
HillTNMed[,1] = HillTN05[,1] = HillTN95[,1] = HillSF[1,]
HillTNMed[,2] = HillTN05[,2] = HillTN95[,2] = HillSF[2,]

#Add dates as column names
colnames(BasinTN05) = colnames(BasinTN95) = colnames(BasinTNMed) = colnames(HillTN05) = colnames(HillTN95) = colnames(HillTNMed) = colnames(BasinSF)

#Save TN timeseries----
#Save R data file
save.image("C:\\Users\\js4yd\\Documents\\BaismanSA\\RHESSysRuns\\TNSAreps_BasinHill.RData")

#tables
write.table(round(BasinTN05,3), file = 'SAResults_BasinTN05_p3.txt', row.names = FALSE, sep = '\t')
write.table(round(BasinTNMed,3), file = 'SAResults_BasinTNMed_p3.txt', row.names = FALSE, sep = '\t')
write.table(round(BasinTN95,3), file = 'SAResults_BasinTN95_p3.txt', row.names = FALSE, sep = '\t')

write.table(round(HillTN05,3), file = 'SAResults_HillTN05_p3.txt', row.names = FALSE, sep = '\t')
write.table(round(HillTNMed,3), file = 'SAResults_HillTNMed_p3.txt', row.names = FALSE, sep = '\t')
write.table(round(HillTN95,3), file = 'SAResults_HillTN95_p3.txt', row.names = FALSE, sep = '\t')

#Wow, scan is a lot faster than read.table for this
# tic = Sys.time()
# h = scan(file = paste0(getwd(), '/h_1.txt'), sep = '\t', what = 'numeric', blank.lines.skip = TRUE, quiet = TRUE, nlines = 1)
# toc = Sys.time()
# print(toc-tic)