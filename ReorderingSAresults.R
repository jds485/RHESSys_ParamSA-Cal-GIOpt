#Script to reorder the data output from streamflow that used i instead of strsplit(fs[i])

setwd("C:\\Users\\js4yd\\Documents\\BaismanSA\\RHESSysRuns")
fs = list.files()
#Remove the output folder from this list
fs = fs[-grep(fs,pattern = 'output')]
#Remove the files with extensions of any kind
fs = fs[-grep(fs,pattern = '.', fixed = TRUE)]

#get a vector of only the numbers from this file list
charnums = strsplit(x = fs, split = 'Run', fixed = TRUE)
nums = as.numeric(unlist(charnums)[!is.na(as.numeric(unlist(charnums)))])

#Get the order that the rows should be placed in
OrdInds = order(nums)

rm(fs, charnums, nums)

#Test with basin streamflow data
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR")
BasinSF = read.table(file = 'SAResults_BasinStreamflow_p4.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillSF = read.table(file = 'SAResults_HillStreamflow_p6.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)

#Reorder according to the correct Morris order
BasinSF = BasinSF[OrdInds, ]
#Get the new replicates named correctly
BasinSF$Replicate = seq(1,10880,1)

#Reorder according to the correct Morris order and rename the replicates
for (h in 1:14){
  HillSF[(1+10880*(h-1)):(1+10880*(h-1)+10879),] = HillSF[(1+10880*(h-1)):(1+10880*(h-1)+10879),][OrdInds,]
  HillSF[(1+10880*(h-1)):(1+10880*(h-1)+10879),]$Replicate = seq(1,10880,1)
}

write.table(BasinSF, file = 'SAResults_BasinStreamflow_p4_Reordered.txt', row.names = FALSE, sep = '\t')
write.table(HillSF, file = 'SAResults_HillStreamflow_p6_Reordered.txt', row.names = FALSE, sep = '\t')

#Make sure that this reordered data matches the re-run data with correct indexing
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR")
BasinSF = read.table(file = 'SAResults_BasinStreamflow_p4_Reordered.txt', row.names = FALSE, sep = '\t', header = TRUE, stringsAsFactors = FALSE)
HillSF = read.table(file = 'SAResults_HillStreamflow_p6_Reordered.txt', row.names = FALSE, sep = '\t', header = TRUE, stringsAsFactors = FALSE)
BasinSF_Corr = read.table(file = 'SAResults_BasinStreamflow_p4_CorrOrder.txt', row.names = FALSE, sep = '\t', header = TRUE, stringsAsFactors = FALSE)
HillSF_Corr = read.table(file = 'SAResults_HillStreamflow_p6_CorrOrder.txt', row.names = FALSE, sep = '\t', header = TRUE, stringsAsFactors = FALSE)

test = which((BasinSF - BasinSF_Corr) !=0)


#Now also reorder the TN results
BasinTN05 = read.table(file = 'SAResults_BasinTN05_p3_All.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
BasinTNMed = read.table(file = 'SAResults_BasinTNMed_p3_All.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
BasinTN95 = read.table(file = 'SAResults_BasinTN95_p3_All.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillTN05 = read.table(file = 'SAResults_HillTN05_p3_All.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillTNMed = read.table(file = 'SAResults_HillTNMed_p3_All.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillTN95 = read.table(file = 'SAResults_HillTN95_p3_All.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)

#Reorder according to the correct Morris order
BasinTN05 = BasinTN05[OrdInds, ]
BasinTNMed = BasinTNMed[OrdInds, ]
BasinTN95 = BasinTN95[OrdInds, ]
#Get the new replicates named correctly
BasinTN05$Replicate = BasinTNMed$Replicate = BasinTN95$Replicate = seq(1,10880,1)

#Reorder according to the correct Morris order and rename the replicates
for (h in 1:14){
  HillTN05[(1+10880*(h-1)):(1+10880*(h-1)+10879),] = HillTN05[(1+10880*(h-1)):(1+10880*(h-1)+10879),][OrdInds,]
  HillTN05[(1+10880*(h-1)):(1+10880*(h-1)+10879),]$Replicate = seq(1,10880,1)
  HillTNMed[(1+10880*(h-1)):(1+10880*(h-1)+10879),] = HillTNMed[(1+10880*(h-1)):(1+10880*(h-1)+10879),][OrdInds,]
  HillTNMed[(1+10880*(h-1)):(1+10880*(h-1)+10879),]$Replicate = seq(1,10880,1)
  HillTN95[(1+10880*(h-1)):(1+10880*(h-1)+10879),] = HillTN95[(1+10880*(h-1)):(1+10880*(h-1)+10879),][OrdInds,]
  HillTN95[(1+10880*(h-1)):(1+10880*(h-1)+10879),]$Replicate = seq(1,10880,1)
}

write.table(BasinTN05, file = 'SAResults_BasinTN05_p3_All_Reordered.txt', row.names = FALSE, sep = '\t')
write.table(BasinTNMed, file = 'SAResults_BasinTNMed_p3_All_Reordered.txt', row.names = FALSE, sep = '\t')
write.table(BasinTN95, file = 'SAResults_BasinTN95_p3_All_Reordered.txt', row.names = FALSE, sep = '\t')
write.table(HillTN05, file = 'SAResults_HillTN05_p3_All_Reordered.txt', row.names = FALSE, sep = '\t')
write.table(HillTNMed, file = 'SAResults_HillTNMed_p3_All_Reordered.txt', row.names = FALSE, sep = '\t')
write.table(HillTN95, file = 'SAResults_HillTN95_p3_All_Reordered.txt', row.names = FALSE, sep = '\t')