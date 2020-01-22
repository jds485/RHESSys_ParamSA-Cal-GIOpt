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

#Start with streamflow data
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
BasinSF = read.table(file = 'SAResults_BasinStreamflow_p4_Reordered.txt', sep = '\t', header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
HillSF = read.table(file = 'SAResults_HillStreamflow_p6_Reordered.txt', sep = '\t', header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
setwd("C:\\Users\\js4yd\\Documents\\BaismanSA\\RHESSysRuns")
BasinSF_Corr = read.table(file = 'SAResults_BasinStreamflow_p4_CorrOrder.txt', sep = '\t', header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
HillSF_Corr = read.table(file = 'SAResults_HillStreamflow_p6_CorrOrder.txt',  sep = '\t', header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

test = which((BasinSF - BasinSF_Corr) !=0)
#Fixme: Some issue with rounding. Should only need 6 decimal places, but they were different to that decimal place
#It seems it's a problem with the original Hillstreamflow_p6 file - it has 5 decimal places for some values and 6 for others. May have been a scipen issue when the text file was written
test = which((round(HillSF,4) - round(HillSF_Corr,4)) !=0)

for (h in 1:14){
  t1 = round(HillSF[HillSF$HillID == h,],4)
  t2 = round(HillSF_Corr[HillSF_Corr$HillID == h,],4)
  
  test = which((t1[,-c(1,2)] - t2[,-c(1,2)]) !=0)
  
  print(test)
}

#Now also reorder the TN results
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR")
BasinTN05 = read.table(file = 'SAResults_BasinTN05_p3_All.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
BasinTNMed = read.table(file = 'SAResults_BasinTNMed_p3_All.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
BasinTN95 = read.table(file = 'SAResults_BasinTN95_p3_All.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)

#Reorder according to the correct Morris order
BasinTN05 = BasinTN05[OrdInds, ]
BasinTNMed = BasinTNMed[OrdInds, ]
BasinTN95 = BasinTN95[OrdInds, ]
#Get the new replicates named correctly
BasinTN05$Replicate = BasinTNMed$Replicate = BasinTN95$Replicate = seq(1,10880,1)

write.table(BasinTN05, file = 'SAResults_BasinTN05_p3_All_Reordered.txt', row.names = FALSE, sep = '\t')
write.table(BasinTNMed, file = 'SAResults_BasinTNMed_p3_All_Reordered.txt', row.names = FALSE, sep = '\t')
write.table(BasinTN95, file = 'SAResults_BasinTN95_p3_All_Reordered.txt', row.names = FALSE, sep = '\t')

rm(BasinTN05, BasinTN95, BasinTNMed)

HillTN05 = read.table(file = 'SAResults_HillTN05_p3_All.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillTNMed = read.table(file = 'SAResults_HillTNMed_p3_All.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillTN95 = read.table(file = 'SAResults_HillTN95_p3_All.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)

#Reorder according to the correct Morris order and rename the replicates
for (h in 1:14){
  HillTN05[(1+10880*(h-1)):(1+10880*(h-1)+10879),] = HillTN05[(1+10880*(h-1)):(1+10880*(h-1)+10879),][OrdInds,]
  HillTN05[(1+10880*(h-1)):(1+10880*(h-1)+10879),]$Replicate = seq(1,10880,1)
  HillTNMed[(1+10880*(h-1)):(1+10880*(h-1)+10879),] = HillTNMed[(1+10880*(h-1)):(1+10880*(h-1)+10879),][OrdInds,]
  HillTNMed[(1+10880*(h-1)):(1+10880*(h-1)+10879),]$Replicate = seq(1,10880,1)
  HillTN95[(1+10880*(h-1)):(1+10880*(h-1)+10879),] = HillTN95[(1+10880*(h-1)):(1+10880*(h-1)+10879),][OrdInds,]
  HillTN95[(1+10880*(h-1)):(1+10880*(h-1)+10879),]$Replicate = seq(1,10880,1)
}

write.table(HillTN05, file = 'SAResults_HillTN05_p3_All_Reordered.txt', row.names = FALSE, sep = '\t')
write.table(HillTNMed, file = 'SAResults_HillTNMed_p3_All_Reordered.txt', row.names = FALSE, sep = '\t')
write.table(HillTN95, file = 'SAResults_HillTN95_p3_All_Reordered.txt', row.names = FALSE, sep = '\t')


#Reorder likelihood results
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR")
Likes = read.csv(file = 'SA_Params_logL_Baisman_Flow.csv', stringsAsFactors = FALSE, check.names = FALSE)
LikesTN = read.csv(file = 'SA_Params_logL_Baisman_TN.csv', stringsAsFactors = FALSE, check.names = FALSE)

#Reorder according to the correct Morris order
Likes = Likes[OrdInds, ]
LikesTN = LikesTN[OrdInds, ]

#Get the new replicates named correctly
Likes$Replicate = seq(1,10880,1)
LikesTN$Replicate = seq(1,10880,1)

write.csv(Likes, file = 'SA_Params_logL_Baisman_Flow_Reordered.csv', row.names = FALSE)
write.csv(LikesTN, file = 'SA_Params_logL_Baisman_TN_Reordered.csv', row.names = FALSE)
