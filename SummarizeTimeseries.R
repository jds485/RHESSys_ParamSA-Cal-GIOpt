#Script for processing results from RHESSys using WRTDS, plotting, and saving as simpler datasets

setwd("C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SAOutputCompare")

library(stringi)
library(stringr)
library(rgdal)
library(GISTools)
library(EGRET)
library(EGRETci)
library(sensitivity)

#Search for SA runs that should be re-run because of errors----
setwd(paste0(getwd(), '/output_Orig'))
#Output files
ofs = list.files()
#Create a matrix to store the time cost in seconds
time = matrix(NA, nrow = length(ofs), ncol = 3)
#Create a matrix to store the errors
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
    #Have to add 9999 to the number
    Pyind = as.numeric(strsplit(strsplit(ofs[i], split = 'Run_P9999_')[[1]][2], '.out')[[1]]) + 9999
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
rm(i, ind, Pyind, s, l)
#Find missing runs by index (not Python index - subtract 1 for the Python index):
Missing = seq(1,10880,1)[-which(seq(1,10880,1) %in% errs$ind)]
if (length(Missing) > 0){
  print(paste('Missing run for index', Missing))
}
#Index 10000 was rerun successfully

#Python Tracebacks - These happened due to node failure.
Trace = errs$ind[which(errs$traceback == 1)]
#3252, 3280, 9089, and 9092 are fine. All output is reported, and all defs inputs are correct.

#Collect the errors indices
Errors = errs$ind[which((errs$error == 1) & (is.na(errs$traceback)))]

#Array of runs that had K_absorption+reflectance+transmittance summation tracebacks. These were run before that bug was fixed, and should be rerun.
s = paste0(as.character(errs[order(errs$ind),][which(errs[order(errs$ind),]$error == 1),]$ind[1:32])[1], ',')
for (st in 1:(length(paste0(as.character(errs[order(errs$ind),][which(errs[order(errs$ind),]$error == 1),]$ind[1:32]), sep=','))-1)){
  if (st == (length(paste0(as.character(errs[order(errs$ind),][which(errs[order(errs$ind),]$error == 1),]$ind[1:32])))-1)){
    s = str_c(s, paste0(as.character(errs[order(errs$ind),][which(errs[order(errs$ind),]$error == 1),]$ind[1:32])[st+1], sep=''))
  }else{
    s = str_c(s, paste0(as.character(errs[order(errs$ind),][which(errs[order(errs$ind),]$error == 1),]$ind[1:32])[st+1], sep=',')) 
  }
}
rm(st)
#All of these were rerun successfully

#Remaining error files
Errors = sort(Errors)[-seq(1,32,1)]

#Error in line 1 or 5 - rerun these completely, and delete the folder if it exists. These did not run at all.
L1 = errs$ind[which(errs$ind %in% Errors)][which(errs$Eline[which(errs$ind %in% Errors)] == 1)]
L5 = errs$ind[which(errs$ind %in% Errors)][which(errs$Eline[which(errs$ind %in% Errors)] == 5)]
L15 = sort(c(L1, L5))
#Remove the values less than 10000. Those were run separately successfully.
L15_l10000 = L15[L15 >= 10000]
L15 = L15[L15 < 10000]

#Array to rerun
s15 = paste0(as.character(L15[1]), sep=',')
for (st in 1:(length(L15)-1)){
  if (st == (length(L15)-1)){
    s15 = str_c(s15, paste0(as.character(L15[st+1]), sep=''))
  }else{
    s15 = str_c(s15, paste0(as.character(L15[st+1]), sep=',')) 
  }
}
rm(st)

#Remaining error files:
Errors = Errors[-which(Errors %in% L15)]
Errors = Errors[-which(Errors %in% L15_l10000)]

#Error in line 101 or 109 or 152 or 154 - rerun completely
L101 = errs$ind[which(errs$ind %in% Errors)][which(errs$Eline[which(errs$ind %in% Errors)] == 101)]
L109 = errs$ind[which(errs$ind %in% Errors)][which(errs$Eline[which(errs$ind %in% Errors)] == 109)]
L152 = errs$ind[which(errs$ind %in% Errors)][which(errs$Eline[which(errs$ind %in% Errors)] == 152)]
L154 = errs$ind[which(errs$ind %in% Errors)][which(errs$Eline[which(errs$ind %in% Errors)] == 154)]
L101p = sort(c(L101,L109,L152,L154))

#Array to rerun
s101p = paste0(as.character(L101p[1]), sep=',')
for (st in 1:(length(L101p)-1)){
  if (st == (length(L101p)-1)){
    s101p = str_c(s101p, paste0(as.character(L101p[st+1]), sep=''))
  }else{
    s101p = str_c(s101p, paste0(as.character(L101p[st+1]), sep=',')) 
  }
}
rm(st)

#Remaining error files:
Errors = Errors[-which(Errors %in% L101p)]

#Just RHESSys rerun
L156 = errs$ind[which(errs$ind %in% Errors)][which(errs$Eline[which(errs$ind %in% Errors)] == 156)]
#Remove indices greater than 10000. They were successfully run separately.
L156_10000 = L156[L156 >= 10000]
L156 = L156[L156 < 10000]

#Array of reruns
s156 = paste0(as.character(L156[1]), sep=',')
for (st in 1:(length(L156)-1)){
  if (st == (length(L156)-1)){
    s156 = str_c(s156, paste0(as.character(L156[st+1]), sep=''))
  }else{
    s156 = str_c(s156, paste0(as.character(L156[st+1]), sep=',')) 
  }
}
rm(st)

#Remaining error files:
Errors = Errors[-which(Errors %in% L156)]
Errors = Errors[-which(Errors %in% L156_10000)]

#May not need to rerun RHESSys - checked individually. All of these files were created before the decision to not move to storage in the run.
#These all stopped while moving files.
L196 = errs$ind[which(errs$ind %in% Errors)][which(errs$Eline[which(errs$ind %in% Errors)] == 196)]
#All files moved. None needed to be rerun.

#Check for missing runs and errors in final set of output files----
#Find missing runs by index (not Python index - subtract 1 for the Python index):
setwd("C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SAOutputCompare")
setwd(paste0(getwd(), '/output'))
#Output files
ofs = list.files()
#Create a matrix to store the time cost in seconds
time = matrix(NA, nrow = length(ofs), ncol = 3)
#Create a matrix to store the errors
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
    #Have to add 9999 to the number
    Pyind = as.numeric(strsplit(strsplit(ofs[i], split = 'Run_P9999_')[[1]][2], '.out')[[1]]) + 9999
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
rm(i, ind, Pyind, s, l)
#Find missing runs by index (not Python index - subtract 1 for the Python index):
Missing = seq(1,10880,1)[-which(seq(1,10880,1) %in% errs$ind)]
if (length(Missing) > 0){
  print(paste('Missing run for index', Missing))
}

#Python Tracebacks - These happened due to node failure.
Trace = errs$ind[which(errs$traceback == 1)]
#3252, 3280, 9089, and 9092 are fine. All output is reported, and all defs inputs are correct.
if (length(Trace) != 4){
  print(paste('Tracebacks greater than before rerunning. There are new tracebacks', Trace))
}

#Collect the errors indices
Errors = errs$ind[which((errs$error == 1) & (is.na(errs$traceback)))]
if (length(Errors) != length(L196)){
  print(paste('There are new errors', Errors[-which(Errors %in% L196)]))
}

#Time in hours for completed results----
setwd("C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SAOutputCompare")
time[,3] = time[,3]/3600
#Sort by index, then plot by index to see if there are trends
time = time[order(time[,1]),]
png('computationTime.png', res = 300, units = 'in', width = 5, height = 5)
plot(time[,1], time[,3], pch = 16, cex = 0.2, ylim = c(0,1), xlab = 'Morris Index', ylab = 'Time (hrs)')
dev.off()

#Time per year
png('computationTimePerYear.png', res = 300, units = 'in', width = 5, height = 5)
plot(time[,1], time[,3]/11, pch = 16, cex = 0.2, ylim = c(0,.1), xlab = 'Morris Index', ylab = 'Time (hrs)', main = 'Time per Year of Simulation')
dev.off()

#Histogram of the time
png('computationTimeHist.png', res = 300, units = 'in', width = 5, height = 5)
hist(time[,3], xlim = c(0,1), breaks=50, xlab = 'Time (hrs)')
dev.off()

#Forecasted time per year of Calibration period
png('computationTimeHist_CalibrationEstimate.png', res = 300, units = 'in', width = 5, height = 5)
hist(time[,3]/11*14, xlim = c(0,1.5), breaks=50, xlab = 'Time (hrs)')
dev.off()

#Fixme: try to diagnose runtime length by parameters - there may not be a correlation here.

#Go through results folders----
#Folder list. These all have results and input files in them.
fs = list.files()
#Remove the output folder from this list
fs = fs[-grep(fs,pattern = 'output')]
#Remove the png files
fs = fs[-grep(fs,pattern = '.png')]

#Read the worldfile. This is used to extract the area of basin and hillslopes----
#Patch resolution, m
res = 30

#Fixme: Baisman cross sectional area, m^2 - is this needed?

world = read.csv(paste0(getwd(), '/', fs[1], '/worldfiles/worldfile.csv'), stringsAsFactors = FALSE)

#Taking the unique patch IDs because strata can exist in more than one patch.
Area.basin = length(unique(world$patchID))*res^2
#Multiplier conversion for basin streamflow
conversion_b = Area.basin/1000*(100^3)/(2.54^3)/(12^3)/24/3600

#Get hillslope areas and conversion factor for streamflow in hillslopes
uhills = unique(world$hillID)
Area.Hills = matrix(NA, nrow = length(uhills), ncol = 2)
conversion_h = matrix(NA, nrow = length(uhills), ncol = 2)
for (h in 1:length(uhills)){
  Area.Hills[h,1] = h
  conversion_h[h,1] = h
  #some patches have multiple strata, so their area cannot be counted from the count of cells.
  Area.Hills[h,2] = length(which(world[which(duplicated(world$patchID) == FALSE),]$hillID == h))*res^2
  conversion_h[h,2] = Area.Hills[h,2]/1000*(100^3)/(2.54^3)/(12^3)/24/3600
}
rm(h)

#Make the worldfile a spatial dataframe to get a map. Plot information in the worldfile on the maps----
coordinates(world) = c('patchX', 'patchY')
proj4string(world) = CRS('+init=epsg:26918')
#Change to degrees
world=spTransform(world, CRSobj = CRS('+init=epsg:4326'))

cols = rainbow(n = length(uhills))

#Fixme: add stream to this map (white?)
png('hillslopeMap.png', res = 300, height = 6, width = 6, units ='in')
par(mar= c(2.5,2.5,1,1))
plot(world, col = 'white')
for (h in 1:length(uhills)){
  plot(world[world$hillID == uhills[h],], pch = 22, add = TRUE, lwd=10)
  plot(world[world$hillID == uhills[h],], col = cols[h], pch = 15, add = TRUE)
}
legend('bottomright', title = expression(bold('Hillslope')), legend=seq(1,length(uhills),1), fill = cols, border = 'black', ncol = 2)
degAxis(side = 1, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 1, at = seq(-76.7,-76,.02))
degAxis(side = 3, at = seq(-77,-76,.01), labels = FALSE)
degAxis(side = 2, at = seq(39.45, 40,.01))
degAxis(side = 4, at = seq(39.45, 40,.01), labels = FALSE)
north.arrow(xb = -76.712, yb = 39.469, len = .0005, lab = 'N', tcol = 'black', col='black')
text(x = -76.712, y = 39.467, 'WGS84')
box(which = 'figure', lwd = 2)
dev.off()
rm(h)

#Check that the input def file parameter values match the output parameter values----
for (i in 1:length(fs)){
  od = getwd()
  setwd(paste0(od, '/', fs[i]))
  #Check that the def file and the output information from RHESSys parameter files match.
  fs_def = list.files(paste0(getwd(), '/defs'))
  fs_Rdef = list.files(paste0(getwd(), '/output'))
  
  #Loop over def files and compare the values of parameters in them.
  for (d in 1:length(fs_def)){
    #Find the RHESSys output file corresponding to the input def file. Skip basin because that's not reported by RHESSys
    if (strsplit(fs_def[d], split = '.def', fixed = TRUE)[[1]] != 'basin_basin'){
      f = read.table(paste0(getwd(), '/output/', fs_Rdef[grep(fs_Rdef, pattern = paste0(strsplit(fs_def[d], split = '.def', fixed = TRUE)[[1]], '_', strsplit(grep(fs_Rdef, pattern = strsplit(fs_def[d], split = '.def', fixed = TRUE)[[1]], value = TRUE), split = '_')[[1]][2]))]), header = FALSE, stringsAsFactors = FALSE, colClasses = 'character')
      
      #The variable epc.min_percent_leafg in the grass def file for some reason did not get entered, 
      #but the value for it is correct in the output def file because it is assigned the value for epc.leaf_turnover. 
      #Fixing the input file here
      if (fs_def[d] == 'stratum_grass.def'){
        fo = read.table(paste0(getwd(), '/defs/', fs_def[d]), header = FALSE, stringsAsFactors = FALSE, colClasses = 'character', fill = TRUE)
        #Edit the row with epc.min_percent_leafg
        fo[fo$V1 == 'epc.min_percent_leafg', 2] = 'epc.min_percent_leafg'
        fo[fo$V1 == 'epc.min_percent_leafg', 1] = as.character(signif(as.numeric(fo[fo$V2 == 'epc.leaf_turnover', 1]), 6))
      }else{
        fo = read.table(paste0(getwd(), '/defs/', fs_def[d]), header = FALSE, stringsAsFactors = FALSE, colClasses = 'character')
      }
      
      #Compare the values for variables
      for (v in nrow(f)){
        if (fo[fo$V2 == f$V2[v],1] != f[v,1]){
          print(paste('Values not equal for def file ', i, d, fs_def[d], ' Variable ', f[v,2], ' run ', fs[i]))
        }
      }
    }
  }
  setwd(od)
}
rm(f, fo, d, fs_def, fs_Rdef, i, v)

print(paste("Input def files match output def files."))

#Loop through all of the folders and extract the data needed----
temp_out = list.files(paste0(getwd(), '/', fs[1], '/output'))
tempb = read.table(paste0(getwd(), '/', fs[1], '/output/', temp_out[grep(temp_out, pattern = 'basin.daily')]), stringsAsFactors = FALSE, header = TRUE)
BasinStreamflow = BasinSatDef = matrix(NA, nrow = length(fs), ncol = (1 + nrow(tempb)))
HillStreamflow = HillSatDef = matrix(NA, nrow = length(fs)*length(uhills), ncol = (2 + nrow(tempb)))
rm(temp_out, tempb)

MakeFigs = FALSE
for (i in 1:length(fs)){
  #Make figures of the basin output----
  od = getwd()
  #Fixme: ? Could read inn worldfile and plot all of the worldfile info for each run, or save info and compare for each run.
  
  setwd(paste0(od, '/', fs[i], '/output'))
  
  #Obtain basin and hillslope data
  fs_out = list.files()
  bs = read.table(paste0(getwd(), '/', fs_out[grep(fs_out, pattern = 'basin.daily')]), stringsAsFactors = FALSE, header = TRUE)
  hs = read.table(paste0(getwd(), '/', fs_out[grep(fs_out, pattern = 'hillslope.daily')]), stringsAsFactors = FALSE, header = TRUE)
  
  #Make a new date column
  bs$Date = as.Date(paste0(bs$year, '-', bs$month, '-', bs$day))
  hs$Date = as.Date(paste0(hs$year, '-', hs$month, '-', hs$day))
  
  #Make figures, if requested
  if (MakeFigs == TRUE){
    #Make folders to store the figures
    dir.create(paste0(od, '/', fs[i], '/figures'))
    dir.create(paste0(od, '/', fs[i], '/figures/basin'))
    dir.create(paste0(od, '/', fs[i], '/figures/hillslope'))
    
    #Basin plots----
    setwd(paste0(od, '/', fs[i], '/figures/basin'))
    #Streamflow
    png(paste0(fs[i], '_streamflowBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$streamflow*conversion_b, main = 'Basin Streamflow', type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)')
    dev.off()
    
    #Baseflow
    png(paste0(fs[i], '_baseflowBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$baseflow*conversion_b, main = 'Basin Baseflow', type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)')
    dev.off()
    
    # Return flow
    png(paste0(fs[i], '_returnFlowBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$return*conversion_b, main = 'Basin Return Flow', type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)')
    dev.off()
    
    # Rain
    png(paste0(fs[i], '_rainBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$rain_thr, main = 'Basin Rain', type = 'l', xlab = 'Year', ylab = 'Rain')
    dev.off()
    
    # Snow Throughfall
    png(paste0(fs[i], '_snowThroughfallBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$snow_thr, main = 'Basin Snow Throughfall', type = 'l', xlab = 'Year', ylab = 'Snow')
    dev.off()
    
    # Sat Def z
    png(paste0(fs[i], '_satDefZBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$sat_def_z, main = 'Basin Saturation Deficit with Depth', type = 'l', xlab = 'Year', ylab = 'Saturation Deficit')
    dev.off()
    
    # Sat Def
    png(paste0(fs[i], '_satDefBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$sat_def, main = 'Basin Saturation Deficit', type = 'l', xlab = 'Year', ylab = 'Saturation Deficit')
    dev.off()
    
    # Root Zone Storage
    png(paste0(fs[i], '_rootStorageBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$rz_storage, main = 'Basin Root Zone Storage', type = 'l', xlab = 'Year', ylab = 'Storage')
    dev.off()
    
    # Root Zone Drainage
    png(paste0(fs[i], '_rootDrainageBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$rz_drainage, main = 'Basin Root Zone Drainage', type = 'l', xlab = 'Year', ylab = 'Drainage')
    dev.off()
    
    # Unsaturated Zone Storage
    png(paste0(fs[i], '_unsaturatedStorageBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$unsat_stor, main = 'Basin Unsaturated Storage', type = 'l', xlab = 'Year', ylab = 'Storage')
    dev.off()
    
    # Unsaturated Zone Drainage
    png(paste0(fs[i], '_unsaturatedDrainageBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$unsat_drain, main = 'Basin Unsaturated Drainage', type = 'l', xlab = 'Year', ylab = 'Drainage')
    dev.off()
    
    # Capillary Rise
    png(paste0(fs[i], '_capRiseBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$cap, main = 'Basin Capillary Rise', type = 'l', xlab = 'Year', ylab = 'Rise')
    dev.off()
    
    # Evaporation
    png(paste0(fs[i], '_evapBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$evap, main = 'Basin Evaporation', type = 'l', xlab = 'Year', ylab = 'Evaporation')
    dev.off()
    
    # Transpiration
    png(paste0(fs[i], '_transpirationBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$trans, main = 'Basin Transpiration', type = 'l', xlab = 'Year', ylab = 'Transpiration')
    dev.off()
    
    # Transpiration Variance
    png(paste0(fs[i], '_transVariabilityBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$trans_var, main = 'Basin Transpiration Spatial Variance', type = 'l', xlab = 'Year', ylab = 'Transpiration Variance')
    dev.off()
    
    # Net Photosynthesis
    png(paste0(fs[i], '_netPhotosynthesisBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$psn, main = 'Basin Net Photosynthesis', type = 'l', xlab = 'Year', ylab = 'Net Photosynthesis')
    dev.off()
    
    # Tree LAI
    png(paste0(fs[i], '_treeLAIBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$laiTREE, main = 'Basin Tree LAI', type = 'l', xlab = 'Year', ylab = 'LAI')
    dev.off()
    
    # GW Output
    png(paste0(fs[i], '_groundwaterOutBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$gw.Qout, main = 'Basin Groundwater Output', type = 'l', xlab = 'Year', ylab = 'Groundwater Flow')
    dev.off()
    
    # GW Storage
    png(paste0(fs[i], '_groundwaterStorageBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$gw.storage, main = 'Basin Groundwater Storage', type = 'l', xlab = 'Year', ylab = 'Groundwater Storage')
    dev.off()
    
    # Detention Storage
    png(paste0(fs[i], '_detentionStorageBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$detention_store, main = 'Basin Detention Storage', type = 'l', xlab = 'Year', ylab = 'Storage')
    dev.off()
    
    # Percent Saturated Area
    png(paste0(fs[i], '_pctSatAreaBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$X.sat_area, main = 'Basin Percent Saturated Area', type = 'l', xlab = 'Year', ylab = 'Percent Saturated')
    dev.off()
    
    # Litter Storage
    png(paste0(fs[i], '_litterStorageBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$litter_store, main = 'Basin Litter Storage', type = 'l', xlab = 'Year', ylab = 'Storage')
    dev.off()
    
    # Canopy Storage
    png(paste0(fs[i], '_canopyStorageBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$canopy_store, main = 'Basin Canopy Storage', type = 'l', xlab = 'Year', ylab = 'Storage')
    dev.off()
    
    # Percent Snow Cover
    png(paste0(fs[i], '_pctSnowCoverBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$X.snow_cover, main = 'Basin Percent Snow Cover', type = 'l', xlab = 'Year', ylab = 'Percent Cover')
    dev.off()
    
    # Snowpack
    png(paste0(fs[i], '_snowpackBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$snowpack, main = 'Basin Snowpack', type = 'l', xlab = 'Year', ylab = 'Snowpack')
    dev.off()
    
    # Snow Sublimation
    png(paste0(fs[i], '_snowSublimationBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$snow_subl, main = 'Basin Snow Sublimation', type = 'l', xlab = 'Year', ylab = 'Sublimation')
    dev.off()
    
    # Accumulated Transpiration
    png(paste0(fs[i], '_AccumulatedTransBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$acc_trans, main = 'Basin Accumulated Transpiration', type = 'l', xlab = 'Year', ylab = 'Transpiration')
    dev.off()
    
    # Accumulated Transpiration Spatial Variance
    png(paste0(fs[i], '_AccumulatedTransVarBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$acctransv_var, main = 'Basin Accumulated Transpiration: Spatial variance', type = 'l', xlab = 'Year', ylab = 'Variance of Transpiration')
    dev.off()
    
    # PET
    png(paste0(fs[i], '_PotentialETBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$pet, main = 'Basin Potential Evapotranspiration', type = 'l', xlab = 'Year', ylab = 'PET')
    dev.off()
    
    # C13 discrimination
    png(paste0(fs[i], '_C13DiscriminationBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$dC13, main = 'Basin C13 Discrimination', type = 'l', xlab = 'Year', ylab = 'Discrimination')
    dev.off()
    
    # Precipitation
    #Fixme: make sure this matches the input file
    png(paste0(fs[i], '_PrecipitationBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$precip, main = 'Basin Precipitation', type = 'l', xlab = 'Year', ylab = 'Precipitation')
    dev.off()
    
    # Rain+Snow throughfall
    png(paste0(fs[i], '_Rain+SnowBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$pcp_assim, main = 'Basin Rain+Snow', type = 'l', xlab = 'Year', ylab = 'Precipitation')
    dev.off()
    
    # Tree mortality fraction
    png(paste0(fs[i], '_TreeMortalityBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$mortf, main = 'Basin Tree Mortality', type = 'l', xlab = 'Year', ylab = 'Percent')
    dev.off()
    
    # Tmax
    #Fixme: make sure this matches the input file
    png(paste0(fs[i], '_TMaxBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$tmax, main = 'Basin Maximum Temperature', type = 'l', xlab = 'Year', ylab = 'Temperature')
    dev.off()
    
    # Tmin
    #Fixme: make sure this matches the input file
    png(paste0(fs[i], '_TMinBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$tmin, main = 'Basin Minimum Temperature', type = 'l', xlab = 'Year', ylab = 'Temperature')
    dev.off()
    
    # Tavg
    png(paste0(fs[i], '_TAvgBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$tavg, main = 'Basin Average Temperature', type = 'l', xlab = 'Year', ylab = 'Temperature')
    dev.off()
    
    # Vapor Pressure Deficit
    png(paste0(fs[i], '_VPDBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$vpd, main = 'Basin Vapor Pressure Deficit', type = 'l', xlab = 'Year', ylab = 'Vapor Pressure Deficit')
    dev.off()
    
    # Snowfall
    png(paste0(fs[i], '_SnowfallBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$snowfall, main = 'Basin Snowfall', type = 'l', xlab = 'Year', ylab = 'Snowfall')
    dev.off()
    
    # Soil Recharge
    png(paste0(fs[i], '_soilRechargeBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$recharge, main = 'Basin Soil Recharge', type = 'l', xlab = 'Year', ylab = 'Recharge')
    dev.off()
    
    # Gross Photosynthesis
    png(paste0(fs[i], '_grossPhotosynthesisBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$gpsn, main = 'Basin Gross Photosynthesis', type = 'l', xlab = 'Year', ylab = 'Gross Photosynthesis')
    dev.off()
    
    # Respiration
    png(paste0(fs[i], '_respirationBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$resp, main = 'Basin Respiration', type = 'l', xlab = 'Year', ylab = 'Respiration')
    dev.off()
    
    # Canopy Conductance
    png(paste0(fs[i], '_canopyConductanceBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$gs, main = 'Basin Canopy Conductance', type = 'l', xlab = 'Year', ylab = 'Conductance')
    dev.off()
    
    # Root Depth
    png(paste0(fs[i], '_rootDepthBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$rootdepth, main = 'Basin Root Depth', type = 'l', xlab = 'Year', ylab = 'Depth')
    dev.off()
    
    # Snowmelt
    png(paste0(fs[i], '_snowmeltBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$snowmelt, main = 'Basin Snowmelt', type = 'l', xlab = 'Year', ylab = 'Snowmelt')
    dev.off()
    
    # Canopy Sublimation
    png(paste0(fs[i], '_canopySublimationBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$canopysubl, main = 'Basin Canopy Sublimation', type = 'l', xlab = 'Year', ylab = 'Sublimation')
    dev.off()
    
    # Routed Streamflow
    png(paste0(fs[i], '_routedStreamflowBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$routedstreamflow, main = 'Basin Routed Streamflow', type = 'l', xlab = 'Year', ylab = 'Streamflow')
    dev.off()
    
    # Canopy Intercepted Snow
    png(paste0(fs[i], '_canopySnowBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$canopy_snow, main = 'Basin Canopy Intercepted Snow', type = 'l', xlab = 'Year', ylab = 'Snow')
    dev.off()
    
    # Canopy Height
    png(paste0(fs[i], '_canopyHeightBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$height, main = 'Basin Canopy Height', type = 'l', xlab = 'Year', ylab = 'Height')
    dev.off()
    
    # Canopy Evaporation
    png(paste0(fs[i], '_canopyEvapBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$evap_can, main = 'Basin Canopy Evaporation', type = 'l', xlab = 'Year', ylab = 'Evaporation')
    dev.off()
    
    # Litter Evaporation
    png(paste0(fs[i], '_litterEvapBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$evap_lit, main = 'Basin Litter Evaporation', type = 'l', xlab = 'Year', ylab = 'Evaporation')
    dev.off()
    
    # Soil Evaporation
    png(paste0(fs[i], '_soilEvapBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$evap_soil, main = 'Basin Soil Evaporation', type = 'l', xlab = 'Year', ylab = 'Evaporation')
    dev.off()
    
    # Litter Carbon
    png(paste0(fs[i], '_litterCarbonBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$litrc, main = 'Basin Litter Carbon', type = 'l', xlab = 'Year', ylab = 'Carbon')
    dev.off()
    
    # Downward Shortwave Radiation
    png(paste0(fs[i], '_KdownBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$Kdown, main = 'Basin Downward Shortwave Radiation', type = 'l', xlab = 'Year', ylab = 'Downward Shortwave Radiation')
    dev.off()
    
    # Downward Longwave Radiation
    png(paste0(fs[i], '_LdownBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$Ldown, main = 'Basin Downward Longwave Radiation', type = 'l', xlab = 'Year', ylab = 'Downward Longwave Radiation')
    dev.off()
    
    # Upward Shortwave Radiation
    png(paste0(fs[i], '_KupBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$Kup, main = 'Basin Upward Shortwave Radiation', type = 'l', xlab = 'Year', ylab = 'Upward Shortwave Radiation')
    dev.off()
    
    # Upward Longwave Radiation
    png(paste0(fs[i], '_LdownBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$Lup, main = 'Basin Upward Longwave Radiation', type = 'l', xlab = 'Year', ylab = 'Upward Longwave Radiation')
    dev.off()
    
    # Canopy Absorbed Shortwave Radiation
    png(paste0(fs[i], '_canopyAbsorbKBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$Kstar_can, main = 'Basin Canopy Absorbed Shortwave Radiation', type = 'l', xlab = 'Year', ylab = 'Radiation')
    dev.off()
    
    # Soil Absorbed Shortwave Radiation
    png(paste0(fs[i], '_soilAbsorbKBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$Kstar_soil, main = 'Basin Soil Absorbed Shortwave Radiation', type = 'l', xlab = 'Year', ylab = 'Radiation')
    dev.off()
    
    # Snow Absorbed Shortwave Radiation
    png(paste0(fs[i], '_snowAbsorbKBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$Kstar_snow, main = 'Basin Snow Absorbed Shortwave Radiation', type = 'l', xlab = 'Year', ylab = 'Radiation')
    dev.off()
    
    # Canopy Absorbed Longwave Radiation
    png(paste0(fs[i], '_canopyAbsorbLBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$Lstar_can, main = 'Basin Canopy Absorbed Longwave Radiation', type = 'l', xlab = 'Year', ylab = 'Radiation')
    dev.off()
    
    # Soil Absorbed Longwave Radiation
    png(paste0(fs[i], '_soilAbsorbLBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$Lstar_soil, main = 'Basin Soil Absorbed Longwave Radiation', type = 'l', xlab = 'Year', ylab = 'Radiation')
    dev.off()
    
    # Snow Absorbed Longwave Radiation
    png(paste0(fs[i], '_snowAbsorbLBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$Lstar_snow, main = 'Basin Snow Absorbed Longwave Radiation', type = 'l', xlab = 'Year', ylab = 'Radiation')
    dev.off()
    
    # Canopy Latent Heat Evaporated
    png(paste0(fs[i], '_canopyHeatEvapBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$LE_canopy, main = 'Basin Canopy Heat Evaporation', type = 'l', xlab = 'Year', ylab = 'Heat Evaporation')
    dev.off()
    
    # soil Latent Heat Evaporated
    png(paste0(fs[i], '_soilHeatEvapBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$LE_soil, main = 'Basin Soil Heat Evaporation', type = 'l', xlab = 'Year', ylab = 'Heat Evaporation')
    dev.off()
    
    # Snow Latent Heat Evaporated
    png(paste0(fs[i], '_snowHeatEvapBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$LE_snow, main = 'Basin Snow Heat Evaporation', type = 'l', xlab = 'Year', ylab = 'Heat Evaporation')
    dev.off()
    
    # ??
    png(paste0(fs[i], '_LstarBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$Lstar_strat, main = 'Basin ??', type = 'l', xlab = 'Year', ylab = '??')
    dev.off()
    
    # Canopy Drip
    png(paste0(fs[i], '_canopyDripBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$canopydrip, main = 'Basin Canopy Drip', type = 'l', xlab = 'Year', ylab = 'Drip')
    dev.off()
    
    # Aerodynamic Conductance
    png(paste0(fs[i], '_aerodynamicConductanceBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$ga, main = 'Basin Canopy Heat Evaporation', type = 'l', xlab = 'Year', ylab = 'Heat Evaporation')
    dev.off()
    
    # Stormdrain
    png(paste0(fs[i], '_stormdrainBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$stormdrain, main = 'Basin Storm Drainage', type = 'l', xlab = 'Year', ylab = 'Drainage')
    dev.off()
    
    # Stem Carbon
    png(paste0(fs[i], '_stemCarbonBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$stemc, main = 'Basin Stem Carbon', type = 'l', xlab = 'Year', ylab = 'Carbon')
    dev.off()
    
    # Plant Carbon
    png(paste0(fs[i], '_plantCarbonBasin.png'), res = 300, height = 5, width=5, units = 'in')
    plot(bs$Date, bs$plantc, main = 'Basin Plant Carbon', type = 'l', xlab = 'Year', ylab = 'Carbon')
    dev.off()
    
    #Hillslope plots----
    setwd(paste0(od, '/', fs[i], '/figures/hillslope'))
    
    #Streamflow
    png(paste0(fs[i], '_streamflowHillslope.png'), res = 300, height = 10, width=10, units = 'in')
    layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12), c(13,14)))
    par(mar = c(3,3,3,0.5))
    for (h in 1:length(uhills)){
      plot(hs$Date[hs$hillID == uhills[h]], hs$streamflow[hs$hillID == uhills[h]]*conversion_h[conversion_h[,1] == uhills[h], 2], main = paste0('Hillslope ', uhills[h], ' Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)')
    }
    rm(h)
    dev.off()
    
    #Streamflow - same y-axis
    png(paste0(fs[i], '_streamflowScaledHillslope.png'), res = 300, height = 10, width=10, units = 'in')
    layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12), c(13,14)))
    par(mar = c(3,3,3,0.5))
    for (h in 1:length(uhills)){
      plot(hs$Date[hs$hillID == uhills[h]], hs$streamflow[hs$hillID == uhills[h]]*conversion_h[conversion_h[,1] == uhills[h], 2], main = paste0('Hillslope ', uhills[h], ' Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)', ylim = c(0,4))
    }
    rm(h)
    dev.off()
    
    #Baseflow
    png(paste0(fs[i], '_baseflowHillslope.png'), res = 300, height = 10, width=10, units = 'in')
    layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12), c(13,14)))
    par(mar = c(3,3,3,0.5))
    for (h in 1:length(uhills)){
      plot(hs$Date[hs$hillID == uhills[h]], hs$baseflow[hs$hillID == uhills[h]]*conversion_h[conversion_h[,1] == uhills[h], 2], main = paste0('Hillslope ', uhills[h], ' Baseflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)')
    }
    rm(h)
    dev.off()
    
    #Baseflow - same y-axis
    png(paste0(fs[i], '_baseflowScaledHillslope.png'), res = 300, height = 10, width=10, units = 'in')
    layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12), c(13,14)))
    par(mar = c(3,3,3,0.5))
    for (h in 1:length(uhills)){
      plot(hs$Date[hs$hillID == uhills[h]], hs$baseflow[hs$hillID == uhills[h]]*conversion_h[conversion_h[,1] == uhills[h], 2], main = paste0('Hillslope ', uhills[h], ' Baseflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)', ylim = c(0,1))
    }
    rm(h)
    dev.off()
    
    #Return flow
    png(paste0(fs[i], '_returnflowHillslope.png'), res = 300, height = 10, width=10, units = 'in')
    layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12), c(13,14)))
    par(mar = c(3,3,3,0.5))
    for (h in 1:length(uhills)){
      plot(hs$Date[hs$hillID == uhills[h]], hs$return[hs$hillID == uhills[h]]*conversion_h[conversion_h[,1] == uhills[h], 2], main = paste0('Hillslope ', uhills[h], ' Return Flow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)')
    }
    rm(h)
    dev.off()
    
    #Saturation Deficit with Depth
    png(paste0(fs[i], '_saturationDeficitZHillslope.png'), res = 300, height = 10, width=10, units = 'in')
    layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12), c(13,14)))
    par(mar = c(3,3,3,0.5))
    for (h in 1:length(uhills)){
      plot(hs$Date[hs$hillID == uhills[h]], hs$sat_def_z[hs$hillID == uhills[h]], main = paste0('Hillslope ', uhills[h], ' Saturation Deficit with Depth'), type = 'l', xlab = 'Year', ylab = 'Saturation Deficit')
    }
    rm(h)
    dev.off()
    
    #Saturation Deficit
    png(paste0(fs[i], '_saturationDeficitHillslope.png'), res = 300, height = 10, width=10, units = 'in')
    layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12), c(13,14)))
    par(mar = c(3,3,3,0.5))
    for (h in 1:length(uhills)){
      plot(hs$Date[hs$hillID == uhills[h]], hs$sat_def[hs$hillID == uhills[h]], main = paste0('Hillslope ', uhills[h], ' Saturation Deficit'), type = 'l', xlab = 'Year', ylab = 'Saturation Deficit')
    }
    rm(h)
    dev.off()
    
    #Detention Storage
    png(paste0(fs[i], '_detentionStorageHillslope.png'), res = 300, height = 10, width=10, units = 'in')
    layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12), c(13,14)))
    par(mar = c(3,3,3,0.5))
    for (h in 1:length(uhills)){
      plot(hs$Date[hs$hillID == uhills[h]], hs$detention_store[hs$hillID == uhills[h]], main = paste0('Hillslope ', uhills[h], ' Detention Storage'), type = 'l', xlab = 'Year', ylab = 'Saturation Deficit')
    }
    rm(h)
    dev.off()
    
    #Saturation Area
    png(paste0(fs[i], '_saturationAreaHillslope.png'), res = 300, height = 10, width=10, units = 'in')
    layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12), c(13,14)))
    par(mar = c(3,3,3,0.5))
    for (h in 1:length(uhills)){
      plot(hs$Date[hs$hillID == uhills[h]], hs$sat_area[hs$hillID == uhills[h]], main = paste0('Hillslope ', uhills[h], ' Saturation Area'), type = 'l', xlab = 'Year', ylab = 'Saturation Deficit')
    }
    rm(h)
    dev.off()
    
    #Precipitation
    png(paste0(fs[i], '_precipitationHillslope.png'), res = 300, height = 10, width=10, units = 'in')
    layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12), c(13,14)))
    par(mar = c(3,3,3,0.5))
    for (h in 1:length(uhills)){
      plot(hs$Date[hs$hillID == uhills[h]], hs$precip[hs$hillID == uhills[h]], main = paste0('Hillslope ', uhills[h], ' Precipitation'), type = 'l', xlab = 'Year', ylab = 'Saturation Deficit')
    }
    rm(h)
    dev.off()
    
    #ET
    png(paste0(fs[i], '_evapotranspirationHillslope.png'), res = 300, height = 10, width=10, units = 'in')
    layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12), c(13,14)))
    par(mar = c(3,3,3,0.5))
    for (h in 1:length(uhills)){
      plot(hs$Date[hs$hillID == uhills[h]], hs$evap[hs$hillID == uhills[h]] + hs$trans[hs$hillID == uhills[h]], main = paste0('Hillslope ', uhills[h], ' Evapotranspiration'), type = 'l', xlab = 'Year', ylab = 'Saturation Deficit')
    }
    rm(h)
    dev.off()
  }
  
  #Save basin and hillslope timeseries----
  BasinStreamflow[i,] = c(i, bs$streamflow*conversion_b)
  BasinSatDef[i,] = c(i, bs$sat_def)
  for (h in 1:length(uhills)){
    HillStreamflow[i + length(fs)*(uhills[h]-1),] = c(i, uhills[h], hs$streamflow[hs$hillID == uhills[h]]*conversion_h[conversion_h[,1] == uhills[h], 2])
    HillSatDef[i + length(fs)*(uhills[h]-1),] = c(i, uhills[h], hs$sat_def[hs$hillID == uhills[h]])
  }
  rm(h)
  
  setwd(od)
}

#Make basin and hillslope matrices into dataframes----
BasinStreamflow = as.data.frame(BasinStreamflow)
BasinSatDef = as.data.frame(BasinSatDef)
colnames(BasinStreamflow) = colnames(BasinSatDef) = c('Replicate', as.character(bs$Date))
HillStreamflow = as.data.frame(HillStreamflow)
HillSatDef = as.data.frame(HillSatDef)
colnames(HillStreamflow) = colnames(HillSatDef) = c('Replicate', 'HillID', as.character(bs$Date))

rm(i, fs_out, bs, hs)

#Make plots of the streamflow and saturation deficit observed across the SA runs----
#Basin----
png(paste0('streamflowRepsBasin_Med.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(colnames(BasinStreamflow)[-1]), y = t(BasinStreamflow[,-1]), col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = BasinStreamflow[,-1], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(colnames(BasinStreamflow)[-1]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
     xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()

png(paste0('streamflowRepsBasin_Cut1yr_Med.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(colnames(BasinStreamflow)[-seq(1,414,1)]), y = t(BasinStreamflow[,-seq(1,414,1)]), col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = BasinStreamflow[,-seq(1,414,1)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(colnames(BasinStreamflow)[-seq(1,414,1)]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1,
     xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()

png(paste0('streamflowRepsBasin_Cut1yr_Quants.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(colnames(BasinStreamflow)[-seq(1,414,1)]), y = t(BasinStreamflow[,-seq(1,414,1)]), col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = BasinStreamflow[,-seq(1,414,1)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
for (q in 1:nrow(quants)){
  par(new=TRUE)
  plot(x = as.Date(colnames(BasinStreamflow)[-seq(1,414,1)]), y = quants[q,], col = 'red', lty = qlty[q], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1,
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

png(paste0('streamflowRepsBasin_2007_Med.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(colnames(BasinStreamflow)[seq(2606,2606+365,1)]), y = t(BasinStreamflow[,seq(2606,2606+365,1)]), col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Streamflow (cfs)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%b %Y"), cex.axis = 1.5)
axis(2, at = seq(0,50,2), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = BasinStreamflow[,seq(2606,2606+365,1)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(colnames(BasinStreamflow)[seq(2606,2606+365,1)]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1,
     xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()

png(paste0('satDefRepsBasin_Med.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(colnames(BasinSatDef)[-1]), y = t(BasinSatDef[,-1]), col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Saturation Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,20000,500), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = BasinSatDef[,-1], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
par(new=TRUE)
plot(x = as.Date(colnames(BasinSatDef)[-1]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
     xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
dev.off()

png(paste0('satDefRepsBasin_Quants.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(colnames(BasinSatDef)[-1]), y = t(BasinSatDef[,-1]), col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Saturation Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
axis(2, at = seq(0,20000,500), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = BasinSatDef[,-1], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
for (q in 1:nrow(quants)){
  par(new=TRUE)
  plot(x = as.Date(colnames(BasinSatDef)[-1]), y = quants[q,], col = 'red', lty = qlty[q], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1,
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
legend('bottomright', title = 'Quantiles', legend = seq(0,1,0.25), lty = qlty, col = 'red')
dev.off()

png(paste0('satDefRepsBasin_2007_Med.png'), res = 300, height = 5, width=5, units = 'in')
matplot(x = as.Date(colnames(BasinSatDef)[seq(2606,2606+365,1)]), y = t(BasinSatDef[,seq(2606,2606+365,1)]), col = grey(level = 0.1, alpha = 0.01), xlab = 'Year', ylab = 'Saturation Deficit (mm)', type = 'l', axes=FALSE, cex.lab = 1.5)
box()
axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%b %Y"), cex.axis = 1.5)
axis(2, at = seq(0,20000,500), labels = TRUE, cex.axis = 1.5)
#Add quantiles
quants = apply(X = BasinSatDef[,seq(2606,2606+365,1)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
qlty = c(3,2,1,2,3)
for (q in 1:nrow(quants)){
  par(new=TRUE)
  plot(x = as.Date(colnames(BasinSatDef)[seq(2606,2606+365,1)]), y = quants[q,], col = 'red', lty = qlty[q], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1,
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()

#Hillslope----
png(paste0('streamflowRepsHill_Med.png'), res = 300, height = 10, width=10, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12), c(13,14)))
par(mar = c(3,3,3,0.5))
for (h in 1:length(uhills)){
  matplot(x = as.Date(colnames(BasinStreamflow)[-1]), y = t(HillStreamflow[seq(1+(uhills[h]-1)*length(fs), length(fs)+(uhills[h]-1)*length(fs), 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,.1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = HillStreamflow[seq(1+(uhills[h]-1)*length(fs), length(fs)+(uhills[h]-1)*length(fs), 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(colnames(BasinStreamflow)[-1]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h)

png(paste0('streamflowRepsHill_Cut1yr_Med.png'), res = 300, height = 10, width=10, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12), c(13,14)))
par(mar = c(3,3,3,0.5))
for (h in 1:length(uhills)){
  matplot(x = as.Date(colnames(BasinStreamflow)[-seq(1,414,1)]), y = t(HillStreamflow[seq(1+(uhills[h]-1)*length(fs), length(fs)+(uhills[h]-1)*length(fs), 1), -c(1,2)][-seq(1,413,1)]),
          main = paste0('Hillslope ', uhills[h], ' Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,.1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = HillStreamflow[seq(1+(uhills[h]-1)*length(fs), length(fs)+(uhills[h]-1)*length(fs), 1), -c(1,2)][-seq(1,413,1)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(colnames(BasinStreamflow)[-seq(1,414,1)]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h)

png(paste0('streamflowRepsHill_Cut1yr_Quants.png'), res = 300, height = 10, width=10, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12), c(13,14)))
par(mar = c(3,3,3,0.5))
for (h in 1:length(uhills)){
  matplot(x = as.Date(colnames(BasinStreamflow)[-seq(1,414,1)]), y = t(HillStreamflow[seq(1+(uhills[h]-1)*length(fs), length(fs)+(uhills[h]-1)*length(fs), 1), -c(1,2)][-seq(1,413,1)]),
          main = paste0('Hillslope ', uhills[h], ' Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,.1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = HillStreamflow[seq(1+(uhills[h]-1)*length(fs), length(fs)+(uhills[h]-1)*length(fs), 1), -c(1,2)][-seq(1,413,1)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  for (q in 1:nrow(quants)){
    par(new=TRUE)
    plot(x = as.Date(colnames(BasinStreamflow)[-seq(1,414,1)]), y = quants[q,], col = 'red', lty = qlty[q], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
         xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  }
}
dev.off()
rm(h)

png(paste0('streamflowRepsHill_2007_Med.png'), res = 300, height = 10, width=10, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12), c(13,14)))
par(mar = c(3,3,3,0.5))
for (h in 1:length(uhills)){
  matplot(x = as.Date(colnames(BasinStreamflow)[seq(2606,2606+365,1)]), y = t(HillStreamflow[seq(1+(uhills[h]-1)*length(fs), length(fs)+(uhills[h]-1)*length(fs), 1), -c(1,2)][seq(2605,2605+365,1)]),
          main = paste0('Hillslope ', uhills[h], ' Streamflow'), type = 'l', xlab = 'Year', ylab = 'Streamflow (cfs)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%b %Y"), cex.axis = 1.5)
  axis(2, at = seq(0,50,.1), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = HillStreamflow[seq(1+(uhills[h]-1)*length(fs), length(fs)+(uhills[h]-1)*length(fs), 1), -c(1,2)][seq(2605,2605+365,1)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(colnames(BasinStreamflow)[seq(2606,2606+365,1)]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h)

png(paste0('satDefRepsHill_Med.png'), res = 300, height = 10, width=10, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12), c(13,14)))
par(mar = c(3,3,3,0.5))
for (h in 1:length(uhills)){
  matplot(x = as.Date(colnames(BasinSatDef)[-1]), y = t(HillSatDef[seq(1+(uhills[h]-1)*length(fs), length(fs)+(uhills[h]-1)*length(fs), 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Saturation Deficit'), type = 'l', xlab = 'Year', ylab = 'Saturation Deficit (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,20000,500), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = HillSatDef[seq(1+(uhills[h]-1)*length(fs), length(fs)+(uhills[h]-1)*length(fs), 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(colnames(BasinSatDef)[-1]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h)

png(paste0('satDefRepsHill_Quants.png'), res = 300, height = 10, width=10, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12), c(13,14)))
par(mar = c(3,3,3,0.5))
for (h in 1:length(uhills)){
  matplot(x = as.Date(colnames(BasinSatDef)[-1]), y = t(HillSatDef[seq(1+(uhills[h]-1)*length(fs), length(fs)+(uhills[h]-1)*length(fs), 1), -c(1,2)]),
          main = paste0('Hillslope ', uhills[h], ' Saturation Deficit'), type = 'l', xlab = 'Year', ylab = 'Saturation Deficit (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,20000,500), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = HillSatDef[seq(1+(uhills[h]-1)*length(fs), length(fs)+(uhills[h]-1)*length(fs), 1), -c(1,2)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  for (q in 1:nrow(quants)){
    par(new=TRUE)
    plot(x = as.Date(colnames(BasinSatDef)[-1]), y = quants[q,], col = 'red', lty = qlty[q], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
         xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
  }
}
dev.off()
rm(h)

png(paste0('satDefRepsHill_2007_Med.png'), res = 300, height = 10, width=10, units = 'in')
layout(rbind(c(1,2), c(3,4), c(5,6), c(7,8), c(9,10), c(11,12), c(13,14)))
par(mar = c(3,3,3,0.5))
for (h in 1:length(uhills)){
  matplot(x = as.Date(colnames(BasinSatDef)[seq(2606,2606+365,1)]), y = t(HillSatDef[seq(1+(uhills[h]-1)*length(fs), length(fs)+(uhills[h]-1)*length(fs), 1), -c(1,2)][seq(2605,2605+365,1)]),
          main = paste0('Hillslope ', uhills[h], ' Saturation Deficit'), type = 'l', xlab = 'Year', ylab = 'Saturation Deficit (mm)',
          col = grey(level = 0.1, alpha = 0.01), axes=FALSE, cex.lab = 1.5)
  box()
  axis(1, at = as.Date(paste0(seq(1999,2020,1), '-01-01')), labels = format(as.Date(paste0(seq(1999,2020,1), '-01-01')), "%Y"), cex.axis = 1.5)
  axis(2, at = seq(0,20000,500), labels = TRUE, cex.axis = 1.5)
  #Add quantiles
  quants = apply(X = HillSatDef[seq(1+(uhills[h]-1)*length(fs), length(fs)+(uhills[h]-1)*length(fs), 1), -c(1,2)][seq(2605,2605+365,1)], MARGIN = 2, FUN = quantile, seq(0, 1, 0.25), na.rm = FALSE, names = TRUE, type = 7)
  qlty = c(3,2,1,2,3)
  par(new=TRUE)
  plot(x = as.Date(colnames(BasinSatDef)[seq(2606,2606+365,1)]), y = quants[3,], col = 'red', lty = qlty[3], xlab = '', ylab = '', type = 'l', axes=FALSE, lwd = 0.1, 
       xaxs="i", yaxs="i", xlim=par('usr')[c(1,2)], ylim=par('usr')[c(3,4)])
}
dev.off()
rm(h)

#Estimate the nitrogen timeseries using WRTDS----
#Load the streamflow data into WRTDS format
Daily = readUserDaily(filePath = "C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\obs", fileName = 'BaismanStreamflow_Cal.txt', hasHeader = TRUE, separator = '\t', qUnit = 1, verbose = FALSE)
#Read the TN data
Sample = readUserSample(filePath = "C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\obs", fileName = 'TN_Cal_WRTDS.txt', hasHeader = TRUE, separator = '\t', verbose = FALSE)
#Set the required information
INFO = readUserInfo(filePath = "C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\obs", fileName = 'WRTDS_INFO.csv', interactive = FALSE)
eList = mergeReport(INFO = INFO, Daily = Daily, Sample = Sample)
saveResults("C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\obs\\", eList)

WRTDSmod = modelEstimation(eList = eList, windowY = 7, windowQ = 2, windowS = .5, minNumObs = 100, minNumUncen = 50, edgeAdjust = TRUE)

setwd("C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\obs\\")
png('ConcFluxTime.png', res = 300, units ='in', width = 12, height = 6)
layout(rbind(c(1,2)))
plotConcTimeDaily(WRTDSmod)
plotFluxTimeDaily(WRTDSmod)
dev.off()

png('ConcFluxTimeErrs.png', res = 300, units ='in', width = 12, height = 6)
layout(rbind(c(1,2)))
plotConcPred(WRTDSmod)
plotFluxPred(WRTDSmod)
dev.off()

plotResidPred(WRTDSmod)
plotResidQ(WRTDSmod)
plotResidTime(WRTDSmod)
boxResidMonth(WRTDSmod)
boxConcThree(WRTDSmod)
boxQTwice(WRTDSmod)
plotFluxHist(WRTDSmod)
plotConcHist(WRTDSmod)

png('BiasPlot.png', res = 300, units ='in', width = 12, height = 12)
fluxBiasMulti(WRTDSmod, cex.axis = 1.5, cex.main = 1.5, cex.lab=1.5)
dev.off()

png('ContourPlotMean.png', res = 300, units ='in', width = 6, height = 6)
plotContours(WRTDSmod, yearStart = 1999, yearEnd = 2011, contourLevels=seq(0,.8,0.05),qUnit=1, qBottom = 0.01, qTop = 100, whatSurface = 1)
dev.off()

png('ContourPlotErr.png', res = 300, units ='in', width = 6, height = 6)
plotContours(WRTDSmod, yearStart = 1999, yearEnd = 2011, contourLevels=seq(0,.3,0.05),qUnit=1, qBottom = 0.01, qTop = 100, whatSurface = 2)
dev.off()

estSurfaces(eList, windowY = windowY, windowQ = windowQ, windowS = windowS, minNumObs = minNumObs, minNumUncen = minNumUncen, edgeAdjust = edgeAdjust, verbose = verbose, run.parallel = run.parallel)
estDailyFromSurfaces
checkSurfaceSpan(eList)

#Model#2 WRTDS
WRTDSmod2 = modelEstimation(eList = eList, windowY = 3, windowQ = 2, windowS = .5, minNumObs = 60, minNumUncen = 50, edgeAdjust = TRUE)

setwd("C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\obs\\")
png('ConcFluxTime.png', res = 300, units ='in', width = 12, height = 6)
layout(rbind(c(1,2)))
plotConcTimeDaily(WRTDSmod2)
plotFluxTimeDaily(WRTDSmod2)
dev.off()

png('ConcFluxTimeErrs.png', res = 300, units ='in', width = 12, height = 6)
layout(rbind(c(1,2)))
plotConcPred(WRTDSmod2)
plotFluxPred(WRTDSmod2)
dev.off()

plotResidPred(WRTDSmod2)
plotResidQ(WRTDSmod2)
plotResidTime(WRTDSmod2)
boxResidMonth(WRTDSmod2)
boxConcThree(WRTDSmod2)
boxQTwice(WRTDSmod2)
plotFluxHist(WRTDSmod2)
plotConcHist(WRTDSmod2)

png('BiasPlot.png', res = 300, units ='in', width = 12, height = 12)
fluxBiasMulti(WRTDSmod2, cex.axis = 1.5, cex.main = 1.5, cex.lab=1.5)
dev.off()

png('ContourPlotMean.png', res = 300, units ='in', width = 6, height = 6)
plotContours(WRTDSmod2, yearStart = 1999, yearEnd = 2011, contourLevels=seq(0,2,0.4),qUnit=1, qBottom = 0.01, qTop = 100, whatSurface = 1)
dev.off()

png('ContourPlotErr.png', res = 300, units ='in', width = 6, height = 6)
plotContours(WRTDSmod2, yearStart = 1999, yearEnd = 2011, contourLevels=seq(0,.4,0.05),qUnit=1, qBottom = 0.01, qTop = 100, whatSurface = 2)
dev.off()

#Will want to get all replicates for SA using a metric of choice
sensitivity::pcc()
sensitivity::plot3d.morris()
?sensitivity::morrisMultOut()
?sensitivity::morris()

#Show SA metrics for the basin and for each hillslope - ranks, and maps for hillslope
