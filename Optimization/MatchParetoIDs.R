#Getting the ID numbers of the Pareto set
#MORO Pareto set----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptMORO")
DVO_MORO = read.table('GInolic.referenceDVO', header = FALSE, sep = ' ')
colnames(DVO_MORO) = c('H9d','H9m','H9u','H10d','H10m','H10u', 'Flooding', 'LowFlow', 'NumTrees') 

#ID dataset
IDs_MORO = as.data.frame(read.table('DVO_c_MOROc.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE))

#Weights for MAP, parameter sets
Weights = c(0.1145924, 0.1093115, 0.1132633, 0.1120881, 0.1123109, 0.1142459, 0.1112661, 0.1102818, 0.1026399)

#Join IDs for NFE and master number to Pareto Set
DVO_MORO$N = 0
DVO_MORO$M = 0
for (i in 1:nrow(DVO_MORO)){
  #Find the IDs
  #Note that there is a special case of "do nothing" that appears for multiple parameter sets. This is handled.
  if (all(DVO_MORO[i,] == rep(0,9))){
    #Take the first instance. Any will do.
    RepPos = IDs_MORO[which(apply(X = t(apply(X = signif(IDs_MORO[,4:9],5), MARGIN = 1, FUN = "==", signif(DVO_MORO[i,1:6],5))), MARGIN = 1, FUN = all) == TRUE),][1,]
    
    #Add the IDs to DVO
    DVO_MORO$N[i] = RepPos$N[1]
    DVO_MORO$M[i] = RepPos$M[1]
  }else{
    #Get the possible replicates that could be the Pareto replicate
    # Based on significant digits dowsn't work because of rounding issues. need an all close comparison, but it's complex
    #RepPos = IDs_MORO[which(apply(X = t(apply(X = signif(IDs_MORO[,4:9],5), MARGIN = 1, FUN = "==", signif(DVO_MORO[i,1:6],5))), MARGIN = 1, FUN = all) == TRUE),]
    
    # Using instead number of trees, which will be the same for MORO and DVO
    RepPos = IDs_MORO[IDs_MORO$NumTrees == DVO_MORO$NumTrees[i],]
    
    #Calculate the flooding and low flow objectives and compare to the DVO value, and take the one with the smallest difference
    fld = lfw = vector('numeric', length=nrow(unique(RepPos[,c(1,3)])))
    for (j in 1:length(fld)){
      #Check that the decision variables are close to the DVO decision variable values
      d1 = abs(signif(RepPos$H9d[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))][1], 6) - signif(DVO_MORO$H9d[i], 6))
      d2 = abs(signif(RepPos$H9m[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))][1], 6) - signif(DVO_MORO$H9m[i], 6))
      d3 = abs(signif(RepPos$H9u[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))][1], 6) - signif(DVO_MORO$H9u[i], 6))
      d4 = abs(signif(RepPos$H10d[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))][1], 6) - signif(DVO_MORO$H10d[i], 6))
      d5 = abs(signif(RepPos$H10m[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))][1], 6) - signif(DVO_MORO$H10m[i], 6))
      d6 = abs(signif(RepPos$H10u[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))][1], 6) - signif(DVO_MORO$H10u[i], 6))
      if (sum(d1,d2,d3,d4,d5,d6) < 1e-5){
        fld[j] = signif(sum(RepPos$Flood[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))] * Weights), 6)
        lfw[j] = signif(sum(RepPos$LowFlow[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))] * Weights), 6)
      }else{
        fld[j] = 999
        lfw[j] = 999
      }
    }
    #Choose the RepPos whose fld and lfw values are closest to the DVO value (they can be different due to rounding)
    RepPos = RepPos[which((RepPos$N == unique(RepPos[,c(1,3)])[which(abs(fld - signif(DVO_MORO$Flooding[i], 6)) + abs(lfw - signif(DVO_MORO$LowFlow[i], 6)) == min(abs(fld - signif(DVO_MORO$Flooding[i], 6)) + abs(lfw - signif(DVO_MORO$LowFlow[i], 6)))),1]) & (RepPos$M == unique(RepPos[,c(1,3)])[which(abs(fld - signif(DVO_MORO$Flooding[i], 6)) + abs(lfw - signif(DVO_MORO$LowFlow[i], 6)) == min(abs(fld - signif(DVO_MORO$Flooding[i], 6)) + abs(lfw - signif(DVO_MORO$LowFlow[i], 6)))),2])),]
    
    #Add the IDs to DVO
    DVO_MORO$N[i] = RepPos$N[1]
    DVO_MORO$M[i] = RepPos$M[1]
    
    rm(d1, d2, d3, d4, d5, d6, fld, lfw)
  }
  rm(RepPos)
}
rm(i)

# Write file----
write.table(DVO_MORO, 'GInolic_IDs.referenceDVO', sep = '\t', row.names=FALSE)

#Synthetic Pareto set----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptSynTruth")
DVO_Syn = read.table('GInolic.referenceDVO', header = FALSE, sep = ' ')
colnames(DVO_Syn) = c('H9d','H9m','H9u','H10d','H10m','H10u', 'Flooding', 'LowFlow', 'NumTrees') 

#ID dataset
IDs_Syn = as.data.frame(read.table('DVO_c_Syn.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE))

#Join IDs for NFE and master number to Pareto Set
DVO_Syn$N = 0
DVO_Syn$M = 0
for (i in 1:nrow(DVO_Syn)){
  #Find the IDs
  #Get the possible replicates that could be the Pareto replicate
  # Using instead number of trees, which will be the same for Syn and DVO
  RepPos = IDs_Syn[IDs_Syn$NumTrees == DVO_Syn$NumTrees[i],]
  
  #Calculate the flooding and low flow objectives and compare to the DVO value, and take the one with the smallest difference
  #Unique identifiers are N and M.
  fld = lfw = vector('numeric', length=nrow(unique(RepPos[,c(1,3)])))
  if (nrow(RepPos) != nrow(unique(RepPos[,c(1,3)]))){
    print(paste('not equal', i))
  }
  for (j in 1:length(fld)){
    #Check that the decision variables are close to the DVO decision variable values
    d1 = abs(signif(RepPos$H9d[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))][1], 6) - signif(DVO_Syn$H9d[i], 6))
    d2 = abs(signif(RepPos$H9m[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))][1], 6) - signif(DVO_Syn$H9m[i], 6))
    d3 = abs(signif(RepPos$H9u[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))][1], 6) - signif(DVO_Syn$H9u[i], 6))
    d4 = abs(signif(RepPos$H10d[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))][1], 6) - signif(DVO_Syn$H10d[i], 6))
    d5 = abs(signif(RepPos$H10m[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))][1], 6) - signif(DVO_Syn$H10m[i], 6))
    d6 = abs(signif(RepPos$H10u[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))][1], 6) - signif(DVO_Syn$H10u[i], 6))
    if (sum(d1,d2,d3,d4,d5,d6) < 1e-5){
      fld[j] = signif(RepPos$Flood[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))], 6)
      lfw[j] = signif(RepPos$LowFlow[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))], 6)
      if (length(signif(RepPos$Flood[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))], 6)) > 1){
        print(j)
      }
    }else{
      fld[j] = 999
      lfw[j] = 999
    }
  }
  #Choose the RepPos whose fld and lfw values are closest to the DVO value (they can be different due to rounding)
  RepPos = RepPos[which((RepPos$N == unique(RepPos[,c(1,3)])[which(abs(fld - signif(DVO_Syn$Flooding[i], 6)) + abs(lfw - signif(DVO_Syn$LowFlow[i], 6)) == min(abs(fld - signif(DVO_Syn$Flooding[i], 6)) + abs(lfw - signif(DVO_Syn$LowFlow[i], 6)))),1]) & (RepPos$M == unique(RepPos[,c(1,3)])[which(abs(fld - signif(DVO_Syn$Flooding[i], 6)) + abs(lfw - signif(DVO_Syn$LowFlow[i], 6)) == min(abs(fld - signif(DVO_Syn$Flooding[i], 6)) + abs(lfw - signif(DVO_Syn$LowFlow[i], 6)))),2])),]
  
  #Add the IDs to DVO
  DVO_Syn$N[i] = RepPos$N[1]
  DVO_Syn$M[i] = RepPos$M[1]
  rm(RepPos, d1, d2, d3, d4, d5, d6, fld, lfw)
}
rm(i)

# Write file----
write.table(DVO_Syn, 'GInolic_IDs.referenceDVO', sep = '\t', row.names=FALSE)

#MAP Pareto set----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptMAP")
DVO_MAP = read.table('GInolic.referenceDVO', header = FALSE, sep = ' ')
colnames(DVO_MAP) = c('H9d','H9m','H9u','H10d','H10m','H10u', 'Flooding', 'LowFlow', 'NumTrees') 

#ID dataset
IDs_MAP = as.data.frame(read.table('DVO_c_MAP.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE))

#Join IDs for NFE and master number to Pareto Set
DVO_MAP$N = 0
DVO_MAP$M = 0
for (i in 1:nrow(DVO_MAP)){
  #Find the IDs
  #Note that there is a special case of "do nothing" that appears for multiple parameter sets. This is handled.
  if (all(DVO_MAP[i,] == rep(0,9))){
    #Take the first instance. Any will do.
    RepPos = IDs_MAP[which(apply(X = t(apply(X = signif(IDs_MAP[,4:9],5), MARGIN = 1, FUN = "==", signif(DVO_MAP[i,1:6],5))), MARGIN = 1, FUN = all) == TRUE),][1,]
    
    #Add the IDs to DVO
    DVO_MAP$N[i] = RepPos$N[1]
    DVO_MAP$M[i] = RepPos$M[1]
  }else{
    #Get the possible replicates that could be the Pareto replicate
    # Using instead number of trees, which will be the same for MAP and DVO
    RepPos = IDs_MAP[IDs_MAP$NumTrees == DVO_MAP$NumTrees[i],]
    
    #Calculate the flooding and low flow objectives and compare to the DVO value, and take the one with the smallest difference
    fld = lfw = vector('numeric', length=nrow(unique(RepPos[,c(1,3)])))
    if (nrow(RepPos) != nrow(unique(RepPos[,c(1,3)]))){
      print(paste('not equal', i))
    }
    for (j in 1:length(fld)){
      #Check that the decision variables are close to the DVO decision variable values
      d1 = abs(signif(RepPos$H9d[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))][1], 6) - signif(DVO_MAP$H9d[i], 6))
      d2 = abs(signif(RepPos$H9m[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))][1], 6) - signif(DVO_MAP$H9m[i], 6))
      d3 = abs(signif(RepPos$H9u[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))][1], 6) - signif(DVO_MAP$H9u[i], 6))
      d4 = abs(signif(RepPos$H10d[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))][1], 6) - signif(DVO_MAP$H10d[i], 6))
      d5 = abs(signif(RepPos$H10m[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))][1], 6) - signif(DVO_MAP$H10m[i], 6))
      d6 = abs(signif(RepPos$H10u[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))][1], 6) - signif(DVO_MAP$H10u[i], 6))
      if (sum(d1,d2,d3,d4,d5,d6) < 1e-5){
        fld[j] = signif(RepPos$Flood[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))], 6)
        lfw[j] = signif(RepPos$LowFlow[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))], 6)
      }else{
        fld[j] = 999
        lfw[j] = 999
      }
    }
    #Choose the RepPos whose fld and lfw values are closest to the DVO value (they can be different due to rounding)
    RepPos = RepPos[which((RepPos$N == unique(RepPos[,c(1,3)])[which(abs(fld - signif(DVO_MAP$Flooding[i], 6)) + abs(lfw - signif(DVO_MAP$LowFlow[i], 6)) == min(abs(fld - signif(DVO_MAP$Flooding[i], 6)) + abs(lfw - signif(DVO_MAP$LowFlow[i], 6)))),1]) & (RepPos$M == unique(RepPos[,c(1,3)])[which(abs(fld - signif(DVO_MAP$Flooding[i], 6)) + abs(lfw - signif(DVO_MAP$LowFlow[i], 6)) == min(abs(fld - signif(DVO_MAP$Flooding[i], 6)) + abs(lfw - signif(DVO_MAP$LowFlow[i], 6)))),2])),]
    
    #Add the IDs to DVO
    DVO_MAP$N[i] = RepPos$N[1]
    DVO_MAP$M[i] = RepPos$M[1]
    
    rm(d1, d2, d3, d4, d5, d6, fld, lfw)
  }
  rm(RepPos)
}
rm(i)

# Write file----
write.table(DVO_MAP, 'GInolic_IDs.referenceDVO', sep = '\t', row.names=FALSE)


#MOROMinMax Pareto set----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\OptMinMax")
DVO_MinMax = read.table('GI.referenceDVO', header = FALSE, sep = ' ')
colnames(DVO_MinMax) = c('H9d','H9m','H9u','H10d','H10m','H10u', 'Flooding', 'LowFlow', 'NumTrees') 

#ID dataset
IDs_MinMax = as.data.frame(read.table('DVO_c_MOROMinMax.txt', header = TRUE, sep = '\t', stringsAsFactors = FALSE))

#Join IDs for NFE and master number to Pareto Set
DVO_MinMax$N = 0
DVO_MinMax$M = 0
for (i in 1:nrow(DVO_MinMax)){
  #Find the IDs
  #Note that there is a special case of "do nothing" that appears for multiple parameter sets. This is handled.
  if (all(DVO_MinMax[i,] == rep(0,9))){
    #Take the first instance. Any will do.
    RepPos = IDs_MinMax[which(apply(X = t(apply(X = signif(IDs_MinMax[,4:9],5), MARGIN = 1, FUN = "==", signif(DVO_MinMax[i,1:6],5))), MARGIN = 1, FUN = all) == TRUE),][1,]
    
    #Add the IDs to DVO
    DVO_MinMax$N[i] = RepPos$N[1]
    DVO_MinMax$M[i] = RepPos$M[1]
  }else{
    #Get the possible replicates that could be the Pareto replicate
    # Based on significant digits doesn't work because of rounding issues. need an all close comparison, but it's complex
    #RepPos = IDs_MinMax[which(apply(X = t(apply(X = signif(IDs_MinMax[,4:9],5), MARGIN = 1, FUN = "==", signif(DVO_MinMax[i,1:6],5))), MARGIN = 1, FUN = all) == TRUE),]
    
    # Using instead number of trees, which will be the same for MinMax and DVO
    RepPos = IDs_MinMax[which(IDs_MinMax$NumTrees == DVO_MinMax$NumTrees[i]),]
    
    #Calculate the flooding and low flow objectives and compare to the DVO value, and take the one with the smallest difference
    fld = lfw = vector('numeric', length=nrow(unique(RepPos[,c(1,3)])))
    for (j in 1:length(fld)){
      #Check that the decision variables are close to the DVO decision variable values
      d1 = abs(signif(RepPos$H9d[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))][1], 6) - signif(DVO_MinMax$H9d[i], 6))
      d2 = abs(signif(RepPos$H9m[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))][1], 6) - signif(DVO_MinMax$H9m[i], 6))
      d3 = abs(signif(RepPos$H9u[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))][1], 6) - signif(DVO_MinMax$H9u[i], 6))
      d4 = abs(signif(RepPos$H10d[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))][1], 6) - signif(DVO_MinMax$H10d[i], 6))
      d5 = abs(signif(RepPos$H10m[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))][1], 6) - signif(DVO_MinMax$H10m[i], 6))
      d6 = abs(signif(RepPos$H10u[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))][1], 6) - signif(DVO_MinMax$H10u[i], 6))
      if (sum(d1,d2,d3,d4,d5,d6) < 1e-5){
        fld[j] = signif(max(RepPos$Flood[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))]), 6)
        lfw[j] = signif(max(RepPos$LowFlow[which((RepPos$N == unique(RepPos[,c(1,3)])[j,1]) & (RepPos$M == unique(RepPos[,c(1,3)])[j,2]))]), 6)
      }else{
        fld[j] = 999
        lfw[j] = 999
      }
    }
    #Choose the RepPos whose fld and lfw values are closest to the DVO value (they can be different due to rounding)
    RepPos = RepPos[which((RepPos$N == unique(RepPos[,c(1,3)])[which(abs(fld - signif(DVO_MinMax$Flooding[i], 6)) + abs(lfw - signif(DVO_MinMax$LowFlow[i], 6)) == min(abs(fld - signif(DVO_MinMax$Flooding[i], 6)) + abs(lfw - signif(DVO_MinMax$LowFlow[i], 6)))),1]) & (RepPos$M == unique(RepPos[,c(1,3)])[which(abs(fld - signif(DVO_MinMax$Flooding[i], 6)) + abs(lfw - signif(DVO_MinMax$LowFlow[i], 6)) == min(abs(fld - signif(DVO_MinMax$Flooding[i], 6)) + abs(lfw - signif(DVO_MinMax$LowFlow[i], 6)))),2])),]
    
    #Add the IDs to DVO
    DVO_MinMax$N[i] = RepPos$N[1]
    DVO_MinMax$M[i] = RepPos$M[1]
    
    rm(d1, d2, d3, d4, d5, d6, fld, lfw)
  }
  rm(RepPos)
}
rm(i)

# Write file----
write.table(DVO_MinMax, 'GInolic_IDs.referenceDVO', sep = '\t', row.names=FALSE)
