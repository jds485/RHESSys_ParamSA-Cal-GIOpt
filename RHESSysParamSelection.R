#Script to select parameters to use in calibration, based on the SA results from Smith et al.

#Set directories----
#Color functions - from JDS github repo: Geothermal_ESDA
dir_Main = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR"
dir_ColFuns = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\Hydrology\\USGSGauges"

#Set file locations----
f_HelpFuns = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\RHESSys_ParameterSA\\SAScriptFunctions.R"

#Load libraries----
library(sp)
library(rgdal)
library(vroom)
library(cluster)
library(factoextra)
library(dendextend)
library(tidyverse)
library(scico)

#Load functions----
source(paste0(dir_ColFuns, '\\ColorFunctions.R'))
source(f_HelpFuns)

#Set directory----
setwd(dir_Main)

#Load data----
load("SApaperData_2021June13.RData")

#Compute the correlation of parameters that have non-zero EEs----
#Matrix for the similarity measure
#Determine number of columns based on number of non-zero EEs
CorCols = length(which(RanksMua05_b$EE05_b != 0))
CosPhi05_b = CosPhi95_b = CosPhiot_b = matrix(NA, nrow = CorCols, ncol = CorCols)
for (i in 1:CorCols){
  for (j in 1:CorCols){
    CosPhi05_b[i,j] = abs(t(EEs05_b[,which((colnames(EEs05_b) %in% ParamRanges$NumberedParams))][,which(muaEEs05_b != 0)][,i]) %*% EEs05_b[,which((colnames(EEs05_b) %in% ParamRanges$NumberedParams))][,which(muaEEs05_b != 0)][,j])/norm_vec(EEs05_b[,which((colnames(EEs05_b) %in% ParamRanges$NumberedParams))][,which(muaEEs05_b != 0)][,i])/norm_vec(EEs05_b[,which((colnames(EEs05_b) %in% ParamRanges$NumberedParams))][,which(muaEEs05_b != 0)][,j])
    CosPhi95_b[i,j] = abs(t(EEs95_b[,which((colnames(EEs95_b) %in% ParamRanges$NumberedParams))][,which(muaEEs95_b != 0)][,i]) %*% EEs95_b[,which((colnames(EEs95_b) %in% ParamRanges$NumberedParams))][,which(muaEEs95_b != 0)][,j])/norm_vec(EEs95_b[,which((colnames(EEs95_b) %in% ParamRanges$NumberedParams))][,which(muaEEs95_b != 0)][,i])/norm_vec(EEs95_b[,which((colnames(EEs95_b) %in% ParamRanges$NumberedParams))][,which(muaEEs95_b != 0)][,j])
    CosPhiot_b[i,j] = abs(t(EEsot_b[,which((colnames(EEsot_b) %in% ParamRanges$NumberedParams))][,which(muaEEsot_b != 0)][,i]) %*% EEsot_b[,which((colnames(EEsot_b) %in% ParamRanges$NumberedParams))][,which(muaEEsot_b != 0)][,j])/norm_vec(EEsot_b[,which((colnames(EEsot_b) %in% ParamRanges$NumberedParams))][,which(muaEEsot_b != 0)][,i])/norm_vec(EEsot_b[,which((colnames(EEsot_b) %in% ParamRanges$NumberedParams))][,which(muaEEsot_b != 0)][,j])
  }
}
rm(i,j)

colnames(CosPhi05_b) = colnames(OrigParams[which(muaEEs05_b != 0)])
colnames(CosPhi95_b) = colnames(OrigParams[which(muaEEs95_b != 0)])
colnames(CosPhiot_b) = colnames(OrigParams[which(muaEEsot_b != 0)])
rownames(CosPhi05_b) = colnames(OrigParams[which(muaEEs05_b != 0)])
rownames(CosPhi95_b) = colnames(OrigParams[which(muaEEs95_b != 0)])
rownames(CosPhiot_b) = colnames(OrigParams[which(muaEEsot_b != 0)])

#Heatmaps----
png('ParamCorrelations_EE05.png', res = 300, height = 8, width = 8, units = 'in')
heatmap(x = CosPhi05_b, na.rm = TRUE, symm = TRUE, revC = TRUE)
dev.off()

png('ParamCorrelations_EE95.png', res = 300, height = 8, width = 8, units = 'in')
heatmap(x = CosPhi95_b, na.rm = TRUE, symm = TRUE, revC = TRUE)
dev.off()

png('ParamCorrelations_EEot.png', res = 300, height = 8, width = 8, units = 'in')
heatmap(x = CosPhiot_b, na.rm = TRUE, symm = TRUE, revC = TRUE)
dev.off()

# Aggregated----
CorCols_Agg = length(which(RanksMua05_b_Agg$EE05_b != 0))
CosPhi05_b_Agg = CosPhi95_b_Agg = CosPhiot_b_Agg = matrix(NA, nrow = CorCols_Agg, ncol = CorCols_Agg)
#Save the length of the norm for use in selecting total number of parameters
NormLen05_b_Agg = NormLen95_b_Agg = NormLenot_b_Agg = vector('numeric', length = CorCols_Agg)

for (i in 1:CorCols_Agg){
  for (j in 1:CorCols_Agg){
    CosPhi05_b_Agg[i,j] = abs(t(EEs05_b[,which((!(colnames(EEs05_b) %in% ColsAggregated)) & (EEs05_b_mua_m != 0))][,i]) %*% EEs05_b[,which((!(colnames(EEs05_b) %in% ColsAggregated)) & (EEs05_b_mua_m != 0))][,j])/norm_vec(EEs05_b[,which((!(colnames(EEs05_b) %in% ColsAggregated)) & (EEs05_b_mua_m != 0))][,i])/norm_vec(EEs05_b[,which((!(colnames(EEs05_b) %in% ColsAggregated)) & (EEs05_b_mua_m != 0))][,j])
    CosPhi95_b_Agg[i,j] = abs(t(EEs95_b[,which((!(colnames(EEs95_b) %in% ColsAggregated)) & (EEs95_b_mua_m != 0))][,i]) %*% EEs95_b[,which((!(colnames(EEs95_b) %in% ColsAggregated)) & (EEs95_b_mua_m != 0))][,j])/norm_vec(EEs95_b[,which((!(colnames(EEs95_b) %in% ColsAggregated)) & (EEs95_b_mua_m != 0))][,i])/norm_vec(EEs95_b[,which((!(colnames(EEs95_b) %in% ColsAggregated)) & (EEs95_b_mua_m != 0))][,j])
    CosPhiot_b_Agg[i,j] = abs(t(EEsot_b[,which((!(colnames(EEsot_b) %in% ColsAggregated)) & (EEsot_b_mua_m != 0))][,i]) %*% EEsot_b[,which((!(colnames(EEsot_b) %in% ColsAggregated)) & (EEsot_b_mua_m != 0))][,j])/norm_vec(EEsot_b[,which((!(colnames(EEsot_b) %in% ColsAggregated)) & (EEsot_b_mua_m != 0))][,i])/norm_vec(EEsot_b[,which((!(colnames(EEsot_b) %in% ColsAggregated)) & (EEsot_b_mua_m != 0))][,j])
  }
  NormLen05_b_Agg[i] = norm_vec(EEs05_b[,which((!(colnames(EEs05_b) %in% ColsAggregated)) & (EEs05_b_mua_m != 0))][,i])
  NormLen95_b_Agg[i] = norm_vec(EEs95_b[,which((!(colnames(EEs95_b) %in% ColsAggregated)) & (EEs95_b_mua_m != 0))][,i])
  NormLenot_b_Agg[i] = norm_vec(EEsot_b[,which((!(colnames(EEsot_b) %in% ColsAggregated)) & (EEsot_b_mua_m != 0))][,i])
}
rm(i,j)

colnames(CosPhi05_b_Agg) = colnames(EEs05_b[,which((!(colnames(EEs05_b) %in% ColsAggregated)) & (EEs05_b_mua_m != 0))])
colnames(CosPhi95_b_Agg) = colnames(EEs95_b[,which((!(colnames(EEs95_b) %in% ColsAggregated)) & (EEs95_b_mua_m != 0))])
colnames(CosPhiot_b_Agg) = colnames(EEsot_b[,which((!(colnames(EEsot_b) %in% ColsAggregated)) & (EEsot_b_mua_m != 0))])
rownames(CosPhi05_b_Agg) = colnames(EEs05_b[,which((!(colnames(EEs05_b) %in% ColsAggregated)) & (EEs05_b_mua_m != 0))])
rownames(CosPhi95_b_Agg) = colnames(EEs95_b[,which((!(colnames(EEs95_b) %in% ColsAggregated)) & (EEs95_b_mua_m != 0))])
rownames(CosPhiot_b_Agg) = colnames(EEsot_b[,which((!(colnames(EEsot_b) %in% ColsAggregated)) & (EEsot_b_mua_m != 0))])

#Get the parameters whose lengths are within 2.5% percentage of the maximum vector length. These will be evaluated for clustering
ParamsCluster = sort(unique(c(colnames(EEs05_b[,which((!(colnames(EEs05_b) %in% ColsAggregated)) & (EEs05_b_mua_m != 0))][,which(NormLen05_b_Agg >= max(NormLen05_b_Agg)*.025)]),
                              colnames(EEs95_b[,which((!(colnames(EEs95_b) %in% ColsAggregated)) & (EEs95_b_mua_m != 0))][,which(NormLen95_b_Agg >= max(NormLen95_b_Agg)*.025)]),
                              colnames(EEsot_b[,which((!(colnames(EEsot_b) %in% ColsAggregated)) & (EEsot_b_mua_m != 0))][,which(NormLenot_b_Agg >= max(NormLenot_b_Agg)*.025)]))))

png('ParamCorrelations_EE05_Agg.png', res = 300, height = 8, width = 8, units = 'in')
heatmap(x = CosPhi05_b_Agg[which(rownames(CosPhi05_b_Agg) %in% ParamsCluster),which(colnames(CosPhi05_b_Agg) %in% ParamsCluster)], symm = TRUE, revC = TRUE)
dev.off()

png('ParamCorrelations_EE95_Agg.png', res = 300, height = 8, width = 8, units = 'in')
heatmap(x = CosPhi95_b_Agg[which(rownames(CosPhi95_b_Agg) %in% ParamsCluster),which(colnames(CosPhi95_b_Agg) %in% ParamsCluster)], symm = TRUE, revC = TRUE)
dev.off()

png('ParamCorrelations_EEot_Agg.png', res = 300, height = 8, width = 8, units = 'in')
heatmap(x = CosPhiot_b_Agg[which(rownames(CosPhiot_b_Agg) %in% ParamsCluster),which(colnames(CosPhiot_b_Agg) %in% ParamsCluster)], symm = TRUE, revC = TRUE)
dev.off()

#Evaluate total number of selected parameters as a function of correlation----
#Make a 3D plot of total parameters selected as function of threshold and correlation----

#Evaluate correlations for the selected parameters----
RankSelParams_Agg = sort(unique(c(ParamSelect_b_Agg, ParamSelectTN_b_Agg)))
RankSelParams_h_Agg = sort(unique(c(ParamSelect_b_Agg, ParamSelectTN_b_Agg, ParamSelect_h_Agg, ParamSelectTN_h_Agg)))
RankSelParams_h_Agg910 = sort(ParamSelect_h_Agg910)

# Loop over the variables and get the parameters correlated by a certain amount
CorParams = CorParams_h = NULL
corr = 0.5
for (i in 1:length(RankSelParams_Agg)){
  CorParams = unique(c(CorParams, 
                       names(CosPhi05_b_Agg[,which(colnames(CosPhi05_b_Agg) %in% RankSelParams_Agg)][,i][which(CosPhi05_b_Agg[,which(colnames(CosPhi05_b_Agg) %in% RankSelParams_Agg)][,i] > corr)][!(names(CosPhi05_b_Agg[,which(colnames(CosPhi05_b_Agg) %in% RankSelParams_Agg)][,i][which(CosPhi05_b_Agg[,which(colnames(CosPhi05_b_Agg) %in% RankSelParams_Agg)][,i] > corr)]) %in% RankSelParams_Agg)]),
                       names(CosPhi95_b_Agg[,which(colnames(CosPhi95_b_Agg) %in% RankSelParams_Agg)][,i][which(CosPhi95_b_Agg[,which(colnames(CosPhi95_b_Agg) %in% RankSelParams_Agg)][,i] > corr)][!(names(CosPhi95_b_Agg[,which(colnames(CosPhi95_b_Agg) %in% RankSelParams_Agg)][,i][which(CosPhi95_b_Agg[,which(colnames(CosPhi95_b_Agg) %in% RankSelParams_Agg)][,i] > corr)]) %in% RankSelParams_Agg)]),
                       names(CosPhiot_b_Agg[,which(colnames(CosPhiot_b_Agg) %in% RankSelParams_Agg)][,i][which(CosPhiot_b_Agg[,which(colnames(CosPhiot_b_Agg) %in% RankSelParams_Agg)][,i] > corr)][!(names(CosPhiot_b_Agg[,which(colnames(CosPhiot_b_Agg) %in% RankSelParams_Agg)][,i][which(CosPhiot_b_Agg[,which(colnames(CosPhiot_b_Agg) %in% RankSelParams_Agg)][,i] > corr)]) %in% RankSelParams_Agg)])))
}
rm(i)
RankCorSelParams_Agg = unique(c(CorParams, RankSelParams_Agg))

for (i in 1:length(RankSelParams_h_Agg)){
  CorParams_h = unique(c(CorParams_h, 
                         names(CosPhi05_b_Agg[,which(colnames(CosPhi05_b_Agg) %in% RankSelParams_h_Agg)][,i][which(CosPhi05_b_Agg[,which(colnames(CosPhi05_b_Agg) %in% RankSelParams_h_Agg)][,i] > corr)][!(names(CosPhi05_b_Agg[,which(colnames(CosPhi05_b_Agg) %in% RankSelParams_h_Agg)][,i][which(CosPhi05_b_Agg[,which(colnames(CosPhi05_b_Agg) %in% RankSelParams_h_Agg)][,i] > corr)]) %in% RankSelParams_h_Agg)]),
                         names(CosPhi95_b_Agg[,which(colnames(CosPhi95_b_Agg) %in% RankSelParams_h_Agg)][,i][which(CosPhi95_b_Agg[,which(colnames(CosPhi95_b_Agg) %in% RankSelParams_h_Agg)][,i] > corr)][!(names(CosPhi95_b_Agg[,which(colnames(CosPhi95_b_Agg) %in% RankSelParams_h_Agg)][,i][which(CosPhi95_b_Agg[,which(colnames(CosPhi95_b_Agg) %in% RankSelParams_h_Agg)][,i] > corr)]) %in% RankSelParams_h_Agg)]),
                         names(CosPhiot_b_Agg[,which(colnames(CosPhiot_b_Agg) %in% RankSelParams_h_Agg)][,i][which(CosPhiot_b_Agg[,which(colnames(CosPhiot_b_Agg) %in% RankSelParams_h_Agg)][,i] > corr)][!(names(CosPhiot_b_Agg[,which(colnames(CosPhiot_b_Agg) %in% RankSelParams_h_Agg)][,i][which(CosPhiot_b_Agg[,which(colnames(CosPhiot_b_Agg) %in% RankSelParams_h_Agg)][,i] > corr)]) %in% RankSelParams_h_Agg)])))
}
rm(i)
RankCorSelParams_h_Agg = unique(c(CorParams_h, RankSelParams_h_Agg))

# Make plot of the number of variables as a function of the correlation----
xcor = seq(0.5,1,0.01)
NumParamsCor = vector('numeric', length(xcor))
EEsum = vector('numeric', length(xcor))
for (ci in 1:length(xcor)){
  CorParams_plt = NULL
  for (i in 1:length(RankSelParams_Agg)){
    CorParams_plt = unique(c(CorParams_plt, 
                             names(CosPhi05_b_Agg[,which(colnames(CosPhi05_b_Agg) %in% RankSelParams_Agg)][,i][which(CosPhi05_b_Agg[,which(colnames(CosPhi05_b_Agg) %in% RankSelParams_Agg)][,i] > xcor[ci])][!(names(CosPhi05_b_Agg[,which(colnames(CosPhi05_b_Agg) %in% RankSelParams_Agg)][,i][which(CosPhi05_b_Agg[,which(colnames(CosPhi05_b_Agg) %in% RankSelParams_Agg)][,i] > xcor[ci])]) %in% RankSelParams_Agg)]),
                             names(CosPhi95_b_Agg[,which(colnames(CosPhi95_b_Agg) %in% RankSelParams_Agg)][,i][which(CosPhi95_b_Agg[,which(colnames(CosPhi95_b_Agg) %in% RankSelParams_Agg)][,i] > xcor[ci])][!(names(CosPhi95_b_Agg[,which(colnames(CosPhi95_b_Agg) %in% RankSelParams_Agg)][,i][which(CosPhi95_b_Agg[,which(colnames(CosPhi95_b_Agg) %in% RankSelParams_Agg)][,i] > xcor[ci])]) %in% RankSelParams_Agg)]),
                             names(CosPhiot_b_Agg[,which(colnames(CosPhiot_b_Agg) %in% RankSelParams_Agg)][,i][which(CosPhiot_b_Agg[,which(colnames(CosPhiot_b_Agg) %in% RankSelParams_Agg)][,i] > xcor[ci])][!(names(CosPhiot_b_Agg[,which(colnames(CosPhiot_b_Agg) %in% RankSelParams_Agg)][,i][which(CosPhiot_b_Agg[,which(colnames(CosPhiot_b_Agg) %in% RankSelParams_Agg)][,i] > xcor[ci])]) %in% RankSelParams_Agg)])))
  }
  NumParamsCor[ci] = length(unique(c(CorParams_plt, RankSelParams_Agg)))
  EEsum[ci] = sum(EEs95_b_mua_m[names(EEs95_b_mua_m) %in% unique(c(CorParams_plt, RankSelParams_Agg))])
}
rm(ci, CorParams_plt, i)

png('TotalParamsCorr_Agg.png', res = 300, units = 'in', width = 5, height = 5)
plot(x = xcor, y = NumParamsCor, type = 'l', xlab = 'Similarity Index Cutoff', ylab = 'Number of Parameters Selected', ylim = c(0,100), xlim = c(0.5,1), main = 'Threshold = 10%')
dev.off()

png('TotalParamsCorr_CumulativeEE_Agg.png', res = 300, units = 'in', width = 5, height = 5)
plot(x = xcor, y = EEsum, type = 'l', xlab = 'Similarity Index Cutoff', ylab = 'Sum of Normalized Elementary Effects', ylim = c(0,1000), xlim = c(0.5,1), main = 'Threshold = 10%')
dev.off()

#Perform heirarchical clustering for discovering variable clusters----
#Inspired by blog post: https://uc-r.github.io/hc_clustering
# methods to assess
ClustMethods <- c( "average", "single", "complete", "ward")
names(ClustMethods) <- c( "average", "single", "complete", "ward")

# All variables----
df = (EEs05_b[,which((!(colnames(EEs05_b) %in% ColsAggregated)) & (EEs05_b_mua_m != 0))])
#Seems to be very odd clusters when they are scaled, but good clusters when they are not scaled
# for (i in 1:ncol(df)){
#   #Get column mean and sd to normalize
#   df[,i] = (df[,i] - colMeans(x = df)[i])/sd(df[,i])
# }
# rm(i)

map_dbl(ClustMethods, ac)

pltree(agnes(t(df), method = 'ward'), cex = 0.6, hang = -1, main = "Dendrogram of agnes", )
pltree(agnes(t(df), method = 'complete'), cex = 0.6, hang = -1, main = "Dendrogram of agnes", )

# Only the ParamsCluster----
#  Streamflow 5th %-ile----
df = (EEs05_b[,which((colnames(EEs05_b) %in% ParamsCluster))])
for (i in 1:ncol(df)){
  #Get column mean and sd to normalize
  df[,i] = (df[,i] - colMeans(x = df)[i])/sd(df[,i])
}
rm(i)

map_dbl(ClustMethods, ac)

d05 = agnes(t(df), method = 'ward')
for (k in seq(15,35,5)){
  png(paste0('d05_',k,'.png'), res = 300, units = 'in', width = 7, height = 5)
  #pltree(agnes(t(df), method = 'ward'), cex = 0.6, hang = -1, main = "Dendrogram of agnes")
  plot(as.hclust(agnes(t(df), method = 'ward')), cex = 0.6, hang = -1)
  rect.hclust(as.hclust(agnes(t(df), method = 'ward')), k = k, border = rainbow(45))
  axis(side = 1, at = seq(.8,50.8,1), labels = d05$order.lab, tick = 0, line = -7.95, las = 2, cex.axis = 0.6)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEs05_b_mua_95[which(names(EEs05_b_mua_m) %in% d05$order.lab)][match(d05$order.lab, names(EEs05_b_mua_m)[which(names(EEs05_b_mua_m) %in% d05$order.lab)])]), 0), tick = 0, line = -1, cex.axis = 0.3, col.axis = 'blue')
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsot_b_mua_95[which(names(EEsot_b_mua_m) %in% d05$order.lab)][match(d05$order.lab, names(EEsot_b_mua_m)[which(names(EEsot_b_mua_m) %in% d05$order.lab)])]), 0), tick = 0, line = -0.8, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEs95_b_mua_95[which(names(EEs95_b_mua_m) %in% d05$order.lab)][match(d05$order.lab, names(EEs95_b_mua_m)[which(names(EEs95_b_mua_m) %in% d05$order.lab)])]), 0), tick = 0, line = -0.6, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTN05_b_mua_95[which(names(EEsTN05_b_mua_m) %in% d05$order.lab)][match(d05$order.lab, names(EEsTN05_b_mua_m)[which(names(EEsTN05_b_mua_m) %in% d05$order.lab)])]), 0), tick = 0, line = -0.4, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTNMed_b_mua_95[which(names(EEsTNMed_b_mua_m) %in% d05$order.lab)][match(d05$order.lab, names(EEsTNMed_b_mua_m)[which(names(EEsTNMed_b_mua_m) %in% d05$order.lab)])]), 0), tick = 0, line = -0.2, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTN95_b_mua_95[which(names(EEsTN95_b_mua_m) %in% d05$order.lab)][match(d05$order.lab, names(EEsTN95_b_mua_m)[which(names(EEsTN95_b_mua_m) %in% d05$order.lab)])]), 0), tick = 0, line = 0, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1)[d05$order.lab %in% RankSelParams_h_Agg], labels = d05$order.lab[d05$order.lab %in% RankSelParams_h_Agg], tick = 0, line = -7.95, las = 2, cex.axis = 0.6, col.axis = 'red')
  dev.off()
}

#Somewhere from 12-17 clusters seems appropriate
fviz_nbclust(x = df, FUNcluster = hcut, method = 'wss', k.max = 35)
fviz_nbclust(x = df, FUNcluster = hcut, method = 'silhouette', k.max = 35)
gap_stat = clusGap(df, FUNcluster = hcut, K.max = 35, B = 100, d.power = 2, )
fviz_gap_stat(gap_stat)

sort(cutree(as.hclust(agnes(t(df), method = 'ward')), k = 30))
fviz_cluster(list(data = t(df), cluster = cutree(as.hclust(agnes(t(df), method = 'ward')), k = 35)))

#Panel plot with PCAs
fviz_cluster(list(data = t(df), cluster = cutree(as.hclust(agnes(t(df), method = 'ward')), k = 15)), stand = TRUE, show.clust.cent = FALSE, ellipse = TRUE, shape = 16, geom = 'point', axes = c(1,2))

#  Streamflow 5th-95th %-ile----
df = (EEsot_b[,which((colnames(EEsot_b) %in% ParamsCluster))])
for (i in 1:ncol(df)){
  #Get column mean and sd to normalize
  df[,i] = (df[,i] - colMeans(x = df)[i])/sd(df[,i])
}
rm(i)

map_dbl(ClustMethods, ac)

pltree(agnes(t(df), method = 'ward'), cex = 0.6, hang = -1, main = "Dendrogram of agnes", )

dot = agnes(t(df), method = 'ward')
for (k in seq(15,35,5)){
  png(paste0('dot_',k,'.png'), res = 300, units = 'in', width = 7, height = 5)
  #pltree(agnes(t(df), method = 'ward'), cex = 0.6, hang = -1, main = "Dendrogram of agnes")
  plot(as.hclust(agnes(t(df), method = 'ward')), cex = 0.6, hang = -1)
  rect.hclust(as.hclust(agnes(t(df), method = 'ward')), k = k, border = rainbow(45))
  axis(side = 1, at = seq(.8,50.8,1), labels = dot$order.lab, tick = 0, line = -7.95, las = 2, cex.axis = 0.6)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEs05_b_mua_95[which(names(EEs05_b_mua_m) %in% dot$order.lab)][match(dot$order.lab, names(EEs05_b_mua_m)[which(names(EEs05_b_mua_m) %in% dot$order.lab)])]), 0), tick = 0, line = -1, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsot_b_mua_95[which(names(EEsot_b_mua_m) %in% dot$order.lab)][match(dot$order.lab, names(EEsot_b_mua_m)[which(names(EEsot_b_mua_m) %in% dot$order.lab)])]), 0), tick = 0, line = -0.8, cex.axis = 0.3, col.axis = 'blue')
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEs95_b_mua_95[which(names(EEs95_b_mua_m) %in% dot$order.lab)][match(dot$order.lab, names(EEs95_b_mua_m)[which(names(EEs95_b_mua_m) %in% dot$order.lab)])]), 0), tick = 0, line = -0.6, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTN05_b_mua_95[which(names(EEsTN05_b_mua_m) %in% dot$order.lab)][match(dot$order.lab, names(EEsTN05_b_mua_m)[which(names(EEsTN05_b_mua_m) %in% dot$order.lab)])]), 0), tick = 0, line = -0.4, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTNMed_b_mua_95[which(names(EEsTNMed_b_mua_m) %in% dot$order.lab)][match(dot$order.lab, names(EEsTNMed_b_mua_m)[which(names(EEsTNMed_b_mua_m) %in% dot$order.lab)])]), 0), tick = 0, line = -0.2, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTN95_b_mua_95[which(names(EEsTN95_b_mua_m) %in% dot$order.lab)][match(dot$order.lab, names(EEsTN95_b_mua_m)[which(names(EEsTN95_b_mua_m) %in% dot$order.lab)])]), 0), tick = 0, line = 0, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1)[dot$order.lab %in% RankSelParams_h_Agg], labels = dot$order.lab[dot$order.lab %in% RankSelParams_h_Agg], tick = 0, line = -7.95, las = 2, cex.axis = 0.6, col.axis = 'red')
  dev.off()
}

#Somewhere from 12-17 clusters seems appropriate
fviz_nbclust(x = df, FUNcluster = hcut, method = 'wss', k.max = 35)
fviz_nbclust(x = df, FUNcluster = hcut, method = 'silhouette', k.max = 35)
gap_stat = clusGap(df, FUNcluster = hcut, K.max = 35, B = 100, d.power = 2, )
fviz_gap_stat(gap_stat)

sort(cutree(as.hclust(agnes(t(df), method = 'ward')), k = 15))
plot(as.hclust(agnes(t(df), method = 'ward')), cex = 0.6, hang = -1)
rect.hclust(as.hclust(agnes(t(df), method = 'ward')), k = 15, border = 2:16)
fviz_cluster(list(data = t(df), cluster = cutree(as.hclust(agnes(t(df), method = 'ward')), k = 15)))

#Panel plot with PCAs
fviz_cluster(list(data = t(df), cluster = cutree(as.hclust(agnes(t(df), method = 'ward')), k = 15)), stand = TRUE, show.clust.cent = FALSE, ellipse = TRUE, shape = 16, geom = 'point', axes = c(1,2))

#  Streamflow 95th %-ile----
df = (EEs95_b[,which((colnames(EEs95_b) %in% ParamsCluster))])
for (i in 1:ncol(df)){
  #Get column mean and sd to normalize
  df[,i] = (df[,i] - colMeans(x = df)[i])/sd(df[,i])
}
rm(i)

map_dbl(ClustMethods, ac)

pltree(agnes(t(df), method = 'ward'), cex = 0.6, hang = -1, main = "Dendrogram of agnes", )

d95 = agnes(t(df), method = 'ward')
for (k in seq(15,35,5)){
  png(paste0('d95_',k,'.png'), res = 300, units = 'in', width = 7, height = 5)
  #pltree(agnes(t(df), method = 'ward'), cex = 0.6, hang = -1, main = "Dendrogram of agnes")
  plot(as.hclust(agnes(t(df), method = 'ward')), cex = 0.6, hang = -1)
  rect.hclust(as.hclust(agnes(t(df), method = 'ward')), k = k, border = rainbow(45))
  axis(side = 1, at = seq(.8,50.8,1), labels = d95$order.lab, tick = 0, line = -7.95, las = 2, cex.axis = 0.6)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEs05_b_mua_95[which(names(EEs05_b_mua_m) %in% d95$order.lab)][match(d95$order.lab, names(EEs05_b_mua_m)[which(names(EEs05_b_mua_m) %in% d95$order.lab)])]), 0), tick = 0, line = -1, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsot_b_mua_95[which(names(EEsot_b_mua_m) %in% d95$order.lab)][match(d95$order.lab, names(EEsot_b_mua_m)[which(names(EEsot_b_mua_m) %in% d95$order.lab)])]), 0), tick = 0, line = -0.8, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEs95_b_mua_95[which(names(EEs95_b_mua_m) %in% d95$order.lab)][match(d95$order.lab, names(EEs95_b_mua_m)[which(names(EEs95_b_mua_m) %in% d95$order.lab)])]), 0), tick = 0, line = -0.6, cex.axis = 0.3, col.axis = 'blue')
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTN05_b_mua_95[which(names(EEsTN05_b_mua_m) %in% d95$order.lab)][match(d95$order.lab, names(EEsTN05_b_mua_m)[which(names(EEsTN05_b_mua_m) %in% d95$order.lab)])]), 0), tick = 0, line = -0.4, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTNMed_b_mua_95[which(names(EEsTNMed_b_mua_m) %in% d95$order.lab)][match(d95$order.lab, names(EEsTNMed_b_mua_m)[which(names(EEsTNMed_b_mua_m) %in% d95$order.lab)])]), 0), tick = 0, line = -0.2, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTN95_b_mua_95[which(names(EEsTN95_b_mua_m) %in% d95$order.lab)][match(d95$order.lab, names(EEsTN95_b_mua_m)[which(names(EEsTN95_b_mua_m) %in% d95$order.lab)])]), 0), tick = 0, line = 0, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1)[d95$order.lab %in% RankSelParams_h_Agg], labels = d95$order.lab[d95$order.lab %in% RankSelParams_h_Agg], tick = 0, line = -7.95, las = 2, cex.axis = 0.6, col.axis = 'red')
  dev.off()
}

#Somewhere from 12-17 clusters seems appropriate
fviz_nbclust(x = df, FUNcluster = hcut, method = 'wss', k.max = 35)
fviz_nbclust(x = df, FUNcluster = hcut, method = 'silhouette', k.max = 35)
gap_stat = clusGap(df, FUNcluster = hcut, K.max = 35, B = 100, d.power = 2, )
fviz_gap_stat(gap_stat)

sort(cutree(as.hclust(agnes(t(df), method = 'ward')), k = 15))
plot(as.hclust(agnes(t(df), method = 'ward')), cex = 0.6, hang = -1)
rect.hclust(as.hclust(agnes(t(df), method = 'ward')), k = 15, border = 2:16)
fviz_cluster(list(data = t(df), cluster = cutree(as.hclust(agnes(t(df), method = 'ward')), k = 15)))

#Panel plot with PCAs
fviz_cluster(list(data = t(df), cluster = cutree(as.hclust(agnes(t(df), method = 'ward')), k = 15)), stand = TRUE, show.clust.cent = FALSE, ellipse = TRUE, shape = 16, geom = 'point', axes = c(1,2))


#  TN 5th %-ile----
df = (EEsTN05_b[,which((colnames(EEs05_b) %in% ParamsCluster))])
for (i in 1:ncol(df)){
  #Get column mean and sd to normalize
  df[,i] = (df[,i] - colMeans(x = df)[i])/sd(df[,i])
}
rm(i)

map_dbl(ClustMethods, ac)

dTN05 = agnes(t(df), method = 'ward')
for (k in seq(15,35,5)){
  png(paste0('dTN05_',k,'.png'), res = 300, units = 'in', width = 7, height = 5)
  plot(as.hclust(agnes(t(df), method = 'ward')), cex = 0.6, hang = -1)
  rect.hclust(as.hclust(agnes(t(df), method = 'ward')), k = k, border = rainbow(45))
  axis(side = 1, at = seq(.8,50.8,1), labels = dTN05$order.lab, tick = 0, line = -7.95, las = 2, cex.axis = 0.6)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEs05_b_mua_95[which(names(EEs05_b_mua_m) %in% dTN05$order.lab)][match(dTN05$order.lab, names(EEs05_b_mua_m)[which(names(EEs05_b_mua_m) %in% dTN05$order.lab)])]), 0), tick = 0, line = -1, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsot_b_mua_95[which(names(EEsot_b_mua_m) %in% dTN05$order.lab)][match(dTN05$order.lab, names(EEsot_b_mua_m)[which(names(EEsot_b_mua_m) %in% dTN05$order.lab)])]), 0), tick = 0, line = -0.8, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEs95_b_mua_95[which(names(EEs95_b_mua_m) %in% dTN05$order.lab)][match(dTN05$order.lab, names(EEs95_b_mua_m)[which(names(EEs95_b_mua_m) %in% dTN05$order.lab)])]), 0), tick = 0, line = -0.6, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTN05_b_mua_95[which(names(EEsTN05_b_mua_m) %in% dTN05$order.lab)][match(dTN05$order.lab, names(EEsTN05_b_mua_m)[which(names(EEsTN05_b_mua_m) %in% dTN05$order.lab)])]), 0), tick = 0, line = -0.4, cex.axis = 0.3, col.axis = 'blue')
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTNMed_b_mua_95[which(names(EEsTNMed_b_mua_m) %in% dTN05$order.lab)][match(dTN05$order.lab, names(EEsTNMed_b_mua_m)[which(names(EEsTNMed_b_mua_m) %in% dTN05$order.lab)])]), 0), tick = 0, line = -0.2, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTN95_b_mua_95[which(names(EEsTN95_b_mua_m) %in% dTN05$order.lab)][match(dTN05$order.lab, names(EEsTN95_b_mua_m)[which(names(EEsTN95_b_mua_m) %in% dTN05$order.lab)])]), 0), tick = 0, line = 0, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1)[dTN05$order.lab %in% RankSelParams_h_Agg], labels = dTN05$order.lab[dTN05$order.lab %in% RankSelParams_h_Agg], tick = 0, line = -7.95, las = 2, cex.axis = 0.6, col.axis = 'red')
  dev.off()
}

#  TN 50th %-ile----
df = (EEsTNMed_b[,which((colnames(EEs05_b) %in% ParamsCluster))])
for (i in 1:ncol(df)){
  #Get column mean and sd to normalize
  df[,i] = (df[,i] - colMeans(x = df)[i])/sd(df[,i])
}
rm(i)

map_dbl(ClustMethods, ac)

dTNMed = agnes(t(df), method = 'ward')
for (k in seq(15,35,5)){
  png(paste0('dTNMed_',k,'.png'), res = 300, units = 'in', width = 7, height = 5)
  plot(as.hclust(agnes(t(df), method = 'ward')), cex = 0.6, hang = -1)
  rect.hclust(as.hclust(agnes(t(df), method = 'ward')), k = k, border = rainbow(45))
  axis(side = 1, at = seq(.8,50.8,1), labels = dTNMed$order.lab, tick = 0, line = -7.95, las = 2, cex.axis = 0.6)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEs05_b_mua_95[which(names(EEs05_b_mua_m) %in% dTNMed$order.lab)][match(dTNMed$order.lab, names(EEs05_b_mua_m)[which(names(EEs05_b_mua_m) %in% dTNMed$order.lab)])]), 0), tick = 0, line = -1, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsot_b_mua_95[which(names(EEsot_b_mua_m) %in% dTNMed$order.lab)][match(dTNMed$order.lab, names(EEsot_b_mua_m)[which(names(EEsot_b_mua_m) %in% dTNMed$order.lab)])]), 0), tick = 0, line = -0.8, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEs95_b_mua_95[which(names(EEs95_b_mua_m) %in% dTNMed$order.lab)][match(dTNMed$order.lab, names(EEs95_b_mua_m)[which(names(EEs95_b_mua_m) %in% dTNMed$order.lab)])]), 0), tick = 0, line = -0.6, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTN05_b_mua_95[which(names(EEsTN05_b_mua_m) %in% dTNMed$order.lab)][match(dTNMed$order.lab, names(EEsTN05_b_mua_m)[which(names(EEsTN05_b_mua_m) %in% dTNMed$order.lab)])]), 0), tick = 0, line = -0.4, cex.axis = 0.3, col.axis = 'blue')
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTNMed_b_mua_95[which(names(EEsTNMed_b_mua_m) %in% dTNMed$order.lab)][match(dTNMed$order.lab, names(EEsTNMed_b_mua_m)[which(names(EEsTNMed_b_mua_m) %in% dTNMed$order.lab)])]), 0), tick = 0, line = -0.2, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTN95_b_mua_95[which(names(EEsTN95_b_mua_m) %in% dTNMed$order.lab)][match(dTNMed$order.lab, names(EEsTN95_b_mua_m)[which(names(EEsTN95_b_mua_m) %in% dTNMed$order.lab)])]), 0), tick = 0, line = 0, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1)[dTNMed$order.lab %in% RankSelParams_h_Agg], labels = dTNMed$order.lab[dTNMed$order.lab %in% RankSelParams_h_Agg], tick = 0, line = -7.95, las = 2, cex.axis = 0.6, col.axis = 'red')
  dev.off()
}

#  TN 5th %-ile----
df = (EEsTN95_b[,which((colnames(EEs05_b) %in% ParamsCluster))])
for (i in 1:ncol(df)){
  #Get column mean and sd to normalize
  df[,i] = (df[,i] - colMeans(x = df)[i])/sd(df[,i])
}
rm(i)

map_dbl(ClustMethods, ac)

dTN95 = agnes(t(df), method = 'ward')
for (k in seq(15,35,5)){
  png(paste0('dTN95_',k,'.png'), res = 300, units = 'in', width = 7, height = 5)
  plot(as.hclust(agnes(t(df), method = 'ward')), cex = 0.6, hang = -1)
  rect.hclust(as.hclust(agnes(t(df), method = 'ward')), k = k, border = rainbow(45))
  axis(side = 1, at = seq(.8,50.8,1), labels = dTN95$order.lab, tick = 0, line = -7.95, las = 2, cex.axis = 0.6)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEs05_b_mua_95[which(names(EEs05_b_mua_m) %in% dTN95$order.lab)][match(dTN95$order.lab, names(EEs05_b_mua_m)[which(names(EEs05_b_mua_m) %in% dTN95$order.lab)])]), 0), tick = 0, line = -1, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsot_b_mua_95[which(names(EEsot_b_mua_m) %in% dTN95$order.lab)][match(dTN95$order.lab, names(EEsot_b_mua_m)[which(names(EEsot_b_mua_m) %in% dTN95$order.lab)])]), 0), tick = 0, line = -0.8, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEs95_b_mua_95[which(names(EEs95_b_mua_m) %in% dTN95$order.lab)][match(dTN95$order.lab, names(EEs95_b_mua_m)[which(names(EEs95_b_mua_m) %in% dTN95$order.lab)])]), 0), tick = 0, line = -0.6, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTN05_b_mua_95[which(names(EEsTN05_b_mua_m) %in% dTN95$order.lab)][match(dTN95$order.lab, names(EEsTN05_b_mua_m)[which(names(EEsTN05_b_mua_m) %in% dTN95$order.lab)])]), 0), tick = 0, line = -0.4, cex.axis = 0.3, col.axis = 'blue')
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTNMed_b_mua_95[which(names(EEsTNMed_b_mua_m) %in% dTN95$order.lab)][match(dTN95$order.lab, names(EEsTNMed_b_mua_m)[which(names(EEsTNMed_b_mua_m) %in% dTN95$order.lab)])]), 0), tick = 0, line = -0.2, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1), labels = round(as.numeric(EEsTN95_b_mua_95[which(names(EEsTN95_b_mua_m) %in% dTN95$order.lab)][match(dTN95$order.lab, names(EEsTN95_b_mua_m)[which(names(EEsTN95_b_mua_m) %in% dTN95$order.lab)])]), 0), tick = 0, line = 0, cex.axis = 0.3)
  axis(side = 1, at = seq(.8,50.8,1)[dTN95$order.lab %in% RankSelParams_h_Agg], labels = dTN95$order.lab[dTN95$order.lab %in% RankSelParams_h_Agg], tick = 0, line = -7.95, las = 2, cex.axis = 0.6, col.axis = 'red')
  dev.off()
}

#Save parameters to be used for calibration in a new file----
#Dendrograms suggest clustering in different ways for different metrics. Will need all variables if we want to parse out the important variables for each metric.
#Select top 35 ParamRanges[ParamRanges$NumberedParams %in% RankSelParams_h_Agg,1:4]
RankSelParams_Agg = sort(unique(c(ParamSelect_b_Agg, ParamSelectTN_b_Agg)))
RankSelParams_h_Agg = sort(unique(c(ParamSelect_b_Agg, ParamSelectTN_b_Agg, ParamSelect_h_Agg, ParamSelectTN_h_Agg)))
RankSelParams_h_Agg910 = sort(ParamSelect_h_Agg910)

#And also disaggregate variables to add to that list.
RankSelParams_h_Disagg = c(RankSelParams_h_Agg[-which(RankSelParams_h_Agg %in% ColsAggregated_key)], ColsAggregated[which(ColsAggregated_key %in% RankSelParams_h_Agg[which(RankSelParams_h_Agg %in% colnames(EEs05_b_mua[,272:291]))])])
RankSelParams_h_Disagg910 = c(RankSelParams_h_Agg910[-which(RankSelParams_h_Agg910 %in% ColsAggregated_key)], ColsAggregated[which(ColsAggregated_key %in% RankSelParams_h_Agg910[which(RankSelParams_h_Agg910 %in% colnames(EEs05_b_mua[,272:291]))])])
#Also remove the landuse % impervious. That will not be considered uncertain because it will be assigned from land use maps
RankSelParams_h_Disagg = RankSelParams_h_Disagg[-grep(RankSelParams_h_Disagg, pattern = '.percent_impervious', fixed = TRUE)]
RankSelParams_h_Disagg910 = RankSelParams_h_Disagg910[-grep(RankSelParams_h_Disagg910, pattern = '.percent_impervious', fixed = TRUE)]

ParamRanges_Cal = ParamRanges[ParamRanges$NumberedParams %in% RankSelParams_h_Disagg,1:4]
ParamRanges_Cal910 = ParamRanges[ParamRanges$NumberedParams %in% RankSelParams_h_Disagg910,1:4]
#Edit several lower bounds that changed since SA run
ParamRanges_Cal$Lower[ParamRanges_Cal$NumberedParams == 's9_Ksat_0_v'] = 0.2
ParamRanges_Cal$Lower[ParamRanges_Cal$NumberedParams == 's109_Ksat_0_v'] = 0.05
ParamRanges_Cal$Lower[ParamRanges_Cal$NumberedParams == 's8_Ksat_0_v'] = 0.2
ParamRanges_Cal$Lower[ParamRanges_Cal$NumberedParams == 's108_Ksat_0_v'] = 0.05
ParamRanges_Cal$Lower[ParamRanges_Cal$NumberedParams == 'l4_septic_water_load'] = 0

ParamRanges_Cal910$Lower[ParamRanges_Cal910$NumberedParams == 's9_Ksat_0_v'] = 0.2
ParamRanges_Cal910$Lower[ParamRanges_Cal910$NumberedParams == 's109_Ksat_0_v'] = 0.05
ParamRanges_Cal910$Lower[ParamRanges_Cal910$NumberedParams == 's8_Ksat_0_v'] = 0.2
ParamRanges_Cal910$Lower[ParamRanges_Cal910$NumberedParams == 's108_Ksat_0_v'] = 0.05
ParamRanges_Cal910$Lower[ParamRanges_Cal910$NumberedParams == 'l4_septic_water_load'] = 0
#And edit the GW parameter bounds as well based on discussion with Laurence
ParamRanges_Cal910$Upper[ParamRanges_Cal910$NumberedParams == 'h_gw_loss_coeff'] = 0.3
ParamRanges_Cal910$Upper[ParamRanges_Cal910$NumberedParams == 's9_sat_to_gw_coeff'] = 0.3
ParamRanges_Cal910$Upper[ParamRanges_Cal910$NumberedParams == 's109_sat_to_gw_coeff'] = 0.3

#Add all of the parameters of the likelihood function as well
# (kurotsis) beta=-1: uniform, beta=0: Gaussian, beta=1: double exponential
# (skewness) xi=1: symmetric, xi<1: negatively skewed, xi>1: positively skewed
# (standard deviation when mean=0)
# (linear rate of change in standard deviation with mean)
# (lag-1 auto-correlation), phi_1=0: no auto-correlation, phi_1=1: perfect auto-correlation
# (mean bias factor)
LikelihoodParams = cbind(c('PL_beta', 'PL_xi', 'PL_sigma_0', 'PL_sigma_1', 'PL_phi_1', 'PL_mu_h', 
                           'PL_beta_TN', 'PL_xi_TN', 'PL_sigma_0_TN', 'PL_sigma_1_TN', 'PL_phi_1_TN', 'PL_mu_h_TN'), 
                         c('beta', 'xi', 'sigma_0', 'sigma_1', 'phi_1', 'mu_h', 'beta', 'xi', 'sigma_0', 'sigma_1', 'phi_1', 'mu_h'), 
                         c(-1, 0, 0, 0, 0, 0), c(1, 10, 1, 1, 1, 100))
colnames(LikelihoodParams) = colnames(ParamRanges_Cal)
ParamRanges_Cal_Likes = rbind(ParamRanges_Cal, LikelihoodParams)

options(scipen = 999)
write.csv(ParamRanges_Cal, file = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs_Calibration\\BaismanCalibrationParameterProblemFile.csv", row.names = FALSE)
write.csv(ParamRanges_Cal_Likes, file = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs_Calibration\\BaismanCalibrationParameterProblemFile_LikelihoodParams.csv", row.names = FALSE)

write.csv(ParamRanges_Cal910, file = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs_Calibration/BaismanCalibrationParameterProblemFile_NewGWBounds.csv", row.names = FALSE)
options(scipen = 0)