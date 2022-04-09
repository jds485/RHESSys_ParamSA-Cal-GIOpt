#Script for calculating summary statistics of Morris EEs

#Set directories----
dir_Main = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR"
dir_ParamInCutoff = paste0(dir_Main, '/SelectedParamsTotals+Fig2')
dir_EEpanels = paste0(dir_Main, '/EEpanels+Fig3+Fig6')
dir_HillPanels = paste0(dir_Main, '/Hillpanels+Fig4+Fig7')
dir_Mult = paste0(dir_Main, '/MultiplierEval+Fig5')

#Set file locations----
f_HelpFuns = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\RHESSys_ParameterSA\\SensitivityAnalysis\\SAScriptFunctions.R"

#Load libraries----
library(sp)
library(rgdal)
library(vroom)
library(tidyverse)
library(scico)

#EEs were computed on Rivanna with saved datasets.
#Load data from Rivanna run----
#Input data load - loads all data that were saved in the save.image file in the RHESSysSensitivityAnalysisSetup.R script
setwd(dir_Main)
dir_Main1 = dir_Main
load("EEs_All_Setup_paper.RData")
load("EEs_Meds_Setup_b.RData")
dir_Main = dir_Main1
rm(dir_Main1, dir_ColFuns)

#Load Helper Functions----
source(f_HelpFuns)

#Load EE and Deltas info----
Deltas = read.table(file = paste0(getwd(), '/Deltas_AllMetrics.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEs05_b = read.table(file = paste0(getwd(), '/EEs05_b_AllMetrics.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEs95_b = read.table(file = paste0(getwd(), '/EEs95_b_AllMetrics.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE) 
EEsot_b = read.table(file = paste0(getwd(), '/EEsot_b_AllMetrics.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEsTN05_b = read.table(file = paste0(getwd(), '/EEsTN05_b_All_Add5.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE) 
EEsTNMed_b = read.table(file = paste0(getwd(), '/EEsTNMed_b_All_Add5.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE) 
EEsTN95_b = read.table(file = paste0(getwd(), '/EEsTN95_b_All_Add5.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEs05g_b = read.table(file = paste0(getwd(), '/EEs05g_b_AllMetrics.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEsotg_b = read.table(file = paste0(getwd(), '/EEsotg_b_AllMetrics.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEsLogL_b = read.table(file = paste0(getwd(), '/EEsLogL_b_AllMetrics.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEsNSE_b = read.table(file = paste0(getwd(), '/EEsNSE_b_AllMetrics.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEsLNSE_b = read.table(file = paste0(getwd(), '/EEsLNSE_b_AllMetrics.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEspBias_b = read.table(file = paste0(getwd(), '/EEspBias_b_AllMetrics.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEs05_h = read.table(file = paste0(getwd(), '/EEs05_h_AllMetrics.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEs95_h = read.table(file = paste0(getwd(), '/EEs95_h_AllMetrics.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEsot_h = read.table(file = paste0(getwd(), '/EEsot_h_AllMetrics.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEsTN05_h = read.table(file = paste0(getwd(), '/EEsTN05_h_AllMetrics.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE) 
EEsTNMed_h = read.table(file = paste0(getwd(), '/EEsTNMed_h_AllMetrics.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE) 
EEsTN95_h = read.table(file = paste0(getwd(), '/EEsTN95_h_AllMetrics.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEs05g_h = read.table(file = paste0(getwd(), '/EEs05g_h_AllMetrics.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEsotg_h = read.table(file = paste0(getwd(), '/EEsotg_h_AllMetrics.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEs05gm_b = read.table(file = paste0(getwd(), '/EEs05gm_b_All.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEsotgm_b = read.table(file = paste0(getwd(), '/EEsotgm_b_All.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE)
EEs95m_b = read.table(file = paste0(getwd(), '/EEs95m_b_All.txt'), sep = '\t', header = TRUE, stringsAsFactors = FALSE) 

#Check if EEs have NA values because of sampling scheme errors
if (any(c(any(is.na(EEs05_b)),
any(is.na(EEs95_b)),
any(is.na(EEsot_b)),
any(is.na(EEsTN05_b)),
any(is.na(EEsTN95_b)),
any(is.na(EEsTNMed_b)),
any(is.na(EEs05g_b)),
any(is.na(EEsotg_b)),
any(is.na(EEsLNSE_b)),
any(is.na(EEsNSE_b)),
any(is.na(EEsLogL_b)),
any(is.na(EEspBias_b)),
any(is.na(EEs05_h)),
any(is.na(EEs95_h)),
any(is.na(EEsot_h)),
any(is.na(EEs05g_h)),
any(is.na(EEsotg_h)),
any(is.na(EEsTN05_h)),
any(is.na(EEsTN95_h)),
any(is.na(EEsTNMed_h)),
any(is.na(EEs05gm_b)),
any(is.na(EEsotgm_b)),
any(is.na(EEs95m_b))))){
  print('There are NA Values in EE tables. Please fix.')
}

#Corrections are needed to v102_epc.frootlitr_fcel to correct a numerical issue
EEs05_b$v102_epc.frootlitr_fcel[c(13,19)] = 0
EEsot_b$v102_epc.frootlitr_fcel[c(13,19)] = 0
EEs95_b$v102_epc.frootlitr_fcel[c(13,19)] = 0
EEs05g_b$v102_epc.frootlitr_fcel[c(13,19)] = 0
EEsotg_b$v102_epc.frootlitr_fcel[c(13,19)] = 0
EEsTN05_b$v102_epc.frootlitr_fcel[c(13,19)] = 0
EEsTNMed_b$v102_epc.frootlitr_fcel[c(13,19)] = 0
EEsTN95_b$v102_epc.frootlitr_fcel[c(13,19)] = 0
EEsLogL_b$v102_epc.frootlitr_fcel[c(13,19)] = 0
EEsNSE_b$v102_epc.frootlitr_fcel[c(13,19)] = 0
EEsLNSE_b$v102_epc.frootlitr_fcel[c(13,19)] = 0
EEspBias_b$v102_epc.frootlitr_fcel[c(13,19)] = 0
EEs05gm_b$v102_epc.frootlitr_fcel[c(13,19)] = 0
EEsotgm_b$v102_epc.frootlitr_fcel[c(13,19)] = 0
EEs95m_b$v102_epc.frootlitr_fcel[c(13,19)] = 0

EEs05_h$v102_epc.frootlitr_fcel[c(12*14+seq(1,14,1),14*18+seq(1,14,1))] = 0
EEsot_h$v102_epc.frootlitr_fcel[c(12*14+seq(1,14,1),14*18+seq(1,14,1))] = 0
EEs95_h$v102_epc.frootlitr_fcel[c(12*14+seq(1,14,1),14*18+seq(1,14,1))] = 0
EEs05g_h$v102_epc.frootlitr_fcel[c(12*14+seq(1,14,1),14*18+seq(1,14,1))] = 0
EEsotg_h$v102_epc.frootlitr_fcel[c(12*14+seq(1,14,1),14*18+seq(1,14,1))] = 0
EEsTN05_h$v102_epc.frootlitr_fcel[c(12*14+seq(1,14,1),14*18+seq(1,14,1))] = 0
EEsTNMed_h$v102_epc.frootlitr_fcel[c(12*14+seq(1,14,1),14*18+seq(1,14,1))] = 0
EEsTN95_h$v102_epc.frootlitr_fcel[c(12*14+seq(1,14,1),14*18+seq(1,14,1))] = 0

#Aggregate the EEs for variables that require it----
# Make a vector of the column names that will be aggregated---- 
#Later, they will be removed from the dataframe.
ColsAggregated = c('s8_silt', 's8_sand', 's8_clay', 's108_silt', 's108_sand', 's108_clay', 's9_silt', 's9_sand', 's9_clay', 's109_silt', 's109_sand', 's109_clay',
                   'v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance',
                   'v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance',
                   'v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig', 'v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig',
                   'v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig', 'v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig',
                   's108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v', 's109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v',
                   's108_m', 's8_m', 's109_m', 's9_m',
                   's108_porosity_0', 's8_porosity_0', 's109_porosity_0', 's9_porosity_0',
                   'v102_epc.topt', 'v102_epc.tmax', 'v3_epc.topt', 'v3_epc.tmax',
                   'v102_epc.leaf_cn', 'v102_epc.leaflitr_cn', 'v3_epc.leaf_cn', 'v3_epc.leaflitr_cn')
#Indicator key for the aggregated name.
ColsAggregated_key = c('s8_SoilTexture', 's8_SoilTexture', 's8_SoilTexture', 's108_SoilTexture', 's108_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's9_SoilTexture', 's9_SoilTexture', 's109_SoilTexture', 's109_SoilTexture', 's109_SoilTexture',
                      'v102_K_All', 'v102_K_All', 'v102_K_All',
                      'v102_PAR_All', 'v102_PAR_All', 'v102_PAR_All',
                      'v102_epc.frootlitr_All', 'v102_epc.frootlitr_All', 'v102_epc.frootlitr_All', 'v3_epc.frootlitr_All', 'v3_epc.frootlitr_All', 'v3_epc.frootlitr_All',
                      'v102_epc.leaflitr_All', 'v102_epc.leaflitr_All', 'v102_epc.leaflitr_All', 'v3_epc.leaflitr_All', 'v3_epc.leaflitr_All', 'v3_epc.leaflitr_All',
                      'Soil8_Ksat', 'Soil8_Ksat', 'Soil8_Ksat', 'Soil8_Ksat', 'Soil9_Ksat', 'Soil9_Ksat', 'Soil9_Ksat', 'Soil9_Ksat',
                      'Soil8_m', 'Soil8_m', 'Soil9_m', 'Soil9_m',
                      'Soil8_porosity_0', 'Soil8_porosity_0', 'Soil9_porosity_0', 'Soil9_porosity_0',
                      'v102_Temp', 'v102_Temp', 'v3_Temp', 'v3_Temp',
                      'v102_epc.LeafLitrCN', 'v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

# Sum constrained variables: 10 new averaged/max variables----
#  Soil Texture----
# 's8_silt' 's8_sand' 's8_clay'
# 's108_silt' 's108_sand' 's108_clay'
# 's9_silt' 's9_sand' 's9_clay'
# 's109_silt' 's109_sand' 's109_clay'
EEs05_b[,seq(ncol(EEs05_b)+1,ncol(EEs05_b)+4,1)] = AggregateEEs(EEs = EEs05_b, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEs05_b)[(ncol(EEs05_b)-3):ncol(EEs05_b)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEs95_b[,seq(ncol(EEs95_b)+1,ncol(EEs95_b)+4,1)] = AggregateEEs(EEs = EEs95_b, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEs95_b)[(ncol(EEs95_b)-3):ncol(EEs95_b)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEsot_b[,seq(ncol(EEsot_b)+1,ncol(EEsot_b)+4,1)] = AggregateEEs(EEs = EEsot_b, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEsot_b)[(ncol(EEsot_b)-3):ncol(EEsot_b)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEs05g_b[,seq(ncol(EEs05g_b)+1,ncol(EEs05g_b)+4,1)] = AggregateEEs(EEs = EEs05g_b, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEs05g_b)[(ncol(EEs05g_b)-3):ncol(EEs05g_b)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEsotg_b[,seq(ncol(EEsotg_b)+1,ncol(EEsotg_b)+4,1)] = AggregateEEs(EEs = EEsotg_b, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEsotg_b)[(ncol(EEsotg_b)-3):ncol(EEsotg_b)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEsLNSE_b[,seq(ncol(EEsLNSE_b)+1,ncol(EEsLNSE_b)+4,1)] = AggregateEEs(EEs = EEsLNSE_b, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEsLNSE_b)[(ncol(EEsLNSE_b)-3):ncol(EEsLNSE_b)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEsNSE_b[,seq(ncol(EEsNSE_b)+1,ncol(EEsNSE_b)+4,1)] = AggregateEEs(EEs = EEsNSE_b, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEsNSE_b)[(ncol(EEsNSE_b)-3):ncol(EEsNSE_b)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEsLogL_b[,seq(ncol(EEsLogL_b)+1,ncol(EEsLogL_b)+4,1)] = AggregateEEs(EEs = EEsLogL_b, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEsLogL_b)[(ncol(EEsLogL_b)-3):ncol(EEsLogL_b)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEspBias_b[,seq(ncol(EEspBias_b)+1,ncol(EEspBias_b)+4,1)] = AggregateEEs(EEs = EEspBias_b, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEspBias_b)[(ncol(EEspBias_b)-3):ncol(EEspBias_b)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEsTN05_b[,seq(ncol(EEsTN05_b)+1,ncol(EEsTN05_b)+4,1)] = AggregateEEs(EEs = EEsTN05_b, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEsTN05_b)[(ncol(EEsTN05_b)-3):ncol(EEsTN05_b)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEsTN95_b[,seq(ncol(EEsTN95_b)+1,ncol(EEsTN95_b)+4,1)] = AggregateEEs(EEs = EEsTN95_b, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEsTN95_b)[(ncol(EEsTN95_b)-3):ncol(EEsTN95_b)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEsTNMed_b[,seq(ncol(EEsTNMed_b)+1,ncol(EEsTNMed_b)+4,1)] = AggregateEEs(EEs = EEsTNMed_b, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEsTNMed_b)[(ncol(EEsTNMed_b)-3):ncol(EEsTNMed_b)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEs05gm_b[,seq(ncol(EEs05gm_b)+1,ncol(EEs05gm_b)+4,1)] = AggregateEEs(EEs = EEs05gm_b, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEs05gm_b)[(ncol(EEs05gm_b)-3):ncol(EEs05gm_b)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEs95m_b[,seq(ncol(EEs95m_b)+1,ncol(EEs95m_b)+4,1)] = AggregateEEs(EEs = EEs95m_b, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEs95m_b)[(ncol(EEs95m_b)-3):ncol(EEs95m_b)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEsotgm_b[,seq(ncol(EEsotgm_b)+1,ncol(EEsotgm_b)+4,1)] = AggregateEEs(EEs = EEsotgm_b, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEsotgm_b)[(ncol(EEsotgm_b)-3):ncol(EEsotgm_b)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEs05_h[,seq(ncol(EEs05_h)+1,ncol(EEs05_h)+4,1)] = AggregateEEs(EEs = EEs05_h, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEs05_h)[(ncol(EEs05_h)-3):ncol(EEs05_h)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEs95_h[,seq(ncol(EEs95_h)+1,ncol(EEs95_h)+4,1)] = AggregateEEs(EEs = EEs95_h, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEs95_h)[(ncol(EEs95_h)-3):ncol(EEs95_h)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEsot_h[,seq(ncol(EEsot_h)+1,ncol(EEsot_h)+4,1)] = AggregateEEs(EEs = EEsot_h, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEsot_h)[(ncol(EEsot_h)-3):ncol(EEsot_h)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEsotg_h[,seq(ncol(EEsotg_h)+1,ncol(EEsotg_h)+4,1)] = AggregateEEs(EEs = EEsotg_h, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEsotg_h)[(ncol(EEsotg_h)-3):ncol(EEsotg_h)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEs05g_h[,seq(ncol(EEs05g_h)+1,ncol(EEs05g_h)+4,1)] = AggregateEEs(EEs = EEs05g_h, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEs05g_h)[(ncol(EEs05g_h)-3):ncol(EEs05g_h)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEsTN05_h[,seq(ncol(EEsTN05_h)+1,ncol(EEsTN05_h)+4,1)] = AggregateEEs(EEs = EEsTN05_h, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEsTN05_h)[(ncol(EEsTN05_h)-3):ncol(EEsTN05_h)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEsTN95_h[,seq(ncol(EEsTN95_h)+1,ncol(EEsTN95_h)+4,1)] = AggregateEEs(EEs = EEsTN95_h, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEsTN95_h)[(ncol(EEsTN95_h)-3):ncol(EEsTN95_h)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

EEsTNMed_h[,seq(ncol(EEsTNMed_h)+1,ncol(EEsTNMed_h)+4,1)] = AggregateEEs(EEs = EEsTNMed_h, ColNames = as.matrix(rbind(c('s8_silt', 's8_sand', 's8_clay'), c('s108_silt', 's108_sand', 's108_clay'), c('s9_silt', 's9_sand', 's9_clay'), c('s109_silt', 's109_sand', 's109_clay'))), FUN = meanabs)
colnames(EEsTNMed_h)[(ncol(EEsTNMed_h)-3):ncol(EEsTNMed_h)] = c('s8_SoilTexture', 's108_SoilTexture', 's9_SoilTexture', 's109_SoilTexture')

#  K Radiation----
# 'v102_K_absorptance' 'v102_K_reflectance' 'v102_K_transmittance'
EEs05_b[,seq(ncol(EEs05_b)+1,ncol(EEs05_b)+1,1)] = AggregateEEs(EEs = EEs05_b, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEs05_b)[(ncol(EEs05_b)-0):ncol(EEs05_b)] = 'v102_K_All'

EEs95_b[,seq(ncol(EEs95_b)+1,ncol(EEs95_b)+1,1)] = AggregateEEs(EEs = EEs95_b, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEs95_b)[(ncol(EEs95_b)-0):ncol(EEs95_b)] = 'v102_K_All'

EEsot_b[,seq(ncol(EEsot_b)+1,ncol(EEsot_b)+1,1)] = AggregateEEs(EEs = EEsot_b, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEsot_b)[(ncol(EEsot_b)-0):ncol(EEsot_b)] = 'v102_K_All'

EEsotg_b[,seq(ncol(EEsotg_b)+1,ncol(EEsotg_b)+1,1)] = AggregateEEs(EEs = EEsotg_b, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEsotg_b)[(ncol(EEsotg_b)-0):ncol(EEsotg_b)] = 'v102_K_All'

EEs05g_b[,seq(ncol(EEs05g_b)+1,ncol(EEs05g_b)+1,1)] = AggregateEEs(EEs = EEs05g_b, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEs05g_b)[(ncol(EEs05g_b)-0):ncol(EEs05g_b)] = 'v102_K_All'

EEsLNSE_b[,seq(ncol(EEsLNSE_b)+1,ncol(EEsLNSE_b)+1,1)] = AggregateEEs(EEs = EEsLNSE_b, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEsLNSE_b)[(ncol(EEsLNSE_b)-0):ncol(EEsLNSE_b)] = 'v102_K_All'

EEsNSE_b[,seq(ncol(EEsNSE_b)+1,ncol(EEsNSE_b)+1,1)] = AggregateEEs(EEs = EEsNSE_b, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEsNSE_b)[(ncol(EEsNSE_b)-0):ncol(EEsNSE_b)] = 'v102_K_All'

EEspBias_b[,seq(ncol(EEspBias_b)+1,ncol(EEspBias_b)+1,1)] = AggregateEEs(EEs = EEspBias_b, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEspBias_b)[(ncol(EEspBias_b)-0):ncol(EEspBias_b)] = 'v102_K_All'

EEsLogL_b[,seq(ncol(EEsLogL_b)+1,ncol(EEsLogL_b)+1,1)] = AggregateEEs(EEs = EEsLogL_b, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEsLogL_b)[(ncol(EEsLogL_b)-0):ncol(EEsLogL_b)] = 'v102_K_All'

EEsTN05_b[,seq(ncol(EEsTN05_b)+1,ncol(EEsTN05_b)+1,1)] = AggregateEEs(EEs = EEsTN05_b, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEsTN05_b)[(ncol(EEsTN05_b)-0):ncol(EEsTN05_b)] = 'v102_K_All'

EEsTN95_b[,seq(ncol(EEsTN95_b)+1,ncol(EEsTN95_b)+1,1)] = AggregateEEs(EEs = EEsTN95_b, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEsTN95_b)[(ncol(EEsTN95_b)-0):ncol(EEsTN95_b)] = 'v102_K_All'

EEsTNMed_b[,seq(ncol(EEsTNMed_b)+1,ncol(EEsTNMed_b)+1,1)] = AggregateEEs(EEs = EEsTNMed_b, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEsTNMed_b)[(ncol(EEsTNMed_b)-0):ncol(EEsTNMed_b)] = 'v102_K_All'

EEs05gm_b[,seq(ncol(EEs05gm_b)+1,ncol(EEs05gm_b)+1,1)] = AggregateEEs(EEs = EEs05gm_b, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEs05gm_b)[(ncol(EEs05gm_b)-0):ncol(EEs05gm_b)] = 'v102_K_All'

EEs95m_b[,seq(ncol(EEs95m_b)+1,ncol(EEs95m_b)+1,1)] = AggregateEEs(EEs = EEs95m_b, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEs95m_b)[(ncol(EEs95m_b)-0):ncol(EEs95m_b)] = 'v102_K_All'

EEsotgm_b[,seq(ncol(EEsotgm_b)+1,ncol(EEsotgm_b)+1,1)] = AggregateEEs(EEs = EEsotgm_b, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEsotgm_b)[(ncol(EEsotgm_b)-0):ncol(EEsotgm_b)] = 'v102_K_All'

EEs05_h[,seq(ncol(EEs05_h)+1,ncol(EEs05_h)+1,1)] = AggregateEEs(EEs = EEs05_h, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEs05_h)[(ncol(EEs05_h)-0):ncol(EEs05_h)] = 'v102_K_All'

EEs95_h[,seq(ncol(EEs95_h)+1,ncol(EEs95_h)+1,1)] = AggregateEEs(EEs = EEs95_h, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEs95_h)[(ncol(EEs95_h)-0):ncol(EEs95_h)] = 'v102_K_All'

EEsot_h[,seq(ncol(EEsot_h)+1,ncol(EEsot_h)+1,1)] = AggregateEEs(EEs = EEsot_h, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEsot_h)[(ncol(EEsot_h)-0):ncol(EEsot_h)] = 'v102_K_All'

EEsotg_h[,seq(ncol(EEsotg_h)+1,ncol(EEsotg_h)+1,1)] = AggregateEEs(EEs = EEsotg_h, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEsotg_h)[(ncol(EEsotg_h)-0):ncol(EEsotg_h)] = 'v102_K_All'

EEs05g_h[,seq(ncol(EEs05g_h)+1,ncol(EEs05g_h)+1,1)] = AggregateEEs(EEs = EEs05g_h, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEs05g_h)[(ncol(EEs05g_h)-0):ncol(EEs05g_h)] = 'v102_K_All'

EEsTN05_h[,seq(ncol(EEsTN05_h)+1,ncol(EEsTN05_h)+1,1)] = AggregateEEs(EEs = EEsTN05_h, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEsTN05_h)[(ncol(EEsTN05_h)-0):ncol(EEsTN05_h)] = 'v102_K_All'

EEsTN95_h[,seq(ncol(EEsTN95_h)+1,ncol(EEsTN95_h)+1,1)] = AggregateEEs(EEs = EEsTN95_h, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEsTN95_h)[(ncol(EEsTN95_h)-0):ncol(EEsTN95_h)] = 'v102_K_All'

EEsTNMed_h[,seq(ncol(EEsTNMed_h)+1,ncol(EEsTNMed_h)+1,1)] = AggregateEEs(EEs = EEsTNMed_h, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), FUN = meanabs)
colnames(EEsTNMed_h)[(ncol(EEsTNMed_h)-0):ncol(EEsTNMed_h)] = 'v102_K_All'

#  PAR Radiation----
# 'v102_PAR_absorptance' 'v102_PAR_reflectance' 'v102_PAR_transmittance'
EEs05_b[,seq(ncol(EEs05_b)+1,ncol(EEs05_b)+1,1)] = AggregateEEs(EEs = EEs05_b, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEs05_b)[(ncol(EEs05_b)-0):ncol(EEs05_b)] = 'v102_PAR_All'

EEs95_b[,seq(ncol(EEs95_b)+1,ncol(EEs95_b)+1,1)] = AggregateEEs(EEs = EEs95_b, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEs95_b)[(ncol(EEs95_b)-0):ncol(EEs95_b)] = 'v102_PAR_All'

EEsot_b[,seq(ncol(EEsot_b)+1,ncol(EEsot_b)+1,1)] = AggregateEEs(EEs = EEsot_b, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEsot_b)[(ncol(EEsot_b)-0):ncol(EEsot_b)] = 'v102_PAR_All'

EEs05g_b[,seq(ncol(EEs05g_b)+1,ncol(EEs05g_b)+1,1)] = AggregateEEs(EEs = EEs05g_b, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEs05g_b)[(ncol(EEs05g_b)-0):ncol(EEs05g_b)] = 'v102_PAR_All'

EEsotg_b[,seq(ncol(EEsotg_b)+1,ncol(EEsotg_b)+1,1)] = AggregateEEs(EEs = EEsotg_b, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEsotg_b)[(ncol(EEsotg_b)-0):ncol(EEsotg_b)] = 'v102_PAR_All'

EEsLNSE_b[,seq(ncol(EEsLNSE_b)+1,ncol(EEsLNSE_b)+1,1)] = AggregateEEs(EEs = EEsLNSE_b, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEsLNSE_b)[(ncol(EEsLNSE_b)-0):ncol(EEsLNSE_b)] = 'v102_PAR_All'

EEsNSE_b[,seq(ncol(EEsNSE_b)+1,ncol(EEsNSE_b)+1,1)] = AggregateEEs(EEs = EEsNSE_b, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEsNSE_b)[(ncol(EEsNSE_b)-0):ncol(EEsNSE_b)] = 'v102_PAR_All'

EEspBias_b[,seq(ncol(EEspBias_b)+1,ncol(EEspBias_b)+1,1)] = AggregateEEs(EEs = EEspBias_b, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEspBias_b)[(ncol(EEspBias_b)-0):ncol(EEspBias_b)] = 'v102_PAR_All'

EEsLogL_b[,seq(ncol(EEsLogL_b)+1,ncol(EEsLogL_b)+1,1)] = AggregateEEs(EEs = EEsLogL_b, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEsLogL_b)[(ncol(EEsLogL_b)-0):ncol(EEsLogL_b)] = 'v102_PAR_All'

EEsTN05_b[,seq(ncol(EEsTN05_b)+1,ncol(EEsTN05_b)+1,1)] = AggregateEEs(EEs = EEsTN05_b, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEsTN05_b)[(ncol(EEsTN05_b)-0):ncol(EEsTN05_b)] = 'v102_PAR_All'

EEsTN95_b[,seq(ncol(EEsTN95_b)+1,ncol(EEsTN95_b)+1,1)] = AggregateEEs(EEs = EEsTN95_b, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEsTN95_b)[(ncol(EEsTN95_b)-0):ncol(EEsTN95_b)] = 'v102_PAR_All'

EEsTNMed_b[,seq(ncol(EEsTNMed_b)+1,ncol(EEsTNMed_b)+1,1)] = AggregateEEs(EEs = EEsTNMed_b, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEsTNMed_b)[(ncol(EEsTNMed_b)-0):ncol(EEsTNMed_b)] = 'v102_PAR_All'

EEs05gm_b[,seq(ncol(EEs05gm_b)+1,ncol(EEs05gm_b)+1,1)] = AggregateEEs(EEs = EEs05gm_b, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEs05gm_b)[(ncol(EEs05gm_b)-0):ncol(EEs05gm_b)] = 'v102_PAR_All'

EEs95m_b[,seq(ncol(EEs95m_b)+1,ncol(EEs95m_b)+1,1)] = AggregateEEs(EEs = EEs95m_b, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEs95m_b)[(ncol(EEs95m_b)-0):ncol(EEs95m_b)] = 'v102_PAR_All'

EEsotgm_b[,seq(ncol(EEsotgm_b)+1,ncol(EEsotgm_b)+1,1)] = AggregateEEs(EEs = EEsotgm_b, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEsotgm_b)[(ncol(EEsotgm_b)-0):ncol(EEsotgm_b)] = 'v102_PAR_All'

EEs05_h[,seq(ncol(EEs05_h)+1,ncol(EEs05_h)+1,1)] = AggregateEEs(EEs = EEs05_h, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEs05_h)[(ncol(EEs05_h)-0):ncol(EEs05_h)] = 'v102_PAR_All'

EEs95_h[,seq(ncol(EEs95_h)+1,ncol(EEs95_h)+1,1)] = AggregateEEs(EEs = EEs95_h, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEs95_h)[(ncol(EEs95_h)-0):ncol(EEs95_h)] = 'v102_PAR_All'

EEsot_h[,seq(ncol(EEsot_h)+1,ncol(EEsot_h)+1,1)] = AggregateEEs(EEs = EEsot_h, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEsot_h)[(ncol(EEsot_h)-0):ncol(EEsot_h)] = 'v102_PAR_All'

EEs05g_h[,seq(ncol(EEs05g_h)+1,ncol(EEs05g_h)+1,1)] = AggregateEEs(EEs = EEs05g_h, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEs05g_h)[(ncol(EEs05g_h)-0):ncol(EEs05g_h)] = 'v102_PAR_All'

EEsotg_h[,seq(ncol(EEsotg_h)+1,ncol(EEsotg_h)+1,1)] = AggregateEEs(EEs = EEsotg_h, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEsotg_h)[(ncol(EEsotg_h)-0):ncol(EEsotg_h)] = 'v102_PAR_All'

EEsTN05_h[,seq(ncol(EEsTN05_h)+1,ncol(EEsTN05_h)+1,1)] = AggregateEEs(EEs = EEsTN05_h, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEsTN05_h)[(ncol(EEsTN05_h)-0):ncol(EEsTN05_h)] = 'v102_PAR_All'

EEsTN95_h[,seq(ncol(EEsTN95_h)+1,ncol(EEsTN95_h)+1,1)] = AggregateEEs(EEs = EEsTN95_h, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEsTN95_h)[(ncol(EEsTN95_h)-0):ncol(EEsTN95_h)] = 'v102_PAR_All'

EEsTNMed_h[,seq(ncol(EEsTNMed_h)+1,ncol(EEsTNMed_h)+1,1)] = AggregateEEs(EEs = EEsTNMed_h, ColNames = as.matrix(rbind(c('v102_PAR_absorptance', 'v102_PAR_reflectance', 'v102_PAR_transmittance'))), FUN = meanabs)
colnames(EEsTNMed_h)[(ncol(EEsTNMed_h)-0):ncol(EEsTNMed_h)] = 'v102_PAR_All'

#  Fine Root to Litter----
# 'v102_epc.frootlitr_fcel' 'v102_epc.frootlitr_flab' 'v102_epc.frootlitr_flig'
# 'v3_epc.frootlitr_fcel' 'v3_epc.frootlitr_flab' 'v3_epc.frootlitr_flig'
EEs05_b[,seq(ncol(EEs05_b)+1,ncol(EEs05_b)+2,1)] = AggregateEEs(EEs = EEs05_b, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEs05_b)[(ncol(EEs05_b)-1):ncol(EEs05_b)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEs95_b[,seq(ncol(EEs95_b)+1,ncol(EEs95_b)+2,1)] = AggregateEEs(EEs = EEs95_b, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEs95_b)[(ncol(EEs95_b)-1):ncol(EEs95_b)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEsot_b[,seq(ncol(EEsot_b)+1,ncol(EEsot_b)+2,1)] = AggregateEEs(EEs = EEsot_b, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEsot_b)[(ncol(EEsot_b)-1):ncol(EEsot_b)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEsotg_b[,seq(ncol(EEsotg_b)+1,ncol(EEsotg_b)+2,1)] = AggregateEEs(EEs = EEsotg_b, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEsotg_b)[(ncol(EEsotg_b)-1):ncol(EEsotg_b)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEs05g_b[,seq(ncol(EEs05g_b)+1,ncol(EEs05g_b)+2,1)] = AggregateEEs(EEs = EEs05g_b, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEs05g_b)[(ncol(EEs05g_b)-1):ncol(EEs05g_b)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEsLNSE_b[,seq(ncol(EEsLNSE_b)+1,ncol(EEsLNSE_b)+2,1)] = AggregateEEs(EEs = EEsLNSE_b, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEsLNSE_b)[(ncol(EEsLNSE_b)-1):ncol(EEsLNSE_b)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEsNSE_b[,seq(ncol(EEsNSE_b)+1,ncol(EEsNSE_b)+2,1)] = AggregateEEs(EEs = EEsNSE_b, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEsNSE_b)[(ncol(EEsNSE_b)-1):ncol(EEsNSE_b)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEsLogL_b[,seq(ncol(EEsLogL_b)+1,ncol(EEsLogL_b)+2,1)] = AggregateEEs(EEs = EEsLogL_b, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEsLogL_b)[(ncol(EEsLogL_b)-1):ncol(EEsLogL_b)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEspBias_b[,seq(ncol(EEspBias_b)+1,ncol(EEspBias_b)+2,1)] = AggregateEEs(EEs = EEspBias_b, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEspBias_b)[(ncol(EEspBias_b)-1):ncol(EEspBias_b)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEsTN05_b[,seq(ncol(EEsTN05_b)+1,ncol(EEsTN05_b)+2,1)] = AggregateEEs(EEs = EEsTN05_b, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEsTN05_b)[(ncol(EEsTN05_b)-1):ncol(EEsTN05_b)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEsTN95_b[,seq(ncol(EEsTN95_b)+1,ncol(EEsTN95_b)+2,1)] = AggregateEEs(EEs = EEsTN95_b, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEsTN95_b)[(ncol(EEsTN95_b)-1):ncol(EEsTN95_b)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEsTNMed_b[,seq(ncol(EEsTNMed_b)+1,ncol(EEsTNMed_b)+2,1)] = AggregateEEs(EEs = EEsTNMed_b, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEsTNMed_b)[(ncol(EEsTNMed_b)-1):ncol(EEsTNMed_b)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEs05gm_b[,seq(ncol(EEs05gm_b)+1,ncol(EEs05gm_b)+2,1)] = AggregateEEs(EEs = EEs05gm_b, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEs05gm_b)[(ncol(EEs05gm_b)-1):ncol(EEs05gm_b)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEs95m_b[,seq(ncol(EEs95m_b)+1,ncol(EEs95m_b)+2,1)] = AggregateEEs(EEs = EEs95m_b, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEs95m_b)[(ncol(EEs95m_b)-1):ncol(EEs95m_b)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEsotgm_b[,seq(ncol(EEsotgm_b)+1,ncol(EEsotgm_b)+2,1)] = AggregateEEs(EEs = EEsotgm_b, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEsotgm_b)[(ncol(EEsotgm_b)-1):ncol(EEsotgm_b)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEs05_h[,seq(ncol(EEs05_h)+1,ncol(EEs05_h)+2,1)] = AggregateEEs(EEs = EEs05_h, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEs05_h)[(ncol(EEs05_h)-1):ncol(EEs05_h)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEs95_h[,seq(ncol(EEs95_h)+1,ncol(EEs95_h)+2,1)] = AggregateEEs(EEs = EEs95_h, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEs95_h)[(ncol(EEs95_h)-1):ncol(EEs95_h)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEsot_h[,seq(ncol(EEsot_h)+1,ncol(EEsot_h)+2,1)] = AggregateEEs(EEs = EEsot_h, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEsot_h)[(ncol(EEsot_h)-1):ncol(EEsot_h)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEs05g_h[,seq(ncol(EEs05g_h)+1,ncol(EEs05g_h)+2,1)] = AggregateEEs(EEs = EEs05g_h, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEs05g_h)[(ncol(EEs05g_h)-1):ncol(EEs05g_h)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEsotg_h[,seq(ncol(EEsotg_h)+1,ncol(EEsotg_h)+2,1)] = AggregateEEs(EEs = EEsotg_h, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEsotg_h)[(ncol(EEsotg_h)-1):ncol(EEsotg_h)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEsTN05_h[,seq(ncol(EEsTN05_h)+1,ncol(EEsTN05_h)+2,1)] = AggregateEEs(EEs = EEsTN05_h, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEsTN05_h)[(ncol(EEsTN05_h)-1):ncol(EEsTN05_h)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEsTN95_h[,seq(ncol(EEsTN95_h)+1,ncol(EEsTN95_h)+2,1)] = AggregateEEs(EEs = EEsTN95_h, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEsTN95_h)[(ncol(EEsTN95_h)-1):ncol(EEsTN95_h)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

EEsTNMed_h[,seq(ncol(EEsTNMed_h)+1,ncol(EEsTNMed_h)+2,1)] = AggregateEEs(EEs = EEsTNMed_h, ColNames = as.matrix(rbind(c('v102_epc.frootlitr_fcel', 'v102_epc.frootlitr_flab', 'v102_epc.frootlitr_flig'), c('v3_epc.frootlitr_fcel', 'v3_epc.frootlitr_flab', 'v3_epc.frootlitr_flig'))), FUN = meanabs)
colnames(EEsTNMed_h)[(ncol(EEsTNMed_h)-1):ncol(EEsTNMed_h)] = c('v102_epc.frootlitr_All', 'v3_epc.frootlitr_All')

#  Leaf to Litter----
# 'v102_epc.leaflitr_fcel' 'v102_epc.leaflitr_flab' 'v102_epc.leaflitr_flig'
# 'v3_epc.leaflitr_fcel' 'v3_epc.leaflitr_flab' 'v3_epc.leaflitr_flig'
EEs05_b[,seq(ncol(EEs05_b)+1,ncol(EEs05_b)+2,1)] = AggregateEEs(EEs = EEs05_b, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEs05_b)[(ncol(EEs05_b)-1):ncol(EEs05_b)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEs95_b[,seq(ncol(EEs95_b)+1,ncol(EEs95_b)+2,1)] = AggregateEEs(EEs = EEs95_b, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEs95_b)[(ncol(EEs95_b)-1):ncol(EEs95_b)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEsot_b[,seq(ncol(EEsot_b)+1,ncol(EEsot_b)+2,1)] = AggregateEEs(EEs = EEsot_b, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEsot_b)[(ncol(EEsot_b)-1):ncol(EEsot_b)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEsotg_b[,seq(ncol(EEsotg_b)+1,ncol(EEsotg_b)+2,1)] = AggregateEEs(EEs = EEsotg_b, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEsotg_b)[(ncol(EEsotg_b)-1):ncol(EEsotg_b)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEs05g_b[,seq(ncol(EEs05g_b)+1,ncol(EEs05g_b)+2,1)] = AggregateEEs(EEs = EEs05g_b, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEs05g_b)[(ncol(EEs05g_b)-1):ncol(EEs05g_b)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEsLNSE_b[,seq(ncol(EEsLNSE_b)+1,ncol(EEsLNSE_b)+2,1)] = AggregateEEs(EEs = EEsLNSE_b, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEsLNSE_b)[(ncol(EEsLNSE_b)-1):ncol(EEsLNSE_b)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEsNSE_b[,seq(ncol(EEsNSE_b)+1,ncol(EEsNSE_b)+2,1)] = AggregateEEs(EEs = EEsNSE_b, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEsNSE_b)[(ncol(EEsNSE_b)-1):ncol(EEsNSE_b)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEspBias_b[,seq(ncol(EEspBias_b)+1,ncol(EEspBias_b)+2,1)] = AggregateEEs(EEs = EEspBias_b, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEspBias_b)[(ncol(EEspBias_b)-1):ncol(EEspBias_b)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEsLogL_b[,seq(ncol(EEsLogL_b)+1,ncol(EEsLogL_b)+2,1)] = AggregateEEs(EEs = EEsLogL_b, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEsLogL_b)[(ncol(EEsLogL_b)-1):ncol(EEsLogL_b)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEsTN05_b[,seq(ncol(EEsTN05_b)+1,ncol(EEsTN05_b)+2,1)] = AggregateEEs(EEs = EEsTN05_b, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEsTN05_b)[(ncol(EEsTN05_b)-1):ncol(EEsTN05_b)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEsTN95_b[,seq(ncol(EEsTN95_b)+1,ncol(EEsTN95_b)+2,1)] = AggregateEEs(EEs = EEsTN95_b, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEsTN95_b)[(ncol(EEsTN95_b)-1):ncol(EEsTN95_b)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEsTNMed_b[,seq(ncol(EEsTNMed_b)+1,ncol(EEsTNMed_b)+2,1)] = AggregateEEs(EEs = EEsTNMed_b, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEsTNMed_b)[(ncol(EEsTNMed_b)-1):ncol(EEsTNMed_b)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEs05gm_b[,seq(ncol(EEs05gm_b)+1,ncol(EEs05gm_b)+2,1)] = AggregateEEs(EEs = EEs05gm_b, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEs05gm_b)[(ncol(EEs05gm_b)-1):ncol(EEs05gm_b)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEs95m_b[,seq(ncol(EEs95m_b)+1,ncol(EEs95m_b)+2,1)] = AggregateEEs(EEs = EEs95m_b, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEs95m_b)[(ncol(EEs95m_b)-1):ncol(EEs95m_b)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEsotgm_b[,seq(ncol(EEsotgm_b)+1,ncol(EEsotgm_b)+2,1)] = AggregateEEs(EEs = EEsotgm_b, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEsotgm_b)[(ncol(EEsotgm_b)-1):ncol(EEsotgm_b)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEs05_h[,seq(ncol(EEs05_h)+1,ncol(EEs05_h)+2,1)] = AggregateEEs(EEs = EEs05_h, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEs05_h)[(ncol(EEs05_h)-1):ncol(EEs05_h)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEs95_h[,seq(ncol(EEs95_h)+1,ncol(EEs95_h)+2,1)] = AggregateEEs(EEs = EEs95_h, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEs95_h)[(ncol(EEs95_h)-1):ncol(EEs95_h)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEsot_h[,seq(ncol(EEsot_h)+1,ncol(EEsot_h)+2,1)] = AggregateEEs(EEs = EEsot_h, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEsot_h)[(ncol(EEsot_h)-1):ncol(EEsot_h)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEsotg_h[,seq(ncol(EEsotg_h)+1,ncol(EEsotg_h)+2,1)] = AggregateEEs(EEs = EEsotg_h, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEsotg_h)[(ncol(EEsotg_h)-1):ncol(EEsotg_h)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEs05g_h[,seq(ncol(EEs05g_h)+1,ncol(EEs05g_h)+2,1)] = AggregateEEs(EEs = EEs05g_h, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEs05g_h)[(ncol(EEs05g_h)-1):ncol(EEs05g_h)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEsTN05_h[,seq(ncol(EEsTN05_h)+1,ncol(EEsTN05_h)+2,1)] = AggregateEEs(EEs = EEsTN05_h, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEsTN05_h)[(ncol(EEsTN05_h)-1):ncol(EEsTN05_h)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEsTN95_h[,seq(ncol(EEsTN95_h)+1,ncol(EEsTN95_h)+2,1)] = AggregateEEs(EEs = EEsTN95_h, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEsTN95_h)[(ncol(EEsTN95_h)-1):ncol(EEsTN95_h)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

EEsTNMed_h[,seq(ncol(EEsTNMed_h)+1,ncol(EEsTNMed_h)+2,1)] = AggregateEEs(EEs = EEsTNMed_h, ColNames = as.matrix(rbind(c('v102_epc.leaflitr_fcel', 'v102_epc.leaflitr_flab', 'v102_epc.leaflitr_flig'), c('v3_epc.leaflitr_fcel', 'v3_epc.leaflitr_flab', 'v3_epc.leaflitr_flig'))), FUN = meanabs)
colnames(EEsTNMed_h)[(ncol(EEsTNMed_h)-1):ncol(EEsTNMed_h)] = c('v102_epc.leaflitr_All', 'v3_epc.leaflitr_All')

# Inequality relations: 10 new averaged/max variables----
#  Soil Sat. Cond.----
# 's109_Ksat_0' 's9_Ksat_0' 's109_Ksat_0_v' 's9_Ksat_0_v'
# 's108_Ksat_0' 's8_Ksat_0' 's108_Ksat_0_v' 's8_Ksat_0_v'
EEs05_b[,seq(ncol(EEs05_b)+1,ncol(EEs05_b)+2,1)] = AggregateEEs(EEs = EEs05_b, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEs05_b)[(ncol(EEs05_b)-1):ncol(EEs05_b)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEs95_b[,seq(ncol(EEs95_b)+1,ncol(EEs95_b)+2,1)] = AggregateEEs(EEs = EEs95_b, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEs95_b)[(ncol(EEs95_b)-1):ncol(EEs95_b)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEsot_b[,seq(ncol(EEsot_b)+1,ncol(EEsot_b)+2,1)] = AggregateEEs(EEs = EEsot_b, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEsot_b)[(ncol(EEsot_b)-1):ncol(EEsot_b)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEs05g_b[,seq(ncol(EEs05g_b)+1,ncol(EEs05g_b)+2,1)] = AggregateEEs(EEs = EEs05g_b, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEs05g_b)[(ncol(EEs05g_b)-1):ncol(EEs05g_b)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEsotg_b[,seq(ncol(EEsotg_b)+1,ncol(EEsotg_b)+2,1)] = AggregateEEs(EEs = EEsotg_b, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEsotg_b)[(ncol(EEsotg_b)-1):ncol(EEsotg_b)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEsLNSE_b[,seq(ncol(EEsLNSE_b)+1,ncol(EEsLNSE_b)+2,1)] = AggregateEEs(EEs = EEsLNSE_b, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEsLNSE_b)[(ncol(EEsLNSE_b)-1):ncol(EEsLNSE_b)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEsNSE_b[,seq(ncol(EEsNSE_b)+1,ncol(EEsNSE_b)+2,1)] = AggregateEEs(EEs = EEsNSE_b, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEsNSE_b)[(ncol(EEsNSE_b)-1):ncol(EEsNSE_b)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEspBias_b[,seq(ncol(EEspBias_b)+1,ncol(EEspBias_b)+2,1)] = AggregateEEs(EEs = EEspBias_b, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEspBias_b)[(ncol(EEspBias_b)-1):ncol(EEspBias_b)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEsLogL_b[,seq(ncol(EEsLogL_b)+1,ncol(EEsLogL_b)+2,1)] = AggregateEEs(EEs = EEsLogL_b, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEsLogL_b)[(ncol(EEsLogL_b)-1):ncol(EEsLogL_b)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEsTN05_b[,seq(ncol(EEsTN05_b)+1,ncol(EEsTN05_b)+2,1)] = AggregateEEs(EEs = EEsTN05_b, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEsTN05_b)[(ncol(EEsTN05_b)-1):ncol(EEsTN05_b)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEsTN95_b[,seq(ncol(EEsTN95_b)+1,ncol(EEsTN95_b)+2,1)] = AggregateEEs(EEs = EEsTN95_b, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEsTN95_b)[(ncol(EEsTN95_b)-1):ncol(EEsTN95_b)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEsTNMed_b[,seq(ncol(EEsTNMed_b)+1,ncol(EEsTNMed_b)+2,1)] = AggregateEEs(EEs = EEsTNMed_b, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEsTNMed_b)[(ncol(EEsTNMed_b)-1):ncol(EEsTNMed_b)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEs05gm_b[,seq(ncol(EEs05gm_b)+1,ncol(EEs05gm_b)+2,1)] = AggregateEEs(EEs = EEs05gm_b, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEs05gm_b)[(ncol(EEs05gm_b)-1):ncol(EEs05gm_b)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEs95m_b[,seq(ncol(EEs95m_b)+1,ncol(EEs95m_b)+2,1)] = AggregateEEs(EEs = EEs95m_b, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEs95m_b)[(ncol(EEs95m_b)-1):ncol(EEs95m_b)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEsotgm_b[,seq(ncol(EEsotgm_b)+1,ncol(EEsotgm_b)+2,1)] = AggregateEEs(EEs = EEsotgm_b, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEsotgm_b)[(ncol(EEsotgm_b)-1):ncol(EEsotgm_b)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEs05_h[,seq(ncol(EEs05_h)+1,ncol(EEs05_h)+2,1)] = AggregateEEs(EEs = EEs05_h, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEs05_h)[(ncol(EEs05_h)-1):ncol(EEs05_h)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEs95_h[,seq(ncol(EEs95_h)+1,ncol(EEs95_h)+2,1)] = AggregateEEs(EEs = EEs95_h, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEs95_h)[(ncol(EEs95_h)-1):ncol(EEs95_h)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEsot_h[,seq(ncol(EEsot_h)+1,ncol(EEsot_h)+2,1)] = AggregateEEs(EEs = EEsot_h, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEsot_h)[(ncol(EEsot_h)-1):ncol(EEsot_h)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEsotg_h[,seq(ncol(EEsotg_h)+1,ncol(EEsotg_h)+2,1)] = AggregateEEs(EEs = EEsotg_h, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEsotg_h)[(ncol(EEsotg_h)-1):ncol(EEsotg_h)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEs05g_h[,seq(ncol(EEs05g_h)+1,ncol(EEs05g_h)+2,1)] = AggregateEEs(EEs = EEs05g_h, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEs05g_h)[(ncol(EEs05g_h)-1):ncol(EEs05g_h)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEsTN05_h[,seq(ncol(EEsTN05_h)+1,ncol(EEsTN05_h)+2,1)] = AggregateEEs(EEs = EEsTN05_h, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEsTN05_h)[(ncol(EEsTN05_h)-1):ncol(EEsTN05_h)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEsTN95_h[,seq(ncol(EEsTN95_h)+1,ncol(EEsTN95_h)+2,1)] = AggregateEEs(EEs = EEsTN95_h, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEsTN95_h)[(ncol(EEsTN95_h)-1):ncol(EEsTN95_h)] = c('Soil8_Ksat', 'Soil9_Ksat')

EEsTNMed_h[,seq(ncol(EEsTNMed_h)+1,ncol(EEsTNMed_h)+2,1)] = AggregateEEs(EEs = EEsTNMed_h, ColNames = as.matrix(rbind(c('s108_Ksat_0', 's8_Ksat_0', 's108_Ksat_0_v', 's8_Ksat_0_v'), c('s109_Ksat_0', 's9_Ksat_0', 's109_Ksat_0_v', 's9_Ksat_0_v'))), FUN = meanabs)
colnames(EEsTNMed_h)[(ncol(EEsTNMed_h)-1):ncol(EEsTNMed_h)] = c('Soil8_Ksat', 'Soil9_Ksat')

#  Soil m decay----
# 's109_m' 's9_m'
# 's108_m' 's8_m'
EEs05_b[,seq(ncol(EEs05_b)+1,ncol(EEs05_b)+2,1)] = AggregateEEs(EEs = EEs05_b, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEs05_b)[(ncol(EEs05_b)-1):ncol(EEs05_b)] = c('Soil8_m', 'Soil9_m')

EEs95_b[,seq(ncol(EEs95_b)+1,ncol(EEs95_b)+2,1)] = AggregateEEs(EEs = EEs95_b, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEs95_b)[(ncol(EEs95_b)-1):ncol(EEs95_b)] = c('Soil8_m', 'Soil9_m')

EEsot_b[,seq(ncol(EEsot_b)+1,ncol(EEsot_b)+2,1)] = AggregateEEs(EEs = EEsot_b, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEsot_b)[(ncol(EEsot_b)-1):ncol(EEsot_b)] = c('Soil8_m', 'Soil9_m')

EEs05g_b[,seq(ncol(EEs05g_b)+1,ncol(EEs05g_b)+2,1)] = AggregateEEs(EEs = EEs05g_b, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEs05g_b)[(ncol(EEs05g_b)-1):ncol(EEs05g_b)] = c('Soil8_m', 'Soil9_m')

EEsotg_b[,seq(ncol(EEsotg_b)+1,ncol(EEsotg_b)+2,1)] = AggregateEEs(EEs = EEsotg_b, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEsotg_b)[(ncol(EEsotg_b)-1):ncol(EEsotg_b)] = c('Soil8_m', 'Soil9_m')

EEsLNSE_b[,seq(ncol(EEsLNSE_b)+1,ncol(EEsLNSE_b)+2,1)] = AggregateEEs(EEs = EEsLNSE_b, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEsLNSE_b)[(ncol(EEsLNSE_b)-1):ncol(EEsLNSE_b)] = c('Soil8_m', 'Soil9_m')

EEsNSE_b[,seq(ncol(EEsNSE_b)+1,ncol(EEsNSE_b)+2,1)] = AggregateEEs(EEs = EEsNSE_b, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEsNSE_b)[(ncol(EEsNSE_b)-1):ncol(EEsNSE_b)] = c('Soil8_m', 'Soil9_m')

EEspBias_b[,seq(ncol(EEspBias_b)+1,ncol(EEspBias_b)+2,1)] = AggregateEEs(EEs = EEspBias_b, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEspBias_b)[(ncol(EEspBias_b)-1):ncol(EEspBias_b)] = c('Soil8_m', 'Soil9_m')

EEsLogL_b[,seq(ncol(EEsLogL_b)+1,ncol(EEsLogL_b)+2,1)] = AggregateEEs(EEs = EEsLogL_b, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEsLogL_b)[(ncol(EEsLogL_b)-1):ncol(EEsLogL_b)] = c('Soil8_m', 'Soil9_m')

EEsTN05_b[,seq(ncol(EEsTN05_b)+1,ncol(EEsTN05_b)+2,1)] = AggregateEEs(EEs = EEsTN05_b, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEsTN05_b)[(ncol(EEsTN05_b)-1):ncol(EEsTN05_b)] = c('Soil8_m', 'Soil9_m')

EEsTN95_b[,seq(ncol(EEsTN95_b)+1,ncol(EEsTN95_b)+2,1)] = AggregateEEs(EEs = EEsTN95_b, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEsTN95_b)[(ncol(EEsTN95_b)-1):ncol(EEsTN95_b)] = c('Soil8_m', 'Soil9_m')

EEsTNMed_b[,seq(ncol(EEsTNMed_b)+1,ncol(EEsTNMed_b)+2,1)] = AggregateEEs(EEs = EEsTNMed_b, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEsTNMed_b)[(ncol(EEsTNMed_b)-1):ncol(EEsTNMed_b)] = c('Soil8_m', 'Soil9_m')

EEs05gm_b[,seq(ncol(EEs05gm_b)+1,ncol(EEs05gm_b)+2,1)] = AggregateEEs(EEs = EEs05gm_b, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEs05gm_b)[(ncol(EEs05gm_b)-1):ncol(EEs05gm_b)] = c('Soil8_m', 'Soil9_m')

EEs95m_b[,seq(ncol(EEs95m_b)+1,ncol(EEs95m_b)+2,1)] = AggregateEEs(EEs = EEs95m_b, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEs95m_b)[(ncol(EEs95m_b)-1):ncol(EEs95m_b)] = c('Soil8_m', 'Soil9_m')

EEsotgm_b[,seq(ncol(EEsotgm_b)+1,ncol(EEsotgm_b)+2,1)] = AggregateEEs(EEs = EEsotgm_b, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEsotgm_b)[(ncol(EEsotgm_b)-1):ncol(EEsotgm_b)] = c('Soil8_m', 'Soil9_m')

EEs05_h[,seq(ncol(EEs05_h)+1,ncol(EEs05_h)+2,1)] = AggregateEEs(EEs = EEs05_h, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEs05_h)[(ncol(EEs05_h)-1):ncol(EEs05_h)] = c('Soil8_m', 'Soil9_m')

EEs95_h[,seq(ncol(EEs95_h)+1,ncol(EEs95_h)+2,1)] = AggregateEEs(EEs = EEs95_h, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEs95_h)[(ncol(EEs95_h)-1):ncol(EEs95_h)] = c('Soil8_m', 'Soil9_m')

EEsot_h[,seq(ncol(EEsot_h)+1,ncol(EEsot_h)+2,1)] = AggregateEEs(EEs = EEsot_h, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEsot_h)[(ncol(EEsot_h)-1):ncol(EEsot_h)] = c('Soil8_m', 'Soil9_m')

EEsotg_h[,seq(ncol(EEsotg_h)+1,ncol(EEsotg_h)+2,1)] = AggregateEEs(EEs = EEsotg_h, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEsotg_h)[(ncol(EEsotg_h)-1):ncol(EEsotg_h)] = c('Soil8_m', 'Soil9_m')

EEs05g_h[,seq(ncol(EEs05g_h)+1,ncol(EEs05g_h)+2,1)] = AggregateEEs(EEs = EEs05g_h, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEs05g_h)[(ncol(EEs05g_h)-1):ncol(EEs05g_h)] = c('Soil8_m', 'Soil9_m')

EEsTN05_h[,seq(ncol(EEsTN05_h)+1,ncol(EEsTN05_h)+2,1)] = AggregateEEs(EEs = EEsTN05_h, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEsTN05_h)[(ncol(EEsTN05_h)-1):ncol(EEsTN05_h)] = c('Soil8_m', 'Soil9_m')

EEsTN95_h[,seq(ncol(EEsTN95_h)+1,ncol(EEsTN95_h)+2,1)] = AggregateEEs(EEs = EEsTN95_h, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEsTN95_h)[(ncol(EEsTN95_h)-1):ncol(EEsTN95_h)] = c('Soil8_m', 'Soil9_m')

EEsTNMed_h[,seq(ncol(EEsTNMed_h)+1,ncol(EEsTNMed_h)+2,1)] = AggregateEEs(EEs = EEsTNMed_h, ColNames = as.matrix(rbind(c('s108_m', 's8_m'), c('s109_m', 's9_m'))), FUN = meanabs)
colnames(EEsTNMed_h)[(ncol(EEsTNMed_h)-1):ncol(EEsTNMed_h)] = c('Soil8_m', 'Soil9_m')

#  Soil porosity at surface----
# 's109_porosity_0' 's9_porosity_0'
# 's108_porosity_0' 's8_porosity_0'
EEs05_b[,seq(ncol(EEs05_b)+1,ncol(EEs05_b)+2,1)] = AggregateEEs(EEs = EEs05_b, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEs05_b)[(ncol(EEs05_b)-1):ncol(EEs05_b)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEs95_b[,seq(ncol(EEs95_b)+1,ncol(EEs95_b)+2,1)] = AggregateEEs(EEs = EEs95_b, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEs95_b)[(ncol(EEs95_b)-1):ncol(EEs95_b)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEsot_b[,seq(ncol(EEsot_b)+1,ncol(EEsot_b)+2,1)] = AggregateEEs(EEs = EEsot_b, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEsot_b)[(ncol(EEsot_b)-1):ncol(EEsot_b)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEs05g_b[,seq(ncol(EEs05g_b)+1,ncol(EEs05g_b)+2,1)] = AggregateEEs(EEs = EEs05g_b, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEs05g_b)[(ncol(EEs05g_b)-1):ncol(EEs05g_b)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEsotg_b[,seq(ncol(EEsotg_b)+1,ncol(EEsotg_b)+2,1)] = AggregateEEs(EEs = EEsotg_b, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEsotg_b)[(ncol(EEsotg_b)-1):ncol(EEsotg_b)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEsLNSE_b[,seq(ncol(EEsLNSE_b)+1,ncol(EEsLNSE_b)+2,1)] = AggregateEEs(EEs = EEsLNSE_b, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEsLNSE_b)[(ncol(EEsLNSE_b)-1):ncol(EEsLNSE_b)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEsNSE_b[,seq(ncol(EEsNSE_b)+1,ncol(EEsNSE_b)+2,1)] = AggregateEEs(EEs = EEsNSE_b, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEsNSE_b)[(ncol(EEsNSE_b)-1):ncol(EEsNSE_b)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEspBias_b[,seq(ncol(EEspBias_b)+1,ncol(EEspBias_b)+2,1)] = AggregateEEs(EEs = EEspBias_b, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEspBias_b)[(ncol(EEspBias_b)-1):ncol(EEspBias_b)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEsLogL_b[,seq(ncol(EEsLogL_b)+1,ncol(EEsLogL_b)+2,1)] = AggregateEEs(EEs = EEsLogL_b, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEsLogL_b)[(ncol(EEsLogL_b)-1):ncol(EEsLogL_b)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEsTN05_b[,seq(ncol(EEsTN05_b)+1,ncol(EEsTN05_b)+2,1)] = AggregateEEs(EEs = EEsTN05_b, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEsTN05_b)[(ncol(EEsTN05_b)-1):ncol(EEsTN05_b)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEsTN95_b[,seq(ncol(EEsTN95_b)+1,ncol(EEsTN95_b)+2,1)] = AggregateEEs(EEs = EEsTN95_b, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEsTN95_b)[(ncol(EEsTN95_b)-1):ncol(EEsTN95_b)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEsTNMed_b[,seq(ncol(EEsTNMed_b)+1,ncol(EEsTNMed_b)+2,1)] = AggregateEEs(EEs = EEsTNMed_b, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEsTNMed_b)[(ncol(EEsTNMed_b)-1):ncol(EEsTNMed_b)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEs05gm_b[,seq(ncol(EEs05gm_b)+1,ncol(EEs05gm_b)+2,1)] = AggregateEEs(EEs = EEs05gm_b, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEs05gm_b)[(ncol(EEs05gm_b)-1):ncol(EEs05gm_b)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEs95m_b[,seq(ncol(EEs95m_b)+1,ncol(EEs95m_b)+2,1)] = AggregateEEs(EEs = EEs95m_b, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEs95m_b)[(ncol(EEs95m_b)-1):ncol(EEs95m_b)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEsotgm_b[,seq(ncol(EEsotgm_b)+1,ncol(EEsotgm_b)+2,1)] = AggregateEEs(EEs = EEsotgm_b, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEsotgm_b)[(ncol(EEsotgm_b)-1):ncol(EEsotgm_b)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEs05_h[,seq(ncol(EEs05_h)+1,ncol(EEs05_h)+2,1)] = AggregateEEs(EEs = EEs05_h, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEs05_h)[(ncol(EEs05_h)-1):ncol(EEs05_h)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEs95_h[,seq(ncol(EEs95_h)+1,ncol(EEs95_h)+2,1)] = AggregateEEs(EEs = EEs95_h, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEs95_h)[(ncol(EEs95_h)-1):ncol(EEs95_h)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEsot_h[,seq(ncol(EEsot_h)+1,ncol(EEsot_h)+2,1)] = AggregateEEs(EEs = EEsot_h, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEsot_h)[(ncol(EEsot_h)-1):ncol(EEsot_h)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEsotg_h[,seq(ncol(EEsotg_h)+1,ncol(EEsotg_h)+2,1)] = AggregateEEs(EEs = EEsotg_h, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEsotg_h)[(ncol(EEsotg_h)-1):ncol(EEsotg_h)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEs05g_h[,seq(ncol(EEs05g_h)+1,ncol(EEs05g_h)+2,1)] = AggregateEEs(EEs = EEs05g_h, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEs05g_h)[(ncol(EEs05g_h)-1):ncol(EEs05g_h)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEsTN05_h[,seq(ncol(EEsTN05_h)+1,ncol(EEsTN05_h)+2,1)] = AggregateEEs(EEs = EEsTN05_h, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEsTN05_h)[(ncol(EEsTN05_h)-1):ncol(EEsTN05_h)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEsTN95_h[,seq(ncol(EEsTN95_h)+1,ncol(EEsTN95_h)+2,1)] = AggregateEEs(EEs = EEsTN95_h, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEsTN95_h)[(ncol(EEsTN95_h)-1):ncol(EEsTN95_h)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

EEsTNMed_h[,seq(ncol(EEsTNMed_h)+1,ncol(EEsTNMed_h)+2,1)] = AggregateEEs(EEs = EEsTNMed_h, ColNames = as.matrix(rbind(c('s108_porosity_0', 's8_porosity_0'), c('s109_porosity_0', 's9_porosity_0'))), FUN = meanabs)
colnames(EEsTNMed_h)[(ncol(EEsTNMed_h)-1):ncol(EEsTNMed_h)] = c('Soil8_porosity_0', 'Soil9_porosity_0')

#  Optimal growing temperature----
# 'v102_epc.topt' 'v102_epc.tmax'
# 'v3_epc.topt' 'v3_epc.tmax'
EEs05_b[,seq(ncol(EEs05_b)+1,ncol(EEs05_b)+2,1)] = AggregateEEs(EEs = EEs05_b, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEs05_b)[(ncol(EEs05_b)-1):ncol(EEs05_b)] = c('v102_Temp', 'v3_Temp')

EEs95_b[,seq(ncol(EEs95_b)+1,ncol(EEs95_b)+2,1)] = AggregateEEs(EEs = EEs95_b, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEs95_b)[(ncol(EEs95_b)-1):ncol(EEs95_b)] = c('v102_Temp', 'v3_Temp')

EEsot_b[,seq(ncol(EEsot_b)+1,ncol(EEsot_b)+2,1)] = AggregateEEs(EEs = EEsot_b, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEsot_b)[(ncol(EEsot_b)-1):ncol(EEsot_b)] = c('v102_Temp', 'v3_Temp')

EEs05g_b[,seq(ncol(EEs05g_b)+1,ncol(EEs05g_b)+2,1)] = AggregateEEs(EEs = EEs05g_b, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEs05g_b)[(ncol(EEs05g_b)-1):ncol(EEs05g_b)] = c('v102_Temp', 'v3_Temp')

EEsotg_b[,seq(ncol(EEsotg_b)+1,ncol(EEsotg_b)+2,1)] = AggregateEEs(EEs = EEsotg_b, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEsotg_b)[(ncol(EEsotg_b)-1):ncol(EEsotg_b)] = c('v102_Temp', 'v3_Temp')

EEsLNSE_b[,seq(ncol(EEsLNSE_b)+1,ncol(EEsLNSE_b)+2,1)] = AggregateEEs(EEs = EEsLNSE_b, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEsLNSE_b)[(ncol(EEsLNSE_b)-1):ncol(EEsLNSE_b)] = c('v102_Temp', 'v3_Temp')

EEsNSE_b[,seq(ncol(EEsNSE_b)+1,ncol(EEsNSE_b)+2,1)] = AggregateEEs(EEs = EEsNSE_b, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEsNSE_b)[(ncol(EEsNSE_b)-1):ncol(EEsNSE_b)] = c('v102_Temp', 'v3_Temp')

EEspBias_b[,seq(ncol(EEspBias_b)+1,ncol(EEspBias_b)+2,1)] = AggregateEEs(EEs = EEspBias_b, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEspBias_b)[(ncol(EEspBias_b)-1):ncol(EEspBias_b)] = c('v102_Temp', 'v3_Temp')

EEsLogL_b[,seq(ncol(EEsLogL_b)+1,ncol(EEsLogL_b)+2,1)] = AggregateEEs(EEs = EEsLogL_b, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEsLogL_b)[(ncol(EEsLogL_b)-1):ncol(EEsLogL_b)] = c('v102_Temp', 'v3_Temp')

EEsTN05_b[,seq(ncol(EEsTN05_b)+1,ncol(EEsTN05_b)+2,1)] = AggregateEEs(EEs = EEsTN05_b, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEsTN05_b)[(ncol(EEsTN05_b)-1):ncol(EEsTN05_b)] = c('v102_Temp', 'v3_Temp')

EEsTN95_b[,seq(ncol(EEsTN95_b)+1,ncol(EEsTN95_b)+2,1)] = AggregateEEs(EEs = EEsTN95_b, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEsTN95_b)[(ncol(EEsTN95_b)-1):ncol(EEsTN95_b)] = c('v102_Temp', 'v3_Temp')

EEsTNMed_b[,seq(ncol(EEsTNMed_b)+1,ncol(EEsTNMed_b)+2,1)] = AggregateEEs(EEs = EEsTNMed_b, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEsTNMed_b)[(ncol(EEsTNMed_b)-1):ncol(EEsTNMed_b)] = c('v102_Temp', 'v3_Temp')

EEs05gm_b[,seq(ncol(EEs05gm_b)+1,ncol(EEs05gm_b)+2,1)] = AggregateEEs(EEs = EEs05gm_b, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEs05gm_b)[(ncol(EEs05gm_b)-1):ncol(EEs05gm_b)] = c('v102_Temp', 'v3_Temp')

EEs95m_b[,seq(ncol(EEs95m_b)+1,ncol(EEs95m_b)+2,1)] = AggregateEEs(EEs = EEs95m_b, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEs95m_b)[(ncol(EEs95m_b)-1):ncol(EEs95m_b)] = c('v102_Temp', 'v3_Temp')

EEsotgm_b[,seq(ncol(EEsotgm_b)+1,ncol(EEsotgm_b)+2,1)] = AggregateEEs(EEs = EEsotgm_b, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEsotgm_b)[(ncol(EEsotgm_b)-1):ncol(EEsotgm_b)] = c('v102_Temp', 'v3_Temp')

EEs05_h[,seq(ncol(EEs05_h)+1,ncol(EEs05_h)+2,1)] = AggregateEEs(EEs = EEs05_h, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEs05_h)[(ncol(EEs05_h)-1):ncol(EEs05_h)] = c('v102_Temp', 'v3_Temp')

EEs95_h[,seq(ncol(EEs95_h)+1,ncol(EEs95_h)+2,1)] = AggregateEEs(EEs = EEs95_h, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEs95_h)[(ncol(EEs95_h)-1):ncol(EEs95_h)] = c('v102_Temp', 'v3_Temp')

EEsot_h[,seq(ncol(EEsot_h)+1,ncol(EEsot_h)+2,1)] = AggregateEEs(EEs = EEsot_h, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEsot_h)[(ncol(EEsot_h)-1):ncol(EEsot_h)] = c('v102_Temp', 'v3_Temp')

EEsotg_h[,seq(ncol(EEsotg_h)+1,ncol(EEsotg_h)+2,1)] = AggregateEEs(EEs = EEsotg_h, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEsotg_h)[(ncol(EEsotg_h)-1):ncol(EEsotg_h)] = c('v102_Temp', 'v3_Temp')

EEs05g_h[,seq(ncol(EEs05g_h)+1,ncol(EEs05g_h)+2,1)] = AggregateEEs(EEs = EEs05g_h, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEs05g_h)[(ncol(EEs05g_h)-1):ncol(EEs05g_h)] = c('v102_Temp', 'v3_Temp')

EEsTN05_h[,seq(ncol(EEsTN05_h)+1,ncol(EEsTN05_h)+2,1)] = AggregateEEs(EEs = EEsTN05_h, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEsTN05_h)[(ncol(EEsTN05_h)-1):ncol(EEsTN05_h)] = c('v102_Temp', 'v3_Temp')

EEsTN95_h[,seq(ncol(EEsTN95_h)+1,ncol(EEsTN95_h)+2,1)] = AggregateEEs(EEs = EEsTN95_h, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEsTN95_h)[(ncol(EEsTN95_h)-1):ncol(EEsTN95_h)] = c('v102_Temp', 'v3_Temp')

EEsTNMed_h[,seq(ncol(EEsTNMed_h)+1,ncol(EEsTNMed_h)+2,1)] = AggregateEEs(EEs = EEsTNMed_h, ColNames = as.matrix(rbind(c('v102_epc.topt', 'v102_epc.tmax'), c('v3_epc.topt', 'v3_epc.tmax'))), FUN = meanabs)
colnames(EEsTNMed_h)[(ncol(EEsTNMed_h)-1):ncol(EEsTNMed_h)] = c('v102_Temp', 'v3_Temp')

#  Leaf and Leaf Litter CN ratio----
# 'v102_epc.leaf_cn' 'v102_epc.leaflitr_cn'
# 'v3_epc.leaf_cn' 'v3_epc.leaflitr_cn'
EEs05_b[,seq(ncol(EEs05_b)+1,ncol(EEs05_b)+2,1)] = AggregateEEs(EEs = EEs05_b, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEs05_b)[(ncol(EEs05_b)-1):ncol(EEs05_b)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEs95_b[,seq(ncol(EEs95_b)+1,ncol(EEs95_b)+2,1)] = AggregateEEs(EEs = EEs95_b, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEs95_b)[(ncol(EEs95_b)-1):ncol(EEs95_b)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEsot_b[,seq(ncol(EEsot_b)+1,ncol(EEsot_b)+2,1)] = AggregateEEs(EEs = EEsot_b, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEsot_b)[(ncol(EEsot_b)-1):ncol(EEsot_b)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEs05g_b[,seq(ncol(EEs05g_b)+1,ncol(EEs05g_b)+2,1)] = AggregateEEs(EEs = EEs05g_b, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEs05g_b)[(ncol(EEs05g_b)-1):ncol(EEs05g_b)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEsotg_b[,seq(ncol(EEsotg_b)+1,ncol(EEsotg_b)+2,1)] = AggregateEEs(EEs = EEsotg_b, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEsotg_b)[(ncol(EEsotg_b)-1):ncol(EEsotg_b)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEsLNSE_b[,seq(ncol(EEsLNSE_b)+1,ncol(EEsLNSE_b)+2,1)] = AggregateEEs(EEs = EEsLNSE_b, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEsLNSE_b)[(ncol(EEsLNSE_b)-1):ncol(EEsLNSE_b)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEsNSE_b[,seq(ncol(EEsNSE_b)+1,ncol(EEsNSE_b)+2,1)] = AggregateEEs(EEs = EEsNSE_b, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEsNSE_b)[(ncol(EEsNSE_b)-1):ncol(EEsNSE_b)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEspBias_b[,seq(ncol(EEspBias_b)+1,ncol(EEspBias_b)+2,1)] = AggregateEEs(EEs = EEspBias_b, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEspBias_b)[(ncol(EEspBias_b)-1):ncol(EEspBias_b)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEsLogL_b[,seq(ncol(EEsLogL_b)+1,ncol(EEsLogL_b)+2,1)] = AggregateEEs(EEs = EEsLogL_b, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEsLogL_b)[(ncol(EEsLogL_b)-1):ncol(EEsLogL_b)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEsTN05_b[,seq(ncol(EEsTN05_b)+1,ncol(EEsTN05_b)+2,1)] = AggregateEEs(EEs = EEsTN05_b, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEsTN05_b)[(ncol(EEsTN05_b)-1):ncol(EEsTN05_b)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEsTN95_b[,seq(ncol(EEsTN95_b)+1,ncol(EEsTN95_b)+2,1)] = AggregateEEs(EEs = EEsTN95_b, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEsTN95_b)[(ncol(EEsTN95_b)-1):ncol(EEsTN95_b)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEsTNMed_b[,seq(ncol(EEsTNMed_b)+1,ncol(EEsTNMed_b)+2,1)] = AggregateEEs(EEs = EEsTNMed_b, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEsTNMed_b)[(ncol(EEsTNMed_b)-1):ncol(EEsTNMed_b)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEs05gm_b[,seq(ncol(EEs05gm_b)+1,ncol(EEs05gm_b)+2,1)] = AggregateEEs(EEs = EEs05gm_b, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEs05gm_b)[(ncol(EEs05gm_b)-1):ncol(EEs05gm_b)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEs95m_b[,seq(ncol(EEs95m_b)+1,ncol(EEs95m_b)+2,1)] = AggregateEEs(EEs = EEs95m_b, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEs95m_b)[(ncol(EEs95m_b)-1):ncol(EEs95m_b)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEsotgm_b[,seq(ncol(EEsotgm_b)+1,ncol(EEsotgm_b)+2,1)] = AggregateEEs(EEs = EEsotgm_b, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEsotgm_b)[(ncol(EEsotgm_b)-1):ncol(EEsotgm_b)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEs05_h[,seq(ncol(EEs05_h)+1,ncol(EEs05_h)+2,1)] = AggregateEEs(EEs = EEs05_h, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEs05_h)[(ncol(EEs05_h)-1):ncol(EEs05_h)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEs95_h[,seq(ncol(EEs95_h)+1,ncol(EEs95_h)+2,1)] = AggregateEEs(EEs = EEs95_h, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEs95_h)[(ncol(EEs95_h)-1):ncol(EEs95_h)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEsot_h[,seq(ncol(EEsot_h)+1,ncol(EEsot_h)+2,1)] = AggregateEEs(EEs = EEsot_h, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEsot_h)[(ncol(EEsot_h)-1):ncol(EEsot_h)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEsotg_h[,seq(ncol(EEsotg_h)+1,ncol(EEsotg_h)+2,1)] = AggregateEEs(EEs = EEsotg_h, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEsotg_h)[(ncol(EEsotg_h)-1):ncol(EEsotg_h)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEs05g_h[,seq(ncol(EEs05g_h)+1,ncol(EEs05g_h)+2,1)] = AggregateEEs(EEs = EEs05g_h, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEs05g_h)[(ncol(EEs05g_h)-1):ncol(EEs05g_h)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEsTN05_h[,seq(ncol(EEsTN05_h)+1,ncol(EEsTN05_h)+2,1)] = AggregateEEs(EEs = EEsTN05_h, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEsTN05_h)[(ncol(EEsTN05_h)-1):ncol(EEsTN05_h)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEsTN95_h[,seq(ncol(EEsTN95_h)+1,ncol(EEsTN95_h)+2,1)] = AggregateEEs(EEs = EEsTN95_h, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEsTN95_h)[(ncol(EEsTN95_h)-1):ncol(EEsTN95_h)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

EEsTNMed_h[,seq(ncol(EEsTNMed_h)+1,ncol(EEsTNMed_h)+2,1)] = AggregateEEs(EEs = EEsTNMed_h, ColNames = as.matrix(rbind(c('v102_epc.leaf_cn', 'v102_epc.leaflitr_cn'), c('v3_epc.leaf_cn', 'v3_epc.leaflitr_cn'))), FUN = meanabs)
colnames(EEsTNMed_h)[(ncol(EEsTNMed_h)-1):ncol(EEsTNMed_h)] = c('v102_epc.LeafLitrCN', 'v3_epc.LeafLitrCN')

# Compute bootstrapped samples of the mean absolute value with replacement of the EEs for each parameter----
#Each parameter is done separately as opposed to resampling by trajectory to avoid correlation in SDs of bootstrapped results
set.seed(12319)
#Number of bootstrapped samples to take
reps = 1000
#Matrix to store the computed mean absolute value of the elementary effects
EEs05_b_mua = matrix(NA, ncol = nrow(ParamRanges), nrow = reps)
EEs95_b_mua = matrix(NA, ncol = nrow(ParamRanges), nrow = reps)
EEsot_b_mua = matrix(NA, ncol = nrow(ParamRanges), nrow = reps)
EEs05g_b_mua = matrix(NA, ncol = nrow(ParamRanges), nrow = reps)
EEsotg_b_mua = matrix(NA, ncol = nrow(ParamRanges), nrow = reps)
EEsLNSE_b_mua = matrix(NA, ncol = nrow(ParamRanges), nrow = reps)
EEsNSE_b_mua = matrix(NA, ncol = nrow(ParamRanges), nrow = reps)
EEspBias_b_mua = matrix(NA, ncol = nrow(ParamRanges), nrow = reps)
EEsLogL_b_mua = matrix(NA, ncol = nrow(ParamRanges), nrow = reps)
EEsTN05_b_mua = matrix(NA, ncol = nrow(ParamRanges), nrow = reps)
EEsTN95_b_mua = matrix(NA, ncol = nrow(ParamRanges), nrow = reps)
EEsTNMed_b_mua = matrix(NA, ncol = nrow(ParamRanges), nrow = reps)
EEs05_h_mua = matrix(NA, ncol = nrow(ParamRanges)+1, nrow = reps*length(uhills))
EEs95_h_mua = matrix(NA, ncol = nrow(ParamRanges)+1, nrow = reps*length(uhills))
EEsot_h_mua = matrix(NA, ncol = nrow(ParamRanges)+1, nrow = reps*length(uhills))
EEs05g_h_mua = matrix(NA, ncol = nrow(ParamRanges)+1, nrow = reps*length(uhills))
EEsotg_h_mua = matrix(NA, ncol = nrow(ParamRanges)+1, nrow = reps*length(uhills))
EEsTN05_h_mua = matrix(NA, ncol = nrow(ParamRanges)+1, nrow = reps*length(uhills))
EEsTN95_h_mua = matrix(NA, ncol = nrow(ParamRanges)+1, nrow = reps*length(uhills))
EEsTNMed_h_mua = matrix(NA, ncol = nrow(ParamRanges)+1, nrow = reps*length(uhills))
EEs05gm_b_mua = matrix(NA, ncol = nrow(ParamRanges), nrow = reps)
EEs95m_b_mua = matrix(NA, ncol = nrow(ParamRanges), nrow = reps)
EEsotgm_b_mua = matrix(NA, ncol = nrow(ParamRanges), nrow = reps)

#Array to store the matrices of the bootstrapped samples for constrained variables. These will have to be aggregated afterwards.
EEs05_b_mua_cons = array(NA, dim = c(reps,r,length(ColsAggregated)))
EEs95_b_mua_cons = array(NA, dim = c(reps,r,length(ColsAggregated)))
EEsot_b_mua_cons = array(NA, dim = c(reps,r,length(ColsAggregated)))
EEs05g_b_mua_cons = array(NA, dim = c(reps,r,length(ColsAggregated)))
EEsotg_b_mua_cons = array(NA, dim = c(reps,r,length(ColsAggregated)))
EEsLNSE_b_mua_cons = array(NA, dim = c(reps,r,length(ColsAggregated)))
EEsNSE_b_mua_cons = array(NA, dim = c(reps,r,length(ColsAggregated)))
EEspBias_b_mua_cons = array(NA, dim = c(reps,r,length(ColsAggregated)))
EEsLogL_b_mua_cons = array(NA, dim = c(reps,r,length(ColsAggregated)))
EEsTN05_b_mua_cons = array(NA, dim = c(reps,r,length(ColsAggregated)))
EEsTN95_b_mua_cons = array(NA, dim = c(reps,r,length(ColsAggregated)))
EEsTNMed_b_mua_cons = array(NA, dim = c(reps,r,length(ColsAggregated)))
EEs05_h_mua_cons = array(NA, dim = c(reps*length(uhills),r,length(ColsAggregated)+1))
EEs95_h_mua_cons = array(NA, dim = c(reps*length(uhills),r,length(ColsAggregated)+1))
EEsot_h_mua_cons = array(NA, dim = c(reps*length(uhills),r,length(ColsAggregated)+1))
EEs05g_h_mua_cons = array(NA, dim = c(reps*length(uhills),r,length(ColsAggregated)+1))
EEsotg_h_mua_cons = array(NA, dim = c(reps*length(uhills),r,length(ColsAggregated)+1))
EEsTN05_h_mua_cons = array(NA, dim = c(reps*length(uhills),r,length(ColsAggregated)+1))
EEsTN95_h_mua_cons = array(NA, dim = c(reps*length(uhills),r,length(ColsAggregated)+1))
EEsTNMed_h_mua_cons = array(NA, dim = c(reps*length(uhills),r,length(ColsAggregated)+1))
EEs05gm_b_mua_cons = array(NA, dim = c(reps,r,length(ColsAggregated)))
EEs95m_b_mua_cons = array(NA, dim = c(reps,r,length(ColsAggregated)))
EEsotgm_b_mua_cons = array(NA, dim = c(reps,r,length(ColsAggregated)))

#Loop over all of the parameters
for (p in 1:(nrow(ParamRanges))){
  #Draw bootstrapped samples for that parameter
  tReps = t(replicate(expr = ceiling(runif(n = r, min = 0, max = r)), n = reps))
  #Extract the EEs for that parameter using the indices in tReps.
  EEs05_b_mua[,p] = apply(matrix(EEs05_b[tReps,p], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
  EEs95_b_mua[,p] = apply(matrix(EEs95_b[tReps,p], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
  EEsot_b_mua[,p] = apply(matrix(EEsot_b[tReps,p], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
  EEs05g_b_mua[,p] = apply(matrix(EEs05g_b[tReps,p], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
  EEsotg_b_mua[,p] = apply(matrix(EEsotg_b[tReps,p], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
  EEsLNSE_b_mua[,p] = apply(matrix(EEsLNSE_b[tReps,p], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
  EEsNSE_b_mua[,p] = apply(matrix(EEsNSE_b[tReps,p], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
  EEspBias_b_mua[,p] = apply(matrix(EEspBias_b[tReps,p], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
  EEsLogL_b_mua[,p] = apply(matrix(EEsLogL_b[tReps,p], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
  EEsTN05_b_mua[,p] = apply(matrix(EEsTN05_b[tReps,p], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
  EEsTN95_b_mua[,p] = apply(matrix(EEsTN95_b[tReps,p], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
  EEsTNMed_b_mua[,p] = apply(matrix(EEsTNMed_b[tReps,p], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
  EEs05gm_b_mua[,p] = apply(matrix(EEs05gm_b[tReps,p], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
  EEs95m_b_mua[,p] = apply(matrix(EEs95m_b[tReps,p], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
  EEsotgm_b_mua[,p] = apply(matrix(EEsotgm_b[tReps,p], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
  for (h in 1:length(uhills)){
    indsh = seq(1+(uhills[h]-1)*reps,reps*uhills[h],1)
    EEs05_h_mua[indsh, p+1] = apply(matrix(EEs05_h[EEs05_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
    EEs05_h_mua[indsh, 1] = uhills[h]
    
    EEs95_h_mua[indsh, p+1] = apply(matrix(EEs95_h[EEs95_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
    EEs95_h_mua[indsh, 1] = uhills[h]
    
    EEsot_h_mua[indsh, p+1] = apply(matrix(EEsot_h[EEsot_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
    EEsot_h_mua[indsh, 1] = uhills[h]
    
    EEs05g_h_mua[indsh, p+1] = apply(matrix(EEs05g_h[EEs05g_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
    EEs05g_h_mua[indsh, 1] = uhills[h]
    
    EEsotg_h_mua[indsh, p+1] = apply(matrix(EEsotg_h[EEsotg_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
    EEsotg_h_mua[indsh, 1] = uhills[h]
    
    EEsTN05_h_mua[indsh, p+1] = apply(matrix(EEsTN05_h[EEsTN05_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
    EEsTN05_h_mua[indsh, 1] = uhills[h]
    
    EEsTN95_h_mua[indsh, p+1] = apply(matrix(EEsTN95_h[EEsTN95_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
    EEsTN95_h_mua[indsh, 1] = uhills[h]
    
    EEsTNMed_h_mua[indsh, p+1] = apply(matrix(EEsTNMed_h[EEsTNMed_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps), MARGIN = 1, FUN = meanabs)
    EEsTNMed_h_mua[indsh, 1] = uhills[h]
  }
  rm(indsh, h)
  #If the variable p is one of the ones that is used to aggregate, save the bootstrapped EEs array for aggregation later
  if (colnames(EEs05_b)[p] %in% ColsAggregated){
    indp = which(ColsAggregated == colnames(EEs05_b)[p])
    EEs05_b_mua_cons[ , , indp] = matrix(EEs05_b[tReps,p], ncol = r, nrow = reps)
    EEs95_b_mua_cons[ , , indp] = matrix(EEs95_b[tReps,p], ncol = r, nrow = reps)
    EEsot_b_mua_cons[ , , indp] = matrix(EEsot_b[tReps,p], ncol = r, nrow = reps)
    EEs05g_b_mua_cons[ , , indp] = matrix(EEs05g_b[tReps,p], ncol = r, nrow = reps)
    EEsotg_b_mua_cons[ , , indp] = matrix(EEsotg_b[tReps,p], ncol = r, nrow = reps)
    EEsLNSE_b_mua_cons[ , , indp] = matrix(EEsLNSE_b[tReps,p], ncol = r, nrow = reps)
    EEsNSE_b_mua_cons[ , , indp] = matrix(EEsNSE_b[tReps,p], ncol = r, nrow = reps)
    EEspBias_b_mua_cons[ , , indp] = matrix(EEspBias_b[tReps,p], ncol = r, nrow = reps)
    EEsLogL_b_mua_cons[ , , indp] = matrix(EEsLogL_b[tReps,p], ncol = r, nrow = reps)
    EEsTN05_b_mua_cons[ , , indp] = matrix(EEsTN05_b[tReps,p], ncol = r, nrow = reps)
    EEsTN95_b_mua_cons[ , , indp] = matrix(EEsTN95_b[tReps,p], ncol = r, nrow = reps)
    EEsTNMed_b_mua_cons[ , , indp] = matrix(EEsTNMed_b[tReps,p], ncol = r, nrow = reps)
    EEs05gm_b_mua_cons[ , , indp] = matrix(EEs05gm_b[tReps,p], ncol = r, nrow = reps)
    EEs95m_b_mua_cons[ , , indp] = matrix(EEs95m_b[tReps,p], ncol = r, nrow = reps)
    EEsotgm_b_mua_cons[ , , indp] = matrix(EEsotgm_b[tReps,p], ncol = r, nrow = reps)
    
    for (h in 1:length(uhills)){
      indsh = seq(1+(uhills[h]-1)*reps,reps*uhills[h],1)
      EEs05_h_mua_cons[indsh, , indp+1] = matrix(EEs05_h[EEs05_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps)
      EEs95_h_mua_cons[indsh, , indp+1] = matrix(EEs95_h[EEs95_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps)
      EEsot_h_mua_cons[indsh, , indp+1] = matrix(EEsot_h[EEsot_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps)
      EEs05g_h_mua_cons[indsh, , indp+1] = matrix(EEs05g_h[EEs05g_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps)
      EEsotg_h_mua_cons[indsh, , indp+1] = matrix(EEsotg_h[EEsotg_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps)
      EEsTN05_h_mua_cons[indsh, , indp+1] = matrix(EEsTN05_h[EEsTN05_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps)
      EEsTN95_h_mua_cons[indsh, , indp+1] = matrix(EEsTN95_h[EEsTN95_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps)
      EEsTNMed_h_mua_cons[indsh, , indp+1] = matrix(EEsTNMed_h[EEsTNMed_h$HillID == uhills[h],p+1][tReps], ncol = r, nrow = reps)
      
      EEs05_h_mua_cons[indsh, , 1] = uhills[h]
      EEs95_h_mua_cons[indsh, , 1] = uhills[h]
      EEsot_h_mua_cons[indsh, , 1] = uhills[h]
      EEs05g_h_mua_cons[indsh, , 1] = uhills[h]
      EEsotg_h_mua_cons[indsh, , 1] = uhills[h]
      EEsTN05_h_mua_cons[indsh, , 1] = uhills[h]
      EEsTN95_h_mua_cons[indsh, , 1] = uhills[h]
      EEsTNMed_h_mua_cons[indsh, , 1] = uhills[h]
    }
    rm(indsh, h, indp)
  }
}
rm(p, tReps)

#Assign column names
colnames(EEs05_b_mua) = colnames(EEs95_b_mua) = colnames(EEsot_b_mua) = colnames(EEs05g_b_mua) = colnames(EEsotg_b_mua) = colnames(EEs05gm_b_mua) = colnames(EEs95m_b_mua) = colnames(EEsotgm_b_mua) = colnames(EEsLNSE_b_mua) = colnames(EEsNSE_b_mua) = colnames(EEspBias_b_mua) = colnames(EEsLogL_b_mua) = colnames(EEsTN05_b_mua) = colnames(EEsTN95_b_mua) = colnames(EEsTNMed_b_mua) = colnames(EEs05_b)[1:nrow(ParamRanges)]
colnames(EEs05_h_mua) = colnames(EEs95_h_mua) = colnames(EEsot_h_mua) = colnames(EEs05g_h_mua) = colnames(EEsotg_h_mua) = colnames(EEsTN05_h_mua) = colnames(EEsTN95_h_mua) = colnames(EEsTNMed_h_mua) = colnames(EEs05_h)[1:(nrow(ParamRanges)+1)]
#Name the arrays
dimnames(EEs05_b_mua_cons) = dimnames(EEs95_b_mua_cons) = dimnames(EEsot_b_mua_cons) = dimnames(EEs05g_b_mua_cons) = dimnames(EEsotg_b_mua_cons) = dimnames(EEs05gm_b_mua_cons) = dimnames(EEs95m_b_mua_cons) = dimnames(EEsotgm_b_mua_cons) = dimnames(EEsLNSE_b_mua_cons) = dimnames(EEsNSE_b_mua_cons) = dimnames(EEspBias_b_mua_cons) = dimnames(EEsLogL_b_mua_cons) = dimnames(EEsTN05_b_mua_cons) = dimnames(EEsTN95_b_mua_cons) = dimnames(EEsTNMed_b_mua_cons) = list(Reps = seq(1,reps,1), Traj = seq(1,r,1), Cols = c(ColsAggregated))
dimnames(EEs05_h_mua_cons) = dimnames(EEs95_h_mua_cons) = dimnames(EEsot_h_mua_cons) = dimnames(EEs05g_h_mua_cons) = dimnames(EEsotg_h_mua_cons) = dimnames(EEsTN05_h_mua_cons) = dimnames(EEsTN95_h_mua_cons) = dimnames(EEsTNMed_h_mua_cons) = list(Reps = seq(1,reps*length(uhills),1), Traj = seq(1,r,1), Cols = c('HillID', ColsAggregated))

# Aggregate the variables that are constrained----
#  EEs05_b_mua----
EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's8_silt')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's8_sand')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 's8_SoilTexture'
EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's108_silt')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's108_sand')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 's108_SoilTexture'
EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's9_silt')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's9_sand')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 's9_SoilTexture'
EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's109_silt')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's109_sand')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 's109_SoilTexture'

EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_K_absorptance')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_K_reflectance')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'v102_K_All'

EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_absorptance')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_reflectance')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'v102_PAR_All'

EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_fcel')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flab')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'v102_epc.frootlitr_All'
EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_fcel')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flab')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'v3_epc.frootlitr_All'

EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_fcel')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flab')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'v102_epc.leaflitr_All'
EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_fcel')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flab')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'v3_epc.leaflitr_All'

EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0_v')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'Soil8_Ksat'
EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0_v')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'Soil9_Ksat'

EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's108_m')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'Soil8_m'
EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's109_m')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'Soil9_m'

EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's108_porosity_0')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'Soil8_porosity_0'
EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's109_porosity_0')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'Soil9_porosity_0'

EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.topt')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'v102_Temp'
EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.topt')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'v3_Temp'

EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaf_cn')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'v102_epc.LeafLitrCN'
EEs05_b_mua = cbind(EEs05_b_mua, apply(X = (abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaf_cn')])+abs(EEs05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_b_mua)[ncol(EEs05_b_mua)] = 'v3_epc.LeafLitrCN'

#  EEs95_b_mua----
EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's8_silt')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's8_sand')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 's8_SoilTexture'
EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's108_silt')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's108_sand')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 's108_SoilTexture'
EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's9_silt')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's9_sand')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 's9_SoilTexture'
EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's109_silt')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's109_sand')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 's109_SoilTexture'

EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_K_absorptance')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_K_reflectance')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'v102_K_All'

EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_absorptance')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_reflectance')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'v102_PAR_All'

EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_fcel')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flab')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'v102_epc.frootlitr_All'
EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_fcel')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flab')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'v3_epc.frootlitr_All'

EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_fcel')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flab')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'v102_epc.leaflitr_All'
EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_fcel')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flab')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'v3_epc.leaflitr_All'

EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0_v')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'Soil8_Ksat'
EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0_v')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'Soil9_Ksat'

EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's108_m')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'Soil8_m'
EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's109_m')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'Soil9_m'

EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's108_porosity_0')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'Soil8_porosity_0'
EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's109_porosity_0')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'Soil9_porosity_0'

EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.topt')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'v102_Temp'
EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.topt')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'v3_Temp'

EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaf_cn')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'v102_epc.LeafLitrCN'
EEs95_b_mua = cbind(EEs95_b_mua, apply(X = (abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaf_cn')])+abs(EEs95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_b_mua)[ncol(EEs95_b_mua)] = 'v3_epc.LeafLitrCN'

#  EEsot_b_mua----
EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's8_silt')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's8_sand')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 's8_SoilTexture'
EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's108_silt')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's108_sand')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 's108_SoilTexture'
EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's9_silt')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's9_sand')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 's9_SoilTexture'
EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's109_silt')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's109_sand')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 's109_SoilTexture'

EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_K_absorptance')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_K_reflectance')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'v102_K_All'

EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_absorptance')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_reflectance')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'v102_PAR_All'

EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_fcel')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flab')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'v102_epc.frootlitr_All'
EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_fcel')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flab')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'v3_epc.frootlitr_All'

EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_fcel')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flab')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'v102_epc.leaflitr_All'
EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_fcel')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flab')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'v3_epc.leaflitr_All'

EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0_v')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'Soil8_Ksat'
EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0_v')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'Soil9_Ksat'

EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's108_m')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'Soil8_m'
EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's109_m')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'Soil9_m'

EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's108_porosity_0')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'Soil8_porosity_0'
EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's109_porosity_0')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'Soil9_porosity_0'

EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_epc.topt')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'v102_Temp'
EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v3_epc.topt')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'v3_Temp'

EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaf_cn')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'v102_epc.LeafLitrCN'
EEsot_b_mua = cbind(EEsot_b_mua, apply(X = (abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaf_cn')])+abs(EEsot_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_b_mua)[ncol(EEsot_b_mua)] = 'v3_epc.LeafLitrCN'

#  EEs05g_b_mua----
EEs05g_b_mua = cbind(EEs05g_b_mua, apply(X = (abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's8_silt')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's8_sand')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05g_b_mua)[ncol(EEs05g_b_mua)] = 's8_SoilTexture'
EEs05g_b_mua = cbind(EEs05g_b_mua, apply(X = (abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's108_silt')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's108_sand')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05g_b_mua)[ncol(EEs05g_b_mua)] = 's108_SoilTexture'
EEs05g_b_mua = cbind(EEs05g_b_mua, apply(X = (abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's9_silt')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's9_sand')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05g_b_mua)[ncol(EEs05g_b_mua)] = 's9_SoilTexture'
EEs05g_b_mua = cbind(EEs05g_b_mua, apply(X = (abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's109_silt')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's109_sand')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05g_b_mua)[ncol(EEs05g_b_mua)] = 's109_SoilTexture'

EEs05g_b_mua = cbind(EEs05g_b_mua, apply(X = (abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v102_K_absorptance')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v102_K_reflectance')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05g_b_mua)[ncol(EEs05g_b_mua)] = 'v102_K_All'

EEs05g_b_mua = cbind(EEs05g_b_mua, apply(X = (abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_absorptance')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_reflectance')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05g_b_mua)[ncol(EEs05g_b_mua)] = 'v102_PAR_All'

EEs05g_b_mua = cbind(EEs05g_b_mua, apply(X = (abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_fcel')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flab')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05g_b_mua)[ncol(EEs05g_b_mua)] = 'v102_epc.frootlitr_All'
EEs05g_b_mua = cbind(EEs05g_b_mua, apply(X = (abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_fcel')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flab')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05g_b_mua)[ncol(EEs05g_b_mua)] = 'v3_epc.frootlitr_All'

EEs05g_b_mua = cbind(EEs05g_b_mua, apply(X = (abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_fcel')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flab')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05g_b_mua)[ncol(EEs05g_b_mua)] = 'v102_epc.leaflitr_All'
EEs05g_b_mua = cbind(EEs05g_b_mua, apply(X = (abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_fcel')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flab')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05g_b_mua)[ncol(EEs05g_b_mua)] = 'v3_epc.leaflitr_All'

EEs05g_b_mua = cbind(EEs05g_b_mua, apply(X = (abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0_v')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEs05g_b_mua)[ncol(EEs05g_b_mua)] = 'Soil8_Ksat'
EEs05g_b_mua = cbind(EEs05g_b_mua, apply(X = (abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0_v')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEs05g_b_mua)[ncol(EEs05g_b_mua)] = 'Soil9_Ksat'

EEs05g_b_mua = cbind(EEs05g_b_mua, apply(X = (abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's108_m')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05g_b_mua)[ncol(EEs05g_b_mua)] = 'Soil8_m'
EEs05g_b_mua = cbind(EEs05g_b_mua, apply(X = (abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's109_m')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05g_b_mua)[ncol(EEs05g_b_mua)] = 'Soil9_m'

EEs05g_b_mua = cbind(EEs05g_b_mua, apply(X = (abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's108_porosity_0')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05g_b_mua)[ncol(EEs05g_b_mua)] = 'Soil8_porosity_0'
EEs05g_b_mua = cbind(EEs05g_b_mua, apply(X = (abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's109_porosity_0')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05g_b_mua)[ncol(EEs05g_b_mua)] = 'Soil9_porosity_0'

EEs05g_b_mua = cbind(EEs05g_b_mua, apply(X = (abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v102_epc.topt')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05g_b_mua)[ncol(EEs05g_b_mua)] = 'v102_Temp'
EEs05g_b_mua = cbind(EEs05g_b_mua, apply(X = (abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v3_epc.topt')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05g_b_mua)[ncol(EEs05g_b_mua)] = 'v3_Temp'

EEs05g_b_mua = cbind(EEs05g_b_mua, apply(X = (abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaf_cn')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05g_b_mua)[ncol(EEs05g_b_mua)] = 'v102_epc.LeafLitrCN'
EEs05g_b_mua = cbind(EEs05g_b_mua, apply(X = (abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaf_cn')])+abs(EEs05g_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05g_b_mua)[ncol(EEs05g_b_mua)] = 'v3_epc.LeafLitrCN'

#  EEsotg_b_mua----
EEsotg_b_mua = cbind(EEsotg_b_mua, apply(X = (abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's8_silt')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's8_sand')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotg_b_mua)[ncol(EEsotg_b_mua)] = 's8_SoilTexture'
EEsotg_b_mua = cbind(EEsotg_b_mua, apply(X = (abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's108_silt')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's108_sand')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotg_b_mua)[ncol(EEsotg_b_mua)] = 's108_SoilTexture'
EEsotg_b_mua = cbind(EEsotg_b_mua, apply(X = (abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's9_silt')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's9_sand')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotg_b_mua)[ncol(EEsotg_b_mua)] = 's9_SoilTexture'
EEsotg_b_mua = cbind(EEsotg_b_mua, apply(X = (abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's109_silt')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's109_sand')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotg_b_mua)[ncol(EEsotg_b_mua)] = 's109_SoilTexture'

EEsotg_b_mua = cbind(EEsotg_b_mua, apply(X = (abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v102_K_absorptance')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v102_K_reflectance')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotg_b_mua)[ncol(EEsotg_b_mua)] = 'v102_K_All'

EEsotg_b_mua = cbind(EEsotg_b_mua, apply(X = (abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_absorptance')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_reflectance')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotg_b_mua)[ncol(EEsotg_b_mua)] = 'v102_PAR_All'

EEsotg_b_mua = cbind(EEsotg_b_mua, apply(X = (abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_fcel')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flab')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotg_b_mua)[ncol(EEsotg_b_mua)] = 'v102_epc.frootlitr_All'
EEsotg_b_mua = cbind(EEsotg_b_mua, apply(X = (abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_fcel')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flab')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotg_b_mua)[ncol(EEsotg_b_mua)] = 'v3_epc.frootlitr_All'

EEsotg_b_mua = cbind(EEsotg_b_mua, apply(X = (abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_fcel')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flab')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotg_b_mua)[ncol(EEsotg_b_mua)] = 'v102_epc.leaflitr_All'
EEsotg_b_mua = cbind(EEsotg_b_mua, apply(X = (abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_fcel')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flab')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotg_b_mua)[ncol(EEsotg_b_mua)] = 'v3_epc.leaflitr_All'

EEsotg_b_mua = cbind(EEsotg_b_mua, apply(X = (abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0_v')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsotg_b_mua)[ncol(EEsotg_b_mua)] = 'Soil8_Ksat'
EEsotg_b_mua = cbind(EEsotg_b_mua, apply(X = (abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0_v')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsotg_b_mua)[ncol(EEsotg_b_mua)] = 'Soil9_Ksat'

EEsotg_b_mua = cbind(EEsotg_b_mua, apply(X = (abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's108_m')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsotg_b_mua)[ncol(EEsotg_b_mua)] = 'Soil8_m'
EEsotg_b_mua = cbind(EEsotg_b_mua, apply(X = (abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's109_m')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsotg_b_mua)[ncol(EEsotg_b_mua)] = 'Soil9_m'

EEsotg_b_mua = cbind(EEsotg_b_mua, apply(X = (abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's108_porosity_0')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsotg_b_mua)[ncol(EEsotg_b_mua)] = 'Soil8_porosity_0'
EEsotg_b_mua = cbind(EEsotg_b_mua, apply(X = (abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's109_porosity_0')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsotg_b_mua)[ncol(EEsotg_b_mua)] = 'Soil9_porosity_0'

EEsotg_b_mua = cbind(EEsotg_b_mua, apply(X = (abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v102_epc.topt')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsotg_b_mua)[ncol(EEsotg_b_mua)] = 'v102_Temp'
EEsotg_b_mua = cbind(EEsotg_b_mua, apply(X = (abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v3_epc.topt')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsotg_b_mua)[ncol(EEsotg_b_mua)] = 'v3_Temp'

EEsotg_b_mua = cbind(EEsotg_b_mua, apply(X = (abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaf_cn')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsotg_b_mua)[ncol(EEsotg_b_mua)] = 'v102_epc.LeafLitrCN'
EEsotg_b_mua = cbind(EEsotg_b_mua, apply(X = (abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaf_cn')])+abs(EEsotg_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsotg_b_mua)[ncol(EEsotg_b_mua)] = 'v3_epc.LeafLitrCN'

#  EEsLNSE_b_mua----
EEsLNSE_b_mua = cbind(EEsLNSE_b_mua, apply(X = (abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's8_silt')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's8_sand')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsLNSE_b_mua)[ncol(EEsLNSE_b_mua)] = 's8_SoilTexture'
EEsLNSE_b_mua = cbind(EEsLNSE_b_mua, apply(X = (abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's108_silt')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's108_sand')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsLNSE_b_mua)[ncol(EEsLNSE_b_mua)] = 's108_SoilTexture'
EEsLNSE_b_mua = cbind(EEsLNSE_b_mua, apply(X = (abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's9_silt')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's9_sand')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsLNSE_b_mua)[ncol(EEsLNSE_b_mua)] = 's9_SoilTexture'
EEsLNSE_b_mua = cbind(EEsLNSE_b_mua, apply(X = (abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's109_silt')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's109_sand')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsLNSE_b_mua)[ncol(EEsLNSE_b_mua)] = 's109_SoilTexture'

EEsLNSE_b_mua = cbind(EEsLNSE_b_mua, apply(X = (abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v102_K_absorptance')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v102_K_reflectance')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsLNSE_b_mua)[ncol(EEsLNSE_b_mua)] = 'v102_K_All'

EEsLNSE_b_mua = cbind(EEsLNSE_b_mua, apply(X = (abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_absorptance')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_reflectance')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsLNSE_b_mua)[ncol(EEsLNSE_b_mua)] = 'v102_PAR_All'

EEsLNSE_b_mua = cbind(EEsLNSE_b_mua, apply(X = (abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_fcel')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flab')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsLNSE_b_mua)[ncol(EEsLNSE_b_mua)] = 'v102_epc.frootlitr_All'
EEsLNSE_b_mua = cbind(EEsLNSE_b_mua, apply(X = (abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_fcel')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flab')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsLNSE_b_mua)[ncol(EEsLNSE_b_mua)] = 'v3_epc.frootlitr_All'

EEsLNSE_b_mua = cbind(EEsLNSE_b_mua, apply(X = (abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_fcel')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flab')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsLNSE_b_mua)[ncol(EEsLNSE_b_mua)] = 'v102_epc.leaflitr_All'
EEsLNSE_b_mua = cbind(EEsLNSE_b_mua, apply(X = (abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_fcel')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flab')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsLNSE_b_mua)[ncol(EEsLNSE_b_mua)] = 'v3_epc.leaflitr_All'

EEsLNSE_b_mua = cbind(EEsLNSE_b_mua, apply(X = (abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0_v')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsLNSE_b_mua)[ncol(EEsLNSE_b_mua)] = 'Soil8_Ksat'
EEsLNSE_b_mua = cbind(EEsLNSE_b_mua, apply(X = (abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0_v')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsLNSE_b_mua)[ncol(EEsLNSE_b_mua)] = 'Soil9_Ksat'

EEsLNSE_b_mua = cbind(EEsLNSE_b_mua, apply(X = (abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's108_m')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsLNSE_b_mua)[ncol(EEsLNSE_b_mua)] = 'Soil8_m'
EEsLNSE_b_mua = cbind(EEsLNSE_b_mua, apply(X = (abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's109_m')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsLNSE_b_mua)[ncol(EEsLNSE_b_mua)] = 'Soil9_m'

EEsLNSE_b_mua = cbind(EEsLNSE_b_mua, apply(X = (abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's108_porosity_0')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsLNSE_b_mua)[ncol(EEsLNSE_b_mua)] = 'Soil8_porosity_0'
EEsLNSE_b_mua = cbind(EEsLNSE_b_mua, apply(X = (abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's109_porosity_0')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsLNSE_b_mua)[ncol(EEsLNSE_b_mua)] = 'Soil9_porosity_0'

EEsLNSE_b_mua = cbind(EEsLNSE_b_mua, apply(X = (abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v102_epc.topt')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsLNSE_b_mua)[ncol(EEsLNSE_b_mua)] = 'v102_Temp'
EEsLNSE_b_mua = cbind(EEsLNSE_b_mua, apply(X = (abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v3_epc.topt')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsLNSE_b_mua)[ncol(EEsLNSE_b_mua)] = 'v3_Temp'

EEsLNSE_b_mua = cbind(EEsLNSE_b_mua, apply(X = (abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaf_cn')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsLNSE_b_mua)[ncol(EEsLNSE_b_mua)] = 'v102_epc.LeafLitrCN'
EEsLNSE_b_mua = cbind(EEsLNSE_b_mua, apply(X = (abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaf_cn')])+abs(EEsLNSE_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsLNSE_b_mua)[ncol(EEsLNSE_b_mua)] = 'v3_epc.LeafLitrCN'

#  EEsNSE_b_mua----
EEsNSE_b_mua = cbind(EEsNSE_b_mua, apply(X = (abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's8_silt')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's8_sand')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsNSE_b_mua)[ncol(EEsNSE_b_mua)] = 's8_SoilTexture'
EEsNSE_b_mua = cbind(EEsNSE_b_mua, apply(X = (abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's108_silt')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's108_sand')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsNSE_b_mua)[ncol(EEsNSE_b_mua)] = 's108_SoilTexture'
EEsNSE_b_mua = cbind(EEsNSE_b_mua, apply(X = (abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's9_silt')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's9_sand')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsNSE_b_mua)[ncol(EEsNSE_b_mua)] = 's9_SoilTexture'
EEsNSE_b_mua = cbind(EEsNSE_b_mua, apply(X = (abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's109_silt')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's109_sand')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsNSE_b_mua)[ncol(EEsNSE_b_mua)] = 's109_SoilTexture'

EEsNSE_b_mua = cbind(EEsNSE_b_mua, apply(X = (abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v102_K_absorptance')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v102_K_reflectance')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsNSE_b_mua)[ncol(EEsNSE_b_mua)] = 'v102_K_All'

EEsNSE_b_mua = cbind(EEsNSE_b_mua, apply(X = (abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_absorptance')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_reflectance')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsNSE_b_mua)[ncol(EEsNSE_b_mua)] = 'v102_PAR_All'

EEsNSE_b_mua = cbind(EEsNSE_b_mua, apply(X = (abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_fcel')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flab')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsNSE_b_mua)[ncol(EEsNSE_b_mua)] = 'v102_epc.frootlitr_All'
EEsNSE_b_mua = cbind(EEsNSE_b_mua, apply(X = (abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_fcel')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flab')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsNSE_b_mua)[ncol(EEsNSE_b_mua)] = 'v3_epc.frootlitr_All'

EEsNSE_b_mua = cbind(EEsNSE_b_mua, apply(X = (abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_fcel')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flab')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsNSE_b_mua)[ncol(EEsNSE_b_mua)] = 'v102_epc.leaflitr_All'
EEsNSE_b_mua = cbind(EEsNSE_b_mua, apply(X = (abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_fcel')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flab')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsNSE_b_mua)[ncol(EEsNSE_b_mua)] = 'v3_epc.leaflitr_All'

EEsNSE_b_mua = cbind(EEsNSE_b_mua, apply(X = (abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0_v')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsNSE_b_mua)[ncol(EEsNSE_b_mua)] = 'Soil8_Ksat'
EEsNSE_b_mua = cbind(EEsNSE_b_mua, apply(X = (abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0_v')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsNSE_b_mua)[ncol(EEsNSE_b_mua)] = 'Soil9_Ksat'

EEsNSE_b_mua = cbind(EEsNSE_b_mua, apply(X = (abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's108_m')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsNSE_b_mua)[ncol(EEsNSE_b_mua)] = 'Soil8_m'
EEsNSE_b_mua = cbind(EEsNSE_b_mua, apply(X = (abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's109_m')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsNSE_b_mua)[ncol(EEsNSE_b_mua)] = 'Soil9_m'

EEsNSE_b_mua = cbind(EEsNSE_b_mua, apply(X = (abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's108_porosity_0')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsNSE_b_mua)[ncol(EEsNSE_b_mua)] = 'Soil8_porosity_0'
EEsNSE_b_mua = cbind(EEsNSE_b_mua, apply(X = (abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's109_porosity_0')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsNSE_b_mua)[ncol(EEsNSE_b_mua)] = 'Soil9_porosity_0'

EEsNSE_b_mua = cbind(EEsNSE_b_mua, apply(X = (abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v102_epc.topt')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsNSE_b_mua)[ncol(EEsNSE_b_mua)] = 'v102_Temp'
EEsNSE_b_mua = cbind(EEsNSE_b_mua, apply(X = (abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v3_epc.topt')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsNSE_b_mua)[ncol(EEsNSE_b_mua)] = 'v3_Temp'

EEsNSE_b_mua = cbind(EEsNSE_b_mua, apply(X = (abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaf_cn')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsNSE_b_mua)[ncol(EEsNSE_b_mua)] = 'v102_epc.LeafLitrCN'
EEsNSE_b_mua = cbind(EEsNSE_b_mua, apply(X = (abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaf_cn')])+abs(EEsNSE_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsNSE_b_mua)[ncol(EEsNSE_b_mua)] = 'v3_epc.LeafLitrCN'

#  EEspBias_b_mua----
EEspBias_b_mua = cbind(EEspBias_b_mua, apply(X = (abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's8_silt')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's8_sand')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEspBias_b_mua)[ncol(EEspBias_b_mua)] = 's8_SoilTexture'
EEspBias_b_mua = cbind(EEspBias_b_mua, apply(X = (abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's108_silt')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's108_sand')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEspBias_b_mua)[ncol(EEspBias_b_mua)] = 's108_SoilTexture'
EEspBias_b_mua = cbind(EEspBias_b_mua, apply(X = (abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's9_silt')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's9_sand')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEspBias_b_mua)[ncol(EEspBias_b_mua)] = 's9_SoilTexture'
EEspBias_b_mua = cbind(EEspBias_b_mua, apply(X = (abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's109_silt')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's109_sand')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEspBias_b_mua)[ncol(EEspBias_b_mua)] = 's109_SoilTexture'

EEspBias_b_mua = cbind(EEspBias_b_mua, apply(X = (abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v102_K_absorptance')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v102_K_reflectance')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEspBias_b_mua)[ncol(EEspBias_b_mua)] = 'v102_K_All'

EEspBias_b_mua = cbind(EEspBias_b_mua, apply(X = (abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_absorptance')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_reflectance')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEspBias_b_mua)[ncol(EEspBias_b_mua)] = 'v102_PAR_All'

EEspBias_b_mua = cbind(EEspBias_b_mua, apply(X = (abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_fcel')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flab')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEspBias_b_mua)[ncol(EEspBias_b_mua)] = 'v102_epc.frootlitr_All'
EEspBias_b_mua = cbind(EEspBias_b_mua, apply(X = (abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_fcel')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flab')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEspBias_b_mua)[ncol(EEspBias_b_mua)] = 'v3_epc.frootlitr_All'

EEspBias_b_mua = cbind(EEspBias_b_mua, apply(X = (abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_fcel')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flab')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEspBias_b_mua)[ncol(EEspBias_b_mua)] = 'v102_epc.leaflitr_All'
EEspBias_b_mua = cbind(EEspBias_b_mua, apply(X = (abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_fcel')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flab')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEspBias_b_mua)[ncol(EEspBias_b_mua)] = 'v3_epc.leaflitr_All'

EEspBias_b_mua = cbind(EEspBias_b_mua, apply(X = (abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0_v')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEspBias_b_mua)[ncol(EEspBias_b_mua)] = 'Soil8_Ksat'
EEspBias_b_mua = cbind(EEspBias_b_mua, apply(X = (abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0_v')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEspBias_b_mua)[ncol(EEspBias_b_mua)] = 'Soil9_Ksat'

EEspBias_b_mua = cbind(EEspBias_b_mua, apply(X = (abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's108_m')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEspBias_b_mua)[ncol(EEspBias_b_mua)] = 'Soil8_m'
EEspBias_b_mua = cbind(EEspBias_b_mua, apply(X = (abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's109_m')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEspBias_b_mua)[ncol(EEspBias_b_mua)] = 'Soil9_m'

EEspBias_b_mua = cbind(EEspBias_b_mua, apply(X = (abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's108_porosity_0')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEspBias_b_mua)[ncol(EEspBias_b_mua)] = 'Soil8_porosity_0'
EEspBias_b_mua = cbind(EEspBias_b_mua, apply(X = (abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's109_porosity_0')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEspBias_b_mua)[ncol(EEspBias_b_mua)] = 'Soil9_porosity_0'

EEspBias_b_mua = cbind(EEspBias_b_mua, apply(X = (abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v102_epc.topt')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEspBias_b_mua)[ncol(EEspBias_b_mua)] = 'v102_Temp'
EEspBias_b_mua = cbind(EEspBias_b_mua, apply(X = (abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v3_epc.topt')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEspBias_b_mua)[ncol(EEspBias_b_mua)] = 'v3_Temp'

EEspBias_b_mua = cbind(EEspBias_b_mua, apply(X = (abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaf_cn')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEspBias_b_mua)[ncol(EEspBias_b_mua)] = 'v102_epc.LeafLitrCN'
EEspBias_b_mua = cbind(EEspBias_b_mua, apply(X = (abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaf_cn')])+abs(EEspBias_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEspBias_b_mua)[ncol(EEspBias_b_mua)] = 'v3_epc.LeafLitrCN'

#  EEsLogL_b_mua----
EEsLogL_b_mua = cbind(EEsLogL_b_mua, apply(X = (abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's8_silt')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's8_sand')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsLogL_b_mua)[ncol(EEsLogL_b_mua)] = 's8_SoilTexture'
EEsLogL_b_mua = cbind(EEsLogL_b_mua, apply(X = (abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's108_silt')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's108_sand')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsLogL_b_mua)[ncol(EEsLogL_b_mua)] = 's108_SoilTexture'
EEsLogL_b_mua = cbind(EEsLogL_b_mua, apply(X = (abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's9_silt')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's9_sand')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsLogL_b_mua)[ncol(EEsLogL_b_mua)] = 's9_SoilTexture'
EEsLogL_b_mua = cbind(EEsLogL_b_mua, apply(X = (abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's109_silt')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's109_sand')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsLogL_b_mua)[ncol(EEsLogL_b_mua)] = 's109_SoilTexture'

EEsLogL_b_mua = cbind(EEsLogL_b_mua, apply(X = (abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v102_K_absorptance')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v102_K_reflectance')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsLogL_b_mua)[ncol(EEsLogL_b_mua)] = 'v102_K_All'

EEsLogL_b_mua = cbind(EEsLogL_b_mua, apply(X = (abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_absorptance')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_reflectance')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsLogL_b_mua)[ncol(EEsLogL_b_mua)] = 'v102_PAR_All'

EEsLogL_b_mua = cbind(EEsLogL_b_mua, apply(X = (abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_fcel')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flab')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsLogL_b_mua)[ncol(EEsLogL_b_mua)] = 'v102_epc.frootlitr_All'
EEsLogL_b_mua = cbind(EEsLogL_b_mua, apply(X = (abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_fcel')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flab')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsLogL_b_mua)[ncol(EEsLogL_b_mua)] = 'v3_epc.frootlitr_All'

EEsLogL_b_mua = cbind(EEsLogL_b_mua, apply(X = (abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_fcel')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flab')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsLogL_b_mua)[ncol(EEsLogL_b_mua)] = 'v102_epc.leaflitr_All'
EEsLogL_b_mua = cbind(EEsLogL_b_mua, apply(X = (abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_fcel')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flab')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsLogL_b_mua)[ncol(EEsLogL_b_mua)] = 'v3_epc.leaflitr_All'

EEsLogL_b_mua = cbind(EEsLogL_b_mua, apply(X = (abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0_v')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsLogL_b_mua)[ncol(EEsLogL_b_mua)] = 'Soil8_Ksat'
EEsLogL_b_mua = cbind(EEsLogL_b_mua, apply(X = (abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0_v')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsLogL_b_mua)[ncol(EEsLogL_b_mua)] = 'Soil9_Ksat'

EEsLogL_b_mua = cbind(EEsLogL_b_mua, apply(X = (abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's108_m')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsLogL_b_mua)[ncol(EEsLogL_b_mua)] = 'Soil8_m'
EEsLogL_b_mua = cbind(EEsLogL_b_mua, apply(X = (abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's109_m')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsLogL_b_mua)[ncol(EEsLogL_b_mua)] = 'Soil9_m'

EEsLogL_b_mua = cbind(EEsLogL_b_mua, apply(X = (abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's108_porosity_0')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsLogL_b_mua)[ncol(EEsLogL_b_mua)] = 'Soil8_porosity_0'
EEsLogL_b_mua = cbind(EEsLogL_b_mua, apply(X = (abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's109_porosity_0')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsLogL_b_mua)[ncol(EEsLogL_b_mua)] = 'Soil9_porosity_0'

EEsLogL_b_mua = cbind(EEsLogL_b_mua, apply(X = (abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v102_epc.topt')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsLogL_b_mua)[ncol(EEsLogL_b_mua)] = 'v102_Temp'
EEsLogL_b_mua = cbind(EEsLogL_b_mua, apply(X = (abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v3_epc.topt')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsLogL_b_mua)[ncol(EEsLogL_b_mua)] = 'v3_Temp'

EEsLogL_b_mua = cbind(EEsLogL_b_mua, apply(X = (abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaf_cn')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsLogL_b_mua)[ncol(EEsLogL_b_mua)] = 'v102_epc.LeafLitrCN'
EEsLogL_b_mua = cbind(EEsLogL_b_mua, apply(X = (abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaf_cn')])+abs(EEsLogL_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsLogL_b_mua)[ncol(EEsLogL_b_mua)] = 'v3_epc.LeafLitrCN'

#  EEsTN05_b_mua----
EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's8_silt')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's8_sand')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 's8_SoilTexture'
EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's108_silt')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's108_sand')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 's108_SoilTexture'
EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's9_silt')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's9_sand')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 's9_SoilTexture'
EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's109_silt')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's109_sand')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 's109_SoilTexture'

EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_K_absorptance')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_K_reflectance')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'v102_K_All'

EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_absorptance')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_reflectance')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'v102_PAR_All'

EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_fcel')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flab')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'v102_epc.frootlitr_All'
EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_fcel')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flab')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'v3_epc.frootlitr_All'

EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_fcel')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flab')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'v102_epc.leaflitr_All'
EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_fcel')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flab')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'v3_epc.leaflitr_All'

EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0_v')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'Soil8_Ksat'
EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0_v')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'Soil9_Ksat'

EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's108_m')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'Soil8_m'
EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's109_m')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'Soil9_m'

EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's108_porosity_0')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'Soil8_porosity_0'
EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's109_porosity_0')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'Soil9_porosity_0'

EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.topt')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'v102_Temp'
EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.topt')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'v3_Temp'

EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaf_cn')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'v102_epc.LeafLitrCN'
EEsTN05_b_mua = cbind(EEsTN05_b_mua, apply(X = (abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaf_cn')])+abs(EEsTN05_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_b_mua)[ncol(EEsTN05_b_mua)] = 'v3_epc.LeafLitrCN'

#  EEsTN95_b_mua----
EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's8_silt')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's8_sand')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 's8_SoilTexture'
EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's108_silt')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's108_sand')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 's108_SoilTexture'
EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's9_silt')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's9_sand')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 's9_SoilTexture'
EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's109_silt')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's109_sand')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 's109_SoilTexture'

EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_K_absorptance')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_K_reflectance')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'v102_K_All'

EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_absorptance')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_reflectance')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'v102_PAR_All'

EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_fcel')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flab')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'v102_epc.frootlitr_All'
EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_fcel')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flab')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'v3_epc.frootlitr_All'

EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_fcel')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flab')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'v102_epc.leaflitr_All'
EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_fcel')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flab')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'v3_epc.leaflitr_All'

EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0_v')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'Soil8_Ksat'
EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0_v')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'Soil9_Ksat'

EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's108_m')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'Soil8_m'
EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's109_m')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'Soil9_m'

EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's108_porosity_0')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'Soil8_porosity_0'
EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's109_porosity_0')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'Soil9_porosity_0'

EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.topt')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'v102_Temp'
EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.topt')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'v3_Temp'

EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaf_cn')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'v102_epc.LeafLitrCN'
EEsTN95_b_mua = cbind(EEsTN95_b_mua, apply(X = (abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaf_cn')])+abs(EEsTN95_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_b_mua)[ncol(EEsTN95_b_mua)] = 'v3_epc.LeafLitrCN'

#  EEsTNMed_b_mua----
EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's8_silt')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's8_sand')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 's8_SoilTexture'
EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's108_silt')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's108_sand')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 's108_SoilTexture'
EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's9_silt')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's9_sand')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 's9_SoilTexture'
EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's109_silt')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's109_sand')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 's109_SoilTexture'

EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_K_absorptance')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_K_reflectance')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'v102_K_All'

EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_absorptance')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_reflectance')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'v102_PAR_All'

EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_fcel')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flab')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'v102_epc.frootlitr_All'
EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_fcel')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flab')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'v3_epc.frootlitr_All'

EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_fcel')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flab')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'v102_epc.leaflitr_All'
EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_fcel')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flab')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'v3_epc.leaflitr_All'

EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0_v')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'Soil8_Ksat'
EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0_v')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'Soil9_Ksat'

EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's108_m')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'Soil8_m'
EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's109_m')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'Soil9_m'

EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's108_porosity_0')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'Soil8_porosity_0'
EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's109_porosity_0')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'Soil9_porosity_0'

EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_epc.topt')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'v102_Temp'
EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v3_epc.topt')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'v3_Temp'

EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaf_cn')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'v102_epc.LeafLitrCN'
EEsTNMed_b_mua = cbind(EEsTNMed_b_mua, apply(X = (abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaf_cn')])+abs(EEsTNMed_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_b_mua)[ncol(EEsTNMed_b_mua)] = 'v3_epc.LeafLitrCN'

#  EEs05gm_b_mua----
EEs05gm_b_mua = cbind(EEs05gm_b_mua, apply(X = (abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's8_silt')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's8_sand')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05gm_b_mua)[ncol(EEs05gm_b_mua)] = 's8_SoilTexture'
EEs05gm_b_mua = cbind(EEs05gm_b_mua, apply(X = (abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's108_silt')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's108_sand')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05gm_b_mua)[ncol(EEs05gm_b_mua)] = 's108_SoilTexture'
EEs05gm_b_mua = cbind(EEs05gm_b_mua, apply(X = (abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's9_silt')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's9_sand')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05gm_b_mua)[ncol(EEs05gm_b_mua)] = 's9_SoilTexture'
EEs05gm_b_mua = cbind(EEs05gm_b_mua, apply(X = (abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's109_silt')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's109_sand')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05gm_b_mua)[ncol(EEs05gm_b_mua)] = 's109_SoilTexture'

#these results are the same as this. Cannot make the ColNames have >1 row because the resulting matrix will have 40*rows to aggregate. 
#apply(apply(X = EEs05gm_b_mua_cons, MARGIN = 1, FUN = AggregateEEs, ColNames = as.matrix(rbind(c('v102_K_absorptance', 'v102_K_reflectance', 'v102_K_transmittance'))), meanabs), 2, mean)
EEs05gm_b_mua = cbind(EEs05gm_b_mua, apply(X = (abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v102_K_absorptance')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v102_K_reflectance')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05gm_b_mua)[ncol(EEs05gm_b_mua)] = 'v102_K_All'

EEs05gm_b_mua = cbind(EEs05gm_b_mua, apply(X = (abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_absorptance')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_reflectance')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05gm_b_mua)[ncol(EEs05gm_b_mua)] = 'v102_PAR_All'

EEs05gm_b_mua = cbind(EEs05gm_b_mua, apply(X = (abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_fcel')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flab')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05gm_b_mua)[ncol(EEs05gm_b_mua)] = 'v102_epc.frootlitr_All'
EEs05gm_b_mua = cbind(EEs05gm_b_mua, apply(X = (abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_fcel')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flab')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05gm_b_mua)[ncol(EEs05gm_b_mua)] = 'v3_epc.frootlitr_All'

EEs05gm_b_mua = cbind(EEs05gm_b_mua, apply(X = (abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_fcel')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flab')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05gm_b_mua)[ncol(EEs05gm_b_mua)] = 'v102_epc.leaflitr_All'
EEs05gm_b_mua = cbind(EEs05gm_b_mua, apply(X = (abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_fcel')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flab')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05gm_b_mua)[ncol(EEs05gm_b_mua)] = 'v3_epc.leaflitr_All'

EEs05gm_b_mua = cbind(EEs05gm_b_mua, apply(X = (abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0_v')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEs05gm_b_mua)[ncol(EEs05gm_b_mua)] = 'Soil8_Ksat'
EEs05gm_b_mua = cbind(EEs05gm_b_mua, apply(X = (abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0_v')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEs05gm_b_mua)[ncol(EEs05gm_b_mua)] = 'Soil9_Ksat'

EEs05gm_b_mua = cbind(EEs05gm_b_mua, apply(X = (abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's108_m')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05gm_b_mua)[ncol(EEs05gm_b_mua)] = 'Soil8_m'
EEs05gm_b_mua = cbind(EEs05gm_b_mua, apply(X = (abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's109_m')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05gm_b_mua)[ncol(EEs05gm_b_mua)] = 'Soil9_m'

EEs05gm_b_mua = cbind(EEs05gm_b_mua, apply(X = (abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's108_porosity_0')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05gm_b_mua)[ncol(EEs05gm_b_mua)] = 'Soil8_porosity_0'
EEs05gm_b_mua = cbind(EEs05gm_b_mua, apply(X = (abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's109_porosity_0')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05gm_b_mua)[ncol(EEs05gm_b_mua)] = 'Soil9_porosity_0'

EEs05gm_b_mua = cbind(EEs05gm_b_mua, apply(X = (abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v102_epc.topt')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05gm_b_mua)[ncol(EEs05gm_b_mua)] = 'v102_Temp'
EEs05gm_b_mua = cbind(EEs05gm_b_mua, apply(X = (abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v3_epc.topt')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05gm_b_mua)[ncol(EEs05gm_b_mua)] = 'v3_Temp'

EEs05gm_b_mua = cbind(EEs05gm_b_mua, apply(X = (abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaf_cn')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05gm_b_mua)[ncol(EEs05gm_b_mua)] = 'v102_epc.LeafLitrCN'
EEs05gm_b_mua = cbind(EEs05gm_b_mua, apply(X = (abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaf_cn')])+abs(EEs05gm_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05gm_b_mua)[ncol(EEs05gm_b_mua)] = 'v3_epc.LeafLitrCN'

#  EEs95m_b_mua----
EEs95m_b_mua = cbind(EEs95m_b_mua, apply(X = (abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's8_silt')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's8_sand')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95m_b_mua)[ncol(EEs95m_b_mua)] = 's8_SoilTexture'
EEs95m_b_mua = cbind(EEs95m_b_mua, apply(X = (abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's108_silt')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's108_sand')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95m_b_mua)[ncol(EEs95m_b_mua)] = 's108_SoilTexture'
EEs95m_b_mua = cbind(EEs95m_b_mua, apply(X = (abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's9_silt')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's9_sand')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95m_b_mua)[ncol(EEs95m_b_mua)] = 's9_SoilTexture'
EEs95m_b_mua = cbind(EEs95m_b_mua, apply(X = (abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's109_silt')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's109_sand')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95m_b_mua)[ncol(EEs95m_b_mua)] = 's109_SoilTexture'

EEs95m_b_mua = cbind(EEs95m_b_mua, apply(X = (abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v102_K_absorptance')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v102_K_reflectance')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95m_b_mua)[ncol(EEs95m_b_mua)] = 'v102_K_All'

EEs95m_b_mua = cbind(EEs95m_b_mua, apply(X = (abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_absorptance')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_reflectance')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95m_b_mua)[ncol(EEs95m_b_mua)] = 'v102_PAR_All'

EEs95m_b_mua = cbind(EEs95m_b_mua, apply(X = (abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_fcel')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flab')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95m_b_mua)[ncol(EEs95m_b_mua)] = 'v102_epc.frootlitr_All'
EEs95m_b_mua = cbind(EEs95m_b_mua, apply(X = (abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_fcel')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flab')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95m_b_mua)[ncol(EEs95m_b_mua)] = 'v3_epc.frootlitr_All'

EEs95m_b_mua = cbind(EEs95m_b_mua, apply(X = (abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_fcel')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flab')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95m_b_mua)[ncol(EEs95m_b_mua)] = 'v102_epc.leaflitr_All'
EEs95m_b_mua = cbind(EEs95m_b_mua, apply(X = (abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_fcel')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flab')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95m_b_mua)[ncol(EEs95m_b_mua)] = 'v3_epc.leaflitr_All'

EEs95m_b_mua = cbind(EEs95m_b_mua, apply(X = (abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0_v')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEs95m_b_mua)[ncol(EEs95m_b_mua)] = 'Soil8_Ksat'
EEs95m_b_mua = cbind(EEs95m_b_mua, apply(X = (abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0_v')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEs95m_b_mua)[ncol(EEs95m_b_mua)] = 'Soil9_Ksat'

EEs95m_b_mua = cbind(EEs95m_b_mua, apply(X = (abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's108_m')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95m_b_mua)[ncol(EEs95m_b_mua)] = 'Soil8_m'
EEs95m_b_mua = cbind(EEs95m_b_mua, apply(X = (abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's109_m')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95m_b_mua)[ncol(EEs95m_b_mua)] = 'Soil9_m'

EEs95m_b_mua = cbind(EEs95m_b_mua, apply(X = (abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's108_porosity_0')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95m_b_mua)[ncol(EEs95m_b_mua)] = 'Soil8_porosity_0'
EEs95m_b_mua = cbind(EEs95m_b_mua, apply(X = (abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's109_porosity_0')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95m_b_mua)[ncol(EEs95m_b_mua)] = 'Soil9_porosity_0'

EEs95m_b_mua = cbind(EEs95m_b_mua, apply(X = (abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v102_epc.topt')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95m_b_mua)[ncol(EEs95m_b_mua)] = 'v102_Temp'
EEs95m_b_mua = cbind(EEs95m_b_mua, apply(X = (abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v3_epc.topt')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95m_b_mua)[ncol(EEs95m_b_mua)] = 'v3_Temp'

EEs95m_b_mua = cbind(EEs95m_b_mua, apply(X = (abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaf_cn')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95m_b_mua)[ncol(EEs95m_b_mua)] = 'v102_epc.LeafLitrCN'
EEs95m_b_mua = cbind(EEs95m_b_mua, apply(X = (abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaf_cn')])+abs(EEs95m_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95m_b_mua)[ncol(EEs95m_b_mua)] = 'v3_epc.LeafLitrCN'

#  EEsotgm_b_mua----
EEsotgm_b_mua = cbind(EEsotgm_b_mua, apply(X = (abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's8_silt')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's8_sand')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotgm_b_mua)[ncol(EEsotgm_b_mua)] = 's8_SoilTexture'
EEsotgm_b_mua = cbind(EEsotgm_b_mua, apply(X = (abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's108_silt')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's108_sand')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotgm_b_mua)[ncol(EEsotgm_b_mua)] = 's108_SoilTexture'
EEsotgm_b_mua = cbind(EEsotgm_b_mua, apply(X = (abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's9_silt')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's9_sand')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotgm_b_mua)[ncol(EEsotgm_b_mua)] = 's9_SoilTexture'
EEsotgm_b_mua = cbind(EEsotgm_b_mua, apply(X = (abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's109_silt')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's109_sand')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotgm_b_mua)[ncol(EEsotgm_b_mua)] = 's109_SoilTexture'

EEsotgm_b_mua = cbind(EEsotgm_b_mua, apply(X = (abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v102_K_absorptance')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v102_K_reflectance')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotgm_b_mua)[ncol(EEsotgm_b_mua)] = 'v102_K_All'

EEsotgm_b_mua = cbind(EEsotgm_b_mua, apply(X = (abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_absorptance')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_reflectance')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotgm_b_mua)[ncol(EEsotgm_b_mua)] = 'v102_PAR_All'

EEsotgm_b_mua = cbind(EEsotgm_b_mua, apply(X = (abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_fcel')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flab')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotgm_b_mua)[ncol(EEsotgm_b_mua)] = 'v102_epc.frootlitr_All'
EEsotgm_b_mua = cbind(EEsotgm_b_mua, apply(X = (abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_fcel')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flab')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotgm_b_mua)[ncol(EEsotgm_b_mua)] = 'v3_epc.frootlitr_All'

EEsotgm_b_mua = cbind(EEsotgm_b_mua, apply(X = (abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_fcel')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flab')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotgm_b_mua)[ncol(EEsotgm_b_mua)] = 'v102_epc.leaflitr_All'
EEsotgm_b_mua = cbind(EEsotgm_b_mua, apply(X = (abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_fcel')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flab')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotgm_b_mua)[ncol(EEsotgm_b_mua)] = 'v3_epc.leaflitr_All'

EEsotgm_b_mua = cbind(EEsotgm_b_mua, apply(X = (abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's108_Ksat_0_v')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsotgm_b_mua)[ncol(EEsotgm_b_mua)] = 'Soil8_Ksat'
EEsotgm_b_mua = cbind(EEsotgm_b_mua, apply(X = (abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's109_Ksat_0_v')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsotgm_b_mua)[ncol(EEsotgm_b_mua)] = 'Soil9_Ksat'

EEsotgm_b_mua = cbind(EEsotgm_b_mua, apply(X = (abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's108_m')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsotgm_b_mua)[ncol(EEsotgm_b_mua)] = 'Soil8_m'
EEsotgm_b_mua = cbind(EEsotgm_b_mua, apply(X = (abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's109_m')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsotgm_b_mua)[ncol(EEsotgm_b_mua)] = 'Soil9_m'

EEsotgm_b_mua = cbind(EEsotgm_b_mua, apply(X = (abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's108_porosity_0')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsotgm_b_mua)[ncol(EEsotgm_b_mua)] = 'Soil8_porosity_0'
EEsotgm_b_mua = cbind(EEsotgm_b_mua, apply(X = (abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's109_porosity_0')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsotgm_b_mua)[ncol(EEsotgm_b_mua)] = 'Soil9_porosity_0'

EEsotgm_b_mua = cbind(EEsotgm_b_mua, apply(X = (abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v102_epc.topt')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsotgm_b_mua)[ncol(EEsotgm_b_mua)] = 'v102_Temp'
EEsotgm_b_mua = cbind(EEsotgm_b_mua, apply(X = (abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v3_epc.topt')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsotgm_b_mua)[ncol(EEsotgm_b_mua)] = 'v3_Temp'

EEsotgm_b_mua = cbind(EEsotgm_b_mua, apply(X = (abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaf_cn')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsotgm_b_mua)[ncol(EEsotgm_b_mua)] = 'v102_epc.LeafLitrCN'
EEsotgm_b_mua = cbind(EEsotgm_b_mua, apply(X = (abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaf_cn')])+abs(EEsotgm_b_mua_cons[,,which(ColsAggregated == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsotgm_b_mua)[ncol(EEsotgm_b_mua)] = 'v3_epc.LeafLitrCN'

#  EEs05_h_mua----
EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's8_silt')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's8_sand')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 's8_SoilTexture'
EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's108_silt')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's108_sand')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 's108_SoilTexture'
EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's9_silt')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's9_sand')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 's9_SoilTexture'
EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's109_silt')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's109_sand')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 's109_SoilTexture'

EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_K_absorptance')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_K_reflectance')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'v102_K_All'

EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_PAR_absorptance')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_PAR_reflectance')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'v102_PAR_All'

EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_epc.frootlitr_fcel')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_epc.frootlitr_flab')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'v102_epc.frootlitr_All'
EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v3_epc.frootlitr_fcel')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v3_epc.frootlitr_flab')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'v3_epc.frootlitr_All'

EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_epc.leaflitr_fcel')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_epc.leaflitr_flab')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'v102_epc.leaflitr_All'
EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v3_epc.leaflitr_fcel')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v3_epc.leaflitr_flab')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'v3_epc.leaflitr_All'

EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's108_Ksat_0')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's8_Ksat_0')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's108_Ksat_0_v')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'Soil8_Ksat'
EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's109_Ksat_0')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's9_Ksat_0')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's109_Ksat_0_v')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'Soil9_Ksat'

EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's108_m')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'Soil8_m'
EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's109_m')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'Soil9_m'

EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's108_porosity_0')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'Soil8_porosity_0'
EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's109_porosity_0')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'Soil9_porosity_0'

EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_epc.topt')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'v102_Temp'
EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v3_epc.topt')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'v3_Temp'

EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_epc.leaf_cn')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'v102_epc.LeafLitrCN'
EEs05_h_mua = cbind(EEs05_h_mua, apply(X = (abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v3_epc.leaf_cn')])+abs(EEs05_h_mua_cons[,,which(dimnames(EEs05_h_mua_cons)$Cols == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05_h_mua)[ncol(EEs05_h_mua)] = 'v3_epc.LeafLitrCN'

#  EEs95_h_mua----
EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's8_silt')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's8_sand')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 's8_SoilTexture'
EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's108_silt')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's108_sand')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 's108_SoilTexture'
EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's9_silt')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's9_sand')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 's9_SoilTexture'
EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's109_silt')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's109_sand')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 's109_SoilTexture'

EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_K_absorptance')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_K_reflectance')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'v102_K_All'

EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_PAR_absorptance')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_PAR_reflectance')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'v102_PAR_All'

EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_epc.frootlitr_fcel')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_epc.frootlitr_flab')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'v102_epc.frootlitr_All'
EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v3_epc.frootlitr_fcel')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v3_epc.frootlitr_flab')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'v3_epc.frootlitr_All'

EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_epc.leaflitr_fcel')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_epc.leaflitr_flab')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'v102_epc.leaflitr_All'
EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v3_epc.leaflitr_fcel')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v3_epc.leaflitr_flab')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'v3_epc.leaflitr_All'

EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's108_Ksat_0')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's8_Ksat_0')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's108_Ksat_0_v')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'Soil8_Ksat'
EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's109_Ksat_0')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's9_Ksat_0')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's109_Ksat_0_v')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'Soil9_Ksat'

EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's108_m')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'Soil8_m'
EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's109_m')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'Soil9_m'

EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's108_porosity_0')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'Soil8_porosity_0'
EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's109_porosity_0')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'Soil9_porosity_0'

EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_epc.topt')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'v102_Temp'
EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v3_epc.topt')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'v3_Temp'

EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_epc.leaf_cn')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'v102_epc.LeafLitrCN'
EEs95_h_mua = cbind(EEs95_h_mua, apply(X = (abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v3_epc.leaf_cn')])+abs(EEs95_h_mua_cons[,,which(dimnames(EEs95_h_mua_cons)$Cols == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs95_h_mua)[ncol(EEs95_h_mua)] = 'v3_epc.LeafLitrCN'
#  EEsot_h_mua----
EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's8_silt')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's8_sand')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 's8_SoilTexture'
EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's108_silt')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's108_sand')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 's108_SoilTexture'
EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's9_silt')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's9_sand')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 's9_SoilTexture'
EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's109_silt')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's109_sand')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 's109_SoilTexture'

EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_K_absorptance')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_K_reflectance')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'v102_K_All'

EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_PAR_absorptance')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_PAR_reflectance')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'v102_PAR_All'

EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_epc.frootlitr_fcel')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_epc.frootlitr_flab')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'v102_epc.frootlitr_All'
EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v3_epc.frootlitr_fcel')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v3_epc.frootlitr_flab')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'v3_epc.frootlitr_All'

EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_epc.leaflitr_fcel')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_epc.leaflitr_flab')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'v102_epc.leaflitr_All'
EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v3_epc.leaflitr_fcel')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v3_epc.leaflitr_flab')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'v3_epc.leaflitr_All'

EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's108_Ksat_0')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's8_Ksat_0')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's108_Ksat_0_v')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'Soil8_Ksat'
EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's109_Ksat_0')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's9_Ksat_0')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's109_Ksat_0_v')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'Soil9_Ksat'

EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's108_m')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'Soil8_m'
EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's109_m')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'Soil9_m'

EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's108_porosity_0')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'Soil8_porosity_0'
EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's109_porosity_0')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'Soil9_porosity_0'

EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_epc.topt')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'v102_Temp'
EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v3_epc.topt')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'v3_Temp'

EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_epc.leaf_cn')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'v102_epc.LeafLitrCN'
EEsot_h_mua = cbind(EEsot_h_mua, apply(X = (abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v3_epc.leaf_cn')])+abs(EEsot_h_mua_cons[,,which(dimnames(EEsot_h_mua_cons)$Cols == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsot_h_mua)[ncol(EEsot_h_mua)] = 'v3_epc.LeafLitrCN'
#  EEs05g_h_mua----
EEs05g_h_mua = cbind(EEs05g_h_mua, apply(X = (abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's8_silt')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's8_sand')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05g_h_mua)[ncol(EEs05g_h_mua)] = 's8_SoilTexture'
EEs05g_h_mua = cbind(EEs05g_h_mua, apply(X = (abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's108_silt')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's108_sand')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05g_h_mua)[ncol(EEs05g_h_mua)] = 's108_SoilTexture'
EEs05g_h_mua = cbind(EEs05g_h_mua, apply(X = (abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's9_silt')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's9_sand')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05g_h_mua)[ncol(EEs05g_h_mua)] = 's9_SoilTexture'
EEs05g_h_mua = cbind(EEs05g_h_mua, apply(X = (abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's109_silt')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's109_sand')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05g_h_mua)[ncol(EEs05g_h_mua)] = 's109_SoilTexture'

EEs05g_h_mua = cbind(EEs05g_h_mua, apply(X = (abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v102_K_absorptance')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v102_K_reflectance')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05g_h_mua)[ncol(EEs05g_h_mua)] = 'v102_K_All'

EEs05g_h_mua = cbind(EEs05g_h_mua, apply(X = (abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v102_PAR_absorptance')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v102_PAR_reflectance')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05g_h_mua)[ncol(EEs05g_h_mua)] = 'v102_PAR_All'

EEs05g_h_mua = cbind(EEs05g_h_mua, apply(X = (abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v102_epc.frootlitr_fcel')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v102_epc.frootlitr_flab')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05g_h_mua)[ncol(EEs05g_h_mua)] = 'v102_epc.frootlitr_All'
EEs05g_h_mua = cbind(EEs05g_h_mua, apply(X = (abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v3_epc.frootlitr_fcel')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v3_epc.frootlitr_flab')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05g_h_mua)[ncol(EEs05g_h_mua)] = 'v3_epc.frootlitr_All'

EEs05g_h_mua = cbind(EEs05g_h_mua, apply(X = (abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v102_epc.leaflitr_fcel')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v102_epc.leaflitr_flab')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05g_h_mua)[ncol(EEs05g_h_mua)] = 'v102_epc.leaflitr_All'
EEs05g_h_mua = cbind(EEs05g_h_mua, apply(X = (abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v3_epc.leaflitr_fcel')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v3_epc.leaflitr_flab')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEs05g_h_mua)[ncol(EEs05g_h_mua)] = 'v3_epc.leaflitr_All'

EEs05g_h_mua = cbind(EEs05g_h_mua, apply(X = (abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's108_Ksat_0')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's8_Ksat_0')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's108_Ksat_0_v')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEs05g_h_mua)[ncol(EEs05g_h_mua)] = 'Soil8_Ksat'
EEs05g_h_mua = cbind(EEs05g_h_mua, apply(X = (abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's109_Ksat_0')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's9_Ksat_0')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's109_Ksat_0_v')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEs05g_h_mua)[ncol(EEs05g_h_mua)] = 'Soil9_Ksat'

EEs05g_h_mua = cbind(EEs05g_h_mua, apply(X = (abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's108_m')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05g_h_mua)[ncol(EEs05g_h_mua)] = 'Soil8_m'
EEs05g_h_mua = cbind(EEs05g_h_mua, apply(X = (abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's109_m')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05g_h_mua)[ncol(EEs05g_h_mua)] = 'Soil9_m'

EEs05g_h_mua = cbind(EEs05g_h_mua, apply(X = (abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's108_porosity_0')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05g_h_mua)[ncol(EEs05g_h_mua)] = 'Soil8_porosity_0'
EEs05g_h_mua = cbind(EEs05g_h_mua, apply(X = (abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's109_porosity_0')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05g_h_mua)[ncol(EEs05g_h_mua)] = 'Soil9_porosity_0'

EEs05g_h_mua = cbind(EEs05g_h_mua, apply(X = (abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v102_epc.topt')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05g_h_mua)[ncol(EEs05g_h_mua)] = 'v102_Temp'
EEs05g_h_mua = cbind(EEs05g_h_mua, apply(X = (abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v3_epc.topt')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05g_h_mua)[ncol(EEs05g_h_mua)] = 'v3_Temp'

EEs05g_h_mua = cbind(EEs05g_h_mua, apply(X = (abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v102_epc.leaf_cn')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05g_h_mua)[ncol(EEs05g_h_mua)] = 'v102_epc.LeafLitrCN'
EEs05g_h_mua = cbind(EEs05g_h_mua, apply(X = (abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v3_epc.leaf_cn')])+abs(EEs05g_h_mua_cons[,,which(dimnames(EEs05g_h_mua_cons)$Cols == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEs05g_h_mua)[ncol(EEs05g_h_mua)] = 'v3_epc.LeafLitrCN'

#  EEsotg_h_mua----
EEsotg_h_mua = cbind(EEsotg_h_mua, apply(X = (abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's8_silt')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's8_sand')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotg_h_mua)[ncol(EEsotg_h_mua)] = 's8_SoilTexture'
EEsotg_h_mua = cbind(EEsotg_h_mua, apply(X = (abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's108_silt')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's108_sand')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotg_h_mua)[ncol(EEsotg_h_mua)] = 's108_SoilTexture'
EEsotg_h_mua = cbind(EEsotg_h_mua, apply(X = (abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's9_silt')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's9_sand')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotg_h_mua)[ncol(EEsotg_h_mua)] = 's9_SoilTexture'
EEsotg_h_mua = cbind(EEsotg_h_mua, apply(X = (abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's109_silt')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's109_sand')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotg_h_mua)[ncol(EEsotg_h_mua)] = 's109_SoilTexture'

EEsotg_h_mua = cbind(EEsotg_h_mua, apply(X = (abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v102_K_absorptance')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v102_K_reflectance')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotg_h_mua)[ncol(EEsotg_h_mua)] = 'v102_K_All'

EEsotg_h_mua = cbind(EEsotg_h_mua, apply(X = (abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v102_PAR_absorptance')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v102_PAR_reflectance')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotg_h_mua)[ncol(EEsotg_h_mua)] = 'v102_PAR_All'

EEsotg_h_mua = cbind(EEsotg_h_mua, apply(X = (abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v102_epc.frootlitr_fcel')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v102_epc.frootlitr_flab')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotg_h_mua)[ncol(EEsotg_h_mua)] = 'v102_epc.frootlitr_All'
EEsotg_h_mua = cbind(EEsotg_h_mua, apply(X = (abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v3_epc.frootlitr_fcel')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v3_epc.frootlitr_flab')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotg_h_mua)[ncol(EEsotg_h_mua)] = 'v3_epc.frootlitr_All'

EEsotg_h_mua = cbind(EEsotg_h_mua, apply(X = (abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v102_epc.leaflitr_fcel')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v102_epc.leaflitr_flab')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotg_h_mua)[ncol(EEsotg_h_mua)] = 'v102_epc.leaflitr_All'
EEsotg_h_mua = cbind(EEsotg_h_mua, apply(X = (abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v3_epc.leaflitr_fcel')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v3_epc.leaflitr_flab')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsotg_h_mua)[ncol(EEsotg_h_mua)] = 'v3_epc.leaflitr_All'

EEsotg_h_mua = cbind(EEsotg_h_mua, apply(X = (abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's108_Ksat_0')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's8_Ksat_0')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's108_Ksat_0_v')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsotg_h_mua)[ncol(EEsotg_h_mua)] = 'Soil8_Ksat'
EEsotg_h_mua = cbind(EEsotg_h_mua, apply(X = (abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's109_Ksat_0')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's9_Ksat_0')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's109_Ksat_0_v')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsotg_h_mua)[ncol(EEsotg_h_mua)] = 'Soil9_Ksat'

EEsotg_h_mua = cbind(EEsotg_h_mua, apply(X = (abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's108_m')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsotg_h_mua)[ncol(EEsotg_h_mua)] = 'Soil8_m'
EEsotg_h_mua = cbind(EEsotg_h_mua, apply(X = (abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's109_m')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsotg_h_mua)[ncol(EEsotg_h_mua)] = 'Soil9_m'

EEsotg_h_mua = cbind(EEsotg_h_mua, apply(X = (abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's108_porosity_0')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsotg_h_mua)[ncol(EEsotg_h_mua)] = 'Soil8_porosity_0'
EEsotg_h_mua = cbind(EEsotg_h_mua, apply(X = (abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's109_porosity_0')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsotg_h_mua)[ncol(EEsotg_h_mua)] = 'Soil9_porosity_0'

EEsotg_h_mua = cbind(EEsotg_h_mua, apply(X = (abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v102_epc.topt')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsotg_h_mua)[ncol(EEsotg_h_mua)] = 'v102_Temp'
EEsotg_h_mua = cbind(EEsotg_h_mua, apply(X = (abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v3_epc.topt')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsotg_h_mua)[ncol(EEsotg_h_mua)] = 'v3_Temp'

EEsotg_h_mua = cbind(EEsotg_h_mua, apply(X = (abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v102_epc.leaf_cn')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsotg_h_mua)[ncol(EEsotg_h_mua)] = 'v102_epc.LeafLitrCN'
EEsotg_h_mua = cbind(EEsotg_h_mua, apply(X = (abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v3_epc.leaf_cn')])+abs(EEsotg_h_mua_cons[,,which(dimnames(EEsotg_h_mua_cons)$Cols == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsotg_h_mua)[ncol(EEsotg_h_mua)] = 'v3_epc.LeafLitrCN'

#  EEsTN05_h_mua----
EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's8_silt')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's8_sand')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 's8_SoilTexture'
EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's108_silt')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's108_sand')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 's108_SoilTexture'
EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's9_silt')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's9_sand')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 's9_SoilTexture'
EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's109_silt')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's109_sand')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 's109_SoilTexture'

EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_K_absorptance')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_K_reflectance')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'v102_K_All'

EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_PAR_absorptance')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_PAR_reflectance')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'v102_PAR_All'

EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_epc.frootlitr_fcel')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_epc.frootlitr_flab')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'v102_epc.frootlitr_All'
EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v3_epc.frootlitr_fcel')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v3_epc.frootlitr_flab')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'v3_epc.frootlitr_All'

EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_epc.leaflitr_fcel')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_epc.leaflitr_flab')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'v102_epc.leaflitr_All'
EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v3_epc.leaflitr_fcel')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v3_epc.leaflitr_flab')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'v3_epc.leaflitr_All'

EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's108_Ksat_0')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's8_Ksat_0')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's108_Ksat_0_v')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'Soil8_Ksat'
EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's109_Ksat_0')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's9_Ksat_0')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's109_Ksat_0_v')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'Soil9_Ksat'

EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's108_m')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'Soil8_m'
EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's109_m')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'Soil9_m'

EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's108_porosity_0')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'Soil8_porosity_0'
EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's109_porosity_0')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'Soil9_porosity_0'

EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_epc.topt')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'v102_Temp'
EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v3_epc.topt')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'v3_Temp'

EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_epc.leaf_cn')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'v102_epc.LeafLitrCN'
EEsTN05_h_mua = cbind(EEsTN05_h_mua, apply(X = (abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v3_epc.leaf_cn')])+abs(EEsTN05_h_mua_cons[,,which(dimnames(EEsTN05_h_mua_cons)$Cols == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN05_h_mua)[ncol(EEsTN05_h_mua)] = 'v3_epc.LeafLitrCN'
#  EEsTN95_h_mua----
EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's8_silt')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's8_sand')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 's8_SoilTexture'
EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's108_silt')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's108_sand')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 's108_SoilTexture'
EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's9_silt')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's9_sand')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 's9_SoilTexture'
EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's109_silt')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's109_sand')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 's109_SoilTexture'

EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_K_absorptance')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_K_reflectance')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'v102_K_All'

EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_PAR_absorptance')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_PAR_reflectance')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'v102_PAR_All'

EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_epc.frootlitr_fcel')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_epc.frootlitr_flab')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'v102_epc.frootlitr_All'
EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v3_epc.frootlitr_fcel')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v3_epc.frootlitr_flab')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'v3_epc.frootlitr_All'

EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_epc.leaflitr_fcel')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_epc.leaflitr_flab')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'v102_epc.leaflitr_All'
EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v3_epc.leaflitr_fcel')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v3_epc.leaflitr_flab')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'v3_epc.leaflitr_All'

EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's108_Ksat_0')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's8_Ksat_0')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's108_Ksat_0_v')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'Soil8_Ksat'
EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's109_Ksat_0')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's9_Ksat_0')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's109_Ksat_0_v')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'Soil9_Ksat'

EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's108_m')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'Soil8_m'
EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's109_m')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'Soil9_m'

EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's108_porosity_0')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'Soil8_porosity_0'
EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's109_porosity_0')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'Soil9_porosity_0'

EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_epc.topt')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'v102_Temp'
EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v3_epc.topt')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'v3_Temp'

EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_epc.leaf_cn')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'v102_epc.LeafLitrCN'
EEsTN95_h_mua = cbind(EEsTN95_h_mua, apply(X = (abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v3_epc.leaf_cn')])+abs(EEsTN95_h_mua_cons[,,which(dimnames(EEsTN95_h_mua_cons)$Cols == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTN95_h_mua)[ncol(EEsTN95_h_mua)] = 'v3_epc.LeafLitrCN'
#  EEsTNMed_h_mua----
EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's8_silt')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's8_sand')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's8_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 's8_SoilTexture'
EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's108_silt')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's108_sand')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's108_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 's108_SoilTexture'
EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's9_silt')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's9_sand')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's9_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 's9_SoilTexture'
EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's109_silt')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's109_sand')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's109_clay')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 's109_SoilTexture'

EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_K_absorptance')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_K_reflectance')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_K_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'v102_K_All'

EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_PAR_absorptance')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_PAR_reflectance')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_PAR_transmittance')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'v102_PAR_All'

EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_epc.frootlitr_fcel')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_epc.frootlitr_flab')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'v102_epc.frootlitr_All'
EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v3_epc.frootlitr_fcel')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v3_epc.frootlitr_flab')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v3_epc.frootlitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'v3_epc.frootlitr_All'

EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_epc.leaflitr_fcel')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_epc.leaflitr_flab')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'v102_epc.leaflitr_All'
EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v3_epc.leaflitr_fcel')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v3_epc.leaflitr_flab')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v3_epc.leaflitr_flig')]))/3, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'v3_epc.leaflitr_All'

EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's108_Ksat_0')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's8_Ksat_0')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's108_Ksat_0_v')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's8_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'Soil8_Ksat'
EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's109_Ksat_0')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's9_Ksat_0')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's109_Ksat_0_v')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's9_Ksat_0_v')]))/4, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'Soil9_Ksat'

EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's108_m')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's8_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'Soil8_m'
EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's109_m')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's9_m')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'Soil9_m'

EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's108_porosity_0')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's8_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'Soil8_porosity_0'
EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's109_porosity_0')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 's9_porosity_0')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'Soil9_porosity_0'

EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_epc.topt')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'v102_Temp'
EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v3_epc.topt')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v3_epc.tmax')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'v3_Temp'

EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_epc.leaf_cn')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v102_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'v102_epc.LeafLitrCN'
EEsTNMed_h_mua = cbind(EEsTNMed_h_mua, apply(X = (abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v3_epc.leaf_cn')])+abs(EEsTNMed_h_mua_cons[,,which(dimnames(EEsTNMed_h_mua_cons)$Cols == 'v3_epc.leaflitr_cn')]))/2, MARGIN = 1, FUN = mean))
colnames(EEsTNMed_h_mua)[ncol(EEsTNMed_h_mua)] = 'v3_epc.LeafLitrCN'

# Compute the mean, 5th, and 95th quantiles of the EE distributions----
EEs05_b_mua_m = apply(X = EEs05_b_mua, MARGIN = 2, FUN = mean)
EEs05_b_mua_05 = apply(X = EEs05_b_mua, MARGIN = 2, FUN = quantile, probs = 0.05)
EEs05_b_mua_95 = apply(X = EEs05_b_mua, MARGIN = 2, FUN = quantile, probs = 0.95)

EEs95_b_mua_m = apply(X = EEs95_b_mua, MARGIN = 2, FUN = mean)
EEs95_b_mua_05 = apply(X = EEs95_b_mua, MARGIN = 2, FUN = quantile, probs = 0.05)
EEs95_b_mua_95 = apply(X = EEs95_b_mua, MARGIN = 2, FUN = quantile, probs = 0.95)

EEsot_b_mua_m = apply(X = EEsot_b_mua, MARGIN = 2, FUN = mean)
EEsot_b_mua_05 = apply(X = EEsot_b_mua, MARGIN = 2, FUN = quantile, probs = 0.05)
EEsot_b_mua_95 = apply(X = EEsot_b_mua, MARGIN = 2, FUN = quantile, probs = 0.95)

EEs05g_b_mua_m = apply(X = EEs05g_b_mua, MARGIN = 2, FUN = mean)
EEs05g_b_mua_05 = apply(X = EEs05g_b_mua, MARGIN = 2, FUN = quantile, probs = 0.05)
EEs05g_b_mua_95 = apply(X = EEs05g_b_mua, MARGIN = 2, FUN = quantile, probs = 0.95)

EEsotg_b_mua_m = apply(X = EEsotg_b_mua, MARGIN = 2, FUN = mean)
EEsotg_b_mua_05 = apply(X = EEsotg_b_mua, MARGIN = 2, FUN = quantile, probs = 0.05)
EEsotg_b_mua_95 = apply(X = EEsotg_b_mua, MARGIN = 2, FUN = quantile, probs = 0.95)

EEs05gm_b_mua_m = apply(X = EEs05gm_b_mua, MARGIN = 2, FUN = mean)
EEs05gm_b_mua_05 = apply(X = EEs05gm_b_mua, MARGIN = 2, FUN = quantile, probs = 0.05)
EEs05gm_b_mua_95 = apply(X = EEs05gm_b_mua, MARGIN = 2, FUN = quantile, probs = 0.95)

EEs95m_b_mua_m = apply(X = EEs95m_b_mua, MARGIN = 2, FUN = mean)
EEs95m_b_mua_05 = apply(X = EEs95m_b_mua, MARGIN = 2, FUN = quantile, probs = 0.05)
EEs95m_b_mua_95 = apply(X = EEs95m_b_mua, MARGIN = 2, FUN = quantile, probs = 0.95)

EEsotgm_b_mua_m = apply(X = EEsotgm_b_mua, MARGIN = 2, FUN = mean)
EEsotgm_b_mua_05 = apply(X = EEsotgm_b_mua, MARGIN = 2, FUN = quantile, probs = 0.05)
EEsotgm_b_mua_95 = apply(X = EEsotgm_b_mua, MARGIN = 2, FUN = quantile, probs = 0.95)

EEsLNSE_b_mua_m = apply(X = EEsLNSE_b_mua, MARGIN = 2, FUN = mean)
EEsLNSE_b_mua_05 = apply(X = EEsLNSE_b_mua, MARGIN = 2, FUN = quantile, probs = 0.05)
EEsLNSE_b_mua_95 = apply(X = EEsLNSE_b_mua, MARGIN = 2, FUN = quantile, probs = 0.95)

EEsNSE_b_mua_m = apply(X = EEsNSE_b_mua, MARGIN = 2, FUN = mean)
EEsNSE_b_mua_05 = apply(X = EEsNSE_b_mua, MARGIN = 2, FUN = quantile, probs = 0.05)
EEsNSE_b_mua_95 = apply(X = EEsNSE_b_mua, MARGIN = 2, FUN = quantile, probs = 0.95)

EEspBias_b_mua_m = apply(X = EEspBias_b_mua, MARGIN = 2, FUN = mean)
EEspBias_b_mua_05 = apply(X = EEspBias_b_mua, MARGIN = 2, FUN = quantile, probs = 0.05)
EEspBias_b_mua_95 = apply(X = EEspBias_b_mua, MARGIN = 2, FUN = quantile, probs = 0.95)

EEsLogL_b_mua_m = apply(X = EEsLogL_b_mua, MARGIN = 2, FUN = mean)
EEsLogL_b_mua_05 = apply(X = EEsLogL_b_mua, MARGIN = 2, FUN = quantile, probs = 0.05)
EEsLogL_b_mua_95 = apply(X = EEsLogL_b_mua, MARGIN = 2, FUN = quantile, probs = 0.95)

EEsTN05_b_mua_m = apply(X = EEsTN05_b_mua, MARGIN = 2, FUN = mean)
EEsTN05_b_mua_05 = apply(X = EEsTN05_b_mua, MARGIN = 2, FUN = quantile, probs = 0.05)
EEsTN05_b_mua_95 = apply(X = EEsTN05_b_mua, MARGIN = 2, FUN = quantile, probs = 0.95)

EEsTN95_b_mua_m = apply(X = EEsTN95_b_mua, MARGIN = 2, FUN = mean)
EEsTN95_b_mua_05 = apply(X = EEsTN95_b_mua, MARGIN = 2, FUN = quantile, probs = 0.05)
EEsTN95_b_mua_95 = apply(X = EEsTN95_b_mua, MARGIN = 2, FUN = quantile, probs = 0.95)

EEsTNMed_b_mua_m = apply(X = EEsTNMed_b_mua, MARGIN = 2, FUN = mean)
EEsTNMed_b_mua_05 = apply(X = EEsTNMed_b_mua, MARGIN = 2, FUN = quantile, probs = 0.05)
EEsTNMed_b_mua_95 = apply(X = EEsTNMed_b_mua, MARGIN = 2, FUN = quantile, probs = 0.95)

#Loop over hillslopes
EEs05_h_mua_m = EEs95_h_mua_m = EEsot_h_mua_m = EEs05g_h_mua_m = EEsotg_h_mua_m = EEsTN05_h_mua_m = EEsTN95_h_mua_m = EEsTNMed_h_mua_m = matrix(NA, nrow = length(uhills), ncol = length(EEs05_b_mua_05)+1)
EEs05_h_mua_05 = EEs95_h_mua_05 = EEsot_h_mua_05 = EEs05g_h_mua_05 = EEsotg_h_mua_05 = EEsTN05_h_mua_05 = EEsTN95_h_mua_05 = EEsTNMed_h_mua_05 = matrix(NA, nrow = length(uhills), ncol = length(EEs05_b_mua_05)+1)
EEs05_h_mua_95 = EEs95_h_mua_95 = EEsot_h_mua_95 = EEs05g_h_mua_95 = EEsotg_h_mua_95 = EEsTN05_h_mua_95 = EEsTN95_h_mua_95 = EEsTNMed_h_mua_95 = matrix(NA, nrow = length(uhills), ncol = length(EEs05_b_mua_05)+1)

for (h in 1:length(uhills)){
  EEs05_h_mua_m[uhills[h], ] = apply(X = EEs05_h_mua[EEs05_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  EEs05_h_mua_05[uhills[h], ] = apply(X = EEs05_h_mua[EEs05_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = quantile, probs = 0.05)
  EEs05_h_mua_95[uhills[h], ] = apply(X = EEs05_h_mua[EEs05_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = quantile, probs = 0.95)
  
  EEs95_h_mua_m[uhills[h], ] = apply(X = EEs95_h_mua[EEs95_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  EEs95_h_mua_05[uhills[h], ] = apply(X = EEs95_h_mua[EEs95_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = quantile, probs = 0.05)
  EEs95_h_mua_95[uhills[h], ] = apply(X = EEs95_h_mua[EEs95_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = quantile, probs = 0.95)
  
  EEsot_h_mua_m[uhills[h], ] = apply(X = EEsot_h_mua[EEsot_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  EEsot_h_mua_05[uhills[h], ] = apply(X = EEsot_h_mua[EEsot_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = quantile, probs = 0.05)
  EEsot_h_mua_95[uhills[h], ] = apply(X = EEsot_h_mua[EEsot_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = quantile, probs = 0.95)
  
  EEs05g_h_mua_m[uhills[h], ] = apply(X = EEs05g_h_mua[EEs05g_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  EEs05g_h_mua_05[uhills[h], ] = apply(X = EEs05g_h_mua[EEs05g_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = quantile, probs = 0.05)
  EEs05g_h_mua_95[uhills[h], ] = apply(X = EEs05g_h_mua[EEs05g_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = quantile, probs = 0.95)
  
  EEsotg_h_mua_m[uhills[h], ] = apply(X = EEsotg_h_mua[EEsotg_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  EEsotg_h_mua_05[uhills[h], ] = apply(X = EEsotg_h_mua[EEsotg_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = quantile, probs = 0.05)
  EEsotg_h_mua_95[uhills[h], ] = apply(X = EEsotg_h_mua[EEsotg_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = quantile, probs = 0.95)
  
  EEsTN05_h_mua_m[uhills[h], ] = apply(X = EEsTN05_h_mua[EEsTN05_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  EEsTN05_h_mua_05[uhills[h], ] = apply(X = EEsTN05_h_mua[EEsTN05_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = quantile, probs = 0.05)
  EEsTN05_h_mua_95[uhills[h], ] = apply(X = EEsTN05_h_mua[EEsTN05_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = quantile, probs = 0.95)
  
  EEsTN95_h_mua_m[uhills[h], ] = apply(X = EEsTN95_h_mua[EEsTN95_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  EEsTN95_h_mua_05[uhills[h], ] = apply(X = EEsTN95_h_mua[EEsTN95_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = quantile, probs = 0.05)
  EEsTN95_h_mua_95[uhills[h], ] = apply(X = EEsTN95_h_mua[EEsTN95_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = quantile, probs = 0.95)
  
  EEsTNMed_h_mua_m[uhills[h], ] = apply(X = EEsTNMed_h_mua[EEsTNMed_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = mean)
  EEsTNMed_h_mua_05[uhills[h], ] = apply(X = EEsTNMed_h_mua[EEsTNMed_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = quantile, probs = 0.05)
  EEsTNMed_h_mua_95[uhills[h], ] = apply(X = EEsTNMed_h_mua[EEsTNMed_h_mua[,1] == uhills[h],], MARGIN = 2, FUN = quantile, probs = 0.95)
}
#Name the columns
colnames(EEs05_h_mua_m) = colnames(EEs95_h_mua_m) = colnames(EEsot_h_mua_m) = colnames(EEs05g_h_mua_m) = colnames(EEsotg_h_mua_m) = colnames(EEsTN05_h_mua_m) = colnames(EEsTN95_h_mua_m) = colnames(EEsTNMed_h_mua_m) = c('HillID', names(EEs05_b_mua_m))
colnames(EEs05_h_mua_05) = colnames(EEs95_h_mua_05) = colnames(EEsot_h_mua_05) = colnames(EEs05g_h_mua_05) = colnames(EEsotg_h_mua_05) = colnames(EEsTN05_h_mua_05) = colnames(EEsTN95_h_mua_05) = colnames(EEsTNMed_h_mua_05) = c('HillID', names(EEs05_b_mua_05))
colnames(EEs05_h_mua_95) = colnames(EEs95_h_mua_95) = colnames(EEsot_h_mua_95) = colnames(EEs05g_h_mua_95) = colnames(EEsotg_h_mua_95) = colnames(EEsTN05_h_mua_95) = colnames(EEsTN95_h_mua_95) = colnames(EEsTNMed_h_mua_95) = c('HillID', names(EEs05_b_mua_95))

#Compute the mean, standard deviation, and absolute mean of the EEs from the original sample----
muEEs05_b = apply(X = EEs05_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = mean)
muEEs95_b = apply(X = EEs95_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = mean)
muEEsot_b = apply(X = EEsot_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = mean)
muEEs05g_b = apply(X = EEs05g_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = mean)
muEEsotg_b = apply(X = EEsotg_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = mean)
muEEsLNSE_b = apply(X = EEsLNSE_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = mean)
muEEsNSE_b = apply(X = EEsNSE_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = mean)
muEEspBias_b = apply(X = EEspBias_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = mean)
muEEsLogL_b = apply(X = EEsLogL_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = mean)
muEEsTN05_b = apply(X = EEsTN05_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = mean)
muEEsTN95_b = apply(X = EEsTN95_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = mean)
muEEsTNMed_b = apply(X = EEsTNMed_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = mean)
muEEs05gm_b = apply(X = EEs05gm_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = mean)
muEEs95m_b = apply(X = EEs95m_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = mean)
muEEsotgm_b = apply(X = EEsotgm_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = mean)

sdEEs05_b = apply(X = EEs05_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = sd)
sdEEs95_b = apply(X = EEs95_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = sd)
sdEEsot_b = apply(X = EEsot_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = sd)
sdEEs05g_b = apply(X = EEs05g_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = sd)
sdEEsotg_b = apply(X = EEsotg_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = sd)
sdEEsLNSE_b = apply(X = EEsLNSE_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = sd)
sdEEsNSE_b = apply(X = EEsNSE_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = sd)
sdEEspBias_b = apply(X = EEspBias_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = sd)
sdEEsLogL_b = apply(X = EEsLogL_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = sd)
sdEEsTN05_b = apply(X = EEsTN05_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = sd)
sdEEsTN95_b = apply(X = EEsTN95_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = sd)
sdEEsTNMed_b = apply(X = EEsTNMed_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = sd)
sdEEs05gm_b = apply(X = EEs05gm_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = sd)
sdEEs95m_b = apply(X = EEs95m_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = sd)
sdEEsotgm_b = apply(X = EEsotgm_b[,1:nrow(ParamRanges)], MARGIN = 2, FUN = sd)

muaEEs05_b = apply(X = abs(EEs05_b[,1:nrow(ParamRanges)]), MARGIN = 2, FUN = mean)
muaEEs95_b = apply(X = abs(EEs95_b[,1:nrow(ParamRanges)]), MARGIN = 2, FUN = mean)
muaEEsot_b = apply(X = abs(EEsot_b[,1:nrow(ParamRanges)]), MARGIN = 2, FUN = mean)
muaEEs05g_b = apply(X = abs(EEs05g_b[,1:nrow(ParamRanges)]), MARGIN = 2, FUN = mean)
muaEEsotg_b = apply(X = abs(EEsotg_b[,1:nrow(ParamRanges)]), MARGIN = 2, FUN = mean)
muaEEsLNSE_b = apply(X = abs(EEsLNSE_b[,1:nrow(ParamRanges)]), MARGIN = 2, FUN = mean)
muaEEsNSE_b = apply(X = abs(EEsNSE_b[,1:nrow(ParamRanges)]), MARGIN = 2, FUN = mean)
muaEEspBias_b = apply(X = abs(EEspBias_b[,1:nrow(ParamRanges)]), MARGIN = 2, FUN = mean)
muaEEsLogL_b = apply(X = abs(EEsLogL_b[,1:nrow(ParamRanges)]), MARGIN = 2, FUN = mean)
muaEEsTN05_b = apply(X = abs(EEsTN05_b[,1:nrow(ParamRanges)]), MARGIN = 2, FUN = mean)
muaEEsTN95_b = apply(X = abs(EEsTN95_b[,1:nrow(ParamRanges)]), MARGIN = 2, FUN = mean)
muaEEsTNMed_b = apply(X = abs(EEsTNMed_b[,1:nrow(ParamRanges)]), MARGIN = 2, FUN = mean)
muaEEs05gm_b = apply(X = abs(EEs05gm_b[,1:nrow(ParamRanges)]), MARGIN = 2, FUN = mean)
muaEEs95m_b = apply(X = abs(EEs95m_b[,1:nrow(ParamRanges)]), MARGIN = 2, FUN = mean)
muaEEsotgm_b = apply(X = abs(EEsotgm_b[,1:nrow(ParamRanges)]), MARGIN = 2, FUN = mean)

#Hillslopes will have one per hillslope
muEEs05_h = muEEs95_h = muEEsot_h = muEEs05g_h = muEEsotg_h = muEEsTN05_h = muEEsTN95_h = muEEsTNMed_h = sdEEs05_h = sdEEs95_h = sdEEsot_h = sdEEs05g_h = sdEEsotg_h = sdEEsTN05_h = sdEEsTN95_h = sdEEsTNMed_h = muaEEs05_h = muaEEs95_h = muaEEsot_h = muaEEs05g_h = muaEEsotg_h = muaEEsTN05_h = muaEEsTN95_h = muaEEsTNMed_h = matrix(NA, nrow = length(uhills), ncol = as.numeric(cols))
for (h in 1:length(uhills)){
  inds = seq(h, r*length(uhills)-(length(uhills)-h), length(uhills))
  muEEs05_h[h,] = apply(X = EEs05_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = mean)
  muEEs95_h[h,] = apply(X = EEs95_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = mean)
  muEEsot_h[h,] = apply(X = EEsot_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = mean)
  muEEs05g_h[h,] = apply(X = EEs05g_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = mean)
  muEEsotg_h[h,] = apply(X = EEsotg_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = mean)
  muEEsTN05_h[h,] = apply(X = EEsTN05_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = mean)
  muEEsTN95_h[h,] = apply(X = EEsTN95_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = mean)
  muEEsTNMed_h[h,] = apply(X = EEsTNMed_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = mean)
  
  sdEEs05_h[h,] = apply(X = EEs05_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = sd)
  sdEEs95_h[h,] = apply(X = EEs95_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = sd)
  sdEEsot_h[h,] = apply(X = EEsot_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = sd)
  sdEEs05g_h[h,] = apply(X = EEs05g_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = sd)
  sdEEsotg_h[h,] = apply(X = EEsotg_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = sd)
  sdEEsTN05_h[h,] = apply(X = EEsTN05_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = sd)
  sdEEsTN95_h[h,] = apply(X = EEsTN95_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = sd)
  sdEEsTNMed_h[h,] = apply(X = EEsTNMed_h[inds,2:(nrow(ParamRanges)+1)], MARGIN = 2, FUN = sd)
  
  muaEEs05_h[h,] = apply(X = abs(EEs05_h[inds,2:(nrow(ParamRanges)+1)]), MARGIN = 2, FUN = mean)
  muaEEs95_h[h,] = apply(X = abs(EEs95_h[inds,2:(nrow(ParamRanges)+1)]), MARGIN = 2, FUN = mean)
  muaEEsot_h[h,] = apply(X = abs(EEsot_h[inds,2:(nrow(ParamRanges)+1)]), MARGIN = 2, FUN = mean)
  muaEEs05g_h[h,] = apply(X = abs(EEs05g_h[inds,2:(nrow(ParamRanges)+1)]), MARGIN = 2, FUN = mean)
  muaEEsotg_h[h,] = apply(X = abs(EEsotg_h[inds,2:(nrow(ParamRanges)+1)]), MARGIN = 2, FUN = mean)
  muaEEsTN05_h[h,] = apply(X = abs(EEsTN05_h[inds,2:(nrow(ParamRanges)+1)]), MARGIN = 2, FUN = mean)
  muaEEsTN95_h[h,] = apply(X = abs(EEsTN95_h[inds,2:(nrow(ParamRanges)+1)]), MARGIN = 2, FUN = mean)
  muaEEsTNMed_h[h,] = apply(X = abs(EEsTNMed_h[inds,2:(nrow(ParamRanges)+1)]), MARGIN = 2, FUN = mean)
}
rm(inds, h)

#Make a list of ordered parameter names for mua using original sample----
RanksMua05_b = data.frame(Param = colnames(InputParams)[rev(order(muaEEs05_b))], EE05_b = muaEEs05_b[rev(order(muaEEs05_b))], stringsAsFactors = FALSE)
RanksMua95_b = data.frame(Param = colnames(InputParams)[rev(order(muaEEs95_b))], EE95_b = muaEEs95_b[rev(order(muaEEs95_b))], stringsAsFactors = FALSE)
RanksMuaot_b = data.frame(Param = colnames(InputParams)[rev(order(muaEEsot_b))], EEot_b = muaEEsot_b[rev(order(muaEEsot_b))], stringsAsFactors = FALSE)
RanksMua05g_b = data.frame(Param = colnames(InputParams)[rev(order(muaEEs05g_b))], EE05g_b = muaEEs05g_b[rev(order(muaEEs05g_b))], stringsAsFactors = FALSE)
RanksMuaotg_b = data.frame(Param = colnames(InputParams)[rev(order(muaEEsotg_b))], EEotg_b = muaEEsotg_b[rev(order(muaEEsotg_b))], stringsAsFactors = FALSE)
RanksMuaLNSE_b = data.frame(Param = colnames(InputParams)[rev(order(muaEEsLNSE_b))], EELNSE_b = muaEEsLNSE_b[rev(order(muaEEsLNSE_b))], stringsAsFactors = FALSE)
RanksMuaNSE_b = data.frame(Param = colnames(InputParams)[rev(order(muaEEsNSE_b))], EENSE_b = muaEEsNSE_b[rev(order(muaEEsNSE_b))], stringsAsFactors = FALSE)
RanksMuapBias_b = data.frame(Param = colnames(InputParams)[rev(order(muaEEspBias_b))], EEpBias_b = muaEEspBias_b[rev(order(muaEEspBias_b))], stringsAsFactors = FALSE)
RanksMuaLogL_b = data.frame(Param = colnames(InputParams)[rev(order(muaEEsLogL_b))], EELogL_b = muaEEsLogL_b[rev(order(muaEEsLogL_b))], stringsAsFactors = FALSE)
RanksMuaTN05_b = data.frame(Param = colnames(InputParams)[rev(order(muaEEsTN05_b))], EETN05_b = muaEEsTN05_b[rev(order(muaEEsTN05_b))], stringsAsFactors = FALSE)
RanksMuaTN95_b = data.frame(Param = colnames(InputParams)[rev(order(muaEEsTN95_b))], EETN95_b = muaEEsTN95_b[rev(order(muaEEsTN95_b))], stringsAsFactors = FALSE)
RanksMuaTNMed_b = data.frame(Param = colnames(InputParams)[rev(order(muaEEsTNMed_b))], EETNMed_b = muaEEsTNMed_b[rev(order(muaEEsTNMed_b))], stringsAsFactors = FALSE)
RanksMua05gm_b = data.frame(Param = colnames(InputParams)[rev(order(muaEEs05gm_b))], EE05gm_b = muaEEs05gm_b[rev(order(muaEEs05gm_b))], stringsAsFactors = FALSE)
RanksMua95m_b = data.frame(Param = colnames(InputParams)[rev(order(muaEEs95m_b))], EE95m_b = muaEEs95m_b[rev(order(muaEEs95m_b))], stringsAsFactors = FALSE)
RanksMuaotgm_b = data.frame(Param = colnames(InputParams)[rev(order(muaEEsotgm_b))], EEotgm_b = muaEEsotgm_b[rev(order(muaEEsotgm_b))], stringsAsFactors = FALSE)

RanksMua05_h = RanksMua95_h = RanksMuaot_h = RanksMua05g_h = RanksMuaotg_h = RanksMuaTN05_h = RanksMuaTN95_h = RanksMuaTNMed_h = list()
for (h in 1:length(uhills)){
  RanksMua05_h = c(RanksMua05_h, list(data.frame(Param = colnames(InputParams)[rev(order(muaEEs05_h[h,]))], EE05_h = muaEEs05_h[h,][rev(order(muaEEs05_h[h,]))], stringsAsFactors = FALSE)))
  RanksMua95_h = c(RanksMua95_h, list(data.frame(Param = colnames(InputParams)[rev(order(muaEEs95_h[h,]))], EE95_h = muaEEs95_h[h,][rev(order(muaEEs95_h[h,]))], stringsAsFactors = FALSE)))
  RanksMuaot_h = c(RanksMuaot_h, list(data.frame(Param = colnames(InputParams)[rev(order(muaEEsot_h[h,]))], EEot_h = muaEEsot_h[h,][rev(order(muaEEsot_h[h,]))], stringsAsFactors = FALSE)))
  RanksMua05g_h = c(RanksMua05g_h, list(data.frame(Param = colnames(InputParams)[rev(order(muaEEs05g_h[h,]))], EE05g_h = muaEEs05g_h[h,][rev(order(muaEEs05g_h[h,]))], stringsAsFactors = FALSE)))
  RanksMuaotg_h = c(RanksMuaotg_h, list(data.frame(Param = colnames(InputParams)[rev(order(muaEEsotg_h[h,]))], EEotg_h = muaEEsotg_h[h,][rev(order(muaEEsotg_h[h,]))], stringsAsFactors = FALSE)))
  RanksMuaTN05_h = c(RanksMuaTN05_h, list(data.frame(Param = colnames(InputParams)[rev(order(muaEEsTN05_h[h,]))], EETN05_h = muaEEsTN05_h[h,][rev(order(muaEEsTN05_h[h,]))], stringsAsFactors = FALSE)))
  RanksMuaTN95_h = c(RanksMuaTN95_h, list(data.frame(Param = colnames(InputParams)[rev(order(muaEEsTN95_h[h,]))], EETN95_h = muaEEsTN95_h[h,][rev(order(muaEEsTN95_h[h,]))], stringsAsFactors = FALSE)))
  RanksMuaTNMed_h = c(RanksMuaTNMed_h, list(data.frame(Param = colnames(InputParams)[rev(order(muaEEsTNMed_h[h,]))], EETNMed_h = muaEEsTNMed_h[h,][rev(order(muaEEsTNMed_h[h,]))], stringsAsFactors = FALSE)))
}
names(RanksMua05_h) = names(RanksMua95_h) = names(RanksMuaot_h) = names(RanksMua05g_h) = names(RanksMuaotg_h) = names(RanksMuaTN05_h) = names(RanksMuaTN95_h) = names(RanksMuaTNMed_h) = paste0('Hill', seq(1,14,1))
rm(h)

# Using Aggregated variables----
RanksMua05_b_Agg = data.frame(Param = names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)])[order(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)],decreasing = TRUE)], EE05_b = as.numeric(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), stringsAsFactors = FALSE)
RanksMua95_b_Agg = data.frame(Param = names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])[order(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)],decreasing = TRUE)], EE95_b = as.numeric(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), stringsAsFactors = FALSE)
RanksMuaot_b_Agg = data.frame(Param = names(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)])[order(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)],decreasing = TRUE)], EEot_b = as.numeric(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][order(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), stringsAsFactors = FALSE)
RanksMua05g_b_Agg = data.frame(Param = names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)])[order(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)],decreasing = TRUE)], EE05g_b = as.numeric(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), stringsAsFactors = FALSE)
RanksMuaotg_b_Agg = data.frame(Param = names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)])[order(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)],decreasing = TRUE)], EEotg_b = as.numeric(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), stringsAsFactors = FALSE)
RanksMuaLNSE_b_Agg = data.frame(Param = names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)])[order(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)],decreasing = TRUE)], EELNSE_b = as.numeric(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), stringsAsFactors = FALSE)
RanksMuaNSE_b_Agg = data.frame(Param = names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)])[order(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)],decreasing = TRUE)], EENSE_b = as.numeric(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), stringsAsFactors = FALSE)
RanksMuapBias_b_Agg = data.frame(Param = names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)])[order(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)],decreasing = TRUE)], EEpBias_b = as.numeric(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), stringsAsFactors = FALSE)
RanksMuaLogL_b_Agg = data.frame(Param = names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)])[order(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)],decreasing = TRUE)], EELogL_b = as.numeric(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), stringsAsFactors = FALSE)
RanksMuaTN05_b_Agg = data.frame(Param = names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])[order(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)],decreasing = TRUE)], EETN05_b = as.numeric(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), stringsAsFactors = FALSE)
RanksMuaTN95_b_Agg = data.frame(Param = names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])[order(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)],decreasing = TRUE)], EETN95_b = as.numeric(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), stringsAsFactors = FALSE)
RanksMuaTNMed_b_Agg = data.frame(Param = names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])[order(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)],decreasing = TRUE)], EETNMed_b = as.numeric(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), stringsAsFactors = FALSE)
RanksMua05gm_b_Agg = data.frame(Param = names(EEs05gm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)])[order(EEs05gm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)],decreasing = TRUE)], EE05gm_b = as.numeric(EEs05gm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)][order(EEs05gm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), stringsAsFactors = FALSE)
RanksMua95m_b_Agg = data.frame(Param = names(EEs95m_b_mua_m[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)])[order(EEs95m_b_mua_m[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)],decreasing = TRUE)], EE95m_b = as.numeric(EEs95m_b_mua_m[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)][order(EEs95m_b_mua_m[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), stringsAsFactors = FALSE)
RanksMuaotgm_b_Agg = data.frame(Param = names(EEsotgm_b_mua_m[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)])[order(EEsotgm_b_mua_m[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)],decreasing = TRUE)], EEotgm_b = as.numeric(EEsotgm_b_mua_m[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)][order(EEsotgm_b_mua_m[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), stringsAsFactors = FALSE)

RanksMua05_h_Agg = RanksMua95_h_Agg = RanksMuaot_h_Agg = RanksMua05g_h_Agg = RanksMuaotg_h_Agg = RanksMuaTN05_h_Agg = RanksMuaTN95_h_Agg = RanksMuaTNMed_h_Agg = list()
for (h in 1:length(uhills)){
  RanksMua05_h_Agg = c(RanksMua05_h_Agg, list(data.frame(Param = colnames(EEs05_h_mua_m)[-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))][order(EEs05_h_mua_m[h, -c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)], EE05_h = as.numeric(EEs05_h_mua_m[h, -c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))][order(EEs05_h_mua_m[h, -c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)]), stringsAsFactors = FALSE)))
  RanksMua95_h_Agg = c(RanksMua95_h_Agg, list(data.frame(Param = colnames(EEs95_h_mua_m)[-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))][order(EEs95_h_mua_m[h, -c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)], EE95_h = as.numeric(EEs95_h_mua_m[h, -c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))][order(EEs95_h_mua_m[h, -c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)]), stringsAsFactors = FALSE)))
  RanksMuaot_h_Agg = c(RanksMuaot_h_Agg, list(data.frame(Param = colnames(EEsot_h_mua_m)[-c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))][order(EEsot_h_mua_m[h, -c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)], EEot_h = as.numeric(EEsot_h_mua_m[h, -c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))][order(EEsot_h_mua_m[h, -c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)]), stringsAsFactors = FALSE)))
  RanksMua05g_h_Agg = c(RanksMua05g_h_Agg, list(data.frame(Param = colnames(EEs05g_h_mua_m)[-c(1,which(colnames(EEs05g_h_mua_m) %in% ColsAggregated))][order(EEs05g_h_mua_m[h, -c(1,which(colnames(EEs05g_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)], EE05g_h = as.numeric(EEs05g_h_mua_m[h, -c(1,which(colnames(EEs05g_h_mua_m) %in% ColsAggregated))][order(EEs05g_h_mua_m[h, -c(1,which(colnames(EEs05g_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)]), stringsAsFactors = FALSE)))
  RanksMuaotg_h_Agg = c(RanksMuaotg_h_Agg, list(data.frame(Param = colnames(EEsotg_h_mua_m)[-c(1,which(colnames(EEsotg_h_mua_m) %in% ColsAggregated))][order(EEsotg_h_mua_m[h, -c(1,which(colnames(EEsotg_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)], EEotg_h = as.numeric(EEsotg_h_mua_m[h, -c(1,which(colnames(EEsotg_h_mua_m) %in% ColsAggregated))][order(EEsotg_h_mua_m[h, -c(1,which(colnames(EEsotg_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)]), stringsAsFactors = FALSE)))
  RanksMuaTN05_h_Agg = c(RanksMuaTN05_h_Agg, list(data.frame(Param = colnames(EEsTN05_h_mua_m)[-c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))][order(EEsTN05_h_mua_m[h, -c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)], EETN05_h = as.numeric(EEsTN05_h_mua_m[h, -c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))][order(EEsTN05_h_mua_m[h, -c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)]), stringsAsFactors = FALSE)))
  RanksMuaTN95_h_Agg = c(RanksMuaTN95_h_Agg, list(data.frame(Param = colnames(EEsTN95_h_mua_m)[-c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))][order(EEsTN95_h_mua_m[h, -c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)], EETN95_h = as.numeric(EEsTN95_h_mua_m[h, -c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))][order(EEsTN95_h_mua_m[h, -c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)]), stringsAsFactors = FALSE)))
  RanksMuaTNMed_h_Agg = c(RanksMuaTNMed_h_Agg, list(data.frame(Param = colnames(EEsTNMed_h_mua_m)[-c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))][order(EEsTNMed_h_mua_m[h, -c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)], EETNMed_h = as.numeric(EEsTNMed_h_mua_m[h, -c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))][order(EEsTNMed_h_mua_m[h, -c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))],decreasing = TRUE)]), stringsAsFactors = FALSE)))
}
names(RanksMua05_h_Agg) = names(RanksMua95_h_Agg) = names(RanksMuaot_h_Agg) = names(RanksMua05g_h_Agg) = names(RanksMuaotg_h_Agg) = names(RanksMuaTN05_h_Agg) = names(RanksMuaTN95_h_Agg) = names(RanksMuaTNMed_h_Agg) = paste0('Hill', seq(1,14,1))
rm(h)

#Paper SI table info with confidence intervals added----
RanksMua05_b_AggCI = cbind(RanksMua05_b_Agg, data.frame(P05 = as.numeric(EEs05_b_mua_05[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), P95 = as.numeric(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)])))
RanksMuaot_b_AggCI = cbind(RanksMuaot_b_Agg, data.frame(P05 = as.numeric(EEsot_b_mua_05[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][order(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), P95 = as.numeric(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][order(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)])))
RanksMua95_b_AggCI = cbind(RanksMua95_b_Agg, data.frame(P05 = as.numeric(EEs95_b_mua_05[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), P95 = as.numeric(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)])))
RanksMua05g_b_AggCI = cbind(RanksMua05g_b_Agg, data.frame(P05 = as.numeric(EEs05g_b_mua_05[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), P95 = as.numeric(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)])))
RanksMuaotg_b_AggCI = cbind(RanksMuaotg_b_Agg, data.frame(P05 = as.numeric(EEsotg_b_mua_05[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), P95 = as.numeric(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)])))
RanksMuaLNSE_b_AggCI = cbind(RanksMuaLNSE_b_Agg, data.frame(P05 = as.numeric(EEsLNSE_b_mua_05[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), P95 = as.numeric(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)])))
RanksMuaNSE_b_AggCI = cbind(RanksMuaNSE_b_Agg, data.frame(P05 = as.numeric(EEsNSE_b_mua_05[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), P95 = as.numeric(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)])))
RanksMuapBias_b_AggCI = cbind(RanksMuapBias_b_Agg, data.frame(P05 = as.numeric(EEspBias_b_mua_05[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), P95 = as.numeric(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)])))
RanksMuaLogL_b_AggCI = cbind(RanksMuaLogL_b_Agg, data.frame(P05 = as.numeric(EEsLogL_b_mua_05[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), P95 = as.numeric(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)])))
RanksMuaTN05_b_AggCI = cbind(RanksMuaTN05_b_Agg, data.frame(P05 = as.numeric(EEsTN05_b_mua_05[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), P95 = as.numeric(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)])))
RanksMuaTNMed_b_AggCI = cbind(RanksMuaTNMed_b_Agg, data.frame(P05 = as.numeric(EEsTNMed_b_mua_05[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), P95 = as.numeric(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)])))
RanksMuaTN95_b_AggCI = cbind(RanksMuaTN95_b_Agg, data.frame(P05 = as.numeric(EEsTN95_b_mua_05[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), P95 = as.numeric(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)])))
RanksMua05gm_b_AggCI = cbind(RanksMua05gm_b_Agg, data.frame(P05 = as.numeric(EEs05gm_b_mua_05[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)][order(EEs05gm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), P95 = as.numeric(EEs05gm_b_mua_95[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)][order(EEs05gm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)])))
RanksMuaotgm_b_AggCI = cbind(RanksMuaotgm_b_Agg, data.frame(P05 = as.numeric(EEsotgm_b_mua_05[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)][order(EEsotgm_b_mua_m[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), P95 = as.numeric(EEsotgm_b_mua_95[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)][order(EEsotgm_b_mua_m[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)])))
RanksMua95m_b_AggCI = cbind(RanksMua95m_b_Agg, data.frame(P05 = as.numeric(EEs95m_b_mua_05[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)][order(EEs95m_b_mua_m[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)]), P95 = as.numeric(EEs95m_b_mua_95[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)][order(EEs95m_b_mua_m[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)])))

RanksMua05_b_AggCI[,c(2,3,4)] = signif(RanksMua05_b_AggCI[,c(2,3,4)], 3)
RanksMuaot_b_AggCI[,c(2,3,4)] = signif(RanksMuaot_b_AggCI[,c(2,3,4)], 3)
RanksMua95_b_AggCI[,c(2,3,4)] = signif(RanksMua95_b_AggCI[,c(2,3,4)], 3)
RanksMua05g_b_AggCI[,c(2,3,4)] = signif(RanksMua05g_b_AggCI[,c(2,3,4)], 3)
RanksMuaotg_b_AggCI[,c(2,3,4)] = signif(RanksMuaotg_b_AggCI[,c(2,3,4)], 3)
RanksMuaLNSE_b_AggCI[,c(2,3,4)] = signif(RanksMuaLNSE_b_AggCI[,c(2,3,4)], 3)
RanksMuaNSE_b_AggCI[,c(2,3,4)] = signif(RanksMuaNSE_b_AggCI[,c(2,3,4)], 3)
RanksMuapBias_b_AggCI[,c(2,3,4)] = signif(RanksMuapBias_b_AggCI[,c(2,3,4)], 3)
RanksMuaLogL_b_AggCI[,c(2,3,4)] = signif(RanksMuaLogL_b_AggCI[,c(2,3,4)], 3)
RanksMuaTN05_b_AggCI[,c(2,3,4)] = signif(RanksMuaTN05_b_AggCI[,c(2,3,4)], 3)
RanksMuaTNMed_b_AggCI[,c(2,3,4)] = signif(RanksMuaTNMed_b_AggCI[,c(2,3,4)], 3)
RanksMuaTN95_b_AggCI[,c(2,3,4)] = signif(RanksMuaTN95_b_AggCI[,c(2,3,4)], 3)
RanksMua05gm_b_AggCI[,c(2,3,4)] = signif(RanksMua05gm_b_AggCI[,c(2,3,4)], 3)
RanksMuaotgm_b_AggCI[,c(2,3,4)] = signif(RanksMuaotgm_b_AggCI[,c(2,3,4)], 3)
RanksMua95m_b_AggCI[,c(2,3,4)] = signif(RanksMua95m_b_AggCI[,c(2,3,4)], 3)

#In order of parameters in Fig 2 (formerly figure 1)
Fig1Order = names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)])
RanksMua05_b_AggCI_FigOrder = RanksMua05_b_AggCI[match(table = RanksMua05_b_AggCI$Param, x = Fig1Order),] 
RanksMuaot_b_AggCI_FigOrder = RanksMuaot_b_AggCI[match(table = RanksMuaot_b_AggCI$Param, x = Fig1Order),]
RanksMua95_b_AggCI_FigOrder = RanksMua95_b_AggCI[match(table = RanksMua95_b_AggCI$Param, x = Fig1Order),]
RanksMua05g_b_AggCI_FigOrder = RanksMua05g_b_AggCI[match(table = RanksMua05g_b_AggCI$Param, x = Fig1Order),]
RanksMuaotg_b_AggCI_FigOrder = RanksMuaotg_b_AggCI[match(table = RanksMuaotg_b_AggCI$Param, x = Fig1Order),]
RanksMuaLNSE_b_AggCI_FigOrder = RanksMuaLNSE_b_AggCI[match(table = RanksMuaLNSE_b_AggCI$Param, x = Fig1Order),]
RanksMuaNSE_b_AggCI_FigOrder = RanksMuaNSE_b_AggCI[match(table = RanksMuaNSE_b_AggCI$Param, x = Fig1Order),]
RanksMuapBias_b_AggCI_FigOrder = RanksMuapBias_b_AggCI[match(table = RanksMuapBias_b_AggCI$Param, x = Fig1Order),]
RanksMuaLogL_b_AggCI_FigOrder = RanksMuaLogL_b_AggCI[match(table = RanksMuaLogL_b_AggCI$Param, x = Fig1Order),]
RanksMuaTN05_b_AggCI_FigOrder = RanksMuaTN05_b_AggCI[match(table = RanksMuaTN05_b_AggCI$Param, x = Fig1Order),]
RanksMuaTNMed_b_AggCI_FigOrder = RanksMuaTNMed_b_AggCI[match(table = RanksMuaTNMed_b_AggCI$Param, x = Fig1Order),]
RanksMuaTN95_b_AggCI_FigOrder = RanksMuaTN95_b_AggCI[match(table = RanksMuaTN95_b_AggCI$Param, x = Fig1Order),]
RanksMua05gm_b_AggCI_FigOrder = RanksMua05gm_b_AggCI[match(table = RanksMua05gm_b_AggCI$Param, x = Fig1Order),] 
RanksMuaotgm_b_AggCI_FigOrder = RanksMuaotgm_b_AggCI[match(table = RanksMuaotgm_b_AggCI$Param, x = Fig1Order),]
RanksMua95m_b_AggCI_FigOrder = RanksMua95m_b_AggCI[match(table = RanksMua95m_b_AggCI$Param, x = Fig1Order),]

# Hillslope Tables----
RanksMua05_h_AggCI = RanksMua95_h_AggCI = RanksMuaot_h_AggCI = RanksMua05g_h_AggCI = RanksMuaotg_h_AggCI = RanksMuaTN05_h_AggCI = RanksMuaTN95_h_AggCI = RanksMuaTNMed_h_AggCI = list()
for (h in 1:length(uhills)){
  RanksMua05_h_AggCI = c(RanksMua05_h_AggCI, 
                         list(cbind(RanksMua05_h_Agg[[h]], 
                                    data.frame(P05 = as.numeric(EEs05_h_mua_05[h, -c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))][order(EEs05_h_mua_m[h, -c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))], decreasing = TRUE)]), 
                                               P95 = as.numeric(EEs05_h_mua_95[h, -c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))][order(EEs05_h_mua_m[h, -c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))], decreasing = TRUE)]),
                                               Rank = rownames(RanksMua05_h_Agg[[h]])))))
  RanksMua95_h_AggCI = c(RanksMua95_h_AggCI, 
                         list(cbind(RanksMua95_h_Agg[[h]], 
                                    data.frame(P05 = as.numeric(EEs95_h_mua_05[h, -c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))][order(EEs95_h_mua_m[h, -c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))], decreasing = TRUE)]), 
                                               P95 = as.numeric(EEs95_h_mua_95[h, -c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))][order(EEs95_h_mua_m[h, -c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))], decreasing = TRUE)]),
                                               Rank = rownames(RanksMua95_h_Agg[[h]])))))
  RanksMuaot_h_AggCI = c(RanksMuaot_h_AggCI, 
                         list(cbind(RanksMuaot_h_Agg[[h]], 
                                    data.frame(P05 = as.numeric(EEsot_h_mua_05[h, -c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))][order(EEsot_h_mua_m[h, -c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))], decreasing = TRUE)]), 
                                               P95 = as.numeric(EEsot_h_mua_95[h, -c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))][order(EEsot_h_mua_m[h, -c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))], decreasing = TRUE)]),
                                               Rank = rownames(RanksMuaot_h_Agg[[h]])))))
  RanksMua05g_h_AggCI = c(RanksMua05g_h_AggCI, 
                         list(cbind(RanksMua05g_h_Agg[[h]], 
                                    data.frame(P05 = as.numeric(EEs05g_h_mua_05[h, -c(1,which(colnames(EEs05g_h_mua_m) %in% ColsAggregated))][order(EEs05g_h_mua_m[h, -c(1,which(colnames(EEs05g_h_mua_m) %in% ColsAggregated))], decreasing = TRUE)]), 
                                               P95 = as.numeric(EEs05g_h_mua_95[h, -c(1,which(colnames(EEs05g_h_mua_m) %in% ColsAggregated))][order(EEs05g_h_mua_m[h, -c(1,which(colnames(EEs05g_h_mua_m) %in% ColsAggregated))], decreasing = TRUE)]),
                                               Rank = rownames(RanksMua05g_h_Agg[[h]])))))
  RanksMuaotg_h_AggCI = c(RanksMuaotg_h_AggCI, 
                         list(cbind(RanksMuaotg_h_Agg[[h]], 
                                    data.frame(P05 = as.numeric(EEsotg_h_mua_05[h, -c(1,which(colnames(EEsotg_h_mua_m) %in% ColsAggregated))][order(EEsotg_h_mua_m[h, -c(1,which(colnames(EEsotg_h_mua_m) %in% ColsAggregated))], decreasing = TRUE)]), 
                                               P95 = as.numeric(EEsotg_h_mua_95[h, -c(1,which(colnames(EEsotg_h_mua_m) %in% ColsAggregated))][order(EEsotg_h_mua_m[h, -c(1,which(colnames(EEsotg_h_mua_m) %in% ColsAggregated))], decreasing = TRUE)]),
                                               Rank = rownames(RanksMuaotg_h_Agg[[h]])))))
  RanksMuaTN05_h_AggCI = c(RanksMuaTN05_h_AggCI, 
                           list(cbind(RanksMuaTN05_h_Agg[[h]], 
                                      data.frame(P05 = as.numeric(EEsTN05_h_mua_05[h, -c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))][order(EEsTN05_h_mua_m[h, -c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))], decreasing = TRUE)]), 
                                                 P95 = as.numeric(EEsTN05_h_mua_95[h, -c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))][order(EEsTN05_h_mua_m[h, -c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))], decreasing = TRUE)]),
                                                 Rank = rownames(RanksMuaTN05_h_Agg[[h]])))))
  RanksMuaTN95_h_AggCI = c(RanksMuaTN95_h_AggCI, 
                           list(cbind(RanksMuaTN95_h_Agg[[h]], 
                                      data.frame(P05 = as.numeric(EEsTN95_h_mua_05[h, -c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))][order(EEsTN95_h_mua_m[h, -c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))], decreasing = TRUE)]), 
                                                 P95 = as.numeric(EEsTN95_h_mua_95[h, -c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))][order(EEsTN95_h_mua_m[h, -c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))], decreasing = TRUE)]),
                                                 Rank = rownames(RanksMuaTN95_h_Agg[[h]])))))
  RanksMuaTNMed_h_AggCI = c(RanksMuaTNMed_h_AggCI, 
                            list(cbind(RanksMuaTNMed_h_Agg[[h]], 
                                       data.frame(P05 = as.numeric(EEsTNMed_h_mua_05[h, -c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))][order(EEsTNMed_h_mua_m[h, -c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))], decreasing = TRUE)]), 
                                                  P95 = as.numeric(EEsTNMed_h_mua_95[h, -c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))][order(EEsTNMed_h_mua_m[h, -c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))], decreasing = TRUE)]),
                                                  Rank = rownames(RanksMuaTNMed_h_Agg[[h]])))))
  #Round
  RanksMua05_h_AggCI[[h]][,c(2,3,4)] = signif(RanksMua05_h_AggCI[[h]][,c(2,3,4)], 3)
  RanksMuaot_h_AggCI[[h]][,c(2,3,4)] = signif(RanksMuaot_h_AggCI[[h]][,c(2,3,4)], 3)
  RanksMua95_h_AggCI[[h]][,c(2,3,4)] = signif(RanksMua95_h_AggCI[[h]][,c(2,3,4)], 3)
  RanksMua05g_h_AggCI[[h]][,c(2,3,4)] = signif(RanksMua05g_h_AggCI[[h]][,c(2,3,4)], 3)
  RanksMuaotg_h_AggCI[[h]][,c(2,3,4)] = signif(RanksMuaotg_h_AggCI[[h]][,c(2,3,4)], 3)
  RanksMuaTN05_h_AggCI[[h]][,c(2,3,4)] = signif(RanksMuaTN05_h_AggCI[[h]][,c(2,3,4)], 3)
  RanksMuaTNMed_h_AggCI[[h]][,c(2,3,4)] = signif(RanksMuaTNMed_h_AggCI[[h]][,c(2,3,4)], 3)
  RanksMuaTN95_h_AggCI[[h]][,c(2,3,4)] = signif(RanksMuaTN95_h_AggCI[[h]][,c(2,3,4)], 3)
  
  #In order of parameters in Fig 2
  RanksMua05_h_AggCI[[h]] = RanksMua05_h_AggCI[[h]][match(table = RanksMua05_h_AggCI[[h]]$Param, x = Fig1Order),] 
  RanksMuaot_h_AggCI[[h]] = RanksMuaot_h_AggCI[[h]][match(table = RanksMuaot_h_AggCI[[h]]$Param, x = Fig1Order),]
  RanksMua95_h_AggCI[[h]] = RanksMua95_h_AggCI[[h]][match(table = RanksMua95_h_AggCI[[h]]$Param, x = Fig1Order),]
  RanksMua05g_h_AggCI[[h]] = RanksMua05g_h_AggCI[[h]][match(table = RanksMua05g_h_AggCI[[h]]$Param, x = Fig1Order),]
  RanksMuaotg_h_AggCI[[h]] = RanksMuaotg_h_AggCI[[h]][match(table = RanksMuaotg_h_AggCI[[h]]$Param, x = Fig1Order),]
  RanksMuaTN05_h_AggCI[[h]] = RanksMuaTN05_h_AggCI[[h]][match(table = RanksMuaTN05_h_AggCI[[h]]$Param, x = Fig1Order),]
  RanksMuaTNMed_h_AggCI[[h]] = RanksMuaTNMed_h_AggCI[[h]][match(table = RanksMuaTNMed_h_AggCI[[h]]$Param, x = Fig1Order),]
  RanksMuaTN95_h_AggCI[[h]] = RanksMuaTN95_h_AggCI[[h]][match(table = RanksMuaTN95_h_AggCI[[h]]$Param, x = Fig1Order),]
  
  #Columns in same order as reported for basin
  RanksMua05_h_AggCI[[h]] = RanksMua05_h_AggCI[[h]][,c(1,3,2,4,5)]
  RanksMuaot_h_AggCI[[h]] = RanksMuaot_h_AggCI[[h]][,c(1,3,2,4,5)]
  RanksMua95_h_AggCI[[h]] = RanksMua95_h_AggCI[[h]][,c(1,3,2,4,5)]
  RanksMua05g_h_AggCI[[h]] = RanksMua05g_h_AggCI[[h]][,c(1,3,2,4,5)]
  RanksMuaotg_h_AggCI[[h]] = RanksMuaotg_h_AggCI[[h]][,c(1,3,2,4,5)]
  RanksMuaTN05_h_AggCI[[h]] = RanksMuaTN05_h_AggCI[[h]][,c(1,3,2,4,5)]
  RanksMuaTNMed_h_AggCI[[h]] = RanksMuaTNMed_h_AggCI[[h]][,c(1,3,2,4,5)]
  RanksMuaTN95_h_AggCI[[h]] = RanksMuaTN95_h_AggCI[[h]][,c(1,3,2,4,5)]
}
rm(h)

#Get the top X% unique variables based on the flow and TN metrics----
#Top 10% of the parameters is desired. Get the index of the 10% of non-zero EEs, rounded up
#Note that one could use the bootstrapped mean or the original mean. 
#Maximum difference in EE value:
MaxDiffEEs05Boot = max(abs(muaEEs05_b/max(muaEEs05_b) - EEs05_b_mua_m[1:271]/max(EEs05_b_mua_m[1:271])))
MaxDiffEEsotBoot = max(abs(muaEEsot_b/max(muaEEsot_b) - EEsot_b_mua_m[1:271]/max(EEsot_b_mua_m[1:271])))
MaxDiffEEs95Boot = max(abs(muaEEs95_b/max(muaEEs95_b) - EEs95_b_mua_m[1:271]/max(EEs95_b_mua_m[1:271])))
MaxDiffEEs05gBoot = max(abs(muaEEs05g_b/max(muaEEs05g_b) - EEs05g_b_mua_m[1:271]/max(EEs05g_b_mua_m[1:271])))
MaxDiffEEsotgBoot = max(abs(muaEEsotg_b/max(muaEEsotg_b) - EEsotg_b_mua_m[1:271]/max(EEsotg_b_mua_m[1:271])))
MaxDiffEEsLNSEBoot = max(abs(muaEEsLNSE_b/max(muaEEsLNSE_b) - EEsLNSE_b_mua_m[1:271]/max(EEsLNSE_b_mua_m[1:271])))
MaxDiffEEsNSEBoot = max(abs(muaEEsNSE_b/max(muaEEsNSE_b) - EEsNSE_b_mua_m[1:271]/max(EEsNSE_b_mua_m[1:271])))
MaxDiffEEspBiasBoot = max(abs(muaEEspBias_b/max(muaEEspBias_b) - EEspBias_b_mua_m[1:271]/max(EEspBias_b_mua_m[1:271])))
MaxDiffEEsLogLBoot = max(abs(muaEEsLogL_b/max(muaEEsLogL_b) - EEsLogL_b_mua_m[1:271]/max(EEsLogL_b_mua_m[1:271])))
MaxDiffEEsTN05Boot = max(abs(muaEEsTN05_b/max(muaEEsTN05_b) - EEsTN05_b_mua_m[1:271]/max(EEsTN05_b_mua_m[1:271])))
MaxDiffEEsTNMedBoot = max(abs(muaEEsTNMed_b/max(muaEEsTNMed_b) - EEsTNMed_b_mua_m[1:271]/max(EEsTNMed_b_mua_m[1:271])))
MaxDiffEEsTN95Boot = max(abs(muaEEsTN95_b/max(muaEEsTN95_b) - EEsTN95_b_mua_m[1:271]/max(EEsTN95_b_mua_m[1:271])))
MaxDiffEEs05gmBoot = max(abs(muaEEs05gm_b/max(muaEEs05gm_b) - EEs05gm_b_mua_m[1:271]/max(EEs05gm_b_mua_m[1:271])))
MaxDiffEEsotgmBoot = max(abs(muaEEsotgm_b/max(muaEEsotgm_b) - EEsotgm_b_mua_m[1:271]/max(EEsotgm_b_mua_m[1:271])))
MaxDiffEEs95mBoot = max(abs(muaEEs95m_b/max(muaEEs95m_b) - EEs95m_b_mua_m[1:271]/max(EEs95m_b_mua_m[1:271])))

Top1005 = ceiling((length(muaEEs05_b) - length(which(muaEEs05_b == 0)))*0.1)
Top10ot = ceiling((length(muaEEsot_b) - length(which(muaEEsot_b == 0)))*0.1)
Top1095 = ceiling((length(muaEEs95_b) - length(which(muaEEs95_b == 0)))*0.1)
Top1005g = ceiling((length(muaEEs05g_b) - length(which(muaEEs05g_b == 0)))*0.1)
Top10otg = ceiling((length(muaEEsotg_b) - length(which(muaEEsotg_b == 0)))*0.1)
Top10LNSE = ceiling((length(muaEEsLNSE_b) - length(which(muaEEsLNSE_b == 0)))*0.1)
Top10NSE = ceiling((length(muaEEsNSE_b) - length(which(muaEEsNSE_b == 0)))*0.1)
Top10pBias = ceiling((length(muaEEspBias_b) - length(which(muaEEspBias_b == 0)))*0.1)
Top10LogL = ceiling((length(muaEEsLogL_b) - length(which(muaEEsLogL_b == 0)))*0.1)
Top10TN05 = ceiling((length(muaEEsTN05_b) - length(which(muaEEsTN05_b == 0)))*0.1)
Top10TNMed = ceiling((length(muaEEsTNMed_b) - length(which(muaEEsTNMed_b == 0)))*0.1)
Top10TN95 = ceiling((length(muaEEsTN95_b) - length(which(muaEEsTN95_b == 0)))*0.1)
Top1005gm = ceiling((length(muaEEs05gm_b) - length(which(muaEEs05gm_b == 0)))*0.1)
Top10otgm = ceiling((length(muaEEsotgm_b) - length(which(muaEEsotgm_b == 0)))*0.1)
Top1095m = ceiling((length(muaEEs95m_b) - length(which(muaEEs95m_b == 0)))*0.1)
#Forcing log L to have same selection as all other metrics. Non-zero EEs result from MLE equifinality.
Top10LogL = Top1005

RanksMua_b = unique(c(RanksMua05_b$Param[1:Top1005],RanksMuaot_b$Param[1:Top10ot],RanksMua95_b$Param[1:Top1095]))
RanksMua_bg = unique(c(RanksMua05g_b$Param[1:Top1005g],RanksMuaotg_b$Param[1:Top10otg],RanksMua95_b$Param[1:Top1095]))
RanksMua_bgm = unique(c(RanksMua05gm_b$Param[1:Top1005gm],RanksMuaotgm_b$Param[1:Top10otgm],RanksMua95m_b$Param[1:Top1095m]))
RanksMuaTN_b = unique(c(RanksMuaTN05_b$Param[1:Top10TN05],RanksMuaTNMed_b$Param[1:Top10TNMed],RanksMuaTN95_b$Param[1:Top10TN95]))

#Unique across basin and TN are same for top 10%
length(unique(c(RanksMua_b, RanksMuaTN_b)))

# Select all parameters whose 95th quantile estimate of EE is greater than the selected threshold point
ParamSelect_b = unique(c(names(EEs05_b_mua_95[1:nrow(ParamRanges)][EEs05_b_mua_95[1:nrow(ParamRanges)] >= RanksMua05_b$EE05_b[Top1005]]),
                         names(EEsot_b_mua_95[1:nrow(ParamRanges)][EEsot_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaot_b$EEot_b[Top10ot]]),
                         names(EEs95_b_mua_95[1:nrow(ParamRanges)][EEs95_b_mua_95[1:nrow(ParamRanges)] >= RanksMua95_b$EE95_b[Top1095]])))
ParamSelect_bg = unique(c(names(EEs05g_b_mua_95[1:nrow(ParamRanges)][EEs05g_b_mua_95[1:nrow(ParamRanges)] >= RanksMua05g_b$EE05g_b[Top1005g]]),
                         names(EEsotg_b_mua_95[1:nrow(ParamRanges)][EEsotg_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaotg_b$EEotg_b[Top10otg]]),
                         names(EEs95_b_mua_95[1:nrow(ParamRanges)][EEs95_b_mua_95[1:nrow(ParamRanges)] >= RanksMua95_b$EE95_b[Top1095]])))
ParamSelect_bgm = unique(c(names(EEs05gm_b_mua_95[1:nrow(ParamRanges)][EEs05gm_b_mua_95[1:nrow(ParamRanges)] >= RanksMua05gm_b$EE05gm_b[Top1005gm]]),
                          names(EEsotgm_b_mua_95[1:nrow(ParamRanges)][EEsotgm_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaotgm_b$EEotgm_b[Top10otgm]]),
                          names(EEs95m_b_mua_95[1:nrow(ParamRanges)][EEs95m_b_mua_95[1:nrow(ParamRanges)] >= RanksMua95m_b$EE95m_b[Top1095m]])))
ParamSelectTN_b = unique(c(names(EEsTN05_b_mua_95[1:nrow(ParamRanges)][EEsTN05_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaTN05_b$EETN05_b[Top10TN05]]),
                         names(EEsTNMed_b_mua_95[1:nrow(ParamRanges)][EEsTNMed_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaTNMed_b$EETNMed_b[Top10TNMed]]),
                         names(EEsTN95_b_mua_95[1:nrow(ParamRanges)][EEsTN95_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaTN95_b$EETN95_b[Top10TN95]])))

ParamSelect_b_Cal = unique(c(names(EEsLNSE_b_mua_95[1:nrow(ParamRanges)][EEsLNSE_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaLNSE_b$EELNSE_b[Top10LNSE]]),
                         names(EEsNSE_b_mua_95[1:nrow(ParamRanges)][EEsNSE_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaNSE_b$EENSE_b[Top10NSE]]),
                         names(EEspBias_b_mua_95[1:nrow(ParamRanges)][EEspBias_b_mua_95[1:nrow(ParamRanges)] >= RanksMuapBias_b$EEpBias_b[Top10pBias]]),
                         names(EEsLogL_b_mua_95[1:nrow(ParamRanges)][EEsLogL_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaLogL_b$EELogL_b[Top10LogL]])))

#Some differences using this method
length(unique(c(ParamSelect_b, ParamSelectTN_b)))

# Same, but dropping the aggregated parameters----
Top1005_Agg = ceiling((length(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*0.1)
Top10ot_Agg = ceiling((length(EEsot_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsot_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*0.1)
Top1095_Agg = ceiling((length(EEs95_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEs95_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*0.1)
Top1005g_Agg = ceiling((length(EEs05g_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEs05g_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*0.1)
Top10otg_Agg = ceiling((length(EEsotg_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsotg_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*0.1)
Top10LNSE_Agg = ceiling((length(EEsLNSE_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsLNSE_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*0.1)
Top10NSE_Agg = ceiling((length(EEsNSE_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsNSE_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*0.1)
Top10pBias_Agg = ceiling((length(EEspBias_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEspBias_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*0.1)
Top10LogL_Agg = ceiling((length(EEsLogL_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsLogL_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*0.1)
Top10TN05_Agg = ceiling((length(EEsTN05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsTN05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*0.1)
Top10TNMed_Agg = ceiling((length(EEsTNMed_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsTNMed_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*0.1)
Top10TN95_Agg = ceiling((length(EEsTN95_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsTN95_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*0.1)
Top10LogL_Agg = Top1005_Agg
Top1005gm_Agg = ceiling((length(EEs05gm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)]) - length(which(EEs05gm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)] == 0)))*0.1)
Top10otgm_Agg = ceiling((length(EEsotgm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)]) - length(which(EEsotgm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)] == 0)))*0.1)
Top1095m_Agg = ceiling((length(EEs95m_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)]) - length(which(EEs95m_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)] == 0)))*0.1)

RanksMua_b_Agg = unique(c(RanksMua05_b_Agg$Param[1:Top1005_Agg],RanksMuaot_b_Agg$Param[1:Top10ot_Agg],RanksMua95_b_Agg$Param[1:Top1095_Agg]))
RanksMua_bg_Agg = unique(c(RanksMua05g_b_Agg$Param[1:Top1005g_Agg],RanksMuaotg_b_Agg$Param[1:Top10otg_Agg],RanksMua95_b_Agg$Param[1:Top1095_Agg]))
RanksMua_bgm_Agg = unique(c(RanksMua05gm_b_Agg$Param[1:Top1005gm_Agg],RanksMuaotgm_b_Agg$Param[1:Top10otgm_Agg],RanksMua95m_b_Agg$Param[1:Top1095m_Agg]))
RanksMuaTN_b_Agg = unique(c(RanksMuaTN05_b_Agg$Param[1:Top10TN05_Agg],RanksMuaTNMed_b_Agg$Param[1:Top10TNMed_Agg],RanksMuaTN95_b_Agg$Param[1:Top10TN95_Agg]))

# Select all parameters whose 95th quantile estimate of EE is greater than the selected threshold point
ParamSelect_b_Agg = unique(c(names(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] >= RanksMua05_b_Agg$EE05_b[Top1005_Agg]]),
                         names(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)] >= RanksMuaot_b_Agg$EEot_b[Top10ot_Agg]]),
                         names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)] >= RanksMua95_b_Agg$EE95_b[Top1095_Agg]])))
ParamSelect_bg_Agg = unique(c(names(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)] >= RanksMua05g_b_Agg$EE05g_b[Top1005g_Agg]]),
                             names(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)] >= RanksMuaotg_b_Agg$EEotg_b[Top10otg_Agg]]),
                             names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)] >= RanksMua95_b_Agg$EE95_b[Top1095_Agg]])))
ParamSelect_bgm_Agg = unique(c(names(EEs05gm_b_mua_95[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)][EEs05gm_b_mua_95[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)] >= RanksMua05gm_b_Agg$EE05gm_b[Top1005gm_Agg]]),
                              names(EEsotgm_b_mua_95[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)][EEsotgm_b_mua_95[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)] >= RanksMuaotgm_b_Agg$EEotgm_b[Top10otgm_Agg]]),
                              names(EEs95m_b_mua_95[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)][EEs95m_b_mua_95[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)] >= RanksMua95m_b_Agg$EE95m_b[Top1095m_Agg]])))
ParamSelectTN_b_Agg = unique(c(names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)] >= RanksMuaTN05_b_Agg$EETN05_b[Top10TN05_Agg]]),
                           names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)] >= RanksMuaTNMed_b_Agg$EETNMed_b[Top10TNMed_Agg]]),
                           names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)] >= RanksMuaTN95_b_Agg$EETN95_b[Top10TN95_Agg]])))

ParamSelect_b_Cal_Agg = unique(c(names(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)] >= RanksMuaLNSE_b_Agg$EELNSE_b[Top10LNSE_Agg]]),
                             names(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)] >= RanksMuaNSE_b_Agg$EENSE_b[Top10NSE_Agg]]),
                             names(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)] >= RanksMuapBias_b_Agg$EEpBias_b[Top10pBias_Agg]]),
                             names(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)] >= RanksMuaLogL_b_Agg$EELogL_b[Top10LogL_Agg]])))

length(unique(c(ParamSelect_b_Agg, ParamSelectTN_b_Agg)))

#  Hillslope based selection of parameters----
RanksMua_h_Agg = RanksMua_hg_Agg = RanksMuaTN_h_Agg = NULL
for (h in 1:length(uhills)){
  Top10h05_Agg = ceiling((length(EEs05_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEs05_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*0.1)
  Top10hot_Agg = ceiling((length(EEsot_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEsot_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*0.1)
  Top10h95_Agg = ceiling((length(EEs95_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEs95_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*0.1)
  Top10h05g_Agg = ceiling((length(EEs05g_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEs05g_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*0.1)
  Top10hotg_Agg = ceiling((length(EEsotg_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEsotg_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*0.1)
  Top10hTN05_Agg = ceiling((length(EEsTN05_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEsTN05_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*0.1)
  Top10hTNMed_Agg = ceiling((length(EEsTNMed_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEsTNMed_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*0.1)
  Top10hTN95_Agg = ceiling((length(EEsTN95_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEsTN95_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*0.1)
  
  RanksMua_h_Agg = unique(c(RanksMua_h_Agg, RanksMua05_h_Agg[[h]]$Param[1:Top10h05_Agg],RanksMuaot_h_Agg[[h]]$Param[1:Top10hot_Agg],RanksMua95_h_Agg[[h]]$Param[1:Top10h95_Agg]))
  RanksMua_hg_Agg = unique(c(RanksMua_hg_Agg, RanksMua05g_h_Agg[[h]]$Param[1:Top10h05g_Agg],RanksMuaotg_h_Agg[[h]]$Param[1:Top10hotg_Agg],RanksMua95_h_Agg[[h]]$Param[1:Top10h95_Agg]))
  RanksMuaTN_h_Agg = unique(c(RanksMuaTN_h_Agg, RanksMuaTN05_h_Agg[[h]]$Param[1:Top10hTN05_Agg],RanksMuaTNMed_h_Agg[[h]]$Param[1:Top10hTNMed_Agg],RanksMuaTN95_h_Agg[[h]]$Param[1:Top10hTN95_Agg]))
}
rm(h)

RanksMua_h_Agg910 = RanksMua_hg_Agg910 = NULL
for (h in 9:10){
  Top10h05_Agg = ceiling((length(EEs05_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEs05_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*0.1)
  Top10hot_Agg = ceiling((length(EEsot_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEsot_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*0.1)
  Top10h05g_Agg = ceiling((length(EEs05g_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEs05g_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*0.1)
  Top10hotg_Agg = ceiling((length(EEsotg_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEsotg_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*0.1)
  Top10h95_Agg = ceiling((length(EEs95_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEs95_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*0.1)

  RanksMua_h_Agg910 = unique(c(RanksMua_h_Agg910, RanksMua05_h_Agg[[h]]$Param[1:Top10h05_Agg],RanksMuaot_h_Agg[[h]]$Param[1:Top10hot_Agg],RanksMua95_h_Agg[[h]]$Param[1:Top10h95_Agg]))
  RanksMua_hg_Agg910 = unique(c(RanksMua_hg_Agg910, RanksMua05g_h_Agg[[h]]$Param[1:Top10h05g_Agg],RanksMuaotg_h_Agg[[h]]$Param[1:Top10hotg_Agg],RanksMua95_h_Agg[[h]]$Param[1:Top10h95_Agg]))
}
rm(h)

# Select all parameters whose 95th quantile estimate of EE is greater than the selected threshold point
ParamSelect_h_Agg = ParamSelect_hg_Agg = ParamSelectTN_h_Agg = NULL
for (h in 1:length(uhills)){
  ParamSelect_h_Agg = unique(c(ParamSelect_h_Agg, colnames(EEs05_h_mua_95)[-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))][EEs05_h_mua_95[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] >= RanksMua05_h_Agg[[h]][Top10h05_Agg,2]],
                               colnames(EEsot_h_mua_95)[-c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))][EEsot_h_mua_95[h,-c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))] >= RanksMuaot_h_Agg[[h]][Top10hot_Agg,2]],
                               colnames(EEs95_h_mua_95)[-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))][EEs95_h_mua_95[h,-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))] >= RanksMua95_h_Agg[[h]][Top10h95_Agg,2]]))
  ParamSelect_hg_Agg = unique(c(ParamSelect_hg_Agg, colnames(EEs05g_h_mua_95)[-c(1,which(colnames(EEs05g_h_mua_m) %in% ColsAggregated))][EEs05g_h_mua_95[h,-c(1,which(colnames(EEs05g_h_mua_m) %in% ColsAggregated))] >= RanksMua05g_h_Agg[[h]][Top10h05g_Agg,2]],
                               colnames(EEsotg_h_mua_95)[-c(1,which(colnames(EEsotg_h_mua_m) %in% ColsAggregated))][EEsotg_h_mua_95[h,-c(1,which(colnames(EEsotg_h_mua_m) %in% ColsAggregated))] >= RanksMuaotg_h_Agg[[h]][Top10hotg_Agg,2]],
                               colnames(EEs95_h_mua_95)[-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))][EEs95_h_mua_95[h,-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))] >= RanksMua95_h_Agg[[h]][Top10h95_Agg,2]]))
  ParamSelectTN_h_Agg = unique(c(ParamSelectTN_h_Agg, colnames(EEsTN05_h_mua_95)[-c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))][EEsTN05_h_mua_95[h,-c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))] >= RanksMuaTN05_h_Agg[[h]][Top10hTN05_Agg,2]],
                                                      colnames(EEsTNMed_h_mua_95)[-c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))][EEsTNMed_h_mua_95[h,-c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))] >= RanksMuaTNMed_h_Agg[[h]][Top10hTNMed_Agg,2]],
                                                      colnames(EEsTN95_h_mua_95)[-c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))][EEsTN95_h_mua_95[h,-c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))] >= RanksMuaTN95_h_Agg[[h]][Top10hTN95_Agg,2]]))
}
rm(h)

length(unique(c(ParamSelect_h_Agg, ParamSelectTN_h_Agg)))

ParamSelect_h_Agg910 = ParamSelect_hg_Agg910 = NULL
for (h in 9:10){
  ParamSelect_h_Agg910 = unique(c(ParamSelect_h_Agg910, colnames(EEs05_h_mua_95)[-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))][EEs05_h_mua_95[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] >= RanksMua05_h_Agg[[h]][Top10h05_Agg,2]],
                               colnames(EEsot_h_mua_95)[-c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))][EEsot_h_mua_95[h,-c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))] >= RanksMuaot_h_Agg[[h]][Top10hot_Agg,2]],
                               colnames(EEs95_h_mua_95)[-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))][EEs95_h_mua_95[h,-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))] >= RanksMua95_h_Agg[[h]][Top10h95_Agg,2]]))
  ParamSelect_hg_Agg910 = unique(c(ParamSelect_hg_Agg910, colnames(EEs05g_h_mua_95)[-c(1,which(colnames(EEs05g_h_mua_m) %in% ColsAggregated))][EEs05g_h_mua_95[h,-c(1,which(colnames(EEs05g_h_mua_m) %in% ColsAggregated))] >= RanksMua05g_h_Agg[[h]][Top10h05g_Agg,2]],
                                  colnames(EEsotg_h_mua_95)[-c(1,which(colnames(EEsotg_h_mua_m) %in% ColsAggregated))][EEsotg_h_mua_95[h,-c(1,which(colnames(EEsotg_h_mua_m) %in% ColsAggregated))] >= RanksMuaotg_h_Agg[[h]][Top10hotg_Agg,2]],
                                  colnames(EEs95_h_mua_95)[-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))][EEs95_h_mua_95[h,-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))] >= RanksMua95_h_Agg[[h]][Top10h95_Agg,2]]))
}
rm(h)
#Note: These variables not selected for calibration: 
#"z_trans_coeff1", "v102_epc.day_leafon", "z_trans_coeff2", "v3_epc.gl_smax", "s9_pore_size_index", "Soil8_m", "Soil9_porosity_0"

#Make a plot of the number of parameters selected versus the threshold percentage----
dir.create(dir_ParamInCutoff, showWarnings = FALSE)
setwd(dir_ParamInCutoff)
x = seq(0.01,1,0.01)
ParamTotals_ThreshPercent = ParamTotals_ThreshPercent_g = ParamTotals_ThreshPercent_gm = vector('numeric', length(x))
for (i in 1:length(x)){
  Tops = ceiling((length(muaEEs05_b) - length(which(muaEEs05_b == 0)))*x[i])
  # Select all parameters whose 95th quantile estimate of EE is greater than the selected threshold point
  pb = unique(c(names(EEs05_b_mua_95[1:nrow(ParamRanges)][EEs05_b_mua_95[1:nrow(ParamRanges)] >= RanksMua05_b$EE05_b[Tops]]),
                           names(EEsot_b_mua_95[1:nrow(ParamRanges)][EEsot_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaot_b$EEot_b[Tops]]),
                           names(EEs95_b_mua_95[1:nrow(ParamRanges)][EEs95_b_mua_95[1:nrow(ParamRanges)] >= RanksMua95_b$EE95_b[Tops]])))
  pbg = unique(c(names(EEs05g_b_mua_95[1:nrow(ParamRanges)][EEs05g_b_mua_95[1:nrow(ParamRanges)] >= RanksMua05g_b$EE05g_b[Tops]]),
                names(EEsotg_b_mua_95[1:nrow(ParamRanges)][EEsotg_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaotg_b$EEotg_b[Tops]]),
                names(EEs95_b_mua_95[1:nrow(ParamRanges)][EEs95_b_mua_95[1:nrow(ParamRanges)] >= RanksMua95_b$EE95_b[Tops]])))
  pbgm = unique(c(names(EEs05gm_b_mua_95[1:nrow(ParamRanges)][EEs05gm_b_mua_95[1:nrow(ParamRanges)] >= RanksMua05gm_b$EE05gm_b[Tops]]),
                 names(EEsotgm_b_mua_95[1:nrow(ParamRanges)][EEsotgm_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaotgm_b$EEotgm_b[Tops]]),
                 names(EEs95m_b_mua_95[1:nrow(ParamRanges)][EEs95m_b_mua_95[1:nrow(ParamRanges)] >= RanksMua95m_b$EE95m_b[Tops]])))
  pbTN = unique(c(names(EEsTN05_b_mua_95[1:nrow(ParamRanges)][EEsTN05_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaTN05_b$EETN05_b[Tops]]),
                             names(EEsTNMed_b_mua_95[1:nrow(ParamRanges)][EEsTNMed_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaTNMed_b$EETNMed_b[Tops]]),
                             names(EEsTN95_b_mua_95[1:nrow(ParamRanges)][EEsTN95_b_mua_95[1:nrow(ParamRanges)] >= RanksMuaTN95_b$EETN95_b[Tops]])))
  
  ParamTotals_ThreshPercent[i] = length(unique(c(pb, pbTN)))
  ParamTotals_ThreshPercent_g[i] = length(unique(c(pbg, pbTN)))
  ParamTotals_ThreshPercent_gm[i] = length(unique(c(pbgm, pbTN)))
}
rm(pb, pbg, pbTN, Tops, i, x, pbgm)

png('ParamsInThresholdCutoff_b.png', res = 300, units = 'in', width = 5, height = 5)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent, type = 'l', xlab = 'Top X Percent Selected', ylab = 'Number of Parameters Selected', ylim = c(0,120), xlim = c(0,100))
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_g, type = 'l', xlab = 'Top X Percent Selected', ylab = 'Number of Parameters Selected', ylim = c(0,120), xlim = c(0,100), col = 'red')
legend('topleft', legend = c('Based on Global Lower 5th', 'Based on Annual Lower 5th'), lty = 1, col = c('red', 'black'))
dev.off()

# Aggregated----
x = seq(0.01,1,0.01)
ParamTotals_ThreshPercent_Agg = ParamTotals_ThreshPercent_g_Agg = ParamTotals_ThreshPercent_gm_Agg = vector('numeric', length(x))
for (i in 1:length(x)){
  Tops05 = ceiling((length(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  Topsot = ceiling((length(EEsot_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsot_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  Topsotg = ceiling((length(EEsotg_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsotg_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  Tops05g = ceiling((length(EEs05g_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEs05g_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  Tops95 = ceiling((length(EEs95_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEs95_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  Topsotgm = ceiling((length(EEsotgm_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsotgm_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  Tops05gm = ceiling((length(EEs05gm_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEs05gm_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  Tops95m = ceiling((length(EEs95m_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEs95m_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  TopsTN05 = ceiling((length(EEsTN05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsTN05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  TopsTNMed = ceiling((length(EEsTNMed_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsTNMed_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  TopsTN95 = ceiling((length(EEsTN95_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsTN95_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  
  # Select all parameters whose 95th quantile estimate of EE is greater than the selected threshold point
  pb = unique(c(names(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] >= RanksMua05_b_Agg$EE05_b[Tops05]]),
                               names(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)] >= RanksMuaot_b_Agg$EEot_b[Topsot]]),
                               names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)] >= RanksMua95_b_Agg$EE95_b[Tops95]])))
  pbg = unique(c(names(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)] >= RanksMua05g_b_Agg$EE05g_b[Tops05g]]),
                names(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)] >= RanksMuaotg_b_Agg$EEotg_b[Topsotg]]),
                names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)] >= RanksMua95_b_Agg$EE95_b[Tops95]])))
  pbgm = unique(c(names(EEs05gm_b_mua_95[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)][EEs05gm_b_mua_95[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)] >= RanksMua05gm_b_Agg$EE05gm_b[Tops05gm]]),
                 names(EEsotgm_b_mua_95[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)][EEsotgm_b_mua_95[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)] >= RanksMuaotgm_b_Agg$EEotgm_b[Topsotgm]]),
                 names(EEs95m_b_mua_95[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)][EEs95m_b_mua_95[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)] >= RanksMua95m_b_Agg$EE95m_b[Tops95m]])))
  pbTN = unique(c(names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)] >= RanksMuaTN05_b_Agg$EETN05_b[TopsTN05]]),
                                 names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)] >= RanksMuaTNMed_b_Agg$EETNMed_b[TopsTNMed]]),
                                 names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)] >= RanksMuaTN95_b_Agg$EETN95_b[TopsTN95]])))
  
  ParamTotals_ThreshPercent_Agg[i] = length(unique(c(pb, pbTN)))
  ParamTotals_ThreshPercent_g_Agg[i] = length(unique(c(pbg, pbTN)))
  ParamTotals_ThreshPercent_gm_Agg[i] = length(unique(c(pbgm, pbTN)))
}
rm(pb, pbg, pbgm, pbTN, Tops05, Topsot, Tops05g, Topsotg, Tops95, Tops05gm, Topsotgm, Tops95m, TopsTN05, TopsTNMed, TopsTN95, i, x)

png('ParamsInThresholdCutoff_b_Agg.png', res = 300, units = 'in', width = 5, height = 5)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_Agg, type = 'l', xlab = 'Top X Percent Selected', ylab = 'Number of Parameters Selected', ylim = c(0,105), xlim = c(0,100))
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_g_Agg, type = 'l', xlab = 'Top X Percent Selected', ylab = 'Number of Parameters Selected', ylim = c(0,105), xlim = c(0,100), col = 'red')
legend('topleft', legend = c('Based on Global Lower 5th', 'Based on Annual Lower 5th'), lty = 1, col = c('red', 'black'))
dev.off()

# Aggregated with hillslope variables----
x = seq(0.01,1,0.01)
ParamTotals_b_Agg = ParamTotals_bg_Agg = ParamTotals_bgm_Agg = vector('numeric', length(x))
ParamTotals_h_Agg = ParamTotals_hg_Agg = vector('numeric', length(x))
ParamTotals_ThreshPercent_h_Agg = ParamTotals_ThreshPercent_hg_Agg = ParamTotals_ThreshPercent_hgm_Agg = vector('numeric', length(x))
ParamTotals_ThreshPercent_onlyh_Agg = ParamTotals_ThreshPercent_onlyhg_Agg = vector('numeric', length(x))
ParamTotals_hg_Agg_NoTN = ParamTotals_ThreshPercent_hg_Agg_NoTN = ParamTotals_ThreshPercent_hgm_Agg_NoTN = ParamTotals_ThreshPercent_onlyhg_Agg_NoTN = vector('numeric', length(x))
for (i in 1:length(x)){
  Tops05 = ceiling((length(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  Topsot = ceiling((length(EEsot_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsot_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  Tops05g = ceiling((length(EEs05g_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEs05g_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  Topsotg = ceiling((length(EEsotg_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsotg_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  Tops95 = ceiling((length(EEs95_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEs95_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  Tops05gm = ceiling((length(EEs05gm_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEs05gm_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  Topsotgm = ceiling((length(EEsotgm_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsotgm_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  Tops95m = ceiling((length(EEs95m_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEs95m_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  TopsTN05 = ceiling((length(EEsTN05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsTN05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  TopsTNMed = ceiling((length(EEsTNMed_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsTNMed_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  TopsTN95 = ceiling((length(EEsTN95_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]) - length(which(EEsTN95_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] == 0)))*x[i])
  
  # Select only parameters based on mean
  pb_mean = unique(c(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] >= RanksMua05_b_Agg$EE05_b[Tops05]]),
                     names(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)] >= RanksMuaot_b_Agg$EEot_b[Topsot]]),
                     names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)] >= RanksMua95_b_Agg$EE95_b[Tops95]]), 
                     names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)] >= RanksMuaTN05_b_Agg$EETN05_b[TopsTN05]]),
                     names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)] >= RanksMuaTNMed_b_Agg$EETNMed_b[TopsTNMed]]),
                     names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)] >= RanksMuaTN95_b_Agg$EETN95_b[TopsTN95]])))
  pbg_mean = unique(c(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)] >= RanksMua05g_b_Agg$EE05g_b[Tops05g]]),
                     names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)] >= RanksMuaotg_b_Agg$EEotg_b[Topsotg]]),
                     names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)] >= RanksMua95_b_Agg$EE95_b[Tops95]]), 
                     names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)] >= RanksMuaTN05_b_Agg$EETN05_b[TopsTN05]]),
                     names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)] >= RanksMuaTNMed_b_Agg$EETNMed_b[TopsTNMed]]),
                     names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)] >= RanksMuaTN95_b_Agg$EETN95_b[TopsTN95]])))
  pbgm_mean = unique(c(names(EEs05gm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)][EEs05gm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)] >= RanksMua05gm_b_Agg$EE05gm_b[Tops05gm]]),
                      names(EEsotgm_b_mua_m[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)][EEsotgm_b_mua_m[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)] >= RanksMuaotgm_b_Agg$EEotgm_b[Topsotgm]]),
                      names(EEs95m_b_mua_m[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)][EEs95m_b_mua_m[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)] >= RanksMua95m_b_Agg$EE95m_b[Tops95m]]), 
                      names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)] >= RanksMuaTN05_b_Agg$EETN05_b[TopsTN05]]),
                      names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)] >= RanksMuaTNMed_b_Agg$EETNMed_b[TopsTNMed]]),
                      names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)] >= RanksMuaTN95_b_Agg$EETN95_b[TopsTN95]])))
  
  # Select all parameters whose 95th quantile estimate of EE is greater than the selected threshold point
  pb = unique(c(names(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)] >= RanksMua05_b_Agg$EE05_b[Tops05]]),
                names(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)] >= RanksMuaot_b_Agg$EEot_b[Topsot]]),
                names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)] >= RanksMua95_b_Agg$EE95_b[Tops95]])))
  pbg = unique(c(names(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)] >= RanksMua05g_b_Agg$EE05g_b[Tops05g]]),
                names(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)] >= RanksMuaotg_b_Agg$EEotg_b[Topsotg]]),
                names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)] >= RanksMua95_b_Agg$EE95_b[Tops95]])))
  pbgm = unique(c(names(EEs05gm_b_mua_95[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)][EEs05gm_b_mua_95[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)] >= RanksMua05gm_b_Agg$EE05gm_b[Tops05gm]]),
                 names(EEsotgm_b_mua_95[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)][EEsotgm_b_mua_95[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)] >= RanksMuaotgm_b_Agg$EEotgm_b[Topsotgm]]),
                 names(EEs95m_b_mua_95[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)][EEs95m_b_mua_95[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)] >= RanksMua95m_b_Agg$EE95m_b[Tops95m]])))
  pbTN = unique(c(names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)] >= RanksMuaTN05_b_Agg$EETN05_b[TopsTN05]]),
                  names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)] >= RanksMuaTNMed_b_Agg$EETNMed_b[TopsTNMed]]),
                  names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)] >= RanksMuaTN95_b_Agg$EETN95_b[TopsTN95]])))
  
  # Select all parameters whose 95th quantile estimate of EE is greater than the selected threshold point
  ph_mean = ph = phg_mean = phg = phTN = phg_mean_NoTN = NULL
  if (round(x[i],2) == 0.1){
    ph10 = ph10g = phTN10 = list()
  }
  for (h in 1:length(uhills)){
    Topsh05 = ceiling((length(EEs05_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEs05_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*x[i])
    Topshot = ceiling((length(EEsot_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEsot_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*x[i])
    Topshotg = ceiling((length(EEsotg_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEsotg_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*x[i])
    Topsh05g = ceiling((length(EEs05g_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEs05g_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*x[i])
    Topsh95 = ceiling((length(EEs95_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEs95_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*x[i])
    TopshTN05 = ceiling((length(EEsTN05_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEsTN05_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*x[i])
    TopshTNMed = ceiling((length(EEsTNMed_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEsTNMed_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*x[i])
    TopshTN95 = ceiling((length(EEsTN95_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))]) - length(which(EEsTN95_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] == 0)))*x[i])
    
    ph_mean = unique(c(ph_mean, 
                       colnames(EEs05_h_mua_m)[-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))][EEs05_h_mua_m[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] >= RanksMua05_h_Agg[[h]][Topsh05,2]],
                       colnames(EEsot_h_mua_m)[-c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))][EEsot_h_mua_m[h,-c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))] >= RanksMuaot_h_Agg[[h]][Topshot,2]],
                       colnames(EEs95_h_mua_m)[-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))][EEs95_h_mua_m[h,-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))] >= RanksMua95_h_Agg[[h]][Topsh95,2]],
                       colnames(EEsTN05_h_mua_m)[-c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))][EEsTN05_h_mua_m[h,-c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))] >= RanksMuaTN05_h_Agg[[h]][TopshTN05,2]],
                       colnames(EEsTNMed_h_mua_m)[-c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))][EEsTNMed_h_mua_m[h,-c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))] >= RanksMuaTNMed_h_Agg[[h]][TopshTNMed,2]],
                       colnames(EEsTN95_h_mua_m)[-c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))][EEsTN95_h_mua_m[h,-c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))] >= RanksMuaTN95_h_Agg[[h]][TopshTN95,2]]))
    phg_mean = unique(c(phg_mean, 
                       colnames(EEs05g_h_mua_m)[-c(1,which(colnames(EEs05g_h_mua_m) %in% ColsAggregated))][EEs05g_h_mua_m[h,-c(1,which(colnames(EEs05g_h_mua_m) %in% ColsAggregated))] >= RanksMua05g_h_Agg[[h]][Topsh05g,2]],
                       colnames(EEsotg_h_mua_m)[-c(1,which(colnames(EEsotg_h_mua_m) %in% ColsAggregated))][EEsotg_h_mua_m[h,-c(1,which(colnames(EEsotg_h_mua_m) %in% ColsAggregated))] >= RanksMuaotg_h_Agg[[h]][Topshotg,2]],
                       colnames(EEs95_h_mua_m)[-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))][EEs95_h_mua_m[h,-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))] >= RanksMua95_h_Agg[[h]][Topsh95,2]],
                       colnames(EEsTN05_h_mua_m)[-c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))][EEsTN05_h_mua_m[h,-c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))] >= RanksMuaTN05_h_Agg[[h]][TopshTN05,2]],
                       colnames(EEsTNMed_h_mua_m)[-c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))][EEsTNMed_h_mua_m[h,-c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))] >= RanksMuaTNMed_h_Agg[[h]][TopshTNMed,2]],
                       colnames(EEsTN95_h_mua_m)[-c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))][EEsTN95_h_mua_m[h,-c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))] >= RanksMuaTN95_h_Agg[[h]][TopshTN95,2]]))
    
    phg_mean_NoTN = unique(c(phg_mean_NoTN, 
                             colnames(EEs05g_h_mua_m)[-c(1,which(colnames(EEs05g_h_mua_m) %in% ColsAggregated))][EEs05g_h_mua_m[h,-c(1,which(colnames(EEs05g_h_mua_m) %in% ColsAggregated))] >= RanksMua05g_h_Agg[[h]][Topsh05g,2]],
                             colnames(EEsotg_h_mua_m)[-c(1,which(colnames(EEsotg_h_mua_m) %in% ColsAggregated))][EEsotg_h_mua_m[h,-c(1,which(colnames(EEsotg_h_mua_m) %in% ColsAggregated))] >= RanksMuaotg_h_Agg[[h]][Topshotg,2]],
                             colnames(EEs95_h_mua_m)[-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))][EEs95_h_mua_m[h,-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))] >= RanksMua95_h_Agg[[h]][Topsh95,2]]))
    
    ph = unique(c(ph, colnames(EEs05_h_mua_95)[-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))][EEs05_h_mua_95[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] >= RanksMua05_h_Agg[[h]][Topsh05,2]],
                                 colnames(EEsot_h_mua_95)[-c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))][EEsot_h_mua_95[h,-c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))] >= RanksMuaot_h_Agg[[h]][Topshot,2]],
                                 colnames(EEs95_h_mua_95)[-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))][EEs95_h_mua_95[h,-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))] >= RanksMua95_h_Agg[[h]][Topsh95,2]]))
    phg = unique(c(phg, colnames(EEs05g_h_mua_95)[-c(1,which(colnames(EEs05g_h_mua_m) %in% ColsAggregated))][EEs05g_h_mua_95[h,-c(1,which(colnames(EEs05g_h_mua_m) %in% ColsAggregated))] >= RanksMua05g_h_Agg[[h]][Topsh05g,2]],
                  colnames(EEsotg_h_mua_95)[-c(1,which(colnames(EEsotg_h_mua_m) %in% ColsAggregated))][EEsotg_h_mua_95[h,-c(1,which(colnames(EEsotg_h_mua_m) %in% ColsAggregated))] >= RanksMuaotg_h_Agg[[h]][Topshotg,2]],
                  colnames(EEs95_h_mua_95)[-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))][EEs95_h_mua_95[h,-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))] >= RanksMua95_h_Agg[[h]][Topsh95,2]]))
    phTN = unique(c(phTN, colnames(EEsTN05_h_mua_95)[-c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))][EEsTN05_h_mua_95[h,-c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))] >= RanksMuaTN05_h_Agg[[h]][TopshTN05,2]],
                                   colnames(EEsTNMed_h_mua_95)[-c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))][EEsTNMed_h_mua_95[h,-c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))] >= RanksMuaTNMed_h_Agg[[h]][TopshTNMed,2]],
                                   colnames(EEsTN95_h_mua_95)[-c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))][EEsTN95_h_mua_95[h,-c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))] >= RanksMuaTN95_h_Agg[[h]][TopshTN95,2]]))
    if (round(x[i],2) == 0.10){
      ph10 = c(ph10, list(unique(c(colnames(EEs05_h_mua_95)[-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))][EEs05_h_mua_95[h,-c(1,which(colnames(EEs05_h_mua_m) %in% ColsAggregated))] >= RanksMua05_h_Agg[[h]][Topsh05,2]],
                      colnames(EEsot_h_mua_95)[-c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))][EEsot_h_mua_95[h,-c(1,which(colnames(EEsot_h_mua_m) %in% ColsAggregated))] >= RanksMuaot_h_Agg[[h]][Topshot,2]],
                      colnames(EEs95_h_mua_95)[-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))][EEs95_h_mua_95[h,-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))] >= RanksMua95_h_Agg[[h]][Topsh95,2]]))))
      ph10g = c(ph10g, list(unique(c(colnames(EEs05g_h_mua_95)[-c(1,which(colnames(EEs05g_h_mua_m) %in% ColsAggregated))][EEs05g_h_mua_95[h,-c(1,which(colnames(EEs05g_h_mua_m) %in% ColsAggregated))] >= RanksMua05g_h_Agg[[h]][Topsh05g,2]],
                                   colnames(EEsotg_h_mua_95)[-c(1,which(colnames(EEsotg_h_mua_m) %in% ColsAggregated))][EEsotg_h_mua_95[h,-c(1,which(colnames(EEsotg_h_mua_m) %in% ColsAggregated))] >= RanksMuaotg_h_Agg[[h]][Topshotg,2]],
                                   colnames(EEs95_h_mua_95)[-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))][EEs95_h_mua_95[h,-c(1,which(colnames(EEs95_h_mua_m) %in% ColsAggregated))] >= RanksMua95_h_Agg[[h]][Topsh95,2]]))))
      phTN10 = c(phTN10, list(unique(c(colnames(EEsTN05_h_mua_95)[-c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))][EEsTN05_h_mua_95[h,-c(1,which(colnames(EEsTN05_h_mua_m) %in% ColsAggregated))] >= RanksMuaTN05_h_Agg[[h]][TopshTN05,2]],
                      colnames(EEsTNMed_h_mua_95)[-c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))][EEsTNMed_h_mua_95[h,-c(1,which(colnames(EEsTNMed_h_mua_m) %in% ColsAggregated))] >= RanksMuaTNMed_h_Agg[[h]][TopshTNMed,2]],
                      colnames(EEsTN95_h_mua_95)[-c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))][EEsTN95_h_mua_95[h,-c(1,which(colnames(EEsTN95_h_mua_m) %in% ColsAggregated))] >= RanksMuaTN95_h_Agg[[h]][TopshTN95,2]]))))
    }
  }
  
  ParamTotals_b_Agg[i] = length(unique(c(pb_mean)))
  ParamTotals_bg_Agg[i] = length(unique(c(pbg_mean)))
  ParamTotals_bgm_Agg[i] = length(unique(c(pbgm_mean)))
  ParamTotals_h_Agg[i] = length(unique(c(ph_mean)))
  ParamTotals_hg_Agg[i] = length(unique(c(phg_mean)))
  ParamTotals_ThreshPercent_h_Agg[i] = length(unique(c(pb, pbTN, ph, phTN)))
  ParamTotals_ThreshPercent_hg_Agg[i] = length(unique(c(pbg, pbTN, phg, phTN)))
  ParamTotals_ThreshPercent_hgm_Agg[i] = length(unique(c(pbgm, pbTN, phg, phTN)))
  ParamTotals_ThreshPercent_onlyh_Agg[i] = length(unique(c(ph, phTN)))
  ParamTotals_ThreshPercent_onlyhg_Agg[i] = length(unique(c(phg, phTN)))
  ParamTotals_ThreshPercent_onlyhg_Agg_NoTN[i] = length(unique(c(phg)))
  ParamTotals_ThreshPercent_hg_Agg_NoTN[i] = length(unique(c(pbg, pbTN, phg)))
  ParamTotals_ThreshPercent_hgm_Agg_NoTN[i] = length(unique(c(pbgm, pbTN, phg)))
  ParamTotals_hg_Agg_NoTN[i] = length(unique(c(phg_mean_NoTN)))
}
rm(pb, pbg, pbgm, pbgm_mean, pbTN, Tops05, Tops05g, Tops95, Topsot, Topsotg, Tops05gm, Tops95m, Topsotgm, Topsh05, Topsh95, Topshot, Topsh05g, Topshotg, TopsTN05, TopsTNMed, TopsTN95, TopshTN05, TopshTNMed, TopshTN95, i, x, h, ph, phg, phTN, pb_mean, pbg_mean, ph_mean, phg_mean, phg_mean_NoTN)

png('ParamsInThresholdCutoff_h_Agg.png', res = 300, units = 'in', width = 5, height = 5)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_h_Agg, type = 'l', xlab = 'Top X Percent Selected', ylab = 'Number of Parameters Selected', ylim = c(0,105), xlim = c(0,100))
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_hg_Agg, type = 'l', xlab = 'Top X Percent Selected', ylab = 'Number of Parameters Selected', ylim = c(0,105), xlim = c(0,100), col = 'red')
legend('bottomright', legend = c('Based on Global Lower 5th', 'Based on Annual Lower 5th'), lty = 1, col = c('red', 'black'))
dev.off()

png('ParamsInThresholdCutoff_bh_Agg.png', res = 300, units = 'in', width = 5, height = 5)
par(mar=c(4,4,0.5,0.5))
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_onlyh_Agg, type = 'l', xlab = 'Top X Percent Selected', ylab = 'Number of Parameters Selected', ylim = c(0,105), xlim = c(0,100), col = 'gray')
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_h_Agg, type = 'l', ylim = c(0,105), xlim = c(0,100), col = 'black', xlab = '', ylab = '', axes = FALSE)
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_Agg, type = 'o', ylim = c(0,105), xlim = c(0,100), col = 'blue', xlab = '', ylab = '', axes = FALSE, pch = 15, cex = 0.3)
lines(c(10,10), c(-10,200), lty=2, col='gray')
legend('bottomright', legend = c('Hillslope + Basin Outlets', 'Hillslope Outlets Only', 'Basin Outlet Only', 'Selected Percentage'), lty = c(1,1,1,2), col = c('black', 'gray', 'blue', 'gray'), pch = c(NA,NA,15,NA), pt.cex = 0.3)
dev.off()

png('ParamsInThresholdCutoff_bhg_Agg.png', res = 300, units = 'in', width = 5, height = 5)
par(mar=c(4,4,0.5,0.5))
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_onlyhg_Agg, type = 'l', xlab = 'Top X Percent Selected', ylab = 'Number of Parameters Selected', ylim = c(0,105), xlim = c(0,100), col = 'gray')
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_hg_Agg, type = 'l', ylim = c(0,105), xlim = c(0,100), col = 'black', xlab = '', ylab = '', axes = FALSE)
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_g_Agg, type = 'o', ylim = c(0,105), xlim = c(0,100), col = 'blue', xlab = '', ylab = '', axes = FALSE, pch = 15, cex = 0.3)
lines(c(10,10), c(-10,200), lty=2, col='gray')
legend('bottomright', legend = c('Hillslope + Basin Outlets', 'Hillslope Outlets Only', 'Basin Outlet Only', 'Selected Percentage'), lty = c(1,1,1,2), col = c('black', 'gray', 'blue', 'gray'), pch = c(NA,NA,15,NA), pt.cex = 0.3)
dev.off()

png('ParamsInThresholdCutoff_bhg_Agg_Mean.png', res = 300, units = 'in', width = 5, height = 5)
par(mar=c(4,4,0.5,0.5))
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_onlyhg_Agg, type = 'l', xlab = 'Top X Percent of Parameters Selected', ylab = 'Number of Parameters Selected', ylim = c(0,105), xlim = c(0,100), col = 'gray')
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_hg_Agg, type = 'l', ylim = c(0,105), xlim = c(0,100), col = 'gray', xlab = '', ylab = '', axes = FALSE, cex = 0.4)
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_hg_Agg, type = 'l', ylim = c(0,105), xlim = c(0,100), col = 'black', xlab = '', ylab = '', axes = FALSE)
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_bg_Agg, type = 'o', pch = 16, ylim = c(0,105), xlim = c(0,100), col = 'skyblue', xlab = '', ylab = '', axes = FALSE, cex = 0.4)
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_g_Agg, type = 'o', ylim = c(0,105), xlim = c(0,100), col = 'blue', xlab = '', ylab = '', axes = FALSE, pch = 16, cex = 0.4)
lines(c(10,10), c(-10,200), lty=2, col='gray')
legend('bottomright', legend = c('95th: Hillslopes', 'Mean: Hillslopes', '95th: Basin', 'Mean: Basin', 'Selected Percentage'), lty = c(1,1,1,1,2), col = c('black', 'gray', 'blue', 'skyblue', 'gray'), pch = c(NA,NA,16,16,NA), pt.cex = 0.4)
dev.off()

png('ParamsInThresholdCutoff_bh_Agg_Mean.png', res = 300, units = 'in', width = 5, height = 5)
par(mar=c(4,4,0.5,0.5))
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_onlyh_Agg, type = 'l', xlab = 'Top X Percent of Parameters Selected', ylab = 'Number of Parameters Selected', ylim = c(0,105), xlim = c(0,100), col = 'gray')
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_h_Agg, type = 'l', ylim = c(0,105), xlim = c(0,100), col = 'gray', xlab = '', ylab = '', axes = FALSE, cex = 0.4)
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_h_Agg, type = 'l', ylim = c(0,105), xlim = c(0,100), col = 'black', xlab = '', ylab = '', axes = FALSE)
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_b_Agg, type = 'o', pch = 16, ylim = c(0,105), xlim = c(0,100), col = 'skyblue', xlab = '', ylab = '', axes = FALSE, cex = 0.4)
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_Agg, type = 'o', ylim = c(0,105), xlim = c(0,100), col = 'blue', xlab = '', ylab = '', axes = FALSE, pch = 16, cex = 0.4)
lines(c(10,10), c(-10,200), lty=2, col='gray')
legend('bottomright', legend = c('95th: Hillslopes', 'Mean: Hillslopes', '95th: Basin', 'Mean: Basin', 'Selected Percentage'), lty = c(1,1,1,1,2), col = c('black', 'gray', 'blue', 'skyblue', 'gray'), pch = c(NA,NA,16,16,NA), pt.cex = 0.4)
dev.off()

png(file = 'ParamsInThresholdCutoff_bhg_Agg_Mean_NoHillTN.png', width = 5, height = 5, units = 'in', res = 300)
par(mar=c(4,4,0.5,0.5))
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_onlyhg_Agg_NoTN, type = 'l', xlab = 'Top X Percent of Parameters Selected', ylab = 'Number of Parameters Selected', ylim = c(0,105), xlim = c(0,100), col = 'gray')
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_hg_Agg_NoTN, type = 'l', ylim = c(0,105), xlim = c(0,100), col = 'gray', xlab = '', ylab = '', axes = FALSE, cex = 0.4)
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_hg_Agg_NoTN, type = 'l', ylim = c(0,105), xlim = c(0,100), col = 'black', xlab = '', ylab = '', axes = FALSE)
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_bg_Agg, type = 'o', pch = 16, ylim = c(0,105), xlim = c(0,100), col = 'skyblue', xlab = '', ylab = '', axes = FALSE, cex = 0.4)
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_g_Agg, type = 'o', ylim = c(0,105), xlim = c(0,100), col = 'blue', xlab = '', ylab = '', axes = FALSE, pch = 16, cex = 0.4)
lines(c(10,10), c(-10,200), lty=2, col='gray')
legend('bottomright', legend = c('95th: Hillslopes (No TN Metrics)', 'Mean: Hillslopes (No TN Metrics)', '95th: Basin', 'Mean: Basin', 'Selected Percentage'), lty = c(1,1,1,1,2), col = c('black', 'gray', 'blue', 'skyblue', 'gray'), pch = c(NA,NA,16,16,NA), pt.cex = 0.4, cex=0.8)
dev.off()

pdf(file = 'fig2.pdf', width = 5, height = 5, title = 'SAfig2')
par(mar=c(4,4,0.5,0.5))
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_onlyhg_Agg_NoTN, type = 'l', xlab = 'Top X Percent of Parameters Selected', ylab = 'Number of Parameters Selected', ylim = c(0,105), xlim = c(0,100), col = 'gray')
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_hg_Agg_NoTN, type = 'l', ylim = c(0,105), xlim = c(0,100), col = 'gray', xlab = '', ylab = '', axes = FALSE, cex = 0.4)
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_hg_Agg_NoTN, type = 'l', ylim = c(0,105), xlim = c(0,100), col = 'black', xlab = '', ylab = '', axes = FALSE)
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_bg_Agg, type = 'o', pch = 16, ylim = c(0,105), xlim = c(0,100), col = 'skyblue', xlab = '', ylab = '', axes = FALSE, cex = 0.4)
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_g_Agg, type = 'o', ylim = c(0,105), xlim = c(0,100), col = 'blue', xlab = '', ylab = '', axes = FALSE, pch = 16, cex = 0.4)
lines(c(10,10), c(-10,200), lty=2, col='gray')
legend('bottomright', title = 'Bootstrapped EE Value: Scale', legend = c('95th: Hillslopes (No TN Metrics)', 'Mean: Hillslopes (No TN Metrics)', '95th: Basin', 'Mean: Basin', 'Selected Percentage'), lty = c(1,1,1,1,2), col = c('black', 'gray', 'blue', 'skyblue', 'gray'), pch = c(NA,NA,16,16,NA), pt.cex = 0.4, cex=0.8)
dev.off()

pdf(file = 'fig2m.pdf', width = 5, height = 5, title = 'SAfig2m')
par(mar=c(4,4,0.5,0.5))
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_onlyhg_Agg_NoTN, type = 'l', xlab = 'Top X Percent of Parameters Selected', ylab = 'Number of Parameters Selected', ylim = c(0,105), xlim = c(0,100), col = 'gray')
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_hg_Agg_NoTN, type = 'l', ylim = c(0,105), xlim = c(0,100), col = 'gray', xlab = '', ylab = '', axes = FALSE, cex = 0.4)
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_hgm_Agg_NoTN, type = 'l', ylim = c(0,105), xlim = c(0,100), col = 'black', xlab = '', ylab = '', axes = FALSE)
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_bgm_Agg, type = 'o', pch = 16, ylim = c(0,105), xlim = c(0,100), col = 'skyblue', xlab = '', ylab = '', axes = FALSE, cex = 0.4)
par(new=TRUE)
plot(x = seq(1,100,1), y = ParamTotals_ThreshPercent_gm_Agg, type = 'o', ylim = c(0,105), xlim = c(0,100), col = 'blue', xlab = '', ylab = '', axes = FALSE, pch = 16, cex = 0.4)
lines(c(10,10), c(-10,200), lty=2, col='gray')
legend('bottomright', title = 'Bootstrapped EE Value: Scale', legend = c('95th: Hillslopes (No TN Metrics)', 'Mean: Hillslopes (No TN Metrics)', '95th: Basin', 'Mean: Basin', 'Selected Percentage'), lty = c(1,1,1,1,2), col = c('black', 'gray', 'blue', 'skyblue', 'gray'), pch = c(NA,NA,16,16,NA), pt.cex = 0.4, cex=0.8)
dev.off()

setwd(dir_Main)

#Plot EEs by mu, sd, and mua----
dir.create(dir_EEpanels, showWarnings = FALSE)
setwd(dir_EEpanels)

# Color scheme for plotting EEs in categories of parameters----
#One color per category
colos = c(rainbow(12), 'black')
colos[3] = 'gray'
colos_paper = c(scico::scico(n = 5, palette = 'hawaii'), 'black')

# Unaggregated----
#Assign colors to the categories
ColPlots = vector('character', length=cols)
for (i in 1:cols){
  if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 'h'){
    ColPlots[i] = colos[1]
  }else if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 'z'){
    ColPlots[i] = colos[2]
  }else if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 's9'){
    ColPlots[i] = colos[3]
  }else if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 's109'){
    ColPlots[i] = colos[4]
  }else if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 's8'){
    ColPlots[i] = colos[5]
  }else if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 's108'){
    ColPlots[i] = colos[6]
  }else if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 'l1'){
    ColPlots[i] = colos[7]
  }else if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 'l2'){
    ColPlots[i] = colos[8]
  }else if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 'l3'){
    ColPlots[i] = colos[9]
  }else if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 'l4'){
    ColPlots[i] = colos[10]
  }else if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 'v102'){
    ColPlots[i] = colos[11]
  }else if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 'v3'){
    ColPlots[i] = colos[12]
  }else if (strsplit(colnames(InputParams)[i], split = '_', fixed = TRUE)[[1]][1] == 'v4'){
    ColPlots[i] = colos[13]
  }
}
rm(i)

# Aggregated----
#Assign colors to the categories
ColPlots_Agg = vector('character', length=length(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)]))
ColPlots_Agg_Paper = ColPlots_Agg
for (i in 1:length(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)])){
  if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 'h'){
    ColPlots_Agg[i] = colos[1]
    ColPlots_Agg_Paper[i] = colos_paper[1]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 'z'){
    ColPlots_Agg[i] = colos[2]
    ColPlots_Agg_Paper[i] = colos_paper[5]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 's9'){
    ColPlots_Agg[i] = colos[3]
    ColPlots_Agg_Paper[i] = colos_paper[2]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 's109'){
    ColPlots_Agg[i] = colos[4]
    ColPlots_Agg_Paper[i] = colos_paper[2]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 's8'){
    ColPlots_Agg[i] = colos[5]
    ColPlots_Agg_Paper[i] = colos_paper[2]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 's108'){
    ColPlots_Agg[i] = colos[6]
    ColPlots_Agg_Paper[i] = colos_paper[2]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 'l1'){
    ColPlots_Agg[i] = colos[7]
    ColPlots_Agg_Paper[i] = colos_paper[3]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 'l2'){
    ColPlots_Agg[i] = colos[8]
    ColPlots_Agg_Paper[i] = colos_paper[3]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 'l3'){
    ColPlots_Agg[i] = colos[9]
    ColPlots_Agg_Paper[i] = colos_paper[3]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 'l4'){
    ColPlots_Agg[i] = colos[10]
    ColPlots_Agg_Paper[i] = colos_paper[3]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 'v102'){
    ColPlots_Agg[i] = colos[11]
    ColPlots_Agg_Paper[i] = colos_paper[4]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 'v3'){
    ColPlots_Agg[i] = colos[12]
    ColPlots_Agg_Paper[i] = colos_paper[4]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 'v4'){
    ColPlots_Agg[i] = colos[13]
    ColPlots_Agg_Paper[i] = colos_paper[6]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 'Soil8'){
    ColPlots_Agg[i] = 'darkgreen'
    ColPlots_Agg_Paper[i] = colos_paper[2]
  }else if (strsplit(colnames(EEs05_b_mua)[-which(colnames(EEs05_b_mua) %in% ColsAggregated)][i], split = '_', fixed = TRUE)[[1]][1] == 'Soil9'){
    ColPlots_Agg[i] = 'darkgray'
    ColPlots_Agg_Paper[i] = colos_paper[2]
  }
}
rm(i)

#Make plots of the sd vs. mu of EEs - basin----
png('EE05_sdVsMu_b.png', res = 300, units = 'in', height = 7, width = 7)
plot(x = muEEs05_b, y = sdEEs05_b, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SAE for Lower 5th Percentile of Flow', col = ColPlots)
legend('bottomleft', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos)
dev.off()
png('EE95_sdVsMu_b.png', res = 300, units = 'in', height = 7, width = 7)
plot(x = muEEs95_b, y = sdEEs95_b, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SAE for Upper 5th Percentile of Flow', col = ColPlots)
legend('topleft', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos)
dev.off()
png('EEot_sdVsMu_b.png', res = 300, units = 'in', height = 7, width = 7)
plot(x = muEEsot_b, y = sdEEsot_b, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SAE for 5th-95th Percentile Flows', col = ColPlots, xlim = c(-3000,3000))
legend('bottomleft', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos)
dev.off()
png('EETN05_sdVsMu_b.png', res = 300, units = 'in', height = 7, width = 7)
plot(x = muEEsTN05_b, y = sdEEsTN05_b, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SAE for Lower 5th Quantile of TN', col = ColPlots, xlim=c(-40,40))
legend('bottomleft', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos)
dev.off()
png('EETN95_sdVsMu_b.png', res = 300, units = 'in', height = 7, width = 7)
plot(x = muEEsTN95_b, y = sdEEsTN95_b, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SAE for Upper 5th Quantile of TN', col = ColPlots, xlim = c(-15,15))
legend('bottomright', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos)
dev.off()
png('EETNMed_sdVsMu_b.png', res = 300, units = 'in', height = 7, width = 7)
plot(x = muEEsTNMed_b, y = sdEEsTNMed_b, pch = 16, xlab = 'Mean of the Elementary Effect', ylab = 'Standard Deviation of the Elementary Effect', main = 'Metric: SAE for Mean of TN', col = ColPlots, xlim = c(-30,30))
legend('bottomleft', legend = c('Hillslope', 'Zone', 'Soil: #9', 'Soil: Comp. #9', 'Soil: #8', 'Soil: Comp. #8', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Veg: Trees', 'Veg: Grass', 'Buildings'), pch = 16, col = colos)
dev.off()

#Make plots of the ranking for mean absolute value - basin----
# Paper figures----
png('Panel_mua_b_Agg_Paper.png', res = 300, units = 'in', height = 8, width = 12)
layout(rbind(c(1,2,3), c(4,5,6)))
#Streamflow 05
barplot(height = EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Lower 5th Percentile of Flow', col = ColPlots_Agg_Paper[order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEs05_b_mua_05[-which(names(EEs05_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs05_b_mua_05[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEs05_b_mua_05[-which(names(EEs05_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1005_Agg])/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1005_Agg])/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)
#legend('topright', legend = c('Hillslope: GW', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Soil: #8', 'Soil: Comp. #8', 'Soil: #9', 'Soil: Comp. #9', 'Veg: Trees', 'Veg: Grass', 'Buildings', 'Zone: Atm.'), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), cex = 1, ncol = 2)
legend('topright', legend = c('Hillslope: GW', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Soil: Riparian', 'Soil: Other', 'Veg: Trees', 'Veg: Grass', 'Buildings', 'Zone: Atm.'), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), cex = 1.1, ncol = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEs05_b_mua_m)[-which(names(EEs05_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMua05_b_Agg$EE05_b[Top1005_Agg]]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEs05_b_mua_95[-which(names(EEs05_b_mua_m) %in% ColsAggregated)][order(names(EEs05_b_mua_m[-which(names(EEs05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}
#Streamflow mid
par(xpd=FALSE)
barplot(height = EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][order(names(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for 5th - 95th Percentile of Flow', col = ColPlots_Agg_Paper[order(names(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEsot_b_mua_05[-which(names(EEsot_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsot_b_mua_05[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][order(names(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsot_b_mua_05[-which(names(EEsot_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][order(names(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10ot_Agg])/max(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10ot_Agg])/max(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEsot_b_mua_m)[-which(names(EEsot_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][order(names(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][order(names(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaot_b_Agg$EEot_b[Top10ot_Agg]]/max(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][order(names(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][order(names(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][order(names(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsot_b_mua_95[-which(names(EEsot_b_mua_m) %in% ColsAggregated)][order(names(EEsot_b_mua_m[-which(names(EEsot_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}

#Streamflow 95
par(xpd=FALSE)
barplot(height = EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Upper 95th Percentile of Flow', col = ColPlots_Agg_Paper[order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEs95_b_mua_05[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs95_b_mua_05[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEs95_b_mua_05[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1095_Agg])/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1095_Agg])/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEs95_b_mua_m)[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMua95_b_Agg$EE95_b[Top1095_Agg]]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}

#TN 05
par(xpd=FALSE)
barplot(height = EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Lower 5th Percentile of TN', col = ColPlots_Agg_Paper[order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEsTN05_b_mua_05[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTN05_b_mua_05[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsTN05_b_mua_05[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TN05_Agg])/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TN05_Agg])/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEsTN05_b_mua_m)[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaTN05_b_Agg$EETN05_b[Top10TN05_Agg]]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}

#TN mid
par(xpd=FALSE)
barplot(height = EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Mean of TN', col = ColPlots_Agg_Paper[order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEsTNMed_b_mua_05[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTNMed_b_mua_05[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsTNMed_b_mua_05[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TNMed_Agg])/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TNMed_Agg])/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEsTNMed_b_mua_m)[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaTNMed_b_Agg$EETNMed_b[Top10TNMed_Agg]]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}

#TN 95
par(xpd=FALSE)
barplot(height = EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Upper 95th Percentile of TN', col = ColPlots_Agg_Paper[order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEsTN95_b_mua_05[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTN95_b_mua_05[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsTN95_b_mua_05[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TN95_Agg])/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TN95_Agg])/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEsTN95_b_mua_m)[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaTN95_b_Agg$EETN95_b[Top10TN95_Agg]]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}
dev.off()

png('Panel_mua_b_Agg_Paper_g.png', res = 300, units = 'in', height = 8, width = 12)
layout(rbind(c(1,2,3), c(4,5,6)))
#Streamflow 05g
barplot(height = EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Lower 5th Percentile of Flow', col = ColPlots_Agg_Paper[order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEs05g_b_mua_05[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs05g_b_mua_05[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEs05g_b_mua_05[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1005g_Agg])/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1005g_Agg])/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)
#legend('topright', legend = c('Hillslope: GW', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Soil: #8', 'Soil: Comp. #8', 'Soil: #9', 'Soil: Comp. #9', 'Veg: Trees', 'Veg: Grass', 'Buildings', 'Zone: Atm.'), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), cex = 1, ncol = 2)
legend('topright', legend = c('Hillslope: GW', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Soil: Riparian', 'Soil: Other', 'Veg: Trees', 'Veg: Grass', 'Buildings', 'Zone: Atm.'), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), cex = 1.1, ncol = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEs05g_b_mua_m)[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMua05g_b_Agg$EE05g_b[Top1005g_Agg]]/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}
#Streamflow mid
par(xpd=FALSE)
barplot(height = EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for 5th - 95th Percentile of Flow', col = ColPlots_Agg_Paper[order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEsotg_b_mua_05[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsotg_b_mua_05[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsotg_b_mua_05[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10otg_Agg])/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10otg_Agg])/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEsotg_b_mua_m)[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaotg_b_Agg$EEotg_b[Top10otg_Agg]]/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}

#Streamflow 95
par(xpd=FALSE)
barplot(height = EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Upper 95th Percentile of Flow', col = ColPlots_Agg_Paper[order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEs95_b_mua_05[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs95_b_mua_05[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEs95_b_mua_05[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1095_Agg])/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1095_Agg])/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEs95_b_mua_m)[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMua95_b_Agg$EE95_b[Top1095_Agg]]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}

#TN 05
par(xpd=FALSE)
barplot(height = EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Lower 5th Percentile of TN', col = ColPlots_Agg_Paper[order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEsTN05_b_mua_05[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTN05_b_mua_05[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsTN05_b_mua_05[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TN05_Agg])/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TN05_Agg])/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEsTN05_b_mua_m)[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaTN05_b_Agg$EETN05_b[Top10TN05_Agg]]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}

#TN mid
par(xpd=FALSE)
barplot(height = EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Mean of TN', col = ColPlots_Agg_Paper[order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEsTNMed_b_mua_05[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTNMed_b_mua_05[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsTNMed_b_mua_05[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TNMed_Agg])/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TNMed_Agg])/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEsTNMed_b_mua_m)[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaTNMed_b_Agg$EETNMed_b[Top10TNMed_Agg]]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}

#TN 95
par(xpd=FALSE)
barplot(height = EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Upper 95th Percentile of TN', col = ColPlots_Agg_Paper[order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEsTN95_b_mua_05[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTN95_b_mua_05[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsTN95_b_mua_05[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TN95_Agg])/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TN95_Agg])/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEsTN95_b_mua_m)[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaTN95_b_Agg$EETN95_b[Top10TN95_Agg]]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}
dev.off()
#   PDF Figure 3----
pdf(file = 'fig3-AllParams.pdf', height = 12, width = 8, title = 'SAfig3')
layout(rbind(c(1,4), c(2,5), c(3,6)))
par(mar=c(1.1, 4.1, 4.1, 2.1))
#Streamflow 05g
barplot(height = EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]), ylab = '', xlab = '', names.arg = NA, main = 'Metric: SAE for Lower 5th Percentile of Flow', col = ColPlots_Agg_Paper[order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEs05g_b_mua_05[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs05g_b_mua_05[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEs05g_b_mua_05[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1005g_Agg])/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1005g_Agg])/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)
#legend('topright', legend = c('Hillslope: GW', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Soil: Riparian', 'Soil: Other', 'Veg: Trees', 'Veg: Grass', 'Buildings', 'Zone: Atm.'), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), cex = 1.1, ncol = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEs05g_b_mua_m)[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.07,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '', cex = c(1,1,1,1,1,rep(1.5,6)))

#Labels for parameters above threshold
tmpnames = names(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMua05g_b_Agg$EE05g_b[Top1005g_Agg]]/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])), cex = 1.5)
}
#Streamflow mid
par(xpd=FALSE)
barplot(height = EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = '', names.arg = NA, main = 'Metric: SAE for 5th - 95th Percentile of Flow', col = ColPlots_Agg_Paper[order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEsotg_b_mua_05[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsotg_b_mua_05[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsotg_b_mua_05[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10otg_Agg])/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10otg_Agg])/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)
legend('topright', legend = c('Hillslope: GW', 'Land: Grass', 'Land: Forest', 'Land: Urban', 
                              'Land: Septic', 'Soil: Riparian', 'Soil: Other', 'Veg: Trees', 
                              'Veg: Grass', 'Buildings', 'Zone: Atm.'), 
       col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), cex = 1.5, ncol = 1)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEsotg_b_mua_m)[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.07,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '', cex = c(1,1,1,1,1,rep(1.5,6)))

#Labels for parameters above threshold
tmpnames = names(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaotg_b_Agg$EEotg_b[Top10otg_Agg]]/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])), cex = 1.5)
}

#Streamflow 95
par(xpd=FALSE)
par(mar=c(4.1, 4.1, 4.1, 2.1))
barplot(height = EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), ylab = '', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Upper 95th Percentile of Flow', col = ColPlots_Agg_Paper[order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEs95_b_mua_05[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs95_b_mua_05[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEs95_b_mua_05[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1095_Agg])/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1095_Agg])/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEs95_b_mua_m)[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.07,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '', cex = c(1,1,1,1,1,rep(1.5,6)))

#Labels for parameters above threshold
tmpnames = names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMua95_b_Agg$EE95_b[Top1095_Agg]]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])), cex = 1.5)
}

#TN 05
par(xpd=FALSE)
par(mar=c(1.1, 4.1, 4.1, 2.1))
barplot(height = EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]), ylab = '', xlab = '', names.arg = NA, main = 'Metric: SAE for Lower 5th Percentile of TN', col = ColPlots_Agg_Paper[order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEsTN05_b_mua_05[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTN05_b_mua_05[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsTN05_b_mua_05[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TN05_Agg])/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TN05_Agg])/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEsTN05_b_mua_m)[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.07,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '', cex = c(1,1,1,1,1,rep(1.5,6)))

#Labels for parameters above threshold
tmpnames = names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaTN05_b_Agg$EETN05_b[Top10TN05_Agg]]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])), cex = 1.5)
}

#TN mid
par(xpd=FALSE)
barplot(height = EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]), ylab = '', xlab = '', names.arg = NA, main = 'Metric: SAE for Mean of TN', col = ColPlots_Agg_Paper[order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEsTNMed_b_mua_05[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTNMed_b_mua_05[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsTNMed_b_mua_05[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TNMed_Agg])/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TNMed_Agg])/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEsTNMed_b_mua_m)[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.07,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '', cex = c(1,1,1,1,1,rep(1.5,6)))

#Labels for parameters above threshold
tmpnames = names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaTNMed_b_Agg$EETNMed_b[Top10TNMed_Agg]]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])), cex = 1.5)
}

#TN 95
par(xpd=FALSE)
par(mar=c(4.1, 4.1, 4.1, 2.1))
barplot(height = EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]), ylab = '', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Upper 95th Percentile of TN', col = ColPlots_Agg_Paper[order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEsTN95_b_mua_05[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTN95_b_mua_05[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsTN95_b_mua_05[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TN95_Agg])/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TN95_Agg])/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEsTN95_b_mua_m)[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.07,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '', cex = c(1,1,1,1,1,rep(1.5,6)))

#Labels for parameters above threshold
tmpnames = names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaTN95_b_Agg$EETN95_b[Top10TN95_Agg]]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])), cex = 1.5)
}
dev.off()


pdf(file = 'fig3-EEs.pdf', height = 12, width = 8, title = 'SAfig3')
layout(rbind(c(1,4), c(2,5), c(3,6)))
par(mar=c(1.1, 4.1, 4.1, 2.1))
#Streamflow 05g
barplot(height = EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]), 
        ylab = '', xlab = '', names.arg = NA, main = 'Metric: SAE for Lower 5th Percentile of Flow', 
        col = ColPlots_Agg_Paper[order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper], 
        border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,40))
arrows(seq(0.5,length(SortRanksMua_hg_Agg_paper)-0.5,1), 
       EEs05g_b_mua_05[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]), 
       seq(0.5,length(SortRanksMua_hg_Agg_paper)-0.5,1), 
       EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]), 
       length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1005g_Agg])/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1005g_Agg])/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(SortRanksMua_hg_Agg_paper)
lines(x = c(0,0), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(11,11), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
plot(x = c(0.5,1.5,3,7.5,16.5,26,31,34.5), y = rep(-0.07,8), col = colos_paper[c(1,3,3,2,2,4,4,5)], pch = c(16,18,15,16,17,16,17,16), 
     xlim = c(0,40), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMua05g_b_Agg$EE05g_b[Top1005g_Agg]]/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = -0.5+which(names(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]) == tmpnames[i]),
       y = offset(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper][which(names(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]) == tmpnames[i])]/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])), 
       cex = 1.5)
}

#Streamflow mid
par(xpd=FALSE)
barplot(height = EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]), 
        ylab = 'Normalized Mean Abs. Val. of EE', xlab = '', names.arg = NA, main = 'Metric: SAE for 5th - 95th Percentile of Flow', 
        col = ColPlots_Agg_Paper[order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper], 
        border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,40))
arrows(seq(0.5,length(SortRanksMua_hg_Agg_paper)-0.5,1), 
       EEsotg_b_mua_05[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]), 
       seq(0.5,length(SortRanksMua_hg_Agg_paper)-0.5,1), 
       EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]), 
       length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10otg_Agg])/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10otg_Agg])/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(SortRanksMua_hg_Agg_paper)
lines(x = c(0,0), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(11,11), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
plot(x = c(0.5,1.5,3,7.5,16.5,26,31,34.5), y = rep(-0.07,8), col = colos_paper[c(1,3,3,2,2,4,4,5)], pch = c(16,18,15,16,17,16,17,16), 
     xlim = c(0,40), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaotg_b_Agg$EEotg_b[Top10otg_Agg]]/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = -0.5+which(names(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]) == tmpnames[i]),
       y = offset(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper][which(names(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]) == tmpnames[i])]/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])), 
       cex = 1.5)
}
legend('topright', legend = c('Hillslope: GW', 'Land: Urban', 
                              'Land: Septic', 'Soil: Riparian', 'Soil: Other', 'Veg: Trees', 
                              'Veg: Grass', 'Zone: Atm.'), 
       col = colos_paper[c(1,3,3,2,2,4,4,5)], pch = c(16,18,15,16,17,16,17,16), cex = 1.5, ncol = 1)

#Streamflow 95
par(xpd=FALSE)
par(mar=c(4.1, 4.1, 4.1, 2.1))
barplot(height = EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), 
        ylab = '', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Upper 95th Percentile of Flow', 
        col = ColPlots_Agg_Paper[order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper], 
        border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,40))
arrows(seq(0.5,length(SortRanksMua_hg_Agg_paper)-0.5,1), 
       EEs95_b_mua_05[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), 
       seq(0.5,length(SortRanksMua_hg_Agg_paper)-0.5,1), 
       EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), 
       length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1095_Agg])/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1095_Agg])/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(SortRanksMua_hg_Agg_paper)
lines(x = c(0,0), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(11,11), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
plot(x = c(0.5,1.5,3,7.5,16.5,26,31,34.5), y = rep(-0.07,8), col = colos_paper[c(1,3,3,2,2,4,4,5)], pch = c(16,18,15,16,17,16,17,16), 
     xlim = c(0,40), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMua95_b_Agg$EE95_b[Top1095_Agg]]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = -0.5+which(names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]) == tmpnames[i]),
       y = offset(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper][which(names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]) == tmpnames[i])]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])), 
       cex = 1.5)
}

#TN 05
par(xpd=FALSE)
par(mar=c(1.1, 4.1, 4.1, 2.1))
barplot(height = EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]), 
        ylab = '', xlab = '', names.arg = NA, main = 'Metric: SAE for Lower 5th Percentile of TN', 
        col = ColPlots_Agg_Paper[order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper], 
        border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,40))
arrows(seq(0.5,length(SortRanksMua_hg_Agg_paper)-0.5,1), 
       EEsTN05_b_mua_05[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]), 
       seq(0.5,length(SortRanksMua_hg_Agg_paper)-0.5,1), 
       EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]), 
       length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TN05_Agg])/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TN05_Agg])/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(SortRanksMua_hg_Agg_paper)
lines(x = c(0,0), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(11,11), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
plot(x = c(0.5,1.5,3,7.5,16.5,26,31,34.5), y = rep(-0.07,8), col = colos_paper[c(1,3,3,2,2,4,4,5)], pch = c(16,18,15,16,17,16,17,16), 
     xlim = c(0,40), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaTN05_b_Agg$EETN05_b[Top10TN05_Agg]]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = -0.5+which(names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]) == tmpnames[i]),
       y = offset(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper][which(names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]) == tmpnames[i])]/max(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsTN05_b_mua_95[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)][order(names(EEsTN05_b_mua_m[-which(names(EEsTN05_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])), 
       cex = 1.5)
}

#TN mid
par(xpd=FALSE)
barplot(height = EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]), 
        ylab = '', xlab = '', names.arg = NA, main = 'Metric: SAE for Mean of TN', 
        col = ColPlots_Agg_Paper[order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper], 
        border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,40))
arrows(seq(0.5,length(SortRanksMua_hg_Agg_paper)-0.5,1), 
       EEsTNMed_b_mua_05[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]), 
       seq(0.5,length(SortRanksMua_hg_Agg_paper)-0.5,1), 
       EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]), 
       length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TNMed_Agg])/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TNMed_Agg])/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(SortRanksMua_hg_Agg_paper)
lines(x = c(0,0), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(11,11), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
plot(x = c(0.5,1.5,3,7.5,16.5,26,31,34.5), y = rep(-0.07,8), col = colos_paper[c(1,3,3,2,2,4,4,5)], pch = c(16,18,15,16,17,16,17,16), 
     xlim = c(0,40), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaTNMed_b_Agg$EETNMed_b[Top10TNMed_Agg]]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = -0.5+which(names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]) == tmpnames[i]),
       y = offset(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper][which(names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]) == tmpnames[i])]/max(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsTNMed_b_mua_95[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)][order(names(EEsTNMed_b_mua_m[-which(names(EEsTNMed_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])), 
       cex = 1.5)
}

#TN 95
par(xpd=FALSE)
par(mar=c(4.1, 4.1, 4.1, 2.1))
barplot(height = EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]), 
        ylab = '', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Upper 95th Percentile of TN', 
        col = ColPlots_Agg_Paper[order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper], 
        border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,40))
arrows(seq(0.5,length(SortRanksMua_hg_Agg_paper)-0.5,1), 
       EEsTN95_b_mua_05[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]), 
       seq(0.5,length(SortRanksMua_hg_Agg_paper)-0.5,1), 
       EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]), 
       length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TN95_Agg])/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10TN95_Agg])/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(SortRanksMua_hg_Agg_paper)
lines(x = c(0,0), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(11,11), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
plot(x = c(0.5,1.5,3,7.5,16.5,26,31,34.5), y = rep(-0.07,8), col = colos_paper[c(1,3,3,2,2,4,4,5)], pch = c(16,18,15,16,17,16,17,16), 
     xlim = c(0,40), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaTN95_b_Agg$EETN95_b[Top10TN95_Agg]]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = -0.5+which(names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]) == tmpnames[i]),
       y = offset(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper][which(names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]) == tmpnames[i])]/max(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsTN95_b_mua_95[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)][order(names(EEsTN95_b_mua_m[-which(names(EEsTN95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])), 
       cex = 1.5)
}
dev.off()



#   S9 For basin median streamflow deviation SAE and SAMD comparison----
pdf(file = 'SAE_SAMD.pdf', height = 12, width = 8, title = 'SAfigS9')
layout(rbind(c(1,4), c(2,5), c(3,6)))
par(mar=c(1.1, 4.1, 4.1, 2.1))
#Streamflow 05g
barplot(height = EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]), ylab = '', xlab = '', names.arg = NA, main = 'Metric: SAE for Lower 5th Percentile of Flow', col = ColPlots_Agg_Paper[order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEs05g_b_mua_05[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs05g_b_mua_05[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEs05g_b_mua_05[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1005g_Agg])/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1005g_Agg])/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)
#legend('topright', legend = c('Hillslope: GW', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Soil: Riparian', 'Soil: Other', 'Veg: Trees', 'Veg: Grass', 'Buildings', 'Zone: Atm.'), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), cex = 1.1, ncol = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEs05g_b_mua_m)[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.07,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '', cex = c(1,1,1,1,1,rep(1.5,6)))

#Labels for parameters above threshold
tmpnames = names(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMua05g_b_Agg$EE05g_b[Top1005g_Agg]]/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEs05g_b_mua_95[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)][order(names(EEs05g_b_mua_m[-which(names(EEs05g_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])), cex = 1.5)
}
#Streamflow mid
par(xpd=FALSE)
barplot(height = EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = '', names.arg = NA, main = 'Metric: SAE for 5th - 95th Percentile of Flow', col = ColPlots_Agg_Paper[order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEsotg_b_mua_05[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsotg_b_mua_05[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsotg_b_mua_05[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10otg_Agg])/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10otg_Agg])/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)
legend('topright', legend = c('Hillslope: GW', 'Land: Grass', 'Land: Forest', 'Land: Urban', 
                              'Land: Septic', 'Soil: Riparian', 'Soil: Other', 'Veg: Trees', 
                              'Veg: Grass', 'Buildings', 'Zone: Atm.'), 
       col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), cex = 1.5, ncol = 1)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEsotg_b_mua_m)[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.07,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '', cex = c(1,1,1,1,1,rep(1.5,6)))

#Labels for parameters above threshold
tmpnames = names(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaotg_b_Agg$EEotg_b[Top10otg_Agg]]/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsotg_b_mua_95[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)][order(names(EEsotg_b_mua_m[-which(names(EEsotg_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])), cex = 1.5)
}

#Streamflow 95
par(xpd=FALSE)
par(mar=c(4.1, 4.1, 4.1, 2.1))
barplot(height = EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), ylab = '', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAE for Upper 95th Percentile of Flow', col = ColPlots_Agg_Paper[order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEs95_b_mua_05[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs95_b_mua_05[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEs95_b_mua_05[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1095_Agg])/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1095_Agg])/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEs95_b_mua_m)[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.07,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '', cex = c(1,1,1,1,1,rep(1.5,6)))

#Labels for parameters above threshold
tmpnames = names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMua95_b_Agg$EE95_b[Top1095_Agg]]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEs95_b_mua_95[-which(names(EEs95_b_mua_m) %in% ColsAggregated)][order(names(EEs95_b_mua_m[-which(names(EEs95_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])), cex = 1.5)
}

#Streamflow 05gm
par(mar=c(1.1, 4.1, 4.1, 2.1))
barplot(height = EEs05gm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)][order(names(EEs05gm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs05gm_b_mua_95[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)]), ylab = '', xlab = '', names.arg = NA, main = 'Metric: SAMD for Lower 5th Percentile of Flow', col = ColPlots_Agg_Paper[order(names(EEs05gm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEs05gm_b_mua_05[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs05gm_b_mua_05[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)][order(names(EEs05gm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs05gm_b_mua_95[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEs05gm_b_mua_05[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs05gm_b_mua_95[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)][order(names(EEs05gm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs05gm_b_mua_95[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEs05gm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1005gm_Agg])/max(EEs05gm_b_mua_95[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEs05gm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1005gm_Agg])/max(EEs05gm_b_mua_95[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)
#legend('topright', legend = c('Hillslope: GW', 'Land: Grass', 'Land: Forest', 'Land: Urban', 'Land: Septic', 'Soil: Riparian', 'Soil: Other', 'Veg: Trees', 'Veg: Grass', 'Buildings', 'Zone: Atm.'), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), cex = 1.1, ncol = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEs05gm_b_mua_m)[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.07,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '', cex = c(1,1,1,1,1,rep(1.5,6)))

#Labels for parameters above threshold
tmpnames = names(EEs05gm_b_mua_95[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)][order(names(EEs05gm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEs05gm_b_mua_95[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)][order(names(EEs05gm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMua05gm_b_Agg$EE05gm_b[Top1005gm_Agg]]/max(EEs05gm_b_mua_95[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEs05gm_b_mua_95[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)][order(names(EEs05gm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEs05gm_b_mua_95[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)][order(names(EEs05gm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEs05gm_b_mua_95[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)][order(names(EEs05gm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEs05gm_b_mua_95[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEs05gm_b_mua_95[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)][order(names(EEs05gm_b_mua_m[-which(names(EEs05gm_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])), cex = 1.5)
}
#Streamflow mid
par(xpd=FALSE)
barplot(height = EEsotgm_b_mua_m[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)][order(names(EEsotgm_b_mua_m[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsotgm_b_mua_95[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = '', names.arg = NA, main = 'Metric: SAMD for 5th - 95th Percentile of Flow', col = ColPlots_Agg_Paper[order(names(EEsotgm_b_mua_m[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEsotgm_b_mua_05[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsotgm_b_mua_05[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)][order(names(EEsotgm_b_mua_m[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsotgm_b_mua_95[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsotgm_b_mua_05[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsotgm_b_mua_95[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)][order(names(EEsotgm_b_mua_m[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsotgm_b_mua_95[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsotgm_b_mua_m[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10otgm_Agg])/max(EEsotgm_b_mua_95[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsotgm_b_mua_m[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10otgm_Agg])/max(EEsotgm_b_mua_95[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)
legend('topright', legend = c('Hillslope: GW', 'Land: Grass', 'Land: Forest', 'Land: Urban', 
                              'Land: Septic', 'Soil: Riparian', 'Soil: Other', 'Veg: Trees', 
                              'Veg: Grass', 'Buildings', 'Zone: Atm.'), 
       col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), cex = 1.5, ncol = 1)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEsotgm_b_mua_m)[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.07,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '', cex = c(1,1,1,1,1,rep(1.5,6)))

#Labels for parameters above threshold
tmpnames = names(EEsotgm_b_mua_95[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)][order(names(EEsotgm_b_mua_m[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsotgm_b_mua_95[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)][order(names(EEsotgm_b_mua_m[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaotgm_b_Agg$EEotgm_b[Top10otgm_Agg]]/max(EEsotgm_b_mua_95[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEsotgm_b_mua_95[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)][order(names(EEsotgm_b_mua_m[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEsotgm_b_mua_95[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)][order(names(EEsotgm_b_mua_m[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEsotgm_b_mua_95[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)][order(names(EEsotgm_b_mua_m[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEsotgm_b_mua_95[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsotgm_b_mua_95[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)][order(names(EEsotgm_b_mua_m[-which(names(EEsotgm_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])), cex = 1.5)
}

#Streamflow 95
par(xpd=FALSE)
par(mar=c(4.1, 4.1, 4.1, 2.1))
barplot(height = EEs95m_b_mua_m[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)][order(names(EEs95m_b_mua_m[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs95m_b_mua_95[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)]), ylab = '', xlab = 'Parameters', names.arg = NA, main = 'Metric: SAMD for Upper 95th Percentile of Flow', col = ColPlots_Agg_Paper[order(names(EEs95m_b_mua_m[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEs95m_b_mua_05[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs95m_b_mua_05[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)][order(names(EEs95m_b_mua_m[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs95m_b_mua_95[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEs95m_b_mua_05[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)])-0.5,1), EEs95m_b_mua_95[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)][order(names(EEs95m_b_mua_m[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEs95m_b_mua_95[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEs95m_b_mua_m[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1095m_Agg])/max(EEs95m_b_mua_95[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEs95m_b_mua_m[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top1095m_Agg])/max(EEs95m_b_mua_95[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEs95m_b_mua_m)[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.07,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '', cex = c(1,1,1,1,1,rep(1.5,6)))

#Labels for parameters above threshold
tmpnames = names(EEs95m_b_mua_95[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)][order(names(EEs95m_b_mua_m[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEs95m_b_mua_95[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)][order(names(EEs95m_b_mua_m[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMua95m_b_Agg$EE95m_b[Top1095m_Agg]]/max(EEs95m_b_mua_95[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEs95m_b_mua_95[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)][order(names(EEs95m_b_mua_m[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEs95m_b_mua_95[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)][order(names(EEs95m_b_mua_m[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEs95m_b_mua_95[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)][order(names(EEs95m_b_mua_m[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEs95m_b_mua_95[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEs95m_b_mua_95[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)][order(names(EEs95m_b_mua_m[-which(names(EEs95m_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])), cex = 1.5)
}
dev.off()


# Plot Median vs. Observed Timeseries----
png('MedObsFlows.png', res=300, units = 'in', width = 4, height = 9)
layout(rbind(1,2,3))
plot(obs05g$Flow, type='l',ylim=c(0,25),xlim=c(0,120), main='Lower 5th Percentile Flows', ylab='Flow (cfs)')
par(new=TRUE)
plot(MedBasins05g, type='l',ylim=c(0,25),xlim=c(0,120), col='red', ylab='')
legend('topright', legend = c('Observed', 'Median Simulated'), col = c('black', 'red'), lty=1)

plot(obsotg$Flow, type='l',ylim=c(0,25), main='5th - 95th Percentile Flows', ylab='Flow (cfs)')
par(new=TRUE)
plot(MedBasinsotg, type='l',ylim=c(0,25), col='red', ylab='')

plot(obs95$Flow, type='l',ylim=c(0,25),xlim=c(0,120), main='Upper 5th Percentile Flows', ylab='Flow (cfs)')
par(new=TRUE)
plot(MedBasins95, type='l',ylim=c(0,25),xlim=c(0,120), col='red', ylab='')
dev.off()

#  Calibration Metrics----
png('Panel_mua_b_Agg_Paper_Cal.png', res = 300, units = 'in', height = 8, width = 8)
layout(rbind(c(1,2), c(3,4)))
#LNSE
barplot(height = EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = 'Parameters', names.arg = NA, main = 'Metric: LNSE', col = ColPlots_Agg_Paper[order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEsLNSE_b_mua_05[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsLNSE_b_mua_05[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsLNSE_b_mua_05[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10LNSE_Agg])/max(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10LNSE_Agg])/max(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEsLNSE_b_mua_m)[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaLNSE_b_Agg$EELNSE_b[Top10LNSE_Agg]]/max(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}
par(xpd=FALSE)

#NSE
barplot(height = EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = 'Parameters', names.arg = NA, main = 'Metric: NSE', col = ColPlots_Agg_Paper[order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEsNSE_b_mua_05[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsNSE_b_mua_05[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsNSE_b_mua_05[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10NSE_Agg])/max(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10NSE_Agg])/max(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEsNSE_b_mua_m)[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaNSE_b_Agg$EENSE_b[Top10NSE_Agg]]/max(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}
par(xpd=FALSE)

#pBias
barplot(height = EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = 'Parameters', names.arg = NA, main = 'Metric: pBias', col = ColPlots_Agg_Paper[order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEspBias_b_mua_05[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)])-0.5,1), EEspBias_b_mua_05[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEspBias_b_mua_05[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)])-0.5,1), EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10pBias_Agg])/max(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10pBias_Agg])/max(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEspBias_b_mua_m)[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuapBias_b_Agg$EEpBias_b[Top10pBias_Agg]]/max(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}
par(xpd=FALSE)

#LogL
barplot(height = EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]), ylab = 'Normalized Mean Abs. Val. of EE', xlab = 'Parameters', names.arg = NA, main = 'Metric: LogL', col = ColPlots_Agg_Paper[order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEsLogL_b_mua_05[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsLogL_b_mua_05[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsLogL_b_mua_05[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10LogL_Agg])/max(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10LogL_Agg])/max(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEsLogL_b_mua_m)[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaLogL_b_Agg$EELogL_b[Top10LogL_Agg]]/max(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}
dev.off()

#   PDF Figure 6 ----
pdf(file = 'fig6-AllParams.pdf', height = 8, width = 8, title = 'SAfig6')
layout(rbind(c(1,2), c(3,4)))
par(mar = c(2.1,4.5,4.1,2.1))
#LNSE
barplot(height = EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]), ylab = '', xlab = '', names.arg = NA, main = 'Metric: LNSE', col = ColPlots_Agg_Paper[order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEsLNSE_b_mua_05[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsLNSE_b_mua_05[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsLNSE_b_mua_05[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10LNSE_Agg])/max(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10LNSE_Agg])/max(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEsLNSE_b_mua_m)[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaLNSE_b_Agg$EELNSE_b[Top10LNSE_Agg]]/max(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}
par(xpd=FALSE)

par(mar = c(2.1,2.1,4.1,2.1))
#NSE
barplot(height = EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]), ylab = '', xlab = '', names.arg = NA, main = 'Metric: NSE', col = ColPlots_Agg_Paper[order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEsNSE_b_mua_05[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsNSE_b_mua_05[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsNSE_b_mua_05[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10NSE_Agg])/max(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10NSE_Agg])/max(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEsNSE_b_mua_m)[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaNSE_b_Agg$EENSE_b[Top10NSE_Agg]]/max(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}
par(xpd=FALSE)

par(mar = c(3.1,4.5,4.1,2.1))
#pBias
barplot(height = EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]), ylab = '', xlab = '', names.arg = NA, main = 'Metric: pBias', col = ColPlots_Agg_Paper[order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEspBias_b_mua_05[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)])-0.5,1), EEspBias_b_mua_05[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEspBias_b_mua_05[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)])-0.5,1), EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10pBias_Agg])/max(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10pBias_Agg])/max(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEspBias_b_mua_m)[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuapBias_b_Agg$EEpBias_b[Top10pBias_Agg]]/max(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}
axis(side = 1, labels = 'Parameters', tick = FALSE, at = 280, line = 1, cex.axis = 1.5)
par(xpd=FALSE)

par(mar = c(3.1,2.1,4.1,2.1))
#LogL
barplot(height = EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]), ylab = '', xlab = '', names.arg = NA, main = 'Metric: LogL', col = ColPlots_Agg_Paper[order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))], border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,250))
arrows(seq(0.5,length(EEsLogL_b_mua_05[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsLogL_b_mua_05[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]), seq(0.5,length(EEsLogL_b_mua_05[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)])-0.5,1), EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]/max(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]), length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10LogL_Agg])/max(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10LogL_Agg])/max(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(names(EEsLogL_b_mua_m)[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)])
SortNames = SortNames[c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]
lines(x = c(-1,-1), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], 0.5+grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))], 0.5+grep(SortNames, pattern = '^l1_')[length(grep(SortNames, pattern = '^l1_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))], 0.5+grep(SortNames, pattern = '^l2_')[length(grep(SortNames, pattern = '^l2_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], 0.5+grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], 0.5+grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))], 0.5+grep(SortNames, pattern = '^s8_')[length(grep(SortNames, pattern = '^s8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))], 0.5+grep(SortNames, pattern = '^s108_')[length(grep(SortNames, pattern = '^s108_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))], 0.5+grep(SortNames, pattern = '^Soil8_')[length(grep(SortNames, pattern = '^Soil8_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))], 0.5+grep(SortNames, pattern = '^s9_')[length(grep(SortNames, pattern = '^s9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
#lines(x = c(0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))], 0.5+grep(SortNames, pattern = '^s109_')[length(grep(SortNames, pattern = '^s109_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], 0.5+grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], 0.5+grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], 0.5+grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))], 0.5+grep(SortNames, pattern = '^v4_')[length(grep(SortNames, pattern = '^v4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], 0.5+grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
#plot(x = c(1,3.5,5.5,8,11.5,24,46,67,89,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,8)), col = colos_paper[c(1,3,3,3,3,2,2,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,18,15,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')
plot(x = c(1,3.5,5.5,8,11.5,35,78,135,193,222,232), y = c(-0.06, -0.08, -0.06, -0.08, -0.06, rep(-0.08,6)), col = colos_paper[c(1,3,3,3,3,2,2,4,4,6,5)], pch = c(16,16,17,18,15,16,17,16,17,16,16), xlim = c(0,250), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaLogL_b_Agg$EELogL_b[Top10LogL_Agg]]/max(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = which(names(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i]),
       y = offset(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][which(names(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])]/max(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])))
}

#axis labels
par(xpd = FALSE)
axis(side = 2, labels = 'Normalized Mean Abs. Val. of EE', tick = FALSE, at = 1.02, line = 24, cex.axis = 1.5)

dev.off()


pdf(file = 'fig6.pdf', height = 8, width = 8, title = 'SAfig6')
layout(rbind(c(1,2), c(3,4)))
par(mar = c(2.1,4.5,4.1,2.1))
#LNSE
barplot(height = EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]), 
        ylab = '', xlab = '', names.arg = NA, main = 'Metric: LNSE', 
        col = ColPlots_Agg_Paper[order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper], 
        border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,40))
arrows(seq(0.5,length(SortRanksMua_hg_Agg_paper)-0.5,1), 
       EEsLNSE_b_mua_05[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]), 
       seq(0.5,length(SortRanksMua_hg_Agg_paper)-0.5,1), 
       EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]), 
       length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10LNSE_Agg])/max(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10LNSE_Agg])/max(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(SortRanksMua_hg_Agg_paper)
lines(x = c(0,0), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(11,11), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
plot(x = c(0.5,1.5,3,7.5,16.5,26,31,34.5), y = rep(-0.07,8), col = colos_paper[c(1,3,3,2,2,4,4,5)], pch = c(16,16,15,16,17,16,17,16), 
     xlim = c(0,40), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaLNSE_b_Agg$EELNSE_b[Top10LNSE_Agg]]/max(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = -0.5+which(names(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]) == tmpnames[i]),
       y = offset(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper][which(names(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]) == tmpnames[i])]/max(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsLNSE_b_mua_m[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])), 
       cex = 1)
}

par(xpd=FALSE)
par(mar = c(2.1,2.1,4.1,2.1))
#NSE
barplot(height = EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]), 
        ylab = '', xlab = '', names.arg = NA, main = 'Metric: NSE', 
        col = ColPlots_Agg_Paper[order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper], 
        border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,40))
arrows(seq(0.5,length(SortRanksMua_hg_Agg_paper)-0.5,1), 
       EEsNSE_b_mua_05[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]), 
       seq(0.5,length(SortRanksMua_hg_Agg_paper)-0.5,1), 
       EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]), 
       length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10NSE_Agg])/max(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10NSE_Agg])/max(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(SortRanksMua_hg_Agg_paper)
lines(x = c(0,0), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(11,11), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
plot(x = c(0.5,1.5,3,7.5,16.5,26,31,34.5), y = rep(-0.07,8), col = colos_paper[c(1,3,3,2,2,4,4,5)], pch = c(16,16,15,16,17,16,17,16), 
     xlim = c(0,40), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaNSE_b_Agg$EENSE_b[Top10NSE_Agg]]/max(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = -0.5+which(names(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]) == tmpnames[i]),
       y = offset(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper][which(names(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]) == tmpnames[i])]/max(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][order(names(EEsNSE_b_mua_m[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])), 
       cex = 1)
}

par(xpd=FALSE)
par(mar = c(3.1,4.5,4.1,2.1))
#pBias
barplot(height = EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]), 
        ylab = '', xlab = '', names.arg = NA, main = 'Metric: pBias', 
        col = ColPlots_Agg_Paper[order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper], 
        border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,40))
arrows(seq(0.5,length(SortRanksMua_hg_Agg_paper)-0.5,1), 
       EEspBias_b_mua_05[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]), 
       seq(0.5,length(SortRanksMua_hg_Agg_paper)-0.5,1), 
       EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]), 
       length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10pBias_Agg])/max(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10pBias_Agg])/max(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(SortRanksMua_hg_Agg_paper)
lines(x = c(0,0), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(11,11), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
plot(x = c(0.5,1.5,3,7.5,16.5,26,31,34.5), y = rep(-0.07,8), col = colos_paper[c(1,3,3,2,2,4,4,5)], pch = c(16,16,15,16,17,16,17,16), 
     xlim = c(0,40), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuapBias_b_Agg$EEpBias_b[Top10pBias_Agg]]/max(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = -0.5+which(names(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]) == tmpnames[i]),
       y = offset(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper][which(names(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]) == tmpnames[i])]/max(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][order(names(EEspBias_b_mua_m[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])), 
       cex = 1)
}
axis(side = 1, labels = 'Parameters', tick = FALSE, at = 45, line = 1, cex.axis = 1.5)

par(xpd=FALSE)
par(mar = c(3.1,2.1,4.1,2.1))
#LogL
barplot(height = EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]), 
        ylab = '', xlab = '', names.arg = NA, main = 'Metric: LogL', 
        col = ColPlots_Agg_Paper[order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper], 
        border = NA, ylim = c(0,1), space = 0, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5, xlim = c(0,40))
arrows(seq(0.5,length(SortRanksMua_hg_Agg_paper)-0.5,1), 
       EEsLogL_b_mua_05[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]), 
       seq(0.5,length(SortRanksMua_hg_Agg_paper)-0.5,1), 
       EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]/max(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]), 
       length=0.05, angle=90, code=3)
lines(x = c(-100,round(nrow(ParamRanges), -2)), y = c(as.numeric(sort(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10LogL_Agg])/max(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]), as.numeric(sort(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)], decreasing = TRUE)[Top10LogL_Agg])/max(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)])), col = 'black', lwd = 2)

#x-axis lines
par(xpd=TRUE)
SortNames = sort(SortRanksMua_hg_Agg_paper)
lines(x = c(0,0), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))], grep(SortNames, pattern = '^h_')[length(grep(SortNames, pattern = '^h_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))], grep(SortNames, pattern = '^l3_')[length(grep(SortNames, pattern = '^l3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))], grep(SortNames, pattern = '^l4_')[length(grep(SortNames, pattern = '^l4_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(11,11), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))], grep(SortNames, pattern = '^Soil9_')[length(grep(SortNames, pattern = '^Soil9_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))], grep(SortNames, pattern = '^v102_')[length(grep(SortNames, pattern = '^v102_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))], grep(SortNames, pattern = '^v3_')[length(grep(SortNames, pattern = '^v3_'))]), y = c(-0.05,0), lty = 1, col = 'black')
lines(x = c(grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))], grep(SortNames, pattern = '^z_')[length(grep(SortNames, pattern = '^z_'))]), y = c(-0.05,0), lty = 1, col = 'black')

#x-axis pch
par(new=TRUE)
plot(x = c(0.5,1.5,3,7.5,16.5,26,31,34.5), y = rep(-0.07,8), col = colos_paper[c(1,3,3,2,2,4,4,5)], pch = c(16,16,15,16,17,16,17,16), 
     xlim = c(0,40), ylim = c(0,1), axes = FALSE, xlab = '', ylab = '')

#Labels for parameters above threshold
tmpnames = names(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)] >= RanksMuaLogL_b_Agg$EELogL_b[Top10LogL_Agg]]/max(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))
for (i in 1:length(tmpnames)){
  text(x = -0.5+which(names(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]) == tmpnames[i]),
       y = offset(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper][which(names(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)][names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) %in% SortRanksMua_hg_Agg_paper]) == tmpnames[i])]/max(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)])),
       labels = as.character(which(names(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][order(names(EEsLogL_b_mua_m[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)]))][c(1:13, 54:73, 14:33, 94:96, 74:93, 34:53, 97:99, 100:237)]) == tmpnames[i])), 
       cex = 1)
}

#axis labels
par(xpd = FALSE)
axis(side = 2, labels = 'Normalized Mean Abs. Val. of EE', tick = FALSE, at = 1.02, line = 24, cex.axis = 1.5)

dev.off()

#Hillslope Plots for mua----
dir.create(dir_HillPanels, showWarnings = FALSE)
setwd(dir_HillPanels)

#Hillslope Plots for mua with all parameters with EEs > 95% of 10% threshold----
SortRanksMua_b_Agg = unique(c(ParamSelect_b_Agg, ParamSelectTN_b_Agg))[c(1,2,16,17,13,14,12,18,4,3,19,20,5,6,7,9,10,11,15,21,8)]
LabelNames = c('H: GW Loss Coef.', 'Z: Wind Speed', 'Z: Avg. Temp. Coef.', 'Z: Atm. Trans. Coef.', 
               'Riparian Soil (S8+S108): m', 'Other Soil (S9+S109): m', 'Other Soil (S9+S109): Ksat', 'Other Soil (S9+S109): Poro.', 'S9: Sat. to GW Coef.', 'S9: Soil Thickness', 
               'S9: Pore Size', 'S9: Air Entry Pres.', 'S109: Soil Thickness', 'S109: Air Entry Pres.', 'S109: Sat. to GW Coef.', 
               'Tree: Max Stomatal Cond.', 'Tree: Stomatal Fraction', 'Tree: Rainwater Capacity', 'Tree: Day Leaf On', 'Tree: Leaf Cuticular Cond.', 'L4: Septic Water Load')
SortRanksMua_b_Agg_paper = rev(SortRanksMua_b_Agg[c(1,21,5:10,12,11,15,13,14,16:20,2:4)])
LabelNames_paper = rev(LabelNames[c(1,21,5:10,12,11,15,13,14,16:20,2:4)])

SortRanksMua_bg_Agg = unique(c(ParamSelect_bg_Agg, ParamSelectTN_b_Agg))[c(1,2,17,18,14,15,13,19,5,3,20,4,6,7,8,10,11,12,16,21,9)]
SortRanksMua_bg_Agg_paper = rev(SortRanksMua_bg_Agg[c(1,21,5:10,12,11,15,13,14,16:20,2:4)])

SortRanksMua_bgm_Agg = unique(c(ParamSelect_bgm_Agg, ParamSelectTN_b_Agg))[c(1,2,17,18,14,15,13,19,5,3,20,4,6,7,8,10,11,12,16,21,9)]
SortRanksMua_bgm_Agg_paper = rev(SortRanksMua_bgm_Agg[c(1,21,5:10,12,11,15,13,14,16:20,2:4)])

SortRanksMua_h_Agg = unique(c(ParamSelect_b_Agg, ParamSelectTN_b_Agg, ParamSelect_h_Agg, ParamSelectTN_h_Agg))[c(1,2,16,17,26,30,31,13,25,22,23,24,36,14,12,18,3,4,19,20,5,6,7,32,9,10,11,15,21,27,28,29,37,34,33,35,8)]
LabelNames_h = c('H: GW Loss Coef.', 'Z: Wind Speed', 'Z: Avg. Temp. Coef.', 'Z: Atm. Trans. Coef. 2', 'Z: Atm. Trans. Coef. 1', 'Z: Clear Sky Trans.', 
               'Riparian Soil (S8+S108): Ksat','Riparian Soil (S8+S108): m', 'S8: Air Entry Pres.', 'S8: Soil Thickness', 'S8: Sat. to GW Coef.', 'S8: Pore Size', 'S108: Sat. to GW Coef.',
               'Other Soil (S9+S109): m', 'Other Soil (S9+S109): Ksat', 'Other Soil (S9+S109): Poro.', 'S9: Soil Thickness', 'S9: Sat. to GW Coef.', 
               'S9: Pore Size', 'S9: Air Entry Pres.', 'S109: Soil Thickness', 'S109: Air Entry Pres.', 'S109: Sat. to GW Coef.', 'S109: Pore Size', 
               'Tree: Max Stomatal Cond.', 'Tree: Stomatal Fraction', 'Tree: Rainwater Capacity', 'Tree: Day Leaf On', 'Tree: Leaf Cuticular Cond.', 'Tree: Day Leaf Off', 'Tree: Days Leaves Expand', 'Tree: Days Leaves Fall', 'Grass: Rainwater Capacity', 'Grass: Max Stomatal Cond.',
               'L3: Percent Impervious', 'L4: Percent Impervious', 'L4: Septic Water Load')
SortRanksMua_h_Agg_paper = rev(SortRanksMua_h_Agg[c(1,35:37,7:13,15,14,16:27,29,28,30:34,2:6)])
LabelNames_h_paper = rev(LabelNames_h[c(1,35:37,7:13,15,14,16:27,29,28,30:34,2:6)])

SortRanksMua_hg_Agg = unique(c(ParamSelect_bg_Agg, ParamSelectTN_b_Agg, ParamSelect_hg_Agg, ParamSelectTN_h_Agg))[c(1,2,17,18,26,30,31,14,25,22,23,24,36,15,13,19,3,5,20,4,6,7,8,32,10,11,12,16,21,27,29,28,37,34,33,35,9)]
SortRanksMua_hg_Agg_paper = rev(SortRanksMua_h_Agg[c(1,35:37,7:13,15,14,16:27,29,28,30:34,2:6)])

# Aggregated Rank order----
Ranks05_Agg = Ranksot_Agg = Ranks95_Agg = RanksTN05_Agg = RanksTNMed_Agg = RanksTN95_Agg = NULL
colPal = colorRampPalette(colors = scico(n = 4, palette = 'nuuk'))
#  In or not in the selected parameters to calibrate aggregated over all 6 metrics----
scaleRange = c(0,1)
scaleBy = 1
Pal = colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1)
png('HillRankTop1295_All_AgghNoTN_paper_g.png', res = 300, height = 7, width = 7, units = 'in')
par(mar = c(2,12.5,2,4.5), mgp = c(1,1,0))
for (h in 0:length(uhills)){
  #Loop over the top 10% ranks
  for (j in 1:length(SortRanksMua_hg_Agg_paper)){
    if (h == 0){
      #Basin
      if (j == 1){
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% unique(c(ParamSelect_bg_Agg, ParamSelectTN_b_Agg)), 1, 0)), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(-0.5,-0.5,14.5,14.5), y = c(0.5,37.5,37.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% unique(c(ParamSelect_bg_Agg, ParamSelectTN_b_Agg)), 1, 0)), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% unique(c(ParamSelect_bg_Agg, ParamSelectTN_b_Agg)), 1, 0)), axes = FALSE)
      }
    }else{
      #Hillslopes
      plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% unique(c(ph10g[[h]])), 1, 0)), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = 0, labels = 'B', pos = 0.5)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_hg_Agg_paper),1), labels = LabelNames_h_paper, las = 1)
legend('right', title = expression(bold('Selected for\nCalibration?')), legend = c('Yes', 'No'), pch = 15, col = colFun(c(1,0)), inset = -0.23, xpd = TRUE, bty = 'n')
lines(c(-0.5,15), c(36.5,36.5), col = 'white')
lines(c(-0.5,15), c(35.5,35.5), col = 'white')
lines(c(-0.5,15), c(33.5,33.5), col = 'white')
lines(c(-0.5,15), c(26.5,26.5), col = 'white')
lines(c(-0.5,15), c(15.5,15.5), col = 'white')
lines(c(-0.5,15), c(7.5,7.5), col = 'white')
lines(c(-0.5,15), c(5.5,5.5), col = 'white')
lines(c(8.5,8.5), c(0,40), col = 'white')
lines(c(0.5,0.5), c(0,40), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(-1,8),y = c(3,6.5,11.5,21,30,34.5,36,37), col = colos_paper[c(5,4,4,2,2,3,3,1)], pch = c(16,17,16,17,16,15,18,16), axes=FALSE, xlab = '', ylab = '', xlim = c(-1,15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)))
text(x = 4.5, y = 38.5, expression(bold('   More\nForested')))
text(x = 11.5, y = 38.5, expression(bold('     More\nImpervious')))
text(x = 0, y = 39, expression(bold('Basin')), srt = 90)
text(x = -12, y = 39, 'D', cex = 1.5)
par(xpd=FALSE)
box(which = 'figure')
dev.off()

png('HillRankTop1295_All_AgghbNoTN_paper_g.png', res = 300, height = 7, width = 7, units = 'in')
par(mar = c(2,12.5,2,4.5), mgp = c(1,1,0))
for (h in 0:length(uhills)){
  #Loop over the top 10% ranks
  for (j in 1:length(SortRanksMua_hg_Agg_paper)){
    if (h == 0){
      #Basin
      if (j == 1){
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% unique(c(ParamSelect_bg_Agg)), 1, 0)), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(-0.5,-0.5,14.5,14.5), y = c(0.5,37.5,37.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% unique(c(ParamSelect_bg_Agg)), 1, 0)), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% unique(c(ParamSelect_bg_Agg)), 1, 0)), axes = FALSE)
      }
    }else{
      #Hillslopes
      plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% unique(c(ph10g[[h]])), 1, 0)), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = 0, labels = 'B', pos = 0.5)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_hg_Agg_paper),1), labels = LabelNames_h_paper, las = 1)
legend('right', title = expression(bold('Selected for\nCalibration?')), legend = c('Yes', 'No'), pch = 15, col = colFun(c(1,0)), inset = -0.23, xpd = TRUE, bty = 'n')
lines(c(-0.5,15), c(36.5,36.5), col = 'white')
lines(c(-0.5,15), c(35.5,35.5), col = 'white')
lines(c(-0.5,15), c(33.5,33.5), col = 'white')
lines(c(-0.5,15), c(26.5,26.5), col = 'white')
lines(c(-0.5,15), c(15.5,15.5), col = 'white')
lines(c(-0.5,15), c(7.5,7.5), col = 'white')
lines(c(-0.5,15), c(5.5,5.5), col = 'white')
lines(c(8.5,8.5), c(0,40), col = 'white')
lines(c(0.5,0.5), c(0,40), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(-1,8),y = c(3,6.5,11.5,21,30,34.5,36,37), col = colos_paper[c(5,4,4,2,2,3,3,1)], pch = c(16,17,16,17,16,15,18,16), axes=FALSE, xlab = '', ylab = '', xlim = c(-1,15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)))
text(x = 4.5, y = 38.5, expression(bold('   More\nForested')))
text(x = 11.5, y = 38.5, expression(bold('     More\nImpervious')))
text(x = 0, y = 39, expression(bold('Basin')), srt = 90)
text(x = -12, y = 39, 'D', cex = 1.5)
par(xpd=FALSE)
box(which = 'figure')
dev.off()

pdf('fig4b-AllParams.pdf', height = 7, width = 7, title = 'SAfig4b')
par(mar = c(2,12.5,2,4.5), mgp = c(1,1,0))
for (h in 0:length(uhills)){
  #Loop over the top 10% ranks
  for (j in 1:length(SortRanksMua_hg_Agg_paper)){
    if (h == 0){
      #Basin
      if (j == 1){
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% unique(c(ParamSelect_bg_Agg, ParamSelectTN_b_Agg)), 1, 0)), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(-0.5,-0.5,14.5,14.5), y = c(0.5,37.5,37.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% unique(c(ParamSelect_bg_Agg, ParamSelectTN_b_Agg)), 1, 0)), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% unique(c(ParamSelect_bg_Agg, ParamSelectTN_b_Agg)), 1, 0)), axes = FALSE)
      }
    }else{
      #Hillslopes
      plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% unique(c(ph10g[[h]])), 1, 0)), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = 0, labels = 'B', pos = 0.5)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_hg_Agg_paper),1), labels = LabelNames_h_paper, las = 1)
legend('right', title = expression(bold('Selected for\nCalibration?')), legend = c('Yes', 'No'), pch = 15, col = colFun(c(1,0)), inset = -0.23, xpd = TRUE, bty = 'n')
lines(c(-0.5,15), c(36.5,36.5), col = 'white')
lines(c(-0.5,15), c(35.5,35.5), col = 'white')
lines(c(-0.5,15), c(33.5,33.5), col = 'white')
lines(c(-0.5,15), c(26.5,26.5), col = 'white')
lines(c(-0.5,15), c(15.5,15.5), col = 'white')
lines(c(-0.5,15), c(7.5,7.5), col = 'white')
lines(c(-0.5,15), c(5.5,5.5), col = 'white')
lines(c(8.5,8.5), c(0,40), col = 'white')
lines(c(0.5,0.5), c(0,40), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(-1,8),y = c(3,6.5,11.5,21,30,34.5,36,37), col = colos_paper[c(5,4,4,2,2,3,3,1)], pch = c(16,17,16,17,16,15,18,16), axes=FALSE, xlab = '', ylab = '', xlim = c(-1,15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)))
text(x = 4.5, y = 38.5, expression(bold('   More\nForested')))
text(x = 11.5, y = 38.5, expression(bold('     More\nImpervious')))
text(x = 0, y = 39, expression(bold('Basin')), srt = 90)
text(x = -12, y = 39, 'B', cex = 1.5)
par(xpd=FALSE)
box(which = 'figure')
dev.off()

pdf('fig4b.pdf', height = 7, width = 7, title = 'SAfig4b')
par(mar = c(2,12.5,2,4.5), mgp = c(1,1,0))
for (h in 0:2){
  #Loop over the top 10% ranks
  for (j in 1:length(SortRanksMua_hg_Agg_paper)){
    if (h == 0){
      #Basin
      if (j == 1){
        plot(x = h-0.4, y = j, xlim = c(-1, 2), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = 'Spatial Area', ylab = '', col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% unique(c(ParamSelect_bg_Agg, ParamSelectTN_b_Agg)), 1, 0)), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(-0.75,-0.75,1.75,1.75), y = c(0.5,37.5,37.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h-0.4, y = j, xlim = c(-1, 2), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% unique(c(ParamSelect_bg_Agg, ParamSelectTN_b_Agg)), 1, 0)), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h-0.4, y = j, xlim = c(-1, 2), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% unique(c(ParamSelect_bg_Agg, ParamSelectTN_b_Agg)), 1, 0)), axes = FALSE)
      }
    }else if (h == 1){
      #Hillslopes 1-8
      plot(x = h-0.5, y = j, xlim = c(-1, 2), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), 
           pch = 15, xlab = '', ylab = '', 
           col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% unique(c(ph10g[[1]], ph10g[[2]], ph10g[[3]], ph10g[[4]], 
                                                                          ph10g[[5]], ph10g[[6]], ph10g[[7]], ph10g[[8]])), 
                               1, 0)), axes = FALSE)
    }else if (h == 2){
      #Hillslopes 9-14
      plot(x = h-0.6, y = j, xlim = c(-1, 2), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), 
           pch = 15, xlab = '', ylab = '', 
           col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% unique(c(ph10g[[9]], ph10g[[10]], ph10g[[11]], ph10g[[12]], 
                                                                          ph10g[[13]], ph10g[[14]])), 
                               1, 0)), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = -0.4, labels = 'Basin', pos = 0.5, tick = 0)
axis(side = 1, at = 0.5, labels = 'Hillslopes\n1-8', pos = 0.5, tick = 0)
axis(side = 1, at = 1.4, labels = 'Hillslopes\n9-14', pos = 0.5, tick = 0)
axis(side = 2, at = seq(1,length(SortRanksMua_hg_Agg_paper),1), labels = LabelNames_h_paper, las = 1)
legend('right', title = expression(bold('Selected for\nCalibration?')), legend = c('Yes', 'No'), pch = 15, col = colFun(c(1,0)), inset = -0.23, xpd = TRUE, bty = 'n')
lines(c(-0.75,15), c(36.5,36.5), col = 'white')
lines(c(-0.75,15), c(35.5,35.5), col = 'white')
lines(c(-0.75,15), c(33.5,33.5), col = 'white')
lines(c(-0.75,15), c(26.5,26.5), col = 'white')
lines(c(-0.75,15), c(15.5,15.5), col = 'white')
lines(c(-0.75,15), c(7.5,7.5), col = 'white')
lines(c(-0.75,15), c(5.5,5.5), col = 'white')
lines(c(0,0), c(0,40), col = 'white')
lines(c(1,1), c(0,40), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(-1,8),y = c(3,6.5,11.5,21,30,34.5,36,37), col = colos_paper[c(5,4,4,2,2,3,3,1)], pch = c(16,17,16,17,16,15,18,16), axes=FALSE, xlab = '', ylab = '', xlim = c(-1,15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)))
text(x = 7, y = 38.5, expression(bold('   More\nForested')))
text(x = 12, y = 38.5, expression(bold('     More\nImpervious')))
text(x = 2, y = 39, expression(bold('Basin')))
text(x = -12, y = 39, 'B', cex = 1.5)
par(xpd=FALSE)
box(which = 'figure')
dev.off()

#   Compare selection with calibration metrics----
#HillRankTop1295_All_AgghCal_paper_g
pdf('fig7.pdf', height = 7, width = 7, title = 'SAfig7')
par(mar = c(4,12.5,2,4.5), mgp = c(1,1,0))
for (h in 0:6){
  #Loop over the top 10% ranks
  for (j in 1:length(SortRanksMua_hg_Agg_paper)){
    if (h == 5){
      #Basin - All Decision Metrics
      plot(x = h, y = j, xlim = c(-1, 7), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% unique(c(ParamSelect_bg_Agg, ParamSelectTN_b_Agg)), 1, 0)), axes = FALSE)
    }else if (h == 6){
      #Hillslopes and Basin - All Decision Metrics
      plot(x = h, y = j, xlim = c(-1, 7), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% unique(c(ParamSelect_bg_Agg, ParamSelect_hg_Agg, ParamSelectTN_b_Agg, ParamSelectTN_h_Agg)), 1, 0)), axes = FALSE)
    }else if (h == 4){
      #Basin - All Calibration Metrics
      plot(x = h, y = j, xlim = c(-1, 7), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% ParamSelect_b_Cal_Agg, 1, 0)), axes = FALSE)
    }else if (h == 1){
      #Basin - LNSE
      plot(x = h, y = j, xlim = c(-1, 7), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% names(EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)][EEsLNSE_b_mua_95[-which(names(EEsLNSE_b_mua_m) %in% ColsAggregated)] >= RanksMuaLNSE_b_Agg$EELNSE_b[Top10LNSE_Agg]]), 1, 0)), axes = FALSE)
    }else if (h == 0){
      #Basin - NSE
      if (j == 1){
        plot(x = h, y = j, xlim = c(-1, 7), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% names(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)] >= RanksMuaNSE_b_Agg$EENSE_b[Top10NSE_Agg]]), 1, 0)), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(-0.5,-0.5,6.5,6.5), y = c(0.5,37.5,37.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(-1, 7), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% names(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)] >= RanksMuaNSE_b_Agg$EENSE_b[Top10NSE_Agg]]), 1, 0)), axes = FALSE)
        axis(side = 1, at = 3, line = 2, labels = 'Sensitivity Metric', tick = 0, cex.axis = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(-1, 7), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% names(EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)][EEsNSE_b_mua_95[-which(names(EEsNSE_b_mua_m) %in% ColsAggregated)] >= RanksMuaNSE_b_Agg$EENSE_b[Top10NSE_Agg]]), 1, 0)), axes = FALSE)
      }
    }else if (h == 2){
      #Basin - pBias
      plot(x = h, y = j, xlim = c(-1, 7), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% names(EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)][EEspBias_b_mua_95[-which(names(EEspBias_b_mua_m) %in% ColsAggregated)] >= RanksMuapBias_b_Agg$EEpBias_b[Top10pBias_Agg]]), 1, 0)), axes = FALSE)
    }else if (h == 3){
      #Basin - LogL
      plot(x = h, y = j, xlim = c(-1, 7), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(ifelse(SortRanksMua_hg_Agg_paper[j] %in% names(EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)][EEsLogL_b_mua_95[-which(names(EEsLogL_b_mua_m) %in% ColsAggregated)] >= RanksMuaLogL_b_Agg$EELogL_b[Top10LogL_Agg]]), 1, 0)), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = seq(0,6,1), labels = c('NSE, B', 'LNSE, B', 'pBias, B', 'LogL, B', 'All, B', 'All, B', 'All, H'), pos = 0.5, las=2)
axis(side = 2, at = seq(1,length(SortRanksMua_hg_Agg_paper),1), labels = LabelNames_h_paper, las = 1)
legend('right', title = expression(bold('Selected for\nCalibration?')), legend = c('Yes', 'No'), pch = 15, col = colFun(c(1,0)), inset = -0.23, xpd = TRUE, bty = 'n')
lines(c(-0.5,15), c(36.5,36.5), col = 'white')
lines(c(-0.5,15), c(35.5,35.5), col = 'white')
lines(c(-0.5,15), c(33.5,33.5), col = 'white')
lines(c(-0.5,15), c(26.5,26.5), col = 'white')
lines(c(-0.5,15), c(15.5,15.5), col = 'white')
lines(c(-0.5,15), c(7.5,7.5), col = 'white')
lines(c(-0.5,15), c(5.5,5.5), col = 'white')
lines(c(4.5,4.5), c(0,40), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(-1,8),y = c(3,6.5,11.5,21,30,34.5,36,37), col = colos_paper[c(5,4,4,2,2,3,3,1)], pch = c(16,17,16,17,16,15,18,16), axes=FALSE, xlab = '', ylab = '', xlim = c(-1,15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)))
text(x = 11.5, y = 38.5, expression(bold('  Decision\n    Metrics')))
text(x = 4.5, y = 38.5, expression(bold('Calibration\n    Metrics')))
par(xpd=FALSE)
box(which = 'figure')
dev.off()

#  Streamflow 5th %-ile----
colPal = colorRampPalette(colors = scico(n = 4, palette = 'nuuk'))
scaleRange = c(1, 34)
scaleBy = 11
Pal = rev(colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1))
png('HillRankTop1295_05s_Aggh_paper.png', res = 300, height = 7, width = 7, units = 'in')
par(mar = c(2,12.5,2,4.5), mgp = c(1,1,0))
for (h in 0:length(uhills)){
  #Loop over the top 10% ranks
  for (j in 1:length(SortRanksMua_h_Agg_paper)){
    if (h == 0){
      #Basin
      if (j == 1){
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMua05_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(-0.5,-0.5,14.5,14.5), y = c(0.5,37.5,37.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua05_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua05_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE)
      }
    }else{
      #Hillslopes
      plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua05_h_Agg[[h]]$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = 0, labels = 'B', pos = 0.5)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_h_Agg_paper),1), labels = LabelNames_h_paper, las = 1)
legend('right', title = expression(bold('Rank')), legend = c('1 - 11', '12 - 22', '23 - 33', '>33'), pch = 15, col = colFun(seq(1,34,11)), inset = -0.2, xpd = TRUE)
lines(c(-0.5,15), c(36.5,36.5), col = 'white')
lines(c(-0.5,15), c(35.5,35.5), col = 'white')
lines(c(-0.5,15), c(33.5,33.5), col = 'white')
lines(c(-0.5,15), c(26.5,26.5), col = 'white')
lines(c(-0.5,15), c(15.5,15.5), col = 'white')
lines(c(-0.5,15), c(7.5,7.5), col = 'white')
lines(c(-0.5,15), c(5.5,5.5), col = 'white')
lines(c(8.5,8.5), c(0,40), col = 'white')
lines(c(0.5,0.5), c(0,40), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(-1,8),y = c(3,6.5,11.5,21,30,34.5,36,37), col = colos_paper[c(5,4,4,2,2,3,3,1)], pch = c(16,17,16,17,16,15,18,16), axes=FALSE, xlab = '', ylab = '', xlim = c(-1,15), ylim = c(0,length(SortRanksMua_h_Agg_paper)))
text(x = 4.5, y = 38.5, expression(bold('   More\nForested')))
text(x = 11.5, y = 38.5, expression(bold('     More\nImpervious')))
text(x = 0, y = 39, expression(bold('Basin')), srt = 90)
text(x = 17, y = 23, expression(bold('      Metric:\n  Streamflow\n   Lower 5th\n   Percentile')))
par(xpd=FALSE)
box(which = 'figure')
dev.off()

png('HillRankTop1295_05s_Aggh_paper_g.png', res = 300, height = 7, width = 7, units = 'in')
par(mar = c(2,12.5,2,4.5), mgp = c(1,1,0))
for (h in 0:length(uhills)){
  #Loop over the top 10% ranks
  for (j in 1:length(SortRanksMua_hg_Agg_paper)){
    if (h == 0){
      #Basin
      if (j == 1){
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMua05g_b_Agg$Param == SortRanksMua_hg_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(-0.5,-0.5,14.5,14.5), y = c(0.5,37.5,37.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua05g_b_Agg$Param == SortRanksMua_hg_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua05g_b_Agg$Param == SortRanksMua_hg_Agg_paper[j])), axes = FALSE)
      }
    }else{
      #Hillslopes
      plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua05g_h_Agg[[h]]$Param == SortRanksMua_hg_Agg_paper[j])), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = 0, labels = 'B', pos = 0.5)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_hg_Agg_paper),1), labels = LabelNames_h_paper, las = 1)
legend('right', title = expression(bold('Rank')), legend = c('1 - 11', '12 - 22', '23 - 33', '>33'), pch = 15, col = colFun(seq(1,34,11)), inset = -0.2, xpd = TRUE)
lines(c(-0.5,15), c(36.5,36.5), col = 'white')
lines(c(-0.5,15), c(35.5,35.5), col = 'white')
lines(c(-0.5,15), c(33.5,33.5), col = 'white')
lines(c(-0.5,15), c(26.5,26.5), col = 'white')
lines(c(-0.5,15), c(15.5,15.5), col = 'white')
lines(c(-0.5,15), c(7.5,7.5), col = 'white')
lines(c(-0.5,15), c(5.5,5.5), col = 'white')
lines(c(8.5,8.5), c(0,40), col = 'white')
lines(c(0.5,0.5), c(0,40), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(-1,8),y = c(3,6.5,11.5,21,30,34.5,36,37), col = colos_paper[c(5,4,4,2,2,3,3,1)], pch = c(16,17,16,17,16,15,18,16), axes=FALSE, xlab = '', ylab = '', xlim = c(-1,15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)))
text(x = 4.5, y = 38.5, expression(bold('   More\nForested')))
text(x = 11.5, y = 38.5, expression(bold('     More\nImpervious')))
text(x = 0, y = 39, expression(bold('Basin')), srt = 90)
text(x = 17, y = 23, expression(bold('      Metric:\n  Streamflow\n   Lower 5th\n   Percentile')))
par(xpd=FALSE)
box(which = 'figure')
dev.off()

#  Streamflow 5th-95th %-ile----
scaleRange = c(1, 34)
scaleBy = 11
Pal = rev(colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1))
png('HillRankTop1295_ots_Aggh_paper.png', res = 300, height = 7, width = 7, units = 'in')
par(mar = c(2,12.5,2,4.5), mgp = c(1,1,0))
for (h in 0:length(uhills)){
  #Loop over the top 10% ranks
  for (j in 1:length(SortRanksMua_h_Agg_paper)){
    if (h == 0){
      #Basin
      if (j == 1){
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMuaot_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(-0.5,-0.5,14.5,14.5), y = c(0.5,37.5,37.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaot_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaot_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE)
      }
    }else{
      #Hillslopes
      plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaot_h_Agg[[h]]$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = 0, labels = 'B', pos = 0.5)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_h_Agg_paper),1), labels = LabelNames_h_paper, las = 1)
legend('right', title = expression(bold('Rank')), legend = c('1 - 11', '12 - 22', '23 - 33', '>33'), pch = 15, col = colFun(seq(1,34,11)), inset = -0.2, xpd = TRUE)
lines(c(-0.5,15), c(36.5,36.5), col = 'white')
lines(c(-0.5,15), c(35.5,35.5), col = 'white')
lines(c(-0.5,15), c(33.5,33.5), col = 'white')
lines(c(-0.5,15), c(26.5,26.5), col = 'white')
lines(c(-0.5,15), c(15.5,15.5), col = 'white')
lines(c(-0.5,15), c(7.5,7.5), col = 'white')
lines(c(-0.5,15), c(5.5,5.5), col = 'white')
lines(c(8.5,8.5), c(0,40), col = 'white')
lines(c(0.5,0.5), c(0,40), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(-1,8),y = c(3,6.5,11.5,21,30,34.5,36,37), col = colos_paper[c(5,4,4,2,2,3,3,1)], pch = c(16,17,16,17,16,15,18,16), axes=FALSE, xlab = '', ylab = '', xlim = c(-1,15), ylim = c(0,length(SortRanksMua_h_Agg_paper)))
text(x = 4.5, y = 38.5, expression(bold('   More\nForested')))
text(x = 11.5, y = 38.5, expression(bold('     More\nImpervious')))
text(x = 0, y = 39, expression(bold('Basin')), srt = 90)
text(x = 17, y = 23, expression(bold('    Metric:\nStreamflow\n   5th-95th\n Percentile')))
par(xpd=FALSE)
box(which = 'figure')
dev.off()

png('HillRankTop1295_ots_Aggh_paper_g.png', res = 300, height = 7, width = 7, units = 'in')
par(mar = c(2,12.5,2,4.5), mgp = c(1,1,0))
for (h in 0:length(uhills)){
  #Loop over the top 10% ranks
  for (j in 1:length(SortRanksMua_hg_Agg_paper)){
    if (h == 0){
      #Basin
      if (j == 1){
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMuaotg_b_Agg$Param == SortRanksMua_hg_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(-0.5,-0.5,14.5,14.5), y = c(0.5,37.5,37.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaotg_b_Agg$Param == SortRanksMua_hg_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaotg_b_Agg$Param == SortRanksMua_hg_Agg_paper[j])), axes = FALSE)
      }
    }else{
      #Hillslopes
      plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMuaotg_h_Agg[[h]]$Param == SortRanksMua_hg_Agg_paper[j])), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = 0, labels = 'B', pos = 0.5)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_hg_Agg_paper),1), labels = LabelNames_h_paper, las = 1)
legend('right', title = expression(bold('Rank')), legend = c('1 - 11', '12 - 22', '23 - 33', '>33'), pch = 15, col = colFun(seq(1,34,11)), inset = -0.2, xpd = TRUE)
lines(c(-0.5,15), c(36.5,36.5), col = 'white')
lines(c(-0.5,15), c(35.5,35.5), col = 'white')
lines(c(-0.5,15), c(33.5,33.5), col = 'white')
lines(c(-0.5,15), c(26.5,26.5), col = 'white')
lines(c(-0.5,15), c(15.5,15.5), col = 'white')
lines(c(-0.5,15), c(7.5,7.5), col = 'white')
lines(c(-0.5,15), c(5.5,5.5), col = 'white')
lines(c(8.5,8.5), c(0,40), col = 'white')
lines(c(0.5,0.5), c(0,40), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(-1,8),y = c(3,6.5,11.5,21,30,34.5,36,37), col = colos_paper[c(5,4,4,2,2,3,3,1)], pch = c(16,17,16,17,16,15,18,16), axes=FALSE, xlab = '', ylab = '', xlim = c(-1,15), ylim = c(0,length(SortRanksMua_hg_Agg_paper)))
text(x = 4.5, y = 38.5, expression(bold('   More\nForested')))
text(x = 11.5, y = 38.5, expression(bold('     More\nImpervious')))
text(x = 0, y = 39, expression(bold('Basin')), srt = 90)
text(x = 17, y = 23, expression(bold('    Metric:\nStreamflow\n   5th-95th\n Percentile')))
par(xpd=FALSE)
box(which = 'figure')
dev.off()

#  Streamflow 95th %-ile----
scaleRange = c(1, 34)
scaleBy = 11
Pal = rev(colPal((scaleRange[2] - scaleRange[1])/scaleBy + 1))
png('HillRankTop1295_95s_Aggh_paper.png', res = 300, height = 7, width = 7, units = 'in')
par(mar = c(2,12.5,2,4.5), mgp = c(1,1,0))
for (h in 0:length(uhills)){
  #Loop over the top 10% ranks
  for (j in 1:length(SortRanksMua_h_Agg_paper)){
    if (h == 0){
      #Basin
      if (j == 1){
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMua95_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(-0.5,-0.5,14.5,14.5), y = c(0.5,37.5,37.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua95_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua95_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE)
      }
    }else{
      #Hillslopes
      plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua95_h_Agg[[h]]$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = 0, labels = 'B', pos = 0.5)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_h_Agg_paper),1), labels = LabelNames_h_paper, las = 1)
legend('right', title = expression(bold('Rank')), legend = c('1 - 11', '12 - 22', '23 - 33', '>33'), pch = 15, col = colFun(seq(1,34,11)), inset = -0.2, xpd = TRUE)
lines(c(-0.5,15), c(36.5,36.5), col = 'white')
lines(c(-0.5,15), c(35.5,35.5), col = 'white')
lines(c(-0.5,15), c(33.5,33.5), col = 'white')
lines(c(-0.5,15), c(26.5,26.5), col = 'white')
lines(c(-0.5,15), c(15.5,15.5), col = 'white')
lines(c(-0.5,15), c(7.5,7.5), col = 'white')
lines(c(-0.5,15), c(5.5,5.5), col = 'white')
lines(c(8.5,8.5), c(0,40), col = 'white')
lines(c(0.5,0.5), c(0,40), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(-1,8),y = c(3,6.5,11.5,21,30,34.5,36,37), col = colos_paper[c(5,4,4,2,2,3,3,1)], pch = c(16,17,16,17,16,15,18,16), axes=FALSE, xlab = '', ylab = '', xlim = c(-1,15), ylim = c(0,length(SortRanksMua_h_Agg_paper)))
text(x = 4.5, y = 38.5, expression(bold('   More\nForested')))
text(x = 11.5, y = 38.5, expression(bold('     More\nImpervious')))
text(x = 0, y = 39, expression(bold('Basin')), srt = 90)
text(x = 17, y = 23, expression(bold('      Metric:\n  Streamflow\n   Upper 5th\n   Percentile')))
par(xpd=FALSE)
box(which = 'figure')
dev.off()

pdf('fig4a.pdf', height = 7, width = 7, title = 'SAfig4a')
par(mar = c(2,12.5,2,4.5), mgp = c(1,1,0))
for (h in 0:length(uhills)){
  #Loop over the top 10% ranks
  for (j in 1:length(SortRanksMua_h_Agg_paper)){
    if (h == 0){
      #Basin
      if (j == 1){
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = 'Hillslope ID', ylab = '', col = colFun(which(RanksMua95_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(-0.5,-0.5,14.5,14.5), y = c(0.5,37.5,37.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua95_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua95_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE)
      }
    }else{
      #Hillslopes
      plot(x = h, y = j, xlim = c(-1, 15), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua95_h_Agg[[h]]$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = 0, labels = 'B', pos = 0.5)
axis(side = 1, at = seq(1,14,1), labels = TRUE, pos = 0.5)
axis(side = 2, at = seq(1,length(SortRanksMua_h_Agg_paper),1), labels = LabelNames_h_paper, las = 1)
legend('right', title = expression(bold('Rank')), legend = c('1 - 11', '12 - 22', '23 - 33', '>33'), pch = 15, col = colFun(seq(1,34,11)), inset = -0.2, xpd = TRUE)
lines(c(-0.5,15), c(36.5,36.5), col = 'white')
lines(c(-0.5,15), c(35.5,35.5), col = 'white')
lines(c(-0.5,15), c(33.5,33.5), col = 'white')
lines(c(-0.5,15), c(26.5,26.5), col = 'white')
lines(c(-0.5,15), c(15.5,15.5), col = 'white')
lines(c(-0.5,15), c(7.5,7.5), col = 'white')
lines(c(-0.5,15), c(5.5,5.5), col = 'white')
lines(c(8.5,8.5), c(0,40), col = 'white')
lines(c(0.5,0.5), c(0,40), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(-1,8),y = c(3,6.5,11.5,21,30,34.5,36,37), col = colos_paper[c(5,4,4,2,2,3,3,1)], pch = c(16,17,16,17,16,15,18,16), axes=FALSE, xlab = '', ylab = '', xlim = c(-1,15), ylim = c(0,length(SortRanksMua_h_Agg_paper)))
text(x = 4.5, y = 38.5, expression(bold('   More\nForested')))
text(x = 11.5, y = 38.5, expression(bold('     More\nImpervious')))
text(x = 0, y = 39, expression(bold('Basin')), srt = 90)
text(x = 17, y = 23, expression(bold('      Metric:\n  Streamflow\n   Upper 5th\n   Percentile')))
text(x = -12, y = 39, 'A', cex = 1.5)
par(xpd=FALSE)
box(which = 'figure')
dev.off()

pdf('fig4a-simple.pdf', height = 7, width = 7, title = 'SAfig4a')
par(mar = c(2,12.5,2,4.5), mgp = c(1,1,0))
for (h in 0:2){
  #Loop over the top 10% ranks
  for (j in 1:length(SortRanksMua_h_Agg_paper)){
    if (h == 0){
      #Basin
      if (j == 1){
        plot(x = h-0.4, y = j, xlim = c(-1, 2), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = 'Spatial Area', ylab = '', col = colFun(which(RanksMua95_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
        polygon(x = c(-0.75,-0.75,1.75,1.75), y = c(0.5,37.5,37.5,0.5), col='black')
        par(new=TRUE)
        plot(x = h-0.4, y = j, xlim = c(-1, 2), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua95_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE, cex.lab = 1.5)
      }else{
        plot(x = h-0.4, y = j, xlim = c(-1, 2), ylim = c(0,length(SortRanksMua_h_Agg_paper)), pch = 15, xlab = '', ylab = '', col = colFun(which(RanksMua95_b_Agg$Param == SortRanksMua_h_Agg_paper[j])), axes = FALSE)
      }
    }else if (h == 1){
      #Hillslopes 1-8
      med_h = median(c(which(RanksMua95_h_Agg[[1]]$Param == SortRanksMua_h_Agg_paper[j]),
                       which(RanksMua95_h_Agg[[2]]$Param == SortRanksMua_h_Agg_paper[j]),
                       which(RanksMua95_h_Agg[[3]]$Param == SortRanksMua_h_Agg_paper[j]),
                       which(RanksMua95_h_Agg[[4]]$Param == SortRanksMua_h_Agg_paper[j]),
                       which(RanksMua95_h_Agg[[5]]$Param == SortRanksMua_h_Agg_paper[j]),
                       which(RanksMua95_h_Agg[[6]]$Param == SortRanksMua_h_Agg_paper[j]),
                       which(RanksMua95_h_Agg[[7]]$Param == SortRanksMua_h_Agg_paper[j]),
                       which(RanksMua95_h_Agg[[8]]$Param == SortRanksMua_h_Agg_paper[j])))
      plot(x = h-0.5, y = j, xlim = c(-1, 2), ylim = c(0,length(SortRanksMua_h_Agg_paper)), 
           pch = 15, xlab = '', ylab = '', col = colFun(med_h), 
           axes = FALSE)
    }else if (h == 2){
      #Hillslopes 9-14
      med_h = median(c(which(RanksMua95_h_Agg[[9]]$Param == SortRanksMua_h_Agg_paper[j]),
                       which(RanksMua95_h_Agg[[10]]$Param == SortRanksMua_h_Agg_paper[j]),
                       which(RanksMua95_h_Agg[[11]]$Param == SortRanksMua_h_Agg_paper[j]),
                       which(RanksMua95_h_Agg[[12]]$Param == SortRanksMua_h_Agg_paper[j]),
                       which(RanksMua95_h_Agg[[13]]$Param == SortRanksMua_h_Agg_paper[j]),
                       which(RanksMua95_h_Agg[[14]]$Param == SortRanksMua_h_Agg_paper[j])))
      plot(x = h-0.6, y = j, xlim = c(-1, 2), ylim = c(0,length(SortRanksMua_h_Agg_paper)), 
           pch = 15, xlab = '', ylab = '', col = colFun(med_h), 
           axes = FALSE)
    }
    par(new=TRUE)
  }
}
rm(h,j)
par(new=FALSE, mgp = c(3,1,0))
axis(side = 1, at = -0.4, labels = 'Basin', pos = 0.5, tick = 0)
axis(side = 1, at = 0.5, labels = 'Hillslopes\n1-8', pos = 0.5, tick = 0)
axis(side = 1, at = 1.4, labels = 'Hillslopes\n9-14', pos = 0.5, tick = 0)
axis(side = 2, at = seq(1,length(SortRanksMua_hg_Agg_paper),1), labels = LabelNames_h_paper, las = 1)
legend('right', title = expression(bold('Rank')), legend = c('1 - 11', '12 - 22', '23 - 33', '>33'), pch = 15, col = colFun(seq(1,34,11)), inset = -0.2, xpd = TRUE)
lines(c(-0.75,15), c(36.5,36.5), col = 'white')
lines(c(-0.75,15), c(35.5,35.5), col = 'white')
lines(c(-0.75,15), c(33.5,33.5), col = 'white')
lines(c(-0.75,15), c(26.5,26.5), col = 'white')
lines(c(-0.75,15), c(15.5,15.5), col = 'white')
lines(c(-0.75,15), c(7.5,7.5), col = 'white')
lines(c(-0.75,15), c(5.5,5.5), col = 'white')
lines(c(0,0), c(0,40), col = 'white')
lines(c(1,1), c(0,40), col = 'white')
#Category symbol from EE plot
par(new=TRUE, xpd = TRUE)
plot(x = rep(-1,8),y = c(3,6.5,11.5,21,30,34.5,36,37), col = colos_paper[c(5,4,4,2,2,3,3,1)], pch = c(16,17,16,17,16,15,18,16), axes=FALSE, xlab = '', ylab = '', xlim = c(-1,15), ylim = c(0,length(SortRanksMua_h_Agg_paper)))
text(x = 7, y = 38.5, expression(bold('   More\nForested')))
text(x = 12, y = 38.5, expression(bold('     More\nImpervious')))
text(x = 2, y = 39, expression(bold('Basin')))
text(x = 16.5, y = 23, expression(bold('      Metric:\n  Streamflow\n   Upper 5th\n   Percentile')))
text(x = -12, y = 39, 'A', cex = 1.5)
par(xpd=FALSE)
box(which = 'figure')
dev.off()

#Compare the parameters affected by multipliers after aggregating constrained parameters to see if they are different in sensitivity----
dir.create(dir_Mult, showWarnings = FALSE)
setwd(dir_Mult)
# Streamflow 5%----
#  panel----
png('BasinAllMultEEs_05s.png', res = 300, width = 9, height = 9, units = 'in')
opar=par
layout(rbind(c(1,2,3), c(4,5,6), c(7,8,9)))
barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs05_b_mua_m), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs05_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108', 'S8 and S108', 'S9 and S109'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'm: Ksat Decay with Sat. Def.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,5.5,1), 
       EEs05_b_mua_05[c(grep(names(EEs05_b_mua_05), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs05_b_mua_05), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs05_b_mua_95), 
       seq(0.5,5.5,1), 
       EEs05_b_mua_95[c(grep(names(EEs05_b_mua_95), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs05_b_mua_95), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs05_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Sat. to GW Coef.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs05_b_mua_05[c(grep(names(EEs05_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs05_b_mua_95[c(grep(names(EEs05_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
       length=0.05, angle=90, code=3)

par(mar=c(6,4,3,0.5))
barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
        names.arg = c('S9 Ksat', 'S9 vKsat', 'S109 Ksat', 'S109 vKsat', 'S8 Ksat', 'S8 vKsat', 'S108 Ksat', 'S108 vKsat', 'S8 and S108', 'S9 and S109'), 
        ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Hydraulic Conductivity', xlab = '', las = 2, cex.main=1.5)
axis(side = 1, at = 5, labels = 'Soils', line = 4, cex.axis = 1.2, tick = FALSE)
arrows(seq(0.5,9.5,1), 
       EEs05_b_mua_05[grep(names(EEs05_b_mua_05), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95), 
       seq(0.5,9.5,1), 
       EEs05_b_mua_95[grep(names(EEs05_b_mua_95), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95), 
       length=0.05, angle=90, code=3)

par(opar)
barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Air Entry Pressure', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs05_b_mua_05[c(grep(names(EEs05_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs05_b_mua_95[c(grep(names(EEs05_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Pore Size', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs05_b_mua_05[c(grep(names(EEs05_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs05_b_mua_95[c(grep(names(EEs05_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = 'active', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Soil Thickness', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs05_b_mua_05[grep(names(EEs05_b_mua_05), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs05_b_mua_95[grep(names(EEs05_b_mua_95), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.001), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Snow Melt Temp. Coef.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs05_b_mua_05[grep(names(EEs05_b_mua_05), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs05_b_mua_95[grep(names(EEs05_b_mua_95), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.001), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Max Snow Energy Deficit', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs05_b_mua_05[grep(names(EEs05_b_mua_05), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs05_b_mua_95[grep(names(EEs05_b_mua_95), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs05_b_mua_m[c(grep(names(EEs05_b_mua_m), pattern = 'sla', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_b_mua_95), 
        names.arg = c('Tree', 'Grass'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Specific Leaf Area', xlab = 'Vegetation', cex.main=1.5)
arrows(seq(0.5,1.5,1), 
       EEs05_b_mua_05[grep(names(EEs05_b_mua_05), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95), 
       seq(0.5,1.5,1), 
       EEs05_b_mua_95[grep(names(EEs05_b_mua_95), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_b_mua_95), 
       length=0.05, angle=90, code=3)
dev.off()

png('BasinAllMultEEs_05s_g.png', res = 300, width = 9, height = 9, units = 'in')
opar=par
layout(rbind(c(1,2,3), c(4,5,6), c(7,8,9)))
barplot(height = EEs05g_b_mua_m[c(grep(names(EEs05g_b_mua_m), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs05g_b_mua_m), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs05g_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108', 'S8 and S108', 'S9 and S109'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'm: Ksat Decay with Sat. Def.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,5.5,1), 
       EEs05g_b_mua_05[c(grep(names(EEs05g_b_mua_05), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs05g_b_mua_05), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs05g_b_mua_95), 
       seq(0.5,5.5,1), 
       EEs05g_b_mua_95[c(grep(names(EEs05g_b_mua_95), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs05g_b_mua_95), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs05g_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs05g_b_mua_m[c(grep(names(EEs05g_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Sat. to GW Coef.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs05g_b_mua_05[c(grep(names(EEs05g_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs05g_b_mua_95[c(grep(names(EEs05g_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_b_mua_95), 
       length=0.05, angle=90, code=3)

par(mar=c(6,4,3,0.5))
barplot(height = EEs05g_b_mua_m[c(grep(names(EEs05g_b_mua_m), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_b_mua_95), 
        names.arg = c('S9 Ksat', 'S9 vKsat', 'S109 Ksat', 'S109 vKsat', 'S8 Ksat', 'S8 vKsat', 'S108 Ksat', 'S108 vKsat', 'S8 and S108', 'S9 and S109'), 
        ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Hydraulic Conductivity', xlab = '', las = 2, cex.main=1.5)
axis(side = 1, at = 5, labels = 'Soils', line = 4, cex.axis = 1.2, tick = FALSE)
arrows(seq(0.5,9.5,1), 
       EEs05g_b_mua_05[grep(names(EEs05g_b_mua_05), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEs05g_b_mua_95), 
       seq(0.5,9.5,1), 
       EEs05g_b_mua_95[grep(names(EEs05g_b_mua_95), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEs05g_b_mua_95), 
       length=0.05, angle=90, code=3)

par(opar)
barplot(height = EEs05g_b_mua_m[c(grep(names(EEs05g_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Air Entry Pressure', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs05g_b_mua_05[c(grep(names(EEs05g_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs05g_b_mua_95[c(grep(names(EEs05g_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs05g_b_mua_m[c(grep(names(EEs05g_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Pore Size', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs05g_b_mua_05[c(grep(names(EEs05g_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs05g_b_mua_95[c(grep(names(EEs05g_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs05g_b_mua_m[c(grep(names(EEs05g_b_mua_m), pattern = 'active', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Soil Thickness', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs05g_b_mua_05[grep(names(EEs05g_b_mua_05), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEs05g_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs05g_b_mua_95[grep(names(EEs05g_b_mua_95), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEs05g_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs05g_b_mua_m[c(grep(names(EEs05g_b_mua_m), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.001), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Snow Melt Temp. Coef.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs05g_b_mua_05[grep(names(EEs05g_b_mua_05), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEs05g_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs05g_b_mua_95[grep(names(EEs05g_b_mua_95), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEs05g_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs05g_b_mua_m[c(grep(names(EEs05g_b_mua_m), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.001), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Max Snow Energy Deficit', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs05g_b_mua_05[grep(names(EEs05g_b_mua_05), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEs05g_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs05g_b_mua_95[grep(names(EEs05g_b_mua_95), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEs05g_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs05g_b_mua_m[c(grep(names(EEs05g_b_mua_m), pattern = 'sla', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_b_mua_95), 
        names.arg = c('Tree', 'Grass'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Specific Leaf Area', xlab = 'Vegetation', cex.main=1.5)
arrows(seq(0.5,1.5,1), 
       EEs05g_b_mua_05[grep(names(EEs05g_b_mua_05), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEs05g_b_mua_95), 
       seq(0.5,1.5,1), 
       EEs05g_b_mua_95[grep(names(EEs05g_b_mua_95), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEs05g_b_mua_95), 
       length=0.05, angle=90, code=3)
dev.off()

#  panel - hillslopes----
for (h in 1:length(uhills)){
  png(paste0('Hill', uhills[h],'AllMultEEs_05s.png'), res = 300, width = 9, height = 9, units = 'in')
  opar=par
  layout(rbind(c(1,2,3), c(4,5,6), c(7,8,9)))
  barplot(height = EEs05_h_mua_m[uhills[h], c(grep(names(EEs05_h_mua_m[uhills[h],]), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs05_h_mua_m[uhills[h],]), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs05_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108', 'S8 and S108', 'S9 and S109'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'm: Ksat Decay with Sat. Def.', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,5.5,1), 
         EEs05_h_mua_05[uhills[h],c(grep(names(EEs05_h_mua_05[uhills[h],]), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs05_h_mua_05[uhills[h],]), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs05_h_mua_95[uhills[h],]), 
         seq(0.5,5.5,1), 
         EEs05_h_mua_95[uhills[h],c(grep(names(EEs05_h_mua_95[uhills[h],]), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs05_h_mua_95[uhills[h],]), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs05_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEs05_h_mua_m[uhills[h],c(grep(names(EEs05_h_mua_m[uhills[h],]), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = paste0('Hillslope ', uhills[uhills[h]], '\nSat. to GW Coef.'), xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEs05_h_mua_05[uhills[h],c(grep(names(EEs05_h_mua_m[uhills[h],]), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEs05_h_mua_95[uhills[h],c(grep(names(EEs05_h_mua_m[uhills[h],]), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  par(mar=c(6,4,3,0.5))
  barplot(height = EEs05_h_mua_m[uhills[h],c(grep(names(EEs05_h_mua_m[uhills[h],]), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_h_mua_95[uhills[h],]), 
          names.arg = c('S9 Ksat', 'S9 vKsat', 'S109 Ksat', 'S109 vKsat', 'S8 Ksat', 'S8 vKsat', 'S108 Ksat', 'S108 vKsat', 'S8 and S108', 'S9 and S109'), 
          ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Hydraulic Conductivity', xlab = '', las = 2, cex.main=1.5)
  axis(side = 1, at = 5, labels = 'Soils', line = 4, cex.axis = 1.2, tick = FALSE)
  arrows(seq(0.5,9.5,1), 
         EEs05_h_mua_05[uhills[h],grep(names(EEs05_h_mua_05[uhills[h],]), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_h_mua_95[uhills[h],]), 
         seq(0.5,9.5,1), 
         EEs05_h_mua_95[uhills[h],grep(names(EEs05_h_mua_95[uhills[h],]), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  par(opar)
  barplot(height = EEs05_h_mua_m[uhills[h],c(grep(names(EEs05_h_mua_m[uhills[h],]), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Air Entry Pressure', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEs05_h_mua_05[uhills[h],c(grep(names(EEs05_h_mua_m[uhills[h],]), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEs05_h_mua_95[uhills[h],c(grep(names(EEs05_h_mua_m[uhills[h],]), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEs05_h_mua_m[uhills[h],c(grep(names(EEs05_h_mua_m[uhills[h],]), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Pore Size', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEs05_h_mua_05[uhills[h],c(grep(names(EEs05_h_mua_m[uhills[h],]), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEs05_h_mua_95[uhills[h],c(grep(names(EEs05_h_mua_m[uhills[h],]), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEs05_h_mua_m[uhills[h],c(grep(names(EEs05_h_mua_m[uhills[h],]), pattern = 'active', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Soil Thickness', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEs05_h_mua_05[uhills[h],grep(names(EEs05_h_mua_05[uhills[h],]), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEs05_h_mua_95[uhills[h],grep(names(EEs05_h_mua_95[uhills[h],]), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEs05_h_mua_m[uhills[h],c(grep(names(EEs05_h_mua_m[uhills[h],]), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.001), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Snow Melt Temp. Coef.', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEs05_h_mua_05[uhills[h],grep(names(EEs05_h_mua_05[uhills[h],]), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEs05_h_mua_95[uhills[h],grep(names(EEs05_h_mua_95[uhills[h],]), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEs05_h_mua_m[uhills[h],c(grep(names(EEs05_h_mua_m[uhills[h],]), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.001), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Max Snow Energy Deficit', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEs05_h_mua_05[uhills[h],grep(names(EEs05_h_mua_05[uhills[h],]), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEs05_h_mua_95[uhills[h],grep(names(EEs05_h_mua_95[uhills[h],]), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEs05_h_mua_m[uhills[h],c(grep(names(EEs05_h_mua_m[uhills[h],]), pattern = 'sla', fixed=TRUE, ignore.case = FALSE))]/max(EEs05_h_mua_95[uhills[h],]), 
          names.arg = c('Tree', 'Grass'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Specific Leaf Area', xlab = 'Vegetation', cex.main=1.5)
  arrows(seq(0.5,1.5,1), 
         EEs05_h_mua_05[uhills[h],grep(names(EEs05_h_mua_05[uhills[h],]), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_h_mua_95[uhills[h],]), 
         seq(0.5,1.5,1), 
         EEs05_h_mua_95[uhills[h],grep(names(EEs05_h_mua_95[uhills[h],]), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEs05_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  dev.off()
  
  png(paste0('Hill', uhills[h],'AllMultEEs_05s_g.png'), res = 300, width = 9, height = 9, units = 'in')
  opar=par
  layout(rbind(c(1,2,3), c(4,5,6), c(7,8,9)))
  barplot(height = EEs05g_h_mua_m[uhills[h], c(grep(names(EEs05g_h_mua_m[uhills[h],]), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs05g_h_mua_m[uhills[h],]), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs05g_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108', 'S8 and S108', 'S9 and S109'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'm: Ksat Decay with Sat. Def.', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,5.5,1), 
         EEs05g_h_mua_05[uhills[h],c(grep(names(EEs05g_h_mua_05[uhills[h],]), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs05g_h_mua_05[uhills[h],]), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs05g_h_mua_95[uhills[h],]), 
         seq(0.5,5.5,1), 
         EEs05g_h_mua_95[uhills[h],c(grep(names(EEs05g_h_mua_95[uhills[h],]), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs05g_h_mua_95[uhills[h],]), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs05g_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEs05g_h_mua_m[uhills[h],c(grep(names(EEs05g_h_mua_m[uhills[h],]), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = paste0('Hillslope ', uhills[uhills[h]], '\nSat. to GW Coef.'), xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEs05g_h_mua_05[uhills[h],c(grep(names(EEs05g_h_mua_m[uhills[h],]), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEs05g_h_mua_95[uhills[h],c(grep(names(EEs05g_h_mua_m[uhills[h],]), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  par(mar=c(6,4,3,0.5))
  barplot(height = EEs05g_h_mua_m[uhills[h],c(grep(names(EEs05g_h_mua_m[uhills[h],]), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_h_mua_95[uhills[h],]), 
          names.arg = c('S9 Ksat', 'S9 vKsat', 'S109 Ksat', 'S109 vKsat', 'S8 Ksat', 'S8 vKsat', 'S108 Ksat', 'S108 vKsat', 'S8 and S108', 'S9 and S109'), 
          ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Hydraulic Conductivity', xlab = '', las = 2, cex.main=1.5)
  axis(side = 1, at = 5, labels = 'Soils', line = 4, cex.axis = 1.2, tick = FALSE)
  arrows(seq(0.5,9.5,1), 
         EEs05g_h_mua_05[uhills[h],grep(names(EEs05g_h_mua_05[uhills[h],]), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEs05g_h_mua_95[uhills[h],]), 
         seq(0.5,9.5,1), 
         EEs05g_h_mua_95[uhills[h],grep(names(EEs05g_h_mua_95[uhills[h],]), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEs05g_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  par(opar)
  barplot(height = EEs05g_h_mua_m[uhills[h],c(grep(names(EEs05g_h_mua_m[uhills[h],]), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Air Entry Pressure', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEs05g_h_mua_05[uhills[h],c(grep(names(EEs05g_h_mua_m[uhills[h],]), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEs05g_h_mua_95[uhills[h],c(grep(names(EEs05g_h_mua_m[uhills[h],]), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEs05g_h_mua_m[uhills[h],c(grep(names(EEs05g_h_mua_m[uhills[h],]), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Pore Size', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEs05g_h_mua_05[uhills[h],c(grep(names(EEs05g_h_mua_m[uhills[h],]), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEs05g_h_mua_95[uhills[h],c(grep(names(EEs05g_h_mua_m[uhills[h],]), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEs05g_h_mua_m[uhills[h],c(grep(names(EEs05g_h_mua_m[uhills[h],]), pattern = 'active', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Soil Thickness', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEs05g_h_mua_05[uhills[h],grep(names(EEs05g_h_mua_05[uhills[h],]), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEs05g_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEs05g_h_mua_95[uhills[h],grep(names(EEs05g_h_mua_95[uhills[h],]), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEs05g_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEs05g_h_mua_m[uhills[h],c(grep(names(EEs05g_h_mua_m[uhills[h],]), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.001), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Snow Melt Temp. Coef.', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEs05g_h_mua_05[uhills[h],grep(names(EEs05g_h_mua_05[uhills[h],]), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEs05g_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEs05g_h_mua_95[uhills[h],grep(names(EEs05g_h_mua_95[uhills[h],]), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEs05g_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEs05g_h_mua_m[uhills[h],c(grep(names(EEs05g_h_mua_m[uhills[h],]), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.001), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Max Snow Energy Deficit', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEs05g_h_mua_05[uhills[h],grep(names(EEs05g_h_mua_05[uhills[h],]), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEs05g_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEs05g_h_mua_95[uhills[h],grep(names(EEs05g_h_mua_95[uhills[h],]), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEs05g_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEs05g_h_mua_m[uhills[h],c(grep(names(EEs05g_h_mua_m[uhills[h],]), pattern = 'sla', fixed=TRUE, ignore.case = FALSE))]/max(EEs05g_h_mua_95[uhills[h],]), 
          names.arg = c('Tree', 'Grass'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Specific Leaf Area', xlab = 'Vegetation', cex.main=1.5)
  arrows(seq(0.5,1.5,1), 
         EEs05g_h_mua_05[uhills[h],grep(names(EEs05g_h_mua_05[uhills[h],]), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEs05g_h_mua_95[uhills[h],]), 
         seq(0.5,1.5,1), 
         EEs05g_h_mua_95[uhills[h],grep(names(EEs05g_h_mua_95[uhills[h],]), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEs05g_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  dev.off()
}

# Streamflow 5-95----
#  panel----
png('BasinAllMultEEs_ots.png', res = 300, width = 9, height = 9, units = 'in')
opar=par
layout(rbind(c(1,2,3), c(4,5,6), c(7,8,9)))
barplot(height = EEsot_b_mua_m[c(grep(names(EEsot_b_mua_m), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEsot_b_mua_m), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEsot_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108', 'S8 and S108', 'S9 and S109'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'm: Ksat Decay with Sat. Def.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,5.5,1), 
       EEsot_b_mua_05[c(grep(names(EEsot_b_mua_05), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEsot_b_mua_05), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEsot_b_mua_95), 
       seq(0.5,5.5,1), 
       EEsot_b_mua_95[c(grep(names(EEsot_b_mua_95), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEsot_b_mua_95), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEsot_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsot_b_mua_m[c(grep(names(EEsot_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Sat. to GW Coef.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsot_b_mua_05[c(grep(names(EEsot_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsot_b_mua_95[c(grep(names(EEsot_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_b_mua_95), 
       length=0.05, angle=90, code=3)

par(mar=c(6,4,3,0.5))
barplot(height = EEsot_b_mua_m[c(grep(names(EEsot_b_mua_m), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_b_mua_95), 
        names.arg = c('S9 Ksat', 'S9 vKsat', 'S109 Ksat', 'S109 vKsat', 'S8 Ksat', 'S8 vKsat', 'S108 Ksat', 'S108 vKsat', 'S8 and S108', 'S9 and S109'), 
        ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Hydraulic Conductivity', xlab = '', las = 2, cex.main=1.5)
axis(side = 1, at = 5, labels = 'Soils', line = 4, cex.axis = 1.2, tick = FALSE)
arrows(seq(0.5,9.5,1), 
       EEsot_b_mua_05[grep(names(EEsot_b_mua_05), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_b_mua_95), 
       seq(0.5,9.5,1), 
       EEsot_b_mua_95[grep(names(EEsot_b_mua_95), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_b_mua_95), 
       length=0.05, angle=90, code=3)

par(opar)
barplot(height = EEsot_b_mua_m[c(grep(names(EEsot_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Air Entry Pressure', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsot_b_mua_05[c(grep(names(EEsot_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsot_b_mua_95[c(grep(names(EEsot_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsot_b_mua_m[c(grep(names(EEsot_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Pore Size', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsot_b_mua_05[grep(names(EEsot_b_mua_05), pattern = 'pore', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsot_b_mua_95[grep(names(EEsot_b_mua_95), pattern = 'pore', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsot_b_mua_m[c(grep(names(EEsot_b_mua_m), pattern = 'active', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Soil Thickness', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsot_b_mua_05[grep(names(EEsot_b_mua_05), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsot_b_mua_95[grep(names(EEsot_b_mua_95), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsot_b_mua_m[c(grep(names(EEsot_b_mua_m), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.001), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Snow Melt Temp. Coef.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsot_b_mua_05[grep(names(EEsot_b_mua_05), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsot_b_mua_95[grep(names(EEsot_b_mua_95), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsot_b_mua_m[c(grep(names(EEsot_b_mua_m), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.001), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Max Snow Energy Deficit', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsot_b_mua_05[grep(names(EEsot_b_mua_05), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsot_b_mua_95[grep(names(EEsot_b_mua_95), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsot_b_mua_m[c(grep(names(EEsot_b_mua_m), pattern = 'sla', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_b_mua_95), 
        names.arg = c('Tree', 'Grass'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Specific Leaf Area', xlab = 'Vegetation', cex.main=1.5)
arrows(seq(0.5,1.5,1), 
       EEsot_b_mua_05[grep(names(EEsot_b_mua_05), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_b_mua_95), 
       seq(0.5,1.5,1), 
       EEsot_b_mua_95[grep(names(EEsot_b_mua_95), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_b_mua_95), 
       length=0.05, angle=90, code=3)
dev.off()

png('BasinAllMultEEs_ots_g.png', res = 300, width = 9, height = 9, units = 'in')
opar=par
layout(rbind(c(1,2,3), c(4,5,6), c(7,8,9)))
barplot(height = EEsotg_b_mua_m[c(grep(names(EEsotg_b_mua_m), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEsotg_b_mua_m), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEsotg_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108', 'S8 and S108', 'S9 and S109'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'm: Ksat Decay with Sat. Def.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,5.5,1), 
       EEsotg_b_mua_05[c(grep(names(EEsotg_b_mua_05), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEsotg_b_mua_05), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEsotg_b_mua_95), 
       seq(0.5,5.5,1), 
       EEsotg_b_mua_95[c(grep(names(EEsotg_b_mua_95), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEsotg_b_mua_95), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEsotg_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsotg_b_mua_m[c(grep(names(EEsotg_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Sat. to GW Coef.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsotg_b_mua_05[c(grep(names(EEsotg_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsotg_b_mua_95[c(grep(names(EEsotg_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_b_mua_95), 
       length=0.05, angle=90, code=3)

par(mar=c(6,4,3,0.5))
barplot(height = EEsotg_b_mua_m[c(grep(names(EEsotg_b_mua_m), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_b_mua_95), 
        names.arg = c('S9 Ksat', 'S9 vKsat', 'S109 Ksat', 'S109 vKsat', 'S8 Ksat', 'S8 vKsat', 'S108 Ksat', 'S108 vKsat', 'S8 and S108', 'S9 and S109'), 
        ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Hydraulic Conductivity', xlab = '', las = 2, cex.main=1.5)
axis(side = 1, at = 5, labels = 'Soils', line = 4, cex.axis = 1.2, tick = FALSE)
arrows(seq(0.5,9.5,1), 
       EEsotg_b_mua_05[grep(names(EEsotg_b_mua_05), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEsotg_b_mua_95), 
       seq(0.5,9.5,1), 
       EEsotg_b_mua_95[grep(names(EEsotg_b_mua_95), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEsotg_b_mua_95), 
       length=0.05, angle=90, code=3)

par(opar)
barplot(height = EEsotg_b_mua_m[c(grep(names(EEsotg_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Air Entry Pressure', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsotg_b_mua_05[c(grep(names(EEsotg_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsotg_b_mua_95[c(grep(names(EEsotg_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsotg_b_mua_m[c(grep(names(EEsotg_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Pore Size', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsotg_b_mua_05[grep(names(EEsotg_b_mua_05), pattern = 'pore', fixed=TRUE, ignore.case = FALSE)]/max(EEsotg_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsotg_b_mua_95[grep(names(EEsotg_b_mua_95), pattern = 'pore', fixed=TRUE, ignore.case = FALSE)]/max(EEsotg_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsotg_b_mua_m[c(grep(names(EEsotg_b_mua_m), pattern = 'active', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Soil Thickness', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsotg_b_mua_05[grep(names(EEsotg_b_mua_05), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEsotg_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsotg_b_mua_95[grep(names(EEsotg_b_mua_95), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEsotg_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsotg_b_mua_m[c(grep(names(EEsotg_b_mua_m), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.001), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Snow Melt Temp. Coef.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsotg_b_mua_05[grep(names(EEsotg_b_mua_05), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEsotg_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsotg_b_mua_95[grep(names(EEsotg_b_mua_95), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEsotg_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsotg_b_mua_m[c(grep(names(EEsotg_b_mua_m), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.001), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Max Snow Energy Deficit', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsotg_b_mua_05[grep(names(EEsotg_b_mua_05), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEsotg_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsotg_b_mua_95[grep(names(EEsotg_b_mua_95), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEsotg_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsotg_b_mua_m[c(grep(names(EEsotg_b_mua_m), pattern = 'sla', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_b_mua_95), 
        names.arg = c('Tree', 'Grass'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Specific Leaf Area', xlab = 'Vegetation', cex.main=1.5)
arrows(seq(0.5,1.5,1), 
       EEsotg_b_mua_05[grep(names(EEsotg_b_mua_05), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEsotg_b_mua_95), 
       seq(0.5,1.5,1), 
       EEsotg_b_mua_95[grep(names(EEsotg_b_mua_95), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEsotg_b_mua_95), 
       length=0.05, angle=90, code=3)
dev.off()

#  panel - hillslopes----
for (h in 1:length(uhills)){
  png(paste0('Hill', uhills[h],'AllMultEEs_ots.png'), res = 300, width = 9, height = 9, units = 'in')
  opar=par
  layout(rbind(c(1,2,3), c(4,5,6), c(7,8,9)))
  barplot(height = EEsot_h_mua_m[uhills[h], c(grep(names(EEsot_h_mua_m[uhills[h],]), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEsot_h_mua_m[uhills[h],]), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEsot_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108', 'S8 and S108', 'S9 and S109'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'm: Ksat Decay with Sat. Def.', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,5.5,1), 
         EEsot_h_mua_05[uhills[h],c(grep(names(EEsot_h_mua_05[uhills[h],]), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEsot_h_mua_05[uhills[h],]), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEsot_h_mua_95[uhills[h],]), 
         seq(0.5,5.5,1), 
         EEsot_h_mua_95[uhills[h],c(grep(names(EEsot_h_mua_95[uhills[h],]), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEsot_h_mua_95[uhills[h],]), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEsot_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEsot_h_mua_m[uhills[h],c(grep(names(EEsot_h_mua_m[uhills[h],]), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = paste0('Hillslope ', uhills[uhills[h]], '\nSat. to GW Coef.'), xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEsot_h_mua_05[uhills[h],c(grep(names(EEsot_h_mua_m[uhills[h],]), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEsot_h_mua_95[uhills[h],c(grep(names(EEsot_h_mua_m[uhills[h],]), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  par(mar=c(6,4,3,0.5))
  barplot(height = EEsot_h_mua_m[uhills[h],c(grep(names(EEsot_h_mua_m[uhills[h],]), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_h_mua_95[uhills[h],]), 
          names.arg = c('S9 Ksat', 'S9 vKsat', 'S109 Ksat', 'S109 vKsat', 'S8 Ksat', 'S8 vKsat', 'S108 Ksat', 'S108 vKsat', 'S8 and S108', 'S9 and S109'), 
          ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Hydraulic Conductivity', xlab = '', las = 2, cex.main=1.5)
  axis(side = 1, at = 5, labels = 'Soils', line = 4, cex.axis = 1.2, tick = FALSE)
  arrows(seq(0.5,9.5,1), 
         EEsot_h_mua_05[uhills[h],grep(names(EEsot_h_mua_05[uhills[h],]), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_h_mua_95[uhills[h],]), 
         seq(0.5,9.5,1), 
         EEsot_h_mua_95[uhills[h],grep(names(EEsot_h_mua_95[uhills[h],]), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  par(opar)
  barplot(height = EEsot_h_mua_m[uhills[h],c(grep(names(EEsot_h_mua_m[uhills[h],]), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Air Entry Pressure', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEsot_h_mua_05[uhills[h],c(grep(names(EEsot_h_mua_m[uhills[h],]), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEsot_h_mua_95[uhills[h],c(grep(names(EEsot_h_mua_m[uhills[h],]), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEsot_h_mua_m[uhills[h],c(grep(names(EEsot_h_mua_m[uhills[h],]), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Pore Size', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEsot_h_mua_05[uhills[h],c(grep(names(EEsot_h_mua_m[uhills[h],]), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEsot_h_mua_95[uhills[h],c(grep(names(EEsot_h_mua_m[uhills[h],]), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEsot_h_mua_m[uhills[h],c(grep(names(EEsot_h_mua_m[uhills[h],]), pattern = 'active', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Soil Thickness', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEsot_h_mua_05[uhills[h],grep(names(EEsot_h_mua_05[uhills[h],]), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEsot_h_mua_95[uhills[h],grep(names(EEsot_h_mua_95[uhills[h],]), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEsot_h_mua_m[uhills[h],c(grep(names(EEsot_h_mua_m[uhills[h],]), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.001), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Snow Melt Temp. Coef.', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEsot_h_mua_05[uhills[h],grep(names(EEsot_h_mua_05[uhills[h],]), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEsot_h_mua_95[uhills[h],grep(names(EEsot_h_mua_95[uhills[h],]), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEsot_h_mua_m[uhills[h],c(grep(names(EEsot_h_mua_m[uhills[h],]), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.001), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Max Snow Energy Deficit', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEsot_h_mua_05[uhills[h],grep(names(EEsot_h_mua_05[uhills[h],]), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEsot_h_mua_95[uhills[h],grep(names(EEsot_h_mua_95[uhills[h],]), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEsot_h_mua_m[uhills[h],c(grep(names(EEsot_h_mua_m[uhills[h],]), pattern = 'sla', fixed=TRUE, ignore.case = FALSE))]/max(EEsot_h_mua_95[uhills[h],]), 
          names.arg = c('Tree', 'Grass'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Specific Leaf Area', xlab = 'Vegetation', cex.main=1.5)
  arrows(seq(0.5,1.5,1), 
         EEsot_h_mua_05[uhills[h],grep(names(EEsot_h_mua_05[uhills[h],]), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_h_mua_95[uhills[h],]), 
         seq(0.5,1.5,1), 
         EEsot_h_mua_95[uhills[h],grep(names(EEsot_h_mua_95[uhills[h],]), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEsot_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  dev.off()
  
  png(paste0('Hill', uhills[h],'AllMultEEs_ots_g.png'), res = 300, width = 9, height = 9, units = 'in')
  opar=par
  layout(rbind(c(1,2,3), c(4,5,6), c(7,8,9)))
  barplot(height = EEsotg_h_mua_m[uhills[h], c(grep(names(EEsotg_h_mua_m[uhills[h],]), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEsotg_h_mua_m[uhills[h],]), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEsotg_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108', 'S8 and S108', 'S9 and S109'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'm: Ksat Decay with Sat. Def.', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,5.5,1), 
         EEsotg_h_mua_05[uhills[h],c(grep(names(EEsotg_h_mua_05[uhills[h],]), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEsotg_h_mua_05[uhills[h],]), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEsotg_h_mua_95[uhills[h],]), 
         seq(0.5,5.5,1), 
         EEsotg_h_mua_95[uhills[h],c(grep(names(EEsotg_h_mua_95[uhills[h],]), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEsotg_h_mua_95[uhills[h],]), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEsotg_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEsotg_h_mua_m[uhills[h],c(grep(names(EEsotg_h_mua_m[uhills[h],]), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = paste0('Hillslope ', uhills[uhills[h]], '\nSat. to GW Coef.'), xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEsotg_h_mua_05[uhills[h],c(grep(names(EEsotg_h_mua_m[uhills[h],]), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEsotg_h_mua_95[uhills[h],c(grep(names(EEsotg_h_mua_m[uhills[h],]), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  par(mar=c(6,4,3,0.5))
  barplot(height = EEsotg_h_mua_m[uhills[h],c(grep(names(EEsotg_h_mua_m[uhills[h],]), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_h_mua_95[uhills[h],]), 
          names.arg = c('S9 Ksat', 'S9 vKsat', 'S109 Ksat', 'S109 vKsat', 'S8 Ksat', 'S8 vKsat', 'S108 Ksat', 'S108 vKsat', 'S8 and S108', 'S9 and S109'), 
          ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Hydraulic Conductivity', xlab = '', las = 2, cex.main=1.5)
  axis(side = 1, at = 5, labels = 'Soils', line = 4, cex.axis = 1.2, tick = FALSE)
  arrows(seq(0.5,9.5,1), 
         EEsotg_h_mua_05[uhills[h],grep(names(EEsotg_h_mua_05[uhills[h],]), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEsotg_h_mua_95[uhills[h],]), 
         seq(0.5,9.5,1), 
         EEsotg_h_mua_95[uhills[h],grep(names(EEsotg_h_mua_95[uhills[h],]), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEsotg_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  par(opar)
  barplot(height = EEsotg_h_mua_m[uhills[h],c(grep(names(EEsotg_h_mua_m[uhills[h],]), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Air Entry Pressure', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEsotg_h_mua_05[uhills[h],c(grep(names(EEsotg_h_mua_m[uhills[h],]), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEsotg_h_mua_95[uhills[h],c(grep(names(EEsotg_h_mua_m[uhills[h],]), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEsotg_h_mua_m[uhills[h],c(grep(names(EEsotg_h_mua_m[uhills[h],]), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Pore Size', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEsotg_h_mua_05[uhills[h],c(grep(names(EEsotg_h_mua_m[uhills[h],]), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEsotg_h_mua_95[uhills[h],c(grep(names(EEsotg_h_mua_m[uhills[h],]), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEsotg_h_mua_m[uhills[h],c(grep(names(EEsotg_h_mua_m[uhills[h],]), pattern = 'active', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Soil Thickness', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEsotg_h_mua_05[uhills[h],grep(names(EEsotg_h_mua_05[uhills[h],]), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEsotg_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEsotg_h_mua_95[uhills[h],grep(names(EEsotg_h_mua_95[uhills[h],]), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEsotg_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEsotg_h_mua_m[uhills[h],c(grep(names(EEsotg_h_mua_m[uhills[h],]), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.001), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Snow Melt Temp. Coef.', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEsotg_h_mua_05[uhills[h],grep(names(EEsotg_h_mua_05[uhills[h],]), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEsotg_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEsotg_h_mua_95[uhills[h],grep(names(EEsotg_h_mua_95[uhills[h],]), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEsotg_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEsotg_h_mua_m[uhills[h],c(grep(names(EEsotg_h_mua_m[uhills[h],]), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.001), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Max Snow Energy Deficit', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEsotg_h_mua_05[uhills[h],grep(names(EEsotg_h_mua_05[uhills[h],]), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEsotg_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEsotg_h_mua_95[uhills[h],grep(names(EEsotg_h_mua_95[uhills[h],]), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEsotg_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEsotg_h_mua_m[uhills[h],c(grep(names(EEsotg_h_mua_m[uhills[h],]), pattern = 'sla', fixed=TRUE, ignore.case = FALSE))]/max(EEsotg_h_mua_95[uhills[h],]), 
          names.arg = c('Tree', 'Grass'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Specific Leaf Area', xlab = 'Vegetation', cex.main=1.5)
  arrows(seq(0.5,1.5,1), 
         EEsotg_h_mua_05[uhills[h],grep(names(EEsotg_h_mua_05[uhills[h],]), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEsotg_h_mua_95[uhills[h],]), 
         seq(0.5,1.5,1), 
         EEsotg_h_mua_95[uhills[h],grep(names(EEsotg_h_mua_95[uhills[h],]), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEsotg_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  dev.off()
}

# Streamflow 95%----
#  panel----
png('BasinAllMultEEs_95s.png', res = 300, width = 9, height = 9, units = 'in')
opar=par
layout(rbind(c(1,2,3), c(4,5,6), c(7,8,9)))
barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs95_b_mua_m), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108', 'S8 and S108', 'S9 and S109'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'm: Ksat Decay with Sat. Def.', xlab = 'Soils', las=2, cex.main=1.5)
arrows(seq(0.5,5.5,1), 
       EEs95_b_mua_05[c(grep(names(EEs95_b_mua_05), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs95_b_mua_05), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs95_b_mua_95), 
       seq(0.5,5.5,1), 
       EEs95_b_mua_95[c(grep(names(EEs95_b_mua_95), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs95_b_mua_95), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Sat. to GW Coef.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs95_b_mua_05[c(grep(names(EEs95_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs95_b_mua_95[c(grep(names(EEs95_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)

par(mar=c(6,4,3,0.5))
barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
        names.arg = c('S9 Ksat', 'S9 vKsat', 'S109 Ksat', 'S109 vKsat', 'S8 Ksat', 'S8 vKsat', 'S108 Ksat', 'S108 vKsat', 'S8 and S108', 'S9 and S109'), 
        ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Hydraulic Conductivity', xlab = '', las = 2, cex.main=1.5)
axis(side = 1, at = 5, labels = 'Soils', line = 4, cex.axis = 1.2, tick = FALSE)
arrows(seq(0.5,9.5,1), 
       EEs95_b_mua_05[grep(names(EEs95_b_mua_05), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       seq(0.5,9.5,1), 
       EEs95_b_mua_95[grep(names(EEs95_b_mua_95), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)

par(opar)
barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Air Entry Pressure', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs95_b_mua_05[c(grep(names(EEs95_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs95_b_mua_95[c(grep(names(EEs95_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Pore Size', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs95_b_mua_05[grep(names(EEs95_b_mua_05), pattern = 'pore', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs95_b_mua_95[grep(names(EEs95_b_mua_95), pattern = 'pore', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = 'active', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Soil Thickness', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs95_b_mua_05[grep(names(EEs95_b_mua_05), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs95_b_mua_95[grep(names(EEs95_b_mua_95), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.005), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Snow Melt Temp. Coef.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs95_b_mua_05[grep(names(EEs95_b_mua_05), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs95_b_mua_95[grep(names(EEs95_b_mua_95), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.005), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Max Snow Energy Deficit', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs95_b_mua_05[grep(names(EEs95_b_mua_05), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs95_b_mua_95[grep(names(EEs95_b_mua_95), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = 'sla', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
        names.arg = c('Tree', 'Grass'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Specific Leaf Area', xlab = 'Vegetation', cex.main=1.5)
arrows(seq(0.5,1.5,1), 
       EEs95_b_mua_05[grep(names(EEs95_b_mua_05), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       seq(0.5,1.5,1), 
       EEs95_b_mua_95[grep(names(EEs95_b_mua_95), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)
dev.off()

#  panel - hillslopes----
for (h in 1:length(uhills)){
  png(paste0('Hill', uhills[h],'AllMultEEs_95s.png'), res = 300, width = 9, height = 9, units = 'in')
  opar=par
  layout(rbind(c(1,2,3), c(4,5,6), c(7,8,9)))
  barplot(height = EEs95_h_mua_m[uhills[h], c(grep(names(EEs95_h_mua_m[uhills[h],]), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs95_h_mua_m[uhills[h],]), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs95_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108', 'S8 and S108', 'S9 and S109'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'm: Ksat Decay with Sat. Def.', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,5.5,1), 
         EEs95_h_mua_05[uhills[h],c(grep(names(EEs95_h_mua_05[uhills[h],]), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs95_h_mua_05[uhills[h],]), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs95_h_mua_95[uhills[h],]), 
         seq(0.5,5.5,1), 
         EEs95_h_mua_95[uhills[h],c(grep(names(EEs95_h_mua_95[uhills[h],]), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs95_h_mua_95[uhills[h],]), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs95_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEs95_h_mua_m[uhills[h],c(grep(names(EEs95_h_mua_m[uhills[h],]), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = paste0('Hillslope ', uhills[uhills[h]], '\nSat. to GW Coef.'), xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEs95_h_mua_05[uhills[h],c(grep(names(EEs95_h_mua_m[uhills[h],]), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEs95_h_mua_95[uhills[h],c(grep(names(EEs95_h_mua_m[uhills[h],]), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  par(mar=c(6,4,3,0.5))
  barplot(height = EEs95_h_mua_m[uhills[h],c(grep(names(EEs95_h_mua_m[uhills[h],]), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_h_mua_95[uhills[h],]), 
          names.arg = c('S9 Ksat', 'S9 vKsat', 'S109 Ksat', 'S109 vKsat', 'S8 Ksat', 'S8 vKsat', 'S108 Ksat', 'S108 vKsat', 'S8 and S108', 'S9 and S109'), 
          ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Hydraulic Conductivity', xlab = '', las = 2, cex.main=1.5)
  axis(side = 1, at = 5, labels = 'Soils', line = 4, cex.axis = 1.2, tick = FALSE)
  arrows(seq(0.5,9.5,1), 
         EEs95_h_mua_05[uhills[h],grep(names(EEs95_h_mua_05[uhills[h],]), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_h_mua_95[uhills[h],]), 
         seq(0.5,9.5,1), 
         EEs95_h_mua_95[uhills[h],grep(names(EEs95_h_mua_95[uhills[h],]), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  par(opar)
  barplot(height = EEs95_h_mua_m[uhills[h],c(grep(names(EEs95_h_mua_m[uhills[h],]), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Air Entry Pressure', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEs95_h_mua_05[uhills[h],c(grep(names(EEs95_h_mua_m[uhills[h],]), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEs95_h_mua_95[uhills[h],c(grep(names(EEs95_h_mua_m[uhills[h],]), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEs95_h_mua_m[uhills[h],c(grep(names(EEs95_h_mua_m[uhills[h],]), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Pore Size', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEs95_h_mua_05[uhills[h],c(grep(names(EEs95_h_mua_m[uhills[h],]), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEs95_h_mua_95[uhills[h],c(grep(names(EEs95_h_mua_m[uhills[h],]), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEs95_h_mua_m[uhills[h],c(grep(names(EEs95_h_mua_m[uhills[h],]), pattern = 'active', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Soil Thickness', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEs95_h_mua_05[uhills[h],grep(names(EEs95_h_mua_05[uhills[h],]), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEs95_h_mua_95[uhills[h],grep(names(EEs95_h_mua_95[uhills[h],]), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEs95_h_mua_m[uhills[h],c(grep(names(EEs95_h_mua_m[uhills[h],]), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.001), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Snow Melt Temp. Coef.', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEs95_h_mua_05[uhills[h],grep(names(EEs95_h_mua_05[uhills[h],]), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEs95_h_mua_95[uhills[h],grep(names(EEs95_h_mua_95[uhills[h],]), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEs95_h_mua_m[uhills[h],c(grep(names(EEs95_h_mua_m[uhills[h],]), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_h_mua_95[uhills[h],]), 
          names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.001), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Max Snow Energy Deficit', xlab = 'Soils', cex.main=1.5)
  arrows(seq(0.5,3.5,1), 
         EEs95_h_mua_05[uhills[h],grep(names(EEs95_h_mua_05[uhills[h],]), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_h_mua_95[uhills[h],]), 
         seq(0.5,3.5,1), 
         EEs95_h_mua_95[uhills[h],grep(names(EEs95_h_mua_95[uhills[h],]), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  
  barplot(height = EEs95_h_mua_m[uhills[h],c(grep(names(EEs95_h_mua_m[uhills[h],]), pattern = 'sla', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_h_mua_95[uhills[h],]), 
          names.arg = c('Tree', 'Grass'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Specific Leaf Area', xlab = 'Vegetation', cex.main=1.5)
  arrows(seq(0.5,1.5,1), 
         EEs95_h_mua_05[uhills[h],grep(names(EEs95_h_mua_05[uhills[h],]), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_h_mua_95[uhills[h],]), 
         seq(0.5,1.5,1), 
         EEs95_h_mua_95[uhills[h],grep(names(EEs95_h_mua_95[uhills[h],]), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_h_mua_95[uhills[h],]), 
         length=0.05, angle=90, code=3)
  dev.off()
}

#  pdf----
pdf(file = 'fig5.pdf', width = 9, height = 9, title = 'SAfig5')
par(mar=c(5.1, 4.5, 4.1, 2.1))
opar=par
layout(rbind(c(1,2,3), c(4,5,6), c(7,8,9)))
barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs95_b_mua_m), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs95_b_mua_95), 
        names.arg = NA, ylab = '', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'm: Ksat Decay with Sat. Def.', xlab = 'Soils', cex.main=1.5, axes=FALSE)
arrows(seq(0.5,5.5,1), 
       EEs95_b_mua_05[c(grep(names(EEs95_b_mua_05), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs95_b_mua_05), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs95_b_mua_95), 
       seq(0.5,5.5,1), 
       EEs95_b_mua_95[c(grep(names(EEs95_b_mua_95), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEs95_b_mua_95), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)
axis(side = 2, at = seq(0,1,0.2), labels = TRUE, cex.axis=1.5)
axis(side = 1, at = seq(0.5,5.5,1), labels = c('S9', 'S109', 'S8', 'S108', 'S8 and S108', 'S9 and S109'), cex.axis=1, las = 2)
par(xpd=TRUE)
text(labels = 'A', x = -1.2, y = 1.2, cex = 2)
par(xpd=FALSE)

barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = '', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Sat. to GW Coef.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs95_b_mua_05[c(grep(names(EEs95_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs95_b_mua_95[c(grep(names(EEs95_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)
par(xpd=TRUE)
text(labels = 'B', x = -0.8, y = 1.2, cex = 2)
par(xpd=FALSE)

par(mar=c(6,4.5,3,0.5))
barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
        names.arg = NA, 
        ylab = '', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Hydraulic Conductivity', xlab = '', las = 2, cex.main=1.5, axes=FALSE)
axis(side = 2, at = seq(0,1,0.2), labels = TRUE, cex.axis=1.5)
axis(side = 1, at = seq(0.5,9.5,1), labels = c('S9 Ksat', 'S9 vKsat', 'S109 Ksat', 'S109 vKsat', 'S8 Ksat', 'S8 vKsat', 'S108 Ksat', 'S108 vKsat', 'S8 and S108', 'S9 and S109'), cex.axis=1, las = 2)
axis(side = 1, at = 5, labels = 'Soils', line = 4, cex.axis = 1.2, tick = FALSE)
arrows(seq(0.5,9.5,1), 
       EEs95_b_mua_05[grep(names(EEs95_b_mua_05), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       seq(0.5,9.5,1), 
       EEs95_b_mua_95[grep(names(EEs95_b_mua_95), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)
par(xpd=TRUE)
text(labels = 'C', x = -1.8, y = 1.1, cex = 2)
par(xpd=FALSE)

par(opar)
barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Mean Abs. Val. of EE', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Air Entry Pressure', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs95_b_mua_05[c(grep(names(EEs95_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs95_b_mua_95[c(grep(names(EEs95_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)
par(xpd=TRUE)
text(labels = 'D', x = -.7, y = 1.1, cex = 2)
par(xpd=FALSE)

barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = '', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Pore Size', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs95_b_mua_05[grep(names(EEs95_b_mua_05), pattern = 'pore', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs95_b_mua_95[grep(names(EEs95_b_mua_95), pattern = 'pore', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)
par(xpd=TRUE)
text(labels = 'E', x = -0.7, y = 1.1, cex = 2)
par(xpd=FALSE)

barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = 'active', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = '', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Soil Thickness', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs95_b_mua_05[grep(names(EEs95_b_mua_05), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs95_b_mua_95[grep(names(EEs95_b_mua_95), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)
par(xpd=TRUE)
text(labels = 'F', x = -0.7, y = 1.1, cex = 2)
par(xpd=FALSE)

barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = '', ylim = c(0,0.005), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Snow Melt Temp. Coef.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs95_b_mua_05[grep(names(EEs95_b_mua_05), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs95_b_mua_95[grep(names(EEs95_b_mua_95), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)
par(xpd=TRUE)
text(labels = 'G', x = -0.7, y = 0.0055, cex = 2)
par(xpd=FALSE)

barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = '', ylim = c(0,0.005), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Max Snow Energy Deficit', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEs95_b_mua_05[grep(names(EEs95_b_mua_05), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       seq(0.5,3.5,1), 
       EEs95_b_mua_95[grep(names(EEs95_b_mua_95), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)
par(xpd=TRUE)
text(labels = 'H', x = -0.7, y = 0.0055, cex = 2)
par(xpd=FALSE)

barplot(height = EEs95_b_mua_m[c(grep(names(EEs95_b_mua_m), pattern = 'sla', fixed=TRUE, ignore.case = FALSE))]/max(EEs95_b_mua_95), 
        names.arg = c('Tree', 'Grass'), ylab = '', ylim = c(0,0.1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Specific Leaf Area', xlab = 'Vegetation', cex.main=1.5)
arrows(seq(0.5,1.5,1), 
       EEs95_b_mua_05[grep(names(EEs95_b_mua_05), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       seq(0.5,1.5,1), 
       EEs95_b_mua_95[grep(names(EEs95_b_mua_95), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEs95_b_mua_95), 
       length=0.05, angle=90, code=3)
par(xpd=TRUE)
text(labels = 'I', x = -0.3, y = .112, cex = 2)
par(xpd=FALSE)
dev.off()

# TN 5%----
#  panel----
png('BasinAllMultEEs_TN05.png', res = 300, width = 9, height = 9, units = 'in')
opar=par
layout(rbind(c(1,2,3), c(4,5,6), c(7,8,9)))
barplot(height = EEsTN05_b_mua_m[c(grep(names(EEsTN05_b_mua_m), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEsTN05_b_mua_m), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEsTN05_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108', 'S8 and S108', 'S9 and S109'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'm: Ksat Decay with Sat. Def.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,5.5,1), 
       EEsTN05_b_mua_05[c(grep(names(EEsTN05_b_mua_05), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEsTN05_b_mua_05), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEsTN05_b_mua_95), 
       seq(0.5,5.5,1), 
       EEsTN05_b_mua_95[c(grep(names(EEsTN05_b_mua_95), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEsTN05_b_mua_95), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEsTN05_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsTN05_b_mua_m[c(grep(names(EEsTN05_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN05_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Sat. to GW Coef.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsTN05_b_mua_05[c(grep(names(EEsTN05_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN05_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsTN05_b_mua_95[c(grep(names(EEsTN05_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN05_b_mua_95), 
       length=0.05, angle=90, code=3)

par(mar=c(6,4,3,0.5))
barplot(height = EEsTN05_b_mua_m[c(grep(names(EEsTN05_b_mua_m), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN05_b_mua_95), 
        names.arg = c('S9 Ksat', 'S9 vKsat', 'S109 Ksat', 'S109 vKsat', 'S8 Ksat', 'S8 vKsat', 'S108 Ksat', 'S108 vKsat', 'S8 and S108', 'S9 and S109'), 
        ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Hydraulic Conductivity', xlab = '', las = 2, cex.main=1.5)
axis(side = 1, at = 5, labels = 'Soils', line = 4, cex.axis = 1.2, tick = FALSE)
arrows(seq(0.5,9.5,1), 
       EEsTN05_b_mua_05[grep(names(EEsTN05_b_mua_05), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEsTN05_b_mua_95), 
       seq(0.5,9.5,1), 
       EEsTN05_b_mua_95[grep(names(EEsTN05_b_mua_95), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEsTN05_b_mua_95), 
       length=0.05, angle=90, code=3)

par(opar)
barplot(height = EEsTN05_b_mua_m[c(grep(names(EEsTN05_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN05_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Air Entry Pressure', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsTN05_b_mua_05[c(grep(names(EEsTN05_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN05_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsTN05_b_mua_95[c(grep(names(EEsTN05_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN05_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsTN05_b_mua_m[c(grep(names(EEsTN05_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN05_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Pore Size', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsTN05_b_mua_05[c(grep(names(EEsTN05_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN05_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsTN05_b_mua_95[c(grep(names(EEsTN05_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN05_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsTN05_b_mua_m[c(grep(names(EEsTN05_b_mua_m), pattern = 'active', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN05_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Soil Thickness', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsTN05_b_mua_05[grep(names(EEsTN05_b_mua_05), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEsTN05_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsTN05_b_mua_95[grep(names(EEsTN05_b_mua_95), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEsTN05_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsTN05_b_mua_m[c(grep(names(EEsTN05_b_mua_m), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN05_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.005), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Snow Melt Temp. Coef.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsTN05_b_mua_05[grep(names(EEsTN05_b_mua_05), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEsTN05_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsTN05_b_mua_95[grep(names(EEsTN05_b_mua_95), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEsTN05_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsTN05_b_mua_m[c(grep(names(EEsTN05_b_mua_m), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN05_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.005), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Max Snow Energy Deficit', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsTN05_b_mua_05[grep(names(EEsTN05_b_mua_05), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEsTN05_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsTN05_b_mua_95[grep(names(EEsTN05_b_mua_95), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEsTN05_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsTN05_b_mua_m[c(grep(names(EEsTN05_b_mua_m), pattern = 'sla', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN05_b_mua_95), 
        names.arg = c('Tree', 'Grass'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Specific Leaf Area', xlab = 'Vegetation', cex.main=1.5)
arrows(seq(0.5,1.5,1), 
       EEsTN05_b_mua_05[grep(names(EEsTN05_b_mua_05), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEsTN05_b_mua_95), 
       seq(0.5,1.5,1), 
       EEsTN05_b_mua_95[grep(names(EEsTN05_b_mua_95), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEsTN05_b_mua_95), 
       length=0.05, angle=90, code=3)
dev.off()

# TN Med----
#  panel----
png('BasinAllMultEEs_TNMed.png', res = 300, width = 9, height = 9, units = 'in')
opar=par
layout(rbind(c(1,2,3), c(4,5,6), c(7,8,9)))
barplot(height = EEsTNMed_b_mua_m[c(grep(names(EEsTNMed_b_mua_m), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEsTNMed_b_mua_m), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEsTNMed_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108', 'S8 and S108', 'S9 and S109'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'm: Ksat Decay with Sat. Def.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,5.5,1), 
       EEsTNMed_b_mua_05[c(grep(names(EEsTNMed_b_mua_05), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEsTNMed_b_mua_05), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEsTNMed_b_mua_95), 
       seq(0.5,5.5,1), 
       EEsTNMed_b_mua_95[c(grep(names(EEsTNMed_b_mua_95), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEsTNMed_b_mua_95), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEsTNMed_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsTNMed_b_mua_m[c(grep(names(EEsTNMed_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEsTNMed_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Sat. to GW Coef.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsTNMed_b_mua_05[c(grep(names(EEsTNMed_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEsTNMed_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsTNMed_b_mua_95[c(grep(names(EEsTNMed_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEsTNMed_b_mua_95), 
       length=0.05, angle=90, code=3)

par(mar=c(6,4,3,0.5))
barplot(height = EEsTNMed_b_mua_m[c(grep(names(EEsTNMed_b_mua_m), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE))]/max(EEsTNMed_b_mua_95), 
        names.arg = c('S9 Ksat', 'S9 vKsat', 'S109 Ksat', 'S109 vKsat', 'S8 Ksat', 'S8 vKsat', 'S108 Ksat', 'S108 vKsat', 'S8 and S108', 'S9 and S109'), 
        ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Hydraulic Conductivity', xlab = '', las = 2, cex.main=1.5)
axis(side = 1, at = 5, labels = 'Soils', line = 4, cex.axis = 1.2, tick = FALSE)
arrows(seq(0.5,9.5,1), 
       EEsTNMed_b_mua_05[grep(names(EEsTNMed_b_mua_05), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEsTNMed_b_mua_95), 
       seq(0.5,9.5,1), 
       EEsTNMed_b_mua_95[grep(names(EEsTNMed_b_mua_95), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEsTNMed_b_mua_95), 
       length=0.05, angle=90, code=3)

par(opar)
barplot(height = EEsTNMed_b_mua_m[c(grep(names(EEsTNMed_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEsTNMed_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Air Entry Pressure', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsTNMed_b_mua_05[c(grep(names(EEsTNMed_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEsTNMed_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsTNMed_b_mua_95[c(grep(names(EEsTNMed_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEsTNMed_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsTNMed_b_mua_m[c(grep(names(EEsTNMed_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEsTNMed_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Pore Size', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsTNMed_b_mua_05[c(grep(names(EEsTNMed_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEsTNMed_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsTNMed_b_mua_95[c(grep(names(EEsTNMed_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEsTNMed_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsTNMed_b_mua_m[c(grep(names(EEsTNMed_b_mua_m), pattern = 'active', fixed=TRUE, ignore.case = FALSE))]/max(EEsTNMed_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Soil Thickness', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsTNMed_b_mua_05[grep(names(EEsTNMed_b_mua_05), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEsTNMed_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsTNMed_b_mua_95[grep(names(EEsTNMed_b_mua_95), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEsTNMed_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsTNMed_b_mua_m[c(grep(names(EEsTNMed_b_mua_m), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE))]/max(EEsTNMed_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.005), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Snow Melt Temp. Coef.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsTNMed_b_mua_05[grep(names(EEsTNMed_b_mua_05), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEsTNMed_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsTNMed_b_mua_95[grep(names(EEsTNMed_b_mua_95), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEsTNMed_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsTNMed_b_mua_m[c(grep(names(EEsTNMed_b_mua_m), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE))]/max(EEsTNMed_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.005), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Max Snow Energy Deficit', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsTNMed_b_mua_05[grep(names(EEsTNMed_b_mua_05), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEsTNMed_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsTNMed_b_mua_95[grep(names(EEsTNMed_b_mua_95), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEsTNMed_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsTNMed_b_mua_m[c(grep(names(EEsTNMed_b_mua_m), pattern = 'sla', fixed=TRUE, ignore.case = FALSE))]/max(EEsTNMed_b_mua_95), 
        names.arg = c('Tree', 'Grass'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Specific Leaf Area', xlab = 'Vegetation', cex.main=1.5)
arrows(seq(0.5,1.5,1), 
       EEsTNMed_b_mua_05[grep(names(EEsTNMed_b_mua_05), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEsTNMed_b_mua_95), 
       seq(0.5,1.5,1), 
       EEsTNMed_b_mua_95[grep(names(EEsTNMed_b_mua_95), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEsTNMed_b_mua_95), 
       length=0.05, angle=90, code=3)
dev.off()

# TN 95%----
#  panel----
png('BasinAllMultEEs_TN95.png', res = 300, width = 9, height = 9, units = 'in')
opar=par
layout(rbind(c(1,2,3), c(4,5,6), c(7,8,9)))
barplot(height = EEsTN95_b_mua_m[c(grep(names(EEsTN95_b_mua_m), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEsTN95_b_mua_m), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEsTN95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108', 'S8 and S108', 'S9 and S109'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'm: Ksat Decay with Sat. Def.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,5.5,1), 
       EEsTN95_b_mua_05[c(grep(names(EEsTN95_b_mua_05), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEsTN95_b_mua_05), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEsTN95_b_mua_95), 
       seq(0.5,5.5,1), 
       EEsTN95_b_mua_95[c(grep(names(EEsTN95_b_mua_95), pattern = '9_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)], grep(names(EEsTN95_b_mua_95), pattern = '8_m', fixed=TRUE, ignore.case = FALSE)[c(1,3,5)])][c(1,2,4,5,6,3)]/max(EEsTN95_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsTN95_b_mua_m[c(grep(names(EEsTN95_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Sat. to GW Coef.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsTN95_b_mua_05[c(grep(names(EEsTN95_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN95_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsTN95_b_mua_95[c(grep(names(EEsTN95_b_mua_m), pattern = 'sat_to_gw', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN95_b_mua_95), 
       length=0.05, angle=90, code=3)

par(mar=c(6,4,3,0.5))
barplot(height = EEsTN95_b_mua_m[c(grep(names(EEsTN95_b_mua_m), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN95_b_mua_95), 
        names.arg = c('S9 Ksat', 'S9 vKsat', 'S109 Ksat', 'S109 vKsat', 'S8 Ksat', 'S8 vKsat', 'S108 Ksat', 'S108 vKsat', 'S8 and S108', 'S9 and S109'), 
        ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Hydraulic Conductivity', xlab = '', las = 2, cex.main=1.5)
axis(side = 1, at = 5, labels = 'Soils', line = 4, cex.axis = 1.2, tick = FALSE)
arrows(seq(0.5,9.5,1), 
       EEsTN95_b_mua_05[grep(names(EEsTN95_b_mua_05), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEsTN95_b_mua_95), 
       seq(0.5,9.5,1), 
       EEsTN95_b_mua_95[grep(names(EEsTN95_b_mua_95), pattern = 'Ksat', fixed=TRUE, ignore.case = FALSE)]/max(EEsTN95_b_mua_95), 
       length=0.05, angle=90, code=3)

par(opar)
barplot(height = EEsTN95_b_mua_m[c(grep(names(EEsTN95_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Air Entry Pressure', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsTN95_b_mua_05[c(grep(names(EEsTN95_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN95_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsTN95_b_mua_95[c(grep(names(EEsTN95_b_mua_m), pattern = 'psi_air', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN95_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsTN95_b_mua_m[c(grep(names(EEsTN95_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Pore Size', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsTN95_b_mua_05[c(grep(names(EEsTN95_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN95_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsTN95_b_mua_95[c(grep(names(EEsTN95_b_mua_m), pattern = 'pore', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN95_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsTN95_b_mua_m[c(grep(names(EEsTN95_b_mua_m), pattern = 'active', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Soil Thickness', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsTN95_b_mua_05[grep(names(EEsTN95_b_mua_05), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEsTN95_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsTN95_b_mua_95[grep(names(EEsTN95_b_mua_95), pattern = 'active', fixed=TRUE, ignore.case = FALSE)]/max(EEsTN95_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsTN95_b_mua_m[c(grep(names(EEsTN95_b_mua_m), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.005), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Snow Melt Temp. Coef.', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsTN95_b_mua_05[grep(names(EEsTN95_b_mua_05), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEsTN95_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsTN95_b_mua_95[grep(names(EEsTN95_b_mua_95), pattern = 'snow_melt', fixed=TRUE, ignore.case = FALSE)]/max(EEsTN95_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsTN95_b_mua_m[c(grep(names(EEsTN95_b_mua_m), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN95_b_mua_95), 
        names.arg = c('S9', 'S109', 'S8', 'S108'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.005), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Max Snow Energy Deficit', xlab = 'Soils', cex.main=1.5)
arrows(seq(0.5,3.5,1), 
       EEsTN95_b_mua_05[grep(names(EEsTN95_b_mua_05), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEsTN95_b_mua_95), 
       seq(0.5,3.5,1), 
       EEsTN95_b_mua_95[grep(names(EEsTN95_b_mua_95), pattern = 'snow_energy', fixed=TRUE, ignore.case = FALSE)]/max(EEsTN95_b_mua_95), 
       length=0.05, angle=90, code=3)

barplot(height = EEsTN95_b_mua_m[c(grep(names(EEsTN95_b_mua_m), pattern = 'sla', fixed=TRUE, ignore.case = FALSE))]/max(EEsTN95_b_mua_95), 
        names.arg = c('Tree', 'Grass'), ylab = 'Normalized Elementary Effect', ylim = c(0,0.1), cex.axis = 1.5, cex.lab = 1.5, space = 0, main = 'Specific Leaf Area', xlab = 'Vegetation', cex.main=1.5)
arrows(seq(0.5,1.5,1), 
       EEsTN95_b_mua_05[grep(names(EEsTN95_b_mua_05), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEsTN95_b_mua_95), 
       seq(0.5,1.5,1), 
       EEsTN95_b_mua_95[grep(names(EEsTN95_b_mua_95), pattern = 'sla', fixed=TRUE, ignore.case = FALSE)]/max(EEsTN95_b_mua_95), 
       length=0.05, angle=90, code=3)
dev.off()

setwd(dir_Main)

#Pre-2020 Diagnosis of problems with EEs----
# for (i in 1:cols){
#   ind = i+(1+cols)*(t-1)
#   parm = which((OrigParams[ind+1,] - OrigParams[ind,]) != 0)
#   if(parm == 65){
#     print(i)
#     break
#   }
# }
# rm(i, ind, parm)
# 
# #For all of these, the input parameter did not change when it was supposed to (it did change in the original input file)
# #(3365 and 3366) for v102_epc.frootlitr_fcel is NaN because delta and change in values are 0
# #(4913 and 4914) for v102_epc.frootlitr_fcel is NaN
# #(5837 and 5838) for "s8_Ksat_0_v" is NaN
# #(644 and 645) for "s109_Ksat_0_v" is NaN
# #(5388 and 5389) for "s109_Ksat_0_v" is NaN
# 
# for (i in 1:r){
#   j = which(Deltas[r,] == min(abs(Deltas)))
#   if (length(j) > 0){
#     print(j)
#   }
# }
# rm(i,j)
# 
# #Resample values for the NaN replicates
# #644:
# set.seed(644)
# runif(n = 1, min = 0.1, max = 0.36767)
# #3365:
# set.seed(3365)
# runif(n = 1, min = 0.4, max = 0.5)
# #4913:
# set.seed(4913)
# runif(n = 1, min = 0.4, max = 0.5)
# #5388
# set.seed(5388)
# runif(n = 1, min = 0.1, max = .909091)
# #5837
# #set manually to 0.4 because both Ksat0 and Ksat0v needed adjustment from lower bound
