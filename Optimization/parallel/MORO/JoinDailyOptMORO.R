library(vroom)
library(timeDate)
library(timeSeries)
library(fBasics)
library(fGarch)

#arguments are:
#1: directory with RHESSysRuns
#2: Extension to output ('/RHESSys_Baisman30m_g74')
#3: save directory
#4: original decision variables and objectives file with IDs for master and NFE
#5: number of parameter sets evaluated
#6: RHESSys grid cell resolution
#7: Start date for evaluations

arg = commandArgs(T)

#Load likelihood simulation functions----
source(paste0(arg[3],'/LikelihoodFun/GL_maineqs.R'))

setwd(arg[1])

#Read in decision vars and objectives saved in one file
DVO = read.table(arg[4], sep='\t', header=TRUE)

#Get conversion_b for streamflow----
world = read.csv(paste0(arg[3], arg[2],'/worldfiles/worldfile.csv'), stringsAsFactors = FALSE)
res = as.numeric(arg[6])
#Taking the unique patch IDs because strata can exist in more than one patch.
Area.basin = length(unique(world$patchID))*res^2
rm(world, res)
#Multiplier conversion for basin streamflow (mm/d)*conversion_b -> cfs
conversion_b = Area.basin/1000/(.3048^3)/24/3600
rm(Area.basin)

#Make dataframe to store the compiled data
#Read in temporary file
Qi = vroom(paste0('./Run', DVO$N[1], '_P1_M', DVO$M[1], arg[2], '/output/Run', DVO$N[1], '_P1_M', DVO$M[1], '_basin.daily'), delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
#Make a new Date column
Qi$Date = as.Date(paste0(Qi$year, '-', Qi$month, '-', Qi$day))
#Retain only streamflow, ET, sat. def columns for space
Qi = as.data.frame(Qi[,c('Date','streamflow','evap','trans','sat_def')])
#Trim off spin-up years
Qi = Qi[which(as.Date(Qi$Date) >= as.Date(arg[7])),]
#Convert simulated streamflow to cfs units
Qi$streamflow = round(Qi$streamflow*conversion_b, 6)

#First 3 columns are N, P, M
Q_b = as.data.frame(matrix(NA, nrow = nrow(DVO)*as.numeric(arg[5]), ncol = 3+nrow(Qi)))
colnames(Q_b) = c('N', 'P', 'M', as.Date(Qi$Date))
QSyn_b = QEval_b = QEvalp05_b = QEvalp95_b = Q_b
SD_b = as.data.frame(matrix(NA, nrow = nrow(DVO)*as.numeric(arg[5]), ncol = 3+nrow(Qi)))
colnames(SD_b) = c('N', 'P', 'M', as.Date(Qi$Date))
E_b = as.data.frame(matrix(NA, nrow = nrow(DVO)*as.numeric(arg[5]), ncol = 3+nrow(Qi)))
colnames(E_b) = c('N', 'P', 'M', as.Date(Qi$Date))
T_b = as.data.frame(matrix(NA, nrow = nrow(DVO)*as.numeric(arg[5]), ncol = 3+nrow(Qi)))
colnames(T_b) = c('N', 'P', 'M', as.Date(Qi$Date))
rm(Qi)

#Load synthetic likelihood parameters
LQ = read.csv(paste0(arg[3], arg[2],'/defs0/Params_logLQ_0.csv'), stringsAsFactors=FALSE)

#Begin loop to extract data  
for (num in 1:nrow(DVO)){  
  #Loop over parameter sets
  for (p in 1:as.numeric(arg[5])){
    #Read in flow
    Qi = vroom(paste0('./Run', DVO$N[num], '_P', p, '_M', DVO$M[num], arg[2], '/output/Run', DVO$N[num], '_P', p, '_M', DVO$M[num], '_basin.daily'), delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)
    #Make a new Date column
    Qi$Date = as.Date(paste0(Qi$year, '-', Qi$month, '-', Qi$day))
    #Retain only streamflow, ET, sat. def columns for space
    Qi = as.data.frame(Qi[,c('Date','streamflow','evap','trans','sat_def')])
    #Trim off spin-up years
    Qi = Qi[which(as.Date(Qi$Date) >= as.Date(arg[7])),]
    #Convert simulated streamflow to cfs units
    Qi$streamflow = round(Qi$streamflow*conversion_b, 6)
    
    #Add to storage dataframe
    Q_b$N[p + as.numeric(arg[5])*(num-1)] = DVO$N[num]
    Q_b$P[p + as.numeric(arg[5])*(num-1)] = p
    Q_b$M[p + as.numeric(arg[5])*(num-1)] = DVO$M[num]
    Q_b[p + as.numeric(arg[5])*(num-1), -c(1,2,3)] = Qi$streamflow
    
    SD_b$N[p + as.numeric(arg[5])*(num-1)] = DVO$N[num]
    SD_b$P[p + as.numeric(arg[5])*(num-1)] = p
    SD_b$M[p + as.numeric(arg[5])*(num-1)] = DVO$M[num]
    SD_b[p + as.numeric(arg[5])*(num-1), -c(1,2,3)] = Qi$sat_def
    
    E_b$N[p + as.numeric(arg[5])*(num-1)] = DVO$N[num]
    E_b$P[p + as.numeric(arg[5])*(num-1)] = p
    E_b$M[p + as.numeric(arg[5])*(num-1)] = DVO$M[num]
    E_b[p + as.numeric(arg[5])*(num-1), -c(1,2,3)] = Qi$evap
    
    T_b$N[p + as.numeric(arg[5])*(num-1)] = DVO$N[num]
    T_b$P[p + as.numeric(arg[5])*(num-1)] = p
    T_b$M[p + as.numeric(arg[5])*(num-1)] = DVO$M[num]
    T_b[p + as.numeric(arg[5])*(num-1), -c(1,2,3)] = Qi$trans
    
    #Add error to streamflow timeseries
    #Synthetic
    set.seed(157)
    SynQ = et_syn_MaxSigt(sim_inflow = Qi$streamflow,
                          sigma_0 = LQ$sigma_0, sigma_1 = LQ$sigma_1, phi_1 = LQ$phi_1,
                          beta = LQ$beta, xi = LQ$xi, mu_h = LQ$mu_h)
    SynQ = SynQ[[2]]
    #Set negative flow to 0
    SynQ[SynQ < 0] = 0
    
    QSyn_b$N[p + as.numeric(arg[5])*(num-1)] = DVO$N[num]
    QSyn_b$P[p + as.numeric(arg[5])*(num-1)] = p
    QSyn_b$M[p + as.numeric(arg[5])*(num-1)] = DVO$M[num]
    QSyn_b[p + as.numeric(arg[5])*(num-1), -c(1,2,3)] = SynQ
    
    #As evaluated initially
    #Load the pth set of likelihood parameters
    LQEval = read.csv(paste0(arg[3], arg[2],'/defs', p, '/Params_logLQ_', p, '.csv'), stringsAsFactors=FALSE)
    #Base random seed
    seed = 2000 + p + 19 * DVO$N[num] + DVO$M[num] * 10000
    
    #Matrix to store generated flows
    Qflow = matrix(0, nrow = nrow(Qi), ncol = 1000)
    
    #Loop over 1000 seeds
    for (i in 1:1000){
      set.seed(seed*3 + i - 1)
      q = et_syn_MaxSigt(sim_inflow = Qi$streamflow, 
                     sigma_0 = LQEval$sigma_0, sigma_1 = LQEval$sigma_1, phi_1 = LQEval$phi_1, 
                     beta = LQEval$beta, xi = LQEval$xi, mu_h = LQEval$mu_h)
      Qflow[,i] = q[[2]]
  
      #Set negative flow to 0
      Qflow[,i][Qflow[,i] < 0] = 0
    }
    #Save file with all 1000 replicates for later use
    write.table(round(Qflow,7), paste0(arg[3], '/Q_N', DVO$N[num], '_P', p, '_M', DVO$M[num], '_MoroErr.txt'), row.names=FALSE, col.names=TRUE, sep='\t')
    
    EvalQ = rowMeans(Qflow)
    EvalQ_p05 = apply(Qflow,1,quantile, probs=0.05)
    EvalQ_p95 = apply(Qflow,1,quantile, probs=0.95)
    
    QEval_b$N[p + as.numeric(arg[5])*(num-1)] = DVO$N[num]
    QEval_b$P[p + as.numeric(arg[5])*(num-1)] = p
    QEval_b$M[p + as.numeric(arg[5])*(num-1)] = DVO$M[num]
    QEval_b[p + as.numeric(arg[5])*(num-1), -c(1,2,3)] = EvalQ
    
    QEvalp05_b$N[p + as.numeric(arg[5])*(num-1)] = DVO$N[num]
    QEvalp05_b$P[p + as.numeric(arg[5])*(num-1)] = p
    QEvalp05_b$M[p + as.numeric(arg[5])*(num-1)] = DVO$M[num]
    QEvalp05_b[p + as.numeric(arg[5])*(num-1), -c(1,2,3)] = EvalQ_p05
    
    QEvalp95_b$N[p + as.numeric(arg[5])*(num-1)] = DVO$N[num]
    QEvalp95_b$P[p + as.numeric(arg[5])*(num-1)] = p
    QEvalp95_b$M[p + as.numeric(arg[5])*(num-1)] = DVO$M[num]
    QEvalp95_b[p + as.numeric(arg[5])*(num-1), -c(1,2,3)] = EvalQ_p95  
  }
}

#Save compiled data
setwd(arg[3])
write.table(Q_b, paste0('Q_c_MORO_NoErr.txt'), row.names=FALSE, col.names=TRUE, sep='\t')
write.table(QSyn_b, paste0('Q_c_MORO_SynErr.txt'), row.names=FALSE, col.names=TRUE, sep='\t')
write.table(QEval_b, paste0('Q_c_MORO_MeanMoroErr.txt'), row.names=FALSE, col.names=TRUE, sep='\t')
write.table(QEvalp05_b, paste0('Q_c_MORO_05MoroErr.txt'), row.names=FALSE, col.names=TRUE, sep='\t')
write.table(QEvalp95_b, paste0('Q_c_MORO_95MoroErr.txt'), row.names=FALSE, col.names=TRUE, sep='\t')
write.table(round(SD_b,1), paste0('SD_c_MORO_NoErr.txt'), row.names=FALSE, col.names=TRUE, sep='\t')
write.table(round(E_b,6), paste0('E_c_MORO_NoErr.txt'), row.names=FALSE, col.names=TRUE, sep='\t')
write.table(round(T_b,6), paste0('T_c_MORO_NoErr.txt'), row.names=FALSE, col.names=TRUE, sep='\t')