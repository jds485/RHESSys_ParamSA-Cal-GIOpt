print('Starting Objective Calculation')

#1: NFE
#2: i (parameterization)
#3: master number
#4: directory of simulated streamflow file
#5: random seed
#6: full path to the likelihood parameter csv file for the ith parameterization
#7: modified lulcFrac file
#8: directory to place a copy of the objectives for Borg to read
#9: number of error timeseries to draw
arg = commandArgs(T)
.libPaths(arg[10])

#Load libraries----
library(vroom)
library(timeDate)
library(timeSeries)
library(fBasics)
library(fGarch)

#Load likelihood simulation functions----
source('/scratch/js4yd/Bais910Hill30mOptGI_MOROMinMax/LikelihoodFun/GL_maineqs.R')

#Read in the pre-GI streamflow in cfs units----
#Already has desired date range
Qp = read.table(paste0('/scratch/js4yd/Bais910Hill30mOptGI_MOROMinMax/obs/Q', arg[2], 'NoError.txt'), header=TRUE, sep='\t', stringsAsFactors=FALSE)

#Get conversion_b for streamflow----
world = read.csv('/scratch/js4yd/Bais910Hill30mOptGI_MOROMinMax/RHESSys_Baisman30m_g74/worldfiles/worldfile.csv', stringsAsFactors = FALSE)
res = 30
#Taking the unique patch IDs because strata can exist in more than one patch.
Area.basin = length(unique(world$patchID))*res^2
rm(world, res)
#Multiplier conversion for basin streamflow (mm/d)*conversion_b -> cfs
conversion_b = Area.basin/1000/(.3048^3)/24/3600
rm(Area.basin)

#Read in with-GI simulated basin streamflow and convert to cfs----
Q = vroom(paste0(arg[4], '/Run', arg[1], '_P', arg[2], '_M', arg[3], '_basin.daily'), delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)

#Make a new Date column
Q$Date = as.Date(paste0(Q$year, '-', Q$month, '-', Q$day))

#Retain only streamflow and Date columns for space
Q = as.data.frame(Q[,c('Date','streamflow')])

#Trim off spin-up years
Q = Q[which(as.Date(Q$Date) >= as.Date(Qp$Date[1])),]

#Convert simulated streamflow to cfs units
Q$streamflow = round(Q$streamflow*conversion_b, 6)

#Load the ith set of likelihood parameters----
LQ = read.csv(arg[6], stringsAsFactors=FALSE)

#Add residual error to streamflows using the likelihood parameters----
#Fixme: how should this be completed for the GI edited landscape? We don't have a bias parameter, so the error is not scaled by flow magnitude.
#Flow matrix
Qpflow = Qflow = matrix(0, nrow = nrow(Qp), ncol = as.numeric(arg[9]))
#Objective vectors
O1 = O2 = vector('numeric', length=as.numeric(arg[9]))

for (i in 1:as.numeric(arg[9])){
  set.seed(as.numeric(arg[5])*3 + i - 1)
  q = et_syn_MaxSigt(sim_inflow = Qp$streamflow, 
                     sigma_0 = LQ$sigma_0, sigma_1 = LQ$sigma_1, phi_1 = LQ$phi_1, 
                     beta = LQ$beta, xi = LQ$xi, mu_h = LQ$mu_h)
  Qpflow[,i] = q[[2]]
  
  set.seed(as.numeric(arg[5])*3 + i - 1)
  q = et_syn_MaxSigt(sim_inflow = Q$streamflow, 
                     sigma_0 = LQ$sigma_0, sigma_1 = LQ$sigma_1, phi_1 = LQ$phi_1, 
                     beta = LQ$beta, xi = LQ$xi, mu_h = LQ$mu_h)
  Qflow[,i] = q[[2]]
  
  #Set negative flow to 0
  Qpflow[,i][Qpflow[,i] < 0] = 0
  Qflow[,i][Qflow[,i] < 0] = 0
  
  # Compute the 5th and 95th quantiles of the pre-GI flow----
  Qp05 = as.numeric(quantile(Qpflow[,i], 0.05))
  Qp95 = as.numeric(quantile(Qpflow[,i], 0.95))
  
  #Compute objectives for each of the simulated streamflows----
  # O1: Flooding----
  S11 = sum(Qpflow[,i][Qpflow[,i] >= Qp95])
  S12 = sum(Qflow[,i][Qflow[,i] >= Qp95])
  O1[i] = (S11 - S12)/S11
  
  # O2: Low flow----
  S21 = sum(Qpflow[,i][Qpflow[,i] <= Qp05])
  S22 = sum(Qflow[,i][Qflow[,i] <= Qp05])
  O2[i] = (S21 - S22)/S21
}
rm(i, q)

#Write the flows to a matrix - large data files! Not printing for now.
#write.csv(Qflow, paste0(arg[4], '/Run', arg[1], '_P', arg[2], '_M', arg[3], '_Flows.csv'), row.names=as.Date(Qp$Date))

# O3: Area converted----
#This should be from the lulc file printed from GIAllocation script.
lcfrac = read.csv(arg[7])
O3 = sum(lcfrac$lulc13)/9

# Combine into a vector----
#Note: First 2 should be maximized, so negating for minimization in Borg
Objs = c(-mean(O1), -mean(O2), O3)

#Write objectives to file----
write.table(Objs, paste0(arg[4], '/Run', arg[1], '_P', arg[2], '_M', arg[3], '_Objs.txt'), row.names = FALSE, col.names = FALSE, sep = '\n')
write.table(Objs, paste0(arg[8], '/Run', arg[1], '_P', arg[2], '_M', arg[3], '_Objs.txt'), row.names = FALSE, col.names = FALSE, sep = '\n')

print(Objs)
print('Ending Objective Calculation')