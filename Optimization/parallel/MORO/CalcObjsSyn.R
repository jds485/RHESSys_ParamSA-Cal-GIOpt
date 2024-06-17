print('Starting Objective Calculation')

#1: Row of file in arg[10] to read in (decision variables with ID for N and M)
#2: parameter set (0 because synthetic)
#3: blank
#4: directory of simulated streamflow file
#5: random seed
#6: full path to the likelihood parameter csv file for the ith parameterization
#7: modified lulcFrac file
#8: number of error timeseries to draw (1 for synthetic, 1000 for not)
arg = commandArgs(T)

#Load libraries----
library(vroom)
library(timeDate)
library(timeSeries)
library(fBasics)
library(fGarch)

#Load likelihood simulation functions----
source('/scratch/js4yd/Bais910Hill30mOptimizeGI_MORO/LikelihoodFun/GL_maineqs.R')

#Read in the pre-GI streamflow in cfs units----
#Already has desired date range
Qp = read.table(paste0('/scratch/js4yd/Bais910Hill30mOptimizeGI_MORO/obs/Q', arg[2], '.txt'), header=TRUE, sep='\t', stringsAsFactors=FALSE)
# Compute the 5th and 95th quantiles----
Qp05 = as.numeric(quantile(Qp$Flow, 0.05))
Qp95 = as.numeric(quantile(Qp$Flow, 0.95))

#Get conversion_b for streamflow----
world = read.csv('/scratch/js4yd/Bais910Hill30mOptimizeGI_MORO/RHESSys_Baisman30m_g74/worldfiles/worldfile.csv', stringsAsFactors = FALSE)
res = 30
#Taking the unique patch IDs because strata can exist in more than one patch.
Area.basin = length(unique(world$patchID))*res^2
rm(world, res)
#Multiplier conversion for basin streamflow (mm/d)*conversion_b -> cfs
conversion_b = Area.basin/1000/(.3048^3)/24/3600
rm(Area.basin)

#Read in with-GI simulated basin streamflow and convert to cfs----
Q = vroom(paste0(arg[4], '/Run', arg[1], '_Syn_basin.daily'), delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)

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
set.seed(as.numeric(arg[5]))
#Fixme: how should this be completed for the GI edited landscape? We don't have a bias parameter, so the error is not scaled by flow magnitude.

#Flow matrix
Qflow = matrix(0, nrow = nrow(Qp), ncol = as.numeric(arg[8]))
#Objective vectors
O1 = O2 = vector('numeric', length=as.numeric(arg[8]))
#Sum for objective 1
S11 = sum(Qp$Flow[Qp$Flow >= Qp95])
#Sum for objective 2
S21 = sum(Qp$Flow[Qp$Flow <= Qp05])

for (i in 1:as.numeric(arg[8])){
  q = et_syn_MaxSigt(sim_inflow = Q$streamflow, 
                     sigma_0 = LQ$sigma_0, sigma_1 = LQ$sigma_1, phi_1 = LQ$phi_1, 
                     beta = LQ$beta, xi = LQ$xi, mu_h = LQ$mu_h)
  Qflow[,i] = q[[2]]
  #Set negative flow to 0
  Qflow[,i][Qflow[,i] < 0] = 0
  
  #Compute objectives for each of the simulated streamflows----
  # O1: Flooding----
  S12 = sum(Qflow[,i][Qflow[,i] >= Qp95])
  O1[i] = (S11 - S12)/S11
  
  # O2: Low flow----
  S22 = sum(Qflow[,i][Qflow[,i] <= Qp05])
  O2[i] = (S21 - S22)/S21
}
rm(i, q)

# O3: Area converted----
#This should be from the lulc file printed from GIAllocation script.
lcfrac = read.csv(arg[7])
O3 = sum(lcfrac$lulc13)/9

# Combine into a vector----
#Note: First 2 should be maximized, so negating for minimization in Borg
Objs = c(-mean(O1), -mean(O2), O3)

#Write objectives to file----
write.table(Objs, paste0(arg[4], '/Run', arg[1], '_Syn_Objs.txt'), row.names = FALSE, col.names = FALSE, sep = '\n')

print('Ending Objective Calculation')