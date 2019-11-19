library(sensitivity)

#Load Morris parameter file
InputParams = read.csv("C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\defs\\MorrisSampleLocs\\MorrisSamples_AfterProcessing.csv", stringsAsFactors = FALSE)

#Remove all of the parameters with _orig. They were not modified
InputParams = InputParams[-grep(x = colnames(InputParams), pattern = '_orig', fixed = TRUE)]
cols = ncol(InputParams)

#Number of trajectories
r = 40

#Load the observed streamflow record
obs = read.table("C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\obs\\BaismanStreamflow_Cal.txt", header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, sep = '\t')
#Remove observations later than 9/30/2010 (SA timeperiod end)
obs = obs[as.Date(obs$Date) <= as.Date(colnames(BasinSF)[ncol(BasinSF)]),]
#Remove observations earlier than 10/01/2004 (SA timeperiod start)
obs = obs[as.Date(obs$Date) >= as.Date('2004-10-01'),]

#Find the days with the highest 5th percentile, lowest 5th percentile and all other flows
q05 = quantile(x = obs$Flow, probs = 0.05)
q95 = quantile(x = obs$Flow, probs = 0.95)

days05 = as.Date(obs$Date[obs$Flow <= q05])
days95 = as.Date(obs$Date[obs$Flow >= q95])

#Check what days these correspond to - want a uniform distribution in time
hist(days05, breaks = 100)
hist(days95, breaks = 100, freq = TRUE)

#Load RHESSys streamflow data
setwd("C:\\Users\\jsmif\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR")
BasinSF = read.table(file = 'SAResults_BasinStreamflow_p4.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)
HillSF = read.table(file = 'SAResults_HillStreamflow_p6.txt', sep = '\t', stringsAsFactors = FALSE, header = TRUE, check.names = FALSE)

#Remove observations earlier than 10/01/2004 (SA timeperiod start)
BasinSF = BasinSF[, c(1, which(as.Date(colnames(BasinSF[,-1])) >= as.Date('2004-10-01')))]

#Loop over the trajectories
for (t in 1:r){
  #Compute the EEs for all parameters in the trajectory
  for (i in 1:cols){
    #Compute the difference for highest 5th percentile, lowest 5th percentile and all other flows, as defined by the days from the observed streamflow record
    diff = BasinSF[i+(1+cols)*(t-1)+1,-1] - BasinSF[i+(1+cols)*(t-1),-1]  
  }
}

#Get all replicates for SA using a metric of choice----

#sensitivity::pcc()
#sensitivity::plot3d.morris()
#?sensitivity::morrisMultOut()
#?sensitivity::morris()

#Show SA metrics for the basin and for each hillslope - ranks, and maps for hillslope----