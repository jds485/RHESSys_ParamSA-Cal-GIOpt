#Script to check the synthetic residuals distribution

#Load libraries----
library(fGarch)

#Set random seed and sample size info----
set.seed(487)
reps = 1000

#Load SEP code----
setwd("C:\\Users\\js4yd\\OneDrive - University of Virginia\\Code\\GenLikelihood_Zach")
source('GL_maineqs.R')

#Load observed streamflow and TN at basin outlet----
obsQ = read.table("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\obs\\BaismanStreamflow_Feb2020Revised_Cal_p.txt", stringsAsFactors = FALSE, sep = '\t', header=TRUE)
obsTN = read.table("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\RHESSysFilePreparation\\obs\\TN_Feb2020Revised_Cal_p.txt", stringsAsFactors = FALSE, sep = '\t', header=TRUE)

#Load simulated streamflow and TN----
#Parameter set used for GI allocation study = most likely from 700 chain steps
SimQ = read.table("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation\\Q.txt", stringsAsFactors = FALSE, sep = '\t', header = TRUE)
SimTN = read.table("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation\\TN.txt", stringsAsFactors = FALSE, sep = '\t', header = TRUE)

#Box-Cox Transform Streamflow (TN didn't have a transform)----
#Lambda from Python estimated value
Qlambda = 0.03891046118748475
SimQ$streamflow_BC = ((SimQ$streamflow+0.001)**Qlambda-1)/Qlambda

#Add BC transform to observed data
obsQ$Flow_BC = ((obsQ$Flow+0.001)**Qlambda-1)/Qlambda

#Load likelihood function parameter values----
LQ = read.csv('C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation\\Params_logLQ_Run192_Ch9.csv', stringsAsFactors = FALSE)
LTN = read.csv('C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\Optimization\\GIAllocation\\Params_logLTN_Run192_Ch9.csv', stringsAsFactors = FALSE)

#BC transform with corrected bias in likelihood
LQ$beta = 2.922088
LQ$xi = 1.028359
LQ$phi_1 = 0.993485
LQ$sigma_0 = 0.111253
LQ$sigma_1 = 0.285754
LQ$mu_h = 0

#No BC transform with corrected bias in likelihood
#LQ$beta = 4.447389014760954
#LQ$xi = 0.9983325601584668
#LQ$phi_1 = 0.9714746732012267
#LQ$sigma_0 = 1.0003614566639278e-09
#LQ$sigma_1 = 0.29064531446053377
#LQ$mu_h = 0.02240242623251768

#Compute residuals----
Qresid = TNresid = Qflow = TNflow = matrix(0, nrow = nrow(obsQ), ncol = reps)
for (i in 1:reps){
  q = et_syn_MaxSigt(sim_inflow = SimQ$streamflow_BC, 
             sigma_0 = LQ$sigma_0, sigma_1 = LQ$sigma_1, phi_1 = LQ$phi_1, 
             beta = LQ$beta, xi = LQ$xi, mu_h = LQ$mu_h)
  Qresid[,i] = q[[1]]
  Qflow[,i] = q[[2]]
  
  t = et_syn(sim_inflow = SimTN$TN, 
             sigma_0 = LTN$sigma_0, sigma_1 = LTN$sigma_1, phi_1 = LTN$phi_1, 
             beta = LTN$beta, xi = LTN$xi, mu_h = LTN$mu_h)
  TNresid[,i] = t[[1]]
  TNflow[,i] = t[[2]]
}
rm(i, q, t)

#Diagnostics on Residuals----
#empirical
raw_resid<-obsQ$Flow_BC - E_t_noAbs(SimQ$streamflow_BC,mu_t(SimQ$streamflow_BC,LQ$mu_h))
at_resid<-a_t(e_t = raw_resid, phi_i = LQ$phi_1, sigma_t = apply(X = t(t(sigma_t(sigma_0 = LQ$sigma_0, sigma_1 = LQ$sigma_1, 
                                                                                 E_t = E_t_noAbs(sim_inflow = SimQ$streamflow_BC, 
                                                                                                 mu_t = mu_t(SimQ$streamflow_BC, mu_h = LQ$mu_h))))), MARGIN = 1, FUN = max, LQ$sigma_0))
#synthetic
syn_resid<-a_t(e_t = Qresid, phi_i = LQ$phi_1, sigma_t = apply(X = t(t(sigma_t(sigma_0 = LQ$sigma_0, sigma_1 = LQ$sigma_1, 
                                                                               E_t = E_t_noAbs(sim_inflow = SimQ$streamflow_BC, 
                                                                                               mu_t = mu_t(SimQ$streamflow_BC, mu_h = LQ$mu_h))))), MARGIN = 1, FUN = max, LQ$sigma_0))

#plot empirical vs synthetic
par(mfrow=c(1,2))
sged_hist(at_resid,LQ$sigma_0,LQ$sigma_1,LQ$phi_1,LQ$beta,LQ$xi,LQ$mu_h)
lines(seq(-10,10,.01), dsged(seq(-10,10,.01), mean=0, sd=1, nu=(2/(1+LQ$beta)), xi=LQ$xi), col='red',lwd=2)

sged_hist(syn_resid,LQ$sigma_0,LQ$sigma_1,LQ$phi_1,LQ$beta,LQ$xi,LQ$mu_h)
lines(seq(-10,10,.01), dsged(seq(-10,10,.01), mean=0, sd=1, nu=(2/(1+LQ$beta)), xi=LQ$xi), col='red',lwd=2)

#CDF plots
x<-seq(-15,15,.1)
y<-(1:length(at_resid))/(length(at_resid)+1)
nep_th<-psged(x, mean=0, sd=1, nu = (2/(1+LQ$beta)), xi = LQ$xi)

plot(sort(at_resid),y,main='CDF Fit',
     col='black',xlab='Error',ylab='NEP',xlim=c(-10,10),ylim=c(0,1),cex=0.75)
lines(x,nep_th,type='l',col='red',lwd=2)
legend('bottomright',c('Obs','Fitted'),col=c('black','red'),pch=c(1,1))

#Q-Q plots
nep_at<-psged(at_resid,mean=0,sd=1,nu = (2/(1+LQ$beta)), xi = LQ$xi)

plot(sort(nep_at),y,main='Q-Q Plot',
     col='red',xlab='SGED Quantile',ylab='Empirical Quantile',xlim=c(0,1),ylim=c(0,1), cex=0.75)
abline(0,1,lwd=2)

#Check that synthetic generation is perfectly straight line
nep_syn<-psged(syn_resid[,1],mean=0,sd=1,nu = (2/(1+LQ$beta)), xi = LQ$xi)

plot(sort(nep_syn),y,main='Q-Q Plot', 
     col='red',xlab='SGED Quantile',ylab='Empirical Quantile',xlim=c(0,1),ylim=c(0,1), cex=0.75)
abline(0,1,lwd=2)

#Reproduce the original timeseries through transformation function----
raw_resid<-obsQ$Flow_BC - E_t_noAbs(SimQ$streamflow_BC,mu_t(SimQ$streamflow_BC,LQ$mu_h))
at_resid<-a_t(e_t = raw_resid, phi_i = LQ$phi_1, sigma_t = apply(X = t(t(sigma_t(sigma_0 = LQ$sigma_0, sigma_1 = LQ$sigma_1, 
                                                                                 E_t = E_t_noAbs(sim_inflow = SimQ$streamflow_BC, 
                                                                                                 mu_t = mu_t(SimQ$streamflow_BC, mu_h = LQ$mu_h))))), MARGIN = 1, FUN = max, LQ$sigma_0))
e_t<-vector('numeric', length = length(obsQ$Flow_BC))
E_t1<-E_t_noAbs(SimQ$streamflow_BC[1],mu_t(SimQ$streamflow_BC[1],LQ$mu_h))
sigma_t1<-max(LQ$sigma_0, sigma_t(LQ$sigma_0,LQ$sigma_1,E_t1))
e_t[1]<-sigma_t1*at_resid[1] #assume initial error of zero

#recorrelate and re-heteroscedasticize residuals
for(i in 2:length(obsQ$Flow_BC)){
  Et<-E_t_noAbs(SimQ$streamflow_BC[i],mu_t(SimQ$streamflow_BC[i],LQ$mu_h))
  e_t[i]<-e_t[i-1]*LQ$phi_1 + max(LQ$sigma_0, sigma_t(LQ$sigma_0,LQ$sigma_1,Et))*at_resid[i]
}
rm(i, Et)
Y_t<-E_t_noAbs(SimQ$streamflow_BC,mu_t(SimQ$streamflow_BC,LQ$mu_h)) + e_t

length(which(abs(Y_t - obsQ$Flow_BC) > 0.0001)) 
#0

#Inverse Box-Cox transform flow----
Qflow = (Qflow*Qlambda + 1)^(1/Qlambda) - 0.001
#All NaN are those flows that are essentially 0
Qflow[is.nan(Qflow)] = 0.001
Qflow[Qflow<0] = 0.001

#Quantile timeseries
Q05 = apply(X = Qflow, MARGIN = 1, FUN = quantile, probs = 0.05)
Q25 = apply(X = Qflow, MARGIN = 1, FUN = quantile, probs = 0.25)
Q50 = apply(X = Qflow, MARGIN = 1, FUN = quantile, probs = 0.50)
Q75 = apply(X = Qflow, MARGIN = 1, FUN = quantile, probs = 0.75)
Q95 = apply(X = Qflow, MARGIN = 1, FUN = quantile, probs = 0.95)
Qm = apply(X = Qflow, MARGIN = 1, FUN = mean)

TN05 = apply(X = TNflow, MARGIN = 1, FUN = quantile, probs = 0.05)
TN95 = apply(X = TNflow, MARGIN = 1, FUN = quantile, probs = 0.95)

#Plot
plot(x = as.Date(obsQ$Date), y = obsQ$Flow, col = 'black', type = 'l', lty = 1, ylim = c(0.001,1E30), log='y')
par(new = T)
plot(x = as.Date(obsQ$Date), y = SimQ$streamflow, col = 'blue', type = 'l', lty = 1, ylim = c(0.001,1E30), log='y')
par(new = T)
plot(x = as.Date(SimQ$Date), Q05, col = 'green', type = 'l', lty = 2, ylim = c(0.001,1E30), log='y')
par(new = T)
plot(x = as.Date(SimQ$Date), Q95, col = 'red', type = 'l', lty = 2, ylim = c(0.001,1E30), log='y')

#Not log scale
plot(x = as.Date(obsQ$Date), y = obsQ$Flow, col = 'black', type = 'l', lty = 1, ylim = c(0.001,300))
par(new = T)
plot(x = as.Date(obsQ$Date), y = SimQ$streamflow, col = 'blue', type = 'l', lty = 1, ylim = c(0.001,300))
par(new = T)
plot(x = as.Date(SimQ$Date), Q05, col = 'green', type = 'l', lty = 2, ylim = c(0.001,300))
par(new = T)
plot(x = as.Date(SimQ$Date), Q95, col = 'red', type = 'l', lty = 2, ylim = c(0.001,300))

#2007 Not log scale
plot(x = as.Date(obsQ$Date)[which((as.Date(obsQ$Date) >= as.Date('2007-01-01')) & (as.Date(obsQ$Date) < as.Date('2008-01-01')))], y = obsQ$Flow[which((as.Date(obsQ$Date) >= as.Date('2007-01-01')) & (as.Date(obsQ$Date) < as.Date('2008-01-01')))], col = 'black', type = 'l', lty = 1, ylim = c(0.001,50))
par(new = T)
plot(x = as.Date(obsQ$Date)[which((as.Date(obsQ$Date) >= as.Date('2007-01-01')) & (as.Date(obsQ$Date) < as.Date('2008-01-01')))], y = SimQ$streamflow[which((as.Date(obsQ$Date) >= as.Date('2007-01-01')) & (as.Date(obsQ$Date) < as.Date('2008-01-01')))], col = 'blue', type = 'l', lty = 1, ylim = c(0.001,50))
par(new = T)
plot(x = as.Date(SimQ$Date)[which((as.Date(obsQ$Date) >= as.Date('2007-01-01')) & (as.Date(obsQ$Date) < as.Date('2008-01-01')))], Q50[which((as.Date(obsQ$Date) >= as.Date('2007-01-01')) & (as.Date(obsQ$Date) < as.Date('2008-01-01')))], col = 'green', type = 'l', lty = 2, ylim = c(0.001,50))
par(new = T)
plot(x = as.Date(SimQ$Date)[which((as.Date(obsQ$Date) >= as.Date('2007-01-01')) & (as.Date(obsQ$Date) < as.Date('2008-01-01')))], Q05[which((as.Date(obsQ$Date) >= as.Date('2007-01-01')) & (as.Date(obsQ$Date) < as.Date('2008-01-01')))], col = 'green', type = 'l', lty = 2, ylim = c(0.001,50))
par(new = T)
plot(x = as.Date(SimQ$Date)[which((as.Date(obsQ$Date) >= as.Date('2007-01-01')) & (as.Date(obsQ$Date) < as.Date('2008-01-01')))], Q95[which((as.Date(obsQ$Date) >= as.Date('2007-01-01')) & (as.Date(obsQ$Date) < as.Date('2008-01-01')))], col = 'red', type = 'l', lty = 2, ylim = c(0.001,50))

#Compare number of observations within %-level prediction intervals
length(which(obsQ$Flow < Q95))/length(obsQ$Flow)
length(which(obsQ$Flow < Q75))/length(obsQ$Flow)
length(which(obsQ$Flow < Q50))/length(obsQ$Flow)
length(which(obsQ$Flow < Q25))/length(obsQ$Flow)
length(which(obsQ$Flow < Q05))/length(obsQ$Flow)

#Test R Optimization----
lb<-c(0.0000000001,0,0,-0.99,0.001,0) #lower bounds for sig0, sig1, phi1, beta, xi, muh; -1 for beta gives an error
ub<-c(1,1,1,7,5,1) #upper bounds for sig0, sig1, phi1, beta, xi, muh
st<-c(.5,.5,.5,1,1,0) #starting parameters for sig0, sig1, phi1, beta, xi, muh

gl_mle_BC<-optim(par=st,fn=GL_fun,inflow=obsQ$Flow_BC,sim_inflow=SimQ$streamflow_BC,
              method = 'L-BFGS-B',lower = lb, upper = ub,
              control = list(fnscale=-1,maxit=10000))

gl_mle<-optim(par=st,fn=GL_fun,inflow=obsQ$Flow,sim_inflow=SimQ$streamflow,
              method = 'L-BFGS-B',lower = lb, upper = ub,
              control = list(fnscale=-1,maxit=10000))


#Test using BC Transform to return known parameter values----
TestQ = et_syn_MaxSigt(sim_inflow = obsQ$Flow_BC, 
                   sigma_0 = 0.1, sigma_1 = 0.05, phi_1 = 0.97, 
                   beta = 3, xi = 1.05, mu_h = 0.01)
TestQ_BC = TestQ[[2]]
TestQ = (TestQ_BC*Qlambda + 1)^(1/Qlambda) - 0.001

gl_mle_BC<-optim(par=st,fn=GL_fun_MaxSigt,inflow=TestQ_BC,sim_inflow=obsQ$Flow_BC,
                 method = 'L-BFGS-B',lower = lb, upper = ub,
                 control = list(fnscale=-1,maxit=100000))

#Make sure results match after back transforming (they do)
raw_resid<-TestQ_BC - E_t_noAbs(obsQ$Flow_BC,mu_t(obsQ$Flow_BC,0.01))
at_resid<-a_t(e_t = raw_resid, phi_i = 0.97, sigma_t = apply(X = t(t(sigma_t(sigma_0 = 0.1, sigma_1 = 0.05, 
                                                                   E_t = E_t_noAbs(sim_inflow = obsQ$Flow_BC, 
                                                                   mu_t = mu_t(obsQ$Flow_BC, mu_h = 0.01))))), MARGIN = 1, FUN = max, 0.1))
e_t<-vector('numeric', length = length(obsQ$Flow_BC))
E_t1<-E_t_noAbs(obsQ$Flow_BC[1],mu_t(obsQ$Flow_BC[1],0.01))
sigma_t1<-max(0.1,sigma_t(0.1,0.05,E_t1))
e_t[1]<-sigma_t1*at_resid[1] #assume initial error of zero

#recorrelate and re-heteroscedasticize residuals
for(i in 2:length(obsQ$Flow_BC)){
  Et<-E_t_noAbs(obsQ$Flow_BC[i],mu_t(obsQ$Flow_BC[i],0.01))
  e_t[i]<-e_t[i-1]*0.97 + max(0.1,sigma_t(0.1,0.05,Et))*at_resid[i]
}
rm(i, Et)
Y_t<-E_t_noAbs(obsQ$Flow_BC,mu_t(obsQ$Flow_BC,0.01)) + e_t
length(which(abs(Y_t - TestQ_BC) > 0.0001)) 

Y_t = (Y_t*Qlambda + 1)^(1/Qlambda) - 0.001

Qresid = Qflow = matrix(0, nrow = nrow(obsQ), ncol = reps)
for (i in 1:reps){
  q = et_syn_MaxSigt(sim_inflow = obsQ$Flow_BC, 
             sigma_0 = 0.1, sigma_1 = 0.05, phi_1 = 0.97, 
             beta = 3, xi = 1.05, mu_h = 0.01)
  Qresid[,i] = q[[1]]
  Qflow[,i] = q[[2]]
}
rm(i, q)
Qflow = (Qflow*Qlambda + 1)^(1/Qlambda) - 0.001

Q05 = apply(X = Qflow, MARGIN = 1, FUN = quantile, probs = 0.05)
Q25 = apply(X = Qflow, MARGIN = 1, FUN = quantile, probs = 0.25)
Q50 = apply(X = Qflow, MARGIN = 1, FUN = quantile, probs = 0.50)
Q75 = apply(X = Qflow, MARGIN = 1, FUN = quantile, probs = 0.75)
Q95 = apply(X = Qflow, MARGIN = 1, FUN = quantile, probs = 0.95)

length(which(TestQ_BC < Q95))/length(obsQ$Flow)
length(which(TestQ_BC < Q75))/length(obsQ$Flow)
length(which(TestQ_BC < Q50))/length(obsQ$Flow)
length(which(TestQ_BC < Q25))/length(obsQ$Flow)
length(which(TestQ_BC < Q05))/length(obsQ$Flow)

length(which(TestQ < Q95))/length(obsQ$Flow)
length(which(TestQ < Q75))/length(obsQ$Flow)
length(which(TestQ < Q50))/length(obsQ$Flow)
length(which(TestQ < Q25))/length(obsQ$Flow)
length(which(TestQ < Q05))/length(obsQ$Flow)

test = (qsep(inflow = TestQ_BC, sim_inflow = obsQ$Flow_BC, sigma_0 = 0.1, sigma_1 = 0.05, phi_1 = 0.97, beta = 3, xi = 1.05, mu_h = 0.01, p = 0.95)[[2]]*Qlambda + 1)^(1/Qlambda) - 0.001

#Make synthetic error timeseries for hillslope 910----
library(vroom)

SynObs = vroom("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\SyntheticDataCreation\\Run_basin.daily", delim = ' ', col_names = TRUE, col_types = cols(.default=col_double()), progress = FALSE)

#Make a new Date column
SynObs$Date = as.Date(paste0(SynObs$year, '-', SynObs$month, '-', SynObs$day))

#Trim off spin-up years
SynObs = SynObs[which(as.Date(SynObs$Date) >= as.Date('2004-10-01')),]

#Compute streamflow conversion factor----
world = read.csv("C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\worldfile.csv", stringsAsFactors = FALSE)
res = 30
#Taking the unique patch IDs because strata can exist in more than one patch.
Area.basin = length(unique(world$patchID))*res^2
#Multiplier conversion for basin streamflow (mm/d)*conversion_b -> cfs
conversion_b = Area.basin/1000/(.3048^3)/24/3600

#Convert simulated streamflow to cfs units
SynObs$streamflow = round(SynObs$streamflow*conversion_b, 3)

#Retain only streamflow
SynObs = as.data.frame(SynObs[,c('Date', 'streamflow')])

#Make a weekly timeseries
wSynObs = vector('numeric', (length(unique(format(SynObs$Date, '%Y')))-1)*52)
w = 0
for (i in 1:length(SynObs$streamflow)){
  if ((i %% 7) == 0){
    #Sum the last 6 days + this day
    wSynObs[w] = sum(SynObs$streamflow[(i-6):i])
    w = w + 1
  }
}

#Daily synthetic data----
TestMin = vector('numeric', length = 1000)
for (i in 1:1000){
  set.seed(i)
  TestQ = et_syn_MaxSigt(sim_inflow = SynObs$streamflow, 
                         sigma_0 = 0.05, sigma_1 = 0, phi_1 = 0.7, 
                         beta = 0, xi = 1, mu_h = 0)
  TestQ = TestQ[[2]]
  TestMin[i] = min(TestQ)
}

set.seed(157)
TestQ = et_syn_MaxSigt(sim_inflow = SynObs$streamflow, 
                       sigma_0 = 0.05, sigma_1 = 0, phi_1 = 0.7, 
                       beta = 0, xi = 1, mu_h = 0)
TestQ = round(TestQ[[2]], 3)

lb<-c(0.000001,0,0,-0.99,0.01,0) #lower bounds for sig0, sig1, phi1, beta, xi, muh; -1 for beta gives an error
ub<-c(1,1,1,7,5,1) #upper bounds for sig0, sig1, phi1, beta, xi, muh
st<-c(.5,.5,.5,1,1,0) #starting parameters for sig0, sig1, phi1, beta, xi, muh
gl_mle_Syn<-optim(par=st,fn=GL_fun_MaxSigt,inflow=TestQ,sim_inflow=SynObs$streamflow,
                 method = 'L-BFGS-B',lower = lb, upper = ub,
                 control = list(fnscale=-1,maxit=100000))

#Make sure results match after back transforming (they do)
raw_resid<-TestQ - E_t_noAbs(SynObs$streamflow,mu_t(SynObs$streamflow,0))
at_resid<-a_t(e_t = raw_resid, phi_i = 0.7, sigma_t = apply(X = t(t(sigma_t(sigma_0 = 0.05, sigma_1 = 0, 
                                                                             E_t = E_t_noAbs(sim_inflow = SynObs$streamflow, 
                                                                                             mu_t = mu_t(SynObs$streamflow, mu_h = 0))))), MARGIN = 1, FUN = max, 0.05))
e_t<-vector('numeric', length = length(SynObs$streamflow))
E_t1<-E_t_noAbs(SynObs$streamflow[1],mu_t(SynObs$streamflow[1],0))
sigma_t1<-max(0.05,sigma_t(0.05,0,E_t1))
e_t[1]<-sigma_t1*at_resid[1] #assume initial error of zero

#recorrelate and re-heteroscedasticize residuals
for(i in 2:length(SynObs$streamflow)){
  Et<-E_t_noAbs(SynObs$streamflow[i],mu_t(SynObs$streamflow[i],0))
  e_t[i]<-e_t[i-1]*0.7 + max(0.05,sigma_t(0.05,0,Et))*at_resid[i]
}
rm(i, Et)
Y_t<-E_t_noAbs(SynObs$streamflow,mu_t(SynObs$streamflow,0)) + e_t
length(which(abs(Y_t - TestQ) > 0.0001)) 

Qresid = Qflow = matrix(0, nrow = nrow(SynObs), ncol = reps)
for (i in 1:reps){
  q = et_syn_MaxSigt(sim_inflow = SynObs$streamflow, 
                     sigma_0 = 0.05, sigma_1 = 0, phi_1 = 0.7, 
                     beta = 0, xi = 1, mu_h = 0)
  Qresid[,i] = q[[1]]
  Qflow[,i] = q[[2]]
}
rm(i, q)

#Check that quantiles are good match (they are)
Q05 = apply(X = Qflow, MARGIN = 1, FUN = quantile, probs = 0.05)
Q25 = apply(X = Qflow, MARGIN = 1, FUN = quantile, probs = 0.25)
Q50 = apply(X = Qflow, MARGIN = 1, FUN = quantile, probs = 0.50)
Q75 = apply(X = Qflow, MARGIN = 1, FUN = quantile, probs = 0.75)
Q95 = apply(X = Qflow, MARGIN = 1, FUN = quantile, probs = 0.95)

length(which(TestQ < Q95))/length(SynObs$streamflow)
length(which(TestQ < Q75))/length(SynObs$streamflow)
length(which(TestQ < Q50))/length(SynObs$streamflow)
length(which(TestQ < Q25))/length(SynObs$streamflow)
length(which(TestQ < Q05))/length(SynObs$streamflow)

#Write the synthetic timeseries
TestQ = data.frame(Date = as.Date(SynObs$Date), Flow = TestQ, stringsAsFactors = FALSE)
options(scipen = 999)
write.table(TestQ, file = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\SyntheticDataCreation\\QSyn910_1.txt", row.names = FALSE, col.names = TRUE, sep = '\t')
write.table(SynObs, file = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\SyntheticDataCreation\\Q_NoErr_1.txt", row.names = FALSE, col.names = TRUE, sep = '\t')
options(scipen = 0)

#Weekly Synthetic Data----
wTestMin = vector('numeric', length = 1000)
for (i in 1:1000){
  set.seed(i)
  wTestQ = et_syn_MaxSigt(sim_inflow = wSynObs, 
                         sigma_0 = 0.1, sigma_1 = 0, phi_1 = 0.7, 
                         beta = 0, xi = 1, mu_h = 0)
  wTestQ = wTestQ[[2]]
  wTestMin[i] = min(wTestQ)
}

set.seed(157)
wTestQ = et_syn_MaxSigt(sim_inflow = wSynObs, 
                       sigma_0 = 0.1, sigma_1 = 0, phi_1 = 0.7, 
                       beta = 0, xi = 1, mu_h = 0)
wTestQ = round(wTestQ[[2]], 3)

lb<-c(0.000001,0,0,-0.99,0.01,0) #lower bounds for sig0, sig1, phi1, beta, xi, muh; -1 for beta gives an error
ub<-c(1,1,1,7,5,1) #upper bounds for sig0, sig1, phi1, beta, xi, muh
st<-c(.5,.5,.5,1,1,0) #starting parameters for sig0, sig1, phi1, beta, xi, muh
gl_mle_Syn<-optim(par=st,fn=GL_fun_MaxSigt,inflow=wTestQ,sim_inflow=wSynObs,
                  method = 'L-BFGS-B',lower = lb, upper = ub,
                  control = list(fnscale=-1,maxit=100000))

#Make sure results match after back transforming (they do)
raw_resid<-wTestQ - E_t_noAbs(wSynObs,mu_t(wSynObs,0))
at_resid<-a_t(e_t = raw_resid, phi_i = 0.7, sigma_t = apply(X = t(t(sigma_t(sigma_0 = 0.1, sigma_1 = 0, 
                                                                            E_t = E_t_noAbs(sim_inflow = wSynObs, 
                                                                                            mu_t = mu_t(wSynObs, mu_h = 0))))), MARGIN = 1, FUN = max, 0.1))
e_t<-vector('numeric', length = length(wSynObs))
E_t1<-E_t_noAbs(wSynObs[1],mu_t(wSynObs[1],0))
sigma_t1<-max(0.1,sigma_t(0.1,0,E_t1))
e_t[1]<-sigma_t1*at_resid[1] #assume initial error of zero

#recorrelate and re-heteroscedasticize residuals
for(i in 2:length(wSynObs)){
  Et<-E_t_noAbs(wSynObs[i],mu_t(wSynObs[i],0))
  e_t[i]<-e_t[i-1]*0.7 + max(0.1,sigma_t(0.1,0,Et))*at_resid[i]
}
rm(i, Et)
Y_t<-E_t_noAbs(wSynObs,mu_t(wSynObs,0)) + e_t
length(which(abs(Y_t - wTestQ) > 0.0001)) 

wQresid = wQflow = matrix(0, nrow = length(wSynObs), ncol = reps)
for (i in 1:reps){
  q = et_syn_MaxSigt(sim_inflow = wSynObs, 
                     sigma_0 = 0.1, sigma_1 = 0, phi_1 = 0.7, 
                     beta = 0, xi = 1, mu_h = 0)
  wQresid[,i] = q[[1]]
  wQflow[,i] = q[[2]]
}
rm(i, q)

#Check that quantiles are good match (they are)
wQ05 = apply(X = wQflow, MARGIN = 1, FUN = quantile, probs = 0.05)
wQ25 = apply(X = wQflow, MARGIN = 1, FUN = quantile, probs = 0.25)
wQ50 = apply(X = wQflow, MARGIN = 1, FUN = quantile, probs = 0.50)
wQ75 = apply(X = wQflow, MARGIN = 1, FUN = quantile, probs = 0.75)
wQ95 = apply(X = wQflow, MARGIN = 1, FUN = quantile, probs = 0.95)

length(which(wTestQ < wQ95))/length(wSynObs)
length(which(wTestQ < wQ75))/length(wSynObs)
length(which(wTestQ < wQ50))/length(wSynObs)
length(which(wTestQ < wQ25))/length(wSynObs)
length(which(wTestQ < wQ05))/length(wSynObs)

#Write the synthetic timeseries
options(scipen = 999)
write.table(wTestQ, file = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\SyntheticDataCreation\\wQSyn910.txt", row.names = FALSE, col.names = FALSE, sep = '\t')
write.table(wSynObs, file = "C:\\Users\\js4yd\\OneDrive - University of Virginia\\BES_Data\\BES_Data\\RHESSysFiles\\BR&POBR\\SyntheticHill11+12\\SyntheticDataCreation\\wQ_NoErr.txt", row.names = FALSE, col.names = FALSE, sep = '\t')
options(scipen = 0)
