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

#Compute residuals----
Qresid = TNresid = Qflow = TNflow = matrix(0, nrow = nrow(obsQ), ncol = reps)
for (i in 1:reps){
  q = et_syn(sim_inflow = SimQ$streamflow_BC, 
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
raw_resid<-obsQ$Flow_BC-SimQ$streamflow_BC
at_resid<-a_t(e_t = raw_resid, phi_i = LQ$phi_1, sigma_t = sigma_t(sigma_0 = LQ$sigma_0, sigma_1 = LQ$sigma_1, 
                                                                   E_t = E_t(sim_inflow = SimQ$streamflow_BC, 
                                                                             mu_t = mu_t(sim_inflow = SimQ$streamflow_BC, mu_h = LQ$mu_h))))

#synthetic
syn_resid<-a_t(e_t = Qresid, phi_i = LQ$phi_1, sigma_t = sigma_t(sigma_0 = LQ$sigma_0,sigma_1 = LQ$sigma_1,
                                                                 E_t = E_t(sim_inflow = SimQ$streamflow_BC, 
                                                                           mu_t = mu_t(sim_inflow = SimQ$streamflow_BC, mu_h = LQ$mu_h))))

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
at_resid<-a_t(e_t = raw_resid, phi_i = LQ$phi_1, sigma_t = sigma_t(sigma_0 = LQ$sigma_0, sigma_1 = LQ$sigma_1, 
                                                                   E_t = E_t(sim_inflow = SimQ$streamflow_BC, 
                                                                             mu_t = mu_t(sim_inflow = SimQ$streamflow_BC, mu_h = LQ$mu_h))))
e_t<-vector('numeric', length = length(obsQ$Flow_BC))
E_t1<-E_t(SimQ$streamflow_BC[1],mu_t(SimQ$streamflow_BC[1],LQ$mu_h))
sigma_t1<-sigma_t(LQ$sigma_0,LQ$sigma_1,E_t1)
e_t[1]<-sigma_t1*at_resid[1] #assume initial error of zero

#recorrelate and re-heteroscedasticize residuals
for(i in 2:length(obsQ$Flow_BC)){
  Et<-E_t(SimQ$streamflow_BC[i],mu_t(SimQ$streamflow_BC[i],LQ$mu_h))
  e_t[i]<-e_t[i-1]*LQ$phi_1 + sigma_t(LQ$sigma_0,LQ$sigma_1,Et)*at_resid[i]
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
plot(x = as.Date(obsQ$Date), y = obsQ$Flow, col = 'black', type = 'l', lty = 1, ylim = c(0.001,100))
par(new = T)
plot(x = as.Date(obsQ$Date), y = SimQ$streamflow, col = 'blue', type = 'l', lty = 1, ylim = c(0.001,100))
par(new = T)
plot(x = as.Date(SimQ$Date), Q05, col = 'green', type = 'l', lty = 2, ylim = c(0.001,100))
par(new = T)
plot(x = as.Date(SimQ$Date), Q95, col = 'red', type = 'l', lty = 2, ylim = c(0.001,100))

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



#Test R Optimization----
lb<-c(0.000001,0,0,-0.99,0.1,0) #lower bounds for sig0, sig1, phi1, beta, xi, muh; -1 for beta gives an error
ub<-c(1,1,1,7,5,30) #upper bounds for sig0, sig1, phi1, beta, xi, muh
st<-c(.5,.5,.5,0,1,1) #starting parameters for sig0, sig1, phi1, beta, xi, muh

gl_mle<-optim(par=st,fn=GL_fun,inflow=obsQ$Flow_BC,sim_inflow=SimQ$streamflow_BC,
              method = 'L-BFGS-B',lower = lb, upper = ub,
              control = list(fnscale=-1,maxit=10000))
