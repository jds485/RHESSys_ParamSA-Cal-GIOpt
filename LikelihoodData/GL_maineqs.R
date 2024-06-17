#Main equations for Generalized Likelihood Function

source('/nv/vol288/quinnlab-value/js4yd/Bais910Hill30mOptGI_MORO/LikelihoodFun/GL_subeqs.R') #bring in subequations in terms of parameters below

#1) Define Generalized Likelihood function per Schoups and Vrugt (2010) without scaling factor (mu_h)
#pars in order: 
#par[1]: sigma_0, intercept for linearly scaled SD (heteroscedastic)
#par[2]: sigma_1, linear coefficient for scale SD
#par[3]: phi_1, autocorrelation coefficient for AR1 model
#par[4]: beta, kurtosis parameter (-1,1) for normalized SEP
#par[5]: xi, skewness parameter (0.1,10) for normalized SEP 

GL_fun_noscale<-function(pars,inflow,sim_inflow){
  n<-length(inflow)
  e_t<-inflow-sim_inflow
  sig_xi<-sigma_xi(M1(pars[4]),M2,pars[5]) #Eq A8 from subequations
  om_b<-omega_beta(pars[4]) #Eq A2 from subequations
  sig_t<-sigma_t(pars[1],pars[2],sim_inflow) #Eq 5 from subequations
  cb<-c_beta(pars[4]) #Eq A3 from subequations
  
  #Eq 6, derived residuals a_xt as a function of observed residuals modified by subequations
  # Drop the first term because it's lag 1 and first term has no lag
  a_xt<-a_xi_t(pars[5],mu_xi(M1(pars[4]),pars[5]),sig_xi,a_t(e_t,pars[3],sig_t))[-1]
  
  #Eq 8, Generalized Log Likelihood Function
  gl_ll<-(n-1)*log((2*sig_xi*om_b)/(pars[5] + pars[5]^-1)) - sum(log(sig_t[-1])) - cb * sum(abs(a_xt)^(2/(1+pars[4])))
  
  return(gl_ll) 
}

#2) This version includes scaling factor
#par[6]: mu_h, exponential scaling parameter based on simulated flow, set to zero for no-scaling
#for this version of function, but retained for more general version

GL_fun<-function(pars,inflow,sim_inflow){
  n<-length(inflow)
  Et<-E_t_noAbs(sim_inflow,mu_t(sim_inflow,pars[6]))
  e_t<-inflow-Et
  sig_xi<-sigma_xi(M1(pars[4]),M2,pars[5]) #Eq A8 from subequations
  om_b<-omega_beta(pars[4]) #Eq A2 from subequations
  sig_t<-sigma_t(pars[1],pars[2],E_t(sim_inflow,mu_t(sim_inflow,pars[6]))) #Eq 5 from subequations
  cb<-c_beta(pars[4]) #Eq A3 from subequations
  
  #Eq 6, derived residuals a_xt as a function of observed residuals modified by subequations
  # Drop the first term because it's lag 1 and first term has no lag
  a_xt<-a_xi_t(pars[5],mu_xi(M1(pars[4]),pars[5]),sig_xi,a_t(e_t,pars[3],sig_t))[-1]
  
  #Eq 8, Generalized Log Likelihood Function
  gl_ll<-(n-1)*log((2*sig_xi*om_b)/(pars[5] + pars[5]^-1)) - sum(log(sig_t[-1])) - cb * sum(abs(a_xt)^(2/(1+pars[4])))

  return(gl_ll) 
}

GL_fun_MaxSigt<-function(pars,inflow,sim_inflow){
  n<-length(inflow)
  Et<-E_t_noAbs(sim_inflow,mu_t(sim_inflow,pars[6]))
  e_t<-inflow-Et
  sig_xi<-sigma_xi(M1(pars[4]),M2,pars[5]) #Eq A8 from subequations
  om_b<-omega_beta(pars[4]) #Eq A2 from subequations
  sig_t<-apply(X = t(t(sigma_t(pars[1],pars[2],E_t_noAbs(sim_inflow,mu_t(sim_inflow,pars[6]))))), MARGIN = 1, FUN = max, pars[1]) #Eq 5 from subequations
  cb<-c_beta(pars[4]) #Eq A3 from subequations
  
  #Eq 6, derived residuals a_xt as a function of observed residuals modified by subequations
  # Drop the first term because it's lag 1 and first term has no lag
  a_xt<-a_xi_t(pars[5],mu_xi(M1(pars[4]),pars[5]),sig_xi,a_t(e_t,pars[3],sig_t))[-1]
  
  #Eq 8, Generalized Log Likelihood Function
  gl_ll<-(n-1)*log((2*sig_xi*om_b)/(pars[5] + pars[5]^-1)) - sum(log(sig_t[-1])) - cb * sum(abs(a_xt)^(2/(1+pars[4])))
  
  return(gl_ll) 
}

#3) Function to generate residuals e_t and synthetic flow Y_t given calculated parameters
et_syn<-function(sim_inflow,sigma_0,sigma_1,phi_1,beta,xi,mu_h){
  at<-rsged(length(sim_inflow),mean=0,sd=1,nu=(2/(1+beta)),xi=xi)
  e_t<-c()
  E_t1<-E_t(sim_inflow[1],mu_t(sim_inflow[1],mu_h))
  sigma_t1<-sigma_t(sigma_0,sigma_1,E_t1)
  e_t[1]<-sigma_t1*at[1] #assume initial error of zero
  
  #recorrelate and re-heteroscedasticize residuals
  for(i in 2:length(sim_inflow)){
    Et<-E_t(sim_inflow[i],mu_t(sim_inflow[i],mu_h))
    e_t[i]<-e_t[i-1]*phi_1 + sigma_t(sigma_0,sigma_1,Et)*at[i]
  }
  Y_t<-E_t_noAbs(sim_inflow,mu_t(sim_inflow,mu_h)) + e_t
  #Y_t[Y_t<0]<-0
  return(list(e_t,Y_t)) #returns list with el [[1]] = residuals, el [[2]] = synthetic flow
}

et_syn_MaxSigt<-function(sim_inflow,sigma_0,sigma_1,phi_1,beta,xi,mu_h){
  at<-rsged(length(sim_inflow),mean=0,sd=1,nu=(2/(1+beta)),xi=xi)
  e_t<-c()
  E_t1<-E_t_noAbs(sim_inflow[1],mu_t(sim_inflow[1],mu_h))
  sigma_t1<-max(sigma_0, sigma_t(sigma_0,sigma_1,E_t1))
  e_t[1]<-sigma_t1*at[1] #assume initial error of zero
  
  #recorrelate and re-heteroscedasticize residuals
  for(i in 2:length(sim_inflow)){
    Et<-E_t_noAbs(sim_inflow[i],mu_t(sim_inflow[i],mu_h))
    e_t[i]<-e_t[i-1]*phi_1 + max(sigma_0,sigma_t(sigma_0,sigma_1,Et))*at[i]
  }
  Y_t<-E_t_noAbs(sim_inflow,mu_t(sim_inflow,mu_h)) + e_t
  #Y_t[Y_t<0]<-0
  return(list(e_t,Y_t)) #returns list with el [[1]] = residuals, el [[2]] = synthetic flow
}

#4) define SEP density as function of calculated xi and beta and random variable axt (a_xi_t)
##this is manually defined version that is functionally the same as 'dsged(...)'
SEP_dens<-function(xi,beta,axt){
  SEP_dens<-((2*sigma_xi(M1(beta),M2,xi))/(xi + xi^(-1))*omega_beta(beta)*
    exp(-c_beta(beta)*abs(a_xi_t(xi,mu_xi(M1(beta),xi),sigma_xi(M1(beta),M2,xi),axt))^(2/(1+beta))))
}

#5) Histogram plotting function for residuals

sged_hist<-function(a_t,sig_0,sig_1,phi_1,beta,xi,mu_h){
  sig0<-format(round(sig_0,3),nsmall=3)
  sig1<-format(round(sig_1,3),nsmall=3)
  phi1<-format(round(phi_1,3),nsmall=3)
  Beta<-format(round(beta,3),nsmall=3)
  Xi<-format(round(xi,3),nsmall=3)
  muh<-format(round(mu_h,3),nsmall=3)

  breaks<-c(-50,seq(-5,5,0.25),50)
  #Histogram of derived observed residuals and SEP density 
  hist(a_t,breaks=breaks,xlim = c(-5,5),ylim=c(0,.8),freq = F, main=bquote(a[t]~' Residuals'),xlab="")
  text(2.5,0.8,bquote(~sigma[0]==.(sig0)))
  text(2.5,0.725,bquote(~sigma[1]==.(sig1)))
  text(2.5,0.65,bquote(~phi[1]==.(phi1)))
  text(2.5,0.575,bquote(~beta==.(Beta)))
  text(2.5,0.5,bquote(~xi==.(Xi)))
  text(2.5,0.425,bquote(~mu[h]==.(muh)))
}

#6) Quantile Function
qsep<-function(inflow,sim_inflow,sigma_0,sigma_1,phi_1,beta,xi,mu_h,p){
  Et<-E_t_noAbs(sim_inflow,mu_t(sim_inflow,mu_h))
  e_t<-inflow-Et
  #Specified quantile value
  atp<-qsged(p,mean=0,sd=1,nu=(2/(1+beta)),xi=xi)
  
  #recorrelate and re-heteroscedasticize residuals
  etp = vector('numeric', length(sim_inflow))
  etp[1]<-sigma_t(sigma_0,sigma_1,E_t(sim_inflow[1],mu_t(sim_inflow[1],mu_h)))*atp #assume initial error of zero
  for(i in 2:length(sim_inflow)){
    etp[i]<-e_t[i-1]*phi_1 + sigma_t(sigma_0,sigma_1,E_t(sim_inflow[i],mu_t(sim_inflow[i],mu_h)))*atp
  }
  Y_t<-Et + etp
  #Y_t[Y_t<0]<-0
  return(list(etp,Y_t)) #returns list with el [[1]] = residuals, el [[2]] = synthetic flow
}

qsep_MaxSigt<-function(inflow,sim_inflow,sigma_0,sigma_1,phi_1,beta,xi,mu_h,p){
  Et<-E_t_noAbs(sim_inflow,mu_t(sim_inflow,mu_h))
  e_t<-inflow-Et
  #Specified quantile value
  atp<-qsged(p,mean=0,sd=1,nu=(2/(1+beta)),xi=xi)
  
  #recorrelate and re-heteroscedasticize residuals
  etp = vector('numeric', length(sim_inflow))
  etp[1]<-max(sigma_0, sigma_t(sigma_0,sigma_1,E_t(sim_inflow[1],mu_t(sim_inflow[1],mu_h))))*atp #assume initial error of zero
  for(i in 2:length(sim_inflow)){
    etp[i]<-e_t[i-1]*phi_1 + max(sigma_0, sigma_t(sigma_0,sigma_1,E_t(sim_inflow[i],mu_t(sim_inflow[i],mu_h))))*atp
  }
  Y_t<-Et + etp
  #Y_t[Y_t<0]<-0
  return(list(etp,Y_t)) #returns list with el [[1]] = residuals, el [[2]] = synthetic flow
}
