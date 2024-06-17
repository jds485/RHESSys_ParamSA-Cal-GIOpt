#Sub-equations for Generalized Likelihood Method, Schoups and Vrugt (2010)

#Eq A2 for omega_beta
omega_beta<-function(beta){
  omega_beta<-(gamma(3/2 * (1+beta))^(1/2))/((1+beta)*gamma((1+beta)/2)^(3/2))
  return(omega_beta)
}

#Eq A3 for c_beta
c_beta<-function(beta){
  c_beta<-(((gamma(3/2*(1+beta)))/(gamma((1+beta)/2))))^(1/(1+beta))
  return(c_beta)
}

#Eq A8 for M1 and value for M2
M1<-function(beta){
  M1<-((gamma(1+beta))/((gamma(3/2*(1+beta))^(1/2))*(gamma((1+beta)/2))^(1/2)))
  return(M1)
}

M2<-1

#Eq A5 for mu_xi
mu_xi<-function(M1,xi){
  mu_xi<-M1*(xi - xi^(-1))
  return(mu_xi)
}

#Eq A6 for sigma_xi
sigma_xi<-function(M1,M2,xi){
  sigma_xi<-sqrt((M2-M1^2) * (xi^2 + xi^(-2)) + 2*M1^2 - M2)
  return(sigma_xi)
}

#Eq 2 for E_t
E_t<-function(sim_inflow,mu_t){
  E_t<-abs(sim_inflow)*mu_t
  return(E_t)
}

#Eq 2 for E_t, without absolute value used for prediction.
E_t_noAbs<-function(sim_inflow,mu_t){
  E_t<-sim_inflow*mu_t
  return(E_t)
}

#Eq 3 for mu_t
mu_t<-function(sim_inflow, mu_h){
  mu_t<-exp(mu_h*abs(sim_inflow))
  return(mu_t)
}

#Eq 4 (AR1) rearranged for a_t
a_t<-function(e_t, phi_i, sigma_t){
  a_t<-(e_t - (c(0,e_t[-c(length(e_t))]))*phi_i)/sigma_t
  return(a_t)
}


#Eq 5 for sigma_t
sigma_t<-function(sigma_0,sigma_1,E_t){
  sigma_t<-sigma_0 + sigma_1*E_t
  return(sigma_t)
}

#Eq 6 for a_xi.t
a_xi_t<-function(xi,mu_xi,sigma_xi,a_t){
  a_xi_t<-xi^(-sign(mu_xi+sigma_xi*a_t)) * (mu_xi + sigma_xi*a_t)
  return(a_xi_t)
}


################################END###############################################