import numpy as np
import math
import warnings

# modified from https://github.com/thouska/spotpy/blob/master/spotpy/likelihoods.py
class LikelihoodError(Exception):
    """
    Define an error class to warn of likelihood errors due to wrong inputs
    """
    pass

#def __generateMeaserror(data):
#    return np.array(data) * 0.1


def __calcSimpleDeviation(data, comparedata, mu_h):
    __standartChecksBeforeStart(data, comparedata)
    d = np.array(data)
    c = np.array(comparedata)
	#Must adjust c for multiplicative bias parameter:
    mu_t = np.exp(mu_h * np.abs(c))
    #Note: No absolute value for this Et because it is the real predicted value, not the adjustment to sigma1 heteroskedasticity.
    Et = c * mu_t
    return d - Et


def __standartChecksBeforeStart(data, comparedata):
    # some standard checks
    if data.__len__() != comparedata.__len__():
        raise LikelihoodError("Simulation and observation data do not have the same length")
    if data.__len__() == 0:
        raise LikelihoodError("Data with no content can not be used as a foundation for calculating a likelihood")


# def __jitter_measerror_if_needed(fun_name, measerror):
#     size = measerror[measerror == 0.0].size
#     if size > 0:
#         warnings.warn(
#             "[" + fun_name + "] realized that there are distinct distributed values. "
#                              "We jittered the values but the result can be far away from the truth.")

#         measerror[measerror == 0.0] = np.random.uniform(0.01, 0.1, size)
#     return measerror

def generalizedLikelihoodFunction(data, comparedata, tIndex, params, month=None):
    #measerror=None: excluded; assuming no measurement error for now; added tIndex in case time step inconsistent
    """
    Under the assumption of having correlated, heteroscedastic, and non-Gaussian errors and assuming that the data are
    coming from a time series modeled as
    .. math::
            \\Phi_p(B)e_t = \\sigma_t a_t
    with `a_t` being an i.i.d. random error with zero mean and unit standard deviation, described by a skew exponential
    power (SEP) density, the likelihood `p` can be calculated as follows:
    .. math::
            p = \\frac{2\\sigma_i}{\\xi+\\xi^{-1}}\\omega_\\beta exp(-c_\\beta |a_{\\xi,t}|^{2/(1+\\beta)})
    where
     .. math::
            a_{\\xi,t} = \\xi^{-sign(\\mu_\\xi+\\sigma_\\xi a_t )}(\\mu_\\xi+\\sigma_\\xi a_t)
    For more details see: http://onlinelibrary.wiley.com/doi/10.1029/2009WR008933/epdf, page 3, formula (6) and pages 15, Appendix A.
    `Usage:` Maximizing the likelihood value guides to the best model.
    :param data: observed measurements as a numerical list
    :type data: list
    :param comparedata: simulated data from a model which should closely fit the original data
    :type comparedata: list
    :param tIndex: time index of observations
    :type tIndex: array
    :param measerror: measurement errors of every data input, if nothing is given a standart calculation is done to
        simulate measurement errors --> excluded for now
    :type measerror: list --> excluded for now
    :param params: Contains a tuple of model parameters which are needed for calculating the likelihood. The first component contains the values and the second the names of the values.
        The following parameters are needed in this function:
        -1 < beta   < 1, (kurotsis) beta=-1: uniform, beta=0: Gaussian, beta=1: double exponential
        0  < xi    <= 10, (skewness) xi=1: symmetric, xi<1: negatively skewed, xi>1: positively skewed
        0 <= sigma_0 <= 1, (standard deviation when mean=0)
        0 <= sigma_1 <= 1, (linear rate of change in standard deviation with mean)
        0 <= phi_1    < 1, (lag-1 auto-correlation), phi_1=0: no auto-correlation, phi_1=1: perfect auto-correlation
        0 <= mu_h    <= 100, (mean bias factor)
    :type params: tuple
    :return: the p value as a likelihood
    :rtype: float
    """
    
    beta = params[0]
    xi = params[1]
    sigma_0 = params[2]
    sigma_1 =params[3]
    phi_1 = params[4]
    mu_h = params[5]
    
    if xi <= 0.:
        warnings.warn("xi must be > 0")
        return np.inf
    if beta <= -1.:
        warnings.warn("beta must be > -1 as programmed.")
        return np.inf

    __standartChecksBeforeStart(data, comparedata)
    errorArr = __calcSimpleDeviation(data, comparedata, mu_h)
    
    comparedata = np.array(comparedata)
    
    '''
	Fixme: this may be incorrect now that errorArr is adjusted by mu_h bias.
    if month is not None:
        # remove seasonality in residuals
        for m in range(12):
            #Get all of the residuals for that month
            inds = np.where(month == (m+1))[0]
            Mu = np.mean(errorArr[inds])
            Sigma = np.std(errorArr[inds])
            errorArr[inds] = (errorArr[inds] - Mu) / Sigma
            #Also adjust the comparedata
            comparedata[inds] = (comparedata[inds] - Mu) / Sigma
    '''
	
    #if measerror is None:
    #    measerror = __generateMeaserror(data)
    #measerror = np.array(measerror)
    #measerror = __jitter_measerror_if_needed("generalizedLikelihoodFunction", measerror)
	
    try:
        omegaBeta = np.sqrt(math.gamma(3 * (1 + beta) / 2)) / ((1 + beta) * np.sqrt(math.gamma((1 + beta) / 2) ** 3))
        M_1 = math.gamma(1 + beta) / (np.sqrt(math.gamma(3 * (1 + beta) / 2)) * np.sqrt(math.gamma((1 + beta) / 2)))
        M_2 = 1
		#Note - absolute value is not part of the paper equation A6.
        sigma_xi = np.sqrt(np.abs(float((M_2 - M_1 ** 2) * (xi ** 2 + xi ** (-2)) + 2 * M_1 ** 2 - M_2)))
        cBeta = (math.gamma(3 * (1 + beta) / 2) / math.gamma((1 + beta) / 2)) ** (1 / (1 + beta))
    except ValueError:
        return np.inf

    if xi != 0.0:
        mu_xi = M_1 * (xi - (xi ** (-1)))
    else:
        mu_xi = 0.0

    # page 3 formula 5 of this paper explains that sigma[t] = sigma_0 + sigma_1*E[t]
    # where E[t] is called y(x) in the main paper (discrepancy) and sigma_0 and sigma_1 are input parameters which also
    # can be generated by the function itself. Then
    # E[t] = Y_{ht}*mu[t] (cite, page 2, formula (2))
    # where Y_{ht} should be the simulated model data and mu_t = exp(mu_h * Y_{ht}).
    # So, mu_h is "a bias parameter to be inferred from the data." (cite, page 3, formula (3))

    #mu_t = np.mean(muh * comparedata) # this was the formula in the function, I changed it to:
    mu_t = np.exp(mu_h * np.abs(comparedata))

    E_t = comparedata * mu_t

    sigma_t = sigma_0 + sigma_1 * E_t
    sigma_t[np.where(sigma_t < sigma_0)] = sigma_0
    
    if sigma_t[sigma_t <= 0.0].size > 0:
        warnings.warn("Sorry, your comparedata have negative values. Maybe your model has some inaccurate"
                      " assumptions or there is another error."
                      " We cannot calculate this likelihood")
        return np.inf
        
    # formula for a_xi_t is from page 3, (6)    
    #a_t = (errorArr[t] - phi_1 * errorArr[t - 1]) / (measerror[t]) # this was the formula in the function, I changed it to:
    #a_t = (errorArr[t] - (phi_1**(tIndex[t]-tIndex[t-1])) * errorArr[t - 1]) / sigma_t[t]
    a_t = (np.delete(errorArr,0) - (phi_1**(np.diff(tIndex)) * np.delete(errorArr,len(errorArr)-1))) / np.delete(sigma_t,0)
    # modification accounts for inconsistent time steps and uses sigma_t instead of measerror[t] in the denominator.
    # also vectorizes to make the calculation much faster and nearly independent of n
    
    a_xi_t = xi ** (-1 * np.sign(mu_xi + sigma_xi * a_t)) * (mu_xi + sigma_xi * a_t)
    sum_a_xi_t = np.sum(np.abs(a_xi_t) ** (2 / (1 + beta)))
    
    #Sample size - 1 because first data point is only used for its residual in lag1 model.
    n = data.__len__() - 1
    
    # I negated this for minimization
    # Also dropped first term in sigma_t because it's not used to estimate anything with lag1 process.
    # Without normalization by number of observations
    # Removed np.abs for xi+1/xi. Negative xi is not allowed, so I added a check for that above.
    return -(n * np.log(omegaBeta * (2 * sigma_xi) / (xi + (1 / xi))) - np.sum(np.log(np.delete(sigma_t,0))) - cBeta * sum_a_xi_t)
    # to normalize by # of observations:
    # return -(np.log(omegaBeta * (2 * sigma_xi) / (xi + (1 / xi))) - np.sum(np.log(np.delete(sigma_t,0)))/n - cBeta * sum_a_xi_t/n)