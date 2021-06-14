#' @export
marginalPlot <- function(x, ...) UseMethod("marginalPlot")

#' Plot MCMC marginals
#' @param x bayesianOutput, or matrix or data.frame containing with samples as rows and parameters as columns
#' @param prior if x is a bayesianOutput, T/F will determine if the prior is drawn (default = T). If x is matrix oder data.frame, a prior can be drawn if a matrix of prior draws with values as rows and parameters as columns can be provided here. 
#' @param xrange vector or matrix of plotting ranges for the x axis. If matrix, the rows must be parameters and the columns min and max values.
#' @param type character determining the plot type. Either 'd' for density plot, or 'v' for violin plot
#' @param singlePanel logical, determining whether the parameter should be plotted in a single panel or each in its own panel
#' @param settings optional list of additional settings for \code{\link{marginalPlotDensity}}, and \code{\link{marginalPlotViolin}}, respectively
#' @param nPriorDraws number of draws from the prior, if x is bayesianOutput
#' @param ... additional arguments passed to \code{\link{getSample}}. If you have a high number of draws from the posterior it is advised to set numSamples (to e.g. 5000) for performance reasons.
#' @example /inst/examples/marginalPlotHelp.R
#' @author Tankred Ott
marginalPlot <- function(x, prior = NULL, xrange = NULL, type = 'd', singlePanel = FALSE, settings = NULL,
                         nPriorDraws = 10000, trueVals=NULL, SynVals=NULL, MAPVals=NULL, ...) {
  
  posteriorMat <- BayesianTools::getSample(x, parametersOnly = TRUE, ...)
  
  # check prior
  
  if ('bayesianOutput' %in% class(x)) {
    
    # default T 
    if (is.null(prior)) prior = TRUE
    
    if (any(c('data.frame', 'matrix') %in% class(prior))) priorMat = prior
    else if (is.logical(prior)){
      if (prior == TRUE) priorMat = getSetup(x)$prior$sampler(nPriorDraws) # draw prior from bayesianSetup
      else if (prior == F) priorMat = NULL
    }
    else stop('wrong argument to prior')
  } else {
    
    # default F
    if (is.null(prior)) prior = FALSE
    
    if (any(c('data.frame', 'matrix') %in% class(prior))) priorMat = prior    
    else if (is.logical(prior)){
      priorMat = NULL
      if (prior == TRUE) message("prior = T will only have an effect if x is of class BayesianOutput")
    } 
    else stop('wrong argument to prior')
  }
  
  if (!is.null(priorMat)) {
    if (ncol(posteriorMat) != ncol(priorMat)) stop("wrong dimensions of prior")
    colnames(priorMat) <- colnames(posteriorMat)    
  }
  
  nPar <- ncol(posteriorMat)
  
  # check xrange
  if (!is.null(xrange)) {
    if (!any(c('numeric', 'matrix') %in% class(xrange))) stop('xrange must be numeric or matrix, or NULL')
    if ('numeric' %in% class(xrange)) xrange <- matrix(rep(xrange), nPar, nrow = 2)
    else if ('matrix' %in% class(xrange)) {
      if (ncol(xrange) != ncol(posteriorMat)) stop('xrange must have as many colums as there are parameterss')
      else if (nrow(xrange) != 2) stop('xrange must have two rows (min, max)')
    }
  } else {
    posteriorRanges <- apply(posteriorMat, 2, range)
    priorRanges <- if(!is.null(priorMat)) apply(priorMat, 2, range) else NULL
    
    xrange <- if (is.null(priorRanges)) posteriorRanges else apply(rbind(priorRanges, posteriorRanges), 2, range)
  }
  
  # check type
  if (any(c('d', 'dens', 'density') == type)) type <- 'd'
  # else if (any(c('h', 'hist', 'histogram') == type)) type <- 'h'
  else if (any(c('v', 'violin') == type)) type <- 'v'
  # else stop('type must be one of "d", "h", "v"')
  else stop('type must be one of "d", "v"')
  
  # check parameter names
  if (is.null(colnames(posteriorMat))) colnames(posteriorMat) <- paste('par', 1:nPar, sep = '')
  if (!is.null(priorMat)) colnames(priorMat) <- colnames(posteriorMat)
  
  # prepare arguments for sub-functions
  .args <- c(list(posteriorMat = posteriorMat, priorMat = priorMat, xrange = xrange, singlePanel = singlePanel, trueVals = trueVals, SynVals=SynVals, MAPVals=MAPVals),
             settings)
  
  if (type == 'd') do.call(marginalPlotDensity, .args)
  # else if (type == 'h') do.call(marginalPlotHistogram, .args)
  else if (type == 'v') do.call(marginalPlotViolin, .args)
}

#' Plot marginals as densities
#' @param posteriorMat matrix with samples as rows and parameters as columns
#' @param priorMat matrix (optional) with samples as rows and parameters as columns
#' @param xrange vector or matrix (optional), determining the plotting range, with parameters as columns and min, max as rows
#' @param col vector of colors for posterior and
#' @param singlePanel logical, determining whether the parameter should be plotted in a single panel or each in its own panel
# #' @param ... further options
#' @author Tankred Ott
#' @keywords internal
# TODO: this could be simplified. It is verbose for now to be able to change stuff easily
marginalPlotDensity <- function(posteriorMat, priorMat = NULL, xrange = NULL, col=c('#FC006299','#00BBAA88'), 
                                singlePanel = TRUE, trueVals=NULL, SynVals=NULL, MAPVals=NULL, ...) {
  
  nPar <- ncol(posteriorMat)
  parNames <- colnames(posteriorMat)
  
  if (is.null(xrange)) {
    posteriorRanges <- apply(posteriorMat, 2, range)
    priorRanges <- if(!is.null(priorMat)) apply(priorMat, 2, range) else NULL
    
    xrange <- if (is.null(priorRanges)) posteriorRanges else apply(rbind(priorRanges, posteriorRanges), 2, range)
  }
  
  posteriorDensities <- lapply(1:ncol(posteriorMat),
                               function(i) density(posteriorMat[,i], from = xrange[1,i], to = xrange[2,i], ...))
  priorDensities <- if (!is.null(priorMat)) lapply(1:ncol(priorMat),
                                                   function(i) density(priorMat[,i], from = xrange[1,i], to = xrange[2,i], ...))
  else NULL
  
  postXY <- lapply(posteriorDensities, function(d) {
    xy <- cbind(c(d$x[1], d$x, d$x[length(d$x)]),
                c(0, d$y, 0))
    colnames(xy) <- c('x', 'y')
    xy
  })
  
  priorXY <- if (!is.null(priorDensities)) lapply(priorDensities, function(d) {
    xy <- cbind(c(d$x[1], d$x, d$x[length(d$x)]),
                c(0, d$y, 0))
    colnames(xy) <- c('x', 'y')
    xy
  }) else NULL
  
  
  if (singlePanel) {
    op <- par(mfrow = c(nPar,1), mar = c(2, 5, 2, 2), oma = c(5, 4, 4, 0))
    on.exit(par(op))
    
    
    for (i in 1:length(posteriorDensities)) {
      postX <- postXY[[i]][,1]
      postY <- postXY[[i]][,2]
      
      priorX <- if (!is.null(priorXY[[i]])) priorXY[[i]][,1] else NULL
      priorY <- if (!is.null(priorXY[[i]])) priorXY[[i]][,2] else NULL
      
      yrange <- if (is.null(priorX)) range(postY) else range(c(postY, priorY))
      
      plot(NULL, NULL, xlim = xrange[,i], ylim = yrange, main = NA,
           xlab = NA, ylab = NA, bty = 'n', yaxt = 'n', xaxt = 'n')
      axis(1, at = xrange[,i], labels = NA, lwd.ticks=0)
      xticks <- axTicks(1)
      xticks <- xticks[xticks >= xrange[1,i] & xticks <= xrange[2,i]]
      
      axis(1, at = xticks)
      
      mtext(sprintf('%20s', parNames[i]), 2, las = 1, adj = 1.25)
      
      
      polygon(postX, postY, col = col[1], border = 1)
      if (!is.null(priorX)) polygon(priorX, priorY, col = col[2], border = 1)
      
      if (!is.null(trueVals)){
        for (lini in 1:nrow(trueVals)){
          lines(x = c(trueVals[lini,i],trueVals[lini,i]), y = yrange, lwd = 2, col = 'green')
        }
      }
      
      if (!is.null(SynVals)){
        lines(x = c(SynVals[i],SynVals[i]), y = yrange, lwd = 2, col = 'black', lty = 2)
      }
      
      if (!is.null(MAPVals)){
        lines(x = c(MAPVals[i],MAPVals[i]), y = yrange, lwd = 2, col = 'blue', lty = 4)
      }
    }
    
    mtext('Marginal parameter uncertainty', outer = TRUE, cex = 1.5)
    
  } else {
    mfrow <- if (nPar < 16) getPanels(nPar) else c(4,4)
    
    op <- par(mfrow = mfrow, mar = c(4.5, 4, 5, 3), oma=c(3, 1.5, 2, 0), xpd=TRUE)
    on.exit(par(op))
    
    for (i in 1:length(posteriorDensities)) {
      postX <- postXY[[i]][,1]
      postY <- postXY[[i]][,2]
      
      priorX <- if (!is.null(priorXY[[i]])) priorXY[[i]][,1] else NULL
      priorY <- if (!is.null(priorXY[[i]])) priorXY[[i]][,2] else NULL
      
      yrange <- if (is.null(priorX)) range(postY) else range(c(postY, priorY))
      
      plot(NULL, NULL, xlim = xrange[,i], ylim = yrange, main = parNames[i],
           xlab = NA, ylab = 'density')
      
      polygon(postX, postY, col = col[1], border = 1)
      if (!is.null(priorX)) polygon(priorX, priorY, col = col[2], border = 1)
      
      if (i %% 16 == 1) mtext('Marginal parameter uncertainty', outer = TRUE, cex = 1.5)
      
      if (!is.null(trueVals)){
        for (lini in 1:nrow(trueVals)){
          lines(x = c(trueVals[lini,i],trueVals[lini,i]), y = yrange, lwd = 2, col = 'green')
        }
      }
      if (!is.null(SynVals)){
        lines(x = c(SynVals[i],SynVals[i]), y = yrange, lwd = 2, col = 'black', lty = 2)
      }
      if (!is.null(MAPVals)){
        lines(x = c(MAPVals[i],MAPVals[i]), y = yrange, lwd = 2, col = 'blue', lty = 4)
      }
    }
  }
  
  # overlay plot with empty plot to be able to place the legends freely
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
  if(!is.null(SynVals)){
    legend('bottom', c('MAP', 'Samples', 'Synthetic Truth','posterior', 'prior'), xpd = TRUE, horiz = FALSE, inset = c(0, 0),
           bty = 'n', pch = c(NA,NA,NA,15,15), col = c('blue','green','black',col), lty = c(4,1,2,NA,NA), cex = 1, ncol = 2)
  }else{
    legend('bottom', if (!is.null(priorX)) c('posterior', 'prior') else 'posterior', xpd = TRUE, horiz = TRUE, inset = c(0, 0),
           bty = 'n', pch = 15, col = col, cex = 1.5)
  }
}

#' Calculates the panel combination for par(mfrow = )
#' @author Florian Hartig
#' @param x the desired number of panels 
#' @keywords internal
getPanels <- function(x){
  if (x <= 0) stop("number can't be < 1")
  
  lower = floor(sqrt(x))
  upper = ceiling(sqrt(x))
  
  if (lower == upper) return(c(lower, lower))
  else{
    if (lower * upper >= x) return(c(lower, upper))
    else return(c(upper, upper))    
  }
}  
