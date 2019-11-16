function (eList, surfaceStart = NA, surfaceEnd = NA, localSample = NA, 
          windowY = 7, windowQ = 2, windowS = 0.5, minNumObs = 100, 
          minNumUncen = 50, edgeAdjust = TRUE, verbose = TRUE, interactive = NULL, 
          run.parallel = FALSE, numTsteps=16, numQsteps=14) 
{
  if (!is.null(interactive)) {
    warning("The argument 'interactive' is deprecated. Please use 'verbose' instead")
    verbose <- interactive
  }
  if (!is.egret(eList)) {
    stop("Please check eList argument")
  }
  localINFO <- getInfo(eList)
  localDaily <- getDaily(eList)
  if (all(is.na(localSample))) {
    localSample <- eList$Sample
  }
  highLow <- decimalHighLow(localSample)
  DecHigh <- highLow[["DecHigh"]]
  DecLow <- highLow[["DecLow"]]
  surfaceInfo <- surfaceIndex(localDaily, numTsteps, numQsteps)
  vectorYear <- surfaceInfo[["vectorYear"]]
  vectorLogQ <- surfaceInfo[["vectorLogQ"]]
  LogQ <- seq(surfaceInfo[["bottomLogQ"]], by = surfaceInfo[["stepLogQ"]], 
              length.out = surfaceInfo[["nVectorLogQ"]])
  if (is.na(surfaceStart) && is.na(surfaceEnd)) {
    nVectorYear <- length(vectorYear)
    estPtYear <- rep(vectorYear, each = numQsteps)
    Year <- seq(surfaceInfo[["bottomYear"]], by = surfaceInfo[["stepYear"]], 
                length.out = surfaceInfo[["nVectorYear"]])
  }else {
    sliceIndex <- which(vectorYear >= decimalDate(as.Date(surfaceStart)) & 
                          vectorYear <= decimalDate(as.Date(surfaceEnd)))
    Year <- vectorYear[c(sliceIndex[1] - 1, sliceIndex, 
                         tail(sliceIndex, n = 1) + 1)]
    nVectorYear <- length(Year)
    estPtYear <- rep(Year, each = numQsteps)
  }
  estPtLogQ <- rep(vectorLogQ, nVectorYear)
  #Here, want to return the values of the parameters in addition to the surface.
  #The function that returns values is runSurvReg, but computations are in run_WRTDS
  resultSurvReg <- runSurvReg(estPtYear = estPtYear, estPtLQ = estPtLogQ, DecLow = DecLow, 
                              DecHigh = DecHigh, Sample = localSample, windowY = windowY, windowQ = windowQ, windowS = windowS, minNumObs = minNumObs, 
                              minNumUncen = minNumUncen, edgeAdjust = edgeAdjust, verbose = verbose, run.parallel = run.parallel)
  surfaces <- array(0, dim = c(numQsteps, nVectorYear, 3))
  for (iQ in 1:numQsteps) {
    for (iY in 1:nVectorYear) {
      k <- (iY - 1) * numQsteps + iQ
      surfaces[iQ, iY, ] <- resultSurvReg[k, ]
    }
  }
  attr(surfaces, "surfaceIndex") <- surfaceInfo
  attr(surfaces, "LogQ") <- LogQ
  attr(surfaces, "Year") <- Year
  return(surfaces)
}


surfaceIndex = function (Daily, numTsteps, numQsteps) 
{
  localDaily <- Daily
  bottomLogQ <- min(localDaily$LogQ, na.rm = TRUE) - 0.05
  topLogQ <- max(localDaily$LogQ, na.rm = TRUE) + 0.05
  stepLogQ <- (topLogQ - bottomLogQ)/(numQsteps-1)
  vectorLogQ <- seq(bottomLogQ, topLogQ, stepLogQ)
  stepYear <- 1/numTsteps
  bottomYear <- floor(min(localDaily$DecYear, na.rm = TRUE))
  topYear <- ceiling(max(localDaily$DecYear, na.rm = TRUE))
  vectorYear <- seq(bottomYear, topYear, stepYear)
  nVectorYear <- length(vectorYear)
  surfaceIndexParameters <- list(bottomLogQ = bottomLogQ, 
                                 stepLogQ = stepLogQ, nVectorLogQ = numQsteps, bottomYear = bottomYear, 
                                 stepYear = stepYear, nVectorYear = nVectorYear, vectorYear = vectorYear, 
                                 vectorLogQ = vectorLogQ)
  return(surfaceIndexParameters)
}


runSurvReg = function (estPtYear, estPtLQ, DecLow, DecHigh, Sample, windowY = 7, 
          windowQ = 2, windowS = 0.5, minNumObs = 100, minNumUncen = 50, 
          verbose = TRUE, interactive = NULL, edgeAdjust = TRUE, run.parallel = FALSE) 
{
  if (!is.null(interactive)) {
    warning("The argument 'interactive' is deprecated. Please use 'verbose' instead")
    verbose <- interactive
  }
  localSample <- Sample
  if (any(is.na(localSample$LogQ))) {
    message("Removing Sample data that does not have corresponding flow data")
    localSample <- localSample[!is.na(localSample$LogQ), 
                               ]
  }
  numSamples <- length(localSample$DecYear)
  numEstPt <- length(estPtYear)
  printUpdate <- floor(seq(1, numEstPt, numEstPt/100))
  endOfLine <- seq(10, 100, 10)
  if (minNumUncen >= sum(localSample$Uncen)) 
    stop("minNumUncen is greater than total number of samples")
  if (minNumObs >= nrow(localSample)) 
    stop("minNumObs is greater than total number of samples")
  warningFlag <- 0
  n <- NULL
  if (run.parallel) {
    `%dopar%` <- foreach::`%dopar%`
    wrtds_return_list <- foreach::foreach(n = 1:numEstPt, 
                                          .packages = c("EGRET")) %dopar% {
                                            wrtds_returns <- run_WRTDS(estY = estPtYear[n], 
                                                                       estLQ = estPtLQ[n], localSample = localSample, 
                                                                       DecLow = DecLow, DecHigh = DecHigh, minNumObs = minNumObs, 
                                                                       minNumUncen = minNumUncen, windowY = windowY, 
                                                                       windowQ = windowQ, windowS = windowS, edgeAdjust = edgeAdjust)
                                          }
    warningFlag <- sum(sapply(wrtds_return_list, function(x) x[["warningFlag"]]))
    resultSurvReg <- t(sapply(wrtds_return_list, function(x) x[["survReg"]]))
  }else {
    resultSurvReg <- array(0, c(numEstPt, 3))
    if (verbose) 
      cat("Survival regression (% complete):\n")
    for (i in 1:numEstPt) {
      wrtds_return <- run_WRTDS(estY = estPtYear[i], estLQ = estPtLQ[i], 
                                localSample = localSample, DecLow = DecLow, 
                                DecHigh = DecHigh, minNumObs = minNumObs, minNumUncen = minNumUncen, 
                                windowY = windowY, windowQ = windowQ, windowS = windowS, 
                                edgeAdjust = edgeAdjust)
      if (i %in% printUpdate & verbose) {
        cat(floor(i * 100/numEstPt), "\t")
        if (floor(i * 100/numEstPt) %in% endOfLine) 
          cat("\n")
      }
      warningFlag <- warningFlag + wrtds_return$warningFlag
      resultSurvReg[i, ] <- wrtds_return$survReg
    }
  }
  if (warningFlag > 0) {
    message("\nIn model estimation, the survival regression function was run ", 
            numEstPt, " times (for different combinations of discharge and time).  In ", 
            warningFlag, " of these runs it did not properly converge. This does not mean that the model is unacceptable, but it is a suggestion that there may be something odd about the data set. You may want to check for outliers, repeated values on a single date, or something else unusual about the data.")
  }
  if (verbose) 
    cat("\nSurvival regression: Done")
  return(resultSurvReg)
}

run_WRTDS = function (estY, estLQ, localSample, DecLow, DecHigh, minNumObs, 
          minNumUncen, windowY, windowQ, windowS, edgeAdjust) 
{
  tempWindowY <- windowY
  tempWindowQ <- windowQ
  tempWindowS <- windowS
  distLow <- estY - DecLow
  distHigh <- DecHigh - estY
  survReg <- c(NA, NA, NA)
  warningFlag <- 0
  if (all(is.na(c(distLow, distHigh)))) {
    return(list(survReg = survReg, warningFlag = warningFlag))
  }
  distTime <- min(distLow, distHigh)
  if (edgeAdjust & !is.na(distTime)) {
    tempWindowY <- if (distTime > tempWindowY) 
      tempWindowY
    else ((2 * tempWindowY) - distTime)
  }
  k <- 1
  repeat {
    Sam <- localSample[abs(localSample$DecYear - estY) <= 
                         tempWindowY, ]
    diffY <- abs(Sam$DecYear - estY)
    weightY <- triCube(diffY, tempWindowY)
    weightQ <- triCube(Sam$LogQ - estLQ, tempWindowQ)
    diffUpper <- ceiling(diffY)
    diffLower <- floor(diffY)
    diffSeason <- pmin(abs(diffUpper - diffY), abs(diffY - 
                                                     diffLower))
    weightS <- triCube(diffSeason, tempWindowS)
    Sam$weight <- weightY * weightQ * weightS
    Sam <- subset(Sam, weight > 0)
    numPosWt <- length(Sam$weight)
    numUncen <- sum(Sam$Uncen)
    tempWindowY <- tempWindowY * 1.1
    tempWindowQ <- tempWindowQ * 1.1
    k <- k + 1
    if (k > 10000) 
      message("Problems converging")
    tempWindowS <- if (windowS <= 0.5) 
      min(tempWindowS * 1.1, 0.5)
    else windowS
    if (numPosWt >= minNumObs & numUncen >= minNumUncen | 
        k > 10000) 
      break
  }
  weight <- Sam$weight
  aveWeight <- sum(weight)/numPosWt
  weight <- weight/aveWeight
  Sam <- data.frame(Sam)
  x <- tryCatch({
    survModel <- survival::survreg(survival::Surv(log(ConcLow), 
                                                  log(ConcHigh), type = "interval2") ~ DecYear + LogQ + 
                                     SinDY + CosDY, data = Sam, weights = weight, dist = "gaus")
  }, warning = function(w) {
    return(NA)
  }, error = function(e) {
    message(e, "Error")
    return(NULL)
  })
  if (exists("survModel")) {
    newdf <- data.frame(DecYear = estY, LogQ = estLQ, SinDY = sin(2 * 
                                                                    pi * estY), CosDY = cos(2 * pi * estY))
    yHat <- predict(survModel, newdf)
    #Check this - is the SD instead of SE? What dimensions does this have? should equal nrow(Sam). How can it be a scalar?
    SE <- survModel$scale
    #This can't be correct. SE is in log space. Why divided by 2?
    bias <- exp((SE^2)/2)
    survReg[1] <- yHat
    survReg[2] <- SE
    survReg[3] <- bias * exp(yHat)
    survReg[c(4,5,6,7,8)] <- as.numeric(survModel$coefficients)
  }
  if (all(is.na(x))) {
    warningFlag <- 1
  }
  #Want to add to this return list the parameters for the regression
  return(list(survReg = survReg, warningFlag = warningFlag))
}
