library(EGRET)
library(survival)
library(pracma)

modelEstimation = function (eList, windowY = 7, windowQ = 2, windowS = 0.5, minNumObs = 100, 
          minNumUncen = 50, edgeAdjust = TRUE, verbose = TRUE, run.parallel = FALSE, numTsteps=16, numQsteps=14) 
{
  if (!is.egret(eList)) {
    stop("Please check eList argument")
  }
  eList <- setUpEstimation(eList = eList, windowY = windowY, 
                           windowQ = windowQ, windowS = windowS, minNumObs = minNumObs, 
                           minNumUncen = minNumUncen, edgeAdjust = edgeAdjust, 
                           verbose = verbose)
  if (verbose) 
    cat("\n first step running estCrossVal may take about 1 minute")
  Sample1 <- estCrossVal(eList$Daily$DecYear[1], eList$Daily$DecYear[length(eList$Daily$DecYear)], 
                         eList$Sample, windowY = windowY, windowQ = windowQ, 
                         windowS = windowS, minNumObs = minNumObs, minNumUncen = minNumUncen, 
                         edgeAdjust = edgeAdjust, verbose = verbose)
  eList$Sample <- Sample1
  if (verbose) 
    cat("\nNext step running  estSurfaces with survival regression:\n")
  surfaces1 <- estSurfaces(eList, windowY = windowY, windowQ = windowQ, 
                           windowS = windowS, minNumObs = minNumObs, minNumUncen = minNumUncen, 
                           edgeAdjust = edgeAdjust, verbose = verbose, run.parallel = run.parallel, numTsteps=numTsteps, numQsteps=numQsteps)
  eList$surfaces <- surfaces1
  Daily1 <- estDailyFromSurfaces(eList)
  eList$Daily <- Daily1
  checkSurfaceSpan(eList)
  return(eList)
}

estSurfaces = function (eList, surfaceStart = NA, surfaceEnd = NA, localSample = NA, 
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
  surfaces <- array(0, dim = c(numQsteps, nVectorYear, 8))
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
    resultSurvReg <- array(0, c(numEstPt, 8))
    if (verbose) 
      cat("Survival regression (% complete):\n")
    for (i in 1:numEstPt) {
      #Added a return of parameters of the regression and the error parameter here
      #Order is: Error in location 2, then locations 4-8 are Intercept, DecYear, LogQ, SinDY, CosDY
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
  survReg <- rep(NA,8)
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
  #Added to this return list the parameters for the regression in survReg vector
  return(list(survReg = survReg, warningFlag = warningFlag))
}

plotContours = function (eList, yearStart, yearEnd, qBottom = NA, qTop = NA, 
          whatSurface = 3, qUnit = 2, contourLevels = NA, span = 60, 
          pval = 0.05, printTitle = TRUE, vert1 = NA, vert2 = NA, 
          horiz = NA, tcl = 0.03, flowDuration = TRUE, customPar = FALSE, 
          yTicks = NA, tick.lwd = 1, usgsStyle = FALSE, lwd = 2, cex.main = 1, 
          cex.axis = 1, color.palette = colorRampPalette(c("white", 
                                                           "gray", "blue", "red")), ...) 
{
  localINFO <- getInfo(eList)
  localDaily <- getDaily(eList)
  localsurfaces <- getSurfaces(eList)
  nVectorYear <- localINFO$nVectorYear
  bottomLogQ <- localINFO$bottomLogQ
  stepLogQ <- localINFO$stepLogQ
  nVectorLogQ <- localINFO$nVectorLogQ
  if (is.numeric(qUnit)) {
    qUnit <- qConst[shortCode = qUnit][[1]]
  }
  else if (is.character(qUnit)) {
    qUnit <- qConst[qUnit][[1]]
  }
  if (!customPar) {
    par(mgp = c(2.5, 0.5, 0))
  }
  surfaceName <- c("log of Concentration", "Standard Error of log(C)", 
                   "Concentration")
  j <- whatSurface
  surf <- localsurfaces
  surfaceMin <- min(surf[, , j])
  surfaceMax <- max(surf[, , j])
  surfaceSpan <- c(surfaceMin, surfaceMax)
  contourLevels <- if (is.na(contourLevels[1])) 
    pretty(surfaceSpan, n = 5)
  else contourLevels
  if (all(c("Year", "LogQ") %in% names(attributes(localsurfaces)))) {
    x <- attr(localsurfaces, "Year")
    y <- attr(localsurfaces, "LogQ")
  }
  else {
    x <- seq(localINFO$bottomYear, by = localINFO$stepYear, 
             length.out = localINFO$nVectorYear)
    y <- ((1:nVectorLogQ) * stepLogQ) + (bottomLogQ - stepLogQ)
  }
  yLQ <- y
  qFactor <- qUnit@qUnitFactor
  y <- exp(y) * qFactor
  numX <- length(x)
  numY <- length(y)
  qBottomT <- ifelse(is.na(qBottom), quantile(localDaily$Q, 
                                              probs = 0.05) * qFactor, qBottom)
  qTopT <- ifelse(is.na(qTop), quantile(localDaily$Q, probs = 0.95) * 
                    qFactor, qTop)
  if (any(is.na(yTicks))) {
    if (is.na(qBottom)) {
      qBottom <- max(0.9 * y[1], qBottomT)
    }
    if (is.na(qTop)) {
      qTop <- min(1.1 * y[numY], qTopT)
    }
    yTicks <- logPretty3(qBottom, qTop)
  }
  if (yearEnd - yearStart >= 4) {
    xSpan <- c(yearStart, yearEnd)
    xTicks <- pretty(xSpan, n = 5)
    xlabels <- xTicks
  }
  else {
    xlabels <- c(as.Date(paste(yearStart, "-01-01", sep = "")), 
                 as.Date(paste(yearEnd, "-01-01", sep = "")))
    xlabels <- pretty(xlabels, n = 5)
    xTicksDates <- as.POSIXlt(xlabels)
    years <- xTicksDates$year + 1900
    day <- xTicksDates$yday
    xTicks <- years + day/365
    xlabels <- format(xlabels, "%Y-%b")
  }
  nYTicks <- length(yTicks)
  surfj <- surf[, , j]
  surft <- t(surfj)
  plotTitle <- if (printTitle) 
    paste(localINFO$shortName, " ", localINFO$paramShortName, 
          "\nEstimated", surfaceName[j], "Surface in Color")
  else NULL
  if (flowDuration) {
    numDays <- length(localDaily$Day)
    freq <- rep(0, nVectorLogQ)
    durSurf <- rep(0, length(x) * length(y))
    dim(durSurf) <- c(length(x), length(y))
    centerDays <- seq(1, 365, 22.9)
    centerDays <- floor(centerDays)
    for (ix in 1:16) {
      startDay <- centerDays[ix] - span
      endDay <- centerDays[ix] + span
      goodDays <- seq(startDay, endDay, 1)
      goodDays <- ifelse(goodDays > 0, goodDays, goodDays + 
                           365)
      goodDays <- ifelse(goodDays < 366, goodDays, goodDays - 
                           365)
      numDays <- length(localDaily$Day)
      isGood <- localDaily$Day %in% goodDays
      spanDaily <- data.frame(localDaily, isGood)
      spanDaily <- subset(spanDaily, isGood)
      n <- length(spanDaily$Day)
      LogQ <- spanDaily$LogQ
      for (jQ in 1:length(y)) {
        ind <- ifelse(LogQ < yLQ[jQ], 1, 0)
        freq[jQ] <- sum(ind)/n
      }
      xInd <- seq(ix, numX, 16)
      numXind <- length(xInd)
      for (ii in 1:numXind) {
        iX <- xInd[ii]
        durSurf[iX, ] <- freq
      }
    }
    plevels <- c(pval, 1 - pval)
    pct1 <- format(plevels[1] * 100, digits = 2)
    pct2 <- format(plevels[2] * 100, digits = 2)
    plotTitle <- plotTitle <- if (printTitle) 
      paste(localINFO$shortName, "  ", localINFO$paramShortName, 
            "\nEstimated", surfaceName[j], "Surface in Color\nBlack lines are", 
            pct1, "and", pct2, "flow percentiles")
  }
  vectorNone <- c(yearStart, log(yTicks[1], 10) - 1, yearEnd, 
                  log(yTicks[1], 10) - 1)
  v1 <- if (is.na(vert1)) 
    vectorNone
  else c(vert1, log(yTicks[1], 10), vert1, log(yTicks[nYTicks], 
                                               10))
  v2 <- if (is.na(vert2)) 
    vectorNone
  else c(vert2, log(yTicks[1], 10), vert2, log(yTicks[nYTicks], 
                                               10))
  h1 <- if (is.na(horiz)) 
    vectorNone
  else c(yearStart, log(horiz, 10), yearEnd, log(horiz, 10))
  deltaY <- (log(yTicks[length(yTicks)], 10) - log(yTicks[1], 
                                                   10))/25
  deltaX <- (yearEnd - yearStart)/25
  yLab <- ifelse(usgsStyle, qUnit@unitUSGS, qUnit@qUnitExpress)
  logY <- log(y, 10)
  filled.contour(x, log(y, 10), surft, levels = contourLevels, 
                 xlim = c(yearStart, yearEnd), ylim = c(log(yTicks[1], 
                                                            10), log(yTicks[nYTicks], 10)), xaxs = "i", yaxs = "i", 
                 color.palette = color.palette, plot.axes = {
                   width <- grconvertX(par("usr")[2], from = "user", 
                                       to = "inches") - grconvertX(par("usr")[1], from = "user", 
                                                                   to = "inches")
                   height <- grconvertY(par("usr")[4], from = "user", 
                                        to = "inches") - grconvertY(par("usr")[3], from = "user", 
                                                                    to = "inches")
                   axis(1, tcl = 0, at = xTicks, labels = xlabels, 
                        cex.axis = cex.axis)
                   axis(2, tcl = 0, las = 1, at = log(yTicks, 10), 
                        labels = yTicks, cex.axis = cex.axis)
                   axis(3, tcl = 0, at = xTicks, labels = FALSE, cex.axis = cex.axis)
                   axis(4, tcl = 0, at = log(yTicks, 10), labels = FALSE, 
                        cex.axis = cex.axis)
                   if (flowDuration) 
                     contour(x, log(y, 10), durSurf, add = TRUE, 
                             drawlabels = FALSE, levels = plevels, lwd = lwd)
                   segments(v1[1], v1[2], v1[3], v1[4])
                   segments(v2[1], v2[2], v2[3], v2[4])
                   segments(h1[1], h1[2], h1[3], h1[4])
                   segments(xTicks, rep(log(yTicks[1], 10), length(xTicks)), 
                            xTicks, rep(grconvertY(grconvertY(par("usr")[3], 
                                                              from = "user", to = "inches") + tcl, from = "inches", 
                                                   to = "user"), length(xTicks)), lwd = tick.lwd)
                   segments(xTicks, rep(log(yTicks[nYTicks], 10), length(xTicks)), 
                            xTicks, rep(grconvertY(grconvertY(par("usr")[4], 
                                                              from = "user", to = "inches") - tcl, from = "inches", 
                                                   to = "user"), length(xTicks)), lwd = tick.lwd)
                   segments(rep(yearStart, length(yTicks)), log(yTicks, 
                                                                10), rep(grconvertX(grconvertX(par("usr")[1], 
                                                                                               from = "user", to = "inches") + tcl, from = "inches", 
                                                                                    to = "user"), length(yTicks)), log(yTicks, 10), 
                            lwd = tick.lwd)
                   segments(rep(grconvertX(grconvertX(par("usr")[2], 
                                                      from = "user", to = "inches") - tcl, from = "inches", 
                                           to = "user"), length(yTicks)), log(yTicks, 10), 
                            rep(yearEnd, length(yTicks)), log(yTicks, 10), 
                            lwd = tick.lwd)
                 }, plot.title = {
                   if (printTitle) {
                     title(main = plotTitle, ylab = yLab, cex.main = cex.main)
                   }
                   else {
                     title(main = NULL, ylab = yLab)
                   }
                 })
}


#Function to predict TN from the provided date and flow by interpolating the model parameters
predictWRTDS = function(Date, Flow){
  #There are some hillslopes with 0 flows to 6 decimal places. Report 0 concentration for those flows
  if (Flow == 0){
    preds = rep(0,3)
    return(preds)
  }else{
    #Date is expected to be in year-mo-dy format as a character
    #Convert to decimal year using egret function that made the table decimal years
    DecYear = decimalDate(rawData = as.Date(Date))
    
    #Flow is expected in real space units in cfs
    #Convert to log space
    LogFlow = log(Flow*12^3*2.54^3/100^3)
    
    #Calculate the sin and cos of the year
    SinDY = sin(2*pi*DecYear)
    CosDY = cos(2*pi*DecYear)
    
    #Check that the LogFlow is in the interpolation range
    if (LogFlow > as.numeric(rownames(TabInt)[nrow(TabInt)])){
      print('The LogFlow value is greater than the values in the interpolation table. Using the largest flow values available in the table for LogFlow.')
      PLogFlow = as.numeric(rownames(TabInt)[nrow(TabInt)])
    }else if (LogFlow < as.numeric(rownames(TabInt)[1])){
      print('The LogFlow value is less than the smallest value in the interpolation table. Using the smallest flow values available in the table for LogFlow.')
      PLogFlow = as.numeric(rownames(TabInt)[1])
    }else{
      PLogFlow = LogFlow
    }
    
    #Check that the DecYear is in the interpolation range
    if (DecYear > as.numeric(colnames(TabInt)[ncol(TabInt)])){
      print('The decimal year value is greater than the values in the interpolation table. Using the largest year available in the table for DecYear.')
      PDecYear = as.numeric(colnames(TabInt)[nrow(TabInt)])
    }else if (DecYear < as.numeric(colnames(TabInt)[1])){
      print('The decimal year value is less than the smallest value in the interpolation table. Using the smallest year available in the table for DecYear.')
      PDecYear = as.numeric(colnames(TabInt)[1])
    }else{
      PDecYear = DecYear
    }
    
    #Get the 5th, median, and 95th percentiles in log space
    rowt = as.numeric(rownames(TabInt))
    colt = as.numeric(colnames(TabInt))
    predMed = (interp2(xp = PDecYear, yp = PLogFlow, method = 'linear', Z = TabInt, y = rowt, x = colt) + 
                 interp2(xp = PDecYear, yp = PLogFlow, method = 'linear', Z = TabYear, y = rowt, x = colt)*DecYear +
                 interp2(xp = PDecYear, yp = PLogFlow, method = 'linear', Z = TabLogQ, y = rowt, x = colt)*LogFlow +
                 interp2(xp = PDecYear, yp = PLogFlow, method = 'linear', Z = TabSinYear, y = rowt, x = colt)*SinDY +
                 interp2(xp = PDecYear, yp = PLogFlow, method = 'linear', Z = TabCosYear, y = rowt, x = colt)*CosDY)
    e = interp2(xp = PDecYear, yp = PLogFlow, method = 'linear', Z = TabLogErr, y = rowt, x = colt)
    pred05 = predMed + e*-2
    
    pred95 = predMed + e*2
    
    #Convert to real space
    preds = exp(c(pred05, predMed, pred95))
    return(preds)
  }
}

#Fill in NAs in interpolation tables
FillTableNAs = function(DateInd, #Date is the column index
                        FlowInd) #Flow is the row index
  {
  #Fill in the NA value assuming that the table is equally spaced in columns and rows.
  #such that inverse distance weighting will work. Treated each dimension as equally important.
  
  #Find the non-NA values in the tables
  IndNotNA = which(!is.na(TempTabInt))
  TotalDist = vector('numeric', length=length(IndNotNA))
  #Get the weights for those indices
  for (w in 1:length(TotalDist)){
    #Find row distance
    RowDist = abs((IndNotNA[w] - nrow(TempTabInt)*(ceiling(IndNotNA[w]/nrow(TempTabInt))-1)) - FlowInd)
    #Find column distance
    ColDist = abs(ceiling(IndNotNA[w]/nrow(TempTabInt)) - DateInd)
    #Total distance
    TotalDist[w] = 1/sqrt(RowDist^2 + ColDist^2)
  }
  weights = TotalDist/sum(TotalDist)
  
  NewTableVals = c(sum(TempTabInt[IndNotNA]*weights), 
                   sum(TempTabYear[IndNotNA]*weights),
                   sum(TempTabLogQ[IndNotNA]*weights),
                   sum(TempTabSinYear[IndNotNA]*weights),
                   sum(TempTabCosYear[IndNotNA]*weights), 
                   sum(TempTabLogErr[IndNotNA]*weights))
  
  return(NewTableVals)
}

#runSurvReg parameters
# estPtYear = estPtYear 
# estPtLQ = estPtLogQ 
# DecLow = DecLow 
# DecHigh = DecHigh 
# Sample = localSample 
# windowY = windowY 
# windowQ = windowQ 
# windowS = windowS 
# minNumObs = minNumObs 
# minNumUncen = minNumUncen 
# edgeAdjust = edgeAdjust 
# verbose = verbose 
# run.parallel = FALSE
# 
# #run_WRTDS parameters
# estY = estPtYear[i] 
# estLQ = estPtLQ[i] 
# localSample = localSample 
# DecLow = DecLow 
# DecHigh = DecHigh
# minNumObs = minNumObs 
# minNumUncen = minNumUncen 
# windowY = windowY 
# windowQ = windowQ 
# windowS = windowS 
# edgeAdjust = edgeAdjust
# 
# #survreg function - returns scale in log space and then converts to real space.
# function (formula, data, weights, subset, na.action, dist = "weibull", 
#           init = NULL, scale = 0, control, parms = NULL, model = FALSE, 
#           x = FALSE, y = TRUE, robust = FALSE, score = FALSE, ...) 
# {
#   Call <- match.call()
#   indx <- match(c("formula", "data", "weights", "subset", 
#                   "na.action"), names(Call), nomatch = 0)
#   if (indx[1] == 0) 
#     stop("A formula argument is required")
#   temp <- Call[c(1, indx)]
#   temp[[1L]] <- quote(stats::model.frame)
#   special <- c("strata", "cluster")
#   temp$formula <- if (missing(data)) 
#     terms(formula, special)
#   else terms(formula, special, data = data)
#   m <- eval(temp, parent.frame())
#   Terms <- attr(m, "terms")
#   weights <- model.extract(m, "weights")
#   Y <- model.extract(m, "response")
#   if (!inherits(Y, "Surv")) 
#     stop("Response must be a survival object")
#   type <- attr(Y, "type")
#   if (type == "counting") 
#     stop("start-stop type Surv objects are not supported")
#   if (type == "mright" || type == "mcounting") 
#     stop("multi-state survival is not supported")
#   strats <- attr(Terms, "specials")$strata
#   cluster <- attr(Terms, "specials")$cluster
#   dropx <- NULL
#   if (length(cluster)) {
#     if (missing(robust)) 
#       robust <- TRUE
#     tempc <- untangle.specials(Terms, "cluster", 1:10)
#     ord <- attr(Terms, "order")[tempc$terms]
#     if (any(ord > 1)) 
#       stop("Cluster can not be used in an interaction")
#     cluster <- strata(m[, tempc$vars], shortlabel = TRUE)
#     dropx <- tempc$terms
#   }
#   if (length(strats)) {
#     temp <- untangle.specials(Terms, "strata", 1)
#     dropx <- c(dropx, temp$terms)
#     if (length(temp$vars) == 1) 
#       strata.keep <- m[[temp$vars]]
#     else strata.keep <- strata(m[, temp$vars], shortlabel = TRUE)
#     strata <- as.numeric(strata.keep)
#     nstrata <- max(strata)
#   }
#   else {
#     nstrata <- 1
#     strata <- 0
#   }
#   if (length(dropx)) {
#     newTerms <- Terms[-dropx]
#     attr(newTerms, "intercept") <- attr(Terms, "intercept")
#   }
#   else newTerms <- Terms
#   X <- model.matrix(newTerms, m)
#   assign <- lapply(attrassign(X, newTerms)[-1], function(x) x - 
#                      1)
#   xlevels <- .getXlevels(newTerms, m)
#   contr.save <- attr(X, "contrasts")
#   if (!all(is.finite(X))) 
#     stop("data contains an infinite predictor")
#   n <- nrow(X)
#   nvar <- ncol(X)
#   offset <- model.offset(m)
#   if (length(offset) == 0 || all(offset == 0)) 
#     offset <- rep(0, n)
#   if (is.character(dist)) {
#     dist <- match.arg(dist, names(survreg.distributions))
#     dlist <- survreg.distributions[[dist]]
#     if (is.null(dlist)) 
#       stop(paste(dist, ": distribution not found"))
#   }
#   else if (is.list(dist)) 
#     dlist <- dist
#   else stop("Invalid distribution object")
#   if (!survregDtest(dlist)) 
#     stop("Invalid distribution object")
#   logcorrect <- 0
#   Ysave <- Y
#   if (!is.null(dlist$trans)) {
#     tranfun <- dlist$trans
#     exactsurv <- Y[, ncol(Y)] == 1
#     if (any(exactsurv)) {
#       if (is.null(weights)) 
#         logcorrect <- sum(log(dlist$dtrans(Y[exactsurv, 
#                                              1])))
#       else logcorrect <- sum(weights[exactsurv] * log(dlist$dtrans(Y[exactsurv, 
#                                                                      1])))
#     }
#     if (type == "interval") {
#       if (any(Y[, 3] == 3)) 
#         Y <- cbind(tranfun(Y[, 1:2]), Y[, 3])
#       else Y <- cbind(tranfun(Y[, 1]), Y[, 3])
#     }
#     else if (type == "left") 
#       Y <- cbind(tranfun(Y[, 1]), 2 - Y[, 2])
#     else Y <- cbind(tranfun(Y[, 1]), Y[, 2])
#     if (!all(is.finite(Y))) 
#       stop("Invalid survival times for this distribution")
#   }
#   else {
#     if (type == "left") 
#       Y[, 2] <- 2 - Y[, 2]
#     else if (type == "interval" && all(Y[, 3] < 3)) 
#       Y <- Y[, c(1, 3)]
#   }
#   if (!is.null(dlist$scale)) {
#     if (!missing(scale)) 
#       warning(paste(dlist$name, "has a fixed scale, user specified value ignored"))
#     scale <- dlist$scale
#   }
#   if (!is.null(dlist$dist)) 
#     if (is.atomic(dlist$dist)) 
#       dlist <- survreg.distributions[[dlist$dist]]
#   else dlist <- dlist$dist
#   ptemp <- dlist$parms
#   if (is.null(ptemp)) {
#     if (!is.null(parms)) 
#       stop(paste(dlist$name, "distribution has no optional parameters"))
#   }
#   else {
#     if (!is.numeric(ptemp)) 
#       stop("Default parameters must be a numeric vector")
#     if (!missing(parms)) {
#       temp <- unlist(parms)
#       indx <- match(names(temp), names(ptemp))
#       if (any(is.na(indx))) 
#         stop("Invalid parameter names")
#       ptemp[names(ptemp)] <- temp
#     }
#     parms <- ptemp
#   }
#   if (missing(control)) 
#     control <- survreg.control(...)
#   else control <- do.call("survreg.control", control)
#   if (any(scale < 0)) 
#     stop("Invalid scale value")
#   if (any(scale > 0) && nstrata > 1) 
#     stop("The scale argument is not valid with multiple strata")
#   pterms <- sapply(m, inherits, "coxph.penalty")
#   if (any(pterms)) {
#     pattr <- lapply(m[pterms], attributes)
#     temp <- c(attr(Terms, "response"), attr(Terms, "offset"))
#     if (length(dropx)) 
#       temp <- c(temp, dropx + 1)
#     pterms <- pterms[-temp]
#     temp <- match((names(pterms))[pterms], attr(Terms, "term.labels"))
#     ord <- attr(Terms, "order")[temp]
#     if (any(ord > 1)) 
#       stop("Penalty terms cannot be in an interaction")
#     assign <- attrassign(X, newTerms)
#     pcols <- assign[match(names(pterms[pterms]), names(assign))]
#     fit <- survpenal.fit(X, Y, weights, offset, init = init, 
#                          controlvals = control, dist = dlist, scale = scale, 
#                          strata = strata, nstrat = nstrata, pcols, pattr, 
#                          parms = parms, assign)
#   }
#   else fit <- survreg.fit(X, Y, weights, offset, init = init, 
#                           controlvals = control, dist = dlist, scale = scale, 
#                           nstrat = nstrata, strata, parms = parms)
#   if (is.character(fit)) 
#     fit <- list(fail = fit)
#   else {
#     if (scale == 0) {
#       nvar <- length(fit$coefficients) - nstrata
#       fit$scale <- exp(fit$coefficients[-(1:nvar)])
#       if (nstrata == 1) 
#         names(fit$scale) <- NULL
#       else names(fit$scale) <- levels(strata.keep)
#       fit$coefficients <- fit$coefficients[1:nvar]
#       fit$idf <- 1 + nstrata
#     }
#     else {
#       fit$scale <- scale
#       fit$idf <- 1
#     }
#     fit$loglik <- fit$loglik + logcorrect
#   }
#   if (!score) 
#     fit$score <- NULL
#   fit$df.residual <- n - sum(fit$df)
#   fit$terms <- Terms
#   fit$contrasts <- contr.save
#   if (length(xlevels)) 
#     fit$xlevels <- xlevels
#   fit$means <- apply(X, 2, mean)
#   if (!is.null(weights)) 
#     fit$weights <- weights
#   fit$call <- Call
#   fit$dist <- dist
#   if (model) 
#     fit$model <- m
#   if (x) 
#     fit$x <- X
#   if (y) 
#     fit$y <- Ysave
#   if (length(parms)) 
#     fit$parms <- parms
#   if (robust) {
#     fit$naive.var <- fit$var
#     if (!model) 
#       fit$model <- m
#     if (length(cluster)) 
#       fit$var <- crossprod(rowsum(residuals.survreg(fit, 
#                                                     "dfbeta"), cluster))
#     else fit$var <- crossprod(residuals.survreg(fit, "dfbeta"))
#     if (!model) 
#       fit$model <- NULL
#   }
#   singular <- (diag(fit$var) == 0)[1:length(fit$coef)]
#   if (any(singular)) 
#     fit$coef <- ifelse(singular, NA, fit$coef)
#   na.action <- attr(m, "na.action")
#   if (length(na.action)) 
#     fit$na.action <- na.action
#   if (any(pterms)) 
#     class(fit) <- c("survreg.penal", "survreg")
#   else class(fit) <- "survreg"
#   fit
# }
