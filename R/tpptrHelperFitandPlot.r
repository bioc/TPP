tpptrHelperFitandPlot <- function(p, yMat, xMat, ciMat, startPars, maxAttempts, 
                                  expNames, verbose, ggplotTheme, grConditions, 
                                  compDF, addLegend, resultPath, plotPathsFull, 
                                  useCI, doPlot){
  # Helper function that combines the code for fitting and plotting of melting 
  # curves for parallel or sequential (in lapply) execution
  
  ## Initialize variables to prevent "no visible binding for global
  ## variable" NOTE by R CMD check:
  protID <- NULL
  
  # retrieving options via getOptions doesn't work with parallel execution
  # therefore options are passed as variables (useCI, doPlot)
  
  yDF = subset(yMat, protID==p)
  
  resFC <- fitMeltCurves(xMat, yDF=yDF, colPrefix = "FC",
                         startPars=startPars,maxAttempts=maxAttempts, 
                         expNames=expNames, 
                         protID=p, verbose=verbose)
  
  # if we ant to use the CIs give the subseit of the CI matrix to the function
  # otherwise NULL
  if (useCI) {
    ciDF=subset(ciMat, protID==p)
    ciDF=ciDF[match(yDF$expName, ciDF$expName),]
    
    ciDFUpper <- ciDFLower <- ciDF
    ciDFUpper[,-(1:2)] <- yDF[,-(1:2)] + ciDF[,-(1:2)] / 2
    ciDFLower[,-(1:2)] <- yDF[,-(1:2)] - ciDF[,-(1:2)] / 2
    
    resUpper <- fitMeltCurves(xMat, yDF=ciDFUpper, colPrefix = "CI",
                              startPars=startPars,maxAttempts=maxAttempts, 
                              expNames=expNames, 
                              protID=p, verbose=verbose)
    
    resLower <- fitMeltCurves(xMat, yDF=ciDFLower, colPrefix = "CI",
                              startPars=startPars,maxAttempts=maxAttempts, 
                              expNames=expNames, 
                              protID=p, verbose=verbose)
    
    listUpper = resUpper[[3]]
    listLower = resLower[[3]]
    
    CI_reportData = data.frame(CI_meltPointUpper = resUpper[[1]]$meltPoint,
                               CI_meltPointLower = resLower[[1]]$meltPoint,
                               CI_meltPoint_delta = resUpper[[1]]$meltPoint - resLower[[1]]$meltPoint)
    
  } else {  
    listUpper = listLower = NULL
  }
  
  if(doPlot){
    pl <- plotMeltingCurve(modelList = resFC[[3]], 
                           listUpper = listUpper, 
                           listLower = listLower,
                           xMat = xMat, 
                           fcMat = resFC[[2]], 
                           curvePars = resFC[[1]], 
                           protID = p,
                           plotTheme = ggplotTheme, 
                           expConditions = grConditions, 
                           expComps = compDF, 
                           addLegend = addLegend, 
                           useCI=useCI)
    
    if(is.null(pl)){
      plotPathRel <- NA_character_
    } else {
      plotPathRel <- plotPathsFull[p]
      pdf(file=file.path(resultPath, plotPathRel), width=7.87, height=9.84, 
          useDingbats=FALSE)
      grid.draw(pl)
      dev.off()
      
    }
  } else {
    plotPathRel <- NA_character_
  }
  
  curveParsWholeProt <- resFC[[1]]
  curveParsWholeProt$protID <- p
  curveParsWholeProt$plot   <- plotPathRel
  curveParsWholeProt$expName <- expNames  
  
  if (useCI){
    curveParsWholeProt = cbind(curveParsWholeProt, CI_reportData)
  }
  
  curveParsWholeProt
}

