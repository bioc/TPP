eSetsToLongTable_fData <- function(data){
  ## Extract feature data from expressionSets and store in long tables
  
  ## Initialize variables to prevent "no visible binding for global
  ## variable" NOTE by R CMD check:
  experiment = variable <- NULL
  
  expNames <- names(data)
  longTabAnnot <- c()
  for (en in expNames){
    datTmp <- data[[en]]
    
    # Retrieve protein ids, pData, fData and fold changes:
    ids <- featureNames(datTmp)
    fDatWide <- pData(featureData(datTmp))
    
    # Long table of fold changes:
    colnames(fDatWide) <- gsub("([^[:alnum:]])", "_", colnames(fDatWide))
    meltCols <- colnames(fDatWide)
      
    fDatWide <- fDatWide %>% mutate(id = ids) %>% mutate_all(as.character) 
    
    fDatLong <- fDatWide %>%
      tidyr::pivot_longer(cols = all_of(meltCols), names_to = "variable", values_to = "value") %>%
      mutate(experiment = en)
    
    longTabAnnot <- rbind(longTabAnnot, fDatLong)
  }
  
  longTabAnnot <- longTabAnnot %>% 
    arrange(id) %>%
    mutate(id = factor(id),
           experiment = factor(experiment),
           variable = factor(variable))
  
  return(longTabAnnot)
}
