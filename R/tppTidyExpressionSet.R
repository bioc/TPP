.tppTidyExpressionSet <- function(eset) {
  if (!inherits(eset, "ExpressionSet")) {
    stop("'eset' must be an ExpressionSet object.")
  }
  
  expressionData <- Biobase::exprs(eset)
  
  if (is.null(rownames(expressionData))) {
    stop("The ExpressionSet must have feature names in its expression matrix.")
  }

  gene <- NULL
  
  tidyExpressionSet <- tibble::as_tibble(
    expressionData,
    rownames="gene"
  ) %>%
    tidyr::pivot_longer(
      cols=-gene,
      names_to="sample",
      values_to="value"
    )
  
  return(tidyExpressionSet)
}