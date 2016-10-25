#' @title Median normalization of protein fold changes of 2D-TPP data
#'   
#' @description Normalizes fold changes retrieved from 2D-TPP experiment by dividing by the median fold
#'   change 
#'   
#' @examples 
#' data("panob2D_isobQuant_example")
#' cfg <- panobinostat_2DTPP_config
#' datRaw <- panobinostat_2DTPP_data
#' data2d <- tpp2dImport(cfg, datRaw, fcStr = NULL)
#' fcData2d <- tpp2dComputeFoldChanges(cfg, data2d)
#' normData2d <- tpp2dNormalize(cfg, fcData2d)
#'   
#' @param configTable data frame that specifies important details of the 2D-TPP experiment
#' @param data data frame that contains the data for the 2D-TPP experiment
#' @param fcStr character string indicating how columns that will contain the actual 
#'   fold change values will be called. The suffix \code{fcStr} will be pasted in front of
#'   the names of the experiments.
#'   
#' @return A dataframe identical to the input dataframe except that the columns containing the
#'   fold change values have been normalized by their median.
#'   
#' @export 
tpp2dNormalize <- function(configTable, data, fcStr="rel_fc_"){
  if (!any(grepl(fcStr, colnames(data)))){
    stop("Please specify a valid fcStr suffix matching the fold change columns!")
  } else{
    message("Performing median normalization...")
    norm.table <- do.call(rbind, lapply(unique(data$temperature), function(temp){
      # subset data to one temperature
      sub.table <- data[which(data$temperature==temp),]
      norm.list <- sapply(colnames(sub.table),function(coln){
        if (grepl(fcStr, coln)){
          col.median <- median(as.numeric(as.character(sub.table[[coln]])), 
                               na.rm=TRUE)
          norm.col <- as.numeric(as.character(sub.table[[coln]])) / col.median
          return(norm.col)
        }
      })
      norm.df <- data.frame(norm.list[!sapply(norm.list, is.null)])
      
      return(norm.df)
    }))
    colnames(norm.table) <- paste("norm", colnames(norm.table), sep="_")
    big.table <- cbind(data, norm.table)
    message("Done.")
    return(big.table)
  }
}
