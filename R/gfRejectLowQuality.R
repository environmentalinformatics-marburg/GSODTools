#' Julendat function to assure measurement quality
#' 
# #' @export gfRejectLowQuality
gfRejectLowQuality <- function(data, 
                               prm.dep = "Ta_200",
                               quality.levels, 
                               ...) {
    
  # Column with specified parameter in ki.data slot @Parameter
  prm.dep.col <- ifelse(!is.list(data), 
                        which(names(data@Parameter) == prm.dep), 
                        which(names(data[[1]]@Parameter) == prm.dep))

  # Limits for quality flag extraction
  if (prm.dep.col == 1) {
    substr.min <- 1 + prm.dep.col + (prm.dep.col - 1) * 2
    substr.max <- substr.min + 2
  } else {  
  substr.min <- prm.dep.col * 3 - 1
  substr.max <- substr.min + 2
  }
  
  if(!is.list(data)) {
    # Identify measurements that yield a bad quality flag
    pos.na.qc <- unique(c(which(is.na(data@Parameter[[prm.dep[1]]])), which(as.numeric(substr(data@Qualityflag, substr.min, substr.max)) %in% quality.levels)))
    # Replace invalid measurements by NA
    data@Parameter[[prm.dep]][pos.na.qc] <- NaN
    # Update quality flag
    data@Qualityflag[pos.na.qc] <- paste(substr(data@Qualityflag[pos.na.qc], 1, substr.min - 1), 
                                         as.character(as.numeric(substr(data@Qualityflag[pos.na.qc], substr.min, substr.max)) + 200), 
                                         substr(data@Qualityflag[pos.na.qc], substr.max + 1, nchar(data@Qualityflag[pos.na.qc])), sep = "")
    data@Qualityflag[-pos.na.qc] <- paste(substr(data@Qualityflag[-pos.na.qc], 1, substr.min - 1), 
                                         as.character(as.numeric(substr(data@Qualityflag[-pos.na.qc], substr.min, substr.max)) + 100), 
                                         substr(data@Qualityflag[-pos.na.qc], substr.max + 1, nchar(data@Qualityflag[-pos.na.qc])), sep = "")
    
  } else {
    lapply(seq(data), function(i) {
      pos.na.qc <- unique(c(data[[i]]@Valid$NAIndex, which(as.numeric(substr(data[[i]]@Qualityflag, substr.min, substr.max)) %in% quality.levels)))
      data[[i]]@Parameter[[prm.dep]][pos.na.qc] <- NaN
    })
  }
  
  # Output
  return(data)
}
