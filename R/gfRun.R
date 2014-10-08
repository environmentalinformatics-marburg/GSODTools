#' Julendat wrapper function controlling imputation of missing values
#' 
# #' @export gfRun
gfRun <- function(files.dep,
                  files.indep,
                  filepath.coords, 
                  quality.levels, 
                  gap.limit,
                  end.datetime,
                  units, 
                  na.limit,
                  time.window,
                  n.plot,
                  prm.dep, 
                  prm.indep, 
                  family, 
                  plevel,
                  ...) {
  
  ## Data import
  
  # Import plot coordinates
  if (!is.null(filepath.coords)) {
    data.coords <- read.csv(filepath.coords, header = T)
    data.coords <- data.coords[, c("PlotID", "Lon", "Lat")]
  } else {
    data.coords <- NULL
  }
  
  # Import data set of dependent plot
  ki.data.dep <- as.ki.data(files.dep)
  
  # Import data sets of independent plots
  ki.data.indep <- lapply(seq(files.indep), function(i) {
    as.ki.data(files.indep[i])
  })
  
  
  ## Rejection of records with bad quality flags
  
  # Loop through dependent parameters
  for (i in seq(prm.dep)) {
    
    # Apply quality control if desired
    if (!is.null(quality.levels)) {
      # Dependent plot
      ki.data.dep <- gfRejectLowQuality(data = ki.data.dep, 
                                        prm.dep = prm.dep[i], 
                                        quality.levels = quality.levels)
      
      # Independent plots
      ki.data.indep <- gfRejectLowQuality(data = ki.data.indep, 
                                          prm.dep = prm.dep[i], 
                                          quality.levels = quality.levels)
    }
    
    
    ## Imputation of missing values
    
    # Output list
    model.output <- list()
    
    # Missing value(s) to be imputed
    pos.na <- which(is.na(ki.data.dep@Parameter[[prm.dep[i]]]))
    
    # Calculate gap lengths and reject too large gaps
    if (length(pos.na) > 0) {
      pos.na <- gfGapLength(data.dep = ki.data.dep, 
                            pos.na = pos.na, 
                            gap.limit = gap.limit,
                            end.datetime = end.datetime, 
                            units = units)
      
      # Impute missing value(s)
      model.output <- lapply(seq(pos.na), function(j) {
        gfImputeMissingValue(data.dep = ki.data.dep, 
                             data.indep = ki.data.indep,
                             na.limit = na.limit, 
                             pos.na = as.numeric(pos.na[[j]]),
                             time.window = time.window,
                             data.coords = data.coords, 
                             n.plot = n.plot, 
                             prm.dep = prm.dep[i], 
                             prm.indep = prm.indep[i], 
                             family = family)
      })
      
      # Replace NA values by predicted values
      for (h in seq(pos.na)) {
        gap.start <- pos.na[[h]][,1]
        gap.end <- pos.na[[h]][,2]
        gap.span <- seq(gap.start, gap.end)
        
        ki.data.dep@Parameter[[prm.dep[i]]][gap.span] <- 
          round(unlist(lapply(seq(model.output[[h]]), function(l) {
            model.output[[h]][[l]][[4]]
          })), digits = 2)
      }
    }
  }
  
  
  # Set up and return output data
  data.output <- gfOutputData(data.dep = ki.data.dep, 
                              plevel = plevel)
  
  return(list(model.output, data.output))
}
