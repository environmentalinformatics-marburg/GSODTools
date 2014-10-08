#' Julendat wrapper function for imputation of missing values
#' 
# #' @export gfImputeMissingValue
gfImputeMissingValue <- function(data.dep, 
                                 data.indep,
                                 na.limit = 0.2,
                                 time.window, 
                                 pos.na, 
                                 data.coords,
                                 n.plot = 10, 
                                 prm.dep = "Ta_200",
                                 prm.indep = NA,
                                 family = gaussian,
                                 ...) {
  
  # Required packages
  library(gmt)
  
  # Revision of defined time window with respect to the current gap before ...
  time.window.pre <- pos.na[1] - time.window
  time.window.pre <- ifelse(time.window.pre > 0, time.window.pre, 1)
  time.window.pre.span <- pos.na[1] - time.window.pre
  # ...and after gap
  time.window.post <- pos.na[2] + time.window
  time.window.post <- ifelse(time.window.post <= length(data.dep@Parameter[[prm.dep]]), 
                             time.window.post, 
                             length(data.dep@Parameter[[prm.dep]]))
  time.window.post.span <- time.window.post - pos.na[2]
  
  
  ## Rejection of independent data sets with a too high amount of NA values
  
  data.indep <- gfValidNaThreshold(data.indep = data.indep,
                                   prm.dep = prm.dep, 
                                   na.limit = na.limit, 
                                   time.window.pre = time.window.pre,
                                   time.window.post = time.window.post,
                                   pos.na = pos.na)
  
  
  # Abandon imputation of current gap if there are no valid independent plots
  if (length(data.indep) == 0) {
    
    # Error message and breakup of current iteration
    print(paste("Filling of gap from", pos.na[1], "to", pos.na[2], "not possible."))
    return(lapply(seq(pos.na[1], pos.na[2]), function(i) {
      list(data.dep@Datetime[i],
           data.dep@PlotId$Unique,
           prm.dep,
           NA,
           NA,
           NA,
           prm.indep,
           NA)
    }))
  }
  
  
  ## Identify plots with valid measurements at the given NA position
  
  data.indep.avl <- gfNonNaStations(data.indep = data.indep, 
                                    pos.na = pos.na, 
                                    prm.dep = prm.dep)
  
  # Abandon imputation of current gap if there are no valid independent plots
  if (sum(data.indep.avl[,2]) == 0) {
    
    # Error message and breakup of current iteration
    print(paste("Filling of gap from", pos.na[1], "to", pos.na[2], "not possible."))
    return(lapply(seq(pos.na[1], pos.na[2]), function(i) {
      list(data.dep@Datetime[i],
           data.dep@PlotId$Unique,
           prm.dep,
           NA,
           NA,
           NA,
           prm.indep,
           NA)
    }))
  }
  
  
  ## Calculate distance between independent plots and dependent plot (optional)
  
  if (!is.null(data.coords)) {
    data.indep.avl[,3] <- unlist(lapply(seq(data.indep.avl[, 1]), function(k) {
      geodist(Nfrom = data.coords[which(data.coords[, 1] == data.dep@PlotId$Unique),"Lat"], 
              Efrom = data.coords[which(data.coords[, 1] == data.dep@PlotId$Unique),"Lon"], 
              Nto = data.coords[which(data.coords[, 1] == data.indep.avl[k, 1]), "Lat"], 
              Eto = data.coords[which(data.coords[, 1] == data.indep.avl[k, 1]), "Lon"])
    }))
    
    # Order independent stations by distance from the dependent plot
    data.indep.avl <- data.indep.avl[order(data.indep.avl[,3]),]
    data.indep <- data.indep[as.numeric(row.names(data.indep.avl))]
  }
  
  
  ## Merge monthly data sets of dependent plot and independent plots
  
  data.prm.cc <- gfCompleteMonthlyCases(data.dep = data.dep, 
                                        data.indep = data.indep, 
                                        data.indep.avl = data.indep.avl, 
                                        n.plot = n.plot, 
                                        prm.dep = prm.dep,
                                        time.window.pre = time.window.pre, 
                                        time.window.post = time.window.post, 
                                        pos.na = pos.na,
                                        prm.indep = prm.indep)
  
  # Abandon imputation of current gap if there are no complete cases
  if (nrow(data.prm.cc[[2]]) == 0) {
    
    # Error message and breakup of current iteration
    print(paste("Filling of gap from", pos.na[1], "to", pos.na[2], "not possible."))
    return(lapply(seq(pos.na[1], pos.na[2]), function(i) {
      list(data.dep@Datetime[i],
           data.dep@PlotId$Unique,
           prm.dep,
           NA,
           NA,
           NA,
           prm.indep,
           NA)
    }))
  }
  
  
  ## Fit generalized linear model 
  
  data.prm.cc.lm <- 
    gfComputeLinearModel(data = data.prm.cc[[1]], 
                         data.cc = data.prm.cc[[2]], 
                         data.dep = data.dep, 
                         family = family, 
                         pos.na = pos.na, 
                         plots = data.indep.avl,
                         n.plot = n.plot, 
                         prm.dep = prm.dep, 
                         prm.indep = prm.indep, 
                         time.window.pre = time.window.pre, 
                         time.window.pre.span = time.window.pre.span, 
                         time.window.post = time.window.post,
                         time.window.post.span = time.window.post.span)
  
  # Return output
  return(data.prm.cc.lm)
}
