#' Identification of plots with valid contemporary measurements
#' 
# #' @export gfCompleteMonthlyCases
gfCompleteMonthlyCases <- function(data.dep, 
                                   data.indep,
                                   data.indep.avl,
                                   n.plot = 10,
                                   prm.dep = "Ta_200",
                                   time.window.pre, 
                                   time.window.post, 
                                   pos.na, 
                                   prm.indep = NA,
                                   ...) {
  
  # Merge lists containing ki.data objects
  data <- append(data.dep, data.indep)
  
  # Convert data.indep.avl to list
  data.indep.avl <- lapply(seq(data.indep.avl[, 2]), function(i) {
    data.indep.avl[i, 2]
  })
  
  # Merge logical lists indicating which stations should be considered
  data.avl <- append(list(T), data.indep.avl)

  # Reassign n.plot in case number of valid plots < n.plot
  if (sum(unlist(data.avl)) < n.plot + 1)
    n.plot <- sum(unlist(data.avl)) - 1

  # List measured values that lie within the given timespan for each valid station
  data.avl.prm <- lapply(which(unlist(data.avl))[1:(n.plot+1)], function(i) {
      data[[i]]@Parameter[[prm.dep]][seq(time.window.pre, time.window.post)]
  })

  # Retrieve dates
  data.avl.date <- list(data[[1]]@Datetime[seq(time.window.pre, time.window.post)])

  # Measured values of independent parameters
  if (!is.na(prm.indep)) {
    data.prm.indep <- list(data[[1]]@Parameter[[prm.indep]][seq(time.window.pre, time.window.post)])
  } else {
    data.prm.indep <- list()
  }

  # Merge dates and measured values
  data.temp <- append(data.avl.date, data.prm.indep)
  data.avl.comp <- append(data.temp, data.avl.prm)

  # Rename list elements
  if (!is.na(prm.indep)) {
    names(data.avl.comp) <- c("Datetime", prm.indep, rep(prm.dep, length(data.avl.prm)))
  } else {
    names(data.avl.comp) <- c("Datetime", rep(prm.dep, length(data.avl.prm)))
  }

  # Merge time series into data.frame
  data.avl.prm.merge <- do.call("data.frame", data.avl.comp)

  # Select complete cases only
  data.avl.prm.merge.cc <- data.avl.prm.merge[complete.cases(data.avl.prm.merge),]

  # Return data.frame containing complete cases of monthly data sets
  return(list(data.avl.prm.merge, data.avl.prm.merge.cc))
}
