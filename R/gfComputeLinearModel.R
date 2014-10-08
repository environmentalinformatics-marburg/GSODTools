#' Julendat gap-filling based on linear modelling
#' 
# #' @export gfComputeLinearModel
gfComputeLinearModel <- function(data = NULL, 
                                 data.cc = NULL,
                                 data.dep,
                                 family = gaussian,
                                 pos.na,
                                 plots = NULL,
                                 n.plot = 10,
                                 prm.dep = "Ta_200",
                                 prm.indep = NA,
                                 time.window.pre, 
                                 time.window.pre.span,
                                 time.window.post,
                                 time.window.post.span,
                                 ...) {
  
  # In case of one plot with dependent and independent parameters
  if (is.null(data))
    data <- as.data.frame(cbind(data.dep@Datetime, 
                                unlist(data.dep@Parameter[prm.indep]), 
                                unlist(data.dep@Parameter[prm.dep])), 
                          row.names = F)
  
  # Replicate data of dependent plot in case is.null(data.cc) == TRUE
  if (is.null(data.cc))
    data.cc <- data
  
  # Column names of dependent and independent plots and parameters
  coln.dep <- names(data.cc)[which(names(data.cc) == prm.dep)]
  coln.indep <- names(data.cc)[-c(1, which(names(data.cc) == prm.dep))]
  
  # Continue if there is at least one valid independent plot
  if (length(coln.indep) > 0) {
  
  # Formula for computation of linear model
  formula <- sapply(list(seq(coln.indep)), function(i) {
    paste(coln.dep, "~", paste(coln.indep[i], collapse=" + "))
  })
  
  # Linear model
  model <- glm(formula, data = data.cc, family = family)
  # Calculate r-squared
  r.squ <- cor(data.cc[,prm.dep], predict(model))^2 
  
  # Formula for imputation of missing value
  lm.formula <- sapply(list(2:length(model$coefficients)), function(i) {
    paste(model$coefficients[1], 
          paste(model$coefficients[i], " * data$", names(model$coefficients[i]), "[pos.na]", sep="", collapse=" + "), sep=" + ")
  })
  
  # Loop through single NA values 
  lm.fitted <- lapply(seq(time.window.pre.span + 1, time.window.pre.span + pos.na[3]), function(h) {
    # Fitted value at pos.na
    sum(unlist(sapply(list(2:length(model$coefficients)), function(i) {
      model$coefficients[i] * data[h,names(model$coefficients[i])]
    }))) + model$coefficients[1]
  })
  
  # Reassign n.plot in case number of valid plots < n.plot
  if (!is.null(plots) && sum(plots[,2]) < n.plot)
    n.plot <- sum(plots[,2])
  
  # Output
  return(lapply(seq(pos.na[1], pos.na[2]), function(i) {
    list(data.dep@Datetime[i],
         data.dep@PlotId$Unique,
         prm.dep,
         lm.fitted[[i-pos.na[1]+1]],
         lm.formula,
         r.squ,
         prm.indep,
         ifelse(!is.null(plots) && n.plot != 0, paste(plots[which(plots[,2])[1:n.plot],1], collapse=", "), NA))
  }))
  
  } else {
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
}  
