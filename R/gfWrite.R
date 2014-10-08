#' Julendat wrapper function for gfRun.R
#' 
# #' @export gfWrite
gfWrite <- function(files.dep, 
                    files.indep, 
                    filepath.output, 
                    filepath.coords = NULL, 
                    quality.levels = NULL, 
                    gap.limit,
                    end.datetime,
                    units = "hours", 
                    na.limit,
                    time.window,
                    n.plot,
                    prm.dep, 
                    prm.indep, 
                    family,
                    plevel,
                    ...) {
  
  # Load function 'gfRun'
  source("gfRun.R")
  
  # Perform imputation
  imputation.data <- gfRun(files.dep = files.dep,
                           files.indep = files.indep,
                           filepath.coords = filepath.coords, 
                           quality.levels = quality.levels,
                           gap.limit = gap.limit,
                           end.datetime = end.datetime,
                           units = units, 
                           na.limit = na.limit,
                           time.window = time.window,
                           n.plot = n.plot,
                           prm.dep = prm.dep, 
                           prm.indep = prm.indep, 
                           family = family, 
                           plevel = plevel)
  
  # Write output table
  write.table(imputation.data[[2]], 
              filepath.output, 
              sep = ",", row.names = FALSE, col.names = TRUE)
  
}
