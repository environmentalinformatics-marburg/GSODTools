#' Julendat identification of plots with valid measurements
#' 
# #' @export gfNonNaStations
gfNonNaStations <- function(data.indep, 
                            pos.na, 
                            prm.dep = "Ta_200", 
                            ...) {
      
  # Identify plots with available records for the given gap
  data.temp <- lapply(seq(data.indep), function(i) {
    (pos.na[1]:pos.na[2]) %in% which(is.na(data.indep[[i]]@Parameter[[prm.dep]]))
  })
  data.avl <- lapply(seq(data.indep), function(i) {
    sum(data.temp[[i]]) == 0
  })

  # Plot names
  plot.avl <- lapply(seq(data.indep), function(i) {
    data.indep[[i]]@PlotId$Unique
  })
  
  # Return data.frame containing plot names and information about availablity of particular record
  return(data.frame(unlist(plot.avl), 
                    unlist(data.avl), 
                    stringsAsFactors = F))
}
