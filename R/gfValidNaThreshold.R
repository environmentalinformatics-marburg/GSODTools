#' Julendat function to calculate the amount of missing values
#' 
# #' @export gfValidNaThreshold
gfValidNaThreshold <- function(data.indep,
                               prm.dep = "Ta_200",
                               na.limit = 0.2, 
                               time.window.pre,
                               time.window.post,
                               pos.na,
                               ...) {
  
  # Time sequences before and after gap
  time.window.pre.seq <- seq(time.window.pre, pos.na[1] - 1)
  time.window.post.seq <- seq(pos.na[2] + 1, time.window.post)
  
  # Proportion of NA values in given time window
  na.ratio <- lapply(seq(data.indep), function(h) {
    sum(is.na(data.indep[[h]]@Parameter[[prm.dep]][c(time.window.pre.seq, time.window.post.seq)])) / length(c(time.window.pre.seq, time.window.post.seq))
  })
  
  # Reject plots with too high number of NA values
  data.indep <- data.indep[unlist(na.ratio) <= na.limit]
  return(data.indep)
}  
  
