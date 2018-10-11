#' @title Plot one-way ANOVA
#' 
#' @description 
#' \code{plot.oneway} Plots a oneway anova 
#' 
#' @details 
#' This function creates side by side boxplots for each group in analysis
#' 
#' @param x an object of class one way \code{oneway}
#' @param col Fill color for boxplots
#' @param ... additional arguments passed to the \code{\link{boxplot}} function
#' 
#' @export 
#' 
#' @return NULL
#' 
#' @author Shota Nakamura <snakamura@@wesleyan.edu>
#' 
#' @example 
#' mileage = oneway(hwy ~ class, cars)
#' plot(mileage)

plot.oneway <- function(x, ...){
  if(!inherits(x, "oneway")) stop("Must be class 'oneway'")
  boxplot(x$anova$terms, x$anova$model, col="skyblue")
}

print.oneway <- function(x, ...){
  if(!inherits(x, "oneway")) stop("Must be class 'oneway'")
  cat("\nSummary Statistics\n", 
      "====================================================\n", sep="")
  print(x$summarystats)
  cat("\nAnova\n", 
      "====================================================\n", sep="")
  print(summary.lm(x$anova))
}