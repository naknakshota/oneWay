#' @title Print one-way ANOVA
#' 
#' @description 
#' \code{print.oneway} Prints a oneway anova 
#' 
#' @details 
#' This function prints oneway ANOVA results created by the oneway function
#' 
#' @param x an object of class one way \code{oneway}
#' @param ... additional arguments passed to the \code{\link{print}} function
#' 
#' @export 
#' 
#' @return input object is returned silently (x gets returned)
#' 
#' @author Shota Nakamura <snakamura@@wesleyan.edu>
#' 
#' @example 
#' mileage = oneway(hwy ~ class, cars)
#' print(mileage)

print.oneway <- function(x, ...){
  if(!inherits(x, "oneway")) stop("Must be class 'oneway'")
  cat("\nSummary Statistics\n", 
      "====================================================\n", sep="")
  print(x$summarystats)
  cat("\nAnova\n", 
      "====================================================\n", sep="")
  print(summary.lm(x$anova))
}
