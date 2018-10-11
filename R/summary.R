#' @title Summary one-way ANOVA
#' 
#' @description 
#' \code{summary.oneway} Summarize a oneway ANOVA results
#' 
#' @details 
#' This function summarises the oneway ANOVA results created by the oneway function
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
#' summary(mileage)

summary.oneway <- function(x, ...){
  if(!inherits(x, "oneway")) stop("x must  be class 'oneway'")
  print(anova(x$anova), ...)
}