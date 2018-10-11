#' @title One Way Analysis of Variance
#' 
#' @description 
#' \code{oneway} computes a oneway analysis of variance and inclues group
#' level summary statistics.
#' 
#' @details 
#' This function computes a standard oneway ANOVA, means, and standard deviations.
#' Missing values are handled via list-wise deletion
#' 
#' @param forumula an object of class formula, relating the dependent variable to the grouping
#' variable
#' @param data a data frame containing the variables in the model.
#' 
#' @export 
#' 
#' @return a list with 2 elements:
#' \item{oneway}{a list with lm results}
#' \item{summarystats}{a data frame with summary statistics}
#' 
#' @author Shota Nakamura <snakamura@@wesleyan.edu>
#' 
#' @example 
#' mileage = oneway(hwy ~ class, cars)
#' summary(mileage)
#' mileage
#' plot(mileage)
#' 
oneway <- function(formula, data) {
  
  # listwise deletion of missing values
  data_complete <- na.omit(data)
  
  # anova
  fit <- lm(formula, data_complete)
  
  stats <-  aggregate(formula,
                      data,
                      function(x) c(n = length(x), mean = mean(x), sd = sd(x)))
  
  
  result <- list(anova = fit, summarystats = stats)
  class(result) <- c("oneway", "list")
  return(result)
  
}
