#'@title Sigmoid Function
#'@description Using Sigmoid function to smoothly assign a label
#'@param x vectors or values to be assigned
#'@param par.sig parameter for sigmoid function
#'@return 0 or 1, based on whether your input number or vector is positive or negative.
#'@examples
#' a <- runif(10, -1, 1)
#' b <- sigmoid(a)
#'@export
sigmoid <- function(x, par.sig=1000){
  res = 1/(1+exp(-par.sig*x));
  return(res)
}
