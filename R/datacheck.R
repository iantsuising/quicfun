
#This function can be used for data check
#' @title Check Input Data Tyoe
#' @description \code{datacheck()} is to check whether the data to be input is appropriate
#' @param data The input matrix of observed values. Rows represent repititions or samples, and columns represent observed time points
#' @param label The input vector of labels. ith element of this vector should correspond to the label of ith row of data matrix X.
#' @param time An input time grid vector
#' @return NULL is appropriate, otherwise warning
#' @examples
#'  library(refund)
#'  data(DTI)
#'  X <- DTI$cca
#'  y <- DTI$case
#'  t <- seq(0, 1, length.out = ncol(X))
#'   # not run
#'   # datacheck(X, y, t)
#'  allData <- cbind(X, y)
#'  allData <- allData[which(DTI$visit == 1), ]
#'  allData <- na.omit(allData)
#'  y <- allData[, ncol(allData)]
#'  X <- allData[, -ncol(allData)]
#'  t <- seq(0, 1, length.out = ncol(X))
#'  datacheck(X, y, t)
#'@export
datacheck <-function(data, label, time){
  if (!is.vector(label)){
    stop("Input label should be vector \n")
  }
  if (!is.vector(time)){
    stop("Input time should be vector \n")
  }
  if (!is.numeric(time)){
    stop("Input time should be numbers \n")
  }
  if (!is.matrix(data)){
    stop("Input data should be matrix \n")
  }
  if (nrow(data) != length(label)){
    stop("The amount of rows of input data should be the same as the length of input label \n")
  }
  if (ncol(data) != length(time)){
    stop("The amount of cloumns of input data should be the same as the length of input time \n")
  }
  if (sum(is.na(data)) > 0){
    stop("NA is not permitted")
  }
  if (length(unique(label))>=3){
    warning("The outcome may not be reliable if there are more than 2 classes")
  }
  return(NULL)
}
