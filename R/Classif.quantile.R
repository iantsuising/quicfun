#'@title Quantile Classifier
#'@description Quantile classifier
#'@param obj An FOBJ for classification
#'@param testset Must be a matrix of testset. Each row is an observation of curve. In terms of the prediction to single curve, view \code{pre.quantile}
#'@param testlabel Testlabel, which length should be equal to the number of rows of testset
#'@param weighted Logical. It is to define whether weight is to be used. Default the same to \code{obj$weighted}.
#'@return A list containing the classification result
#'\item{MCR}{The total misclassification}
#'\item{pre}{The predictive result}
#'\item{FP}{False positive rate if only two groups}
#'\item{FN}{False negative rate if only two groups}
#'@examples
#'rm(list = ls())
#'library(refund)
#'data(DTI)
#'X <- DTI$cca
#'y <- DTI$case
#'t <- seq(0, 1, length.out = ncol(X))
#' #not run
#' #datacheck(X, y, t)
#' allData <- cbind(X, y)
#' allData <- allData[which(DTI$visit == 1), ]
#' allData <- na.omit(allData)
#' y <- allData[, ncol(allData)]
#' X <- allData[, -ncol(allData)]
#' t <- seq(0, 1, length.out = ncol(X))
#' datacheck(X, y, t)
#' Index_0 <- which(y == 0)
#' Index_1 <- which(y == 1)
#'  SplitPara <- 0.8 #Split parameter
#'  trainIndex_0 <- sample(Index_0, SplitPara * length(Index_0))
#'  testIndex_0 <- Index_0[-trainIndex_0]
#'  trainIndex_1 <- sample(Index_1, SplitPara * length(Index_1))
#'  testIndex_1 <- numeric()
#'  for(i in Index_1){
#'      if(i %in% trainIndex_1 ==FALSE){
#'         testIndex_1 <- append(testIndex_1, i)
#'        }
#'     }
#'  trainset <- X[c(trainIndex_0, trainIndex_1), ]
#'  trainlabel <- y[c(trainIndex_0, trainIndex_1)]
#'   testset <- X[c(testIndex_0, testIndex_1), ]
#'  testlabel <- y[c(testIndex_0, testIndex_1)]
#'  w1 <- FOBJ(trainset, trainlabel, t)
#'  c1 <- Classif.quantile(w1, testset, testlabel, weighted = FALSE)
#'@export

Classif.quantile <- function(obj, testset, testlabel, weighted = obj$weighted){

 if(is.vector(testset)){stop("Not a matrix")}

  datacheck(testset, testlabel, obj$t)
  cat(paste0("The weight of FOBJ is", as.character(obj$weighted)))

  if(! weighted){

    pre <- apply(testset, 1, FUN = function(x){pre.quantile(x, obj, weighted = FALSE)})

  }

  else{
    pre <- apply(testset, 1, FUN = function(x){pre.quantile(x, obj, weighted = TRUE)})

  }
  mcr <- length(which(pre != testlabel))/length(testlabel)

  if(obj$K != 2){
    return(list(MCR = mcr, pre = pre))
  }

  else{
    fp <- length(which(testlabel != pre & testlabel == obj$y[1]))/length(which(testlabel == obj$y[1]))
    fn <- length(which(testlabel != pre & testlabel == obj$y[2]))/length(which(testlabel == obj$y[2]))
    return(list(MCR = mcr, FP = fp, FN = fn, pre = pre))
  }
}
