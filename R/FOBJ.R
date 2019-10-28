#'@title Functional Objection
#'@description \code{FOBJ()} is used to compute imformation for within FDA framework.
#'@param X The input matrix of observed values. Rows represent repititions or samples, and columns represent observed time points.
#'@param y The input vector of labels. ith element of this vector should correspond to the label of ith row of data matrix X.
#'@param t An input time grid vector.
#'@param optns A list to control computing such as smoothing, threshold and quantile range.
#'@param weighted Whether weight for quantile loss is to be used. \code{FALSE} is default.
#'@return  It returns a class "FOBJ".
#'\item{DataGroupby}{The list group by the label of each group}
#'\item{y}{Vector. Label of each group}
#'\item{t}{Original time points of observation}
#'\item{K}{Numeber. How many groups}
#'\item{phi}{Matrix. The functional principle componants, listed by colums}
#'\item{scores}{List. The projection scores of each group}
#'\item{cutPoints}{Numebr. How many componants}
#'\item{quantlist}{List. The quantile of projection scores of each group}
#'\item{tau}{Vector. Quantile counter}
#'\item{weight}{Matrix. Weight for classification. Default is matrix 1. The number of colums is equal to \code{tau}}
#'\item{optns}{List. The same to the original optns}
#'\item{weighted}{Logical. The same to the parameter "weighted"}
#'\item{propotion}{Vector. Contains the proportion of variance of each FPC}
#'@import fdapace
#'@import stats
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
#' w1 <- FOBJ(X, y, t)
#'@export


FOBJ <- function(X, y, t, optns = list(smooth=FALSE, FVEthreshold=99.99, a0=0.02, M0=10, par.sig=1000), weighted = FALSE){

   datacheck(X, y, t) #write a sub function to check data format

  origiTime <- t #save t
  #Whether smooth or not
  if (optns$smooth == 1){
    smoothlist <- apply(X, 1, function(x){  #use apply() to smooth
      spline(t,x,method = "natural")
    })
    t <- smoothlist[[1]]$x #replace original t with smoothed t
    X <- matrix(0, nrow = nrow(X), ncol = length(t))
    for (i in 1:nrow(X)){
      X[i, ] <- smoothlist[[i]]$y
    } #repalce original X with smoothed X
  }
  #group X according to label into group X0 and X1 as input for pooledcov(). Here is K class

  K <- length(unique(y))
  Xlist <- vector("list", K)
  bindX <- cbind(X, y)
  #construct index list to record original sequence of labels
  ind <- vector("list",K)
  for (j in 1:K){

    ind[[j]] <- which(bindX[, ncol(bindX)]==(unique(y))[j]) #record row index for each group i

   }

  names(ind) <- c(unique(y)[1: K])

  for (i in 1:K){
    Xlist[[i]] <- bindX[y==(unique(y))[i],]
    Xlist[[i]] <- matrix(as.numeric(Xlist[[i]][, -ncol(bindX)]),nrow(Xlist[[i]]))
  }
  names(Xlist) <- c(unique(y)[1:K]) #assign names to Xlist

  # get covariance function
  Covlist=pooledcov(Xlist,t)

  #Compute basis function by eigendecomposition
  temp.phi <- getPhi(Covlist$Cov, optns$FVEthreshold, t)

  fitphi <- temp.phi$phi

  eigenV <- temp.phi$eigenV

  cutPoints <- temp.phi$cutPoints

  proportion <- temp.phi$proportion

  # compute xi
  xilist=vector("list",length(Xlist))
  for (jj in 1:length(Xlist)){
    xilist[[jj]] <- getXi(Xlist[[jj]], fitphi[, 1:temp.phi$cutPoints], t)
  }
  names(xilist) <- c(unique(y)[1:K])
  J=ncol(xilist[[1]])

  #compute quantile for each column(projection scores).
  qlist=vector("list",length(Xlist))
  a0=optns$a0
  M0=optns$M0
  theta=seq(a0, 1-a0, (1-2*a0)/(M0-1))
  Ltheta=(1:length(theta))

  for (kk in 1:length(Xlist)){ #quantile matrix for each group
    qlist[[kk]]=apply(xilist[[kk]], 2, function(x){ #replace all loops by apply()
      quantile(x,theta)
    })
  }
  names(qlist) <- c(unique(y)[1:K])

  #compute weight for quantile classifier
  if(!weighted){
    weight <- rep(1, J *length(theta))
    dim(weight <- c(J, length(theta)))
  }

  else{
   weight <- rep(0, J * length(theta))

  scorelist <- vector("list",length(Xlist))

  for (K in 1:length(Xlist)){
    for (m in 1:length(Xlist)){
      scorelist[[K]][[m]]=lapply(Ltheta,function(x){
        quantileloss(xilist[[K]],qlist[[m]][x,  ],theta[x])
      })
    }

  }
  names(scorelist) <- c(unique(y)[1:K])

  #convert scorelist to array list
  arraylist=vector("list",length(Xlist))
  for (k in 1:length(Xlist)){
    for (m in 1:length(Xlist)){
      arraylist[[k]][[m]]=array(as.numeric(unlist(scorelist[[k]][[m]])), dim=c(nrow(xilist[[k]]), J, length(theta)))
    }

  }
  names(arraylist) <- c(unique(y)[1:K])

  #convert array to long matrix list
  scorematrixlist=vector("list",length(Xlist))
  for (k in 1:length(Xlist)){
    for (m in 1:length(Xlist)){
      scorematrixlist[[k]][[m]]=matrix(arraylist[[k]][[m]], nrow(xilist[[k]]), (J)*length(theta))
    }
  }
  names(scorematrixlist) <- c(unique(y)[1:K])

  optwei <- findweights(scorematrixlist = scorematrixlist,weight = weight,par.sig = optns$par.sig)$par
  weight <- optwei
  dim(weight) <- c(J, length(theta))
  }

  out <- list(DataGroupby = Xlist, y = unique(y), t = origiTime,
              K = K, poolCov = Covlist$Cov, singleCov = Covlist$singlecov,
              phi = fitphi, scores = xilist, cutPoints = cutPoints,
              proportion = proportion,
              quantlist = qlist, tau = theta, weight = weight, optns = optns,
              weighted = weighted)

  class(out) = "FOBJ"

  return(out)
}
