#'@title Predict Function Using Quantile
#'@description Assign label to a single curve
#'@param X.curve A vector denotes the discrete points of a single curve.
#'@param obj An FOBJ object containing the information for classification.
#'@param weighted Whether multiple weight by \code{FOBJ} is to be used or not. Default the same to \code{obj$weighted}.
#'@import stats
#'@export
pre.quantile <- function(X.curve, obj, weighted = obj$weighted){

  if(! is.vector(X.curve)) stop("X is not a single curve")

  Xi0 <- getXi(X.curve, obj$phi, obj$t)
  L <- numeric(obj$K)

  if(!weighted){

     for(i in 1:obj$K){
      L[i] <- sum(sapply(1:length(obj$tau),
                         FUN = function(x){quantileloss(Xi0, obj$quantlist[[i]][x, ], obj$tau[x])}))
    }

    return(obj$y[which.min(L)])
  }

  else{
   for(i in 1:obj$K){
     L[i] <- sum(sapply(1:length(obj$tau),
                        FUN = function(x){
                          quantileloss(Xi0, obj$quantlist[[i]][x, ], obj$tau[x])
                          }) * obj$weight)
   }
    return(obj$y[which.min(L)])
  }

}
