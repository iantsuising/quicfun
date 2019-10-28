#'@title Get functional priciple componants
#'@description FPCA
#'@param Cov Covariance surface
#'@param FVEthreshold FVE
#'@param t Time points
#'@import fdapace
#'@references Yao F, Müller H G, Wang J L. Functional data analysis for sparse longitudinal data[J]. Journal of the American Statistical Association, 2005, 100(470): 577-590.
#'@export
getPhi <- function(Cov, FVEthreshold=99.99, t){
  eig <- eigen(Cov) #return a list with value and vector
  positiveInd <- eig[['values']] >= 0 #why not use dollar sign?
  if (sum(positiveInd) == 0) {
    stop('All eigenvalues are negative. The covariance estimate is incorrect.')
  }
  d <- eig[['values']][positiveInd] #same?

  eigenV <- eig[['vectors']][, positiveInd, drop=FALSE] #extract positive eigenvalue column


  FVE <- cumsum(d) / sum(d) * 100  # cumulative FVE for all available eigenvalues from fitted cov

  cutPoints <- min(which(FVE >= FVEthreshold)) # final number of component chosen based on FVE.Only 29 is choosen in demo.

  maxK <- cutPoints #maximum K candidate

  d <- d[1:maxK]

  proportion <- d/sum(d) #Contribution of each PC

  eigenV <- eigenV[, 1:maxK, drop=FALSE]


  # normalization
  muWork = 1:dim(eigenV)[1] #return the row number of eigenV. Since column need to be reduced


  phi0 <- apply(eigenV, 2, function(x) { #eigen V is matrix. 2 means apply function to column.
    x <- x / sqrt(fdapace::trapzRcpp(t, x^2))# divide each column by a constant. This integration see paper
    if ( 0 <= sum(x*muWork) )
      return(x)
    else
      return(-x)
  })

  return(list(phi = phi0, FVE = FVE, cutPoints = cutPoints, d = d, proportion = proportion))
}
