#This function can return pooled covariance function given the input data
#X is matrix of curve. Each row means a single curve, each column means a obervation time. It should be
#already groupped when pass into the pooledcov()
#y is a vector of labels.
#'@title Pooled Covariance Function
#'@param Xlist A list of gruoped functional data.
#'@param t Time points
#'@import fdapace

pooledcov <- function(Xlist, t){

  nx=lapply(Xlist,function(x){
    nrow(x)
  })
  n=sum(unlist(nx))

  outlist=vector("list",length(Xlist))
  for (j in 1:length(Xlist)){
    LX0 <- lapply(seq_len(ncol(t(Xlist[[j]]))), function(i) Xlist[[j]][i, ])
    Lt0 <- rep(list(t), nx[[j]])
    outlist[[j]]=FPCA(LX0,Lt0)$fittedCov
  }


    Cov=0
  for (ii in 1:length(outlist)){
     Cov <- Cov+ (nx[[ii]] - 1) * outlist[[ii]]
     Cov <- Cov / (n - length(outlist) + 1)
  }


  return(list(Cov=Cov, singlecov=outlist))
}
