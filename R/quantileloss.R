#This function will compute quantile loss for a single curve(equation in page 7. L)
#Xi is a vector/matrix of projection score value for ith curve, q is empirical quantile, tau is value of tau
#Will return a matrix, column length equal to the number of projection scores.
#tau here should be a single tau.
#q is vector, should not be matrix
#'@title Computing Quantileloss
#'@description Compute quantile loss for a single curve
#'@param Xi A vector/matrix of projection score value for ith curve
#'@param q Emperical quantile
#'@param tau Single tau
#'@references To be confirmed
#'@export
quantileloss <- function(Xi, q, tau){
  if(is.vector(Xi)){
    res <- abs(Xi - q) * ((tau * (Xi > q)) + (1 - tau)*(Xi <= q))
  }

  else{
    s.temp <- Xi - matrix(rep(q, nrow(Xi)), nrow(Xi), length(q), byrow = T)
    res <- tau * s.temp - s.temp * (s.temp < 0)
    # res=t(apply(Xi,1,function(x){
    #   res=tau*(x-q)-(x-q)*(x<q) #use apply to repalce loop
    # }))
  }
  return(res)
}
