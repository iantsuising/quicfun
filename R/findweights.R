#'@title Find optimal weights based on the quantileloss
#'@description A weight is multiplied to the quantileloss to select the important projection scores.
#'@param weight The initial weight to be optimized
#'@param scorematrixlist A list of quantileloss matrices
#'@param par.sig The parameter in sigmoid function, default is 1000
#'@return It returns a list, which is the output of \code{optim()} function. Details can be seen in \code{\link{optim}}.
#'@import stats

findweights <- function(weight, scorematrixlist, par.sig = 1000){
  K=length(scorematrixlist)

  diffmatrix <- vector("list",K*(K-1))

  count <- 1
    for (i in 1:length(scorematrixlist)){  #generate list of diffmatrix
      for (j in 1:length(scorematrixlist)){
        if (i!=j){
          diffmatrix[[count]] <- scorematrixlist[[i]][[j]]-scorematrixlist[[i]][[i]]
          count <- count + 1
        }
      }
    }

    fn <- function(x){  #generalized function
      res <- 0
      for (i in 1:length(scorematrixlist)){
        temp <- do.call(rbind, diffmatrix[((i-1)*(K-1)+1):((i-1)*(K-1)+K-1)]) %*% x #will return a long vector

        sigtemp <- sigmoid(matrix(temp, nrow = (length(temp)/(K-1))), par.sig = par.sig) #divide into matrix with
        product <- numeric()

        if(is.vector(sigtemp)){
          product <- sigtemp
        }

        else{
          product <- apply(sigtemp, 1, prod)
        }

        res=res+sum(product)

      }
      return(-res)
    }

    #optimlist <- Rsolnp::solnp(weight,fn,eqfun = sum,eqB = 1,control = list(trace = 0))#,tol = 1e-3))

     optimlist <- optim(par=weight, fn=fn,lower = 0, upper = 10,method = "L-BFGS-B")
    # optimlist=optim(par=weight,fn=fn,method = "BFGS")

    return(optimlist)
  }



  #ui=rbind(rep(1,length(weight)),rep(-1,length(weight)))
  #ci=c(0.1,-0.1)




 # optimlist <- solnp(initialweight,fn)


