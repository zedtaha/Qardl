


#-------------------------------------------------------------------------------
# Function lagm
# INPUT: m is a matrix, nLags the number of lags
lagm <- function(m, nLags) {
  # JM: This code is redundant. More than 2 arguments will cause an error
  # anyway, less than 2 arguments will cause an error about missing
  # arguments that's more informative than yours.

  # nargin <- length(as.list(match.call())) - 1
  # if (nargin != 2) {
  #   stop('Check function inputs')
  # }

  if(!is.matrix(m))
    stop("Trying to lag something that's not a matrix")

  d <- dim(m)

  #Add column names if they don't exist yet
  if(is.null(colnames(m)))
    colnames(m) <- as.character(seq_len(d[2]))

  #Check
  if(nLags > d[1])
    stop(sprintf("You try to create %d lags but there's only %d rows in m.",
                 nLags, d[1]))

  lagM <- matrix(NA,nrow=d[1], ncol = d[2]*nLags)

  for(i in seq_len(nLags)){
    #Make ids for the columns in result
    cid <- seq(1:d[2]) + d[2]*(i-1)

    lagM[(i+1):d[1],cid] <- m[1:(d[1]-i),]
  }

  #mnames=paste("L",colnames(m),sep = ".")

  cnames <- outer(colnames(m),seq_len(nLags), FUN = paste, sep = "_")

  colnames(lagM) <- c(cnames)

  return(lagM)

}


#-------------------------------------------------------------------------------
# Function lag matrix
nlagm=function(x,nlags){
  if(nlags==0){
    mlag=as.matrix(x)
  }else{
    m=lagm(x,nlags)
    mlag=cbind(x,m)
  }

  mlag
}
#-------------------------------------------------------------------------------
# Function lag matrix
nlagm1=function(x,nlags){
  if(nlags==0){
    mlag=as.matrix(x)
  }else{
    mlag=as.matrix(lagm(x,nlags))
  }

  mlag
}


#-------------------------------------------------------------------------------
# Function lag matrix
lags <- function(x, k=1) {
  i<-is.vector(x)
  if(is.vector(x)) x<-matrix(x) else x<-matrix(x,nrow(x))
  if(k>0) {
    x <- rbind(matrix(rep(NA, k*ncol(x)),ncol=ncol(x)), matrix(x[1:(nrow(x)-k),], ncol=ncol(x)))
  }
  else {
    x <- rbind(matrix(x[(-k+1):(nrow(x)),], ncol=ncol(x)),matrix(rep(NA, -k*ncol(x)),ncol=ncol(x)))
  }
  if(i) x[1:length(x)] else x
}

#-------------------------------------------------------------------------------
# Function lag matrix
mlags=function(x,nlags){
  if(nlags==0){
    mlag=as.matrix(x)
  }else{
    m=lags(x,nlags)
    mlag=cbind(x,m)
  }

  mlag
}


BICC <- function(f) {
  sample.size<-f$df + length(f$coeff)
  bic<-log(sum(residuals(f)^2)/sample.size)+(length(f$coeff)/sample.size)*log(sample.size)
  bic

}

