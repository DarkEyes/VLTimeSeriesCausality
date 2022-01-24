#' @title  TSNANNearestNeighborPropagation
#'
#' @description
#'
#' TSNANNearestNeighborPropagation is a function that fills NA values with nearest real values in the past ( or future if the first position of time series is NA), for time series \code{X}.
#'
#'
#'@param X is a T-by-D matrix numerical time series
#'
#'@return This function returns a list of following relation variables below.
#'
#'\item{Xout}{ is a T-by-D matrix numerical time series that all NAN have been filled with nearest real values.}
#'
#'@examples
#' # Load example data
#'
#' z<-1:20
#' z[2:5]<-NA
#' z<-TSNANNearestNeighborPropagation(z)
#'
#'@export
#'
TSNANNearestNeighborPropagation<-function(X)
{
  if(sum(is.na(X) ) == 0)
    return(X)
  lengthL<-length(X)
  t<-1
  if(is.na(X[1]) )
  {
    for(k in seq(2,lengthL))
    {
      if(!is.na(X[k]))
      {
        X[1:(k-1)]<-X[k]
        t<-k
        break;
      }
    }
  }
  for(k in seq(t+1,lengthL))
  {
    if(is.na(X[k]) )
    {
      X[k]<-X[k-1]
    }
  }
  Xout<-X
  return(Xout)

}
