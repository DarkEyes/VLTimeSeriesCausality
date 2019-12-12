#' @title  followingRelation
#'
#' @description
#'
#' followingRelation is a function that infers whether \code{Y} follows \code{X}.
#'
#'
#'@param Y is a numerical time series of a follower
#'@param X is a numerical time series of a leader
#'@param timeLagWindow is a maximum possible time delay in the term of time steps.
#'@param lagWindow is a maximum possible time delay in the term of percentage of length(X).
#'If \code{timeLagWindow} is missing, then \code{timeLagWindow=ceiling(lagWindow*length(X))}. The defualt is 0.2.
#'
#'@return This function returns a list of following relation variables below.
#'
#'
#'\item{follVal}{ is a following-relation value s.t. if \code{follVal} is positive, then \code{Y} follows \code{X}. If  \code{follVal} is negative, then \code{X} follows \code{Y}.
#' Otherwise, if \code{follVal} is zero, there is no following relation between \code{X,Y}.  }
#'\item{nX}{ is a time series that is rearranged from \code{X} by applying the lags \code{optIndexVec} in order to imitate \code{Y}. }
#'\item{optDelay}{ is the optimal time delay inferred by cross-correlation of \code{X,Y}. It is positive if \code{Y} is simply just a time-shift of \code{X} (e.g. \code{Y[t]=X[t-optDelay]}). }
#'\item{optCor}{ is the optimal correlation of \code{Y[t]=X[t-optDelay]} for all \code{t}.  }
#'\item{optIndexVec}{ is a time series of optimal warping-path from DTW that is corrected by cross correlation.
#' It is approximately that \code{Y[t]=X[t-optIndexVec[t]]}).  }
#'\item{VLval}{ is a percentage of elements in \code{optIndexVec} that is not equal to \code{optDelay}. }
#'\item{ccfout}{ is an output object of \code{ccf} function. }
#'
#'
#'@examples
#' # Generate simulation data
#' TS <- SimpleSimulationVLtimeseries()
#' # Run the function
#' out<-followingRelation(Y=TS$Y,X=TS$X)
#'
#'@importFrom stats dist
#'@import dtw
#'@export
#'
followingRelation<-function(Y,X,timeLagWindow,lagWindow=0.2)
{
  Y<-as.numeric(Y)
  X<-as.numeric(X)
  T<-length(X)
  follVal<-0

  if(missing(timeLagWindow))
  {
    timeLagWindow<-ceiling(lagWindow*T )
  }
  ccfout<-ccf(Y,X,lag.max = timeLagWindow,plot=FALSE)
  optDelay<-which.max( abs(ccfout$acf) )-(timeLagWindow+1)
  optCor<-ccfout$acf[which.max( abs(ccfout$acf) )]
  optIndexVec<-matrix(0, T)
  nX <- matrix(0, T)

  if(optDelay<0)
  {
    follVal<- -1
    VLval<-0
  }
  else{

    if(optCor<0)
      X = -X
    alignment<-dtw(x=Y,y=X,keep.internals=TRUE,window.type = "sakoechiba" ,window.size=optDelay)
    indexVec<-matrix(optDelay, T)
    dtwIndexVec<-1:T-alignment$index2[1:T]


    X<-as.matrix(X)
    Y<-as.matrix(Y)
    for( t in 1:T )
    {
      if( (t-indexVec[t])<1 && (t-dtwIndexVec[t])<1    )
        nX[t,1]<-X[t]
      else if (  ((t-indexVec[t])>=1) && ((t-dtwIndexVec[t])>=1)    )
      {
        if( dist( c(Y[t],X[t-dtwIndexVec[t]]) ) < dist( c(Y[t],X[t-indexVec[t]]) ) )
          optIndexVec[t]<-dtwIndexVec[t]
        else
          optIndexVec[t]<-indexVec[t]
        nX[t,1]<-X[t-optIndexVec[t]]

      }
      else if((t-indexVec[t])>=1)
      {
        optIndexVec[t]<-indexVec[t]
        nX[t,1]<-X[t-optIndexVec[t]]
      }
      else
      {
        optIndexVec[t]<-dtwIndexVec[t]
        nX[t,1]<-X[t-optIndexVec[t]]
      }
    }
    VLval<-mean(sign( optIndexVec!= optDelay))
    follVal<-mean(sign(optIndexVec))
  }

  nX<-as.numeric(nX)

  list(follVal=follVal,nX=nX,optDelay=optDelay,optCor=optCor,optIndexVec=optIndexVec,VLval=VLval,ccfout=ccfout)

}
