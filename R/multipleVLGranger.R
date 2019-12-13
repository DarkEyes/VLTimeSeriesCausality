#' @title  multipleVLGrangerFunc
#'
#' @description
#'
#' multipleVLGrangerFunc is a function that infers Variable-lag Granger Causality of all pairwises of \code{m} time series \code{TS[,1],...TS[,m]}.
#'
#'
#'@param TS is a numerical time series of effect where \code{TS[t,k]} is an element at time \code{t} of \code{k}th time series.
#'@param maxLag is a maximum possible time delay. The default is 0.2*length(Y).
#'@param alpha is a significance level of F-test to determine whether \code{X} Granger-causes \code{Y}.  The default is 0.05.
#'@param autoLagflag is a flag for enabling the automatic lag inference function. The default is true.
#'If it is set to be true, then maxLag is set automatically using cross-correlation.
#'Otherwise, if it is set to be false, then the function takes the maxLag value to infer Granger causality.
#'@param gamma is a parameter to determine whether \code{X} Granger-causes \code{Y} using BIC difference ratio. The default is  0.3.
#'@param family is a parameter of family of function for Generalized Linear Models function (glm). The default is \code{gaussian}.
#'@param VLflag is a flag of Granger causality choice: either \code{VLflag=TRUE} for VL-Granger or \code{VLflag=FALSE} for Granger causality.
#'@param causalFlag is a choice of criterion for inferring causality:
#' \code{causalFlag=0} for BIC difference ratio, \code{causalFlag=1} for f-test, or \code{causalFlag=2} for BIC.
#'
#'@return This function returns of a list of an adjacency matrix of causality where \code{adjMat[i,j]} is true if \code{TS[,i]} causes \code{TS[,j]}.
#'
#'@examples
#' # Generate simulation data
#'TS <- MultipleSimulationVLtimeseries()
#' # Run the function
#'out<-multipleVLGrangerFunc(TS)
#'
#'
#'@import tseries
#'@export
multipleVLGrangerFunc<-function(TS, maxLag,alpha=0.05, gamma=0.3,autoLagflag=TRUE,causalFlag=0,VLflag=TRUE,family = gaussian )
{
  m<-min(dim(TS))
  n<-max(dim(TS))
  adjMat<-matrix(FALSE,m,m) # row cause col
  if(missing(maxLag))
    maxLag<-0.2*length(TS[,1])
  for(i in seq(m-1))
    for(j in seq(i+1,m))
    {
      if(VLflag == FALSE)
      {
        outij<-GrangerFunc(Y=TS[,j],X=TS[,i], maxLag=maxLag,alpha=alpha, gamma=gamma, autoLagflag=autoLagflag, family = family)
        outji<-GrangerFunc(Y=TS[,i],X=TS[,j], maxLag=maxLag,alpha=alpha, gamma=gamma, autoLagflag=autoLagflag, family = family)
      }
      else
      {
        outij<-VLGrangerFunc(Y=TS[,j],X=TS[,i], maxLag=maxLag,alpha=alpha, gamma=gamma,autoLagflag=autoLagflag,family = family)
        outji<-VLGrangerFunc(Y=TS[,i],X=TS[,j], maxLag=maxLag,alpha=alpha, gamma=gamma,autoLagflag=autoLagflag,family = family)
      }
      if(causalFlag==0) # using BICDiffRaio
      {
        adjMat[i,j]<-outij$XgCsY
        adjMat[j,i]<-outji$XgCsY
      }else if (causalFlag==1) # using ftest
      {
        adjMat[i,j]<-outij$XgCsY_ftest
        adjMat[j,i]<-outji$XgCsY_ftest
      }
      else # (causalFlag==2) # using BIC
      {
        adjMat[i,j]<-outij$XgCsY_BIC
        adjMat[j,i]<-outji$XgCsY_BIC
      }
    }
  res<-list(adjMat=adjMat)
  return(res)
}
