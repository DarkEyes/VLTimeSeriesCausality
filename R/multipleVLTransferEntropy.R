#' @title  multipleVLTransferEntropy
#'
#' @description
#'
#' multipleVLTransferEntropy is a function that infers Variable-lag Transfer Entropy of all pairwises of \code{m} time series \code{TS[,1],...TS[,m]}.
#'
#'
#'@param TS is a numerical time series of effect where \code{TS[t,k]} is an element at time \code{t} of \code{k}th time series.
#'@param maxLag is a maximum possible time delay. The default is 0.2*length(Y).
#'@param nboot is a number of times of bootstrapping for RTransferEntropy::transfer_entropy() function.
#'@param lx,ly are lag parameters of RTransferEntropy::transfer_entropy().
#'@param autoLagflag is a flag for enabling the automatic lag inference function. The default is true.
#'If it is set to be true, then maxLag is set automatically using cross-correlation.
#'Otherwise, if it is set to be false, then the function takes the maxLag value to infer Granger causality.
#'@param VLflag is a flag of Granger causality choice: either \code{VLflag=TRUE} for VL-Granger or \code{VLflag=FALSE} for Granger causality.
#'
#'@return This function returns of a list of an adjacency matrix of causality where \code{adjMat[i,j]} is true if \code{TS[,i]} causes \code{TS[,j]}.
#'
#'@examples
#' # Generate simulation data
#'TS <- MultipleSimulationVLtimeseries()
#' # Run the function
#'out<-multipleVLTransferEntropy(TS,maxLag=1)
#'
#'
#'@export
multipleVLTransferEntropy<-function(TS,maxLag,nboot=0,lx=1,ly=1,VLflag=TRUE,autoLagflag=TRUE)
{
  m<-min(dim(TS))
  n<-max(dim(TS))
  if(missing(maxLag))
    maxLag<-0.2*length(TS[,1])
  adjMat<-matrix(FALSE,m,m) # row cause col
  for(i in seq(m-1))
    for(j in seq(i+1,m))
    {
      outij<-VLTransferEntropy(Y=TS[,j],X=TS[,i], maxLag=maxLag,nboot=nboot,lx=lx,ly=ly,VLflag=VLflag,autoLagflag=autoLagflag)
      if(outij$XgCsY_trns)
      {
        adjMat[i,j]<-outij$XgCsY_trns
        adjMat[j,i]<-FALSE
      }else
      {
        adjMat[i,j]<-FALSE
        TEratio<-1/outij$TEratio
        if(!is.na(TEratio))
          if( TEratio >1)
            adjMat[j,i]<-TRUE
      }
    }
  res<-list(adjMat=adjMat)
  return(res)
}
