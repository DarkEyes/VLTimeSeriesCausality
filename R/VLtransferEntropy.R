#' @title  VLTransferEntropy
#'
#' @description
#'
#' VLTransferEntropy is a Variable-lag Transfer Entropy function. It tests whether \code{X} VL-Transfer-Entropy-causes \code{Y}.
#'
#'
#'@param Y is a numerical time series of effect
#'@param X is a numerical time series of cause
#'@param maxLag is a maximum possible time delay. The default is 0.2*length(Y).
#'@param nboot is a number of times of bootstrapping for RTransferEntropy::transfer_entropy() function.
#'@param lx,ly are lag parameters of RTransferEntropy::transfer_entropy().
#'@param autoLagflag is a flag for enabling the automatic lag inference function. The default is true.
#'If it is set to be true, then maxLag is set automatically using cross-correlation.
#'Otherwise, if it is set to be false, then the function takes the maxLag value to infer Granger causality.
#'@param VLflag is a flag of Transfer Entropy choice: either \code{VLflag=TRUE} for VL-Transfer Entropy or \code{VLflag=FALSE} for Transfer Entropy.
#'@alpha is a significant-level threshold for TE bootstrapping by Dimpfl and Peter (2013).
#'
#'@return This function returns of  whether \code{X} (VL-)Transfer-Entropy-causes \code{Y}.
#'
#'
#'\item{TEratio}{ is a Transfer Entropy ratio. If it is greater than one , then \code{X} causes \code{Y}. }
#'\item{res}{ is an object of output from RTransferEntropy::transfer_entropy() }
#'\item{follOut}{ is a list of variables from function \code{followingRelation}. }
#'\item{XgCsY_trns}{The flag is true if \code{X} (VL-)Transfer-Entropy-causes \code{Y} using Transfer Entropy ratio ratio where \code{TEratio >1}
#' if \code{X} causes \code{Y}. Additionally, if \code{nboot>1}, the flag is true only when \code{pval<=alpha}. }
#'\item{pval}{ It is a p-value for TE bootstrapping by Dimpfl and Peter (2013).}
#'
#'@examples
#' # Generate simulation data
#'TS <- SimpleSimulationVLtimeseries()
#' # Run the function
#'out<-VLTransferEntropy(Y=TS$Y,X=TS$X)
#'
#'
#'@import RTransferEntropy
#'@export
VLTransferEntropy<-function(Y,X,maxLag,nboot=0,lx=1,ly=1,VLflag=TRUE,autoLagflag=TRUE,alpha = 0.05)
{
  follOut<-c()
  if(missing(maxLag))
    maxLag<-0.2*length(Y)
  if(VLflag)
  {
    if(autoLagflag == TRUE)
    {
      follOut<-followingRelation(Y=Y,X=X,timeLagWindow=maxLag)
      if(follOut$optDelay>=20) # cannot go above 20
      {
        lx<-20
        ly<-20
      }else
      {
        lx<-follOut$optDelay
        ly<-follOut$optDelay
      }
    }
    else
    {
      follOut<-followingRelation(Y=Y,X=X,maxLag)
    }

    follX<-c(follOut$nX[-1],0) # shift VLX back one time step (so that Y(t)=~ follX(t-1)).
    X<-follX

  }
  else if(autoLagflag)
  {
    follOut<-followingRelation(Y=Y,X=X)
    if(follOut$optDelay>=20) # cannot go above 20
    {
      lx<-20
      ly<-20
    }else
    {
      lx<-follOut$optDelay
      ly<-follOut$optDelay
    }
  }
  lx<-max(1,lx)
  ly<-max(1,ly)

  res<-transfer_entropy(x = X, y = Y, nboot=nboot, lx=lx,ly=ly,quiet=TRUE)
  pval<-res$coef[1,4]
  TEratio<-res$coef[1]/res$coef[2] # TE(X->Y) / (Y->X)

  XgCsY_trns<-FALSE
  if(!is.na(TEratio))
    if(TEratio >1)
    {
      if(!is.na(pval))
      {
        if(pval<=alpha)
          XgCsY_trns=TRUE
      }
      else
        XgCsY_trns=TRUE
    }
  return(list(TEratio=TEratio,res=res,follOut=follOut,XgCsY_trns=XgCsY_trns,pval=pval))
}

