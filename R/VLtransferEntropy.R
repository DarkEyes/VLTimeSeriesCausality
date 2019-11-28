#'
#'@import RTransferEntropy
#'@export
VLTransferEntropy<-function(Y,X,maxLag=1,nboot=0,lx=1,ly=1,VLflag=TRUE,autoLagflag=TRUE)
{
  if(VLflag)
  {
    if(autoLagflag == TRUE)
    {
      follOut<-followingRelation(Y=Y,X=X)
      lx<-follOut$optDelay
      ly<-follOut$optDelay
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
    lx<-follOut$optDelay
    ly<-follOut$optDelay
  }
  res<-transfer_entropy(x = X, y = Y, nboot=nboot, lx=lx,ly=ly,quiet=TRUE)
  TEratio<-res$coef[1]/res$coef[2] # TE(X->Y) / (Y->X)
  return(list(TEratio=TEratio,res=res,follOut=follOut))
}
