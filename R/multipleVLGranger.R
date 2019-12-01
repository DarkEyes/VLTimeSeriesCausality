#'
#'@import tseries
#'@export
multipleVLGrangerFunc<-function(TS, maxLag=1,alpha=0.05,sigma=0.15, gamma=0.5,autoLagflag=TRUE,causalFlag=0,VLflag=TRUE,family = gaussian )
{
  m<-min(dim(TS))
  n<-max(dim(TS))
  adjMat<-matrix(FALSE,m,m) # row cause col
  for(i in seq(m-1))
    for(j in seq(i+1,m))
    {
      if(VLflag == FALSE)
      {
        outij<-GrangerFunc(Y=TS[,j],X=TS[,i], maxLag=maxLag,alpha=alpha, autoLagflag=autoLagflag, family = family)
        outji<-GrangerFunc(Y=TS[,i],X=TS[,j], maxLag=maxLag,alpha=alpha, autoLagflag=autoLagflag, family = family)
      }
      else
      {
        outij<-VLGrangerFunc(Y=TS[,j],X=TS[,i], maxLag=maxLag,alpha=alpha,sigma=sigma, gamma=gamma,autoLagflag=autoLagflag,family = family)
        outji<-VLGrangerFunc(Y=TS[,i],X=TS[,j], maxLag=maxLag,alpha=alpha,sigma=sigma, gamma=gamma,autoLagflag=autoLagflag,family = family)
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
