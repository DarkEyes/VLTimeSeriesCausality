#'@export
multipleVLTransferEntropy<-function(TS,maxLag=1,nboot=0,lx=1,ly=1,VLflag=TRUE,autoLagflag=TRUE)
{
  m<-min(dim(TS))
  n<-max(dim(TS))
  adjMat<-matrix(FALSE,m,m) # row cause col
  for(i in seq(m-1))
    for(j in seq(i+1,m))
    {
      outij<-VLTransferEntropy(Y=TS[,j],X=TS[,i], maxLag=maxLag,nboot=nboot,lx=lx,ly=ly,VLflag=VLflag,autoLagflag=autoLagflag)
      outji<-VLTransferEntropy(Y=TS[,i],X=TS[,j], maxLag=maxLag,nboot=nboot,lx=lx,ly=ly,VLflag=VLflag,autoLagflag=autoLagflag)
      adjMat[i,j]<-outij$XgCsY_trns
      adjMat[j,i]<-outji$XgCsY_trns
    }
  res<-list(adjMat=adjMat)
  return(res)
}
