#'@export
MultipleSimulationVLtimeseries<-function(n=200,lag=5,YstFixInx=111,YfnFixInx=150, XpointFixInx=100,arimaFlag=TRUE)
{
  TS<-matrix(0,n,10) # 10 time series
  A<-SimpleSimulationVLtimeseries(n=n,lag=lag,YstFixInx=YstFixInx,YfnFixInx=YfnFixInx, XpointFixInx=XpointFixInx,arimaFlag=arimaFlag)
  B<-SimpleSimulationVLtimeseries(n=n,lag=lag,YstFixInx=YstFixInx,YfnFixInx=YfnFixInx, XpointFixInx=XpointFixInx,arimaFlag=arimaFlag)
  C<-SimpleSimulationVLtimeseries(n=n,lag=lag,YstFixInx=YstFixInx,YfnFixInx=YfnFixInx, XpointFixInx=XpointFixInx,arimaFlag=arimaFlag)
  TS[,1]<-A$x
  TS[,2]<-B$x
  TS[,3]<-C$x
  TS[,4]<-A$y
  TS[,5]<-B$y
  TS[,6]<-C$y
  TS[,7]<-A$y
  TS[,8]<-B$y
  TS[,9]<-C$y
  TS[,10]<-A$y+B$y+C$y
  return(TS)
}

#'@export
checkMultipleSimulationVLtimeseries<-function(adjMat)
{
  trueAdjMat<-matrix(FALSE,10,10) # row cause col
  trueAdjMat[1,c(4,7,10)]<-TRUE
  trueAdjMat[2,c(5,8,10)]<-TRUE
  trueAdjMat[3,c(6,9,10)]<-TRUE
  TP<-0
  FP<-0
  FN<-0
  for(i in seq(10))
    for(j in seq(10))
    {
      if(trueAdjMat[i,j] && adjMat[i,j])
        TP<-TP+1
      else if( (!trueAdjMat[i,j]) && adjMat[i,j])
        FP<-FP+1
      else if(trueAdjMat[i,j] && (!adjMat[i,j]) )
        FN<-FN+1
    }
  prec<-TP/(TP+FP)
  rec<-TP/(TP+FN)
  F1<-2*prec*rec/(prec+rec)
  return(list(prec=prec,rec=rec,F1=F1))
}
