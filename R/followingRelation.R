#'@import dtw
#'@export
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
  out1<-ccf(Y,X,lag = timeLagWindow,plot=FALSE)
  optDelay<-which.max( abs(out1$acf) )-(timeLagWindow+1)
  optCor<-out1$acf[which.max( abs(out1$acf) )]
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
    alignment<-dtw(Y,X,keep=TRUE,window.type = "sakoechiba" ,window.size=optDelay)

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
        if( dist(Y[t],X[t-dtwIndexVec[t],]) <dist(Y[t],X[t-indexVec[t],]) )
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

  list(follVal=follVal,nX=nX,optDelay=optDelay,optCor=optCor,optIndexVec=optIndexVec,VLval=VLval,out1=out1)

}
