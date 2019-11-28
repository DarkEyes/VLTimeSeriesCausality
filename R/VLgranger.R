#'
#'@import tseries
#'@export
VLGrangerFunc<-function(X,Y, maxLag=1,alpha=0.05,sigma=0.1, family = gaussian )
{
  XgCsY_ftest<-FALSE
  follOut<-followingRelation(Y=Y,X=X,timeLagWindow=maxLag)
  X<-c(follOut$nX[-1],0)
  YX<-cbind(ts(Y),ts(X))
  D <- YX

  # Create time-shift vesions of y and x (y(t),x(t),y(t-1),x(t-1),...)
  for(i in 1:maxLag)
    D <-ts.intersect(D, lag(YX,  - i))

  y  <- D[, 1]
  n  <- length(y)
  xyPast <- D[,  - (1:2)] # delete two targted columns (leave only y past and x past)
  yPast <- xyPast[, ((1:maxLag) * 2) - 1] # delete all x columns (leave only y past)
  #========
  H1 <- glm(y ~ xyPast,family=family)
  H0 <- glm(y ~ yPast,family=family)
  S1 <- sum(H1$resid^2)
  S0 <- sum(H0$resid^2)

  ftest <- ((S0 - S1)/maxLag)/(S1/(n - 2 * maxLag - 1))
  pval <- 1 - pf(ftest, maxLag, n - 2 * maxLag - 1)
  R2<-summary(H1)$r.squared
  BIC_H0<-(S0/n)*n^( (maxLag+1)/n ) # less value is better
  BIC_H1<-(S1/n)*n^( (2*maxLag+1)/n ) # less value is better

  # BIC_H1 < BIC_H0 implies X Granger-causes Y (option 1)
  # pval < \alpha implies  X Granger-causes Y (option 2)
  if(pval<alpha && follOut$follVal>= sigma)
    XgCsY_ftest=TRUE
  XgCsY_BIC<- ( (BIC_H1<BIC_H0) && (follOut$follVal>= sigma) )

  res<-list(ftest = ftest, p.val = pval, R2 = R2,
            BIC_H1=BIC_H1, BIC_H0=BIC_H0,
            XgCsY_ftest=XgCsY_ftest,XgCsY_BIC=XgCsY_BIC)
  return(res)
}
