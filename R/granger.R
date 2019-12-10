#'
#'@import tseries
#'@export
GrangerFunc<-function(Y,X, maxLag=1,alpha=0.05, autoLagflag=TRUE,gamma=0.05, family = gaussian)
{
  XgCsY_ftest<-FALSE
  YX<-cbind(ts(Y),ts(X))
  D <- YX

  if(autoLagflag == TRUE)
  {
    follOut<-followingRelation(Y=Y,X=X)
    maxLag<-max(1,follOut$optDelay)
  }

  # Create time-shift vesions of y and x (y(t),x(t),y(t-1),x(t-1),...)
  for(i in 1:maxLag)
    D <-ts.intersect(D, lag(YX,  - i))

  y  <- D[, 1]
  n  <- length(Y)
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
  if(pval<alpha)
    XgCsY_ftest=TRUE
  XgCsY_BIC<- (BIC_H1<BIC_H0)

  # BICDiffRatio > gamma implies X Granger-causes Y (option 3)
  BICDiffRatio<-(BIC_H0-BIC_H1)/BIC_H0
  XgCsY<- ( (BICDiffRatio>=gamma) ) # Our main flag of X causes Y using BICDiffRatio

  res<-list(ftest = ftest, p.val = pval, BIC_H1=BIC_H1, BIC_H0=BIC_H0,XgCsY=XgCsY,
            XgCsY_ftest=XgCsY_ftest,XgCsY_BIC=XgCsY_BIC,maxLag=maxLag,H1=H1,H0=H0,BICDiffRatio=BICDiffRatio)
  return(res)
}
