#' @title  VLGrangerFunc
#'
#' @description
#'
#' VLGrangerFunc is a Variable-lag Granger Causality function. It tests whether \code{X} VL-Granger-causes \code{Y}.
#'
#'
#'@param Y is a numerical time series of effect
#'@param X is a numerical time series of cause
#'@param maxLag is a maximum possible time delay. The default is 0.2*length(Y).
#'@param alpha is a significance level of f-test to determine whether \code{X} Granger-causes \code{Y}.  The default is 0.05.
#'@param autoLagflag is a flag for enabling the automatic lag inference function. The default is true.
#'If it is set to be true, then maxLag is set automatically using cross-correlation.
#'Otherwise, if it is set to be false, then the function takes the maxLag value to infer Granger causality.
#'@param gamma is a parameter to determine whether \code{X} Granger-causes \code{Y} using BIC difference ratio. The default is  0.5.
#'@param family is a parameter of family of function for Generalized Linear Models function (glm). The default is \code{gaussian}.
#'
#'@return This function returns of  whether \code{X} Granger-causes \code{Y}.
#'
#'
#'\item{ftest}{ F-statistic of Granger causality. }
#'\item{p.val}{ A p-value from F-test. }
#'\item{BIC_H0}{Bayesian Information Criterion (BIC) derived from \code{Y} regressing on \code{Y} past.  }
#'\item{BIC_H1}{Bayesian Information Criterion (BIC) derived from \code{Y} regressing on \code{Y},\code{X} past. }
#'\item{XgCsY}{The flag is true if \code{X} Granger-causes \code{Y} using BIC difference ratio where \code{BICDiffRatio >= gamma}.}
#'\item{XgCsY_ftest}{The flag is true if \code{X} Granger-causes \code{Y} using f-test where \code{p.val>=alpha}. }
#'\item{XgCsY_BIC}{The flag is true if \code{X} Granger-causes \code{Y} using BIC where \code{BIC_H0>=BIC_H1}. }
#'\item{maxLag}{A maximum possible time delay.  }
#'\item{H0}{glm object of \code{Y} regressing on \code{Y} past.  }
#'\item{H1}{glm object of \code{Y} regressing on \code{Y,X} past. }
#'\item{follOut}{ is a list of  variables from function \code{followingRelation}. }
#'\item{BICDiffRatio}{ Bayesian Information Criterion difference ratio: \code{(BIC_H0-BIC_H1)/BIC_H0}.  }
#'
#'@examples
#' # Generate simulation data
#'TS <- SimpleSimulationVLtimeseries()
#' # Run the function
#'out<-VLGrangerFunc(Y=TS$Y,X=TS$X)
#'
#'@importFrom stats ccf gaussian lag glm rnorm pf ts ts.intersect
#'@export
VLGrangerFunc<-function(Y,X,alpha=0.05,maxLag,gamma=0.5, autoLagflag=TRUE,family = gaussian )
{
  XgCsY_ftest<-FALSE
  if(missing(maxLag))
    maxLag<-0.2*length(Y)
  if(autoLagflag == TRUE)
  {

    follOut<-followingRelation(Y=Y,X=X,timeLagWindow=maxLag)
    maxLag<-max(1,follOut$optDelay)
  }
  else
  {
    follOut<-followingRelation(Y=Y,X=X,timeLagWindow=maxLag)
  }
  if(follOut$optDelay ==0 ) # prevent X ~ Y
    X<-follOut$nX
  else
    X<-c(follOut$nX[-1],0)
  YX<-cbind(ts(Y),ts(X))
  D <- YX

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
  BIC_H0<-(S0/n)*n^( (maxLag+1)/n ) # less value is better
  BIC_H1<-(S1/n)*n^( (2*maxLag+1)/n ) # less value is better




  # BIC_H1 < BIC_H0 implies X Granger-causes Y (option 1)
  # pval < \alpha implies  X Granger-causes Y (option 2)
  if( (pval<=alpha) )
    XgCsY_ftest=TRUE
  XgCsY_BIC<- ( (BIC_H1<BIC_H0) )

  # BICDiffRatio > gamma implies X Granger-causes Y (option 3)
  BICDiffRatio<-(BIC_H0-BIC_H1)/BIC_H0
  XgCsY<- ( (BICDiffRatio>=gamma) ) # Our main flag of X causes Y using BICDiffRatio


  res<-list(ftest = ftest, p.val = pval,BIC_H1=BIC_H1, BIC_H0=BIC_H0,
            XgCsY_ftest=XgCsY_ftest,XgCsY_BIC=XgCsY_BIC,follOut=follOut,maxLag=maxLag,H1=H1,H0=H0,BICDiffRatio=BICDiffRatio,XgCsY=XgCsY)
  return(res)
}

