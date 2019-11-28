#'@export
SimpleSimulationVLtimeseries<-function(n=200,lag=5,YstFixInx=111,YfnFixInx=150, XpointFixInx=100,arimaFlag=TRUE)
{
  x <- rep(0, n + lag)
  y <- rep(0, n + lag)


  for (i in seq(n)) {
    if(arimaFlag == FALSE) # using normal generator
    {
      x[i + 1] <- rnorm(1, 0, 2)
    }
    else
      x[i + 1] <- 0.2 * x[i] + rnorm(1, 0, 2)
    y[i + lag] <- x[i] + rnorm(1, 0, 0.5)
  }

  y[YstFixInx:YfnFixInx]<-x[XpointFixInx]

  x <- x[-(1:lag)]
  y <- y[-(1:lag)]
  return(list(x=x,y=y))
}
