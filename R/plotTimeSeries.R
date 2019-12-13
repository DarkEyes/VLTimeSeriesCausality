#' @title  plotTimeSeries
#'
#' @description
#'
#' plotTimeSeries is a function for visualizing time series
#'
#'
#'@param X is a 1st numerical time series
#'@param Y is a 2nd numerical time series. If it is not supplied, the function plots only \code{X}.
#'@param strTitle is a string of the plot title
#'@param TSnames is a list of legend of \code{X,Y} where TSnames[1] is a legend of \code{X} and  TSnames[2] is a legend of \code{Y}.
#'
#'@return This function returns an object of ggplot class.
#'
#'@examples
#' # Generate simulation data
#'TS <- SimpleSimulationVLtimeseries()
#' # Run the function
#'plotTimeSeries(Y=TS$Y,X=TS$X)
#'
#'@import ggplot2
#'@importFrom graphics plot
#'
#'@export
plotTimeSeries<-function(X,Y,strTitle="Time Series Plot",TSnames)
{
  name<-c()
  n<-length(X)
  TS<-c()
  Xaxis<-c()

  if(missing(TSnames))
  {
    strX<-"X"
    strY<-"Y"
  }
  else
  {
    strX<-TSnames[1]
    strY<-TSnames[2]
  }
  name<-c(name,rep(strX,n) )
  TS<-c(TS,X)
  Xaxis<-c(Xaxis,1:n)

  if(missing(Y)==FALSE)
  {
    name<-c(name,rep(strY,n) )
    TS<-c(TS,Y)
    Xaxis<-c(Xaxis,1:n)
  }


  data1<-data.frame(Xaxis,TS,name)
  p<-ggplot(data1, aes(x=Xaxis, y=TS, group=name)) +
    geom_line(aes(color=name))+ scale_color_brewer(palette="Set1")+
    theme_light() + theme( text = element_text(size=20) )+
    ylab("Values") +xlab("Time steps")  +  labs(title = strTitle)
  p$labels$colour<-"Time series"
 # plot(p)
  return(p)
}
