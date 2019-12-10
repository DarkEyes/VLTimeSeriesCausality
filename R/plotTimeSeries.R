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
