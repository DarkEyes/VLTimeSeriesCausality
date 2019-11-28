#'@import ggplot2
#'@importFrom graphics plot
#'
#'@export
plotTimeSeries<-function(x,y)
{
  name<-c()
  n<-length(x)
  TS<-c()
  Xaxis<-c()
  name<-c(name,rep("x",n) )
  TS<-c(TS,x)
  Xaxis<-c(Xaxis,1:n)

  if(missing(y)==FALSE)
  {
    name<-c(name,rep("y",n) )
    TS<-c(TS,y)
    Xaxis<-c(Xaxis,1:n)
  }


  data1<-data.frame(Xaxis,TS,name)
  p<-ggplot(data1, aes(x=Xaxis, y=TS, group=name)) +
    geom_line(aes(color=name))+ scale_color_brewer(palette="Set1")+
    theme_light() + theme( text = element_text(size=20) )+
    ylab("Values") +xlab("Time steps")
  p$labels$colour<-"Time series"
  plot(p)
  return(p)
}
