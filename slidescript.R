xkcd_line <- function(x,y,color,lwd)
{
  len <- length(x);
  rg <- par("usr");
  
}

data <- data.frame(x=1:100)
data$one <- sin(data$x/2)*data$x
data$two <- data$one*rnorm(nrow(data),0,1.5)
data$three <- seq(0,0,length.out=100)
xkcd_line(data$x,data$two,'lightgray',6)
xkcd_line(data$x,data$three,'back',1)
xkcd_line(data$x,data$one,'red',3)