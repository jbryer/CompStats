require(ggplot2)
dat <- data.frame(t=seq(0, 2*pi, by=0.1) )
xhrt <- function(t) 16*sin(t)^3
yhrt <- function(t) 13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
dat$y=yhrt(dat$t)
dat$x=xhrt(dat$t)
ggplot(dat, aes(x=x,y=y)) + geom_polygon(fill='pink') + coord_equal() + xlab('') + ylab('')
