## ----setup, echo=F-------------------------------------------------------

################# 第 2 章 R 程序代码  ####################


knitr::opts_knit$set(root.dir = getwd())
knitr::opts_chunk$set(echo = FALSE, results = 'hide')
knitr::opts_chunk$set(warning = FALSE, message=FALSE)


## ----prepare-------------------------------------------------------------
rm(list=ls())
options(digits=4)
options(scipen=100)
graphics.off()
Sys.setlocale("LC_ALL", "Chinese")


## ----fig-nonpara-compare, eval=T, fig.height=6, fig.width=6.5, out.width="1.05\\textwidth", fig.pos="h", fig.cap = "4种光滑方法拟合的比较", dev=c("cairo_pdf"), dev.args=list(family="Microsoft YaHei UI Light")----

library(SemiPar)
library(splines)

#Comparison Plots
#Simulate A Very Wavy Functional Form
trans<-function(x) {sin(2*pi*x^2)^3}
x<-seq(0,2,by=.01)
y<-trans(x)+.2*rnorm(201)

loess <- loess(y ~ x, span=0.1)

#Loess- Must adjust span to almost minimum
xhatloess <- x
yhatloess <- fitted(loess)

#Figure 3.11
#Black Line Represents Estimated Smoothed Fit
par(mfrow = c(2,2))

#Loess
matplot(x, cbind(y, trans(x)),
    type="pl", lty=2, col=1, pch="",
   xlab="X", ylab="Y", main = "Loess", bty = "l")
lines(xhatloess, yhatloess, lwd=1)

#Cubic B-spline
matplot(x, cbind(y, trans(x)),
   pch="", type="pl", lty=2, col=1,
   xlab="X", ylab="Y", main = "自然立方B样条", bty = "l")
sp1<-lm(y~ns(x, df=15))
lines(x,sp1$fit, lwd=1)

#Lowess
matplot(x, cbind(y, trans(x)),
   pch="", type="pl", lty=2, col=1,
   xlab="X", ylab="Y", main="Lowess", bty = "l")
lines(lowess(x,y, f = 0.05), lwd=1)

#Smoothing Spline
matplot(x, cbind(y, trans(x)),
   pch="", type="pl", lty=2, col=1,
   xlab="X", ylab="Y", main="光滑样条", bty = "l")
fit <- spm(y ~ f(x))
lines(fit, se=FALSE, lwd=1)



## ----fig-multi-nonpara, eval=T, fig.height=5, fig.width=6.5, out.width="1.05\\textwidth", fig.pos="h", fig.cap = "多元非参数回归的分类", dev=c("cairo_pdf"), dev.args=list(family="Microsoft YaHei UI Light")----
par(mfcol=c(2,3),mar=c(2,2.5,2,3))

#------------Linear--------------
pts <- seq (from=0,to=1,len=20)
fff<- function (x,y) { x + y}
rmat <- outer (pts,pts,fff)
info.p <- persp (x=pts,y=pts,rmat,xlab="x2", ylab="x1",zlab="",lwd=1, phi=15, theta=-30, r=10)
title ("参数线性模型")


#-------------Partial Linear--------------------
pts <- seq (from=0,to=1,len=20)
fff<- function (x,y) { x + exp(-15*(y-.5)^2)}
rmat <- outer (pts,pts,fff)
info.p <- persp (x=pts,y=pts,rmat,xlab="z", ylab="x",zlab="",lwd=1, phi=15, theta=-30, r=10)
title ("部分线性模型" )



#-------------Partial Parametric--------------------
pts <- seq (from=0,to=1,len=20)
fff<- function (x,y) { 3*(x-.5)^2 + exp(-15*(y-.5)^2)}
rmat <- outer (pts,pts,fff)
info.p <- persp (x=pts,y=pts,rmat,xlab="z", ylab="x",zlab="",lwd=1, phi=15, theta=-30, r=10)
title ("部分参数模型" )



#------------Additively Separable------------------
pts <- seq (from=0,to=1,len=20)
fff<- function (x,y) { exp(-15*(x-.5)^2) + exp(-15*(y-.5)^2)}
rmat <- outer (pts,pts,fff)
info.p <- persp (x=pts,y=pts,rmat,xlab="xb", ylab="xa",zlab="",lwd=1, phi=30, theta=-45, r=10, d=10)
title ("独立可加模型",cex=.5)


#------------Index Model------------------
pts <- seq (from=0,to=1,len=20)
fff<- function (x,y) { sin((x+y)*5)}
rmat <- outer (pts,pts,fff)
info.p <- persp (x=pts,y=pts,rmat,xlab="x2", ylab="x1",zlab="",lwd=1, phi=30, theta=-45, r=10, d=10)
title ("单指标模型")

#----------------Nonparametric-------------------
pts <- seq (from=0,to=1,len=20)
#fff<- function (x,y) { exp(-15*(x-.5)^2) * exp(-15*(y-.5)^2)}
fff<- function (x,y){(sin(10*x)-cos(5*y))* exp(-15*(x-.5)^2) * exp(-15*(y-.5)^2)}
rmat <- outer (pts,pts,fff)
info.p <- persp (x=pts,y=pts,rmat,xlab="x2", ylab="x1",zlab="",lwd=1, phi=30, theta=-45, r=10, d=10)
title ("完全非参数模型")


