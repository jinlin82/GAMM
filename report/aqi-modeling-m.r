rm(list=ls())
varlist <- read.csv("./data/var_list.csv")
dat.sel.m <- read.csv("./results/dat.sel.m.csv")

varinfo <- varlist[match(names(dat.sel.m), as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]

dat.sel.m$city <- factor(dat.sel.m$city)
dat.sel.m$year <- factor(dat.sel.m$year)
dat.sel.m$month <- factor(dat.sel.m$month)
dat.sel.m$heating <- factor(dat.sel.m$heating)
dat.sel.m$coast <- factor(dat.sel.m$coast)


####### 设置水平值
levels(dat.sel.m$heating) <- c("无集中供暖", "集中供暖")
levels(dat.sel.m$coast) <- c("非临海", "临海")


####### 数据标准化
## dat.sel.m[,-c(23, 24)] <- scale(dat.sel.m[,-c(23,24)])


####### gamsel #######################################################################
library(gamsel)
gam1 <- gamsel(as.matrix(dat.sel.m[,-c(1:4, 24:25)]), as.matrix(log(dat.sel.m[,4])), gamma = 0.369)
gam1

par(mfrow=c(6,4), mar=c(2.1, 3.1, 2.1, 1.1))
## plot(gam1,newx=as.matrix(dat.sel.m[,-c(1, 23:24)]), index=22)
mapply(function(i, y) {
    plot(gam1,newx=as.matrix(dat.sel.m[,-c(1:4, 24:25)]),
         index=27, which = i, main = y, cex.main = 0.95, ylim = c(-0.2, 0.2))},
    as.list(1:22),
    as.list(varinfo[,2][-c(1:4, 24:25)]))

varsel <- as.character(varinfo[,3][-c(1:4, 24:25)][getActive(gam1, 25, "nonzero")[[1]]])

####################################### MODEL #########################################
####### 线性模型
lm.m <- lm(log(AQI)~con_area+car+gas_control
           +mean_temp+rain+humid+wind_speed+pressure
           +lat+heating+coast,
           data=dat.sel.m)

summary(lm.m)
AIC(lm.m)

lm.m.2 <- lm(log(AQI)~con_area+car+gas_control
                   +mean_temp+rain+humid+wind_speed+pressure
                   +lat+heating+coast+year,
                   data=dat.sel.m)

summary(lm.m.2)
AIC(lm.m.2)

lm.m.3 <- lm(log(AQI)~con_area+car+gas_control
                   +mean_temp+rain+humid+wind_speed+pressure
                   +lat+heating+coast+city,
                   data=dat.sel.m)

summary(lm.m.3)
AIC(lm.m.3)

####### gam
library(mgcv)

gam.m <- gam(log(AQI)~con_area+car+gas_control
                   +mean_temp+rain+humid+wind_speed+pressure
                   +heating+coast+s(lat),
                   data=dat.sel.m, method = "REML")

summary(gam.m)
plot(gam.m)

par(mfrow = c(1,3))
par(mar=c(5,4,2,1))
plot.gam(gam.m,
         select=1,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="AQI",xlab = "纬度"
         )
par(mar=c(5,3,2,1))
plot.gam(gam.m,
         select=2,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="",xlab = "汽车拥有量"
         )
par(mar=c(5,3,2,1))
plot.gam(gam.m,
         select=3,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="",xlab = "平均气温"
         )

par(op)

AIC(gam.m)
BIC(gam.m)

moran.g.gam <- tapply(resid(gam.m), as.factor(dat.sel.m$year), 
              function(x) Moran.I(x, city.dists.inv)
)

moran.g.gam <- sapply(moran.g.gam, function(x) c(x$observed, x$p.v))
dimnames(moran.g.gam)[[1]] <- c("Moran's I", "P值")

####### lme

lme.m <- gamm(log(AQI)~con_area+car+gas_control
                   +mean_temp+rain+humid+wind_speed+pressure
                   +heating+coast+lat,
              random=list(city=~1), data=dat.sel.m, method = "REML")
summary(lme.m$lme)
AIC(lme.m$lme)
BIC(lme.m$lme)

ranef(lme.m$lme)

moran.g.lme <- tapply(resid(lme.m$lme), as.factor(dat.sel.m$year), 
              function(x) Moran.I(x, city.dists.inv)
)

moran.g.lme <- sapply(moran.g.lme, function(x) c(x$observed, x$p.v))
dimnames(moran.g.lme)[[1]] <- c("Moran's I", "P值")

####### gamm
gamm.m <- gamm(log(AQI)~con_area+car+gas_control
               +mean_temp+rain+humid+wind_speed+pressure
               +heating+coast+s(lat),
               random=list(city=~1), data=dat.sel.m,
               method = "REML")

summary(gamm.m$lme)
summary(gamm.m$gam)
AIC(gamm.m$lme)
BIC(gamm.m$lme)

plot(gamm.m$gam)

ranef(gamm.m$lme)


moran.g.gamm <- tapply(resid(gamm.m$lme), as.factor(dat.sel.m$year), 
              function(x) Moran.I(x, city.dists.inv)
)

moran.g.gamm <- sapply(moran.g.gamm, function(x) c(x$observed, x$p.v))
dimnames(moran.g.gamm)[[1]] <- c("Moran's I", "P值")

