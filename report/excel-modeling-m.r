rm(list=ls())
varlist <- read.csv("./data/var_list.csv")
dat.sel.m.excel <- read.csv("./results/dat.sel.m.excel.csv")

varinfo <- varlist[match(names(dat.sel.m.excel), as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]

dat.sel.m.excel$city <- factor(dat.sel.m.excel$city)
dat.sel.m.excel$year <- factor(dat.sel.m.excel$year)
dat.sel.m.excel$month <- factor(dat.sel.m.excel$month)
dat.sel.m.excel$heating <- factor(dat.sel.m.excel$heating)
dat.sel.m.excel$coast <- factor(dat.sel.m.excel$coast)


####### 设置水平值
levels(dat.sel.m.excel$heating) <- c("无集中供暖", "集中供暖")
levels(dat.sel.m.excel$coast) <- c("非临海", "临海")


####### 数据标准化
## dat.sel.m.excel[,-c(23, 24)] <- scale(dat.sel.m.excel[,-c(23,24)])



####### gamsel #######################################################################
library(gamsel)
gam1 <- gamsel(as.matrix(dat.sel.m.excel[,-c(1:4, 24:25)]), as.matrix(dat.sel.m.excel[,4]), gamma = 0.39)
gam1

par(mfrow=c(6,4), mar=c(2.1, 3.1, 2.1, 1.1))
## plot(gam1,newx=as.matrix(dat.sel.m.excel[,-c(1, 23:24)]), index=22)
mapply(function(i, y) {
    plot(gam1,newx=as.matrix(dat.sel.m.excel[,-c(1:4, 24:25)]),
         index=25, which = i, main = y, cex.main = 0.95, ylim = c(-10, 10))},
    as.list(1:22),
    as.list(varinfo[,2][-c(1:4, 24:25)]))



varsel <- as.character(varinfo[,3][-c(1:4, 24:25)][getActive(gam1, 25, "nonzero")[[1]]])

####################################### MODEL #########################################
####### 线性模型

lm.m.excel <- lm(dat.sel.m.excel[, c("excellent", varsel, "heating", "coast")])
summary(lm.m.excel)
AIC(lm.m.excel)
BIC(lm.m.excel)

plot(resid(lm.m.excel)~lat, data = dat.sel.m.excel)


lm.m.excel2 <- lm(dat.sel.m.excel[, c("excellent", varsel, "heating", "coast", "year")])
summary(lm.m.excel2)
AIC(lm.m.excel2)

lm.m.excel3 <- lm(dat.sel.m.excel[, c("excellent", varsel, "heating", "coast", "city")])
summary(lm.m.excel3)
AIC(lm.m.excel3)

library(ape)
source("./codes/geodistance.R")
city <- read.csv("./data/city_geo.csv", stringsAsFactors=FALSE)
df.cities <- city[,c(1, 5, 6)]
names(df.cities) <- c("name", "lat", "lon")
city.dist.km <- round(GeoDistanceInMetresMatrix(df.cities) / 1000)
city.dists.inv <- 1/city.dist.km
diag(city.dists.inv) <- 0


moran.g.lm <- tapply(resid(lm.m.excel), as.factor(dat.sel.m.excel$year), 
              function(x) Moran.I(x, city.dists.inv)
)

moran.g.lm <- sapply(moran.g.lm, function(x) c(x$observed, x$p.v))
dimnames(moran.g.lm)[[1]] <- c("Moran's I", "P值")

####### glm
glm.m.excel <- glm(excellent~con_area+gas_control
                   +mean_temp+rain+humid+wind_speed+heating+coast
                   +lat,
                   data=dat.sel.m.excel,
                   family = "poisson")
summary(glm.m.excel)
AIC(glm.m.excel)

####### gam
library(mgcv)

gam.m.excel <- gam(excellent~con_area+gas_control
                   +mean_temp+rain+humid+wind_speed+heating+coast
                   +s(lat),
                   data=dat.sel.m.excel,
                   family = "poisson", method = "REML")

gam.m.excel.best <- gam(excellent~con_area+gas_control
                   +mean_temp+rain+humid+wind_speed+heating
                   +s(lat),
                   data=dat.sel.m.excel,
                   family = "poisson", method = "REML")

summary(gam.m.excel)
AIC(gam.m.excel)
plot(gam.m.excel)

summary(gam.m.excel.best)
AIC(gam.m.excel.best)
plot(gam.m.excel.best)

par(mfrow = c(1,3))
par(mar=c(5,4,2,1))
plot.gam(gam.m.excel,
         select=1,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="excellent",xlab = "纬度"
         )
par(mar=c(5,3,2,1))
plot.gam(gam.m.excel,
         select=2,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="",xlab = "汽车拥有量"
         )
par(mar=c(5,3,2,1))
plot.gam(gam.m.excel,
         select=3,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="",xlab = "平均气温"
         )

par(op)

AIC(gam.m.excel)
BIC(gam.m.excel)

moran.g.gam <- tapply(resid(gam.m.excel), as.factor(dat.sel.m.excel$year), 
              function(x) Moran.I(x, city.dists.inv)
)

moran.g.gam <- sapply(moran.g.gam, function(x) c(x$observed, x$p.v))
dimnames(moran.g.gam)[[1]] <- c("Moran's I", "P值")

####### lme

lme.m.excel <- gamm(excellent~con_area+gas_control+mean_temp+rain+humid+wind_speed +heating+coast+lat,
              random=list(city=~1), data=dat.sel.m.excel, method = "REML")
summary(lme.m.excel$lme)
AIC(lme.m.excel$lme)
BIC(lme.m.excel$lme)

ranef(lme.m.excel$lme)

moran.g.lme <- tapply(resid(lme.m.excel$lme), as.factor(dat.sel.m.excel$year), 
              function(x) Moran.I(x, city.dists.inv)
)

moran.g.lme <- sapply(moran.g.lme, function(x) c(x$observed, x$p.v))
dimnames(moran.g.lme)[[1]] <- c("Moran's I", "P值")

####### gamm
gamm.m.excel <- gamm(excellent~con_area+gas_control
                     +mean_temp+rain+humid+wind_speed+heating+coast
                     +s(lat),
                     random=list(city=~1), data=dat.sel.m.excel,
                     family = "poisson", method = "REML")

gamm.m.excel <- gamm(excellent~con_area+gas_control
                     +mean_temp+rain+humid+wind_speed+heating
                     +s(lat),
                     random=list(city=~1), data=dat.sel.m.excel,
                     family = "poisson", method = "REML")

summary(gamm.m.excel$lme)
summary(gamm.m.excel$gam)
AIC(gamm.m.excel$lme)
BIC(gamm.m.excel$lme)

plot(gamm.m.excel$gam)

ranef(gamm.m.excel$lme)


moran.g.gamm <- tapply(resid(gamm.m.excel$lme), as.factor(dat.sel.m.excel$year), 
              function(x) Moran.I(x, city.dists.inv)
)

moran.g.gamm <- sapply(moran.g.gamm, function(x) c(x$observed, x$p.v))
dimnames(moran.g.gamm)[[1]] <- c("Moran's I", "P值")

