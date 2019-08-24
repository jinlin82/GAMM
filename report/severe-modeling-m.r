rm(list=ls())
varlist <- read.csv("./data/var_list.csv")
dat.sel.m.severe <- read.csv("./results/dat.sel.m.severe.csv")

varinfo <- varlist[match(names(dat.sel.m.severe), as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]

dat.sel.m.severe$city <- factor(dat.sel.m.severe$city)
dat.sel.m.severe$year <- factor(dat.sel.m.severe$year)
dat.sel.m.severe$month <- factor(dat.sel.m.severe$month)
dat.sel.m.severe$heating <- factor(dat.sel.m.severe$heating)
dat.sel.m.severe$coast <- factor(dat.sel.m.severe$coast)


####### 设置水平值
levels(dat.sel.m.severe$heating) <- c("无集中供暖", "集中供暖")
levels(dat.sel.m.severe$coast) <- c("非临海", "临海")

dat.sel.m.severe$severe[dat.sel.m.severe$severe>0] <- 1

####### 数据标准化
## dat.sel.m.severe[,-c(23, 24)] <- scale(dat.sel.m.severe[,-c(23,24)])



####### gamsel #######################################################################
library(gamsel)
gam1 <- gamsel(as.matrix(dat.sel.m.severe[,-c(1:4, 24:25)]), as.matrix(dat.sel.m.severe[,4]), gamma = 0.48, family = "binomial", num_lambda = 50)
gam1

par(mfrow=c(6,4), mar=c(2.1, 3.1, 2.1, 1.1))
## plot(gam1,newx=as.matrix(dat.sel.m.severe[,-c(1, 23:24)]), index=22)
mapply(function(i, y) {
    plot(gam1,newx=as.matrix(dat.sel.m.severe[,-c(1:4, 24:25)]),
         index=37, which = i, main = y, cex.main = 0.95, ylim = c(-0.2, 0.2))},
    as.list(1:22),
    as.list(varinfo[,2][-c(1:4, 24:25)]))

gam1

varsel <- as.character(varinfo[,3][-c(1:4, 24:25)][getActive(gam1, 36, "nonzero")[[1]]])

####################################### MODEL #########################################
####### 线性模型

lm.m.severe <- lm(dat.sel.m.severe[, c("severe", varsel)])
summary(lm.m.severe)
AIC(lm.m.severe)
BIC(lm.m.severe)

plot(resid(lm.m.severe)~lat, data = dat.sel.m.severe)


lm.m.severe2 <- lm(dat.sel.m.severe[, c("severe", varsel, "year")])
summary(lm.m.severe2)
AIC(lm.m.severe2)

lm.m.severe3 <- lm(dat.sel.m.severe[, c("severe", varsel, "city")])
summary(lm.m.severe3)
AIC(lm.m.severe3)

library(ape)
source("./codes/geodistance.R")
city <- read.csv("./data/city_geo.csv", stringsAsFactors=FALSE)
df.cities <- city[,c(1, 5, 6)]
names(df.cities) <- c("name", "lat", "lon")
city.dist.km <- round(GeoDistanceInMetresMatrix(df.cities) / 1000)
city.dists.inv <- 1/city.dist.km
diag(city.dists.inv) <- 0


moran.g.lm <- tapply(resid(lm.m.severe), as.factor(dat.sel.m.severe$year), 
              function(x) Moran.I(x, city.dists.inv)
)

moran.g.lm <- sapply(moran.g.lm, function(x) c(x$observed, x$p.v))
dimnames(moran.g.lm)[[1]] <- c("Moran's I", "P值")

dat.sel.m.severe$severe <- as.factor(dat.sel.m.severe$severe)
levels(dat.sel.m.severe$severe) <- c("正常", "严重污染")

####### glm
glm.m.severe <- glm(severe~pop+car+gas_control
                   +mean_temp+rain+humid+wind_speed+pressure+sun
                   +heating+coast+lat,
                   data=dat.sel.m.severe,
                   family = binomial)
summary(glm.m.severe)
AIC(glm.m.severe)

####### gam
library(mgcv)

gam.m.severe <- gam(severe~s(pop)+car+gas_control
                   +s(mean_temp)+rain+humid+wind_speed+sun
                   +heating+coast+lat,
                   data=dat.sel.m.severe,
                   family = binomial, method = "REML")

gam.m.severe.best <- gam(severe~s(pop)
                   +s(mean_temp)+rain+humid+wind_speed
                   +lat,
                   data=dat.sel.m.severe,
                   family = binomial, method = "REML")

summary(gam.m.severe)
AIC(gam.m.severe)

summary(gam.m.severe.best)
AIC(gam.m.severe)

plot(gam.m.severe)

par(mfrow = c(1,3))
par(mar=c(5,4,2,1))
plot.gam(gam.m.severe,
         select=1,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="severe",xlab = "纬度"
         )
par(mar=c(5,3,2,1))
plot.gam(gam.m.severe,
         select=2,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="",xlab = "汽车拥有量"
         )
par(mar=c(5,3,2,1))
plot.gam(gam.m.severe,
         select=3,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="",xlab = "平均气温"
         )

par(op)

AIC(gam.m.severe)
BIC(gam.m.severe)

moran.g.gam <- tapply(resid(gam.m.severe), as.factor(dat.sel.m.severe$year), 
              function(x) Moran.I(x, city.dists.inv)
)

moran.g.gam <- sapply(moran.g.gam, function(x) c(x$observed, x$p.v))
dimnames(moran.g.gam)[[1]] <- c("Moran's I", "P值")

####### lme

lme.m.severe <- gamm(severe~pop+car+gas_control
                   +mean_temp+rain+humid+wind_speed+pressure+sun
                   +heating+coast+lat,
                   data=dat.sel.m.severe,
                   family = binomial, 
                   random=list(city=~1), method = "REML")

summary(lme.m.severe$lme)
AIC(lme.m.severe$lme)
BIC(lme.m.severe$lme)

ranef(lme.m.severe$lme)

moran.g.lme <- tapply(resid(lme.m.severe$lme), as.factor(dat.sel.m.severe$year), 
              function(x) Moran.I(x, city.dists.inv)
)

moran.g.lme <- sapply(moran.g.lme, function(x) c(x$observed, x$p.v))
dimnames(moran.g.lme)[[1]] <- c("Moran's I", "P值")

####### gamm
gamm.m.severe <- gamm(severe~s(pop)+car
                   +mean_temp+rain+humid+wind_speed+pressure+sun
                   +heating+coast+lat,
                   data=dat.sel.m.severe,
                   family = binomial, 
                     random=list(city=~1),
                   method = "REML")

gamm.m.severe <- gamm(severe~s(pop)
                   +s(mean_temp)+rain+humid+wind_speed+sun
                   +lat,
                   data=dat.sel.m.severe,
                   family = binomial, 
                     random=list(city=~1),
                   method = "REML"
                   )

summary(gamm.m.severe$lme)
summary(gamm.m.severe$gam)
AIC(gamm.m.severe$lme)
BIC(gamm.m.severe$lme)

plot(gamm.m.severe$gam)

ranef(gamm.m.severe$lme)


moran.g.gamm <- tapply(resid(gamm.m.severe$lme), as.factor(dat.sel.m.severe$year), 
              function(x) Moran.I(x, city.dists.inv)
)

moran.g.gamm <- sapply(moran.g.gamm, function(x) c(x$observed, x$p.v))
dimnames(moran.g.gamm)[[1]] <- c("Moran's I", "P值")

