
rm(list=ls())
varlist <- read.csv("./data/var_list.csv")
dat.sel.y.excel <- read.csv("./results/dat.sel.y.excel.csv")

varinfo <- varlist[match(names(dat.sel.y.excel), as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]

dat.sel.y.excel$city <- factor(dat.sel.y.excel$city)
dat.sel.y.excel$year <- factor(dat.sel.y.excel$year)
dat.sel.y.excel$heating <- factor(dat.sel.y.excel$heating)
dat.sel.y.excel$coast <- factor(dat.sel.y.excel$coast)

####### 设置水平值
levels(dat.sel.y.excel$heating) <- c("无集中供暖", "集中供暖")
levels(dat.sel.y.excel$coast) <- c("非临海", "临海")

dat.sel.y.excel[dat.sel.y.excel$excellent==0,3] <- 0.01
dat.sel.y.excel$excellent <- log(dat.sel.y.excel$excellent)
####### 数据标准化
## dat.sel.y.excel[,-c(23, 24)] <- scale(dat.sel.y.excel[,-c(23,24)])



####### gamsel #######################################################################

library(gamsel)
gam1 <- gamsel(as.matrix(dat.sel.y.excel[,-c(1:3, 23:24)]), as.matrix(dat.sel.y.excel[,3]), gamma = 0.4)
## gam1

par(mfrow=c(6,4), mar=c(2.1, 3.1, 2.1, 1.1))
mapply(function(i, y) {
    plot(gam1,newx=as.matrix(dat.sel.y.excel[,-c(1:3, 23:24)]),
         index=21, which = i, main = y, cex.main = 0.95, ylim = c(-1, 1))},
    as.list(1:22),
    as.list(varinfo[,2][-c(1:3, 23:24)]))


varsel <- as.character(varinfo[,3][-c(1:3, 23:24)][getActive(gam1, 21, "nonzero")[[1]]])

####################################### MODEL #########################################
####### 线性模型

lm.y.excel <- lm(dat.sel.y.excel[, c("excellent", varsel, "heating", "coast")])
summary(lm.y.excel)
AIC(lm.y.excel)
BIC(lm.y.excel)

plot(resid(lm.y.excel)~lat, data = dat.sel.y.excel)


lm.y.excel2 <- lm(dat.sel.y.excel[, c("excellent", varsel, "heating", "coast","year")])
summary(lm.y.excel2)
AIC(lm.y.excel2)

lm.y.excel3 <- lm(dat.sel.y.excel[, c("excellent", varsel, "heating", "coast", "city")])
summary(lm.y.excel3)
AIC(lm.y.excel3)

library(ape)
source("./codes/geodistance.R")
city <- read.csv("./data/city_geo.csv", stringsAsFactors=FALSE)
df.cities <- city[,c(1, 5, 6)]
names(df.cities) <- c("name", "lat", "lon")
city.dist.km <- round(GeoDistanceInMetresMatrix(df.cities) / 1000)
city.dists.inv <- 1/city.dist.km
diag(city.dists.inv) <- 0


moran.g.lm <- tapply(resid(lm.y.excel), as.factor(dat.sel.y.excel$year), 
              function(x) Moran.I(x, city.dists.inv)
)

moran.g.lm <- sapply(moran.g.lm, function(x) c(x$observed, x$p.v))
dimnames(moran.g.lm)[[1]] <- c("Moran's I", "P值")


####### gam
library(mgcv)

gam.y.excel <- gam(excellent~s(gdp)+power+gas_control
                   +rain+humid+wind_speed
                   +s(lat)+heating+coast,
                   data=dat.sel.y.excel, method = "REML")

gam.y.excel.best <- gam(excellent~s(gdp)+power+gas_control
                   +humid+wind_speed
                   +s(lat),
                   data=dat.sel.y.excel, method = "REML")

summary(gam.y.excel)
AIC(gam.y.excel)
plot(gam.y.excel)

summary(gam.y.excel.best)
AIC(gam.y.excel.best)


par(mfrow = c(1,3))
par(mar=c(5,4,2,1))
plot.gam(gam.y.excel,
         select=1,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="excellent",xlab = "纬度"
         )
par(mar=c(5,3,2,1))
plot.gam(gam.y.excel,
         select=2,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="",xlab = "汽车拥有量"
         )
par(mar=c(5,3,2,1))
plot.gam(gam.y.excel,
         select=3,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="",xlab = "平均气温"
         )

par(op)

AIC(gam.y.excel)
BIC(gam.y.excel)

moran.g.gam <- tapply(resid(gam.y.excel.best), as.factor(dat.sel.y.excel$year), 
              function(x) Moran.I(x, city.dists.inv)
)

moran.g.gam <- sapply(moran.g.gam, function(x) c(x$observed, x$p.v))
dimnames(moran.g.gam)[[1]] <- c("Moran's I", "P值")

####### lme

lme.y.excel <- gamm(excellent~gdp+power+gas_control
                   +rain+humid+wind_speed
                   +lat+heating+coast,
              random=list(city=~1), data=dat.sel.y.excel, method = "REML")
summary(lme.y.excel$lme)
AIC(lme.y.excel$lme)
BIC(lme.y.excel$lme)

ranef(lme.y.excel$lme)

moran.g.lme <- tapply(resid(lme.y.excel$lme), as.factor(dat.sel.y.excel$year), 
              function(x) Moran.I(x, city.dists.inv)
)

moran.g.lme <- sapply(moran.g.lme, function(x) c(x$observed, x$p.v))
dimnames(moran.g.lme)[[1]] <- c("Moran's I", "P值")

####### gamm
gamm.y.excel <- gamm(excellent~s(gdp)+power+gas_control
                   +rain+humid+wind_speed
                   +s(lat)+heating+coast,
               random=list(city=~1), data=dat.sel.y.excel, method = "REML")

gamm.y.excel <- gamm(excellent~s(gdp)+power+gas_control
                   +humid+wind_speed
                   +s(lat),
               random=list(city=~1), data=dat.sel.y.excel, method = "REML")


summary(gamm.y.excel$lme)
summary(gamm.y.excel$gam)
AIC(gamm.y.excel$lme)
BIC(gamm.y.excel$lme)

plot(gamm.y.excel$gam)

ranef(gamm.y.excel$lme)


moran.g.gamm <- tapply(resid(gamm.y.excel$lme), as.factor(dat.sel.y.excel$year), 
              function(x) Moran.I(x, city.dists.inv)
)

moran.g.gamm <- sapply(moran.g.gamm, function(x) c(x$observed, x$p.v))
dimnames(moran.g.gamm)[[1]] <- c("Moran's I", "P值")

