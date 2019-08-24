
rm(list=ls())
varlist <- read.csv("./data/var_list.csv")
dat.sel.y.severe <- read.csv("./results/dat.sel.y.severe.csv")

varinfo <- varlist[match(names(dat.sel.y.severe), as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]

dat.sel.y.severe$city <- factor(dat.sel.y.severe$city)
dat.sel.y.severe$year <- factor(dat.sel.y.severe$year)
dat.sel.y.severe$heating <- factor(dat.sel.y.severe$heating)
dat.sel.y.severe$coast <- factor(dat.sel.y.severe$coast)

####### 设置水平值
levels(dat.sel.y.severe$heating) <- c("无集中供暖", "集中供暖")
levels(dat.sel.y.severe$coast) <- c("非临海", "临海")


####### 数据标准化
## dat.sel.y.severe[,-c(23, 24)] <- scale(dat.sel.y.severe[,-c(23,24)])



####### gamsel #######################################################################

library(gamsel)
gam1 <- gamsel(as.matrix(dat.sel.y.severe[,-c(1:3, 23:24)]), as.matrix(dat.sel.y.severe[,3]), gamma = 0.5)
gam1

par(mfrow=c(6,4), mar=c(2.1, 3.1, 2.1, 1.1))
mapply(function(i, y) {
    plot(gam1,newx=as.matrix(dat.sel.y.severe[,-c(1:3, 23:24)]),
         index=18, which = i, main = y, cex.main = 0.95, ylim = c(-3, 3))},
    as.list(1:22),
    as.list(varinfo[,2][-c(1:3, 23:24)]))


varsel <- as.character(varinfo[,3][-c(1:3, 23:24)][getActive(gam1, 18, "nonzero")[[1]]])

####################################### MODEL #########################################
####### 线性模型

lm.y.severe <- glm(dat.sel.y.severe[, c("severe", varsel, "heating", "coast")], family = "poisson")
summary(lm.y.severe)
AIC(lm.y.severe)
BIC(lm.y.severe)

plot(resid(lm.y.severe)~lat, data = dat.sel.y.severe)


lm.y.severe2 <- lm(dat.sel.y.severe[, c("severe", varsel, "heating", "coast", "year")])
summary(lm.y.severe2)
AIC(lm.y.severe2)

lm.y.severe3 <- lm(dat.sel.y.severe[, c("severe", varsel, "heating", "coast", "city")])
summary(lm.y.severe3)
AIC(lm.y.severe3)

library(ape)
source("./codes/geodistance.R")
city <- read.csv("./data/city_geo.csv", stringsAsFactors=FALSE)
df.cities <- city[,c(1, 5, 6)]
names(df.cities) <- c("name", "lat", "lon")
city.dist.km <- round(GeoDistanceInMetresMatrix(df.cities) / 1000)
city.dists.inv <- 1/city.dist.km
diag(city.dists.inv) <- 0


moran.g.lm <- tapply(resid(lm.y.severe), as.factor(dat.sel.y.severe$year), 
              function(x) Moran.I(x, city.dists.inv)
)

moran.g.lm <- sapply(moran.g.lm, function(x) c(x$observed, x$p.v))
dimnames(moran.g.lm)[[1]] <- c("Moran's I", "P值")


####### gam
library(mgcv)

gam.y.severe <- gam(severe~s(green)+s(pop)+car
                    +rain+humid+s(wind_speed)+s(sun)
                   +heating+coast+lat,
                   data=dat.sel.y.severe,
                   family = "poisson", method = "REML")

gam.y.severe.best <- gam(severe~s(pop)
                    +rain+wind_speed+s(sun)
                   +heating+coast+lat,
                   data=dat.sel.y.severe,
                   family = "poisson", method = "REML")


summary(gam.y.severe)
AIC(gam.y.severe)

summary(gam.y.severe.best)
AIC(gam.y.severe.best)

plot(gam.y.severe)

par(mfrow = c(1,3))
par(mar=c(5,4,2,1))
plot.gam(gam.y.severe,
         select=1,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="severe",xlab = "纬度"
         )
par(mar=c(5,3,2,1))
plot.gam(gam.y.severe,
         select=2,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="",xlab = "汽车拥有量"
         )
par(mar=c(5,3,2,1))
plot.gam(gam.y.severe,
         select=3,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="",xlab = "平均气温"
         )

par(op)

AIC(gam.y.severe)
BIC(gam.y.severe)

moran.g.gam <- tapply(resid(gam.y.severe.best), as.factor(dat.sel.y.severe$year), 
              function(x) Moran.I(x, city.dists.inv)
)

moran.g.gam <- sapply(moran.g.gam, function(x) c(x$observed, x$p.v))
dimnames(moran.g.gam)[[1]] <- c("Moran's I", "P值")

####### lme

lme.y.severe <- gamm(severe~green+pop+car
                    +rain+humid+wind_speed+sun
                   +heating+coast+lat,
                   random=list(city=~1), data=dat.sel.y.severe,
                   family="poisson", method = "REML")

summary(lme.y.severe$lme)
AIC(lme.y.severe$lme)
BIC(lme.y.severe$lme)

ranef(lme.y.severe$lme)

moran.g.lme <- tapply(resid(lme.y.severe$lme), as.factor(dat.sel.y.severe$year), 
              function(x) Moran.I(x, city.dists.inv)
)

moran.g.lme <- sapply(moran.g.lme, function(x) c(x$observed, x$p.v))
dimnames(moran.g.lme)[[1]] <- c("Moran's I", "P值")

####### gamm
gamm.y.severe <- gamm(severe~s(green)+s(pop)+car
                    +rain+humid+s(wind_speed)+s(sun)
                   +lat,
                   random=list(city=~1), data=dat.sel.y.severe,
                   family = "poisson", method = "REML")

gamm.y.severe <- gamm(severe~+s(pop)
                    +rain+humid+wind_speed+sun
                   +lat,
                   random=list(city=~1), data=dat.sel.y.severe,
                   family = "poisson", method = "REML")

summary(gamm.y.severe$lme)
summary(gamm.y.severe$gam)
AIC(gamm.y.severe$lme)
BIC(gamm.y.severe$lme)

plot(gamm.y.severe$gam)

ranef(gamm.y.severe$lme)


moran.g.gamm <- tapply(resid(gamm.y.severe$lme), as.factor(dat.sel.y.severe$year), 
              function(x) Moran.I(x, city.dists.inv)
)

moran.g.gamm <- sapply(moran.g.gamm, function(x) c(x$observed, x$p.v))
dimnames(moran.g.gamm)[[1]] <- c("Moran's I", "P值")

