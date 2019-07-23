
rm(list=ls())
varlist <- read.csv("./data/var_list.csv")
dat.sel.y <- read.csv("./results/dat.sel.y.csv")

varinfo <- varlist[match(names(dat.sel.y), as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]

dat.sel.y$city <- factor(dat.sel.y$city)
dat.sel.y$year <- factor(dat.sel.y$year)
dat.sel.y$heating <- factor(dat.sel.y$heating)
dat.sel.y$coast <- factor(dat.sel.y$coast)

####### 设置水平值
levels(dat.sel.y$heating) <- c("无集中供暖", "集中供暖")
levels(dat.sel.y$coast) <- c("非临海", "临海")


####### 数据标准化
## dat.sel.y[,-c(23, 24)] <- scale(dat.sel.y[,-c(23,24)])



####### gamsel #######################################################################
library(gamsel)
gam1 <- gamsel(as.matrix(dat.sel.y[,-c(1:3, 23:24)]), as.matrix(dat.sel.y$AQI))

par(mfrow=c(1,2),mar=c(5,4,3,1))
summary(gam1)

par(mfrow=c(6,4), mar=c(2.1, 3.1, 2.1, 1.1))
mapply(function(i, y) {
    plot(gam1,newx=as.matrix(dat.sel.y[,-c(1:3, 23:24)]),
         index=22, which = i, main = y, cex.main = 0.9, ylim = c(-5, 5))},
    as.list(1:22),
    as.list(varinfo[,2][-c(1:3, 23:24)]))

varsel <- c("invest", "car", "gas_control", "mean_temp", "rain", "humid", "wind_speed", "pressure", "lat", "coast", "heating")


####################################### MODEL #########################################
####### 线性模型

lm.y <- lm(dat.sel.y[, c("AQI", varsel)])
summary(lm.y)
AIC(lm.y)
BIC(lm.y)

plot(resid(lm.y)~car, data = dat.sel.y)


lm.y2 <- lm(dat.sel.y[, c("AQI", varsel, "year")])
summary(lm.y2)
AIC(lm.y2)

lm.y3 <- lm(dat.sel.y[, c("AQI", varsel, "city")])
summary(lm.y3)
AIC(lm.y3)

library(ape)
source("./codes/geodistance.R")
city <- read.csv("./data/city_geo.csv", stringsAsFactors=FALSE)
df.cities <- city[,c(1, 5, 6)]
names(df.cities) <- c("name", "lat", "lon")
city.dist.km <- round(GeoDistanceInMetresMatrix(df.cities) / 1000)
city.dists.inv <- 1/city.dist.km
diag(city.dists.inv) <- 0


moran.g.lm <- tapply(resid(lm.y), as.factor(dat.sel.y$year), 
              function(x) Moran.I(x, city.dists.inv)
)

moran.g.lm <- sapply(moran.g.lm, function(x) c(x$observed, x$p.v))
dimnames(moran.g.lm)[[1]] <- c("Moran's I", "P值")


####### gam
library(mgcv)

gam.y <- gam(AQI~te(invest)+te(car)+gas_control+te(mean_temp)+rain+humid+wind_speed+pressure+coast+heating+lat,
             data=dat.sel.y, method = "REML")

gam.y.1 <- gam(AQI~te(invest)+te(car)+gas_control+te(mean_temp)+rain+wind_speed+coast+heating+lat,
             data=dat.sel.y, method = "REML")

summary(gam.y.1)
plot(gam.y)

par(mfrow = c(1,3))
par(mar=c(5,4,2,1))
plot.gam(gam.y,
         select=1,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="AQI",xlab = "废气治理投资"
         )
par(mar=c(5,3,2,1))
plot.gam(gam.y,
         select=2,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="",xlab = "汽车拥有量"
         )
par(mar=c(5,3,2,1))
plot.gam(gam.y,
         select=3,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="",xlab = "平均气温"
         )

par(op)

AIC(gam.y)
BIC(gam.y)

moran.g.gam <- tapply(resid(gam.y.best), as.factor(dat.sel.y$year), 
              function(x) Moran.I(x, city.dists.inv)
)

moran.g.gam <- sapply(moran.g.gam, function(x) c(x$observed, x$p.v))
dimnames(moran.g.gam)[[1]] <- c("Moran's I", "P值")

####### lme

lme.y <- gamm(AQI~car+gas_control+rain+humid+wind_speed+pressure+coast+heating+lat+invest+mean_temp,
              random=list(city=~1), data=dat.sel.y, method = "REML")
summary(lme.y$lme)
AIC(lme.y$lme)
BIC(lme.y$lme)

ranef(lme.y$lme)

moran.g.lme <- tapply(resid(lme.y$lme), as.factor(dat.sel.y$year), 
              function(x) Moran.I(x, city.dists.inv)
)

moran.g.lme <- sapply(moran.g.lme, function(x) c(x$observed, x$p.v))
dimnames(moran.g.lme)[[1]] <- c("Moran's I", "P值")

####### gamm
gamm.y <- gamm(AQI~te(invest)+car+gas_control+te(mean_temp)+rain+humid+wind_speed+pressure+coast+heating+lat,
               random=list(city=~1), data=dat.sel.y, method = "REML")

summary(gamm.y$lme)
summary(gamm.y$gam)
AIC(gamm.y$lme)
BIC(gamm.y$lme)

plot(gamm.y$gam)

ranef(gamm.y$lme)


moran.g.gamm <- tapply(resid(gamm.y$lme), as.factor(dat.sel.y$year), 
              function(x) Moran.I(x, city.dists.inv)
)

moran.g.gamm <- sapply(moran.g.gamm, function(x) c(x$observed, x$p.v))
dimnames(moran.g.gamm)[[1]] <- c("Moran's I", "P值")



####### 线性混合模型
library(lme4)
library(lmerTest)

lmm <- lmer(AQI~
                ## gdp        
            ## +gdp_rate   
            +invest     
            ## +pop        
            ## +green      
            ## +passenger  
            ## +con_area   
            +car        
            ## +power      
            ## +ind_SO2    
            ## +ind_smoke  
            ## +ind_control
            +gas_control
            +mean_temp  
            +rain       
            +humid      
            +wind_speed 
            +pressure   
            ## +sun        
            +heating
            +coast
            ## +long       
            +lat        
            ## +height
            +(1|dat.y$city),     
            data=dat.sel.y)

summary(lmm)

AIC(lmm)
BIC(lmm)


########################################## UNUSED #######################################
####### GLM lasso
library(glmnet)
fm <- glmnet(as.matrix(dat.sel.y[,-c(1)]), as.matrix(dat.sel.y[,1]))
plot(fm, label = T)
print(fm)
coef(fm, s=2)




s <- cv.glmnet(as.matrix(dat.sel.y[,-c(1)]),
          as.matrix(dat.sel.y[,1]))$lambda.1se
s
coef(fm, s)

lm0 <- lm(AQI~
              ## gdp        
          ## +gdp_rate   
          +invest     
          ## +pop        
          ## +green      
          ## +passenger  
          ## +con_area   
          +car        
          ## +power      
          ## +ind_SO2    
          +ind_smoke  
          ## +ind_control
          +gas_control
          ## +mean_temp 
          ## +low_temp   
          +high_temp  
          +rain       
          ## +humid      
          +wind_speed 
          ## +pressure   
          ## +sun        
          +heating
          +coast
          ## +long       
          +lat        
          ## +height
          ## +(1|dat.y$city)
    ,     
          data=dat.sel.y)

summary(lm0)



####### GLMM lasso  感觉不成熟， 模型结果与 lme4 结果相差比较大
library(glmmLasso)

dat.sel.y$city <- dat.y$city

lmm1 <- glmmLasso(AQI~ gdp        
            +gdp_rate   
            +invest     
            +pop        
            +green      
            +passenger  
            +con_area   
            +car        
            +power      
            +ind_SO2    
            +ind_smoke  
            +ind_control
            +gas_control
            +mean_temp  
            +low_temp   
            +high_temp  
            +rain       
            +humid      
            +wind_speed 
            +pressure   
            +sun        
            +heating
            +coast
            +long       
            +lat        
            +height,
            rnd = list(city=~1),
            lambda = 1180,     
            data=dat.sel.y, final.re = T)

summary(lmm1)
