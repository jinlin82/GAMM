#prepare
rm(list=ls())
#path="c:/Works/Funds/2014年02月19日--2014年社科申请/2019-03-GAMM/";
path=getwd()
setwd(path)
tab3 <- read.csv(paste(path,'/report/results/variable.csv',sep=''))



varlist <- read.csv(paste(path,"/report/data/var_list.csv",sep=''))
dat.sel.y.severe <- read.csv(paste(path,"/report/results/dat.sel.y.severe.csv",sep=''))
varinfo <- varlist[match(names(dat.sel.y.severe), as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]
  


dat.sel.y.severe$city <- factor(dat.sel.y.severe$city)
dat.sel.y.severe$year <- factor(dat.sel.y.severe$year)
dat.sel.y.severe$heating <- factor(dat.sel.y.severe$heating)
dat.sel.y.severe$coast <- factor(dat.sel.y.severe$coast)

library(gamsel)
gam1 <- gamsel(as.matrix(dat.sel.y.severe[,-c(1:3, 23:24)]), as.matrix(dat.sel.y.severe[,3]), gamma = 0.5)


par(mfrow=c(6,4), mar=c(2.1, 3.1, 2.1, 1.1))
mapply(function(i, y) {
  plot(gam1,newx=as.matrix(dat.sel.y.severe[,-c(1:3, 23:24)]),
       index=18, which = i, main = y, cex.main = 0.95, ylim = c(-3, 3))},
  as.list(1:22),
  as.list(varinfo[,2][-c(1:3, 23:24)]))

varsel <- c(as.character(varinfo[,3][-c(1:3, 23:24)][getActive(gam1, 18, "nonzero")[[1]]]),
            "heating", "coast")
varselname <- varlist[match(varsel, as.character(varlist$var_code)), c(1:3,7)]
varselname <- as.character(varselname[order(varselname$var_no), 2])
#泊松线性模型
glm.y.severe <- glm(dat.sel.y.severe[, c("severe", varsel)], family = "poisson")
lm.y.severe.coef <- summary(glm.y.severe)$coef
#dimnames(lm.y.severe.coef) <- list(c("截距项", varselname), c("估计值", "标准误", "t值", "P值"))
AIC(glm.y.severe);BIC(glm.y.severe)
#泊松混合效应模型
library(mgcv)
lme.y.severe1 <- gamm(severe~green+pop+car
                        +rain+humid+wind_speed+sun 
                       +heating+coast+lat,
                        random=list(city=~1), data=dat.sel.y.severe,
                       family="poisson", method = "REML") 
  
AIC(lme.y.severe1$lme);BIC(lme.y.severe1$lme)

lme.y.severe <- gamm4(severe~pop+green+car
                     +rain+humid+wind_speed+sun
                     +heating+coast+lat,
                     random=~(1|city), data=dat.sel.y.severe,
                     family=poisson)

lme.y.severe.coef <- summary(lme.y.severe$mer)$coefficients

#dimnames(lme.y.severe.coef) <- list(c("截距项", varselname), c("估计值", "标准误", "t值", "P值"))
AIC(lme.y.severe$mer);BIC(lme.y.severe$mer)

#泊松可加模型
gam.y.severe1 <- gamm(severe~s(pop)+s(green)+car
                     +rain+humid+s(wind_speed)+s(sun) 
                     +heating+coast+lat, 
                     data=dat.sel.y.severe,
                     family = "poisson", method = "REML") 
summary(gam.y.severe1$lme)$tTable[-(8:11),-3]
AIC(gam.y.severe1$lme);BIC(gam.y.severe1$lme)
gam.smooth.tab.y.severe1 <- as.data.frame(summary(gam.y.severe1$gam)$s.table[,-2]) 
dimnames(gam.smooth.tab.y.severe1) <- list(varselname[c(1,2,6,7)], c("经验自由度", "卡方统计量值", "P值"))  

gam.y.severe <- gamm4(severe~s(pop)+s(green)+car
                    +rain+humid+s(wind_speed)+s(sun)
                    +heating+coast+lat,
                    data=dat.sel.y.severe,
                    family = poisson)

gam.y.severe.coef <- summary(gam.y.severe$mer)$coefficients

#dimnames(gam.y.severe.coef) <- list(c("截距项", varselname[c(3:5,8:10)]), c("估计值", "标准误", "t值", "P值"))
AIC(gam.y.severe$mer);BIC(gam.y.severe$mer)
#fig.cap = "年度空气严重污染天数泊松可加回归模型非参数曲线", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")}
par(mfrow = c(2,2))
par(mar=c(5,4,2,1))
plot.gam(gam.y.severe$gam,
         select=1,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="severe",xlab = "人口数"
)
par(mar=c(5,4,2,1))
plot.gam(gam.y.severe$gam,
         select=2,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="severe",xlab = "绿化率"
)
par(mar=c(5,4,2,1))
plot.gam(gam.y.severe$gam,
         select=3,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="severe",xlab = "平均风速"
)
par(mar=c(5,4,2,1))
plot.gam(gam.y.severe$gam,
         select=4,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="severe",xlab = "日照时数"
)
gam.smooth.tab.y.severe <- as.data.frame(summary(gam.y.severe$gam)$s.table[,-2]) 
dimnames(gam.smooth.tab.y.severe) <- list(varselname[c(1,2,6,7)], c("经验自由度", "卡方统计量值", "P值"))  

#泊松可加混合模型

gamm.y.severe1 <- gamm(severe~s(green)+s(pop)+car
                    +rain+humid+s(wind_speed)+s(sun) 
                   +heating+coast+lat, 
                   random=list(city=~1), data=dat.sel.y.severe, 
                   family = "poisson", method = "REML") 
AIC(gamm.y.severe1$lme);BIC(gamm.y.severe1$lme) 
gamm.smooth.tab.y.severe1 <- as.data.frame(summary(gamm.y.severe1$gam)$s.table[,-2])
dimnames(gamm.smooth.tab.y.severe1) <- list(varselname[c(1,2,6,7)],c("经验自由度", "F统计量值", "P值"))
  
gamm.y.severe <- gamm4(severe~s(green)+s(pop)+car
                       +rain+humid+s(wind_speed)+s(sun) 
                       +heating+coast+lat, 
                       random=~(1|city), data=dat.sel.y.severe, 
                       family = poisson) 
gamm.y.severe.coef <- summary(gamm.y.severe$mer)$coefficients
AIC(gamm.y.severe$mer);BIC(gamm.y.severe$mer) 
 

par(mfrow = c(2,2))
par(mar=c(5,4,2,1)) 
plot.gam(gamm.y.severe$gam, 
        select=1, 
        col="blue", 
        shade=T, 
        shade.col="grey",
        ylab="severe",xlab = "绿化率" )
par(mar=c(5,4,2,1)) 
plot.gam(gamm.y.severe$gam, 
         select=2, 
          col="blue",
          shade=T, 
          shade.col="grey", 
          ylab="severe",xlab = "人口数") 
par(mar=c(5,4,2,1)) 
plot.gam(gamm.y.severe$gam, 
         select=3, 
         col="blue", 
         shade=T, 
         shade.col="grey", 
         ylab="severe",xlab = "平均风速" )
par(mar=c(5,4,2,1)) 
plot.gam(gamm.y.severe$gam, 
          select=4,
          col="blue",
          shade=T, 
          shade.col="grey", 
          ylab="severe",xlab = "日照时数" )

gamm.smooth.tab.y.severe <- as.data.frame(summary(gamm.y.severe$gam)$s.table[,-2])
dimnames(gamm.smooth.tab.y.severe) <- list(varselname[c(1,2,6,7)],c("经验自由度", "F统计量值", "P值"))  
AIC(glm.y.severe);BIC(glm.y.severe)
AIC(lme.y.severe$mer);BIC(lme.y.severe$mer) 
AIC(gam.y.severe$mer);BIC(gam.y.severe$mer) 
AIC(gamm.y.severe$mer);BIC(gamm.y.severe$mer) 
##################################月度################ ##############################################
library(gamm4)

tab3 <- read.csv(paste(path,'/report/results/variable.csv',sep=''))
dat.m <- read.csv(paste(path,"/report/data/monthly.csv",sep=''))
dat.sel.m.severe <- read.csv(paste(path,"/report/results/dat.sel.m.severe.csv",sep=""))
varlist <- read.csv(paste(path,"/report/data/var_list.csv",sep=''))
varinfo <- varlist[match(names(dat.sel.m.severe), as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]

dat.sel.m.severe$city <- factor(dat.sel.m.severe$city)
dat.sel.m.severe$year <- factor(dat.sel.m.severe$year)
dat.sel.m.severe$month <- factor(dat.sel.m.severe$month)
dat.sel.m.severe$heating <- factor(dat.sel.m.severe$heating)
dat.sel.m.severe$coast <- factor(dat.sel.m.severe$coast)

dat.sel.m.severe$severe[dat.sel.m.severe$severe>0] <- 1

library(gamsel)
gam1 <- gamsel(as.matrix(dat.sel.m.severe[,-c(1:4, 24:25)]), as.matrix(dat.sel.m.severe[,4]), gamma = 0.48, family = "binomial", num_lambda = 50)
## gam1

par(mfrow=c(6,4), mar=c(2.1, 3.1, 2.1, 1.1))
## plot(gam1,newx=as.matrix(dat.sel.m.severe[,-c(1, 23:24)]), index=22)
mapply(function(i, y) {
    plot(gam1,newx=as.matrix(dat.sel.m.severe[,-c(1:4, 24:25)]),
         index=37, which = i, main = y, cex.main = 0.95, ylim = c(-0.2, 0.2))},
    as.list(1:22),
    as.list(varinfo[,2][-c(1:4, 24:25)]))



## ----logistic回归模型，tab-severe-lm-m, include=FALSE, results='markup'--------------------
severe=ifelse(dat.sel.m.severe$severe>0,1,0) 
dat.sel.m.severe$severe=severe 

varsel <- c(as.character(varinfo[,3][-c(1:4, 24:25)][getActive(gam1, 37, "nonzero")[[1]]]), 
            "heating", "coast") 
varselname <- varlist[match(varsel, as.character(varlist$var_code)), c(1:3,7)] 
varselname <- as.character(varselname[order(varselname$var_no), 2]) 

glm.m.severe1 <- gam(severe~pop+car 
                   +mean_temp+rain+humid+wind_speed+sun 
                   +heating+coast+lat, 
                   data=dat.sel.m.severe, 
                   family = binomial, method = "REML") 
AIC(glm.m.severe1);BIC(glm.m.severe1) 
lm.m.severe.coef <- summary(glm.m.severe1)$p.table
dimnames(lm.m.severe.coef) <- list(c("截距项", varselname), c("估计值", "标准误", "z值", "P值"))


 


## ----Logistic 混合效应模型 (模型 II),tab-severe-lme-m-model, include=FALSE, results='hide'---------------



## lme.m.severe <- gamm(severe~pop+car 
##                    +mean_temp+rain+humid+wind_speed+sun 
##                    +heating+coast+lat, 
##                    data=dat.sel.m.severe, 
##                    family = binomial,  
##                    random=list(city=~1), method = "REML")

lme.m.severe <- gamm4(severe~pop+car 
                   +mean_temp+rain+humid+wind_speed+sun 
                   +heating+coast+lat, 
                   data=dat.sel.m.severe, 
                   family = binomial,  
                   random=~(1|city))

AIC(lme.m.severe$mer);BIC(lme.m.severe$mer) 

lme.m.severe.coef <- summary(lme.m.severe$mer)$coefficients
dimnames(lme.m.severe.coef) <- list(c("截距项", varselname), c("估计值", "标准误", "z值", "P值"))

## ---- Logistic 可加模型 (模型III),tab-severe-gam-m, include=FALSE, results='markup'-------------------


# gam.m.severe <- gamm(severe~s(pop)+car 
#                    +s(mean_temp)+rain+humid+wind_speed+sun 
#                    +heating+coast+s(lat), 
#                    data=dat.sel.m.severe, 
#                    family = binomial, method = "REML")

# gam.m.severe2 <- gam(severe~s(pop)+car 
#                    +s(mean_temp)+rain+humid+wind_speed+sun 
#                    +heating+coast+s(lat), 
#                    data=dat.sel.m.severe, 
#                    family = binomial, method = "REML")


gam.m.severe <- gamm4(severe~s(pop)+car 
                   +s(mean_temp)+rain+humid+wind_speed+sun 
                   +heating+coast+s(lat), 
                   data=dat.sel.m.severe, 
                   family = binomial)

gam.m.severe.coef <- summary(gam.m.severe$mer)$coefficients[c(1,9,2,10,3:8,11),]
#AIC(gam.m.severe2$lme);BIC(gam.m.severe2$lme)
AIC(gam.m.severe$mer);BIC(gam.m.severe$mer)


dimnames(gam.m.severe.coef) <- list(c("截距项", varselname), c("估计值", "标准误", "z值", "P值")) 


## ----fig-severe-gam-m, eval=T, fig.height=5.5, fig.width=6.5,out.width="95%", fig.pos="H", fig.cap = "月度是否出现空 气严重污染Logistic可加模型非参数曲线", dev=c("png","cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
par(mfrow = c(2,2)) 
par(mar=c(5,4,2,1)) 
plot.gam(gam.m.severe$gam, 
         select=1, 
         col="blue", 
         shade=T, 
         shade.col="grey", 
         ylab="severe",xlab = "常住人口数" 
         ) 
plot.gam(gam.m.severe$gam, 
         select=2, 
         col="blue", 
         shade=T, 
         shade.col="grey", 
         ylab="severe",xlab = "平均温度" 
         ) 
plot.gam(gam.m.severe$gam, 
         select=3, 
         col="blue", 
         shade=T, 
         shade.col="grey", 
         ylab="severe",xlab = "纬度" 
         ) 


## ----tab-severe-gam-m-smooth, eval=T, results='markup'-------------------
gam.smooth.tab.m.severe <- as.data.frame(summary(gam.m.severe$gam)$s.table[,-2]) 

dimnames(gam.smooth.tab.m.severe) <- list(varselname[c(1,3,10)], c("经验自由度", "卡方统计量值", "P值")) 

## ---- Logistic 可加混合效应模型 (模型 IV),tab-severe-gamm-m-model, include=FALSE, results='hide'--------------

gamm.m.severe <- gamm4(severe~s(pop)+car 
                   +s(mean_temp)+rain+humid+wind_speed+sun 
                   +heating+coast+s(lat), 
                   data=dat.sel.m.severe,
                   family = "binomial",
                      random=~(1|city))

AIC(gamm.m.severe$mer);BIC(gamm.m.severe$mer) 

gamm.m.severe.coef <- summary(gamm.m.severe$mer)$coefficients[c(1,9,2,10,3:8,11),]

dimnames(gamm.m.severe.coef) <- list(c("截距项", varselname), c("估计值", "标准误", "z值", "P值")) 


## ----fig-severe-gamm-m, eval=T, fig.height=5.5, fig.width=6.5,out.width="95%", fig.pos="H", fig.cap = "月度是否出现空气严重污染Logistic可加混合模型非参数曲线", dev=c("png","cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
par(mfrow = c(2,2)) 
par(mar=c(5,4,2,1)) 
plot.gam(gamm.m.severe$gam, 
         select=1, 
         col="blue", 
         shade=T, 
         shade.col="grey", 
         ylab="severe",xlab = "常住人口数" 
         ) 
par(mar=c(5,4,2,1)) 
plot.gam(gamm.m.severe$gam, 
         select=2, 
         col="blue", 
         shade=T, 
         shade.col="grey", 
         ylab="severe",xlab = "平均温度" 
         ) 
par(mar=c(5,4,2,1)) 
plot.gam(gamm.m.severe$gam, 
         select=3, 
         col="blue", 
         shade=T, 
         shade.col="grey", 
         ylab="severe",xlab = "纬度" 
         ) 



## ----tab-severe-gamm-m-smooth, eval=T, results='markup'------------------
gamm.smooth.tab.m.severe <- as.data.frame(summary(gamm.m.severe$gam)$s.table[,-2]) 

dimnames(gamm.smooth.tab.m.severe) <- list(varselname[c(1,3,10)],c("经验自由度", "F统计量值", "P值")) 


## ----四个模型汇总，tab-severe-m-huizong, eval=T, results='markup'----------------------

month=as.data.frame(cbind(lm.m.severe.coef,lme.m.severe.coef,gam.m.severe.coef,gamm.m.severe.coef))
month=as.data.frame(rbind(c("模型1",'','','',"模型2",'','','',"模型3",'','','',"模型4",'','',''),month))
abic=matrix(c(AIC(glm.m.severe1),BIC(glm.m.severe1),c(rep('',6)),
              AIC(lme.m.severe$mer),BIC(lme.m.severe$mer),c(rep('',6)),
              AIC(gam.m.severe$mer),BIC(gam.m.severe$mer),c(rep('',6)),
              AIC(gamm.m.severe$mer),BIC(gamm.m.severe$mer),c(rep('',6))),
            nrow=2,ncol=16,byrow = FALSE,
            dimnames=list(c('AIC','BIC'),colnames(month)))
month=as.data.frame(rbind(month,abic))
month[c('常住人口','平均气温','纬度'),c(9,13)] = 'NA'
#write.csv(month,"./report/results/month.csv")
