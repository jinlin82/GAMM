#prepare
rm(list=ls())
path=getwd()
setwd(path)
library(mgcv)
library("kableExtra")
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

glm.m.severe1 <- glm(severe~pop+car 
                   +mean_temp+rain+humid+wind_speed+sun 
                   +heating+coast+lat, 
                   data=dat.sel.m.severe, 
                   family = binomial) 
#AIC(glm.m.severe1);BIC(glm.m.severe1) 
lm.m.severe.coef <- summary(glm.m.severe1)$coef 


dimnames(lm.m.severe.coef) <- list(c("截距项", varselname), c("估计值", "标准误", "t值", "P值")) 


## ----Logistic 混合效应模型 (模型 II),tab-severe-lme-m-model, include=FALSE, results='hide'---------------
library(mgcv) 
lme.m.severe <- gamm(severe~pop+car 
                   +mean_temp+rain+humid+wind_speed+sun 
                   +heating+coast+lat, 
                   data=dat.sel.m.severe, 
                   family = binomial,  
                   random=list(city=~1), method = "REML") 
#AIC(lme.m.severe$lme);BIC(lme.m.severe$lme) 

lme.m.severe.coef <- summary(lme.m.severe$lme)$tTable[,-3] 

dimnames(lme.m.severe.coef) <- list(c("截距项", varselname), c("估计值", "标准误", "t值", "P值")) 


## ---- Logistic 可加模型 (模型III),tab-severe-gam-m, include=FALSE, results='markup'-------------------

library(mgcv) 
gam.m.severe <- gamm(severe~s(pop)+car 
                   +s(mean_temp)+rain+humid+wind_speed+sun 
                   +heating+coast+s(lat), 
                   data=dat.sel.m.severe, 
                   family = binomial, method = "REML") 

gam.m.severe.coef <- summary(gam.m.severe$lme)$tTable[c(1,9,2,10,3:8,11),-3]
#AIC(gam.m.severe$lme);BIC(gam.m.severe$lme) 
dimnames(gam.m.severe.coef) <- list(c("截距项", varselname), c("估计值", "标准误", "t值", "P值")) 


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

library(mgcv) 
gamm.m.severe <- gamm(severe~s(pop)+car 
                   +s(mean_temp)+rain+humid+wind_speed+sun 
                   +heating+coast+s(lat), 
                      random=list(city=~1), data=dat.sel.m.severe, 
                     family = "binomial", method = "REML") 

#AIC(gamm.m.severe$lme);BIC(gamm.m.severe$lme) 

gamm.m.severe.coef <- summary(gamm.m.severe$lme)$tTable[c(1,9,2,10,3:8,11),-3]

dimnames(gamm.m.severe.coef) <- list(c("截距项", varselname), c("估计值", "标准误", "t值", "P值")) 


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
              AIC(lme.m.severe$lme),BIC(lme.m.severe$lme),c(rep('',6)),
              AIC(gam.m.severe$lme),BIC(gam.m.severe$lme),c(rep('',6)),
              AIC(gamm.m.severe$lme),BIC(gamm.m.severe$lme),c(rep('',6))),
            nrow=2,ncol=16,byrow = FALSE,
            dimnames=list(c('AIC','BIC'),colnames(month)))
month=as.data.frame(rbind(month,abic))
month[c('常住人口','平均气温','纬度'),c(9,13)] = 'NA'
#write.csv(month,"./report/results/new_severe_month.csv")
