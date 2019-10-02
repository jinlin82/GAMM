## ----setup, echo=F-------------------------------------------------------

################# 第 5 章 R 程序代码  ####################


knitr::opts_knit$set(root.dir = getwd())
knitr::opts_chunk$set(echo = FALSE, results = 'hide')
knitr::opts_chunk$set(warning = FALSE, message=FALSE)


## ----prepare-------------------------------------------------------------
rm(list=ls())
options(digits=4)
options(scipen=100)
graphics.off()
Sys.setlocale("LC_ALL", "Chinese")


## ----fig-dep-dist, eval=T, fig.height=7,fig.width=6.5,out.width="1.0\\textwidth", fig.pos="H", fig.cap = "AQI，空气质量为优天数，严重污染天数 Q-Q 图", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
rm(list=ls())
dat.y <- read.csv("./data/yearly.csv")
dat.m <- read.csv("./data/monthly.csv")

library(car)

## par(mfrow=c(3,2))
layout(matrix(1:6, nrow=3, byrow = T), heights=c(5.5,5,6))
par(mar = c(2.1, 4.1, 4.1, 2.1))
qqPlot(dat.y$AQI, id=F, grid = F, xlab = "", ylab="年度AQI分位点",
       main = "2014-2017年度数据", lwd=1)
qqPlot(dat.m$AQI, id=F, grid = F, xlab = "", ylab="月度AQI分位点",
       main = "2014-2017月度数据", lwd=1)
par(mar = c(2.1, 4.1, 2.1, 2.1))
qqPlot(dat.y$excellent, id=F, grid = F, xlab = "", ylab="年度质量为优天数", lwd=1)
qqPlot(dat.m$excellent, id=F, grid = F, xlab = "", ylab="月度质量为优天数", lwd=1)
par(mar = c(5.1, 4.1, 2.1, 2.1))
qqPlot(dat.y$severe, id=F, grid = F, xlab = "正态分布分位点", ylab="年度严重污染天数", lwd=1)
qqPlot(dat.m$severe, id=F, grid = F, xlab = "正态分布分位点", ylab="月度严重污染天数", lwd=1)



## ----tab-dep-norm, eval=T, results='markup'------------------------------
rm(list=ls())
dat.y <- read.csv("./data/yearly.csv")
dat.m <- read.csv("./data/monthly.csv")

library(tseries)

norm.test <- list(
    shapiro.test(dat.y$AQI),
    shapiro.test(dat.y$excellent),
    shapiro.test(dat.y$severe),

    shapiro.test(dat.m$AQI),
    shapiro.test(dat.m$excellent),
    shapiro.test(dat.m$severe),

    jarque.bera.test(dat.y$AQI),
    jarque.bera.test(dat.y$excellent),
    jarque.bera.test(dat.y$severe),

    jarque.bera.test(dat.m$AQI),
    jarque.bera.test(dat.m$excellent),
    jarque.bera.test(dat.m$severe))

temp <- t(sapply(norm.test, function(x) c(x$stat, x$p.v)))
norm.test <- cbind(temp[1:6, ], temp[7:12, ])
dimnames(norm.test) <- list(c("年均AQI","年度空气质量为优天数","年度严重污染天数",
                              "月均AQI","月度空气质量为优天数","月度严重污染天数"),
                            c("W 统计量","P值", "JB统计量", "P值"))

write.csv(norm.test, "./results/aqi.y17.csv", row.names=F)

library("kableExtra")
knitr::kable(norm.test, row.names =T, align = c(rep("r",4)),
             caption="因变量正态性检验",digits = 3,
             longtable = TRUE, booktabs = TRUE, linesep  = c("","","\\hline"), escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----fig-dep-norm, eval=T, fig.height=2.5,fig.width=6.5,out.width="1.0\\textwidth", fig.pos="h", fig.cap = " 年度 空气质量为优天数，月度平均AQI对数密度图", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
rm(list=ls())
dat.y <- read.csv("./data/yearly.csv")
dat.m <- read.csv("./data/monthly.csv")

par(mfrow=c(1,2))
par(mar = c(1.1, 4.5, 2.1, 1.1))

plot(density(scale(log(dat.y$excellent+3))), main="年度空气质量为优天数对数密度",
     xlim = c(-4,4), ylim = c(0,0.55), ylab = "", col="red",
     cex.main = 0.8, cex.lab=0.7)
lines(seq(-4, 4, 0.01), dnorm(seq(-4, 4, 0.01)), lty=2, col="blue")
legend("topright", legend = c("经验密度", "标准正态密度"),lty=c(1,2), col = c("red", "blue"),
       bty = 'n', cex = 0.7, pt.cex = 1
       )

plot(density(scale(log(dat.m$AQI))), main = "月度平均AQI对数密度",
     xlim = c(-4,4), ylim = c(0,0.55), ylab = "", col = "red",
     cex.main=0.8, cex.lab=0.7)
lines(seq(-4, 4, 0.01), dnorm(seq(-4, 4, 0.01)), lty=2, col = "blue")
legend("topright", legend = c("经验密度", "标准正态密度"),lty=c(1,2), col = c("red", "blue"),
       bty = 'n', cex = 0.7, pt.cex = 1
       )


## ----tab-dep-dist, eval=T, results='markup'------------------------------
rm(list=ls())
dep.dist <- read.csv("./results/dep.dist.csv")

library("kableExtra")
knitr::kable(dep.dist, row.names =F, align = c(rep("l",3)),
             caption="因变量变换及其（近似）服从分布",digits = 3,
             longtable = TRUE, booktabs = TRUE,
             ## linesep  = c("","","\\hline"),
             escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("6.5cm"))


## ----fig-aqi-cor, eval=T, fig.height=9, fig.width=6.5,out.width="1.0\\textwidth", fig.pos="H", fig.cap = " 22个自变量与年均AQI散点图", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
rm(list=ls())
dat.y <- read.csv("./data/yearly.csv")
dat.m <- read.csv("./data/monthly.csv")
varlist <- read.csv("./data/var_list.csv")

indvar <- names(dat.y)[c(17:38)]

varinfo <- varlist[match(indvar, as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]

dat.scat <- dat.y[, as.character(varinfo[,3])]

par(mfrow = c(6, 4), mar = c(2.1, 1.1, 2.1, 1.1))
mapply(function(x,i) {plot(dat.y$AQI~x, main = i, cex.main = 1,
                           yaxt='n', pch = 20, ylim = c(40,200))
    legend("topright", legend = as.character(round(cor(dat.y$AQI,x),3)),
           bty="n", text.font=4, text.col="blue")
    lines(lowess(x, dat.y$AQI), col = "red", f=0.9)
},
dat.scat, varinfo[,2])



## ----fig-aqi-boxplot, eval=T, fig.height=3.5, fig.width=7.5,out.width="1.0\\textwidth", fig.pos="H", fig.cap = "是否供暖和是否临海年均AQI箱线图", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
rm(list=ls())
dat.y <- read.csv("./data/yearly.csv")
dat.m <- read.csv("./data/monthly.csv")
varlist <- read.csv("./data/var_list.csv")

indvar <- names(dat.y)[41:42]

varinfo <- varlist[match(indvar, as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]

dat.box <- dat.y[, as.character(varinfo[,3])]
dat.box$heating <- factor(dat.box$heating)
levels(dat.box$heating) <- c("无集中供暖", "集中供暖")
dat.box$coast <- factor(dat.box$coast)
levels(dat.box$coast) <- c("非临海", "临海")


par(mfrow = c(1, 2), mar = c(3.1, 3.1, 2.1, 1.1))
boxplot(dat.y$AQI~dat.box$heating)
boxplot(dat.y$AQI~dat.box$coast, yaxt="n")



## ---- eval=F-------------------------------------------------------------
## ####### 年度散点图
## pairs(dat.y[, c(3, 28:29)])
## pairs(dat.y[, c(3, 24:27)])
## pairs(dat.y[, c(3, 22:23)])
## pairs(dat.y[, c(3, 17:21)])
## 
## pairs(dat.y[, c(3, 32:39)])
## pairs(dat.y[, c(3, 30:32)])
## 
## boxplot(dat.y$AQI~dat.y$coast)
## boxplot(dat.y$AQI~dat.y$heating)
## 
## cor(dat.y[, c(3, 28:29)])
## cor(dat.y[, c(3, 24:27)])
## cor(dat.y[, c(3, 22:23)])
## cor(dat.y[, c(3, 17:21)])
## 
## cor(dat.y[, c(3, 32:39)])
## cor(dat.y[, c(3, 30:32)])
## 
## ####### 月度散点图
## pairs(dat.m[, c(4, 31:32)])
## pairs(dat.m[, c(4, 27:30)])
## pairs(dat.m[, c(4, 25:28)])
## pairs(dat.m[, c(4, 20:23)])
## 
## 
## pairs(dat.m[, c(4, 36:43)])
## pairs(dat.m[, c(4, 33:35)])
## 
## boxplot(dat.m$AQI~dat.m$coast)
## boxplot(dat.m$AQI~dat.m$heating)
## 
## cor(dat.m[, c(4, 31:32)])
## cor(dat.m[, c(4, 27:30)])
## cor(dat.m[, c(4, 25:28)])
## cor(dat.m[, c(4, 20:23)])
## 
## 
## cor(dat.m[, c(4, 36:43)])
## cor(dat.m[, c(4, 33:35)])
## 


## ----eval=T--------------------------------------------------------------
rm(list=ls())
dat.y <- read.csv("./data/yearly.csv")
dat.m <- read.csv("./data/monthly.csv")
varlist <- read.csv("./data/var_list.csv")

####### 年度数据
indvar <- names(dat.y)[c(1:3, 17:38, 41:42)]
varinfo <- varlist[match(indvar, as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]
dat.sel.y <- dat.y[, as.character(varinfo[,3])]


####### 经纬度转换
dat.sel.y$lat <- paste(substr(dat.sel.y$lat,1,2), substr(dat.sel.y$lat,3,4))
dat.sel.y$long <- paste(substr(dat.sel.y$long, 1, nchar(dat.sel.y$long)-2),
                   substr(dat.sel.y$long, nchar(dat.sel.y$long)-1, nchar(dat.sel.y$long)))

dat.sel.y$lat = as.numeric(measurements::conv_unit(dat.sel.y$lat,
                                   from = 'deg_dec_min', to = 'dec_deg'))
dat.sel.y$long = as.numeric(measurements::conv_unit(dat.sel.y$long,
                                                    from = 'deg_dec_min', to = 'dec_deg'))
dat.sel.y$height <- dat.sel.y$height/10

####### 设置水平值
## levels(dat.sel.y$heating) <- c("无集中供暖", "集中供暖")
## levels(dat.sel.y$coast) <- c("非临海", "临海")


####### 数据标准化
## dat.sel.y[,-c(23, 24)] <- scale(dat.sel.y[,-c(23,24)])

write.csv(dat.sel.y, "./results/dat.sel.y.csv", row.names=F)

dat.sel.y.excel <- dat.sel.y
dat.sel.y.excel$AQI <- dat.y$excellent
colnames(dat.sel.y.excel)[3] <- "excellent"
write.csv(dat.sel.y.excel, "./results/dat.sel.y.excel.csv", row.names=F)

dat.sel.y.severe <- dat.sel.y
dat.sel.y.severe$AQI <- dat.y$severe
colnames(dat.sel.y.severe)[3] <- "severe"
write.csv(dat.sel.y.severe, "./results/dat.sel.y.severe.csv", row.names=F)

####### 月度数据
indvar <- names(dat.m)[c(1:4, 20:41, 44:45)]

varinfo <- varlist[match(indvar, as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]

dat.sel.m <- dat.m[, as.character(varinfo[,3])]


####### 经纬度转换
dat.sel.m$lat <- paste(substr(dat.sel.m$lat,1,2), substr(dat.sel.m$lat,3,4))
dat.sel.m$long <- paste(substr(dat.sel.m$long, 1, nchar(dat.sel.m$long)-2),
                   substr(dat.sel.m$long, nchar(dat.sel.m$long)-1, nchar(dat.sel.m$long)))

dat.sel.m$lat = as.numeric(measurements::conv_unit(dat.sel.m$lat,
                                   from = 'deg_dec_min', to = 'dec_deg'))
dat.sel.m$long = as.numeric(measurements::conv_unit(dat.sel.m$long,
                                                    from = 'deg_dec_min', to = 'dec_deg'))
dat.sel.m$height <- dat.sel.m$height/10

####### 设置水平值
## levels(dat.sel.m$heating) <- c("无集中供暖", "集中供暖")
## levels(dat.sel.m$coast) <- c("非临海", "临海")


####### 数据标准化
## dat.sel.m[,-c(23, 24)] <- scale(dat.sel.m[,-c(23,24)])

write.csv(dat.sel.m, "./results/dat.sel.m.csv", row.names=F)

dat.sel.m.excel <- dat.sel.m
dat.sel.m.excel$AQI <- dat.m$excellent
colnames(dat.sel.m.excel)[4] <- "excellent"
write.csv(dat.sel.m.excel, "./results/dat.sel.m.excel.csv", row.names=F)

dat.sel.m.severe <- dat.sel.m
dat.sel.m.severe$AQI <- dat.m$severe
colnames(dat.sel.m.severe)[4] <- "severe"
write.csv(dat.sel.m.severe, "./results/dat.sel.m.severe.csv", row.names=F)



## ----fig-var-sel, eval=T, fig.height=8.5, fig.width=6.5,out.width="1.0\\textwidth", fig.pos="H", fig.cap = "年均AQI Lasso自变量筛选结果", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
rm(list=ls())
varlist <- read.csv("./data/var_list.csv")
dat.sel.y <- read.csv("./results/dat.sel.y.csv")

varinfo <- varlist[match(names(dat.sel.y), as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]

dat.sel.y$city <- factor(dat.sel.y$city)
dat.sel.y$year <- factor(dat.sel.y$year)
dat.sel.y$heating <- factor(dat.sel.y$heating)
dat.sel.y$coast <- factor(dat.sel.y$coast)

library(gamsel)
gam1 <- gamsel(as.matrix(dat.sel.y[,-c(1:3, 23:24)]), as.matrix(dat.sel.y[,3]), gamma = 0.4)
## summary(gam1) ## 解释 68.04%

par(mfrow=c(6,4), mar=c(2.1, 3.1, 2.1, 1.1))
mapply(function(i, y) {
    plot(gam1,newx=as.matrix(dat.sel.y[,-c(1:3, 23:24)]),
         index=22, which = i, main = y, cex.main = 0.95, ylim = c(-5, 5))},
    as.list(1:22),
    as.list(varinfo[,2][-c(1:3, 23:24)]))



## ----fig-var-sel-m, eval=T, fig.height=8.5, fig.width=6.5,out.width="1.0\\textwidth", fig.pos="H", fig.cap = "对数月均AQI Lasso自变量筛选结果", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
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

library(gamsel)
gam1 <- gamsel(as.matrix(dat.sel.m[,-c(1:4, 24:25)]), as.matrix(log(dat.sel.m[,4])), gamma = 0.369)
## gam1 解释 59.70%

par(mfrow=c(6,4), mar=c(2.1, 3.1, 2.1, 1.1))
## plot(gam1,newx=as.matrix(dat.sel.m[,-c(1, 23:24)]), index=22)
mapply(function(i, y) {
    plot(gam1,newx=as.matrix(dat.sel.m[,-c(1:4, 24:25)]),
         index=27, which = i, main = y, cex.main = 0.95, ylim = c(-0.2, 0.2))},
    as.list(1:22),
    as.list(varinfo[,2][-c(1:4, 24:25)]))


## ----tab-lm-y, eval=T, results='markup'----------------------------------
rm(list=ls())
varlist <- read.csv("./data/var_list.csv")
dat.sel.y <- read.csv("./results/dat.sel.y.csv")

varinfo <- varlist[match(names(dat.sel.y), as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]

dat.sel.y$city <- factor(dat.sel.y$city)
dat.sel.y$year <- factor(dat.sel.y$year)
dat.sel.y$heating <- factor(dat.sel.y$heating)
dat.sel.y$coast <- factor(dat.sel.y$coast)

varsel <- c("invest", "car", "gas_control", "mean_temp", "rain", "humid", "wind_speed", "pressure", "heating", "coast", "lat")
varselname <- varlist[match(varsel, as.character(varlist$var_code)), c(1:3,7)]
varselname <- as.character(varselname[order(varselname$var_no), 2])

lm.y <- lm(dat.sel.y[, c("AQI", varsel)])
lm.y.coef <- summary(lm.y)$coef


dimnames(lm.y.coef) <- list(c("截距项", varselname), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(lm.y.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="年均AQI线性回归模型回归系数表",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-lm-y-res-spatial, eval=T, results='markup'----------------------
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
dimnames(moran.g.lm)<- list(c("莫兰指数", "P值"), paste0(2014:2017, "年"))

library("kableExtra")
knitr::kable(moran.g.lm, row.names =T, align = c("r", "r", "r", "r"),
             caption="年均AQI线性回归模型残差空间自相关检验",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-lme-y, eval=T, results='markup'---------------------------------
rm(list=ls())
varlist <- read.csv("./data/var_list.csv")
dat.sel.y <- read.csv("./results/dat.sel.y.csv")

varinfo <- varlist[match(names(dat.sel.y), as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]

dat.sel.y$city <- factor(dat.sel.y$city)
dat.sel.y$year <- factor(dat.sel.y$year)
dat.sel.y$heating <- factor(dat.sel.y$heating)
dat.sel.y$coast <- factor(dat.sel.y$coast)

varsel <- c("invest", "car", "gas_control", "mean_temp", "rain", "humid", "wind_speed", "pressure", "heating", "coast", "lat")
varselname <- varlist[match(varsel, as.character(varlist$var_code)), c(1:3,7)]
varselname <- as.character(varselname[order(varselname$var_no), 2])

library(mgcv)
lme.y <- gamm(AQI~invest+car+gas_control+mean_temp+rain+humid+wind_speed+pressure+heating+coast+lat,
              random=list(city=~1), data=dat.sel.y, method = "REML")

lme.y.coef <- summary(lme.y$lme)$tTable[,-3]

dimnames(lme.y.coef) <- list(c("截距项", varselname), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(lme.y.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="年均AQI线性混合效应模型回归固定效应表",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-gam-y, eval=T, results='markup'---------------------------------
rm(list=ls())
varlist <- read.csv("./data/var_list.csv")
dat.sel.y <- read.csv("./results/dat.sel.y.csv")

varinfo <- varlist[match(names(dat.sel.y), as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]

dat.sel.y$city <- factor(dat.sel.y$city)
dat.sel.y$year <- factor(dat.sel.y$year)
dat.sel.y$heating <- factor(dat.sel.y$heating)
dat.sel.y$coast <- factor(dat.sel.y$coast)

varsel <- c("invest", "car", "gas_control", "mean_temp", "rain", "humid", "wind_speed", "pressure", "heating", "coast", "lat")
varselname <- varlist[match(varsel, as.character(varlist$var_code)), c(1:3,7)]
varselname <- as.character(varselname[order(varselname$var_no), 2])

library(mgcv)
gam.y <- gam(AQI~te(invest)+te(car)+gas_control+te(mean_temp)+rain+humid+wind_speed+pressure+heating+coast+lat,
             data=dat.sel.y, method = "REML")

gam.y.coef <- summary(gam.y)$p.table

dimnames(gam.y.coef) <- list(c("截距项", varselname[-c(1,2,4)]), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(gam.y.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="年均AQI半参数回归模型参数估计结果",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----fig-gam-y, eval=T, fig.height=2.5, fig.width=6.5,out.width="1.0\\textwidth", fig.pos="H", fig.cap = "年均AQI半参数回归模型非参数曲线", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
par(mfrow = c(1,3))
par(mar=c(5,4,2,1))
plot.gam(gam.y,
         select=1,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="AQI",xlab = "固定资产投资额"
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



## ----tab-gam-y-smooth, eval=T, results='markup'--------------------------
gam.smooth.tab.y <- summary(gam.y)$s.table[,-2]

dimnames(gam.smooth.tab.y) <- list(varselname[c(1,2,4)], c("经验自由度", "F统计量值", "P值"))

library("kableExtra")
knitr::kable(gam.smooth.tab.y, row.names =T, align = c("c", "c", "c", "c"),
             caption="年均AQI半参数回归模型非参数项显著性",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-gamm-y, eval=T, results='markup'--------------------------------
rm(list=ls())
varlist <- read.csv("./data/var_list.csv")
dat.sel.y <- read.csv("./results/dat.sel.y.csv")

varinfo <- varlist[match(names(dat.sel.y), as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]

dat.sel.y$city <- factor(dat.sel.y$city)
dat.sel.y$year <- factor(dat.sel.y$year)
dat.sel.y$heating <- factor(dat.sel.y$heating)
dat.sel.y$coast <- factor(dat.sel.y$coast)

varsel <- c("invest", "car", "gas_control", "mean_temp", "rain", "humid", "wind_speed", "pressure", "heating", "coast", "lat")
varselname <- varlist[match(varsel, as.character(varlist$var_code)), c(1:3,7)]
varselname <- as.character(varselname[order(varselname$var_no), 2])

library(mgcv)
gamm.y <- gamm(AQI~te(invest)+te(car)+gas_control+te(mean_temp)+rain+humid+wind_speed+pressure+heating+coast+lat,
               random=list(city=~1), data=dat.sel.y, method = "REML")

gamm.y.coef <- summary(gamm.y$lme)$tTable[-(10:12),-3]

dimnames(gamm.y.coef) <- list(c("截距项", varselname[-c(1,2,4)]), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(gamm.y.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="年均AQI可加混合效应模型回归固定效应表",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----fig-gamm-y, eval=T, fig.height=2.5, fig.width=6.5,out.width="1.0\\textwidth", fig.pos="H", fig.cap = "年均AQI半参数混合模型非参数曲线", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
par(mfrow = c(1,3))
par(mar=c(5,4,2,1))
plot.gam(gamm.y$gam,
         select=1,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="AQI",xlab = "固定资产投资额"
         )
par(mar=c(5,3,2,1))
plot.gam(gamm.y$gam,
         select=2,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="",xlab = "汽车拥有量"
         )
par(mar=c(5,3,2,1))
plot.gam(gamm.y$gam,
         select=3,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="",xlab = "平均气温"
         )


## ----tab-gamm-y-smooth, eval=T, results='markup'-------------------------
gamm.smooth.tab.y <- summary(gamm.y$gam)$s.table[,-2]

dimnames(gamm.smooth.tab.y) <- list(varselname[c(1,2,4)], c("经验自由度", "F统计量值", "P值"))

library("kableExtra")
knitr::kable(gamm.smooth.tab.y, row.names =T, align = c("c", "c", "c", "c"),
             caption="年均AQI半参数混合模型非参数项显著性",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))


## ----tab-gam-y-best, eval=T, results='markup'----------------------------
rm(list=ls())
varlist <- read.csv("./data/var_list.csv")
dat.sel.y <- read.csv("./results/dat.sel.y.csv")

varinfo <- varlist[match(names(dat.sel.y), as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]

dat.sel.y$city <- factor(dat.sel.y$city)
dat.sel.y$year <- factor(dat.sel.y$year)
dat.sel.y$heating <- factor(dat.sel.y$heating)
dat.sel.y$coast <- factor(dat.sel.y$coast)

varsel <- c("invest", "car", "gas_control", "mean_temp", "rain", "humid", "wind_speed", "pressure", "heating", "coast", "lat")
varselname <- varlist[match(varsel, as.character(varlist$var_code)), c(1:3,7)]
varselname <- as.character(varselname[order(varselname$var_no), 2])

library(mgcv)
gam.y.best <- gam(AQI~te(invest)+te(car)+gas_control+te(mean_temp)+wind_speed+heating+coast+lat,
                  data=dat.sel.y, method = "REML")


gam.y.best.coef <- summary(gam.y.best)$p.table

dimnames(gam.y.best.coef) <- list(c("截距项", varselname[c(3,7,9:11)]), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(gam.y.best.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="年均AQI半参数回归模型参数估计结果",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----fig-gam-y-best, eval=T, fig.height=2.5, fig.width=6.5,out.width="1.0\\textwidth", fig.pos="H", fig.cap = "年均AQI最优模型非参数曲线", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
par(mfrow = c(1,3))
par(mar=c(5,4,2,1))
plot.gam(gam.y.best,
         select=1,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="AQI",xlab = "固定资产投资额"
         )
par(mar=c(5,3,2,1))
plot.gam(gam.y.best,
         select=2,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="",xlab = "汽车拥有量"
         )
par(mar=c(5,3,2,1))
plot.gam(gam.y.best,
         select=3,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="",xlab = "平均气温"
         )



## ----tab-gam-y-smooth-best, eval=T, results='markup'---------------------
gam.smooth.tab.y <- summary(gam.y.best)$s.table[,-2]

dimnames(gam.smooth.tab.y) <- list(varselname[c(1,2,4)], c("经验自由度", "F统计量值", "P值"))

library("kableExtra")
knitr::kable(gam.smooth.tab.y, row.names =T, align = c("c", "c", "c", "c"),
             caption="年均AQI半参数回归模型非参数项显著性",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-gam-y-res-spatial, eval=T, results='markup'---------------------
library(ape)
source("./codes/geodistance.R")
city <- read.csv("./data/city_geo.csv", stringsAsFactors=FALSE)
df.cities <- city[,c(1, 5, 6)]
names(df.cities) <- c("name", "lat", "lon")
city.dist.km <- round(GeoDistanceInMetresMatrix(df.cities) / 1000)
city.dists.inv <- 1/city.dist.km
diag(city.dists.inv) <- 0

moran.g.gam.best <- tapply(resid(gam.y.best), as.factor(dat.sel.y$year), 
              function(x) Moran.I(x, city.dists.inv)
)

moran.g.gam.best <- sapply(moran.g.gam.best, function(x) c(x$observed, x$p.v))
dimnames(moran.g.gam.best) <- list(c("莫兰指数", "P值"), paste0(2014:2017, "年"))

library("kableExtra")
knitr::kable(moran.g.gam.best, row.names =T, align = c("r", "r", "r", "r"),
             caption="年均AQI最优模型残差空间自相关检验",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-lm-m, eval=T, results='markup'----------------------------------
rm(list=ls())
varlist <- read.csv("./data/var_list.csv")
dat.sel.m <- read.csv("./results/dat.sel.m.csv")
month<-read.csv("./data/monthly.csv")
dat.sel.m<-merge(dat.sel.m,month,by=c("city","year","month"))
dat.sel.m<-dat.sel.m[,c(1:28,67)]
names(dat.sel.m)<-c("city","year","month","AQI","gdp","gdp_rate","invest","pop","green","passenger","con_area",
                    "car","power","ind_SO2","ind_smoke","ind_control","gas_control","mean_temp","low_temp",
                    "high_temp","rain","wind_speed","pressure","heating","coast","long","lat","height","sun")


varinfo <- varlist[match(names(dat.sel.m), as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]

dat.sel.m$city <- factor(dat.sel.m$city)
dat.sel.m$year <- factor(dat.sel.m$year)
dat.sel.m$month <- factor(dat.sel.m$month)
dat.sel.m$heating <- factor(dat.sel.m$heating)
dat.sel.m$coast <- factor(dat.sel.m$coast)
dat.sel.m$AQI<-log(dat.sel.m$AQI)

varsel <- c("con_area", "car", "gas_control", "mean_temp", "rain", "wind_speed", "pressure", "sun", "heating", "coast", "lat")
varselname <- varlist[match(varsel, as.character(varlist$var_code)), c(1:3,7)]
varselname <- as.character(varselname[order(varselname$var_no), 2])


lm.m <- lm(dat.sel.m[, c("AQI", varsel)])
lm.m.coef <- summary(lm.m)$coef
#AIC(lm.m)
#BIC(lm.m)
dimnames(lm.m.coef) <- list(c("截距项", varselname), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(lm.m.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="月均AQI线性回归模型回归系数表",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-lme-m, eval=T, results='markup'---------------------------------
rm(list=ls())
varlist <- read.csv("./data/var_list.csv")
dat.sel.m <- read.csv("./results/dat.sel.m.csv")
month<-read.csv("./data/monthly.csv")
dat.sel.m<-merge(dat.sel.m,month,by=c("city","year","month"))
#head(dat.sel.m)
dat.sel.m<-dat.sel.m[,c(1:28,67)]
#head(dat.sel.m)
names(dat.sel.m)<-c("city","year","month","AQI","gdp","gdp_rate","invest","pop","green","passenger","con_area",
                    "car","power","ind_SO2","ind_smoke","ind_control","gas_control","mean_temp","low_temp",
                    "high_temp","rain","wind_speed","pressure","heating","coast","long","lat","height","sun")


varinfo <- varlist[match(names(dat.sel.m), as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]

dat.sel.m$city <- factor(dat.sel.m$city)
dat.sel.m$year <- factor(dat.sel.m$year)
dat.sel.m$month <- factor(dat.sel.m$month)
dat.sel.m$heating <- factor(dat.sel.m$heating)
dat.sel.m$coast <- factor(dat.sel.m$coast)
dat.sel.m$AQI<-log(dat.sel.m$AQI)

varsel <- c("con_area", "car", "gas_control", "mean_temp", "rain", "wind_speed", "pressure", "sun", "heating", "coast", "lat")
varselname <- varlist[match(varsel, as.character(varlist$var_code)), c(1:3,7)]
varselname <- as.character(varselname[order(varselname$var_no), 2])

library(mgcv)
lme.m <- gamm(AQI~con_area+car+gas_control+mean_temp+rain+wind_speed+pressure+sun+heating+coast+lat,
              random=list(city=~1), data=dat.sel.m, method = "REML")
#AIC(lme.m$lme)
#BIC(lme.m$lme)
lme.m.coef <- summary(lme.m$lme)$tTable[,-3]

dimnames(lme.m.coef) <- list(c("截距项", varselname), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(lme.m.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="月均AQI线性混合效应模型回归固定效应表",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-gam-m, eval=T, results='markup'---------------------------------
rm(list=ls())
varlist <- read.csv("./data/var_list.csv")
dat.sel.m <- read.csv("./results/dat.sel.m.csv")
month<-read.csv("./data/monthly.csv")
dat.sel.m<-merge(dat.sel.m,month,by=c("city","year","month"))
#head(dat.sel.m)
dat.sel.m<-dat.sel.m[,c(1:28,67)]
#head(dat.sel.m)
names(dat.sel.m)<-c("city","year","month","AQI","gdp","gdp_rate","invest","pop","green","passenger","con_area",
                    "car","power","ind_SO2","ind_smoke","ind_control","gas_control","mean_temp","low_temp",
                    "high_temp","rain","wind_speed","pressure","heating","coast","long","lat","height","sun")


varinfo <- varlist[match(names(dat.sel.m), as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]

dat.sel.m$city <- factor(dat.sel.m$city)
dat.sel.m$year <- factor(dat.sel.m$year)
dat.sel.m$month <- factor(dat.sel.m$month)
dat.sel.m$heating <- factor(dat.sel.m$heating)
dat.sel.m$coast <- factor(dat.sel.m$coast)
dat.sel.m$AQI<-log(dat.sel.m$AQI)

varsel <- c("con_area", "car", "gas_control", "mean_temp", "rain", "wind_speed", "pressure", "sun", "heating", "coast", "lat")
varselname <- varlist[match(varsel, as.character(varlist$var_code)), c(1:3,7)]
varselname <- as.character(varselname[order(varselname$var_no), 2])

library(mgcv)
gam.m <- gam(AQI~con_area+car+gas_control+mean_temp+rain+wind_speed+pressure+sun+heating+coast+te(lat),
             data=dat.sel.m, method = "REML")
gam.m.coef <- summary(gam.m)$p.table

dimnames(gam.m.coef) <- list(c("截距项", varselname[-11]), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(gam.m.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="月均AQI半参数回归模型参数估计结果",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----fig-gam-m, eval=T, fig.height=3.5, fig.width=5,out.width="0.75\\textwidth", fig.align='center', fig.pos="H", fig.cap = "月均AQI半参数回归模型非参数曲线", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
par(mfrow = c(1,1))
par(mar=c(5,4,2,1))
par(cex.axis = 0.9, cex.lab = 0.9)
plot.gam(gam.m,
         select=1,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="log(AQI)",xlab = "纬度"
         )


## ----tab-gam-m-smooth, eval=T, results='markup'--------------------------
gam.smooth.tab.m <- t(as.matrix(summary(gam.m)$s.table[,-2]))

colnames(gam.smooth.tab.m) <-  c("经验自由度", "F统计量值", "P值")
rownames(gam.smooth.tab.m)<-varselname[11]

library("kableExtra")
knitr::kable(gam.smooth.tab.m, row.names =T, align = c("c", "c", "c", "c"),
             caption="月均AQI半参数回归模型非参数项显著性",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))


## ----tab-gamm-m, eval=T, results='markup'--------------------------------
rm(list=ls())
varlist <- read.csv("./data/var_list.csv")
dat.sel.m <- read.csv("./results/dat.sel.m.csv")
month<-read.csv("./data/monthly.csv")
dat.sel.m<-merge(dat.sel.m,month,by=c("city","year","month"))
#head(dat.sel.m)
dat.sel.m<-dat.sel.m[,c(1:28,67)]
#head(dat.sel.m)
names(dat.sel.m)<-c("city","year","month","AQI","gdp","gdp_rate","invest","pop","green","passenger","con_area",
                    "car","power","ind_SO2","ind_smoke","ind_control","gas_control","mean_temp","low_temp",
                    "high_temp","rain","wind_speed","pressure","heating","coast","long","lat","height","sun")


varinfo <- varlist[match(names(dat.sel.m), as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]

dat.sel.m$city <- factor(dat.sel.m$city)
dat.sel.m$year <- factor(dat.sel.m$year)
dat.sel.m$month <- factor(dat.sel.m$month)
dat.sel.m$heating <- factor(dat.sel.m$heating)
dat.sel.m$coast <- factor(dat.sel.m$coast)
dat.sel.m$AQI<-log(dat.sel.m$AQI)

varsel <- c("con_area", "car", "gas_control", "mean_temp", "rain", "wind_speed", "pressure", "sun", "heating", "coast", "lat")
varselname <- varlist[match(varsel, as.character(varlist$var_code)), c(1:3,7)]
varselname <- as.character(varselname[order(varselname$var_no), 2])

library(mgcv)
gamm.m <- gamm(AQI~con_area+car+gas_control+mean_temp+rain+wind_speed+pressure+sun+heating+coast+te(lat),
               random=list(city=~1), data=dat.sel.m, method = "REML")
#AIC(gamm.m$lme)
#BIC(gamm.m$lme)
gamm.m.coef <- summary(gamm.m$lme)$tTable[-12,-3]

dimnames(gamm.m.coef) <- list(c("截距项", varselname[-11]), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(gamm.m.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="月均AQI可加混合效应模型回归固定效应表",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----fig-gamm-m, eval=T, fig.height=3.5, fig.width=5,out.width="0.75\\textwidth", fig.align='center', fig.pos="H", fig.cap = "月均AQI半参数混合模型非参数曲线", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
par(mfrow = c(1,1))
par(mar=c(5,4,2,1))
par(cex.axis = 0.9, cex.lab = 0.9)


plot.gam(gamm.m$gam,
         select=1,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="log(AQI)",xlab = "纬度"
         )



## ----tab-gamm-m-smooth, eval=T, results='markup'-------------------------
gamm.smooth.tab.m <- t(as.matrix(summary(gamm.m$gam)$s.table[,-2]))

#dimnames(gamm.smooth.tab.m) <- list( varselname[11],c("经验自由度", "F统计量值", "P值"))

colnames(gamm.smooth.tab.m)<-c("经验自由度", "F统计量值", "P值")
rownames(gamm.smooth.tab.m)<-varselname[11]


library("kableExtra")
knitr::kable(gamm.smooth.tab.m, row.names =T, align = c("c", "c", "c", "c"),
             caption="月均AQI可加混合模型非参数项显著性",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-gam-m-best, eval=T, results='markup'----------------------------
rm(list=ls())
varlist <- read.csv("./data/var_list.csv")
dat.sel.m <- read.csv("./results/dat.sel.m.csv")
month<-read.csv("./data/monthly.csv")
dat.sel.m<-merge(dat.sel.m,month,by=c("city","year","month"))
#head(dat.sel.m)
dat.sel.m<-dat.sel.m[,c(1:28,67)]
#head(dat.sel.m)
names(dat.sel.m)<-c("city","year","month","AQI","gdp","gdp_rate","invest","pop","green","passenger","con_area",
                    "car","power","ind_SO2","ind_smoke","ind_control","gas_control","mean_temp","low_temp",
                    "high_temp","rain","wind_speed","pressure","heating","coast","long","lat","height","sun")


varinfo <- varlist[match(names(dat.sel.m), as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]

dat.sel.m$city <- factor(dat.sel.m$city)
dat.sel.m$year <- factor(dat.sel.m$year)
dat.sel.m$month <- factor(dat.sel.m$month)
dat.sel.m$heating <- factor(dat.sel.m$heating)
dat.sel.m$coast <- factor(dat.sel.m$coast)
dat.sel.m$AQI<-log(dat.sel.m$AQI)

varsel <- c("con_area", "car", "gas_control", "mean_temp", "rain", "wind_speed", "pressure", "sun", "heating", "coast", "lat")
varselname <- varlist[match(varsel, as.character(varlist$var_code)), c(1:3,7)]
varselname <- as.character(varselname[order(varselname$var_no), 2])

library(mgcv)
gam.m.best <- gam(AQI~con_area+car+gas_control+mean_temp+rain+wind_speed+pressure+heating+coast+te(lat),
                  data=dat.sel.m, method = "REML")


gam.m.best.coef <- summary(gam.m.best)$p.table

dimnames(gam.m.best.coef) <- list(c("截距项", varselname[c(1:7,9:10)]), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(gam.m.best.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="月均AQI最优模型参数估计结果",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----fig-gam-m-best, eval=T, fig.height=3.5, fig.width=5,out.width="0.75\\textwidth", fig.align='center', fig.pos="H", fig.cap = "月均AQI最优模型非参数曲线", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
par(mfrow = c(1,1))
par(mar=c(5,4,2,1))
par(cex.axis = 0.9, cex.lab = 0.9)

plot.gam(gam.m.best,
         select=1,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="log(AQI)",xlab = "纬度"
         )



## ----tab-gam-m-smooth-best, eval=T, results='markup'---------------------
gam.smooth.tab.m <- t(as.matrix(summary(gam.m.best)$s.table[,-2]))

#dimnames(gam.smooth.tab.m) <- list(varselname[c(1,2,4)], c("经验自由度", "F统计量值", "P值"))
colnames(gam.smooth.tab.m)<-c("经验自由度", "F统计量值", "P值")
rownames(gam.smooth.tab.m)<-varselname[11]
library("kableExtra")
knitr::kable(gam.smooth.tab.m, row.names =T, align = c("c", "c", "c", "c"),
             caption="月均AQI最优模型非参数项显著性",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))


