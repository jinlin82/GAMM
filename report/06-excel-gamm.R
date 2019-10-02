## ----setup, echo=F-------------------------------------------------------

################# 第 6 章 R 程序代码  ####################


knitr::opts_knit$set(root.dir = getwd())
knitr::opts_chunk$set(echo = FALSE, results = 'hide')
knitr::opts_chunk$set(warning = FALSE, message=FALSE)


## ----prepare-------------------------------------------------------------
rm(list=ls())
options(digits=4)
options(scipen=100)
graphics.off()
Sys.setlocale("LC_ALL", "Chinese")


## ----fig-excel-var-sel-y, eval=T, fig.height=8.5, fig.width=6.5,out.width="1.0\\textwidth", fig.pos="H", fig.cap = "年度空气质量为优天数Lasso自变量筛选结果", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
rm(list=ls())
varlist <- read.csv("./data/var_list.csv")
dat.sel.y.excel <- read.csv("./results/dat.sel.y.excel.csv")

varinfo <- varlist[match(names(dat.sel.y.excel), as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]

dat.sel.y.excel$city <- factor(dat.sel.y.excel$city)
dat.sel.y.excel$year <- factor(dat.sel.y.excel$year)
dat.sel.y.excel$heating <- factor(dat.sel.y.excel$heating)
dat.sel.y.excel$coast <- factor(dat.sel.y.excel$coast)

dat.sel.y.excel[dat.sel.y.excel$excellent==0,3] <- 0.01
dat.sel.y.excel$excellent <- log(dat.sel.y.excel$excellent)

library(gamsel)
gam1 <- gamsel(as.matrix(dat.sel.y.excel[,-c(1:3, 23:24)]), as.matrix(dat.sel.y.excel[,3]), gamma = 0.4)
######## gam1 解释 55.49%

par(mfrow=c(6,4), mar=c(2.1, 3.1, 2.1, 1.1))
mapply(function(i, y) {
    plot(gam1,newx=as.matrix(dat.sel.y.excel[,-c(1:3, 23:24)]),
         index=21, which = i, main = y, cex.main = 0.95, ylim = c(-1, 1))},
    as.list(1:22),
    as.list(varinfo[,2][-c(1:3, 23:24)]))



## ----tab-excel-lm-y, eval=T, results='markup'----------------------------
varsel <- c(as.character(varinfo[,3][-c(1:3, 23:24)][getActive(gam1, 21, "nonzero")[[1]]]),
            "heating", "coast")
varselname <- varlist[match(varsel, as.character(varlist$var_code)), c(1:3,7)]
varselname <- as.character(varselname[order(varselname$var_no), 2])

lm.y.excel <- lm(dat.sel.y.excel[, c("excellent", varsel)])
lm.y.excel.coef <- summary(lm.y.excel)$coef


dimnames(lm.y.excel.coef) <- list(c("截距项", varselname), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(lm.y.excel.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="年度空气质量为优天数线性回归模型回归系数表",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-excel-lm-y-res-spatial, eval=T, results='markup'----------------
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
dimnames(moran.g.lm)<- list(c("Moran's I", "P值"), paste0(2014:2017, "年"))

library("kableExtra")
knitr::kable(moran.g.lm, row.names =T, align = c("r", "r", "r", "r"),
             caption="年度空气质量为优天数线性回归模型残差空间自相关检验",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-excel-lme-y, eval=T, results='markup'---------------------------

library(mgcv)
lme.y.excel <- gamm(excellent~gdp+power+gas_control
                   +rain+humid+wind_speed
                   +heating+coast+lat,
              random=list(city=~1), data=dat.sel.y.excel, method = "REML")

lme.y.excel.coef <- summary(lme.y.excel$lme)$tTable[,-3]

dimnames(lme.y.excel.coef) <- list(c("截距项", varselname), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(lme.y.excel.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="年度空气质量为优天数线性混合效应模型回归固定效应表",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-excel-gam-y, eval=T, results='markup'---------------------------
library(mgcv)
gam.y.excel <- gam(excellent~s(gdp)+power+gas_control
                   +rain+humid+wind_speed
                   +heating+coast+s(lat),
                   data=dat.sel.y.excel, method = "REML")
gam.y.excel.coef <- summary(gam.y.excel)$p.table

dimnames(gam.y.excel.coef) <- list(c("截距项", varselname[-c(1,9)]), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(gam.y.excel.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="年度空气质量为优天数半参数回归模型参数估计结果",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----fig-excel-gam-y, eval=T, fig.height=3.5, fig.width=6.5,out.width="0.9\\textwidth", fig.pos="H", fig.cap = "年度空气质量为优天数半参数回归模型非参数曲线", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
par(mfrow = c(1,2))
par(mar=c(5,4,2,1))
plot.gam(gam.y.excel,
         select=1,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="excellent",xlab = "GDP"
         )
par(mar=c(5,3,2,1))
plot.gam(gam.y.excel,
         select=2,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="",xlab = "纬度"
         )



## ----tab-excel-gam-y-smooth, eval=T, results='markup'--------------------
gam.smooth.tab.y.excel <- summary(gam.y.excel)$s.table[,-2]

dimnames(gam.smooth.tab.y.excel) <- list(varselname[c(1,9)], c("经验自由度", "F统计量值", "P值"))

library("kableExtra")
knitr::kable(gam.smooth.tab.y.excel, row.names =T, align = c("c", "c", "c", "c"),
             caption="年度空气质量为优天数半参数回归模型非参数项显著性",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-excel-gamm-y, eval=T, results='markup'--------------------------
library(mgcv)
gamm.y.excel <- gamm(excellent~s(gdp)+power+gas_control
                   +rain+humid+wind_speed
                   +heating+coast+s(lat),
               random=list(city=~1), data=dat.sel.y.excel, method = "REML")

gamm.y.excel.coef <- summary(gamm.y.excel$lme)$tTable[-(9:10),-3]

dimnames(gamm.y.excel.coef) <- list(c("截距项", varselname[-c(1,9)]), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(gamm.y.excel.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="年度空气质量为优天数半参数混合效应模型固定效应表",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----fig-excel-gamm-y, eval=T, fig.height=3.5, fig.width=6.5,out.width="0.9\\textwidth", fig.pos="H", fig.cap = "年度空气质量为 优天数半参数混合模型非参数曲线", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
par(mfrow = c(1,2))
par(mar=c(5,4,2,1))
plot.gam(gamm.y.excel$gam,
         select=1,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="excellent",xlab = "GDP"
         )
par(mar=c(5,3,2,1))
plot.gam(gamm.y.excel$gam,
         select=2,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="",xlab = "纬度"
         )



## ----tab-excel-gamm-y-smooth, eval=T, results='markup'-------------------
gamm.smooth.tab.y.excel <- summary(gamm.y.excel$gam)$s.table[,-2]

dimnames(gamm.smooth.tab.y.excel) <- list(varselname[c(1,9)], c("经验自由度", "F统计量值", "P值"))

library("kableExtra")
knitr::kable(gamm.smooth.tab.y.excel, row.names =T, align = c("c", "c", "c", "c"),
             caption="年度空气质量为优天数半参数混合模型非参数项显著性",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-excel-gam-y-best, eval=T, results='markup'----------------------
library(mgcv)
gam.y.excel.best <- gam(excellent~s(gdp)+power+gas_control
                   +humid+wind_speed
                   +s(lat),
                   data=dat.sel.y.excel, method = "REML")


gam.y.excel.best.coef <- summary(gam.y.excel.best)$p.table

dimnames(gam.y.excel.best.coef) <- list(c("截距项", varselname[c(2,3,5,6)]), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(gam.y.excel.best.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="年度空气质量为优天数最优模型参数估计结果",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----fig-excel-gam-y-best, eval=T, fig.height=3.5, fig.width=6.5,out.width="0.9\\textwidth", fig.pos="H", fig.cap = "年度空气质量为 优天数最优模型非参数曲线", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
par(mfrow = c(1,2))
par(mar=c(5,4,2,1))
plot.gam(gam.y.excel.best,
         select=1,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="excellent",xlab = "GDP"
         )
par(mar=c(5,3,2,1))
plot.gam(gam.y.excel.best,
         select=2,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="",xlab = "纬度"
         )




## ----tab-excel-gam-y-smooth-best, eval=T, results='markup'---------------
gam.smooth.tab.y.excel <- summary(gam.y.excel.best)$s.table[,-2]

dimnames(gam.smooth.tab.y.excel) <- list(varselname[c(1,9)], c("经验自由度", "F统计量值", "P值"))

library("kableExtra")
knitr::kable(gam.smooth.tab.y.excel, row.names =T, align = c("c", "c", "c", "c"),
             caption="年度空气质量为优天数最优模型非参数项显著性",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-excel-gam-y-res-spatial, eval=T, results='markup'---------------
library(ape)
source("./codes/geodistance.R")
city <- read.csv("./data/city_geo.csv", stringsAsFactors=FALSE)
df.cities <- city[,c(1, 5, 6)]
names(df.cities) <- c("name", "lat", "lon")
city.dist.km <- round(GeoDistanceInMetresMatrix(df.cities) / 1000)
city.dists.inv <- 1/city.dist.km
diag(city.dists.inv) <- 0

moran.g.gam.best <- tapply(resid(gam.y.excel.best), as.factor(dat.sel.y.excel$year), 
              function(x) Moran.I(x, city.dists.inv))

moran.g.gam.best <- sapply(moran.g.gam.best, function(x) c(x$observed, x$p.v))
dimnames(moran.g.gam.best) <- list(c("莫兰指数", "P值"), paste0(2014:2017, "年"))

library("kableExtra")
knitr::kable(moran.g.gam.best, row.names =T, align = c("r", "r", "r", "r"),
             caption="年度空气质量为优天数最优模型残差空间自相关检验",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----fig-var-sel-m-excel, eval=T, fig.height=8.5, fig.width=6.5,out.width="1.0\\textwidth", fig.pos="H", fig.cap = "月度空气质量为优天数Lasso自变量筛选结果", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
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

library(gamsel)
gam1 <- gamsel(as.matrix(dat.sel.m.excel[,-c(1:4, 24:25)]), as.matrix(dat.sel.m.excel[,4]), gamma = 0.39)
####### gam1 解释48.22%

par(mfrow=c(6,4), mar=c(2.1, 3.1, 2.1, 1.1))
## plot(gam1,newx=as.matrix(dat.sel.m.excel[,-c(1, 23:24)]), index=22)
mapply(function(i, y) {
    plot(gam1,newx=as.matrix(dat.sel.m.excel[,-c(1:4, 24:25)]),
         index=25, which = i, main = y, cex.main = 0.95, ylim = c(-10, 10))},
    as.list(1:22),
    as.list(varinfo[,2][-c(1:4, 24:25)]))



## ----tab-excel-lm-m, eval=T, results='markup'----------------------------
varsel <- c(as.character(varinfo[,3][-c(1:4, 24:25)][getActive(gam1, 25, "nonzero")[[1]]]),
            "heating", "coast")
varselname <- varlist[match(varsel, as.character(varlist$var_code)), c(1:3,7)]
varselname <- as.character(varselname[order(varselname$var_no), 2])

glm.m.excel <- glm(excellent~con_area+gas_control
                   +mean_temp+rain+humid+wind_speed+heating+coast
                   +lat,
                   data=dat.sel.m.excel,
                   family = "poisson")
lm.m.excel.coef <- summary(glm.m.excel)$coef


dimnames(lm.m.excel.coef) <- list(c("截距项", varselname), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(lm.m.excel.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="月度空气质量为优天数泊松回归模型回归系数表",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-excel-lm-m-res-spatial, eval=F, results='markup'----------------
## library(ape)
## source("./codes/geodistance.R")
## city <- read.csv("./data/city_geo.csv", stringsAsFactors=FALSE)
## df.cities <- city[,c(1, 5, 6)]
## names(df.cities) <- c("name", "lat", "lon")
## city.dist.km <- round(GeoDistanceInMetresMatrix(df.cities) / 1000)
## city.dists.inv <- 1/city.dist.km
## diag(city.dists.inv) <- 0
## 
## moran.g.lm <- tapply(resid(glm.m.excel), as.factor(dat.sel.m.excel$year),
##               function(x) Moran.I(x, city.dists.inv)
## )
## 
## moran.g.lm <- sapply(moran.g.lm, function(x) c(x$observed, x$p.v))
## dimnames(moran.g.lm)<- list(c("Moran's I", "P值"), paste0(2014:2017, "年"))
## 
## library("kableExtra")
## knitr::kable(moran.g.lm, row.names =T, align = c("r", "r", "r", "r"),
##              caption="月度空气质量为优天数泊松回归模型残差空间自相关检验",digits = 4,
##              longtable = TRUE, booktabs = TRUE, escape = F) %>%
##     kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
##                   ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
##                   repeat_header_text = "(续)")%>%
##     kable_styling(full_width = T) %>%
##     column_spec(1, width = c("4.5cm"))
## 


## ----tab-excel-lme-m, eval=T, results='markup'---------------------------
library(mgcv)
lme.m.excel <- gamm(excellent~con_area+gas_control+mean_temp+rain+humid+wind_speed +heating+coast+lat,
              random=list(city=~1), data=dat.sel.m.excel, method = "REML")

lme.m.excel.coef <- summary(lme.m.excel$lme)$tTable[,-3]

dimnames(lme.m.excel.coef) <- list(c("截距项", varselname), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(lme.m.excel.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="月度空气质量为优天数泊松混合效应模型回归固定效应表",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-excel-gam-m, eval=T, results='markup'---------------------------

library(mgcv)
gam.m.excel <- gam(excellent~con_area+gas_control
                   +mean_temp+rain+humid+wind_speed+heating+coast
                   +s(lat),
                   data=dat.sel.m.excel,
                   family = "poisson", method = "REML")
gam.m.excel.coef <- summary(gam.m.excel)$p.table

dimnames(gam.m.excel.coef) <- list(c("截距项", varselname[-c(9)]), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(gam.m.excel.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="月度空气质量为优天数泊松半参数回归模型参数估计结果",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----fig-excel-gam-m, eval=T, fig.height=3.5, fig.width=5,out.width="0.7\\textwidth", fig.align='center', fig.pos="H", fig.cap = "月度空气质量为优天数泊松可加模型非参数曲线", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
par(mfrow = c(1,1))
par(mar=c(5,4,2,1))
par(cex.axis = 0.9, cex.lab = 0.9)
plot.gam(gam.m.excel,
         select=1,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="excellent",xlab = "纬度"
         )



## ----tab-excel-gam-m-smooth, eval=T, results='markup'--------------------
gam.smooth.tab.m.excel <- t(as.data.frame(summary(gam.m.excel)$s.table[,-2]))

dimnames(gam.smooth.tab.m.excel) <- list(varselname[c(9)], c("经验自由度", "卡方统计量值", "P值"))

library("kableExtra")
knitr::kable(gam.smooth.tab.m.excel, row.names =T, align = c("c", "c", "c", "c"),
             caption="月度空气质量为优天数泊松半参数回归模型非参数项显著性",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-excel-gamm-m-model, eval=T, results='hide'----------------------

library(mgcv)
gamm.m.excel <- gamm(excellent~con_area+gas_control
                     +mean_temp+rain+humid+wind_speed+heating+coast
                     +s(lat),
                     random=list(city=~1), data=dat.sel.m.excel,
                     family = "poisson", method = "REML")



## ----tab-excel-gamm-m, eval=T, results='markup'--------------------------
gamm.m.excel.coef <- summary(gamm.m.excel$lme)$tTable[-(10),-3]

dimnames(gamm.m.excel.coef) <- list(c("截距项", varselname[-c(9)]), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(gamm.m.excel.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="月度空气质量为优天数泊松可加混合效应模型回归固定效应表",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----fig-excel-gamm-m, eval=T, fig.height=3.5, fig.width=5,out.width="0.7\\textwidth", fig.align='center', fig.pos="H", fig.cap = "月度空气质量为优天数泊松可加混合模型非参数曲线", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
par(mfrow = c(1,1))
par(mar=c(5,4,2,1))
par(cex.axis = 0.9, cex.lab = 0.9)

plot.gam(gamm.m.excel$gam,
         select=1,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="excellent",xlab = "纬度"
         )



## ----tab-excel-gamm-m-smooth, eval=T, results='markup'-------------------
gamm.smooth.tab.m.excel <- t(as.data.frame(summary(gamm.m.excel$gam)$s.table[,-2]))

dimnames(gamm.smooth.tab.m.excel) <- list(varselname[c(9)],c("经验自由度", "F统计量值", "P值"))

library("kableExtra")
knitr::kable(gamm.smooth.tab.m.excel, row.names =T, align = c("c", "c", "c", "c"),
             caption="月度空气质量为优天数泊松可加混合模型非参数项显著性",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-excel-gam-m-best, eval=T, results='markup'----------------------

library(mgcv)
gam.m.excel.best <- gam(excellent~con_area+gas_control
                   +mean_temp+rain+humid+wind_speed+heating
                   +s(lat),
                   data=dat.sel.m.excel,
                   family = "poisson", method = "REML")


gam.m.excel.best.coef <- summary(gam.m.excel.best)$p.table

dimnames(gam.m.excel.best.coef) <- list(c("截距项", varselname[-c(8,9)]), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(gam.m.excel.best.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="月度空气质量为优天数最优模型参数估计结果",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----fig-excel-gam-m-best, eval=T, fig.height=3.5, fig.width=5,out.width="0.7\\textwidth", fig.align='center', fig.pos="H", fig.cap = "月度空气质量为优天数最优模型非参数曲线", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
par(mfrow = c(1,1))
par(mar=c(5,4,2,1))
par(cex.axis = 0.9, cex.lab = 0.9)

plot.gam(gam.m.excel.best,
         select=1,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="excellent",xlab = "纬度"
         )



## ----tab-excel-gam-m-smooth-best, eval=T, results='markup'---------------
gam.smooth.tab.m.excel <- t(as.data.frame(summary(gam.m.excel.best)$s.table[,-2]))

dimnames(gam.smooth.tab.m.excel) <- list(varselname[c(9)], c("经验自由度", "F统计量值", "P值"))

library("kableExtra")
knitr::kable(gam.smooth.tab.m.excel, row.names =T, align = c("c", "c", "c", "c"),
             caption="月度空气质量为优天数最优模型非参数项显著性",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))


