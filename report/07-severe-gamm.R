## ----setup, echo=F-------------------------------------------------------

################# 第 7 章 R 程序代码  ####################


knitr::opts_knit$set(root.dir = getwd())
knitr::opts_chunk$set(echo = FALSE, results = 'hide')
knitr::opts_chunk$set(warning = FALSE, message=FALSE)


## ----prepare-------------------------------------------------------------
rm(list=ls())
options(digits=4)
options(scipen=100)
graphics.off()
Sys.setlocale("LC_ALL", "Chinese")


## ----fig-var-sel-y-severe, eval=T, fig.height=8.5, fig.width=6.5,out.width="1.0\\textwidth", fig.pos="H", fig.cap = "年度严重污染天数自变量筛选结果", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
rm(list=ls())
varlist <- read.csv("./data/var_list.csv")
dat.sel.y.severe <- read.csv("./results/dat.sel.y.severe.csv")

varinfo <- varlist[match(names(dat.sel.y.severe), as.character(varlist$var_code)), c(1:3,7)]
varinfo <- varinfo[order(varinfo$var_no), ]

dat.sel.y.severe$city <- factor(dat.sel.y.severe$city)
dat.sel.y.severe$year <- factor(dat.sel.y.severe$year)
dat.sel.y.severe$heating <- factor(dat.sel.y.severe$heating)
dat.sel.y.severe$coast <- factor(dat.sel.y.severe$coast)

library(gamsel)
gam1 <- gamsel(as.matrix(dat.sel.y.severe[,-c(1:3, 23:24)]), as.matrix(dat.sel.y.severe[,3]), gamma = 0.5)
gam1

par(mfrow=c(6,4), mar=c(2.1, 3.1, 2.1, 1.1))
mapply(function(i, y) {
    plot(gam1,newx=as.matrix(dat.sel.y.severe[,-c(1:3, 23:24)]),
         index=18, which = i, main = y, cex.main = 0.95, ylim = c(-3, 3))},
    as.list(1:22),
    as.list(varinfo[,2][-c(1:3, 23:24)]))


## ----tab-severe-lm-y, eval=T, results='markup'---------------------------
varsel <- c(as.character(varinfo[,3][-c(1:3, 23:24)][getActive(gam1, 18, "nonzero")[[1]]]),
            "heating", "coast")
varselname <- varlist[match(varsel, as.character(varlist$var_code)), c(1:3,7)]
varselname <- as.character(varselname[order(varselname$var_no), 2])

glm.y.severe <- glm(dat.sel.y.severe[, c("severe", varsel)], family = "poisson")
lm.y.severe.coef <- summary(glm.y.severe)$coef


dimnames(lm.y.severe.coef) <- list(c("截距项", varselname), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(lm.y.severe.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="年度空气严重污染天数泊松回归模型回归系数表",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))


## ----tab-severe-lm-y-res-spatial, eval=F, results='markup'---------------
## library(ape)
## source("./codes/geodistance.R")
## city <- read.csv("./data/city_geo.csv", stringsAsFactors=FALSE)
## df.cities <- city[,c(1, 5, 6)]
## names(df.cities) <- c("name", "lat", "lon")
## city.dist.km <- round(GeoDistanceInMetresMatrix(df.cities) / 1000)
## city.dists.inv <- 1/city.dist.km
## diag(city.dists.inv) <- 0
## 
## moran.g.lm <- tapply(resid(glm.y.severe), as.factor(dat.sel.y.severe$year),
##               function(x) Moran.I(x, city.dists.inv)
## )
## 
## moran.g.lm <- sapply(moran.g.lm, function(x) c(x$observed, x$p.v))
## dimnames(moran.g.lm)<- list(c("莫兰指数", "P值"), paste0(2014:2017, "年"))
## 
## library("kableExtra")
## knitr::kable(moran.g.lm, row.names =T, align = c("r", "r", "r", "r"),
##              caption="年度空气严重污染天数泊松回归模型残差空间自相关检验",digits = 4,
##              longtable = TRUE, booktabs = TRUE, escape = F) %>%
##     kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
##                   ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
##                   repeat_header_text = "(续)")%>%
##     kable_styling(full_width = T) %>%
##     column_spec(1, width = c("4.5cm"))
## 


## ----tab-severe-lme-y-model, eval=T, results='hide'----------------------

library(mgcv)
lme.y.severe <- gamm(severe~green+pop+car
                    +rain+humid+wind_speed+sun
                   +heating+coast+lat,
                   random=list(city=~1), data=dat.sel.y.severe,
                   family="poisson", method = "REML")


## ----tab-severe-lme-y, eval=T, results='markup'--------------------------
lme.y.severe.coef <- summary(lme.y.severe$lme)$tTable[,-3]

dimnames(lme.y.severe.coef) <- list(c("截距项", varselname), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(lme.y.severe.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="年度空气严重污染天数泊松混合效应模型回归固定效应表",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-severe-gam-y, eval=T, results='markup'--------------------------

library(mgcv)
gam.y.severe <- gam(severe~s(pop)+s(green)+car
                    +rain+humid+s(wind_speed)+s(sun)
                   +heating+coast+lat,
                   data=dat.sel.y.severe,
                   family = "poisson", method = "REML")

gam.y.severe.coef <- summary(gam.y.severe)$p.table

dimnames(gam.y.severe.coef) <- list(c("截距项", varselname[c(3:5,8:10)]), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(gam.y.severe.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="年度空气严重污染天数泊松可加回归模型参数估计结果",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----fig-severe-gam-y, eval=T, fig.height=5.5, fig.width=6.5,out.width="1.0\\textwidth", fig.pos="H", fig.cap = "年度空气严重污染天数泊松可加回归模型非参数曲线", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
par(mfrow = c(2,2))
par(mar=c(5,4,2,1))
plot.gam(gam.y.severe,
         select=1,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="severe",xlab = "人口数"
         )
par(mar=c(5,4,2,1))
plot.gam(gam.y.severe,
         select=2,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="severe",xlab = "绿化率"
         )
par(mar=c(5,4,2,1))
plot.gam(gam.y.severe,
         select=3,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="severe",xlab = "平均风速"
         )
par(mar=c(5,4,2,1))
plot.gam(gam.y.severe,
         select=4,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="severe",xlab = "日照时数"
         )



## ----tab-severe-gam-y-smooth, eval=T, results='markup'-------------------
gam.smooth.tab.y.severe <- as.data.frame(summary(gam.y.severe)$s.table[,-2])

dimnames(gam.smooth.tab.y.severe) <- list(varselname[c(1,2,6,7)], c("经验自由度", "卡方统计量值", "P值"))

library("kableExtra")
knitr::kable(gam.smooth.tab.y.severe, row.names =T, align = c("c", "c", "c", "c"),
             caption="年度空气严重污染天数泊松可加模型非参数项显著性",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-severe-gamm-y-model, eval=T, results='hide'---------------------

library(mgcv)
gamm.y.severe <- gamm(severe~s(green)+s(pop)+car
                    +rain+humid+s(wind_speed)+s(sun)
                  +heating+coast+lat,
                   random=list(city=~1), data=dat.sel.y.severe,
                   family = "poisson", method = "REML")



## ----tab-severe-gamm-y, eval=T, results='markup'-------------------------
gamm.y.severe.coef <- summary(gamm.y.severe$lme)$tTable[-(6:9),-3]

dimnames(gamm.y.severe.coef) <- list(c("截距项", varselname[c(3:5, 8:10)]), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(gamm.y.severe.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="年度空气严重污染天数泊松可加混合效应模型回归固定效应表",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----fig-severe-gamm-y, eval=T, fig.height=5.5, fig.width=6.5,out.width="1.0\\textwidth", fig.pos="H", fig.cap = "年度空气严重污染天数泊松可加混合模型非参数曲线", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
par(mfrow = c(2,2))
par(mar=c(5,4,2,1))
plot.gam(gamm.y.severe$gam,
         select=1,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="severe",xlab = "人口数"
         )
par(mar=c(5,4,2,1))
plot.gam(gamm.y.severe$gam,
         select=2,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="severe",xlab = "绿化率"
         )
par(mar=c(5,4,2,1))
plot.gam(gamm.y.severe$gam,
         select=3,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="severe",xlab = "平均风速"
         )
par(mar=c(5,4,2,1))
plot.gam(gamm.y.severe$gam,
         select=4,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="severe",xlab = "日照时数"
         )



## ----tab-severe-gamm-y-smooth, eval=T, results='markup'------------------
gamm.smooth.tab.y.severe <- as.data.frame(summary(gamm.y.severe$gam)$s.table[,-2])

dimnames(gamm.smooth.tab.y.severe) <- list(varselname[c(1,2,6,7)],c("经验自由度", "F统计量值", "P值"))

library("kableExtra")
knitr::kable(gamm.smooth.tab.y.severe, row.names =T, align = c("c", "c", "c", "c"),
             caption="年度空气严重污染天数泊松可加混合模型非参数项显著性",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-severe-gam-y-best, eval=T, results='markup'---------------------

library(mgcv)
gam.y.severe.best <- gam(severe~s(pop)
                    +rain+wind_speed+s(sun)
                   +heating+coast+lat,
                   data=dat.sel.y.severe,
                   family = "poisson", method = "REML")


gam.y.severe.best.coef <- summary(gam.y.severe.best)$p.table

dimnames(gam.y.severe.best.coef) <- list(c("截距项", varselname[c(4,6,8:10)]), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(gam.y.severe.best.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="年度空气严重污染天数最优模型参数估计结果",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----fig-severe-gam-y-best, eval=T, fig.height=3.0, fig.width=6.5,out.width="1.0\\textwidth", fig.pos="H", fig.cap = "年度空气严重污 染天数最优模型非参数曲线", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
par(mfrow = c(1,2))
par(mar=c(5,4,2,1))
par(cex.lab = 0.9)
plot.gam(gam.y.severe.best,
         select=1,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="severe",xlab = "常住人口数"
         )

par(mar=c(5,4,2,1))
plot.gam(gam.y.severe.best,
         select=2,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="severe",xlab = "日照时数"
         )



## ----tab-severe-gam-y-smooth-best, eval=T, results='markup'--------------
gam.smooth.tab.y.severe <- as.data.frame(summary(gam.y.severe.best)$s.table[,-2])

dimnames(gam.smooth.tab.y.severe) <- list(varselname[c(1,7)], c("经验自由度", "F统计量值", "P值"))

library("kableExtra")
knitr::kable(gam.smooth.tab.y.severe, row.names =T, align = c("c", "c", "c", "c"),
             caption="年度空气严重污染天数最优模型非参数项显著性",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----fig-var-sel-m-severe, eval=T, fig.height=8.5, fig.width=6.5,out.width="1.0\\textwidth", fig.pos="H", fig.cap = "月度是否出现严重污染Lasso自变量筛选结果", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
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



## ----tab-severe-lm-m, eval=T, results='markup'---------------------------
varsel <- c(as.character(varinfo[,3][-c(1:4, 24:25)][getActive(gam1, 37, "nonzero")[[1]]]),
            "heating", "coast")
varselname <- varlist[match(varsel, as.character(varlist$var_code)), c(1:3,7)]
varselname <- as.character(varselname[order(varselname$var_no), 2])

glm.m.severe <- glm(severe~pop+car
                   +mean_temp+rain+humid+wind_speed+sun
                   +heating+coast+lat,
                   data=dat.sel.m.severe,
                   family = binomial)

lm.m.severe.coef <- summary(glm.m.severe)$coef


dimnames(lm.m.severe.coef) <- list(c("截距项", varselname), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(lm.m.severe.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="月度是否出现空气严重污染Logistic回归模型回归系数表",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-severe-lme-m-model, eval=T, results='hide'----------------------

library(mgcv)

lme.m.severe <- gamm(severe~pop+car
                   +mean_temp+rain+humid+wind_speed+sun
                   +heating+coast+lat,
                   data=dat.sel.m.severe,
                   family = binomial, 
                   random=list(city=~1), method = "REML")


## ----tab-severe-lme-m, eval=T, results='markup'--------------------------
lme.m.severe.coef <- summary(lme.m.severe$lme)$tTable[,-3]

dimnames(lme.m.severe.coef) <- list(c("截距项", varselname), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(lme.m.severe.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="月度是否出现空气严重污染Logistic混合效应模型回归固定效应表",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-severe-gam-m, eval=T, results='markup'--------------------------

library(mgcv)
gam.m.severe <- gam(severe~s(pop)+car
                   +s(mean_temp)+rain+humid+wind_speed+sun
                   +heating+coast+s(lat),
                   data=dat.sel.m.severe,
                   family = binomial, method = "REML")
gam.m.severe.coef <- summary(gam.m.severe)$p.table

dimnames(gam.m.severe.coef) <- list(c("截距项", varselname[-c(1,3,10)]), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(gam.m.severe.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="月度是否出现空气严重污染Logistic可加模型参数估计结果",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----fig-severe-gam-m, eval=T, fig.height=5.5, fig.width=6.5,out.width="1.0\\textwidth", fig.pos="H", fig.cap = "月度是否出现空 气严重污染Logistic可加模型非参数曲线", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
par(mfrow = c(2,2))
par(mar=c(5,4,2,1))
plot.gam(gam.m.severe,
         select=1,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="severe",xlab = "常住人口数"
         )
plot.gam(gam.m.severe,
         select=2,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="severe",xlab = "平均温度"
         )
plot.gam(gam.m.severe,
         select=3,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="severe",xlab = "纬度"
         )


## ----tab-severe-gam-m-smooth, eval=T, results='markup'-------------------
gam.smooth.tab.m.severe <- as.data.frame(summary(gam.m.severe)$s.table[,-2])

dimnames(gam.smooth.tab.m.severe) <- list(varselname[c(1,3,10)], c("经验自由度", "卡方统计量值", "P值"))

library("kableExtra")
knitr::kable(gam.smooth.tab.m.severe, row.names =T, align = c("c", "c", "c", "c"),
             caption="月度是否出现空气严重污染Logistic可加模型非参数项显著性",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-severe-gamm-m-model, eval=T, results='hide'---------------------

library(mgcv)
gamm.m.severe <- gamm(severe~s(pop)+car
                   +s(mean_temp)+rain+humid+wind_speed+sun
                   +heating+coast+s(lat),
                      random=list(city=~1), data=dat.sel.m.severe,
                     family = "binomial", method = "REML")



## ----tab-severe-gamm-m, eval=T, results='markup'-------------------------
gamm.m.severe.coef <- summary(gamm.m.severe$lme)$tTable[-(9:11),-3]

dimnames(gamm.m.severe.coef) <- list(c("截距项", varselname[-c(1,3,10)]), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(gamm.m.severe.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="月度是否出现空气严重污染Logistic可加混合模型回归固定效应表",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----fig-severe-gamm-m, eval=T, fig.height=5.5, fig.width=6.5,out.width="1.0\\textwidth", fig.pos="H", fig.cap = "月度是否出现空气严重污染Logistic可加混合模型非参数曲线", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
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

library("kableExtra")
knitr::kable(gamm.smooth.tab.m.severe, row.names =T, align = c("c", "c", "c", "c"),
             caption="月度是否出现空气严重污染Logistic可加混合模型非参数项显著性",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----tab-severe-gam-m-best, eval=T, results='markup'---------------------

library(mgcv)
gam.m.severe.best <- gam(severe~s(pop)+car
                   +s(mean_temp)+rain+humid+wind_speed+sun
                  +s(lat),
                   data=dat.sel.m.severe,
                   family = "binomial", method = "REML")


gam.m.severe.best.coef <- summary(gam.m.severe.best)$p.table

dimnames(gam.m.severe.best.coef) <- list(c("截距项", varselname[-c(1,3,8:10)]), c("估计值", "标准误", "t值", "P值"))

library("kableExtra")
knitr::kable(gam.m.severe.best.coef, row.names =T, align = c("r", "r", "r", "r"),
             caption="月度是否出现空气严重污染最优模型参数估计结果",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))



## ----fig-severe-gam-m-best, eval=T, fig.height=5.5, fig.width=6.5,out.width="1.0\\textwidth", fig.pos="H", fig.cap = "月度是否出现空气严重污染最优模型非参数曲线", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
par(mfrow = c(2,2))
par(mar=c(5,4,2,1))
plot.gam(gam.m.severe.best,
         select=1,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="severe",xlab = "常住人口数"
         )
par(mar=c(5,4,2,1))
plot.gam(gam.m.severe.best,
         select=2,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="severe",xlab = "平均温度"
         )
par(mar=c(5,4,2,1))
plot.gam(gam.m.severe.best,
         select=3,
         col="blue",
         shade=T,
         shade.col="grey",
         ylab="severe",xlab = "纬度"
         )


## ----tab-severe-gam-m-smooth-best, eval=T, results='markup'--------------
gam.smooth.tab.m.severe <- as.data.frame(summary(gam.m.severe.best)$s.table[,-2])

dimnames(gam.smooth.tab.m.severe) <- list(varselname[c(1,3,10)], c("经验自由度", "F统计量值", "P值"))

library("kableExtra")
knitr::kable(gam.smooth.tab.m.severe, row.names =T, align = c("c", "c", "c", "c"),
             caption="月度是否出现空气严重污染最优模型非参数项显著性",digits = 4,
             longtable = TRUE, booktabs = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = c("4.5cm"))

