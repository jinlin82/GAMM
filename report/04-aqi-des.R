## ----setup, echo=F, message=F--------------------------------------------

################# 第 4 章 R 程序代码  ####################


knitr::opts_knit$set(root.dir = getwd())
knitr::opts_chunk$set(echo = FALSE, results = 'hide')
knitr::opts_chunk$set(warning = FALSE, message=FALSE)
options(knitr.kable.NA = '')


## ----prepare-------------------------------------------------------------
rm(list=ls())
options(digits=4)
options(scipen=100)
graphics.off()
Sys.setlocale("LC_ALL", "Chinese")
windowsFonts(msyh=windowsFont("微软雅黑"))
library("kableExtra")


## ----tab-air-grade, eval=T,results='markup', cache=F---------------------
tab1 <- read.csv('./results/airpollution.csv')
knitr::kable(tab1, row.names =F, align = "l", caption="环境空气污染物基本项目浓度限值",
      longtable = TRUE, booktabs = TRUE, linesep  = "", escape = F)%>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header"),
                  stripe_index = c(1:3,7:8,11:12))%>%
    kable_styling(font_size = 11)%>%
    footnote(general ="生态环境部发布《环境空气质量指数（AQI）技术规定（试行）》",
             general_title = "数据来源：")


## ----tab2, eval=T,results='markup', cache=F------------------------------
tab2 <- read.csv('./results/airindex.csv')
knitr::kable(tab2, row.names =F, align = "l", caption="空气质量指数及其等级划分",
             longtable = TRUE, booktabs = TRUE, linesep  = "")%>%
    kable_styling(full_width = T) %>%
    column_spec(1:3, width = "4cm")%>%
    footnote(general ="生态环境部发布《环境空气质量指数（AQI）技术规定（试行）》",
             general_title = "数据来源：")


## ----tab3, eval=T,results='markup', cache=F------------------------------
library("kableExtra")

tab3 <- read.csv('./results/variable.csv')
knitr::kable(tab3, row.names =F, align = "l", caption="空气质量影响因素指标体系",
             longtable = TRUE, booktabs = TRUE, escape = F,
             linesep  = c(rep("", 12), "\\hline", rep("",5), "\\hline")) %>%
			     kable_styling(latex_options = c("scale_down", "repeat_header", "hold_position"),
                 repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1:4, width = c("3cm", "3cm", "5cm", "2cm"))
			 


## ----tab4, eval=T,results='markup', cache=F------------------------------
tab4 <- read.csv('./results/city.csv')
knitr::kable(tab4, row.names =F, align = "l", caption="城市基本情况概述",
      longtable = TRUE, booktabs = TRUE, linesep  = "", escape = F)%>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    kable_styling(full_width = T) %>%
    column_spec(1:3, width = c("2cm", "2.5cm","2.5cm"))%>%
    footnote(general ="《中国气象年鉴2017》，其中纬度和经度是该城市气象观测站所在点的纬度和经度",
             general_title = "数据来源：")



## ----tab-aqi-year, eval=T, results='markup'------------------------------
dat.y <- read.csv("./data/yearly.csv")

aqi.y17 <- dat.y[dat.y$year==2017, c(1,3,10,15)]
aqi.y17$grade <- cut(aqi.y17$AQI, c(0,50,100,150), labels = c("优","良","轻度污染"))
aqi.y17$excellent.ratio <- aqi.y17$excellent/365*100
aqi.y17 <- aqi.y17[,c(1:2,5,3,6,4)]
names(aqi.y17) <- c("城市","年均AQI","等级","等级为优天数","等级为优百分比","严重污染天数")
write.csv(aqi.y17, "./results/aqi.y17.csv", row.names=F)

library("kableExtra")
knitr::kable(aqi.y17, row.names =F, align = "c", caption="2017年35个城市空气质量概况",
             longtable = TRUE, booktabs = TRUE, linesep  = "", escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    footnote(general ="通过生态环境部城市空气质量状况月报数据整理计算得到",
             general_title = "数据来源：")



## ----tab-eco, eval=T, results='markup'-----------------------------------

varlist <- read.csv("./data/var_list.csv")
dat.y <- read.csv("./data/yearly.csv")
ecovar <- names(dat.y)[c(1, 17:29)]
eco.y17 <- dat.y[dat.y$year==2017, ecovar]

varinfo <- varlist[match(ecovar[-1], as.character(varlist$var_code)), c(1,2,7)]

eco.stat <- as.data.frame(t(apply(eco.y17[,-1], 2, function(x) c(mean(x), min(x), max(x), sd(x)))))
eco.stat <- cbind(varinfo, eco.stat)
eco.stat <- eco.stat[order(eco.stat[,1]),][,-1]

names(eco.stat) <- c("变量", "单位", "均值", "最小值", "最大值", "标准差")
write.csv(eco.stat, "./results/eco.stat.csv", row.names=F)

library("kableExtra")
knitr::kable(eco.stat, row.names =F, align = c("l","l",rep("r", 4)),
             digits=2,
             caption="2017年35个城市社会经济统计变量描述统计",
             longtable = TRUE, booktabs = TRUE, linesep  = "", escape = F) %>%
    kable_styling(full_width = T) %>%
    column_spec(1:2, width = c("4.3cm","1.9cm"))%>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)",
                  )%>%
    footnote(general ="2018年《中国统计年鉴》，各城市统计年鉴的数据整理计算得到。",
             general_title = "数据来源：")



## ----tab-wea, eval=T, results='markup'-----------------------------------

varlist <- read.csv("./data/var_list.csv")
dat.y <- read.csv("./data/yearly.csv")
weavar <- names(dat.y)[c(1, 33:38)]
wea.y17 <- dat.y[dat.y$year==2017, weavar]

varinfo <- varlist[match(weavar[-1], as.character(varlist$var_code)), c(1,2,7)]

wea.stat <- as.data.frame(t(apply(wea.y17[,-1], 2, function(x) c(mean(x), min(x), max(x), sd(x)))))
wea.stat <- cbind(varinfo, wea.stat)
wea.stat <- wea.stat[order(wea.stat[,1]),][,-1]

names(wea.stat) <- c("变量", "单位", "均值", "最小值", "最大值", "标准差")
write.csv(wea.stat, "./results/wea.stat.csv", row.names=F)

library("kableExtra")
knitr::kable(wea.stat, row.names =F, align = c("l","l",rep("r", 4)),
             digits=2,
             caption="2017年35个城市气象变量描述统计",
             longtable = TRUE, booktabs = TRUE, linesep  = "", escape = F) %>%
    kable_styling(full_width = T) %>%
    column_spec(1, width = "4cm")%>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)",
                  )%>%
    footnote(general ="2018年《中国统计年鉴》重点城市气象数据，中国气象局2017年各城市气象数据库数据整理计算得到",
             general_title = "数据来源：",
             threeparttable = T)



## ----tab-geo, eval=T, results='markup'-----------------------------------
varlist <- read.csv("./data/var_list.csv")
dat.y <- read.csv("./data/yearly.csv")

geovar <- names(dat.y)[c(1,3,10,15, 41,42)]
geo.y17 <- dat.y[dat.y$year==2017, geovar]

varinfo <- varlist[match(geovar[-1], as.character(varlist$var_code)), c(1,2,7)]

geo.stat <- as.data.frame(rbind(
apply(geo.y17[2:4], 2, function(x) {temp <- t.test(x~geo.y17$heating)
    c(temp$esti, temp$stat, temp$p.v)
})[c(2,1,3,4),],
apply(geo.y17[2:4], 2, function(x) {temp <- t.test(x~geo.y17$coast)
    c(temp$esti, temp$stat, temp$p.v)
})[c(2,1,3,4),]))

geo.count <- c(as.vector(table(geo.y17[,5]))[2:1],NA,NA,
               as.vector(table(geo.y17[,6]))[2:1],NA,NA)

geo.factor <- c("是否集体供暖", NA,"t统计量值","P值", "是否临海", NA,"t统计量值","P值")
geo.factor2 <- c("是", "否", rep(NA, 2), "是", "否", rep(NA, 2))
geo.stat <- cbind(geo.factor, geo.factor2, geo.count, geo.stat)


names(geo.stat) <- c("属性", "取值", "城市数量", "AQI均值", "为优的平均天数", "严重污染平均天数")
write.csv(geo.stat, "./results/geo.stat.csv", row.names=F)

library("kableExtra")
knitr::kable(geo.stat, row.names =F, align = c("l","l","c", rep("r", 3)),
             digits=2,
             caption="2017年35个城市空气质量与地理环境关系检验",
             longtable = TRUE, booktabs = TRUE, linesep  = c("", "", "", "\\hline"), escape = F) %>%
    kable_styling(full_width = T) %>%
    column_spec(1:3, width = c("3.5cm", "1.1cm", "2cm"))%>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)",
                  )%>%
    footnote(general ="城市是否集中供暖来自各城市政府网站，城市是否临海根据城市地理位置确定",
             general_title = "数据来源：",
             threeparttable = T)



## ----fig-aqi-year, eval=T, fig.height=7.5, fig.width=6.5, out.width="1.05\\textwidth", fig.pos="h", fig.cap = "35个城市2014-2017年平均AQI变化趋势图", dev=c("cairo_pdf"), dev.args=list(family="Microsoft YaHei UI Light")----
varlist <- read.csv("./data/var_list.csv")
dat.y <- read.csv("./data/yearly.csv")

dat.y$city <- factor(as.character(dat.y$city), levels=dat.y$city[(0:34)*4+1])

library(lattice)


xyplot(AQI ~ year | city, data=dat.y,
layout = c(5,7), type = c("o"), 
as.table = TRUE, grid = F,
abline=c(list(h=c(75, 100, 125)),trellis.par.get("reference.line")),
strip = strip.custom(bg="grey", strip.levels = T),
par.strip.text=list(cex=0.8),
digits=0, cex=0.7, cex.axis = 0.8, pch=20,
scales=list(x=list(at=2014:2017, labels=c(14:17)),
            y=list(at=seq(50, 150, 50))),
xlab=list(label="年份",cex=0.9, digits=0), ylab = ""
)



## ----fig-excel-year, eval=T, fig.height=7.5, fig.width=6.5, out.width="1.05\\textwidth", fig.pos="h", fig.cap = "35城市2014-2017年空气质量为优天数点阵图", dev=c("cairo_pdf"), dev.args=list(family="Microsoft YaHei")----
varlist <- read.csv("./data/var_list.csv")
dat.y <- read.csv("./data/yearly.csv")

dat.y$city <- factor(as.character(dat.y$city), levels=dat.y$city[(0:34)*4+1])

library(lattice)

dotplot(reorder(city, excellent) ~ excellent, data=dat.y,
        jitter.y = F, pch = c("4","5", "6", "7"), cex=1,
        xlab = list(label="空气质量为优天数",cex=0.9),
        xlim=c(-15, 280),
        scales=list(x=list(at=seq(0, 270, 30))),
        type = c("p", "a"), lwd=0.5, col=1:4)



## ----fig-aqi-month, eval=T, fig.height=7.4,fig.width=6.5,out.width="1.0\\textwidth", fig.pos="h", fig.cap = "7个代表性城市2014-2017年月度平均AQI变化趋势图", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----
varlist <- read.csv("./data/var_list.csv")
dat.m <- read.csv("./data/monthly.csv")

dat.m$city <- factor(as.character(dat.m$city), levels=dat.m$city[(0:34)*48+1])

select.city <- c("北京","上海", "深圳","武汉","西安", "青岛", "昆明")
select.m <- dat.m[dat.m$city %in% select.city,c(1:4,13,18)]
select.m$city <- factor(as.character(select.m$city), levels=select.m$city[(0:34)*48+1])

library(zoo)
aqi.select.m <- tapply(select.m$AQI, select.m$city, rep)
aqi.select.m <- as.zoo(ts(sapply(aqi.select.m, rep), start=c(2014,1), frequency = 12))

library(lattice)
xyplot(aqi.select.m, type="o", pch=20,
       layout = c(1,7), as.table = T,
       ylim=c(20,250),
       strip.left = T, strip = F, grid=T,
       scales=list(x=list(at=2014:2018),
                   y=list(at=c(50, 150, 250))
                   ),
       xlab=list(label="时间",cex=0.9, digits=0),
       ## ylab = "AQI"
       )



## ----tab-season, eval=T, results='markup'--------------------------------
varlist <- read.csv("./data/var_list.csv")
dat.m <- read.csv("./data/monthly.csv")

dat.m$city <- factor(as.character(dat.m$city), levels=dat.m$city[(0:34)*48+1])

select.city <- c("北京","上海", "深圳","武汉","西安", "青岛", "昆明")
select.m <- dat.m[dat.m$city %in% select.city,c(1:4,13,18)]
select.m$city <- factor(as.character(select.m$city), levels=select.m$city[(0:34)*48+1])

aqi.select.m <- tapply(select.m$AQI, select.m$city, rep)
aqi.select.season <- sapply(aqi.select.m,
                            function(x) decompose(ts(x, start=c(2014,1), frequency = 12), type = "multi")$figure)


aqi.select.trend <- sapply(aqi.select.m,
                           function(x) {temp <- decompose(ts(x, start=c(2014,1), frequency = 12), type = "multi")
                               temp$x/temp$seasonal
                           })

library("tseries")

dimnames(aqi.select.season)[[1]] <- paste0(1:12, "月")
write.csv(aqi.select.season, "./results/aqi.select.season.csv", row.names=T)

library("kableExtra")
knitr::kable(aqi.select.season, row.names =T, align = "c",
             digits=2,
             caption="7个代表性城市月度季节指数",
             longtable = TRUE, booktabs = TRUE, linesep  = "", escape = F) %>%
    kable_styling(full_width = T) %>%
    column_spec(1:8, width = "1.4cm")%>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)",
                  )%>%
    footnote(general ="对每个城市月度平均AQI建立时间序列因素分解乘法模型计算得到",
             general_title = "数据来源：",
             threeparttable = T)



## ----tab-box, eval=T, results='markup'-----------------------------------
varlist <- read.csv("./data/var_list.csv")
dat.m <- read.csv("./data/monthly.csv")

dat.m$city <- factor(as.character(dat.m$city), levels=dat.m$city[(0:34)*48+1])

select.city <- c("北京","上海", "深圳","武汉","西安", "青岛", "昆明")
select.m <- dat.m[dat.m$city %in% select.city,c(1:4,13,18)]
select.m$city <- factor(as.character(select.m$city), levels=select.m$city[(0:34)*48+1])

aqi.select.m <- tapply(select.m$AQI, select.m$city, rep)
aqi.select.season <- sapply(aqi.select.m,
                            function(x) decompose(ts(x, start=c(2014,1), frequency = 12), type = "multi")$figure)

aqi.select.box <- sapply(aqi.select.m,
                           function(x) {temp <- decompose(ts(x, start=c(2014,1), frequency = 12), type = "multi")
                               box.res <- Box.test(temp$x/temp$seasonal, type = "Ljung")
                               c(48, box.res$stat, box.res$p.v)
                           })

temp.name <- dimnames(aqi.select.box)[[2]]
aqi.select.box <- matrix(as.character(round(aqi.select.box,3)),3)
dimnames(aqi.select.box)[[1]] <- c("样本数", "卡方统计量", "P值")
dimnames(aqi.select.box)[[2]] <- temp.name

write.csv(aqi.select.box, "./results/aqi.select.box.csv", row.names=T)

library("kableExtra")
knitr::kable(aqi.select.box, row.names =T, align = "c",
             digits=3,
             caption="7个代表性城市剔除季节性因素后月均AQI自相关检验",
             longtable = TRUE, booktabs = TRUE, linesep  = "", escape = F) %>%
    kable_styling(full_width = T) %>%
    column_spec(1:8, width = c("2.2cm", rep("1.3cm", 7)))%>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)",
                  )%>%
    footnote(general ="对7个代表性城市剔除季节性因素后月均AQI数据进行 Ljung-Box 检验结果整理得到",
             general_title = "数据来源：",
             threeparttable = T)



## ----fig-city-map, eval=T, fig.height=7, fig.width=6.5,out.width="1.0\\textwidth", fig.pos="h", fig.cap = "35个城市2017年平均AQI空间分布图", dev=c("cairo_pdf"), dev.args=list(family="Microsoft YaHei UI Light")----

library(ggplot2)
library(ggrepel)
library(maptools)
mydat <- readShapePoly('./data/mapdata/bou2_4p.shp')
china <- fortify(mydat)

city <- read.csv("./data/city_geo.csv", stringsAsFactors=FALSE)
city$lat <- paste(substr(city$lat,1,2), substr(city$lat,3,4))
city$long <- paste(substr(city$long, 1, nchar(city$long)-2),
                   substr(city$long, nchar(city$long)-1, nchar(city$long)))

city$lat = as.numeric(measurements::conv_unit(city$lat,
                                   from = 'deg_dec_min', to = 'dec_deg'))
city$long = as.numeric(measurements::conv_unit(city$long,
                                               from = 'deg_dec_min', to = 'dec_deg'))

aqi.y17 <- read.csv("./results/aqi.y17.csv")
city$aqi <- aqi.y17[,2]
city$grade <- aqi.y17[,3]
write.csv(city, "./results/city_co.csv")
levels(city$grade) <- c("yellow2", "orange", "green")

ggplot() +
    geom_polygon(data=china, aes(x = long, y = lat, group = group),
                 col = '#FD9FA4', fill = NA, lwd=0.15) +
    geom_point(data = city, aes(x = long, y = lat), cex=city$aqi/20,
               show.legend = F, alpha = 0.8, color = as.character(city$grade)) +
    geom_text_repel(data = city, aes(x = long, y = lat, label = city),
                    cex=3) +
    labs(x = '经度', y = '纬度') + 
    theme_bw()+
    coord_map()+
  theme(text = element_text(),
        ## plot.title = element_text(hjust = 0.5)
        )



## ----tab-global-moran, eval=T, results='markup'--------------------------

varlist <- read.csv("./data/var_list.csv")

dat.y <- read.csv("./data/yearly.csv")
city <- read.csv("./data/city_geo.csv", stringsAsFactors=FALSE)

dat.y$city <- factor(as.character(dat.y$city), levels=dat.y$city[(0:34)*4+1])
dat.y$lat <- rep(city$lat_d, each=4)
dat.y$long <- rep(city$long_d, each=4)

####### 全局 莫兰指数
library(ape)
source("./codes/geodistance.R")
df.cities <- city[,c(1, 5, 6)]
names(df.cities) <- c("name", "lat", "lon")
city.dist.km <- round(GeoDistanceInMetresMatrix(df.cities) / 1000)
city.dists.inv <- 1/city.dist.km
diag(city.dists.inv) <- 0


moran.g <- by(dat.y[,c(3, 30:31)], as.factor(dat.y$year), 
              function(x) Moran.I(x$AQI, city.dists.inv)
)

moran.g <- sapply(moran.g, function(x) c(x$observed, x$p.v))
dimnames(moran.g)[[1]] <- c("Moran's I", "P值")

write.csv(moran.g, "./results/moran.g.csv", row.names=T)

library("kableExtra")
knitr::kable(moran.g, row.names =T, align = "c",
             digits=3,
             caption="2014-2017年 35个城市莫兰指数",
             longtable = TRUE, booktabs = TRUE, linesep  = "", escape = F) %>%
    kable_styling(full_width = T) %>%
    column_spec(1:5, width = c("3cm", rep("2cm", 4)))%>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  ## stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)",
                  )%>%
    footnote(general ="通过全局莫兰指数定义计算得到",
             general_title = "数据来源：",
             threeparttable = T)



## ----fig-cor-log, eval=T, fig.height=6,fig.width=6.5,out.width="1.05\\textwidth", fig.pos="h", fig.cap = "2014-2017 年年度平均AQI空间相关图", dev=c("cairo_pdf"),dev.args=list(family="Microsoft YaHei UI Light")----

varlist <- read.csv("./data/var_list.csv")

dat.y <- read.csv("./data/yearly.csv")
city <- read.csv("./data/city_geo.csv", stringsAsFactors=FALSE)

dat.y$city <- factor(as.character(dat.y$city), levels=dat.y$city[(0:34)*4+1])
dat.y$lat <- rep(city$lat_d, each=4)
dat.y$long <- rep(city$long_d, each=4)

####### correlogram
library(ncf)

par(mfrow = c(2,2))
par(mar = c(4.5, 4.1, 2.8, 2.1))
cor.log <- by(dat.y[,c(2:3, 30:31)], as.factor(dat.y$year), 
              function(x){
                  plot(correlog(x$long, x$lat, z=x$AQI,
                                increment = 100, latlon = T, resamp=500, quiet=TRUE),
                       main = x$year[1], ylab="莫兰指数", xlab="距离(km)", ylim=c(-1.0,1.0))
                  abline(h=0, col="grey")
              })



## ----fig-moran-map, eval=T, fig.height=6.5, fig.width=6.5,out.width="1.05\\textwidth", fig.pos="h", fig.cap = "35个城市2017年平均AQI空间分布图", dev=c("cairo_pdf"), dev.args=list(family="Microsoft YaHei UI Light")----

varlist <- read.csv("./data/var_list.csv")

aqi.y17 <- read.csv("./results/aqi.y17.csv")
city <- read.csv("./data/city_geo.csv", stringsAsFactors=FALSE)
city$aqi <- aqi.y17$年均AQI

library(ncf)

aqi.lisa <- lisa(city$long_d, city$lat_d, city$aqi,
                 latlon = T, neigh=1000, resamp=500, quiet=TRUE)

library(maptools)
mydat <- readShapePoly('./data/mapdata/bou2_4p.shp')
par(mar = c(4.5, 4.1, 2.8, 2.1))
plot(mydat, border="gray20", fill=NA, lwd=0.2, xlim=c(73,135), ylim=c(20,40),
     axes=TRUE, asp = 1.2, xlab="经度", ylab="纬度")
plot(aqi.lisa, negh.mean=FALSE, add = T, lwd=1, cex=0.8)



## ----tab-local-moran, eval=T, results='markup'---------------------------

varlist <- read.csv("./data/var_list.csv")

dat.y <- read.csv("./data/yearly.csv")
city <- read.csv("./data/city_geo.csv", stringsAsFactors=FALSE)

dat.y$city <- factor(as.character(dat.y$city), levels=dat.y$city[(0:34)*4+1])
dat.y$lat <- rep(city$lat_d, each=4)
dat.y$long <- rep(city$long_d, each=4)

library(ncf)

aqi.lisa.y <- by(dat.y[,c(2:3, 30:31)], as.factor(dat.y$year), 
              function(x){
                  temp <- lisa(x$long, x$lat, z=x$AQI,
                               latlon = T, neigh=2000, resamp=500, quiet=TRUE)
                  cbind(temp$correlation, temp$p)
              })


aqi.lisa.y <- cbind(city$city, do.call(cbind.data.frame, aqi.lisa.y))
names(aqi.lisa.y) <- c("城市", "2014指数","P值","2015指数","P值","2016指数","P值", "2017指数","P值")

write.csv(aqi.lisa.y, "./results/aqi.lisa.y.csv", row.names=T)

library("kableExtra")
knitr::kable(aqi.lisa.y, row.names =F, align = "c", caption="2014-2017年 35个城市局部Moran's I指数",
             digits = 3,
             longtable = TRUE, booktabs = TRUE, linesep  = "", escape = F) %>%
    kable_styling(latex_options = c("striped", "scale_down", "repeat_header", "hold_position"),
                  stripe_index = rep(1:5, 4)+rep(c(0,10,20,30), each = 5),
                  repeat_header_text = "(续)")%>%
    footnote(general ="通过局部Moran's I指数定义计算得到",
             general_title = "数据来源：")


