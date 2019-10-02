## ----setup, echo=F-------------------------------------------------------

################# 第 3 章 R 程序代码  ####################


knitr::opts_knit$set(root.dir = getwd())
knitr::opts_chunk$set(echo = FALSE, results = 'hide')
knitr::opts_chunk$set(warning = FALSE, message=FALSE)


## ----prepare-------------------------------------------------------------
rm(list=ls())
options(digits=4)
options(scipen=100)
graphics.off()
Sys.setlocale("LC_ALL", "Chinese")


## ----fig-code, echo=FALSE, fig.cap='Gibbs抽样方案算法', dev='png', fig.pos="H",results='markup'----
knitr::include_graphics("gibbs.png")

