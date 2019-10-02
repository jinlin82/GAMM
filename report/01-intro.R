## ----setup, echo=F-------------------------------------------------------

################# 第 1 章 R 程序代码  ####################


knitr::opts_knit$set(root.dir = getwd())
knitr::opts_chunk$set(echo = FALSE, results = 'hide')
knitr::opts_chunk$set(warning = FALSE, message=FALSE)


## ----prepare-------------------------------------------------------------
rm(list=ls())
options(digits=4)
options(scipen=100)
graphics.off()
Sys.setlocale("LC_ALL", "Chinese")

