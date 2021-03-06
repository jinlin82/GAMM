---
title: "gammSlice 用法"
author: "Jin"
date: "2019-03"
output:
  bookdown::html_document2:
    number_sections: true
    seq_numbering: true
    fig_caption: true
    highlight: haddock
    theme: null
    md_extensions: +east_asian_line_breaks
    keep_md: true
    toc: false
    pandoc_args: ["--filter", "pandoc-crossref", "-M", "eqnPrefix="]
  bookdown::word_document2:
    fig_caption: true
    reference_docx: ./style/word-styles-02.docx
    md_extensions: +east_asian_line_breaks
    pandoc_args: ["--filter", "pandoc-crossref"]
  bookdown::pdf_document2:
    keep_tex: yes
    toc: false
    latex_engine: xelatex
    md_extensions: +east_asian_line_breaks
    pandoc_args: ["--listing", "--filter", "pandoc-crossref"]
css: ./style/markdown.css
autoEqnLabels: true
eqnPrefixTemplate: ($$i$$)
linkReferences: true
bibliography: Bibfile.bib
csl: ./style/chinese-gb7714-2005-numeric.csl
link-citations: true
---

**TODO**






**摘要：**我们论证了在广义可加混合模型中R包：gammSlice用于贝叶斯拟合和推断的应用。
这类模型包括广义线性混合模型和广义可加模型作为特例。通过足够大的马尔可夫链蒙特卡洛样本
可以实现精确的贝叶斯推断。切片采样是MCMC过程中的关键部分。和现有的广义可加混合模型软件
相比，gammSlice以更长的计算时间为代价，改善了推断的精确度。

**关键词：**广义可加模型；广义线性混合模型；切片抽样；MCMC；惩罚样条；R

# 简介

我们论述了在R计算环境中用gammSlice包进行广义可加混合模型（GAMM）分析的使用。广义线性
混合模型（GLMM）和广义可加模型（GAM）是GAMM的特列。本文中，混合指的是模型包括混合效应
和随机效应。在贝叶斯框架下用切片抽样进行拟合和推断，其中切片抽样是一种特殊类型的
马尔可夫链蒙特卡洛（MCMC）抽样。MCMC用于GAMM分析中的优势就是它推断的精确性。通过
这种方式，感兴趣参数的实际后验密度函数的数值信息，比如等后尾95%的可信集，能够通过
绘制足够大的MCMC样本得以任意精确。用于GAMM分析的更加成熟的R包，比如Wood（2017），
使用拉普拉斯基于概率惩罚的拟似然估计（PQL）。然而，就像Breslow&Lin（1995）和本文
第四部分阐述的一样，PQL是不太准确的。Bates等（2015）用于GLMM更加精确的拉普拉斯估计
至今还未扩展到GAMM的场合。

GAMM通过允许连续预测变量扩展GLMM非参数函数对平均响应的影响。假定，在一个纵向数据集中，
$y_{ij}$ 是一个二元响应变量 $y$ 的第 $i$ 个对象的第 $j$ 个测量，对于任意的
$1\leq j\leq n_i$ ，$1\leq i \leq m$ 。对于连续预测变量 $x_1$ $x_2$ ，类似地定义
$x_{1ij}$ $x_2ij$ 。这个模型的GLMM

$$y_{ij}|U_i \overset{ind}\sim Bernoulli(logit^{-1}(U_i+\beta_1x_{1ij}+\beta_2x_{2ij})),\qquad
  U_i \overset{ind}\sim N(0,\sigma^2)$$ {#eq:model1}

其中 $U_i$ 是随机截距项。在[@eq:model1]中：$x_i \overset{ind}\sim D_i$ 
表明 $x_i$ 是依赖于 $D_i$ 分布的，$x \sim Bernoulli(p)$ 意味着
$x$ 服从均值为 $p$ 的bernoulli分布，且 $logit^{-1}(x)=\frac{e^x}{1+e^x}$。
同样的数据GAMM为

$$y_{ij}|U_i \overset{ind}\sim Bernoulli(logit^{-1}(U_i+f_1(x_{1ij})+f_2(x_{2ij}))),\quad \quad U_i \overset{ind}\sim N(0,\sigma^2)$$

其中 $f_1$, $f_2$ 假定为任意光滑的函数。这些模型在基于纵向跟踪主题的生物医学领域（如，
Fitzmaurice等 2008），由于学生在同一个班级存在多层次结构的教育研究（如，Goldstein 2010）和
基于所谓的面板数据的经济学研究中都比较有用。有很多方法可以估计 $f_1$ $f_2$。在gammaSlice
中，$f_1(x_1)+f_2(x_2)$ 使用基于惩罚样条的混合模型来建模：

$$f_1(x_1)+f_2(x_2)=\beta_0+\beta_{x1}x_1+\sum_{k=1}^{K_1}u_{1k}z_{1k}(x_1)+\beta_{x2}x_2+\sum_{k=1}^{K_2}u_{2k}z_{2k}(x_2)$$ 

其中 $u_{1k} \overset{ind}\sim N(0,\sigma_1^2),1\leq k \leq K_1$ 且 $u_{2k} \overset{ind}\sim N(0,\sigma_2^2),1\leq k \leq K_2$

下标 $z$ 是正交化的O‘Sullivan样条基函数（Wand和Ormerod 2008）。有关它们的进一步的信息
参见第二部分和3.2部分。

gammSlice包本质上是GAMM的MCMC实现，一般设计的贝叶斯广义线性混合模型中GLMM和GAM这两个
特例在Zhao等（2006）中有所描述。读者可以读那篇文章获得这个模型非常广泛的类的细节。

截止2018年初，在comprehensive R Archive Network（www.cran.r-project.org） 中有十几个支持GLMM分析的包。它们中的很多，比如MASS（Venables和Ripley 2002），
gamma4（Wood和Scheipl 2017）和mgcv（Wood 2017），使用拉普拉斯近似，但是有一些，
比如，glmmBUGS（Brown和Zhou 2018），MCMCglmm（Hadfield 2017），R2Bayesx（Umlauf等 2017）
和spikeSlabGAM（Scheipl 2011），使用MCMC的方法。glmmML（Brostrom和Holmberg 2018）
和lme4（Bates等 2015）支持通过正交基于精确最大似然的方法。据我们所知，GAMM扩展
只有gamma4，macv，R2BayesX和spikeSlabGAM支持。前两个用拉普拉斯近似，和GAMM分析
很相似。第三个和第四个支持通过MCMC进行GAMM拟合。Scheipl（2011）提供了spikeSlabGAM
的详细描述，虽然这篇文章主要关注广义可加模型而不是GAMM。spikeSlabGAM在函数估计上
不同于gammaSlice和mgcv，因为它们在样条系数上使用尖峰和平板类型的先验分布。R2BayesX包
支持和gammSlice所支持的相似的模型。截止2017年，我们用R2BayesX1.1版本测试了一些GAMM的
分析，但是由于模型不支持，很多都失败了。截止2017年11月，我们认为gammaSlice是唯一提供
基于MCMC进行GAMM分析，且使用像mgcv一样的简单惩罚样条方法推断的R包。

第二节包含gammaSlice包的概述。我们对gammSlice的论证开始于第三部分，其中有四个基于模拟
数据的说明性示例。第四部分我们阐述了gammaSlice在一些已有的GAMM软件中的精确优势。第五
部分包含一些gammaSlice的应用。第六部分简单讨论了gammSlice可能的扩展。附录给出了一些
在GAMM分析中使用gammSlice进行贝叶斯推断的抽样细节。

# gammaSlice的概述

gammaSlice包的核心是gSlc()这个函数。gSlc()的主要参数是formula，用于指定与数据参数中
的数据相对应的GAMM模型。

公式参数类似于广泛使用的GAM和GAMM包mgcv公式中的参数，并支持预测变量的可加函数的规范。
数值预测器可以线性地或以预测器的任意光滑函数的形式进入模型，尽管后者只有在以这种方式
使用的预测器有许多唯一值时才适用。平滑函数规范通过s()函数进行。比如，公式

```
y ~ x1+x2+s(x3)+s(x4)
```

对应于以下形式的有线性预测器的GAM或GAMM

$$\beta_1x_1+\beta_2x_2+f_3(x_3)+f_4(x_4)$$

在gSlc()中，我们使用和在mgcv包中的gam()、gamm()相同的构造函数s()，R中的命令为

```
help(s,package="mgcv")
```

给出了s()完整的描述和具体的细节，在本文第三部分给出了一些例子。fomula参数也支持通过
factor()构造函数进入模型的类别预测变量。比如，如果 $x_1$ , $x_2$ 是数值预测变量，而 $x_3$ 通过字符串“female”和“male”包含了性别信息，则

```
y ~ x1+s(x2)+factor(x3)
```

将为男性类别中的观测设置指示变量。女性作为基准类别，因为“female”在词典排序中
比“male”靠前。

通过random参数可以设置随机效应结构。family参数用于设置响应变量的分布族，目前只能
使用“binomial”和“poisson”分布。其他参数可以通过control参数来设置，同时可以使用
gSlc.control()这个函数。具体细节可以通过在R中键入以下命令获得：

```
library(gammSlice) 
help(gSlc)  
help(gSlc.control)
```

gammSlice包还为gSlc()生成的对象提供了两种方法函数。主要的方法函数是summary.gSlc()，
提供了MCMC结果的总结和模型参数的贝叶斯推断的总结。另一个方法函数是plot.gSlc()，
支持估计函数的显示。更多细节通过以下命令获得：

```
help(summary.gSlc)
help(plot.gSlc)
```

gammSlice中唯一的其他部分就是名为indonRespir和toenail的数据集。在第五部分，
这个数据集被用来通过gammSlice进行GAMM分析。

# 使用模拟数据的说明性示例

首先我们给出了一个包含模拟数据的例子。这可以传达gammSlice的本质，又没有
来自应用研究数据的复杂性。

## 简单的logistic混合模型

首先我们模拟的数据例子包含简单贝叶斯逻辑斯蒂混合模型

$$\begin{array}{l}
y_{ij}|x_{ij},\beta_0,\beta_x,U_i,\overset{ind}\sim Bernoulli(logit^{-1}(\beta_0+\beta_xx_{ij}+U_i)),\quad 1\leq i \leq m,1\leq j \leq n \\
x_{ij}\overset{ind}\sim Uniform(0,1),\quad U_i|\sigma^2 \overset{ind}\sim N(0,\sigma^2)\\
\beta_0,\beta_x \overset{ind}\sim N(0,\sigma_\beta^2),\quad \sigma \sim Half-Cauchy(A)
\end{array}$$ {#eq:model2}

其中 $\sigma\sim Half-Cauchy(A)$ 意味着 $\sigma$ 有密度函数
$p(\sigma)=\frac{2A}{\pi(\sigma^2+A^2)},\sigma>0,A>0$。
超参数 $\sigma_\beta^2$ 和 $A$ 可以由使用者指定。
它们的默认值在这部分的后面讨论。

为了阐述在gammSlice中拟合[@eq:model2]，我们按照如下方法产生了一些数据，
$m=100,n=2,\beta_0=0.5,\beta_x=1.7,\sigma^2=0.8$：

```
set.seed(39402) 
m<-100 
n<-2 
beta0True<-0.5 
betaxTrue<-1.7 
sigsqTrue<-0.8 
idnum<-rep(1:m,each=n) 
x<-runif(m*n) 
U<-rep(rnorm(m,0,sqrt(sigsqTrue)),each=n) 
mu<-1/(1+exp(-(beta0True+betaxTrue*x+U))) 
y<-rbinom((m*n),1,mu)
```

之后使用如下命令进行拟合：

```
fit1<-gSlc(y~x,random=list(idnum=~1),family="binomial")
```

为了获得切片抽样输出的结果，我们使用：

```
summary(fit1)
```

结果显示在图1中。第二列包含了MCMC样本的跟踪图，通常称其为链，是通过时间顺序绘制的值。
第三列绘制了链中的每个值与其之间值的关系，以便快速可视地评估序列相关性的数量。第四列
是每个链的样本自相关函数，以更加一般的方式评估每个滞后期链的相关性。第五列是每个参数
后验密度函数的核估计，第六列是基于相关链的数值总结。

<div class="figure">
<img src=".\results\fig1-logistic.png" alt="基于MCMC抽样的简单logistic回归模型的信息总结"  />
<p class="caption">(\#fig:fig1)基于MCMC抽样的简单logistic回归模型的信息总结</p>
</div>

图 \@ref(fig:fig1) 中2-4列显示了MCMC收敛的迹象，看起来相当好：跟踪图显示没有趋势，滞后一期的相关系数
很小，自相关函数的峰值合理地衰减。最后两列显示出估计的后验密度函数和一些基本的数值总结。

### 输入数据的前期处理

在gammSlice中gSlc()函数的默认版本在切片采样发生之前应用以下方式进行数据预处理：

$$\frac{(x_{ij}-min(x_{ij}))}{(max(x_{ij})-min(x_{ij}))}$$

相应的反向转换应用在MCMC的输出中。这个转换保证了在使用默认的超参数时结果是不变的。
这些默认值在接下来进行讨论。

### 默认设置及其修改

gSlc()函数有几个默认设置。我们简单的描述了最重要的几个以及如果需要的话，使用者
如何修改它们。

gSlc()中默认的超参数如下所示：

```
fixedEffPriorVar=sigma_beta^2=10^10
and sdPriorScale=A=10^5
```

比如，一个人想要通过之前的分布去设置

$$\beta_0,\beta_x \overset{ind}\sim N(0,10^{13}),\quad \quad \sigma \sim Half-Cauchy(10^3)$$

那么命令将会如下所示：

```
fit1MyPriors<-gSlc(y~x,random=list(idnum=~1),
family="binomial"),control=gSlc.control(fixeEffPriorVar=1e13,sdPriorScale=1e3)
```

在gSlc()中切片抽样样本大小有以下默认设置：

```
nBurn=number of burn-in iterations=5000,
nKept=number of kept iterations=5000,
nThin=thinning factor=5
```

指定大小为10000的burn-in，迭代次数等于8000，以及稀疏因子为10，gSlc()的命令应为：

```
fit1BigMCMC<-gSlc(y~x,random=list(idnum=~1),family="binomial",
control=gSlc.control(nBurn=10000,nkept=8000,nThin=10))
```

### 马尔可夫链蒙特卡洛样本的提取

summary和plot命令给gSlc()拟合一个快速且便利的检查。然而，使用者可能希望去获得
基于MCMC输出的其他总结信息。相关的值可以通过以下命令进行提取：

```
betaMCMC<-fit1$beta 
sigmaSquaredMCMC<-fit1$sigmasquared
```

### 其他总结信息的构建

MCMC输出可以用来关于拟合模型的总结性信息。比如，$x$ 的第三四分位数（$Q_3$）和 $x$ 的第一四分位数（$Q_1$）相比，以每个主题的随机截距为条件的优比为

$$OR_x=exp(\beta_x(Q_3-Q_1))$$

以下命令计算了近似后验均值优比和　$OR_x$ 95%的置信区间

```
Q1<-as.numeric(quantile(x,0.25)) 
Q3<-as.numeric(quantile(x,0.75)) 
betaxMCMC<-fit1beta[,1] 
ORxMCMC<-exp(betaxMCMC*(Q3-Q1)) 
postMeanORx<-mean(ORxMCMC) 
credSetORx<-as,numeric(quantile(ORxMCMC,c(0.025,0.975)))
```

对应于图1的数据和MCMC输出，我们得到 $OR_x$ 的后验均值为1.53，相应的95%的置信
区间为 $(0.720,2.89)$

## 泊松非参数回归

接下来，我们考虑泊松非参数回归模型

$$y_i \overset{ind}\sim Poisson(exp(f(x_i))),\quad 1 \leq i \leq n$$ {#eq:model3}

光滑函数 $f$ 使用惩罚样条进行建模：

$$f(x)=\beta_0+\beta_xx+\sum_{k=1}^K u_kz_k(x),\quad u_k \overset{ind}\sim N(0,\sigma^2)$$ {#eq:model4}

$\{Z_K(\cdot):1 \leq k \leq K\}$ 是在第四部分描述的Wand和Ormerod（2008）的 
正交化的 O'Sullivan样条函数。对于惩罚样条，K的选择相对较小。默认值是
$min(25,n_U/4)$ 其中 $n_U$ 是 $x_i$ 值的数量。


适合gammaSlice拟合和默认的超参数，完全贝叶斯模型是

$$\begin{array}{l}
y_i|x_i,\beta_0,\beta_x,u_1,\cdots,u_k \overset{ind}\sim Poisson(exp(\beta_0+\beta_xx_i+\sum_{k=1}^Ku_kz_k(x_i))),\\
x_i \overset{ind}\sim Uniform(0,1),\quad u_k|\sigma^2 \overset{ind}\sim N(0,\sigma^2),\\
\beta_0,\beta_x \overset{ind}\sim N(0,10^{10}),\quad \sigma \sim Half-Cauchy(10^5)
\end{array}$$ {#eq:model5}

为了解释gammaSlice的拟合，我们模拟数据 $n=400$，$x_i$服从 $(0,1)$ 上的均匀
分布，且

$$f(x)=cos(4\pi x)+2x-1$$

如下：

```
set.seed(53902)
n<-400 
x<-runif(n) 
y<-rpois(n,exp(cos(4*pi*x)+2*x-1)) 
```

为了拟合[@eq:model5]，我们使用gSlc()如下：

```
fit2<-gSlc(y~s(x),family="poisson")
```

这里使用的函数s()是内部函数，将 $f$ 建模为基于惩罚样条的混合模型，
就像[@eq:model4]描述的一样。

在[@eq:model5]中没有明确可识别和可解释的参数。对于这个模型的收敛性评估，
我们建议使用平滑函数成分的有效自由度（edf）进行度量。在当前的模型中，
只有一个光滑函数成分，有效自由度被定义为：

$$edf=tr((C^{T}WC+\sigma^{-2}D)^{-1}C^{T}WC)$$ {#eq:mpdel6}

其中

$$C=[1\quad x_i \quad z_1(x_i) \cdots z_k(x_i)]_{1 \leq i \leq n},\quad D=diag(0,0,1,\cdots,1),\quad W=diag(exp(f))$$

且 $f=C[\beta_0,\beta_x,u_1,\cdots,u_k]^{T}$，$diag(v)$ 表示对角有 $v$
个元素的对角矩阵。[eq:model6]的合理性在Hastie和Tibshirani（1990）中给出。

通过有效自由度测量的MCMC收敛可以通过如下命令进行检查：

```
summary(fit2)
```

将以上模拟的数据绘制在图 \@ref(fig:fig2) 中。有效自由度的收敛似乎是合理的，因为在跟踪图中全局趋势
是平坦的，滞后一期的相关性也很小，自相关峰值远低于1。有效自由度的后验分布似乎以
10.5为中心，这就显示出高度的非线性性——与使用我们产生的数据的均值函数一致。$f$
估计的可视化在图 \@ref(fig:fig3) 中显示，通过以下命令获得：

```
plot(fit2) 
plot(fit2,responseScale=TRUE) 
points(x,y)
```

在plot()中设置responseScale=TRUE将 $f$ 的估计转化成数据的比例——在这种情况下
为 $exp(f)$，由 $f$ 组成的反向连接函数。

使用额外的ad hoc R命令增加 $f$ 曲线和图例，将其设置为虚线。从gSlc()可以看出
在这个例子中f的估计相当好。

<div class="figure">
<img src=".\results\fig2-poisson.png" alt="有效自由度的MCMC输出信息汇总"  />
<p class="caption">(\#fig:fig2)有效自由度的MCMC输出信息汇总</p>
</div>


<div class="figure">
<img src=".\results\fig2-poisson1.png" alt="贝叶斯后验密度均值和95%的置信区间"  />
<p class="caption">(\#fig:fig3)贝叶斯后验密度均值和95%的置信区间</p>
</div>

## 半参数logistic回归

我们第三个例子是简单的半参数logistic模型：

$$y_i \overset{ind}\sim Bernoulli(logit^{-1}(\beta_{x|x_{1i}}+f(x_{2i}))),\quad 1 \leq i \leq n$$ {#eq:model7}

贝叶斯惩罚样条公式是：

$$\begin{array}{l}
y_i|x_{1i},x_{2i},\beta_0,\beta_{x1},\beta_{x2,u_1,\cdots,u_K} \overset{ind}\sim Bernoulli(logit^{-1}(\beta_0+\beta_{x1}x_{1i}+\beta_{x2}x_{2i}+\sum_{k=1}^Ku_kz_k(x_{2i}))) \\
x_{1i}\overset{ind}\sim Bernoulli(\frac{1}{2}),\quad x_{2i}\overset{ind}\sim Uniform(0,1),\\
u_k|\sigma^2 \overset{ind}\sim N(0,\sigma^2),\quad \beta_0,\beta_{x1},\beta_{x2} \overset{ind}\sim N(0,10^{10}),\quad \sigma \sim Half-Cauchy(10^5)
\end{array}$$ {#eq:model8}

考虑数据集 $n=500$ ， $ f(x)=sin(2\pi x)$ ：

```
set.seed(981127)
n<-500
betax1True<-0.5
x1<-sample(c(0,1),n,replace=TRUE)
x2<-runif(n)
mu<-1/(1+exp(-(betax1True*x1+sin(2*pi*x2))))
y<-rbinom(n,1,mu)
```

拟合模型[@eq:model8]并汇总信息，在gammSlice中使用：

```
fit3<-gSlc(y~x1+s(x2),family="binomial)
summary(fit3)
```

图 \@ref(fig:fig4) 显示了summary(fit3)的结果。MCMC收敛对于关键参数 $\beta_{x1}$ 是很好的，对于 $f(x_2)$ 组成成分的有效自由度也是合理的。

<div class="figure">
<img src=".\results\fig3-semi_logistic.png" alt="基于切片MCMC抽样的信息总结"  />
<p class="caption">(\#fig:fig4)基于切片MCMC抽样的信息总结</p>
</div>

最后，$f$ 的估计的可视化可以使用：

```
plot(fit3)
```

这就产生了图 \@ref(fig:fig5)，捕获了真正 $f$ 函数的正弦性质。同时注意到gammSlice是去绘制感兴趣
的预测变量的拟合，所有其他的预测变量设置成它们的样本均值。图5显示我们绘制的是针对
$x_2$ 的 $\beta_x|\bar{x}_1+f(x_2)$ 的贝叶斯估计，其中 $\bar{x}_1$ 是 $x_{1i}$ 的均值。

<div class="figure">
<img src=".\results\fig3-semi_logistic1.png" alt="f的贝叶斯后验均值估计及95%置信区间"  />
<p class="caption">(\#fig:fig5)f的贝叶斯后验均值估计及95%置信区间</p>
</div>

## 泊松可加混合模型

拟合数据的最后一个例子是泊松混合可加模型

$$y_{ij}\overset{ind}\sim Poisson(exp(U_i+f_1(x_{1ij})+f_2(x_{2ij}))) \quad U_i \overset{ind}\sim N(0,\sigma^2)$$ {#eq:model9}

相应的贝叶斯惩罚样条如下：

$$\begin{array}{l}
y_{ij}|x_{1ij},x_{2ij},\beta_0,\beta_{x1},u_11,\cdots,u_{1K_1},u_{21},\cdots,u_{2K_2}\overset{ind}\sim \\
Poisson(exp(\beta_o+U_i+\beta_{x1}x_{1i}+\sum_{k=1}^{k_1}u_{1k}z_{1k}(x_{1ij})+\beta_{x2}x_{2ij}+\sum_{k=1}^{K_2}u_{2k}z_{2k}(x_{2ij}))) \\
x_{1ij} \overset{ind}\sim Uniform(0,1),\quad x_{2ij}\overset{ind}\sim Uniform(0,1),\\
U_i\overset{ind}\sim N(0,\sigma^2),\quad u_{1k}|\sigma_{x_1}^2\overset{ind}\sim N(0,\sigma_{x_1}^2),\quad u_{2k}|\sigma_{x2}^2\overset{ind}\sim N(0,\sigma_{x_2}^2),\\
\beta_0,beta_{x1},\beta_{x2}\overset{ind}\sim N(0,10^{10}),\quad \sigma\sim Half-Cauchy(10^5),\\
\sigma_{x1}\sim Half-Cauchy(10^5),\quad \sigma_{x_2}\sim Half-Cauchy(10^5)
\end{array}$$  {#eq:model10}

其中

$$\{z_{1k}(\cdot):1\leq k \leq K_1\},\quad \{z_{2k}(\cdot):1 \leq k \leq K_2\}$$

分别是 $x_{1i}$ 和 $x_{2i}$ 的基函数。

考虑又[@eq:model9]产生的数据 $m=100,n=10$，$x_{1i}$，$x_{2i}$ 独立同分布于
均匀分布(0,1)，$\sigma^2=1$ 且均值函数为

$$f_1(x)=cos(4\pi x)+2x,\quad f_2(x)=sin(2\pi x^2)$$

```
set.seed(2966703)
m<-100
n<-10
x1<-runif(m*n)
x2<-runif(m*n)
idnum<-rep(1:m,each=n)
sigsqTrue<-1
U<-rep(rnorm(m,0,sqrt(sigsqTrue)),each=n)
mu<-exp(U+cos(4*pi*x1)+2*x1+sin(2*pi*x2^2))
y<-rpois(m*n,mu)
```

为了拟合模型，我们使用：

```
fit4<-gSlc(y~s(x1)+s(x2),random=list(idnum=~1),family="Poisson")
```

如之前所述，总结信息和函数拟合图由以下命令得到：

```
summary(fit4)
plot(fit4)
```

图\@ref(fig:fig6)显示了summary(fit4)的结果。在默认MCMC样本量的情况下显示出很好的收敛性。
$\sigma^2$ 95%的置信区间是 $(0.535,0.975)$ ，接近我们产生的数据集中包括的1值。

<div class="figure">
<img src=".\results\fig4-poisson_mix.png" alt="基于切片MCMC抽样的模型拟合信息总结"  />
<p class="caption">(\#fig:fig6)基于切片MCMC抽样的模型拟合信息总结</p>
</div>

高的有效自由度反映出拟合函数部分的非线性性。非线性效应在图 \@ref(fig:fig7)中进一步加强。

<div class="figure">
<img src=".\results\fig4-poisson_mix1.png" alt="f1和f2的贝叶斯后验均值估计及95%的置信区间"  />
<p class="caption">(\#fig:fig7)f1和f2的贝叶斯后验均值估计及95%的置信区间</p>
</div>

# 精准预测

用小规模的模拟研究去评估gammSlice的精确性，并了解它与基于拉普拉斯近似快速方法的比较。

根据GLMM我们产生了100个样本

$$y_{ij}\overset{ind}\sim Bernoulli(logit^{-1}(\beta_0+U_i)),\quad 1\leq i \leq 250,1\leq j \leq2,U_i\overset{ind}\sim N(0,\sigma^2)$$ {#eq:model11}

真实值

$$\beta_0=2,\quad \sigma=1$$

对于每一个样本，我们通过使用MASS包（Venables和Ripley 2002）中的glmmPQL()和intervals()
函数、lme4包（Bates等 2015）中的glmer()和confint()函数，获得了 $\beta_0$ 和 $\sigma$ 的点估计和95%的置信区间。就像它的名称所显示的，glmmPQL()使用惩罚拟似然（PQL）（如，
Breslow和Clayton 1993）近似推断。然而，glmer()使用拉普拉斯近似的不同版本，就像在Bates等
（2015）的作品中描述的一样。每个样本基于贝叶斯的MCMC估计和95%的置信带也可以通过使用gSlc()
获得，并可以进行比较。由于贝叶斯和频率推断的混合，这样的比较可能会从哲学的角度受到批评。
然而，由于贝叶斯推断是基于先验，至少在非正式意义下结果是可比的。

图8展示了由模拟数据可视化的信息总结。PQL的结果显示出 $\beta_0$ 和 $\sigma$ 的结果存在
显著的正偏差。尤其是 $\sigma$，所公布的95%的近似置信区间似乎是非常糟糕的。由gammSlice
所产生的贝叶斯估计和95%的置信带显示出更好的收敛，尽管贝叶斯估计有过高估计目标的趋势。

为了更好地解决与名义公布的95%的收敛相匹配的经验收敛问题，我们对图8的研究进行了900多次
重复，并跟踪哪些区间包括1000次重复中的真实值。相应的比例也展示在图8中。

经验收敛值再一次强调了这样一个事实，由glmmPQL()产生的近似置信区间有非常糟糕的收敛。
另一方面，由glmer()和gSlc()产生的置信带的经验收敛似乎相当接近95%的名义水平。同时
注意到在PQL结果中存在的显著的偏差由glmer()中更好的拉普拉斯近似所纠正。

将对存在平滑函数的GAMM进行扩展，进行类似的精度比较。然而，目前的mgcv和gamma4并不能
完全支持置信区间。尽管它们有缺点，这些结果显示，如果精确度是最重要的，那么对于基于
推断的拟似然来说，gammSlice是个更好的选择。

# 医学研究数据的应用

现在我们论证gammSlice对于来自医学研究真实数据集的分析。每个数据集都在之前的GLMM
文献中出现过，这就可以和之前文献结果进行比较。

## 脚趾甲数据

脚趾甲数据来自于De Backer等人（1998）对纵向数据的研究。这个研究关注甲剥离的治疗，
即指甲与正常附着于甲床的分离。响应测量是

$$onycholysis_{ij}=\begin{cases} 0, & \text{第j次访问病人i没有甲剥离或轻度甲剥离}\\ 1, &\text{第j次访问病人i中度到重度甲剥离} \end{cases}$$

其中 $1\leq i \leq 294$，$1\leq j\leq n_i$，$n_i\in\{1,\cdots,7\}$ 是第 $i$ 个病人访问的数量。

相应的预测变量如下：

$$terb_i=\begin{cases} 0,&\text{第i个病人用伊曲康唑治疗}\\ 1,&\text{第i个病人用特比萘芬治疗} \end{cases}$$

$months_{ij}$ 表示第 $i$ 个病人在第 $j$ 次访问时从治疗开始的月份的数量。图9显示了甲剥离与月份的关系，根据治疗用灰度编码。

我们考虑logistic混合模型：

$$\begin{array}{l}
onycholysis_{ij}|\beta_{terb},\beta_{months},\beta_{terbXmonths},U_i\overset{ind}\sim Bernoulli(logit^{-1}(\beta_0+U_i+\beta_{terb}(terb_i)+ \\
\beta_{months}(months_{ij})+\beta_{terbXmonths}(terb_i\times months_{ij}))),\quad U_i \overset{ind}\sim N(0,\sigma^2)
\end{array}$$ {#eq:model12}

[@eq:model12]基于切片的抽样拟合和总结可以由以下代码实现：

```
data("toenail")
terbXmonths<-toenail[,"terb"]*toenail[,"months"]
toenailPlus<-cbind(toenail,terbXmonths)
fitTN<-gSlc(onycholysis~terb+months+terbXmonths,random=list(idum=~1),data=toenailPlus,family="binomial")
summary(fitTN)
```
结果绘制在图\@ref(fig:fig10)中。这个结果显示出所有关键变量非常好的MCMC收敛。由月计算的时间效应
显著，$\beta_{months}$ 的95%的置信区间是 $(-0.489,-0.313)$。这就显示出在研究过程中显著
降低了甲剥离的患病率。同样，$\beta_{terbXmonths}$ 的置信区间是 $(-0.278,-0.00379)$。
显示出特比萘芬在减少甲剥离患病率上有轻微的影响。$\sigma^2$ 95%的置信带 $(12.4,26)$ 
构成了患者相关的证据。

### 说明性优比总结

优比，以每个对象的随机截距为条件，$terb=1$，则$months=$月份的中位数，值为 $Q_2$ ；$terb=0$，
则$months=$ 0，优比 $OR_{terb}=exp(\beta_{terb} Q_2)$。通过类比3.1.4中的计算，$OR_{terb}$
的后验均值是0.767,95%的置信集是 $(1.87\times10^{-7},0.670)$ 。这个结果和图\@ref(fig:fig10)、11中统计
显著交互效应是一致的。

<div class="figure">
<img src=".\results\fig5-toenail.png" alt="基于切片MCMC抽样拟合logistic混合模型的信息汇总"  />
<p class="caption">(\#fig:fig10)基于切片MCMC抽样拟合logistic混合模型的信息汇总</p>
</div>


### 惩罚拟似然和精确最大似然的比较

图11提供了在gammSlice中使用惩罚拟似然和精确似然的贝叶斯估计和95%的置信集的比较。
惩罚拟似然通过使用MASS包中的galmmPQL()函数获得。精确似然通过使用lme4包中的glmer()
函数获得，其使用自适应Gauss-Hermite积分实现精确性。就像在第4部分提到的，频率和
贝叶斯推断的比较的潜在的哲学批评这里也同样存在。先验的扩散意味着至少非正式的比较是合理的。

从图11中可以看到，gammSlice和AGHQ的结果是非常接近的，进一步提供了切片抽样方法
精确性的证据。PQL对于一些参数是不精确的，如 $\beta_{months}$ 和 $\sigma$ 。

## 印度尼西亚儿童健康研究数据

印度尼西亚儿童健康研究数据来自275个印度尼西亚的儿童，最初由Sommer（1982）使用，
之后由Diggle等（2002）使用。响应变量是呼吸道感染（0表示没有感染，1表示感染），
潜在的预测变量是年龄，维生素A缺少的指标，性别，身高，发育不良的指标和每个孩子
门诊就诊的次数。这里，我们使用门诊就诊次数的指标，需要5个这样的指标。让 $x_{ij}$ 
表示 $9\times 1$ 的向量，包含这些指标值，$1\leq i \leq 275$ 表示孩子， $1 \leq j \leq n_i$
表示对每个孩子重复的测量。一个可加混合模型如下：

$$logit(P(respiratory infection_{ij}=1))=U_i+\beta^{T}x_{ij}+f(age_{ij}),\quad U_i \overset{ind}\sim N(0,\sigma^2)$$  {#eq:model13}
 
模型通过以下命令进行拟合：

```
data("indonRespir")
fitIR<-gSlc(respirInfec~s(age)+vitAdefic+female+height+stunted+visit2+visit3+visit4
+visit5+visit6,random=list(idnum=~1),family="binomial",data=indonRespir)
```

为了看一些汇总信息，我们键入：

```
summary(fitIR)
```

我们得到了图\@ref(fig:fig12)的汇总信息。和Zhao等（2006）的图1是相似的，表明富含维生素A的饮食和
规则的诊所问诊往往会降低呼吸道感染的风险。

<div class="figure">
<img src=".\results\fig6-disease.png" alt="基于切片MCMC抽样的拟合logistic可加混合模型的信息汇总"  />
<p class="caption">(\#fig:fig12)基于切片MCMC抽样的拟合logistic可加混合模型的信息汇总</p>
</div>


图\@ref(fig:fig13)显示了作为年龄的函数呼吸道感染的概率，由以下代码获得：

```
plot(fitIR,responseScale=TRUE)
```

图\@ref(fig:fig13)显示了有趣的非单调年龄效应，这也被Lin和Carrill（2001）和Zhao（2006）等发现过。
一个更简单的模型，就像没有惩罚样条的广义线性模型，也不会发现非线性的年龄效应。
因此，广义可加混合模型提供了一个更加全面更深刻的分析。

<div class="figure">
<img src=".\results\fig6-disease.png" alt="f(age)的贝叶斯后验均值估计及95%的置信区间"  />
<p class="caption">(\#fig:fig13)f(age)的贝叶斯后验均值估计及95%的置信区间</p>
</div>

# 扩展

与任何这种类型的包一样，可以考虑gammaSlice的扩展。其中一个更明显的扩展就是响应变量
可加分布族的提供。比如伽玛分布，负二项分布，广义极值分布。另外一个扩展就是容许广义
可加模型和广义可加混合模型在模型中有连续预测变量的双变量函数。这些有时被称为地理
可加模型，在Zhao等（2006）的广义线性混合模型的框架中有所描述。最后一个扩展就是允许
向量随机效应结构——适用于有随机截距和斜率的模型。基于gammaSlice的结构，所有的这些拓展
都相对简单。时间和资源限制了对当前gammaSlice版本的通常限制。

# 参考文献
[//]: # (\bibliography{Bibfile})
