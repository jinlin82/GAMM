
n<-1000
y<-rpois(n,lambda=1)
y
x1<-rnorm(n,mean=0,sd=1)
x1
x2<-seq(0,1,0.001)
x2
f2<-function(x2){
  x2+x2^2+x2^3
}
plot(f2(x2),type="l")
b<-rnorm(n,mean=0,sd=1)
epsilon<-rnorm(n,0,1)

beta0<-rnorm(n,0,1)
beta1<-rnorm(n,0,1)

library(mgcv)


###### gammslice
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
y<-rbinom(m*n,1,mu)
library(gammSlice)
fit1<-gSlc(y~x,random=list(idnum=~1),family="binomial")
summary(fit1)


###### poison可加混合模型

set.seed(2966703)
m<-100
n<-10
x1<-runif(m*n)
x1
x2<-runif(m*n)
x2
idnum<-rep(1:m,each=n)   ###抽取1：100，重复10次
idnum
sigsqTrue<-1
U<-rep(rnorm(m,0,sqrt(sigsqTrue)),each=n)
U
mu<-exp(U+cos(4*pi*x1)+2*x1*sin(2*pi*x2^2))
mu
y<-rpois(m*n,mu)
y
fit4<-gSlc(y~s(x1)+s(x2),random=list(idnum=~1),family="poisson")
summary(fit4)
fit4
plot(fit4)



############################## 数值模拟

### 拟合yt=beta0+beta1*I(groupt=group2)+et
set.seed(123)
ngroup<-2   ###有两个组
nrep<-10   ###每个组有10个观察值
b0<-5   ###beta0的值设定为5
b1<--2    ###beta1的值设定为-2
sd<-2    ###et的方差设定为4，标准差为2

group<-rep(c("group1","group2"),each=nrep)
group   ###group1、group2重复10次

eps<-rnorm(ngroup*nrep,0,sd)   ###产生20个随机扰动项
eps

growth<-b0+b1*(group=="group2")+eps   ###示性变量取1时yt的取值

growthfit<-lm(growth~group)
summary(growthfit)

###函数封装
twogroup_fun<-function(nrep=10,b0=5,b1=-2,sigma=2){
  ngroup<-2
  group<-rep(c("group1","group2"),each=nrep)
  eps<-rnorm(ngroup*nrep,0,sigma)
  growth<-b0+b1*(group=="group2")+eps
  growthfit<-lm(growth~group)
  growthfit
}
set.seed(123)
twogroup_fun()

twogroup_fun(sigma=1)    ###改变方差

library(purrr)
sims<-rerun(1000,twogroup_fun())    ###运行1000次，即拟合1000次模型，看拟合模型的参数与给定参数的差异
head(sims)

library(broom)
tidy(growthfit)   ###返回系数和检验统计量的信息

summary(growthfit)$sigma   ###提取残差标准差，即epsilon的标准差

suppressMessages(library(dplyr))
library(ggplot2)

###绘制beta1的密度图
sims %>%
  map_df(tidy) %>%
  filter(term=="groupgroup2") %>%
  ggplot(aes(estimate))+geom_density(fill="blue",alpha=0.5)+geom_vline(xintercept = -2)
###绘制sigma的密度图   
sims %>%
  map_dbl(~summary(.x)$sigma) %>%
  data.frame(sigma= .) %>%
  ggplot(aes(sigma))+geom_density(fill="blue",alpha=0.5)+geom_vline(xintercept = 2)

sims %>%
  map_dbl(~summary(.x)$sigma) %>%
  {.<2} %>%
  mean()

sims %>%
  map_df(tidy) %>%
  filter(term=="groupgroup2") %>%
  pull(p.value) %>%
  {.<0.05} %>%
  mean()

########################## 线性混合模型:yt=μ+(bs)t+et #######################
###yt是第t次的观测值，μ是总体均值，bs是随机效应，服从正态分布,et是水平观测的随机效应
### https://aosmith.rbind.io/2018/04/23/simulate-simulate-part-2/

set.seed(16)

###给定参数真值
nstand<-5
nplot<-4
mu<-10
sds<-2
sd<-1

stand<-rep(LETTERS[1:nstand],each=nplot)  ###stand表示省份
plot<-letters[1:(nstand*nplot)]   ###plot表示每个省份的取值
standeff<-rnorm(nstand,0,sds)    ###bs表示每个省份的随机效应
standeff<-rep(standeff,each=nplot)   ##因为每个省份有4个取值，所以每个省份都需要有四个相同的随机效应
ploteff<-rnorm(nstand*nplot,0,sd)   ###每个省份的每个值对响应变量都有随机效应，即随机误差项

dat<-data.frame(stand,standeff,plot,ploteff)  ###建立模拟的数据集

dat$resp<-with(dat,mu+standeff+ploteff)   ###给dat增加一列y，y是根据设定的公式形式计算的
dat

###模型拟合
library(lme4)
fit1<-lmer(resp~1+(1|stand),data=dat)   ###拟合线性混合效应模型（限制极大似然或极大似然）
fit1     ###1是固定效应，(1|stand)表示随机效应

###封装成函数
twolevel_fun<-function(nstand=5,nplot=4,mu=10,sigma_s=2,sigma=1){
  standeff<-rep(rnorm(nstand,0,sigma_s),each=nplot)
  stand<-rep(LETTERS[1:nstand],each=nplot)
  ploteff<-rnorm(nstand*nplot,0,sigma)
  resp<-mu+standeff+ploteff
  dat<-data.frame(stand,resp)    ###怎么只写stand和resp
  lmer(resp~1+(1|stand),data=dat)
}
set.seed(16)
twolevel_fun()

sims<-replicate(100,twolevel_fun())   ###重复模拟100次
sims[[100]]
library(broom)
tidy(fit1)    ###提取感兴趣的值
tidy(fit1,effects="fixed")   ###提取固定效应
tidy(fit1,effects="ran_pars",scales="vcov")    ###提取随机效应和误差项

######拟合混合模型yt=beta0+beta1*(Elevations)t+beta2*slopet+(bs)t+et
nstand<-5
nplot<-4
b0<--1
b1<-0.005
b2<-0.1
sds<-2
sd<-1
set.seed(16)
stand<-rep(LETTERS[1:nstand],each=nplot)
stand
standeff<-rep(rnorm(nstand,0,sds),each=nplot)
standeff   ###在正态分布中抽取bi
ploteff<-rnorm(nstand*nplot,0,sd)
ploteff   ###在正态分布中抽取epsilon
elevation<-rep(runif(nstand,1000,1500),each=nplot)  ###固定效应设计矩阵
elevation   ###相当于x1
slope<-runif(nstand*nplot,2,75)   ###固定效应设计矩阵
slope   ###相当于x2
resp2<-b0+b1*elevation+b2*slope+standeff+ploteff   ###根据方程产生y
lmer(resp2~elevation+slope+(1|stand))

######拟合混合模型yt=b0+b1*(elevations)t+b2*slopet+(bs)t*slopet+et

nstand<-5
nplot<-4
b0<--1
b1<-0.005
b2<-0.1
sds<-2
sd<-1
set.seed(16)
stand<-rep(LETTERS[1:nstand],each=nplot)
standeff<-rep(rnorm(nstand,0,sds),each=nplot)
ploteff<-rnorm(nstand*nplot,0,sd)
elevation<-rep(runif(nstand,1000,1500),each=nplot)  ###固定效应设计矩阵
slope<-runif(nstand*nplot,2,75) 
resp2<-b0+b1*elevation+b2*slope+standeff*slope+ploteff   ###根据方程产生y
lmer(resp2~elevation+slope+(slope|stand))

######拟合混合模型yt=b0+b1*(elevations)t+b2*slopet+(bs)t*slopet+(bs)t+et
nstand<-5
nplot<-6
b0<--1
b1<-0.005
b2<-0.1
sds1<-2
sds2 <- 1.5
sd<-1
set.seed(16)
stand<-rep(LETTERS[1:nstand],each=nplot)
standeff1<-rep(rnorm(nstand,0,sds1),each=nplot)
standeff2<-rep(rnorm(nstand,0,sds2),each=nplot)
ploteff<-rnorm(nstand*nplot,0,sd)
elevation<-rep(runif(nstand,1000,1500),each=nplot)  ###固定效应设计矩阵
slope<-runif(nstand*nplot,2,75) 
resp3<-b0+b1*elevation+b2*slope+standeff1+standeff2*slope+ploteff   ###根据方程产生y

cbind(stand, resp3, elevation, slope)

fit3 <- lmer(resp3~elevation+slope+(slope|stand)+(1|stand))
fit4 <- lmer(resp3~elevation+slope+(1+slope|stand))
fit5 <- lmer(resp3~elevation+slope+(0+slope|stand))

t(as.matrix(fit3@pp$Zt))
t(as.matrix(fit4@pp$Zt))
t(as.matrix(fit5@pp$Zt))


### 利用矩阵模拟模型yt=b0+b1*(elevations)t+b2*slopet+(bs)t*slopet+(bs)t+et
library(Matrix)
nstand<-5
nplot<-6
b0<--1
b1<-0.005
b2<-0.1
sds1<-2
sds2 <- 1.5
sd<-1
set.seed(16)
stand<-rep(LETTERS[1:nstand],each=nplot)
standeff1<-rep(rnorm(nstand,0,sds1),each=nplot)
standeff2<-rep(rnorm(nstand,0,sds2),each=nplot)
ploteff<-rnorm(nstand*nplot,0,sd)
elevation<-rep(runif(nstand,1000,1500),each=nplot)  ###固定效应设计矩阵
slope<-runif(nstand*nplot,2,75)

X <- cbind(rep(1, nstand*nplot), elevation, slope)
beta <- c(b0, b1, b2)
Z <- as.matrix(bdiag(by(X[,c(1,3)], stand, as.matrix)))
u <- as.vector(unlist(by(cbind(standeff1, standeff2), stand, function(x) x[1,])))
e <- ploteff

y <- X%*%beta+Z%*%u+e




###### yt~poisson(λt)（GLMM） 
###yt是第t次观测的数据，λt是第t次观测不可观测的均值，log(λt)=β0+β1xt+β2xt^2
###定义参数真值
b0<-0.5
b1<-0.5
b2<-5
###抽取自变量x~U(0,1)
set.seed(16)
x<-runif(100,0,1)
head(x)
###根据公式代入设定的值计算λ
lambda<-exp(b0+b1*x+b2*x^2)
head(lambda)
###根据计算的poison分布的均值抽取poisson分布随机数
y<-rpois(100,lambda = lambda)
head(y)
###绘图看x和log(y)、y之间的关系
plot(x,log(y))
plot(x,y)

###### GAMM
###yt~poisson(λt)，g(μt)=b0+b1*x1+b2*x2+f(x3)+(bs)t*x1，连接函数log
set.seed(123)
b0<-0.5
b1<-0.5
b2<-2   ###设定固定效应参数真值
x1<-runif(100,0,1)
x2<-runif(100,0,1)
x3<-runif(100,0,1)   ###抽取x
f<-function(x3){2*sin(pi)*x^2}   ###定义非线性函数形式
sigma_s<-2
bs<-rep(LETTERS[1:10],each=10)   
bs_eff<-rep(rnorm(10,0,sigma_s),each=10)   ###随机效应
mu<-exp(b0+b1*x1+b2*x2+f(x3)+bs_eff*x1)    ###根据设定的值计算泊松分布均值
y<-rpois(100,lambda = mu)    ###抽取泊松分布随机数
dat<-data.frame(x1,x2,x3,y,bs,bs_eff)
dat
library(gamm4)    ###根据随机数进行拟合
fit1<-gamm4(y~x1+x2+s(x3),data=dat,random=~(x1|bs),family = "poisson")
fit1
plot(fit1$gam)

######拟合GAM
library(mgcv)
set.seed(0)
n<-200
sig2<-4
x1 <- runif(n, 0, 1)
x2 <- runif(n, 0, 1)
x3 <- runif(n, 0, 1)
fac<-c(rep(1,n/2),rep(2,n/2)) # create factor
fac.1<-rep(0,n)+(fac==1)   ###fac=1的话就是0+1，fac=0的话就是0+0
fac.2<-1-fac.1 # and dummy variables
fac<-as.factor(fac)
f1 <-  exp(2 * x1) - 3.75887
f2 <-  0.2 * x1^11 * (10 * (1 - x1))^6 + 10 * (10 * x1)^3 * (1 - x1)^10
f<-f1*fac.1+f2*fac.2+x2
e <- rnorm(n, 0, sqrt(abs(sig2)))
y <- f + e
# NOTE: smooths will be centered, so need to include fac in model....
b<-gam(y~fac+s(x1,by=fac)+x2) 
plot(b,pages=1)


######mixed model(PPT)
library(plyr)
library(mvtnorm)
library(lme4)
make.data.generator<-function(true.effects=c(0,0),
                              resid.var=1,
                              ranef.covar=diag(c(1,1)),
                              n.subj=24,
                              n.obs=24
                              )
{
  data.str<-data.frame(freq=factor(c(rep("high",n.obs/2),rep("low",n.obs/2))))
  contrasts(data.str$freq)<-contr.sum(2)
  model.mat<-model.matrix(~1+freq,data.str)
  
  generate.data<-function(){
    simulated.data<-rdply(n.subj,{
      beta<-t(rmvnorm(n=1,sigma=ranef.covar))+true.effects
      expected.RT<-model.mat%*%beta
      epsilon<-rnorm(n=length(expected.RT),mean=0,sd=sqrt(resid.var))
      data.frame(data.str,RT=expected.RT+epsilon)
  })
    names(simulated.data)[1]<-"subject"
    simulated.data
  }
}

fit.models<-function(simulated.data){
  lm.coefs<-coefficients(summary(lm(RT~1+freq,simulated.data)))[,1:3]
  rand.int.coefs<-coefficients(summary(lmer(RT~1+freq+(1|subject),simulated.data)))
  rand.slope.coefs<-coefficients(summary(lmer(RT~1+freq+(1|subject),simulated.data)))
}



#######################################1999 DPQL二项可加混合模型数值模拟 logit(E(y|b))=beta0+beta1*ti+f1(x2i)+f2(x3ij)+bi
set.seed(123456)
i<-1:100
j<-1:5
beta0<--0.5   ###beta0初始值
beta1<-1      ###beta1初始值
k<-1:500      ###数据范围
t<-ifelse(k<=N/2,1,0)      ###t取值
###定义函数Fp,q(x)=gamma(p+q)/(gamma(p)*gamma(q))*x^(p-1)*(1-x)^(q-1)
f<-function(p,q,x){
  (gamma(p+q)/(gamma(p)*gamma(q)))*x^(p-1)*(1-x)^(q-1)
}
###定义函数f1(x1)=1/3*[2F8,8(x1)+F5,5(x1)]-1,f2(x2)=1/10*[6*F30,17(x2)+4*F3,11(x2)]-1
f1<-function(x1){
  1/3*(2*f(8,8,x1)+f(5,5,x1))-1
}
f2<-function(x2){
  1/10*(6*f(30,17,x2)+4*f(3,11,x2))-1
}
###定义x1,x2
x1<-trunc((i+1)/2)/100
x2<-trunc((i+4)/5)/100+0.2*(j-1)
###抽取y
b<-rep(1:100,each=5)
b_eff<-rep(rnorm(100,0,sqrt(0.5)),each=5)
head(b_eff)
mu1<-exp(beta0+beta1*t+f1(x1)+f2(x2)+b_eff)/(1+exp(beta0+beta1*t+f1(x1)+f2(x2)+b_eff))
y<-rbinom(N,1,mu1)
###建立GAMM求参数
dat1<-data.frame(y,t,x1,x2,b,b_eff)
head(dat1)
library(gamm4)
fit1<-gamm4(y~t+s(x1)+s(x2),data=dat1,random=~(1|b),family = "binomial")
fit1  ###结果显示：beta0=-0.5394,beta1=1.0336,真实值是-0.5和1
str(fit1)
betafit<-c(fit1$gam$coefficients[1],fit1$gam$coefficients[2])
betafit

######################################泊松可加混合模型数值模拟
set.seed(111111)
y2<-rpois(N,exp(beta0+beta1*t+f1(x1)+f2(x2)+b_eff))
dat2<-data.frame(y2,t,x1,x2,b,b_eff)
head(dat2)
fit2<-gamm4(y2~t+s(x1)+s(x2),data=dat2,random=~(1|b),family = "poisson")
fit2  

######################################高斯可加混合模型数值模拟
set.seed(111111)
y3<-rnorm(N,beta0+beta1*t+f1(x1)+f2(x2)+b_eff)
dat3<-data.frame(y3,t,x1,x2,b,b_eff)
head(dat3)
fit3<-gamm4(y3~t+s(x1)+s(x2),data=dat3,random=~(1|b),family = "gaussian")
fit3  
plot(fit3$gam)

#####################################模拟多次，观察误差
### 二项可加混合模型
library(gamm4)
library(gammSlice)
A<-matrix(0,nrow = 10,ncol = 2)
colnames(A)<-c("beta0","beta1")
B<-matrix(0,nrow=10,ncol = 2)
colnames(B)<-c("beta0","beta1")
for(a in 1:10) {
    i<-1:100
    j<-1:5
    beta0<--0.5   
    beta1<-1      
    k<-1:500   
    N<-500
    t<-ifelse(k<=N/2,1,0)      
    f<-function(p,q,x){
      (gamma(p+q)/(gamma(p)*gamma(q)))*x^(p-1)*(1-x)^(q-1)
    }
    f1<-function(x1){
      1/3*(2*f(8,8,x1)+f(5,5,x1))-1
    }
    f2<-function(x2){
      1/10*(6*f(30,17,x2)+4*f(3,11,x2))-1
    }
    x1<-trunc((i+1)/2)/100
    x2<-trunc((i+4)/5)/100+0.2*(j-1)
    b<-rep(1:100,each=5)
    b_eff<-rep(rnorm(100,0,sqrt(0.5)),each=5)
    mu1<-exp(beta0+beta1*t+f1(x1)+f2(x2)+b_eff)/(1+exp(beta0+beta1*t+f1(x1)+f2(x2)+b_eff))
    y<-rbinom(N,1,mu1)
    dat1<-data.frame(y,t,x1,x2,b,b_eff)
    fit11<-gamm4(y~t+s(x1)+s(x2),data=dat1,random=~(1|b),family = "binomial")
    A[a,]<-c(fit11$gam$coefficients[1],fit11$gam$coefficients[2])     
    fit12<-gSlc(y~t+s(x1)+s(x2),data=dat1,random=~(1|b),family = "binomial")
    B[a,]<-c(fit12$gam$coefficients[1],fit12$gam$coefficients[2]) 
}







########################################## M-H抽样
m<-5000  ###链的长度
a<-1
b<-1
xt<-numeric(m)
n<-30
mu<-c(0,5)       ###均值是0,5两个值
sigma<-c(1,1)    ###方差是1,1两个值
i<-sample(1:2,size=n,replace=TRUE,prob = c(0.2,0.8))  ##从1,2中抽取样本，样本容量为n，可重复抽样，1,2概率分别为0.2和0.8
x<-rnorm(n,mu[i],sigma[i])  ###产生服从正态分布的样本，样本量为n，其中20%是第一个均值和方差，80%是第二个均值和方差
u<-runif(m)   ###产生均匀分布随机数5000个
y<-rbeta(m,a,b)   ###产生beta(1,1)随机数5000个
xt[1]<-0.5    ###给定初始值
for (i in 2:m) {
  fy<-y[i]*dnorm(x,mu[1],sigma[1])+(1-y[i])*dnorm(x,mu[2],sigma[2])
  fx<-xt[i-1]*dnorm(x,mu[1],sigma[1])+(1-xt[i-1])*dnorm(x,mu[2],sigma[2])
  alpha<-prod(fy/fx)*(xt[i-1]^(a-1)*(1-xt[i-1])^(b-1))/(y[i]^(a-1)*(1-y[i])^(b-1))
  if(u[i]<=alpha) xt[i]=y[i]
  else xt[i]=xt[i-1]
}
###链的路径图和丢掉100个样本后的直方图
plot(xt,type="l",ylab="p")
hist(xt[100:m],main="p",prob=TRUE)
print(mean(xt[101:m]))


########################################## Gibbs采样
N<-5000   ###链的长度
burn<-1000   ###燃烧期
X<-matrix(0,N,2)   ###用来放抽到的数据
rho<--0.75   ###pho初始值
mu1<-0
mu2<-2
sigma1<-1
sigma2<-0.5
s1<-sqrt(1-rho^2)*sigma1
s2<-sqrt(1-rho^2)*sigma2   ###条件分布方差
X[1,]<-c(mu1,mu2)   ###X第一行第一个值和第二个值分别为mu1,mu2，初始值
for (i in 2:N) {
  x2<-X[i-1,2]
  m1<-mu1+rho*(x2-mu2)*sigma1/sigma2   ###条件均值
  X[i,1]<-rnorm(1,m1,s1)
  x1<-X[i,1]
  m2<-mu2+rho*(x1-mu1)*sigma2/sigma1
  X[i,2]<-rnorm(1,m2,s2)
}
b<-burn+1
x<-X[b:N,]
colMeans(x)
cov(x)
cor(x)
plot(x,main="",cex=.5,xlab=bquote(X[1]),ylab=bquote(X[2]),ylim=range(X[,2]))
