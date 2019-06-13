---
title: "开题报告"
author: "Li"
date: '2019-04-15'
csl: ./style/chinese-gb7714-2005-numeric.csl
css: ./style/markdown.css
bibliography: Bibfile.bib
eqnPrefixTemplate: ($$i$$)
link-citations: yes
linkReferences: yes
notice: '@*'
autoEqnLabels: yes
---

**TODO**










# 城市空气质量状况描述及时空分布特征

## 空气质量简介

### 空气质量指数简介

在《环境空气质量标准》中，将环境空气定义为人群、植物、动物和建筑物所暴露的室外空气。环境空气
功能区分为二类：一类为自然保护区、风景名胜区和其他需要特殊保护的区域；二类区为居住区、商业交
通居民混合区、文化区、工业区和农村地区。一类区适用一级浓度限值，二类区适用二级浓度限值。环境
空气常规污染物基本浓度限值如下表所示。


Table: (\#tab:tab1)环境空气污染物基本项目浓度限值

序号   污染物项目                平均时间          浓度限值   浓度限值.1   单位        
-----  ------------------------  ----------------  ---------  -----------  ------------
                                                   一级       二级                     
1      二氧化硫（SO2）           年平均            20         60           $\mu g/m^3$ 
                                 24小时平均        50         150          $\mu g/m^3$ 
                                 1小时平均         150        500          $\mu g/m^3$ 
2      二氧化氮（NO2）           年平均            40         40           $\mu g/m^3$ 
                                 24小时平均        80         80           $\mu g/m^3$ 
                                 1小时平均         200        200          $\mu g/m^3$ 
3      一氧化碳（CO）            24小时平均        4          4            $m g/m^3$   
                                 1小时平均         10         10           $m g/m^3$   
4      臭氧（O3）                日最大8小时平均   100        160          $\mu g/m^3$ 
                                 1小时平均         160        200          $\mu g/m^3$ 
5      颗粒物（粒径小于等于10    年平均            40         70           $\mu g/m^3$ 
       ）                        24小时平均        50         150          $\mu g/m^3$ 
6      颗粒物（粒径小于等于2.5   年平均            15         35           $\mu g/m^3$ 
       ）                        24小时平均        35         75           $\mu g/m^3$ 

注：颗粒物（粒径小于等于10 $\mu m$）指的是环境空气中空气动力学当量直径小于等于10
$\mu m$ 的颗粒物，也称可吸入颗粒物；颗粒物（粒径小于等于2.5 $\mu m$ ）指的是环境
空气中空气动力学当量直径小于等于2.5 $\mu m$ 的颗粒物，也称细颗粒物。1小时平均指
任何1小时污染物浓度的算术平均值；8小时平均指连续8小时平均浓度的算术平均值，也称8
小时滑动平均；24小时平均指一个自然日24小时平均浓度的算术平均值，也称为日平均；年
平均指一个日历年内各日平均浓度的算术平均值。

《环境空气质量指数（AQI）技术规定（试行）》对空气质量指数及常规污染物进行了定义。空气质量
指数（air quality index，AQI）是定量描述空气质量状况的无量纲指数，是空气质量分指数中的最大
值。空气质量分指数（individual air quality index，IAQI）指单项污染物的空气质量指数。用数学
语言表述即为： $\mathrm{AQI}=\max \left\{\mathrm{IAQI}_{1}, \mathrm{IAQI}_{2},
\ldots, \mathrm{IAQI}_{\mathrm{n}}\right\}$ ，
其中 $n$ 表示污染物项目。 AQI 大于50时 IAQI 最大的污染物为首要污染物指，若
IAQI 最大的污染物为两项或两项以上，则并列为首要污染物。 IAQI 大于100的污染物为超标污染物。

空气质量指数相关信息如表2所示。当空气质量为优时，空气质量令人满意，基本无空气污染，各类人群
可正常活动。当空气质量为良时，空气质量可接受，但某些污染物可能对极少数异常敏感人群健康有较
弱影响，此时，极少数异常敏感人群应减少户外活动。当空气质量为轻度污染时，易感人群症状有轻度
加剧，健康人群出现刺激症状，儿童、老年人及心脏病、呼吸系统疾病患者应减少长时间、高强度的户
外锻炼。当空气质量为中度污染时，会进一步加剧易感人群的症状，可能对健康人群心脏、呼吸系统有
影响，此时，儿童、老年人及心脏病、呼吸系统疾病患者应避免长时间、高强度的户外锻炼，一般人群
也要适量减少户外运动。当空气质量为重度污染时，心脏病和肺病患者症状显著加剧，运动耐受力降低，
健康人群普遍出现一些症状，这时，儿童、老年人和心脏病患者应停留在室内，停止户外运动，一般人
群也需要减少户外运动。当空气质量为严重污染时，健康人群运动耐受力降低，有明显强烈的症状，会
提前出现某些疾病，儿童、老年人和病人应当留在室内，避免体力消耗，一般人群也应避免户外活动。


Table: (\#tab:tab2)空气质量指数及其等级划分

空气质量指数   空气质量指数级别   空气质量指数级别及表示颜色   X      
-------------  -----------------  ---------------------------  -------
0~50           一级               优                           绿色   
51~100         二级               良                           黄色   
101~150        三级               轻度污染                     橙色   
151~200        四级               中度污染                     红色   
201~300        五级               重度污染                     紫色   
>300           六级               严重污染                     褐红色 

2013年起，环境保护部要求对六种常规污染物进行监测，包括二氧化硫、二氧化氮、一氧化
碳、臭氧、PM10 和 PM2.5 。《2017中国生态环境状况公报》指出，2017 年，全国 338 个
地级及以上城市中，有 99 个城市环境空气质量达标，占全部城市数的 29.3% ，239 个城
市环境空气质量超标，占 70.7% 。338 个城市发生重度污染 2311 次、严重污染 802 次，
以 PM2.5 为首要污染物的天数占重度及以上污染天数的 74.2% ，以 PM10 为首要污染物的
占 20.4% ，以臭氧为首要污染物的占 5.9% 。有研究表明， PM2.5 及以下的微粒，75% 会
在肺泡沉积，长期停留在呼吸系统内，沉积在呼吸系统深处，人体对 PM2.5 没有任何过滤、
阻拦能力，而 PM2.5 对人体的危害极其恐怖。在欧盟国家中，PM2.5 导致人们的平均寿命
减少 8.6 个月，在众多污染物中，PM2.5 对健康的影响尤为显著。

### 空气质量影响因素概述

空气质量的好坏反映了空气污染的程度，空气污染是一个复杂的现象，在特定时间和地点空气质量受
很多因素的影响，比如常见的空气质量影响因素包括地理因素、气象因素及经济社会因素。

#### 地理因素

地理因素主要包括地形地貌、海拔、是否临海等。地形影响气流，气流影响空气流向。通常情况下，
地势高的地方空气流通较好，有利于污染物的扩散，空气质量相对较好。同样，地处海边的城市，
因为有海风和气流的影响，污染物更易扩散，所以相对内陆来讲，临海城市的空气质量一般都要比
内陆好。

#### 气象因素

气象因素是影响空气质量的主要因素之一。通常影响空气质量的气象因素包括气温、气压、风向、
风速、降水量、相对湿度等。大气污染在垂直方向的扩散主要取决于气温的垂直分布，当气温较高
时，大气处于不稳定状态，在热力对流的作用下污染物向上扩散，空气质量就会变好；反之，当
气温较低时，大气变得稳定，污染物的扩散受到抑制作用，空气质量就会变差。一般来讲，当低
压控制时，大气处于中性或不稳定状态，低层空气辐合上升，近地面的污染物随空气上升到高空，
有利于近地面污染物向高空扩散；当高压控制时，空气作下沉运动，并常形成下沉逆温，阻止污
染物的向上扩散。风是边界层内影响污染物稀释的重要因子，风速是造成快速水平输送或平流的
主要原因，而风向则决定大气污染物浓度的分布。风速对污染物的浓度环境浓度具有双重影响，
在一定范围内，风速越大越有利于空气污染物的扩散和稀释，空气质量越优；超过这一范围，风
速增大将使空气中可吸入颗粒物浓度明显增加，导致空气污染加重。针对降水量，大气降水不仅
可以冲刷空气中的部分颗粒物，也可以在一定程度上抑制地面扬尘发生，从而有效控制颗粒物的
排放，对空气质量有较好的净化作用[1]。相对湿度能够促进一次污染物向二次颗粒污染物转化[2]，
相对湿度较大时，由于常规污染物周围被水分包裹，质量加重，重力增大造成一定程度沉降，从而
减小污染物浓度，空气质量相对较好[3]。

#### 经济社会因素

现有的大部分研究表明，经济社会因素即人为因素是影响空气质量最重要的因素，主要表现在人
口因素、经济发展、技术水平、交通运输、绿化建设、能源消耗等方面。空气质量的好坏主要受
污染物排放的影响，汽车尾气、工厂废气、建筑施工、居民生活和取暖、垃圾焚烧等都会产生一
定程度的污染排放。一般来讲，一个地区经济发展越好，人口越多，交通越发达，能源消耗越多，
其污染物排放越多，相应地空气质量也越差；一个地区绿化建设越好，污染治理投入越多，其空
气质量会越好。

## 变量选择及数据预处理

### 变量选择

空气质量的好坏反映了空气污染的程度，空气质量指数是反映空气质量好坏最重要的指标之一，
根据空气质量指数划分的空气质量等级可以对空气质量进行更加直观的判断。严重的空气污染会
导致健康人群运动耐受力降低，有明显强烈的症状，身体出现各种不适，对机体的健康产生一定
的影响，提前出现某些疾病。空气质量为优是我们所期待出现的结果，空气质量令人满意，基本
无空气污染，各类人群可正常活动。这就为我们在进行空气质量的研究时提供了一个全新的视角。
进而结合数据特征，我们可以选取月均空气质量指数、每月是否存在严重污染及月度空气质量为
优的天数作为因变量进行相应分析。

在自变量的选取过程中，通过查阅相关文献，结合空气质量的影响因素及数据的可获得性，可以
初步从经济社会因素、气象因素和地理因素中选取以下指标进行具体分析。经济社会因素中，经
济增长、人口因素、绿化建设、交通运输、能源消耗、废气排放等都会对空气质量产生影响。地
区生产总值是衡量一个地区经济发展的重要指标，固定资产投资在一定程度上也会拉动经济增长。
因此，初步选取地区生产总值和固定资产投资作为城市经济增长的指标。有研究表明，人口集聚
会对空气质量产生一定的影响，人口密度可以反映人口地理分布的疏密程度，但是考虑到市辖区
人口较多，郊区人口较少，从某种意义上讲，人口密度会将市辖区和郊区对空气质量的影响视为
等同，因此，选取年末常住人口表示人口因素。空气质量的两大污染源包括固定污染源和移动污
染源，固定污染源是指向环境排放或释放有害物质或对环境产生有害影响的场所、设备和装置，
用房屋建筑施工面积代表。移动污染源主要指空气排放污染物的交通工具，用私人汽车拥有量和
城市公路客运量进行表示。能源消耗产生的废气严重污染了空气，尤其是煤炭消耗，电能是清洁
高效的二次能源，用全社会用电量代表能源消耗，同时选取工业二氧化硫排放量和工业烟粉尘排
放量作为废气排放的指标。绿化建设可以吸附灰尘、净化空气、减少微粒并减少空气中细菌含量，
对空气的净化有重要的作用。同时，废气治理力度的大小也会对空气质量的好坏产生重要的影响。
因此，选取建成区绿化覆盖率和废气治理完成投资作为绿化建设和污染治理的指标。

经济社会因素可以视为影响空气质量好坏的人为因素，这些因素可以根据实际情况做出相应改变。
而气象因素及地理因素则是影响空气质量好坏的固定因素，气温、降水量、气压、风速、相对湿度、
日照时数都会对空气质量产生一定的影响。同时，城市的地形、南北方差异，北方供暖会产生大量
废气排放，沿海城市因为有海风有利于污染物扩散都会对空气质量产生相应的影响。

根据以上分析，鉴于数据的可获得性，选取比较有代表性的指标，最终确定空气质量印象因素为：
社会经济因素、气象因素和地理因素。其中社会经济因素表现在经济增长、人口因素、绿化建设、
交通运输、固定污染源、移动污染源、能源消耗、废气排放、污染治理这些方面。其中，固定资产
投资、地区生产总值、地区生产总值增长率表示经济增长；常住人口表示人口因素；建成区绿化
覆盖率表示绿化建设；城市公路客运量表示交通运输；房屋建筑施工面积表示固定污染源；私人
汽车拥有量表示移动污染源；全社会用电量表示能源消耗；工业二氧化硫排放量、工业烟粉尘排
放量表示废气排放；废气治理完成投资表示污染治理。最终选定的气象因素包括：最低气温、
最高气温、降水量、平均气压、平均风速、平均气温、平均相对湿度、日照时数。最终选的地理
因素有：是否供暖、是否临海、经度、纬度、海拔。具体的指标体系如表\@ref(tab:tab3) 所示。


Table: (\#tab:tab3)空气质量影响因素指标体系

一级指标       二级指标     三级指标             单位     
-------------  -----------  -------------------  ---------
社会经济因素   经济增长     固定资产投资额       亿元     
                            地区生产总值         亿元     
                            地区生产总值增长率   %        
               人口因素     常住人口             万人     
               绿化建设     建成区绿化覆盖率     %        
               交通运输     城市公路客运量       万人     
               固定污染源   房屋建筑施工面积     万平方米 
               移动污染源   私人汽车拥有量       辆       
               能源消耗     全社会用电量         亿千瓦时 
               废气排放     工业二氧化硫排放量   吨       
                            工业烟粉尘排放量     吨       
               污染治理     废气治理完成投资额   亿元     
气象因素                    最低气温             ℃        
                            最高气温             ℃        
                            降水量               毫米     
                            平均气压             百帕     
                            平均风速             米/秒    
                            平均气温             ℃        
                            平均相对湿度         %        
                            日照时数             小时     
地理因素                    是否供暖                      
                            是否临海                      
                            经度                          
                            纬度                          
                            海拔                 0.1m     

### 数据来源及预处理

（一）数据来源

省会城市和计划单列市一向是我们研究的重点。在数据采集过程中，由于拉萨数据存在严重缺失，
因此本文研究范围为除拉萨以外的30个省会城市及5个计划单列市共35个城市。各城市基本情况
显示在表 \@ref(tab:tab4) 中。其中，城市基本数据中的经度、纬度、海拔来源于中国气象局发布的中国地面国际
交换站气候资料月值数据集，是否供暖根据国家公布的全国供暖时间表进行判断。


Table: (\#tab:tab4)城市基本情况概述

序号   城市名称   所属省份   经度            纬度           海拔    是否临海   是否供暖   X  
-----  ---------  ---------  --------------  -------------  ------  ---------  ---------  ---
1      北京       北京       东经116度28分   北纬39度48分   313     否         是            
2      天津       天津       东经117度04分   北纬39度05分   25      是         是            
3      石家庄     河北       东经114度21分   北纬38度04分   1036    否         是            
4      太原       山西       东经112度33分   北纬37度47分   7783    否         是            
5      呼和浩特   内蒙古     东经111度41分   北纬40度49分   10630   否         是            
6      沈阳       辽宁       东经123度31分   北纬41度44分   490     否         是            
7      大连       辽宁       东经121度38分   北纬38度54分   915     是         是            
8      长春       吉林       东经125度13分   北纬43度54分   2368    否         是            
9      哈尔滨     黑龙江     东经126度46分   北纬45度45分   1423    否         是            
10     上海       上海       东经121度27分   北纬31度24分   55      是         否            
11     南京       江苏       东经118度48分   北纬32度00分   71      否         否            
12     杭州       浙江       东经120度10分   北纬30度14分   417     否         否            
13     宁波       浙江       东经122度06分   北纬30度02分   357     是         否            
14     合肥       安徽       东经117度18分   北纬31度47分   270     否         否            
15     福州       福建       东经119度17分   北纬26度05分   840     是         否            
16     厦门       福建       东经118度04分   北纬24度29分   1394    是         否            
17     南昌       江西       东经115度55分   北纬28度36分   467     否         否            
18     济南       山东       东经117度03分   北纬36度36分   1703    否         是            
19     青岛       山东       东经120度20分   北纬36度04分   760     是         是            
20     郑州       河南       东经113度39分   北纬34度43分   1104    否         是            
21     武汉       湖北       东经114度08分   北纬30度37分   231     否         否            
22     长沙       湖南       东经113度05分   北纬28度12分   449     否         否            
23     广州       广东       东经113度20分   北纬23度10分   410     是         否            
24     深圳       广东       东经114度00分   北纬22度33分   630     是         否            
25     南宁       广西       东经108度13分   北纬22度38分   1216    否         否            
26     海口       海南       东经110度15分   北纬20度00分   635     是         否            
27     重庆       重庆       东经106度28分   北纬29度35分   2591    否         否            
28     成都       四川       东经103度50分   北纬30度42分   5393    否         否            
29     贵阳       贵州       东经106度44分   北纬26度35分   12238   否         否            
30     昆明       云南       东经102度41分   北纬25度01分   18924   否         否            
31     西安       陕西       东经108度58分   北纬34度26分   4100    否         是            
32     兰州       甘肃       东经103度56分   北纬36度21分   16685   否         是            
33     西宁       青海       东经101度45分   北纬36度43分   22952   否         是            
34     银川       宁夏       东经106度13分   北纬38度29分   11114   否         是            
35     乌鲁木齐   新疆       东经873度39分   北纬43度47分   9350    否         是            

由于空气质量是实时数据，年度数据分析可以了解基本概况，月度数据分析可能会使结果更加
准确，因此，本文从年度和月度两个角度对空气质量状况进行分析。考虑到数据的可获得性，
选取2014年1月到2017年12月的城市月度数据，相应的选取2014-2017年的年度数据进行相应分析。
在数据获取过程中，空气质量相关数据及气象指标可以直接获得月度数据，其中，空气质量相关
数据（空气质量指数、PM2.5、PM10、SO2、NO2、CO、O3）来源于中华人民共和国生态环境部发布
的2014年1月到2017年12月的《城市空气质量状况月报》，气象相关数据（最低气温、最高气温、
降水量、平均气压、平均风速、平均气温、平均相对湿度、日照时数）来源于2015-2018年《中国
统计年鉴》、2015-2017年《中国气象年鉴》及中国气象局发布的中国地面国际交换站气候资料
月值数据集。对于经济社会指标相关数据，在所涉及的指标中，表示经济增长的地区生产总值季度
数据及固定资产投资月度数据均来自于各城市2014年2月到2017年12月的月度统计公报，数据可以
从各城市的统计局官网获得。对于年末常住人口、房屋建筑施工面积、城市公路客运量、私人汽车
拥有量、全社会用电量、工业二氧化硫、工业烟粉尘排放等年度数据，均来自2014-2018年相应的
城市统计年鉴，对于部分缺失数据，在各城市年度统计公报中查找。

（二）数据预处理

由于各城市月度固定资产投资数据均为累计值，非单月指标，因此，对于固定资产投资，采用
后一期减前一期的计算方法，得出本月值。同时由于1-2月为累计值，春节期间不单独对单月
数据进行统计，因此，文本采用取平均值的方法，用1-2月累计值的平均值作为1、2月单月数据。

对于部分缺失数据，比如太原市2017年城市公路客运量，乌鲁木齐市2017年常住人口，由于城市
公路客运量和常住人口在不同年度变化幅度不是太大，因此，直接将2016年太原市城市公路客运量
和2016年乌鲁木齐市的常住人口作为2017年的数据进行填充。对于私人汽车拥有量这一指标，比如
呼和浩特市，已有的数据为2014年私人汽车拥有量和2014、2017年民用汽车拥有量，用2014年私人
汽车拥有量在民用汽车拥有量中的占比乘以2017年民用汽车拥有量作为2017年私人汽车拥有量的
数据，2014年私人汽车拥有量加2014-2017年的平均增长量作为2015年的数据，同样2016年的数据
也是在2014年数据的基础上加两年平均增长量。对于全社会用电量这一指标，比如昆明市，已有
2014、2017年的数据，用2014、2017年数据的平均值作为2015年和2016年的数据。其他城市这些
指标的缺失值均采用类似的方法进行处理。对于气象因素中缺失数据的处理，均采用上年同期数据
对本年当期数据进行填充。

对于废气污染治理投资，各城市并未公布具体数据，但其对空气质量的好坏有很大程度的影响。
我们可以从统计年鉴中获得各省份的废气污染治理投资，直观上可以将其视为地区生产总值的一
部分。因此，将各城市地区生产总值占所在省份生产总值的比重乘以所在省份废气治理投资作为
各城市废气污染治理投资的数据。

由于要进行月度分析，在所获得的数据中，针对非月度数据，需要采用合适的频率转化方法进行频率
转化，转为月度数据进行分析。经济指标的数据类型可以分为存量与流量数据，对于地区生产总值、
城市公路客运量、房屋建筑施工面积、全社会用电量、工业二氧化硫排放量、工业烟粉尘排放量、
废气污染治理投资这些流量数据，季度地区生产总值除以3作为月度地区生产总值的近似，其他年度
数据均采用年度数据除以12的方法将数据平均分配到各月中。对于常住人口、建成区绿化覆盖率、
私人汽车拥有量这些存量数据，均将年末值作为各月值进行分析。

## 城市空气质量状况描述

### 城市空气污染状况描述

### 城市空气质量影响因素描述

## 空气质量指数时空分布特征

### 空气质量指数时间分布特征

### 空气质量指数空间分布特征

### 空气质量指数时空格局演变



<!--# 参考文献 {-}-->
[//]: # (\bibliography{Bibfile})