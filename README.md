---
header-includes:
- \usepackage{xeCJK}
- \usepackage{fontspec}
- \setCJKmainfont{微軟正黑體}
- \XeTeXlinebreaklocale "zh"
- \XeTeXlinebreakskip = 0pt plus 1pt

title: "Regression analysis 期末專題計畫書"
author: "吳翔宇"
output: 
  pdf_document:
    latex_engine: xelatex
---

```{r global options, include=FALSE}
knitr::opts_chunk$set(warning = FALSE,message = FALSE,fig.showtext = TRUE)
library(showtext)
showtext_auto()
options(digits = 4,scipen = 999)
```
            
# 一、研究動機
            
隨著科技發展，醫療衛生的進步，人的預期壽命不斷向上提高，但隨著科技進步貧富差距也隨之擴大。這狀況也反應在預期壽命上，有的人能享有世界上最好醫療資源，但也有生個小病都能致命的狀況發生。因應各國不同的醫療政策及經濟環境，不同國家人民的預期壽命也可能天差地遠。
           
# 二、研究目的
         
## 本次研究目的:
         
- 探討開發中國家與已開發國家預期壽命是否有顯著差異
- 影響預期壽命的因子與其重要程度
         
           
# 三、資料簡介與篩選

## 1. 資料來源
       
本次使用資料為 [Kaggle:Life Expectancy (WHO)](https://www.kaggle.com/kumarajarshi/life-expectancy-who)
       
## 2. 資料背景
         
世界衛生組織(WHO)所屬的全球衛生觀察站(GHO)在2015時發現，過去15年由於全球衛生水準迅速提高，與過去30年相比，人類死亡路有所提高，在開發中國家尤其明顯，因此在WHO的開放資料中收集了2000年至2015年中193個國家的預期壽命與健康因素相關的其他數據，希望藉由此數據來探討影響人們預期壽命的重要因素。
           
## 3. 變數
               
```{r}
data <- read.csv("LifeExpectancyData.csv")[,-c(1,15,18:21)]
colnames(data) <- c("year","status","life","am","ind","alcohol","perexp","HB","measles",
                    "bmi","ufd","polio","texp","hiv","gdp","school")
data$status = ifelse(data$status == "Developing",0,1)
data$status = factor(data$status)
```

                  
|變數|定義|
|:---|:---|
|year|年份|
|status|0:開發中國家(Developing) 1:已開發國家(Developed)|
|Life|預期壽命|
|am|15至60歲人民，每1000人死亡人數|
|ind|低於1歲之嬰兒，每1000人死亡人數|
|alcohol|15歲以上人民人均酒精消費量。|
|perexp|醫療支出佔人均GDP百分比|
|HB|1歲孩童B型肝炎疫苗覆蓋率|
|measles|麻疹病例數|
|bmi|體重(kg)除身高(m)的平方$\frac{kg}{m^2}$|
|ufd|5歲以下，每1000人死亡人數|
|polio|1歲一下小兒麻痺症疫苗覆蓋率|
|texp|政府醫療支出佔總開支百分比|
|hiv|0至4歲孩童，每1000人死於愛滋病毒/愛滋病人數|
|gdp|人均GDP(美元)|
|school|受教育年數(年)|
                
```{r}
str(data)
summary(data)
```
      
可以看出資料內含有許多遺漏值，且因為是WHO的資料，經檢查過後並沒有特別異常的值。
             
## 4. 資料概述與清洗
                        
```{r}
dim(data)
sum(is.na(data))
```
                 
此次使用之資料共有18個變數每個變數共有2938列。其中共有缺失值1657筆。 
     
(1) 刪除資料    
        
```{r}
# 刪除缺失值大於等於5筆的列
tem = is.na.data.frame(data)
dat = data[-which(rowSums(tem) >= 5),]
dim(dat)[1] - dim(dat)[1]
colSums(is.na(dat))
```
    
共刪除209筆缺失值大於等於5筆的列，刪除後缺失值數量為2100筆。
     
(2) 補值法
     
- life:預期壽命
     
```{r}
# 因life為反應變數，因此有缺失的列直接刪除
dat = dat[!is.na(dat$life),]
dim(dat)
# 查看各變數下缺失值數量。
colSums(is.na(dat))
```
    
- alcohol:15歲以上人民人均酒精消費量。
     
```{r}
# 檢查alcohol分別在開發中與已開法國家缺失值數量
sum(is.na(dat$alcohol)[dat$status == 0])
sum(is.na(dat$alcohol)[dat$status == 1])
# alcohol在開發中與已開法國家缺失值數量各為154與28，並分別用其median補值
alcohol_median_status0 = median(dat$alcohol[dat$status == 0], na.rm = TRUE)
alcohol_median_status1 = median(dat$alcohol[dat$status == 1], na.rm = TRUE)
dat$alcohol[dat$status == 0 & is.na(dat$alcohol)] = alcohol_median_status0
dat$alcohol[dat$status == 1 & is.na(dat$alcohol)] = alcohol_median_status1
# 檢查alcohol的缺失值
sum(is.na(dat$alcohol))
```
     
- HB:1歲孩童B型肝炎疫苗覆蓋率
     
```{r}
# 檢查HB分別在開發中與已開法國家缺失值數量
sum(is.na(dat$HB)[dat$status == 0])
sum(is.na(dat$HB)[dat$status == 1])
# HB在開發中與已開法國家缺失值數量各為369與173，並分別用其median補值
HB_median_status0 = median(dat$HB[dat$status == 0], na.rm = TRUE)
HB_median_status1 = median(dat$HB[dat$status == 1], na.rm = TRUE)
dat$HB[dat$status == 0 & is.na(dat$HB)] = HB_median_status0
dat$HB[dat$status == 1 & is.na(dat$HB)] = HB_median_status1
# 檢查HB的缺失值
sum(is.na(dat$HB))
```
         
- bmi:體重(kg)除身高(m)的平方$\frac{kg}{m^2}$
    
```{r}
# 檢查bmi分別在開發中與已開法國家缺失值數量
sum(is.na(dat$bmi)[dat$status == 0])
sum(is.na(dat$bmi)[dat$status == 1])
# bmi的21筆缺失值都在開發中國家，並依照開發中國家的mean補值
bmi_mean_status0 = mean(dat$bmi[dat$status == 0], na.rm = TRUE)
dat$bmi[is.na(dat$bmi)] = bmi_mean_status0
# 檢查bmi的缺失值
sum(is.na(dat$bmi))
```
      
- polio:1歲一下小兒麻痺症疫苗覆蓋率
       
```{r}
# 檢查polio分別在開發中與已開法國家缺失值數量
sum(is.na(dat$polio)[dat$status == 0])
sum(is.na(dat$polio)[dat$status == 1])
# polio的8筆缺失值都在開發中國家，並依照開發中國家的median補值
polio_median_status0 = round(median(dat$polio[dat$status == 0], na.rm = TRUE),0)
dat$polio[is.na(dat$polio)] = polio_median_status0
# 檢查polio的缺失值
sum(is.na(dat$polio))
```
       
- texp:政府醫療支出佔總開支百分比
          
```{r}
# 檢查texp分別在開發中與已開法國家缺失值數量
sum(is.na(dat$texp)[dat$status == 0])
sum(is.na(dat$texp)[dat$status == 1])
# texp在開發中與已開法國家缺失值數量各為183與32，並分別用其median補值
texp_median_status0 = round(median(dat$texp[dat$status == 0], na.rm = TRUE),2)
texp_median_status1 = round(median(dat$texp[dat$status == 1], na.rm = TRUE),2)
dat$texp[dat$status == 0 & is.na(dat$texp)] = texp_median_status0
dat$texp[dat$status == 1 & is.na(dat$texp)] = texp_median_status1
# 檢查texp的缺失值
sum(is.na(dat$texp))
```
          
- gdp:人均GDP(美元)
          
```{r}
# 檢查gdp分別在開發中與已開法國家缺失值數量
sum(is.na(dat$gdp)[dat$status == 0])
sum(is.na(dat$gdp)[dat$status == 1])
# gdp在開發中與已開法國家缺失值數量各為371與64，並分別用其median補值
gdp_median_status0 = round(median(dat$gdp[dat$status == 0], na.rm = TRUE),3)
gdp_median_status1 = round(median(dat$gdp[dat$status == 1], na.rm = TRUE),3)
dat$gdp[dat$status == 0 & is.na(dat$gdp)] = gdp_median_status0
dat$gdp[dat$status == 1 & is.na(dat$gdp)] = gdp_median_status1
# 檢查gdp的缺失值
sum(is.na(dat$gdp))
```
          
- school:受教育年數(年)
        
```{r}
# 檢查school分別在開發中與已開法國家缺失值數量
sum(is.na(dat$school)[dat$status == 0])
sum(is.na(dat$school)[dat$status == 1])
# school在開發中與已開法國家缺失值數量各為112與48，並分別用其median補值
school_median_status0 = round(median(dat$school[dat$status == 0], na.rm = TRUE),1)
school_median_status1 = round(median(dat$school[dat$status == 1], na.rm = TRUE),1)
dat$school[dat$status == 0 & is.na(dat$school)] = school_median_status0
dat$school[dat$status == 1 & is.na(dat$school)] = school_median_status1
# 檢查school的缺失值
sum(is.na(dat$school))
```
     
### 最後檢驗
     
```{r}
sum(is.na(dat))
```
            
# 四、建立迴歸模型及檢驗
     

```{r}
fit1 = lm(life ~ ., data = dat)
summary(fit1)
```
          
- 殘差檢定
           
(1) Constant Variance      
          
```{r}
plot(residuals(fit1)~fitted(fit1), xlab = "Fitted", ylab = "Residuals",
     main = "Residual plot")
abline(h = 0, col = "red")
library(car)
ncvTest(fit1)
```
      
在Non-constant Variance Score Test中p-value < 0.05，因此殘差並不符合constant的假設。   
        
(2) Normality        
      
```{r}
qqnorm(residuals(fit1))
qqline(residuals(fit1))
# 常態檢定
shapiro.test(residuals(fit1))
```
        
Shapiro-Wilk 常態檢定中p-value < 0.05，因此殘差並不符合常態性的假設。
        
(3) Correlated Errors
       
```{r}
plot(residuals(fit1)[-length(residuals(fit1))], residuals(fit1)[-1],
     xlab = expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]))
library(lmtest)
dwtest(life ~ . - year, data = dat)
```
       
Durbin-Watson 檢定中p-value < 0.05，因此殘差不符合獨立性的假設。       
       
```{r}
library(forecast)
y = dat$life
x = dat[,colnames(dat) != c("life","ind")]
x_mat = model.matrix(fit1)[,-c(1,5)]
x_mat[x_mat$status1 == 1,]
z = auto.arima(y, xreg = x_mat, seasonal = TRUE, seasonal.test = "seas")

fit_ar = arima(y, xreg = x_mat, order = c(3,0,1))
residuals(fit_ar)
plot(residuals(fit_ar)[-length(residuals(fit_ar))], residuals(fit_ar)[-1],
     xlab = expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]))

checkresiduals(fit_ar)
tsdiag(fit_ar)
Box.test(residuals(fit_ar),type="Ljung-Box", fitdf = 4)
```
       
- 共線性       
       
```{r}
library(faraway)
x = model.matrix(fit1)[,-1]
vif(x)
```
       
可以看到ind(低於1歲之嬰兒，每1000人死亡人數)與ufd(5歲以下人口，每1000人死亡人數)兩個變數vif > 100，我們將ind去除後重新配適模型。       
    
```{r}
fit2 = lm(life ~ . -year -ind, data = dat)
summary(fit2)
x = model.matrix(fit2)[,-1]
vif(x)
```
      
刪除ind後模型就不存在共線性問題。      
       
- 離群值&影響點偵測
               
```{r}
hatv = hatvalues(fit2)
max(hatv)

n = dim(dat)[1]
p = dim(dat)[2]+1
stud = rstudent(fit2)
t_stat = abs(qt(0.05/(n*2),n-p-1))
stud[which(abs(stud)>t_stat)]
```

















