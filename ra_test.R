rm(list=ls())
library(readxl)
library(plm)
library(stargazer)
library(lmtest)
library(sandwich)

setwd("C:\\Users\\ASUS\\Desktop\\FL\\Test_task")
testdata<-read.csv("ra_test_data.csv") 

panle_data<-pdata.frame(testdata)

#within model
model1<- plm(y~trt+post+trt:post,data = panle_data,model="within")
summary(model1)
x<-coeftest(model1, vcov = vcovHC(model1, type = "HC0"))
summary(x)
stargazer(x, title="Effect of treatment on Y", align=TRUE,type="text",out="x.text")

#diagnostic tests

#Check if residuals are normally distributed
library(ggplot2)
ggplot()+
  geom_histogram(aes(x=model1$residuals, y=..density..), binwidth = .05, fill = "grey")

#Below two codes are used to construct a quintile-quintile plot of residuals.
#Just like the histogram of residuals , this plot is also a visual aid to check normality of residuals. 
#qqnorm(model1$residuals)
qqline(model1$residuals)
# From the plot we can see that upper tails and lower tails of residuals stray/deviate away from the line
#This plot shows that residuals form an "S" shape indicating over dispersion relative to normal distribution/ residuals are not normally distributed.

plot(fitted(panle_data), resid(model1), col = "grey", pch = 20,
     xlab = "Fitted", ylab = "Residuals", main = "Data from Model 2")
abline(h = 0, col = "darkorange", lwd = 2)

#bootstrap
install.packages("boot")
library(boot)
set.seed(10)
meanFunc <- function(panle_data,i){mean(panle_data[i])}

#calculate standard error using 100 bootstrapped samples
boot(panle_data, meanFunc, 100)
#Above is throwing an error, so using own formula
#Own formula

btstrp<-mean(replicate(100, sd(sample(panle_data, replace=T))/sqrt(length(panle_data))))
