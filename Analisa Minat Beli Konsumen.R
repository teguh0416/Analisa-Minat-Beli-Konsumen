library(readxl)
library(psych)
library(dplyr)
library(tidyverse) 
library(coefficientalpha)
library(ltm)

#Uji Realibilitas
X1 <- select(data_kuesioner,c(X1.1, 
                              X1.2,
                              X1.3,
                              X1.4,
                              X1.5,
                              X1.6,
                              X1.7,
                              X1.8,
                              X1.9,
                              X1.10,
                              X1.11,
                              X1.12))
X2 <- select(data_kuesioner,c(X2.1, 
                              X2.2,
                              X2.3,
                              X2.4,
                              X2.5,
                              X2.6))
Y <- select(data_kuesioner,c(Y1.1, 
                             Y1.2,
                             Y1.3,
                             Y1.4))

cronbach.alpha(X2)
#psych::alpha(X2)
#Jika error, uninstall dplyr

#Uji Validitas
val_x1 <- cor(select(data_kuesioner, X1.1, 
           X1.2,
           X1.3,
           X1.4,
           X1.5,
           X1.6,
           X1.7,
           X1.8,
           X1.9,
           X1.10,
           X1.11,
           X1.12,
           X1))

val_x2 <- cor(select(data_kuesioner, X2.1, 
                     X2.2,
                     X2.3,
                     X2.4,
                     X2.5,
                     X2.6,
                     X2))

val_y <- cor(select(data_kuesioner, Y1.1, 
                     Y1.2,
                     Y1.3,
                     Y1.4,
                    Y1.5,
                    Y1.6,
                    Y1.7,
                    Y1.8,
                    Y))
#psych::alpha(val_x2)

#Model Regresi
x_1 = data_kuesioner$X1
x_2 = data_kuesioner$X2
y1 = data_kuesioner$Y
model <- lm(y ~ x_1+x_2)

#Grafik Normalitas dan Heterodekedasitas
par(mfrow=c(2,2))
plot(model)

#Uji Normalitas
library(nortest)
library(qqplotr)
lillie.test(model$residuals)
ks.test(model$residuals, ecdf(model$residuals))
#qqnorm(data_kuesioner$Y+data_kuesioner$X1+data_kuesioner$X2)
#qqline(data_kuesioner$Y+data_kuesioner$X1+data_kuesioner$X2,lwd=2)

#Uji Multikolinieritas
library(olsrr)
ols_vif_tol(model)

#Uji Autokorelasi DW
library(lmtest)
dwtest(model)

#Uji Breusch-Pagan (Heterokedastisitas)
distBCMod <- caret::BoxCoxTrans(data_kuesioner$Y)
minat_beli <- cbind(data_kuesioner, Y_new = predict(distBCMod,data_kuesioner$Y))
lmMod_bc <- lm(Y_new ~ X1 + X2, data=minat_beli)
bptest(lmMod_bc)

#Uji F, Uji T, Koefisien Determinasi
summary(model)
