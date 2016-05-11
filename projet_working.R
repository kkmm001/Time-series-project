#install.packages("forecast", dependencies=TRUE) # dependencies should be loaded


library("urca")
library("tseries")
library("foreign")
library("dygraphs")
library('forecast')

## Clean up
rm(list=ls()) 
cat("\014") 

#Q1 corriger la serie originale -- desaisonnalisation

#Indice brut de la production industrielle (base 100 en 2010) - 
#Production et distribution d'?lectricit?, de gaz, de vapeur et d'air conditionn? (NAF r?v. 2, niveau section, poste D)

#i) importation de donnees - GAZ.csv
data <- read.csv2("Gaz_eletricity.csv", col.names=c("year", "month", "value"))
ts.data = ts(data$value, frequency = 10, end = c(2016,1),start = c(1990,1))
plot(ts.data, col=c("blue"))

#ii)acf et pacf disent que ts.data est saisonnier
ts.acf = acf(ts.data, main="S?rie brute ACF")
ts.pacf = pacf(ts.data, main="S?rie brute PACF")

#iii) desaisonnalisation par difference saisonniere

ts.diff = diff(ts.data, lag=12)
plot(ts.diff, main="S?rie d?saisonnalis?e")

#iv) test stationnarite
pp.test(ts.diff)
adf.test(ts.diff)

  # conclusion: ts.diff est stationnaire

#Q2 Modele ARMA

#i) En analysant la fn acf/pacf, on retrouve les ordres du modele ARMA tels que:
acf(ts.diff, main="S?rie d?saisonnalis?e")  #q=2
pacf(ts.diff, main="S?rie d?saisonnalis?e") #p=1

# Remarque: on constate que ACF/PACF sont encore significatives. C'est li? ? la P/Q du mod?le SARIMA

sarima.ts = arima(ts.diff, order=c(1, 0, 2), seasonal = list(order = c(0, 0, 1), period = 12))
sarima.ts.1 = arima(ts.diff, order=c(1, 0, 2), seasonal = list(order = c(1, 0, 1), period = 12))
sarima.ts.2 = arima(ts.diff, order=c(1, 0, 2), seasonal = list(order = c(0, 0, 2), period = 12))

#On en d?duit que (P,Q)=(0,1)

#ii)On calcule la matrice AIC, BIC pour d?terminer p,q
pmax = 5
qmax = 5

AICmatrix=matrix(NA,pmax,qmax)
BICmatrix=matrix(NA,pmax,qmax)

rownames(AICmatrix)=0:(pmax-1)
colnames(AICmatrix)=0:(qmax-1)

rownames(BICmatrix)=0:(pmax-1)
colnames(BICmatrix)=0:(qmax-1)


for (p in 1:pmax){
  for (q in 1:qmax){
    sarima.ts<-arima(ts.diff,order=c(p-1,0,q-1), seasonal = list(order = c(0, 0, 1), period = 12), method="ML")
    AICmatrix[p,q]<-AIC(sarima.ts)
    BICmatrix[p,q]<-AIC(sarima.ts, k=log(length(sarima.ts)))
  }
}

AICmatrix
BICmatrix

# On en trouve un groupe: (p,q)=(2,1)

#iii) Test de significativit?s pour ces deux groupes
sarima.ts = arima(ts.diff, order=c(2, 0, 1), seasonal = list(order = c(0, 0, 1), period = 12))
sarima.ts$coef/sqrt(diag(sarima.ts$var.coef)) #pour voir si significatif
# tous les params sont significatifs.

#iv) Test des r?sidus
Box.test(sarima.ts$residuals, lag=30,type = c("Box-Pierce"))
Box.test(sarima.ts$residuals, lag=30,type = c("Ljung-Box"))

tsdiag(sarima.ts, gof.lag=50)
plot(sarima.ts$residuals)

#v) Test de normalit?

qqnorm(sarima.ts$residuals)
qqline(sarima.ts$residuals)

hist(as.numeric(sarima.ts$residuals))
#probplot(sarima.ts$residuals, qdist=qnorm)
shapiro.test(sarima.ts$residuals)


#Q3) Prediction

pred = forecast(sarima.ts, h=24, level=95)
lower = pred$lower
upper = pred$upper

pred.upper=ts(pred$upper, frequency = 10, end = c(2018,1),start = c(2016,1))
pred.lower=ts(pred$lower, frequency = 10, end = c(2018,1),start = c(2016,1))
pred.mean=ts(pred, frequency = 10, end = c(2018,1),start = c(2016,1))



plot(y=ts.diff, x=)
par(new = TRUE)
plot(pred$lower, type="o", col=c("red"))

plot()


