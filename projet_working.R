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
data <- read.csv2("Gaz_eletricity.csv", col.names=c("year", "month", "value"), header=FALSE)
data=data[rev(rownames(data)),]

ts.data = ts(data$value, frequency = 10, end = c(2016,2),start = c(1995,1))

#ii) desaisonnalisation par difference saisonniere
ts.diff = diff(ts.data, lag=12)

mean(ts.diff)

par(mfrow=c(1,2))
plot(ts.data, col=c("blue"), main="Série brute")
plot(ts.diff, main="Série désaisonnalisée")

#iii)acf et pacf disent que ts.data est saisonnier
par(mfrow=c(1,2))
ts.acf = acf(ts.data, main="Série brute ACF")
ts.pacf = pacf(ts.data, main="Série brute PACF")

#iv) test stationnarite
pp.test(ts.diff)
adf.test(ts.diff)

  # conclusion: ts.diff est stationnaire

#Q2 Modele ARMA

#i) En analysant la fn acf/pacf, on retrouve les ordres du modele ARMA tels que:
par(mfrow=c(1,2))
acf(ts.diff, main="Série désaisonnalisée ACF")  #q=2
pacf(ts.diff, main="Série désaisonnalisée PACF") #p=1

# Remarque: on constate que ACF/PACF sont encore significatives. C'est li? ? la P/Q du mod?le SARIMA

sarima.ts = arima(ts.diff, order=c(1, 0, 1), seasonal = list(order = c(0, 0, 1), period = 12))
sarima.ts$coef/sqrt(diag(sarima.ts$var.coef))
sarima.ts.1 = arima(ts.diff, order=c(1, 0, 1), seasonal = list(order = c(1, 0, 1), period = 12))
sarima.ts.1$coef/sqrt(diag(sarima.ts.1$var.coef))
sarima.ts.2 = arima(ts.diff, order=c(1, 0, 1), seasonal = list(order = c(0, 0, 2), period = 12))
sarima.ts.2$coef/sqrt(diag(sarima.ts.2$var.coef))

sarima.ts.3 = arima(ts.diff, order=c(1, 0, 1), seasonal = list(order = c(0, 1, 1), period = 12))
sarima.ts.3$coef/sqrt(diag(sarima.ts.3$var.coef))

sarima.ts.4 = arima(ts.diff, order=c(1, 1, 1), seasonal = list(order = c(0, 0, 1), period = 12))
sarima.ts.4$coef/sqrt(diag(sarima.ts.4$var.coef))

sarima.ts.5 = arima(ts.diff, order=c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
sarima.ts.5$coef/sqrt(diag(sarima.ts.5$var.coef))

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
sarima.ts = arima(ts.diff, order=c(3, 0, 2), seasonal = list(order = c(0, 0, 1), period = 12))
sarima.ts$coef/sqrt(diag(sarima.ts$var.coef)) #pour voir si significatif


s#iv) Test des r?sidus
Box.test(sarima.ts$residuals, lag=30,type = c("Box-Pierce"))
Box.test(sarima.ts$residuals, lag=30,type = c("Ljung-Box"))

tsdiag(sarima.ts, gof.lag=50)

#v) Test de normalit?
par(mfrow=c(1,1))

qqnorm(sarima.ts$residuals)
qqline(sarima.ts$residuals)

hist(as.numeric(sarima.ts$residuals))
#probplot(sarima.ts$residuals, qdist=qnorm)
shapiro.test(sarima.ts$residuals)


#Q3) Prediction

pred = forecast(sarima.ts, h=12, level=95)

plot(pred)


