
library("urca")
library("tseries")
library("foreign")
library("dygraphs")
#library("apt")
#library("erer")
#install.packages("ggplot2")





## Clean up
rm(list=ls()) 
cat("\014") 


data <- read.csv2("Industry_production.csv", col.names=c("year", "month", "value"))
ts.data = ts(data$value, frequency = 12, end = c(2016,1),start = c(1993,1))
plot(ts.data, col=c("blue"))

diff.data=diff(ts.data, lag=12)
plot(diff.data)
grid()

par(new = TRUE)

diff.data_1=diff(diff.data, lag=1)
plot(diff.data_1, col=c("red"))




f12 <- rep(1/20, 20)
diff.data.lag <- filter(diff.data, f12, sides=1)
plot(diff.data.lag, col=c("blue"))






pp.test(ts.data)
pp.test(diff.data)
pp.test(diff.data_1)

adf.test(ts.data)
adf.test(diff.data)
adf.test(diff.data_1)
