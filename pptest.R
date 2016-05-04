library("tseries")
x <- rnorm(1000)  # no unit-root
pp.test(x)

y <- cumsum(x)  # has unit root
pp.test(y)  #Plus DF est faible, mieux c'est 


x <- rnorm(1000)  # no unit-root
adf.test(x)

y <- diffinv(x)   # contains a unit-root
adf.test(y)