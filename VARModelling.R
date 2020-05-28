### VAR med SARMA-data
library("dse")
library("vars")

### Matrix med data
vardat <- matrix(c(armadata, armadata2), ncol = 2)
colnames(vardat) <- c("pris", "forbrug")
plot.ts(vardat)

### Informationskriterier
infocrit <- VARselect(vardat, lag.max = 25, type = "none")
infocrit 

### Estimer model
varest <- VAR(vardat, p = 8, type = "none", season = NULL, exogen = NULL)
summary(varest)
resvar2 <- restrict(varest, method = "ser")
modsum <- summary(resvar2)
modsum$covres


### Residual plot. pris
plot(resvar2)
acf(resvar2$varresult$pris$residuals)
ts.plot(resvar2$varresult$pris$residuals)
qqnorm(resvar2$varresult$pris$residuals)
qqline(resvar2$varresult$pris$residuals)

### forbrug
acf(resvar2$varresult$forbrug$residuals)
ts.plot(resvar2$varresult$forbrug$residuals)
qqnorm(resvar2$varresult$forbrug$residuals)
qqline(resvar2$varresult$forbrug$residuals)

### Diagnostiske tests

### Breusch-Godfrey test - test om der er korrelation i residualer

bgtest <- serial.test(resvar2, type = "BG")
bgtest

plot(bgtest, names = "pris")
plot(bgtest, names = "forbrug")


### Test - heteroskedacity
vararch <- arch.test(resvar2, lags.multi = 5, multivariate.only = TRUE)
vararch # Afviser nulhypotese. Der er heteroskedacity


### Test - normalitet
varnorm <- normality.test(resvar2, multivariate.only = TRUE)
varnorm$jb.mul # Afviser nulhypotese

plot(resvar2)
qqPlot(resvar2$varresult$forbrug$residuals)

### Cholesky og reduceret form til strukturel
### Ligning1
A <- matrix(data = c( 3737,10543, 10543, 5076977), nrow = 2)
ch <- t(chol(A))
dd <- diag(ch)
dm <- matrix(data= c(61.13101, 0,0,2246.60468), nrow = 2)
d <- dm^2
dm1 <- matrix(data= c(1/61.13101, 0,0,1/2246.60468), nrow = 2)
l <- ch%*%dm1
linv <- matrix(data = c( 1, -2.82, 0, 1), nrow = 2)

### Ligning2
A <- matrix(data = c( 5076977,10543, 10543, 3737), nrow = 2)
ch <- t(chol(A))
dd <- diag(ch)
dm <- matrix(data= c(2253.31481, 0,0,60.95167), nrow = 2)
d <- dm^2
dm1 <- matrix(data= c(1/2253.31481, 0,0,1/60.95167), nrow = 2)
l <- ch%*%dm1
linv <- matrix(data = c( 1, -0.002076537, 0, 1), nrow = 2)
