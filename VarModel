library("dse")
library("vars")

### Matrix med data
vardat <- matrix(c(skp, skf), nrow = 2556, ncol = 2)
colnames(vardat) <- c("pris", "forbrug")
plot.ts(vardat)

### Informationskriterier
infocrit <- VARselect(vardat, lag.max = 25, type = "none")
infocrit 

### Estimer model
varest <- VAR(vardat, p = 7, type = "none", season = NULL, exogen = NULL)
summary(varest)

### Tjekker r?dder
roots <- roots(varest)
roots # Alle mindre end 1


### Diagnostiske tests

### Portmanteau-test - test om der er korrelation i residualer

varport <- serial.test(varest, lags.pt = 16, type = "PT.asymptotic")
varport # Afviser nulhypotese

plot(varport, names = "pris")
plot(varport, names = "forbrug")


### Test - heteroskedacity
vararch <- arch.test(varest, lags.multi = 5, multivariate.only = TRUE)
vararch # Afviser nulhypotese


### Test - normalitet
varnorm <- normality.test(varest, multivariate.only = TRUE)
varnorm$jb.mul # Afviser nulhypotese
