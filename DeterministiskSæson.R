library(forecast)
library(stats)
library(astsa)
library(fracdiff)
library(car)
library(FitAR)
library(tseries)


### S?sonkorrigering af pris
y_t = DK1$P
t = 1:length(y_t)

### Frekvens
omega_year = 1/365

### Sinus
s1 = cos(2 * pi * omega_year * t)
s2 = sin(2 * pi * omega_year * t)
s3 = cos(4 * pi * omega_year * t)
s4 = sin(4 * pi * omega_year * t)
s5 = cos(8 * pi * omega_year * t)
s6 = sin(8 * pi * omega_year * t)
s7 = cos(24 * pi* omega_year * t)
s8 = sin(24 * pi * omega_year * t)

### Model
spris <- lm(y_t ~ t + I(t^2) + s1 + s2 + s3 + s4 + wkd)
summary(spris)

### S?sonkorrigeret
skp <- y_t-fitted(spris)
plot(dato ,skp, main = 'Fjernet s?son', type = "l", xlab = "Dato", ylab = "Pris i DKK")

par(mfrow = c(1,1))
### Deterministisk s?son
plot(dato,y_t, type = "l", main = "S?son ved pris", xlab = "Dato", ylab = "Pris i DKK")
lines(dato, fitted(spris), lwd = 2, col = "dark blue")


### S?sonkorrigering af forbrug
y_t = DK1$C
t = 1:length(y_t)

### Model

sforbrug <- lm(y_t ~ t + I(t^2)+s1 + s2 + s3 + s4 + wkd)
summary(sforbrug)

### S?sonkorrigeret
skf <- y_t-fitted(sforbrug)
plot(dato ,skf, main = 'Fjernet s?son', type = "l", xlab = "Dato", ylab = "Forbrug i MWh")

par(mfrow = c(1,1))
### Deterministisk s?son
plot(dato,y_t, type = "l", main = "S?son ved forbrug", xlab = "Dato", ylab = "forbrug i MWh")
lines(dato, fitted(sforbrug), lwd = 2, col = "dark blue")

### ADF test
adf.test(skp) #Afviser nulhypotese
adf.test(skf) #Afviser nulhypotese

### ACF
acf2(skp) #Ser ikke station?rt ud
acf2(skf) #Der er stadig noget ugentlig s?son 


## SARMA p? s?sonkorrigeret data
# sarima_model_AIC = Inf
# for (p in c(0,1,2,3,4)) for (d in c(0,1)) for (q in c(0,1,2,3,4)) for (P in c(0,1,2,3,4)) for (D in c(0,1)) for (Q in c(0,1,2,3,4)) {
#   sarima_model = tryCatch(sarima(xdata = skf, p, d, q, P, D, Q, S = 7, details = FALSE, Model = F), error = function(e) Inf)
#   if (is.recursive(sarima_model) == TRUE) {
#     sarima_model_AIC_now = sarima_model$AIC
#   }
#   if (is.recursive(sarima_model) == FALSE) {
#     sarima_model_AIC_now = sarima_model
#   }
#   if (sarima_model_AIC_now < sarima_model_AIC) {
#     sarima_model_AIC = sarima_model_AIC_now
#     best_model = sarima_model
#   }
# }
## Bedste model if?lge AIC.
 # best_model$fit


unloadNamespace("dplyr")

polymult <- function(x,a,b)
  # a(B) = a_0 + a_1*B + a_2*B² + ... + a_p*B^p
  # b(B) = b_0 + b_1*B + b_2*B² + ... + a_q*B^q
  # where wlog p >= q
  # Result is a(B)b(B) x_t
{
  if (length(b) == 1){polycoeff <- b[1]*array(a)}
  else
  {alpha <- array(a)
  p <- dim(alpha) - 1
  beta <- array(b)
  q <- dim(beta) - 1
  # Assumed: p >= q
  polycoeff <- array(rep(0,p+q + 1))
  for (k in 0:(p+q))
    for (j in max(0,k-q):min(k,p))
    {polycoeff[k+1] <- polycoeff[k+1] + alpha[j+1]*beta[k-j+1]}
  }
  return(filter(x,c(polycoeff),sides=1, method="convolution"))
}


polyinvers <- function (x, a, maxlag = 30)
  # a(B) = a_0 + a_1*B + a_2*B² + ... + a_p*B^p
  # Result is (1/a(B)) x_t
{
  phi <- array(a)
  p <- dim(phi) - 1 # assumed >= 1
  polycoeff <- array(rep(0,maxlag))
  polycoeff[1] <- 1/phi[1]
  for (k in 1:(maxlag - 1))
    for (j in (1:min(p,k)))
    {polycoeff[k+1] <- polycoeff[k+1] - phi[j+1]*polycoeff[k-j+1]/phi[1]}
  return(filter(x,c(polycoeff),sides=1, method="convolution"))
}



### Bedste modeller if?lge AIC
sarmaskp <- sarima(xdata = skp, 2, 0, 3, 0, 1, 2, S = 7, no.constant = TRUE)
sarmaskf <- sarima(xdata = skf, 3, 0, 3, 3, 1, 3, S = 7, no.constant = TRUE)

### Pris Sarima
sarmaskp$ttable
plot(sarmaskp$fit$residuals, type="l")
acf2(sarmaskp$fit$residuals)

koefs<- sarmaskp$fit$coef
print(koefs)

koefs_ar <- koefs[1:2]
koefs_ma <- koefs[3:5]
koefs_sar <- 0
koefs_sma <- koefs[6:7]

koefs_saeson_Phi <- c(1,rep(0,6),-koefs_sar)
koefs_saeson_Theta <- c(1,rep(0,5),koefs_sma)

mellemregning <- polymult(diff(skp,7),koefs_saeson_Phi,1)

armadata <- polyinvers(mellemregning,koefs_saeson_Theta)
acf2(armadata)


### Forbrug Sarima
sarmaskf$ttable
plot(sarmaskf$fit$residuals, type="l")
acf2(sarmaskf$fit$residuals)

koefs<- sarmaskf$fit$coef
print(koefs)

koefs_ar <- koefs[1:3]
koefs_ma <- koefs[4:6]
koefs_sar <- koefs[7:9]
koefs_sma <- koefs[10:12]

koefs_saeson_Phi <- c(1,rep(0,4),-koefs_sar)
koefs_saeson_Theta <- c(1,rep(0,4),koefs_sma)

mellemregning <- polymult(diff(skf,7),koefs_saeson_Phi,1)


armadata2 <- polyinvers(mellemregning,koefs_saeson_Theta)
acf2(armadata2)







### VAR med SARMA-data
library("dse")
library("vars")

### Matrix med data
vardat <- matrix(c(armadata[365:2191], armadata2[365:2191]), nrow = 1827, ncol = 2)
colnames(vardat) <- c("pris", "forbrug")
plot.ts(vardat)

### Informationskriterier
infocrit <- VARselect(vardat, lag.max = 25, type = "none")
infocrit 

### Estimer model
varest <- VAR(vardat, p = 7, type = "none", season = NULL, exogen = NULL)
summary(varest)

### Diagnostiske tests

### Portmanteau-test - test om der er korrelation i residualer

varport <- serial.test(varest, lags.pt = 16, type = "PT.asymptotic")
varport

plot(varport, names = "pris")
plot(varport, names = "forbrug")


### Test - heteroskedacity
vararch <- arch.test(varest, lags.multi = 5, multivariate.only = TRUE)
vararch # Afviser nulhypotese


### Test - normalitet
varnorm <- normality.test(varest, multivariate.only = TRUE)
varnorm$jb.mul # Afviser nulhypotese
