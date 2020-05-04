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
acf(skp) #Ser ikke station?rt ud
acf(skf) #Der er stadig noget ugentlig s?son 


### SARMA p? s?sonkorrigeret data
sarima_model_AIC = Inf
for (p in c(0,1,2,3,4)) for (q in c(0,1,2,3,4)) for (P in c(0,1,2,3,4)) for (Q in c(0,1,2,3,4)) {
  sarima_model = tryCatch(sarima(xdata = skp, p, d=0, q, P, D=0, Q, S = 7, details = FALSE, Model = F), error = function(e) Inf)
  if (is.recursive(sarima_model) == TRUE) {
    sarima_model_AIC_now = sarima_model$AIC
  }
  if (is.recursive(sarima_model) == FALSE) {
    sarima_model_AIC_now = sarima_model
  }
  if (sarima_model_AIC_now < sarima_model_AIC) {
    sarima_model_AIC = sarima_model_AIC_now
    best_model = sarima_model
  }
}
### Bedste model if?lge AIC.
best_model$fit

### Bedste modeller if?lge AIC
sarmaskp <- sarima(xdata = skp, 3, 0, 3, 2, 0, 2, S = 7, details = FALSE, Model = F)
sarmaskf <- sarima(xdata = skf, 3, 0, 3, 3, 0, 4, S = 7, details = FALSE, Model = F)

newskp <- sarmaskp$fit$residuals
newskf <- sarmaskf$fit$residuals

### VAR med SARMA-data
library("dse")
library("vars")

### Matrix med data
vardat <- matrix(c(newskp, newskf), nrow = 2556, ncol = 2)
colnames(vardat) <- c("pris", "forbrug")
plot.ts(vardat)

### Informationskriterier
infocrit <- VARselect(vardat, lag.max = 25, type = "none")
infocrit 

### Estimer model
varest <- VAR(vardat, p = 5, type = "none", season = NULL, exogen = NULL)
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
