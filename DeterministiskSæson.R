library(forecast)
library(stats)
library(astsa)
library(fracdiff)
library(car)
library(FitAR)
library(tseries)


### Sæsonkorrigering af pris
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

### Sæsonkorrigeret
skp <- y_t-fitted(spris)
plot(dato ,skp, main = 'Fjernet sæson', type = "l", xlab = "Dato", ylab = "Pris i DKK")

par(mfrow = c(1,1))
### Deterministisk sæson
plot(dato,y_t, type = "l", main = "Sæson ved pris", xlab = "Dato", ylab = "Pris i DKK")
lines(dato, fitted(spris), lwd = 2, col = "dark blue")


### Sæsonkorrigering af forbrug
y_t = DK1$C
t = 1:length(y_t)

### Model

sforbrug <- lm(y_t ~ t + I(t^2)+s1 + s2 + s3 + s4 + wkd)
summary(sforbrug)

### Sæsonkorrigeret
skf <- y_t-fitted(sforbrug)
plot(dato ,skf, main = 'Fjernet sæson', type = "l", xlab = "Dato", ylab = "Forbrug i MWh")

par(mfrow = c(1,1))
### Deterministisk sæson
plot(dato,y_t, type = "l", main = "Sæson ved forbrug", xlab = "Dato", ylab = "forbrug i MWh")
lines(dato, fitted(sforbrug), lwd = 2, col = "dark blue")

### ADF test
adf.test(skp) #Afviser nulhypotese
adf.test(skf) #Afviser nulhypotese

### ACF
acf(skp) #Ser ikke stationært ud
acf(skf) #Der er stadig noget ugentlig sæson 


### SARMA på sæsonkorrigeret data
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
### Bedste model ifølge AIC.
best_model$fit

### Bedste modeller ifølge AIC
sarmaskp <- sarima(xdata = skp, 3, 0, 3, 2, 0, 2, S = 7, details = FALSE, Model = F)
sarmaskf <- sarima(xdata = skf, 3, 0, 3, 3, 0, 4, S = 7, details = FALSE, Model = F)