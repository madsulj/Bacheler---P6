library(forecast)
library(stats)
library(astsa)
library(fracdiff)
library(car)
library(FitAR)
library(tseries)

### ACF af pris og forburg
acf(DK1$P)
acf(DK1$C)

### S?sonkorrigering af pris
y_t = c(DK1$P,OOS$P[1:7])
t = 1:length(y_t)
wkd <- c(wkd,wkd.oos[1:7])

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
plot(c(dato,datoOOS[1:7]) ,skp, main = 'Fjernet s?son', type = "l", xlab = "Dato", ylab = "Pris i DKK")

par(mfrow = c(1,1))
### Deterministisk s?son
plot(c(dato,datoOOS[1:7]),y_t, type = "l", main = "S?son ved pris", xlab = "Dato", ylab = "Pris i DKK")
lines(c(dato,datoOOS[1:7]), fitted(spris), lwd = 2, col = "dark blue")


### S?sonkorrigering af forbrug
y_t = c(DK1$C,OOS$C[1:7])
t = 1:length(y_t)

### Model

sforbrug <- lm(y_t ~ t + I(t^2)+s1 + s2 + s3 + s4 + wkd)
summary(sforbrug)

### S?sonkorrigeret
skf <- y_t-fitted(sforbrug)
plot(c(dato,datoOOS[1:7]) ,skf, main = 'Fjernet s?son', type = "l", xlab = "Dato", ylab = "Forbrug i MWh")

par(mfrow = c(1,1))
### Deterministisk s?son
plot(c(dato,datoOOS[1:7]),y_t, type = "l", main = "S?son ved forbrug", xlab = "Dato", ylab = "forbrug i MWh")
lines(c(dato,datoOOS[1:7]), fitted(sforbrug), lwd = 2, col = "dark blue")

### ADF test
adf.test(skp) #Afviser nulhypotese
adf.test(skf) #Afviser nulhypotese

### ACF
acf(skp) #Ser ikke station?rt ud
acf(skf) #Der er stadig noget ugentlig s?son 


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
  return(stats::filter(x,c(polycoeff),sides=1, method="convolution"))
}


polyinvers <- function (x, a, maxlag = 50)
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
  return(stats::filter(x,c(polycoeff),sides=1, method="convolution"))
}

acf2(skp)
acf2(skf)

### Bedste modeller if?lge AIC
sarmaskp <- sarima(xdata = skp, 7, 0, 1, 0, 1, 1, S = 7, no.constant = TRUE)
sarmaskf <- sarima(xdata = skf, 7, 0, 1, 0, 1, 1, S = 7, no.constant = TRUE)

### Pris Sarima
sarmaskp$ttable
plot(sarmaskp$fit$residuals, type="l")
acf2(sarmaskp$fit$residuals)

koefs<- sarmaskp$fit$coef
print(koefs)

koefs_ar <- koefs[1:7]
koefs_ma <- koefs[8]
koefs_sma <- koefs[9]

#koefs_saeson_Phi <- c(1,rep(0,6),-koefs_sar)
koefs_saeson_Theta1 <- c(1,rep(0,6),koefs_sma)


armadata <- polyinvers(diff(skp,7),koefs_saeson_Theta1)
acf2(armadata)
armadata11 <- armadata
armadata <- armadata[50:2556]
acf(armadata)
plot(dato[50:2556] ,armadata, main = 'Fjernet s?son', type = "l", xlab = "Dato", ylab = "Pris DKK")
adf.test(armadata)

### Forbrug Sarima
sarmaskf$ttable
plot(sarmaskf$fit$residuals, type="l")
acf2(sarmaskf$fit$residuals)

koefs<- sarmaskf$fit$coef
print(koefs)

koefs_ar <- koefs[1:7]
koefs_ma <- koefs[8]
koefs_sma <- koefs[9]

#koefs_saeson_Phi <- c(1,rep(0,6),-koefs_sar)
koefs_saeson_Theta2 <- c(1,rep(0,6),koefs_sma)


armadata2 <- polyinvers(diff(skf,7),koefs_saeson_Theta2)
acf2(armadata2)
armadata22 <- armadata2
armadata2 <- armadata2[50:2556]
acf(armadata2)
plot(dato[50:2556] ,armadata2, main = 'Fjernet s?son', type = "l", xlab = "Dato", ylab = "Forbrug i MWh")
adf.test(armadata2)
