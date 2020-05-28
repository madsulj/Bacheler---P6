### Impulse response function
### Impuls pris
oir <- irf(resvar2, impulse = "pris", response = "forbrug", n.ahead = 30, ortho = TRUE, cumulative = FALSE, boot = TRUE, runs = 1000, seed = 12345)
plot(oir)

### Impuls forbrug
oir2 <- irf(resvar2, impulse = "forbrug", response = "pris", n.ahead = 30, ortho = TRUE, cumulative = FALSE, boot = TRUE, runs = 1000, seed = 12345)
plot(oir2)
plot(irf(resvar2, impulse = "forbrug", response = "pris", n.ahead = 30, ortho = TRUE, cumulative = FALSE, boot = TRUE))

### Samlet
oir3 <- irf(resvar2, n.ahead = 30, ortho = TRUE, cumulative = FALSE, boot = TRUE, runs = 1000, seed = 12345)
ts.plot(oir3$irf$forbrug)

### Forberende arbejde til forecast
### Trend 2020
y_t <- c(DK1$P[2550:2556],OOS$P)
t.oos  <- 1:(length(datoOOS)+6)
wkd.oos <- c(wkd[2550:2556],wkd.oos)

s1.oos = cos(2 * pi * omega_year * t.oos)
s2.oos = sin(2 * pi * omega_year * t.oos)
s3.oos = cos(4 * pi * omega_year * t.oos)
s4.oos = sin(4 * pi * omega_year * t.oos)
s5.oos = cos(8 * pi * omega_year * t.oos)
s6.oos = sin(8 * pi * omega_year * t.oos)
s7.oos = cos(24 * pi* omega_year * t.oos)
s8.oos = sin(24 * pi * omega_year * t.oos)

ooslmp <- lm(y_t ~ t.oos + I(t.oos^2) + 
               s1.oos + s2.oos + s3.oos + s4.oos + wkd.oos)
summary(ooslmp)
oosp <- residuals(ooslmp)[8:68]

df.oos <- data.frame(wkd = wkd.oos, t = t.oos, 
                     s1 =cos(2 * pi * omega_year * t.oos), s2 =sin(2 * pi * omega_year * t.oos),
                     s3 =cos(4 * pi * omega_year * t.oos), s4 =sin(4 * pi * omega_year * t.oos))

p.pred.oos <- predict(spris, newdata = df.oos)
ts.plot(p.pred.oos)

### forbrug

y_t <- c(DK1$C[2550:2556],OOS$C)

ooslmc <- lm(y_t ~ t.oos + I(t.oos^2) + 
               s1.oos + s2.oos + s3.oos + s4.oos + wkd.oos)
summary(ooslmc)
oosc <- residuals(ooslmc)[8:68]

c.pred.oos <- predict(sforbrug, newdata = df.oos)


### forecast VAR med skp og skf

dat    <- matrix(c(skp, oosp, skf, oosc), ncol = 2)
oosdat <- ts(matrix(c(OOS$P, OOS$C), nrow = 61, ncol = 2)) 
trend20 <- ts(matrix(c(p.pred.oos[8:68], c.pred.oos[8:68]), ncol = 2))

### Forecast med ARMAdata

skpoos <- c(DK1$P[2550:2556],OOS$P)-p.pred.oos
skfoos <- c(DK1$C[2550:2556],OOS$C)-c.pred.oos
sarmaskpoos <- sarima(xdata = skpoos, 7, 0, 1, 0, 1, 1, S = 7, no.constant = TRUE)
sarmaskfoos <- sarima(xdata = skfoos, 7, 0, 1, 0, 1, 1, S = 7, no.constant = TRUE)

### Pris
koefsoos<- sarmaskpoos$fit$coef
print(koefsoos)

koefs_aroos <- koefsoos[1:7]
koefs_maoos <- koefsoos[8]
koefs_smaoos <- koefsoos[9]

koefs_saeson_Thetaoos <- c(1,rep(0,6),koefs_smaoos)
armadataoos <- polyinvers(diff(skpoos,7),koefs_saeson_Thetaoos, maxlag = 7)[8:61]
acf2(armadataoos)

### Forbrug

koefsoos<- sarmaskfoos$fit$coef
print(koefsoos)

koefs_aroos <- koefsoos[1:7]
koefs_maoos <- koefsoos[8]
koefs_smaoos <- koefsoos[9]

koefs_saeson_Thetaoos <- c(1,rep(0,6),koefs_smaoos)
armadataoos2 <- polyinvers(diff(skfoos,7),koefs_saeson_Thetaoos, maxlag = 7)[8:61]
acf2(armadataoos2)

dat <- matrix(c(armadata, armadataoos, armadata2, armadataoos2), ncol = 2)
plot.ts(dat)


### Forecast funktion
{forecast_VARroll<- function(x){
  
  start <- length(armadata)
  data <- ts(dat)
  forecast_step <- c()
  real <- c()
  cint <- c()
  for (i in 1:x) {
    
    fitvar <- VAR(data[1:(start+i-1),], p = 8, type = "none", season = NULL, exogen = NULL)
    
    resvar <- restrict(fitvar, method = "ser")
    
    fore <- predict(resvar,n.ahead=1)
    
    foreboth <- matrix(c(fore$fcst$Series.1[1],fore$fcst$Series.2[1]), nrow = 1, ncol = 2)
    
    forecast_step <- as.ts(rbind(forecast_step,foreboth))
    
    inter <- matrix(c(fore$fcst$Series.1[4],fore$fcst$Series.2[4]), nrow = 1, ncol = 2)
    
    cint <- as.ts(rbind(cint,inter))
    
}
  colnames(forecast_step) <- c("Pris","Forbrug")
forecast_step <- forecast_step
forecaststeppris <- polymult(forecast_step[,1],koefs_saeson_Theta1, 1)
forecaststepforbrug <- polymult(forecast_step[,2],koefs_saeson_Theta2, 1)
cint <- cint

for (i in 1:x)  {
    pris <- oosp[i] + forecaststeppris[i]
    forbrug <- oosc[i]+forecaststepforbrug[i]
    pf <- matrix(c(pris,forbrug), ncol = 2)
    real <- as.ts(rbind(real,pf))}

real <- real


if (i==x) {
  forecast_var <- (real+trend20)
    coint1 <- (forecast_var-(cint/2))
    coint2 <- (forecast_var+(cint/2))
}
varforeres <- matrix(c(forecast_var,coint1,coint2),ncol = 6)
  return(varforeres)

}
}

varfore <- forecast_VARroll(55)


### Intervaller og forecast - pris
forecastpris <- c()
forecastpris[2564:2611] <- varfore[8:55,1]
fcprisintl <- c()
fcprisintl[2564:2611] <- varfore[8:55,3]
fcprisintu <- c()
fcprisintu[2564:2611] <- varfore[8:55,5]

### QQplot forecast pris
ggplot()+geom_line(aes(x = c(dato[2350:2556],datoOOS[1:55]),
                       y =c(DK1$P[2350:2556],oosdat[1:55,1])), size = 1.0)+
  geom_line(aes(x = c(dato[2350:2556],datoOOS[1:55]),
                 y = forecastpris[2350:2611], col="Forecasts"),size = 1.0)+
  geom_line(aes(x=c(dato[2350:2556],datoOOS[1:55]), y=fcprisintl[2350:2611], col="95% Prediction interval"), size = 1) +
  geom_line(aes(x=c(dato[2350:2556],datoOOS[1:55]), y=fcprisintu[2350:2611], col="95% Prediction interval"), size = 1) +
  geom_ribbon(aes(x =c(dato[2350:2556],datoOOS[1:55]), ymin = fcprisintl[2350:2611], ymax = fcprisintu[2350:2611]), fill = "gray",
              alpha = .3)+xlab("Days") + ylab("Pris i DKK/MWh") +
  scale_color_manual(values = c("Real price" = "firebrick3", "Forecasts" = "red",
                                "95% Prediction interval" = "gray70")) + theme(legend.position="bottom") +
  theme(legend.title=element_blank())

### Intervaller og forecast - forbrug

forecastforbrug <- c()
forecastforbrug[2564:2611] <- varfore[8:55,2]
fcfbintl <- c()
fcfbintl[2564:2611] <- varfore[8:55,4]
fcfbintu <- c()
fcfbintu[2564:2611] <- varfore[8:55,6]

### QQplot forecast forbrug

ggplot()+geom_line(aes(x = c(dato[2350:2556],datoOOS[1:55]),
                       y =c(DK1$C[2350:2556],oosdat[1:55,2])), size = 1.0)+
  geom_line(aes(x = c(dato[2350:2556],datoOOS[1:55]),
                y = forecastforbrug[2350:2611], col="Forecasts"),size = 1.0)+
  geom_line(aes(x=c(dato[2350:2556],datoOOS[1:55]), y=fcfbintl[2350:2611], col="95% Prediction interval"), size = 1) +
  geom_line(aes(x=c(dato[2350:2556],datoOOS[1:55]), y=fcfbintu[2350:2611], col="95% Prediction interval"), size = 1) +
  geom_ribbon(aes(x =c(dato[2350:2556],datoOOS[1:55]), ymin = fcfbintl[2350:2611], ymax = fcfbintu[2350:2611]), fill = "gray",
              alpha = .3)+xlab("Days") + ylab("Forbrug i MWh") +
  scale_color_manual(values = c("Real price" = "firebrick3", "Forecasts" = "red",
                                "95% Prediction interval" = "gray70")) + theme(legend.position="bottom") +
  theme(legend.title=element_blank())



###RMSE
### Pris
rmsep <- sqrt(1/(47)*sum((forecastpris[2564:2611] -
                            OOS$P[8:55])^2))
rmsepris <- rmsep/mean(OOS$P[8:55])
rmsepris*100

### forburg
rmsec <- sqrt(1/(47)*sum((forecastforbrug[2564:2611] -
                                                   OOS$C[8:55])^2))
rmseforbrug <- rmsec/mean(OOS$C[8:55])
rmseforbrug*100

