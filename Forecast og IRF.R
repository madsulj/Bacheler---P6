### Impulse response function
### Samlet
oir <- irf(varest, impulse = "pris", response = "forbrug", n.ahead = 30, ortho = TRUE, cumulative = FALSE, boot = TRUE, runs = 1000, seed = 12345)
plot(oir)

### Impuls forbrug
oir2 <- irf(varest, impulse = "forbrug", response = "pris", n.ahead = 30, ortho = TRUE, cumulative = FALSE, boot = TRUE, runs = 1000, seed = 12345)
plot(oir2)

### Impuls Pris
oir3 <- irf(varest, n.ahead = 30, ortho = TRUE, cumulative = FALSE, boot = TRUE, runs = 1000, seed = 12345)
plot(oir3)

### Trend 2020
y_t <- OOS$P
t.oos  <- 1:(length(datoOOS)-1)

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
ts.plot(ooslmp$residuals)
oosp <- residuals(ooslmp)

df.oos <- data.frame(wkd = wkd.oos, t = t.oos, 
                     s1 =cos(2 * pi * omega_year * t.oos), s2 =sin(2 * pi * omega_year * t.oos),
                     s3 =cos(4 * pi * omega_year * t.oos), s4 =sin(4 * pi * omega_year * t.oos))

p.pred.oos <- predict(spris, newdata = df.oos)
ts.plot(p.pred.oos)

### forbrug

y_t <- OOS$C

ooslmc <- lm(y_t ~ t.oos + I(t.oos^2) + 
               s1.oos + s2.oos + s3.oos + s4.oos + wkd.oos)
summary(ooslmc)
ts.plot(ooslmc$residuals)
oosc <- residuals(ooslmc)

c.pred.oos <- predict(sforbrug, newdata = df.oos)
ts.plot(c.pred.oos)


### forecast med var

dat    <- matrix(c(skp, oosp, skf, oosc), nrow = 2617, ncol = 2)
oosdat <- ts(matrix(c(OOS$P, OOS$C), nrow = 61, ncol = 2)) 
trend20 <- ts(matrix(c(p.pred.oos, c.pred.oos), ncol = 2))


### Forecast funktion
{forecast_VARroll<- function(x){
  
  start <- length(skp)
  data <- ts(dat)
  forecast_step <- c()
  
  for (i in 1:x) {
    
    fitvar <- VAR(data[1:(start+i-1),], p = 8, type = "none", season = NULL, exogen = NULL)
    
    fore <- predict(fitvar,n.ahead=1)
    
    foreboth <- matrix(c(fore$fcst$Series.1[1],fore$fcst$Series.2[1]), nrow = 1, ncol = 2)
    
    forecast_step <- as.ts(rbind(forecast_step,foreboth))
}
  colnames(forecast_step) <- c("Pris","Forbrug")
forecast_step <- forecast_step

if (i==x) {
  forecast_var <- (forecast_step+trend20)
   plot(oosdat[1:61],type = "l",col="red")
   lines(forecast_var[1:61])
  # plot(oosdat[1:61,2],type = "l",col="red")
  # lines(forecast_var[1:61,2])
}
  return(forecast_var)
}
}


forecast_VARroll(61)

