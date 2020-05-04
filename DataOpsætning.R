setwd("C:/Users/madsu/Desktop/P6/Data")

library(readxl) #indlæs excel
library(dplyr)
library(tseries) # til tidsrækker
# Data for priser --------------------------------------------------------------------

###2020
data_p_20 <- read_excel("Priser/elspot-prices_2020_daily_dkk.xlsx", 
                        skip = 3)
###2019
data_p_19 <- read_excel("Priser/elspot-prices_2019_daily_dkk.xlsx", 
                        skip = 2)
###2018
data_p_18 <- read_excel("Priser/elspot-prices_2018_daily_dkk.xlsx", 
                        skip = 2)
###2017
data_p_17 <- read_excel("Priser/elspot-prices_2017_daily_dkk.xlsx", 
                        skip = 2)
###2016
data_p_16 <- read_excel("Priser/elspot-prices_2016_daily_dkk.xlsx", 
                        skip = 2)
###2015
data_p_15 <- read_excel("Priser/elspot-prices_2015_daily_dkk.xlsx", 
                        skip = 2)
###2014
data_p_14 <- read_excel("Priser/elspot-prices_2014_daily_dkk.xlsx", 
                        skip = 2)
###2013
data_p_13 <- read_excel("Priser/elspot-prices_2013_daily_dkk.xlsx", 
                        skip = 3)

###Dato
dato = as.Date(c(data_p_13$...1, data_p_14$...1, data_p_15$...1, data_p_16$...1, data_p_17$...1, data_p_18$...1, data_p_19$...1))
datoOOS <- as.Date(c(data_p_20$...1))

###DK1
DK1 = c(data_p_13$DK1, data_p_14$DK1, data_p_15$DK1, data_p_16$DK1, data_p_17$DK1, data_p_18$DK1, data_p_19$DK1)
DK1OOS = c(data_p_20$DK1[1:61])

###DK2 - Bruges ikke.
DK2 = c(data_p_13$DK2, data_p_14$DK2, data_p_15$DK2, data_p_16$DK2, data_p_17$DK2, data_p_18$DK2, data_p_19$DK2)

#Ser der er outlier
ts.plot(ts(DK1))

#Fjern outlier
DK1_clean = DK1
idx = which.max(DK1) #Dag 158
DK1_clean[idx] = mean(c(DK1[151:157], DK1[159:165]))

ts.plot(ts(DK1_clean)) #ingen outlier
ts.plot(ts(DK2)) #ingen outlier
DK2_clean <- DK2

# Gennemsnit - bruges ikke
dkgennemsnit <- (DK1_clean+DK2_clean)/2
ts.plot(ts(dkgennemsnit))

# Data for forbrug

###2020c
data_c_20 <- read_excel("Forbrug/consumption-dk-areas_2020_daily.xlsx",
                        skip = 2)
###2019c
data_c_19 <- read_excel("Forbrug/consumption-dk-areas_2019_daily.xlsx",
                        skip = 2)
###2018c
data_c_18 <- read_excel("Forbrug/consumption-dk-areas_2018_daily.xlsx", 
                        skip = 2)
#data_c_18 <- c(data_p_18[1:90,])
###2017c
data_c_17 <- read_excel("Forbrug/consumption-dk-areas_2017_daily.xlsx", 
                        skip = 2)
###2016c
data_c_16 <- read_excel("Forbrug/consumption-dk-areas_2016_daily.xlsx", 
                        skip = 2)
###2015c
data_c_15 <- read_excel("Forbrug/consumption-dk-areas_2015_daily.xlsx", 
                        skip = 2)
###2014c
data_c_14 <- read_excel("Forbrug/consumption-dk-areas_2014_daily.xlsx", 
                        skip = 2)
###2013c
data_c_13 <- read_excel("Forbrug/consumption-dk-areas_2013_daily.xlsx", 
                        skip = 2)

###DK1
DK1_consumption = c(data_c_13$DK1, data_c_14$DK1, data_c_15$DK1, data_c_16$DK1, data_c_17$DK1, data_c_18$DK1, data_c_19$DK1)
DK1_consumptionOOS = c(data_c_20$DK1[1:61])

###DK2 - bruges ikke.
DK2_consumption = c(data_c_13$DK2, data_c_14$DK2, data_c_15$DK2, data_c_16$DK2, data_c_17$DK2, data_c_18$DK2, data_c_19$DK2)
ts.plot(ts(DK1_consumption)[1:35])

###Samlet forburg DK - bruges ikke.
DK <- c(data_c_13$DK, data_c_14$DK, data_c_15$DK, data_c_16$DK, data_c_17$DK, data_c_18$DK, data_c_19$DK)


### Totale antal observationer
n.obs <- length(dato)
n.obs.oos <- length(datoOOS-1)

### Hverdag
wkd = rep(c(1,1,1,1,0,0,1), length.out = length(DK1_clean))
### Helligdage sættes som weekend.
wkd[c(1,    83,   87,   88,   90,   91,   116,  129,  139,  140,  359,  360,  # 2013
      366,  468,  472,  473,  475,  476,  501,  514,  524,  525,  724,  725,  # 2014
      731,  818,  822,  823,  825,  826,  851,  864,  874,  875,  1089, 1090, # 2015
      1096, 1175, 1179, 1180, 1182, 1183, 1208, 1221, 1231, 1232, 1455, 1456, # 2016
      1462, 1560, 1564, 1565, 1567, 1568, 1593, 1606, 1616, 1617, 1820, 1821, # 2017
      1827, 1910, 1914, 1915, 1917, 1918, 1943, 1956, 1966, 1967, 2185, 2186, # 2018
      2192, 2295, 2299, 2300, 2302, 2303, 2328, 2341, 2351, 2352, 2550, 2551  # 2019
)] <- 0

### Hverdag - OOS
wkd.oos = rep(c(1,1,1,0,0,1,1), length.out = length(DK1OOS))
### Helligdad
wkd.oos[c(1)] <- 0

### Samlet liste
DK1 <-list(P = ts(DK1_clean, frequency = 1),
             C = ts(DK1_consumption, frequency = 1),
             wkd = wkd) 


OOS <- list(P   = ts(DK1OOS, frequency = 1),
            C   = ts(DK1_consumptionOOS, frequency = 1),
            wkd = wkd.oos)
