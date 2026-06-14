## Correct Fluorometry Data
## Original Feb, 24, 2025
rm(list = ls())
library(openxlsx)
library(SimpleMapper)


#### Load in Data ####
#Downcast with corrected oxygen
downcast = readRDS('./data/downcast_oxcorr.RDS')

l = which(downcast$v2 > 1)
downcast$Station[l] ## Station 0, Station 6
downcast$v2[l] = NA
downcast$flECO.AFL[l] = NA

#Bottle with corrected oxygen
bottle = readRDS('./data/bottle_oxcorr.RDS')
l = which(bottle$V2 > 1)
bottle$Station[l] ## Station 0, Station 6
bottle$V2[l] = NA
bottle$FlECO.AFL[l] = NA

plot(y = downcast$depSM, x = downcast$flECO.AFL, ylim = c(4000,0), pch = '.')
plot(y = downcast$depSM, x = downcast$flECO.AFL, ylim = c(1000,0), pch = '.')
plot(y = downcast$depSM, x = downcast$flECO.AFL, ylim = c(4000,0), xlim = c(-1,5), pch = '.')
plot(y = downcast$depSM, x = downcast$v2, ylim = c(4000,0), xlim = c(-1,1), pch = '.')

l = which(downcast$StationNo >0 & downcast$StationNo < 9)

plot(y = downcast$depSM[l], x = downcast$v2[l], ylim = c(500, 0), xlim = c(-0.1,1), pch = '.')
plot(y = downcast$depSM[l], x = downcast$flECO.AFL[l], ylim = c(500, 0), xlim = c(-1,1), pch = '.')

#Measured Chlorophyll 
#"G:/Shared drives/NABOS/Data/Hydrochemistry/2023/chlorophyll_with SCN.xlsx"
#chlorophyll = read.xlsx('./chlorophyll_with SCN.xlsx')
chlorophyll = read.xlsx('../2023/aHealy 2023 Revised Chlor Final.xlsx', startRow = 3)
chlorophyll = read.xlsx('./data/Chlorophyll_LW active.xlsx', startRow = 3)


#### Match Observational Data ####
chlorophyll$FluorChl.up = NA
chlorophyll$FluorChl.up.V = NA
for (i in 1:nrow(chlorophyll)) {
  l = which(bottle$StationNo == chlorophyll$Station[i] & 
              bottle$CastNo == chlorophyll$Cast[i] &
              bottle$Bottle == chlorophyll$Niskin[i]
  )
  if (length(l) > 0) {
    chlorophyll$FluorChl.up[i] = bottle$FlECO.AFL[l]
    chlorophyll$FluorChl.up.V[i] = bottle$V2[l]
    message('here')
  }
  if (length(l) > 1) {
    message(chlorophyll$Stn[i], ' ', chlorophyll$Niskin[i], ' has more than one match.')
  }
  
}

for (stn in unique(chlorophyll$Station)) {
  l = which(chlorophyll$Station == stn)
  
  if (length(l) > 0 & all(!is.na(chlorophyll$FluorChl.up.V[l]))){
  plot(x = chlorophyll$Chl[l], y = chlorophyll$FluorChl.up.V[l], 
       xlab = 'Measured Chl', ylab = 'Fluorometer Chl V', 
       main = paste0('Station ', stn), 
       pch = 20)
  }
}


#Station 21 == great 
#Station 32 == not great 

chlorophyll$datetime = convertToDateTime(x = chlorophyll$Date)
colorpal = pals::brewer.oranges(n=20)


plot(x = chlorophyll$Chl, y = chlorophyll$FluorChl.up, 
     pch = 20, 
     col = mapColor(x = chlorophyll$Date, pal = colorpal),
     xlim = c(0,3), ylim = c(0,3))

par(mar = c(4, 4, 1, 4))
plot(x = chlorophyll$Chl, y = chlorophyll$FluorChl.up, 
     pch = 20, 
     col = mapColor(x = chlorophyll$`Fo/Fa`, pal = colorpal,xlim = c(0,5)),
     xlim = c(0,2.5), ylim = c(0,2.5),
     xlab = 'Measured Chlorophyll', ylab = 'CTD Sensor, Calculated Chlorophyll')
SimpleMapper::colorbar(colorpal, zlim = c(0,5))
mtext(text = 'z = Fo/Fa', side = 2, line = -23)

l = which(chlorophyll$`Fo/Fa` < 2)

trend = lm(chlorophyll$FluorChl.up[l] ~ chlorophyll$Chl[l])
summary(trend)
par(mar = c(4, 4, 1, 4))
plot(x = chlorophyll$Chl[l], y = chlorophyll$FluorChl.up[l], 
     pch = 20, 
     xlim = c(0,2.5), ylim = c(0,2.5),
     xlab = 'Measured Chlorophyll', ylab = 'CTD Sensor, Calculated Chlorophyll')
abline(a = trend$coefficients)
mtext(text = 'slope = 1.8, r2 = 0.37', side = 1, line = -2)
#slope = 1.8
#intercept = 0.006 
#r2 = 0.37

par(mar = c(4, 4, 1, 4))
plot(x = chlorophyll$Phaeo, y = chlorophyll$FluorChl.up, 
     pch = 20, 
     col = mapColor(x = chlorophyll$Station, pal = colorpal,xlim = c(0,45)),
     xlim = c(0,5), ylim = c(0,2.5),
     xlab = 'Measured Phaeo', ylab = 'CTD Sensor, Calculated Chlorophyll')
SimpleMapper::colorbar(colorpal, zlim = c(0,45))
mtext(text = 'z = Station Number', side = 2, line = -23)

colorpal = pals::brewer.qualbin(n=45)
trend_phaeo = lm(chlorophyll$FluorChl.up[l] ~ chlorophyll$Phaeo[l])
summary(trend_phaeo)

par(mar = c(4, 4, 1, 4))
plot(x = chlorophyll$Phaeo[l], y = chlorophyll$FluorChl.up[l], 
     pch = 20, 
     xlim = c(0,5), ylim = c(0,2.5),
     xlab = 'Measured Phaeo', ylab = 'CTD Sensor, Calculated Chlorophyll')
abline(a = trend_phaeo$coefficients)
mtext(text = 'slope = 0.27, r2 = 0.45', side = 3, line = -2)
#slope = 0.27
#intercept = -0.07 
#r2 = 0.45

par(mfrow = c(2,1))
plot(x = chlorophyll$Station, y = as.numeric(chlorophyll$`Chl/Phaeo`), 
     xlab = 'Station', ylab = "Chl/Phaeo", 
     pch = 16,
     ylim = c(-4,4))
plot(x = chlorophyll$Station, y = 1/as.numeric(chlorophyll$`Chl/Phaeo`), 
     xlab = 'Station', ylab = "Phaeo/Chl",
     pch = 16)

plot(x = chlorophyll$Chl, y = chlorophyll$FluorChl.up.V, pch = 20)

plot(x = chlorophyll$Chl, y = chlorophyll$FluorChl.up, pch = 20,
     ylim = c(0, 1))
plot(x = chlorophyll$Chl, y = chlorophyll$FluorChl.up.V, pch = 20,
     ylim = c(0, 0.2))
plot(x = chlorophyll$Chl + chlorophyll$Phaeo, y = chlorophyll$FluorChl.up.V, pch = 20,
     ylim = c(0, 0.2))

abline(lm(chlorophyll$FluorChl.up.V ~ chlorophyll$Chl))

plot(x = sort(chlorophyll$Chl, na.last = T), y = sort(chlorophyll$FluorChl.up.V, na.last = T), pch = 20,
     ylim = c(0, 0.2))
plot(x = sort(chlorophyll$Chl, na.last = T), y = sort(chlorophyll$FluorChl.up, na.last = T), pch = 20,
     ylim = c(0, 3), xlim = c(0,3))


plot(x = downcast$flECO.AFL, y = downcast$depSM, ylim = c(500,0))



####NEEDS UPDATE 

bottle$Oxygen.Meas = NA
for (i in 1:nrow(bottle)) {
  l = which(samples$Stn == bottle$StationNo[i] & 
              samples$Cast == bottle$CastNo[i] &
              samples$Niskin == bottle$Bottle[i] & 
              !is.na(samples$Oxygen))
  
  if (length(l) > 0 ) {
    bottle$Oxygen.Meas[i] = samples$Oxygen[l]
    message('here')
  }
  if (length(l) > 1) {
    message(bottle$Station[i], ' ', bottle$Bottle[i], ' has more than one match.')
  }
  
}

chlorophyll$FluorChl.down = NA
chlorophyll$FluorChl.down.V = NA
for (i in 1:nrow(chlorophyll)) {
  l = which(downcast$StationNo == chlorophyll$Station[i] & 
              round(downcast$depSM) == round(chlorophyll$Depth)[i]
  )
  if (length(l) > 0) {
    chlorophyll$FluorChl.down[i] = downcast$flECO.AFL[l]
    chlorophyll$FluorChl.down.V[i] = downcast$v2[l]
    message('here')
  }
  if (length(l) > 1) {
    message(chlorophyll$Stn[i], ' ', chlorophyll$Depth[i], ' has more than one match.')
  }
  
}


plot(x = chlorophyll$Chl, y = chlorophyll$FluorChl.down)
