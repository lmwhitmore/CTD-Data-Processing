## Correct Fluorometry Data
## Original Feb, 24, 2025
rm(list = ls())
library(openxlsx)

#### Load in Data ####
#Downcast with corrected oxygen
downcast = readRDS('./output/2025/downcast_oxcorr.RDS')

l = which(downcast$v2 > 1)
downcast$Station[l] ## Station 0, Station 6
downcast$v2[l] = NA
downcast$flECO.AFL[l] = NA

#Bottle with corrected oxygen
bottle = readRDS('./output/2025/bottle_oxcorr.RDS')
l = which(bottle$V2 > 1)
bottle$Station[l] ## Station 0, Station 6
bottle$V2[l] = NA
bottle$FlECO.AFL[l] = NA

plot(y = downcast$depSM, x = downcast$flECO.AFL, ylim = c(4000,0), pch = '.')
plot(y = downcast$depSM, x = downcast$flECO.AFL, ylim = c(1000,0), pch = '.')
plot(y = downcast$depSM, x = downcast$flECO.AFL, ylim = c(4000,0), xlim = c(-1,5), pch = '.')


#Measured Chlorophyll 
#"G:/Shared drives/NABOS/Data/Hydrochemistry/2023/chlorophyll_with SCN.xlsx"
#chlorophyll = read.xlsx('./chlorophyll_with SCN.xlsx')
chlorophyll = read.xlsx('../2023/aHealy 2023 Revised Chlor Final.xlsx', startRow = 3)


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

sum(diff(chlorophyll$Series) > 1, na.rm = T)
unique(chlorophyll$Series)

plot(chlorophyll$Series, pch = '.', cex = 3)

plot(x = chlorophyll$Chl, y = chlorophyll$FluorChl.up, pch = 20)
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
