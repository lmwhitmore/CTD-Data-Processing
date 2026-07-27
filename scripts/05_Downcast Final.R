## Generate Cruise "Hydrochemistry" File 
## Updates: L.Whitmore June 19, 2026

rm(list = ls())
library(openxlsx)
library(dplyr)
source('scripts/source_NABOS.R')

## Import Intermediate Downcast File
downcast = readRDS('./output/intermediate stage/NABOS2025_20260721_INTERMEDIATE_Downcast.RDS')

## Generate Data Frame

final = data.frame(Cruise = 'NABOS2025', 
                   Event_Key = NA,
                   Datetime = downcast$datetime,
                   Station = downcast$Station, 
                   Cast = downcast$Cast,
                   Longitude = downcast$longitude,
                   Latitude = downcast$latitude,
                   Depth = downcast$depSM,
                   Pressure = downcast$prDM,
                   CTD.Conductivity = downcast$c0mS.cm,
                   CTD.Conductivity.Flag = 2,
                   CTD.Salinity = downcast$sal00,
                   CTD.Salinity.Flag = 2,
                   CTD.Temperature = downcast$t090C,
                   CTD.Temperature.Flag = 2,
                   CTD.Oxygen = downcast$sbox0Mm.Kg,
                   CTD.Oxygen.Corr = downcast$Oxygen.Corr,
                   CTD.Oxygen.Flag = 2,
                   CTD.ChlFluor.V = downcast$v2,
                   CTD.ChlFluor.Unit = downcast$flECO.AFL,
                   CTD.ChlFluor.Flag = NA,
                   CTD.CDOMFluor.V = downcast$v6,
                   CTD.CDOMFluor.Unit = downcast$wetCDOM, 
                   CTD.CDOM.Flag = NA,
                   CTD.Transmissometry.V = downcast$v3,
                   CTD.Transmissometry.Unit = downcast$CStarAt0,
                   CTD.Transmissometry.Flag = NA,
                   CTD.Altimeter = downcast$altM,
                   Elapsed.Time = downcast$timeM,
                   Scan.Count = downcast$scan
                   )


#### Add Event details 
## import event log
event = read.xlsx('~/Documents/Tasks/07 Data Submission Prep/NABOS/Event Log/ShipLogger 2025-10-05 151146.xlsx')

event = event[event$instrument == 'CTD Rosette',]
event$cast[event$station == 4] = 1

event$cast[event$group_id == '01K4TKQ5TKX4CF8T0GQXWV014Y'] = 1 #stn 12
event$cast[event$group_id == '01K50TX8PQS933N20K4S8NSFFS'] = 1 #stn 3
event$cast = as.numeric(event$cast)

event$station = as.numeric(event$station)

final$Cast[final$Station == 7 & final$Cast == 2] = 4


for (i in 1:nrow(final)) {
  l = which(final$Station[i] == event$station & final$Cast[i] == event$cast)
  final$Event_Key[i] = event$group_id[l[1]]
  
}

## Plots for Station-by station QA
for (stn in unique(final$Station)) {
  l = which(final$Station == stn)
  
  if (max(final$Depth[l]) < 500) {
    par(mfrow = c(1,4))
  plot(x = final$CTD.Temperature[l], y = final$Depth[l], ylim = c(max(final$Depth[l]),0), pch = '.', cex = 3)
  mtext(paste0('Station ', stn), side = 3)
  plot(x = final$CTD.Conductivity[l], y = final$Depth[l], ylim = c(max(final$Depth[l]),0), pch = '.', cex = 3)
  plot(x = final$CTD.Salinity[l], y = final$Depth[l], ylim = c(max(final$Depth[l]),0), pch = '.', cex = 3)
  plot(x = final$CTD.Oxygen.Corr[l], y = final$Depth[l], ylim = c(max(final$Depth[l]),0), pch = '.', cex = 3)
  }
  
  if (max(final$Depth[l]) > 500) {
  par(mfrow = c(2,4))
  plot(x = final$CTD.Temperature[l], y = final$Depth[l], ylim = c(500,0), pch = '.', cex = 3)
  mtext(paste0('Station ', stn), side = 3)
  plot(x = final$CTD.Conductivity[l], y = final$Depth[l], ylim = c(500,0), pch = '.', cex = 3)
  plot(x = final$CTD.Salinity[l], y = final$Depth[l], ylim = c(500,0), pch = '.', cex = 3)
  plot(x = final$CTD.Oxygen.Corr[l], y = final$Depth[l], ylim = c(500,0), pch = '.', cex = 3)
  
  plot(x = final$CTD.Temperature[l], y = final$Depth[l], ylim = c(max(final$Depth[l]),501), pch = '.', cex = 3)
  plot(x = final$CTD.Conductivity[l], y = final$Depth[l], ylim = c(max(final$Depth[l]),501), pch = '.', cex = 3)
  plot(x = final$CTD.Salinity[l], y = final$Depth[l], ylim = c(max(final$Depth[l]),501), pch = '.', cex = 3)
  plot(x = final$CTD.Oxygen.Corr[l], y = final$Depth[l], ylim = c(max(final$Depth[l]),501), pch = '.', cex = 3)
  }
}

## profile-by-profile notes 
#Stn 43 = funky oxygen circa >1000
#Stn 39 = funky oxygen circa >1300
#Stn 35 = maybe the deepest oxygen value is questionable
#Stn 34 = funky oxygen circa 1000, 1750, and 2000
#Stn 30 = funky oxygen circa <1500
#Stn 14 why isn't the data continous to 1200 m
#Stn 5 difference in oxygen between casts (and also temperature)
#Stn 4 = is the temperature profile suspicious? 

## Flag applications 

#Stn 43
l = which(final$Station == 43 & final$Depth >1000 & final$Depth < 1100)
plot(x = final$CTD.Oxygen.Corr[l], y = final$Depth[l], ylim = c(1100,1000), pch = '.', cex = 3)
abline(v = 302)
#oxygen goes above 302 between 1030 and 1050
l = which(final$Station == 43 & final$Depth >1030 & final$Depth < 1050 & final$CTD.Oxygen.Corr > 302)
final$CTD.Oxygen.Flag[l] = 4

#Stn 39
l = which(final$Station == 39 & final$Depth >1350 & final$Depth < 1450)
plot(x = final$CTD.Oxygen.Corr[l], y = final$Depth[l], ylim = c(1450,1350), pch = '.', cex = 3)
abline(v = 300)
#oxygen flag
l = which(final$Station == 39 & final$Depth >1350 & final$Depth < 1450 & final$CTD.Oxygen.Corr > 300)
final$CTD.Oxygen.Flag[l] = 4

#Stn 35
l = which(final$Station == 35 & final$Depth >1190 & final$Depth < 1205)
plot(x = final$CTD.Oxygen.Corr[l], y = final$Depth[l], ylim = c(1205,1190), pch = '.', cex = 3)
abline(v = 305)
abline(v = 306)
#oxygen flag
l = which(final$Station == 35 & final$Depth >1190 & final$Depth < 1205 & final$CTD.Oxygen.Corr < 305)# 
final$CTD.Oxygen.Flag[l] = 3
l = which(final$Station == 35 & final$Depth >1190 & final$Depth < 1205 & final$CTD.Oxygen.Corr > 306)# 
final$CTD.Oxygen.Flag[l] = 4


#Stn 34
l = which(final$Station == 34 & final$Depth >960 & final$Depth < 990)
plot(x = final$CTD.Oxygen.Corr[l], y = final$Depth[l], ylim = c(990,960), pch = '.', cex = 3)
abline(v = 310)
abline(h = 973)
abline(h = 981)
#oxygen flags
l = which(final$Station == 34 & final$Depth >973 & final$Depth < 981 & final$CTD.Oxygen.Corr > 310)# 
final$CTD.Oxygen.Flag[l] = 4

l = which(final$Station == 34 & final$Depth >1735 & final$Depth < 1750)
plot(x = final$CTD.Oxygen.Corr[l], y = final$Depth[l], ylim = c(1750,1735), pch = '.', cex = 3) 
abline(v = 296.8)
abline(h = 1738)
abline(h = 1741)
#oxygen flags
l = which(final$Station == 34 & final$Depth >1738 & final$Depth < 1744 & final$CTD.Oxygen.Corr > 297)
final$CTD.Oxygen.Flag[l] = 4
l = which(final$Station == 34 & final$Depth >1741 & final$Depth < 1743) 
final$CTD.Oxygen.Flag[l] = 3

l = which(final$Station == 34 & final$Depth >2050 & final$Depth < 2100)
plot(x = final$CTD.Oxygen.Corr[l], y = final$Depth[l], ylim = c(2100,2050), pch = '.', cex = 3) 
abline(v = 297)
#oxygen flags
l = which(final$Station == 34 & final$Depth >2050 & final$Depth < 2100 & final$CTD.Oxygen.Corr > 297)
final$CTD.Oxygen.Flag[l] = 4


#Stn 30
l = which(final$Station == 30 & final$Depth >1425 & final$Depth < 1460)
plot(x = final$CTD.Oxygen.Corr[l], y = final$Depth[l], ylim = c(1460,1425), pch = '.', cex = 3)
abline(v = 300)
abline(h = 1440)
abline(v = 299.)

#oxygen flags
l = which(final$Station == 30 & final$Depth >1425 & final$Depth < 1460 & final$CTD.Oxygen.Corr > 300)# 
final$CTD.Oxygen.Flag[l] = 4

##Stn 14
## The downcast stopped communicating at 1000 m and kicked back on at 1200 (ish) -- will need to grab upcast data for this station to fill in the missing data, or leave it incomplete. 

## Stn 5
#The two casts have different features at 50 m. 

## Stn 4
#maybe a blip at 30 m, but not notable enough to flag. 


## Plots for Station-by station QA
for (stn in c(43,39,35,34,30)) {
  l = which(final$Station == stn)
  
    par(mfrow = c(1,4))
    plot(x = final$CTD.Temperature[l], y = final$Depth[l], ylim = c(max(final$Depth[l]),0), pch = '.', cex = 3)
    mtext(paste0('Station ', stn), side = 3)
    plot(x = final$CTD.Conductivity[l], y = final$Depth[l], ylim = c(max(final$Depth[l]),0), pch = '.', cex = 3)
    plot(x = final$CTD.Salinity[l], y = final$Depth[l], ylim = c(max(final$Depth[l]),0), pch = '.', cex = 3)
    plot(x = final$CTD.Oxygen.Corr[l], y = final$Depth[l], ylim = c(max(final$Depth[l]),0), pch = '.', cex = 3)
    points(x = final$CTD.Oxygen.Corr[l][final$CTD.Oxygen.Flag[l] == 4], y = final$Depth[l][final$CTD.Oxygen.Flag[l] == 4], pch = 16, col = 'red')
    points(x = final$CTD.Oxygen.Corr[l][final$CTD.Oxygen.Flag[l] == 3], y = final$Depth[l][final$CTD.Oxygen.Flag[l] == 3], pch = 16, col = 'orange')
    points(x = final$CTD.Temperature[l][final$CTD.Temperature.Flag[l] == 4], y = final$Depth[l][final$CTD.Temperature.Flag[l] == 4], pch = 16, col = 'red')
    points(x = final$CTD.Temperature[l][final$CTD.Temperature.Flag[l] == 3], y = final$Depth[l][final$CTD.Temperature.Flag[l] == 3], pch = 16, col = 'orange')
    
}
 
## WRITE DOWNCAST FILE
saveRDS(final, './output/final stage/NABOS2025_20260727_Final Downcast.RDS')
write.xlsx(final, './output/final stage/NABOS2025_20260727_Final Downcast.xlsx')
write.csv(final, './output/final stage/NABOS2025_20260727_Final Downcast.csv')

                    
final.igor = final
final.igor = final.igor %>% replace(is.na(final.igor), -9999)

write.csv(final.igor, './output/final stage/NABOS2025_20260727_Final Downcast_-9999.csv')
          

