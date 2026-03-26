## Tidy Downcast Data, Begin QC 
## Laura M. Whitmore
## Update: Jan. 13, 2025

## If a downcast file has not been created yet use "Automatic CTD processing (NABOS2023).R" 
## to generate a compiled downcast file. 

readRDS('./output/downcast.RDS')

## Remove preceding text before station and cast numbers
# Note, bottle file should have data for station 21 cast 1a 
# since there was an error in acquisition during the up-cast; the downcast 
# file should have no 1a data, so all both of these columns should be able to 
# be numeric. 

downcast$StationNo = -99
downcast$CastNo = -99
for (i in 1:nrow(downcast)){
  downcast$StationNo[i] = strsplit(downcast$Station[i], split = 'station')[[1]][2]  
  downcast$CastNo[i] = strsplit(downcast$Cast[i], split = 'cast')[[1]][2]  
}

downcast$StationNo = as.numeric(downcast$StationNo)
downcast$CastNo = as.numeric(downcast$CastNo)
downcast = downcast[order(downcast$StationNo, downcast$CastNo),]

## Add Rudimentary Quality Flags 

downcast$prDM.Flag = NA
downcast$t090C.Flag = NA
downcast$t190C.Flag = NA
downcast$c0mS.cm.Flag = NA
downcast$c1mS.cm.Flag = NA
downcast$sal00.Flag = NA
downcast$sal11.Flag = NA
downcast$sbox0Mm.Kg.Flag = NA #oxygen primary
downcast$sbox1Mm.Kg.Flag = NA #oxygen secondary
downcast$flECO.AFL.Flag = NA
downcast$wetCDOM.Flag = NA
downcast$CStarAt0.Flag = NA

## Visualization 

par(mfrow = c(1,4))
plot(x = downcast$t090C, y = downcast$prDM, 
     pch = 16, cex = 0.7, 
     ylim = c(7000,0),
     xlab = 'CTD Temperature (umol/kg)',
     ylab = 'Pressure (dbar)')

plot(x = downcast$t190C, y = downcast$prDM, 
     pch = 16, cex = 0.7, 
     ylim = c(7000,0),
     xlab = 'CTD Temperature (umol/kg)',
     ylab = 'Pressure (dbar)')


plot(x = downcast$sbox0Mm.Kg, y = downcast$prDM, 
     pch = 16, cex = 0.7, 
     ylim = c(7000,0),
     xlab = 'CTD Oxygen (umol/kg)',
     ylab = 'Pressure (dbar)')


plot(x = downcast$sbox1Mm.Kg, y = downcast$prDM, 
     pch = 16, cex = 0.7, 
     ylim = c(7000,0),
     xlab = 'CTD Oxygen (umol/kg)',
     ylab = 'Pressure (dbar)')

# Notes on visualization of all casts: primary temperature has issue that 
# effects primary oxygen. Secondary temperature basically has two profiles that 
# are offset, plus at least one profile with a super weird oxygen minimum at 1000 m
# may need to look at primary oxygen for all but the corrupted temperature casts to 
# see if it also has some of these offsets/profile shapes. 

## Station by station plots
pdf(file = './output/Figures/Profiles/Stn Profiles Temperature.pdf')
pdf(file = './output/Figures/Profiles/Stn Profiles Temperature_Q90.pdf')

for (s in unique(downcast$StationNo)) {
  l = which(downcast$StationNo == s)
  for (c in unique(downcast$CastNo[l])) {
    j = which(downcast$CastNo[l] == c)
    par(mfrow = c(1,3))
    plot(x = downcast$t090C[l[j]], 
         y = downcast$prDM[l[j]],
         ylim = c(max(downcast$prDM[l[j]], na.rm = T), 0),
         pch = 16, cex = 0.7, 
         xlab = 'Temperature [ITS-90] (deg C)',
         ylab = 'Pressure (dbar)', main = paste0(s, ', ', c))
    plot(x = downcast$t190C[l[j]], 
         y = downcast$prDM[l[j]],
         ylim = c(max(downcast$prDM[l[j]], na.rm =T), 0),
         pch = 16, cex = 0.7, 
         xlab = 'Temperature2 [ITS-90] (deg C)',
         ylab = 'Pressure (dbar)', main = paste0(s, ', ', c))
    plot(x = (downcast$t090C[l[j]] - downcast$t190C[l[j]]), 
         y = downcast$prDM[l[j]],
         ylim = c(max(downcast$prDM[l[j]], na.rm =T), 0),
         xlim = c(-0.1, 0.1), #xlim determined by quantile(abs(downcast$t090C - downcast$t190C), na.rm = T, probs = 0.99)
         pch = 16, cex = 0.7, 
         xlab = 'Difference between sensors (deg C)',
         ylab = 'Pressure (dbar)', main = paste0(s, ', ', c))
  }
}

dev.off()

quantile(abs(downcast$t090C - downcast$t190C), na.rm = T, probs = 0.90)
# At station 15 onward it seems like there is an issue causing higher variability
# than typical in several profiles, it's not every profile though.  

# At station 26, Cast 1, difference between t0 and t1 is squirly at about 500m. 
# It looks like an issue with t1, because the profile is not particularly clean. 

#At station 38, the difference between the sensors begins to take the shape of the profile...
# pressure effects become more obvious as well.  


## Checking unbinned csv file for station 27, it has really bad variability in BOTH
# temperature sensors. These are on two separate pumps, so probably not a pump or a
# sensor problem. Current hypothesis is it's a pressure sensor issue. 

temp = oce::read.ctd.sbe('./proc/hly2302_station27_cast1.cnv')
dat = as.data.frame(temp@data)
dat$datetime = temp@metadata$date ## NMEA DATE (temp@metadata$startTime) is corrupted for many casts -- using computer time instead
dat$ID = 'hly2302_station27_cast1'
#temp.name = strsplit(ctd.files[i], '_')[[1]]
#dat$HLYID = temp.name[1]
#dat$Station = temp.name[2] ## use gsub to remove "Station" or "Cast" and leave a numeric value
#dat$Cast = strsplit(temp.name[3], 'avg.')[[1]][1]
plot(dat$timeM*60, dat$pressure, pch = 20)
plot(dat$timeM[-1]*60, diff(dat$pressure), pch = '.', xlim = c(500,1000))

#dat2$datetime = temp@metadata$date ## NMEA DATE (temp@metadata$startTime) is corrupted for many casts -- using computer time instead
#dat$ID = 'hly2302_station27_cast1'
#temp.name = strsplit(ctd.files[i], '_')[[1]]
#dat$HLYID = temp.name[1]
#dat$Station = temp.name[2] ## use gsub to remove "Station" or "Cast" and leave a numeric value
#dat$Cast = strsplit(temp.name[3], 'avg.')[[1]][1]
plot(dat2$timeM*60, dat$pressure, pch = 20)
plot(dat2$timeM[-1]*60, diff(dat2$pressure), pch = '.', xlim = c(500,1000))


#dat2$datetime = temp@metadata$date ## NMEA DATE (temp@metadata$startTime) is corrupted for many casts -- using computer time instead
#dat$ID = 'hly2302_station27_cast1'
#temp.name = strsplit(ctd.files[i], '_')[[1]]
#dat$HLYID = temp.name[1]
#dat$Station = temp.name[2] ## use gsub to remove "Station" or "Cast" and leave a numeric value
#dat$Cast = strsplit(temp.name[3], 'avg.')[[1]][1]
plot(dat2$timeM*60, dat$pressure, pch = 20)
plot(dat2$timeM[-1]*60, diff(dat2$pressure), pch = '.', xlim = c(500,1000))


n2321 = oce::read.ctd.sbe('./proc/hly2302_station21_cast2.cnv')
nab23st21 = as.data.frame(n2321@data)
n2334 = oce::read.ctd.sbe('./proc/hly2302_station34_cast1.cnv')
nab23st34 = as.data.frame(n2334@data)
n2340 = oce::read.ctd.sbe('./proc/hly2302_station40_cast1.cnv')
nab23st40 = as.data.frame(n2338@data)

n2111 = oce::read.ctd.sbe('G:/Shared drives/NABOS/NABOS2021 Cruise/Data/CTD/NABOS2021_CTD_Stage5_StabCheck/Stage5_StabCheck/NABOS21011_QC.cnv')
nab21st11 = as.data.frame(n2111@data)
n2122 = oce::read.ctd.sbe('G:/Shared drives/NABOS/NABOS2021 Cruise/Data/CTD/NABOS2021_CTD_Stage5_StabCheck/Stage5_StabCheck/NABOS21022_QC.cnv')
nab21st22 = as.data.frame(n2122@data)
n2144 = oce::read.ctd.sbe('G:/Shared drives/NABOS/NABOS2021 Cruise/Data/CTD/NABOS2021_CTD_Stage5_StabCheck/Stage5_StabCheck/NABOS21044_QC.cnv')
nab21st44 = as.data.frame(n2167@data)

par(mfrow = c(1,3))
plot(nab21st22$temperature, nab21st22$pressure, type = 'l', ylim = c(1200,0),
     xlab = 'Temperature', ylab = 'Pressure')
lines(nab23st34$temperature, nab23st34$pressure, col = 'gray')
text(x = -1.45, y = 1000, 
     labels = paste0('2021 St 22: ', round(n2122@metadata$latitude, digits = 1), ', ', round(n2122@metadata$longitude, digits = 1)), 
     adj = 0)
text(x = -1.45, y = 1100, 
     labels = paste0('2023 St 34: ', round(n2334@metadata$latitude, digits = 1), ', ', round(n2334@metadata$longitude, digits = 1)), 
     adj = 0, col = 'gray')

plot(nab21st44$temperature, nab21st44$pressure, type = 'l', ylim = c(1200,0),
     xlab = 'Temperature', ylab = 'Pressure')
lines(nab23st21$temperature, nab23st21$pressure, col = 'gray')
text(x = -1.45, y = 1000, 
     labels = paste0('2021 St 44: ', round(n2144@metadata$latitude, digits = 1), ', ', round(n2144@metadata$longitude, digits = 1)), 
     adj = 0)
text(x = -1.45, y = 1100, 
     labels = paste0('2023 St 21: ', round(n2321@metadata$latitude, digits = 1), ', ', round(n2321@metadata$longitude, digits = 1)), 
     adj = 0, col = 'gray')

plot(nab21st11$temperature, nab21st11$pressure, type = 'l', ylim = c(1200,0),
     xlab = 'Temperature', ylab = 'Pressure')
lines(nab23st40$temperature, nab23st40$pressure, col = 'gray')
text(x = -1.45, y = 1000, 
     labels = paste0('2021 St 11: ', round(n2111@metadata$latitude, digits = 1), ', ', round(n2111@metadata$longitude, digits = 1)), 
     adj = 0)
text(x = -1.45, y = 1100, 
     labels = paste0('2023 St 40: ', round(n2340@metadata$latitude, digits = 1), ', ', round(n2340@metadata$longitude, digits = 1)), 
     adj = 0, col = 'gray')
     

par(mfrow = c(1,2))
plot(nab21st22$temperature, nab21st22$pressure,  pch = '.', ylim = c(1200,0),
     xlab = 'Temperature', ylab = 'Pressure')
points(nab23st34$temperature, nab23st34$pressure, pch = '.', col = 'gray')
text(x = -1.45, y = 1000, 
     labels = paste0('2021 St 22: ', round(n2122@metadata$latitude, digits = 1), ', ', round(n2122@metadata$longitude, digits = 1)), 
     adj = 0)
text(x = -1.45, y = 1100, 
     labels = paste0('2023 St 34: ', round(n2334@metadata$latitude, digits = 1), ', ', round(n2334@metadata$longitude, digits = 1)), 
     adj = 0, col = 'gray')

plot(nab21st44$temperature, nab21st44$pressure, pch = '.', ylim = c(1200,0),
     xlab = 'Temperature', ylab = 'Pressure')
points(nab23st21$temperature, nab23st21$pressure, pch = '.', col = 'gray')
text(x = -1.45, y = 1000, 
     labels = paste0('2021 St 44: ', round(n2144@metadata$latitude, digits = 1), ', ', round(n2144@metadata$longitude, digits = 1)), 
     adj = 0)
text(x = -1.45, y = 1100, 
     labels = paste0('2023 St 21: ', round(n2321@metadata$latitude, digits = 1), ', ', round(n2321@metadata$longitude, digits = 1)), 
     adj = 0, col = 'gray')


par(mfrow = c(1,3))
plot(nab21st22$conductivity, nab21st22$pressure,  pch = '.', ylim = c(1200,0),
     xlab = 'Conductivity', ylab = 'Pressure', xlim = c(23,32))
points(nab23st34$conductivity, nab23st34$pressure, pch = '.', col = 'gray')
text(x = 27, y = 1000, 
     labels = paste0('2021 St 22: ', round(n2122@metadata$latitude, digits = 1), ', ', round(n2122@metadata$longitude, digits = 1)), 
     adj = 0)
text(x = 27, y = 1100, 
     labels = paste0('2023 St 34: ', round(n2334@metadata$latitude, digits = 1), ', ', round(n2334@metadata$longitude, digits = 1)), 
     adj = 0, col = 'gray')

plot(nab21st44$conductivity, nab21st44$pressure, pch = '.', ylim = c(1200,0),
     xlab = 'Conductivity', ylab = 'Pressure', xlim = c(23,32))
points(nab23st21$conductivity, nab23st21$pressure, pch = '.', col = 'gray')
text(x = 24, y = 1000, 
     labels = paste0('2021 St 44: ', round(n2144@metadata$latitude, digits = 1), ', ', round(n2144@metadata$longitude, digits = 1)), 
     adj = 0)
text(x = 24, y = 1100, 
     labels = paste0('2023 St 21: ', round(n2321@metadata$latitude, digits = 1), ', ', round(n2321@metadata$longitude, digits = 1)), 
     adj = 0, col = 'gray')

plot(nab21st11$conductivity, nab21st11$pressure, pch = '.', ylim = c(1200,0),
     xlab = 'Conductivity', ylab = 'Pressure', xlim = c(23,32))
points(nab23st40$conductivity, nab23st40$pressure, pch = '.', col = 'gray')
text(x = 27, y = 1000, 
     labels = paste0('2021 St 11: ', round(n2111@metadata$latitude, digits = 1), ', ', round(n2111@metadata$longitude, digits = 1)), 
     adj = 0)
text(x = 27, y = 1100, 
     labels = paste0('2023 St 40: ', round(n2340@metadata$latitude, digits = 1), ', ', round(n2340@metadata$longitude, digits = 1)), 
     adj = 0, col = 'gray')


source("https://raw.githubusercontent.com/tbrycekelly/TheSource/refs/heads/master/R/source.physics.r")
nab21st22$rho = calc.rho(S = nab21st22$salinity, Tmp = nab21st22$temperature, P = nab21st22$pressure)
nab21st44$rho = calc.rho(S = nab21st44$salinity, Tmp = nab21st44$temperature, P = nab21st44$pressure)
nab21st11$rho = calc.rho(S = nab21st11$salinity, Tmp = nab21st11$temperature, P = nab21st11$pressure)

nab23st21$rho = calc.rho(S = nab23st21$salinity, Tmp = nab23st21$temperature, P = nab23st21$pressure)
nab23st34$rho = calc.rho(S = nab23st34$salinity, Tmp = nab23st34$temperature, P = nab23st34$pressure)
nab23st40$rho = calc.rho(S = nab23st40$salinity, Tmp = nab23st40$temperature, P = nab23st40$pressure)

par(mfrow = c(1,1))
plot(nab23st34$rho, nab23st34$pressure,  pch = '.', ylim = c(1200,0),
     xlab = 'In-situ Density', ylab = 'Pressure')

plot(downcast$rho[downcast$StationNo == 21], downcast$prDM[downcast$StationNo == 21], pch = '.', ylim = c(1200,0))

par(mfrow = c(1,3))
plot(nab21st22$rho, nab21st22$pressure,  pch = '.', ylim = c(1200,0),
     xlab = 'In-situ Density', ylab = 'Pressure')
points(nab23st34$rho, nab23st34$pressure, pch = '.', col = 'gray')
text(x = 1030, y = 1000, 
     labels = paste0('2021 St 22: ', round(n2122@metadata$latitude, digits = 1), ', ', round(n2122@metadata$longitude, digits = 1)), 
     adj = 0)
text(x = 1030, y = 1100, 
     labels = paste0('2023 St 34: ', round(n2334@metadata$latitude, digits = 1), ', ', round(n2334@metadata$longitude, digits = 1)), 
     adj = 0, col = 'gray')

plot(nab21st44$rho, nab21st44$pressure, pch = '.', ylim = c(1200,0),
     xlab = 'In-Situ Density', ylab = 'Pressure')
points(nab23st21$rho, nab23st21$pressure, pch = '.', col = 'gray')
text(x = 1024, y = 1000, 
     labels = paste0('2021 St 44: ', round(n2144@metadata$latitude, digits = 1), ', ', round(n2144@metadata$longitude, digits = 1)), 
     adj = 0)
text(x = 1024, y = 1100, 
     labels = paste0('2023 St 21: ', round(n2321@metadata$latitude, digits = 1), ', ', round(n2321@metadata$longitude, digits = 1)), 
     adj = 0, col = 'gray')

plot(nab21st11$rho, nab21st11$pressure, pch = '.', ylim = c(1200,0),
     xlab = 'In-Situ Density', ylab = 'Pressure')
points(nab23st40$rho, nab23st40$pressure, pch = '.', col = 'gray')
text(x = 1028, y = 1000, 
     labels = paste0('2021 St 11: ', round(n2144@metadata$latitude, digits = 1), ', ', round(n2144@metadata$longitude, digits = 1)), 
     adj = 0)
text(x = 1028, y = 1100, 
     labels = paste0('2023 St 40: ', round(n2321@metadata$latitude, digits = 1), ', ', round(n2321@metadata$longitude, digits = 1)), 
     adj = 0, col = 'gray')

par(mfrow = c(1,1))
l = which(nab23st21$depth < 1200 & nab23st21$depth > 600)
nab23st21$ptemp = calc.ptemp(S=nab23st21$salinity, 
                             Tmp = nab23st21$temperature, 
                             P = nab23st21$pressure, P.ref = 0)
plot.TS(S = nab23st21$salinity[l], 
        Tmp = nab23st21$ptemp[l], 
        xlim = c(34.8,34.9), ylim = c(-0.5,0.5), pch = '.')


l = which(downcast$StationNo == 27 & downcast$CastNo == 1 & 
            downcast$depSM < 1200 & downcast$depSM > 400)

l = which(downcast$StationNo == 27 & downcast$CastNo == 1)

downcast$ptemp = calc.ptemp(S=downcast$sal00, 
                             Tmp = downcast$t090C, 
                             P = downcast$prDM, P.ref = 0)
downcast$sigma = calc.sigma.theta(S=downcast$sal00, 
                            Tmp = downcast$ptemp, 
                            P = 0, P.ref = 0)

plot(downcast$sigma[l], downcast$depSM[l], type = 'l',
     ylim = c(300,0), xlim = c(27.7, 28))
plot(downcast$sigma[l], downcast$depSM[l], pch = 20,
     ylim = c(1000,600), xlim = c(27.95, 28))
grid()

plot(downcast$t090C[l], downcast$prDM[l], pch = '.',
     ylim = c(1200,0), xlab = 'Temperature', ylab = 'Pressure')
grid()

plot(downcast$sal00[l], downcast$prDM[l], pch = '.',
     ylim = c(1200,0), xlab = 'Salinity', ylab = 'Pressure')


plot.TS(S = downcast$sal00[l], 
        Tmp = downcast$ptemp[l], 
        xlim = c(34.8,34.9), ylim = c(-0.5,0.75),
        pch = '.', cex = 3, levels = seq(27, 29, by = 0.02))


text(x = 0, y = 1000, labels(object = n2122@metadata$latitude))
plot(nab23st34$temperature, nab23st34$pressure, type = 'l', ylim = c(1200,0),
     main = '2023 Stn 34', xlab = 'Temperature', ylab = 'Pressure')
plot(nab21st67$temperature, nab21st67$pressure, type = 'l', ylim = c(1200,0),
     main = '2021 Stn 67', xlab = 'Temperature', ylab = 'Pressure')
plot(nab23st21$temperature, nab23st21$pressure, type = 'l', ylim = c(1200,0),
     main = '2023 Stn 21', xlab = 'Temperature', ylab = 'Pressure')



par(mfrow = c(1,2))
plot(nab21st22$temperature, nab21st22$pressure, type = 'l', ylim = c(1200,0))
plot(nab23st34$temperature, nab23st34$pressure, type = 'l', ylim = c(1200,0))


## this replaces funky symbols with character strings
for (j in 1:length(temp@metadata$dataNamesOriginal)) {
  colnames(dat)[j] = make.names(gsub(useBytes = T, '\xe900', 'theta', temp@metadata$dataNamesOriginal[[j]]))
}

write.xlsx(dat, file = out.name)
message('Finished file ', i)




for (s in unique(downcast$Station)) {
  l = which(downcast$Station == s)
  for (c in unique(downcast$Cast[l])) {
    j = which(downcast$Cast[l] == c)
    par(mfrow = c(1,3))
    plot(x = downcast$CTDCOND..mS.cm.[l[j]], 
         y = downcast$CTDPRS..dbar.[l[j]],
         ylim = c(max(downcast$CTDPRS..dbar.[l[j]], na.rm = T), 0),
         pch = 16, cex = 0.7, 
         xlab = 'Conductivity (mS/cm)',
         ylab = 'Pressure (dbar)', main = paste0(s, ', ', c))
    plot(x = downcast$CTDCOND2..mS.cm.[l[j]], 
         y = downcast$CTDPRS..dbar.[l[j]],
         ylim = c(max(downcast$CTDPRS..dbar.[l[j]], na.rm =T), 0),
         pch = 16, cex = 0.7, 
         xlab = 'Conductivity2 (mS/cm)',
         ylab = 'Pressure (dbar)', main = paste0(s, ', ', c))
    plot(x = (downcast$CTDCOND..mS.cm.[l[j]] - downcast$CTDCOND2..mS.cm.[l[j]]), 
         y = downcast$CTDPRS..dbar.[l[j]],
         ylim = c(max(downcast$CTDPRS..dbar.[l[j]], na.rm =T), 0),
         pch = 16, cex = 0.7, 
         xlab = 'Difference between sensors',
         ylab = 'Pressure (dbar)', main = paste0(s, ', ', c))
    abline(v= 0, lty = 2, col = 'white')
  }
}

#Stn 38, Cast 1 has weird variability around 500 m

## So, in deciding which T/S to use we have the option to pick, Primary, Secondary, 
# a blend of Primary and Secondary on a cast-by-cast basis, or an average of primary
# and secondary where both sensors are good with exceptions where one sensor behaved 
# squirly. 
# I think the BlueFins group elected to identify one sensor rather than an average.  




#difference between sensors relative to CTDTMP2
(downcast$CTDTMP..ITS.90.[l[j]] - downcast$CTDTMP2..ITS.90.[l[j]])/downcast$CTDTMP2..ITS.90.[l[j]]


downcast$CTDPRS..dbar.[downcast$Station == 13 & downcast$Cast == 2]

downcast$CTDPRS..dbar.[downcast$Station == '13' & downcast$Cast == '2']

#Profile by profile, station 45 is the only obviously bad CTDTMP1 profile-- 
#worth just flagging the whole profile as bad.  
# Stn 42, Cast 1 has a suspicious datapoint that could be flagged as bad. 
# Stn 34, Cast 1 has weird pressure data > 6000 db









## Rudimentary Quality Flagging

downcast$CTDPRS.Flag[downcast$CTDPRS..dbar. > 5000] = 4
#Station 34 had some corruption to the pressure sensor. 
downcast$CTDOXY1.Flag[downcast$CTDOXY..μmol.kg. < 0] = 4
downcast$CTDOXY2.Flag[downcast$CTDOXY2..μmol.kg. < 0] = 4

freeze = oce::swTFreeze(salinity = downcast$`CTDSAL.[PSS-78]`, 
                        pressure = downcast$`CTDPRS.[dbar]`)
summary(freeze)

for (i in 1:nrow(downcast)) {
  if (downcast$CTDTMP..ITS.90.[i] < freeze[i]) {
    downcast$CTDTMP1.Flag = 4
  }
  if (downcast$CTDTMP2..ITS.90.[i] < freeze[i]) {
    downcast$CTDTMP2.Flag = 4
  }
}


plot(y = downcast$prDM, x = downcast$wetCDOM, xlim = c(0,0.3))
plot(y = downcast$prDM, x = downcast$v6, xlim = c(0,0.3))

write.xlsx(x = downcast, file = '../Automatic CTD Processing/output/downcast_forODV.xlsx')

par(mfrow=c(1,1))
l = which(downcast$StationNo == 27 & downcast$CastNo == 1)
plot(diff(downcast$timeM[l])*60, downcast$depSM[l[-1]], pch = 20, xlim = c(0, 5))
plot(downcast$t190C[l], downcast$depSM[l], type = 'l')

plot(diff(downcast$t090C), downcast$depSM[-1], pch = '.', xlim = c(-0.1,0.1))
plot(downcast$sal00[l], downcast$depSM[l], pch = 20)
plot(downcast$c0mS.cm[l], downcast$depSM[l], pch = 20)


k = dat$timeM*60 > 500 & dat$timeM*60 < 700
summary(diff(dat$pressure)[k])
plot(dat$timeM[30100:3100], dat$depth[3000:3100], pch = '.')
