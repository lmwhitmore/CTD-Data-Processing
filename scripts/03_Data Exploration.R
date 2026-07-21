rm(list = ls())
library(SimpleMapper)
library(SimpleGridder)
library(pals)
library(oce)
library(batlow)

source("https://raw.githubusercontent.com/tbrycekelly/TheSource/refs/heads/master/R/source.physics.r")
source('https://raw.githubusercontent.com/tbrycekelly/TheSource/refs/heads/master/R/pal.R')

#import downcast and bottle
downcast = readRDS('./output/sbe stage/20260720_SBEStage_downcast.RDS')
bottles = readRDS('./output/sbe stage/20260720_SBEStage_bottles.RDS')


#new.downcast = downcast ## this is without clearing rm(list = ls()) from step 02_getCombinedOutput.R
#sbe.downcast = readRDS('../../Tasks/07 Data Submission Prep/NABOS/CTD/2025 Automatic CTD Processing/output/sbe stage/20260619_SBEStage_downcast.RDS')
#sbe.bottles = readRDS('./output/sbe stage/20260619_SBEStage_bottles.RDS')


##compare new downcast to old downcast (new downcast does filter and cellTM)
nrow(new.downcast)
nrow(sbe.downcast)
plot(x = new.downcast$t090C, y = new.downcast$depSM, y = xlim)

par(mfrow = c(1, 3))
plot(x = new.downcast$t090C, y = sbe.downcast$t090C, pch = '.', xlab = 'Temperature (with Filter/cellTM)', ylab = 'Temperature (without Filter/cellTM)')
abline(a = 0, b =1, col = 'red', lty = 2)
plot(x = new.downcast$c0mS.cm, y = sbe.downcast$c0mS.cm, pch = '.',  xlab = 'Conductivity (with Filter/cellTM)', ylab = 'Conductivity (without Filter/cellTM)')
abline(a = 0, b =1, col = 'red', lty = 2)
plot(x = new.downcast$sal00, y = sbe.downcast$sal00, pch = '.', xlab = 'Salinity (with Filter/cellTM)', ylab = 'Salinity (without Filter/cellTM)')
abline(a = 0, b =1, col = 'red', lty = 2)

par(mfrow = c(1, 3))
plot(x = sbe.downcast$t090C, y = sbe.downcast$t090C-new.downcast$t090C, pch = '.', xlab = 'Temperature (without Filter/cellTM)', ylab = 'Temperature-Temperature (new)')
plot(x = sbe.downcast$c0mS.cm, y = sbe.downcast$c0mS.cm-new.downcast$c0mS.cm, pch = '.',  xlab = 'Conductivity (without Filter/cellTM)', ylab = 'Conductivity-Conductivity(new)')
plot(x = sbe.downcast$sal00, y = sbe.downcast$sal00-new.downcast$sal00, pch = '.', xlab = 'Salinity (without Filter/cellTM)', ylab = 'Salinity-Salinity(new)')

#working intermediate file
downcast = sbe.downcast
bottles = sbe.bottles

#### INTERMEDIATE STAGE UPDATES ####

#station 41 secondary data is not good (clog in pump for part of cast)
downcast$t190C[downcast$Station == 41] = NA
downcast$sal11[downcast$Station == 41] = NA
downcast$sbox1Mm.Kg[downcast$Station == 41] = NA

bottles$t190C[bottles$Station == 41] = NA
bottles$sal11[bottles$Station == 41] = NA
bottles$Sbox1Mm.Kg[bottles$Station == 41] = NA


#secondary oxygen is bad for the whole cruise
downcast$sbox1Mm.Kg = NA

bottles$Sbox1Mm.Kg = NA

## oxygen calibration
  #conduct calibration in 2025 Oxygen Data Processing
  #import calibration
  calibration = readRDS('../../Tasks/07 Data Submission Prep/NABOS/Dissolved Oxygen/2025 Oxygen Data Processing/20260619_NABOS2025 Oxygen Calibration.RDS')

  downcast$Oxygen.Corr = calibration$coefficients[1] + calibration$coefficients[2] * downcast$sbox0Mm.Kg
  bottles$Oxygen.Corr = calibration$coefficients[1] + calibration$coefficients[2] * bottles$Sbox0Mm.Kg
  
## Altimeter clean 
  #make alt = 100 m == NA
  l = which(bottles$AltM == 100)
  bottles$AltM[l] = NA
  
  l = which(downcast$altM == 100)
  downcast$altM[l] = NA
  
#### EXPORT INTERMEDIATE CTD DATA ####
  
  saveRDS(downcast, './output/intermediate stage/NABOS2025_20260720_INTERMEDIATE_Downcast.RDS')
  saveRDS(bottles, './output/intermediate stage/NABOS2025_20260720_INTERMEDIATE_Bottle.RDS')
  write.xlsx(downcast, './output/intermediate stage/NABOS2025_20260720_INTERMEDIATE_Downcast.xlsx')
  write.xlsx(bottles, './output/intermediate stage/NABOS2025_20260720_INTERMEDIATE_Bottle.xlsx')
  
  
#### Compare to 20260619 Version

june.downcast = readRDS('../../Tasks/07 Data Submission Prep/NABOS/CTD/2025 Automatic CTD Processing/output/intermediate stage/NABOS2025_INTERMEDIATE_Downcast.RDS')


par(mfrow = c(1, 3))
plot(x = downcast$t090C, y = june.downcast$t090C, pch = '.', xlab = 'Temperature (with Filter/cellTM)', ylab = 'Temperature (without Filter/cellTM)')
abline(a = 0, b =1, col = 'red', lty = 2)
plot(x = downcast$c0mS.cm, y = june.downcast$c0mS.cm, pch = '.',  xlab = 'Conductivity (with Filter/cellTM)', ylab = 'Conductivity (without Filter/cellTM)')
abline(a = 0, b =1, col = 'red', lty = 2)
plot(x = downcast$sal00, y = june.downcast$sal00, pch = '.', xlab = 'Salinity (with Filter/cellTM)', ylab = 'Salinity (without Filter/cellTM)')
abline(a = 0, b =1, col = 'red', lty = 2)

colbar = get.pal(n = 20, pal = 'parula')
par(mfrow = c(1, 3))
plot(x = june.downcast$t090C, 
     y = june.downcast$t090C-downcast$t090C, 
     pch = '.', xlab = 'Temperature (without Filter/cellTM)', ylab = 'Temperature-Temperature (new)',
     col = make.pal(x = june.downcast$Station, n = 20, pal = 'parula', min = 1, max = 45))
plot(x = june.downcast$c0mS.cm, 
     y = june.downcast$c0mS.cm-downcast$c0mS.cm, pch = '.',  
     xlab = 'Conductivity (without Filter/cellTM)', ylab = 'Conductivity-Conductivity(new)',
     col = make.pal(x = june.downcast$Station, n = 20, pal = 'parula', min = 1, max = 45))
plot(x = june.downcast$sal00, y = june.downcast$sal00-downcast$sal00, pch = '.', 
     xlab = 'Salinity (without Filter/cellTM)', ylab = 'Salinity-Salinity(new)',
     col = make.pal(x = june.downcast$Station, n = 20, pal = 'parula', min = 1, max = 45))

check = downcast
check$TminusT = june.downcast$t090C-downcast$t090C
check$CminusC = june.downcast$c0mS.cm-downcast$c0mS.cm
check$SminusS = june.downcast$sal00-downcast$sal00

check$Station[abs(check$TminusT) > 0.0005]  
check$Station[abs(check$CminusC) > 0.0005]
check$Station[abs(check$SminusS) > 0.0005]

check$depSM[abs(check$TminusT) > 0.0005]  
check$depSM[abs(check$CminusC) > 0.0005]
check$depSM[abs(check$SminusS) > 0.0005]

check$Cast[abs(check$TminusT) > 0.0005]  
check$Cast[abs(check$CminusC) > 0.0005]
check$Cast[abs(check$SminusS) > 0.0005]


plot(check$t090C[check$Station == 15], y = check$depSM[check$Station == 15], ylim = c(250, 0), pch = '.',
     xlab = 'Temperature (ITS-90 deg C)', ylab = 'Depth (m)')
points(check$t090C[check$Station == 15 & check$Cast == 2], 
       y = check$depSM[check$Station == 15 & check$Cast == 2], 
       ylim = c(250, 0), pch = '.', cex = 2, col = 'green')
plot(check$c0mS.cm[check$Station == 15], y = check$depSM[check$Station == 15], ylim = c(250, 0), pch = '.',
     xlab = 'Conductivity (uS/cm)', ylab = 'Depth (m)')
points(check$c0mS.cm[check$Station == 15 & check$Cast == 2], 
       y = check$depSM[check$Station == 15 & check$Cast == 2], 
       ylim = c(250, 0), pch = '.', cex = 2, col = 'green', 
       xlab = 'Salinity (ppt)', ylab = 'Depth (m)')
mtext('black = station 15, cast 1; green = station 15, cast 2', side = 3 )
plot(check$sal00[check$Station == 15], y = check$depSM[check$Station == 15], ylim = c(250, 0), pch = '.')
points(check$sal00[check$Station == 15 & check$Cast == 2], 
       y = check$depSM[check$Station == 15 & check$Cast == 2], 
       ylim = c(250, 0), pch = '.', cex = 2, col = 'green')
mtext('black = station 15, cast 1; green = station 15, cast 2', side = 3 )
points(june.downcast$sal00[june.downcast$Station == 15 & check$Cast == 2], 
       y = june.downcast$depSM[june.downcast$Station == 15 & check$Cast == 2], 
       ylim = c(250, 0), pch = '.', cex = 2, col = 'blue')


#### Data Exploration ####
  
## Check NMEA Metadata
unique(downcast$datetime)
#datetimes are 2025 and the appropriate cruise times by checking a couple cruise logs. 

## Check locations 

## Check soak/ down

## Check for temperature anomalies 
## Temperature2-Temperature1 difference profiles with a color gradient for station no. 
TempDiff = downcast$t190C-downcast$t090C

par(plt = c(0.2, 0.85, 0.2, 0.9))
plot(x = TempDiff, y = downcast$prDM, 
     ylim = c(4500,0), #xlim = c(-0.01, 0.01), ## original range 0.25 to -0.25, captures bad data 
     xlab = 'Difference in Primary & Secondary Temperature (deg C)', 
     ylab = 'Pressure [dbar]',
     pch = '.', cex = 2, 
     col = make.pal(x = downcast$Station, min = 0, max = 45, n = 25, pal = 'brewer.ylorrd'))

colorbar(pal = brewer.ylorrd(25), zlim = c(0, 45))

## Temperature difference investigation
l = which(TempDiff > 0.1)
downcast$Station[l]

##station = 41 -- clogged pump on secondary sensor -- looks like a downcast issue, but will convert secondary to NA for bottle file too. 
plot(x = downcast$t090C[downcast$Station == 41], y = downcast$prDM[downcast$Station == 41], pch = '.', ylim = c(1500,0))
points(x = downcast$t190C[downcast$Station == 41], y = downcast$prDM[downcast$Station == 41], pch = '.', col = 'red')

plot(x = downcast$sal00[downcast$Station == 41], y = downcast$prDM[downcast$Station == 41], pch = '.', ylim = c(1500,0), xlim = c(30,36))
points(x = downcast$sal11[downcast$Station == 41], y = downcast$prDM[downcast$Station == 41], pch = '.', col = 'red')

## Compare sensor to sensor plot, check if any other stations need to be investigated
plot(x = downcast$t090C, y = downcast$t190C, pch = '.')
plot(x = downcast$sal00, y = downcast$sal11, pch = '.')
plot(x = downcast$sbox0Mm.Kg, y = downcast$sbox1Mm.Kg, pch = '.')

SalDiff = downcast$sal11 - downcast$sal00

plot(x = SalDiff, y = downcast$prDM, 
     ylim = c(4500,0), #xlim = c(-0.01, 0.01), ## original range 0.25 to -0.25, captures bad data 
     xlab = 'Difference in Primary & Secondary Salinity (ppt)', 
     ylab = 'Pressure [dbar]',
     pch = '.', cex = 2, 
     col = make.pal(x = downcast$Station, min = 0, max = 45, n = 25, pal = 'brewer.ylorrd'))

colorbar(pal = brewer.ylorrd(25), zlim = c(0, 45))

OxDiff = downcast$sbox1Mm.Kg - downcast$sbox0Mm.Kg
plot(x = OxDiff, y = downcast$prDM, 
     ylim = c(4500,0), #xlim = c(-0.01, 0.01), ## original range 0.25 to -0.25, captures bad data 
     xlab = 'Difference in Primary & Secondary Oxygen (ppt)', 
     ylab = 'Pressure [dbar]',
     pch = '.', cex = 2, 
     col = make.pal(x = downcast$Station, min = 0, max = 45, n = 25, pal = 'brewer.ylorrd'))

colorbar(pal = brewer.ylorrd(25), zlim = c(0, 45))


## Oxygen has several places where the difference between the two sensors is large. Identify where those are and if it is a consistent sensor. 
l = which(OxDiff < -80)
unique(downcast$Station[l])

plot(x = downcast$sbox0Mm.Kg[downcast$Station == 25], y = downcast$depSM[downcast$Station == 25], 
     pch = '.', ylim = c(1500,0),
     xlab = 'Oxygen (umol/kg)', ylab = 'Pressure (dbar)', main = 'Station 25')
points(x = downcast$sbox1Mm.Kg[downcast$Station == 25], y = downcast$depSM[downcast$Station == 25], pch = '.', cex = 2, ylim = c(1500,0), col = 'red')

plot(x = downcast$sal00[downcast$Station == 25], y = downcast$depSM[downcast$Station == 25], pch = 16, ylim = c(1500,0))
points(x = downcast$sal11[downcast$Station == 25], y = downcast$depSM[downcast$Station == 25], pch = 16, ylim = c(1500,0), col = 'red')

plot(x = downcast$sbox0Mm.Kg[downcast$Station == 2], y = downcast$depSM[downcast$Station == 2], pch = 16, ylim = c(50,0))
points(x = downcast$sbox1Mm.Kg[downcast$Station == 2], y = downcast$depSM[downcast$Station == 2], pch = 16, ylim = c(50,0), col = 'red')

plot(x = downcast$sal00[downcast$Station == 2], y = downcast$depSM[downcast$Station == 2], pch = 16, ylim = c(50,0))
points(x = downcast$sal11[downcast$Station == 2], y = downcast$depSM[downcast$Station == 2], pch = 16, ylim = c(50,0), col = 'red')

plot(x = downcast$sbox0Mm.Kg[downcast$Station == 38], y = downcast$depSM[downcast$Station == 38], pch = 16, ylim = c(1500,0))
points(x = downcast$sbox1Mm.Kg[downcast$Station == 38], y = downcast$depSM[downcast$Station == 38], pch = 16, ylim = c(1500,0), col = 'red')

plot(x = downcast$sal00[downcast$Station == 38], y = downcast$depSM[downcast$Station == 38], pch = 16, ylim = c(1500,0))
points(x = downcast$sal11[downcast$Station == 38], y = downcast$depSM[downcast$Station == 38], pch = 16, ylim = c(1500,0), col = 'red')
#conclusion is that it's secondary oxygen sensor that's wonky, no evidence that it's from salinity or temperature sensors. 

## these next stations do not have differences <-80, but still have a substantial ammount of scatter in secondary oxygen. 
plot(x = downcast$sbox0Mm.Kg[downcast$Station == 15], y = downcast$depSM[downcast$Station == 15], pch = 16, ylim = c(4000,0))
points(x = downcast$sbox1Mm.Kg[downcast$Station == 15], y = downcast$depSM[downcast$Station == 15], pch = 16, ylim = c(4000,0), col = 'red')

plot(x = downcast$sbox0Mm.Kg[downcast$Station == 30], y = downcast$depSM[downcast$Station == 30], pch = '.', ylim = c(2000,0), 
     xlab = 'Oxygen (umol/kg)', ylab = 'Pressure (dbar)', main = 'Station 30')
points(x = downcast$sbox1Mm.Kg[downcast$Station == 30], y = downcast$depSM[downcast$Station == 30], pch = '.', ylim = c(2000,0), col = 'red')

## do we see fluctuations in the t & conductivity profile in Barents Sea Branch Water
plot(downcast$t090C, y = downcast$prDM, pch = '.', ylim = c(2000,0))
plot(downcast$c0mS.cm, y = downcast$prDM, pch = '.', ylim = c(2000,0), xlim = c(28,30))

## Comparison of primary oxygen sensor and secondary oxygen sensor
par(plt = c(0.2, 0.85, 0.2, 0.9))
plot(x = downcast$sbox0Mm.Kg, y = downcast$sbox1Mm.Kg, 
     ylim = c(150,450), xlim = c(150,450),
     xlab = 'Primary Oxygen (umol/kg)', 
     ylab = 'Secondary Oxygen (umol/kg)',
     pch = '.', cex = 2, 
     col = make.pal(x = downcast$Station, min = 0, max = 45, n = 25, pal = 'brewer.ylorrd'))
abline(0,1, lty = 2)
colorbar(pal = brewer.ylorrd(25), zlim = c(0, 45))

## IMPORT CORRECTED BOTTLE & DOWNCAST FILES
samples = readRDS('../../Tasks/07 Data Submission Prep/NABOS/Dissolved Oxygen/2025 Oxygen Data Processing/20260618_NABOS2025 SAMPLES OxCorr.RDS')

## Comparison of Oxygen Calibration 
##intercept 19.61
##slope = 0.9733
##multiple r-squared = 0.9676


par(plt = c(0.2, 0.85, 0.2, 0.9))
plot(x = samples$Oxygen, y = samples$down.Oxy, pch = 20, cex = 0.9,
     xlab = 'Winkler Oxygen (umol/kg)', ylab = 'Downcast Oxygen (umol/kg)'
     #col = make.pal(x = samples$Depth, min = 0, max = 500, n = 25, pal = 'ocean.haline', rev = T)
     )
#points(x = samples$Oxygen, y = samples$down.Oxy, pch = 21)
abline(19.61,0.9733)
abline(0,1, lty = 2)
text(380, 295, paste0('m = ', '0.97'))
text(380, 300, paste0('b = ', '19.61'))
text(380, 290, paste0('r^2 = ', '0.9578'))

y = mx+b 

y - original = differnce/original

summary(((samples$down.Oxy.Corr-samples$down.Oxy)/samples$down.Oxy)*100)

n2321 = oce::read.ctd.sbe('./HLY2302_Station21_Cast2.cnv')
nab23st21 = as.data.frame(n2321@data)

plot(nab23st21$temperature, nab23st21$pressure, pch = '.', ylim = c(2500,0))

downcast = readRDS(file = 'C:/Users/lmwhi/Desktop/Automatic CTD Processing/output/downcast.RDS')
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

downcast$rho = calc.rho(S = downcast$sal11, 
                        Tmp = downcast$t190C, 
                        P = downcast$prDM)

downcast$ptemp = calc.ptemp(S = downcast$sal00, 
                            Tmp = downcast$t090C, 
                            P = downcast$prDM, 
                            P.ref = 0)

downcast$sigma = calc.sigma.theta(S = downcast$sal00, 
                                  Tmp = calc.ptemp(S = downcast$sal00, 
                                                   Tmp = downcast$t090C, 
                                                   P = downcast$prDM, 
                                                   P.ref = 0),
                                  P = 0)


stn = which(downcast$StationNo == 21)
par(mfrow = c(1,3))
plot(x = downcast$t090C[stn], 
     y = downcast$prDM[stn],
     ylim = c(max(downcast$prDM[stn], na.rm = T), 0),
     pch = 16, cex = 0.7, 
     xlab = 'Temperature [ITS-90] (deg C)',
     ylab = 'Pressure (dbar)', main = paste0('21', ', ', '1'))
plot(x = downcast$t190C[stn], 
     y = downcast$prDM[stn],
     ylim = c(max(downcast$prDM[stn], na.rm =T), 0),
     pch = 16, cex = 0.7, 
     xlab = 'Temperature2 [ITS-90] (deg C)',
     ylab = 'Pressure (dbar)', main = paste0('21', ', ', '1'))
plot(x = (downcast$t090C[stn] - downcast$t190C[stn]), 
     y = downcast$prDM[stn],
     ylim = c(max(downcast$prDM[stn], na.rm =T), 0),
     xlim = c(-0.1, 0.1), #xlim determined by quantile(abs(downcast$t090C - downcast$t190C), na.rm = T, probs = 0.99)
     pch = 16, cex = 0.7, 
     xlab = 'Difference between sensors (deg C)',
     ylab = 'Pressure (dbar)', main = paste0('21', ', ', '1'))

{
  stn = which(downcast$StationNo == 27)
  par(mfrow = c(1,5))
  plot(x = downcast$t090C[stn], 
       y = downcast$prDM[stn],
       ylim = c(max(downcast$prDM[stn], na.rm = T), 0),
       pch = 16, cex = 0.7, 
       xlab = 'Temperature [ITS-90] (deg C)',
       ylab = 'Pressure (dbar)', main = paste0('27', ', ', '1'))
  plot(x = downcast$c0mS.cm[stn], 
       y = downcast$prDM[stn],
       ylim = c(max(downcast$prDM[stn], na.rm =T), 0),
       pch = 16, cex = 0.7, 
       xlab = 'Conductivity (mS/cm)',
       ylab = 'Pressure (dbar)', main = paste0('27', ', ', '1'))
  plot(x = (downcast$rho[stn]), 
       y = downcast$prDM[stn],
       ylim = c(max(downcast$prDM[stn], na.rm =T), 0),
       pch = 20, cex = 0.7, 
       xlab = 'In-Situ Density (kg/m3)',
       ylab = 'Pressure (dbar)', main = paste0('27', ', ', '1'))
  plot(x = (downcast$sigma[stn]), 
       y = downcast$prDM[stn],
       ylim = c(max(downcast$prDM[stn], na.rm =T), 0),
       pch = 16, cex = 0.7, 
       xlab = 'Sigma-theta (kg/m3)',
       ylab = 'Pressure (dbar)', main = paste0('27', ', ', '1'))
  plot(x = (downcast$nbin[stn]), 
       y = downcast$prDM[stn],
       ylim = c(max(downcast$prDM[stn], na.rm =T), 0),
       pch = 16, cex = 0.7, xlim = c(0,30),
       xlab = 'data points per bin',
       ylab = 'Pressure (dbar)', main = paste0('27', ', ', '1'))
  
}

{
  stn = which(downcast$StationNo == 21)
  par(mfrow = c(1,5))
  plot(x = downcast$t090C[stn], 
       y = downcast$prDM[stn],
       ylim = c(max(downcast$prDM[stn], na.rm = T), 0),
       pch = 16, cex = 0.7, 
       xlab = 'Temperature [ITS-90] (deg C)',
       ylab = 'Pressure (dbar)', main = paste0('21', ', ', '1'))
  plot(x = downcast$c0mS.cm[stn], 
       y = downcast$prDM[stn],
       ylim = c(max(downcast$prDM[stn], na.rm =T), 0),
       pch = 16, cex = 0.7, 
       xlab = 'Conductivity (mS/cm)',
       ylab = 'Pressure (dbar)', main = paste0('21', ', ', '1'))
  plot(x = (downcast$rho[stn]), 
       y = downcast$prDM[stn],
       ylim = c(max(downcast$prDM[stn], na.rm =T), 0),
       pch = 20, cex = 0.7, 
       xlab = 'In-Situ Density (kg/m3)',
       ylab = 'Pressure (dbar)', main = paste0('21', ', ', '1'))
  plot(x = (downcast$sigma[stn]), 
       y = downcast$prDM[stn],
       ylim = c(max(downcast$prDM[stn], na.rm =T), 0),
       pch = 16, cex = 0.7, 
       xlab = 'Sigma-theta (kg/m3)',
       ylab = 'Pressure (dbar)', main = paste0('21', ', ', '1'))
  plot(x = (downcast$nbin[stn]), 
       y = downcast$prDM[stn],
       ylim = c(max(downcast$prDM[stn], na.rm =T), 0),
       pch = 16, cex = 0.7, xlim = c(0,30),
       xlab = 'data points per bin',
       ylab = 'Pressure (dbar)', main = paste0('21', ', ', '1'))
  
}

par(mfrow = c(1,1))
plot(x = (downcast$rho[stn]), 
     y = downcast$prDM[stn],
     ylim = c(max(downcast$prDM[stn], na.rm =T), 0),
     pch = 20, cex = 0.7, 
     xlab = 'In-Situ Density (kg/m3)',
     ylab = 'Pressure (dbar)', main = paste0('21', ', ', '1'))

{
  stn = which(downcast$StationNo == 38)
  par(mfrow = c(1,5))
  plot(x = downcast$t090C[stn], 
       y = downcast$prDM[stn],
       ylim = c(max(downcast$prDM[stn], na.rm = T), 0),
       pch = 16, cex = 0.7, 
       xlab = 'Temperature [ITS-90] (deg C)',
       ylab = 'Pressure (dbar)', main = paste0('38', ', ', '1'))
  plot(x = downcast$c0mS.cm[stn], 
       y = downcast$prDM[stn],
       ylim = c(max(downcast$prDM[stn], na.rm =T), 0),
       pch = 16, cex = 0.7, 
       xlab = 'Conductivity (mS/cm)',
       ylab = 'Pressure (dbar)', main = paste0('38', ', ', '1'))
  plot(x = (downcast$rho[stn]), 
       y = downcast$prDM[stn],
       ylim = c(max(downcast$prDM[stn], na.rm =T), 0),
       pch = 20, cex = 0.7, 
       xlab = 'In-Situ Density (kg/m3)',
       ylab = 'Pressure (dbar)', main = paste0('38', ', ', '1'))
  plot(x = (downcast$sigma[stn]), 
       y = downcast$prDM[stn],
       ylim = c(max(downcast$prDM[stn], na.rm =T), 0),
       pch = 16, cex = 0.7, 
       xlab = 'Sigma-theta (kg/m3)',
       ylab = 'Pressure (dbar)', main = paste0('38', ', ', '1'))
  plot(x = (downcast$nbin[stn]), 
       y = downcast$prDM[stn],
       ylim = c(max(downcast$prDM[stn], na.rm =T), 0),
       pch = 16, cex = 0.7, xlim = c(0,30),
       xlab = 'data points per bin',
       ylab = 'Pressure (dbar)', main = paste0('38', ', ', '1'))
  
}

par(mfrow = c(1,1))
plot(x = (downcast$rho[stn]), 
     y = downcast$prDM[stn],
     ylim = c(max(downcast$prDM[stn], na.rm =T), 0),
     pch = 20, cex = 0.7, 
     xlab = 'In-Situ Density (kg/m3)',
     ylab = 'Pressure (dbar)', main = paste0('38', ', ', '1'))


par(mfrow = c(1,1))
stn = which(downcast$StationNo == 21)
plot.TS(S = downcast$sal00[stn], 
        Tmp = downcast$ptemp[stn], 
        pch = '.', cex = 2,
        xlim = c(32,35.5),
        ylim = c(-2,2), 
        levels = seq(25.5, 29, by = 0.5))

stn = which(downcast$StationNo == 21 & downcast$depSM > 600 & downcast$depSM <1000)
plot.TS(S = downcast$sal00[stn][], 
        Tmp = downcast$ptemp[stn], 
        pch = '.', cex = 2,
        xlim = c(34.8,34.9),
        ylim = c(-0.4,0.8), 
        levels = seq(27, 29, by = 0.02))


stn = which(downcast$StationNo == 21)
plot(downcast$sigma[stn], downcast$prDM[stn], pch = '.',
     ylab = 'Pressure (dbar)', xlab = 'Sigma-theta (kg/m3)',
     ylim = c(2500,0))

stn = which(downcast$StationNo == 21 & downcast$depSM > 600 & downcast$depSM <1000)
plot(downcast$sigma[stn], downcast$depSM[stn], pch = '.',
     ylab = 'Pressure (dbar)', xlab = 'Sigma-theta (kg/m3)',
     ylim = c(1000,600))
grid()

plot(downcast$t090C[l], downcast$prDM[l], pch = '.',
     ylim = c(1200,0), xlab = 'Temperature', ylab = 'Pressure')
grid()



location = unique(data.frame(StationNo = downcast$StationNo,
                             Latitude = round(downcast$latitude, digits = 1),
                             Longitude = round(downcast$longitude, digits = 1)))


map = plotBasemap(lon = 160, lat = 78, scale = 1300)
map = addLatitude(basemap = map)
map = addLongitude(basemap = map)
map = addText(basemap = map, 
              lon = location$Longitude, 
              lat = location$Latitude, 
              label = location$StationNo, cex = 0.8)

#transect 1 = stations 1 - 15 (plot x axis = latitude)
#transect 2 = station 10, 16-39 (plot x axis = longitude)
#transect 3 = station 38-45 (plot x axis = latitude)

{ ## Overview Temperature for Transect 1
t1 = which(downcast$StationNo < 16 & downcast$StationNo > 0)
grid = buildGrid(xlim = range(downcast$latitude[t1]), ylim = c(0, 3000), nx = 50, ny = 50)
grid = appendData(grid = grid, 
                  x = downcast$latitude[t1], 
                  y = downcast$prDM[t1], 
                  z = downcast$t190C[t1], 
                  label = 'Temperature2')
grid = setGridder(grid = grid, gridder = gridWeighted)
grid = interpData(grid = grid)

plotGrid(section = grid, label = 'Temperature2', ylim = c(3000,0), pal = ocean.thermal(n = 25))
#addMask(grid) needs update in simple gridder to add bathy mask
}

{ ## Temperature2 for area of squiggles
  t1 = which(downcast$StationNo < 16 & downcast$StationNo > 0)
  grid = buildGrid(xlim = c(75.5, 78.5), ylim = c(400, 1200), nx = 50, ny = 500)
  grid = appendData(grid = grid, 
                    x = downcast$latitude[t1], 
                    y = downcast$prDM[t1], 
                    z = downcast$t190C[t1], 
                    label = 'Temperature2')
  grid = setGridder(grid = grid, gridder = gridWeighted)
  grid = interpData(grid = grid)
  
  par(plt = c(0.1,0.9,0.1,0.9))
  plotGrid(section = grid, label = 'Temperature2', 
           ylim = c(1200,400), 
           zlim = c(-0.5, 1.0), 
           pal = ocean.thermal(n = 25))
  #addMask(grid) needs update in simple gridder to add bathy mask
  points(x = grid$data$x, y = grid$data$y, pch = '.')
  addContour(grid, 'Temperature2')
}


{ ## Temperature2 for area of squiggles in t2
  t2 = which(downcast$StationNo %in% c(10, 16:39))
  grid = buildGrid(xlim = c(130,180), ylim = c(400, 1200), nx = 50, ny = 500)
  grid = appendData(grid = grid, 
                    x = downcast$longitude[t2], 
                    y = downcast$prDM[t2], 
                    z = downcast$t190C[t2], 
                    label = 'Temperature2')
  grid = setGridder(grid = grid, gridder = gridWeighted)
  grid = interpData(grid = grid)
  
  par(plt = c(0.1,0.9,0.1,0.9))
  plotGrid(section = grid, label = 'Temperature2', 
           ylim = c(1200,400),  
           zlim = c(-0.5, 1.0), 
           pal = ocean.thermal(n = 25))
  #addMask(grid) needs update in simple gridder to add bathy mask
  points(x = grid$data$x, y = grid$data$y, pch = '.')
  addContour(grid, 'Temperature2')
}

{ ## Temperature2 for area of squiggles in t3
  t3 = which(downcast$StationNo < 46 & downcast$StationNo > 37)
  grid = buildGrid(xlim = range(downcast$latitude[t3]), ylim = c(400, 1200), nx = 50, ny = 500)
  grid = appendData(grid = grid, 
                    x = downcast$latitude[t3], 
                    y = downcast$prDM[t3], 
                    z = downcast$t190C[t3], 
                    label = 'Temperature2')
  grid = setGridder(grid = grid, gridder = gridWeighted)
  grid = interpData(grid = grid)
  
  par(plt = c(0.1,0.85,0.1,0.9))
  plotGrid(section = grid, label = 'Temperature2', 
           ylim = c(1200,400), 
           zlim = c(-0.5, 1.0),
           pal = ocean.thermal(n = 25))
  #addMask(grid) needs update in simple gridder to add bathy mask
  points(x = grid$data$x, y = grid$data$y, pch = '.')
  addContour(grid, 'Temperature2')
  colorbar(pal = ocean.thermal(25), zlim = c(-0.5, 1))

}

range(grid$interp$Temperature2)


#### Check Chlorophyll