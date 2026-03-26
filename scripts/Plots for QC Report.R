library(SimpleMapper)
library(SimpleGridder)
library(pals)
library(oce)

source("https://raw.githubusercontent.com/tbrycekelly/TheSource/refs/heads/master/R/source.physics.r")
source('https://raw.githubusercontent.com/tbrycekelly/TheSource/refs/heads/master/R/pal.R')

## Temperature2-Temperature1 difference profiles with a color gradient for station no. 
TempDiff = downcast$t190C-downcast$t090C

par(plt = c(0.2, 0.85, 0.2, 0.9))
plot(x = TempDiff, y = downcast$prDM, 
     ylim = c(4500,0), xlim = c(-0.05, 0.05),
     xlab = 'Difference in Primary & Secondary Temperature (deg C)', 
     ylab = 'Pressure [dbar]',
     pch = '.', cex = 2, 
     col = make.pal(x = downcast$StationNo, min = 0, max = 45, n = 25, pal = 'brewer.ylorrd'))

colorbar(pal = brewer.ylorrd(25), zlim = c(0, 45))

## Comparison of primary oxygen sensor and secondary oxygen sensor
par(plt = c(0.2, 0.85, 0.2, 0.9))
plot(x = downcast$sbox0Mm.Kg, y = downcast$sbox1Mm.Kg, 
     ylim = c(150,450), xlim = c(150,450),
     xlab = 'Primary Oxygen (umol/kg)', 
     ylab = 'Secondary Oxygen (umol/kg)',
     pch = '.', cex = 2, 
     col = make.pal(x = downcast$StationNo, min = 0, max = 45, n = 25, pal = 'brewer.ylorrd'))
abline(0,1, lty = 2)
colorbar(pal = brewer.ylorrd(25), zlim = c(0, 45))


## Comparison of Oxygen Calibration 
par(mfrow = c(1,2))
par(plt = c(0.2, 0.85, 0.2, 0.9))
plot(x = samples$Oxygen, y = samples$Oxygen.CTD, pch = 20,
     xlab = 'Winkler Oxygen (umol/kg)', ylab = 'Downcast Oxygen (umol/kg)',
     col = make.pal(x = samples$Depth, min = 0, max = 500, n = 25, pal = 'ocean.haline', rev = T))
points(x = samples$Oxygen, y = samples$Oxygen.CTD, pch = 21)
abline(calibration)
abline(0,1, lty = 2)
text(320, 295, paste0('m = ', round(calibration$coefficients[2], digits = 2)))
text(320, 300, paste0('b = ', round(calibration$coefficients[1], digits = 2)))
text(320, 290, paste0('r^2 = ', 0.9578))


plot(x = samples$Oxygen, y = samples$Oxygen.Bottle.CTD, pch = 20,
     xlab = 'Winkler Oxygen (umol/kg)', ylab = 'Upcast/Bottle Oxygen (umol/kg)',
     col = make.pal(x = samples$Depth, min = 0, max = 500, n = 25, pal = 'ocean.haline', rev = T))
points(x = samples$Oxygen, y = samples$Oxygen.Bottle.CTD, pch = 21)
abline(0,1, lty = 2)
abline(calibration.bottle)
text(320, 295, paste0('m = ', round(calibration.bottle$coefficients[2], digits = 2)))
text(320, 300, paste0('b = ', round(calibration.bottle$coefficients[1], digits = 2)))
text(320, 290, paste0('r^2 = ', 0.9903))

colorbar(pal = rev(ocean.haline(25)), zlim = c(0, 500))

#plot(x = samples$Oxygen.Bottle.CTD-samples$Oxygen.CTD, y = samples$Depth)
#plot(x = samples$Oxygen-samples$Oxygen.CTD, y = samples$Depth)


calibration.bottle = lm(samples$Oxygen.Bottle.CTD ~ samples$Oxygen)




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

