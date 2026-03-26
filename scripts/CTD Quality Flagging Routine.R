

plot(x = downcast$CTDOXY..μmol.kg., y = downcast$CTDPRS..dbar., 
     pch = 16, cex = 0.7, 
     ylim = c(7000,0),
     xlab = 'CTD Oxygen (umol/kg)',
     ylab = 'Pressure (dbar)')


plot(x = downcast$CTDOXY2..μmol.kg., y = downcast$CTDPRS..dbar., 
     pch = 16, cex = 0.7, 
     ylim = c(7000,0),
     xlab = 'CTD Oxygen (umol/kg)',
     ylab = 'Pressure (dbar)')


plot(x = downcast$CTDTMP..ITS.90., y = downcast$CTDPRS..dbar., 
     pch = 16, cex = 0.7, 
     ylim = c(7000,0),
     xlab = 'CTD Temperature (umol/kg)',
     ylab = 'Pressure (dbar)')


plot(x = downcast$CTDTMP2..ITS.90., y = downcast$CTDPRS..dbar., 
     pch = 16, cex = 0.7, 
     ylim = c(7000,0),
     xlab = 'CTD Temperature (umol/kg)',
     ylab = 'Pressure (dbar)')


## Station by station plots

downcast = downcast[order(downcast$Station, downcast$Cast),]
downcast$Station = as.numeric(downcast$Station)

for (s in unique(downcast$Station)) {
  l = which(downcast$Station == s)
  for (c in unique(downcast$Cast[l])) {
    j = which(downcast$Cast[l] == c)
    par(mfrow = c(1,3))
    plot(x = downcast$CTDTMP..ITS.90.[l[j]], 
         y = downcast$CTDPRS..dbar.[l[j]],
         ylim = c(max(downcast$CTDPRS..dbar.[l[j]], na.rm = T), 0),
         pch = 16, cex = 0.7, 
         xlab = 'Temperature [ITS-90] (deg C)',
         ylab = 'Pressure (dbar)', main = paste0(s, ', ', c))
    plot(x = downcast$CTDTMP2..ITS.90.[l[j]], 
         y = downcast$CTDPRS..dbar.[l[j]],
         ylim = c(max(downcast$CTDPRS..dbar.[l[j]], na.rm =T), 0),
         pch = 16, cex = 0.7, 
         xlab = 'Temperature2 [ITS-90] (deg C)',
         ylab = 'Pressure (dbar)', main = paste0(s, ', ', c))
    plot(x = (downcast$CTDTMP..ITS.90.[l[j]] - downcast$CTDTMP2..ITS.90.[l[j]]), 
         y = downcast$CTDPRS..dbar.[l[j]],
         ylim = c(max(downcast$CTDPRS..dbar.[l[j]], na.rm =T), 0),
         pch = 16, cex = 0.7, 
         xlab = 'Difference between sensors',
         ylab = 'Pressure (dbar)', main = paste0(s, ', ', c))
  }
}
#At station 38, the difference between the sensors begins to take the shape of the profile...
# pressure effects become more obvious as well.  


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

downcast$CTDPRS.Flag = NA
downcast$CTDTMP1.Flag = NA
downcast$CTDTMP2.Flag = NA
downcast$CTDSAL1.Flag = NA
downcast$CTDSAL2.Flag = NA
downcast$CTDCOND1.Flag = NA
downcast$CTDCOND2.Flag = NA
downcast$CTDOXY1.Flag = NA
downcast$CTDOXY2.Flag = NA

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