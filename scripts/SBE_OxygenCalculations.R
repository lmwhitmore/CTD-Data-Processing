##Laura M Whitmore
## Compare output of Oxygen for sea-bird calculated and "R-calculated" oxygen
## NABOS 2023 Cruise 
## Original: Feb 20, 2025

library(openxlsx)
library(xml2)

#Note, the processing script: Automatic CTD processing (NABOS2025) needs to be 
#run prior to this Oxygen correction calculation 

#### Load Data ####
downcast = readRDS('./output/2025/downcast.RDS')

for(i in 1:nrow(downcast)) {
  downcast$StationNo[i] = strsplit(downcast$Station[i], 'station')[[1]][2]
  downcast$CastNo[i] = strsplit(downcast$Cast[i], 'cast')[[1]][2]
  
}

downcast$StationNo = as.numeric(downcast$StationNo)
downcast$CastNo = as.numeric(downcast$CastNo)


bottle = readRDS('./output/2025/bottles.RDS')

for(i in 1:nrow(bottle)) {
  bottle$StationNo[i] = strsplit(bottle$Station[i], 'station')[[1]][2]
  bottle$CastNo[i] = strsplit(bottle$Cast[i], 'cast')[[1]][2]
  
}

bottle$StationNo = as.numeric(bottle$StationNo)
bottle$CastNo = as.numeric(bottle$CastNo)


samples = read.xlsx('../Oxygen Data Processing/Oxygen/NABOS Oxygen Titration Log.xlsx', 
                    sheet = 'Samples', startRow = 2, check.names = T)
samples$Oxygen[samples$Flag !=1] = NA


#### Functions ####

## Required for the SBE Calculation of Oxygen (ml/l) from voltage
calc.oxygen.sol = function(Salinity, Temperature, GordonGarcia = T) {
  
  S = Salinity #(psu) 
  Temperature = Temperature #(ITS-90, deg C) 
  ##Gordon & Garcia 1992
  #Oxsol(T,S) = oxygen saturation value (ml/l) 
  if (GordonGarcia) {
    
    #Parameters
    Ts = log((298.15 - Temperature)/(273.15 + Temperature)) 
    A0 = 2.00907
    A1 = 3.22014     
    A2 = 4.0501
    A3 = 4.94457   
    A4 = - 0.256847   
    A5 = 3.88767 
    B0 = -0.00624523    
    B1 = -0.00737614
    B2 = -0.010341        
    B3 = -0.00817083 
    C0 = -0.000000488682
    
    #calculation (oxygen saturation in mL/L)
    Oxsol = exp(A0 + A1*Ts + A2*(Ts)^2 + A3*(Ts)^3 + A4*(Ts)^4 + A5*(Ts)^5
                + S*(B0 + B1*(Ts) + B2*(Ts)^2 + B3*(Ts)^3) + C0*(S)^2)
  }
  
  else {
    #Parameters
    Ta = (Temperature + 273.15) #absolute water temperature
    A1 = -173.4292     
    A2 = 249.6339     
    A3 = 143.3483     
    A4 = -21.8492 
    B1 = -0.033096     
    B2 = 0.014259      
    B3 = -0.00170 
    
    #Equation Weiss (seabird says this equation should use ITPS-68, rather than 90 for Garcia & Gordon)
    #oxygen saturation in mL/L)
    Oxsol = exp((A1 + A2*(100/Ta) + A3*ln(Ta/100) + A4*( Ta/100))
                + S*(B1 + B2*(Ta/100) + B3*(Ta/100)^2))
    
  }
  
  #Output
  Oxsol
  
}

## Calculate oxygen concentration (ml/L) from voltage (ignore tau correction)
calc.ctd.oxygen = function(SensorCalibration, Oxygen.V, Temperature, Salinity, Pressure){
  
  #oxygen(ml/L) = Soc*(V+Voffset+tau(T,P)*dV/dt) * Oxsol(T,S)*(1.0+A*T+B*T^2+C*T3)*e^((E*P)/K)  
  #this ignores the tau correction which is part of the first term: tau(T,P)*dV/dt 
  
  #Parameters
  Soc = SensorCalibration$SeaBird.Coefficients$Soc
  V = Oxygen.V #Corrected for hysteresis in SBEDataProcessing, but not for Tau
  Voffset = SensorCalibration$SeaBird.Coefficients$offset
  A = SensorCalibration$SeaBird.Coefficients$A
  B = SensorCalibration$SeaBird.Coefficients$B
  C = SensorCalibration$SeaBird.Coefficients$C
  E = SensorCalibration$SeaBird.Coefficients$E
  K = Temperature + 273.15
  P = Pressure
  Oxsol = calc.oxygen.sol(Salinity = Salinity, Temperature = Temperature)
  
  
  #Calculation
  CTDOXY = Soc*(V+Voffset) * 
    Oxsol*(1.0 + A*Temperature + B*Temperature^2 + C*Temperature^3)*exp((E*P)/K)
  
  #Output
  CTDOXY
}

## convert from ml/l to umol/kg
calc.oxygen.units = function(oxygen.mL.L, Salinity, Temperature, Pressure) {
  
  sigma.theta = calc.sigma.theta(S = Salinity, Tmp = Temperature, P = Pressure, verbose = F)
  oxygen.umol.kg = 44660*oxygen.mL.L/(sigma.theta + 1000)
  
  #output
  oxygen.umol.kg
}


#### Manually Enter XMLCON data for Oxygen Sensors #### 

{#Calibration info for sensor ### Would be cool to get this from a "read.xmlcon" function
  ##PRIMARY OXYGEN
  SBE43.0458 = list(instrument = data.frame(Sensor.index = 5, 
                                               SensorID = 38,
                                               SensorType = 'Oxygen Sensor',
                                               SerialNo = 0458,
                                               CalibrationDate = '02-June-23',
                                               Equation = 1), #sensor newer than 2007, use SB calculation 
                       Owens.Millard.Coefficients = data.frame(equation = 0,
                                                               Boc = 0.0000, 
                                                               Soc = 0.0000e+000,
                                                               offset = 0.0000,
                                                               Pcor = 0.00e+000, 
                                                               Tcor = 0.0000, 
                                                               Tau = 0.0), 
                       SeaBird.Coefficients = data.frame(equation = 1, 
                                                         Soc = 5.0460e-001,
                                                         offset = -0.4857,
                                                         A = -4.2535e-003,
                                                         B = 1.8926e-004,
                                                         C = -2.8479e-006,
                                                         D0 = 2.5826e+000,
                                                         D1 = 1.92634e-004,
                                                         D2 = -4.64803e-002,
                                                         E = 3.6000e-002,
                                                         Tau20 = 1.5500,
                                                         H1 = -3.3000e-002,
                                                         H2 = 5.0000e+003,
                                                         H3 = 1.4500e+003)
  )
  
}


{#Calibration info for sensor
  ## SECONDARY OXYGEN
  SBE43.0456 = list(instrument = data.frame(Sensor.index = 6, 
                                               SensorID = 38,
                                               SensorType = 'Oxygen Sensor',
                                               SerialNo = 0456,
                                               CalibrationDate = '24-May-23',
                                               Equation = 1), #sensor newer than 2007, use SB calculation
                       Owens.Millard.Coefficients = data.frame(equation = 0,
                                                               Boc = 0.0000, 
                                                               Soc = 0.0000e+000,
                                                               offset = 0.0000,
                                                               Pcor = 0.00e+000, 
                                                               Tcor = 0.0000, 
                                                               Tau = 0.0), 
                       SeaBird.Coefficients = data.frame(equation = 1, 
                                                         Soc = 5.4502e-001,
                                                         offset = -0.5052,
                                                         A = -4.4639e-003,
                                                         B = 1.9207e-004,
                                                         C = -3.0757e-006,
                                                         D0 = 2.5826e+000,
                                                         D1 = 1.92634e-004,
                                                         D2 = -4.64803e-002,
                                                         E = 3.6000e-002,
                                                         Tau20 = 1.2600,
                                                         H1 =-3.3000e-002,
                                                         H2 = 5.0000e+003,
                                                         H3 = 1.4500e+003)
  )
  
}

#### PRELIMINARY PLOTS TO IDENTIFY ISSUES #### 


#### CALCULATE OXYGEN.CTD from Voltage ####

#Note, secondary oxygen has a major offset during a portion of the cruise. 
#Primary oxygen looks good, BUT, is wonky due to issues with primary temperature 
#when calculated by SBEDataProcessingTools. 

## Calc Oxygen (ml/L) from primary oxygen & secondary Temperature & Salinity
oxygen.0458 = calc.ctd.oxygen(SensorCalibration = SBE43.0458, #Primary oxygen sensor
                              Oxygen.V = downcast$sbeox0V, #Primary Oxygen voltage
                              Salinity = downcast$sal11, #secondary salinity
                              Temperature = downcast$t190C, #secondary temperature
                              Pressure = downcast$prDM
                              )
#Convert Oxygen from ml/l to umol/kg
downcast$Oxygen.CTD = calc.oxygen.units(oxygen.0458, 
                                        Salinity = downcast$sal11, 
                                        Temperature = downcast$t190C, 
                                        Pressure = downcast$prDM)

plot(x = downcast$Oxygen.CTD, y = downcast$sbox0Mm.Kg, pch = '.')
#known issues calculating sbox0mm.kg in seabird, Temp channel 1 == bad. 

## repeat for bottle data 
oxygen.0458b = calc.ctd.oxygen(SensorCalibration = SBE43.0458, #Primary oxygen sensor
                              Oxygen.V = bottle$Sbeox0V, #Primary Oxygen voltage
                              Salinity = bottle$Sal11, #secondary salinity
                              Temperature = bottle$T190C, #secondary temperature
                              Pressure = bottle$PrDM
)
#Convert Oxygen from ml/l to umol/kg
bottle$Oxygen.CTD = calc.oxygen.units(oxygen.0458b, 
                                        Salinity = bottle$Sal11, 
                                        Temperature = bottle$T190C, 
                                        Pressure = bottle$PrDM)

points(x = bottle$Oxygen.CTD, y = bottle$Sbox0Mm.Kg, pch = 20, col = 'red')
plot(x = bottle$Oxygen.CTD, y = bottle$Sbox0Mm.Kg, pch = '.')


#### Match Observational Data
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


samples$Oxygen.CTD = NA
for (i in 1:nrow(samples)) {
  l = which(downcast$StationNo == samples$Stn[i] & 
               downcast$CastNo == samples$Cast[i] &
               round(downcast$depSM) == round(samples$Depth)[i]
            )
  if (length(l) > 0) {
    samples$Oxygen.CTD[i] = downcast$Oxygen.CTD[l]
    message('here')
  }
  if (length(l) > 1) {
    message(samples$Stn[i], ' ', samples$Depth[i], ' has more than one match.')
  }
  
}


samples$Oxygen.Bottle.CTD = NA
for (i in 1:nrow(samples)) {
  l = which(bottle$StationNo == samples$Stn[i] & 
              bottle$CastNo == samples$Cast[i] &
              bottle$Bottle == samples$Niskin[i]
  )
  if (length(l) > 0) {
    samples$Oxygen.Bottle.CTD[i] = bottle$Oxygen.CTD[l]
    message('here')
  }
  if (length(l) > 1) {
    message(samples$Stn[i], ' ', samples$Niskin[i], ' has more than one match.')
  }
  
}

plot(samples$Oxygen.Bottle.CTD, samples$Oxygen.CTD, pch = 20)
abline(0,1, lty = 2)

#plot(oxygen.0456, oxygen.0458, pch = '.', col = '#00000030')

plot(x = downcast$sbox1Mm.Kg, y = downcast$sbox0Mm.Kg, 
     pch = '.', col = '#00000030',
     xlim= c(150, 450), ylim = c(150,450))
points(x = bottle$Sbox1Mm.Kg,  y = bottle$Sbox0Mm.Kg, 
       pch = 20, col = 'black')
points(x = bottle$Sbox1Mm.Kg,  y = bottle$Oxygen.Meas, 
       pch = 20, col = 'red')
points(x = bottle$Oxygen.Meas,  y = bottle$Sbox0Mm.Kg, 
       pch = 20, col = 'blue')

plot(x = bottle$Oxygen.Meas,  y = bottle$Sbox0Mm.Kg, 
       pch = 20, col = 'blue', ylim = c(200, 400))

calibration = lm(samples$Oxygen.CTD ~ samples$Oxygen)
summary(calibration)

plot(x = samples$Oxygen, y = samples$Oxygen.CTD, pch = 20)
abline(calibration)
abline(0,1, lty = 2)
plot(x = samples$Oxygen.Bottle.CTD-samples$Oxygen.CTD, y = samples$Depth)
plot(x = samples$Oxygen-samples$Oxygen.CTD, y = samples$Depth)


calibration.bottle = lm(samples$Oxygen.Bottle.CTD ~ samples$Oxygen)
plot(x = samples$Oxygen, y = samples$Oxygen.Bottle.CTD, pch = 20)
abline(0,1, lty = 2)
abline(calibration.bottle)


#
#corrected oxygen CTD = (downcast$Oxygen.CTD - intercept)/slope
downcast$Oxygen.Corrected = (downcast$Oxygen.CTD - calibration.bottle$coefficients[1])/calibration.bottle$coefficients[2]
bottle$Oxygen.Corrected = (bottle$Oxygen.CTD - calibration.bottle$coefficients[1])/calibration.bottle$coefficients[2]

samples$Oxygen.Corrected = (samples$Oxygen.CTD - calibration.bottle$coefficients[1])/calibration.bottle$coefficients[2]
points(x = samples$Oxygen, y = samples$Oxygen.Corrected, pch = 20, cex = 0.8, col = 'green')



summary(samples$Oxygen - samples$Oxygen.CTD)
summary(samples$Oxygen - samples$Oxygen.Corrected)

#### WRITE DATA FILES #### 

write.xlsx(x = downcast, file = paste0(out.dir, '/', 'downcast_oxcorr.xlsx'))
saveRDS(downcast, file = paste0(out.dir, '/', 'downcast_oxcorr.RDS'))

write.xlsx(x = bottle, file = paste0(out.dir, '/', 'bottle_oxcorr.xlsx'))
saveRDS(bottle, file = paste0(out.dir, '/', 'bottle_oxcorr.RDS'))


#### EXTRAS #### 
## SECONDARY OXYGEN SENSOR
oxygen.0456 = calc.ctd.oxygen(SensorCalibration = SBE43.0456, 
                              Oxygen.V = downcast$sbeox1V,
                              Salinity = downcast$sal11,
                              Temperature = downcast$t190C, 
                              Pressure = downcast$prDM)


plot(oxygen.0456, oxygen.0458, pch = '.', col = '#00000030')



plot(downcast$sbox1Mm.Kg, downcast$sbox0Mm.Kg, pch = '.', col = '#00000030',
     xlim= c(0, 450), ylim = c(0,450))

plot(downcast$sbox1Mm.Kg, oxygen.0456, pch = '.', col = '#00000030')

