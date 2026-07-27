## Generate Cruise "Hydrochemistry" File 
## Updates: L.Whitmore June 19, 2026

rm(list = ls())
library(openxlsx)
source('scripts/source_NABOS.R')

## Import Inventory
inventory = read.xlsx('~/Documents/Tasks/07 Data Submission Prep/NABOS/Logsheets and Inventories/NABOS2025_Rosette and Cast Log.xlsx')

## Import Intermediate Bottle File
bottle = readRDS('./output/intermediate stage/NABOS2025_20260721_INTERMEDIATE_Bottle.RDS')

## Import Macronutrients 
nutrient = read.xlsx('~/Documents/Tasks/07 Data Submission Prep/NABOS/Macronutrient /NABOS2025 UAF/UAF_NAF_NABOS2025_NutrientDataReport_04122026.xlsx', startRow = 8)
nutrient.units = nutrient[1,]
nutrient.lod = nutrient[2,]

nutrient = nutrient[-1,]
nutrient = nutrient[-1,]

colnames(nutrient)[1] = "Index"
colnames(nutrient)[2] = "Fund_Org"
colnames(nutrient)[3] = "SampleID"
colnames(nutrient)[12] = "total_nitrogen"

## import event log
event = read.xlsx('~/Documents/Tasks/07 Data Submission Prep/NABOS/Event Log/ShipLogger 2025-10-05 151146.xlsx')

## Set up DataFrame 
merge = data.frame(Cruise = 'NABOS2025', 
                   Event_Key = NA, 
                   Datetime = NA,
                   Station = inventory$Station, 
                   Cast = inventory$Cast, 
                   Niskin = inventory$Niskin.Position,
                   SampleID = inventory$Sample.ID,
                   Longitude = NA,
                   Latitude = NA,
                   Depth = NA, 
                   Pressure = NA,
                   CTD.Conductivity = NA,
                   CTD.Salinity = NA,
                   CTD.Temperature = NA,
                   CTD.Oxygen = NA,
                   CTD.Oxygen.Corr = NA,
                   CTD.ChlFluor.V = NA,
                   CTD.ChlFluor.Unit = NA,
                   CTD.CDOMFluor.V = NA,
                   CTD.CDOMFluor.Unit = NA, 
                   CTD.Transmissometry.Unit = NA,
                   CTD.Transmissometry.V = NA,
                   CTD.Altimeter = NA,
                   Total_Nitrogen = NA,
                   Total_Nitrogen.Flag = inventory$Nuts,
                   Nitrate = NA,
                   Nitrate.Flag = inventory$Nuts,
                   Nitrite = NA,
                   Nitrite.Flag = inventory$Nuts,
                   Ammonium = NA, 
                   Ammonium.Flag = inventory$Nuts,
                   Phosphate = NA,
                   Phosphate.Flag = inventory$Nuts,
                   Silicate = NA,
                   Silicate.Flag = inventory$Nuts
                   
)

## Merge Bottle File Parameters 
#Longitude, Latitude, Pressure, Depth, Cond1, Cond2, Sal1, Sal2, Temp1, Temp2 
{
  merge$Datetime = add.var.scn(master.station = merge$Station, look.station = bottle$Station,
                                master.cast = merge$Cast, look.cast = bottle$Cast,
                                master.niskin = merge$Niskin, look.niskin = bottle$Bottle,
                                master.val = merge$Datetime, look.val = bottle$datetime)
  merge$Datetime = as.POSIXct(merge$Datetime)
  merge$Longitude = add.var.scn(master.station = merge$Station, look.station = bottle$Station,
                                master.cast = merge$Cast, look.cast = bottle$Cast,
                                master.niskin = merge$Niskin, look.niskin = bottle$Bottle,
                                master.val = merge$Longitude, look.val = bottle$Longitude)
  
  merge$Latitude = add.var.scn(master.station = merge$Station, look.station = bottle$Station,
                               master.cast = merge$Cast, look.cast = bottle$Cast,
                               master.niskin = merge$Niskin, look.niskin = bottle$Bottle,
                               master.val = merge$Latitude, look.val = bottle$Latitude)
  
  merge$Depth = add.var.scn(master.station = merge$Station, look.station = bottle$Station,
                            master.cast = merge$Cast, look.cast = bottle$Cast,
                            master.niskin = merge$Niskin, look.niskin = bottle$Bottle,
                            master.val = merge$Depth, look.val = bottle$DepSM)
  
  merge$Pressure = add.var.scn(master.station = merge$Station, look.station = bottle$Station,
                               master.cast = merge$Cast, look.cast = bottle$Cast,
                               master.niskin = merge$Niskin, look.niskin = bottle$Bottle,
                               master.val = merge$Pressure, look.val = bottle$PrDM)
  
  merge$CTD.Conductivity = add.var.scn(master.station = merge$Station, look.station = bottle$Station,
                                        master.cast = merge$Cast, look.cast = bottle$Cast,
                                        master.niskin = merge$Niskin, look.niskin = bottle$Bottle,
                                        master.val = merge$CTD.Conductivity, look.val = bottle$C0mS.cm)

  merge$CTD.Salinity = add.var.scn(master.station = merge$Station, look.station = bottle$Station,
                                    master.cast = merge$Cast, look.cast = bottle$Cast,
                                    master.niskin = merge$Niskin, look.niskin = bottle$Bottle,
                                    master.val = merge$CTD.Salinity, look.val = bottle$Sal00)
  
  merge$CTD.Temperature = add.var.scn(master.station = merge$Station, look.station = bottle$Station,
                                       master.cast = merge$Cast, look.cast = bottle$Cast,
                                       master.niskin = merge$Niskin, look.niskin = bottle$Bottle,
                                       master.val = merge$CTD.Temperature, look.val = bottle$T090C)

  
  merge$CTD.Oxygen = add.var.scn(master.station = merge$Station, look.station = bottle$Station,
                                  master.cast = merge$Cast, look.cast = bottle$Cast,
                                  master.niskin = merge$Niskin, look.niskin = bottle$Bottle,
                                  master.val = merge$CTD.Oxygen, look.val = bottle$Sbox0Mm.Kg)
  merge$CTD.Oxygen.Corr = add.var.scn(master.station = merge$Station, look.station = bottle$Station,
                                 master.cast = merge$Cast, look.cast = bottle$Cast,
                                 master.niskin = merge$Niskin, look.niskin = bottle$Bottle,
                                 master.val = merge$CTD.Oxygen.Corr, look.val = bottle$Oxygen.Corr)

  
  merge$CTD.ChlFluor.V = add.var.scn(master.station = merge$Station, look.station = bottle$Station,
                                     master.cast = merge$Cast, look.cast = bottle$Cast,
                                     master.niskin = merge$Niskin, look.niskin = bottle$Bottle,
                                     master.val = merge$CTD.ChlFluor.V, look.val = bottle$V2)
  merge$CTD.ChlFluor.Unit = add.var.scn(master.station = merge$Station, look.station = bottle$Station,
                                        master.cast = merge$Cast, look.cast = bottle$Cast,
                                        master.niskin = merge$Niskin, look.niskin = bottle$Bottle,
                                        master.val = merge$CTD.ChlFluor.Unit, look.val = bottle$FlECO.AFL)
  
  merge$CTD.CDOMFluor.V = add.var.scn(master.station = merge$Station, look.station = bottle$Station,
                                      master.cast = merge$Cast, look.cast = bottle$Cast,
                                      master.niskin = merge$Niskin, look.niskin = bottle$Bottle,
                                      master.val = merge$CTD.CDOMFluor.V, look.val = bottle$V6)
  merge$CTD.CDOMFluor.Unit = add.var.scn(master.station = merge$Station, look.station = bottle$Station,
                                         master.cast = merge$Cast, look.cast = bottle$Cast,
                                         master.niskin = merge$Niskin, look.niskin = bottle$Bottle,
                                         master.val = merge$CTD.CDOMFluor.Unit, look.val = bottle$WetCDOM)
  
  
  merge$CTD.Transmissometry.Unit= add.var.scn(master.station = merge$Station, look.station = bottle$Station,
                                              master.cast = merge$Cast, look.cast = bottle$Cast,
                                              master.niskin = merge$Niskin, look.niskin = bottle$Bottle,
                                              master.val = merge$CTD.Transmissometry.Unit, look.val = bottle$CStarAt0)
  merge$CTD.Transmissometry.V= add.var.scn(master.station = merge$Station, look.station = bottle$Station,
                                           master.cast = merge$Cast, look.cast = bottle$Cast,
                                           master.niskin = merge$Niskin, look.niskin = bottle$Bottle,
                                           master.val = merge$CTD.Transmissometry.V, look.val = bottle$V3)
  
  merge$CTD.Altimeter= add.var.scn(master.station = merge$Station, look.station = bottle$Station,
                                   master.cast = merge$Cast, look.cast = bottle$Cast,
                                   master.niskin = merge$Niskin, look.niskin = bottle$Bottle,
                                   master.val = merge$CTD.Altimeter, look.val = bottle$AltM)
}

## Merge Nutrients 

for (i in 1:nrow(merge)) {
  l = which(merge$SampleID[i] == nutrient$SampleID)
  if (length(l) == 1){
    merge$Total_Nitrogen[i] = nutrient$total_nitrogen[l]
    merge$Nitrate[i] = nutrient$Nitrate[l]
    merge$Nitrite[i] = nutrient$Nitrite[l]
    merge$Ammonium[i] = nutrient$Ammonium[l]
    merge$Phosphate[i] = nutrient$Phosphate[l]
    merge$Silicate[i] = nutrient$Silicate[l]
  }
  if (length(l) !=1){
    message('length of ', merge$SampleID[i], 'does not match between merge and nutrient dataframes')
  }
}

## ensure numeric 
merge$Total_Nitrogen = as.numeric(merge$Total_Nitrogen)
merge$Nitrate = as.numeric(merge$Nitrate)
merge$Nitrite = as.numeric(merge$Nitrite)
merge$Ammonium = as.numeric(merge$Ammonium)
merge$Phosphate = as.numeric(merge$Phosphate)
merge$Silicate = as.numeric(merge$Silicate)


## round
merge$Total_Nitrogen = round(merge$Total_Nitrogen, 2)
merge$Nitrate = round(merge$Nitrate, 2)
merge$Nitrite = round(merge$Nitrite, 2)
merge$Ammonium = round(merge$Ammonium, 2)
merge$Phosphate = round(merge$Phosphate, 2)
merge$Silicate = round(merge$Silicate, 2)

#### Add Event details 
event = event[event$instrument == 'CTD Rosette',]
event$cast[event$station == 4] = 1

event$cast[event$group_id == '01K4TKQ5TKX4CF8T0GQXWV014Y'] = 1 #stn 12
event$cast[event$group_id == '01K50TX8PQS933N20K4S8NSFFS'] = 1 #stn 3
event$cast = as.numeric(event$cast)

event$station = as.numeric(event$station)

merge$Cast[merge$Station == 7 & merge$Cast == 2] = 4


for (i in 1:nrow(merge)) {
  l = which(merge$Station[i] == event$station & merge$Cast[i] == event$cast)
    merge$Event_Key[i] = event$group_id[l[1]]

}


## WRITE TEMPORARY HYDROCHEMISTRY FILE
saveRDS(merge, './output/final stage/NABOS2025_Final Bottle.RDS')
write.xlsx(merge, './output/final stage/NABOS2025_Final Bottle.xlsx')
write.csv(merge, './output/final stage/NABOS2025_Final Bottle.csv')

merge.igor = merge %>% replace(is.na(merge), -9999)
write.csv(merge, './output/final stage/NABOS2025_Final Bottle-9999.csv')
