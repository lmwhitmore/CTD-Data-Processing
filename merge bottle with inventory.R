
## load bottle Automated CTD Processing "output" folder

bottle2 = bottle

for (i in 1:nrow(bottle2)) {
bottle2$Station[i] = strsplit(bottle2$Station[i], 'station')[[1]][2]
bottle2$Cast[i] = strsplit(bottle2$Cast[i], 'cast')[[1]][2]
}

colnames(bottle2)[23] = 'Cruise'


## load hydrochemistry data
#inventory = openxlsx::read.xlsx(xlsxFile = '../../../Data/Hydrochemistry/NABOS2023_ShipboardHydrochemistryInventory.xlsx', sheet = 'Bottle Log')

source('../../../Scripts/source_NABOS.R')

merge = inventory

merge$CTD.Salinity00 = NA
merge$CTD.Salinity00 = add.var.scn(master.station = merge$Station, look.station = bottle2$Station,
                              master.cast = merge$CTD.Cast.No, look.cast = bottle2$Cast,
                              master.niskin = merge$Fire.Seq, look.niskin = bottle2$Bottle,
                              master.val = merge$CTD.Salinity00, look.val = bottle2$Sal00)
merge$CTD.Salinity11 = NA
merge$CTD.Salinity11 = add.var.scn(master.station = merge$Station, look.station = bottle2$Station,
                                   master.cast = merge$CTD.Cast.No, look.cast = bottle2$Cast,
                                   master.niskin = merge$Fire.Seq, look.niskin = bottle2$Bottle,
                                   master.val = merge$CTD.Salinity11, look.val = bottle2$Sal11)

merge$lat = NA
merge$lat = add.var.scn(master.station = merge$Station, look.station = bottle2$Station,
                                   master.cast = merge$CTD.Cast.No, look.cast = bottle2$Cast,
                                   master.niskin = merge$Fire.Seq, look.niskin = bottle2$Bottle,
                                   master.val = merge$lat, look.val = bottle2$Latitude)
merge$lon = NA
merge$Clon = add.var.scn(master.station = merge$Station, look.station = bottle2$Station,
                                   master.cast = merge$CTD.Cast.No, look.cast = bottle2$Cast,
                                   master.niskin = merge$Fire.Seq, look.niskin = bottle2$Bottle,
                                   master.val = merge$lon, look.val = bottle2$Longitude)

merge$Depth = NA
merge$Depth = add.var.scn(master.station = merge$Station, look.station = bottle2$Station,
                                   master.cast = merge$CTD.Cast.No, look.cast = bottle2$Cast,
                                   master.niskin = merge$Fire.Seq, look.niskin = bottle2$Bottle,
                                   master.val = merge$Depth, look.val = bottle2$DepSM)
merge$Pressure = NA
merge$Pressure = add.var.scn(master.station = merge$Station, look.station = bottle2$Station,
                                   master.cast = merge$CTD.Cast.No, look.cast = bottle2$Cast,
                                   master.niskin = merge$Fire.Seq, look.niskin = bottle2$Bottle,
                                   master.val = merge$Pressure, look.val = bottle2$PrDM)

merge$CTD.Temperature0 = NA
merge$CTD.Temperature0 = add.var.scn(master.station = merge$Station, look.station = bottle2$Station,
                                   master.cast = merge$CTD.Cast.No, look.cast = bottle2$Cast,
                                   master.niskin = merge$Fire.Seq, look.niskin = bottle2$Bottle,
                                   master.val = merge$CTD.Temperature0, look.val = bottle2$T090C)

merge$CTD.Temperature1 = NA
merge$CTD.Temperature1 = add.var.scn(master.station = merge$Station, look.station = bottle2$Station,
                                     master.cast = merge$CTD.Cast.No, look.cast = bottle2$Cast,
                                     master.niskin = merge$Fire.Seq, look.niskin = bottle2$Bottle,
                                     master.val = merge$CTD.Temperature1, look.val = bottle2$T190C)

## WRITE TEMPORARY LOG FILE FOR METTE
openxlsx::write.xlsx(merge, file = '../../../Data/Hydrochemistry/NABOS2023_TempHydrochemistry.xlsx')

