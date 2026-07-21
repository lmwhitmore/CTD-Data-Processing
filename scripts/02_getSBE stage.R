rm(list = ls())
library(oce)
#library(TheSource)
library(openxlsx)
source('scripts/00_Processing Functions.R')

## Set Working Directories 

proc.dir = './proc/'
out.dir = './output/'

overwrite = T

#### Start Processing Bottle File ####
  #List file paths
  btl.files = list.files(path = 'proc', pattern = '.btl', full.names = F)
   
  #Create Empty Dataframe
  message(Sys.time(), ': Found ', length(btl.files), ' files. Starting.')
  bottle = data.frame()
  
  #merge bottle data
  for (i in 1:length(btl.files)) { 
    temp = load.bottle(paste0(proc.dir, btl.files[i]))
    temp.name = strsplit(btl.files[i], '_')[[1]]
    temp$HLYID = temp.name[1]
    temp$Station = temp.name[2] ## use gsub to remove "Station" or "Cast" and leave a numeric value
    temp$Cast = strsplit(temp.name[3], '.b')[[1]][1]

    bottle = rbind(bottle, temp)
  }
  
  ## Make Cruise Specific Edits: 
  #adjust cruise column name and entry 
  colnames(bottle)[25] = 'Cruise'
  bottle$Cruise = 'NABOS2025'
  
  #remove test cast
  bottle = bottle[bottle$Station != 'testcast.btl',]
  
  #edit station and cast fields
  bottle$Station = as.numeric(bottle$Station)
  bottle$Cast = as.numeric(bottle$Cast)
  bottle$Cast[bottle$Station == 14] = 1 # typo in bottle file 

#### Start Processing Downcast #### 
  #List file paths  
  ctd.files = list.files(path = 'proc', pattern = 'avg.cnv', full.names = F)
  
  #Create Empty Dataframe
  message(Sys.time(), ': Found ', length(ctd.files), ' files. Starting.')
  downcast = data.frame()
  
  ##merge CTD downcast data
  for (i in (1:length(ctd.files))) { 
    out.name = paste0(out.dir, gsub('.cnv', '.xlsx', ctd.files[i]))
    
    if (file.exists(out.name) & !overwrite) {
      message('Skipping file ', i)
      dat = read.xlsx(out.name)
      
    } else {
      
      temp = oce::read.ctd.sbe(paste0(proc.dir, '/', ctd.files[i]))
      dat = as.data.frame(temp@data)
      dat$datetime = temp@metadata$date ## NMEA DATE (temp@metadata$startTime) is corrupted for many casts -- using computer time instead
      dat$ID = ctd.files[i]
      temp.name = strsplit(ctd.files[i], '_')[[1]]
      dat$HLYID = temp.name[1]
      dat$Station = temp.name[2] ## use gsub to remove "Station" or "Cast" and leave a numeric value
      dat$Cast = strsplit(temp.name[3], 'avg.')[[1]][1]
     

      ## this replaces funky symbols with character strings
      for (j in 1:length(temp@metadata$dataNamesOriginal)) {
        colnames(dat)[j] = make.names(gsub(useBytes = T, '\xe900', 'theta', temp@metadata$dataNamesOriginal[[j]]))
      }
      
      write.xlsx(dat, file = out.name)
      message('Finished file ', i)
      
    }
    downcast = rbind(downcast, dat)
  }
  
  for (i in 1:nrow(downcast)) {
    downcast$ID[i] = strsplit(downcast$ID[i], 'avg.')[[1]][1]
  }
  
  ## Make Cruise Specific Edits: 
  #adjust cruise column name and entry 
  colnames(downcast)[28] = 'Cruise'
  downcast$Cruise = 'NABOS2025'
  
  #remove test cast
  downcast = downcast[downcast$Station != 'testcastavg.cnv',]
  
  #edit station and cast fields
  downcast$Station = as.numeric(downcast$Station)
  downcast$Cast = as.numeric(downcast$Cast)
  downcast$Cast[downcast$Station == 14] = 1 # typo in downcast file 
  
#### Add Datetime to Bottle
  bottle$datetime = Sys.time()
  for (i in 1:nrow(bottle)) {
    l = which(bottle$Station[i] == downcast$Station & bottle$Cast == downcast$Cast)
    bottle$datetime[i] = downcast$datetime[l[1]]
  }
  
  bottle$datetime = as.POSIXct(bottle$datetime, tz = 'UTC')
  
#### Data Export ####
  #export as xlsx and RDS
  #out.name = paste0(out.dir, '20260618_NABOS2025_bottles.xlsx') ## UPDATE NAME 
  write.xlsx(bottle, file = './output/sbe stage/20260721_SBEStage_bottles.xlsx') ## UPDATE NAME)
  saveRDS(bottle, file = paste0('./output/sbe stage/', '20260721_SBEStage_bottles.RDS')) ## UPDATE NAME
  
  
  ## export downcast as xlsx and RDS
  #out.name = paste0(out.dir, '20260618_NABOS2025_downcast.xlsx') ## UPDATE NAME
  write.xlsx(downcast, file = paste0(out.dir, 'sbe stage/', '20260721_SBEStage_downcast.xlsx'))
  saveRDS(downcast, file = paste0(out.dir, 'sbe stage/', '20260721_SBEStage_downcast.RDS')) ## UPDATE NAME
  
  
