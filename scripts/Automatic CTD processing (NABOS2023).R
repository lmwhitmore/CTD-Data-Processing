library(oce)
#library(TheSource)
library(openxlsx)
source('Processing Functions.R')


#remote.dir = 'S:/ctd/km2308/'
#local.dir = 'G:/Shared drives/NABOS/NABOS2023 Cruise/NABOS Ship Work/Automatic CTD Processing'
local.dir = 'C:/Users/lmwhi/Desktop/Automatic CTD Processing/'


raw.dir = paste0(local.dir, '/raw/')
proc.dir = paste0(local.dir, '/proc/2025')
out.dir = paste0(local.dir, '/output/2025')

overwrite = T

  
  ## Sync
  #sync.dir(remote.dir, raw.dir)
  
  ## Run SBE
  
  raw.files = list.files(raw.dir, pattern = '.hex')
  
  #i in 2:length(raw.files)
  
  for (i in 2:length(raw.files)) { #skip first file because decktest is messing with things. 
    
    if (!file.exists(paste0(proc.dir, '/', gsub('.hex', '.cnv', raw.files[i]))) | overwrite) {
      temp.file = gsub('.hex', '', raw.files[i])
      #temp.file.str = strsplit(temp.file, '_')[[1]]
      
      message('Processing SBE files for ', temp.file, '.')
      system(paste0('sbebatch "', local.dir, 'scripts/sbe_v2" ', temp.file))  ####UPDATE SBEBATCH in path/download SBE Processing tools
      
      
      #if (!(grepl('016', raw.files[i]) | grepl('017', raw.files[i]))) {
      # message('Processing SBE files for ', temp.file[2],' and ', temp.file[3], '.')
      #  system(paste0('sbebatch "', local.dir, 'scripts/sbe.txt" ', temp.file[2]))
      #} else {
        #message('Processing SBE files without NMEA for cast ', temp.file[2], '.')
        #system(paste0('sbebatch "', local.dir, 'scripts/sbe_noNMEA.txt" ', temp.file[2]))
      #}
    } else {
      message('Skipping file processing for file ', raw.files[i], '.')
    }
    last.run = Sys.time()
  }
  
  
  #### Start Processing
  btl.files = list.files(proc.dir, pattern = '.btl', full.names = F)
  ##NABOS Station 21 cast 1a deleted from avg.cnv files because no data -- see cast log. 
  ctd.files = list.files(proc.dir, pattern = 'avg.cnv', full.names = F)
  
  message(Sys.time(), ': Found ', length(ctd.files), ' files. Starting.')
  downcast = data.frame()
  bottle = data.frame()
  
  for (i in 2:length(btl.files)) {
    temp = load.bottle(paste0(proc.dir, '/', btl.files[i]))
    temp.name = strsplit(btl.files[i], '_')[[1]]
    temp$HLYID = temp.name[1]
    temp$Station = temp.name[2] ## use gsub to remove "Station" or "Cast" and leave a numeric value
    temp$Cast = strsplit(temp.name[3], '.b')[[1]][1]
    
    bottle = rbind(bottle, temp)
  }
  
  out.name = paste0(out.dir, '/', 'bottles.xlsx')
  write.xlsx(bottle, file = out.name)
  saveRDS(bottle, file = paste0(out.dir, '/', 'bottles.RDS'))
  
  
  for (i in (1:length(ctd.files))) { 
    out.name = paste0(out.dir, '/', gsub('.cnv', '.xlsx', ctd.files[i]))
    
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
  
  ## make downcast
  out.name = paste0(out.dir, '/', 'downcast.xlsx')
  write.xlsx(downcast, file = out.name)
  saveRDS(downcast, file = paste0(out.dir, '/', 'downcast.RDS'))
  
  
  ## Figures
  png(paste0(out.dir, '/Figures/TS Plot.png'))
  plot.TS(S = downcast$sal00,
          Tmp = calc.ptemp(S = downcast$sal00, Tmp = downcast$t090C, P = downcast$prDM, P.ref = 0, verbose = F),
          main = 'HLY2302',
          #col = make.pal(as.numeric(downcast$Station), pal = 'cubicl'),
          xlim = c(24,35), ylim = c(-5, 20))
  dev.off()
  
  for (cast in unique(downcast$ID)) {
    png(paste0(out.dir, 'Figures/TS/TS Plot Cast ', cast, '.png'))
    plot.TS(S = downcast$sal00,
            Tmp = calc.ptemp(S = downcast$sal00, Tmp = downcast$t090C, P = downcast$prDM, P.ref = 0, verbose = F),
            main = downcast$ID[cast],
            col = 'grey', xlim = c(28,35), ylim = c(0, 12))
    
    l = downcast$ID == cast
    points(downcast$sal00[l], calc.ptemp(S = downcast$sal00[l], Tmp = downcast$t090C[l], P = downcast$prDM[l], P.ref = 0, verbose = F))
    dev.off()
    
    
    
    
    png(paste0(out.dir, 'Figures/Profiles/Profiles Plot ', cast, '.png'), width = 1200, height = 650)
    par(mfrow = c(1,2))
    l = which(downcast$ID == cast)
    
    t.range = c(-3, 12)
    chl.range = c(0, 8)
    oxy.range = c(0, 400)
    trans.range = c(0, 1)
    
    tmp = (downcast$t090C - t.range[1]) / diff(t.range)
    chl = (downcast$flECO.AFL - chl.range[1]) / diff(chl.range)
    oxy = (downcast$sbox0Mm.Kg - oxy.range[1]) / diff(oxy.range)
    trans = (exp(-downcast$CStarAt0) - trans.range[1]) / diff(trans.range)
    
    plot(tmp[l],
         downcast$depSM[l],
         ylim = c(max(pretty(downcast$depSM[l])),0),
         xlim = c(0, 1),
         yaxs = 'i',
         xaxt = 'n',
         type = 'l',
         col = 'darkred',
         lwd = 2,
         xlab = 'Temperature',
         ylab = 'Depth (m)')
    
    axis(1, at = (c(1:10) - t.range[1])/diff(t.range), labels = c(1:10))
    
    lines(chl[l],
          downcast$depSM[l],
          col = 'darkgreen',
          lwd = 2)
    axis(3, at = (c(1:10) - chl.range[1])/diff(chl.range), labels = c(1:10))
    mtext('Fluorescence', side = 3, line = 3)
    mtext(paste0('Cast ', cast), adj = 0)
    mtext(paste0(' Lat ', round(downcast$latitude[l], digits = 2)), adj = 0, line = -1.5)
    mtext(paste0(' Lon ', round(downcast$longitude[l], digits = 2)), adj = 0, line = -2.5)
    
    k = which(bottle$Cast == cast)
    abline(h = bottle$DepSM[k], col = '#bb888860')
    
    
    plot(oxy[l],
         downcast$depSM[l],
         ylim = c(max(pretty(downcast$depSM[l])),0),
         xlim = c(0, 1),
         yaxs = 'i',
         xaxt = 'n',
         type = 'l',
         col = 'darkblue',
         lwd = 2,
         xlab = 'Oxygen (uM)',
         ylab = 'Depth (m)')
    
    axis(1,
         at = (pretty(downcast$sbox0Mm.Kg) - oxy.range[1])/diff(oxy.range),
         labels = pretty(downcast$sbox0Mm.Kg))
    
    lines(trans[l],
          downcast$depSM[l],
          col = 'darkgrey',
          lwd = 2)
    axis(3, at = (pretty(downcast$CStarAt0) - trans.range[1])/diff(trans.range), labels = pretty(downcast$CStarAt0))
    mtext('Transmission', side = 3, line = 3, col = 'darkgrey')
    
    k = which(bottle$Cast == cast)
    abline(h = bottle$DepSM[k], col = '#bb888860')
    
    dev.off()
  }
  
  
  message(Sys.time(), ': Processed files for ', length(ctd.files), ' files.')



  
for (i in unique(downcast$StationNo)) {
  l = which(downcast$StationNo == i)
  plot(downcast$flECO.AFL[l], downcast$depSM[l], pch = '.', xlab = i)
}
  
