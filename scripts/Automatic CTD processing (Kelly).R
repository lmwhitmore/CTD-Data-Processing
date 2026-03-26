library(oce)
library(TheSource)
library(openxlsx)

ctd.dir = 'S:/ctd/km2308/'
out.dir = 's:/scratch/Kelly/CTD/'

overwrite = F



while (T) {
  
  btl.files = list.files(ctd.dir, pattern = '.btl', full.names = F)
  ctd.files = list.files(ctd.dir, pattern = 'avg.cnv', full.names = F)
  ctd.files = ctd.files[!grepl('svpavg', ctd.files)]
  
  message(Sys.time(), ': Found ', length(ctd.files), ' files. Starting.')
  downcast = data.frame()
  bottle = data.frame()
  
  for (i in 1:length(btl.files)) {
    temp = load.bottle(paste0(ctd.dir, '/', btl.files[i]))
    temp$Cast = gsub('KM202308_', '', gsub('.btl', '', btl.files[i]))
    
    bottle = rbind(bottle, temp)
  }
  
  out.name = paste0(out.dir, '/', 'bottles.xlsx')
  write.xlsx(bottle, file = out.name)
  
  for (i in 1:length(ctd.files)) {
    
    out.name = paste0(out.dir, '/', gsub('.cnv', '.xlsx', ctd.files[i]))
    
    if (file.exists(out.name) & !overwrite) {
      message('Skipping file ', i)
      dat = read.xlsx(out.name)
      
    } else {
      
      temp = oce::read.ctd.sbe(paste0(ctd.dir, '/', ctd.files[i]))
      dat = as.data.frame(temp@data)
      dat$datetime = temp@metadata$startTime
      dat$cast = gsub('KM202308_', '', gsub('avg.cnv', '', ctd.files[i]))
      
      for (j in 1:length(temp@metadata$dataNamesOriginal)) {
        colnames(dat)[j] = make.names(gsub(useBytes = T, '\xe900', 'theta', temp@metadata$dataNamesOriginal[[j]]))
      }
      
      write.xlsx(dat, file = out.name)
      message('Finished file ', i)
      
    }
    downcast = rbind(downcast, dat)
  }
  
  
  ## make downcast
  out.name = paste0(out.dir, '/', 'downcast.xlsx')
  write.xlsx(downcast, file = out.name)
  
  
  
  ## Figures
  png(paste0(out.dir, 'Figures/TS Plot.png'))
  plot.TS(S = downcast$sal00,
          Tmp = calc.ptemp(S = downcast$sal00, Tmp = downcast$t090C, P = downcast$prDM, P.ref = 0, verbose = F),
          main = 'KM202308',
          col = make.pal(as.numeric(downcast$cast), pal = 'cubicl'),
          xlim = c(28,35), ylim = c(0, 10))
  dev.off()
  
  for (cast in unique(downcast$cast)) {
    png(paste0(out.dir, 'Figures/TS/TS Plot Cast ', cast, '.png'))
    plot.TS(S = downcast$sal00,
            Tmp = calc.ptemp(S = downcast$sal00, Tmp = downcast$t090C, P = downcast$prDM, P.ref = 0, verbose = F),
            main = paste0('KM202308-', cast),
            col = 'grey', xlim = c(28,35), ylim = c(0, 10))
    
    l = downcast$cast == cast
    points(downcast$sal00[l], calc.ptemp(S = downcast$sal00[l], Tmp = downcast$t090C[l], P = downcast$prDM[l], P.ref = 0, verbose = F))
    dev.off()
    
    
    
    
    png(paste0(out.dir, 'Figures/Profiles/Profiles Plot ', cast, '.png'), width = 1200, height = 650)
    par(mfrow = c(1,2))
    l = which(downcast$cast == cast)
    
    t.range = c(3, 8)
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
  message('Sleeping for 1 hour...')
  Sys.sleep(60*60)
}



