library(TheSource)
library(data.table)
library(openxlsx)


#' Build a compiled dataset from the Sikuliaq's underway seawater system (LDS)
#' @param proc.path The path to the processed output of the LDS (e.g. ./lds/proc)
#' @param out.path The path to which the Underway datasets will be written (xlsx and RDS formats)
#' @author Thomas Bryce Kelly <tbkelly@alaska.edu>
#' 
build.lds = function(proc.path, out.path) {
  ship.files = list.files(proc.path, pattern = '.txt', recursive = T, full.names = T)
  
  ## Load header
  raw = readLines(ship.files[1])
  header = strsplit(gsub('\\s+', '', raw[grep('Column', raw)]), ':')
  column.names = unlist(header)
  column.names = column.names[seq(2, length(column.names), by = 2)]
  
  ship = fread(ship.files[1], skip = 42)
  colnames(ship) = column.names
  
  
  for (i in c(2:length(ship.files))) {
    message('Reading in file ', i , ' of ', length(ship.files))
    
    ## Load header and read data
    raw = readLines(ship.files[i])
    header = strsplit(gsub('\\s+', '', raw[grep('Column', raw)]), ':')
    column.names = unlist(header)
    column.names = column.names[seq(2, length(column.names), by = 2)]
    
    temp = fread(ship.files[i], skip = 42)
    colnames(temp) = column.names
    
    k = colnames(temp)[!colnames(temp) %in% colnames(ship)]
    for (name in k) {
      message(' Padding for missing column: ', name)
      ship[[name]] = NA
    }
    
    l = colnames(ship)[!colnames(ship) %in% colnames(temp)]
    for (name in l) {
      message(' initializing for new column: ', name)
      temp[[name]] = NA
    }
    
    ship = rbind(ship, temp, fill = T)
  }
  
  colnames(ship) = make.names(colnames(ship))
  
  message('Saving data as RDS and XLSX.')
  saveRDS(ship, paste0(out.path, 'Underway.rds'))
  write.xlsx(ship, paste0(out.path, 'Underway.xlsx'))
  
  ship
}



#' Load and generate an FRRF dataset
build.frrf = function(frrf.dir, out.path, n = NULL){
  
  if (file.exists(paste0(out.path, '/FRRF.rds'))) {
    frrf = readRDS(paste0(out.path, '/FRRF.rds'))
  } else {
    frrf = list()
  }
  
  files = list.files(frrf.dir, pattern = '.csv', recursive = T, full.names = T)
  
  if (is.null(n)) {
    n = max(pretty(1:length(files)))
  }
  
  message('Building FRRF dataset (', length(files), ')...')
  for (i in 1:length(files)) {
    if (!files[i] %in% names(frrf)) {
      temp = load.frrf('', files[i], verbose = F)
      frrf[names(temp)[1]] = temp[1]
    }
  }
  
  saveRDS(frrf, paste0(out.dir, '/FRRF.rds'))
  
  
  
  pdf(paste0(out.dir, '/FRRF Figures.pdf'), width = 6, height = 24)
  par(mfrow = c(8,1))
  
  ## JVPII Line plot
  plot(frrf[[2]]$A$E, frrf[[2]]$A$JPII,
       ylab = 'JVPII', xlab = 'E',
       xlim = c(0, 1e3), type = 'l',
       main = 'Electron Transport Rate')
  
  for (i in 2:length(frrf)) {
    lines(frrf[[i]]$A$E[-1], frrf[[i]]$A$JPII[-1], col = make.pal(i, min = 0, max = 300, pal = 'inferno'))
  }
  grid(); box()
  
  
  ## FVFm
  plot(NULL, NULL, xlim = c(0, n), ylim = c(0.2,0.7), ylab = 'FvFm', xlab = 'Sample', main = 'Quantum Yield')
  for (i in 2:length(frrf)) {
    points(i, frrf[[i]]$A$Fv.Fm[1], pch = 20)
  }
  grid(); box()
  
  
  
  ## Ek
  plot(NULL, NULL, xlim = c(0, n), ylim = c(0,750), ylab = 'Ek', xlab = 'Sample', main = 'Light Half Saturation')
  for (i in 2:length(frrf)) {
    points(i, frrf[[i]]$S$Ek[1], pch = 20)
  }
  grid(); box()
  
  
  
  ## Ek
  plot(NULL, NULL, xlim = c(0, n), ylim = c(0,1), ylab = 'Alpha', xlab = 'Sample', main = 'Apparent Light Aclimation')
  for (i in 2:length(frrf)) {
    points(i, frrf[[i]]$S$Alpha[1], pch = 20)
  }
  grid(); box()
  
  
  # NSV
  plot(NULL, NULL, xlim = c(0, n), ylim = c(0,2), ylab = 'NSV', xlab = 'Sample', main = 'Non-photochemical Quenching')
  for (i in 2:length(frrf)) {
    points(i, frrf[[i]]$A$NSV[1], pch = 20)
  }
  grid(); box()
  
  
  ## JVPII
  plot(NULL, NULL, xlim = c(0, n), ylim = c(0,5), ylab = 'JVPII', xlab = 'Sample', main = 'ETR Across Light Levels')
  
  col = pals::cubicl(5)
  for (E in c(100, 200, 300, 400, 500)) {
    for (i in 2:length(frrf)) {
      if (length(!is.na(frrf[[i]]$A$JVPII)) > 5) {
        points(i,
               approx(frrf[[i]]$A$E[-1], frrf[[i]]$A$JVPII[-1], xout = E, rule = 2)$y,
               pch = 20,
               col = col[E/100])
      }
    }
  }
  
  grid(); box()
  legend('topleft', col = rev(col), pch = 20, legend = c(500,400,300,200,100))
  
  ## Sigma
  plot(NULL, NULL, xlim = c(0, n), ylim = c(0,10), ylab = 'Sigma (nm2)', xlab = 'Sample', main = 'Antenna Complex Size')
  for (i in 2:length(frrf)) {
    points(i, frrf[[i]]$A$Sigma[1], pch = 20)
  }
  grid(); box()
  
  
  
  ## TAU
  plot(NULL, NULL, xlim = c(0, n), ylim = c(1e3,5e3), ylab = 'Tau (time?)', xlab = 'Sample', main = 'Reaction Center Turnover Time')
  for (i in 2:length(frrf)) {
    points(i, frrf[[i]]$A$TauES[1], pch = 20)
  }
  grid(); box()
  
  dev.off()
  
  #Return
  frrf
}





load.bottle = function(file) {
  ## Load data
  temp = readLines(file)
  
  ## Get lines
  start = grep('Bottle', temp)
  header = strsplit(temp[start], '\\s+')[[1]][-c(1,3)]
  header = make.names(header, unique = T)
  
  #header[duplicated(header)] = paste0(header[duplicated(header)], 2)
  bottle = list()
  for (i in 1:length(header)) {
    bottle[[header[i]]] = NA
  }
  bottle = as.data.frame(bottle)
  
  for (i in seq(start + 2, length(temp), by = 2)) {
    row = as.numeric(strsplit(temp[i], '\\s+')[[1]][-c(1,3:5)])
    row = row[-length(row)]
    
    bottle = rbind(bottle, row)
  }
  
  ## Return
  bottle = bottle[-1,]
  rownames(bottle) = bottle$Bottle
  
  bottle
}

