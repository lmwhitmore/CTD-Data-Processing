## Source Functions for working with NABOS Data
## Laura M Whitmore
## March 25, 2022
#library(TheSource)



exaggerate = function(x, power = 0.8) {
  sign(x) * abs(x)^power
}




add.exaggerated.axis = function(side, power = 0.8, at = NULL, labels = NULL, grid = F, lty = 2, col = 'grey', ...) {
  if (is.null(at)) {
    if (side == 1 | side == 3) {
      at = pretty(pmax(0, par('usr')[1:2])^(1/power))
    } else {
      at = pretty(pmax(0, par('usr')[3:4])^(1/power))
    }
  }
  
  if (is.null(labels)) { labels = at }
  
  ## Transform
  at = at ^ power
  
  if (grid & (side == 1 | side == 3)) {
    abline(v = at, lty = lty, col = col)
  }
  if (grid & (side == 2 | side == 4)) {
    abline(h = at, lty = lty, col = col)
  }
  
  axis(side = side, at = at, labels = labels, ...)
}



#### Match Nutrient Analyses with CTD Bottle Data #### 

## Simplest data pairing - by Bottle ID only. 
add.var = function(master.id, look.id, master.vals, look.vals, default.vals = -999) {
  no.match = c()
  multi.match = c()
  l.track = rep(F, length(look.id))
  
  for (i in 1:length(master.id)) {
    l = which(look.id == master.id[i]) #Isolate the id
    l.track[l] = T
    if (!all(is.na(master.vals[i,])) & all(master.vals[i,] != default.vals)){ 
      print(paste0('Entry was already added: ', master.id[i])) ## Entry was already added
      
    } else if (length(l) == 1) { ## there is ONE match
      master.vals[i,] = look.vals[l,]
      
    } else if (length(l) > 1) { ## More than one match, only use first one.
      master.vals[i,] = look.vals[l[1],]
      multi.match = c(multi.match, master.id[i])
      
    } else {
      no.match = c(no.match, master.id[i]) ## No Match
    }
  }   
  
  print('')
  
  ## If you don't really care about which ones were matched then just return the master
  master.vals
  ## If you do, then can return object with match data included
  #list(master.vals = master.vals, 
   #    no.match = no.match, 
    #   multi.match = multi.match, 
     #  l.track = l.track)
}

##pair by stn, cast, niskin --> Good for WOCE and CLIVAR type data
add.var.scn = function(master.station, look.station, master.cast, 
                       look.cast, master.niskin, look.niskin, master.val, look.val, 
                       default.val = -999) {
  
  ##match by equal station, cast, niskin
  for (i in 1:length(master.station)) {
    l = which(look.station == master.station[i] & look.cast == master.cast[i] & look.niskin == master.niskin[i]) ##isolate each station
    
    if (length(l) > 0) {
      if (!is.na(master.val[i]) & master.val[i] != default.val) {
        print(paste0('Entry was already added:', master.station[i], master.cast[i], master.niskin[i]))
      } else if (length(l) == 1 ) {
        master.val[i] = look.val[l]
        print(paste0('Added by matching stn, cast, niskin:', master.station[i], master.cast[i], master.niskin[i]))
      } else if (length(l) > 1) {
        master.val[i] = look.val[l[1]]
        print(paste0('Added by matching stn, cast, niskin:', master.station[i], master.cast[i], master.niskin[i]))
      }
    } 
  }
  
  print('')
  ## Return
  master.val
}


#### PLotting By Station #### 
## I want a panel for each station (par(mfrow = c(2,3)))
## This function is specific to NutQAQC.RDS

##Make Station plots
plot.nut.profiles = function(data, 
                             labels = TRUE, 
                             pch = 16, 
                             col = 'black') {
  
  if (labels){
    data.id = data$SampleID
  }
  
  for (i in unique(data$Station)) {
    l = which(data$Station == i)
    if (length(l) > 2) {
      par(mfrow = c(2,3))
      
      #blank
      plot(NULL, xaxt = 'n', yaxt = 'n', bty = 'n', ylab = '', xlab = '', xlim = 0:1, ylim = 0:1)
      
      #nitrate
      plot(data$IARC.Nitrate[l], data$Depth[l], 
           ylim = c(max(data$Depth[l], na.rm = TRUE) + max(data$Depth[l], na.rm = TRUE)*0.1, 0), 
           xlim = c(0, 20), 
           pch = pch, col = col, xaxs='i', yaxs='i',
           ylab = 'Depth (m)', xlab= 'Nitrate (uM)', main=paste('Station -', i))
      text(data$Nitrate.IARC[l], data$Depth[l], labels=data.id[l], pos = 4, cex = 0.6)
      points(data$Nitrate.AARI[l], data$Depth[l], pch = 16, col = 'dark red')
      
      #nitrite
      plot(data$IARC.Nitrite[l], data$Depth[l], 
           ylim = c(max(data$Depth[l], na.rm = TRUE) + max(data$Depth[l], na.rm = TRUE)*0.1, 0), 
           xlim = c(0,1), 
           pch = pch, col = col, xaxs='i', yaxs='i',
           ylab = 'Depth (m)', xlab= 'Nitrite (uM)', main=paste('Station -', i))
      text(data$IARC.Nitrite[l], data$Depth[l], labels=data.id[l], pos = 4, cex = 0.6)
      points(data$Nitrite.AARI[l], data$Depth[l], pch = 16, col = 'dark red')
      
      #Ammonium
      plot(data$Ammonia.IARC[l], data$Depth[l], 
           ylim = c(max(data$Depth[l], na.rm = TRUE) + max(data$Depth[l], na.rm = TRUE)*0.1, 0), 
           xlim = c(0,5), 
           pch = pch, col = col, xaxs='i', yaxs='i',
           ylab = 'Depth (m)', xlab= 'Ammonium (uM)', main=paste('Station -', i))
      text(data$Ammonia.IARC[l], data$Depth[l], labels=data.id[l], pos = 4, cex = 0.6)
      points(data$Ammonia.AARI[l], data$Depth[l], pch = 16, col = 'dark red')
      
      #Phosphate
      plot(data$Phosphate.IARC[l], data$Depth[l], 
           ylim = c(max(data$Depth[l], na.rm = TRUE) + max(data$Depth[l], na.rm = TRUE)*0.1, 0), 
           xlim = c(0, 3), 
           pch = pch, col = col, xaxs='i', yaxs='i',
           ylab = 'Depth (m)', xlab= 'Phosphate (uM)', main=paste('Station -', i))
      text(data$Phosphate.IARC[l], data$Depth[l], labels=data.id[l], pos = 4, cex = 0.6)
      points(data$Phosphate.AARI[l], data$Depth[l], pch = 16, col = 'dark red')
      
      #Silicate
      plot(data$Silicate.IARC[l], data$Depth[l], 
           ylim = c(max(data$Depth[l], na.rm = TRUE) + max(data$Depth[l], na.rm = TRUE)*0.1, 0), 
           xlim = c(0, 50), 
           pch = pch, col = col, xaxs='i', yaxs='i',
           ylab = 'Depth (m)', xlab= 'Silicate (uM)', main=paste('Station -', i))
      text(data$Silicate.IARC[l], data$Depth[l], labels=data.id[l], pos = 4, cex = 0.6)
      points(data$Silicate.AARI[l], data$Depth[l], pch = 16, col = 'dark red')
      
    } else { 
      print(paste("no station data", l))
    }
  }
}



##Make Station plots
plot.nut.profiles.exaggerate = function(data, 
                             labels = TRUE, 
                             pch = 16, 
                             col = 'black', scale = 0.5) {
  
  if (labels){
    data.id = data$SampleID
  }
  
  for (i in unique(data$Station)) {
    l = which(data$Station == i)
    if (length(l) > 2) {
      par(mfrow = c(2,3))
      
      #blank
      plot(NULL, xaxt = 'n', yaxt = 'n', bty = 'n', ylab = '', xlab = '', xlim = 0:1, ylim = 0:1)
      
      #nitrate
      plot(data$IARC.Nitrate[l], exaggerate(data$Depth[l], scale), 
           ylim = exaggerate(c(max(data$Depth[l], na.rm = TRUE) + max(data$Depth[l], na.rm = TRUE)*0.1, 0), scale),  
           xlim = c(0, 20), 
           pch = pch, col = col, xaxs='i', yaxs='i', yaxt = 'n',
           ylab = 'Depth (m)', xlab= 'Nitrate (uM)', main=paste('Station -', i))
      text(data$IARC.Nitrate[l], exaggerate(data$Depth[l], scale), labels=data.id[l], pos = 4, cex = 0.6)
      points(data$AARI.Nitrate[l], exaggerate(data$Depth[l], scale), pch = 16, col = 'grey')
      points(data$IARC.Nitrate[l][data$IARC.Nitrate.Flag[l] == 3], 
             exaggerate(data$Depth[l][data$IARC.Nitrate.Flag[l] == 3], scale), pch = 16, col = 'dark red')
      add.exaggerated.axis(2, scale)
      
      #nitrite
      plot(data$IARC.Nitrite[l], exaggerate(data$Depth[l], scale),
           ylim = exaggerate(c(max(data$Depth[l], na.rm = TRUE) + max(data$Depth[l], na.rm = TRUE)*0.1, 0), scale), 
           xlim = c(0,1), 
           pch = pch, col = col, xaxs='i', yaxs='i', yaxt = 'n',
           ylab = 'Depth (m)', xlab= 'Nitrite (uM)', main=paste('Station -', i))
      text(data$IARC.Nitrite[l], exaggerate(data$Depth[l], scale), labels=data.id[l], pos = 4, cex = 0.6)
      points(data$AARI.Nitrite[l], exaggerate(data$Depth[l], scale), pch = 16, col = 'grey')
      add.exaggerated.axis(2, scale)  
      
      #Ammonium
      plot(data$IARC.Ammonium[l], exaggerate(data$Depth[l], scale),
           ylim = exaggerate(c(max(data$Depth[l], na.rm = TRUE) + max(data$Depth[l], na.rm = TRUE)*0.1, 0), scale),
           xlim = c(0,5), 
           pch = pch, col = col, xaxs='i', yaxs='i', yaxt = 'n',
           ylab = 'Depth (m)', xlab= 'Ammonium (uM)', main=paste('Station -', i))
      text(data$IARC.Ammonium[l], exaggerate(data$Depth[l], scale), labels=data.id[l], pos = 4, cex = 0.6)
      points(data$AARI.Ammonium[l], exaggerate(data$Depth[l], scale), pch = 16, col = 'grey')
      add.exaggerated.axis(2, scale)    
      
      #Phosphate
      plot(data$IARC.Phosphate[l], exaggerate(data$Depth[l], scale),
           ylim = exaggerate(c(max(data$Depth[l], na.rm = TRUE) + max(data$Depth[l], na.rm = TRUE)*0.1, 0), scale),
           xlim = c(0, 3), 
           pch = pch, col = col, xaxs='i', yaxs='i', yaxt = 'n',
           ylab = 'Depth (m)', xlab= 'Phosphate (uM)', main=paste('Station -', i))
      text(data$IARC.Phosphate[l], exaggerate(data$Depth[l], scale), labels=data.id[l], pos = 4, cex = 0.6)
      points(data$AARI.Phosphate[l], exaggerate(data$Depth[l], scale), pch = 16, col = 'grey')
      points(data$IARC.Phosphate[l][data$IARC.Phosphate.Flag[l] == 3], 
             exaggerate(data$Depth[l][data$IARC.Phosphate.Flag[l] == 3], scale), pch = 16, col = 'dark red')
      add.exaggerated.axis(2, scale)
      
      #Silicate
      plot(data$IARC.Silicate[l], exaggerate(data$Depth[l], scale),
           ylim = exaggerate(c(max(data$Depth[l], na.rm = TRUE) + max(data$Depth[l], na.rm = TRUE)*0.1, 0), scale),
           xlim = c(0, 50), 
           pch = pch, col = col, xaxs='i', yaxs='i', yaxt = 'n',
           ylab = 'Depth (m)', xlab= 'Silicate (uM)', main=paste('Station -', i))
      text(data$IARC.Silicate[l], exaggerate(data$Depth[l], scale), labels=data.id[l], pos = 4, cex = 0.6)
      points(data$AARI.Silicate[l], exaggerate(data$Depth[l], scale), pch = 16, col = 'grey')
      points(data$IARC.Silicate[l][data$IARC.Silicate.Flag[l] == 3], 
             exaggerate(data$Depth[l][data$IARC.Silicate.Flag[l] == 3], scale), pch = 16, col = 'dark red')
      add.exaggerated.axis(2, scale)
      
    } else { 
      print(paste("no station data", l))
    }
  }
}



get.depth = function(lon, lat, bathy) {
  depths = rep(NA, length(lon))
  
  for (i in 1:length(lon)) {
    k1 = which.min(abs(lon[i] - bathy$Lon))
    k2 = which.min(abs(lat[i] - bathy$Lat))
    depths[i] = bathy$Z[k1,k2]
  }
  
  ## Return
  depths
}


##Make Station plots
plot.nut.profiles.exaggerate.rerun = function(data,
                                              data2, 
                                              power = 0.8,
                                              labels = TRUE, 
                                              pch = 16, 
                                              col = 'black', scale = 0.5) {
  
  if (labels){
    data.id = data$SampleID
  }
  
  for (i in unique(data$Station)) {
    l = which(data$Station == i & !is.na(data$Depth)) # Get valid samples (station & good depth)
    k = data2$Station == i
    
    if (length(l) > 2) {
      par(mfrow = c(2,3))
      
      #blank
      plot(data$Nitrogen[l],
           exaggerate(data$Depth[l], power = power), 
           ylim = exaggerate(c(max(data$Depth[l], na.rm = TRUE) * 1.1, 0), power),
           xlim = c(0, 20), 
           pch = pch, col = col, xaxs='i', yaxs='i', yaxt = 'n',
           ylab = 'Depth (m)', xlab= 'Nitrogen (uM)', main=paste('Station -', i))
      text(data$Nitrogen[l], exaggerate(data$Depth[l], power = power), labels=data.id[l], pos = 4, cex = 0.6)
      points(data2$Nitrogen[k], exaggerate(data2$Depth[k], power = power), pch = 16, col = 'green')
      add.exaggerated.axis(2, power = power)
      
            #nitrate
      plot(data$Nitrate[l], exaggerate(data$Depth[l], power = power), 
           ylim = exaggerate(c(max(data$Depth[l], na.rm = TRUE) * 1.1, 0), power),  
           xlim = c(0, 20), 
           pch = pch, col = col, xaxs='i', yaxs='i', yaxt = 'n',
           ylab = 'Depth (m)', xlab= 'Nitrate (uM)', main=paste('Station -', i))
      text(data$Nitrate[l], exaggerate(data$Depth[l], power = power), labels=data.id[l], pos = 4, cex = 0.6)
      points(data2$Nitrate[k], exaggerate(data2$Depth[k], power), pch = 16, col = 'green')
      add.exaggerated.axis(2, power = power)
      
      #nitrite
      plot(data$Nitrite[l], exaggerate(data$Depth[l], power = power),
           ylim = exaggerate(c(max(data$Depth[l], na.rm = TRUE) + max(data$Depth[l], na.rm = TRUE)*0.1, 0), power), 
           xlim = c(0,1), 
           pch = pch, col = col, xaxs='i', yaxs='i', yaxt = 'n',
           ylab = 'Depth (m)', xlab= 'Nitrite (uM)', main=paste('Station -', i))
      text(data$Nitrite[l], exaggerate(data$Depth[l], power = power), labels=data.id[l], pos = 4, cex = 0.6)
      points(data2$Nitrite[k], exaggerate(data2$Depth[k], power), pch = 16, col = 'green')
      add.exaggerated.axis(2, power)  
      
      #Ammonium
      plot(data$Ammonia[l], exaggerate(data$Depth[l], power = power),
           ylim = exaggerate(c(max(data$Depth[l], na.rm = TRUE) + max(data$Depth[l], na.rm = TRUE)*0.1, 0), power),
           xlim = c(0,5), 
           pch = pch, col = col, xaxs='i', yaxs='i', yaxt = 'n',
           ylab = 'Depth (m)', xlab= 'Ammonium (uM)', main=paste('Station -', i))
      text(data$Ammonia[l], exaggerate(data$Depth[l], power = power), labels=data.id[l], pos = 4, cex = 0.6)
      points(data2$Ammonia[k], exaggerate(data2$Depth[k], power = power), pch = 16, col = 'green')
      add.exaggerated.axis(2, power = power)    
      
      #Phosphate
      plot(data$Phosphate[l], exaggerate(data$Depth[l], power = power),
           ylim = exaggerate(c(max(data$Depth[l], na.rm = TRUE) + max(data$Depth[l], na.rm = TRUE)*0.1, 0), power),
           xlim = c(0, 3), 
           pch = pch, col = col, xaxs='i', yaxs='i', yaxt = 'n',
           ylab = 'Depth (m)', xlab= 'Phosphate (uM)', main=paste('Station -', i))
      text(data$Phosphate[l], exaggerate(data$Depth[l], power = power), labels=data.id[l], pos = 4, cex = 0.6)
      points(data2$Phosphate[k], exaggerate(data2$Depth[k], power = power), pch = 16, col = 'green')
      add.exaggerated.axis(2, power = power)
      
      #Silicate
      plot(data$Silicate[l], exaggerate(data$Depth[l], power = power),
           ylim = exaggerate(c(max(data$Depth[l], na.rm = TRUE) * 1.1, 0), power),
           xlim = c(0, 50), 
           pch = pch, col = col, xaxs='i', yaxs='i', yaxt = 'n',
           ylab = 'Depth (m)', xlab= 'Silicate (uM)', main=paste('Station -', i))
      text(data$Silicate[l], exaggerate(data$Depth[l], power = power), labels=data.id[l], pos = 4, cex = 0.6)
      points(data2$Silicate[k], exaggerate(data2$Depth[k], power = power), pch = 16, col = 'green')
      add.exaggerated.axis(2, power = power)
      
    } else { 
      print(paste("no station data", l))
    }
  }
}



get.depth = function(lon, lat, bathy) {
  depths = rep(NA, length(lon))
  
  for (i in 1:length(lon)) {
    k1 = which.min(abs(lon[i] - bathy$Lon))
    k2 = which.min(abs(lat[i] - bathy$Lat))
    depths[i] = bathy$Z[k1,k2]
  }
  
  ## Return
  depths
}


get.transect.stations = function(start.lon, start.lat, end.lon, end.lat, n = 10) {
  
  points = projectionStereographic(lon = c(start.lon, end.lon), lat = c(start.lat, end.lat), lon0 = start.lon+1e-6, lat0 = start.lat+1e-6)
  points = data.frame(lon = seq(points$x[1], points$x[2], length.out = n),
                      lat = seq(points$y[1], points$y[2], length.out = n))
  points2 = points
  points = projectionStereographic(lon = points$lon, lat = points$lat, 
                               lon0 = start.lon, lat0 = start.lat, inv = T)
  
  points$x = points2$lon
  points$y = points2$lat
  points
}




add.map.contour = function (map, lon, lat, z, trim = T, col = "black", levels = NULL, 
                          zlim = NULL, n = 3, labels = TRUE, lty = 1, lwd = 1, verbose = T,
                          N = 10) 
{
  if (verbose) {
    message("Add Map Contours:")
  }
  lon = lon%%360
  map$lon.min = map$lon.min%%360
  map$lon.max = map$lon.max%%360
  lon = as.array(lon)
  lat = as.array(lat)
  z = as.array(z)
  if (is.null(zlim)) {
    zlim = range(z, na.rm = T)
  }
  if (is.null(levels)) {
    levels = pretty.default(zlim, n = n + 2)[-c(1, n + 2)]
    if (verbose) {
      message(" Levels: ", paste(levels, collapse = ", "))
    }
  }
  n = length(levels)
  if (length(col) != n) {
    col = rep(col, n)
  }
  if (length(lty) != n) {
    lty = rep(lty, n)
  }
  if (length(lwd) != n) {
    lwd = rep(lwd, n)
  }
  if (is.na(dim(lon)[2]) & is.na(dim(lat)[2])) {
    if (length(z) == length(lon) * length(lat)) {
      z = array(z, dim = c(length(lon), length(lat)))
      lon = matrix(lon, nrow = dim(z)[1], ncol = dim(z)[2])
      lat = matrix(lat, nrow = dim(z)[1], ncol = dim(z)[2], 
                   byrow = T)
    }
    else {
      z = array(z, dim = c(length(unique(lon)), length(unique(lat))))
      lon = matrix(unique(lon), nrow = dim(z)[1], ncol = dim(z)[2])
      lat = matrix(unique(lat), nrow = dim(z)[1], ncol = dim(z)[2], 
                   byrow = T)
    }
  }
  if (trim) {
    nz = length(z)
    if (verbose) {
      message(" Starting domain trimming... ", appendLF = F)
    }
    corners = expand.grid(lon = c(par("usr")[1], par("usr")[2]), 
                          lat = c(par("usr")[3], par("usr")[4]))
    corners = rgdal::project(cbind(corners$lon, corners$lat), 
                             proj = map$p, inv = T)
    field = expand.grid(lon = seq(par("usr")[1], par("usr")[2], 
                                  length.out = 360), lat = seq(par("usr")[3], par("usr")[4], 
                                                               length.out = 10))
    field = rgdal::project(cbind(field$lon, field$lat), proj = map$p, 
                           inv = T)
    field[, 1] = field[, 1]%%360
    field.lon = range(field[, 1], na.rm = T)
    field.lat = range(field[, 2], na.rm = T)
    if (corners[1, 1] > corners[2, 1]) {
      if (verbose) {
        message(" antimeridian... ", appendLF = F)
      }
      k = apply(lon, 1, function(x) {
        any(x >= field.lon[1] & x <= field.lon[2])
      })
      if (length(k) > 0) {
        z = z[k, ]
        lon = lon[k, ]
        lat = lat[k, ]
      }
    }
    else {
      if (verbose) {
        message(" longitude... ", appendLF = F)
      }
      k = apply(lon, 1, function(x) {
        any(x <= field.lon[2] & x >= field.lon[1])
      })
      if (length(k) > 0) {
        z = z[k, ]
        lon = lon[k, ]
        lat = lat[k, ]
      }
    }
    if (verbose) {
      message(" latitude... ", appendLF = F)
    }
    k = apply(lat, 2, function(x) {
      any(x >= field.lat[1] & x <= field.lat[2])
    })
    if (length(k) > 0) {
      z = z[, k]
      lon = lon[, k]
      lat = lat[, k]
    }
    if (verbose) {
      message(" complete, n = ", length(z), " (", 100 * 
                round(1 - n/nz, digits = 3), " %)")
    }
  }
  contour = contourLines(x = c(1:dim(z)[1]), y = c(1:dim(z)[2]), 
                         z = z, levels = levels)
  if (verbose) {
    message(" Starting plotting... ", appendLF = F)
  }
  for (i in 1:length(contour)) {
    n = which(contour[[i]]$level == levels)
    if (length(contour[[i]]$x) > N) {
      xx = grid.interp(lon, contour[[i]]$x, contour[[i]]$y)
      yy = grid.interp(lat, contour[[i]]$x, contour[[i]]$y)
      add.map.line(map, xx, yy, col = col[n], lty = lty[n], 
                   lwd = lwd[n], greatCircle = F)
    }
  }
  redraw.map(map)
  if (verbose) {
    message(" done.")
  }
}

