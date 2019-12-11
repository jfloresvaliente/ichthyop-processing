#=============================================================================#
# Name   : compute_recruitment_ichthyop_drifters
# Author : C. Lett; modified by Jorge Flores
# Date   : 
# Version:
# Aim    : Compute recruitment ICHTHYOP outputs
# URL    : 
#=============================================================================#
compute_recruitment_ichthyop_drifters <- function(
  dirpath = dirpath
  ,firstdrifter = 1
  ,lastdrifter = 5000
  ,firsttime = 1
  ,lasttime = 31
  ,recruitmentzone = 1
  ,old_path
  ,new_path
){
  #============ ============ Arguments ============ ============#
  
  # ncfile = ICHTHYOP netcdf output file
  
  # In case one wishes to consider only a subset of all drifters
  # firstdrifter = Index of first drifter to be compued
  # lastdrifter  = Index of last  drifter to be computed
  
  # In case one wishes to consider only a subset of all steptime to plot 
  # firsttime   = The time record at which to compute recruitment
  # lasttime  = The number of release zones
  
  # recruitmentzone = The index of the recruitment zone for which recruitment is computed
  
  # To read 'xml' files from a directory different to original directory where files were stored
  # old_path = path written in each ncdf input file as attribute
  # new_path = path where '.xml' files are stored
  
  # Then you can calculate new features.
  # Do not forget to add them in the 'return' of the 'compute_recruitment_file' internal function
  
  #============ ============ Arguments ============ ============#
  
  library(ncdf4)
  library(stringr)
  library(XML)
  
  # An inner function that computes recruitment for one .nc output file
  compute_recruitment_file <- function(filename){
    nc <- nc_open(filename)
    
    # An inner function that computes year and day from time in seconds
    compute_yearday <- function(time){
      nbdays <- 1+time/86400
      year <- 1+as.integer(nbdays/360)
      day <- as.integer(nbdays-360*(year-1))
      return(c(year,day))
    }
    
    # Gets the value of time of release
    t0 <- ncvar_get(nc,'time',1,1)
    
    # Computes year and day of release
    yearday <- compute_yearday(t0)
    
    # Get real release dates
    dates <- subset(dates, dates$year == yearday[1] & dates$day == yearday[2])
    
    # Scrump time of released particles like t0,t5,t10...
    t_x <- dates$t_x
    
    # Get the year and month of release particles from 'times'
    year <- dates$Y
    month <- dates$M
    # yearday <- c(year,month)
    
    drifter <- rep(seq(firstdrifter, lastdrifter), each = lasttime)
    timer   <- rep(seq(firsttime, lasttime), times = lastdrifter)
    lon     <- as.vector(t(ncvar_get(nc, 'lon',   c(firstdrifter, firsttime), c(lastdrifter, lasttime))))
    lat     <- as.vector(t(ncvar_get(nc, 'lat',   c(firstdrifter, firsttime), c(lastdrifter, lasttime))))
    depth   <- as.vector(t(ncvar_get(nc, 'depth', c(firstdrifter, firsttime), c(lastdrifter, lasttime))))
    
    # Gets the value of recruited for the recruitment zone considered for all drifters at time of computation
    recruited <- ncvar_get(nc,'recruited_zone',c(recruitmentzone,firstdrifter,lasttime),c(1,lastdrifter,1))
    recruited <- rep(recruited, each = lasttime)
    
    # # Gets the value of release zone for all drifters
    # releasezone <- ncvar_get(nc,'zone',c(1,firstdrifter,1),c(1,lastdrifter,1)) + 1
    # releasezone <- rep(releasezone, each = lasttime)
    
    # df <- data.frame(drifter, timer, lon, lat, depth, recruited, releasezone)
    df <- data.frame(drifter, year, month, lon, lat, depth, recruited, timer)
    df <- subset(df, df$timer == firsttime)
    
    for(i in 1:dim(xy)[1]){
      index1 <- which(round(df$lon, 2) == round(xy$V1[i],2) & round(df$lat, 2) == round(xy$V2[i],2))
      df$PixelCoast[index1] <- xy[i,3]
    }
    df <- df[,-c(8)]
    
    lat1 <- seq(-5, -20, -1)
    lat2 <- seq(-6, -21, -1)
    for(i in 1:length(lat1)){
      lati <- which(df$lat < lat1[i] & df$lat >= lat2[i])
      if(length(lati) == 0) next() else df$MeanLat[lati] <- abs(mean(c(lat1[i], lat2[i])))
    }
    df$depth <- df$depth * -1
    return(df)
  }
  
  # Gets filenames of all files in the dirpath directory  '.*\\.txt'
  filenames <- list.files(path = dirpath, pattern = '.*\\.nc', full.names = TRUE, recursive = F)
  
  # Computes recruitment for all files
  dataset <- NULL
  for(i in seq_along(filenames)){
    # Shows name of opened file on the console
    print(filenames[i])
    flush.console()
    # Computes recruitment data for file i
    data <- compute_recruitment_file(filenames[i])
    # Adds recruitment data computed for file i to those computed from all previous files
    dataset <- rbind(dataset,data)
  }
  colnames(dataset) <- c('Drifter', 'Year','Day','Lon','Lat', 'Depth', 'Recruited', 'PixelCoast','ReleaseArea')
  rownames(dataset) <- NULL
  return (dataset)
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#