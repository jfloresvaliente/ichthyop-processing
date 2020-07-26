#=============================================================================#
# Name   : compute_recruitment_ichthyop_drifters_DEB
# Author : C. Lett; modified by Jorge Flores
# Date   : 
# Version:
# Aim    : Compute recruitment ICHTHYOP outputs
# URL    : 
#=============================================================================#
compute_recruitment_ichthyop_drifters_DEB <- function(
  dirpath          = dirpath
  ,firstdrifter    = 1
  ,lastdrifter     = 5000
  ,firsttime       = 1
  ,lasttime        = 31
  ,recruitmentzone = 1
  ,dates           = dates
  ,xy              = xy
  ,length_min      = 20
  ,depth_min       = 50
  ,polyg           = polyg
){
  #============ ============ Arguments ============ ============#
  
  # dirpath = Directory path which contains series of ICHTHYOP netcdf outputs
  
  # In case one wishes to consider only a subset of all drifters
  # firstdrifter = Index of first drifter to be computed
  # lastdrifter  = Index of last drifter to be computed
  # firsttime    = Index of first time to be computed
  # lasttime     = Index of last  time to be computed
  
  # recruitmentzone = The index of the recruitment zone for which recruitment is computed
  
  # dates = .csv file with YEAR/MONTH index to match with t0
  
  # length_min = minimum length (in mm) to consider a particle as recruited
  
  # The '.csv' output file will have the form.....
  # ['Drifter','Year','Day','Lon,'Lat','Depth','Recruited','Name_file','PixelCoast','ReleaseArea']
  
  # The '.txt' file with the initial positions of all particles and its pixel-distance to the coast
  
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

    # Test if a particle is considered as recruited
    lon       <- ncvar_get(nc, 'lon')
    lat       <- ncvar_get(nc, 'lat')
    xy_par    <- cbind(as.vector(lon), as.vector(lat))
    talla     <- ncvar_get(nc, 'length')
    depth     <- abs(ncvar_get(nc, 'depth'))
    
    cond1 <- talla >= length_min
    cond2 <- depth <= depth_min
    cond3 <- matrix(data = in.out(bnd = as.matrix(polyg), x = xy_par), nrow = lastdrifter, ncol = lasttime)
    
    recruited <- cond1 + cond2 + cond3
    recruited[recruited != 3] <- 0
    recruited[recruited == 3] <- 1  
    
    for(timer in 1:lasttime){
      if(sum(recruited[,timer]) != 0){
        drifs <- which(recruited[,timer] == 1)
        recruited[c(drifs), timer:lasttime] <- 1
      }
    }
    recruited <- recruited[,lasttime]
    recruited <- rep(recruited, each = lasttime)

    drifter <- rep(seq(firstdrifter, lastdrifter), each = lasttime)
    timer   <- rep(seq(firsttime, lasttime), times = lastdrifter)
    lon     <- as.vector(t(ncvar_get(nc, 'lon',   c(firstdrifter, firsttime), c(lastdrifter, lasttime))))
    lat     <- as.vector(t(ncvar_get(nc, 'lat',   c(firstdrifter, firsttime), c(lastdrifter, lasttime))))
    depth   <- as.vector(t(ncvar_get(nc, 'depth', c(firstdrifter, firsttime), c(lastdrifter, lasttime))))
    
    #Gets the name (not full name) of the '.nc' file
    m <- str_locate(string = nc$filename, pattern = '/out_ichthyop') # Begin position of name
    n <- str_locate(string = nc$filename, pattern = '.nc') # End position of name
    name_file <- substr(nc$filename , start = m[1]+1 , stop = n[1]-1)
    
    df <- data.frame(drifter, year, month, lon, lat, depth, recruited, timer, name_file)
    df <- subset(df, df$timer == firsttime)
    
    df$PixelCoast <- rep(NA, dim(df)[1])
    for(i in 1:dim(xy)[1]){
      index1 <- which(round(df$lon, 2) == round(xy$V1[i],2) & round(df$lat, 2) == round(xy$V2[i],2))
      df$PixelCoast[c(index1)] <- xy[i,3]
    }
    df <- df[,-c(8)]
    
    lats <- round(range(xy[,2]))
    lat1 <- seq(lats[2]+1, lats[1]-1, -1)
    lat2 <- lat1 - 1
    for(i in 1:length(lat1)){
      lati <- which(df$lat < lat1[i] & df$lat >= lat2[i])
      if(length(lati) == 0) next() else df$MeanLat[lati] <- abs(mean(c(lat1[i], lat2[i])))
    }
    df$depth <- abs(df$depth)
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
  
  rownames(dataset) <- NULL
  colnames(dataset) <- c(
    'Drifter'
    ,'Year'
    ,'Day'
    ,'Lon'
    ,'Lat'
    ,'Depth'
    ,'IfRecruited'
    ,'Name_file'
    ,'PixelCoast'
    ,'ReleaseArea'
  )
  return (dataset)
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#