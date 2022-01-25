#=============================================================================#
# Name   : get_trajectories_noshelf
# Author : C. Lett; modified by Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Get trajectories from ICHTHYOP outputs (.nc)
# URL    : 
#=============================================================================#
get_trajectories_noshelf <- function(
  ncfile            = NULL
  ,firstdrifter      = 1
  ,lastdrifter       = 5000
  ,firsttime         = 1
  ,lasttime          = 31
  ,recruitmentzone   = 1
  ,old_path          = old_path
  ,new_path          = new_path
  ,dates             = dates
  ,continental_shelf = 1
  ,length_min        = 20
  ,variname          = NULL
){
  #============ ============ Arguments ============ ============#
  
  # ncfile = ICHTHYOP output file (.nc)
  
  # In case one wishes to consider only a subset of all drifters
  # firstdrifter = Index of first drifter to be computed
  # lastdrifter  = Index of last drifter to be computed
  # firsttime    = Index of first time to be computed
  # lasttime     = Index of last  time to be computed
  
  # recruitmentzone = The index of the recruitment zone for which recruitment is computed
  
  # To read configuration files (.xml) from a different directory to original directory where files were stored
  # old_path = path written in each ncdf input file as attribute
  # new_path = path where '.xml' files are stored
  
  # dates = (.csv) file with YEAR/MONTH index to match with t0 (beginning of simulation)
  # length_min = 20; minimum size to calculate recruitment (mm)
  
  # variname = name of environmental variable tracking
  
  # The '.Rdata' output file will have the form.....
  # ['Drifter','Timer','Lon','Lat','Depth','IfRecruited','Mortality','ReleaseArea',
  # 'Year','Month','t_x','Zone_name','ReleaseDepth','ReleaseBathy]'
  
  # Then you can calculate new features.
  # Do not forget to add them in the 'return'
  
  #============ ============ Arguments ============ ============#
  
  library(ncdf4)
  library(stringr)
  library(XML)
  
  # An inner function that computes year and day from time in seconds
  compute_yearday <- function(time){
    nbdays <- 1 + time/86400
    year   <- 1 + as.integer(nbdays/360)
    day    <- as.integer(nbdays-360*(year-1))
    return(c(year,day))
  }
  
  nc <- nc_open(ncfile)
  
  # Get the value of time of release
  t0 <- ncvar_get(nc,'time',1,1)
  
  # Computes year and day of release
  yearday <- compute_yearday(t0)
  
  # Get real release dates
  dates <- subset(dates, dates$year == yearday[1] & dates$day == yearday[2])
  
  # Scrum time of released particles like t0,t5,t10...
  t_x <- dates$t_x
  
  # Get the year and month of release particles from 'times'
  year      <- dates$Y
  month     <- dates$M
  
  drifter   <- rep(seq(firstdrifter, lastdrifter), each = lasttime)
  timer     <- rep(seq(firsttime, lasttime), times = lastdrifter)
  lon       <- as.vector(t(ncvar_get(nc, 'lon',   c(firstdrifter, firsttime), c(lastdrifter, lasttime))))
  lat       <- as.vector(t(ncvar_get(nc, 'lat',   c(firstdrifter, firsttime), c(lastdrifter, lasttime))))
  depth     <- as.vector(t(ncvar_get(nc, 'depth', c(firstdrifter, firsttime), c(lastdrifter, lasttime))))
  mortality <- as.vector(t(ncvar_get(nc, 'mortality', c(firstdrifter, firsttime), c(lastdrifter, lasttime))))
  
  # Gets the value of recruited for the recruitment zone considered for all drifters at time of computation
  recruited <- ncvar_get(nc, 'length')
  recruited[recruited <  length_min] <- 0
  recruited[recruited >= length_min] <- 1
  recruited <- as.vector(t(recruited))

  # Gets the value of release zone for all drifters
  releasezone <- ncvar_get(nc,'zone',c(1,firstdrifter,1),c(1,lastdrifter,1)) + 1
  releasezone <- rep(releasezone, each = lasttime)
  
  df <- data.frame(drifter, timer, lon, lat, depth, recruited, mortality, releasezone, year, month, t_x)
  
  # Read the XML release zones file
  # filezone <- gsub(pattern = '\\\\', replacement = '/', x = ncatt_get(nc = nc, 0 , 'release.bottom.zone_file')$value) # if you release particles from BOTTOM
  filezone <- gsub(pattern = '\\\\', replacement = '/', x = ncatt_get(nc = nc, 0 , 'release.zone.zone_file')$value)
  filezone <- gsub(pattern = old_path, replacement = new_path, filezone)
  filezone <- xmlTreeParse(filezone, useInternalNode=TRUE)
  
  # Get bathymetry limits
  inshore  <- xmlToDataFrame(nodes = getNodeSet(filezone, '//zone/bathy_mask/line_inshore'))
  inshore  <- as.numeric(as.character(inshore[,1]))
  offshore <- xmlToDataFrame(nodes = getNodeSet(filezone, '//zone/bathy_mask/line_offshore'))
  offshore <- as.numeric(as.character(offshore[,1]))
  
  # Get spawning depth limits
  mindepth <- xmlToDataFrame(nodes = getNodeSet(filezone, '//zone/thickness/upper_depth'))
  mindepth <- as.numeric(as.character(mindepth[,1]))
  maxdepth <- xmlToDataFrame(nodes = getNodeSet(filezone, '//zone/thickness/lower_depth'))
  maxdepth <- as.numeric(as.character(maxdepth[,1]))
  
  # Get spawning zones names
  zone_names <- xmlToDataFrame(nodes = getNodeSet(filezone, '//zone/key'))
  zone_names <- as.character(zone_names[,1])
  
  zone_charac <- NULL
  for(i in 1:length(zone_names)){
    df$ZoneName    [df$releasezone == i] <- zone_names[i]
    df$ReleaseDepth[df$releasezone == i] <- paste0(mindepth[i],'-',maxdepth[i])
    df$ReleaseBathy[df$releasezone == i] <- paste0(inshore[i],'-',offshore[i])
  }
  
  if(is.null(variname)){
    colnames(df) <- c('Drifter','Timer','Lon','Lat','Depth','IfRecruited','Mortality','ReleaseArea','Year','Month','t_x','Zone_name','ReleaseDepth','ReleaseBathy')
  }else{
    for(i in 1:length(variname)){
      vari    <- as.vector(t(ncvar_get(nc, variname[i],c(firstdrifter, firsttime), c(lastdrifter, lasttime))))
      df <- cbind(df, vari)
    }
    colnames(df) <- c('Drifter','Timer','Lon','Lat','Depth','IfRecruited','Mortality','ReleaseArea','Year','Month','t_x','Zone_name','ReleaseDepth','ReleaseBathy',variname)
  }
  nc_close(nc)
  rownames(df) <- NULL
  return(df)
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#