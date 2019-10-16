#=============================================================================#
# Name   : get_trajectories
# Author : C. Lett; modified by Jorge Flores
# Date   : 
# Version:
# Aim    : Compute recruitment ICHTHYOP outputs
# URL    : 
#=============================================================================#
get_trajectories <- function(
  ncfile = NULL
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
  
  nc <- nc_open(ncfile)
  
  drifter <- rep(seq(firstdrifter, lastdrifter), each = lasttime)
  timer   <- rep(seq(firsttime, lasttime), times = lastdrifter)
  lon     <- as.vector(t(ncvar_get(nc, 'lon',   c(firstdrifter, firsttime), c(lastdrifter, lasttime))))
  lat     <- as.vector(t(ncvar_get(nc, 'lat',   c(firstdrifter, firsttime), c(lastdrifter, lasttime))))
  depth   <- as.vector(t(ncvar_get(nc, 'depth', c(firstdrifter, firsttime), c(lastdrifter, lasttime))))
  
  # Gets the value of recruited for the recruitment zone considered for all drifters at time of computation
  recruited <- ncvar_get(nc,'recruited_zone',c(recruitmentzone,firstdrifter,lasttime),c(1,lastdrifter,1))
  recruited <- rep(recruited, each = lasttime)
  
  # Gets the value of release zone for all drifters
  releasezone <- ncvar_get(nc,'zone',c(1,firstdrifter,1),c(1,lastdrifter,1)) + 1
  releasezone <- rep(releasezone, each = lasttime)
  
  df <- data.frame(drifter, timer, lon, lat, depth, recruited, releasezone)
  
  # Reads the XML release zones file
  # filezone <- ncatt_get(nc,0,'release.bottom.zone_file')$value ## if you release particles from BOTTOM
  filezone <- ncatt_get(nc,0,'release.zone.zone_file')$value
  filezone <- gsub(pattern = old_path, replacement = new_path, filezone)
  filezone <- xmlTreeParse(filezone, useInternalNode=TRUE)
  
  # Gets bathymetry limits
  inshore  <- xmlToDataFrame(nodes = getNodeSet(filezone, '//zone/bathy_mask/line_inshore'))
  inshore  <- as.numeric(as.character(inshore[,1]))
  offshore <- xmlToDataFrame(nodes = getNodeSet(filezone, '//zone/bathy_mask/line_offshore'))
  offshore <- as.numeric(as.character(offshore[,1]))
  
  # Gets spawning depth limits
  mindepth <- xmlToDataFrame(nodes = getNodeSet(filezone, '//zone/thickness/upper_depth'))
  mindepth <- as.numeric(as.character(mindepth[,1]))
  maxdepth <- xmlToDataFrame(nodes = getNodeSet(filezone, '//zone/thickness/lower_depth'))
  maxdepth <- as.numeric(as.character(maxdepth[,1]))
  
  # Gets spawning zones names
  zone_names <- xmlToDataFrame(nodes = getNodeSet(filezone, '//zone/key'))
  zone_names <- as.character(zone_names[,1])
  
  zone_charac <- NULL
  for(i in 1:length(zone_names)){
    df$ZoneName    [df$releasezone == i] <- zone_names[i]
    df$ReleaseDepth[df$releasezone == i] <- paste0(mindepth[i],'-',maxdepth[i])
    df$ReleaseBathy[df$releasezone == i] <- paste0(inshore[i],'-',offshore[i])
  }
  
  colnames(df) <- c('Drifter', 'Timer','Lon','Lat', 'Depth', 'IfRecruited', 'ReleaseArea', 'Zone_name','ReleaseDepth','ReleaseBathy')
  return(df)
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#