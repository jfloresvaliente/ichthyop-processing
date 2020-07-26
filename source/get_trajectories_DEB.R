#=============================================================================#
# Name   : get_trajectories_DEB
# Author : C. Lett; modified by Jorge Flores
# Date   : 
# Version:
# Aim    : Compute recruitment ICHTHYOP outputs
# URL    : 
#=============================================================================#
get_trajectories_DEB <- function(
  ncfile           = NULL
  ,firstdrifter    = 1
  ,lastdrifter     = 5000
  ,firsttime       = 1
  ,lasttime        = 31
  ,recruitmentzone = 1
  ,old_path
  ,new_path
  ,variname        = NULL
  ,length_min      = 20
  ,depth_min       = 50
  ,polyg           = polyg
  ,days            = 30
){
  #============ ============ Arguments ============ ============#
  
  # ncfile = ncdf file which contains ICHTHYOP outputs
  
  # In case one wishes to consider only a subset of all drifters
  # firstdrifter = Index of first drifter to be computed
  # lastdrifter  = Index of last drifter to be computed
  # firsttime    = Index of first time to be computed
  # lasttime     = Index of last  time to be computed
  
  # recruitmentzone = The index of the recruitment zone for which recruitment is computed
  
  # To read 'xml' files from a directory different to original directory where files were stored
  # old_path = path written in each ncdf input file as attribute
  # new_path = path where '.xml' files are stored
  
  # variname = name of environmental variable tracking
  # length_min = minimum length (in mm) to consider a particle as recruited
  
  # Then you can calculate new features.
  # Do not forget to add them in the 'return' of the 'compute_recruitment_file' internal function
  
  #============ ============ Arguments ============ ============#
  
  library(ncdf4)
  library(stringr)
  library(XML)
  
  nc <- nc_open(ncfile)
  
  # Test if a particle is considered as recruited
  lon       <- ncvar_get(nc, 'lon')
  lat       <- ncvar_get(nc, 'lat')
  xy        <- cbind(as.vector(lon), as.vector(lat))
  talla     <- ncvar_get(nc, 'length')
  depth     <- abs(ncvar_get(nc, 'depth'))
  
  cond1 <- talla >= length_min
  cond2 <- depth <= depth_min
  cond3 <- matrix(data = in.out(bnd = as.matrix(polyg), x = xy), nrow = lastdrifter, ncol = lasttime)
  
  recruited <- cond1 + cond2 + cond3
  recruited[recruited != 3] <- 0
  recruited[recruited == 3] <- 1  
  
  for(timer in 1:lasttime){
    if(sum(recruited[,timer]) != 0){
      drifs <- which(recruited[,timer] == 1)
      recruited[c(drifs), timer:lasttime] <- 1
    }
  }
  recruited <- as.vector(t(recruited))
  
  drifter <- rep(seq(firstdrifter, lastdrifter), each = lasttime)
  timer   <- rep(seq(firsttime, lasttime), times = lastdrifter)
  lon     <- as.vector(t(ncvar_get(nc, 'lon',   c(firstdrifter, firsttime), c(lastdrifter, lasttime))))
  lat     <- as.vector(t(ncvar_get(nc, 'lat',   c(firstdrifter, firsttime), c(lastdrifter, lasttime))))
  depth   <- as.vector(t(ncvar_get(nc, 'depth', c(firstdrifter, firsttime), c(lastdrifter, lasttime))))
  
  # Gets the value of release zone for all drifters
  releasezone <- ncvar_get(nc,'zone',c(1,firstdrifter,1),c(1,lastdrifter,1)) + 1
  releasezone <- rep(releasezone, each = lasttime)
  
  df <- data.frame(drifter, timer, lon, lat, depth, recruited, releasezone)

  # Reads the XML release zones file
  # filezone <- gsub(pattern = '\\\\', replacement = '/', x = ncatt_get(nc = nc, 0 , 'release.bottom.zone_file')$value) # if you release particles from BOTTOM
  filezone <- gsub(pattern = '\\\\', replacement = '/', x = ncatt_get(nc = nc, 0 , 'release.zone.zone_file')$value)
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
  
  if(is.null(variname)){
    colnames(df) <- c('Drifter', 'Timer','Lon','Lat', 'Depth', 'IfRecruited', 'ReleaseArea', 'Zone_name','ReleaseDepth','ReleaseBathy')
  }else{
    for(i in 1:length(variname)){
      vari    <- as.vector(t(ncvar_get(nc, variname[i],c(firstdrifter, firsttime), c(lastdrifter, lasttime))))
      df <- cbind(df, vari)
    }
    colnames(df) <- c('Drifter', 'Timer','Lon','Lat', 'Depth', 'IfRecruited', 'ReleaseArea', 'Zone_name','ReleaseDepth','ReleaseBathy', variname)
  }
  nc_close(nc)
  
  df <- subset(df, df$Timer %in% seq(1,lasttime, length.out = (days+1)))
  df$Timer <- rep(1:(days+1), times = lastdrifter)

  rownames(df) <- NULL
  return(df)
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#