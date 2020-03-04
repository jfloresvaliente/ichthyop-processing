#=============================================================================#
# Name   : get_trajectories_drifters_DEB
# Author : C. Lett; modified by Jorge Flores
# Date   : 
# Version:
# Aim    : Compute recruitment ICHTHYOP outputs
# URL    : 
#=============================================================================#
get_trajectories_drifters_DEB <- function(
  ncfile = NULL
  ,firstdrifter = 1
  ,lastdrifter = 5000
  ,firsttime = 1
  ,lasttime = 31
  ,recruitmentzone = 1
  ,old_path
  ,new_path
  ,variname = NULL
  ,length_min      = 20
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
  
  # get length#
  nbdrifter <- lastdrifter-firstdrifter+1
  talla <- ncvar_get(nc,'length', c(firstdrifter,lasttime),c(nbdrifter,1))
  talla[talla <  length_min] <- 0
  talla[talla >= length_min] <- 1
  
  # Gets the value of recruited for the recruitment zone considered for all drifters at time of computation
  recruited <- ncvar_get(nc,'recruited_zone',c(recruitmentzone,firstdrifter,lasttime),c(1,lastdrifter,1))
  recruited <- recruited + talla
  recruited[recruited != 2] <- 0
  recruited[recruited == 2] <- 1
  recruited <- rep(recruited, each = lasttime)
  
  df <- data.frame(drifter, timer, lon, lat, depth, recruited)
  
  if(is.null(variname)){
    colnames(df) <- c('Drifter', 'Timer','Lon','Lat', 'Depth', 'IfRecruited')
  }else{
    for(i in 1:length(variname)){
      vari    <- as.vector(t(ncvar_get(nc, variname[i],c(firstdrifter, firsttime), c(lastdrifter, lasttime))))
      df <- cbind(df, vari)
    }
    colnames(df) <- c('Drifter', 'Timer','Lon','Lat', 'Depth', 'IfRecruited', variname)
  }
  nc_close(nc)
  rownames(df) <- NULL
  return(df)
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#