#=============================================================================#
# Name   : get_trajectories_drifters
# Author : C. Lett; modified by Jorge Flores
# Date   : 
# Version:
# Aim    : Compute recruitment ICHTHYOP outputs
# URL    : 
#=============================================================================#
get_trajectories_drifters <- function(
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
  
  df <- data.frame(drifter, timer, lon, lat, depth, recruited)
  colnames(df) <- c('Drifter', 'Timer','Lon','Lat', 'Depth', 'IfRecruited')
  return(df)
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#