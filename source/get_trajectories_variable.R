#=============================================================================#
# Name   : get_trajectories_variable
# Author : C. Lett; modified by Jorge Flores
# Date   : 
# Version:
# Aim    : Compute recruitment ICHTHYOP outputs
# URL    : 
#=============================================================================#
get_trajectories_variable <- function(
  ncfile = NULL
  ,firstdrifter = 1
  ,lastdrifter = 5000
  ,firsttime = 1
  ,lasttime = 31
  ,recruitmentzone = 1
  ,old_path
  ,new_path
  ,variname = 'temp'
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
  
  df <- data.frame(drifter, timer, lon, lat)
  for(i in 1:length(variname)){
    vari    <- as.vector(t(ncvar_get(nc, variname[i],c(firstdrifter, firsttime), c(lastdrifter, lasttime))))
    df <- cbind(df, vari)
  }
  colnames(df) <- c('Drifter', 'Timer','Lon','Lat', variname)
  return(df)
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#