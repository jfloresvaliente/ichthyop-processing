#=============================================================================#
# Name   : get_data_atRecruitmentAgeDirecto_V
# Author : C. Lett; modified by Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Get trajectories from ICHTHYOP outputs (.nc)
# URL    : 
#=============================================================================#
get_data_atRecruitmentAgeDirecto_V <- function(
    dirpath            = NULL
    ,firstdrifter      = 1
    ,lastdrifter       = 5000
    ,firsttime         = 1
    ,lasttime          = 31
    ,recruitmentzone   = 1
    ,old_path          = old_path
    ,new_path          = new_path
    ,dates             = dates
    ,variname          = NULL
    ,N0                = 1 # Initial value of the particle affected by mortality
    ,freq_record       = 1 # Record frequency in Ichthyop
    ,V                 = 0.003982
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
  # variname = name of environmental variable tracking
  # N0, Initial value of the particle affected by mortality
  # v = Structure threshold at metamorphosis
  
  
  # The '.Rdata' output file will have the form.....
  # ['Drifter','Timer','Lon','Lat','Depth','IfRecruited','Mortality','ReleaseArea',
  # 'Year','Month','t_x','Zone_name','ReleaseDepth','ReleaseBathy]'
  
  # Then you can calculate new features.
  # Do not forget to add them in the 'return'
  
  #============ ============ Arguments ============ ============#
  
  library(ncdf4)
  library(stringr)
  library(XML)
  
  # An inner function that computes recruitment for one .nc output file
  get_data_atRecruitmentAge_file <- function(filename){
    # An inner function that computes year and day from time in seconds
    compute_yearday <- function(time){
      nbdays <- 1 + time/86400
      year   <- 1 + as.integer(nbdays/360)
      day    <- as.integer(nbdays-360*(year-1))
      return(c(year,day))
    }
    
    nc <- nc_open(filename)
    
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
    
    # Get the value of recruited for the recruitment zone considered for all drifters at time of computation
    # recruited <- as.vector(t(ncvar_get(nc, 'recruited_zone', c(recruitmentzone, firstdrifter, firsttime), c(recruitmentzone, lastdrifter, lasttime))))
    recruited <- as.vector(t(ncvar_get(nc, 'V') >= V))
    
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
    
    for(i in 1:9) df$Zone_name[grep(pattern = paste0('zone', i), x = df$Zone_name)] <- paste0('zone', i)
    
    nc_close(nc)
    
    # Add mortality
    talla <- df$length/10 #convert from mm to cm
    
    # Get length-dependent mortality matrix (M_length)
    M_length <- 0.189 * exp( -(talla) / 2.468) # Brochier et al 2018, dividir entre 10, porque se aplica en cm
    M_length <- matrix(data = M_length, ncol = lasttime, nrow = dim(df)[1]/lasttime, byrow = T)
    
    # Get value of each particle affected by length-dependent mortality (N_length)
    N_length <- matrix(data = NA, ncol = lasttime, nrow = dim(df)[1]/lasttime, byrow = T)
    N_length[,1] <- N0
    
    for(i in 2:lasttime){
      N_length[,i] <- N_length[,i-1] * (1 - M_length[,i-1])
    }
    df$N_length <- as.vector(t(N_length))
    
    # Get constant mortality matrix (M_constant)
    M_constant <- matrix(data = 0.1, ncol = lasttime, nrow = dim(df)[1]/lasttime, byrow = T)
    
    # Get value of each particle affected by constant mortality (N_constant)
    N_constant <- matrix(data = NA, ncol = lasttime, nrow = dim(df)[1]/lasttime, byrow = T)
    N_constant[,1] <- N0
    
    for(i in 2:lasttime){
      N_constant[,i] <- N_constant[,i-1] * (1 - M_constant[,i-1])
    }
    df$N_constant <- as.vector(t(N_constant))
    
    # Get data at recruitment age
    df$Age <- (df$Timer-1)/freq_record
    
    # Get recruited particles
    recruited_drifter <- subset(df, df$Timer == lasttime & df$IfRecruited == 1)$Drifter
    recruited         <- subset(df, df$Drifter %in% recruited_drifter)
    recruitment_timer_matrix <- matrix(data = recruited$IfRecruited, ncol = lasttime, nrow = dim(recruited)[1]/lasttime, byrow = T)
    recruitment_timer <- apply(recruitment_timer_matrix, 1, which.max)
    
    for(j in seq_along(recruitment_timer)){
      inicol <- recruitment_timer[j]
      
      if(inicol == lasttime) next()
      recruitment_timer_matrix[j, (inicol+1) : lasttime] <- NA
    }
    recruitment_timer_matrix <- as.vector(t(recruitment_timer_matrix))
    
    Lon_ini <- subset(recruited, recruited$Timer == 1)$Lon
    Lat_ini <- subset(recruited, recruited$Timer == 1)$Lat
    
    recruited <- recruited[which(recruitment_timer_matrix == 1),]
    recruited$Lon_ini <- Lon_ini
    recruited$Lat_ini <- Lat_ini
    
    # Get non-recruited particles
    non_recruited <- subset(df, df$Drifter %nin% recruited_drifter)
    
    non_recruited_ini <- subset(non_recruited, non_recruited$Timer == 1)
    Lon_ini <- subset(non_recruited_ini, non_recruited_ini$Timer == 1)$Lon
    Lat_ini <- subset(non_recruited_ini, non_recruited_ini$Timer == 1)$Lat
    
    non_recruited_fin <- subset(non_recruited, non_recruited$Timer == lasttime)
    non_recruited_fin$Lon_ini <- Lon_ini
    non_recruited_fin$Lat_ini <- Lat_ini
    
    particles <- rbind(recruited, non_recruited_fin)
    
    rm_col <- c('Drifter','Timer','Depth','length','MESO','temp')
    rm_col_ind <- NULL
    for(i in 1:length(rm_col)){
      rm_ind <- which(names(particles) == rm_col[i])
      rm_col_ind <- c(rm_col_ind, rm_ind)
    }
    
    df <- particles[,-c(rm_col_ind)]
    return(df)
  }
  
  # Get filenames of all files in the dirpath directory  '.*\\.txt'
  filenames <- list.files(path = dirpath, pattern = '.*\\.nc', full.names = TRUE, recursive = F)
  
  # Compute recruitment for all files
  dataset <- NULL
  for(i in seq_along(filenames)){
    # Shows name of opened file on the console
    print(filenames[i])
    flush.console()
    # Compute 'get_data...' function for file in loop [i]
    data <- get_data_atRecruitmentAge_file(filenames[i])
    # Add 'get_data...' computed for file in loop [i] to those computed from all previous files
    dataset <- rbind(dataset,data)
  }
  
  names_col <- c('Lon_end', 'Lat_end', 'IfRecruited', 'Mortality', 'ReleaseArea',
                 'Year', 'Month', 't_x','Zone_name', 'ReleaseDepth',
                 'ReleaseBathy', 'N_length', 'N_constant', 'Age','Lon_ini','Lat_ini')
  colnames(dataset) <- names_col
  rownames(dataset) <- NULL
  return (dataset)
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#