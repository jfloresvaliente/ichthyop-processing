#=============================================================================#
# Name   : compute_retention_ichthyop
# Author : C. Lett; modified by Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Compute recruitment from ICHTHYOP outputs (.nc)
# URL    : 
#=============================================================================#
compute_retention_ichthyop <- function(
  dirpath         = dirpath
  ,firstdrifter    = 1
  ,lastdrifter     = 5000
  ,computeattime   = 31
  ,nbreleasezones  = 1
  ,recruitmentzone = 1
  ,old_path        = old_path
  ,new_path        = new_path
  ,dates           = dates
  ,recruit_zone    = recruit_zone
){
  
  #============ ============ Arguments ============ ============#
  
  # dirpath = Directory path which contains series of ICHTHYOP outputs (.nc)
  
  # In case one wishes to consider only a subset from all drifters
  # firstdrifter = Index of first drifter to be computed
  # lastdrifter  = Index of last drifter to be computed
  
  # computeattime   = The time record at which to compute recruitment
  # nbreleasezones  = The number of release zones
  # recruitmentzone = The index of the recruitment zone for which recruitment is computed
  
  # To read configuration files (.xml) from a different directory to original directory where files were stored
  # old_path = path written in each ncdf input file as attribute
  # new_path = path where '.xml' files are stored
  
  # dates = (.csv) file with YEAR/MONTH index to match with t0 (beginning of simulation)
  
  # The '.csv' output file will have the form.....
  # ['NumberReleased','NumberRecruited','ReleaseArea','Year','Month','Eps','Age','Coast_Behavior',...
  # 'Temp_min','Name_file','t_x','Zone_name','Depth','Bathy','TotalParticles','Recruitprop]'
  
  # Then one can calculate new features.
  # Do not forget to add them in the 'return' of the 'compute_recruitment_file' internal function
  
  #============ ============ Arguments ============ ============#
  
  library(ncdf4)
  library(XML)
  library(stringr)
  
  # An inner function that computes recruitment for one .nc output file
  compute_recruitment_file <- function(filename){
    
    # An inner function that computes year and day from time in seconds
    compute_yearday <- function(time){
      nbdays <- 1 + time/86400
      year   <- 1 + as.integer(nbdays/360)
      day    <- as.integer(nbdays-360*(year-1))
      return(c(year,day))
    }
    
    nc <- nc_open(filename)
    
    lon <- ncvar_get(nc,'lon')
    lat <- ncvar_get(nc,'lat')
    
    lon_lat <- cbind(lon[,computeattime], lat[,computeattime])
    lon_lat <- in.out(bnd = recruit_zone, x = lon_lat)
    
    # Get the value of time of release
    t0 <- ncvar_get(nc,'time',1,1)
    
    # Computes year and day of release
    yearday <- compute_yearday(t0)
    
    # Get real release dates
    dates <- subset(dates, dates$year == yearday[1] & dates$day == yearday[2])
    
    # Scrum time of released particles like t0,t5,t10...
    t_x <- dates$t_x
    
    # Get the year and month of release particles from 'times'
    year    <- dates$Y
    month   <- dates$M
    yearday <- c(year,month)
    
    # Read .XML release zones file
    # filezone <- gsub(pattern = '\\\\', replacement = '/', x = ncatt_get(nc = nc, 0 , 'release.bottom.zone_file')$value) # if you release particles from BOTTOM
    filezone <- gsub(pattern = '\\\\', replacement = '/', x = ncatt_get(nc = nc, 0 , 'release.zone.zone_file')$value)
    filezone <- gsub(pattern = old_path, replacement = new_path, filezone)
    filezone <- xmlTreeParse(filezone, useInternalNode = TRUE)
    
    # Get bathymetry limits for each release zone
    inshore  <- xmlToDataFrame(nodes = getNodeSet(filezone, '//zone/bathy_mask/line_inshore'))
    inshore  <- as.numeric(as.character(inshore[,1]))
    offshore <- xmlToDataFrame(nodes = getNodeSet(filezone, '//zone/bathy_mask/line_offshore'))
    offshore <- as.numeric(as.character(offshore[,1]))
    
    # Get spawning depth limits for each release zone
    mindepth <- xmlToDataFrame(nodes = getNodeSet(filezone, '//zone/thickness/upper_depth'))
    mindepth <- as.numeric(as.character(mindepth[,1]))
    maxdepth <- xmlToDataFrame(nodes = getNodeSet(filezone, '//zone/thickness/lower_depth'))
    maxdepth <- as.numeric(as.character(maxdepth[,1]))
    
    # Get spawning zones names
    zone_names <- xmlToDataFrame(nodes = getNodeSet(filezone, '//zone/key'))
    zone_names <- as.character(zone_names[,1])
    
    zone_char <- NULL
    for(i in 1:length(zone_names)){
      zon <- c(zone_names[i], paste0(mindepth[i],'-',maxdepth[i]), paste0(inshore[i],'-',offshore[i]))
      zone_char <- rbind(zone_char, zon)
    }
    
    # Get the value for 'Cold Lethal Temperature' for larvae. 'temp_min' will be '0' if it was not activated in the model
    temp_min <- ncatt_get(nc,0,'action.lethal_temp.cold_lethal_temperature_larva')$value
    
    # Get the value for 'Age minimal recruitment'
    age <- ncatt_get(nc , 0 , 'action.recruitment.zone.limit_age')$value
    
    # Get the value for 'coastline_behavior'
    coast_behavior <- ncatt_get(nc , 0 , 'app.transport.coastline_behavior')$value
    
    # Get the value for 'disipation rate'
    epsilon <- ncatt_get(nc , 0 , 'action.hdisp.epsilon')$value
    
    # Get the value of recruited for the recruitment zone considered for all drifters at time of computation
    nbdrifter <- lastdrifter - firstdrifter + 1
    # recruited <- ncvar_get(nc,'recruited_zone',c(recruitmentzone,firstdrifter,computeattime),c(1,nbdrifter,1))
    recruited <- lon_lat
    # Get the value of release zone for all drifters
    releasezone <- ncvar_get(nc,'zone',c(1,firstdrifter,1),c(1,nbdrifter,1)) + 1
    
    # Calculate the number of recruits from every release zone
    recruitnb <- hist(recruited*releasezone,seq(0,nbreleasezones+1)-0.5,plot=FALSE)$counts[2:(nbreleasezones+1)]
    
    # Calculate the number of released from every release zone
    releasenb <- hist(releasezone,seq(0,nbreleasezones+1)-0.5,plot=FALSE)$counts[2:(nbreleasezones+1)]
    
    # Get the name (not full name) of the '.nc' file
    name_file <- str_remove(string = nc$filename, pattern = dirpath)
    
    # Get the total number of particles released
    particles <- ncatt_get(nc , 0 , 'release.zone.number_particles')$value 
    
    nc_close(nc)
    # returns a collage of columns, i.e., a table, that looks like the following
    # releasenb1 recruitnb1 1 year month depth
    # releasenb2 recruitnb2 2 year month depth
    # releasenb3 recruitnb3 3 year month depth
    # ...
    return(cbind(
      releasenb
      ,recruitnb
      ,seq(1,nbreleasezones)
      ,rep(yearday[1],nbreleasezones)
      ,rep(yearday[2],nbreleasezones)
      ,rep(epsilon,nbreleasezones)
      ,rep(age,nbreleasezones)
      ,rep(coast_behavior,nbreleasezones)
      ,rep(temp_min,nbreleasezones)
      ,rep(name_file,nbreleasezones)
      ,rep(t_x,nbreleasezones)
      ,zone_char
      ,particles
    ))
  }
  
  # Get filenames of all files in the dirpath directory  '.*\\.txt'
  filenames <- list.files(path = dirpath, pattern = '.*\\.nc', full.names = TRUE, recursive = F)
  
  # Compute recruitment for all files
  dataset <- NULL
  for(i in seq_along(filenames)){
    # Shows name of opened file on the console
    print(filenames[i])
    flush.console()
    # Compute recruitment data for file in loop [i]
    data <- compute_recruitment_file(filenames[i])
    # Add recruitment data computed for file in loop [i] to those computed from all previous files
    dataset <- rbind(dataset,data)
  }
  
  recruitprop <- 100*as.numeric(dataset[,2])/as.numeric(dataset[,1])
  dataset <- as.data.frame(cbind(dataset , recruitprop), stringsAsFactors = FALSE)
  
  rownames(dataset) <- NULL
  colnames(dataset) <- c(
    'NumberReleased'
    ,'NumberRecruited'
    ,'ReleaseArea'
    ,'Year'
    ,'Month'
    ,'Eps'
    ,'Age'
    ,'Coast_Behavior'
    ,'Temp_min'
    ,'Name_file'
    ,'t_x'
    ,'Zone_name'
    ,'Depth'
    ,'Bathy'
    ,'TotalParticles'
    ,'Recruitprop'
  )
  
  dataset$NumberReleased  <- as.numeric(dataset$NumberReleased)
  dataset$NumberRecruited <- as.numeric(dataset$NumberRecruited)
  dataset$ReleaseArea     <- as.numeric(dataset$ReleaseArea)
  dataset$Year            <- as.numeric(dataset$Year)
  dataset$Month           <- as.numeric(dataset$Month)
  dataset$Eps             <- as.numeric(dataset$Eps)
  dataset$Age             <- as.numeric(dataset$Age)
  dataset$Temp_min        <- as.numeric(dataset$Temp_min)
  dataset$Recruitprop     <- as.numeric(dataset$Recruitprop)
  
  return (dataset)
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#