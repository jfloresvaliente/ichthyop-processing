#=============================================================================#
# Name   : compute_recruitment_ichthyop
# Author : C. Lett; modified by Jorge Flores
# Date   : 
# Version:
# Aim    : Compute recruitment ICHTHYOP outputs
# URL    : 
#=============================================================================#
compute_recruitment_ichthyop <- function(
  dirpath
  ,firstdrifter = 1
  ,lastdrifter = 20000
  ,computeattime = 55
  ,nbreleasezones = 1
  ,recruitmentzone = 1
  ,ymax
  ,dates
  ,old_path
  ,new_path
){
  #============ ============ Arguments ============ ============#
  
  # dirpath = Directory path which contains series of ICHTHYOP netcdf outputs
  
  # In case one wishes to consider only a subset of all drifters
  # firstdrifter = Index of first drifter to be compued
  # lastdrifter  = Index of last  drifter to be computed
  
  # computeattime   = The time record at which to compute recruitment
  # nbreleasezones  = The number of release zones
  # recruitmentzone = The index of the recruitment zone for which recruitment is computed
  # ymax            = The maximum value of the larval retention/transport success for 'ylabel' in the plot (in %)
  
  # To read 'xml' files from a directory different to original directory where files were stored
  # old_path = path written in each ncdf input file as attribute
  # new_path = path where '.xml' files are stored
  
  # The '.csv' output file will have the form.....
  # ['NumberReleased','NumberRecruited','ReleaseArea','Year','Day',...
  # ...'Depth','Age','Coast_Behavior','Temp_min','name_file','Recruitprop']
  # Then you can calculate new features.
  # Do not forget to add them in the 'return' of the 'compute_recruitment_file' internal function
  
  #============ ============ Arguments ============ ============#
  
  library(ncdf4)
  library(XML)
  library(stringr)
  
  # An inner function that computes recruitment for one file
  compute_recruitment_file <- function(filename){
    
    # An inner function that computes year and day from time in seconds
    compute_yearday <- function(time){
      nbdays <- 1+time/86400
      year <- 1+as.integer(nbdays/360)
      day <- as.integer(nbdays-360*(year-1))
      #if (day < 10) day <- paste('0',day,sep='')
      #if (as.numeric(day) < 100) day <- paste('0',day,sep='')
      return(c(year,day))
    }
    
    nc <- nc_open(paste0(filename))
    
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
    yearday <- c(year,month)
    
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
      # print(i)
      zon <- c(zone_names[i], paste0(mindepth[i],'-',maxdepth[i]), paste0(inshore[i],'-',offshore[i]))
      # zon <- c(paste0(mindepth[i],'-',maxdepth[i]), paste0(inshore[i],'-',offshore[i]))
      zone_charac <- rbind(zone_charac, zon)
    }
    
    # Gets the value for 'Cold Lethal Temperature' for larvae. 'temp_min' will be '0' if it was not activated in the model
    temp_min <- ncatt_get(nc,0,'action.lethal_temp.cold_lethal_temperature_larva')$value
    
    # Gets the value for 'Age minimal recruitment'
    age <- ncatt_get(nc , 0 , 'action.recruitment.zone.limit_age')$value
    
    # Gets the value for 'coastline_behavior'
    coast_behavior <- ncatt_get(nc , 0 , 'app.transport.coastline_behavior')$value
    
    # Gets the value for 'disipation rate'
    epsilon <- ncatt_get(nc , 0 , 'action.hdisp.epsilon')$value
    
    nbdrifter <- lastdrifter-firstdrifter+1
    
    # Gets the value of recruited for the recruitment zone considered for all drifters at time of computation
    recruited <- ncvar_get(nc,'recruited_zone',c(recruitmentzone,firstdrifter,computeattime),c(1,nbdrifter,1))
    
    # Gets the value of release zone for all drifters
    releasezone <- ncvar_get(nc,'zone',c(1,firstdrifter,1),c(1,nbdrifter,1))
    releasezone <- releasezone + 1
    
    # Calculates the number of recruits from every release zone
    recruitnb <- hist(recruited*releasezone,seq(0,nbreleasezones+1)-0.5,plot=FALSE)$counts[2:(nbreleasezones+1)]
    
    # Calculates the number of released from every release zone
    releasenb <- hist(releasezone,seq(0,nbreleasezones+1)-0.5,plot=FALSE)$counts[2:(nbreleasezones+1)]
    
    #Gets the name (not full name) of the '.nc' file
    m <- str_locate(string = nc$filename, pattern = '_s') # Begin position of name
    n <- str_locate(string = nc$filename, pattern = '.nc') # End position of name
    name_file <- substr(nc$filename , start = m[2]+1 , stop = n[1]-1)
    
    nc_close(nc)
    # returns a collage of columns, i.e., a table, that looks like the following
    # releasenb1 recruitnb1 1 year day depth
    # releasenb2 recruitnb2 2 year day depth
    # releasenb3 recruitnb3 3 year day depth
    # ...
    return(cbind(
      releasenb
      ,recruitnb
      ,seq(1,nbreleasezones)
      ,rep(yearday[1],nbreleasezones)
      ,rep(yearday[2],nbreleasezones)
      ,rep(epsilon,nbreleasezones)
      # ,rep(depth,nbreleasezones)
      ,rep(age,nbreleasezones)
      ,rep(coast_behavior,nbreleasezones)
      ,rep(temp_min,nbreleasezones)
      ,rep(name_file, nbreleasezones)
      ,zone_charac
    ))
  }
  
  # An inner function that computes statistics of recruitment for a given factor
  # compute_recruitment_stats <- function(released,recruited,factor){
  #   mean <- tapply(recruited,factor,sum)/tapply(released,factor,sum)
  #   var  <- tapply(recruited^2,factor,sum)/tapply(released^2,factor,sum)-mean^2
  #   sem  <- sqrt(var/table(factor))
  #   return(cbind(mean,sem))
  # }
  
  # Gets filenames of all files in the dirpath directory  '.*\\.txt'
  filenames <- list.files(path = dirpath, pattern = '.*\\.nc', full.names = TRUE, recursive = T)
  
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
  
  # # Computes stats (mean, std error of the mean) of recruitment for every 'release area, year, day, and depth'
  # dataarea_stats=compute_recruitment_stats(released = as.numeric(dataset[,1]), recruited = as.numeric(dataset[,2]), factor = as.factor(dataset[,10]))
  # dataarea_mean=dataarea_stats[,1]
  # dataarea_sem=dataarea_stats[,2]
  # 
  # datayear_stats=compute_recruitment_stats(as.numeric(dataset[,1]),as.numeric(dataset[,2]),as.numeric(dataset[,4]))
  # datayear_mean=datayear_stats[,1]
  # datayear_sem=datayear_stats[,2]
  # 
  # dataday_stats=compute_recruitment_stats(as.numeric(dataset[,1]),as.numeric(dataset[,2]),as.numeric(dataset[,5]))
  # dataday_mean=dataday_stats[,1]
  # dataday_sem=dataday_stats[,2]
  # 
  # datadepth_stats=compute_recruitment_stats(as.numeric(dataset[,1]),as.numeric(dataset[,2]),dataset[,11])
  # datadepth_mean=datadepth_stats[,1]
  # datadepth_sem=datadepth_stats[,2]
  
  # Makes the corresponding plots, only for 'release area, year, day, and depth' factors
  # x11() ; par(mfrow=c(2,2))
  # 
  # areaplot <- barplot(100*dataarea_mean,xlab='Release area',ylab='Transport success (%)',ylim = c(0,ymax))
  # arrows(areaplot,100*(dataarea_mean+dataarea_sem),areaplot,100*(dataarea_mean-dataarea_sem),angle=90,code=3,length=0.05)
  # 
  # dayplot <- barplot(100*dataday_mean,xlab='Release day',ylab='Transport success (%)',ylim = c(0,ymax))
  # arrows(dayplot,100*(dataday_mean+dataday_sem),dayplot,100*(dataday_mean-dataday_sem),angle=90,code=3,length=0.05)
  # 
  # yearplot <- barplot(100*datayear_mean,xlab='Release year',ylab='Transport success (%)',ylim = c(0,ymax))
  # arrows(yearplot,100*(datayear_mean+datayear_sem),yearplot,100*(datayear_mean-datayear_sem),angle=90,code=3,length=0.05)
  # 
  # depthplot <- barplot(100*datadepth_mean,xlab='Release depth (m)',ylab='Transport success (%)',ylim = c(0,ymax))
  # arrows(depthplot,100*(datadepth_mean+datadepth_sem),depthplot,100*(datadepth_mean-datadepth_sem),angle=90,code=3,length=0.05)
  # 
  recruitprop <- 100*as.numeric(dataset[,2])/as.numeric(dataset[,1])
  dataset <- as.data.frame(cbind(dataset , recruitprop), stringsAsFactors = FALSE)
  
  colnames(dataset) <- c(
    'NumberReleased'
    ,'NumberRecruited'
    ,'ReleaseArea'
    ,'Year'
    ,'Day'
    ,'Eps'
    ,'Age'
    ,'Coast_Behavior'
    ,'Temp_min'
    ,'Name_file'
    ,'Zone_name'
    ,'Depth'
    ,'Bathy'
    ,'Recruitprop'
  )
  dataset$NumberReleased  <- as.numeric(dataset$NumberReleased)
  dataset$NumberRecruited <- as.numeric(dataset$NumberRecruited)
  dataset$ReleaseArea <- as.numeric(dataset$ReleaseArea)
  dataset$Year <- as.numeric(dataset$Year)
  dataset$Day <- as.numeric(dataset$Day)
  dataset$Eps <- as.numeric(dataset$Eps)
  dataset$Age <- as.numeric(dataset$Age)
  dataset$Temp_min <- as.numeric(dataset$Temp_min)
  dataset$Recruitprop <- as.numeric(dataset$Recruitprop)

  rownames(dataset) <- NULL
  return (dataset)
  #mod <- lm(recruitprop ~ factor(ReleaseArea) + factor(Day) + factor(Year) + factor(Depth)
  #			+ factor(ReleaseArea):factor(Day) + factor(ReleaseArea):factor(Year) + factor(ReleaseArea):factor(Depth)
  #			+ factor(Day):factor(Year) + factor(Day):factor(Depth) + factor(Year):factor(Depth), data = dataset)
  #aov <- anova(mod)
  #print(aov)
  #print(100 * aov[2] / sum(aov[2]))
  #interaction.plot(dataset$Day,dataset$ReleaseArea,recruitprop,fixed=TRUE,xlab='Release day',ylab='Transport success (%)',lty=1,col=seq(1,length(areaplot)))
  #interaction.plot(dataset$Depth,dataset$ReleaseArea,recruitprop,fixed=TRUE,xlab='Release depth (m)',ylab='Transport success (%)',lty=1,col=seq(1,length(areaplot)))
  #interaction.plot(dataset$Day,dataset$Year,recruitprop,fixed=TRUE,xlab='Release day',ylab='Transport success (%)',lty=1,col=seq(1,length(yearplot)))
}

#=============================================================================#
# END OF PROGRAM
#=============================================================================#