#=============================================================================#
# Name   : get_scrumtime_roms
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : get scrumTime de ROMS files
# URL    : 
#=============================================================================#
timer <- function(
  dirpath
  ,yearini = 2008
  ,yearend = 2011
  ,monthini = 1
  ,monthend = 12
){
  #============ ============ Arguments ============ ============#
  # dirpath  = Directory path which contains ROMS netcdf files
  # yearini  = Initial year of ROMS silumation
  # yearend  = Final year of ROMS simulation
  # monthini = Inital month of ROMS simulation
  # monthend = Final month of ROMS simulation
  #============ ============ Arguments ============ ============#
  library(ncdf4)
  
  # An inner function that computes year and day from time in seconds
  compute_yearday <- function(time){
    nbdays <- 1+time/86400
    year   <- 1+as.integer(nbdays/360)
    day    <- as.integer(nbdays-360*(year-1))
    day2  <- day
    
    if(day %% 30 == 0) month <- day/30 else month  <- 1+as.integer(day/30)
    if(day > 30)       day <- day - (month-1) * 30
    
    return(cbind(year,day2,year,month,day))
  }
  
  # Gets filenames of all files in the dirpath directory 
  file_date <- NULL
  for(i in yearini:yearend){
    for(j in monthini:monthend){
      
      # You must change the particular 'name' of each ROMS simulation
      
      # ncfile <- paste0(dirpath, 'roms_avg_Y',i, 'M',j,'.newperushtopoP.nc')
      ncfile <- paste0(dirpath, 'roms_avg_Y',i,'M',j,'.AscatMerClim.nc')
      # ncfile <- paste0(dirpath, 'roms_avg_6d_Y',i,'M',j,'.AscatMerClim.nc')
      # ncfile <- paste0(dirpath, 'roms6b_avg.Y',i,'.M',j,'.rl1b.nc')
      # ncfile <- paste0(dirpath, 'newperush_avg.Y',i,'.M',j,'.newperush.nc')
      # ncfile <- paste0(dirpath, 'roms_avg_Y',i,'M',j,'.AscatMerClim.nc')
      # ncfile <- paste0(dirpath, 'sacw3_avg.Y',i,'M',j,'.nc')
      # ncfile <- paste0(dirpath, 'roms6b_avg.Y',i,'.M',j,'.rsodi1.nc')
      # ncfile <- paste0(dirpath, 'roms_avg_Y',i,'M',j,'.Jaard10kmClim.nc')
      # ncfile <- paste0(dirpath, 'roms_avg_6d_Y',i,'M',j,'.AscatMerClim.nc')
      
      print(ncfile)
      nc <- nc_open(ncfile)
      
      # Gets the value of time (scrum time in seconds), year and day
      t0 <- ncvar_get(nc, 'time')
      # t0 <- ncvar_get(nc, 'scrum_time')
      
      year_day <- NULL
      for (k in 1:length(t0)) {
        year_day2 <- compute_yearday(t0[k])
        year_day <- rbind(year_day, year_day2)
      }
      # year_day <- compute_yearday(t0)
      
      Y <- rep(i, times = length(t0))
      M <- rep(j, times = length(t0))
      t_x <- 1:length(t0)
      
      date <- cbind(t0, year_day, Y, M, t_x, ncfile)
      file_date <- rbind(file_date, date)
      nc_close(nc)
    }
  }

  colnames(file_date) <- c('t0'
                           ,'year'
                           ,'day'
                           ,'sim_year'
                           ,'sim_month'
                           ,'sim_day'
                           ,'Y'
                           ,'M'
                           ,'t_x'
                           ,'name_file'
  )
  return(file_date)
}
dirpath <- 'E:/ROMS_SIMULATIONS/peru10km/'
a <- timer(dirpath = dirpath, yearini = 2008, yearend = 2011)
write.table(x = a, file = paste0(dirpath, 'date_scrum_time_ichthyop.csv'), sep = ';', row.names = F)
#=============================================================================#
# END OF PROGRAM
#=============================================================================#