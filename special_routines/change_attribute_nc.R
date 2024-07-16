library(ncdf4)

dirpath <- 'E:/ICHTHYOP/rsodi1/DEB_TC5_TCseuil0.052abj/case2kx0.6/'
nc_files <- list.files(path = dirpath, pattern = 'nc', full.names = T)

for(i in 1:length(nc_files)){
  nc <- nc_open(nc_files[i], write = T)
  
  old_name <- ncatt_get(nc = nc, varid = 0, attname = 'release.zone.zone_file')$value
  new_name <- '/media/ummisco/LaCie/JORGE/ROMS_SIMULATIONS/rsodi1/DEB_TC5_TCseuil0.052abj/cfg/release.xml'
  ncatt_put(nc = nc, varid = 0, attname = 'release.zone.zone_file', attval = new_name)
  
  old_name <- ncatt_get(nc = nc, varid = 0, attname = 'action.recruitment.zone.zone_file')$value
  new_name <- '/media/ummisco/LaCie/JORGE/ROMS_SIMULATIONS/rsodi1/DEB_TC5_TCseuil0.052abj/cfg/retention.xml'
  ncatt_put(nc = nc, varid = 0, attname = 'action.recruitment.zone.zone_file', attval = new_name)
  
  nc_close(nc)
  print(nc_files[i])
}
