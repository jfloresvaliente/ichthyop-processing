library(ncdf4)
library(stringr)
source('source/compute_recruitment_ichthyop.R')
source('source/recruitment_age.R')
source('source/recruitment_area.R')
source('source/recruitment_bathy.R')
source('source/recruitment_behavior.R')
source('source/recruitment_day.R')
source('source/recruitment_depth.R')
source('source/recruitment_eps.R')
source('source/recruitment_temp.R')
source('source/recruitment_year.R')
source('source/recruitment_zone.R')

dirpath   <- 'D:/ICHTHYOP/peru10km/DistCoast/out/7zonas/'
new_path  <- 'D:/ICHTHYOP/peru10km/DistCoast/cfg/'
ymax      <- 60

#---- Do not change anythig after here ----#
nc              <- nc_open(list.files(path = dirpath, pattern = '.nc', full.names = T)[1])
cfgnc           <- ncatt_get(nc = nc, 0 , 'xml_file')$value
old_path        <- substr(x = cfgnc , start = 1 , stop = str_locate(string = cfgnc, pattern = 'cfg')[2])
firstdrifter    <- 1
lastdrifter     <- as.numeric(ncatt_get(nc , 0 , 'release.zone.number_particles')$value)
computeattime   <- length(ncvar_get(nc, 'time'))
nbreleasezones  <- ncatt_get(nc , 0 , 'nb_zones')$value -1
recruitmentzone <- 1
dates           <- read.table(paste0(new_path, 'date_scrum_time_ichthyop.csv'), header = T, sep = ';')

dat <- compute_recruitment_ichthyop(dirpath = dirpath,
                                    firstdrifter = firstdrifter,
                                    lastdrifter = lastdrifter,
                                    computeattime = computeattime,
                                    nbreleasezones = nbreleasezones,
                                    recruitmentzone = recruitmentzone,
                                    dates = dates,
                                    old_path = old_path,
                                    new_path = new_path)

for(i in 1:9){
  dat$Zone_name[grep(pattern = paste0('zone', i), x = dat$Zone_name)] <- paste0('zone', i)
}

dir.create(path = paste0(dirpath, 'results'), showWarnings = F)
write.table(x = dat, file = paste0(dirpath, '/results/ichthyop_output.csv'), sep = ';', row.names = F)

year  <- recruitment_year(dat)
day   <- recruitment_day(dat)
age   <- recruitment_age(dat)
coast <- recruitment_behavior(dat)
eps   <- recruitment_eps(dat)
temp  <- recruitment_temp(dat)
area  <- recruitment_area(dat)
depth <- recruitment_depth(dat)
bathy <- recruitment_bathy(dat)
zone  <- recruitment_zone(dat)

png(filename = paste0(dirpath, '/results/ichthyop_output.png'), height = 850, width = 1250, res = 120)
par(mfrow = c(2,3), mar = c(4,4,1,1))

yearplot <- barplot(year[,1], ylim = c(0, ymax)); abline(h = seq(0,ymax,10), lty = 3, lwd = .05)
arrows(yearplot, year[,2], yearplot, year[,3], angle = 90, code = 3, length = 0.05)

dayplot <- barplot(day[,1], ylim = c(0, ymax)); abline(h = seq(0,ymax,10), lty = 2, lwd = .25)
arrows(dayplot, day[,2], dayplot, day[,3], angle = 90, code = 3, length = 0.05)

depthplot <- barplot(depth[,1], ylim = c(0, ymax)); abline(h = seq(0,ymax,10), lty = 2, lwd = .25)
arrows(depthplot, depth[,2], depthplot, depth[,3], angle = 90, code = 3, length = 0.05)

bathyplot <- barplot(bathy[,1], ylim = c(0, ymax)); abline(h = seq(0,ymax,10), lty = 2, lwd = .25)
arrows(bathyplot, bathy[,2], bathyplot, bathy[,3], angle = 90, code = 3, length = 0.05)

# zoneplot <- barplot(zone[,1], ylim = c(0, ymax), names.arg = c('6º-8º','8º-10º','10º-12º','12º-14º')); abline(h = seq(0,ymax,10), lty = 3, lwd = .05)
zoneplot <- barplot(zone[,1], ylim = c(0, ymax)); abline(h = seq(0,ymax,10), lty = 3, lwd = .05)
arrows(zoneplot, zone[,2], zoneplot, zone[,3], angle = 90, code = 3, length = 0.05)

dev.off()

