#=============================================================================#
# Name   : main_compute_recruitment_ichthyop
# Author : C. Lett; modified by Jorge Flores
# Date   : 
# Version:
# Aim    : Compute recruitment ICHTHYOP outputs
# URL    : 
#=============================================================================#
source('source/ichthyop_libraries.R')
source('source/ichthyop_functions.R')

dirpath  <- 'E:/ICHTHYOP/10kmparent/DEB/out/encrasicolus40m/'
new_path <- 'E:/ICHTHYOP/10kmparent/DEB/cfg/'
ymax     <- 70
lats     <- seq(from = 2, to = 20, by = 2)
hlines   <- seq(0,ymax,10)
years    <- seq(1:1) # number of years

#---- Do not change anythig after here ----#
nc              <- nc_open(list.files(path = dirpath, pattern = '.nc', full.names = T)[1])
cfgnc           <- gsub(pattern = '\\\\', replacement = '/', x = ncatt_get(nc = nc, 0 , 'xml_file')$value)
old_path        <- substr(x = cfgnc , start = 1 , stop = str_locate(string = cfgnc, pattern = 'cfg')[2])
firstdrifter    <- 1
lastdrifter     <- as.numeric(ncatt_get(nc , 0 , 'release.zone.number_particles')$value)
computeattime   <- length(ncvar_get(nc, 'time'))
nbreleasezones  <- ncatt_get(nc , 0 , 'nb_zones')$value -1
recruitmentzone <- 1
dates           <- read.table(paste0(new_path, 'date_scrum_time_ichthyop.csv'), header = T, sep = ';')
nc_close(nc)

dat <- compute_recruitment_ichthyop(dirpath          = dirpath,
                                    firstdrifter     = firstdrifter
                                    ,lastdrifter     = lastdrifter
                                    ,computeattime   = computeattime
                                    ,nbreleasezones  = nbreleasezones
                                    ,recruitmentzone = recruitmentzone
                                    ,old_path        = old_path
                                    ,new_path        = new_path
                                    ,dates           = dates
)

for(i in 1:9) dat$Zone_name[grep(pattern = paste0('zone', i), x = dat$Zone_name)] <- paste0('zone', i)
dir.create(path = paste0(dirpath, 'results'), showWarnings = F)
write.table(x = dat, file = paste0(dirpath, '/results/ichthyop_output.csv'), sep = ';', row.names = F)

# dat <- read.table(paste0(dirpath, '/results/ichthyop_output.csv'), sep = ';', header = T)
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
par(mfrow = c(2,3), mar = c(5,4,1,1))

yearlab <- NULL
for(i in 1:length(years)) yearlab <- c(yearlab, paste0('Y', years[i]))

yearplot <- barplot(year[,1], ylim = c(0, ymax), axes = F, names.arg = yearlab)
axis(2, las = 2)
abline(h = hlines, lty = 3, lwd = .05)
arrows(yearplot, year[,2], yearplot, year[,3], angle = 90, code = 3, length = 0.05)
mtext(text = 'Release Year', side = 1, line = 2, cex = 0.75)

dayplot <- barplot(day[,1], ylim = c(0, ymax), axes = F)
axis(2, las = 2)
abline(h = hlines, lty = 3, lwd = .05)
arrows(dayplot, day[,2], dayplot, day[,3], angle = 90, code = 3, length = 0.05)
mtext(text = 'Release Month', side = 1, line = 2, cex = 0.75)

depthplot <- barplot(depth[,1], ylim = c(0, ymax), axes = F, names.arg = rownames(depth))
axis(2, las = 2)
abline(h = hlines, lty = 3, lwd = .05)
arrows(depthplot, depth[,2], depthplot, depth[,3], angle = 90, code = 3, length = 0.05)
mtext(text = 'Release Depth', side = 1, line = 2, cex = 0.75)

bathyplot <- barplot(bathy[,1], ylim = c(0, ymax), axes = F)
axis(2, las = 2)
abline(h = hlines, lty = 3, lwd = .05)
arrows(bathyplot, bathy[,2], bathyplot, bathy[,3], angle = 90, code = 3, length = 0.05)
mtext(text = 'Release Bathymetry', side = 1, line = 2, cex = 0.75)

latlab <- NULL
for(i in 1:(length(lats)-1)) latlab <- c(latlab, paste0(lats[i],'º-', lats[i] + 2, 'º'))

zoneplot <- barplot(zone[,1], ylim = c(0, ymax), names.arg = latlab, axisnames = FALSE, axes = F)
axis(1, at=dayplot, labels = FALSE, tick = FALSE)
text(zoneplot, par('usr')[3], labels = latlab, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex = .85)
axis(2, las = 2)
abline(h = hlines, lty = 3, lwd = .05)
arrows(zoneplot, zone[,2], zoneplot, zone[,3], angle = 90, code = 3, length = 0.05)
mtext(text = 'Release Latitude', side = 1, line = 2, cex = 0.75)

# areaplot <- barplot(area[,1], ylim = c(0, ymax)); abline(h = seq(0,ymax,10), lty = 3, lwd = .05)
# arrows(areaplot, area[,2], areaplot, area[,3], angle = 90, code = 3, length = 0.05)

dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#