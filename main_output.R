library(ncdf4)
source('source/compute_recruitment_ichthyop.R')

# Siempre se debe abrir un archivo .nc para verificar los parametros de la simulacion
nc <- nc_open('D:/ICHTHYOP/peru10km/ichthyopPeruCoast/out/out_ichthyop-run201902211232_s1.nc')
dirpath    <- 'D:/ICHTHYOP/peru10km/ichthyopPeruCoast/out/'
new_path   <- 'D:/ICHTHYOP/peru10km/ichthyopPeruCoast/cfg/'
old_path   <- '/run/media/jtam/JORGE_NEW/ICHTHYOP/peru10km/ichthyopPeruCoast/cfg/'

firstdrifter = 1
lastdrifter = 20000
computeattime = 31
nbreleasezones = 4
recruitmentzone = 1
ymax = 60
dates <- read.table(paste0(new_path, 'date_scrum_time_ichthyop.csv'), header = T, sep = ';')

dat <- compute_recruitment_ichthyop(dirpath = dirpath,
                                    firstdrifter = firstdrifter,
                                    lastdrifter = lastdrifter,
                                    computeattime = computeattime,
                                    nbreleasezones = nbreleasezones,
                                    recruitmentzone = recruitmentzone,
                                    ymax = ymax,
                                    dates = dates,
                                    old_path = old_path,
                                    new_path = new_path)

dir.create(path = paste0(dirpath, 'results'), showWarnings = F)
write.table(x = dat, file = paste0(dirpath, '/results/ichthyop_output.csv'), sep = ';', row.names = F)

# year  <- tapply(as.numeric(dat$Recruitprop), list(dat$Year), mean, na.rm = T)
# day   <- tapply(as.numeric(dat$Recruitprop), list(dat$Month), mean, na.rm = T)
# age   <- tapply(as.numeric(dat$Recruitprop), list(dat$Age), mean, na.rm = T)
# coast <- tapply(as.numeric(dat$Recruitprop), list(dat$Coast_Behavior), mean, na.rm = T)
# eps   <- tapply(as.numeric(dat$Recruitprop), list(dat$Eps), mean, na.rm = T)
# temp  <- tapply(as.numeric(dat$Recruitprop), list(dat$Temp_min), mean, na.rm = T)
# area  <- tapply(as.numeric(dat$Recruitprop), list(dat$Zone_name), mean, na.rm = T)
# depth <- tapply(as.numeric(dat$Recruitprop), list(dat$Depth), mean, na.rm = T)
# bathy <- tapply(as.numeric(dat$Recruitprop), list(dat$Bathy), mean, na.rm = T)
# 
# png(filename = paste0(dirpath, '/results/ichthyop_output.png'), height = 850, width = 1250, res = 120)
# par(mfrow = c(2,3), mar = c(4,4,1,1))
# barplot(year, ylim = c(0, ymax)); abline(h = seq(0,ymax,10), lty = 2, lwd = .25)
# barplot(day, ylim = c(0, ymax)); abline(h = seq(0,ymax,10), lty = 2, lwd = .25)
# barplot(coast, ylim = c(0, ymax)); abline(h = seq(0,ymax,10), lty = 2, lwd = .25)
# barplot(eps, ylim = c(0, ymax)); abline(h = seq(0,ymax,10), lty = 2, lwd = .25)
# barplot(area, ylim = c(0, ymax)); abline(h = seq(0,ymax,10), lty = 2, lwd = .25)
# barplot(depth, ylim = c(0, ymax)); abline(h = seq(0,ymax,10), lty = 2, lwd = .25)
# barplot(bathy, ylim = c(0, ymax)); abline(h = seq(0,ymax,10), lty = 2, lwd = .25)
# dev.off()
