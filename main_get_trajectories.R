#=============================================================================#
# Name   : main_get_trajectories
# Author : C. Lett; modified by Jorge Flores
# Date   : 
# Version:
# Aim    : Get trajectories from ICHTHYOP simulations
# URL    : 
#=============================================================================#
source('source/source_libraries_functions.R')

dirpath   <- 'D:/ICHTHYOP/10kmparent/Fisica-DEB/out/meso/'
new_path  <- 'D:/ICHTHYOP/10kmparent/Fisica-DEB/cfg/'

#---- Do not change anythig after here ----#
ncfile          <- list.files(path = dirpath, pattern = '.nc', full.names = T)[1]
nc              <- nc_open(ncfile)
cfgnc           <- gsub(pattern = '\\\\', replacement = '/', x = ncatt_get(nc = nc, 0 , 'xml_file')$value)
old_path        <- substr(x = cfgnc , start = 1 , stop = str_locate(string = cfgnc, pattern = 'cfg')[2])
firstdrifter    <- 1
lastdrifter     <- as.numeric(ncatt_get(nc , 0 , 'release.zone.number_particles')$value)
firsttime       <- 1
lasttime        <- length(ncvar_get(nc, 'time'))
recruitmentzone <- 1
variname        <- c('E','length')

# The paths of all .nc ichthyop files will be extracted month by month and combined into a single .RData file
dat <- read.table(paste0(dirpath, '/results/ichthyop_output.csv'), header = T, sep = ';')

for(i in 1:1){
  month <- subset(dat, dat$Day == i)
  month <- levels(factor(month$Name_file))
  
  trajectories <- NULL
  for(j in 1:length(month)){
    ncfile <- paste0(dirpath, month[j], '.nc')
    print(ncfile)
    trajs <- get_trajectories(ncfile  = ncfile
                     ,firstdrifter    = firstdrifter
                     ,lastdrifter     = lastdrifter
                     ,firsttime       = firsttime
                     ,lasttime        = lasttime
                     ,recruitmentzone = recruitmentzone
                     ,old_path        = old_path
                     ,new_path        = new_path
                     ,variname        = variname)
    trajectories <- rbind(trajectories, trajs)
  }
  trajectories$Drifter <- rep(seq(1, lastdrifter*length(month)), each = lasttime)
  # Saving on object in RData format
  RData <- paste0(dirpath, '/results/', 'trajectoriesM', i, '.Rdata')
  print(paste0('saving .....', RData))
  save(trajectories, file = RData)
}
#=============================================================================#
# END OF PROGRAM
#=============================================================================#




 

# #------------------------------------------------#
# dat <- get_trajectories(ncfile = ncfile, old_path = old_path, new_path = new_path)
# dat$Depth <- abs(dat$Depth) * -1
# 
# for (i in 1:9) {
#   dat$Zone_name[grep(pattern = paste0('zone', i), x = dat$Zone_name)] <- paste0('zone', i)
# }
# 
# recsi <- subset(dat, dat$IfRecruited == 1)
# recno <- subset(dat, dat$IfRecruited == 0)
# 
# timerA <- NULL
# timerB <- NULL
# for(i in levels(factor(dat$Zone_name))){
#   bathyA <- subset(recsi, recsi$Zone_name == i)
#   bathyB <- subset(recno, recno$Zone_name == i)
#   
#   trajA <- tapply(bathyA$Depth, bathyA$Timer, mean)
#   trajB <- tapply(bathyB$Depth, bathyB$Timer, mean)
#   
#   timerA <- cbind(timerA, trajA)
#   timerB <- cbind(timerB, trajB)
# }
# 
# plot(1:31, ylim = c(-40, -10), type = 'n')
# lty = 0
# pch = 0
# for(i in 1:4){
#   lty = lty + 1;  pch = pch + 1
#   
#   lines(timerA[,i], pch = pch, type = 'o', col = 'red')
#   lines(timerB[,i], pch = pch, type = 'o', col = 'blue')
# }
# 
# legend('topright',legend = levels(factor(dat$Zone_name)), lty = 1, pch = 1:4, bty = 'n')
# 
# 
# # bathy1 <- subset(dat, dat$ReleaseBathy == '0-50')
# # bathy2 <- subset(dat, dat$ReleaseBathy == '50-100')
# 
# # traj1 <- tapply(bathy1$Depth, list(bathy1$Timer), mean)
# # traj2 <- tapply(bathy2$Depth, list(bathy2$Timer), mean)
# 
# # range(traj2)
# # 
# # df <- bathy2
# # # dat$Depth <- abs(dat$Depth)
# # # df <- subset(dat, dat$IfRecruited == 1 & dat$Timer == 29)
# # 
# # zlim <- c(0,70)
# # xlimmap <- c(-85, -75)
# # ylimmap <- c(-16, -5)
# # 
# # map <- ggplot(data = df, aes(x = Lon, y = Lat))
# # map <- map +
# #   # geom_point(data = df, aes(x = Lon, y = Lat, colour = Depth), size = .1) +
# #   geom_path(data = df, aes(group = Drifter, x = Lon, y = Lat, colour = Depth), size = .25) +
# #   scale_colour_gradientn(colours = tim.colors(n = 64, alpha = 1), limits = zlim, expression(Depth (m))) +
# #   labs(x = 'Longitude (W)', y = 'Latitude (S)') +
# #   borders(fill='grey',colour='grey') +
# #   coord_fixed(xlim = xlimmap, ylim = ylimmap, ratio = 2/2) +
# #   theme(axis.text.x  = element_text(face='bold', color='black', size=15, angle=0),
# #         axis.text.y  = element_text(face='bold', color='black', size=15, angle=0),
# #         axis.title.x = element_text(face='bold', color='black', size=15, angle=0),
# #         axis.title.y = element_text(face='bold', color='black', size=15, angle=90),
# #         legend.text  = element_text(size=15),
# #         legend.title = element_text(size=15, face= 'bold'),
# #         legend.position   = c(0.9, 0.85),
# #         legend.background = element_rect(fill=adjustcolor( 'red', alpha.f = 0), size=0.5, linetype='solid'))
# # x11()
# # map
# # # if(!is.null(PNG)) ggsave(filename = PNG, width = 9, height = 9) else map
# # # print(PNG); flush.console()
