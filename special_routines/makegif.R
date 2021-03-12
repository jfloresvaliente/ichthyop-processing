library(ggplot2)
dirpath <- 'C:/Users/jflores/Documents/JORGE/ICHTHYOP/10kmparent/DEB/k_x1.6/out/results/'
dir.create(path = paste0(dirpath, 'gif'), showWarnings = F)

xlim     <- c(-85, -70) # Londitude
ylim     <- c(-20, 0)   # Latitude

###############---
Rdata <- paste0(dirpath,'trajectoriesM',1,'.Rdata')
load(Rdata)
print(Rdata)
dat <- trajectories; rm(trajectories)

rec_index <- subset(dat, dat$Timer == 61 & dat$IfRecruited == 1)

dat <- subset(dat, dat$Drifter %in% rec_index$Drifter)

for(i in 1:61){
  sub_dat <- subset(dat, dat$Timer == i)
  ggname <- paste0(dirpath,'gif/timer',i,'.png')
  ggplot(data = sub_dat, mapping = aes(x = Lon, y = Lat))+
    geom_point(size = 0.05)+
    borders(fill='grey',colour='grey') +
    coord_fixed(xlim = xlim, ylim = ylim)+
  ggsave(filename = ggname, plot = last_plot(), width = 8, height = 8)
}

setwd(paste0(dirpath,'gif/'))
library(magick)
# library(ggmap)
library(purrr)
# Step 2: List those Plots, Read them in, and then make animation
list.files(path = "C:/Users/jflores/Documents/JORGE/ICHTHYOP/DEB/out/results/gif/Nueva carpeta", pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2) %>% # animates, can opt for number of loops
  image_write("ndwi_aug_hgm.gif") # write to current dir
