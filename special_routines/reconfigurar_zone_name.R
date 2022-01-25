# library(ggplot2)
# reconfigurar_zone_name
dirpath <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5/out_simu2/results_no_shelf/'

df <- NULL
for (i in 1:12){
  file <- paste0(dirpath,'trajectoriesM',i,'.Rdata')
  print(file)
  load(file)
  
  for(j in 1:9) trajectories$Zone_name[grep(pattern = paste0('zone', j), x = trajectories$Zone_name)] <- paste0('zone', j)
  
  save(trajectories, file = file)
}

# rm(list = ls())
# 
# dirpath <- 'C:/Users/jflores/Desktop/DEB/k_x1.6/out/results_no_shelf/'
# i = 1
# file <- paste0(dirpath,'trajectoriesM',i,'.Rdata')
# print(file)
# load(file)
# levels(factor(trajectories$Zone_name))
