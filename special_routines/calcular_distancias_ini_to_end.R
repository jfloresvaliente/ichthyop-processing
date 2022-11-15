# medir distancias entre inicio y fin de particulas

library(geosphere)
dirpath  <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5_TCseuil0.052/case1/results/'
load(file = paste0(dirpath, 'data_atRecruitmentAge.Rdata'))
dat <- df; rm(df)

m <- list(length(12))
for(i in 1:12){
  print(i)
  df <- subset(dat, dat$IfRecruited == 1 & dat$Month == i & dat$ReleaseDepth == '0-15')
  xy1 <- cbind(df$Lon_ini, df$Lat_ini)
  xy2 <- cbind(df$Lon_end, df$Lat_end)
  a <- distm(xy1, xy2, fun = distCosine)/1000
  m[[i]] <- diag(a)
  
  boxplot(m)
}
mean(m[[6]])


# xy1 <- rbind(c(0,0),c(0,1),c(0,2),c(0,3))
# # distm(xy)
# xy2 <- rbind(c(0,0),c(0,1),c(0,2),c(0,3))
# a <- distm(xy1, xy2)
# diag(x = a)
