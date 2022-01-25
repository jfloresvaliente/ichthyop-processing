library(ncdf4)

dirpath <- 'C:/Users/jflores/Desktop/output/'

lastday      <- 150
firstdrifter <- 1
lastdrifter  <- 1000
firsttime    <- 1
lasttime     <- (lastday * 24) + 1

#==================#
nc_list <- list.files(path = dirpath, pattern = '.nc', full.names = T)

df <- NULL
for(i in 1:length(nc_list)){
  
  nc <- nc_open(filename = nc_list[i])
  drifter   <- rep(seq(firstdrifter, lastdrifter), each = lasttime)
  timer     <- (rep(seq(firsttime, lasttime), times = lastdrifter)-1)/24 # Poner tiempo en dias
  lon       <- as.vector(t(ncvar_get(nc, 'lon',   c(firstdrifter, firsttime), c(lastdrifter, lasttime))))
  lat       <- as.vector(t(ncvar_get(nc, 'lat',   c(firstdrifter, firsttime), c(lastdrifter, lasttime))))
  nc_close(nc)
  
  nc <- data.frame(drifter, timer, lon, lat)
  
  df <- rbind(df, nc)
}
df$drifter <- rep(1:(lastdrifter*length(nc_list)), each = lasttime)
Rdata <- paste0(dirpath, 'REPSOL_trajs.Rdata')
save(df, file = Rdata)