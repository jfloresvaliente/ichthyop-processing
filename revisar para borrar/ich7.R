source('source/ichthyop_libraries.R')

dirpath   <- 'C:/Users/jflores/Desktop/ich_test/out/'
new_path  <- 'C:/Users/jflores/Desktop/ich_test/cfg/'

drif <- 100
step_time <- 361

nc <- nc_open(list.files(path = dirpath, pattern = 'nc', full.names = T)[1])

# Test if a particle is considered as recruited
lon       <- ncvar_get(nc, 'lon')
lat       <- ncvar_get(nc, 'lat')
xy        <- cbind(as.vector(lon), as.vector(lat))
polyg <- read.table(paste0(new_path, 'ichthyop_recruitment_polygon.txt'))

in_out <- matrix(data = in.out(bnd = as.matrix(polyg), x = xy), nrow = drif, ncol = step_time)

lon <- as.vector(t(ncvar_get(nc, 'lon')))
lat <- as.vector(t(ncvar_get(nc, 'lat')))
depth <- as.vector(t(ncvar_get(nc, 'depth')))
talla <- as.vector(t(ncvar_get(nc, 'length')))
recruited <- as.vector(t(ncvar_get(nc, 'recruited_zone')))
in_out <- as.vector(t(in_out))

drifter <- rep(seq(1, drif), each = step_time)
timer   <- rep(seq(1, step_time), times = drif)

df <- data.frame(drifter, timer, lon, lat, depth, talla, recruited, in_out)
# write.table(x = df, file = paste0(dirpath, 'traj1.csv'), sep = ';', row.names = F)
 
rec <- ncvar_get(nc, 'recruited_zone')
which(rec[,step_time] == 1)

dat <- subset(df, df$timer == step_time)
dat1 <- subset(dat, dat$recruited == 1); dim(dat1)
dat2 <- subset(dat, dat$in_out == T); dim(dat2)

dat <- df
# Get Edad de reclutamiento
index <- subset(dat, dat$timer == (step_time) & dat$recruited == 1)$drifter
recruited <- subset(dat, dat$drifter %in% index)
recruited <- matrix(data = recruited$recruited, ncol = (step_time), nrow = dim(recruited)[1]/(step_time), byrow = T)
recruited_day <- apply(recruited, 1, which.max)
hist(recruited_day, breaks = seq(0,70,1),
     ylim = c(0, 5), xlim = c(0,30),
     xlab = '', ylab = '', main = '')
# mtext(text = paste('Month', i), side = 3, line = -2, adj = 0.1)
# mtext(text = 'Recruited Age', side = 1, line = 2.5)

parti <- subset(df, df$drifter == 3)
plot(parti$timer/12, parti$talla, type ='l')

# a <- subset(df, df$drifter == 96); #View(a)
# plot(1, xlim = c(-85, -70), ylim = c(-20,0))
# plot(1, xlim = c(-74, -70), ylim = c(-20,-16))
# lines(polyg$V1, polyg$V2)
# lines(a$lon, a$lat, col = 'red')




talla <- ncvar_get(nc, 'length'); dim(talla)
hist(talla[,31])
