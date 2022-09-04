library(R.matlab)
dirpath   <- 'D:/ROMS_SILUMATIONS/rsodi1/interpolatedYearMonth/'

namevar   <- 'MESO'
k_x       <- NULL # en caso se quiere calcular f, la 'namevar' debe ser MESO y k_x diferente de NULL
xy        <- read.table(paste0(dirpath, 'release_zone_rowcol_index.txt'))
mask      <- as.matrix(read.table(paste0(dirpath, 'mask.txt')))
ver_lev   <- as.vector(read.table(paste0(dirpath, 'depth.txt'), header = T))[,1]
depth_lim <- range(ver_lev)   # Latitude extension of the area 
nlevels   <- 50               # Number of levels in the color palette
years     <- c(1980, 2000)    # Years comprising the simulation
months    <- c(1,12)          # Months comprising the simulation
time_step <- 6                # Time steps in ROMS
zplot     <- -50              # Z vertical level to calculate
lat       <- as.matrix(read.table(paste0(dirpath, 'lat.txt')))

zplot <- which(ver_lev == zplot)
ver_lev <- ver_lev[1:zplot]

# Create the polygon mask in 4D from the ROMS mask
mask4D <- matrix(data = NA, nrow = dim(mask)[1], ncol = dim(mask)[2])

for(i in 1:dim(xy)[1]) mask4D[ xy[i,1] , xy[i,2] ] <- 1

mask4D <- rep(as.vector(mask4D), time = length(ver_lev) * time_step)
mask4D <- array(data = mask4D, dim = c(dim(mask)[1], dim(mask)[2], length(ver_lev), time_step))

hovmuller <- NULL
for(year in years[1]:years[2]){
  for(month in months[1]:months[2]){
    
    matfile <- paste0(dirpath, namevar, 'Y', year, 'M', month,'.mat')
    print(matfile)
    vari <- readMat(matfile)$newvar
    
    if (month == 12){
      vari <- vari[1:time_step,1:zplot,,]
    }
    vari <- vari[,1:zplot,,]
    
    # Convert to the classical dimension as R reads the ncdf [lon, lat, depth, time].
    vari2 <- array(data = NA, dim = rev(dim(vari)))
    for(i in 1:dim(vari)[1]){
      for(j in 1:dim(vari)[2]){
        subvari <- t(vari[i,j,,])
        vari2[,,j,i] <- subvari
      }
    }
    
    vari <- vari2; rm(vari2)
    
    # If you want to calculate the functional response
    if(!is.null(k_x)) vari <- vari / (vari + k_x)
    
    # Multiplicar por la mascara en 4D con los indices de la zona
    vari <- vari * mask4D
    vari <- apply(vari, c(1,2,4), mean, na.rm = T)
    # dim(vari)
    # image.plot(vari[,,10])
    
    var_mat <- NULL
    for(i in 1:dim(xy)[1]){
      # print(i)
      var_lat <- vari[xy[i,1], xy[i,2],]
      var_mat <- rbind(var_mat, var_lat)
    }
    
    hovmuller <- cbind(hovmuller, var_mat)
  }
}
csv_name <- paste0(dirpath, namevar, '_hovmullerReleaseZone.csv')
write.table(x = hovmuller, file = csv_name, col.names = F, row.names = F, sep = ';')

