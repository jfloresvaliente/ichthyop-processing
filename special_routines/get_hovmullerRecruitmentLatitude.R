#=============================================================================#
# Name   : get_hovmullerRecruitmentLatitude
# Author : 
# Date   : 
# Version:
# Aim    : Get Hovmuller of anual mean Recruitment.
# URL    : 
#=============================================================================#
dirpath       <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/FISICA/out/results_30days/'
latilim       <- c(-20, -2) # Latitude extension of the spawning zone
lat_div       <- 2        # Latitudinal resolution
t_x           <- c(1,4,7) # 10kmparent
# t_x           <- c(1,3,5) # rsodi
# t_x           <- c(1,6,11) # peru02km

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
dir.create(path = paste0(dirpath, '/hovmuller/'), showWarnings = F)
hovmullerRdata <- paste0(dirpath, '/hovmuller/hovmullerRecruitmentLatitude', lat_div, 'degrees.Rdata')

lat_ini <- seq(latilim[1], latilim[2], lat_div)
lat_out <- lat_ini + lat_div

load(file = paste0(dirpath, 'data_atRecruitmentAge.Rdata'))

hov_release <- NULL
hov_recruit <- NULL
for(i in 1:12){print(paste('Month : ', i))
  for(j in t_x){
    sub_df <- subset(df, df$Month == i & df$t_x == j)
    
    lat_release <- NULL
    lat_recruit <- NULL
    for(k in 1:(length(lat_ini)-1)){
      
      lat_sub  <- subset(sub_df, sub_df$Lat_ini >= lat_ini[k] & sub_df$Lat_ini < lat_out[k])
      released <- dim(lat_sub)[1]
      recruite <- dim(subset(lat_sub, lat_sub$IfRecruited == 1))[1]
      
      if(is.na(released) | is.na(recruite)) recruite <- 0
      
      lat_release <- c(lat_release, released)
      lat_recruit <- c(lat_recruit, recruite)
    }
    hov_release <- cbind(hov_release, lat_release)
    hov_recruit <- cbind(hov_recruit, lat_recruit)
  }
}
hovmuller <- hov_recruit / hov_release * 100

# Rotate matrix and named rows & cols
z <- t(hovmuller)
x <- seq(from = 1, to = 12.999, length.out = 12*length(t_x))
y <- seq(from = latilim[1], to = latilim[2], length.out = dim(z)[2])
rownames(z) <- x
colnames(z) <- y

hovmuller <- list(x = x, y = y, z = z)
save(hovmuller, file = hovmullerRdata)
#=============================================================================#
# END OF PROGRAM
#=============================================================================#