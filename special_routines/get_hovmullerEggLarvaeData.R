#=============================================================================#
# Name   : get_hovmullerEggLarvaeData
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Para los datos de P. Ayon
# URL    : 
#=============================================================================#
dirpath       <- 'C:/Users/jflores/Documents/JORGE/TESIS/TESIS_PHD/'
latilim       <- c(-20, -2) # Latitude extension of the spawning zone
lat_div       <- 1        # Latitudinal resolution

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
dir.create(path = paste0(dirpath, '/hovmuller/'), showWarnings = F)
hovmullerRdata <- paste0(dirpath, '/hovmuller/hovmullerEgg', lat_div, 'degrees.Rdata')

lat_ini <- seq(latilim[1], latilim[2], lat_div)
lat_out <- lat_ini + lat_div

df <- read.table(paste0(dirpath, 'Climatology_Anc_egg.csv'), header = T, sep = ';')
df$lon <- df$lon - 360

hov_recruit <- NULL
for(i in 1:12){

    sub_df <- subset(df, df$mes == i)
    

    lat_recruit <- NULL
    for(k in 1:(length(lat_ini)-1)){
      
      lat_sub <- subset(sub_df, sub_df$lat >= lat_ini[k] & sub_df$lat < lat_out[k])
      lat_sub <- mean(lat_sub$Anc_egg)

      lat_recruit <- c(lat_recruit, lat_sub)
    }

    hov_recruit <- cbind(hov_recruit, lat_recruit)

}
hovmuller <- hov_recruit

# Rotate matrix and named rows & cols
z <- t(hovmuller)
x <- seq(from = 1, to = 12, length.out = 12)
y <- seq(from = latilim[1], to = latilim[2], length.out = dim(z)[2])
rownames(z) <- x
colnames(z) <- y

hovmuller <- list(x = x, y = y, z = z)
save(hovmuller, file = hovmullerRdata)

#=============================================================================#
# END OF PROGRAM
#=============================================================================#

par(mfrow = c(1,3))
m <- tapply(X = df$Anc_egg, INDEX = list(df$mes), FUN = mean)
barplot(m, main = 'Egg mean [#]')

m <- tapply(X = df$Anc_egg, INDEX = list(df$mes), FUN = sum)
barplot(m, main = 'Egg sum [#]')

n <- tapply(X = df$Anc_egg, INDEX = list(df$mes), FUN = length)
barplot(n, main = 'number of samples [#]')

