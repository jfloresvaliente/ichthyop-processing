library(fields)
library(maps)
library(mapdata)
library(ncdf4)
source(file = 'source/compute_recruitment_ichthyop.R')
source(file = 'source/recruitment_area.R')
source(file = 'source/recruitment_zone.R')

dirpath <- 'E:/ICHTHYOP/PeruChileSACW/out/2grados/'
cfg <- 'E:/ICHTHYOP/PeruChileSACW/cfg/'
dates <- read.table(paste0(cfg, 'date_scrum_time_ichthyop.csv'), header = T, sep = ';')
nzones <- 15

# count <- nzones
# for(i in 1:nzones){
#   zona <- compute_recruitment_ichthyop(dirpath = dirpath,
#                                        firstdrifter = 1,
#                                        lastdrifter = 20000,
#                                        computeattime = 31,
#                                        nbreleasezones = nzones,
#                                        recruitmentzone = i,
#                                        ymax = 50,
#                                        dates = dates,
#                                        old_path = cfg,
#                                        new_path = cfg)
  # write.table(x = zona, file = paste0(dirpath, 'zona_reclutamiento',count,'.csv'), row.names = F, sep = ';')
  # write.table(x = zona, file = paste0(dirpath, 'zona_reclutamiento',i,'.csv'), row.names = F, sep = ';')
  # count <- count - 1
# }

mat <- NULL
for(i in 1:nzones){
  rec <- read.table(file = paste0(dirpath, 'zona_reclutamiento',i, '.csv'), header = T, sep = ';')
  rec <- recruitment_area(dataset = rec)[,1]
  mat <- cbind(mat, rec)
}
mat[mat == 0] = NA

a <- seq(5,33,2)
b <- seq(7,35,2)

# a <- seq(5,30,5)
# b <- seq(10,35,5)

lats <- NULL
for (i in 1:length(a)) {
  k <- paste0(a[i],'-',b[i])
  lats <- c(lats, k)
}

x <- matrix(data = 1:nzones, nrow = nzones, ncol = nzones, byrow = T)
y <- matrix(data = -1:-nzones, nrow = nzones, ncol = nzones, byrow = F)

x11()
par(mar = c(1,6,4,1))
image.plot(x,y,mat, axes = F, xlab = '', ylab = '', zlim = c(0,23))
axis(side = 2, at = -1:-nzones, labels = lats, las = 2)
axis(side = 3, at = 1:nzones, labels = lats, las = 1)
mtext(text = 'Recruitment Zones', side = 3, line = 3)
mtext(text = 'Release Zones', side = 2, line = 4)
box()
