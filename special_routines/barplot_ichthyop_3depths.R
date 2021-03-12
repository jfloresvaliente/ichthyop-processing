#=============================================================================#
# Name   : barplot_ichthyop_3depths
# Author : 
# Date   : 
# Version:
# Aim    : 
# URL    : 
#=============================================================================#
source('ichthyop_libraries.R')
source('ichthyop_functions.R')

dirpath <- 'C:/Users/jflores/Desktop/'

dat1 <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/FISICA/out/results_30days/'
dat2 <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEBf1/k_x0_90days/out/results/'
dat3 <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB/k_x1.6_90days/out/results/'

# ylab <- 'Retention (%)'
# ylab <- 'Recruitment (%)'
ylab <- 'Pre-recruitment (%)'

lats     <- seq(from = 2, to = 20, by = 2)
ymax     <- 100
col_bars <- c('grey30','grey60','grey90')

# legend   <- c( '10 km', '02 km', '02 km new')
titulos   <- c( 'Age criteria', 'Size criteria k_x = 0', 'Size criteria k_x = 1.6')

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
paths <- c(dat1, dat2, dat3)
png(paste0(dirpath, 'barplot_ichthyop_3depths.png'), height = 450, width = 1250, res = 120)
par(mfrow = c(1,3))
par(mar=c(4 , 4 , 1.5 , 0.1))
for(i in 1:length(paths)){
  # print(paths[i])
  dat <- read.table(paste0(paths[i], 'ichthyop_output.csv'), header = T, sep = ';')
  legend <- levels(factor(dat$Depth))
  
  day      <- NULL
  day_up   <- NULL
  day_down <- NULL
  for(j in 1:length(legend)){
    # print(legend[j])
    dat_sub <- subset(dat, dat$Depth == legend[j])
    dat_sub <- recruitment_month(dat_sub)
    day <- rbind(day, dat_sub[,1])
    day_up <- rbind(day_up, dat_sub[,3])
    day_down <- rbind(day_down, dat_sub[,2])
  }
  
  dayplot   <- barplot(day, beside = T, xlab="", ylab= "" ,ylim = c(0,ymax),
                       axes = T, axisnames = T, col = col_bars, yaxt='n')
  axis(2, las = 2)
  arrows(dayplot[1,], day_up[1,],
         dayplot[1,], day_down[1,],
         angle=90,code=3,length=0.015)
  arrows(dayplot[2,], day_up[2,],
         dayplot[2,], day_down[2,],
         angle=90,code=3,length=0.015)
  arrows(dayplot[3,], day_up[3,],
         dayplot[3,], day_down[3,],
         angle=90,code=3,length=0.015)
  legend('topright', legend = legend, bty = 'n', fill = col_bars)
  mtext(side = 1, line = 2.5, cex = 0.75, font = 2, text = 'Spawning Month')
  mtext(side = 2, line = 2.5, cex = 0.75, font = 2, text = ylab)
  mtext(side = 3, line = -2., cex = 0.75, font = 2, text = titulos[i], adj = .1)
}
dev.off()
#=============================================================================#
# END OF PROGRAM
#=============================================================================#