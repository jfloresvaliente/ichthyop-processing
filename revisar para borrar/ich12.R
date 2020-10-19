source('source/ichthyop_libraries.R')
dirpath <- 'E:/ICHTHYOP/10kmparent/Fisica-DEB/out/MESO60dias/results_DEB/'
timer   <- 61

# vari    <- 'temp'
# ylab    <- 'Temperature [ºC]'
# ylim    <- c(15,28)

# vari    <- 'MESO'
# ylab    <- 'Mesozooplankton [umol C L-1]'
# ylim    <- c(0,5)

vari    <- 'O2'
ylab    <- 'Oxygen [umol L-1]'
ylim    <- c(50,300)

# vari    <- 'length'
# ylab    <- 'Size [mm]'
# ylim    <- c(0,85)

error_bar <- function(x, a = 0.05){
  n  <- length(x)
  m  <- mean(x, na.rm = T)
  tt <- -qt(p = a/2, df = n-1)
  ee <- sd(x)/sqrt(n)  # standard error
  e  <- tt*ee          # error range
  d  <- e/m            # relative error, says that the confidence interval is a percentage of the value
  li <- m-e            # lower limit
  ls <- m+e            # upper limit
  # stat <- c(m, li, ls)
  stat <- c(m, m-sd(x), m+sd(x))
}

png(paste0(dirpath, vari,'_experimentado.png'), width = 1350,height = 950, res = 120)
par(mfrow = c(3,4), mar = c(4, 4, .5, .5))
# par(mfrow = c(3, 4),        # 2x2 layout
#     oma = c(2, 2, 1, 0.5),  # two rows of text at the outer left and bottom margin
#     mar = c(1, 1, .5, 3.7), # space for one row of text at ticks and to separate plots
#     mgp = c(2, .5, 0),      # axis label at 2 rows distance, tick labels at 1 row
#     xpd = F,                # allow content to protrude into outer margin (and beyond)
#     font = 2)

for(month in 1:12){
  rfile <- paste0(dirpath, 'trajectoriesM', month, '.Rdata')
  load(rfile); print(rfile)
  dat <- trajectories; rm(trajectories)
  dat$Timer <- dat$Timer - 1
  
  name_col <- names(dat)
  name_col <- which(name_col == vari)
  
  depth <- matrix(data = dat[,name_col], byrow = T, ncol = timer)
  depth <- apply(depth, c(2), error_bar)
  j <- month
  
  plot(0:(timer-1), 0:(timer-1), ylim = ylim, type = 'n', axes = F, xlab = '', ylab = '')
  axis(side = 1)
  axis(side = 2, las = 2)
  
  if(j == 1 | j == 5 | j == 9)
    mtext(side = 2, line = 2.5, cex = .75, font = 2, text = ylab)
  if(j == 9 | j == 10 | j== 11 | j == 12)
    mtext(side = 1, line = 2  , cex = .75, font = 2, text = 'Age in Days')
  
  color <- 'blue'
  x <- c(0:(timer-1), (timer-1):0)
  y <- c(depth[2,], rev(depth[3,]))
  lines(0:(timer-1), depth[1,], lty = 1, lwd = 2, col = color)
  polygon(x = x, y = y, col = adjustcolor(color, alpha.f = 0.1), border = NA)
}
dev.off()


# dat <- subset(dat, dat$Drifter %in% 1:100)

# # Factors for Release Depth
# depth_lev <- levels(factor(dat$ReleaseDepth))
# j <- month
# plot(0:(timer-1), 0:(timer-1), ylim = c(15,25), type = 'n', axes = F, xlab = '', ylab = '')
# # legend('bottomleft', legend = paste('Month',j), adj = .2, text.font = 2)
# if(j == 1 | j == 5 | j == 9) axis(2, las = 2, lwd = 2, font = 2)
# if(j == 9 | j == 10 | j== 11 | j == 12) axis(1, lwd = 2, font = 2)
# 
# for(i in 1:length(depth_lev)){
#   depth <- subset(dat, dat$ReleaseDepth == depth_lev[i])
#   depth <- matrix(data = depth$temp, byrow = T, ncol = timer)
#   depth <- apply(depth, c(2), error_bar)
#   
#   if(i == 1) color <- 'blue'
#   if(i == 2) color <- 'red'
#   if(i == 3) color <- 'green'
#   
#   x <- c(0:(timer-1), (timer-1):0)
#   y <- c(depth[2,], rev(depth[3,]))
#   lines(0:(timer-1), depth[1,], lty = 1, lwd = 2, col = color)
#   # lines(0:(timer-1), depth[2,], lty = 3, col = color)
#   # lines(0:(timer-1), depth[3,], lty = 3, col = color)
#   
#   polygon(x = x, y = y, col = adjustcolor(color, alpha.f = 0.1), border = NA)
# }