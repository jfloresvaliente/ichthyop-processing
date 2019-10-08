#===============================================================================
# Name   : main_plots_ichthyop_byfactor
# Author : Jorge Flores
# Date   : 
# Version:
# Aim    : 
# URL    : 
#===============================================================================
source('source/recruitment_year.R')
source('source/recruitment_day.R')
source('source/recruitment_depth.R')
source('source/recruitment_age.R')
source('source/recruitment_area.R')
# source('source/plot_layout_3.R')
# source('source/plot_layout_4.R')

dirpath  <- 'E:/F/ichthyop_output_analysis/RUN2/csv_files/recruited/'
out_path <- 'E:/'

winds <- 'daily'
simu  <- 'lobos'
simulacion <- paste0(winds,'_',simu)
file <- paste0(dirpath, simulacion, '.csv')

fig_name  <- paste0(out_path, simulacion, '_L3')
# fig_name2 <- paste0(out_path, simulacion, '_L4')


igs <- read.table('E:/F/SCALLOP_ARTICLE/csv_files/igs.csv', sep = ',', header = T)
igs <- igs$igs_prom
x <- 1:12

# Don't change anything after here
if(simu == 'lobos' | simu == 'sechura'){
  ymax = 9.8    # 9 para retencion  ; # 0.2 para transporte
  ylab = 1    # 0 = larval transport ; 1 = larval retention
}else{
  ymax = 0.2  # 9 para retencion  ; # 0.2 para transporte
  ylab = 0    # 0 = larval transport ; 1 = larval retention
}


legend <- 0  # legend == 1 turn on legend, if 0, legned turn off
legend.cex <- 1 # size of the legend inside the plot

dataset <- read.csv(file , sep='')
dataset <- subset(dataset, dataset$Year %in% c(2009:2011))

datayear  <- recruitment_year (dataset)
dataday   <- recruitment_day  (dataset)
datadepth <- recruitment_depth(dataset)
dataage   <- recruitment_age  (dataset)
dataarea  <- recruitment_area (dataset)

##  plot ##
if(ylab == 1){
  ylab = 'Local Retention (%)'
}else{
  ylab = 'Transport Success (%)'
}

months = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

png(paste0(fig_name,'.png'), width = 1050, height = 350, res=120)
# x11()
mat <- matrix(c(1,2,3), 1, 3, byrow = TRUE)
nf <- layout(mat, widths = c(8,8,8), height = c(8), TRUE)

### PLOT AS FUNCTION OF MONTH OF SPAWNING
par(mar=c(4 , 5.5 , 1.5 , 0.15))
dayplot   <- barplot(dataday[,1], xlab='', ylab= '' ,ylim = c(0,ymax), axes = FALSE, axisnames = FALSE)
arrows(dayplot,dataday[,2],dayplot,dataday[,3],angle=90,code=3,length=0.05)
axis(1, at=dayplot, labels = FALSE, tick = FALSE)
mtext(ylab, side=2, line=3.5 , cex=1.2)
axis(2, lwd = 3, cex.axis = 1.4, las = 2)
text(dayplot, par('usr')[3], labels = months, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.9)
mtext('Spawning Month', side=1, line=2.5 , cex=0.9)

if(legend == 1) legend('topleft', 'A)', bty = 'n', cex = legend.cex)

# par(new=T)
# plot(dayplot, igs, axes=F, ylim = c(12,20), xlab="", ylab="", 
#      type="l",lty=2, main="",lwd=1.5 , col="red")
# axis(side=4, ylim=c(10,20),lwd=1.5,line=0, font = 2, col='red',col.axis = 'red', las = 2)
# # mtext(text = 'Gonadosomatic Index', side = 4, col = 'red', font = 2, line = 3., cex = 1.2)
# legend('top', legend = 'GI', bty = 'n', lty = 2, lwd = c(1.5), col = 'red', text.col = 'red')


### PLOT AS FUNCTION OF SPAWNING DEPTH
par(mar=c(4 , 1.5 , 1.5 , 0.15))
depthplot <- barplot(datadepth[,1], xlab='', ylab='', ylim = c(0,ymax), axes = FALSE, cex.names=.8)
arrows(depthplot,datadepth[,2],depthplot,datadepth[,3],angle=90,code=3,length=0.05)
# axis(2, lwd = 3, cex.axis = 1.4, las = 2, labels = FALSE)
mtext('Spawning depth (m)', side=1, line=2.5, cex=0.9)
# mtext(ylab, side=2, line=2.5 , cex=1.2)

if(legend == 1) legend('topleft', 'B)', bty = 'n', cex = legend.cex)

### PLOT AS FUNCTION OF AGE MINIMUN TO SETTLEMENT
par(mar=c(4 , 1.5 , 1.5 , 0.15))
ageplot   <- barplot(dataage[,1], xlab='',ylab= '', ylim = c(0,ymax), axes = FALSE, cex.names=.8)
arrows(ageplot,dataage[,2],ageplot,dataage[,3],angle=90,code=3,length=0.05)
# axis(2, lwd = 3, cex.axis = 1.4, las = 2, labels = FALSE)
mtext('Minimum age to settlement (d)', side=1, line=2.5, cex=0.9)
# mtext(ylab, side=2, line=2.5 , cex=1.2)

if(legend == 1) legend('topleft', 'C)', bty = 'n', cex = legend.cex)

dev.off()

#===============================================================================
# END OF PROGRAM
#===============================================================================