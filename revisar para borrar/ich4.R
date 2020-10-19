dat <- read.table('E:/ICHTHYOP/10kmparent/DEB/out/E_ringens/results_60days/ichthyop_output.csv', sep = ';', header = T)

depth <- tapply(X = dat$Recruitprop, INDEX = list(dat$Day, dat$Depth), FUN = mean)

png('C:/Users/jflores/Desktop/releaseDepthPeru.png', height = 850, width = 850, res = 120)
leg_names <- paste(levels(dat$Depth), 'm')
cols <- c('grey90', 'grey60', 'grey20')
barplot(t(depth), beside = T, col = cols, yaxt = 'n')
axis(2, las = 2)
legend('topright', legend = leg_names, bty = 'n', fill = cols, title = 'Release Depth')
dev.off()
