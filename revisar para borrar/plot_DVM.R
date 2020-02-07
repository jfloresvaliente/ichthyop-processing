source('source/recruitment_day.R')

dat <- read.csv('D:/ICHTHYOP/peru10km/Brochier2008/LatitudeBathyDVM/out/target1-30/results/ichthyop_output.csv', sep = ';')
depth1_30 <- recruitment_day(dataset = dat)

depths <- cbind(depth1_30[,1], depth1[,1], depth15[,1], depth30[,1])
lim1   <- cbind(depth1_30[,2], depth1[,2], depth15[,2], depth30[,2])
lim2   <- cbind(depth1_30[,3], depth1[,3], depth15[,3], depth30[,3])


x11()
cols <- c('red', 'grey20', 'grey50', 'grey80')
depthsplot <- barplot(t(depths), beside = T, ylim = c(0, 100), col = cols)
for(i in 1:4) arrows(depthsplot[i,], lim1[,i], depthsplot[i,], lim2[,i], angle=90, code=3, length=0.05)
legend('topleft', legend = c('DVM', '1m', '15m', '30m'), bty = 'n', fill = cols, adj = 0.3)
