# dat <- read.csv('C:/Users/jflores/Desktop/Crecimiento_E.ringens.csv', header = T)
# 
# autors <- levels(dat$Autor)
# autors_list <- list()
# 
# for(i in 1:length(autors)){
#   sub_dat <- subset(dat, dat$Autor == autors[i])
#   plot(sub_dat$t, sub_dat$L, xlab = 'Time [d]', ylab = 'Length [mm]', main = autors[i])
#   autors_list[[i]] <- sub_dat
# }
# 
# 
# 
# set.seed(1452225)
# 
# # sigmoid maps the real line to the unit interval
# sigmoid = function(x) {
#   1/(1 + exp(-x))
# }
# 
# # data with two different regimes of behavior
# d <- rbind(
#   data.frame(
#     score = sigmoid(0.4*(rnorm(1000) - 2.5) ),
#     y = sample(c(TRUE, FALSE), prob = c(0.02, 0.98), 
#                size = 1000, replace = TRUE)),
#   data.frame(
#     score = sigmoid( 0.4*(rnorm(200) + 2.5) ), 
#     y = sample(c(TRUE, FALSE), size = 200, replace = TRUE))
# )
# 
# library(ggplot2)
# library(WVPlots)
# 
# ROCPlot(d, "score", "y", truthTarget=TRUE, title="Model ROC") 
