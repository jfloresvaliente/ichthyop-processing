source('ichthyop_libraries.R')
source('ichthyop_functions.R')

new_path <- 'D:/ICHTHYOP/sen02km/cfg/'

dirs <- c(
  'C:/Users/jflores/Documents/ICHTHYOP/rsodi1/DEB_TC5_TCseuil0.052/case1/results/',
  'C:/Users/jflores/Documents/ICHTHYOP/rsodi1/DEB_TC5_TCseuil0.052/case2/results/',
  'C:/Users/jflores/Documents/ICHTHYOP/rsodi1/DEB_TC5_TCseuil0.052/case1f1/results/',
  'C:/Users/jflores/Documents/ICHTHYOP/rsodi1/DEB_TC5_TCseuil0.052/case2f1/results/'

)

times <- list(length(dirs))
for(m in 1:length(dirs)){
  a <- Sys.time()
  print(dirs[m])
  dirpath <- dirs[m]
  source('special_routines/barplot_ichthyop_output.R')
  b <- Sys.time()
  ba <- b-a
  print(ba)
  
  times[m] <- ba
}

unlist(times)
print(sum(unlist(times)))
