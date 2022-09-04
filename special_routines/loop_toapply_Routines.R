source('ichthyop_libraries.R')
source('ichthyop_functions.R')

new_path <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5_TCseuil0.052abj/cfg/'

dirs <- c(

  'D:/ICHTHYOP/10kmparent/DEB_TC5_TCseuil0.052abj/case2f0.5/',
  'D:/ICHTHYOP/10kmparent/DEB_TC5_TCseuil0.052abj/case2f0.75/'

        )

times <- list(length(dirs))
for(m in 1:length(dirs)){
  a <- Sys.time()
  print(dirs[m])
  dirpath <- dirs[m]
  source('main_get_trajectories.R')
  b <- Sys.time()
  ba <- b-a
  print(ba)
  
  times[m] <- ba
}

unlist(times)
print(sum(unlist(times)))
