source('ichthyop_libraries.R')
source('ichthyop_functions.R')

new_path <- 'E:/ICHTHYOP/rsodi1/DEB_TC5_TCseuil0.052/cfg/'

dirs <- c(

  # 'E:/ICHTHYOP/rsodi1/DEB_TC5_TCseuil0.052/case1/',
  # 'E:/ICHTHYOP/rsodi1/DEB_TC5_TCseuil0.052/case1f1/',
  # 'E:/ICHTHYOP/rsodi1/DEB_TC5_TCseuil0.052/case2/',
  'E:/ICHTHYOP/rsodi1/DEB_TC5_TCseuil0.052/case2f1/'

)

times <- list(length(dirs))
for(m in 1:length(dirs)){
  a <- Sys.time()
  print(dirs[m])
  dirpath <- dirs[m]
  source('main_get_data_atRecruitmentAgeDirecto.R')
  b <- Sys.time()
  ba <- b-a
  print(ba)
  
  times[m] <- ba
}

unlist(times)
print(sum(unlist(times)))
