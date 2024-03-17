source('ichthyop_libraries.R')
source('ichthyop_functions.R')

new_path <- 'E:/ICHTHYOP/10kmparent/DEB_TC5_TCseuil0.052abj/cfg/'

dirs <- c(
  'E:/ICHTHYOP/rsodi1/DEB_TC5_TCseuil0.052abj/out_case1/',
  'E:/ICHTHYOP/rsodi1/DEB_TC5_TCseuil0.052abj/out_case2/'
)


for(i in 1:length(dirs)){
  print(dirs[i])
  dirpath <- dirs[i]
  source('main_get_data_atRecruitmentAgeDirecto.R')
}
