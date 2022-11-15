source('ichthyop_libraries.R')
source('ichthyop_functions.R')

new_path <- 'E:/ICHTHYOP/rsodi1/DEB_TC5_TCseuil0.052abj/cfg/'

dirs <- c(
  'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5_TCseuil0.052/case1/'
  # 'E:/ICHTHYOP/rsodi1/DEB_TC5_TCseuil0.052abj/case1kx0.4/',
  # 'E:/ICHTHYOP/rsodi1/DEB_TC5_TCseuil0.052abj/case1kx0.6/',
  # 'E:/ICHTHYOP/rsodi1/DEB_TC5_TCseuil0.052abj/case2kx0.2/',
  # 'E:/ICHTHYOP/rsodi1/DEB_TC5_TCseuil0.052abj/case2kx0.4/',
  # 'E:/ICHTHYOP/rsodi1/DEB_TC5_TCseuil0.052abj/case2kx0.6/'
)


for(i in 1:length(dirs)){
  print(dirs[i])
  dirpath <- dirs[i]
  source('special_routines/plot_hovmullerRecruitmentLatitude.R')
}
