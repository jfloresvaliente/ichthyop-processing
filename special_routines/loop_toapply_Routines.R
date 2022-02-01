source('ichthyop_libraries.R')
source('ichthyop_functions.R')
lat_div       <- 2        # Latitudinal resolution
dirs <- c(
   'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5/out_simu1/results/'
  ,'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5/out_simu2/results/'
  ,'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5/out_simu3/results/'
  ,'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5/out_simu4/results/'
  ,'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5/out_simu5/results/'
  ,'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5/out_simu6/results/'
  ,'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC1/out19C/results/'
  ,'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC1/out23C/results/'
  ,'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC1/out25C/results/',
          
  'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5/out_simu1/results_no_shelf/'
  ,'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5/out_simu2/results_no_shelf/'
  ,'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5/out_simu3/results_no_shelf/'
  ,'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5/out_simu4/results_no_shelf/'
  ,'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5/out_simu5/results_no_shelf/'
  ,'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5/out_simu6/results_no_shelf/'
  ,'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC1/out19C/results_no_shelf/'
  ,'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC1/out23C/results_no_shelf/'
  ,'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC1/out25C/results_no_shelf/'
          )

times <- list(length(dirs))
for(m in 1:length(dirs)){
  a <- Sys.time()
  print(dirs[m])
  dirpath <- dirs[m]
  source('special_routines/plot_hovmullerAgeLatitude.R')
  b <- Sys.time()
  ba <- b-a
  # print(ba)
  
  # times[m] <- ba
}

# unlist(times)
# print(sum(unlist(times)))
