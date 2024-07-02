#=============================================================================#
# Name   : compute_recruitment_ichthyop2
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    :
# URL    : 
#=============================================================================#
dirpath <- 'E:/ICHTHYOP/10kmparent/DEB_TC5_TCseuil0.052abj/case1_kx1.6/'

csv <- read.table(paste0(dirpath, '/results/ichthyop_output.csv'), header = T, sep = ';')
Rdata <- paste0(dirpath,'/results/data_atRecruitmentAge.Rdata')
print(Rdata)
load(Rdata)

for(i in 1:9) df$Zone_name[grep(pattern = paste0('zone', i), x = df$Zone_name)] <- paste0('zone', i)

dat <- array(data = NA, dim = c(dim(csv)[1], 11))
for(i in 1:dim(csv)[1]){
  sub_df <- subset(df, df$Year         == csv$Year[i] &
                       df$Month        == csv$Month[i] &
                       df$t_x          == csv$t_x[i] &
                       df$Zone_name    == csv$Zone_name[i] &
                       df$ReleaseDepth == csv$ReleaseDepth[i] &
                       df$ReleaseBathy == csv$ReleaseBathy[i])
  
  NumberReleased <- dim(sub_df)[1]
  NumberRecruited <- sum(sub_df$IfRecruited)
  Recruitprop <- NumberRecruited/NumberReleased*100
  
  recruited  <- subset(sub_df, sub_df$IfRecruited == 1)
  N_length   <- sum(recruited$N_length)/NumberReleased*100
  N_constant <- sum(recruited$N_constant)/NumberReleased*100
  
  dat[i,] <- cbind(NumberReleased,
                   NumberRecruited,
                   csv$Year[i],
                   csv$Month[i],
                   csv$t_x[i],
                   csv$Zone_name[i],
                   csv$ReleaseDepth[i],
                   csv$ReleaseBathy[i],
                   Recruitprop,
                   N_length,
                   N_constant)
}

colnames(dat) <- c(
  'NumberReleased'
  ,'NumberRecruited'
  ,'Year'
  ,'Month'
  ,'t_x'
  ,'Zone_name'
  ,'ReleaseDepth'
  ,'ReleaseBathy'
  ,'Recruitprop'
  ,'N_lengthprop'
  ,'N_constantprop'
)
write.table(x = dat, file = paste0(dirpath, '/results/ichthyop_output2.csv'), sep = ';', row.names = F)
#=============================================================================#
# END OF PROGRAM
#=============================================================================#