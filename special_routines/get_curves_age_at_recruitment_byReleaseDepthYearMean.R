#=============================================================================#
# Name   : get_curves_age_at_recruitment_byReleaseDepthYearMean
# Author : Jorge Flores-Valiente
# Date   : 
# Version:
# Aim    : Gets the percentage of particles at age at recruitment by Release Depth.
# URL    : 
#=============================================================================#
dirpath <- 'C:/Users/jflores/Documents/ICHTHYOP/10kmparent/DEB_TC5/out_simu7/results_no_shelf/'
ages    <- 90 # Age

#=============================================================================#
#===================== Do not change anything from here ======================#
#=============================================================================#
load(file = paste0(dirpath, 'data_atRecruitmentAge.Rdata'))

depth_lev <- levels(factor(df$ReleaseDepth))

age_percent <- NULL
for(i in 1:length(depth_lev)){
    sub_df <- subset(df, df$ReleaseDepth == depth_lev[i])
    released <- dim(sub_df)[1]
    recruite <- subset(sub_df, sub_df$IfRecruited == 1)
    recruited_day <- hist(recruite$Age, 0:(ages), plot = FALSE)$counts
    recruited_day <- recruited_day/released * 100
    age_percent <- rbind(age_percent, recruited_day)
}
rownames(age_percent) <- depth_lev
colnames(age_percent) <- 1:ages
save(age_percent, file = paste0(dirpath, 'recruited_age_percentageReleaseDepthYearMean.Rdata'))
#=============================================================================#
# END OF PROGRAM
#=============================================================================#