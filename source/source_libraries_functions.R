# Libraries
library(ncdf4)
library(XML)
library(stringr)
library(fields)
library(maps)
library(mapdata)

# Functions
source('source/recruitment_age.R')
source('source/recruitment_area.R')
source('source/recruitment_bathy.R')
source('source/recruitment_behavior.R')
source('source/recruitment_day.R')
source('source/recruitment_depth.R')
source('source/recruitment_eps.R')
source('source/recruitment_temp.R')
source('source/recruitment_year.R')
source('source/recruitment_zone.R')

source('source/compute_recruitment_ichthyop.R')
source('source/compute_recruitment_ichthyop_DEB.R')
source('source/compute_recruitment_ichthyop_drifters.R')
source('source/compute_recruitment_ichthyop_drifters_DEB.R')

source('source/get_trajectories.R')
source('source/get_trajectories_DEB.R')
source('source/get_trajectories_drifters.R')
source('source/get_trajectories_drifters_DEB.R')