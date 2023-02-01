################################################################################
#% change and absolute change from the first year of lidar data to the subsequent
#years

#Elizabeth LaRue 
#elarue@purdue.edu
#Updated May 5, 2021
#Updated Apr 13, 2022 by Dennis Choi

#this works with the R Version 3.6.3
################################################################################
# setwd("D:\\Project\\pudue\\neon\\DisturbanceStructuralDiversityEDI")
# 
# dat <- read.csv("disturbance_structural_diversity_data_2021_1228.csv")
# dat <- na.omit(dat)
# 
# #remove QUAN become there is only 1 year of data
# dat <- dat[!dat$site == "GUAN", ]
# 
# #add a year column for building a variable (years between time periods)
# dat <- cbind(dat, dat$year)

############################################################################
############################################################################
#           Weed out plots that do not have a value each year of data
############################################################################
############################################################################
setwd("D:\\Project\\pudue\\neon\\DisturbanceStructuralDiversityEDI")
dat <- read.csv("disturbance_structural_diversity_data_2022_0224.csv")
# dat <- na.omit(dat)
# dat <- BART_BBD_indices

sites <- unique(dat$site)
# 
# dat <- na.omit(dat)

OUT.KEEP <- NULL
for(i in 1:length(sites)){
  dat.i <- dat[dat$site == sites[i], ]
  no.plots <- length(unique(dat.i$year)) #how many years of data available
  plots <- unique(dat.i$plotID)
  
  for(f in 1:length(plots)){
    dat.f <- dat.i[dat.i$plotID == plots[f], ]  
    
    if(nrow(dat.f) == no.plots){
      OUT.KEEP <- rbind(OUT.KEEP, dat.f)
    } else {
      next
    } #end of if else statement
  } #end of inner loop
} #end of outer loop


OUT.KEEP <- OUT.KEEP[,-1]
write.csv(OUT.KEEP, "FSD_noNAplots_02242022.csv")



############################################################################
############################################################################
#        create change FSD dataset   
############################################################################
############################################################################

# dat <- read.csv("FSD_noNAplots_01262022.csv")
# dat <- na.omit(dat)

#remove QUAN become there is only 1 year of data
dat <- dat[!dat$site == "GUAN", ]

# dat <- BART_BBD_indices
#add a year column for building a variable (years between time periods)
dat <- cbind(dat, dat$year)
plots <- unique(dat$plotID)

absolute_change <- NULL
percent_change <- NULL

colnames(dat)
dat <- dat[,-1]
colnames(dat)[1] <- "siteID"
colnames(dat)

for(i in 1:length(plots)){
  dat.i <- dat[dat$plotID == plots[i], ]  
  start_year <- min(dat.i$year)
  
  #first year of data
  year1 <- dat.i[dat.i$year == start_year, ]
  
  #second year of data, 3rd ,etc.
  future <- dat.i[!dat.i$year == start_year, ]
  
  #create a dataframe with number of repeated rows to match future year dataframe
  year1rep <- year1[rep(1, nrow(future)),]
 
  #metrics are columns 10 to 29 
  abs_change_i <- future[,6:29] - year1rep[,6:29]
  abs_change_i <- cbind(future[,1:5], abs_change_i)
  absolute_change <- rbind(absolute_change, abs_change_i)
  
  #% change will be x future year - first year of data / first year of data
  # positive will be an increase and negative will be a decrease relative to original %
  percent_change_i <- (abs_change_i[,6:29] / year1rep[,6:29]) * 100
  percent_change_i <- cbind(future[,1:5], percent_change_i, abs_change_i[29])
  percent_change <- rbind(percent_change, percent_change_i)
  
} #end of loop

colnames(absolute_change)[29] <- "no_years"
# absolute_change <- absolute_change[,-1]
write.csv(absolute_change, "./absolute_change_0413_2022.csv")

colnames(percent_change)[30] <- "no_years"
colnames(percent_change)
percent_change <- percent_change[,-29]
write.csv(percent_change, "./percent_change_0413_2022.csv")

#this loop was checked manually in excel to make sure that it calculated absolute and % change correctly

