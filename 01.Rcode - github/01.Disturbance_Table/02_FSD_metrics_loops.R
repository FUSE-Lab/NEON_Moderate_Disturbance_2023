################################################################################
#Workflow to obtain FSD metrics from plot .las from step 01 of workflow
#Updated Apr. 7, 2021 by Elizabeth LaRue
#Updated September 1, 2022 by Dennis Choi
################################################################################

setwd("D:\\Project\\pudue\\neon\\DisturbanceStructuralDiversityEDI")

######################## Packages ##############################################

library('lidR')
library('leafR')
library('stringr')

#############dennis added)
library('plyr')
library('dplyr')
library("viridis")

#sessionInfo()
#R version 3.6.3 (2020-02-29)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 19042)

#Matrix products: default

#locale:
#[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
#[4] LC_NUMERIC=C                           LC_TIME=English_United States.1252    

#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
#  [1] stringr_1.4.0 leafR_0.3     lidR_3.1.2    raster_3.3-13 sp_1.4-2     

#loaded via a namespace (and not attached):
#[1] compiler_3.6.3    magrittr_1.5      tools_3.6.3       Rcpp_1.0.5        stringi_1.4.6     codetools_0.2-16  grid_3.6.3       
#[8] data.table_1.13.0 lattice_0.20-38  

#####################################################################################
#remove a outliers for a couple of plots and rewrite then (that didn't get caught from filters)
# las.names <- read.csv("./check_outliers_04132021.csv")
# las.names <- las.names[!is.na(las.names$filter),]
# 
# for(i in 1:nrow(las.names)){
#   #-----------------------------------------------------------------
#   #before filtering ground points for quality check ----------------
#   #reading in individual las plots without filtering < .5 m
#   rewrite_las <- readLAS(file.path(paste("./lidar/all_baseplots_dennis/", las.names[i,1], sep="")), 
#                          filter = paste("-drop_z_above", las.names[i, 4]))
#   plot(rewrite_las)
#   #note that these are writen over and even if this loop is run again the files are fixed. 
#   writeLAS(rewrite_las, paste("./lidar/all_baseplots_dennis/", las.names[i,1], sep=""))
# }


##############################################################################
# #remove outliers, drop Z less than 0, and air points###<- dennis add _01/11/2022
setwd("D:\\Project\\pudue\\neon\\DisturbanceStructuralDiversityEDI")
library('lidR')
library('leafR')
library('stringr')
library('plyr')
library('dplyr')
library("viridis")

las <- list.files("./lidar/all_baseplots_0415_2021/", pattern = "*.las", full.names =  F)

# # for (i in seq_along(las)){
# #   rewrite_las <- readLAS(file.path(paste("./lidar/all_baseplots_0415_2021/", las[i], sep ="")), filter = "-drop_z_below -0.5")
# #   rewrite_las <- classify_noise(rewrite_las, sor(15,7))
# #   # rewrite_las <- filter_poi(rewrite_las, Classification != LASNOISE)
# #   writeLAS(rewrite_las, paste("./lidar/all_baseplots_0415_2021/", las[i], sep=""))
# # }
# 
# 
# las.names <- read.csv("check_outliers_01112022.csv")
# las.names <- las.names[!is.na(las.names$filter),]
# 
# for(i in 1:nrow(las.names)){
#   #-----------------------------------------------------------------
#   #before filtering ground points for quality check ----------------
#   #reading in individual las plots without filtering < .5 m
#   rewrite_las <- readLAS(file.path(paste("./lidar/all_baseplots_0415_2021/", las.names[i,1], sep="")),
#                          filter = paste("-drop_z_above", las.names[i, 4]))
#   plot(rewrite_las)
#   #note that these are writen over and even if this loop is run again the files are fixed.
#   writeLAS(rewrite_las, paste("./lidar/all_baseplots_0415_2021/", las.names[i,1], sep=""))
# }
# 

#############only for visualization_leafR causes error assuming that datatype could be changed after remving outliers.
# #remove outliers, drop Z less than 0, and air points###<- dennis add _01/11/2022
# setwd("D:\\Project\\pudue\\neon\\DisturbanceStructuralDiversityEDI")
# library('lidR')
# library('leafR')
# library('stringr')
# library('plyr')
# library('dplyr')
# library("viridis")
# 
# las <- list.files("./lidar/all_baseplots_dennis/", pattern = "*.las", full.names =  F)
# 
# for (i in seq_along(las)){
#   rewrite_las <- readLAS(file.path(paste("./lidar/all_baseplots/", las[i], sep ="")), filter = "-drop_z_below -0.5")
#   rewrite_las <- classify_noise(rewrite_las, sor(15,7))
#   rewrite_las <- filter_poi(rewrite_las, Classification != LASNOISE)
#   # rewrite_las <- filter_poi(rewrite_las, Classification != LASNOISE)
#   writeLAS(rewrite_las, paste("./lidar/all_baseplots_dennis2/", las[i], sep=""))
# }


###################original#############################
# las.names <- read.csv("./check_outliers_04132021.csv")
# las.names <- las.names[!is.na(las.names$filter),]
# 
# for(i in 1:nrow(las.names)){
#   #-----------------------------------------------------------------
#   #before filtering ground points for quality check ----------------
#   #reading in individual las plots without filtering < .5 m
#   rewrite_las <- readLAS(file.path(paste("./lidar/all_baseplots/", las.names[i,1], sep="")), 
#                          filter = paste("-drop_z_above", las.names[i, 4]))
#   plot(rewrite_las)
#   #note that these are writen over and even if this loop is run again the files are fixed. 
#   writeLAS(rewrite_las, paste("./lidar/all_baseplots/", las.names[i,1], sep=""))
# }



#####################################################################################
#### Structural Metric calculations
# These are setup in two functions and then several from the leafR package in a for loop
##################################################################

######################################################
chm_groundpts_metrics <- function(data.40m) {
  #ground points are not filtered out here so can get actual plot area 
  
  #set any points < 3 m as zero because to get rid of ground points (count as < 0.5 m here, and 
  #very short understory vegetation)
  data.40m@data$Z[data.40m@data$Z <= 3] <- 0
  
  #if then statment to filter out any bad plots (no points or no points > 0)
  if(sum(data.40m@data$Z) > 0) {
    chm <- grid_canopy(data.40m, res = 1, p2r()) # this uses all the points in the chm
    
    #metrics from a chm (grid_canopy)
    rumple <- rumple_index(chm)
    
    #deep gap fraction (fraction of total rasterized area (1 m2 cells) w/points that are 
    #zero height (> 3 m))
    cells <- length(chm@data@values) 
    chm.0 <- chm
    chm.0[is.na(chm.0)] <- 0 
    zeros <- which(chm.0@data@values == 0) 
    deepgaps <- length(zeros) 
    deepgap.fraction <- deepgaps/cells 
    
    out.chm <- data.frame(matrix(c(rumple, deepgap.fraction), ncol = 2))
    
  } else {
    #set rumple to 0, gap fraction to 1 if there were not points > .5 m height or no points
    out.chm <- data.frame(matrix(c(0, 1), ncol = 2))
  }
  
  colnames(out.chm) <- c("rumple", "deepgap.fraction")
  return(out.chm)
}

############################################################
############################################################

##################Dennis corrected the fuction related to cloud_metrics, because of errors ###
structural_diversity_metrics <- function(data.40m) {
  #ground points are filtered (< .5 m) out of the point cloud in the loop first
  #don't use without filtering ground points
  
  #metrics from a chm (grid_canopy)
  chm <- grid_canopy(data.40m, res = 1, p2r()) 
  mean.max.canopy.ht <- mean(chm@data@values, na.rm = TRUE) 
  max.canopy.ht <- max(chm@data@values, na.rm=TRUE) 
  top.rugosity <- sd(chm@data@values, na.rm = TRUE) 
  
  #metrics from cloud_metrics
  vert.sd <- sd(data.40m$Z, na.rm = TRUE)  # <- corrected
  meanH <- mean(data.40m$Z, na.rm = TRUE)   # <- corrected
  vertCV <- vert.sd / meanH 
  vertq <- quantile(data.40m$Z, na.rm = TRUE)  # <- corrected
  
  #metrics from grid_metrics
  sd.9m2 <- grid_metrics(data.40m, sd(Z), 3) #9 m2 grid for sd.sd
  sd.sd <- sd(sd.9m2@data@values, na.rm = TRUE) 
  
  #metrics from a Z vector
  Zs <- data.40m@data$Z
  Zs <- Zs[!is.na(Zs)]
  
  gap_frac <- gap_fraction_profile(Zs, dz = 1, z0 = 3) #ignore points < 3 m
  GFP <- mean(gap_frac$gf) 
  LADen<-LAD(Zs, dz = 1, k=.5, z0= 3) #ignore points < 3 m
  VAI <- sum(LADen$lad, na.rm=TRUE) 
  VCI <- VCI(Zs, by = 1, zmax=100) 
  out.plot <- data.frame(matrix(c(mean.max.canopy.ht, max.canopy.ht,
                                  top.rugosity, vert.sd, sd.sd, GFP, VAI,
                                  VCI,  vertCV, vertq), ncol = 14)) 
  colnames(out.plot) <- c("mean.max.canopy.ht",
                          "max.canopy.ht", 
                          "top.rugosity",
                          "vert.sd","sd.sd", "GFP",
                          "VAI", "VCI", "vertCV", 
                          "q0", "q25", "q50", "q75", "q100")
  return(out.plot)
}

#####################################################################################
#STEP 8 FSD metric table 
#################################################################################
#loop through plots that were previously cut to plot area and corrected for elevation

las.names <- list.files("./lidar/all_baseplots_0415_2021/", pattern = "*.las") #set location where all plot .las were added

OUT <- NULL
# skip_to_next <- F
#i=1
for(i in 1:length(las.names)) tryCatch({
  #reading in individual las plots without filtering to get area and density
  data.40m <- readLAS(file.path(paste0("./lidar/all_baseplots_0415_2021/", las.names[i])))
  data.40m <- filter_poi(data.40m, Classification != LASNOISE)
  print(las.names[i])
  
  #get area so can filter out cropped plots
  plot_area_ground <- area(data.40m)
  
  #total number of points divided by plot area
  den <- length(data.40m@data$Z)/plot_area_ground
  
  #max height to catch if outliers are being removed or something is off with outlier filter
  maxZ <- max(data.40m@data$Z)
  
  #chm area metrics function w/ground points kept 
  chm_metrics_i <- chm_groundpts_metrics(data.40m)
  
  #------------------------------------------------------------------
  #reading in individual las plots again and filtering points < 0.5m
  data.40m <- readLAS(file.path(paste0("./lidar/all_baseplots_0415_2021/", las.names[i])), filter = "-drop_z_below .5")
                      # filter = "-drop_z_below .5")
  data.40m <- filter_poi(data.40m, Classification != LASNOISE)
  
  #remove any duplicated points ("deprecated points") for heterogeneity metrics
  data.40m <- filter_duplicates(data.40m)
  
  #get area of veg points, might be less than actual area with ground points
  veg_plot_area <- area(data.40m)
  
  #Second FSD metrics function
  #if there are no points b/c all veg is < 0.5 m keep loop running
  if(veg_plot_area > 0) { #if veg > 0.5 v
    #run the FSD metric function
    FSD.i <- structural_diversity_metrics(data.40m)
    
  } else { #if no veg points < 0.5 m
    FSD.i <- data.frame(matrix(rep(0, 14), ncol = 14))
    colnames(FSD.i) <- c("mean.max.canopy.ht",
                         "max.canopy.ht", 
                         "top.rugosity",
                         "vert.sd","sd.sd", "GFP",
                         "VAI", "VCI", "vertCV", 
                         "q0", "q25", "q50", "q75", "q100")
  }
  
  #------------------------------------------------------------------
  #Metrics from leafR
  #reads in las files different than lidR
  normlas.file <- paste0("./lidar/all_baseplots_0415_2021/", las.names[i])
  
  #lad profile doesn't use points below 1 m
  #k = 1 effective LAI
  #grain.size = 3 m horizontal grid resolution
  VOXELS_LAD <- lad.voxels(normlas.file, grain.size = 3, k = 1) 
  
  lad_profile <- lad.profile(VOXELS_LAD, relative = T) #uses relative total LAI
  fhd <- FHD(lad_profile, evenness = FALSE, LAD.threshold = -1) #default
 
  lad_profile_lai <- lad.profile(VOXELS_LAD, relative = F) #doesn't use relative LAI
  LAI <- lai(lad_profile_lai, min = 1, max = 75)
  LAI_subcanopy <- lai(lad_profile_lai, min = 1, max = 5)
  
  gini <- GC(normlas.file, threshold = .5)
  
  #######dennis added_12/15/2021
  # plot.volume <- voxel_volumes(data.40m, res= c(5,1))
  #-------------------------------------------------------------------
  out.i <- cbind.data.frame(las.names[i], plot_area_ground, 
                            den, maxZ, chm_metrics_i, FSD.i, 
                            fhd, gini, LAI, LAI_subcanopy)
  OUT <- rbind(OUT, out.i)}, 
  error = function(e) {skip_to_next <<- TRUE})


colnames(OUT)[1] <- "filename"
FSD <- OUT

#####################################################################################
# Create some indexing variables for the plot IDs, sites, years, etc. 
#####################################################################################
##add a site, PLOTID, and year column
siteID <- str_sub(FSD$filename, start=1, end = 4)
plotID <- str_sub(FSD$filename, start=1, end = 8)
year <- str_sub(FSD$filename, start=10, end = 13)
plotID_year <- str_sub(FSD$filename, start=1, end = 13)

FSD <- cbind(siteID, plotID, year, plotID_year, FSD)

#############################

#remove the plots that didn't have at least 1500 m2 area (plot_area_ground)
FSD[FSD$plot_area_ground <= 1500, ] 

FSD <- FSD[FSD$plot_area_ground > 1500, ] #dropped 6

#save the results
write.csv(FSD, file = "./disturbance_structural_diversity_data_2022_0422_latest.csv")

#####################################################################################

#End Workflow

#####################################################################################