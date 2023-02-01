library(lidR)

setwd("D:/Project/pudue/neon/DisturbanceStructuralDiversityEDI")


set.seed(9999)
# 
las <- list.files("./lidar/Processed_add/MLBS2015/baseplots/", pattern = "*.las", full.names =  F)

for (i in seq_along(las)) {
  a <- readLAS(paste0("D:/Project/pudue/neon/DisturbanceStructuralDiversityEDI/lidar/Processed_add/MLBS2015/baseplots/", las[[i]]))
  a <- decimate_points(a, homogenize(2,5))
  writeLAS(a, paste0("D:/Project/pudue/neon/DisturbanceStructuralDiversityEDI/lidar/Processed_add/MLBS2015/baseplots_den4/",las[[i]]))
  print(las[[i]])
}


#source(fsd.r)

library('lidR')
library('leafR')
library('stringr')
library('plyr')
library('dplyr')
library("viridis")

las <- list.files("./lidar/Processed_add/MLBS2015/baseplots_den4/", pattern = "*.las", full.names =  F)
chm_groundpts_metrics <- function(data.40m) {
  #ground points are not filtered out here so can get actual plot area 
  
  #set any points < 0.5 m (0503_2022 Dennis Changed the value from 3 m to 0.5) as zero because to get rid of ground points (count as < 0.5 m here, and #very short understory vegetation)
  data.40m@data$Z[data.40m@data$Z <= 0.5] <- 0
  
  #if then statment to filter out any bad plots (no points or no points > 0)
  if(sum(data.40m@data$Z) > 0) {
    chm <- grid_canopy(data.40m, res = 1, pitfree(c(0,2,5,10,15), c(0, 1.5))) # this uses all the points in the chm
    
    #metrics from a chm (grid_canopy)
    rumple <- rumple_index(chm)
    
    #deep gap fraction (fraction of total rasterized area (1 m2 cells) w/points that are 
    #zero height (> 0.5 m))
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
  chm <- grid_canopy(data.40m, res = 1, pitfree(c(0,2,5,10,15), c(0, 1.5))) 
  mean.max.canopy.ht <- mean(chm@data@values, na.rm = TRUE) 
  max.canopy.ht <- max(chm@data@values, na.rm=TRUE) 
  top.rugosity <- sd(chm@data@values, na.rm = TRUE) 
  
  #metrics from cloud_metrics (Dennnis changed the cloud metrics since erros occur)
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
  
  
  ### Dennis Changed the height threshold from 3m to 0.5m since there are shrub plots
  gap_frac <- gap_fraction_profile(Zs, dz = 1, z0 = 0.5) #ignore points < 0.5 m
  GFP <- mean(gap_frac$gf) 
  LADen<-LAD(Zs, dz = 1, k=.5, z0= 0.5) #ignore points < 0.5 m   
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

las.names <- list.files("./lidar/Processed_add/MLBS2015/baseplots_den4/", pattern = "*.las") #set location where all plot .las were added

OUT <- NULL
# skip_to_next <- F
#i=1
for(i in 1:length(las.names)) tryCatch({
  #reading in individual las plots without filtering to get area and density
  data.40m <- readLAS(file.path(paste0("./lidar/Processed_add/MLBS2015/baseplots_den4/", las.names[i])))
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
  data.40m <- readLAS(file.path(paste0("./lidar/Processed_add/MLBS2015/baseplots_den4/", las.names[i])), filter = "-drop_z_below .5")
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
  normlas.file <- paste0("./lidar/Processed_add/MLBS2015/baseplots_den4/", las.names[i])
  
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
head(FSD)
siteID <- str_sub(FSD$filename, start=1, end = 4)
plotID <- str_sub(FSD$filename, start=1, end = 11)
year <- str_sub(FSD$filename, start=13, end = 16)
plotID_year <- str_sub(FSD$filename, start=1, end = 16)

FSD <- cbind(siteID, plotID, year, plotID_year, FSD)

#############################

#remove the plots that didn't have at least 1500 m2 area (plot_area_ground)
FSD[FSD$plot_area_ground <= 1200, ] 

FSD <- FSD[FSD$plot_area_ground > 1200, ] #dropped 6

#save the results
write.csv(FSD, file = "./lidar/Processed_add/MLBS2015/FSD_MLBS2015_den4.csv")

#####################################################################################
