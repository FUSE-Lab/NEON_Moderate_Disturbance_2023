lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(
    paste0('package:', pkgs),
    character.only = T,
    unload = T,
    force = T
  ))
################################################################################
#Workflow to obtain NEON AOP lidar and clean it
#Updated Apr. 7, 2021 by Elizabeth LaRue
#Updated Sep 7, 2022 by Dennis Choi

################################################################################

setwd("D:/Project/pudue/neon/DisturbanceStructuralDiversityEDI")

######################## Packages ##############################################

library('neonUtilities')
library('lidR') 
library("rgdal")
library("magrittr")
library("raster")
library("sp")
library("gstat")
library("EBImage") 
library('sf')
library('mapview')
library("rgeos")
library("terra")
#sessionInfo()

#R version 3.6.3 (2020-02-29)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 19042f)

#Matrix products: default

#locale:
#  [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
#[3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
#[5] LC_TIME=English_United States.1252    

#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
#  [1] sf_0.9-5            EBImage_4.28.1      gstat_2.0-6         magrittr_1.5       
#[5] rgdal_1.5-16        lidR_3.1.2          raster_3.3-13       sp_1.4-2           
#[9] neonUtilities_1.3.6

#loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.5          pillar_1.4.6        compiler_3.6.3      class_7.3-15       
#[5] bitops_1.0-6        tools_3.6.3         xts_0.12-0          digest_0.6.25      
#[9] lubridate_1.7.9     jsonlite_1.7.0      lifecycle_0.2.0     tibble_3.0.3       
#[13] lattice_0.20-38     png_0.1-7           pkgconfig_2.0.3     rlang_0.4.7        
#[17] DBI_1.1.0           rstudioapi_0.11     parallel_3.6.3      e1071_1.7-3        
#[21] dplyr_1.0.2         httr_1.4.2          htmlwidgets_1.5.1   fftwtools_0.9-8    
#[25] generics_0.0.2      vctrs_0.3.3         gtools_3.8.2        hms_0.5.3          
#[29] classInt_0.4-3      locfit_1.5-9.4      grid_3.6.3          tidyselect_1.1.0   
#[33] spacetime_1.2-3     glue_1.4.2          data.table_1.13.0   R6_2.4.1           
#[37] jpeg_0.1-8.1        gdata_2.18.0        tidyr_1.1.1         readr_1.3.1        
#[41] purrr_0.3.4         units_0.6-7         htmltools_0.5.0     BiocGenerics_0.32.0
#[45] codetools_0.2-16    ellipsis_0.3.1      intervals_0.15.2    abind_1.4-5        
#[49] tiff_0.1-5          KernSmooth_2.23-16  RCurl_1.98-1.2      crayon_1.3.4       
#[53] FNN_1.1.3           zoo_1.8-8    

#tutorials for trouble shooting
#https://jean-romain.github.io/lidRbook/engine2.html
#https://cran.r-project.org/web/packages/lidR/vignettes/lidR-LAScatalog-class.html
################################################################################
#                 STEP 1:
#                 DOWNLOADING DATA
#                 
# This workflow is meant to be run for one site and one year of data at a time
################################################################################
################################################################################
####################### NEON site spatial info - base plots ####################

#contains the plot centroids for 15 NEON sites
allplot <- st_read("./plot_location/allbaseplots.shp")
crs(allplot)
# plots <- as.data.frame(plots)

#these are the 15 sites that are the focus of this project
#raw liDAR data is currently stored in the Hardiman data depot at Purdue (~ 70 GB)
#downloaded September 2020 from NEONutilities

#unique(plots$siteID)
#BLAN BART GRSM HARV SOAP TEAK JERC OSBS SCBI STEI/CHEQ TALL UKFS UNDE ABBY GUAN

#SERC, MLBS DELA TREE

####################### Download AOP LiDAR from NEON data portal ################
#must manually enter which site to be downloaded in this step for the entire workflow (year & site)
unique(allplot$siteID)
SITECODE <- "DELA"

# [1] 
# c("ABBY", "BART", "BONA", "CLBJ", "DEJU", "DELA", "DSNY", "GRSM", "GUAN", "HARV", "HEAL", "JERC", "JORN",
# "LENO", "MLBS", "MOAB", "NIWO", "ONAQ", "ORNL", "OSBS", "SCBI", "SERC", "SOAP", "SRER", "STEI", "TALL", 
# "TEAK", "TOOL", "TREE", "UKFS", "UNDE", "BARR", "BLAN", "CPER", "DCFS", "KONZ", "LAJA", "NOGP", "OAES", 
# "SJER", "STER", "WOOD", "RMNP", "PUUM", "KONA", "WREF", "YELL")
# 
# CPER
# DCFS

# SITECODE <- c("BONA", "CLBJ", "DEJU", "DSNY", "HEAL", "JORN","LENO", "MOAB", "NIWO", "ONAQ", "ORNL", "SRER", 
#               "TEAK", "TOOL", "TREE", "BARR", "KONZ", "LAJA", "NOGP", "OAES", 
#               "SJER", "STER", "WOOD", "RMNP", "PUUM", "KONA", "WREF", "YELL")


allplot <- allplot[allplot$siteID == SITECODE, ]

# EASTING <- plots[, "easting"]
# NORTHING <- plots[, "northing"]


#################project UTM zones 18 N to 17N for BLAN site####################
UTM_zones <- unique(allplot$utmZone)[1]

crs.s <- CRS(paste("+proj=utm +zone=", UTM_zones," +datum=WGS84 +units=m +no_defs", sep=""))
allplot <- st_transform(allplot, crs.s)
mapview(allplot)

crs(allplot)
###############################################################################
plots_cent <- st_centroid(allplot)
plot(plots_cent)
crs(plots_cent)
plots_cent
mapview(plots_cent)

# plots_cent <- as.data.frame(plots_cent@coords)

allplot[,c("easting", "northing")] <- st_coordinates(plots_cent)
mapview(allplot)

plots <- allplot

crs(plots)

plots <- as.data.frame(plots)

EASTING <- plots$easting
NORTHING <- plots$northing

#uncomment the desired year
# YEAR <- 2013
# YEAR <- 2014
# YEAR <- 2015
# YEAR <- 2016
# YEAR <- 2017
YEAR <- 2018
# YEAR <- 2019
# YEAR <- 2020
# YEAR <- 2021
# YEAR <- 2022

#uncomment the function to download the site x year combination for forested baseplots
#extract the LIDAR AOP 1 km2 tiles that match the plot centroids specified
byTileAOP(dpID = "DP1.30003.001", site = SITECODE, year = YEAR, #can only do 1 year at a time
          easting = plots[, "easting"], northing = plots[, "northing"], check.size = T, buffer = 110, #110 m buffer around plot centroid
          savepath="./data/")
#will ask if you want to download - indicate y
################################################################################
#non-automated step (just the way NEONUtilities works)
################################################################################
#all the raw .laz tiles were placed in a single folder where specified
#manually insert the file path for all the downloaded data (from NEON utilities above)
#open as a las catalog object

ctg <- readLAScatalog(folder = "D:/Project/pudue/neon/DisturbanceStructuralDiversityEDI/data/DP1.30003.001/neon-aop-products/2015/FullSite/D07/2015_MLBS_1/L1/DiscreteLidar/ClassifiedPointCloud")
las_check(ctg)
plot(ctg)

crs(ctg)
################################################################################
################################################################################
#                 Step 2 - create a polygon of plot areas
################################################################################
################################################################################
#create a shape file of buffer plot centroids for one NEON site
#taken from NEON dataskills tutorial
#https://www.neonscience.org/resources/learning-hub/tutorials/field-data-polygons-centroids
################################################################################

# set the radius for a 80 x 80 m buffer around the plot centroid
radius <- 40 #radius in meters (actually a square not a circle)

# define the plot edges based upon the plot radius. 
yPlus <- plots$northing+radius
xPlus <- plots$easting+radius
yMinus <- plots$northing-radius
xMinus <- plots$easting-radius

# calculate polygon coordinates for each plot centroid. 
square=cbind(xMinus,yPlus,  # NW corner
             xPlus, yPlus,  # NE corner
             xPlus,yMinus,  # SE corner
             xMinus,yMinus, # SW corner
             xMinus,yPlus)  # NW corner again - close ploygon

ID= unique(plots$plotID)

# ID <- vector("list")
# for (i in seq_along(UTM_zones)){
#   ID[[i]] = plots[plots$utmZone == UTM_zones[i], "plotID"]
# }


polys <- SpatialPolygons(mapply(function(poly, id) 
{
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
}, 
split(square, row(square)), ID),
proj4string = crs.s)

#Create SpatialPolygonDataFrame -- this step is required to output multiple polygons.
polys.df.buffer <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))
plot(polys.df.buffer, col=rainbow(50, alpha=0.5))

mapview(polys.df.buffer)+ mapview(allplot)
################################################################################
################################################################################
#Step 3 - clip the lidar tiles with the plot buffer polygon
################################################################################

#Tell the code where you want your clipped plots to go.
# {plotID} is telling the code to name the output files by their plot numbers.

opt_filter(ctg) <- "-drop_class 7" ### Dennis added
opt_output_files(ctg) <- paste0("./lidar/Processed_add/",SITECODE, YEAR,"/buffer/{id}", sep="") 

plot(ctg)
#clip the area within the point clouds from within the plot buffer polygon 
#this step takes a while depending on your RAM and processor

clipped.plot.s <- clip_roi(ctg, polys.df.buffer) 
################################################################################
################################################################################
#STEP 4: Remove outliers and correct for Z height to account for elevation ##
################################################################################

# --------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#function to calculate and remove unwanted outliers in readLAS()
outliers <- function(las_file) {
  #remove outlier points using mean and sd statistics
  Z_sd=sd(las_file@data$Z)
  Z_mean=mean(las_file@data$Z)
  #using 6 sd as a coarse filter due to topopographic variation
  f= paste("-drop_z_below",(Z_mean-6*Z_sd),"-drop_z_above",(Z_mean+6*Z_sd))
  return(f)
}
# --------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------
#running these plots through a for loop because they are relatively small
#and sometimes the buffer areas overlap - cannot use spatially overlapping files
#in catalog functions to correct for elevation
dir.create(paste("./lidar/Processed_add/",SITECODE, YEAR, "/rm_noise/", sep=""))
buffer.names <- list.files(paste0("./lidar/Processed_add/", SITECODE, YEAR, "/buffer", sep="")) 

for(i in 1:length(buffer.names))tryCatch({
  #reading in individual las of each plot
  las_file<- readLAS(file.path(paste("./lidar/Processed_add/",SITECODE, YEAR, "/buffer/", buffer.names[i], sep="")))
  print(buffer.names[i])
  print("Z range pre filter")
  print(range(las_file@data$Z))
  
  #drop out outliers with SD filter
  f <- outliers(las_file)
  las_file <- readLAS(file.path(paste("./lidar/Processed_add/",SITECODE, YEAR, "/buffer/", buffer.names[i], sep="")), filter = f)
  print("Z range post SD filter")
  print(range(las_file@data$Z))
  
  #drop out finer outliers with an IVF filter
  #ivf = isolated voxels filter, which finds points with only a few other points
  #in their surrounding 3 x 3 x 3 = 27 voxels neighborhood
  #res = voxel resolution, n = maximal number of other points in the 27 voxels
  #similar to lasnoise from LAStools https://rapidlasso.com/lastools/lasnoise/
  buffer.noise <- classify_noise(las_file, ivf(res=3,n=0)) 
  #remove points with a classification (18) of noise 
  las_denoise <- filter_poi(buffer.noise, Classification != LASNOISE)
  print("Z range post ivf filter")
  print(range(las_denoise@data$Z)) #after filter Z range
  
  #correct for ground height
  #might get a deprecated points warning, but the function automatically removes these
  las_file <- normalize_height(las_denoise, tin())
  
  #save new clean buffer file
  writeLAS(las_file, paste("./lidar/Processed_add/",SITECODE, YEAR, "/rm_noise/", buffer.names[i], sep=""))
}, 
error = function(e) {skip_to_next <<- TRUE})

# --------------------------------------------------------------------------------------
#read in newly created and correct buffer plot .las files into a catalog
#---------------------------------------------------------------------------------------
rm_noise <- readLAScatalog(folder = paste0("./lidar/Processed_add/",SITECODE, YEAR,"/rm_noise/", sep=""))
plot(rm_noise)

######################################################################################
#####################################################################################
#Step 5 create a shape file of 40 x 40 m plots for final clip
#####################################################################################

#create CRS for each site's UTM zone
# UTM <- droplevels(plots[1, "utmZone"])
# crs.s <- CRS(paste("+proj=utm +zone=", UTM," +datum=WGS84 +units=m +no_defs", sep=""))
crs.s <- crs(ctg)


# set the radius for the plots
radius <- 20 # radius in meters

# define the plot edges based upon the plot radius.
yPlus <- plots$northing+radius
xPlus <- plots$easting+radius
yMinus <- plots$northing-radius
xMinus <- plots$easting-radius

# calculate polygon coordinates for each plot centroid.
square=cbind(xMinus,yPlus,  # NW corner
             xPlus, yPlus,  # NE corner
             xPlus,yMinus,  # SE corner
             xMinus,yMinus, # SW corner
             xMinus,yPlus)  # NW corner again - close ploygon

# ID= droplevels(plots$plotID)
ID = plots$plotID

polys <- SpatialPolygons(mapply(function(poly, id)
{
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
},
split(square, row(square)), ID),
proj4string = crs.s)

#Create SpatialPolygonDataFrame -- this step is required to output multiple polygons.
polys.df.baseplot <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))
plot(polys.df.baseplot)

mapview(allplot) + mapview(polys.df.baseplot)
##################################################################################### 
#####################################################################################
#STEP 6: Clip final 40 x 40 m plot .las
#####################################################################################
#####################################################################################
## Data will be saved in specified folder
opt_output_files(rm_noise) <-  paste0("./lidar/Processed_add/",SITECODE, YEAR, "/baseplots/{id}", "_", YEAR, sep="")

#clip 40 x 40 m plot. las


baseplots <- clip_roi(rm_noise, polys.df.baseplot)
plot(baseplots)

#####################################################################################
#copied the processed 40 x 40 m point clouds .las files created above 
#and put them into one folder to make step 2 
#run through data in one folder ("./lidar/all_baseplots/")

#######Dennis added



#####################################################################################
#End Part of Workflow

#####################################################################################

