###library##
library("ggplot2")
library('viridis')
library('sprof')
library('dplyr')
library('raster')
library('gstat')
library('EBImage')
library("reshape2")
library("RStoolbox")
library("rstatix")
library("tidyverse")
library("RColorBrewer")
library("sf")
library("mapview")
library("sprawl")
library("neondiveRsity")
library("neonUtilities")
library("geoNEON")
library("httr")
library("jsonlite")
library(ggstatsplot)
library("gridExtra")
library(cowplot)
library(ggpubr)
library(ggprism)
library(patchwork)
library(magrittr)
library(rgdal)
library(dotwhisker)
library(nlme)
library(gpboost)

setwd("D:/Project/pudue/neon/DisturbanceStructuralDiversityEDI/")
load("D:/Project/pudue/neon/DisturbanceStructuralDiversityEDI/veg.Rdata")
load("D:/Project/pudue/neon/DisturbanceStructuralDiversityEDI/veg_presence.Rdata")
load("D:/Project/pudue/neon/DisturbanceStructuralDiversityEDI/veglist.Rdata")
load("D:/Project/pudue/neon/DisturbanceStructuralDiversityEDI/vegmap.Rdata")

colnames(veg)
veg_presence$div_10m2Data100m2Data
veglist$vst_apparentindividual
vegmap

veg1 <- merge(veglist$vst_apparentindividual,vegmap, by = c("domainID", "siteID", "plotID", 
                                                           "individualID", "namedLocation"))
veg1 <- veg1 %>%
  arrange(siteID, plotID, individualID, date.x)
veg1 <- distinct(veg1)

colnames(veg1)
# veg1 <- veg1[, c(1:5, 7, 11:13, 16, 22, 36, 39, 45:61, 64, 67:74)]
colnames(veg1)[c(6, 12, 13, 30, 31)] <- c("date", "remarks", "QF_gr", "remarks_mp", "QF_mp")

colnames(veg1)
veg2 <- veg1 %>%
  distinct(veg1[, c("individualID", "date")], .keep_all = T)

veg3 <- veg1 %>%
  arrange(siteID, plotID, individualID, date) %>%
  group_by(domainID, siteID, plotID, individualID, namedLocation, date) %>%
  dplyr::slice(1)

colnames(veg3)

allplot <- st_read("./plot_location/allbaseplots.shp")
prj <- crs(allplot)


plot_info <- allplot[, c("siteID", "plotID", "plotSize", "soilOrder", "nlcdClass")]
veg4 <- merge(veg3, plot_info, by = c("siteID", "plotID"))

siteID <- unique(veg4$siteID)
plotID <- unique(veg4$plotID)

# head(veg)

# plot_1 <- baseplot_40m[baseplot_40m$plotID == "GRSM_062",]
coords <- data.frame(domainID = veg4$domainID,
                     siteID = veg4$siteID,
                     plotID = veg4$plotID,
                     subplotID = veg4$subplotID,
                     nestplotID = veg4$nestedSubplotID,
                     idtID = veg4$individualID,
                     species = veg4$scientificName,
                     class = veg4$nlcdClass,
                     date = veg4$date, 
                     growthform = veg4$growthForm,
                     plotsize = veg4$plotSize,
                     utm = veg4$utmZone,
                     x = c(veg4$adjDecimalLongitude),
                     y = c(veg4$adjDecimalLatitude),
                     xm = veg4$adjEasting,
                     ym = veg4$adjNorthing,
                     loc.uncertainty = veg4$adjCoordinateUncertainty,
                     plantStatus = veg4$plantStatus,
                     remarks = veg4$remarks,
                     species = veg4$scientificName,
                     DBH = veg4$stemDiameter,
                     height = veg4$height,
                     elv = veg4$adjElevation,
                     QF_gr = veg4$QF_gr)



# crs.s <- CRS(paste("+proj=utm +zone=", UTM_zones," +datum=WGS84 +units=m +no_defs", sep=""))
# allplot <- st_transform(allplot, crs.s)

# mapview(allplot)

# coords <- coords[!is.na(coords$x)&!is.na(coords$y), ]
tree_points <- SpatialPointsDataFrame(coords = c(coords[, c("x", "y")]), 
                                      proj4string = prj,
                                      data = coords)

plotid <- unique(coords$plotID)
# 
# for (i in seq_along(plotid)){
#   if (sum(coords$subplotID[coords$plotID == plotid[i]] %in% c(31, 32, 40)) != 0) {
#     coords$plotsize[coords$plotID == plotid[i]] <- 400
#   } else {
#     coords$plotsize[coords$plotID == plotid[i]] <- 400
#   }
#   if (length(unique(coords$plotsize[coords$plotID == plotid[i]])) != 1){
#     ## check whether the plot sizes were recorded wrong or not
#     print(plotid[i])
#   }
# }

coords <- coords %>%
  arrange(domainID, siteID, plotID, subplotID, nestplotID, idtID, date)

# # veg
# unique(coords$subplotID)

# coords <- coords[!is.na(coords$x)&!is.na(coords$y), ]
# coords

coords_df <- coords %>%
  mutate(BA = pi * (0.01 * DBH/2)^2,
         BA_ratio = BA/plotsize,
         Volume = DBH^2 * height) 
write.csv(coords_df, "tree_rm_duplicates.csv", row.names = F)

