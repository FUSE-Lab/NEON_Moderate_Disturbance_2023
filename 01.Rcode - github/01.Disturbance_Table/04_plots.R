# ################################################################################
# #diagnostic plots to see FSD metrics look good
# 
# #Elizabeth LaRue 
# #elarue@purdue.edu
# #Updated April 29, 2021
# #Updated March 7, 2022 by Dennis Choi

# #this works with the R Version 3.6.3
# ################################################################################
# ############################################################################
# ############################################################################
# #           Weed out plots that do not have a value each year of data
# ############################################################################
# # ############################################################################
lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(
    paste0('package:', pkgs),
    character.only = T,
    unload = T,
    force = T
  ))

setwd("D:\\Project\\pudue\\neon\\DisturbanceStructuralDiversityEDI")
# dat <- read.csv("disturbance_structural_diversity_data_2021_1228.csv")
# # dat <- na.omit(dat)

dat <- read.csv("disturbance_structural_diversity_data_2022_0211.csv")
sites <- unique(dat$site)
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
write.csv(OUT.KEEP, "FSD_DISTURB_noNAplots_02152022.csv")
# 
# #######################################################################################
setwd("D:\\Project\\pudue\\neon\\DisturbanceStructuralDiversityEDI")

library(ggplot2)
library("gridExtra")
library(cowplot)

dat <- read.csv("FSD_DISTURB_noNAplots_02252022.csv")
colnames(dat)

# dat <- dat[, -1]

dat <- subset(dat, site == "SCBI"|site == "HARV"|site == "BLAN"|site == "UKFS"|site == "BART")
# #################aaaaaaa###############################################################
#histograms of metrics to look at distribution

a <- ggplot(dat, aes(mean.max.canopy.ht, fill = site)) +
  geom_histogram(binwidth = max(dat$mean.max.canopy.ht*.05, na.rm = TRUE))
a
b <- ggplot(dat, aes(q25, fill = site)) +
  geom_histogram(binwidth = max(dat$q25*.05, na.rm = TRUE))
b
c <- ggplot(dat, aes(q50, fill = site)) +
  geom_histogram(binwidth = max(dat$q50*.05, na.rm = TRUE))
c
d <- ggplot(dat, aes(q75, fill = site)) +
  geom_histogram(binwidth = max(dat$q75*.05, na.rm = TRUE))
d
e <- ggplot(dat, aes(q100, fill = site)) +
  geom_histogram(binwidth = max(dat$q100*.05, na.rm = TRUE))
e


f <- ggplot(dat, aes(deepgap.fraction, fill = site)) +
  geom_histogram(binwidth = max(dat$deepgap.fraction*.05, na.rm = TRUE))

g <- ggplot(dat, aes(GFP, fill = site)) +
  geom_histogram(binwidth = max(dat$GFP*.05, na.rm = TRUE))
g

h <- ggplot(dat, aes(VAI, fill = site)) +
  geom_histogram(binwidth = max(dat$VAI*.05, na.rm = TRUE))
h
i <- ggplot(dat, aes(LAI, fill = site)) +
  geom_histogram(binwidth = max(dat$LAI*.05, na.rm = TRUE))
i
ii <- ggplot(dat, aes(LAI_subcanopy, fill = site)) +
  geom_histogram(binwidth = max(dat$LAI_subcanopy*.05, na.rm = TRUE))
ii
j <- ggplot(dat, aes(rumple, fill = site)) +
  geom_histogram(binwidth = max(dat$rumple*.05, na.rm = TRUE))
j
k <- ggplot(dat, aes(top.rugosity, fill = site)) +
  geom_histogram(binwidth = max(dat$top.rugosity*.05, na.rm = TRUE))
k
l <- ggplot(dat, aes(vert.sd, fill = site)) +
  geom_histogram(binwidth = max(dat$vert.sd *.05, na.rm = TRUE))
l
m <- ggplot(dat, aes(sd.sd, fill = site)) +
  geom_histogram(binwidth = max(dat$sd.sd*.05, na.rm = TRUE))
m
n <- ggplot(dat, aes(vertCV, fill = site)) +
  geom_histogram(binwidth = max(dat$vertCV*.05, na.rm = TRUE))
n
o <- ggplot(dat, aes(VCI, fill = site)) +
  geom_histogram(binwidth = max(dat$VCI*.05, na.rm = TRUE))
o
p <- ggplot(dat, aes(fhd, fill = site)) +
  geom_histogram(binwidth = max(dat$fhd*.05, na.rm = TRUE))
p
q <- ggplot(dat, aes(gini, fill = site)) +
  geom_histogram(binwidth = max(dat$gini*.05, na.rm = TRUE))
q
# 
# pdf("hist_FSD1_per.pdf")
# plot_grid(a, b, c, d,  labels=c("a", "b", "c", "d") ,ncol = 2, nrow = 2)
# e
# dev.off()
# 
# pdf("hist_FSD2_per.pdf")
# plot_grid(f, g, h, i, labels = c("f", "g","h", "i"),
#           ncol = 2, nrow = 2)
# ii
# dev.off()
# 
# pdf("hist_FSD3_per.pdf")
# plot_grid(j,k,l,m, labels = c("j", "k", "l", "m"),
#           ncol = 2, nrow = 2)
# dev.off()
# 
# pdf("hist_FSD4.pdf")
# plot_grid(n,o,p,q, labels = c("n", "o", "p", "q"),
#           ncol = 2, nrow = 2)
# dev.off()
# ############################################################################
# ggplot(dat, aes(den, fill = site)) +
#   geom_histogram(binwidth = max(dat$den*.05, na.rm = TRUE))
# 

#########################################DENNIS##################################
setwd("D:\\Project\\pudue\\neon\\DisturbanceStructuralDiversityEDI")
library(ggplot2)
library("gridExtra")
library(cowplot)
library(viridis)
#BLAN BART GRSM HARV SOAP TEAK JERC OSBS SCBI STEI/CHEQ TALL UKFS UNDE ABBY GUAN


#############################plot by plot#######################
all <- read.csv("disturbance_structural_diversity_data_2022_0118.csv")
sites <- unique(all$site)

all <- all[all$site == "JERC", ]
dat <- all[,-c(1,5, 6, 7)]
# 
# scaled.dat <- scale(dat[, c(5:25)])
# dat <- cbind(dat[,c(1:3)], scaled.dat)

#variable categorizing
df_melt <- reshape2::melt(dat, id.var = c('site', 'plotID', 'year'))

ggplot(df_melt[df_melt$variable == "LAI_subcanopy",], 
       aes(x = year, y = value, label= plotID)) +
  geom_point(shape= 1, alpha= 1, size= 2.5) +
  geom_text(size = 2)+
  facet_wrap(~variable, ncol = 5)





ggplot(df_melt, aes(x = factor(df_melt$year), y = value)) +
  geom_boxplot(outlier.colour = "red", 
               outlier.shape = 20, outlier.size = 3, 
               outlier.alpha = 0.5, lwd=0.1) +
  stat_summary(fun = mean, 
               geom = "point", 
               shape = 1, 
               size = 1) +
  stat_summary(fun = "median", 
               geom = "point", 
               shape = 16, 
               size = 1, 
               color = "lightgreen")+
  # geom_text(check_overlap = T, 
  #           position=position_jitter(width=0.00), 
  #           size= 2)+
  geom_hline(aes(yintercept = 0), colour = "darkgrey", lwd= 0.05)+
  facet_wrap(~variable, ncol = 5)


##############LAI_subcanpoy

LAI_sub_JERC <- df_melt[df_melt$variable == "LAI_subcanopy",]

ggplot(LAI_sub_JERC, aes(x = factor(year), y = value)) +
  geom_boxplot(outlier.colour = "red", 
               outlier.shape = 20, outlier.size = 3, 
               outlier.alpha = 0.5, lwd=0.1) +
  stat_summary(fun = mean, 
               geom = "point", 
               shape = 1, 
               size = 1) +
  stat_summary(fun = "median", 
               geom = "point", 
               shape = 16, 
               size = 1, 
               color = "lightgreen")

######################################################

setwd("D:\\Project\\pudue\\neon\\DisturbanceStructuralDiversityEDI")
library(ggplot2)
library("gridExtra")
library(cowplot)
library(viridis)
library(dplyr)


all <- read.csv("FSD_noNA_percent_change_0118_2022.csv")
all$year <- factor(all$year)
all$year <- paste0(all$year, "_", all$no_years)
sites <- unique(all$site)
dat <- all[,-c(1,3, 5, 6, 7)]
# all <- all[all$site == "GRSM", ]


dat_mean <- dat %>%
  group_by(site) %>%
  summarise_at(vars(dat[,c(4:25)]), list(name = mean))


# scaled.dat <- scale(dat[, c(5:25)])
# dat <- cbind(dat[,c(1:3)], scaled.dat)

#variable categorizing
df_melt <- reshape2::melt(dat, id.var = c('site', 'plotID', 'year'))

ggplot(df_melt, aes(x = year, y = value)) +
  geom_point(shape= 1, alpha= 1, size= 2.5) +
  facet_wrap(~variable, ncol = 5)

#################################################################
setwd("D:\\Project\\pudue\\neon\\DisturbanceStructuralDiversityEDI")
library(ggplot2)
library("gridExtra")
library(cowplot)
library(viridis)
#BLAN BART GRSM HARV SOAP TEAK JERC OSBS SCBI STEI/CHEQ TALL UKFS UNDE ABBY GUAN



for (i in seq_along(sites)){
  dat <- all[all$site == paste0(sites[i]),-c(1,5, 6, 7)]
  # scaled.dat <- scale(dat[, c(5:25)])
  # dat <- cbind(dat[,c(1:3)], scaled.dat)

  #variable categorizing
  df_melt <- reshape2::melt(dat, id.var = c('site', 'plotID', 'year'))
  df_complexity <- rbind(df_melt[df_melt$variable == "rumple",], 
                         df_melt[df_melt$variable == "top.rugosity",])
  df_internal_cpx <- rbind(df_melt[df_melt$variable == "vert.sd",],
                           df_melt[df_melt$variable == "sd.sd",],
                           df_melt[df_melt$variable == "VCI",],
                           df_melt[df_melt$variable == "vertCV",],
                           df_melt[df_melt$variable == "fhd",],
                           df_melt[df_melt$variable == "gini",])
  df_Height <- rbind(df_melt[df_melt$variable == "maxZ",],
                     df_melt[df_melt$variable == "q25",],
                     df_melt[df_melt$variable == "q50",],
                     df_melt[df_melt$variable == "q75",],
                     df_melt[df_melt$variable == "mean.max.canopy.ht",])
  df_open <- rbind(df_melt[df_melt$variable == "deepgap.fraction",],
                   df_melt[df_melt$variable == "GFP",])
  df_density <- rbind(df_melt[df_melt$variable == "VAI",])
  df_LAI <- rbind(df_melt[df_melt$variable == "LAI",],
                  df_melt[df_melt$variable == "LAI_subcanopy",])
  
  ###pdf saving
  pdf(paste0(sites[i], "_yearly_structures_0118_2022.pdf"), width = 11, height = 8.5)
  
  #ggplot
  complexity <- ggplot(df_complexity, aes(x = year, y = value)) +
    geom_point(aes(fill = variable),shape= 21, alpha= 0.7, size= 2.5) +
    scale_fill_manual(values= viridis(length(unique(df_complexity$variable)))) +
    geom_line(aes(color = variable)) + 
    scale_color_manual(values = viridis(length(unique(df_complexity$variable)))) +
    facet_wrap(~plotID, ncol = 5) 
  
  inter_complexity <- ggplot(df_internal_cpx, aes(x = year, y = value)) +
    geom_point(aes(fill = variable),shape= 21, alpha= 0.7, size= 2.5) +
    scale_fill_manual(values= viridis(length(unique(df_internal_cpx$variable)))) +
    geom_line(aes(color = variable), alpha = 0.5) + 
    scale_color_manual(values = viridis(length(unique(df_internal_cpx$variable)))) +
    facet_wrap(~plotID, ncol = 5) 
  
  height <- ggplot(df_Height, aes(x = year, y = value)) +
    geom_point(aes(fill = variable),shape= 21, alpha= 0.7, size= 3) +
    scale_fill_manual(values= viridis(length(unique(df_Height$variable)))) +
    geom_line(aes(color = variable)) + 
    scale_color_manual(values = viridis(length(unique(df_Height$variable)))) +
    facet_wrap(~plotID, ncol = 5) 
  
  density <- ggplot(df_density, aes(x = year, y = value)) +
    geom_point(aes(fill = variable),shape= 21, alpha= 0.7, size= 2.5) +
    scale_fill_manual(values= viridis(length(unique(df_density$variable)))) +
    geom_line(aes(color = variable), alpha = 0.5) + 
    scale_color_manual(values = viridis(length(unique(df_density$variable)))) +
    facet_wrap(~plotID, ncol = 5) 
  
  LAI <- ggplot(df_LAI, aes(x = year, y = value)) +
    geom_point(aes(fill = variable),shape= 21, alpha= 0.7, size= 2.5) +
    scale_fill_manual(values= viridis(length(unique(df_LAI$variable)))) +
    geom_line(aes(color = variable), alpha = 0.5) + 
    scale_color_manual(values = viridis(length(unique(df_LAI$variable)))) +
    facet_wrap(~plotID, ncol = 5) 
  
  open <- ggplot(df_open, aes(x = year, y = value)) +
    geom_point(aes(fill = variable),shape= 21, alpha= 0.7, size= 2.5) +
    scale_fill_manual(values= viridis(length(unique(df_open$variable)))) +
    geom_line(aes(color = variable), alpha = 0.5) + 
    scale_color_manual(values = viridis(length(unique(df_open$variable)))) +
    facet_wrap(~plotID, ncol = 5) 
  
  a <- list(complexity, inter_complexity, height, density, LAI, open)
    for (n in seq_along(a)) {
      print(a[[n]])
    }
  ###finish
  dev.off()
}


#################################boxplot_sitebysite################
setwd("D:\\Project\\pudue\\neon\\DisturbanceStructuralDiversityEDI")
library(ggplot2)
library("gridExtra")
library(cowplot)
library(viridis)
#BLAN BART GRSM HARV SOAP TEAK JERC OSBS SCBI STEI/CHEQ TALL UKFS UNDE ABBY GUAN

all <- read.csv("FSD_noNA_absolute_change_0118_2022.csv")

all <- absolute_change
sites <- unique(all$site)

# ##################example#######################
# dat <- all[,-c(1,5, 6, 7)]
# scaled.dat <- scale(dat[, c(5:25)])
# dat <- cbind(dat[,c(1:3)], scaled.dat, factor(dat$no_years))
# dat$year <- factor(dat$year)
# names(dat)[25] <- c("no_years")
# dat$year_no <- paste0(dat$year, "_",dat$no_years)
# 
# df_melt <- reshape2::melt(dat, id.var = c('site', 'plotID', 'year', 'no_years'))
# 
# 
# df_melt <- reshape2::melt(dat, id.var = c('site', 'plotID', 'year', 'no_years'))
# df_complexity <- rbind(df_melt[df_melt$variable == "rumple",],
#                        df_melt[df_melt$variable == "top.rugosity",])
# df_internal_cpx <- rbind(df_melt[df_melt$variable == "vert.sd",],
#                          df_melt[df_melt$variable == "sd.sd",],
#                          df_melt[df_melt$variable == "VCI",],
#                          df_melt[df_melt$variable == "vertCV",],
#                          df_melt[df_melt$variable == "fhd",],
#                          df_melt[df_melt$variable == "gini",])
# df_Height <- rbind(df_melt[df_melt$variable == "maxZ",],
#                    df_melt[df_melt$variable == "q25",],
#                    df_melt[df_melt$variable == "q50",],
#                    df_melt[df_melt$variable == "q75",],
#                    df_melt[df_melt$variable == "mean.max.canopy.ht",])
# df_open <- rbind(df_melt[df_melt$variable == "deepgap.fraction",],
#                  df_melt[df_melt$variable == "GFP",])
# df_density <- rbind(df_melt[df_melt$variable == "VAI",])
# df_LAI <- rbind(df_melt[df_melt$variable == "LAI",],
#                 df_melt[df_melt$variable == "LAI_subcanopy",])
# 
# ggplot(df_open, aes(x = year, y = value, label = plotID)) +
#   geom_boxplot(outlier.colour = "red", outlier.shape = 20, outlier.size = 3, outlier.alpha = 0.5, lwd=0.1) +
#   stat_summary(fun = mean,
#                geom = "point", shape = 1, size = 1) +
#   stat_summary(fun = "median", geom = "point", shape = 16, size = 1, color = "lightgreen")+
#   geom_text(check_overlap = T, position=position_jitter(width=0.00), size= 2)+
#   geom_hline(aes(yintercept = 0), colour = "darkgrey", lwd= 0.05)+
#   facet_wrap(~variable, ncol = 5)
#####################################################
for (i in seq_along(sites)){
  dat <- all[all$site == paste0(sites[i]),-c(1,5, 6, 7)]
  scaled.dat <- scale(dat[, c(5:25)])
  dat <- cbind(dat[,c(1:3)], scaled.dat, factor(dat$no_years))
  dat$year <- factor(dat$year)
  names(dat)[25] <- c("no_years")
  dat$year_no <- paste0(dat$year, "_",dat$no_years)
  
  #variable categorizing
  df_melt <- reshape2::melt(dat, id.var = c('site', 'plotID', 'year', 'no_years', 'year_no'))
  df_complexity <- rbind(df_melt[df_melt$variable == "rumple",], 
                         df_melt[df_melt$variable == "top.rugosity",])
  df_internal_cpx <- rbind(df_melt[df_melt$variable == "vert.sd",],
                           df_melt[df_melt$variable == "sd.sd",],
                           df_melt[df_melt$variable == "VCI",],
                           df_melt[df_melt$variable == "vertCV",],
                           df_melt[df_melt$variable == "fhd",],
                           df_melt[df_melt$variable == "gini",])
  df_Height <- rbind(df_melt[df_melt$variable == "maxZ",],
                     df_melt[df_melt$variable == "q25",],
                     df_melt[df_melt$variable == "q50",],
                     df_melt[df_melt$variable == "q75",],
                     df_melt[df_melt$variable == "mean.max.canopy.ht",])
  df_open <- rbind(df_melt[df_melt$variable == "deepgap.fraction",],
                   df_melt[df_melt$variable == "GFP",])
  df_density <- rbind(df_melt[df_melt$variable == "VAI",])
  df_LAI <- rbind(df_melt[df_melt$variable == "LAI",],
                  df_melt[df_melt$variable == "LAI_subcanopy",])

  ###pdf saving
  pdf(paste0(sites[i], "_changes_boxplots_0118_2022.pdf"), width = 16, height = 8.5)
  
  #ggplot
  complexity <- ggplot(df_complexity, aes(x = year_no, y = value, label = plotID)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 20, outlier.size = 3, outlier.alpha = 0.5, lwd=0.1) +
    stat_summary(fun = mean, 
                 geom = "point", shape = 1, size = 1) +
    stat_summary(fun = "median", geom = "point", shape = 16, size = 1, color = "lightgreen")+
    geom_text(check_overlap = T, position=position_jitter(width=0.00), size= 2)+
    geom_hline(aes(yintercept = 0), colour = "darkgrey", lwd= 0.05)+
    facet_wrap(~variable, ncol = 5)
  
  inter_complexity <- ggplot(df_internal_cpx, aes(x = year_no, y = value, label = plotID)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 20, outlier.size = 3, outlier.alpha = 0.5, lwd=0.1) +
    stat_summary(fun = mean, 
                 geom = "point", shape = 1, size = 1) +
    stat_summary(fun = "median", geom = "point", shape = 16, size = 1, color = "lightgreen")+
    geom_text(check_overlap = T, position=position_jitter(width=0.00), size= 2)+
    geom_hline(aes(yintercept = 0), colour = "darkgrey", lwd= 0.05)+
    facet_wrap(~variable, ncol = 5)
  
  height <- ggplot(df_Height, aes(x = year_no, y = value, label = plotID)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 20, outlier.size = 3, outlier.alpha = 0.5, lwd=0.1) +
    stat_summary(fun = mean, 
                 geom = "point", shape = 1, size = 1) +
    stat_summary(fun = "median", geom = "point", shape = 16, size = 1, color = "lightgreen")+
    geom_text(check_overlap = T, position=position_jitter(width=0.00), size= 2)+
    geom_hline(aes(yintercept = 0), colour = "darkgrey", lwd= 0.05)+
    facet_wrap(~variable, ncol = 5)
  
  density <- ggplot(df_density, aes(x = year_no, y = value, label = plotID)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 20, outlier.size = 3, outlier.alpha = 0.5, lwd=0.1) +
    stat_summary(fun = mean, 
                 geom = "point", shape = 1, size = 1) +
    stat_summary(fun = "median", geom = "point", shape = 16, size = 1, color = "lightgreen")+
    geom_text(check_overlap = T, position=position_jitter(width=0.00), size= 2)+
    geom_hline(aes(yintercept = 0), colour = "darkgrey", lwd= 0.05)+
    facet_wrap(~variable, ncol = 5)
  
  LAI <- ggplot(df_LAI, aes(x = year_no, y = value, label = plotID)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 20, outlier.size = 3, outlier.alpha = 0.5, lwd=0.1) +
    stat_summary(fun = mean, 
                 geom = "point", shape = 1, size = 1) +
    stat_summary(fun = "median", geom = "point", shape = 16, size = 1, color = "lightgreen")+
    geom_text(check_overlap = T, position=position_jitter(width=0.00), size= 2)+
    geom_hline(aes(yintercept = 0), colour = "darkgrey", lwd= 0.05)+
    facet_wrap(~variable, ncol = 5)
  
  open <- ggplot(df_open, aes(x = year_no, y = value, label = plotID)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 20, outlier.size = 3, outlier.alpha = 0.5, lwd=0.1) +
    stat_summary(fun = mean, 
                 geom = "point", shape = 1, size = 1) +
    stat_summary(fun = "median", geom = "point", shape = 16, size = 1, color = "lightgreen")+
    geom_text(check_overlap = T, position=position_jitter(width=0.00), size= 2)+
    geom_hline(aes(yintercept = 0), colour = "darkgrey", lwd= 0.05)+
    facet_wrap(~variable, ncol = 5)
  
  a <- list(complexity, inter_complexity, height, density, LAI, open)
  for (n in seq_along(a)) {
    print(a[[n]])
  }
  ###finish
  dev.off()
}


# dev.off()
############################################
dev.off()

print(complexity, inter_complexity, height, density, LAI, open)

dat <- dat[dat$site == "SOAP",-c(1,5, 6)]
# dat.na.omit <- na.omit(dat)
# write.csv(dat.na.omit, "./FSD_noNAplots_12282021_nona.csv")
# str.pca <- prcomp(dat[,c(6:26)], center = TRUE,scale. = TRUE)
# 
# library(ggbiplot)
# library(factoextra)
# 
# ggbiplot(str.pca)
# ggbiplot(str.pca, labels=rownames(dat))
# 
# fviz_eig(str.pca)
# fviz_pca_ind(str.pca,
#              col.ind = "cos2", # Color by the quality of representation
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )
# 

df_melt <- reshape2::melt(dat, id.var = c('site', 'plotID', 'year'))
df_complexity <- rbind(df_melt[df_melt$variable == "rumple",], 
                            df_melt[df_melt$variable == "top.rugosity",])

df_internal_cpx <- rbind(df_melt[df_melt$variable == "vert.sd",],
                              df_melt[df_melt$variable == "sd.sd",],
                              df_melt[df_melt$variable == "VCI",],
                              df_melt[df_melt$variable == "vertCV",],
                              df_melt[df_melt$variable == "fhd",],
                              df_melt[df_melt$variable == "gini",])

df_Height <- rbind(df_melt[df_melt$variable == "maxZ",],
                        df_melt[df_melt$variable == "q25",],
                        df_melt[df_melt$variable == "q50",],
                        df_melt[df_melt$variable == "q75",],
                        df_melt[df_melt$variable == "mean.max.canopy.ht",])

df_open <- rbind(df_melt[df_melt$variable == "deepgap.fraction",],
                      df_melt[df_melt$variable == "GFP",])

df_density <- rbind(df_melt[df_melt$variable == "VAI",])

df_LAI <- rbind(df_melt[df_melt$variable == "LAI",],
                        df_melt[df_melt$variable == "LAI_subcanopy",])


pdf("ABBY_yearly_structures.pdf", width = 11, height = 8.5)

complexity <- ggplot(df_complexity, aes(x = year, y = value)) +
  geom_point(aes(fill = variable),shape= 21, alpha= 0.7, size= 2.5) +
  scale_fill_manual(values= viridis(length(unique(df_complexity$variable)))) +
  geom_line(aes(color = variable)) + 
  scale_color_manual(values = viridis(length(unique(df_complexity$variable)))) +
  facet_wrap(~plotID, ncol = 5) 
complexity

inter_complexity <- ggplot(df_internal_cpx, aes(x = year, y = value)) +
  geom_point(aes(fill = variable),shape= 21, alpha= 0.7, size= 2.5) +
  scale_fill_manual(values= viridis(length(unique(df_internal_cpx$variable)))) +
  geom_line(aes(color = variable), alpha = 0.5) + 
  scale_color_manual(values = viridis(length(unique(df_internal_cpx$variable)))) +
  facet_wrap(~plotID, ncol = 5) 
inter_complexity

height <- ggplot(df_Height, aes(x = year, y = value)) +
  geom_point(aes(fill = variable),shape= 21, alpha= 0.7, size= 3) +
  scale_fill_manual(values= viridis(length(unique(df_Height$variable)))) +
  geom_line(aes(color = variable)) + 
  scale_color_manual(values = viridis(length(unique(df_Height$variable)))) +
  facet_wrap(~plotID, ncol = 5) 
height

density <- ggplot(df_density, aes(x = year, y = value)) +
  geom_point(aes(fill = variable),shape= 21, alpha= 0.7, size= 2.5) +
  scale_fill_manual(values= viridis(length(unique(df_density$variable)))) +
  geom_line(aes(color = variable), alpha = 0.5) + 
  scale_color_manual(values = viridis(length(unique(df_density$variable)))) +
  facet_wrap(~plotID, ncol = 5) 
density

LAI <- ggplot(df_LAI, aes(x = year, y = value)) +
  geom_point(aes(fill = variable),shape= 21, alpha= 0.7, size= 2.5) +
  scale_fill_manual(values= viridis(length(unique(df_LAI$variable)))) +
  geom_line(aes(color = variable), alpha = 0.5) + 
  scale_color_manual(values = viridis(length(unique(df_LAI$variable)))) +
  facet_wrap(~plotID, ncol = 5) 
LAI

open <- ggplot(df_open, aes(x = year, y = value)) +
  geom_point(aes(fill = variable),shape= 21, alpha= 0.7, size= 2.5) +
  scale_fill_manual(values= viridis(length(unique(df_open$variable)))) +
  geom_line(aes(color = variable), alpha = 0.5) + 
  scale_color_manual(values = viridis(length(unique(df_open$variable)))) +
  facet_wrap(~plotID, ncol = 5) 
open

dev.off()

############################################################################
#site by year violin (boxplots)
#https://www.tutorialgateway.org/r-ggplot2-violin-plot/
dat <- read.csv("FSD_noNAplots_12192021.csv")
dat$year <- as.character(dat$year)

pdf("violinplot.pdf", width = 11, height = 8.5)

ggplot(dat, aes(x = year, y = max.canopy.ht)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen")

ggplot(dat, aes(x = year, y = q25)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = year, y = q50)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = year, y = q75)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = year, y = q100)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = year, y = deepgap.fraction)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = year, y = GFP)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = year, y = VAI)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = year, y = LAI)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = year, y = LAI_subcanopy)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = year, y = rumple)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = year, y = top.rugosity)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = year, y = vert.sd)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = year, y = vertCV)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = year, y = VCI)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = year, y = sd.sd)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = year, y = fhd)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = year, y = gini)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 


dev.off()  



###########################################################################
#% change for site by year violin (boxplots)
#https://www.tutorialgateway.org/r-ggplot2-violin-plot/
setwd("D:\\Project\\pudue\\neon\\DisturbanceStructuralDiversityEDI")
dat <- read.csv("BART_noNA_percent_change_0126_2022.csv")
dat <- dat[,-c(1, 5, 6, 7, 8)]
dat$no_years <- as.character(dat$no_years)
# dat <- dat[dat$site == "GRSM", ]


df_melt <- reshape2::melt(dat, id.var = c('siteID', 'plotID', 'year', 'no_years'))


library(ggplot2)
library("gridExtra")
library(cowplot)

#optional to transform some because too many outliers (that appear to be real disturbance data)
#using a log10 + 1
#make sure to get minimum negative % value to add appropriate transformation
min(dat[,4:26], na.rm = TRUE) #-100 #  -27.853
log10(100 + 0.01) #the zero line is 2.000043  # 2.000043

dat$q25 <- log10(100.01+dat$q25)
dat$q50 <- log10(100.01+dat$q50)
dat$deepgap.fraction <- log10(100.01+dat$deepgap.fraction)
dat$LAI <- log10(100.01+dat$LAI)
dat$LAI_subcanopy <- log10(100.01+dat$LAI_subcanopy)

# pdf("BART_percent_change_violinplot_outliers_removed_log10_2_0216.pdf", width = 11, height = 8.5)

ggplot(df_melt, aes(x = no_years, y = value)) + 
  geom_point(aes(fill = variable),shape= 21, alpha= 0.7, size= 2.5) +
  facet_wrap(~variable)


ggplot(df_melt, aes(x = no_years, y = value))+ geom_hline(aes(yintercept = 2.000043)) +
  geom_violin(fill = "black") +  facet_wrap(~variable) +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") 
# ggplot(dat, aes(x = no_years, y = max.canopy.ht)) + 
#   geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
#   stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = max.canopy.ht)) + 
  geom_violin(fill = "black") +  #facet_wrap(~) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = q25)) + geom_hline(aes(yintercept = 2.000043)) +
  geom_violin(fill = "black") +  #facet_wrap(~plotID) + 
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") 

ggplot(dat, aes(x = no_years, y = q50)) + geom_hline(aes(yintercept = 2.000043)) +
  geom_violin(fill = "black") + # facet_wrap(~plotID) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = q75)) + 
  geom_violin(fill = "black") +  facet_wrap(~plotID) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = q100)) + 
  geom_violin(fill = "black") +  facet_wrap(~plotID) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = deepgap.fraction)) + geom_hline(aes(yintercept = 2.000043)) +
  geom_violin(fill = "black") +  facet_wrap(~plotID) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = GFP)) + 
  geom_violin(fill = "black") +  facet_wrap(~plotID) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = VAI)) + 
  geom_violin(fill = "black") +  facet_wrap(~plotID) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = LAI)) + geom_hline(aes(yintercept = 2.000043)) +
  geom_violin(fill = "black") +  facet_wrap(~plotID) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = LAI_subcanopy)) + geom_hline(aes(yintercept = 2.000043)) +
  geom_violin(fill = "black") +  facet_wrap(~plotID) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, 
               size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = rumple)) + 
  geom_violin(fill = "black") +  facet_wrap(~plotID) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = top.rugosity)) + 
  geom_violin(fill = "black") +  facet_wrap(~plotID) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = vert.sd)) + 
  geom_violin(fill = "black") +  facet_wrap(~plotID) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = vertCV)) + 
  geom_violin(fill = "black") +  facet_wrap(~plotID) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = VCI)) + 
  geom_violin(fill = "black") +  facet_wrap(~plotID) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = sd.sd)) + 
  geom_violin(fill = "black") +  facet_wrap(~plotID) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = fhd)) + 
  geom_violin(fill = "black") +  facet_wrap(~plotID) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = gini)) + 
  geom_violin(fill = "black") +  facet_wrap(~plotID) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

dev.off()  


##################absolute changes
#% change for site by year violin (boxplots)
#https://www.tutorialgateway.org/r-ggplot2-violin-plot/
setwd("D:\\Project\\pudue\\neon\\DisturbanceStructuralDiversityEDI")
dat <- read.csv("FSD_noNA_absolute_change_0118_2022.csv")
dat <- dat[,-1]

dat <- absolute_change
dat$no_years <- as.character(dat$no_years)
dat.omit <- na.omit(dat)
head(dat)


library(ggplot2)
library("gridExtra")
library(cowplot)

#optional to transform some because too many outliers (that appear to be real disturbance data)
#using a log10 + 1
#make sure to get minimum negative % value to add appropriate transformation
min(dat[,8:28], na.rm = TRUE) #-100 # -27.84575
log10(27.846) #the zero line is 1.444763  # 

dat$q25 <- log10(27.846+dat$q25)
dat$q50 <- log10(27.846+dat$q50)
dat$deepgap.fraction <- log10(27.846+dat$deepgap.fraction)
dat$LAI <- log10(27.846+dat$LAI)
dat$LAI_subcanopy <- log10(27.846+dat$LAI_subcanopy)


pdf("absolute_change_violinplot_outliers_removed_log10_2_0302_2022.pdf", width = 11, height = 8.5)
dev.off()
#############site by site


######################max.canopy.ht###########################
ggplot(dat, aes(x = no_years, y = max.canopy.ht)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen")
# 
# 
# ggplot(dat, aes(x=no_years,y=max.canopy.ht, label = plotID, color=year))+
#   geom_boxplot(width=.5)+
#   # jittered text with geom_text
#   geom_text(check_overlap = TRUE,
#             position=position_jitter(width=0.15))+
#   theme(legend.position="none")


pdf("abs_changes_maxcanopyht_0118.pdf", width = 8, height = 6)

ggplot(dat, aes(x = no_years, y = max.canopy.ht, label = plotID)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 20, outlier.size = 0.5, 
               outlier.alpha = 0.5, lwd=0.1) +
  stat_summary(fun = mean, 
               geom = "point", shape = 1, size = 0.1) +
  facet_wrap(~site) + theme_grey(base_size = 6)+
  stat_summary(fun = "median", geom = "point", shape = 16, size = 0.1, color = "lightgreen")+
  geom_text(check_overlap = T, position=position_jitter(width=0.00), size= 1)+
  geom_hline(aes(yintercept = 1.444763), lwd= 0.05)

dev.off()


##################################################################
ggplot(dat, aes(x = no_years, y = q25)) + geom_hline(aes(yintercept = 1.444763)) +
  geom_violin(fill = "black") +  facet_wrap(~site) + 
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") 

ggplot(dat, aes(x = no_years, y = q50)) + geom_hline(aes(yintercept = 1.444763)) +
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = q75)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = q100)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 



######################deepgap.fraction###########################
ggplot(dat, aes(x = no_years, y = deepgap.fraction)) + geom_hline(aes(yintercept = 1.444763)) +
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") 
#geom_boxplot(width=.12, fill = "lightgreen") 


pdf("abs_changes_deepgapfraction.pdf", width = 4, height = 6)

ggplot(dat, aes(x = no_years, y = deepgap.fraction, label = plotID)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 20, outlier.size = 0.5, outlier.alpha = 0.5, lwd=0.1) +
  stat_summary(fun = mean, 
               geom = "point", shape = 1, size = 0.1) +
  facet_wrap(~site) + theme_grey(base_size = 3)+
  stat_summary(fun = "median", geom = "point", shape = 16, size = 0.1, color = "lightgreen")+
  geom_text(check_overlap = T, position=position_jitter(width=0.00), size= 0.8)+
  geom_hline(aes(yintercept = 1.444763), lwd= 0.05)

dev.off()


################################################################

ggplot(dat, aes(x = no_years, y = GFP)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = VAI)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = LAI)) + geom_hline(aes(yintercept = 1.444763)) +
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 



########################LAI_subcanopy#######################

ggplot(dat, aes(x = no_years, y = LAI_subcanopy)) + geom_hline(aes(yintercept = 1.444763)) +
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

pdf("abs_changes_LAI_subcanopy.pdf", width = 4, height = 6)

ggplot(dat, aes(x = no_years, y = LAI_subcanopy, label = plotID)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 20, outlier.size = 0.5, outlier.alpha = 0.5, lwd=0.1) +
  stat_summary(fun = mean, 
               geom = "point", shape = 1, size = 0.1) +
  facet_wrap(~site) + theme_grey(base_size = 3)+
  stat_summary(fun = "median", geom = "point", shape = 16, size = 0.1, color = "lightgreen")+
  geom_text(check_overlap = T, position=position_jitter(width=0.00), size= 0.8)+
  geom_hline(aes(yintercept = 1.444763), lwd= 0.05)

dev.off()


########################rumple#######################

ggplot(dat, aes(x = no_years, y = rumple)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

pdf("abs_changes_rumple.pdf", width = 4, height = 6)

ggplot(dat, aes(x = no_years, y = rumple, label = plotID)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 20, outlier.size = 0.5, outlier.alpha = 0.5, lwd=0.1) +
  stat_summary(fun = mean, 
               geom = "point", shape = 1, size = 0.1) +
  facet_wrap(~site) + theme_grey(base_size = 3)+
  stat_summary(fun = "median", geom = "point", shape = 16, size = 0.1, color = "lightgreen")+
  geom_text(check_overlap = T, position=position_jitter(width=0.00), size= 0.8)+
  geom_hline(aes(yintercept = 0), lwd= 0.05)

dev.off()


########################top.rugosity#######################
ggplot(dat, aes(x = no_years, y = top.rugosity)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 


pdf("abs_changes_toprugosity.pdf", width = 4, height = 6)

ggplot(dat, aes(x = no_years, y = top.rugosity, label = plotID)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 20, outlier.size = 0.5, outlier.alpha = 0.5, lwd=0.1) +
  stat_summary(fun = mean, 
               geom = "point", shape = 1, size = 0.1) +
  facet_wrap(~site) + theme_grey(base_size = 3)+
  stat_summary(fun = "median", geom = "point", shape = 16, size = 0.1, color = "lightgreen")+
  geom_text(check_overlap = T, position=position_jitter(width=0.00), size= 0.8)+
  geom_hline(aes(yintercept = 0), lwd= 0.05)

dev.off()


#########################

ggplot(dat, aes(x = no_years, y = vert.sd)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 


ggplot(dat, aes(x = no_years, y = vertCV)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = VCI)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = sd.sd)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = fhd)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

ggplot(dat, aes(x = no_years, y = gini)) + 
  geom_violin(fill = "black") +  facet_wrap(~site) + #theme_dark() +
  stat_summary(fun = "median", geom = "point", shape = 16, size = 1.5, color = "lightgreen") #geom_boxplot(width=.12, fill = "lightgreen") 

dev.off()  

