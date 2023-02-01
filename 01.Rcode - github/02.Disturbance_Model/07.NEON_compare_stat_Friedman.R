setwd("D:/Project/pudue/neon/DisturbanceStructuralDiversityEDI/Publish")

library(ggplot2)
library(ggstatsplot)
library("gridExtra")
library(cowplot)
library(viridis)
library(rstatix)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(ggprism)
library(patchwork)
library(magrittr)

#BLAN BART GRSM HARV SOAP TEAK JERC OSBS SCBI STEI/CHEQ TALL UKFS UNDE ABBY GUAN

#############################plot by plot#######################
# all <- read.csv("FSD_noNAplots_01262022.csv")
# all <- all[, -c(1, 5:7)]
# sites <- unique(all$site)

FSD_DIST_CLASS <- read.csv("./04.Data_table/disturbance_structural_diversity_data_2022_1109_den4.csv")
FSD_DIST_CLASS <- FSD_DIST_CLASS[, c(-1)]
FSD_DIST_CLASS <- FSD_DIST_CLASS[FSD_DIST_CLASS$siteID != "TEAK", ]
all <- FSD_DIST_CLASS[which(FSD_DIST_CLASS$siteID %in% c("BART", "HARV", "SCBI", "MLBS", "GRSM")), ]

# head(FSD_DIST_CLASS)
# bxp<- ggboxplot(all, x = "site", y = "den", add = "point") +
#   stat_summary(fun = mean, geom="point", shape=20, size=3, color="red", fill="red")+
#   theme_minimal()+
#   geom_hline(aes(yintercept = 3))
# 
# bxp

ggplot(all, aes(x = siteID, y = den, fill = factor(year))) +
  geom_violin() +
  # geom_dotplot(binaxis = 'y', stackdir = 'center',
  #              position = position_dodge())+
  # stat_summary(fun = mean, 
  #              geom = "point", 
  #              shape = 1, 
  #              size = 1) +
  # stat_summary(fun = "median", 
  #              geom = "point", 
  #              shape = 16, 
  #              size = 1, 
  #              color = "lightgreen")+
# geom_text(check_overlap = T, 
#           position=position_jitter(width=0.00), 
#           size= 2)+
theme_minimal()+
  geom_hline(aes(yintercept = 7), colour = "black", lwd= 0.05)+
  geom_hline(aes(yintercept = 4), colour = "blue", lwd= 0.05) + 
  geom_hline(aes(yintercept = 2), colour = "red", lwd= 0.05)


###########################################metrics#######################
# years_metrics <- list()
# metrics <- list()
# a <- data.frame()
# 
# 
# 
# ## Done by 1/26/2022
# for (i in seq_along(sites)){
#  site_name <- all[all$site == paste0(sites[i]),]
#  years <- unique(site_name$year)
#  metrics[[i]] <- matrix(nrow = c(dim(site_name)[1], ncol = 1))
# 
#  for (n in seq_along(years)){
#    years_metrics[[n]] <- site_name[site_name$year == paste0(years[n]),]
#    names(years_metrics[[n]]) <- paste0(names(years_metrics[[n]]), "_",paste0(years[n]))
#    metrics[[i]] <- cbind(metrics[[i]], years_metrics[[n]])
#  }
#  metrics[[i]]<- metrics[[i]][,-1]
#  write.csv(metrics[[i]], paste0(sites[i], "_", "fsd_yby.csv"))
# }
# 
#########################################
###################Friedman_test##########################
all <- read.csv("./04.Data_table/disturbance_structural_diversity_data_2022_1109_den4.csv")
all <- all[, -c(1, 5:7)]
sites <- unique(all$site)

sites <- c("BART", "HARV", "SCBI", "MLBS", "GRSM")

variables <- colnames(all)[5:25]
variables <- variables[c(4, 3, 20, 21, 6, 19)]

bxp <- vector("list")

#############################################

dat <- read.csv("./04.Data_table/disturbance_structural_diversity_data_2022_1109_den4.csv")
# all <- all[, -c(1, 5:7)]

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
write.csv(OUT.KEEP, "FSD_noNAplots_2022.csv")

all <- read.csv("FSD_noNAplots_2022.csv")
all <- all[, -c(1, 5:7)]
sites <- unique(all$site)

sites <- c("BART", "HARV", "SCBI", "MLBS", "GRSM")

variables <- colnames(all)[5:25]
variables <- variables[c(4, 3, 20, 21, 6, 19)]




all_model <- c("BBD_outputs_2022.RData", "HWA_outputs_2022.RData", "EAB_outputs_2022.RData", 
               "CAK_outputs_2022.RData", "LYD_outputs_2022.RData", "FIRE_outputs_2022.RData")

for (i in seq_along(all_model)) {
  load(all_model[i])
}

all_models <- list(BBD_outputs, HWA_outputs, EAB_outputs, CAK_outputs, LYD_outputs, FIRE_outputs)

BBD_plot <- unique(BBD_outputs[[1]]$data$plotID)
HWA_plot <- unique(HWA_outputs[[1]]$data$plotID)
EAB_plot <- unique(EAB_outputs[[1]]$data$plotID)
CAK_plot <- unique(CAK_outputs[[1]]$data$plotID)
SPM_plot <- unique(LYD_outputs[[1]]$data$plotID)
FIRE_plot <- unique(FIRE_outputs[[1]]$data$plotID)

all_plots <- c(BBD_plot, HWA_plot, EAB_plot, CAK_plot, SPM_plot, FIRE_plot)
all <- all[which(all$plotID %in% all_plots), ]
all


disturbance_tables <- rbind(BBD_outputs[[1]]$data, HWA_outputs[[1]]$data[,-31], EAB_outputs[[1]]$data,
                            CAK_outputs[[1]]$data, LYD_outputs[[1]]$data[,-c(31, 32)], FIRE_outputs[[1]]$data[,-31])
colnames(disturbance_tables)

disturbance_tables <- disturbance_tables[,c(1, 2, 27, 29)] %>%
  distinct()

# stats
summary(disturbance_tables[disturbance_tables$siteID =="BART", ])
sd(disturbance_tables[disturbance_tables$siteID =="BART", ]$Disturbed_BA_ratio)

summary(disturbance_tables[disturbance_tables$siteID =="SCBI", ])
sd(disturbance_tables[disturbance_tables$siteID =="SCBI", ]$Disturbed_BA_ratio)

summary(disturbance_tables[disturbance_tables$disturbed =="HWA", ])
sd(disturbance_tables[disturbance_tables$disturbed =="HWA", ]$Disturbed_BA_ratio)

summary(disturbance_tables[disturbance_tables$disturbed =="LYD", ])
sd(disturbance_tables[disturbance_tables$disturbed =="LYD", ]$Disturbed_BA_ratio)

summary(disturbance_tables[disturbance_tables$disturbed =="Canker", ])
sd(disturbance_tables[disturbance_tables$disturbed =="Canker", ]$Disturbed_BA_ratio)

summary(disturbance_tables[disturbance_tables$disturbed =="fire", ])
sd(disturbance_tables[disturbance_tables$disturbed =="fire", ]$Disturbed_BA_ratio)

#

disturbed <- c("BBD", "HWA", "EAB", "Canker", "LYD", "fire")

all.dist <- merge(all, disturbance_tables, by = c("siteID", "plotID"), all.x = T)

for (i in seq_along(disturbed)){
  NEON <- all.dist[all.dist$disturbed  == paste0(disturbed[i]), ]
  NEON <- NEON %>%
    convert_as_factor(year, plotID)
  # print(head(NEON, 1))
  bxp <- vector("list")
  NEON <- na.omit(NEON)
  jpeg(paste0(disturbed[i], "_", "FRIEDMAN_test_2022.jpeg"), width = 20, height = 4, units = 'in', res = 300)
  
  for (n in seq_along(variables)){
    variable <- noquote(variables[n])
    res.fried <- NEON %>%
      friedman_test(as.formula(paste0(variable," ~ year|plotID")))
    # get_anova_table(res.aov)
    pwc <- NEON %>%
      wilcox_test(as.formula(paste0(variable," ~ year")), paired = TRUE,
                  p.adjust.method = "bonferroni")
    
    pwc <- pwc %>% add_xy_position(x = "year")
    
    bxp[[n]] <- ggboxplot(NEON, x = "year", y = paste(variables[n]), add = "point") +
      # stat_pvalue_manual(pwc, size = 3, tip.length = 0.01, step.increase = 0.1, hide.ns = T) +
      stat_pvalue_manual(pwc, size = 1, tip.length = 0.001, step.increase = 0.01, hide.ns = T) +
      # stat_compare_means(pwc, label = "p.adj.signif", ref.group = "0.5")+
      labs(subtitle = get_test_label(res.fried, detailed = T),
           caption = get_pwc_label(pwc))+
      font("subtitle", size = 8)+
      font("caption", size = 7)+
      # stat_summary(fun.y=mean, geom="point", shape=20, size=3, color="red", fill="red")+
      stat_summary(fun = median, colour="red", geom="text", show.legend = FALSE, 
                   vjust=-0.7, aes( label=round(..y.., digits=2)))
  }
  do.call("grid.arrange", c(bxp, ncol = 6))
  dev.off()
}