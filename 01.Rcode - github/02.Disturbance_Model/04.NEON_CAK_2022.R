lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(
    paste0('package:', pkgs),
    character.only = T,
    unload = T,
    force = T
  ))

rm(list = ls())

###library##
library("ggplot2")
library('viridis')
library('dplyr')
library('raster')
library("reshape2")
library("tidyverse")
library("gridExtra")
library("dotwhisker")
library("nlme")
library("gpboost")
library("sjPlot")
library("lattice")
library("ggpubr")
library("nlme")
library("sjPlot")
library("car")
library("performance")
library("effects")
library("scales")


setwd("D:/Project/pudue/neon/DisturbanceStructuralDiversityEDI/Publish")
################################################################################
################################################################################
dat <- read.csv("./04.Data_table/disturbance_structural_diversity_data_2022_1109_den4.csv") # r
dat <- dat[,c(-1)]
head(dat)

sites <- unique(dat$siteID)
########## Keep data only data has the first year acquisition ##################
CAK_plots <- unique(dat$plotID[which(dat$year == 2015 & dat $ siteID == "MLBS")])

dat <- cbind(dat, dat$year)
dat <- dat[which(dat$plotID %in% CAK_plots),]
dat <- cbind(dat, dat$year)
plots <- unique(dat$plotID)

absolute_change <- NULL
percent_change <- NULL
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
  abs_change_i <- future[,7:29] - year1rep[,7:29]
  abs_change_i <- cbind(future[,1:6], abs_change_i)
  absolute_change <- rbind(absolute_change, abs_change_i)
  
  #% change will be x future year - first year of data / first year of data
  # positive will be an increase and negative will be a decrease relative to original %
  percent_change_i <- (abs_change_i[,7:28] / year1rep[,7:28]) * 100
  percent_change_i <- cbind(future[,1:6], percent_change_i, abs_change_i[29])
  percent_change <- rbind(percent_change, percent_change_i)
  
} #end of loop

colnames(absolute_change)[29] <- "TIME"
colnames(percent_change)[29] <- "TIME"


MLBS_LiDAR <- dat[dat$siteID == "MLBS", ]
plots <- unique(dat$plotID)

########################### set the first year #################################
first_year <- data.frame()
for(i in 1:length(plots)){
  dat.i <- dat[dat$plotID == plots[i], ]  
  start_year <- min(dat.i$year)
  #first year of data
  year1 <- dat.i[dat.i$year == start_year, ]
  first_year <- rbind(first_year, year1)
} #end of loop

year_0 <- first_year
year_0[c(6:29)] <- 0
colnames(year_0)[29] <-"TIME"

colnames(first_year)[c(3, 6:28)] <- paste0(colnames(first_year)[c(3, 6:28)], "_", "0")
first_year <- first_year[, c(1:3, 6:28)]

################################################################################
################################################################################

tree_points <- read.csv("tree_rm_duplicates.csv")
BA_volume <- tree_points

MLBS <- BA_volume[BA_volume$siteID == "MLBS", ]

################################################################################
MLBS$BA <- replace_na(MLBS$BA, 0)
MLBS$Volume <- replace_na(MLBS$Volume, 0)

MLBS_BA_0 <- MLBS[which(MLBS$BA == 0), ]
MLBS_BA_0_treeid <- unique(MLBS_BA_0$idtID)
MLBS_BA_0_treeid

# BA = 0, including earlier and later
MLBS_BA_0
MLBS_BA_0_treeid

#BA != 0 including earlier and later
MLBS_BA_X0 <- MLBS[which(MLBS$BA != 0), ]
MLBS_BA_X0_treeid <- unique(MLBS_BA_X0$idtID)

head(MLBS_BA_X0)
# all year BA ===0
`%ni%` <- Negate(`%in%`)

MLBS_BA_0_entire <- subset(MLBS, (idtID %in% MLBS_BA_0_treeid) & (idtID %ni% MLBS_BA_X0_treeid))
sum(MLBS_BA_0_entire$BA)

MLBS_BA_x_entire <- subset(MLBS, (idtID %ni% MLBS_BA_0_treeid) & (idtID %in% MLBS_BA_X0_treeid))
sum(MLBS_BA_x_entire$BA)

# BA != 0 but later BA = 0 or opposite
MLBS_BA_X0 <- subset(MLBS, (idtID %in% MLBS_BA_0_treeid) & (idtID %in% MLBS_BA_X0_treeid))

colnames(MLBS_BA_X0)
MLBS_BA_X0 <- MLBS_BA_X0%>%
  group_by(domainID, siteID, plotID, subplotID, nestplotID, idtID) %>%
  mutate(DBH = max(DBH),
         height = max(height),
         Volume = max(Volume),
         BA = max(BA)) %>%
  distinct()


MLBS_all <- rbind(MLBS_BA_0_entire, MLBS_BA_x_entire, MLBS_BA_X0)
colnames(MLBS_all)

MLBS_all <- MLBS_all[, -c(20)]
colnames(MLBS_all)

################################################################################
BA_volume <- MLBS_all

CAK <- BA_volume %>%
  mutate(month = substr(format(BA_volume$date), 6,7), year = substr(format(BA_volume$date), 1,4)) %>%
  filter(DBH > 5) %>%
  group_by(siteID, plotID, year) %>%
  mutate(total_BA = sum(BA, na.rm = T),
         total_Volume = sum(Volume, na.rm = T),
         richness = length(unique(species)),
         meanELV = mean(elv)) %>%
  filter(grepl('Quercus', species)) %>%
  mutate(disturbed = "Canker") %>%
  group_by(siteID, plotID, year, disturbed) %>%
  summarize(Disturbed_volume_ratio = sum(Volume, na.rm = T)/total_Volume * 100,
            Disturbed_BA_ratio = sum(BA, na.rm = T)/total_BA * 100,
            richness = richness,
            meanELV = 0) %>%
  distinct()


CAK <- CAK %>%
  group_by(siteID, plotID, disturbed) %>%
  summarize(Disturbed_volume_ratio = mean(Disturbed_volume_ratio),
            Disturbed_BA_ratio = mean(Disturbed_BA_ratio),
            richness = mean(richness))%>%
  distinct()
colnames(CAK)

CAK_plots <- unique(CAK$plotID)
CAK_plots

CAK_ground <- CAK
#################### selecting LiDAR metrics _ ABS changes #####################

CAK_test_abs <- merge(absolute_change, CAK_ground, by = c("siteID", "plotID"), all.y = T)

################### selecting LiDAR metrics _ no changes #######################
MLBS_LiDAR <- dat[dat$siteID == "MLBS", ]
CAK_test_abs_1 <- merge(CAK_test_abs, first_year, by = c("siteID", "plotID"), all.x = T)
################################################################################
CAK_test_1 <- CAK_test_abs_1 %>%
  filter(!is.na(plotID_year)) %>%
  distinct()


CAK_test_1 <- CAK_test_1[CAK_test_1$siteID == "MLBS", ]
CAK_test_1 <- distinct(CAK_test_1)

################################################################################
colnames(CAK_test_1)
CAK_test_1 <- CAK_test_1[, c(1:3, 7:57)]

CAK_test_scaled <- CAK_test_1
CAK_test_scaled[,c(4:25, 34:54)]  <- CAK_test_scaled[,c(4:25,34:54)]  %>%
  mutate(across(where(is.numeric), ~ scale(.)))

CAK_test_1[,c(34:54)]  <- CAK_test_1[,c(34:54)]  %>%
  mutate(across(where(is.numeric), ~ as.numeric(scale(.))))
################################################################################
################################################################################
##############################     mixed      ##################################
################################################################################
################################################################################
CAK_test_scaled <- CAK_test_scaled %>%
  arrange(plotID, year)

Height <- c("mean.max.canopy.ht")
Density <- c("VAI")
Openness <- c("deepgap.fraction")
ExComplex <- c("top.rugosity")
InComplex <- c("VCI")

variable <- "mean.max.canopy.ht"
variable_0 <- paste0(variable, "_0")
variable_0

Disturb_form_1 <- as.formula(paste0(variable, "~ (year)|plotID"))
Disturb_form_2 <- as.formula(paste0(variable, "~ (year)"))

xyplot(Disturb_form_1,
       ylab= paste0("Changes in ", variable), xlab = "Time (Year)", group=plotID,type=c("p","l"), 
       data = CAK_test_scaled, lwd = 2, 
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=0)})

xyplot(Disturb_form_2,
       ylab= paste0("Changes in ", variable), xlab = "Time (Year)", group=plotID,type=c("p","l"), data= CAK_test_scaled)


######## delete outliers
CAK_test_2 <- CAK_test_1
CAK_test_2 <- CAK_test_2 %>%
  arrange(plotID, TIME)

Disturb_form_1 <- as.formula(paste0(variable, "~ (TIME)|plotID"))
Disturb_form_2 <- as.formula(paste0(variable, "~ (TIME)"))

xyplot(Disturb_form_1,
       ylab= paste0("Changes in ", variable), xlab = "Time (Year)", group=plotID,type=c("p","l"), 
       data = CAK_test_2, lwd = 2, 
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=0)})

xyplot(Disturb_form_2,
       ylab= paste0("Changes in ", variable), xlab = "Time (Year)", group=plotID,type=c("p","l"), data= CAK_test_2)

############## stats ##################
hist(sqrt(CAK_test_2$Disturbed_BA_ratio))
mean(CAK_test_2$Disturbed_BA_ratio)

################# tranformation
CAK_test_2$Severity <- sqrt(CAK_test_2$Disturbed_BA_ratio)

################################################################################
################################################################################
################################################################################
################################################################################
CAK_test_2 %>% 
  ggplot(aes(x=TIME, y=mean.max.canopy.ht)) +
  geom_smooth(aes(group=Severity, colour= factor(Severity)))

mixed_model3.canopy.ht = lme(fixed= mean.max.canopy.ht ~ 
                               Severity:TIME + top.rugosity_0:TIME + Severity + top.rugosity_0 + poly(TIME, 2,raw = TRUE), 
                             random = ~ TIME|plotID,
                             method = "REML",
                             correlation=corAR1(-0.1, form= ~ TIME|plotID),
                             control=list(maxIter=10000, niterEM=10000, opt="optim"),
                             data= CAK_test_2)
# grid.arrange(plot(mixed_model3.gfp,type=c("p","smooth")),
#              plot(mixed_model3.gfp,sqrt(abs(resid(.)))~fitted(.),
#                   type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
#              ## "sqrt(abs(resid(x)))"),
#              plot(mixed_model3.gfp,resid(.,type="pearson")~TIME,
#                   type=c("p","smooth")),
#              qqnorm(mixed_model3.gfp,abline=c(0,1)))

mixed_model3.gfp = lme(fixed= deepgap.fraction ~ 
                         Severity:TIME + top.rugosity_0:TIME + Severity + top.rugosity_0 + poly(TIME, 2,raw = TRUE), 
                       random = ~ TIME|plotID,
                       method = "REML",
                       correlation=corAR1(0, form= ~ TIME|plotID),
                       control=list(maxIter=10000, niterEM=10000, opt="optim"),
                       data= CAK_test_2)

mixed_model3.vai = lme(fixed= LAI ~ 
                         Severity:TIME + top.rugosity_0:TIME + Severity  + top.rugosity_0 + poly(TIME, 2,raw = TRUE), 
                       random = ~ TIME|plotID,
                       method = "REML",
                       correlation=corAR1(0, form= ~ TIME|plotID),
                       control=list(maxIter=10000, niterEM=10000, opt="optim"),
                       data= CAK_test_2)

mixed_model3.lai_sub = lme(fixed= LAI_subcanopy ~ 
                         Severity:TIME + top.rugosity_0:TIME + Severity  + top.rugosity_0 + poly(TIME, 2,raw = TRUE), 
                       random = ~ TIME|plotID,
                       method = "REML",
                       # correlation=corAR1(0, form= ~ TIME|plotID),
                       control=list(maxIter=10000, niterEM=10000, opt="optim"),
                       data= CAK_test_2)

mixed_model3.tr = lme(fixed= top.rugosity ~  
                        Severity:TIME + top.rugosity_0:TIME + Severity  + top.rugosity_0 + poly(TIME, 2,raw = TRUE), 
                      random = ~ TIME|plotID,
                      method = "REML",
                      correlation=corAR1(0, form= ~ TIME|plotID),
                      control=list(maxIter=10000, niterEM=10000, opt="optim"),
                      data= CAK_test_2)

mixed_model3.gini = lme(fixed= gini ~  
                         Severity:TIME + top.rugosity_0:TIME + Severity  + top.rugosity_0 + poly(TIME, 2,raw = TRUE), 
                       random = ~ TIME|plotID,
                       method = "REML",
                       correlation=corAR1(0, form= ~ TIME|plotID),
                       control=list(maxIter=10000, niterEM=10000, opt="optim"),
                       data= CAK_test_2)

tab_model(mixed_model3.canopy.ht, mixed_model3.gfp, mixed_model3.vai, mixed_model3.lai_sub, mixed_model3.tr, mixed_model3.gini, digits = 5)

################################################################################
################################################################################
################################################################################
model_outputs <- list(mixed_model3.canopy.ht, mixed_model3.gfp, mixed_model3.vai, mixed_model3.lai_sub, mixed_model3.tr, mixed_model3.gini)

CAK_outputs <- list(mixed_model3.canopy.ht, mixed_model3.gfp, mixed_model3.vai, mixed_model3.lai_sub, mixed_model3.tr, mixed_model3.gini)
save(CAK_outputs, file = "CAK_outputs_2023.RData")


names(model_outputs) <- c("mean.max.canopy.ht", "deepgap.fraction", "LAI", "LAI_sub", "top.rugosity", "gini")
################################################################################
variable <- c("mean.max.canopy.ht", "deepgap.fraction", "LAI", "LAI_sub", "top.rugosity", "gini")
################################################################################
for (i in seq_along(variable)){
  model_sumamry  <- summary(model_outputs[[i]])
  model_table <- as.data.frame(model_sumamry$tTable)
  model_table <- tibble::rownames_to_column(model_table, "Term")
  # model_table
  
  variable_i <- variable[i]
  SLevels <- c(mean(CAK_test_2$Severity)-sd(CAK_test_2$Severity),
               mean(CAK_test_2$Severity),
               mean(CAK_test_2$Severity)+sd(CAK_test_2$Severity))
  
  Results.Model_sev<-Effect(c("TIME","Severity"), model_outputs[[i]],
                            xlevels=list(TIME= unique(CAK_test_2$TIME),
                                         Severity=SLevels))
  
  Results.Model_sev <- as.data.frame(Results.Model_sev)
  
  
  Results.Model_sev$Support.F<-factor(Results.Model_sev$Severity,
                                      levels=SLevels,
                                      labels=c("Low", "Moderate", "Severe"))
  
  
  if (model_table$`p-value`[model_table$Term == "Severity:TIME"] < 0.1) {
    a <- model_table$`p-value`[model_table$Term == "Severity:TIME"]
    Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev,
                                aes(x = TIME, y =fit, group= Support.F))+
      geom_line(size=2, aes(color=Support.F))+
      # facet_wrap(~pc) +
      geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=Support.F, fill=Support.F), alpha=.2)+
      scale_colour_manual(values = c("blue", "grey", "red")) +
      scale_fill_manual(values = c("blue", "grey", "red")) +
      xlab("TIME")+
      ylab(paste0("Changes in ", variable_i))+ theme_bw()+
      # geom_hline(yintercept = c(unique(CAK_test_2[, variable][which(CAK_test_2$TIME ==3)])),
      #            col = c("black")) +
      geom_hline(yintercept = 0,
                 col = c("black")) +
      theme(legend.title=element_blank(),
            axis.ticks.length = unit(5, "pt"),
            legend.position = "none",
            axis.title.x = element_blank(),
            panel.background = element_rect(
              # fill = "black",
              colour = "black",
              size = 4
            )) +
      scale_y_continuous(labels = label_number(accuracy = 0.001)) +
      annotate("text", x = mean(CAK_test_2$TIME) * 1.3, y = mean(CAK_test_2[, variable_i]) / 10, label = paste0("p = ", round(a, 2)), size = 7)
    print(Final.Fixed.Plot.1)
    
  } else {
    Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev,
                                aes(x = TIME, y =fit, group= Support.F))+
      geom_line(size=2, aes(color=Support.F))+
      # facet_wrap(~pc) +
      geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=Support.F, fill=Support.F), alpha=.2)+
      scale_colour_manual(values = c("blue", "grey", "red")) +
      scale_fill_manual(values = c("blue", "grey", "red")) +
      xlab("TIME")+
      ylab(paste0("Changes in ", variable_i))+ theme_bw()+
      # geom_hline(yintercept = c(unique(CAK_test_2[, variable][which(CAK_test_2$TIME ==3)])),
      #            col = c("black")) +
      geom_hline(yintercept = 0,
                 col = c("black")) +
      scale_y_continuous(labels = label_number(accuracy = 0.001)) +
      theme(legend.position = "none",
            legend.title=element_blank(),
            axis.title.x = element_blank())
    print(Final.Fixed.Plot.1)
  }
  # jpeg(paste0(variable_i,"_BBD_severity_1109_Den", ".jpg"))
  plot(Final.Fixed.Plot.1)
  # dev.off()
}
