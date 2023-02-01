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
unique(dat$plotID[which(dat$siteID == "GRSM")])
GRSM_plots <- unique(dat$plotID[which(dat$year == 2015 & dat $ siteID == "GRSM")])

dat <- cbind(dat, dat$year)
dat <- dat[which(dat$plotID %in% GRSM_plots),]

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

GRSM_LiDAR <- dat[dat$siteID == "GRSM", ]
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
colnames(absolute_change)

# absolute_change <- rbind(absolute_change, year_0) %>%
#   arrange(plotID, year)

colnames(first_year)[c(3, 6:28)] <- paste0(colnames(first_year)[c(3, 6:28)], "_", "0")
first_year <- first_year[, c(1:3, 6:28)]


################################################################################
################################################################################

tree_points <- read.csv("tree_rm_duplicates.csv")
BA_volume <- tree_points

#################################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################

GRSM <- BA_volume[BA_volume$siteID == "GRSM", ]

################################################################################
################################################################################
GRSM$BA <- replace_na(GRSM$BA, 0)
GRSM$Volume <- replace_na(GRSM$Volume, 0)

GRSM_BA_0 <- GRSM[which(GRSM$BA == 0), ]
GRSM_BA_0_treeid <- unique(GRSM_BA_0$idtID)
GRSM_BA_0_treeid


# BA = 0, including earlier and later
GRSM_BA_0
GRSM_BA_0_treeid

#BA != 0 including earlier and later
GRSM_BA_X0 <- GRSM[which(GRSM$BA != 0), ]
GRSM_BA_X0_treeid <- unique(GRSM_BA_X0$idtID)

# all year BA ===0
`%ni%` <- Negate(`%in%`)

GRSM_BA_0_entire <- subset(GRSM, (idtID %in% GRSM_BA_0_treeid) & (idtID %ni% GRSM_BA_X0_treeid))
sum(GRSM_BA_0_entire$BA)

GRSM_BA_x_entire <- subset(GRSM, (idtID %ni% GRSM_BA_0_treeid) & (idtID %in% GRSM_BA_X0_treeid))
sum(GRSM_BA_x_entire$BA)

# BA != 0 but later BA = 0 or opposite
GRSM_BA_X0 <- subset(GRSM, (idtID %in% GRSM_BA_0_treeid) & (idtID %in% GRSM_BA_X0_treeid))

colnames(GRSM_BA_X0)
GRSM_BA_X0 <- GRSM_BA_X0%>%
  group_by(domainID, siteID, plotID, subplotID, nestplotID, idtID) %>%
  mutate(DBH = max(DBH),
         height = max(height),
         Volume = max(Volume),
         BA = max(BA)) %>%
  distinct()


GRSM_all <- rbind(GRSM_BA_0_entire, GRSM_BA_x_entire, GRSM_BA_X0)
colnames(GRSM_all)

GRSM_all <- GRSM_all[, -c(20)]
colnames(GRSM_all)

################################################################################
unique(GRSM_all$plotID)
################################# BA_ratio #####################################
BA_volume <- GRSM_all
BA_volume <- BA_volume[which(GRSM_all$siteID == "GRSM"), ]

GRSM_FIRE_1 <- BA_volume %>%
  mutate(month = substr(format(BA_volume$date), 6,7), year = substr(format(BA_volume$date), 1,4)) %>%
  filter(DBH > 5) %>%
  group_by(siteID, plotID, year) %>%
  mutate(total_BA = sum(BA, na.rm = T),
         total_Volume = sum(Volume, na.rm = T),
         richness = length(unique(species)),
         meanELV = mean(elv)) %>%
  filter(grepl('fire|Fire|burned|Burned|Scotch|scotch|burn', remarks)|
           grepl('fire|Fire|burned|Burned|Scotch|scotch|burn', plantStatus)) %>%
  mutate(disturbed = "fire") %>%
  group_by(siteID, plotID, year, disturbed) %>%
  summarize(Disturbed_volume_ratio = sum(Volume, na.rm = T)/total_Volume * 100,
            Disturbed_BA_ratio = sum(BA, na.rm = T)/total_BA * 100,
            richness = richness,
            meanELV = meanELV) %>%
  distinct()


GRSM_FIRE_1 <- GRSM_FIRE_1 %>%
  group_by(siteID, plotID, disturbed) %>%
  summarize(Disturbed_volume_ratio = max(Disturbed_volume_ratio),
            Disturbed_BA_ratio = max(Disturbed_BA_ratio),
            richness = max(richness),
            meanELV = meanELV) %>%
  distinct()
colnames(GRSM_FIRE_1)


FIRE_ground <- GRSM_FIRE_1

#################### selecting LiDAR metrics _ ABS changes #####################

FIRE_test_abs <- merge(absolute_change, FIRE_ground, by = c("siteID" ,"plotID"), all.y = T)
GRSM_LiDAR <- dat[dat$siteID == "GRSM", ]
plots <- unique(dat$plotID)
FIRE_test_abs_1 <- merge(FIRE_test_abs, first_year, by = c("siteID", "plotID"), all.x = T)

################################################################################
FIRE_test_1 <- FIRE_test_abs_1 %>%
  filter(!is.na(plotID_year))

FIRE_test_1 <- FIRE_test_1[FIRE_test_1$siteID == "GRSM", ]
FIRE_test_1 <- distinct(FIRE_test_1)

################################################################################
colnames(FIRE_test_1)
FIRE_test_1 <- FIRE_test_1[, c(1:3, 7:58)]
FIRE_test_scaled <- FIRE_test_1

colnames(FIRE_test_scaled)
FIRE_test_scaled[,c(4:25, 34:55)]  <- FIRE_test_scaled[,c(4:25,34:55)]  %>%
  mutate(across(where(is.numeric), ~ scale(.)))
summary(FIRE_test_scaled)


colnames(FIRE_test_1[ , c(34:55)])
FIRE_test_1[ , c(34:55)] <- FIRE_test_1[ , c(34:55)]  %>%
  mutate(across(where(is.numeric), ~ as.numeric(scale(.))))

############## stats ##################
hist((FIRE_test_1$Disturbed_BA_ratio))
hist(log(FIRE_test_1$Disturbed_BA_ratio))
mean(FIRE_test_1$Disturbed_BA_ratio)
sd(FIRE_test_1$Disturbed_BA_ratio)

# Severity <- read.csv("GRSM_severity.csv")
# FIRE_test_2 <- merge(FIRE_test_2, Severity, by = "plotID")

################# tranformation
FIRE_test_1$Severity <- log(FIRE_test_1$Disturbed_BA_ratio)
hist(FIRE_test_2$Severity)

################################################################################
################################################################################
FIRE_test_scaled <- FIRE_test_scaled %>%
  arrange(plotID, TIME)


Height <- c("mean.max.canopy.ht")
Density <- c("VAI")
Openness <- c("deepgap.fraction")
ExComplex <- c("top.rugosity")
InComplex <- c("VCI")

variable <- "mean.max.canopy.ht"
variable_0 <- paste0(variable, "_0")
variable_0

Disturb_form_1 <- as.formula(paste0(variable, "~ (TIME)|plotID"))
Disturb_form_2 <- as.formula(paste0(variable, "~ (TIME)"))

xyplot(Disturb_form_1,
       ylab= paste0("Changes in ", variable), xlab = "Time (Year)", group=plotID,type=c("p","l"), 
       data = FIRE_test_scaled, lwd = 2, 
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=0)})

xyplot(Disturb_form_2,
       ylab= paste0("Changes in ", variable), xlab = "Time (Year)", group=plotID,type=c("p","l"), data= FIRE_test_scaled)


######## delete outliers #########
FIRE_test_2 <- FIRE_test_1 %>%
  arrange(plotID, TIME)

xyplot(Disturb_form_1,
       ylab= paste0("Changes in ", variable), xlab = "Time (Year)", group=plotID,type=c("p","l"), 
       data = FIRE_test_2, lwd = 2, 
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=0)})

xyplot(Disturb_form_2,
       ylab= paste0("Changes in ", variable), xlab = "Time (Year)", group=plotID,type=c("p","l"), data= FIRE_test_2)
################################################################################
################################################################################
################################################################################
################################################################################
FIRE_test_2 %>% 
  ggplot(aes(x=TIME, y=mean.max.canopy.ht)) +
  geom_smooth(aes(group=Severity, colour=Severity))


mixed_model3.canopy.ht = lme(fixed= mean.max.canopy.ht ~ 
                               Severity:TIME + top.rugosity_0:TIME + Severity + top.rugosity_0 + poly(TIME, 3,raw = TRUE), 
                             random = ~ TIME|plotID,
                             method = "REML",
                             correlation=corAR1(0, form= ~ TIME|plotID),
                             control=list(maxIter=10000, niterEM=10000, opt="optim"),
                             data= FIRE_test_2)

mixed_model3.gfp = lme(fixed= deepgap.fraction ~ 
                         Severity:TIME + top.rugosity_0:TIME + Severity + top.rugosity_0 + poly(TIME, 3,raw = TRUE), 
                       random = ~ TIME|plotID,
                       method = "REML",
                       correlation=corAR1(0, form= ~ TIME|plotID),
                       control=list(maxIter=10000, niterEM=10000, opt="optim"),
                       data= FIRE_test_2)

mixed_model3.vai = lme(fixed= LAI ~ 
                         Severity:TIME + top.rugosity_0:TIME + Severity + top.rugosity_0 + poly(TIME, 3,raw = TRUE), 
                       random = ~ TIME|plotID,
                       method = "REML",
                       correlation=corAR1(0, form= ~ TIME|plotID),
                       control=list(maxIter=10000, niterEM=10000, opt="optim"),
                       data= FIRE_test_2)


mixed_model3.lai_sub = lme(fixed= LAI_subcanopy ~ 
                             Severity:TIME + top.rugosity_0:TIME + Severity + top.rugosity_0 + poly(TIME, 3,raw = TRUE), 
                           random = ~ TIME|plotID,
                           method = "REML",
                           correlation=corAR1(0, form= ~ TIME|plotID),
                           control=list(maxIter=10000, niterEM=10000, opt="optim"),
                           data= FIRE_test_2)


mixed_model3.tr = lme(fixed= top.rugosity ~ 
                        Severity:TIME + top.rugosity_0:TIME + Severity + top.rugosity_0 + poly(TIME, 3,raw = TRUE), 
                      random = ~ TIME|plotID,
                      method = "REML",
                      correlation=corAR1(0, form= ~ TIME|plotID),
                      control=list(maxIter=10000, niterEM=10000, opt="optim"),
                      data= FIRE_test_2)


mixed_model3.gini = lme(fixed= gini ~ 
                         Severity:TIME + top.rugosity_0:TIME + Severity + top.rugosity_0 + poly(TIME, 3,raw = TRUE), 
                       random = ~ TIME|plotID,
                       method = "REML",
                       # correlation=corAR1(0, form= ~ TIME|plotID),
                       control=list(maxIter=10000, niterEM=10000, opt="optim"),
                       data= FIRE_test_2)

tab_model(mixed_model3.canopy.ht, mixed_model3.gfp, mixed_model3.vai,mixed_model3.lai_sub, mixed_model3.tr, mixed_model3.gini)

################################################################################
model_outputs <- list(mixed_model3.canopy.ht, mixed_model3.gfp, mixed_model3.vai, mixed_model3.lai_sub, mixed_model3.tr, mixed_model3.gini)
FIRE_outputs <- list(mixed_model3.canopy.ht, mixed_model3.gfp, mixed_model3.vai, mixed_model3.lai_sub, mixed_model3.tr, mixed_model3.gini)
save(FIRE_outputs, file = "FIRE_outputs_2023.RData")

names(model_outputs) <- c("mean.max.canopy.ht", "deepgap.fraction", "LAI", "LAI_sub","top.rugosity", "gini")
################################################################################
variable <- c("mean.max.canopy.ht", "deepgap.fraction", "LAI", "LAI_sub","top.rugosity", "gini")
################################################################################
for (i in seq_along(variable)) {
  variable_i <- variable[i]
  
  Results.Model_sev<-Effect(c("TIME"), model_outputs[[i]],
                            xlevels=list(TIME= unique(FIRE_test_2$TIME)))
  
  Results.Model_sev <- as.data.frame(Results.Model_sev)
  
  
  Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, 
                              aes(x = TIME, y =fit))+
    geom_line(size=2)+
    # facet_wrap(~pc) + 
    geom_ribbon(aes(ymin= fit-se, ymax= fit+se), alpha=.2)+
    scale_colour_manual(values = c("blue", "grey", "red")) + 
    scale_fill_manual(values = c("blue", "grey", "red")) + 
    xlab("TIME")+
    ylab(paste0("Changes in ", variable_i))+ theme_bw()+
    # geom_hline(yintercept = c(unique(FIRE_test_2[, variable][which(FIRE_test_2$TIME ==3)])), 
    #            col = c("black")) +
    geom_hline(yintercept = 0,
               col = c("black")) +
    theme(legend.position = "top", 
          legend.title=element_blank())
  print(Final.Fixed.Plot.1)
  # jpeg(paste0(variable_i,"_FIRE_TIME_0614", ".jpg"))
  plot(Final.Fixed.Plot.1)
  # dev.off()  
}



for (i in seq_along(variable)){
  variable_i <- variable[i]
  
  SLevels <- c(mean(FIRE_test_2$Severity)-sd(FIRE_test_2$Severity),
               mean(FIRE_test_2$Severity),
               mean(FIRE_test_2$Severity)+sd(FIRE_test_2$Severity))
  
  Results.Model_sev<-Effect(c("TIME","Severity"), model_outputs[[i]],
                            xlevels=list(TIME= unique(FIRE_test_2$TIME), 
                                         Severity=SLevels))
  
  Results.Model_sev <- as.data.frame(Results.Model_sev)
  
  
  Results.Model_sev$Support.F<-factor(Results.Model_sev$Severity,
                                      levels=SLevels,
                                      labels=c("Low", "Moderate", "Severe"))
  
  Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, 
                              aes(x = TIME, y =fit, group= Support.F))+
    geom_line(size=2, aes(color=Support.F))+
    # facet_wrap(~pc) + 
    geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=Support.F, fill=Support.F), alpha=.2)+
    scale_colour_manual(values = c("blue", "grey", "red")) + 
    scale_fill_manual(values = c("blue", "grey", "red")) + 
    xlab("TIME")+
    ylab(paste0("Changes in ", variable_i))+ theme_bw()+
    # geom_hline(yintercept = c(unique(FIRE_test_2[, variable][which(FIRE_test_2$TIME ==3)])), 
    #            col = c("black")) +
    geom_hline(yintercept = 0,
               col = c("black")) +
    geom_vline(xintercept = c(2), col = c("red"), linetype = "longdash")+
    theme(legend.position = "top", 
          legend.title=element_blank())
  print(Final.Fixed.Plot.1)
  # jpeg(paste0(variable_i,"_FIRE_severity_0609", ".jpg"))
  plot(Final.Fixed.Plot.1)
  # dev.off()
}

for (i in seq_along(variable)){
  variable_i <- variable[i]  
  SLevels1<-c(mean(FIRE_test_2$top.rugosity_0)-sd(FIRE_test_2$top.rugosity_0),
              mean(FIRE_test_2$top.rugosity_0),
              mean(FIRE_test_2$top.rugosity_0)+sd(FIRE_test_2$top.rugosity_0))
  #extract fixed effects
  
  Results.Model_top<-Effect(c("TIME", "top.rugosity_0"), model_outputs[[i]],
                            xlevels=list(TIME= unique(FIRE_test_2$TIME), 
                                         top.rugosity_0 = SLevels1))
  Results.Model_top<-as.data.frame(Results.Model_top)
  
  Results.Model_top$pc<-factor(Results.Model_top$top.rugosity_0,
                               levels=SLevels1,
                               labels=c("Low Complexity", "Medium Complexity", "High Complexity"))
  Final.Fixed.Plot.2 <-ggplot(data = Results.Model_top, 
                              aes(x = TIME, y =fit, group= pc))+
    geom_line(size=2, aes(color=pc))+
    # facet_wrap(~pc) + 
    geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=pc, fill=pc), alpha=.2)+
    scale_colour_manual(values = c("#440154FF", "grey", "#218F8DFF")) + 
    scale_fill_manual(values = c("#440154FF", "grey", "#218F8DFF")) + 
    xlab("TIME")+
    ylab(paste0("Changes in ", variable_i))+ theme_bw()+
    # geom_hline(yintercept = c(unique(FIRE_test_2[, variable][which(FIRE_test_2$TIME ==3)])), 
    #            col = c("black")) +
    geom_hline(yintercept = 0,
               col = c("black")) +
    geom_vline(xintercept = c(2), col = c("red"), linetype = "longdash")+
    theme(legend.position = "top", 
          legend.title=element_blank())
  print(Final.Fixed.Plot.2)
  # jpeg(paste0(variable_i,"_FIRE_complexity_0609", ".jpg"))
  plot(Final.Fixed.Plot.2)
  # dev.off()
}
################################################################################
################################################################################