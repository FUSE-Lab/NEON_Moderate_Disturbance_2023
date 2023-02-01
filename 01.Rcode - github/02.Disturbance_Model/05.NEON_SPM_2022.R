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
LyD_plots <- unique(dat$plotID[which(dat$year == 2014 & dat $ siteID == "HARV")])

dat <- cbind(dat, dat$year)
dat <- dat[which(dat$plotID %in% LyD_plots),]
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

HARV_LiDAR <- dat[dat$siteID == "HARV", ]
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
 
################################################################################
################################################################################
################################# LYD ##########################################
################################################################################
################################################################################

HARV <- BA_volume[BA_volume$siteID == "HARV", ]

################################################################################
################################################################################
HARV$BA <- replace_na(HARV$BA, 0)
HARV$Volume <- replace_na(HARV$Volume, 0)

HARV_BA_0 <- HARV[which(HARV$BA == 0), ]
HARV_BA_0_treeid <- unique(HARV_BA_0$idtID)
HARV_BA_0_treeid

# BA = 0, including earlier and later
HARV_BA_0
HARV_BA_0_treeid

#BA != 0 including earlier and later
HARV_BA_X0 <- HARV[which(HARV$BA != 0), ]
HARV_BA_X0_treeid <- unique(HARV_BA_X0$idtID)

# all year BA ===0
`%ni%` <- Negate(`%in%`)

HARV_BA_0_entire <- subset(HARV, (idtID %in% HARV_BA_0_treeid) & (idtID %ni% HARV_BA_X0_treeid))
sum(HARV_BA_0_entire$BA)

HARV_BA_x_entire <- subset(HARV, (idtID %ni% HARV_BA_0_treeid) & (idtID %in% HARV_BA_X0_treeid))
sum(HARV_BA_x_entire$BA)

# BA != 0 but later BA = 0 or opposite
HARV_BA_X0 <- subset(HARV, (idtID %in% HARV_BA_0_treeid) & (idtID %in% HARV_BA_X0_treeid))

colnames(HARV_BA_X0)
HARV_BA_X0 <- HARV_BA_X0%>%
  group_by(domainID, siteID, plotID, subplotID, nestplotID, idtID) %>%
  mutate(DBH = max(DBH),
         height = max(height),
         Volume = max(Volume),
         BA = max(BA)) %>%
  distinct()


HARV_all <- rbind(HARV_BA_0_entire, HARV_BA_x_entire, HARV_BA_X0)
colnames(HARV_all)

HARV_all <- HARV_all[, -c(20)]
colnames(HARV_all)

################################################################################
BA_volume <- HARV_all[which(HARV_all$siteID == "HARV"), ]

LYD <- BA_volume %>%
  mutate(month = substr(format(BA_volume$date), 6,7), year = substr(format(BA_volume$date), 1,4)) %>%
  filter(DBH > 5) %>%
  group_by(siteID, plotID, year) %>%
  mutate(total_BA = sum(BA, na.rm = T),
         total_Volume = sum(Volume, na.rm = T),
         richness = length(unique(species)),
         meanELV = mean(elv)) %>%
  filter(grepl('Gypsy|gypsy', remarks)) %>%
  mutate(disturbed = "LYD") %>%
  group_by(siteID, plotID, year, disturbed) %>%
  summarize(Disturbed_volume_ratio = sum(Volume, na.rm = T)/total_Volume * 100,
            Disturbed_BA_ratio = sum(BA, na.rm = T)/total_BA * 100,
            richness = richness,
            meanELV = 0,
            class = class) %>%
  distinct()


LYD <- LYD %>%
  group_by(siteID, plotID, disturbed) %>%
  summarize(Disturbed_volume_ratio = mean(Disturbed_volume_ratio),
            Disturbed_BA_ratio = mean(Disturbed_BA_ratio),
            richness = max(richness),
            meanELV = 0,
            class = class) %>%
  distinct()
colnames(LYD)

LYD_plots <- unique(LYD$plotID)
LYD_plots
LYD_ground <- LYD

#################### selecting LiDAR metrics _ ABS changes #####################

absolute_change_1 <- absolute_change
LYD_test_abs <- merge(absolute_change_1, LYD_ground, by = c("siteID", "plotID"), all.y = T)

################### selecting LiDAR metrics _ no changes #######################

LYD_test_abs_1 <- merge(LYD_test_abs, first_year, by = c("siteID", "plotID"), all.x = T)

################################################################################
LYD_test_1 <- LYD_test_abs_1 %>%
  filter(!is.na(plotID_year))


LYD_test_1 <- LYD_test_1[LYD_test_1$siteID == "HARV", ]
LYD_test_1 <- distinct(LYD_test_1)

################################################################################
colnames(LYD_test_1)
LYD_test_1 <- LYD_test_1[, c(1:3, 7:59)]
colnames(LYD_test_1)
LYD_test_1 <- LYD_test_1[which(LYD_test_1$class %in% c("deciduousForest", "mixedForest")), ]

LYD_test_scaled <- LYD_test_1
LYD_test_scaled[,c(4:25, 35:56)]  <- LYD_test_scaled[,c(4:25,35:56)]  %>%
  mutate(across(where(is.numeric), ~ scale(.)))
summary(LYD_test_scaled)

colnames(LYD_test_1[,c(35:56)])
LYD_test_1[,c(35:56)] <- LYD_test_1[,c(35:56)]  %>%
  mutate(across(where(is.numeric), ~ as.numeric(scale(.))))

################################################################################
################################################################################
##############################     mixed      ##################################
################################################################################
################################################################################
LYD_test_scaled <- LYD_test_scaled %>%
  arrange(plotID, TIME)

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
       data = LYD_test_scaled, lwd = 2, 
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=0)})

xyplot(Disturb_form_2,
       ylab= paste0("Changes in ", variable), xlab = "Time (Year)", group=plotID,type=c("p","l"), data= LYD_test_scaled)


LYD_test_2 <- LYD_test_1 %>%
  arrange(plotID, TIME)

xyplot(Disturb_form_1,
       ylab= paste0("Changes in ", variable), xlab = "Time (Year)", group=plotID,type=c("p","l"), 
       data = LYD_test_2, lwd = 2, 
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=0)})

xyplot(Disturb_form_2,
       ylab= paste0("Changes in ", variable), xlab = "Time (Year)", group=plotID,type=c("p","l"), data= LYD_test_2)
############## stats ##################
hist(log(LYD_test_2$Disturbed_BA_ratio))
mean(LYD_test_2$Disturbed_BA_ratio)
sd(LYD_test_2$Disturbed_BA_ratio)

################# tranformation

LYD_test_2$Severity <- log(LYD_test_2$Disturbed_BA_ratio)

################################################################################
################################################################################
################################################################################
################################################################################
LYD_test_2 %>% 
  ggplot(aes(x=TIME, y=mean.max.canopy.ht)) +
  geom_smooth(aes(group=Severity, colour=Severity))

mixed_model3.canopy.ht = lme(fixed= mean.max.canopy.ht ~ 
                               Severity:TIME + top.rugosity_0:TIME + Severity + top.rugosity_0 + poly(TIME, 3, raw = TRUE), 
                             random = ~ TIME|plotID,
                             method = "REML",
                             correlation=corAR1(0, form= ~ TIME|plotID),
                             control=list(maxIter=10000, niterEM=10000, opt="optim"),
                             data= LYD_test_2)
# grid.arrange(plot(mixed_model3.gfp,type=c("p","smooth")),
#              plot(mixed_model3.gfp,sqrt(abs(resid(.)))~fitted(.),
#                   type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
#              ## "sqrt(abs(resid(x)))"),
#              plot(mixed_model3.gfp,resid(.,type="pearson")~TIME,
#                   type=c("p","smooth")),
#              qqnorm(mixed_model3.gfp,abline=c(0,1)))

mixed_model3.gfp = lme(fixed= deepgap.fraction ~ 
                         Severity:TIME + top.rugosity_0:TIME + Severity + top.rugosity_0 + poly(TIME, 3, raw = TRUE), 
                       random = ~ TIME|plotID,
                       method = "REML",
                       correlation=corAR1(-0.1, form= ~ TIME|plotID),
                       control=list(maxIter=10000, niterEM=10000, opt="optim"),
                       data= LYD_test_2)

mixed_model3.vai = lme(fixed= LAI ~ 
                         Severity:TIME + top.rugosity_0:TIME + Severity + top.rugosity_0 + poly(TIME, 3, raw = TRUE), 
                       random = ~ TIME|plotID,
                       method = "REML",
                       correlation=corAR1(0, form= ~ TIME|plotID),
                       control=list(maxIter=10000, niterEM=10000, opt="optim"),
                       data= LYD_test_2)

mixed_model3.lai_sub = lme(fixed= LAI_subcanopy ~ 
                             Severity:TIME + top.rugosity_0:TIME + Severity + top.rugosity_0 + poly(TIME, 3, raw = TRUE), 
                           random = ~ TIME|plotID,
                           method = "REML",
                           correlation=corAR1(0, form= ~ TIME|plotID),
                           control=list(maxIter=10000, niterEM=10000, opt="optim"),
                           data= LYD_test_2)
mixed_model3.tr = lme(fixed= top.rugosity ~ 
                        Severity:TIME + top.rugosity_0:TIME + Severity + top.rugosity_0 + poly(TIME, 3, raw = TRUE), 
                      random = ~ TIME|plotID,
                      method = "REML",
                      correlation=corAR1(0, form= ~ TIME|plotID),
                      control=list(maxIter=10000, niterEM=10000, opt="optim"),
                      data= LYD_test_2)
sjPlot::tab_model(mixed_model3.tr)

mixed_model3.gini = lme(fixed= gini ~ 
                         Severity:TIME + top.rugosity_0:TIME + Severity + top.rugosity_0 + poly(TIME, 3, raw = TRUE), 
                       random = ~ TIME|plotID,
                       method = "REML",
                       correlation=corAR1(-0.1, form= ~ TIME|plotID),
                       control=list(maxIter=10000, niterEM=10000, opt="optim"),
                       data= LYD_test_2)

tab_model(mixed_model3.canopy.ht, mixed_model3.gfp, mixed_model3.vai,mixed_model3.lai_sub,  mixed_model3.tr, mixed_model3.gini)

################################################################################
model_outputs <- list(mixed_model3.canopy.ht, mixed_model3.gfp, mixed_model3.vai, mixed_model3.lai_sub, mixed_model3.tr, mixed_model3.gini)
LYD_outputs <- list(mixed_model3.canopy.ht, mixed_model3.gfp, mixed_model3.vai, mixed_model3.lai_sub, mixed_model3.tr, mixed_model3.gini)
save(LYD_outputs, file = "LYD_outputs_2023.RData")


names(model_outputs) <- c("mean.max.canopy.ht", "deepgap.fraction", "LAI", "sub_LAI", "top.rugosity", "gini")
################################################################################
variable <- c("mean.max.canopy.ht", "deepgap.fraction", "LAI", "sub_LAI", "top.rugosity", "gini")
################################################################################
for (i in seq_along(variable)) {
  variable_i <- variable[i]
  
  Results.Model_sev<-Effect(c("TIME"), model_outputs[[i]],
                            xlevels=list(TIME= unique(LYD_test_2$TIME)))
  
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
    # geom_hline(yintercept = c(unique(LYD_test_2[, variable][which(LYD_test_2$TIME ==3)])), 
    #            col = c("black")) +
    geom_hline(yintercept = 0,
               col = c("black")) +
    theme(legend.position = "top", 
          legend.title=element_blank())
  print(Final.Fixed.Plot.1)
  # jpeg(paste0(variable_i,"_LYD_TIME_0614", ".jpg"))
  plot(Final.Fixed.Plot.1)
  # dev.off()  
}


for (i in seq_along(variable)){
  variable_i <- variable[i]
  
  SLevels <- c(mean(LYD_test_2$Severity)-sd(LYD_test_2$Severity),
               mean(LYD_test_2$Severity),
               mean(LYD_test_2$Severity)+sd(LYD_test_2$Severity))
  
  Results.Model_sev<-Effect(c("TIME","Severity"), model_outputs[[i]],
                            xlevels=list(TIME= unique(LYD_test_2$TIME), 
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
    # geom_hline(yintercept = c(unique(LYD_test_2[, variable][which(LYD_test_2$TIME ==3)])), 
    #            col = c("black")) +
    geom_hline(yintercept = 0, col = c("black")) +
    geom_vline(xintercept = c(2,3), col = c("red"), linetype = "longdash")+
    theme(legend.position = "top", 
          legend.title=element_blank())
  print(Final.Fixed.Plot.1)
  # jpeg(paste0(variable_i,"_LYD_severity_0614_den", ".jpg"))
  plot(Final.Fixed.Plot.1)
  # dev.off()
}

for (i in seq_along(variable)){
  variable_i <- variable[i]  
  SLevels1<-c(mean(LYD_test_2$top.rugosity_0)-sd(LYD_test_2$top.rugosity_0),
              mean(LYD_test_2$top.rugosity_0),
              mean(LYD_test_2$top.rugosity_0)+sd(LYD_test_2$top.rugosity_0))
  #extract fixed effects
  
  Results.Model_top<-Effect(c("TIME", "top.rugosity_0"), model_outputs[[i]],
                            xlevels=list(TIME= unique(LYD_test_2$TIME), 
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
    # geom_hline(yintercept = c(unique(LYD_test_2[, variable][which(LYD_test_2$TIME ==3)])), 
    #            col = c("black")) +
    geom_hline(yintercept = 0,
               col = c("black")) +
    geom_vline(xintercept = c(2,3), col = c("red"), linetype = "longdash")+
    theme(legend.position = "top", 
          legend.title=element_blank())
  print(Final.Fixed.Plot.2)
  jpeg(paste0(variable_i,"_LYD_complexity_0614_den", ".jpg"))
  plot(Final.Fixed.Plot.2)
  dev.off()
}
################################################################################
################################################################################

library(glmmTMB)
library(MASS)

model_re <- glmmTMB(
  rumple ~ (PC1) + (TIME + I(TIME^2)+I(TIME^3)+I(TIME^4)) * Severity  + ar1(1 + factor(TIME)|plotID),
  data = LYD_test_2,
  control=glmmTMBControl(optimizer=nlminb,
                         optCtrl=list(iter.max=1e5,eval.max=1e5)),
  REML = TRUE,
  family = gaussian)

summary(model_re)
tab_model(model_re)

plot_model(model_re, type = "re")

plot_model(model_re, 
           type = "pred", 
           mdrt.values = "meansd",
           terms = c("TIME [all]", "Severity", "PC1"),
           # show.data = T,
           show.intercept	= T,
           show.p	= T,
           line.size = 1.5, ci.lvl = 0.95,
           colors = viridis(1000)[c(1, 400, 700)],
           legend.label = c("1", "2", "3"),
           allow.new.levels=TRUE) + theme_sjplot() +
  geom_hline(yintercept = c(-0.654571313), col = c("black")) +
  ggtitle(paste0("Predicted Values of changes in ", "GFP")) +
  ylab(paste0("Changes in ", "GFP")) +
  theme(axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 12), #change font size of legend text
        legend.title = element_text(size = 15), 
        axis.title = element_text(size=15, face="bold")) +
  theme(plot.title =  element_text(size= 15, face="bold"))


grid.arrange(plot(mixed_model2,type=c("p","smooth")),
             plot(mixed_model2,sqrt(abs(resid(.)))~fitted(.),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
             ## "sqrt(abs(resid(x)))"),
             plot(mixed_model2,resid(.,type="pearson")~TIME,
                  type=c("p","smooth")),
             qqnorm(mixed_model2,abline=c(0,1)))

