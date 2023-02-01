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
BBD_plots <- unique(dat$plotID[which(dat$year == 2014 & dat $ siteID == "BART")])

dat <- cbind(dat, dat$year)
dat <- dat[which(dat$plotID %in% BBD_plots),]
dat <- cbind(dat, dat$year)
plots <- unique(dat$plotID)

absolute_change <- NULL
percent_change <- NULL
colnames(dat)


################ Calculating Changes in Canopy structures over time ############
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

BART_LiDAR <- dat[dat$siteID == "BART", ]
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

# absolute_change <- rbind(absolute_change, year_0) %>%
#   arrange(plotID, year)

colnames(first_year)[c(3, 6:28)] <- paste0(colnames(first_year)[c(3, 6:28)], "_", "0")
first_year <- first_year[, c(1:3, 6:28)]

################################################################################
################################################################################

tree_points <- data.table::fread("./04.Data_table/tree_rm_duplicates.csv") ### vege structure data ######
BA_volume <- tree_points

################################################################################
################################################################################
################################# BBD ##########################################
################################################################################
################################################################################

BART <- BA_volume[BA_volume$siteID == "BART", ]

################################################################################
BART$BA <- replace_na(BART$BA, 0)
BART$Volume <- replace_na(BART$Volume, 0)

BART_BA_0 <- BART[which(BART$BA == 0), ]
BART_BA_0_treeid <- unique(BART_BA_0$idtID)
BART_BA_0_treeid

# BA = 0, including earlier and later
BART_BA_0
BART_BA_0_treeid

#BA != 0 including earlier and later
BART_BA_X0 <- BART[which(BART$BA != 0), ]
BART_BA_X0_treeid <- unique(BART_BA_X0$idtID)

# all year BA ===0
`%ni%` <- Negate(`%in%`)

BART_BA_0_entire <- subset(BART, (idtID %in% BART_BA_0_treeid) & (idtID %ni% BART_BA_X0_treeid))
sum(BART_BA_0_entire$BA)

BART_BA_x_entire <- subset(BART, (idtID %ni% BART_BA_0_treeid) & (idtID %in% BART_BA_X0_treeid))
sum(BART_BA_x_entire$BA)

# BA != 0 but later BA = 0 or opposite
BART_BA_X0 <- subset(BART, (idtID %in% BART_BA_0_treeid) & (idtID %in% BART_BA_X0_treeid))

colnames(BART_BA_X0)
BART_BA_X0 <- BART_BA_X0%>%
  group_by(domainID, siteID, plotID, subplotID, nestplotID, idtID) %>%
  mutate(DBH = max(DBH),
         height = max(height),
         Volume = max(Volume),
         BA = max(BA)) %>%
  distinct()


BART_all <- rbind(BART_BA_0_entire, BART_BA_x_entire, BART_BA_X0)
colnames(BART_all)

BART_all <- BART_all[, -c(20)]
colnames(BART_all)

###############################################################################
BA_volume <- BART_all[which(BART_all$siteID == "BART"), ]

BBD <- BA_volume %>%
  mutate(month = substr(format(date), 6,7), year = substr(format(date), 1,4)) %>%
  filter(DBH > 5) %>%
  group_by(siteID, plotID, year) %>%
  mutate(total_BA = sum(BA, na.rm = T),
         total_Volume = sum(Volume, na.rm = T),
         richness = length(unique(species)),
         meanELV = mean(elv)) %>%
  filter(grepl('Beech|beech|Beech bark Disease|Beach|beach|bark disease|BBD|bbd|beech bark disease|Beech bark disease', remarks)) %>%
  mutate(disturbed = "BBD") %>%
  group_by(siteID, plotID, year, disturbed) %>%
  summarize(Disturbed_volume_ratio = sum(Volume, na.rm = T)/total_Volume * 100,
            Disturbed_BA_ratio = sum(BA, na.rm = T)/total_BA * 100,
            richness = richness,
            meanELV = meanELV) %>%
  distinct()


BBD <- BBD %>%
  group_by(siteID, plotID, disturbed) %>%
  summarize(Disturbed_volume_ratio = mean(Disturbed_volume_ratio),
            Disturbed_BA_ratio = mean(Disturbed_BA_ratio),
            richness = mean(richness)) %>%
  distinct()
colnames(BBD)

BBD_plots <- unique(BBD$plotID)
BBD_plots
BBD_ground <- BBD

#################### selecting LiDAR metrics _ ABS changes #####################

absolute_change_1 <- absolute_change
BBD_test_abs <- merge(absolute_change_1, BBD_ground, by = c("siteID", "plotID"), all.y = T)

################### selecting LiDAR metrics _ no changes #######################

BBD_test_abs_1 <- merge(BBD_test_abs, first_year, by = c("siteID", "plotID"), all.x = T)

################################################################################
BBD_test_1 <- BBD_test_abs_1 %>%
  filter(!is.na(plotID_year))


BBD_test_1 <- BBD_test_1[BBD_test_1$siteID == "BART", ]
BBD_test_1 <- distinct(BBD_test_1)
################################################################################
colnames(BBD_test_1)
BBD_test_1 <- BBD_test_1[, c(1:3, 7:57)]
colnames(BBD_test_1)

BBD_test_scaled <- BBD_test_1
BBD_test_scaled[,c(4:25, 34:54)]  <- BBD_test_scaled[,c(4:25,34:54)]  %>%
  mutate(across(where(is.numeric), ~ scale(.)))
summary(BBD_test_scaled)


BBD_test_1[,c(34:54)]  <- BBD_test_1[,c(34:54)]  %>%
  mutate(across(where(is.numeric), ~ scale(.)))
################################################################################
################################################################################
##############################     mixed      ##################################
################################################################################
################################################################################
BBD_test_scaled <- BBD_test_scaled %>%
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
       data = BBD_test_scaled, lwd = 2, 
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=0)})

xyplot(Disturb_form_2,
       ylab= paste0("Changes in ", variable), xlab = "Time (Year)", group=plotID,type=c("p","l"), data= BBD_test_scaled)


######## delete outliers
BBD_test_2 <- BBD_test_1[which(BBD_test_1$plotID %ni% c("BART_033")), ]
BBD_test_2 <- BBD_test_2 %>%
  arrange(plotID, year)

xyplot(Disturb_form_1,
       ylab= paste0("Changes in ", variable), xlab = "Time (Year)", group=plotID,type=c("p","l"), 
       data = BBD_test_2, lwd = 2, 
       panel=function(...) {
         panel.xyplot(...)
         panel.abline(h=0)})

xyplot(Disturb_form_2,
       ylab= paste0("Changes in ", variable), xlab = "Time (Year)", group=plotID,type=c("p","l"), data= BBD_test_2)

############## stats ##################
hist(sqrt(BBD_test_2$Disturbed_BA_ratio))
mean(BBD_test_2$Disturbed_BA_ratio)
median(BBD_test_2$Disturbed_BA_ratio)
sd(BBD_test_2$Disturbed_BA_ratio)
max(BBD_test_2$Disturbed_BA_ratio)
min(BBD_test_2$Disturbed_BA_ratio)

length(unique(BBD_test_2$plotID))

################# tranformation

BBD_test_2$Severity <- sqrt(BBD_test_2$Disturbed_BA_ratio)

################################################################################
BBD_test_2 %>% 
  ggplot(aes(x=TIME, y=mean.max.canopy.ht)) +
  geom_smooth(aes(group=Severity, colour= factor(Severity)))


mixed_model3.canopy.ht = lme(fixed= mean.max.canopy.ht
                             ~ Severity:TIME + top.rugosity_0:TIME + top.rugosity_0 + Severity + poly(TIME, 3, raw = TRUE), 
                             random = ~ TIME|plotID,
                             method = "REML",
                             correlation=corAR1(form= ~ TIME|plotID),
                             control=list(maxIter=10000, niterEM=10000, opt="optim"),
                             data= BBD_test_2)

# grid.arrange(plot(mixed_model3.canopy.ht,type=c("p","smooth")),
#              plot(mixed_model3.canopy.ht,sqrt(abs(resid(.)))~fitted(.),
#                   type=c("p","smooth"),ylab=expression(sqrt(abs(resid)))),
#              ## "sqrt(abs(resid(x)))"),
#              plot(mixed_model3.canopy.ht,resid(.,type="pearson")~TIME,
#                   type=c("p","smooth")),
#              qqnorm(mixed_model3.canopy.ht,abline=c(0,1)))


mixed_model3.gfp = lme(fixed= deepgap.fraction 
                       ~ Severity:TIME + top.rugosity_0:TIME + top.rugosity_0 + Severity + poly(TIME, 3, raw = TRUE), 
                       random = ~ TIME|plotID,
                       method = "REML",
                       correlation=corAR1(form= ~ TIME|plotID),
                       control=list(maxIter=10000, niterEM=10000, opt="optim"),
                       data= BBD_test_2)

mixed_model3.vai = lme(fixed= LAI ~
                         Severity:TIME + top.rugosity_0:TIME + top.rugosity_0 + Severity + poly(TIME, 3, raw = TRUE),
                       random = ~ TIME|plotID,
                       method = "REML",
                       correlation=corAR1(form= ~ TIME|plotID),
                       control=list(maxIter=10000, niterEM=10000, opt="optim"),
                       data= BBD_test_2)

mixed_model3.lai_sub = lme(fixed= LAI_subcanopy ~
                             Severity:TIME + top.rugosity_0:TIME + top.rugosity_0 + Severity + poly(TIME, 3, raw = TRUE),
                           random = ~ TIME|plotID,
                           method = "REML",
                           correlation=corAR1(form= ~ TIME|plotID),
                           control=list(maxIter=10000, niterEM=10000, opt="optim"),
                           data= BBD_test_2) 

mixed_model3.tr = lme(fixed= top.rugosity ~
                        Severity:TIME + top.rugosity_0:TIME + top.rugosity_0 + Severity + poly(TIME, 3, raw = TRUE),
                      random = ~ TIME|plotID,
                      method = "REML",
                      correlation=corAR1(-0.5, form= ~ TIME|plotID),
                      control=list(maxIter=10000, niterEM=10000, opt="optim"),
                      data= BBD_test_2)

mixed_model3.gini = lme(fixed= gini 
                        ~ Severity:TIME + top.rugosity_0:TIME + top.rugosity_0 + Severity + poly(TIME, 3, raw = TRUE),
                        random = ~ TIME|plotID,
                        method = "REML",
                        correlation=corAR1(form= ~ TIME|plotID),
                        control=list(maxIter=10000, niterEM=10000, opt="optim"),
                        data= BBD_test_2)


tab_model(mixed_model3.canopy.ht, mixed_model3.gfp, mixed_model3.vai, mixed_model3.lai_sub, mixed_model3.tr, mixed_model3.gini)


################################################################################
################################################################################
################################################################################
model_outputs <- list(mixed_model3.canopy.ht, mixed_model3.gfp, mixed_model3.vai, mixed_model3.lai_sub, mixed_model3.tr, mixed_model3.gini)
plot_model(model_outputs[[1]], type = "pred", 
           mdrt.values = "meansd",
           terms = c("TIME [all]"))

################################################################################
################################################################################
################################################################################
BBD_outputs <- model_outputs <- list(mixed_model3.canopy.ht, mixed_model3.gfp, mixed_model3.vai, mixed_model3.lai_sub, mixed_model3.tr, mixed_model3.gini)
save(BBD_outputs, file = "./BBD_outputs_2023.RData")


names(model_outputs) <- c("mean.max.canopy.ht", "deepgap.fraction", "LAI", "LAI_sub",  "top.rugosity", "gini")
################################################################################
variable <- c("mean.max.canopy.ht", "deepgap.fraction", "LAI", "LAI_sub","top.rugosity", "gini")
###############################################################################
###################################Severity: TIME ##############################
for (i in seq_along(variable)){
  model_sumamry  <- summary(model_outputs[[i]])
  model_table <- as.data.frame(model_sumamry$tTable)
  model_table <- tibble::rownames_to_column(model_table, "Term")
  # model_table
  
  variable_i <- variable[i]
  SLevels <- c(mean(BBD_test_2$Severity)-sd(BBD_test_2$Severity),
               mean(BBD_test_2$Severity),
               mean(BBD_test_2$Severity)+sd(BBD_test_2$Severity))
  
  Results.Model_sev<-Effect(c("TIME","Severity"), model_outputs[[i]],
                            xlevels=list(TIME= unique(BBD_test_2$TIME),
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
      # geom_hline(yintercept = c(unique(BBD_test_2[, variable][which(BBD_test_2$TIME ==3)])),
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
      annotate("text", x = mean(BBD_test_2$TIME) * 1.3, y = mean(BBD_test_2[, variable_i]) / 10, label = paste0("p = ", round(a, 2)), size = 7)
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
      # geom_hline(yintercept = c(unique(BBD_test_2[, variable][which(BBD_test_2$TIME ==3)])),
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
# 
# 
# ###################################  ICC: TIME #################################
# for (i in seq_along(variable)){
#   model_sumamry  <- summary(model_outputs[[i]])
#   model_table <- as.data.frame(model_sumamry$tTable)
#   model_table <- tibble::rownames_to_column(model_table, "Term")
#   # model_table
#   variable_i <- variable[i]  
#   SLevels1<-c(mean(BBD_test_2$top.rugosity_0)-sd(BBD_test_2$top.rugosity_0),
#               mean(BBD_test_2$top.rugosity_0),
#               mean(BBD_test_2$top.rugosity_0)+sd(BBD_test_2$top.rugosity_0))
#   #extract fixed effects
#   
#   Results.Model_top<-Effect(c("TIME", "top.rugosity_0"), model_outputs[[i]],
#                             xlevels=list(TIME= unique(BBD_test_2$TIME), 
#                                          top.rugosity_0 = SLevels1))
#   Results.Model_top<-as.data.frame(Results.Model_top)
#   
#   Results.Model_top$pc<-factor(Results.Model_top$top.rugosity_0,
#                                levels=SLevels1,
#                                labels=c("Low Complexity", "Medium Complexity", "High Complexity"))
#   if (model_table$`p-value`[model_table$Term == "TIME:top.rugosity_0"] < 0.05) {
#     a <- model_table$`p-value`[model_table$Term == "TIME:top.rugosity_0"]
#     Final.Fixed.Plot.1 <-ggplot(data = Results.Model_top, 
#                                 aes(x = TIME, y =fit, group= pc))+
#       geom_line(size=2, aes(color=pc))+
#       # facet_wrap(~pc) + 
#       geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=pc, fill=pc), alpha=.2)+
#       scale_colour_manual(values = c("#440154FF", "grey", "#218F8DFF")) + 
#       scale_fill_manual(values = c("#440154FF", "grey", "#218F8DFF")) + 
#       xlab("TIME")+
#       ylab(paste0("Changes in ", variable_i))+ theme_bw()+
#       # geom_hline(yintercept = c(unique(BBD_test_2[, variable][which(BBD_test_2$TIME ==3)])), 
#       #            col = c("black")) +
#       geom_hline(yintercept = 0,
#                  col = c("black")) +
#       theme(legend.title=element_blank(),
#             axis.ticks.length = unit(5, "pt"),
#             legend.position = "none",
#             axis.title.x = element_blank(),
#             panel.background = element_rect(
#               # fill = "black",
#               colour = "black",
#               size = 4
#             )) + 
#       scale_y_continuous(labels = label_number(accuracy = 0.001)) + 
#       annotate("text", x = mean(BBD_test_2$TIME) * 1.3, y = mean(BBD_test_2[, variable_i]) / 10, label = paste0("p = ", round(a, 2)), size = 7)
#     print(Final.Fixed.Plot.1)
#   } else {
#     Final.Fixed.Plot.1 <-ggplot(data = Results.Model_top, 
#                                 aes(x = TIME, y =fit, group= pc))+
#       geom_line(size=2, aes(color=pc))+
#       # facet_wrap(~pc) + 
#       geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=pc, fill=pc), alpha=.2)+
#       scale_colour_manual(values = c("#440154FF", "grey", "#218F8DFF")) + 
#       scale_fill_manual(values = c("#440154FF", "grey", "#218F8DFF")) + 
#       xlab("TIME")+
#       ylab(paste0("Changes in ", variable_i))+ theme_bw()+
#       # geom_hline(yintercept = c(unique(BBD_test_2[, variable][which(BBD_test_2$TIME ==3)])), 
#       #            col = c("black")) +
#       geom_hline(yintercept = 0,
#                  col = c("black")) +
#       scale_y_continuous(labels = label_number(accuracy = 0.001)) + 
#       theme(legend.position = "none",
#             axis.title.x = element_blank())
#   }
#   jpeg(paste0(variable_i,"_BBD_complexity_0630_Den", ".jpg"))
#   plot(Final.Fixed.Plot.1)
#   dev.off()
# }
# 
# 
# 
# 
# 
# for (i in seq_along(variable)) {
#   variable_i <- variable[i]
#   
#   Results.Model_sev<-Effect(c("TIME"), model_outputs[[i]],
#                             xlevels=list(TIME= unique(BBD_test_2$TIME)))
#   
#   Results.Model_sev <- as.data.frame(Results.Model_sev)
#   
#   
#   Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, 
#                               aes(x = TIME, y =fit))+
#     geom_line(size=2)+
#     # facet_wrap(~pc) + 
#     geom_ribbon(aes(ymin= fit-se, ymax= fit+se), alpha=.2)+
#     scale_colour_manual(values = c("blue", "grey", "red")) + 
#     scale_fill_manual(values = c("blue", "grey", "red")) + 
#     xlab("TIME")+
#     ylab(paste0("Changes in ", variable_i))+ theme_bw()+
#     # geom_hline(yintercept = c(unique(BBD_test_2[, variable][which(BBD_test_2$TIME ==3)])), 
#     #            col = c("black")) +
#     geom_hline(yintercept = 0,
#                col = c("black")) +
#     theme(legend.position = "top", 
#           legend.title=element_blank(),
#     )
#   print(Final.Fixed.Plot.1)
#   jpeg(paste0(variable_i,"_BBD_TIME_0614", ".jpg"))
#   plot(Final.Fixed.Plot.1)
#   dev.off()  
# }
# for (i in seq_along(variable)){
#   variable_i <- variable[i]
#   
#   SLevels <- c(mean(BBD_test_2$Severity)-sd(BBD_test_2$Severity),
#                mean(BBD_test_2$Severity),
#                mean(BBD_test_2$Severity)+sd(BBD_test_2$Severity))
#   
#   Results.Model_sev<-Effect(c("TIME","Severity"), model_outputs[[i]],
#                             xlevels=list(TIME= unique(BBD_test_2$TIME), 
#                                          Severity=SLevels))
#   
#   Results.Model_sev <- as.data.frame(Results.Model_sev)
#   
#   
#   Results.Model_sev$Support.F<-factor(Results.Model_sev$Severity,
#                                       levels=SLevels,
#                                       labels=c("Low", "Moderate", "Severe"))
#   
#   Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, 
#                               aes(x = TIME, y =fit, group= Support.F))+
#     geom_line(size=2, aes(color=Support.F))+
#     # facet_wrap(~pc) + 
#     geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=Support.F, fill=Support.F), alpha=.2)+
#     scale_colour_manual(values = c("blue", "grey", "red")) + 
#     scale_fill_manual(values = c("blue", "grey", "red")) + 
#     xlab("TIME")+
#     ylab(paste0("Changes in ", variable_i))+ theme_bw()+
#     # geom_hline(yintercept = c(unique(BBD_test_2[, variable][which(BBD_test_2$TIME ==3)])), 
#     #            col = c("black")) +
#     geom_hline(yintercept = 0,
#                col = c("black")) +
#     theme(legend.position = "top", 
#           legend.title=element_blank(),
#           panel.background = element_rect(
#             # fill = "black",
#             colour = "black",
#             size = 4
#           ))
#   print(Final.Fixed.Plot.1)
#   jpeg(paste0(variable_i,"_BBD_severity_0614_Den", ".jpg"))
#   plot(Final.Fixed.Plot.1)
#   dev.off()
# }
# for (i in seq_along(variable)){
#   variable_i <- variable[i]  
#   SLevels1<-c(mean(BBD_test_2$top.rugosity_0)-sd(BBD_test_2$top.rugosity_0),
#               mean(BBD_test_2$top.rugosity_0),
#               mean(BBD_test_2$top.rugosity_0)+sd(BBD_test_2$top.rugosity_0))
#   #extract fixed effects
#   
#   Results.Model_top<-Effect(c("TIME", "top.rugosity_0"), model_outputs[[i]],
#                             xlevels=list(TIME= unique(BBD_test_2$TIME), 
#                                          top.rugosity_0 = SLevels1))
#   Results.Model_top<-as.data.frame(Results.Model_top)
#   
#   Results.Model_top$pc<-factor(Results.Model_top$top.rugosity_0,
#                                levels=SLevels1,
#                                labels=c("Low Complexity", "Medium Complexity", "High Complexity"))
#   Final.Fixed.Plot.2 <-ggplot(data = Results.Model_top, 
#                               aes(x = TIME, y =fit, group= pc))+
#     geom_line(size=2, aes(color=pc))+
#     # facet_wrap(~pc) + 
#     geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=pc, fill=pc), alpha=.2)+
#     scale_colour_manual(values = c("#440154FF", "grey", "#218F8DFF")) + 
#     scale_fill_manual(values = c("#440154FF", "grey", "#218F8DFF")) + 
#     xlab("TIME")+
#     ylab(paste0("Changes in ", variable_i))+ theme_bw()+
#     # geom_hline(yintercept = c(unique(BBD_test_2[, variable][which(BBD_test_2$TIME ==3)])), 
#     #            col = c("black")) +
#     geom_hline(yintercept = 0,
#                col = c("black")) +
#     theme(legend.position = "top", 
#           legend.title=element_blank(),
#           panel.background = element_rect(
#             # fill = "black",
#             colour = "black",
#             size = 4
#           ))
#   print(Final.Fixed.Plot.2)
#   jpeg(paste0(variable_i,"_BBD_complexity_0630_Den", ".jpg"))
#   plot(Final.Fixed.Plot.2)
#   dev.off()
# }
# 
# 
# 
# 
# # par(mfrow=c(1,5))
# for (i in seq_along(variable)) {
#   variable_i <- variable[i]
#   
#   Results.Model_sev<-Effect(c("TIME"), model_outputs[[i]],
#                             xlevels=list(TIME= unique(BBD_test_2$TIME)))
#   
#   Results.Model_sev <- as.data.frame(Results.Model_sev)
#   
#   
#   Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, 
#                               aes(x = TIME, y =fit))+
#     geom_line(size=2)+
#     # facet_wrap(~pc) + 
#     geom_ribbon(aes(ymin= fit-se, ymax= fit+se), alpha=.2)+
#     scale_colour_manual(values = c("blue", "grey", "red")) + 
#     scale_fill_manual(values = c("blue", "grey", "red")) + 
#     xlab("TIME")+
#     ylab(paste0("Changes in ", variable_i))+ theme_bw()+
#     # geom_hline(yintercept = c(unique(BBD_test_2[, variable][which(BBD_test_2$TIME ==3)])), 
#     #            col = c("black")) +
#     geom_hline(yintercept = 0,
#                col = c("black")) +
#     theme(legend.position = "top", 
#           legend.title=element_blank())
#   print(Final.Fixed.Plot.1)
#   jpeg(paste0(variable_i,"_BBD_TIME_0614", ".jpg"))
#   plot(Final.Fixed.Plot.1)
#   dev.off()  
# }
# 
# for (i in seq_along(variable)){
#   
#   
#   variable_i <- variable[i]
#   
#   SLevels <- c(mean(BBD_test_2$Severity)-sd(BBD_test_2$Severity),
#                mean(BBD_test_2$Severity),
#                mean(BBD_test_2$Severity)+sd(BBD_test_2$Severity))
#   
#   Results.Model_sev<-Effect(c("TIME","Severity"), model_outputs[[i]],
#                             xlevels=list(TIME= unique(BBD_test_2$TIME), 
#                                          Severity=SLevels))
#   
#   Results.Model_sev <- as.data.frame(Results.Model_sev)
#   
#   
#   Results.Model_sev$Support.F<-factor(Results.Model_sev$Severity,
#                                       levels=SLevels,
#                                       labels=c("Low", "Moderate", "Severe"))
#   
#   Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, 
#                               aes(x = TIME, y =fit, group= Support.F))+
#     geom_line(size=2, aes(color=Support.F))+
#     # facet_wrap(~pc) + 
#     geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=Support.F, fill=Support.F), alpha=.2)+
#     scale_colour_manual(values = c("blue", "grey", "red")) + 
#     scale_fill_manual(values = c("blue", "grey", "red")) + 
#     xlab("TIME")+
#     ylab(paste0("Changes in ", variable_i))+ theme_bw()+
#     # geom_hline(yintercept = c(unique(BBD_test_2[, variable][which(BBD_test_2$TIME ==3)])), 
#     #            col = c("black")) +
#     geom_hline(yintercept = 0,
#                col = c("black")) +
#     theme(legend.position = "top", 
#           legend.title=element_blank())
#   print(Final.Fixed.Plot.1)
#   jpeg(paste0(variable_i,"_BBD_severity_0614_den", ".jpg"))
#   plot(Final.Fixed.Plot.1)
#   dev.off()
# }
# 
# for (i in seq_along(variable)){
#   variable_i <- variable[i]  
#   SLevels1<-c(mean(BBD_test_2$top.rugosity_0)-sd(BBD_test_2$top.rugosity_0),
#               mean(BBD_test_2$top.rugosity_0),
#               mean(BBD_test_2$top.rugosity_0)+sd(BBD_test_2$top.rugosity_0))
#   #extract fixed effects
#   
#   Results.Model_top<-Effect(c("TIME", "top.rugosity_0"), model_outputs[[i]],
#                             xlevels=list(TIME= unique(BBD_test_2$TIME), 
#                                          top.rugosity_0 = SLevels1))
#   Results.Model_top<-as.data.frame(Results.Model_top)
#   
#   Results.Model_top$pc<-factor(Results.Model_top$top.rugosity_0,
#                                levels=SLevels1,
#                                labels=c("Low Complexity", "Medium Complexity", "High Complexity"))
#   Final.Fixed.Plot.2 <-ggplot(data = Results.Model_top, 
#                               aes(x = TIME, y =fit, group= pc))+
#     geom_line(size=2, aes(color=pc))+
#     # facet_wrap(~pc) + 
#     geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=pc, fill=pc), alpha=.2)+
#     # geom_errorbar(aes(ymax=fit+se, ymin=fit-se), width=.1)+
#     scale_colour_manual(values = c("#440154FF", "grey", "#218F8DFF")) + 
#     scale_fill_manual(values = c("#440154FF", "grey", "#218F8DFF")) + 
#     xlab("TIME")+
#     ylab(paste0("Changes in ", variable_i))+ theme_bw()+
#     # geom_hline(yintercept = c(unique(BBD_test_2[, variable][which(BBD_test_2$TIME ==3)])), 
#     #            col = c("black")) +
#     geom_hline(yintercept = 0,
#                col = c("black")) +
#     theme(legend.position = "top", 
#           legend.title=element_blank())
#   print(Final.Fixed.Plot.2)
#   jpeg(paste0(variable_i,"_BBD_complexity_0614_den", ".jpg"))
#   plot(Final.Fixed.Plot.2)
#   dev.off()
# }
# 
# ################################################################################
# 
# 
# 
# 
# ################################################################################
# 
# library(effects)
# 
# i <- 3
# variable_i <- variable[i]
# 
# SLevels <- c(mean(BBD_test_2$Severity)-sd(BBD_test_2$Severity),
#              mean(BBD_test_2$Severity),
#              mean(BBD_test_2$Severity)+sd(BBD_test_2$Severity))
# 
# 
# 
# 
# Results.Model_sev<-Effect(c("TIME","Severity"), model_outputs[[i]],
#                           xlevels=list(TIME= unique(BBD_test_2$TIME), 
#                                        Severity=SLevels))
# 
# 
# Results.Model_sev <- as.data.frame(Results.Model_sev)
# Results.Model_sev$Support.F<-factor(Results.Model_sev$Severity,
#                                     levels=SLevels,
#                                     labels=c("Low", "Moderate", "Severe"))
# 
# 
# Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, 
#                             aes(x = TIME, y =fit, group= Support.F))+
#   geom_line(size=2, aes(color=Support.F))+
#   # facet_wrap(~pc) + 
#   geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=Support.F, fill=Support.F), alpha=.2)+
#   scale_colour_manual(values = c("blue", "grey", "red")) + 
#   scale_fill_manual(values = c("blue", "grey", "red")) + 
#   xlab("TIME")+
#   ylab(paste0("Changes in ", variable_i))+ theme_bw()+
#   # geom_hline(yintercept = c(unique(BBD_test_2[, variable][which(BBD_test_2$TIME ==3)])), 
#   #            col = c("black")) +
#   geom_hline(yintercept = 0,
#              col = c("black")) +
#   theme(legend.position = "top", 
#         legend.title=element_blank())
# print(Final.Fixed.Plot.1)
# 
# SLevels1<-c(mean(BBD_test_2$top.rugosity_0)-sd(BBD_test_2),$top.rugosity_0),
#             mean(BBD_test_2$top.rugosity_0
#             mean(BBD_test_2$top.rugosity_0)+sd(BBD_test_2$top.rugosity_0))
# #extract fixed effects
# 
# Results.Model.4<-Effect(c("TIME", "top.rugosity_0"), mixed_model3.vai,
#                         xlevels=list(TIME= unique(BBD_test_2$TIME), 
#                                      top.rugosity_0 = SLevels1))
# 
# # Results.Model.4<-Effect(c("TIME","Severity", "disturbed"), mixed_model3,
# #                         xlevels=list(TIME=seq(0, 10, .2), 
# #                                      Severity=SLevels,
# #                                      disturbed = c(min(pest_3$disturbed),max(pest_3$disturbed))))
# #Convert to data frame for ggplot
# Results.Model.4<-as.data.frame(Results.Model.4)
# 
# #Label Support for graphing
# # Results.Model.4$Support.F<-factor(Results.Model.4$Severity,
# #                                   levels=SLevels,
# #                                   labels=c("Low", "Moderate", "Severe"))
# Results.Model.4$pc<-factor(Results.Model.4$top.rugosity_0,
#                            levels=SLevels1,
#                            labels=c("Low Complexity", "Medium Complexity", "High Complexity"))
# 
# viridis(100)[c(1, 50, 100)]
# 
# #Plot fixed effect
# Final.Fixed.Plot.1 <-ggplot(data = Results.Model.4, 
#                             aes(x = TIME, y =fit, group= pc))+
#   geom_line(size=2, aes(color=pc))+
#   # facet_wrap(~pc) + 
#   geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=pc, fill=pc), alpha=.2)+
#   scale_colour_manual(values = c("#440154FF", "grey", "#218F8DFF")) + 
#   scale_fill_manual(values = c("#440154FF", "grey", "#218F8DFF")) + 
#   xlab("TIME")+
#   ylab(paste0("Changes in ", variable))+ theme_bw()+
#   # geom_hline(yintercept = c(unique(BBD_test_2[, variable][which(BBD_test_2$TIME ==3)])), 
#   #            col = c("black")) +
#   geom_hline(yintercept = 0,
#              col = c("black")) +
#   theme(legend.position = "top", 
#         legend.title=element_blank())
# Final.Fixed.Plot.1
# ################################################################################
# 
# model_outputs <- list(mixed_model3.canopy.ht, mixed_model3.gfp, mixed_model3.vai, mixed_model3.tr, mixed_model3.fhd)
# names(model_outputs) <- c("mean.max.canopy.ht", "deepgap.fraction", "LAI", "top.rugosity", "gini")
# ################################################################################
# variable <- c("mean.max.canopy.ht", "deepgap.fraction", "LAI", "top.rugosity", "gini")
# 
# dwplot(model_outputs, ci = 0.95,
#        whisker_args = list(size = 1),
#        vline = geom_vline(
#          xintercept = 0,
#          # colour = "grey60",
#          linetype = 2),
#        # dot_args = list(aes(col = "black"), size = 3),  
#        effects="fixed" )+#, style = "distribution") +
#   # ggtitle(paste0(str_to_title(variable)))+#, "- Model : ", model_name[which.min(which.min(ano$AIC))])) + # models[ano$Model[which.min(ano$AIC)]]
#   theme_bw(base_size = 17) + 
#   xlab("Coefficient Estimate") + 
#   ylab("") +
#   theme(plot.title = element_text(face = "bold"),
#         legend.position = "none") +
#   scale_colour_grey(
#     start = .3,
#     end = .7,
#     name = "Transmission",
#     breaks = c(0, 1),
#     labels = c("Automatic", "Manual"))
# 
