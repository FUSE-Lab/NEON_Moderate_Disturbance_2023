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

all_model <- c("BBD_outputs_2023.RData", "HWA_outputs_2023.RData", "EAB_outputs_2023.RData", 
               "CAK_outputs_2023.RData", "LYD_outputs_2023.RData", "FIRE_outputs_2023.RData")

for (i in seq_along(all_model)) {
  load(all_model[i])
}

all_models <- list(BBD_outputs, HWA_outputs, EAB_outputs, CAK_outputs, LYD_outputs, FIRE_outputs)



for (i in 1:6) {
  names(all_models[[i]]) <- c("mean.max.canopy.ht", "deepgap.fraction", "LAI", "LAI_subcanopy", "top.rugosity", "gini")
}


BBD_plots <- unique(all_models[[1]]$"gini"$data$plotID)
HWA_plots <- unique(all_models[[2]]$"gini"$data$plotID)
EAB_plots <- unique(all_models[[3]]$"gini"$data$plotID)
CAK_plots <- unique(all_models[[4]]$"gini"$data$plotID)
SPM_plots <- unique(all_models[[5]]$"gini"$data$plotID)
FIRE_plots <- unique(all_models[[6]]$"gini"$data$plotID)


HARV_plots <- c(HWA_plots, SPM_plots)
unique(HARV_plots)
################################################################################
variable <- c("mean.max.canopy.ht", "deepgap.fraction", "LAI", "LAI_subcanopy","top.rugosity", "gini")
table_names <- c("BBD_test_2", "HWA_test_2", "EAB_test_2", "CAK_test_2", "LYD_test_2", "FIRE_test_2", "HURRI_test_3")


sjPlot::tab_model(BBD_outputs, p.threshold = c(0.1, 0.05, 0.01, 0.001), show.ci = F, digits = 5, p.style = "stars",
                  CSS = list(
                    # css.depvarhead = 'color: red;',
                    css.centeralign = 'text-align: left;', 
                    css.firsttablecol = 'font-weight: bold;'
                    # css.summary = 'color: blue;'
                  ))
sjPlot::tab_model(HWA_outputs, p.threshold = c(0.1, 0.05, 0.01, 0.001), show.ci = F, digits = 5, p.style = "stars",
                  CSS = list(
                    # css.depvarhead = 'color: red;',
                    css.centeralign = 'text-align: left;', 
                    css.firsttablecol = 'font-weight: bold;'
                    # css.summary = 'color: blue;'
                  ))
sjPlot::tab_model(EAB_outputs, p.threshold = c(0.1, 0.05, 0.01, 0.001), show.ci = F, digits = 5, p.style = "stars",
                  CSS = list(
                    # css.depvarhead = 'color: red;',
                    css.centeralign = 'text-align: left;', 
                    css.firsttablecol = 'font-weight: bold;'
                    # css.summary = 'color: blue;'
                  ))
sjPlot::tab_model(CAK_outputs, p.threshold = c(0.1, 0.05, 0.01, 0.001), show.ci = F, digits = 5, p.style = "stars",
                  CSS = list(
                    # css.depvarhead = 'color: red;',
                    css.centeralign = 'text-align: left;', 
                    css.firsttablecol = 'font-weight: bold;'
                    # css.summary = 'color: blue;'
                  ))
sjPlot::tab_model(LYD_outputs, p.threshold = c(0.1, 0.05, 0.01, 0.001), show.ci = F, digits = 5, p.style = "stars",
                  CSS = list(
                    # css.depvarhead = 'color: red;',
                    css.centeralign = 'text-align: left;', 
                    css.firsttablecol = 'font-weight: bold;'
                    # css.summary = 'color: blue;'
                  ))
sjPlot::tab_model(FIRE_outputs, p.threshold = c(0.1, 0.05, 0.01, 0.001), show.ci = F, digits = 5, p.style = "stars",
                  CSS = list(
                    # css.depvarhead = 'color: red;',
                    css.centeralign = 'text-align: left;', 
                    css.firsttablecol = 'font-weight: bold;'
                    # css.summary = 'color: blue;'
                  ))

require(MuMIn)

round(r.squaredGLMM(BBD_outputs[[1]])[1], 2)
round(r.squaredGLMM(HWA_outputs[[1]])[1], 2)

###################################Severity: TIME ##############################
# all_models[[6]]

all_plots <- vector("list")
plots <- vector("list")

for (n in 1:6) {
  model_outputs <- all_models[[n]]
  
  for (i in 1:5){
    model_sumamry  <- summary(model_outputs[[i]])
    model_table <- as.data.frame(model_sumamry$tTable)
    model_table <- tibble::rownames_to_column(model_table, "Term")
    # model_table
    
    variable_i <- variable[i]
    tables <- model_outputs$mean.max.canopy.ht$data
    
    BBD_test_2 <- tables
    HWA_test_2 <- tables
    EAB_test_2 <- tables
    CAK_test_2 <- tables 
    LYD_test_2 <- tables
    FIRE_test_2 <- tables
    HURRI_test_3 <- tables
    
    SLevels <- c(mean(tables$Severity)-sd(tables$Severity),
                 mean(tables$Severity),
                 mean(tables$Severity)+sd(tables$Severity))
    
    Results.Model_sev<-Effect(c("TIME","Severity"), model_outputs[[i]],
                              xlevels=list(TIME= unique(tables$TIME), 
                                           Severity=SLevels))
    
    Results.Model_sev <- as.data.frame(Results.Model_sev)
    
    Results.Model_sev$Support.F<-factor(Results.Model_sev$Severity,
                                        levels=SLevels,
                                        labels=c("Low", "Moderate", "High"))
    
    if (model_table$`p-value`[model_table$Term == "Severity:TIME"] < 0.01) {
      aaaa <- c(model_table$`p-value`[model_table$Term == "TIME:Severity"], 
                model_table$`p-value`[model_table$Term == "Severity:TIME"])
      a <- aaaa[which(aaaa != 0)]
      Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, aes(x = TIME, y =fit, group= Support.F))+
        geom_line(size=2, aes(color=Support.F, linetype = Support.F))+
        scale_linetype_manual(values=c("solid", "dotted", "solid"))+
        geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=Support.F, fill=Support.F), alpha=.2)+
        scale_colour_manual(values = c("#009e73", "grey", "#d55e00")) + 
        scale_fill_manual(values = c("#009e73", "grey", "#d55e00")) + 
        xlab("TIME")+
        theme_bw()+
        geom_hline(yintercept = 0, col = c("black")) +
        theme(legend.title=element_blank(),
              axis.ticks.length = unit(5, "pt"),
              legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.text.y = element_text(size = 20),
              axis.ticks.x=element_blank(),
              panel.background = element_rect(
                colour = "black",
                size = 4
              )) + 
        
        scale_y_continuous(labels = label_number(accuracy = 0.01)) + 
        annotate("text", x= min(tables$TIME), y= max(tables[, variable_i])/1.2, hjust= 0, vjust= 1, label = paste0("p < 0.01 "), size = 7)

    } else if (model_table$`p-value`[model_table$Term == "Severity:TIME"] < 0.055 & model_table$`p-value`[model_table$Term == "Severity:TIME"] >= 0.01) {
      aaaa <- c(model_table$`p-value`[model_table$Term == "TIME:Severity"], 
                model_table$`p-value`[model_table$Term == "Severity:TIME"])
      a <- aaaa[which(aaaa != 0)]
      Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, 
                                  aes(x = TIME, y =fit, group= Support.F))+
        geom_line(size=2, aes(color=Support.F, linetype = Support.F))+
        scale_linetype_manual(values=c("solid", "dotted", "solid"))+
        geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=Support.F, fill=Support.F), alpha=.2)+
        scale_colour_manual(values = c("#009e73", "grey", "#d55e00")) + 
        scale_fill_manual(values = c("#009e73", "grey", "#d55e00")) + 
        xlab("TIME")+
        theme_bw()+
        geom_hline(yintercept = 0,
                   col = c("black")) +
        theme(legend.title=element_blank(),
              axis.ticks.length = unit(5, "pt"),
              legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.text.y = element_text(size = 20),
              axis.ticks.x=element_blank(),
              panel.background = element_rect(
                colour = "black",
                size = 4
              )) + 
        
        scale_y_continuous(labels = label_number(accuracy = 0.01)) + 
        annotate("text", 
                 x= min(tables$TIME), 
                 y= max(tables[, variable_i])/1.2, 
                 hjust= 0, 
                 vjust= 1, 
                 label = paste0("p = ", round(a, 2)), size = 7)
      
    } else if (model_table$`p-value`[model_table$Term == "Severity:TIME"] < 0.1 & model_table$`p-value`[model_table$Term == "Severity:TIME"] >= 0.055) {
      aaaa <- c(model_table$`p-value`[model_table$Term == "TIME:Severity"], 
                model_table$`p-value`[model_table$Term == "Severity:TIME"])
      a <- aaaa[which(aaaa != 0)]
      Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, 
                                  aes(x = TIME, y =fit, group= Support.F))+
        geom_line(size=2, aes(color=Support.F, linetype = Support.F))+
        scale_linetype_manual(values=c("solid", "dotted", "solid"))+
        geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=Support.F, fill=Support.F), alpha=.2)+
        scale_colour_manual(values = c("#009e73", "grey", "#d55e00")) + 
        scale_fill_manual(values = c("#009e73", "grey", "#d55e00")) + 
        xlab("TIME")+
        theme_bw()+
        geom_hline(yintercept = 0,
                   col = c("black")) +
        theme(legend.title=element_blank(),
              axis.ticks.length = unit(5, "pt"),
              legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.text.y = element_text(size = 20),
              axis.ticks.x=element_blank(),
              # panel.background = element_rect(
              #   colour = "black",
              #   size = 4)
              ) + 
        
        scale_y_continuous(labels = label_number(accuracy = 0.01)) + 
        annotate("text", 
                 x= min(tables$TIME), 
                 y= max(tables[, variable_i])/1.2, 
                 hjust= 0, 
                 vjust= 1, 
                 label = paste0("p < 0.1"), size = 7)
        
    } else {
      Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, aes(x = TIME, y =fit, group= Support.F))+
        geom_line(size=2, aes(color=Support.F, linetype = Support.F))+
        scale_linetype_manual(values=c("solid", "dotted", "solid"))+
        geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=Support.F, fill=Support.F), alpha=.2)+
        scale_colour_manual(values = c("#009e73", "grey", "#d55e00")) + 
        scale_fill_manual(values = c("#009e73", "grey", "#d55e00")) + 
        xlab("TIME")+
        theme_bw()+
        geom_hline(yintercept = 0,
                   col = c("black")) +
        scale_y_continuous(labels = label_number(accuracy = 0.01)) + 
        theme(legend.position = "none", 
              legend.title=element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_text(size = 20),
              axis.ticks.x=element_blank(),
              axis.text.x=element_blank())
    }
    plots[[i]] <- Final.Fixed.Plot.1
    plot(Final.Fixed.Plot.1)
  }
  
  for (i in 6) {
    model_sumamry  <- summary(model_outputs[[i]])
    model_table <- as.data.frame(model_sumamry$tTable)
    model_table <- tibble::rownames_to_column(model_table, "Term")

    variable_i <- variable[i]
    tables <- model_outputs$mean.max.canopy.ht$data
    
    BBD_test_2 <- tables
    HWA_test_2 <- tables
    EAB_test_2 <- tables
    CAK_test_2 <- tables 
    LYD_test_2 <- tables
    FIRE_test_2 <- tables

    SLevels <- c(mean(tables$Severity)-sd(tables$Severity),
                 mean(tables$Severity),
                 mean(tables$Severity)+sd(tables$Severity))
    
    Results.Model_sev<-Effect(c("TIME","Severity"), model_outputs[[i]],
                              xlevels=list(TIME= unique(tables$TIME), 
                                           Severity=SLevels))
    
    Results.Model_sev <- as.data.frame(Results.Model_sev)
    
    
    Results.Model_sev$Support.F<-factor(Results.Model_sev$Severity,
                                        levels=SLevels,
                                        labels=c("Low", "Moderate", "High"))
    
    
    if (model_table$`p-value`[model_table$Term == "Severity:TIME"] < 0.01) {
      aaaa <- c(model_table$`p-value`[model_table$Term == "TIME:Severity"], 
                model_table$`p-value`[model_table$Term == "Severity:TIME"])
      a <- aaaa[which(aaaa != 0)]
      Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, 
                                  aes(x = TIME, y =fit, group= Support.F))+
        geom_line(size=2, aes(color=Support.F, linetype = Support.F))+
        scale_linetype_manual(values=c("solid", "dotted", "solid"))+
        geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=Support.F, fill=Support.F), alpha=.2)+
        scale_colour_manual(values = c("#009e73", "grey", "#d55e00")) + 
        scale_fill_manual(values = c("#009e73", "grey", "#d55e00")) + 
        xlab("TIME")+
        theme_bw()+
        geom_hline(yintercept = 0,
                   col = c("black")) +
        theme(legend.title=element_blank(),
              axis.ticks.length = unit(5, "pt"),
              legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              # axis.text.x=element_blank(),
              axis.text.x = element_text(size = 20),
              axis.text.y = element_text(size = 20),
              axis.ticks.x=element_blank(),
              panel.background = element_rect(
                # fill = "black",
                colour = "black",
                size = 4
              )) + 
        
        scale_y_continuous(labels = label_number(accuracy = 0.01)) + 
        annotate("text", 
                 x= min(tables$TIME), 
                 y= max(tables[, variable_i])/1.2, 
                 hjust= 0, 
                 vjust= 1, 
                 label = paste0("p < 0.01"), 
                 size = 7)
    
    } else if (model_table$`p-value`[model_table$Term == "Severity:TIME"] < 0.055 & model_table$`p-value`[model_table$Term == "Severity:TIME"] >= 0.01) {
      aaaa <- c(model_table$`p-value`[model_table$Term == "TIME:Severity"], model_table$`p-value`[model_table$Term == "Severity:TIME"])
      a <- aaaa[which(aaaa != 0)]
      Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, aes(x = TIME, y =fit, group= Support.F))+
        geom_line(size=2, aes(color=Support.F, linetype = Support.F))+
        scale_linetype_manual(values=c("solid", "dotted", "solid"))+
        geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=Support.F, fill=Support.F), alpha=.2)+
        scale_colour_manual(values = c("#009e73", "grey", "#d55e00")) + 
        scale_fill_manual(values = c("#009e73", "grey", "#d55e00")) + 
        xlab("TIME")+
        theme_bw()+
        geom_hline(yintercept = 0,
                   col = c("black")) +
        theme(legend.title=element_blank(),
              axis.ticks.length = unit(5, "pt"),
              legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_text(size = 20),
              axis.text.x = element_text(size = 20),
              # axis.text.x=element_blank(),
              panel.background = element_rect(
                # fill = "black",
                colour = "black",
                size = 4
              )) + 
        
        scale_y_continuous(labels = label_number(accuracy = 0.01)) + 
        annotate("text", x= min(tables$TIME), y= max(tables[, variable_i])/1.2, hjust= 0, vjust= 1, label = paste0("p = ", round(a, 2)), size = 7)
      # print(Final.Fixed.Plot.1)
    
      
    } else if (model_table$`p-value`[model_table$Term == "Severity:TIME"] < 0.1 & model_table$`p-value`[model_table$Term == "Severity:TIME"] >= 0.055) {
      aaaa <- c(model_table$`p-value`[model_table$Term == "TIME:Severity"], model_table$`p-value`[model_table$Term == "Severity:TIME"])
      a <- aaaa[which(aaaa != 0)]
      Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, 
                                  aes(x = TIME, y =fit, group= Support.F))+
        geom_line(size=2, aes(color=Support.F, linetype = Support.F))+
        scale_linetype_manual(values=c("solid", "dotted", "solid"))+
        # facet_wrap(~pc) + 
        geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=Support.F, fill=Support.F), alpha=.2)+
        scale_colour_manual(values = c("#009e73", "grey", "#d55e00")) + 
        scale_fill_manual(values = c("#009e73", "grey", "#d55e00")) + 
        xlab("TIME")+
        # ylab(paste0("Changes in ", variable_i))+ 
        theme_bw()+
        # geom_hline(yintercept = c(unique(tables[, variable][which(tables$TIME ==3)])), 
        #            col = c("black")) +
        geom_hline(yintercept = 0,
                   col = c("black")) +
        theme(legend.title=element_blank(),
              axis.ticks.length = unit(5, "pt"),
              legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_text(size = 20),
              axis.text.x = element_text(size = 20)
              # axis.text.x=element_blank(),
              # panel.background = element_rect(
              #   # fill = "black",
              #   colour = "black",
              #   size = 4)
              ) + 
        
        scale_y_continuous(labels = label_number(accuracy = 0.01)) + 
        annotate("text", 
                 x= min(tables$TIME), 
                 y= max(tables[, variable_i])/1.2, hjust= 0, vjust= 1, 
                 label = paste0("p < 0.1"), 
                 size = 7)  
      
        
    } else {
      Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, 
                                  aes(x = TIME, y =fit, group= Support.F))+
        geom_line(size=2, aes(color=Support.F, linetype = Support.F))+
        scale_linetype_manual(values=c("solid", "dotted", "solid"))+
        # facet_wrap(~pc) + 
        geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=Support.F, fill=Support.F), alpha=.2)+
        scale_colour_manual(values = c("#009e73", "grey", "#d55e00")) + 
        scale_fill_manual(values = c("#009e73", "grey", "#d55e00")) + 
        xlab("TIME")+
        # ylab(paste0("Changes in ", variable_i))+ 
        theme_bw()+
        # geom_hline(yintercept = c(unique(tables[, variable][which(tables$TIME ==3)])), 
        #            col = c("black")) +
        geom_hline(yintercept = 0,
                   col = c("black")) +
        scale_y_continuous(labels = label_number(accuracy = 0.01)) + 
        theme(legend.position = "none", 
              legend.title=element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_text(size = 20),
              axis.text.y = element_text(size = 20),
              # axis.text.x=element_blank(),
              axis.title.y = element_blank())
      # print(Final.Fixed.Plot.1)
    }
    plots[[i]] <- Final.Fixed.Plot.1
    # jpeg(paste0(variable_i,"_EAB_severity_0630_Den", ".jpg"))
    plot(Final.Fixed.Plot.1)
    # dev.off()
  }
  all_plots[[n]] <- plots
}

library(gridExtra)
plots2 <- vector("list")
for (i in 1:6) {
  plots2[[i]] <- do.call("grid.arrange", c(all_plots[[i]], ncol = 1))
}

results_Severity <- do.call("grid.arrange", c(plots2, ncol = 6))

jpeg(paste0("results_Severity_new_2023.jpg"), width = 21, height = 18, units = 'in', res = 300)
plot(results_Severity)
dev.off()


###################################  ICC: TIME #################################
all_plots <- vector("list")
plots <- vector("list")

for ( n in 1:6) {
  model_outputs <- all_models[[n]]
  
  for (i in 1:5){
    model_sumamry  <- summary(model_outputs[[i]])
    model_table <- as.data.frame(model_sumamry$tTable)
    model_table <- tibble::rownames_to_column(model_table, "Term")
    # model_table
    
    variable_i <- variable[i]
    tables <- model_outputs$mean.max.canopy.ht$data
    
    BBD_test_2 <- tables
    HWA_test_2 <- tables
    EAB_test_2 <- tables
    CAK_test_2 <- tables 
    LYD_test_2 <- tables
    FIRE_test_2 <- tables
    HURRI_test_3 <- tables
    
    SLevels1<-c(mean(tables$top.rugosity_0)-sd(tables$top.rugosity_0),
                mean(tables$top.rugosity_0),
                mean(tables$top.rugosity_0)+sd(tables$top.rugosity_0))
    #extract fixed effects
    
    Results.Model_top<-Effect(c("TIME", "top.rugosity_0"), model_outputs[[i]],
                              xlevels=list(TIME= unique(tables$TIME), 
                                           top.rugosity_0 = SLevels1))
    Results.Model_top<-as.data.frame(Results.Model_top)
    
    Results.Model_top$pc<-factor(Results.Model_top$top.rugosity_0,
                                 levels=SLevels1,
                                 labels=c("Low Complexity", "Medium Complexity", "High Complexity"))
    
    if (model_table$`p-value`[model_table$Term == "TIME:top.rugosity_0" | model_table$Term == "top.rugosity_0:TIME"] < 0.01) {
      aaaa <- c(model_table$`p-value`[model_table$Term == "TIME:Severity"], 
                model_table$`p-value`[model_table$Term == "Severity:TIME"])
      a <- aaaa[which(aaaa != 0)]
      Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, aes(x = TIME, y =fit, group= Support.F))+
        geom_line(size=2, aes(color=Support.F, linetype = Support.F))+
        scale_linetype_manual(values=c("solid", "dotted", "solid"))+
        geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=Support.F, fill=Support.F), alpha=.2)+
        scale_colour_manual(values = c("#000000", "grey", "#d4660a")) + 
        scale_fill_manual(values = c("#000000", "grey", "#d4660a")) + 
        xlab("TIME")+
        theme_bw()+
        geom_hline(yintercept = 0, col = c("black")) +
        theme(legend.title=element_blank(),
              axis.ticks.length = unit(5, "pt"),
              legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.text.y = element_text(size = 20),
              axis.ticks.x=element_blank(),
              panel.background = element_rect(
                colour = "black",
                size = 4
              )) + 
        
        scale_y_continuous(labels = label_number(accuracy = 0.01)) + 
        annotate("text", 
                 x= min(tables$TIME), 
                 y= max(tables[, variable_i])/1.2, 
                 hjust= 0, 
                 vjust= 1, 
                 label = paste0("p < 0.01"), size = 7)
    
    
    } else if ((model_table$`p-value`[model_table$Term == "TIME:top.rugosity_0" | model_table$Term == "top.rugosity_0:TIME"] >= 0.01) &
               (model_table$`p-value`[model_table$Term == "TIME:top.rugosity_0" | model_table$Term == "top.rugosity_0:TIME"] < 0.055)) {
      aaaa <- c(model_table$`p-value`[model_table$Term == "TIME:top.rugosity_0"], model_table$`p-value`[model_table$Term == "top.rugosity_0:TIME"])
      a <- aaaa[which(aaaa != 0)]
      Final.Fixed.Plot.1 <-ggplot(data = Results.Model_top, 
                                  aes(x = TIME, y =fit, group= pc))+
        geom_line(size=2, aes(color=pc, linetype = pc))+
        scale_linetype_manual(values=c("solid", "dotted", "solid"))+
        # facet_wrap(~pc) + 
        geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=pc, fill=pc), alpha=.2)+
        scale_colour_manual(values = c("#000000", "grey", "#d4660a")) + 
        scale_fill_manual(values = c("#000000", "grey", "#d4660a")) + 
        xlab("TIME")+
        ylab(paste0("Changes in ", variable_i))+ theme_bw()+
        # geom_hline(yintercept = c(unique(tables[, variable][which(tables$TIME ==3)])), 
        #            col = c("black")) +
        geom_hline(yintercept = 0,
                   col = c("black")) +
        theme(legend.title=element_blank(),
              axis.ticks.length = unit(5, "pt"),
              legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.text.y = element_text(size = 20),
              axis.ticks.x=element_blank(),
              panel.background = element_rect(
                # fill = "black",
                colour = "black",
                size = 4
              )) + 
        scale_y_continuous(labels = label_number(accuracy = 0.01)) + 
        annotate("text", 
                 x= min(tables$TIME), 
                 y= max(tables[, variable_i])/1.2, hjust= 0, 
                 vjust= 1, 
                 label = paste0("p = ", round(a, 2)), 
                 size = 7)
      print(Final.Fixed.Plot.1)
    
      
    } else if ((model_table$`p-value`[model_table$Term == "TIME:top.rugosity_0" | model_table$Term == "top.rugosity_0:TIME"] >= 0.055) &
               (model_table$`p-value`[model_table$Term == "TIME:top.rugosity_0" | model_table$Term == "top.rugosity_0:TIME"] < 0.1)) {
      aaaa <- c(model_table$`p-value`[model_table$Term == "TIME:top.rugosity_0"], model_table$`p-value`[model_table$Term == "top.rugosity_0:TIME"])
      a <- aaaa[which(aaaa != 0)]
      Final.Fixed.Plot.1 <-ggplot(data = Results.Model_top, 
                                  aes(x = TIME, y =fit, group= pc))+
        geom_line(size=2, aes(color=pc, linetype = pc))+
        scale_linetype_manual(values=c("solid", "dotted", "solid"))+
        # facet_wrap(~pc) + 
        geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=pc, fill=pc), alpha=.2)+
        scale_colour_manual(values = c("#000000", "grey", "#d4660a")) + 
        scale_fill_manual(values = c("#000000", "grey", "#d4660a")) + 
        xlab("TIME")+
        ylab(paste0("Changes in ", variable_i))+ theme_bw()+
        # geom_hline(yintercept = c(unique(tables[, variable][which(tables$TIME ==3)])), 
        #            col = c("black")) +
        geom_hline(yintercept = 0,
                   col = c("black")) +
        theme(legend.title=element_blank(),
              axis.ticks.length = unit(5, "pt"),
              legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.text.y = element_text(size = 20),
              axis.ticks.x=element_blank()) + 
        scale_y_continuous(labels = label_number(accuracy = 0.01)) + 
        annotate("text", 
                 x= min(tables$TIME), 
                 y= max(tables[, variable_i])/1.2, 
                 hjust= 0, vjust= 1, 
                 label = paste0("p < 0.1"), size = 7)
      print(Final.Fixed.Plot.1)  
        
    } else {
      Final.Fixed.Plot.1 <-ggplot(data = Results.Model_top, 
                                  aes(x = TIME, y =fit, group= pc))+
        geom_line(size=2, aes(color=pc, linetype = pc))+
        scale_linetype_manual(values=c("solid", "dotted", "solid"))+
        # facet_wrap(~pc) + 
        geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=pc, fill=pc), alpha=.2)+
        scale_colour_manual(values = c("#000000", "grey", "#d4660a")) + 
        scale_fill_manual(values = c("#000000", "grey", "#d4660a")) + 
        xlab("TIME")+
        ylab(paste0("Changes in ", variable_i))+ theme_bw()+
        # geom_hline(yintercept = c(unique(tables[, variable][which(tables$TIME ==3)])), 
        #            col = c("black")) +
        geom_hline(yintercept = 0,
                   col = c("black")) +
        scale_y_continuous(labels = label_number(accuracy = 0.01)) + 
        theme(legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.y = element_text(size = 20),)
    }
    plots[[i]] <- Final.Fixed.Plot.1
    # jpeg(paste0(variable_i,"_EAB_severity_0630_Den", ".jpg"))
    plot(Final.Fixed.Plot.1)
    # dev.off()
  }
  for ( i in 6) {
    model_sumamry  <- summary(model_outputs[[i]])
    model_table <- as.data.frame(model_sumamry$tTable)
    model_table <- tibble::rownames_to_column(model_table, "Term")
    # model_table
    
    variable_i <- variable[i]
    tables <- model_outputs$mean.max.canopy.ht$data
    
    BBD_test_2 <- tables
    HWA_test_2 <- tables
    EAB_test_2 <- tables
    CAK_test_2 <- tables 
    LYD_test_2 <- tables
    FIRE_test_2 <- tables
    HURRI_test_3 <- tables
    
    SLevels1<-c(mean(tables$top.rugosity_0)-sd(tables$top.rugosity_0),
                mean(tables$top.rugosity_0),
                mean(tables$top.rugosity_0)+sd(tables$top.rugosity_0))
    #extract fixed effects
    
    Results.Model_top<-Effect(c("TIME", "top.rugosity_0"), model_outputs[[i]],
                              xlevels=list(TIME= unique(tables$TIME), 
                                           top.rugosity_0 = SLevels1))
    Results.Model_top<-as.data.frame(Results.Model_top)
    
    Results.Model_top$pc<-factor(Results.Model_top$top.rugosity_0,
                                 levels=SLevels1,
                                 labels=c("Low Complexity", "Medium Complexity", "High Complexity"))
    
    if (model_table$`p-value`[model_table$Term == "TIME:top.rugosity_0" | model_table$Term == "top.rugosity_0:TIME"] < 0.01) {
      aaaa <- c(model_table$`p-value`[model_table$Term == "TIME:Severity"], 
                model_table$`p-value`[model_table$Term == "Severity:TIME"])
      a <- aaaa[which(aaaa != 0)]
      Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, aes(x = TIME, y =fit, group= Support.F))+
        geom_line(size=2, aes(color=Support.F, linetype = Support.F))+
        scale_linetype_manual(values=c("solid", "dotted", "solid"))+
        geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=Support.F, fill=Support.F), alpha=.2)+
        scale_colour_manual(values = c("#009e73", "grey", "#d55e00")) + 
        scale_fill_manual(values = c("#009e73", "grey", "#d55e00")) + 
        xlab("TIME")+
        theme_bw()+
        geom_hline(yintercept = 0, col = c("black")) +
        theme(legend.title=element_blank(),
              axis.ticks.length = unit(5, "pt"),
              legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.text.y = element_text(size = 20),
              axis.ticks.x=element_blank(),
              panel.background = element_rect(
                colour = "black",
                size = 4
              )) + 
        
        scale_y_continuous(labels = label_number(accuracy = 0.01)) + 
        annotate("text", 
                 x= min(tables$TIME), 
                 y= max(tables[, variable_i])/1.2, 
                 hjust= 0, 
                 vjust= 1, 
                 label = paste0("p < 0.01"), 
                 size = 7)
    
    
    } else if ((model_table$`p-value`[model_table$Term == "TIME:top.rugosity_0" | model_table$Term == "top.rugosity_0:TIME"] >= 0.01) &
        (model_table$`p-value`[model_table$Term == "TIME:top.rugosity_0" | model_table$Term == "top.rugosity_0:TIME"] < 0.055)) {
      aaaa <- c(model_table$`p-value`[model_table$Term == "TIME:top.rugosity_0"], model_table$`p-value`[model_table$Term == "top.rugosity_0:TIME"])
      a <- aaaa[which(aaaa != 0)]
      Final.Fixed.Plot.1 <-ggplot(data = Results.Model_top, 
                                  aes(x = TIME, y =fit, group= pc))+
        geom_line(size=2, aes(color=pc, linetype = pc))+
        scale_linetype_manual(values=c("solid", "dotted", "solid"))+
        # facet_wrap(~pc) + 
        geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=pc, fill=pc), alpha=.2)+
        scale_colour_manual(values = c("#000000", "grey", "#d4660a")) + 
        scale_fill_manual(values = c("#000000", "grey", "#d4660a")) + 
        xlab("TIME")+
        ylab(paste0("Changes in ", variable_i))+ theme_bw()+
        # geom_hline(yintercept = c(unique(tables[, variable][which(tables$TIME ==3)])), 
        #            col = c("black")) +
        geom_hline(yintercept = 0,
                   col = c("black")) +
        theme(legend.title=element_blank(),
              axis.ticks.length = unit(5, "pt"),
              legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              # axis.text.x=element_blank(),
              axis.text.y = element_text(size = 20),
              axis.text.x = element_text(size = 20),
              # axis.ticks.x=element_blank(),
              panel.background = element_rect(
                # fill = "black",
                colour = "black",
                size = 4
              )) + 
        scale_y_continuous(labels = label_number(accuracy = 0.01)) + 
        annotate("text", 
                 x= min(tables$TIME), 
                 y= max(tables[, variable_i])/1.2, 
                 hjust= 0, 
                 vjust= 1, 
                 label = paste0("p = ", round(a, 2)), size = 7)
      print(Final.Fixed.Plot.1)
    
    } else if ((model_table$`p-value`[model_table$Term == "TIME:top.rugosity_0" | model_table$Term == "top.rugosity_0:TIME"] >= 0.055) &
               (model_table$`p-value`[model_table$Term == "TIME:top.rugosity_0" | model_table$Term == "top.rugosity_0:TIME"] < 0.1)) {
      aaaa <- c(model_table$`p-value`[model_table$Term == "TIME:top.rugosity_0"], model_table$`p-value`[model_table$Term == "top.rugosity_0:TIME"])
      a <- aaaa[which(aaaa != 0)]
      Final.Fixed.Plot.1 <-ggplot(data = Results.Model_top, 
                                  aes(x = TIME, y =fit, group= pc))+
        geom_line(size=2, aes(color=pc, linetype = pc))+
        scale_linetype_manual(values=c("solid", "dotted", "solid"))+
        # facet_wrap(~pc) + 
        geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=pc, fill=pc), alpha=.2)+
        scale_colour_manual(values = c("#000000", "grey", "#d4660a")) + 
        scale_fill_manual(values = c("#000000", "grey", "#d4660a")) + 
        xlab("TIME")+
        ylab(paste0("Changes in ", variable_i))+ theme_bw()+
        # geom_hline(yintercept = c(unique(tables[, variable][which(tables$TIME ==3)])), 
        #            col = c("black")) +
        geom_hline(yintercept = 0,
                   col = c("black")) +
        theme(legend.title=element_blank(),
              axis.ticks.length = unit(5, "pt"),
              legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              # axis.text.x=element_blank(),
              axis.text.y = element_text(size = 20),
              axis.text.x = element_text(size = 20)) + 
        scale_y_continuous(labels = label_number(accuracy = 0.01)) + 
        annotate("text", 
                 x= min(tables$TIME), 
                 y= max(tables[, variable_i])/1.2, 
                 hjust= 0, 
                 vjust= 1, 
                 label = paste0("p < 0.1"), size = 7)
      print(Final.Fixed.Plot.1)  
      
        
    } else {
      Final.Fixed.Plot.1 <-ggplot(data = Results.Model_top, 
                                  aes(x = TIME, y =fit, group= pc))+
        geom_line(size=2, aes(color=pc, linetype = pc))+
        scale_linetype_manual(values=c("solid", "dotted", "solid"))+
        # facet_wrap(~pc) + 
        geom_ribbon(aes(ymin= fit-se, ymax= fit+se, group=pc, fill=pc), alpha=.2)+
        scale_colour_manual(values = c("#000000", "grey", "#d4660a")) + 
        scale_fill_manual(values = c("#000000", "grey", "#d4660a")) + 
        xlab("TIME")+
        ylab(paste0("Changes in ", variable_i))+ theme_bw()+
        # geom_hline(yintercept = c(unique(tables[, variable][which(tables$TIME ==3)])), 
        #            col = c("black")) +
        geom_hline(yintercept = 0,
                   col = c("black")) +
        scale_y_continuous(labels = label_number(accuracy = 0.01)) + 
        theme(legend.position = "none",
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              # axis.text.x=element_blank(),
              axis.text.y = element_text(size = 20),
              axis.text.x = element_text(size = 20)
              # axis.ticks.x=element_blank(),
              )
    }
    plots[[i]] <- Final.Fixed.Plot.1
    # jpeg(paste0(variable_i,"_EAB_severity_0630_Den", ".jpg"))
    plot(Final.Fixed.Plot.1)
    # dev.off()
  }
  all_plots[[n]] <- plots
}

library(gridExtra)
plots2 <- vector("list")
for (i in 1:6) {
  plots2[[i]] <- do.call("grid.arrange", c(all_plots[[i]], ncol = 1))
}

results_ICC <- do.call("grid.arrange", c(plots2, ncol = 6))

jpeg(paste0("results_ICC_new_2023.jpg"), width = 21, height = 18, units = 'in', res = 300)
plot(results_ICC)
dev.off()



##################only time ###################################################'

all_plots <- vector("list")
plots <- vector("list")

for ( n in 1:6) {
  model_outputs <- all_models[[n]]
  
  for (i in 1:5){
    model_sumamry  <- summary(model_outputs[[i]])
    model_table <- as.data.frame(model_sumamry$tTable)
    model_table <- tibble::rownames_to_column(model_table, "Term")
    # model_table
    
    variable_i <- variable[i]
    tables <- model_outputs$mean.max.canopy.ht$data
    
    BBD_test_2 <- tables
    HWA_test_2 <- tables
    EAB_test_2 <- tables
    CAK_test_2 <- tables 
    LYD_test_2 <- tables
    FIRE_test_2 <- tables
    HURRI_test_3 <- tables
    
    
    Results.Model_sev<-Effect(c("TIME"), model_outputs[[i]],
                              xlevels=list(TIME= unique(tables$TIME)))
    
    Results.Model_sev <- as.data.frame(Results.Model_sev)
    
    if (length(model_table$`p-value`[model_table$Term == "poly(TIME, 3, raw = TRUE)3"] < 0.05) != 0) {
      if (model_table$`p-value`[model_table$Term == "poly(TIME, 3, raw = TRUE)3"] < 0.05) {
        # aaaa <- c(model_table$`p-value`[model_table$Term == "poly(TIME, 3, raw = TRUE)3"])
        # a <- aaaa[which(aaaa != 0)]
        
        Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, 
                                    aes(x = TIME, y =fit))+
          geom_line(size=2, alpha = 0.8, aes(colour = "gren"))+
          # facet_wrap(~pc) + 
          geom_ribbon(aes(ymin= fit-se, ymax= fit+se), alpha=.2)+
          # scale_colour_manual(values = c("blue", "grey", "red")) + 
          # scale_fill_manual(values = c("blue", "grey", "red")) + 
          xlab("TIME")+
          ylab(paste0("Changes in ", variable_i))+ theme_bw()+
          # geom_hline(yintercept = c(unique(tables[, variable][which(tables$TIME ==3)])), 
          #            col = c("black")) +
          geom_hline(yintercept = 0,
                     col = c("black")) +
          theme(legend.title=element_blank(),
                axis.ticks.length = unit(5, "pt"),
                legend.position = "none",
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x=element_blank(),
                # axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20),
                panel.background = element_rect(
                  # fill = "black",
                  colour = "black",
                  size = 4
                )) + 
          scale_y_continuous(labels = label_number(accuracy = 0.01)) +
          annotate("text", x= min(tables$TIME), y= max(tables[, variable_i])/2, hjust= 0, vjust= 1,
                   label = paste0("(", round(r.squaredGLMM(model_outputs[[i]])[1], 2), ", ",round(r.squaredGLMM(model_outputs[[i]])[2], 2), ")"), size = 7)+
          theme(legend.position = "none", 
                legend.title=element_blank(),
                axis.title.x = element_blank(),
                axis.ticks.x=element_blank())
        # print(Final.Fixed.Plot.1)
        
      } else {
        Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, 
                                    aes(x = TIME, y =fit))+
          geom_line(size=2, alpha = 0.8, aes(colour = "gren"))+
          # facet_wrap(~pc) + 
          geom_ribbon(aes(ymin= fit-se, ymax= fit+se), alpha=.2)+
          # scale_colour_manual(values = c("blue", "grey", "red")) + 
          # scale_fill_manual(values = c("blue", "grey", "red")) + 
          xlab("TIME")+
          ylab(paste0("Changes in ", variable_i))+ theme_bw()+
          # geom_hline(yintercept = c(unique(tables[, variable][which(tables$TIME ==3)])), 
          #            col = c("black")) +
          geom_hline(yintercept = 0,
                     col = c("black")) +
          scale_y_continuous(labels = label_number(accuracy = 0.01)) + 
          annotate("text", x= min(tables$TIME), y= max(tables[, variable_i])/2, hjust= 0, vjust= 1,
                   label = paste0("(", round(r.squaredGLMM(model_outputs[[i]])[1], 2), ", ",round(r.squaredGLMM(model_outputs[[i]])[2], 2), ")"), size = 7)+
          theme(legend.position = "none", 
                legend.title=element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x=element_blank(),
                # axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20),
                axis.ticks.x=element_blank())
        # print(Final.Fixed.Plot.1)
      }
      
    } else if (length(model_table$`p-value`[model_table$Term == "poly(TIME, 2, raw = TRUE)2"] < 0.05) != 0) {
      if (model_table$`p-value`[model_table$Term == "poly(TIME, 2, raw = TRUE)2"] < 0.05) {
        # aaaa <- c(model_table$`p-value`[model_table$Term == "poly(TIME, 3, raw = TRUE)3"])
        # a <- aaaa[which(aaaa != 0)]
        
        Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, 
                                    aes(x = TIME, y =fit))+
          geom_line(size=2, alpha = 0.8, aes(colour = "gren"))+
          # facet_wrap(~pc) + 
          geom_ribbon(aes(ymin= fit-se, ymax= fit+se), alpha=.2)+
          # scale_colour_manual(values = c("blue", "grey", "red")) + 
          # scale_fill_manual(values = c("blue", "grey", "red")) + 
          xlab("TIME")+
          ylab(paste0("Changes in ", variable_i))+ theme_bw()+
          # geom_hline(yintercept = c(unique(tables[, variable][which(tables$TIME ==3)])), 
          #            col = c("black")) +
          geom_hline(yintercept = 0,
                     col = c("black")) +
          theme(legend.title=element_blank(),
                axis.ticks.length = unit(5, "pt"),
                legend.position = "none",
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x=element_blank(),
                # axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20),
                axis.ticks.x=element_blank(),
                panel.background = element_rect(
                  # fill = "black",
                  colour = "black",
                  size = 4
                )) + 
          scale_y_continuous(labels = label_number(accuracy = 0.01)) +
          annotate("text", x= min(tables$TIME), y= max(tables[, variable_i])/2, hjust= 0, vjust= 1,
                   label = paste0("(", round(r.squaredGLMM(model_outputs[[i]])[1], 2), ", ",round(r.squaredGLMM(model_outputs[[i]])[2], 2), ")"), size = 7)+
          theme(legend.position = "none", 
                legend.title=element_blank(),
                axis.title.x = element_blank())
        # annotate("text", x = mean(tables$TIME) * 1.15, y = mean(tables[, variable_i]) / 10, label = paste0("p = ", round(a, 5)), size = 7)
        # print(Final.Fixed.Plot.1)
        
      } else {
        Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, 
                                    aes(x = TIME, y =fit))+
          geom_line(size=2, alpha = 0.8, aes(colour = "gren"))+
          # facet_wrap(~pc) + 
          geom_ribbon(aes(ymin= fit-se, ymax= fit+se), alpha=.2)+
          # scale_colour_manual(values = c("blue", "grey", "red")) + 
          # scale_fill_manual(values = c("blue", "grey", "red")) + 
          xlab("TIME")+
          ylab(paste0("Changes in ", variable_i))+ theme_bw()+
          # geom_hline(yintercept = c(unique(tables[, variable][which(tables$TIME ==3)])), 
          #            col = c("black")) +
          geom_hline(yintercept = 0,
                     col = c("black")) +
          scale_y_continuous(labels = label_number(accuracy = 0.01)) + 
          annotate("text", x= min(tables$TIME), y= max(tables[, variable_i])/2, hjust= 0, vjust= 1,
                   label = paste0("(", round(r.squaredGLMM(model_outputs[[i]])[1], 2), ", ",round(r.squaredGLMM(model_outputs[[i]])[2], 2), ")"), size = 7)+
          theme(legend.position = "none", 
                legend.title=element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x=element_blank(),
                # axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20),
                axis.ticks.x=element_blank())
        # print(Final.Fixed.Plot.1)
      }
      
    } else if (length(model_table$`p-value`[model_table$Term == "TIME"] < 0.05) != 0) {
      if (model_table$`p-value`[model_table$Term == "TIME"] < 0.05) {
        # aaaa <- c(model_table$`p-value`[model_table$Term == "poly(TIME, 3, raw = TRUE)3"])
        # a <- aaaa[which(aaaa != 0)]
        
        Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, 
                                    aes(x = TIME, y =fit))+
          geom_line(size=2, alpha = 0.8, aes(colour = "gren"))+
          # facet_wrap(~pc) + 
          geom_ribbon(aes(ymin= fit-se, ymax= fit+se), alpha=.2)+
          # scale_colour_manual(values = c("blue", "grey", "red")) + 
          # scale_fill_manual(values = c("blue", "grey", "red")) + 
          xlab("TIME")+
          ylab(paste0("Changes in ", variable_i))+ theme_bw()+
          # geom_hline(yintercept = c(unique(tables[, variable][which(tables$TIME ==3)])), 
          #            col = c("black")) +
          geom_hline(yintercept = 0,
                     col = c("black")) +
          theme(legend.title=element_blank(),
                axis.ticks.length = unit(5, "pt"),
                legend.position = "none",
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x=element_blank(),
                # axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20),
                axis.ticks.x=element_blank(),
                panel.background = element_rect(
                  # fill = "black",
                  colour = "black",
                  size = 4
                )) + 
          scale_y_continuous(labels = label_number(accuracy = 0.01)) +
          annotate("text", x= min(tables$TIME), y= max(tables[, variable_i])/2, hjust= 0, vjust= 1,
                   label = paste0("(", round(r.squaredGLMM(model_outputs[[i]])[1], 2), ", ",round(r.squaredGLMM(model_outputs[[i]])[2], 2), ")"), size = 7)+
          theme(legend.position = "none", 
                legend.title=element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                # axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20),
                axis.ticks.x=element_blank())
        # annotate("text", x = mean(tables$TIME) * 1.15, y = mean(tables[, variable_i]) / 10, label = paste0("p = ", round(a, 5)), size = 7)
        # print(Final.Fixed.Plot.1)
        
      } else {
        Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, 
                                    aes(x = TIME, y =fit))+
          geom_line(size=2, alpha = 0.8, aes(colour = "gren"))+
          # facet_wrap(~pc) + 
          geom_ribbon(aes(ymin= fit-se, ymax= fit+se), alpha=.2)+
          # scale_colour_manual(values = c("blue", "grey", "red")) + 
          # scale_fill_manual(values = c("blue", "grey", "red")) + 
          xlab("TIME")+
          ylab(paste0("Changes in ", variable_i))+ theme_bw()+
          # geom_hline(yintercept = c(unique(tables[, variable][which(tables$TIME ==3)])), 
          #            col = c("black")) +
          geom_hline(yintercept = 0,
                     col = c("black")) +
          scale_y_continuous(labels = label_number(accuracy = 0.01)) + 
          annotate("text", x= min(tables$TIME), y= max(tables[, variable_i])/2, hjust= 0, vjust= 1,
                   label = paste0("(", round(r.squaredGLMM(model_outputs[[i]])[1], 2), ", ",round(r.squaredGLMM(model_outputs[[i]])[2], 2), ")"), size = 7)+
          theme(legend.position = "none", 
                legend.title=element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x=element_blank(),
                # axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20),
                axis.ticks.x=element_blank())
        # print(Final.Fixed.Plot.1)
      }
      
    }
    
    plots[[i]] <- Final.Fixed.Plot.1
    # jpeg(paste0(variable_i,"_EAB_severity_0630_Den", ".jpg"))
    plot(Final.Fixed.Plot.1)
    # dev.off()
  }
  for (i in 6) {
    model_sumamry  <- summary(model_outputs[[i]])
    model_table <- as.data.frame(model_sumamry$tTable)
    model_table <- tibble::rownames_to_column(model_table, "Term")
    # model_table
    
    variable_i <- variable[i]
    tables <- model_outputs$mean.max.canopy.ht$data
    
    BBD_test_2 <- tables
    HWA_test_2 <- tables
    EAB_test_2 <- tables
    CAK_test_2 <- tables 
    LYD_test_2 <- tables
    FIRE_test_2 <- tables
    HURRI_test_3 <- tables
    
    
    Results.Model_sev<-Effect(c("TIME"), model_outputs[[i]],
                              xlevels=list(TIME= unique(tables$TIME)))
    
    Results.Model_sev <- as.data.frame(Results.Model_sev)
    
    if (length(model_table$`p-value`[model_table$Term == "poly(TIME, 3, raw = TRUE)3"] < 0.05) != 0) {
      if (model_table$`p-value`[model_table$Term == "poly(TIME, 3, raw = TRUE)3"] < 0.05) {
        # aaaa <- c(model_table$`p-value`[model_table$Term == "poly(TIME, 3, raw = TRUE)3"])
        # a <- aaaa[which(aaaa != 0)]
        
        Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, 
                                    aes(x = TIME, y =fit))+
          geom_line(size=2, alpha = 0.8, aes(colour = "gren"))+
          # facet_wrap(~pc) + 
          geom_ribbon(aes(ymin= fit-se, ymax= fit+se), alpha=.2)+
          # scale_colour_manual(values = c("blue", "grey", "red")) + 
          # scale_fill_manual(values = c("blue", "grey", "red")) + 
          xlab("TIME")+
          ylab(paste0("Changes in ", variable_i))+ theme_bw()+
          # geom_hline(yintercept = c(unique(tables[, variable][which(tables$TIME ==3)])), 
          #            col = c("black")) +
          geom_hline(yintercept = 0,
                     col = c("black")) +
          theme(legend.title=element_blank(),
                axis.ticks.length = unit(5, "pt"),
                legend.position = "none",
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20),
                panel.background = element_rect(
                  # fill = "black",
                  colour = "black",
                  size = 4
                )) + 
          scale_y_continuous(labels = label_number(accuracy = 0.01)) +
          annotate("text", x= min(tables$TIME), y= max(tables[, variable_i])/2, hjust= 0, vjust= 1,
                   label = paste0("(", round(r.squaredGLMM(model_outputs[[i]])[1], 2), ", ",round(r.squaredGLMM(model_outputs[[i]])[2], 2), ")"), size = 7)+
          theme(legend.position = "none", 
                legend.title=element_blank(),
                axis.title.x = element_blank())
        # print(Final.Fixed.Plot.1)
        
      } else {
        Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, 
                                    aes(x = TIME, y =fit))+
          geom_line(size=2, alpha = 0.8, aes(colour = "gren"))+
          # facet_wrap(~pc) + 
          geom_ribbon(aes(ymin= fit-se, ymax= fit+se), alpha=.2)+
          # scale_colour_manual(values = c("blue", "grey", "red")) + 
          # scale_fill_manual(values = c("blue", "grey", "red")) + 
          xlab("TIME")+
          ylab(paste0("Changes in ", variable_i))+ theme_bw()+
          # geom_hline(yintercept = c(unique(tables[, variable][which(tables$TIME ==3)])), 
          #            col = c("black")) +
          geom_hline(yintercept = 0,
                     col = c("black")) +
          scale_y_continuous(labels = label_number(accuracy = 0.01)) + 
          annotate("text", x= min(tables$TIME), y= max(tables[, variable_i])/2, hjust= 0, vjust= 1,
                   label = paste0("(", round(r.squaredGLMM(model_outputs[[i]])[1], 2), ", ",round(r.squaredGLMM(model_outputs[[i]])[2], 2), ")"), size = 7)+
          theme(legend.position = "none", 
                legend.title=element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20))
        # print(Final.Fixed.Plot.1)
      }
      
    } else if (length(model_table$`p-value`[model_table$Term == "poly(TIME, 2, raw = TRUE)2"] < 0.05) != 0) {
      if (model_table$`p-value`[model_table$Term == "poly(TIME, 2, raw = TRUE)2"] < 0.05) {
        # aaaa <- c(model_table$`p-value`[model_table$Term == "poly(TIME, 3, raw = TRUE)3"])
        # a <- aaaa[which(aaaa != 0)]
        
        Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, 
                                    aes(x = TIME, y =fit))+
          geom_line(size=2, alpha = 0.8, aes(colour = "gren"))+
          # facet_wrap(~pc) + 
          geom_ribbon(aes(ymin= fit-se, ymax= fit+se), alpha=.2)+
          # scale_colour_manual(values = c("blue", "grey", "red")) + 
          # scale_fill_manual(values = c("blue", "grey", "red")) + 
          xlab("TIME")+
          ylab(paste0("Changes in ", variable_i))+ theme_bw()+
          # geom_hline(yintercept = c(unique(tables[, variable][which(tables$TIME ==3)])), 
          #            col = c("black")) +
          geom_hline(yintercept = 0,
                     col = c("black")) +
          theme(legend.title=element_blank(),
                axis.ticks.length = unit(5, "pt"),
                legend.position = "none",
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20),
                panel.background = element_rect(
                  # fill = "black",
                  colour = "black",
                  size = 4
                )) + 
          scale_y_continuous(labels = label_number(accuracy = 0.01)) +
          annotate("text", x= min(tables$TIME), y= max(tables[, variable_i])/2, hjust= 0, vjust= 1,
                   label = paste0("(", round(r.squaredGLMM(model_outputs[[i]])[1], 2), ", ",round(r.squaredGLMM(model_outputs[[i]])[2], 2), ")"), size = 7)+
          theme(legend.position = "none", 
                legend.title=element_blank(),
                axis.title.x = element_blank())
        # annotate("text", x = mean(tables$TIME) * 1.15, y = mean(tables[, variable_i]) / 10, label = paste0("p = ", round(a, 5)), size = 7)
        # print(Final.Fixed.Plot.1)
        
      } else {
        Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, 
                                    aes(x = TIME, y =fit))+
          geom_line(size=2, alpha = 0.8, aes(colour = "gren"))+
          # facet_wrap(~pc) + 
          geom_ribbon(aes(ymin= fit-se, ymax= fit+se), alpha=.2)+
          # scale_colour_manual(values = c("blue", "grey", "red")) + 
          # scale_fill_manual(values = c("blue", "grey", "red")) + 
          xlab("TIME")+
          ylab(paste0("Changes in ", variable_i))+ theme_bw()+
          # geom_hline(yintercept = c(unique(tables[, variable][which(tables$TIME ==3)])), 
          #            col = c("black")) +
          geom_hline(yintercept = 0,
                     col = c("black")) +
          scale_y_continuous(labels = label_number(accuracy = 0.01)) + 
          annotate("text", x= min(tables$TIME), y= max(tables[, variable_i])/2, hjust= 0, vjust= 1,
                   label = paste0("(", round(r.squaredGLMM(model_outputs[[i]])[1], 2), ", ",round(r.squaredGLMM(model_outputs[[i]])[2], 2), ")"), size = 7)+
          theme(legend.position = "none", 
                legend.title=element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20))
        # print(Final.Fixed.Plot.1)
      }
      
    } else if (length(model_table$`p-value`[model_table$Term == "TIME"] < 0.05) != 0) {
      if (model_table$`p-value`[model_table$Term == "TIME"] < 0.05) {
        # aaaa <- c(model_table$`p-value`[model_table$Term == "poly(TIME, 3, raw = TRUE)3"])
        # a <- aaaa[which(aaaa != 0)]
        
        Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, 
                                    aes(x = TIME, y =fit))+
          geom_line(size=2, alpha = 0.8, aes(colour = "gren"))+
          # facet_wrap(~pc) + 
          geom_ribbon(aes(ymin= fit-se, ymax= fit+se), alpha=.2)+
          # scale_colour_manual(values = c("blue", "grey", "red")) + 
          # scale_fill_manual(values = c("blue", "grey", "red")) + 
          xlab("TIME")+
          ylab(paste0("Changes in ", variable_i))+ theme_bw()+
          # geom_hline(yintercept = c(unique(tables[, variable][which(tables$TIME ==3)])), 
          #            col = c("black")) +
          geom_hline(yintercept = 0,
                     col = c("black")) +
          theme(legend.title=element_blank(),
                axis.ticks.length = unit(5, "pt"),
                legend.position = "none",
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20),
                panel.background = element_rect(
                  # fill = "black",
                  colour = "black",
                  size = 4
                )) + 
          scale_y_continuous(labels = label_number(accuracy = 0.01)) +
          annotate("text", x= min(tables$TIME), y= max(tables[, variable_i])/2, hjust= 0, vjust= 1,
                   label = paste0("(", round(r.squaredGLMM(model_outputs[[i]])[1], 2), ", ",round(r.squaredGLMM(model_outputs[[i]])[2], 2), ")"), size = 7)+
          theme(legend.position = "none", 
                legend.title=element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20))
        # annotate("text", x = mean(tables$TIME) * 1.15, y = mean(tables[, variable_i]) / 10, label = paste0("p = ", round(a, 5)), size = 7)
        # print(Final.Fixed.Plot.1)
        
      } else {
        Final.Fixed.Plot.1 <-ggplot(data = Results.Model_sev, 
                                    aes(x = TIME, y =fit))+
          geom_line(size=2, alpha = 0.8, aes(colour = "gren"))+
          # facet_wrap(~pc) + 
          geom_ribbon(aes(ymin= fit-se, ymax= fit+se), alpha=.2)+
          # scale_colour_manual(values = c("blue", "grey", "red")) + 
          # scale_fill_manual(values = c("blue", "grey", "red")) + 
          xlab("TIME")+
          ylab(paste0("Changes in ", variable_i))+ theme_bw()+
          # geom_hline(yintercept = c(unique(tables[, variable][which(tables$TIME ==3)])), 
          #            col = c("black")) +
          geom_hline(yintercept = 0,
                     col = c("black")) +
          scale_y_continuous(labels = label_number(accuracy = 0.01)) + 
          annotate("text", x= min(tables$TIME), y= max(tables[, variable_i])/2, hjust= 0, vjust= 1,
                   label = paste0("(", round(r.squaredGLMM(model_outputs[[i]])[1], 2), ", ",round(r.squaredGLMM(model_outputs[[i]])[2], 2), ")"), size = 7)+
          theme(legend.position = "none", 
                legend.title=element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.x = element_text(size = 20),
                axis.text.y = element_text(size = 20))
        # print(Final.Fixed.Plot.1)
      }
      
    }
    
    plots[[i]] <- Final.Fixed.Plot.1
    # jpeg(paste0(variable_i,"_EAB_severity_0630_Den", ".jpg"))
    plot(Final.Fixed.Plot.1)
    # dev.off()
  }
    all_plots[[n]] <- plots
}

library(gridExtra)
plots2 <- vector("list")
for (i in 1:6) {
  plots2[[i]] <- do.call("grid.arrange", c(all_plots[[i]], ncol = 1))
}

results_Severity <- do.call("grid.arrange", c(plots2, ncol = 6))

jpeg(paste0("results_time_new_2023.jpg"), width = 21, height = 18, units = 'in', res = 300)
plot(results_Severity)
dev.off()


