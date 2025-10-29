## ---------------------
# Title: Evaluation of modeling approaches to account for hunting data 
# variability across large spatial scales.
# Code provided for repoducibility of the figure 2 
# Submitted to: Journal of Applied Ecology
# Date: 10/28/2025 (mm/dd/yyyy)
# Authors: They will be provided after the acceptance of the manuscript.
## ---------------------

# ----------------------------------------
# charge libraries
# ----------------------------------------
library(tidyverse)    # v.2.0.0
library(spAbundance)  # v.0.2.2
library(ggpubr)       # v.0.6.1
library(ggh4x)        # v.0.3.1
library(magick)       # v.2.9.0
library(rsvg)         # v.2.7.0
library(cowplot)      # v.1.2.0
rm(list=ls())

# -------- create several functions to repeat across the script to:
# 1.1. load all the results saved in a folder from each dataset scenario
results<-function(link){
for (i in 1:length(list.files(link))){
    name<-list.files(link)[i]
    load(paste(link, 
               name, 
               sep=""), envir = .GlobalEnv)
    print(i)
    }
  }

# 1.2. 
results.summary <- function(pattern = "out") {
  
  model_names <- ls(pattern = pattern, envir = .GlobalEnv)
  
  list.models <- vector("list", length(model_names))
  
  for (i in seq_along(model_names)) {
    model_name <- model_names[i]
    model <- get(model_name)
    
    # for all model parameters
    process_block <- function(samples, rhat, ESS, model_name) {
      apply(samples %>% as_tibble(), 2, quantile, c(0.025, 0.975), na.rm = TRUE) %>%
        as_tibble() %>%
        mutate(quantile = c(0.025, 0.975)) %>%
        pivot_longer(cols = !quantile, names_to = "param", values_to = "value") %>%
        mutate(quantile = ifelse(quantile == "0.025", "q0.025", "q0.975")) %>%
        pivot_wider(names_from = quantile) %>%
        mutate(
          model = model_name,
          rhat.model = rhat,
          ESS.model = as.integer(ESS)
        )
    }
    
    # for models with spatial random effects
    if (!is.null(model$ESS$theta)) {
      df_beta <- process_block(model$beta.samples, model$rhat$beta, model$ESS$beta, model_name)
      df_theta <- process_block(model$theta.samples, model$rhat$theta, model$ESS$theta, model_name)
      df <- rbind(df_beta, df_theta)
    } else {
      df <- process_block(model$beta.samples, model$rhat$beta, model$ESS$beta, model_name)
    }
    
    df <- df %>%
      mutate(convergence = factor(
        ifelse(rhat.model > 1.1, "Not converged",
               ifelse(rhat.model < 0.9, "Not converged",
                      ifelse(rhat.model < 1.1 & rhat.model > 0.9 & ESS.model < 200,
                             "Converged, not mixing well", "Converged, mixing well")))))
    
    list.models[[i]] <- df
  }
  
  return(list.models)
}

# 1.3. join the data frames for each species (parameters, ESS, Rhat) and tidy for plot
join.results<-function(lmodels){
  do.call(rbind,lmodels) %>% 
    mutate(sign=ifelse(q0.025<0 & q0.975>0, "95%CI overlaps with 0",
                       ifelse(q0.025>0 & q0.975>0, "positive relation", "negative relation"))) %>% 
    mutate(sign=ifelse(convergence=="Not converged", "Not converged", 
                       ifelse(convergence=="Converged, not mixing well", "Converged, not mixing well", sign))) %>%
    mutate(sign=factor(sign, levels=c("negative relation", "95%CI overlaps with 0", "positive relation", "Not converged", "Converged, not mixing well"))) %>% 
    mutate(model2=ifelse(startsWith(model, "out1"), "Model 1", 
                         ifelse(startsWith(model, "out2"), "Model 2", 
                                ifelse(startsWith(model, "out3"), "Model 3", "Model 4")))) %>% 
    mutate(model=gsub("out", "", model)) %>% 
    # recoding bioregions 
    mutate(modelBR=ifelse(grepl("BR1", model), "B1", 
                          ifelse(grepl("BR2", model), "B2", 
                                 ifelse(grepl("BR3", model), "B3",
                                        ifelse(grepl("BR4", model), "B4", ""))))) %>% 
    mutate(model=substr(model, 2, 2)) %>% 
    # recoding the parameters of the models
    mutate(param=ifelse(param=="(Intercept)", "intercept", ifelse(param=="bio_12", "prec", ifelse(param=="cforest", "forest", param)))) %>% 
    mutate(param=factor(param, levels=c("intercept", "prec", "hfp", "forest", "log(area)", "sigma.sq", "phi")))  %>% 
    # define alpha value for plots
    mutate(alpha=ifelse(convergence=="Converged, mixing well", 0.6, ifelse(convergence=="Converged, not mixing well", 0.55, 0.5))) %>% 
    # recoding the factor of the models used with the names given in the manuscript
    mutate(model2=factor(model2, levels=c("Model 1", "Model 2", "Model 3", "Model 4"), labels=c("ER", "FR", "IR", "SR")),
           model=factor(model, levels=c("a", "b", "c"), labels=c("EA", "OA", "CA")))
  
}

# 2. load model results 
# set the working directory on the folder where you have saved the results of 
# the model Rdata for the WITH dataset scenario
setwd("...")

# each of the following 3 lines do the following for each species:
# 2.X.1. load the results in the global environment,
# 2.X.2. from each model select parameters estimations, ESS, Rhat and assess convergence,
# 2.X.3. from the parameter estimation modify the data frame to get it ready.

# 2.1 models fitted WITH dataset
# ------------------------------
results("ModelResults/wildboar_with/")
list.models <- results_summary(pattern = "out")
model.sum.Wb<- join.results(list.models) %>% mutate(species="Sus scrofa")

rm(list=ls(pattern = "out"))
results("ModelResults/redDeer_with/")
list.models <- results_summary(pattern = "out")
model.sum.Red<-join.results(list.models) %>% mutate(species="Cervus elaphus")

rm(list=ls(pattern = "out"))
results("ModelResults/roeDeer_with/")
list.models <- results_summary(pattern = "out")
model.sum.Roe<-join.results(list.models) %>% mutate(species="Capreolus capreolus") 

rm(list=ls(pattern = "out"))
results("ModelResults/redFox_with/")
list.models <- results_summary(pattern = "out")
model.sum.Fox<-join.results(list.models) %>% mutate(species="Vulpes vulpes")
# ----------------------------

# 2.2 models fitted WITHOUT dataset
# ------------------------------
rm(list=ls(pattern = "out"))
results("ModelResults/wildboar_without/")
list.models <- results_summary(pattern = "out")
model.sum.Wb2<-join.results(list.models) %>% mutate(species="Sus scrofa")

rm(list=ls(pattern = "out"))
results("ModelResults/redDeer_with/")
list.models <- results_summary(pattern = "out")
model.sum.Red2<-join.results(list.models) %>% mutate(species="Cervus elaphus")

rm(list=ls(pattern = "out"))
results("ModelResults/roeDeer_with/")
list.models <- results_summary(pattern = "out")
model.sum.Roe2<-join.results(list.models) %>% mutate(species="Capreolus capreolus") 

rm(list=ls(pattern = "out"))
results("ModelResults/redFox_with/")
list.models <- results_summary(pattern = "out")
model.sum.Fox2<-join.results(list.models) %>% mutate(species="Vulpes vulpes")
# -------------------------------

# 3 plot the estimates
# ----------------------------
my_theme<-theme(plot.title = element_text(size = 16, face="bold.italic"),
                axis.title = element_blank(), 
                axis.text  = element_blank(), 
                axis.ticks = element_blank(),
                legend.title = element_blank(),
                legend.text = element_text(size = 10),
                strip.text.y.left = element_text(size = 13, angle=0, hjust = 1),
                strip.text.x.top  = element_text(size = 10, angle=0),
                panel.spacing.y = unit(0, "lines"),
                panel.spacing.x = unit(0, "lines"),
                strip.placement = "outside",
                panel.border = element_rect(colour = "grey90", fill = NA, size = 0.5),
                panel.background = element_blank()
)

# 3.1 plot dataset with
# -----------------------------
p_w<-ggarrange(
  ggplot(model.sum.Roe %>% filter(param!="biorreg2" & param!="biorreg3" & param!="biorreg4"),
         aes(y=param, x=model, fill=sign))+
    geom_tile()+
    ggtitle("Capreolus capreolus")+
    scale_fill_manual(values=alpha(c("#900b51", "#8C8C8C", "#267E78", "#d3a84f", "#6388B6"),
                                   c(.5,.6, .6, .6,.6)), expand = c(0,0), drop = FALSE) +
    facet_nested(param~model2+model+modelBR, 
                 scales="free", 
                 margins = FALSE,
                 switch = "y",
                 nest_line = element_line(linetype = 1, linewidth = 0.5, colour = "grey90"))+
    my_theme,
  
  ggplot(model.sum.Red %>% filter(param!="biorreg2" & param!="biorreg3" & param!="biorreg4"),
         aes(y=param, x=model, fill=sign))+
    geom_tile()+
    ggtitle("Cervus elaphus")+
    scale_fill_manual(values=alpha(c("#900b51", "#8C8C8C", "#267E78", "#d3a84f", "#6388B6"),
                                   c(.5,.6, .6, .6,.6)), expand = c(0,0), drop = FALSE) +
    facet_nested(param~model2+model+modelBR, 
                 scales="free", 
                 margins = FALSE,
                 switch = "y",
                 nest_line = element_line(linetype = 1, linewidth = 0.5, colour = "grey90"))+
    my_theme,
  
  ggplot(model.sum.Wb %>% filter(param!="biorreg2" & param!="biorreg3" & param!="biorreg4"),
         aes(y=param, x=model, fill=sign))+
    geom_tile()+
    ggtitle("Sus scrofa")+
    scale_fill_manual(values=alpha(c("#900b51", "#8C8C8C", "#267E78", "#d3a84f", "#6388B6"),
                                   c(.5,.6, .6, .6,.6)), expand = c(0,0), drop = FALSE) +
    facet_nested(param~model2+model+modelBR, 
                 scales="free", 
                 margins = FALSE,
                 switch = "y",
                 nest_line = element_line(linetype = 1, linewidth = 0.5, colour = "grey90"))+
    my_theme,
  
  ggplot(model.sum.Fox %>% filter(param!="biorreg2" & param!="biorreg3" & param!="biorreg4"),
         aes(y=param, x=model, fill=sign))+
    geom_tile()+
    ggtitle("Vulpes vulpes")+
    scale_fill_manual(values=alpha(c("#900b51", "#8C8C8C", "#267E78", "#d3a84f", "#6388B6"),
                                   c(.5,.6, .6, .6,.6)), expand = c(0,0), drop = FALSE) +
    facet_nested(param~model2+model+modelBR, 
                 scales="free", 
                 margins = FALSE,
                 switch = "y",
                 nest_line = element_line(linetype = 1, linewidth = 0.5, colour = "grey90"))+
    scale_x_discrete(label = NULL)+
    my_theme,
  ncol=1, nrow=4, common.legend = FALSE, legend="none")
# ----------------------------

# 3.2 plot dataset without
# ----------------------------
p_wo<-ggarrange(
  ggplot(model.sum.Roe2 %>% filter(param!="biorreg2" & param!="biorreg3" & param!="biorreg4"),
         aes(y=param, x=model, fill=sign))+
    geom_tile()+
    ggtitle("Capreolus capreolus")+
    scale_fill_manual(values=alpha(c("#900b51", "#8C8C8C", "#267E78", "#d3a84f", "#6388B6"),
                                   c(.5,.6, .6, .6,.6)), expand = c(0,0), drop = FALSE) +
    facet_nested(param~model2+model+modelBR, 
                 scales="free", 
                 margins = FALSE,
                 switch = "y",
                 nest_line = element_line(linetype = 1, linewidth = 0.5, colour = "grey90"))+
    my_theme,
  
  ggplot(model.sum.Red2 %>% filter(param!="biorreg2" & param!="biorreg3" & param!="biorreg4"),
         aes(y=param, x=model, fill=sign))+
    geom_tile()+
    ggtitle("Cervus elaphus")+
    scale_fill_manual(values=alpha(c("#900b51", "#8C8C8C", "#267E78", "#d3a84f", "#6388B6"),
                                   c(.5,.6, .6, .6,.6)), expand = c(0,0), drop = FALSE) +
    facet_nested(param~model2+model+modelBR, 
                 scales="free", 
                 margins = FALSE,
                 switch = "y",
                 nest_line = element_line(linetype = 1, linewidth = 0.5, colour = "grey90"))+
    my_theme,
  
  ggplot(model.sum.Wb2 %>% filter(param!="biorreg2" & param!="biorreg3" & param!="biorreg4"),
         aes(y=param, x=model, fill=sign))+
    geom_tile()+
    ggtitle("Sus scrofa")+
    scale_fill_manual(values=alpha(c("#900b51", "#8C8C8C", "#267E78", "#d3a84f", "#6388B6"),
                                   c(.5,.6, .6, .6,.6)), expand = c(0,0), drop = FALSE) +
    
    facet_nested(param~model2+model+modelBR, 
                 scales="free", 
                 margins = FALSE,
                 switch = "y",
                 nest_line = element_line(linetype = 1, linewidth = 0.5, colour = "grey90"))+
    my_theme,
  
  
  ggplot(model.sum.Fox2 %>% filter(param!="biorreg2" & param!="biorreg3" & param!="biorreg4"),
         aes(y=param, x=model, fill=sign))+
    geom_tile()+
    ggtitle("Vulpes vulpes")+
    scale_fill_manual(values=alpha(c("#900b51", "#8C8C8C", "#267E78", "#d3a84f", "#6388B6"),
                                   c(.5,.6, .6, .6,.6)), expand = c(0,0), drop = FALSE) +
    facet_nested(param~model2+model+modelBR, 
                 scales="free", 
                 margins = FALSE,
                 switch = "y",
                 nest_line = element_line(linetype = 1, linewidth = 0.5, colour = "grey90"))+
    scale_x_discrete(label = NULL)+
    my_theme,
  ncol=1, nrow=4, common.legend = FALSE, legend="none")
# ------------------------

legend<-ggplot(model.sum.Roe2 %>% filter(param!="biorreg2" & param!="biorreg3" & param!="biorreg4"),
                 aes(y=param, x=model, fill=sign))+
  geom_tile()+
  scale_fill_manual(values=alpha(c("#900b51", "#8C8C8C", "#267E78", "#d3a84f", "#6388B6"),
                                 c(.5,.6, .6, .6,.6)), expand = c(0,0), drop = FALSE) +
  my_theme+
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 14))

legend<-ggpubr::get_legend(legend)

# 3.3 join species plots from both datasets (WITH & WITHOUT)
p<-cowplot::plot_grid(p_w,
                      p_wo,
                      labels = c('A', 'B'), 
                      ncol=2)

# 3.4 add the species icons
p1<-ggdraw()+ 
  draw_plot(p)+
  draw_image(image_read_svg("icons/C.capreolus.svg"),
             scale = .056, x = -0.45, y = 0.445) +
  draw_image(image_read_svg("icons/C.elaphus.svg"),
             scale = .062, x = -0.45,  y = 0.198)+
  draw_image(image_read_svg("icons/S.scrofa.svg"), 
             scale =.065, x = -0.45, y = -0.064)+
  draw_image(image_read_svg("icons/RedFox.svg"), 
             scale = .065, x = -0.45,  y = -0.314)

final_plot <- cowplot::plot_grid(p1, legend, ncol = 1, rel_heights = c(1, 0.03))

# 3.5 Save the Figure 2
ggsave(filename = "3_Plots/Figure2.svg", # filename of the plot 
       plot = final_plot,                # plot to save
       width = 30, height = 40,          # dimensions
       units = "cm",                     # dimension units
       dpi = 300)               
