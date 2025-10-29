## ---------------------
# Title: Evaluation of modeling approaches to account for hunting data 
# variability across large spatial scales.
# Code provided for repoducibility of the figure 4 
# Submitted to: Journal of Applied Ecology
# Date: 10/28/2025 (mm/dd/yyyy)
# Authors: They will be provided after the acceptance of the manuscript.
## ---------------------

# ----------------------------------------
# charge libraries
# ----------------------------------------
library(tidyverse) # v.2.0.0
library(sf)        # v.1.0-21
library(sjPlot)    # v2.9.0
rm(list=ls())      # remove all objects of your global environment
# ----------------------------------------
# 1. Data preparation
# ----------------------------------------

# 1.1. load municipality predictions
# -----------------------------
tmRed_with<-st_read("outRedDeer.gpkg", layer="models_wNA0_NB_munic")
tmRed_wo<-st_read("outRedDeer.gpkg",   layer="models_woNA0_NB_munic")
tmRoe_with<-st_read("outRoeDeer.gpkg", layer="models_wNA0_NB_munic")
tmRoe_wo<-st_read("outRoeDeer.gpkg",   layer="models_woNA0_NB_munic")
tmFox_with<-st_read("outFox.gpkg",     layer="models_wNA0_NB_munic")
tmFox_wo<-st_read("outFox.gpkg",       layer="models_woNA0_NB_munic")
tmWb_with<-st_read("outWb.gpkg",       layer="models_wNA0_NB_munic")
tmWb_wo<-st_read("outWb.gpkg",         layer="models_woNA0_NB_munic")

# 1.2. load the date used to create the models and join it with the predicted data
# --------------------------------------------------------------------------------
load("redDeer_wData.Rdata")
data<-data %>% as_tibble() %>% select(NATCODE, matches("Red"))
tmRed_with<-tmRed_with %>% left_join(data, by="NATCODE")
tmRed_wo<-tmRed_wo %>% left_join(data, by="NATCODE") %>% select(-predData) %>% left_join(tmRed_with %>% as_tibble() %>% select(NATCODE, predData), by="NATCODE")

load("roeDeer_wData.Rdata")
data<-data %>% as_tibble() %>% select(NATCODE, matches("Roe"))
tmRoe_with<-tmRoe_with %>% left_join(data, by="NATCODE")
tmRoe_wo<-tmRoe_wo %>% left_join(data, by="NATCODE") %>% select(-predData) %>% left_join(tmRed_with %>% as_tibble() %>% select(NATCODE, predData), by="NATCODE")

load("redFox_wData.Rdata")
data<-data %>% as_tibble() %>% select(NATCODE, matches("Fox"))
tmFox_with<-tmFox_with %>% left_join(data, by="NATCODE")
tmFox_wo<-tmFox_wo %>% left_join(data, by="NATCODE") %>% select(-predData) %>% left_join(tmRed_with %>% as_tibble() %>% select(NATCODE, predData), by="NATCODE")

load("wb_wData.Rdata")
data<-data %>% as_tibble() %>% select(NATCODE, matches("Wb"))
tmWb_with<-tmWb_with %>% left_join(data, by="NATCODE")
tmWb_wo<-tmWb_wo %>% left_join(data, by="NATCODE") %>% select(-predData) %>% left_join(tmRed_with %>% as_tibble() %>% select(NATCODE, predData), by="NATCODE")

rm(data)


# ----------------------------------------
# 2. Correlation test, method = Spearman 
# ----------------------------------------
.lista.tm<-list()
.lista.tm1<-list()
for (i in 1:length(ls(pattern="tm"))){
  poly_name<-ls(pattern="tm")[i]
  poly<-get(ls(pattern="tm")[i])
  species<-sub("tm", "", sub("_.*", "", poly_name))
  
  poly<-poly %>% as_tibble() %>% select(NATCODE, matches(species,  ignore.case = FALSE), matches("spA"), predData) %>% select(!matches("high") & !matches("low"))  
  poly<-poly %>% mutate(meanALLmodels=rowMeans(poly[6:17], na.rm = TRUE)) %>% 
    mutate(predData=factor(predData))
  
  list.cor<-list()
  list.cor1<-list()
  
  for (j in 1:12){
    
    colnames(poly)[3]<-"obs"
    model_name<-colnames(poly[j+5])
    
    poly2<-poly %>% select(!model_name)
    
    poly2<-poly2 %>% mutate(meanALLmodels=rowMeans(poly2[6:16], na.rm = TRUE)) %>% 
      mutate(predData=factor(predData))
    
    polyNA0ALL <- poly[is.na(poly[,4]),] %>% filter(predData==1)
    polyNA0ALL2 <- poly2[is.na(poly2[,4]),] %>% filter(predData==1)
    
    poly<-poly %>% mutate(obs=replace_na(obs, 0))
    poly2<-poly2 %>% mutate(obs=replace_na(obs, 0))
    
    
    correl<-cor.test(poly[[j+5]], poly[[19]],
                     method="spearman",
                     na.action("na.omit"),
                     use = "complete.obs")
    
    correl2<-cor.test(poly[[j+5]], poly2[[18]],
                      method="spearman",
                      na.action("na.omit"),
                      use = "complete.obs")
    
    correlNA0ALL<-cor.test(polyNA0ALL[[j+5]], polyNA0ALL[[19]],
                           method="spearman",
                           na.action("na.omit"),
                           use = "complete.obs")
    
    correlNA0ALL2<-cor.test(polyNA0ALL[[j+5]], polyNA0ALL2[[18]],
                            method="spearman",
                            na.action("na.omit"),
                            use = "complete.obs")
    
    list.cor[[j]]<-tibble(rho       = unname(correl$estimate),
                          pvalue    = correl$p.value[[1]],
                          rhoL1O    = unname(correl2$estimate),
                          pvalueL1O = correl2$p.value[[1]],
                          model     = model_name)
    
    list.cor1[[j]]<-tibble(rho       = unname(correlNA0ALL$estimate),
                           pvalue    = correlNA0ALL$p.value[[1]],
                           rhoL1O    = unname(correlNA0ALL2$estimate),
                           pvalueL1O = correlNA0ALL2$p.value[[1]],
                           model     = model_name)
    
  }
  
  test<-do.call(rbind, list.cor) %>% mutate(dataset=sub("^[^_]*_", "", poly_name), 
                                            species=species, 
                                            cortest="ALL")
  
  test1<-do.call(rbind, list.cor1) %>% mutate(dataset=sub("^[^_]*_", "", poly_name), 
                                              species=species, 
                                              cortest="NA0ALL")
  
  
  .lista.tm[[i]]<-test
  .lista.tm1[[i]]<-test1
}

evalALL<-do.call(rbind, .lista.tm) %>% mutate(model.type = gsub("spA.m", "", model), 
                                              model.br   = gsub("[a-z]", "", model.type),
                                              model.area = gsub("[0-9]", "", model.type))

evalNA0ALL<-do.call(rbind, 
                    .lista.tm1) %>% mutate(model.type = gsub("spA.m", "", model), 
                                           model.br   = gsub("[a-z]", "", model.type),
                                           model.area = gsub("[0-9]", "", model.type))

rm(list=ls(pattern="test")); 
rm(list=ls(pattern="poly")); 
rm(list=ls(pattern="cor")); 
rm(i,j, species, df, list.mod)

rm(list=ls(pattern="tm"))
rm(model, modeling)


# ----------------------------------------

evalALLm<-evalALL %>% select(-c(pvalue,pvalueL1O)) %>% 
  mutate(dataset=ifelse(dataset=="with", "WITH", "WITHOUT")) %>% 
  pivot_longer(cols = rho:rhoL1O, values_to = "value", names_to = "correlation.type") %>% 
  mutate(across(where(is.character), as.factor))
evalNA0ALLm<-evalNA0ALL %>% select(-c(pvalue,pvalueL1O)) %>% 
  mutate(dataset=ifelse(dataset=="with", "WITH", "WITHOUT")) %>% 
  pivot_longer(cols = rho:rhoL1O, values_to = "value", names_to = "correlation.type") %>% 
  mutate(across(where(is.character), as.factor))

evalALLm<-evalALLm %>% mutate(model.area=factor(model.area, levels = c("a", "b", "c"), labels=c("EA", "OA", "CA")), 
                              model.br=factor(model.br, levels = c("1", "2", "3", "4"), labels=c("ER", "FR", "IR", "SR")))
evalNA0ALLm<-evalNA0ALLm %>% mutate(model.area=factor(model.area, levels = c("a", "b", "c"), labels=c("EA", "OA", "CA")), 
                                    model.br=factor(model.br, levels = c("1", "2", "3", "4"), labels=c("ER", "FR", "IR", "SR")))


# 2. Correlation of predictions: do the spatial predictions of the models differ?
# --------------------------------------------------------------------------------
# model 1: model comparison - average of all models 
m1<-lmerTest::lmer(value~correlation.type+model.br*model.area+model.area*dataset+(1|species), data=evalALLm)
drop1(m1, test="Chi")
# model 2: model comparison - average leaving one out
m2<-lmerTest::lmer(value~correlation.type+model.br*model.area+model.area*dataset+(1|species), data=evalNA0ALLm)
drop1(m2, test="Chi")


# 3.1. join all the graphics of figure 4 in a same object
# ----------------------------------------
my_theme<-theme(legend.position = "top", 
                legend.title = element_blank(), 
                axis.title.x = element_blank(), 
                plot.title = element_text(size=10, face="bold", hjust = 0.5))

p1<-cowplot::plot_grid(
  plot_model(m1, type = "eff", terms=c("model.area", "dataset"))+ # "re" = random effects
    scale_y_continuous(limits = c(.48, 1), breaks = seq(.5, 1, by = .1))+
    scale_color_manual(values = c("#278f8f", "#9bd1d1"))+#c("#5a6b1f", "#c1c6a5"))+
    labs(title = expression(atop(bold("Interaction between model.area and dataset"), bold("over the ") * rho[s] * bold(" model"))),
         y=expression(rho[s]))+ 
    theme_minimal()+
    my_theme, 
  
  plot_model(m1, type = "eff", terms=c("model.br", "model.area"))+ # "re" = random effects
    scale_y_continuous(limits = c(.48, 1), breaks = seq(.5, 1, by = .1))+
    scale_color_manual(values = c("#d97700", "#e3c88f", "#d3a84f", "#b38c3f"))+ #c("#7c003e", "#b97c9b","#c799b0"))+
    labs(title = expression(atop(bold("Interaction between model.area and model.bioregion"), bold("over the ") * rho[s] * bold(" model"))),
         y=expression(rho[s]))+ 
    theme_minimal()+
    my_theme,
  
  labels = c('A', 'B')
)

p1

# 3.2. Save figure 4
ggsave(filename = "Figure4_20251023.svg", # filename of the plot 
       plot = p1,                                            # plot to save
       width = 20, height = 12,                              # dimensions
       units = "cm",                                         # dimension units
       dpi = 300)               
