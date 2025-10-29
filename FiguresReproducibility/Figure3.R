## ---------------------
# Title: Evaluation of modeling approaches to account for hunting data 
# variability across large spatial scales.
# Code provided for repoducibility of the figure 3 
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
# rm(list=ls())    # remove all objects of your global environment

# 1. Data preparation
# ----------------------

# 1.1. load municipality predictions
# ----------------------
tmRed_with<-st_read("outRedDeer.gpkg", layer="models_wNA0_NB_munic")
tmRed_wo<-st_read("outRedDeer.gpkg",   layer="models_woNA0_NB_munic")
tmRoe_with<-st_read("outRoeDeer.gpkg", layer="models_wNA0_NB_munic")
tmRoe_wo<-st_read("outRoeDeer.gpkg",   layer="models_woNA0_NB_munic")
tmFox_with<-st_read("outFox.gpkg",     layer="models_wNA0_NB_munic")
tmFox_wo<-st_read("outFox.gpkg",       layer="models_woNA0_NB_munic")
tmWb_with<-st_read("outWb.gpkg",       layer="models_wNA0_NB_munic")
tmWb_wo<-st_read("outWb.gpkg",         layer="models_woNA0_NB_munic")
# ----------------------------------------

# 1.2. load grid predictions
# ----------------------
gridRed_with<-st_read("outRedDeer.gpkg", layer="models_wNA0_NB_grid")  %>% st_transform(3035)
gridRed_wo<-st_read("outRedDeer.gpkg",   layer="models_woNA0_NB_grid") %>% st_transform(3035)
gridRoe_with<-st_read("outRoeDeer.gpkg", layer="models_wNA0_NB_grid")  %>% st_transform(3035)
gridRoe_wo<-st_read("outRoeDeer.gpkg",   layer="models_woNA0_NB_grid") %>% st_transform(3035)
gridFox_with<-st_read("outFox.gpkg",     layer="models_wNA0_NB_grid")  %>% st_transform(3035)
gridFox_wo<-st_read("outFox.gpkg",       layer="models_woNA0_NB_grid") %>% st_transform(3035)
gridWb_with<-st_read("outWb_test1.gpkg", layer="models_wNA0_NB_grid")  %>% st_transform(3035)
gridWb_wo<-st_read("outWb_test1.gpkg",   layer="models_woNA0_NB_grid") %>% st_transform(3035)


# 1.3. overall sum of predicted values at both resolution scales
# --------------------------------------------------------------
.lista.tm<-list()
for (i in 1:length(ls(pattern="tm"))){
  poly_name<-ls(pattern="tm")[i]
  poly<-get(ls(pattern="tm")[i])
  
  sum.50<-data.frame("sum.p50" =tibble(poly %>% select(!matches("high")) %>% select(!matches("low")))[25:36] %>% colSums(na.rm=TRUE))
  sum.low<-data.frame("sum.Low" =tibble(poly) %>% select(matches("low")) %>% colSums(na.rm=TRUE))
  sum.high<-data.frame("sum.High" = tibble(poly) %>% select(matches("high")) %>% colSums(na.rm=TRUE))
  areaData<-sum((poly %>% filter(predData==1))$area)
  areaPredicted <- sum(poly$area)
  .lista.tm[[i]]<-cbind(sum.50, sum.low, sum.high) %>% mutate(CI=sum.High-sum.Low, 
                                                              prediction="Municipality", 
                                                              poly_name=poly_name, 
                                                              areaData = areaData, 
                                                              areaPredicted = areaPredicted)
  names(.lista.tm)[i]<-poly_name 
}

.lista.grid<-list()
for (i in 1:length(ls(pattern="grid"))){
  poly_name<-ls(pattern="grid")[i]
  poly<-get(ls(pattern="grid")[i])
  
  sum.50<-data.frame("sum.p50" = tibble(poly %>% select(!matches("high")) %>% select(!matches("low")))[14:25] %>% colSums(na.rm=TRUE))
  sum.low<-data.frame("sum.Low" = tibble(poly) %>% select(matches("low")) %>% colSums(na.rm=TRUE))
  sum.high<-data.frame("sum.High" = tibble(poly) %>% select(matches("high")) %>% colSums(na.rm=TRUE))
  areaPredicted <- sum(poly$area, na.rm=TRUE)
  .lista.grid[[i]]<-cbind(sum.50, sum.low, sum.high) %>% mutate(CI=sum.High-sum.Low, 
                                                                prediction="Grid", 
                                                                poly_name=poly_name, 
                                                                areaPredicted = areaPredicted)
  
  names(.lista.grid)[i]<-poly_name 
  
}

sums.tm<-do.call(rbind, .lista.tm)
# sums.tm %>% head(15)
sums.grid<-do.call(rbind, .lista.grid) %>% mutate(areaData=sums.tm$areaData)
# sums.grid %>% head(15)

sumsPred<-rbind(sums.tm, sums.grid) 
sumsPred<-rownames_to_column(sumsPred, var="names")

sumsPred<-sumsPred %>% mutate(species=gsub("tm", "", gsub("grid", "", str_extract(names, "[^_]+"))),
                              species=ifelse(species=="Wb", "Wild boar", 
                                             ifelse(species=="Roe", "Roe deer", 
                                                    ifelse(species=="Red", "Red deer", "Red fox"))), 
                              dataset=paste(gsub("_", "", gsub("(.+?)(\\_.*)", "\\2", poly_name)), "NA / 0 values", sep=" "),
                              dataset=gsub("NA / 0 values", "", dataset),
                              model.type = str_sub(names, -2),
                              model.area = str_sub(names, -1),
                              model.br = gsub("[a-z]", "", model.type), 
                              bioregion=ifelse(model.br==1 | model.br==4, "Not considering bioregions", "Considering bioregions")
                              ) 

# 1.4 load the data use for modeling (scenario WITH and WITHOUT)
listData<-list()
load("data_modeling/redDeer_wData.Rdata")
listData$redDeer_with<-c(observed=sum(data$hMaxRedZero), nData=nrow(data))

load("data_modeling/roeDeer_wData.Rdata")
listData$roeDeer_with<-c(observed=sum(data$hMaxRoeZero), nData=nrow(data))

load("data_modeling/redFox_wData.Rdata")
listData$redFox_with<-c(observed=sum(data$hMaxFoxZero), nData=nrow(data))

load("data_modeling/wb_wData.Rdata")
listData$wb_with<-c(observed=sum(data$hMaxWbZero), nData=nrow(data))


load("redDeer_woData.Rdata")
listData$redDeer_wo<-c(observed=sum(data$hMaxRedZero), nData=nrow(data))

load("roeDeer_woData.Rdata")
listData$roeDeer_wo<-c(observed=sum(data$hMaxRoeZero), nData=nrow(data))

load("redFox_woData.Rdata")
listData$redFox_wo<-c(observed=sum(data$hMaxFoxZero), nData=nrow(data))

load("wb_woData.Rdata")
listData$wb_wo<-c(observed=sum(data$hMaxWbZero), nData=nrow(data)) ; rm(data)

listData<-do.call(rbind, listData)
listData<-rownames_to_column(data.frame(listData), var="names")

listData<-listData %>% mutate(dataset=paste(gsub("_", "", gsub("(.+?)(\\_.*)", "\\2", names)), "NA / 0 values", sep=" "),
                              dataset=gsub("NA / 0 values", "", dataset),
                              species=gsub("_", "", gsub("(.+?)(\\_.*)", "\\1", listData$names)),
                              species=ifelse(species=="redDeer", "Red deer", 
                                             ifelse(species=="roeDeer", "Roe deer",
                                                    ifelse(species=="redFox", "Red fox", "Wild boar"))))

# 1.5 join predictions and data input used for modeling
sumsPred<-sumsPred %>% left_join(listData %>% select(-names), by=c("dataset", "species")); rm(listData)

#1.6. Calculate bias of predicted abundance and credible interval
sumsPred<-sumsPred %>% mutate(bias=observed-sum.p50, 
                              abs_bias=abs(bias)) %>% 
                       mutate(per_area=areaData/areaPredicted)

sumsPred <- sumsPred %>%  mutate(across(where(is.character), as.factor))

# recode names as they appear in the manuscript
sumsPred<-sumsPred %>% mutate(model.area=factor(model.area, levels = c("a", "b", "c"), labels=c("EA", "OA", "CA")), 
                              model.br=factor(model.br, levels = c("1", "2", "3", "4"), labels=c("ER", "FR", "IR", "SR")), 
                              dataset=factor(dataset, levels=c("with ", "wo "), labels=c("WITH", "WITHOUT")))
# ----------------------------------------


# 2. Modeling bias: is there any difference due to the models used?
# -----------------------------------------------------------------
# model 1: log (absolut bias)
model1<-lmerTest::lmer(log(abs_bias)~model.area*prediction+model.br*dataset+per_area*model.area+(1|species),
                       data=sumsPred)
# drop1(model1, test="Chi"); summary(model1)

# model 2: log (credible interval)
# --------------------------
model2<-lmerTest::lmer(log(CI)~model.area*prediction+model.br+dataset+per_area*model.area+(1|species),
                       data=sumsPred)
# drop1(model2, test="Chi"); summary(model2)


# 3. Save the plot_model into an object (which contains the data and the plot)
my_colors<-c("#7c003e", "#278f8f", "#d97700", "#35478c")
p1 <- plot_model(model2, type = "eff", terms = c("dataset"))
p2 <- plot_model(model2, type = "eff", terms = c("model.br"))

# extract the data values that plot_model used
df1 <- p1$data
df2 <- p2$data

# 3.1. join all the graphics of figure 3 in a same object
p1<-cowplot::plot_grid(
  plot_model(model1, type = "eff", 
             terms = c("model.br", "dataset")) + 
    labs(y = "log(abs_bias)", title="Interaction between \n dataset * regional approach")+
    scale_y_continuous(limits = c(10.5, 13.5), breaks = seq(10, 14, by = 1))+
    scale_color_manual(values = c("#278f8f", "#9bd1d1"))+
    theme_minimal()+
    theme(legend.position = "top", legend.title = element_blank(), 
          axis.title.x = element_blank(),
          plot.title = element_text(size=10, face="bold", hjust = 0.5)),
  
  plot_model(model1, type = "eff", 
             terms = c("model.area", "prediction"), return.data = TRUE) + 
    labs(y = "log(abs_bias)", title="Interaction between \n spatial resolution * area approach")+
    scale_y_continuous(limits = c(9.75, 16.4), breaks = seq(10, 16, by = 2))+
    scale_color_manual(values = c("#7c003e", "#b97c9b"))+
    theme_minimal()+
    theme(legend.position = "top", legend.title = element_blank(), 
          axis.title.x = element_blank(),
          plot.title = element_text(size=10, face="bold", hjust = 0.5)),
  
  plot_model(model1, type = "eff",
             terms = c("per_area", "model.area"), return.data = TRUE) + 
    labs(y = "log(abs_bias)", 
         title="Interaction between \n area approach * area dataset/predicted")+
    geom_line()+
    scale_color_manual(values = my_colors)+
    scale_fill_manual(values = my_colors)+
    theme_minimal()+
    theme(legend.position = "top", legend.title = element_blank(), 
          axis.title.x = element_blank(),
          plot.title = element_text(size=10, face="bold", hjust = 0.5)),
  
  ggplot(df2, aes(x = x, y = predicted)) +
    geom_point(color = c("#8DA0CB", "#5370B1", "#35478c", '#232F5B'), 
               size = 3) +
    geom_segment(aes(x = x, xend = x, y = conf.low, yend = conf.high), 
                 color = c("#8DA0CB", "#5370B1", "#35478c", '#232F5B')) +
    scale_x_continuous(breaks = df2$x, labels = attr(p2$data, "x.axis.labels")) +
    labs(y = "log(CI)", title = "Regional approach")+
    theme_minimal()+
    theme(axis.title.x = element_blank(),
          legend.position = "none", 
          legend.title = element_blank(), 
          plot.title = element_text(size=10, face="bold", hjust = 0.5)
    ),
  labels = c('A', 'B', 'C', 'D'),
  nrow=2, ncol=2)
p1

# 3.2. Save figure 3
ggsave(filename = "Figure3.svg", # filename of the plot 
       plot = p1,                # plot to save
       width = 20, height = 20,  # dimensions
       units = "cm",             # dimension units
       dpi = 300)               
