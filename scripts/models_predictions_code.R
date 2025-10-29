## ---------------------
# Title: Evaluation of modeling approaches to account for hunting data 
# variability across large spatial scales.
# Code provided for repoducibility of modeling approaches
# Submitted to: Journal of Applied Ecology
# Date: 10/28/2025 (mm/dd/yyyy)
# Authors: They will be provided after the acceptance of the manuscript.
## ---------------------

# ----------------------------------------
# charge libraries
# ----------------------------------------
library(tidyverse)   # v.2.0.0
library(sf)          # v.1.0-21
library(spAbundance) # v.0.2.2
library(fastDummies) # v.1.7.5
# rm(list=ls())      # remove all objects of your global environment

# 1. Load your data
# -----------------------------
data<-read.csv2("modelingSpData.csv")

## 1.1. Select the species you want to analyse 
# remember that species are coded as following: 
# Ss: Sus scrofa; Ce: Cervus elaphus; Cc: Capreolus capreolus; Vv: Vulpes vulpes
# as an example we'll use Ss
data.bm<-data %>% select(Ss, # Ce, Cc, Vv, # select the species
                         prec,             # precipitation
                         hfp,              # human foot print
                         forest,           # forest percentage
                         area,             # area surface at square kilometers
                         bioreg,           # bioregion
                         Var1,             # x_centroid
                         Var2,             # y_centroid
                         NATCODE
                         )  %>% 
                    mutate(bioreg=as.factor(bioreg)) %>% # be sure that it's a factor
                    tibble() %>% select(-geom) 

colnames(data.bm)[1]<-"counts" # rename the species' name as counts
                               # any model will use it as the response variable

## 1.2. Select the type of dataset you want to analyse (full vs. reduced)
data.bm<-data.bm[!(data.bm$counts/data.bm$area>30),] # for any case: remove densities >30 indiv. harvested 
# for the full dataset do NOT remove the following comment
# for the reduced dataset (>0 individuals harvested) remove the comment below
# data.bm<-data.bm[data.bm$counts>0,] # remove data which VR is 0.

## 1.3. Standarise your variables regarding the dataset you've selected
### 1.3.1. data for Scenarios 1, 2, 4
mb.me.forest<-mean(data.bm$forest, na.rm=TRUE)
mb.sd.forest<-sd(data.bm$forest, na.rm=TRUE)
mb.me.prec<-mean(data.bm$prec, na.rm=TRUE)
mb.sd.prec<-sd(data.bm$prec, na.rm=TRUE)
mb.me.hfp<-mean(data.bm$hfp, na.rm=TRUE)
mb.sd.hfp<-sd(data.bm$hfp, na.rm=TRUE)

x <-data.bm %>% mutate(forest = ((forest - mb.me.forest)/ mb.sd.forest), 
                       prec   = ((prec   - mb.me.prec)  / mb.sd.prec), 
                       hfp    = ((hfp    - mb.me.hfp)   / mb.sd.hfp))

### 1.3.2. data for Scenario (IR): 
### Split the dataset by bioregion (remember number of subsets depends on your data)
data.BR1<-data.bm[data.bm$bioreg==1,]
data.BR2<-data.bm[data.bm$bioreg==2,]
data.BR3<-data.bm[data.bm$bioreg==3,]
data.BR4<-data.bm[data.bm$bioreg==4,]

mb.br1.mprec<-mean(data.BR1$prec, na.rm=T);     mb.br1.sprec <- sd(data.BR1$prec, na.rm=T)
mb.br1.mforest<-mean(data.BR1$forest, na.rm=T); mb.br1.sforest<-sd(data.BR1$forest, na.rm=T)
mb.br1.mhfp<-mean(data.BR1$hfp, na.rm=T);       mb.br1.shfp <- sd(data.BR1$hfp, na.rm=T)

mb.br2.mprec<-mean(data.BR2$prec, na.rm=T);     mb.br2.sprec<-sd(data.BR2$prec, na.rm=T)
mb.br2.mforest<-mean(data.BR2$forest, na.rm=T); mb.br2.sforest<-sd(data.BR2$forest, na.rm=T)
mb.br2.mhfp<-mean(data.BR2$hfp, na.rm=T);       mb.br2.shfp<-sd(data.BR2$hfp, na.rm=T)

mb.br3.mprec<-mean(data.BR3$prec, na.rm=T);     mb.br3.sprec<-sd(data.BR3$prec, na.rm=T)
mb.br3.mforest<-mean(data.BR3$forest, na.rm=T); mb.br3.sforest<-sd(data.BR3$forest, na.rm=T)
mb.br3.mhfp<-mean(data.BR3$hfp, na.rm=T);       mb.br3.shfp<-sd(data.BR3$hfp, na.rm=T)

mb.br4.mprec<-mean(data.BR4$prec, na.rm=T);     mb.br4.sprec<-sd(data.BR4$prec, na.rm=T)
mb.br4.mforest<-mean(data.BR4$forest, na.rm=T); mb.br4.sforest<-sd(data.BR4$forest, na.rm=T)
mb.br4.mhfp<-mean(data.BR4$hfp, na.rm=T);       mb.br4.shfp<-sd(data.BR4$hfp, na.rm=T)


x1<-data.BR1 %>% mutate(prec  = (prec-mb.br1.mprec)/mb.br1.sprec,
                              hfp   = (hfp-mb.br1.mhfp)/mb.br1.shfp,
                              forest= (forest-mb.br1.mforest)/mb.br1.sforest)
x2<-data.BR2 %>% mutate(prec  = (prec-mb.br2.mprec)/mb.br2.sprec,
                              hfp   = (hfp-mb.br2.mhfp)/mb.br2.shfp,
                              forest= (forest-mb.br2.mforest)/mb.br2.sforest)
x3<-data.BR3 %>% mutate(prec  = (prec-mb.br3.mprec)/mb.br3.sprec,
                              hfp   = (hfp-mb.br3.mhfp)/mb.br3.shfp,
                              forest= (forest-mb.br3.mforest)/mb.br3.sforest)
x4<-data.BR4 %>% mutate(prec  = (prec-mb.br4.mprec)/mb.br4.sprec,
                              hfp   = (hfp-mb.br4.mhfp)/mb.br4.shfp,
                              forest= (forest-mb.br4.mforest)/mb.br4.sforest)

# -----------------------------


# 2. make your analysis
#---------------------
# define your parameters
inits<-list(beta = 0, kappa = 0.5)
priors<- list(beta.normal = list(mean = 0, var = 1),
              kappa.unif = c(0, 100))
model.family<-'NB'
tunings<-list(beta = 0.5, kappa = 0.5) #, beta.star = 0.5)

# the following parameters are configured to run the code as demonstration in a fast mode
# to reproduce the results obtained in the analysis use the commented values
n.batchs <- 10 #20
batchs.length <- 100 #5000
n.burns <-  10 #50000
n.thins <-  1  #10
n.chains <- 1  #3
# -----------------------------


# Scenario ER: No dealing with regional differences
# -----------------------------
# model ER.EA
model.formula<- ~ prec + hfp + forest
data.Zero<-list(y=x$counts,
                covs=x)
out1a <- abund(formula = model.formula,
               data = data.Zero,
               inits = inits,
               priors = priors,
               n.batch = n.batchs,
               batch.length = batchs.length,
               tuning = tunings,
               n.omp.threads = 6, #change it according your computer's capacity
               n.report = 200,
               family = model.family,
               verbose = TRUE,
               n.burn = n.burns,
               n.thin = n.thins,
               n.chains = n.chains)
summary(out1a)

# modelo ER.OA
model.formula<- ~ prec + hfp  + forest
data.Zero<-list(y=x$counts,
                covs=x, 
                offset=x$area)
out1b <- abund(formula = model.formula,
               data = data.Zero,
               inits = inits,
               priors = priors,
               n.batch = n.batchs,
               batch.length = batchs.length,
               tuning = tunings,
               n.omp.threads = 6, #change it according your computer's capacity
               n.report = 200,
               family = model.family,
               verbose = TRUE,
               n.burn = n.burns,
               n.thin = n.thins,
               n.chains = n.chains)
summary(out1b)

# modelo ER.CA
model.formula<- ~  prec + hfp  + forest +  log(area)
data.Zero<-list(y=x$counts,
                covs=x)
out1c <- abund(formula = model.formula,
               data = data.Zero,
               inits = inits,
               priors = priors,
               n.batch = n.batchs,
               batch.length = batchs.length,
               tuning = tunings,
               n.omp.threads = 6, #change it according your computer's capacity
               n.report = 200,
               family = model.family,
               verbose = TRUE,
               n.burn = n.burns,
               n.thin = n.thins,
               n.chains = n.chains)
summary(out1c)

# save Scenario ER results 
# save(out1a, file="out1a.Rdata")
# save(out1b, file="out1b.Rdata")
# save(out1c, file="out1c.Rdata")

# -----------------------------


# Scenario 2: Bioregion as fixed factor
# -----------------------------
# model FR.EA
model.formula<- ~ prec + hfp + forest + bioreg
data.Zero<-list(y=x$counts,
                covs=x)
out2a <- abund(formula = model.formula,
               data = data.Zero,
               inits = inits,
               priors = priors,
               n.batch = n.batchs,
               batch.length = batchs.length,
               tuning = tunings,
               n.omp.threads = 6, #change it according your computer's capacity
               n.report = 200,
               family = model.family,
               verbose = TRUE,
               n.burn = n.burns,
               n.thin = n.thins,
               n.chains = n.chains)
summary(out2a)

# model FR.OA
model.formula<- ~ prec + hfp + forest + bioreg
data.Zero<-list(y=x$counts,
                covs=x, 
                offset=x$area)
out2b <- abund(formula = model.formula,
               data = data.Zero,
               inits = inits,
               priors = priors,
               n.batch = n.batchs,
               batch.length = batchs.length,
               tuning = tunings,
               n.omp.threads = 6, #change it according your computer's capacity
               n.report = 200,
               family = model.family,
               verbose = TRUE,
               n.burn = n.burns,
               n.thin = n.thins,
               n.chains = n.chains)
summary(out2b)

# modelo FR.CA
model.formula<- ~  prec + hfp + forest +  log(area) + bioreg
data.Zero<-list(y=x$counts,
                covs=x)
out2c <- abund(formula = model.formula,
               data = data.Zero,
               inits = inits,
               priors = priors,
               n.batch = n.batchs,
               batch.length = batchs.length,
               tuning = tunings,
               n.omp.threads = 6, #change it according your computer's capacity
               n.report = 200,
               family = model.family,
               verbose = TRUE,
               n.burn = n.burns,
               n.thin = n.thins,
               n.chains = n.chains)
summary(out2c)

# save Scenario FR results 
# save(out2a, file="out2a.Rdata")
# save(out2b, file="out2b.Rdata")
# save(out2c, file="out2c.Rdata")

# -----------------------------


# Scenario IR: Bioregion as individual models
# -----------------------------
# model IR.EA
#data
data1<-list(y=x1$counts, 
            covs=x1 %>% select(prec:area))
data2<-list(y=x2$counts, 
            covs=x2 %>% select(prec:area))
data3<-list(y=x3$counts, 
            covs=x3 %>% select(prec:area))
data4<-list(y=x4$counts, 
            covs=x4 %>% select(prec:area))

# modeling
model.formula<- ~ prec + hfp + forest
out3a_BR1 <- abund(formula = model.formula,
                   data = data1,
                   inits = inits, priors = priors,
                   n.batch = n.batchs, batch.length = batchs.length, tuning = tunings,
                   n.omp.threads = 6, #change it according your computer's capacity
                   n.report = 200,
                   family = model.family,
                   verbose = TRUE,
                   n.burn = n.burns, n.thin = n.thins, n.chains = n.chains)
summary(out3a_BR1)

out3a_BR2<- abund(formula = model.formula,
                  data = data2,
                  inits = inits, priors = priors,
                  n.batch = n.batchs, batch.length = batchs.length, tuning = tunings,
                  n.omp.threads = 6, #change it according your computer's capacity
                  n.report = 200,
                  family = model.family,
                  verbose = TRUE,
                  n.burn = n.burns, n.thin = n.thins, n.chains = n.chains)
summary(out3a_BR2)

out3a_BR3 <- abund(formula = model.formula,
                   data = data3,
                   inits = inits, priors = priors,
                   n.batch = n.batchs, batch.length = batchs.length, tuning = tunings,
                   n.omp.threads = 6, #change it according your computer's capacity
                   n.report = 200,
                   family = model.family,
                   verbose = TRUE,
                   n.burn = n.burns, n.thin = n.thins, n.chains = n.chains)
summary(out3a_BR3)

out3a_BR4 <- abund(formula = model.formula,
                   data = data4,
                   inits = inits, priors = priors,
                   n.batch = n.batchs, batch.length = batchs.length, tuning = tunings,
                   n.omp.threads = 6, #change it according your computer's capacity
                   n.report = 200,
                   family = model.family,
                   verbose = TRUE,
                   n.burn = n.burns, n.thin = n.thins, n.chains = n.chains)
summary(out3a_BR4)



# model IR.OA
#data
data1<-list(y=x1$counts, 
            covs=x1 %>% select(prec:area), 
            offset=x1$area)
data2<-list(y=x2$counts, 
            covs=x2 %>% select(prec:area), 
            offset=x2$area)
data3<-list(y=x3$counts, 
            covs=x3 %>% select(prec:area), 
            offset=x3$area)
data4<-list(y=x4$counts, 
            covs=x4 %>% select(prec:area), 
            offset=x4$area)


# modeling
model.formula<- ~ prec + hfp + forest 
out3b_BR1 <- abund(formula = model.formula,
                   data = data1,
                   inits = inits, priors = priors,
                   n.batch = n.batchs, batch.length = batchs.length, tuning = tunings,
                   n.omp.threads = 6, #change it according your computer's capacity
                   n.report = 200,
                   family = model.family,
                   verbose = TRUE,
                   n.burn = n.burns, n.thin = n.thins, n.chains = n.chains)
summary(out3b_BR1)

out3b_BR2 <- abund(formula = model.formula,
                   data = data2,
                   inits = inits, priors = priors,
                   n.batch = n.batchs, batch.length = batchs.length, tuning = tunings,
                   n.omp.threads = 6, #change it according your computer's capacity
                   n.report = 200,
                   family = model.family,
                   verbose = TRUE,
                   n.burn = n.burns, n.thin = n.thins, n.chains = n.chains)
summary(out3b_BR2)

out3b_BR3 <- abund(formula = model.formula,
                   data = data3,
                   inits = inits, priors = priors,
                   n.batch = n.batchs, batch.length = batchs.length, tuning = tunings,
                   n.omp.threads = 6, #change it according your computer's capacity
                   n.report = 200,
                   family = model.family,
                   verbose = TRUE,
                   n.burn = n.burns, n.thin = n.thins, n.chains = n.chains)
summary(out3b_BR3)

out3b_BR4 <- abund(formula = model.formula,
                   data = data4,
                   inits = inits, priors = priors,
                   n.batch = n.batchs, batch.length = batchs.length, tuning = tunings,
                   n.omp.threads = 6, #change it according your computer's capacity
                   n.report = 200,
                   family = model.family,
                   verbose = TRUE,
                   n.burn = n.burns, n.thin = n.thins, n.chains = n.chains)
summary(out3b_BR4)



# model IR.CA
model.formula<- ~ prec + hfp + forest + log(area)

#data
data1<-list(y=x1$counts, 
            covs=x1 %>% select(prec:area))
data2<-list(y=x2$counts, 
            covs=x2 %>% select(prec:area))
data3<-list(y=x3$counts, 
            covs=x3 %>% select(prec:area))
data4<-list(y=x4$counts, 
            covs=x4 %>% select(prec:area))

# modeling
out3c_BR1 <- abund(formula = model.formula,
                   data = data1,
                   inits = inits, priors = priors,
                   n.batch = n.batchs, batch.length = batchs.length, tuning = tunings,
                   n.omp.threads = 6, #change it according your computer's capacity
                   n.report = 200,
                   family = model.family,
                   verbose = TRUE,
                   n.burn = n.burns, n.thin = n.thins, n.chains = n.chains)
summary(out3c_BR1)

out3c_BR2 <- abund(formula = model.formula,
                   data = data2,
                   inits = inits, priors = priors,
                   n.batch = n.batchs, batch.length = batchs.length, tuning = tunings,
                   n.omp.threads = 6, #change it according your computer's capacity
                   n.report = 200,
                   family = model.family,
                   verbose = TRUE,
                   n.burn = n.burns, n.thin = n.thins, n.chains = n.chains)
summary(out3c_BR2)

out3c_BR3 <- abund(formula = model.formula,
                   data = data3,
                   inits = inits, priors = priors,
                   n.batch = n.batchs, batch.length = batchs.length, tuning = tunings,
                   n.omp.threads = 6, #change it according your computer's capacity
                   n.report = 200,
                   family = model.family,
                   verbose = TRUE,
                   n.burn = n.burns, n.thin = n.thins, n.chains = n.chains)
summary(out3c_BR3)

out3c_BR4 <- abund(formula = model.formula,
                   data = data4,
                   inits = inits, priors = priors,
                   n.batch = n.batchs, batch.length = batchs.length, tuning = tunings,
                   n.omp.threads = 6, #change it according your computer's capacity
                   n.report = 200,
                   family = model.family,
                   verbose = TRUE,
                   n.burn = n.burns, n.thin = n.thins, n.chains = n.chains)
summary(out3c_BR4)


# save Scenario IR results 
# save(out3a_BR1, file="out3aBR1.Rdata")
# save(out3a_BR2, file="out3aBR2.Rdata")
# save(out3a_BR3, file="out3aBR3.Rdata")
# save(out3a_BR4, file="out3aBR4.Rdata")
# 
# save(out3b_BR1, file="out3bBR1.Rdata")
# save(out3b_BR2, file="out3bBR2.Rdata")
# save(out3b_BR3, file="out3bBR3.Rdata")
# save(out3b_BR4, file="out3bBR4.Rdata")
# 
# save(out3c_BR1, file="out3cBR1.Rdata")
# save(out3c_BR2, file="out3cBR2.Rdata")
# save(out3c_BR3, file="out3cBR3.Rdata")
# save(out3c_BR4, file="out3cBR4.Rdata")
# -----------------------------


# Scenario SR: spatial random effects
# -----------------------------
coords<-cbind(x$Var1, x$Var2)

dist.coords<-dist(coords)
min.dist <- min(dist.coords)
max.dist <- max(dist.coords)
max.dist # maximum km from side to side in Spain
mean.dist<-mean(dist.coords)
# we think it is better to be more realistic
# we are going to reduce the maximum distance to ~200km
# That distance (200km) is like going through Caceres (bigger province region in Spain)
# So if there were some variable that we are not considering in the model but 
# that could have any possible effect in our response variable, we think that 
# neither management decisions nor other variables that are not being considered
# would over pass that distance
max.dist<-max.dist/5
mean.dist<-mean.dist/5

data.Zero<-list(y=x$counts,
                covs= x %>% select(prec:area),
                # offset=x$area,
                coords=coords
)

# MCMC parameters
inits.list <- list(beta = 0, 
                   kappa = 0.5, 
                   sigma.sq = 0.5,
                   phi = 4/mean.dist,
                   w = rep(0, length(data.Zero$y))
)

prior.list <- list(beta.normal = list(mean = 0, var = 1),
                   phi.unif = c(10/max.dist, 2/10),
                   sigma.sq.ig = c(2, 1),
                   kappa.unif = c(0, 100)
                   )

tunings <- list(phi = 0.5, kappa = 0.5, beta = 0.1, beta.star = 0.1, w = 0.5)

# modelo SR.EA
out4a<- spAbund(formula = ~  prec + hfp + forest,
                data = data.Zero,
                family = model.family,
                
                inits = inits.list,
                priors = prior.list,
                
                n.batch = n.batchs,
                batch.length = batchs.length,
                tuning = tunings,
                
                cov.model = 'exponential',
                
                NNGP = TRUE,
                n.neighbors = 15,
                search.type = 'cb',
                
                accept.rate = 0.43,
                n.omp.threads = 6,
                n.report = 200,
                verbose = TRUE,
                n.burn = n.burns,
                n.thin = n.thins,
                n.chains = n.chains)
summary(out4a)

# modelo SR.OA
data.Zero<-list(y=x$counts,
                covs= x %>% select(prec:area),
                offset=x$area,
                coords=coords
)
out4b<- spAbund(formula = ~  prec + hfp + forest,
                data = data.Zero,
                family = model.family,
                
                inits = inits.list,
                priors = prior.list,
                
                n.batch = n.batchs,
                batch.length = batchs.length,
                tuning = tunings,
                
                cov.model = 'exponential',
                
                NNGP = TRUE,
                n.neighbors = 15,
                search.type = 'cb',
                
                accept.rate = 0.43,
                n.omp.threads = 6,
                n.report = 200,
                verbose = TRUE,
                n.burn = n.burns,
                n.thin = n.thins,
                n.chains = n.chains)
summary(out4b)


# modelo SR.CA
data.Zero<-list(y=x$counts,
                covs= x %>% select(prec:area),
                coords=coords
)

out4c <- spAbund(formula = ~  prec + hfp + forest + log(area),
                 data = data.Zero,
                 family = model.family,
                 
                 inits = inits.list,
                 priors = prior.list,
                 
                 n.batch = n.batchs,
                 batch.length = batchs.length,
                 tuning = tunings,
                 
                 cov.model = 'exponential',
                 
                 NNGP = TRUE,
                 n.neighbors = 15,
                 search.type = 'cb',
                 
                 accept.rate = 0.43,
                 n.omp.threads = 6,
                 n.report = 200,
                 verbose = TRUE,
                 n.burn = n.burns,
                 n.thin = n.thins,
                 n.chains = n.chains)

summary(out4c)

# save Scenario SR results 
# save(out4a, file="out4a.Rdata")
# save(out4b, file="out4b.Rdata")
# save(out4c, file="out4c.Rdata")

# -----------------------------


# 3. make your predictions
# -----------------------------

# 3.1 same spatial unit
# -----------------------------
# 3.1.1. load all Spanish municipalities with covariates
# Spanish municipalities are publicly available at 
# https://datos.gob.es/es/catalogo/e00125901-spaignllm
data.predict<-st_read("./data_modeling_predict.gpkg", layer="pMunicipalities")


# 3.1.2. scale your variables with the values of the data used for modeling
data.predict<-data.predict %>% mutate(prec=((prec-mb.me.prec)/mb.sd.prec), 
                                      forest=((forest-mb.me.forest)/mb.sd.forest), 
                                      hfp=((hfp-mb.me.hfp)/mb.sd.hfp), 
                                      area=area,  
                                      bioreg=as.factor(bioreg))

data.predict<-rbind(data.predict %>% filter(NATCODE %in% data.bm$NATCODE) %>% mutate(predData=1),
                    data.predict %>% filter(!NATCODE %in% data.bm$NATCODE) %>% mutate(predData=0))

x<- tibble(data.predict)

# 3.1.3. Predictions 
# 3.1.3.1 load your model ER (1) and SR (4) results
# -----------------------------
# load(file="out1a.Rdata")
# load(file="out1b.Rdata")
# load(file="out1c.Rdata")
# 
# load(file="out4a.Rdata")
# load(file="out4b.Rdata")
# load(file="out4c.Rdata")

X.0 <- cbind(1, x$prec, x$hfp, x$forest)
colnames(X.0) <- c('(Intercept)', 'prec', "hfp", 'forest')
coords<-cbind(x$Var1, x$Var2)
coords.0<-as.matrix(coords)

# EA (a models)
out.m1a <- predict(out1a, X.0, ignore.RE = FALSE)
mu.0.quants.m1a <- apply(out.m1a$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)

out.m4a <- predict(out4a, X.0, coords.0, ignore.RE = FALSE, 
                   n.omp.threads = 7) #change it according to your computer's capacity
mu.0.quants.m4a <- apply(out.m4a$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)

# OA (b models)
out.m1b <- predict(out1b, X.0, ignore.RE = FALSE)
mu.0.quants.m1b <- apply(out.m1b$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)

out.m4b <- predict(out4b, X.0, coords.0, ignore.RE = FALSE, 
                   n.omp.threads=7) #change it according to your computer's capacity
mu.0.quants.m4b <- apply(out.m4b$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)

# CA (c models)
X.0 <- cbind(1, x$prec, x$hfp, x$forest, log(x$area))
colnames(X.0) <- c('(Intercept)', 'prec', "hfp", 'forest','area')

out.m1c <- predict(out1c, X.0, ignore.RE = FALSE)
mu.0.quants.m1c <- apply(out.m1c$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)

out.m4c <- predict(out4c, X.0, coords.0, ignore.RE = FALSE, 
                   n.omp.threads=7) #change it according to your computer's capacity
mu.0.quants.m4c <- apply(out.m4c$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)


# 3.1.3.2 load your model FR (2) results
# load(file="out2a.Rdata")
# load(file="out2b.Rdata")
# load(file="out2c.Rdata")
x0<-cbind(x,
          dummy_cols(x$bioreg) %>% 
            rename(bioreg1=.data_1, bioreg2=.data_2, bioreg3=.data_3, bioreg4=.data_4) %>% 
            select(bioreg1, bioreg2, bioreg3, bioreg4))

X.0 <- cbind(1, x$prec, x$hfp, x$forest, x0$bioreg2, x0$bioreg3, x0$bioreg4)
colnames(X.0) <- c('(Intercept)', 'prec', "hfp",  'forest', 'bioreg2', 'bioreg3', 'bioreg4')
out.m2a <- predict(out2a, X.0, ignore.RE = FALSE)
mu.0.quants.m2a <- apply(out.m2a$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)

out.m2b <- predict(out2b, X.0, ignore.RE = FALSE)
mu.0.quants.m2b <- apply(out.m2b$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)

X.0 <- cbind(1, x$prec, x$hfp, x$forest, log(x$area), x0$bioreg2, x0$bioreg3, x0$bioreg4)
colnames(X.0) <- c('(Intercept)', 'prec', "hfp",  'forest', 'area', 'bioreg2', 'bioreg3', 'bioreg4')
out.m2c <- predict(out2c, X.0, ignore.RE = FALSE)
mu.0.quants.m2c <- apply(out.m2c$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)


# 3.1.3.2 load your model IR (3) results
#data individuals br
# load(file="species_model3_BR1.Rdata")
# load(file="species_model3_BR2.Rdata")
# load(file="species_model3_BR3.Rdata")
# load(file="species_model3_BR4.Rdata")

# data individuals br
br1.data.predict<-data.predict[data.predict$bioreg==1,]
br2.data.predict<-data.predict[data.predict$bioreg==2,]
br3.data.predict<-data.predict[data.predict$bioreg==3,]
br4.data.predict<-data.predict[data.predict$bioreg==4,]

br1.data.predict<-br1.data.predict %>% mutate(prec  = (prec-mb.br1.mprec)/mb.br1.sprec,
                                              hfp   = (hfp-mb.br1.mhfp)/mb.br1.shfp,
                                              forest= (forest-mb.br1.mforest)/mb.br1.sforest)
br2.data.predict<-br2.data.predict %>% mutate(prec  = (prec-mb.br2.mprec)/mb.br2.sprec,
                                              hfp   = (hfp-mb.br2.mhfp)/mb.br2.shfp,
                                              forest= (forest-mb.br2.mforest)/mb.br2.sforest)
br3.data.predict<-br3.data.predict %>% mutate(prec  = (prec-mb.br3.mprec)/mb.br3.sprec,
                                              hfp   = (hfp-mb.br3.mhfp)/mb.br3.shfp,
                                              forest= (forest-mb.br3.mforest)/mb.br3.sforest)
br4.data.predict<-br4.data.predict %>% mutate(prec  = (prec-mb.br4.mprec)/mb.br4.sprec,
                                              hfp   = (hfp-mb.br4.mhfp)/mb.br4.shfp,
                                              forest= (forest-mb.br4.mforest)/mb.br4.sforest)

br1.data.predict<-rbind(br1.data.predict %>% filter(NATCODE %in% data.BR1$NATCODE) %>% mutate(predData=1),
                        br1.data.predict %>% filter(!NATCODE %in% data.BR1$NATCODE) %>% mutate(predData=0))

br2.data.predict<-rbind(br2.data.predict %>% filter(NATCODE %in% data.BR2$NATCODE) %>% mutate(predData=1),
                        br2.data.predict %>% filter(!NATCODE %in% data.BR2$NATCODE) %>% mutate(predData=0))

br3.data.predict<-rbind(br3.data.predict %>% filter(NATCODE %in% data.BR3$NATCODE) %>% mutate(predData=1),
                        br3.data.predict %>% filter(!NATCODE %in% data.BR3$NATCODE) %>% mutate(predData=0))

br4.data.predict<-rbind(br4.data.predict %>% filter(NATCODE %in% data.BR4$NATCODE) %>% mutate(predData=1),
                        br4.data.predict %>% filter(!NATCODE %in% data.BR4$NATCODE) %>% mutate(predData=0))


x1<- tibble(br1.data.predict[c(2:7)]) 
x2<- tibble(br2.data.predict[c(2:7)]) 
x3<- tibble(br3.data.predict[c(2:7)]) 
x4<- tibble(br4.data.predict[c(2:7)]) 

# load(file="out3aBR1.Rdata")
# load(file="out3aBR2.Rdata")
# load(file="out3aBR3.Rdata")
# load(file="out3aBR4.Rdata")
# 
# load(file="out3bBR1.Rdata")
# load(file="out3bBR2.Rdata")
# load(file="out3bBR3.Rdata")
# load(file="out3bBR4.Rdata")

#predictions EA (a) & OA (b)
X.0 <- cbind(1, x1$prec, x1$hfp,  x1$forest) 
colnames(X.0) <- c('(Intercept)', 'prec', 'hfp', 'forest')
out3a.pred.BR1 <- predict(out3a_BR1, X.0, ignore.RE = TRUE)
mu.0.quants.3a_BR1 <- apply(out3a.pred.BR1$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)
out3b.pred.BR1 <- predict(out3b_BR1, X.0, ignore.RE = TRUE)
mu.0.quants.3b_BR1 <- apply(out3b.pred.BR1$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)

X.0 <- cbind(1, x2$prec, x2$hfp,  x2$forest) 
colnames(X.0) <- c('(Intercept)', 'prec', 'hfp', 'forest')
out3a.pred.BR2 <- predict(out3a_BR2, X.0, ignore.RE = TRUE)
mu.0.quants.3a_BR2 <- apply(out3a.pred.BR2$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)
out3b.pred.BR2 <- predict(out3b_BR2, X.0, ignore.RE = TRUE)
mu.0.quants.3b_BR2 <- apply(out3b.pred.BR2$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)

X.0 <- cbind(1, x3$prec, x3$hfp,  x3$forest) 
colnames(X.0) <- c('(Intercept)', 'prec', 'hfp', 'forest')
out3a.pred.BR3 <- predict(out3a_BR3, X.0, ignore.RE = TRUE)
mu.0.quants.3a_BR3 <- apply(out3a.pred.BR3$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)
out3b.pred.BR3 <- predict(out3b_BR3, X.0, ignore.RE = TRUE)
mu.0.quants.3b_BR3 <- apply(out3b.pred.BR3$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)

X.0 <- cbind(1, x4$prec, x4$hfp,  x4$forest) 
colnames(X.0) <- c('(Intercept)', 'prec', 'hfp', 'forest')
out3a.pred.BR4 <- predict(out3a_BR4, X.0, ignore.RE = TRUE)
mu.0.quants.3a_BR4 <- apply(out3a.pred.BR4$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)
out3b.pred.BR4 <- predict(out3b_BR4, X.0, ignore.RE = TRUE)
mu.0.quants.3b_BR4 <- apply(out3b.pred.BR4$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)

# load(file="out3cBR1.Rdata")
# load(file="out3cBR2.Rdata")
# load(file="out3cBR3.Rdata")
# load(file="out3cBR4.Rdata")

#predictions CA (c)
X.0 <- cbind(1, x1$prec, x1$hfp,  x1$forest,  log(x1$area)) 
colnames(X.0) <- c('(Intercept)', 'prec', 'hfp', 'forest', 'area')
out3c.pred.BR1 <- predict(out3c_BR1, X.0, ignore.RE = TRUE)
mu.0.quants.3c_BR1 <- apply(out3c.pred.BR1$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)

X.0 <- cbind(1, x2$prec, x2$hfp,  x2$forest, log(x2$area)) 
colnames(X.0) <- c('(Intercept)', 'prec', 'hfp', 'forest','area')
out3c.pred.BR2 <- predict(out3c_BR2, X.0, ignore.RE = TRUE)
mu.0.quants.3c_BR2 <- apply(out3c.pred.BR2$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)

X.0 <- cbind(1, x3$prec, x3$hfp,  x3$forest, log(x3$area)) 
colnames(X.0) <- c('(Intercept)', 'prec', 'hfp', 'forest', 'area')
out3c.pred.BR3 <- predict(out3c_BR3, X.0, ignore.RE = TRUE)
mu.0.quants.3c_BR3 <- apply(out3c.pred.BR3$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)

X.0 <- cbind(1, x4$prec, x4$hfp,  x4$forest,  log(x4$area)) 
colnames(X.0) <- c('(Intercept)', 'prec', 'hfp', 'forest', 'area')
out3c.pred.BR4 <- predict(out3c_BR4, X.0, ignore.RE = TRUE)
mu.0.quants.3c_BR4 <- apply(out3c.pred.BR4$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)


x<- tibble(data.predict)

# 3. Save predictions in municipality spatial layer 
spAbund.r<-data.predict %>% mutate("spA.m1a"=mu.0.quants.m1a[2,], 
                                   "spA.m1b"=mu.0.quants.m1b[2,]*x$area, 
                                   "spA.m1c"=mu.0.quants.m1c[2,],
                                   
                                   "spA.m1a.lowCI"=mu.0.quants.m1a[1,], 
                                   "spA.m1b.lowCI"=mu.0.quants.m1b[1,]*x$area, 
                                   "spA.m1c.lowCI"=mu.0.quants.m1c[1,],
                                   
                                   "spA.m1a.highCI"=mu.0.quants.m1a[3,], 
                                   "spA.m1b.highCI"=mu.0.quants.m1b[3,]*x$area, 
                                   "spA.m1c.highCI"=mu.0.quants.m1c[3,]) %>% 
  mutate("spA.m2a"=mu.0.quants.m2a[2,], 
         "spA.m2b"=mu.0.quants.m2b[2,]*x$area, 
         "spA.m2c"=mu.0.quants.m2c[2,],
         
         "spA.m2a.lowCI"=mu.0.quants.m2a[1,], 
         "spA.m2b.lowCI"=mu.0.quants.m2b[1,]*x$area, 
         "spA.m2c.lowCI"=mu.0.quants.m2c[1,],
         
         "spA.m2a.highCI"=mu.0.quants.m2a[3,], 
         "spA.m2b.highCI"=mu.0.quants.m2b[3,]*x$area, 
         "spA.m2c.highCI"=mu.0.quants.m2c[3,])

spAbund.r3<-cbind(rbind(cbind(br1.data.predict, 
                              data.frame("spA.m3a"=mu.0.quants.3a_BR1[2,], 
                                         "spA.m3b"=mu.0.quants.3b_BR1[2,]*br1.data.predict$area, 
                                         "spA.m3c"=mu.0.quants.3c_BR1[2,])),
                        cbind(br2.data.predict, 
                              data.frame("spA.m3a"=mu.0.quants.3a_BR2[2,], 
                                         "spA.m3b"=mu.0.quants.3b_BR2[2,]*br2.data.predict$area, 
                                         "spA.m3c"=mu.0.quants.3c_BR2[2,])),
                        cbind(br3.data.predict,       
                              data.frame("spA.m3a"=mu.0.quants.3a_BR3[2,], 
                                         "spA.m3b"=mu.0.quants.3b_BR3[2,]*br3.data.predict$area,
                                         "spA.m3c"=mu.0.quants.3c_BR3[2,])),
                        cbind(br4.data.predict,       
                              data.frame("spA.m3a"=mu.0.quants.3a_BR4[2,], 
                                         "spA.m3b"=mu.0.quants.3b_BR4[2,]*br4.data.predict$area,
                                         "spA.m3c"=mu.0.quants.3c_BR4[2,]))),
                  
                  rbind(data.frame("spA.m3a.lowCI"=mu.0.quants.3a_BR1[1,], 
                                   "spA.m3b.lowCI"=mu.0.quants.3b_BR1[1,]*br1.data.predict$area, 
                                   "spA.m3c.lowCI"=mu.0.quants.3c_BR1[1,]),
                        data.frame("spA.m3a.lowCI"=mu.0.quants.3a_BR2[1,], 
                                   "spA.m3b.lowCI"=mu.0.quants.3b_BR2[1,]*br2.data.predict$area, 
                                   "spA.m3c.lowCI"=mu.0.quants.3c_BR2[1,]),
                        data.frame("spA.m3a.lowCI"=mu.0.quants.3a_BR3[1,], 
                                   "spA.m3b.lowCI"=mu.0.quants.3b_BR3[1,]*br3.data.predict$area,
                                   "spA.m3c.lowCI"=mu.0.quants.3c_BR3[1,]),
                        data.frame("spA.m3a.lowCI"=mu.0.quants.3a_BR4[1,], 
                                   "spA.m3b.lowCI"=mu.0.quants.3b_BR4[1,]*br4.data.predict$area,
                                   "spA.m3c.lowCI"=mu.0.quants.3c_BR4[1,])),
                  
                  rbind(data.frame("spA.m3a.highCI"=mu.0.quants.3a_BR1[3,], 
                                   "spA.m3b.highCI"=mu.0.quants.3b_BR1[3,]*br1.data.predict$area, 
                                   "spA.m3c.highCI"=mu.0.quants.3c_BR1[3,]),
                        data.frame("spA.m3a.highCI"=mu.0.quants.3a_BR2[3,], 
                                   "spA.m3b.highCI"=mu.0.quants.3b_BR2[3,]*br2.data.predict$area, 
                                   "spA.m3c.highCI"=mu.0.quants.3c_BR2[3,]),
                        data.frame("spA.m3a.highCI"=mu.0.quants.3a_BR3[3,], 
                                   "spA.m3b.highCI"=mu.0.quants.3b_BR3[3,]*br3.data.predict$area,
                                   "spA.m3c.highCI"=mu.0.quants.3c_BR3[3,]),
                        data.frame("spA.m3a.highCI"=mu.0.quants.3a_BR4[3,], 
                                   "spA.m3b.highCI"=mu.0.quants.3b_BR4[3,]*br4.data.predict$area,
                                   "spA.m3c.highCI"=mu.0.quants.3c_BR4[3,]))
)

spAbund.r<-spAbund.r %>% left_join(spAbund.r3 %>% as_tibble() %>% select(NATCODE, matches("spA.m3")), by="NATCODE")

spAbund.r<-spAbund.r %>% mutate("spA.m4a"=mu.0.quants.m4a[2,], 
                                "spA.m4b"=mu.0.quants.m4b[2,]*x$area, 
                                "spA.m4c"=mu.0.quants.m4c[2,],
                                
                                "spA.m4a.lowCI"=mu.0.quants.m4a[1,], 
                                "spA.m4b.lowCI"=mu.0.quants.m4b[1,]*x$area, 
                                "spA.m4c.lowCI"=mu.0.quants.m4c[1,],
                                
                                "spA.m4a.highCI"=mu.0.quants.m4a[3,], 
                                "spA.m4b.highCI"=mu.0.quants.m4b[3,]*x$area, 
                                "spA.m4c.highCI"=mu.0.quants.m4c[3,]
)

# save your predictions 
# st_write(spAbund.r, "/outSpeciesPredictions.gpkg", layer="municipalityPredictions_dataset_species")
# -----------------------------

# 3.2 downscaling (grid spatial unit)
# -----------------------------
# 3.2.1 load the grid with covariates
# -------------------------
grid2km<-st_read("./data_modeling_predict.gpkg", layer="pGridDownscaling")
grid2km<-grid2km %>% select(id, prec, hfp, forest, area, bioreg, Var1, Var2)

# 3.2.1.1 scale your variables with the values of the data used for modeling
# -------------------------
grid2km_scale<-grid2km %>% mutate(prec   = ((prec-mb.me.prec)/mb.sd.prec),
                                  forest = ((forest-mb.me.forest)/mb.sd.forest),
                                  hfp    = ((hfp-mb.me.hfp)/mb.sd.hfp),
                                  area   = area, 
                                  bioreg = as.factor(bioreg), 
                                  Var1,  # x_centroid
                                  Var2   # y_centroid
                                  )
# summary(grid2km_scale)
# 3.2.1.2. remove those cells which covariates have NA values
grid2km_scale<-grid2km_scale[!is.na(grid2km_scale$hfp),]
grid2km_scale<-grid2km_scale[!is.na(grid2km_scale$prec),]


# 3.2.2.
# 3.2.2.1 load your model ER (1) results in case you have remove it
# -----------------------------
# load("out1a.Rdata")
X.0 <- cbind(1, grid2km_scale$prec, grid2km_scale$hfp, grid2km_scale$forest)
colnames(X.0) <- c('(Intercept)', 'prec', "hfp",  'forest')
out.m1a <- predict(out1a, X.0, ignore.RE = FALSE)
mu.0.quants.m1a.grid <- apply(out.m1a$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)
# rm(out1a, out.m1a); gc() # in case you need to free memory of your global environment

# load("out1b.Rdata")
out.m1b <- predict(out1b, X.0, ignore.RE = FALSE)
mu.0.quants.m1b.grid <- apply(out.m1b$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)
# rm(out1b, out.m1b); gc() # in case you need to free memory of your global environment

# load("out1c.Rdata")
X.0 <- cbind(1, grid2km_scale$prec, grid2km_scale$hfp, grid2km_scale$forest, log(grid2km_scale$area))
colnames(X.0) <- c('(Intercept)', 'prec', "hfp", 'forest', 'area')
out.m1c <- predict(out1c, X.0, ignore.RE = FALSE)
mu.0.quants.m1c.grid <- apply(out.m1c$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)
# rm(out1c, out.m1c); gc()  # in case you need to free memory of your global environment

# 3.2.2.2 load your model FR (2) results in case you have remove it
# -----------------------------
grid2km_scale<-cbind(grid2km_scale, 
                     dummy_cols(grid2km_scale$bioreg) %>% rename(bioreg1=.data_1, bioreg2=.data_2, bioreg3=.data_3, bioreg4=.data_4) %>% select(bioreg1, bioreg2, bioreg3, bioreg4))

# load("out2a.Rdata")
X.0 <- cbind(1, grid2km_scale$prec, grid2km_scale$hfp, grid2km_scale$forest, grid2km_scale$bioreg2, grid2km_scale$bioreg3, grid2km_scale$bioreg4)
colnames(X.0) <- c('(Intercept)', 'prec', "hfp", 'forest','bioreg2', 'bioreg3', 'bioreg4')

out.m2a <- predict(out2a, X.0, ignore.RE = FALSE)
mu.0.quants.m2a.grid <- apply(out.m2a$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)
# rm(out2a, out.m2a); gc()  # in case you need to free memory of your global environment

# load("out2b.Rdata")
out.m2b <- predict(out2b, X.0, ignore.RE = FALSE)
mu.0.quants.m2b.grid <- apply(out.m2b$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)
# rm(out2b, out.m2b); gc()  # in case you need to free memory of your global environment

# load("out2c.Rdata")
X.0 <- cbind(1, grid2km_scale$prec, grid2km_scale$hfp, grid2km_scale$forest, log(grid2km_scale$area),
             grid2km_scale$bioreg2, grid2km_scale$bioreg3, grid2km_scale$bioreg4)
colnames(X.0) <- c('(Intercept)', 'prec', "hfp",  'forest', 'area', 'bioreg2', 'bioreg3', 'bioreg4')
out.m2c <- predict(out2c, X.0, ignore.RE = FALSE)
mu.0.quants.m2c.grid <- apply(out.m2c$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)
# rm(out2c, out.m2c); gc()  # in case you need to free memory of your global environment


# 3.2.2.3 load your model IR (3) results in case you have remove it
# -----------------------------
#br1
# load("out3aBR1.Rdata")
# load("out3bBR1.Rdata")
# load("out3cBR1.Rdata")
grid2km_scale_BR1 <- grid2km_scale %>% filter(bioreg==1) %>% 
                         mutate(prec  = (prec-mb.br1.mprec)/mb.br1.sprec,
                                hfp   = (hfp-mb.br1.mhfp)/mb.br1.shfp,
                                forest= (forest-mb.br1.mforest)/mb.br1.sforest)

X.0 <- cbind(1, grid2km_scale_BR1$prec, grid2km_scale_BR1$hfp, grid2km_scale_BR1$forest)
colnames(X.0) <- c('(Intercept)', 'prec', "hfp",  'forest')
out.m3aBR1 <- predict(out3a_BR1, X.0, ignore.RE = FALSE)
mu.0.quants.m3aBR1.grid <- apply(out.m3aBR1$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)

out.m3bBR1 <- predict(out3b_BR1, X.0, ignore.RE = FALSE)
mu.0.quants.m3bBR1.grid <- apply(out.m3bBR1$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)
# rm(out3a_BR1, out.m3aBR1, out3b_BR1, out.m3bBR1); gc() # in case you need to free memory of your global environment

X.0 <- cbind(1, grid2km_scale_BR1$prec, grid2km_scale_BR1$hfp, grid2km_scale_BR1$forest, log(grid2km_scale_BR1$area))
colnames(X.0) <- c('(Intercept)', 'prec', "hfp",  'forest', 'area')
out.m3cBR1 <- predict(out3c_BR1, X.0, ignore.RE = FALSE)
mu.0.quants.m3cBR1.grid <- apply(out.m3cBR1$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)
# rm(out3c_BR1, out.m3cBR1); gc() # in case you need to free memory of your global environment

#br2
# load("out3aBR2.Rdata")
# load("out3bBR2.Rdata")
# load("out3cBR2.Rdata")
grid2km_scale_BR2 <- grid2km_scale %>% filter(bioreg==2) %>%
                        mutate(prec  = (prec-mb.br2.mprec)/mb.br2.sprec,
                               hfp   = (hfp-mb.br2.mhfp)/mb.br2.shfp,
                               forest= (forest-mb.br2.mforest)/mb.br2.sforest)

X.0 <- cbind(1, grid2km_scale_BR2$prec, grid2km_scale_BR2$hfp, grid2km_scale_BR2$forest)
colnames(X.0) <- c('(Intercept)', 'prec', "hfp",  'forest')
out.m3aBR2 <- predict(out3a_BR2, X.0, ignore.RE = FALSE)
mu.0.quants.m3aBR2.grid <- apply(out.m3aBR2$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)

out.m3bBR2 <- predict(out3b_BR2, X.0, ignore.RE = FALSE)
mu.0.quants.m3bBR2.grid <- apply(out.m3bBR2$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)
# rm(out3a_BR2, out.m3aBR2, out3b_BR2, out.m3bBR2); gc() # in case you need to free memory of your global environment

X.0 <- cbind(1, grid2km_scale_BR2$prec, grid2km_scale_BR2$hfp,  grid2km_scale_BR2$forest, log(grid2km_scale_BR2$area))
colnames(X.0) <- c('(Intercept)', 'prec', "hfp",  'forest', 'area')
out.m3cBR2 <- predict(out3c_BR2, X.0, ignore.RE = FALSE)
mu.0.quants.m3cBR2.grid <- apply(out.m3cBR2$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)
# rm(out3c_BR2, out.m3cBR2); gc() # in case you need to free memory of your global environment

#  br 3
# load("out3aBR3.Rdata")
# load("out3bBR3.Rdata")
# load("out3cBR3.Rdata")
grid2km_scale_BR3 <- grid2km_scale %>% filter(bioreg==3) %>% 
                        mutate(prec  = (prec-mb.br3.mprec)/mb.br3.sprec,
                               hfp   = (hfp-mb.br3.mhfp)/mb.br3.shfp,
                               forest= (forest-mb.br3.mforest)/mb.br3.sforest)

X.0 <- cbind(1, grid2km_scale_BR3$prec, grid2km_scale_BR3$hfp, grid2km_scale_BR3$forest)
colnames(X.0) <- c('(Intercept)', 'prec', "hfp",  'forest')
out.m3aBR3 <- predict(out3a_BR3, X.0, ignore.RE = FALSE)
mu.0.quants.m3aBR3.grid <- apply(out.m3aBR3$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)

out.m3bBR3 <- predict(out3b_BR3, X.0, ignore.RE = FALSE)
mu.0.quants.m3bBR3.grid <- apply(out.m3bBR3$mu.0.samples, 2,  quantile, c(0.025, 0.5, 0.975), na.rm=T)
# rm(out3a_BR3, out.m3aBR3, out3b_BR3, out.m3bBR3); gc() # in case you need to free memory of your global environment

X.0 <- cbind(1, grid2km_scale_BR3$prec, grid2km_scale_BR3$hfp,  grid2km_scale_BR3$forest, log(grid2km_scale_BR3$area))
colnames(X.0) <- c('(Intercept)', 'prec', "hfp",  'forest', 'area')
out.m3cBR3 <- predict(out3c_BR3, X.0, ignore.RE = FALSE)
mu.0.quants.m3cBR3.grid <- apply(out.m3cBR3$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)
# rm(out3c_BR3, out.m3cBR3); gc() # in case you need to free memory of your global environment

#  br 4
# load("out3aBR4.Rdata")
# load("out3bBR4.Rdata")
# load("out3cBR4.Rdata")
grid2km_scale_BR4 <- grid2km_scale %>% filter(bioreg==4) %>% 
                           mutate(prec  = (prec-mb.br4.mprec)/mb.br4.sprec,
                                  hfp   = (hfp-mb.br4.mhfp)/mb.br4.shfp,
                                  forest= (forest-mb.br4.mforest)/mb.br4.sforest)

X.0 <- cbind(1, grid2km_scale_BR4$prec, grid2km_scale_BR4$hfp, grid2km_scale_BR4$forest)
colnames(X.0) <- c('(Intercept)', 'prec', "hfp",  'forest')
out.m3aBR4 <- predict(out3a_BR4, X.0, ignore.RE = FALSE)
mu.0.quants.m3aBR4.grid <- apply(out.m3aBR4$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)

out.m3bBR4 <- predict(out3b_BR4, X.0, ignore.RE = FALSE)
mu.0.quants.m3bBR4.grid <- apply(out.m3bBR4$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)
# rm(out3a_BR4, out.m3aBR4, out3b_BR4, out.m3bBR4); gc() # in case you need to free memory of your global environment

X.0 <- cbind(1, grid2km_scale_BR4$prec, grid2km_scale_BR4$hfp,  grid2km_scale_BR4$forest, log(grid2km_scale_BR4$area))
colnames(X.0) <- c('(Intercept)', 'prec', "hfp",  'forest', 'area')
out.m3cBR4 <- predict(out3c_BR4, X.0, ignore.RE = FALSE)
mu.0.quants.m3cBR4.grid <- apply(out.m3cBR4$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)
# rm(out3c_BR4, out.m3cBR4); gc() # in case you need to free memory of your global environment

# Save predictions in the grid spatial layer 
# The global environment memory will be high. 
# This will avoid having to repeat the process if 
# something cracks while predicting 4 approach
# --------------------------------------------------
grid2km_scale2<-grid2km_scale %>% mutate("spA.m1a"=mu.0.quants.m1a.grid[2,],
                                         "spA.m1b"=mu.0.quants.m1b.grid[2,]*area,
                                         "spA.m1c"=mu.0.quants.m1c.grid[2,],
                                         "spA.m2a"=mu.0.quants.m2a.grid[2,],
                                         "spA.m2b"=mu.0.quants.m2b.grid[2,]*area,
                                         "spA.m2c"=mu.0.quants.m2c.grid[2,],
                                         
                                         "spA.m1a.lowCI"=mu.0.quants.m1a.grid[1,],
                                         "spA.m1b.lowCI"=mu.0.quants.m1b.grid[1,]*area,
                                         "spA.m1c.lowCI"=mu.0.quants.m1c.grid[1,],
                                         "spA.m2a.lowCI"=mu.0.quants.m2a.grid[1,],
                                         "spA.m2b.lowCI"=mu.0.quants.m2b.grid[1,]*area,
                                         "spA.m2c.lowCI"=mu.0.quants.m2c.grid[1,], 
                                         
                                         "spA.m1a.highCI"=mu.0.quants.m1a.grid[3,],
                                         "spA.m1b.highCI"=mu.0.quants.m1b.grid[3,]*area,
                                         "spA.m1c.highCI"=mu.0.quants.m1c.grid[3,],
                                         "spA.m2a.highCI"=mu.0.quants.m2a.grid[3,],
                                         "spA.m2b.highCI"=mu.0.quants.m2b.grid[3,]*area,
                                         "spA.m2c.highCI"=mu.0.quants.m2c.grid[3,])

grid2km_scale2<-grid2km_scale2 %>%
  left_join(rbind(cbind(grid2km_scale_BR1, "spA.m3a"=mu.0.quants.m3aBR1.grid[2,], "spA.m3b"=mu.0.quants.m3bBR1.grid[2,]*grid2km_scale_BR1$area, "spA.m3c"=mu.0.quants.m3cBR1.grid[2,]),
                  cbind(grid2km_scale_BR2, "spA.m3a"=mu.0.quants.m3aBR2.grid[2,], "spA.m3b"=mu.0.quants.m3bBR2.grid[2,]*grid2km_scale_BR2$area, "spA.m3c"=mu.0.quants.m3cBR2.grid[2,]),
                  cbind(grid2km_scale_BR3, "spA.m3a"=mu.0.quants.m3aBR3.grid[2,], "spA.m3b"=mu.0.quants.m3bBR3.grid[2,]*grid2km_scale_BR3$area, "spA.m3c"=mu.0.quants.m3cBR3.grid[2,]),
                  cbind(grid2km_scale_BR4, "spA.m3a"=mu.0.quants.m3aBR4.grid[2,], "spA.m3b"=mu.0.quants.m3bBR4.grid[2,]*grid2km_scale_BR4$area, "spA.m3c"=mu.0.quants.m3cBR4.grid[2,])) %>%
              as_tibble() %>%
              dplyr::select(id, spA.m3a, spA.m3b, spA.m3c), by="id") %>%

  left_join(rbind(cbind(grid2km_scale_BR1, "spA.m3a.lowCI"=mu.0.quants.m3aBR1.grid[1,], "spA.m3b.lowCI"=mu.0.quants.m3bBR1.grid[1,]*grid2km_scale_BR1$area, "spA.m3c.lowCI"=mu.0.quants.m3cBR1.grid[1,]),
                  cbind(grid2km_scale_BR2, "spA.m3a.lowCI"=mu.0.quants.m3aBR2.grid[1,], "spA.m3b.lowCI"=mu.0.quants.m3bBR2.grid[1,]*grid2km_scale_BR2$area, "spA.m3c.lowCI"=mu.0.quants.m3cBR2.grid[1,]),
                  cbind(grid2km_scale_BR3, "spA.m3a.lowCI"=mu.0.quants.m3aBR3.grid[1,], "spA.m3b.lowCI"=mu.0.quants.m3bBR3.grid[1,]*grid2km_scale_BR3$area, "spA.m3c.lowCI"=mu.0.quants.m3cBR3.grid[1,]),
                  cbind(grid2km_scale_BR4, "spA.m3a.lowCI"=mu.0.quants.m3aBR4.grid[1,], "spA.m3b.lowCI"=mu.0.quants.m3bBR4.grid[1,]*grid2km_scale_BR4$area, "spA.m3c.lowCI"=mu.0.quants.m3cBR4.grid[1,])) %>%
              as_tibble() %>%
              dplyr::select(id, spA.m3a.lowCI, spA.m3b.lowCI, spA.m3c.lowCI), by="id") %>%

  left_join(rbind(cbind(grid2km_scale_BR1, "spA.m3a.highCI"=mu.0.quants.m3aBR1.grid[3,], "spA.m3b.highCI"=mu.0.quants.m3bBR1.grid[3,]*grid2km_scale_BR1$area, "spA.m3c.highCI"=mu.0.quants.m3cBR1.grid[3,]),
                  cbind(grid2km_scale_BR2, "spA.m3a.highCI"=mu.0.quants.m3aBR2.grid[3,], "spA.m3b.highCI"=mu.0.quants.m3bBR2.grid[3,]*grid2km_scale_BR2$area, "spA.m3c.highCI"=mu.0.quants.m3cBR2.grid[3,]),
                  cbind(grid2km_scale_BR3, "spA.m3a.highCI"=mu.0.quants.m3aBR3.grid[3,], "spA.m3b.highCI"=mu.0.quants.m3bBR3.grid[3,]*grid2km_scale_BR3$area, "spA.m3c.highCI"=mu.0.quants.m3cBR3.grid[3,]),
                  cbind(grid2km_scale_BR4, "spA.m3a.highCI"=mu.0.quants.m3aBR4.grid[3,], "spA.m3b.highCI"=mu.0.quants.m3bBR4.grid[3,]*grid2km_scale_BR4$area, "spA.m3c.highCI"=mu.0.quants.m3cBR4.grid[3,])) %>%
              as_tibble() %>%
              dplyr::select(id, spA.m3a.highCI, spA.m3b.highCI, spA.m3c.highCI), by="id")

# st_write(grid2km_scale2, "./outSpeciesPredictions.gpkg", layer="gridPredictions.temp123_dataset_species")


# 3.2.2.4 load your model SR (4) results in case you have remove it
# -----------------------------
# load("out4a.Rdata")
coords.grid<-cbind(grid2km_scale2$Var1, grid2km_scale2$Var2)
coords.0<-as.matrix(coords.grid); rm(coords.grid)

X.0 <- cbind(1, grid2km_scale2$prec, grid2km_scale2$hfp, grid2km_scale2$forest)
colnames(X.0) <- c('(Intercept)', 'prec', "hfp",  'forest')
out.m4a <- predict(out4a, X.0, coords.0, ignore.RE = FALSE, 
                   n.omp.threads = 7) #change it according to your computer's capacity
mu.0.quants.m4a.grid <- apply(out.m4a$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)
save(mu.0.quants.m4a.grid, file="pred.grid.m4a.grid.Rdata")
# rm(out4a, out.m4a); gc()  # in case you need to free memory of your global environment

# load("out4b.Rdata")
out.m4b <- predict(out4b, X.0, coords.0, ignore.RE = FALSE, 
                   n.omp.threads = 7) #change it according to your computer's capacity
mu.0.quants.m4b.grid <- apply(out.m4b$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)
save(mu.0.quants.m4b.grid, file="pred.grid.m4b.grid.Rdata")
# rm(out4b, out.m4b); gc()  # in case you need to free memory of your global environment

# load("out4c.Rdata")
X.0 <- cbind(1, grid2km_scale2$prec, grid2km_scale2$hfp, grid2km_scale2$forest, log(grid2km_scale2$area))
colnames(X.0) <- c('(Intercept)', 'prec', "hfp",  'forest', 'area')
out.m4c <- predict(out4c, X.0, coords.0, ignore.RE = FALSE, 
                   n.omp.threads = 7) #change it according to your computer's capacity
mu.0.quants.m4c.grid <- apply(out.m4c$mu.0.samples, 2, quantile, c(0.025, 0.5, 0.975), na.rm=T)
save(mu.0.quants.m4c.grid, file="pred.grid.m4c.grid.Rdata")
# rm(out4c, out.m4c); gc()  # in case you need to free memory of your global environment

# 3.2.3. Join all predictions in a same layer and save it
grid2km_scale2<-grid2km_scale2 %>% 
  mutate("spA.m4a"=mu.0.quants.m4a.grid[2,], 
         "spA.m4b"=mu.0.quants.m4b.grid[2,]*area, 
         "spA.m4c"=mu.0.quants.m4c.grid[2,]) %>% 
  
  mutate("spA.m4a.lowCI"=mu.0.quants.m4a.grid[1,],
         "spA.m4b.lowCI"=mu.0.quants.m4b.grid[1,]*area,
         "spA.m4c.lowCI"=mu.0.quants.m4c.grid[1,],
         
         "spA.m4a.highCI"=mu.0.quants.m4a.grid[3,],
         "spA.m4b.highCI"=mu.0.quants.m4b.grid[3,]*area,
         "spA.m4c.highCI"=mu.0.quants.m4c.grid[3,])

st_write(grid2km_scale2, "./outSpeciesPredictions.gpkg", layer="gridPredictions_dataset_species")
# -----------------------------