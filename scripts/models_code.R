## ---------------------
# Title: Evaluation of modeling approaches to account for hunting data 
# variability across large spatial scales.
# Code provided for repoducibility of modeling approaches
# Submitted to: Journal of Applied Ecology
# Date: 11/02/2025 (mm/dd/yyyy)
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
                         Var1,             # x_centroid (km units)
                         Var2,             # y_centroid  (km units)
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