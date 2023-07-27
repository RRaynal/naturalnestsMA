# Libraries
library(ggplot2)
library(dplyr)
library(readr)
library(stats)
library(viridis)
library(tidyverse)
library(ggthemes)
library(MCMCglmm)
library(phytools)
library(rotl)
library(metafor)
library(orchaRd)
library(ape)
library(lme4)
library(lmerTest)
library(emmeans)
library(MuMIn)
library(mgcv)
library(gamm4)



#load data
data <- read_csv("~/Dropbox/PHD/Natural nest temps/Macro Ecology Analysis/Macro Ecology Data.csv")
data$Long <- as.numeric(data$Long)
data$Lat <- as.numeric(data$Lat)
data$Mean <- as.numeric(data$Mean)

reptiles <- data %>% filter(Group2 %in% c("Sea Turtle", "Freshwater Turtle", "Crocodilian", "Lizard/snake"))


####################################################################################################

## validating the missing mean temperature data from minimum and maximum mean temps ##
valid <- lm(Mean ~ test, data = data)
summary(valid)
# r=0.9863
# slope=1.06
 

## Look at the difference between using a GAM and a LMER model using all the data, not grouped 

valid.lm <- lmer(Mean ~ abs(Lat) + (1|study_ID) + (1|Species), data = data, REML = TRUE, na.action = na.exclude)
summary(valid.lm)
r.squaredGLMM(valid.lm)
# Number of obs: 392, groups:  study_ID, 253; Species, 115
## R2 of entire model (conditional) = 0.92


valid.gam <- gamm(Mean~s(abs(Lat)), random=list(study_ID=~1, Species=~1), data=data)
summary(valid.gam$gam)
#R-sq.(adj) =  0.227   
#Scale est. = 2.6907    n = 392

#Models are comparable as they are both using the same data, R2 are quite different 
## Check AIC values

AIC(valid.lm)
#[1] 1930.339
AIC(valid.gam)
#[1] 2154.136

## Linear model looks to be a better fit.

###############################################################################

##########################################################################################

## Run some models without the phylogeny, including species.


## Major taxonomic group

major.mod <- lmer(Mean ~ abs(Lat) + Major.group + (1|study_ID) + (1|Species), data = data, REML = TRUE, na.action = na.exclude)
summary(major.mod)
anova(major.mod)

emm <- emmeans(major.mod, ~ Major.group)

# Perform Tukey's HSD post hoc test
contrast(emm, "pairwise", adjust = "tukey")


## Interaction?
major.int <- lmer(Mean ~ abs(Lat)*Major.group + (1|study_ID) + (1|Species), data = data, REML = TRUE, na.action = na.exclude)
summary(major.int)
anova(major.int)

plot(major.int, which = 2)

## interaction is significant

### Some more models with the different groupings of the taxa

## Reptiles only
mod.reptile <- lmer(Mean ~ abs(Lat) + Group2 + (1|study_ID) + (1|Species), data = reptiles, REML = TRUE, na.action = na.exclude)
summary(mod.reptile)
anova(mod.reptile)

# Compute EMMs
emmrep <- emmeans(mod.reptile, "Group2")
# Perform Tukey's HSD post hoc test
contrast(emmrep, "pairwise", adjust = "tukey")

## Same model but interaction between group*lat
int.reptile <- lmer(Mean ~ abs(Lat)*Group2 + (1|study_ID) + (1|Species), data = reptiles, REML = TRUE, na.action = na.exclude)
summary(int.reptile)
anova(int.reptile)

# No interaction

### Grouped by laying eggs in the water or on land 
mod.water<- lmer(Mean ~ abs(Lat)*Water + (1|study_ID) + (1|Species), data = data, REML = TRUE, na.action = na.exclude)
summary(mod.water)
anova(mod.water)

## latitude and interaction is significant

# Compute EMMs
emmwater <- emmeans(mod.water, "Water")

# Perform Tukey's HSD post hoc test
contrast(emmwater, "pairwise", adjust = "tukey")

### No significant difference between land and water

#################################################################
## Try a pglmm

library(phyr)
library(ape)

## I am getting an error from the Random section, it wants a valid covariance matrix
## I feel like thats what I am providing but the model isnt happy, maybe ask Patrice
## if we decide to keep the phylogeny

pglmm <- phyr::pglmm(Mean ~ abs(Lat), 
                     data = data, 
                     random = ~1|phylogeny, ~1|animal,
                     family = "gaussian")

summary(pglmm)

#################################################################
################# Phylogeny + Bayesian stats ####################
#################################################################


## Create a phylogeny
#load species list
speciesList2 <- read_csv("~/Dropbox/PHD/Natural nest temps/Naturalnests/species.csv")

## Based on Dan Nobles code 2016 ####
## Phylogeny
## Access taxon relationships from Open Tree of Life
# needed for phylogenetic control
animal<-as.character(data$Animal)

# Match species in dataset
tree2 = tnrs_match_names(as.character(unique(speciesList2$Species)), context_name = "Animals")

## Getting HTTP failure error with the tol_induced_subtree step, so following ROTL documentation run the following:

in_tree <- is_in_tree(ott_id(tree2))
in_tree


#Create a tree based on itt id's found on the open tree of life
tl2 <- tol_induced_subtree(ott_id(tree2)[in_tree]) 


#Remove ott labels on end to make sure to matches species in dataset
tl2$tip.label = strip_ott_ids(tl2$tip.label, remove_underscores=FALSE)

#getting a phylogenetic correlaiton matrix from the tree
tl2_brlen <- compute.brlen(tl2, method = "Grafen", power = 0.5) 
tl2_brlen$tip.label[tl2_brlen$tip.label=='Apis_mellifera_(species_in_domain_Eukaryota)'] <- "Apis_mellifera" 

# Check trees are ultrametric
is.ultrametric(tl2_brlen)

# plot the tree
plot(tl2, cex = 0.8, label.offset = .1, no.margin = TRUE)

# Generate the phylogenetic matrix
tl2_brlen$node.label <- NULL
R_phylo <- vcv(tl2_brlen, corr = TRUE)
print(R_phylo)

#checking the spelling etc matches due to error generating the phylogenetic matrix
unique(data$Animal)
rownames(R_phylo) %in% unique(data$Animal)
rownames(R_phylo)[!rownames(R_phylo) %in% unique(data$Animal)]


data$phylogeny <- data$Animal

#######################################################################################
## Patrice and I had a good go at trying to run frequentist models, 
## but we kept running into problems with not being able to include random or fixed effects aswell as the phylogeny
## we tried pglmm and almer.


### SO! Run a Bayesian model

prior <- list(R = list(V = 1, nu = 0.002),
              G = list(G1 = list(V = 1, nu = 0.002,
                                 alpha.mu = 0,
                                 alpha.V = 1000), 
                       G2 = list(V = 1, nu = 0.002,
                                 alpha.mu = 0,
                                 alpha.V = 1000), 
                       G3 = list(V = 1, nu = 0.002,
                                 alpha.mu = 0,
                                 alpha.V = 1000)))


data$tip.label <- data$Animal

data2<- data[data$tip.label %in% tl2_brlen$tip.label, ]
data2$Lat[is.na(data2$Lat)] <- mean(data2$Lat, na.rm = TRUE)

Ainv<- inverseA(tl2_brlen)$Ainv

data2<-as.data.frame(data2)
mod<- MCMCglmm(Mean ~ abs(Lat),
               random= ~study_ID+Species+tip.label, 
               ginverse=list(tip.label = Ainv),
               nitt=10000,
               thin=50,
               burnin=3000,
               prior=prior,
               data=data2)
summary(mod)
ggplot(data, aes(x=abs(Lat), y=Mean))+geom_point()+geom_smooth(method="lm")

## Differences between taxa
mod2<- MCMCglmm(Mean ~ abs(Lat)+Group,
                random= ~study_ID+Species+tip.label, 
                ginverse=list(tip.label = Ainv),
                nitt=10000,
                thin=50,
                burnin=3000,
                prior=prior,
                data=data2)
summary(mod2)

### The model that makes the most sense for looking at mean temp differences b/w groups ####
mod3<- MCMCglmm(Mean ~ abs(Lat)+Group-1,
                random= ~study_ID+Species+tip.label, 
                ginverse=list(tip.label = Ainv),
                nitt=10000,
                thin=50,
                burnin=3000,
                prior=prior,
                data=data2)
summary(mod3)


## Add an interaction term
mod4<- MCMCglmm(Mean ~ abs(Lat)*Group-1,
                random= ~study_ID+Species+tip.label, 
                ginverse=list(tip.label = Ainv),
                nitt=10000,
                thin=50,
                burnin=3000,
                prior=prior,
                data=data2)
summary(mod4) # might need to remove fish and amphibians because they have very low number of datapoints, which clearly overestimates the parameters

lambda<-mod4$VCV[,"tip.label"]/rowSums(mod4$VCV)
posterior.mode(lambda)
HPDinterval(lambda)

prior2 <- list(R = list(V = 1, nu = 0.002),
               G = list(G1 = list(V = 1, nu = 0.002,
                                  alpha.mu = 0,
                                  alpha.V = 1000), 
                        G2 = list(V = 1, nu = 0.002,
                                  alpha.mu = 0,
                                  alpha.V = 1000)))
mod5<- MCMCglmm(Mean ~ abs(Lat)*Group-1,
                random= ~study_ID+Species,
                nitt=10000,
                thin=50,
                burnin=3000,
                prior=prior2,
                data=data2)
summary(mod5) 

#############################################################################

## Now we have a few extra months, we will check out the standard deviation data
# First see what the data looks like
ggplot(data, aes(x=abs(Lat), y=Among_SD))+
  geom_point()+ 
  theme_bw() +
  scale_colour_brewer(palette="Dark2")+
  ylim(0, 5)

## Quite scattered, if anything linear
## Check out the groups
data %>%
  ggplot(aes(x = abs(Lat), y = Among_SD, color = Major.group)) +
  geom_point() +
  labs(x="Latitude", y="Mean temperature", title= "") +
  theme_bw()

## Look at the difference between using a GAM and a LMER model using all the data, not grouped 

validSD.lm <- lmer(Among_SD ~ abs(Lat) + (1|study_ID) + (1|Species), data = data, REML = TRUE, na.action = na.exclude)
summary(validSD.lm)
r.squaredGLMM(validSD.lm)
# Number of obs: 255, groups:  study_ID, 169; Species, 70
## R2 of entire model (conditional) = 0.92


validSD.gam <- gamm(Among_SD~s(abs(Lat)), random=list(study_ID=~1, Species=~1), data=data)
summary(validSD.gam$gam)
#R-sq.(adj) =  0.275   
#Scale est. = 0.105    n = 255

#Models are comparable as they are both using the same data, R2 are quite different 
## Check AIC values

AIC(validSD.lm)
#[1] 561.56
AIC(validSD.gam)
#[1] 562.97

## Using lmer or GAM doesnt seem to be different, linear models are much easier
## to interpret so will continue with linear unless everyone else thinks otherwise. 

##lmer - is standard deviation different across latitude?
SDmod <- lmer(Among_SD ~ abs(Lat) + (1|study_ID) + (1|Species), data = data, REML = TRUE, na.action = na.exclude)
summary(SDmod)


#Fixed effects:
#             Estimate  Std. Error        df t value Pr(>|t|)    
# (Intercept) 7.935e-01  1.935e-01 1.350e+02   4.101 7.08e-05 ***
# abs(Lat)    2.018e-02  5.266e-03 2.273e+02   3.832 0.000165 ***


# Add in major taxanomic group to take a look if there are any patterns there
SDmodgroup <- lmer(Among_SD ~ abs(Lat) + Major.group + (1|study_ID) + (1|Species), data = data, REML = TRUE, na.action = na.exclude)
summary(SDmodgroup)

# Taxanomic group is not significant


## Interaction?
SDmodint <- lmer(Among_SD ~ abs(Lat)*Major.group + (1|study_ID) + (1|Species), data = data, REML = TRUE, na.action = na.exclude)
summary(SDmodint)
anova(SDmodint)

## interaction is not significant

### Some more models with the different groupings of the taxa

## Reptiles only
mod.reptileSD <- lmer(Among_SD ~ abs(Lat) + Group2 + (1|study_ID) + (1|Species), data = reptiles, REML = TRUE, na.action = na.exclude)
summary(mod.reptileSD)
anova(mod.reptileSD)

## No difference between reptile groups

# Compute EMMs
emmreptileSD <- emmeans(mod.reptileSD, "Group2")

# Perform Tukey's HSD post hoc test
contrast(emmreptileSD, "pairwise", adjust = "tukey")


### Grouped by laying eggs in the water or on land 
mod.waterSD<- lmer(Among_SD ~ abs(Lat)*Water + (1|study_ID) + (1|Species), data = data, REML = TRUE, na.action = na.exclude)
summary(mod.waterSD)
anova(mod.waterSD)

## latitude is significant, interaction is close

## Latitude seems to be the most important determinant of among nest SD
# Now lets make some graphics