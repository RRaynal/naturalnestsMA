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
library(ape)
library(lme4)
library(lmerTest)
library(emmeans)
library(MuMIn)
library(mgcv)
library(gamm4)



#load data
data <- read_csv("Macro Ecology Data.csv")
data$Long <- as.numeric(data$Long)
data$Lat <- as.numeric(data$Lat)
data$Mean <- as.numeric(data$Mean)
data$Major.group <- as.factor(data$Major.group)

reptiles <- data %>% filter(Group2 %in% c("Sea Turtle", "Freshwater Turtle", "Crocodilian", "Lizard/snake"))


####################################################################################################

## validating the missing mean temperature data from minimum and maximum mean temps ##
valid <- lm(Mean ~ test, data = data)
summary(valid)
# r=0.9863
# slope=1.06


## Basic stats of data make up 

# Calculate the counts for each level of Major.group
group_counts <- count(data, Major.group)

# Calculate the total number of rows in the data frame
total_rows <- nrow(data)

# Calculate the percentage of data within each level
percentage_per_group <- (group_counts$n / total_rows) * 100

# Add the percentages to the data frame
group_counts$Percentage <- percentage_per_group

# Print or display the result
print(group_counts)

## Within reptiles
# Calculate the counts for each level of Reptile
reptile_counts <- count(reptiles, Group2)

# Calculate the total number of rows in the data frame
total_rowsR <- nrow(reptiles)

# Calculate the percentage of data within each level
percentage_per_groupR <- (reptile_counts$n / total_rowsR) * 100

# Add the percentages to the data frame
reptile_counts$Percentage <- percentage_per_groupR

# Print or display the result
print(reptile_counts)

#Within invertebrates
# Calculate the counts for each level of Reptile
invert_counts <- count(Invert, Group3)

# Calculate the total number of rows in the data frame
total_rowsI <- nrow(Invert)

# Calculate the percentage of data within each level
percentage_per_groupI <- (invert_counts$n / total_rowsI) * 100

# Add the percentages to the data frame
invert_counts$Percentage <- percentage_per_groupI

# Print or display the result
print(invert_counts)

## How many species within each group

Invert %>%
  pull(Species) %>%
  n_distinct()

reptiles %>%
  pull(Species) %>%
  n_distinct()

## Seperate the fish and amphibians to get their species counts
fish <- data %>% filter(Major.group %in% c("Fish"))
amphibians <- data %>% filter(Major.group %in% c("Amphibian"))


fish %>%
  pull(Species) %>%
  n_distinct()


amphibians %>%
  pull(Species) %>%
  n_distinct()

SeaTurts %>%
  pull(Species) %>%
  n_distinct()

# Calculate the counts for each level of sea turtle
seaturts_counts <- count(SeaTurts, Species)

# Calculate the total number of rows in the data frame
total_rowsST <- nrow(SeaTurts)

# Calculate the percentage of data within each level
percentage_per_groupST <- (seaturts_counts$n / total_rowsST) * 100

# Add the percentages to the data frame
seaturts_counts$Percentage <- percentage_per_groupST

# Print or display the result
print(seaturts_counts)

## I want to see if a certain author appears more often than others

# Specify the target word you want to search for
target_word <- "Shine"

# Search for the target word in the 'Author' column and count unique occurrences
word_counts <- sum(grepl(target_word, data$authors, ignore.case = TRUE, fixed = TRUE))

# Print or display the count
cat("Occurrences of", target_word, ":", word_counts, "\n")

## 17 datapoints attributed to Shine

# Specify the target word you want to search for
target_word <- "Janzen"

# Search for the target word in the 'Author' column and count unique occurrences
word_counts <- sum(grepl(target_word, data$authors, ignore.case = TRUE, fixed = TRUE))

# Print or display the count
cat("Occurrences of", target_word, ":", word_counts, "\n")

## 20 datapoints attributed to Janzen



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

####################### Phylogeny #############################


#load species list
speciesList2<- read_csv("species.csv")

## Based on Dan Nobles code 2016 ####
## Access taxon relationships from Open Tree of Life
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

#################################################################
## Try a PGLMM - a linear mixed effect model that includes a phylogeny

library(phyr)
library(ape)

data$study_ID <- as.character(data$study_ID)
data$sp_ <- data$Animal
data$Mean <- as.numeric(data$Mean)

# First run a model without any grouping.

pglmm <- phyr::pglmm(Mean ~ abs(Lat) + (1 | sp_) + (1 | study_ID) + (1 | Animal), 
                     data = data, 
                     cov_ranef = list(sp = tl2$tip.label),
                     family = "gaussian")
summary(pglmm)


# The phylogeny is accounting for most of the variance, however absolute latitude is not significant


# Change the reference level for Major.group
data$Major.group <- relevel(data$Major.group, ref = "Fish")


pglmm2 <- phyr::pglmm(Mean ~ abs(Lat)*Major.group-1 + (1 | sp_) + (1 | Animal) + (1 | study_ID), 
                      data = data, 
                      cov_ranef = list(sp = tl2$tip.label),
                      family = "gaussian")
summary(pglmm2)


# The inclusion of the "Major.group" as a fixed effect in the second model has revealed significant 
# interactions between abs(Lat) and the different levels of "Major.group," which were not evident in the first model
# with only abs(Lat) as the fixed effect. The effect of abs(Lat) on Mean varies across different groups 

# I want to get a p-value for the interaction on its own, so I will perform a 
# maximum liklihood test
## cant seem to use the anova function with a pglmm, tried lmertest and lmtest. 


pglmm3 <- phyr::pglmm(Mean ~ abs(Lat)*Group2-1 + (1 | sp_) + (1 | Animal) + (1 | study_ID), 
                      data = data, 
                      cov_ranef = list(sp = tl2$tip.label),
                      family = "gaussian")
summary(pglmm3)


## The effect of absolute latitude on the mean is only significant between crocodiles and lizards/snakes
## weird.
## The phylogeny is absorbing a lot of the variance across all the models
## species that share a closer evolutionary relationship are expected to have more
## similar mean nest temperatures in relation to latitude, compared to species that
## are more distantly related.

pglmm4 <- phyr::pglmm(Mean ~ abs(Lat)*Water + (1 | sp_) + (1 | Animal) + (1 | study_ID), 
                      data = data, 
                      cov_ranef = list(sp = tl2$tip.label),
                      family = "gaussian")
summary(pglmm4)
# The interaction between latitude and temperatures between animals that lay their eggs in 
# the water or on land is significant, nothing else is. Weird interpretation. 


# Count the number of non-missing values in the column
sum(!is.na(data$Mean))

# Calculate the range of values in the column
range(data$Mean, na.rm = TRUE)






############################ Standard deviation analysis #########################

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

##pglmm - is standard deviation different across latitude?

pglmmSD <- phyr::pglmm(Among_SD ~ abs(Lat) + (1 | sp_) + (1 | study_ID) + (1 | Animal), 
                     data = data, 
                     cov_ranef = list(sp = tl2$tip.label),
                     family = "gaussian")
summary(pglmmSD)

# INTERESTING - phylogeny is not absorbing most of the variance, variance across the random effects
# is quite low. In addition, there is a very significant relationship between standard deviation among nests
# and absolute latitude. As latitude increases, SD increases.


# Add in major taxanomic group to take a look if there are any patterns there
pglmmSD2 <- phyr::pglmm(Among_SD ~ abs(Lat)*Major.group-1 + (1 | sp_) + (1 | study_ID) + (1 | Animal), 
                       data = data, 
                       cov_ranef = list(sp = tl2$tip.label),
                       family = "gaussian")
summary(pglmmSD2)
# However, when you add in the taxanomic groups, latitude is no longer a significant predictor, 
# now nothing is significant.


### Some more models with the different groupings of the taxa
## Reptiles only
pglmmSD3 <- phyr::pglmm(Among_SD ~ abs(Lat)*Group2-1 + (1 | sp_) + (1 | study_ID)+ (1 | Animal), 
                      data = data, 
                      cov_ranef = list(sp = tl2$tip.label),
                      family = "gaussian")
summary(pglmmSD3)

## Same as above, no significance and no difference between reptile groups



### Grouped by laying eggs in the water or on land 
pglmmSD4 <- phyr::pglmm(Among_SD ~ abs(Lat)*Water + (1 | sp_) + (1 | study_ID)+ (1 | Animal), 
                        data = data, 
                        cov_ranef = list(sp = tl2$tip.label),
                        family = "gaussian")
summary(pglmmSD4)

## latitude is significant, interaction is close

## Latitude seems to be the most important determinant of among nest SD
# Now lets make some graphics










##############################################################################
##################### Not using the below at the moment ######################

## Linear models without phylogeny

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

#############################################################################
### Run a Bayesian model #####

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