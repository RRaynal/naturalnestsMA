###  Using the predict function to  create more accurate plots in ggplot from the models ## 
#creating another script while I work it out#

# Libraries
library(ggplot2)
library(dplyr)
library(readr)
library(stats)
library(viridis)
library(tidyverse)
library(ggthemes)
library(phytools)
library(rotl)
library(ape)
library(lme4)
library(lmerTest)
library(emmeans)
library(MuMIn)
library(phyr)


#load data
data <- read_csv("Macro Ecology Data.csv")
data$Long <- as.numeric(data$Long)
data$Lat <- as.numeric(data$Lat)
data$Mean <- as.numeric(data$Mean)
data$Major.group <- as.factor(data$Major.group)



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
data$study_ID <- as.character(data$study_ID)
data$sp_ <- data$Animal


reptiles <- data %>% filter(Group2 %in% c("Sea Turtle", "Freshwater Turtle", "Crocodilian", "Lizard/snake"))
reptiles$Group2 <- as.factor(reptiles$Group2)

#Change the reference level for Major.group
data$Major.group <- relevel(data$Major.group, ref = "Reptile")


pglmmfull <- phyr::pglmm(Mean ~ abs(Lat)*Major.group + (1 | sp_) + (1 | Animal) + (1 | study_ID), 
                           data = data, 
                           cov_ranef = list(sp = tl2$tip.label),
                           family = "gaussian")
summary(pglmmfull)

#Generate data for predictions (Patrice code)
new_data <- data.frame(Lat = seq(min(data$Lat), max(data$Lat), length = 100),
                       Mean = NA,
                       sp_ = NA,
                       study_ID = NA,
                       Animal = NA)

## predict function from the phyr package doesn't seem to include a way to use newdata
fullpredict<- pglmm_predicted_values(
  pglmmfull,
  cpp = TRUE,
  gaussian.pred = c("nearest_node"),
  re.form = NULL,
  type = c("response"),
)

## Try the predict function from the lme4 package, but it just reverts to using the pglmm function
predictions <- predict(pglmmfull, newdata = new_data, type = "response", re.form = NA, level = 0.95)


## Try Bayesian model instead, then use the predict function from the MCMCglmm package
library(MCMCglmm)

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
mod<- MCMCglmm(Mean ~ abs(Lat)*Group,
               random= ~study_ID+Species+tip.label, 
               ginverse=list(tip.label = Ainv),
               nitt = 100000,
               thin = 40, 
               burnin = 20000,
               prior=prior,
               pr= TRUE,
               verbose = FALSE,
               data=data2)

summary(mod)
plot(mod)
## nice trace plots, this seems like a good number of iterations

new_data <- data.frame(Mean = NA,
                       Lat = seq(min(data2$Lat), max(data2$Lat), 100),
                       tip.label = NA, 
                       study_ID = NA,
                       Species = NA)



# Keep getting errors about datasets not being the same length, so this 
# method allows both the newdata and data datasets to be the same length.

# Create new_data with the same structure as the original data
new_data <- data2

# Specify the values for the predictor variable you want to predict (Mean)
new_data$Mean <- NA

# Specify the range of values for Lat
new_data$Lat <- seq(min(data2$Lat), max(data2$Lat), length.out = nrow(data2))


pred <- predict(mod, newdata = new_data, marginal=NULL, interval = "confidence")


# Error in MCMCglmm(fixed = object$Fixed$formula, random = object$Random$formula,  : 
# all data are missing. Use singular.ok=TRUE to sample these effects, but use an informative prior!

