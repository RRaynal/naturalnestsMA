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
reptiledata$Group2 <- as.factor(reptiledata$Group2)

reptiles <- data %>% filter(Group2 %in% c("Sea Turtle", "Freshwater Turtle", "Crocodilian", "Lizard/snake"))


####################################################################################################

## validating the missing mean temperature data from minimum and maximum mean temps ##
valid <- lm(Mean ~ test, data = data)

summary(valid)
# r=0.9863
# slope=1.06


## Basic stats of data make up 

# Calculate the counts for each level of Major.group
group_counts <- count(udata, Group)

# Calculate the total number of rows in the data frame
total_rows <- nrow(udata)

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

#Sea turtle counts
ST <- reptiles %>% filter(Group2 %in% c("Sea Turtle"))

# Calculate the counts for each level of Reptile
SeaT_counts <- count(ST, Species)

# Calculate the total number of rows in the data frame
total_rowsST <- nrow(ST)

# Calculate the percentage of data within each level
percentage_per_groupST <- (SeaT_counts$n / total_rowsST) * 100

# Add the percentages to the data frame
SeaT_counts$Percentage <- percentage_per_groupST

# Print or display the result
print(SeaT_counts)


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

reptiles %>%
  group_by(Group2) %>%
  summarise(SpeciesCount = n_distinct(Species))

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

# Specify the target word you want to search for
target_word <- "Booth"

# Search for the target word in the 'Author' column and count unique occurrences
word_counts <- sum(grepl(target_word, data$authors, ignore.case = TRUE, fixed = TRUE))

# Print or display the count
cat("Occurrences of", target_word, ":", word_counts, "\n")

## 34

# Specify the target word you want to search for
target_word <- "Limpus"

# Search for the target word in the 'Author' column and count unique occurrences
word_counts <- sum(grepl(target_word, data$authors, ignore.case = TRUE, fixed = TRUE))

# Print or display the count
cat("Occurrences of", target_word, ":", word_counts, "\n")

## 14


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



#############################################################################
    ## MCMCGLMM MODELS INCLUDED IN STUDY ##

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



data2$animal <-data2$tip.label
data2<- data[data$tip.label %in% tl2_brlen$tip.label, ]
data2$Lat[is.na(data2$Lat)] <- mean(data2$Lat, na.rm = TRUE)

Ainv<- inverseA(tl2_brlen)$Ainv
data2<-as.data.frame(data2)
data2 <- mutate(data2, abs_lat = abs(Lat)) # Added absolute latitude column

data2$Taxa <- as.factor(data2$Taxa)
data2$Taxa <- relevel(data2$Taxa, ref = "Reptile")



data2<-as.data.frame(data2)

### Mean temp ###

## All taxa - Taxonomic grouping    
mod<- MCMCglmm(Mean ~ abs_lat*Taxa,
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



 ## Reptiles only - Mean Temp ##

reptiles <- data2 %>% filter(Group2 %in% c("Sea Turtle", "Freshwater Turtle", "Crocodilian", "Lizard/snake"))
reptiles$Group2 <- as.factor(reptiles$Group2)
reptiles$Group2 <- relevel(reptiles$Group2, ref = "Sea Turtle")

## Differences between taxa
mod2<- MCMCglmm(Mean ~ abs_lat*Group2,
                random= ~study_ID+Species+tip.label, 
                ginverse=list(tip.label = Ainv),
                nitt = 100000,
                thin = 40, 
                burnin = 20000,
                prior=prior,
                pr= TRUE,
                verbose = FALSE,
                data = reptiles)

summary(mod2)

###  Water/ Land - Mean temp

mod3<- MCMCglmm(Mean ~ abs_lat*Water,
                random= ~study_ID+Species+tip.label, 
                ginverse=list(tip.label = Ainv),
                nitt = 100000,
                thin = 40, 
                burnin = 20000,
                prior=prior,
                pr= TRUE,
                verbose = FALSE,
                data = data2)
summary(mod3)




#############################################################################

## Standard deviation analysis

## All taxa 
modSD<- MCMCglmm(log(Among_SD) ~ abs_lat*Taxa,
               random= ~study_ID+Species+tip.label, 
               ginverse=list(tip.label = Ainv),
               nitt = 100000,
               thin = 40, 
               burnin = 20000,
               prior=prior,
               pr= TRUE,
               verbose = FALSE,
               data=SD_data)
summary(modSD)



## Reptiles only - SD ##

reptiles <- data2 %>% filter(Group2 %in% c("Sea Turtle", "Freshwater Turtle", "Crocodilian", "Lizard/snake"))
reptiles$Group2 <- as.factor(reptiles$Group2)
reptiles$Group2 <- relevel(reptiles$Group2, ref = "Sea Turtle")

## Differences between taxa
mod2SD<- MCMCglmm(log(Among_SD) ~ abs_lat*Group2,
                random= ~study_ID+Species+tip.label, 
                ginverse=list(tip.label = Ainv),
                nitt = 100000,
                thin = 40, 
                burnin = 20000,
                prior=prior,
                pr= TRUE,
                verbose = FALSE,
                data = reptiles)

summary(mod2SD)

###  Water/ Land - SD

mod3SD<- MCMCglmm(log(Among_SD) ~ abs_lat*Water,
                random= ~study_ID+Species+tip.label, 
                ginverse=list(tip.label = Ainv),
                nitt = 100000,
                thin = 40, 
                burnin = 20000,
                prior=prior,
                pr= TRUE,
                verbose = FALSE,
                data = data2)
summary(mod3SD)
