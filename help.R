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
library(MCMCglmm)
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
data2$animal <-data2$tip.label

Ainv<- inverseA(tl2_brlen)$Ainv

data2<-as.data.frame(data2)
mod<- MCMCglmm(Mean ~ abs(Lat)*Major.group,
               random= ~study_ID+Species+animal, 
               pedigree = tl2_brlen,
               #ginverse=list(tip.label = Ainv),
               nitt = 100000,
               thin = 40, 
               burnin = 20000,
               prior=prior,
               pr= TRUE,
               verbose = TRUE,
               data=data2)

summary(mod)
plot(mod)

# Specify the latitude ranges for each Major.group

latitude_ranges <- list(
  Amphibian = seq(-43.24, 45, 0.25),
  Reptile = seq(-42.98, 62.4, 0.25),
  Invertebrate = seq(-41.11, 62.8, 0.25),
  Fish = seq(-1.21, 48, 0.25)
)

# Initialize an empty list to store the estimated means
estimates_list <- list()

# Loop through each Major.group and its associated latitude range
for (group in names(latitude_ranges)) {
  # Make predictions for this group using the specific latitudes
  est <- emmeans(mod, specs = "Lat", data = data2, 
                 at = list(Lat = latitude_ranges[[group]]))
  
  # Store the estimates in the list
  estimates_list[[group]] <- as.tibble(est)
}



# This code didn't create a column for the group names, so I am writing it to do it manually.
write.csv(est.data, file = "data2mod.csv", row.names = FALSE)
#data with groups
est.data <- read_csv("data2mod.csv")

## Now make some scatterplots using the predictions
est.data$Major.group <- as.factor(est.data$Major.group)

Majorscat <- ggplot(est.data, aes(x = abs(Lat), y = emmean, col = Major.group, fill = Major.group )) +
  geom_ribbon(aes(ymin = lower.HPD, ymax = upper.HPD, col = Major.group, fill = Major.group), alpha = 0.2) +
  geom_point(data = data, aes(x = abs(Lat), y = Mean, col = Major.group, fill = Major.group), size = 2) +
  geom_line(data = est.data, aes(group = Major.group), size = 1, linewidth = 1) +  # Use linewidth instead of size
  labs(x = "", y = "") +
  theme_bw() +
  scale_colour_brewer(palette = "Spectral", direction = 1) +
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "lines"),
    axis.title.y = element_text(size = 10)
  )

Majorscat




## Run the other models
#subset the data for reptiles only
repdata <- data2 %>%
  filter(Major.group == "Reptile" & !is.na(Group2))

#Change the reference level for Major.group
repdata$Group2 <- as.factor(repdata$Group2)
repdata$Group2 <- relevel(repdata$Group2, ref = "Sea Turtle")



repmod<- MCMCglmm(Mean ~ abs(Lat)*Group2,
                  random= ~study_ID+Species+animal, 
                  pedigree = tl2_brlen,
                  #ginverse=list(tip.label = Ainv),
                  nitt = 100000,
                  thin = 40, 
                  burnin = 20000,
                  prior=prior,
                  pr= TRUE,
                  verbose = TRUE,
                  data=repdata)

summary(repmod)

## Get predictions from the models for plots
repest <- emmeans(repmod, specs = "Lat", by  = "Group2", data = data2, 
                  at = list(Lat = seq(min(data2$Lat), max(data2$Lat), .25)))

# one way of getting estimates of 5 gourps - you can slice them and use what you want to use
repest.data <- as.tibble(repest)

## reptile only plot


repscat <- ggplot(repest.data, aes(x = abs(Lat), y = emmean, col = Group2, fill = Group2)) +
  geom_ribbon(aes(ymin = lower.HPD, ymax = upper.HPD, col = Group2, fill = Group2), alpha = 0.2) +
  geom_point(data = data, aes(x = abs(Lat), y = Mean, col = Group2, fill = Group2), size = 2) +
  geom_line(data = repest.data, aes(group = Group2), size = 1) +
  labs(x = "", y = "") +
  theme_bw() +
  scale_colour_brewer(palette = "Spectral", direction = 1) +
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "lines"),
    axis.title.y = element_text(size = 10)
  )
repscat