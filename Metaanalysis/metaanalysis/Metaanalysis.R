### Natural oviposition site meta analysis ###
## Incubation duration dataset ##


#load packages
library(MCMCglmm)
library(phytools)
library(stats)
library(rotl)
library(readr)
library(metafor)
library(orchaRd)
library(devtools)
library(ggplot2)

#load data
IDNN <- read_csv("IDNN.csv")

  
#### 1. Calculate Zcor effect size ####
IDNNZR <-escalc(measure = "ZCOR", ri=r , ni=N , data = IDNN)

#Change over factors
IDNNZR$Order <- as.factor(IDNNZR$Order)


  #### 2. Calculating the heterogeneity 
# Test the model by fitting standard rema via rma and rema via rma.mv - you should be able to come up with the same i2 both ways
#rma model doesn't require dataID as the parameter is estimated by default
normal<-rma(yi=yi, vi=vi, data=IDNNZR)
summary(normal)
##I2=95.65

  #### 3. Generating the phylogentic tree and matrix - Based on Dan Nobles code 2016 ####
#load the species list data 
speciesList <- read_csv("specieslistID.csv")

## Phylogeny
## Access taxon relationships from Open Tree of Life
# needed for phylogenetic control
IDNNZR$animal<-as.character(IDNNZR$animal)

# Match species in dataset
tree2 = tnrs_match_names(as.character(unique(speciesList$Species)), context = "Animals")

#Create a tree based on itt id's found on the open tree of life
tl2 <- tol_induced_subtree(ott_ids=na.omit(tree2$ott_id)) 

#Remove ott labels on end to make sure to matches species in dataset
tl2$tip.label = strip_ott_ids(tl2$tip.label, remove_underscores=FALSE)

#getting a phylogenetic correlaiton matrix from the tree
tl2_brlen <- compute.brlen(tl2, method = "Grafen", power = 0.5) 

# Check trees are ultrametric
is.ultrametric(tl2_brlen)

# plot the tree
plot(tl2, cex = 0.8, label.offset = .1, no.margin = TRUE)

# Generate the phylogenetic matrix
tl2_brlen$node.label <- NULL
R_phylo <- vcv(tl2_brlen, corr = TRUE)
print(R_phylo)

#checking the spelling etc matches due to error generating the phylogenetic matrix
unique(IDNNZR$animal)
rownames(R_phylo) %in% unique(IDNNZR$animal)
rownames(R_phylo)[!rownames(R_phylo) %in% unique(IDNNZR$animal)]

IDNNZR$phylogeny <- IDNNZR$animal

## Meta analysis without moderators

mod2 <- rma.mv(yi = yi, V = vi, random = list(~1|phylogeny, ~1|animal, ~1|study_ID), R = list(phylogeny=R_phylo), mod = ~1, data = IDNNZR, method = 'ML')
summary(mod2)

mod2di <- rma.mv(yi = yi, V = vi, random = list(~1|phylogeny, ~1|animal, ~1|study_ID, ~1|data_ID), R = list(phylogeny=R_phylo), mod = ~1, data = IDNNZR, method = 'ML')
summary(mod2di)




#random effects heterogeneity
round(i2_ml(mod2),3)


I2 <- orchaRd::i2_ml(english_MA)

id<- orchard_plot(mod2, mod="1", group="animal", xlab="Zr", alpha = 0.5,
   twig.size = 2, trunk.size = 1.5, branch.size = 2, transfm = "tanh") +
  expand_limits(y=c(-3,3)) +
  scale_fill_manual(values="red") +
  scale_colour_manual(values="red")
id

  
filen <- "naturalnestsMA/MAid"
# PDF Export 
graph2vector(x = id, file = paste0(filen, ".pdf"), width = 8, height = 6)



## 5. Publication bias

#calculating weighted average (or precision)
IDNNZR$wi<-  1/(IDNNZR$vi) 

# Model for publication bias includes all moderators and random effects, as well as year to account for
# time lag and precision (wi)
pub.model <- rma.mv(yi = yi, V = vi, random = list(~1|animal, ~1|data_ID), R = list(animal=R_phylo), mod = ~ year + wi, data = IDNNZR, method = 'ML')
summary(pub.model)

## no publication bias



##Get the prediction intervals for the MLMR 
M <- coef(mod2)
# Calculate Prediction Intervals
predict(mod2)




##############################################################
############### SURVIVAL #####################################
##############################################################

#load data
s.data <- read_csv("SNN.csv")


#### 1. Calculate Zcor effect size ####
s.datazr <-escalc(measure = "ZCOR", ri=r , ni=N , data = s.data)

#Change over factors
s.datazr$Order <- as.factor(s.datazr$Order)


#### 2. Calculating the heterogeneity 
# Test the model by fitting standard rema via rma and rema via rma.mv - you should be able to come up with the same i2 both ways
#rma model doesn't require dataID as the parameter is estimated by default
normals<-rma(yi=yi, vi=vi, data=s.datazr)
summary(normals)
##I2=90.37

#### 3. Generating the phylogentic tree and matrix - Based on Dan Nobles code 2016 ####
#load the species list data 
speciesLists <- read_csv("specieslists.csv")

## Phylogeny
## Access taxon relationships from Open Tree of Life
# needed for phylogenetic control
s.datazr$animal<-as.character(s.datazr$animal)

# Match species in dataset
tree2s = tnrs_match_names(as.character(unique(speciesLists$Species)), context = "Animals")

#Create a tree based on itt id's found on the open tree of life
tl2s <- tol_induced_subtree(ott_ids=na.omit(tree2s$ott_id)) 

#Remove ott labels on end to make sure to matches species in dataset
tl2s$tip.label = strip_ott_ids(tl2s$tip.label, remove_underscores=FALSE)

#getting a phylogenetic correlaiton matrix from the tree
tl2_brlens <- compute.brlen(tl2s, method = "Grafen", power = 0.5) 

# Check trees are ultrametric
is.ultrametric(tl2_brlens)

# plot the tree
plot(tl2s, cex = 0.8, label.offset = .1, no.margin = TRUE)

# Generate the phylogenetic matrix
tl2_brlens$node.label <- NULL
R_phylos <- vcv(tl2_brlens, corr = TRUE)
print(R_phylos)

#checking the spelling etc matches due to error generating the phylogenetic matrix
unique(s.datazr$animal)
rownames(R_phylos) %in% unique(s.datazr$animal)
rownames(R_phylos)[!rownames(R_phylos) %in% unique(s.datazr$animal)]

s.datazr$phylogeny <- s.datazr$animal

## Meta analysis without moderators

mod2s <- rma.mv(yi = yi, V = vi, random = list(~1|phylogeny, ~1|animal, ~1|data_ID), R = list(phylogeny=R_phylos), mod = ~1, data = s.datazr, method = 'ML')
summary(mod2s)



#r2_ml(mod1)
round(i2_ml(mod2s),3)

## backtransform to correlation coefficient for figure

strans <- orchard_plot(mod2s, mod="1", group="animal", xlab="", alpha = 0.5,
                  twig.size = 2, trunk.size = 1.5, branch.size = 2, transfm = "tanh") +
  expand_limits(y=c(-3,3)) +
  scale_fill_manual(values="blue") +
  scale_colour_manual(values="blue")
strans

library(forcats) 
library(export)
library(Cairo)


filen <- "naturalnestsMA/MAs"
# PDF Export 
graph2vector(x = s, file = paste0(filen, ".pdf"), width = 8, height = 6)



## 5. Publication bias

#calculating weighted average (or precision)
s.datazr$wi<-  1/(s.datazr$vi) 

# Model for publication bias includes all moderators and random effects, as well as year to account for
# time lag and precision (wi)
pub.models <- rma.mv(yi = yi, V = vi, random = list(~1|animal, ~1|data_ID), R = list(animal=R_phylos), mod = ~ year + wi, data = s.datazr, method = 'ML')
summary(pub.models)

## no publication bias
summary(pub.models)


##Get the prediction intervals for the MLMR 
Ms <- coef(mod2s)
# Calculate Prediction Intervals
predict(mod2s)

## A graphic for a talk to show taxanomic breakdown
#combine datasets

library(dplyr)

combined_data <- bind_rows(s.data, IDNN) %>%
  distinct(study_ID, .keep_all = TRUE)

# Count occurrences of each level in Order
order_counts <- combined_data %>%
  count(Order, name = "Order_Count")

# View the result
print(order_counts)

# Count occurrences of each level in Species
species_counts <- combined_data %>%
  count(Species, name = "Species_Count")

# View the result
print(species_counts)


## not being used##

pie <- ggplot(data, aes(x="", y=total_count, fill=Order)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  labs(title = "") +
  scale_fill_brewer(palette="Dark2") +
  guides(fill=guide_legend(title="Order")) +
  theme_void() 
pie