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
library(forcats) 
library(export)
library(Cairo)
library(dplyr)

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
##I2=86.75

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

## Meta analysis

mod2 <- rma.mv(yi = yi, V = vi, random = list(~1|phylogeny, ~1|animal, ~1|data_ID), R = list(phylogeny=R_phylo), mod = ~1, data = IDNNZR, method = 'ML')
summary(mod2)


#random effects heterogeneity
round(i2_ml(mod2),3)

# Calculate Prediction Intervals
predict(mod2)

id<- orchard_plot(mod2, mod="1", group="animal", xlab="Zr", alpha = 0.5,
   twig.size = 2, trunk.size = 1.5, branch.size = 2, transfm = "tanh") +
  expand_limits(y=c(-1,1)) +
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
##I2=87.51

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

## Meta analysis

mod2s <- rma.mv(yi = yi, V = vi, random = list(~1|phylogeny, ~1|animal, ~1|data_ID), R = list(phylogeny=R_phylos), mod = ~1, data = s.datazr, method = 'ML')
summary(mod2s)



#r2_ml(mod1)
round(i2_ml(mod2s),3)

## backtransform to correlation coefficient for figure

strans <- orchard_plot(mod2s, mod="1", group="animal", xlab="", alpha = 0.5,
                  twig.size = 2, trunk.size = 1.5, branch.size = 2, transfm = "tanh") +
  expand_limits(y=c(-1,1)) +
  scale_fill_manual(values="blue") +
  scale_colour_manual(values="blue")
strans




filen <- "naturalnestsMA/MAs"
# PDF Export 
graph2vector(x = strans, file = paste0(filen, ".pdf"), width = 8, height = 6)



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

#### Checking R2 values from raw data emailed from authors
## Wood 2014 ##


dfwood <- data.frame(
  Nest_ID = c(4, 10, 23, 3, 5, 6, 11, 14, 19, 22, 25, 2, 7, 15, 18, 27, 31, 8, 17, 20, 28, 29, 31, 37, 40, 42, 44, 46, 47, 39),
  Mean_incubation_temp_C = c(31.3, 31.4, 32.0, 30.1, 30.4, 30.6, 30.8, 30.2, 30.7, 30.6, 30.5, 30.3, 30.3, 30.2, 30.1, 30.1, 29.8, 30.2, 29.8, 29.8, 29.9, 29.6, 29.77, 29.13, 29.41, 29.44, 29.50, 29.50, 32.10, 29.53),
  Hatching_success_percent = c(94.51, 70.00, 84.68, 91.61, 81.65, 77.95, 89.82, 80.90, 78.81, 73.29, 67.86, 89.26, 86.32, 72.03, 86.89, 78.46, 77.59, 86.23, 79.79, 86.40, 84.68, 81.30, 77.59, 88.17, 94.00, 93.23, 91.82, 86.99, 81.94, 94.53),
  Nest_emergence_period_days = c(50, 53, 48, 56, 54, 53, 52, 54, 55, 55, 52, 56, 54, 55, 55, 53, NA, 55, 57, 55, NA, NA, NA, 65, 65, 65, 63, 64, 53, NA)
)

# View the dataframe
print(dfwood)

IDwood <- lm(Mean_incubation_temp_C ~ Nest_emergence_period_days, data = dfwood)
summary(IDwood)
# r2 = 0.638

Swood <- lm(Mean_incubation_temp_C ~ Hatching_success_percent, data = dfwood)
summary(Swood)
#r2 = 0.0503


## Riley 2014 ##
JRdata <- read.csv("Riley 2014_Evol Ecol_data.csv")

clean_data <- JRdata %>%
  filter(!is.na(daily_avg_temp))


#needs to be split to take into account two species

# Group the data by the 'species' column
grouped_data <- clean_data %>%
  group_by(species)

# Split the data into a list of data frames based on the 'species' column
split_data <- grouped_data %>%
  group_split()

# Access the first subset
speciesPNTU <- split_data[[1]]

# Access the second subset
speciesSNTU <- split_data[[2]]


##ID

IDJRPT <- lm(daily_avg_temp ~ incubation_length_days, data = speciesPNTU)
summary(IDJRPT)
# r2 = 0.3493

IDJRST <- lm(daily_avg_temp ~ incubation_length_days, data = speciesSNTU)
summary(IDJRST)
# r2 = 0.7963

## survival
SJRPT <- lm(daily_avg_temp ~ hatching_success, data = speciesPNTU)
summary(SJRPT)
# r2 = 0.00759

SJRST <- lm(daily_avg_temp ~ hatching_success, data = speciesSNTU)
summary(SJRST)
# r2 = 0.00399

## Read 2012 ##
#RP
IncubationRP <- c(57, 53, 49, 49, 47, 48, 45, 46, 48, 49, 49)
TemperatureRP <- c(29.67, 32.25, 31.6, 31.83, 32.5, 32.2, 32.67, 32.53, 30.73, 32.02, 32.13)
Hatching_SRP <- c(97, 93, 68, 95, 59, 73, 87, 89, 86, 67, 83)

#MP
IncubationMP <- c(53, 53, 54, 61, 56, 52, 54, 56, 49, 64, 58, 58, 56, 56, 54, 55)
TemperatureMP <- c(  29.5, 29.69, 29.52, 29.05, 29.03,
                     30.44, 30.61, 30.58, 28.6, 28.11,
                     29.27, 29.08, 29.56, 28.71, 30, 29.71)

Hatching_SMP <- c(95, 93, 99, 98, 98, 98, 87, 97, 99, 96, 99, 94, 97, 98, 80, 99)  

# Create the data frame
ReadIDRP <- data.frame(IncubationRP = IncubationRP, TemperatureRP = TemperatureRP)
ReadSRP <- data.frame(Hatching_SRP = Hatching_SRP, TemperatureRP = TemperatureRP)

ReadIDMP <- data.frame(IncubationMP = IncubationMP, TemperatureMP = TemperatureMP)
ReadSMP <- data.frame(Hatching_SMP = Hatching_SMP, TemperatureMP = TemperatureMP)


IDRP <- lm(TemperatureRP ~ IncubationRP, data = ReadIDRP)
summary(IDRP)
# r2 = 0.476

## survival
SRP <- lm(TemperatureRP ~ Hatching_SRP, data = ReadSRP)
summary(SRP)
# r2 = 0.03253

#ID
IDMP <- lm(TemperatureMP ~ IncubationMP, data = ReadIDMP)
summary(IDMP)
# r2 = 0.1279

## survival
SMP <- lm(TemperatureMP ~ Hatching_SMP, data = ReadSMP)
summary(SMP)
# r2 = 0.08551

## Bonach 2011 ##

ID <- c(53.20, 53.20, 41.80, 58.20, 71.70, 60.60, 56.00, 56.10, 31.10, 31.10, 39.00, 39.30)
MeanT <- c(31.469, 28.402, 30.265, 31.24, 29.806, 30.121, 30.683, 29.831, 29.147, 29.271, 30.298, 30.198)

# Create a data frame
dfbon <- data.frame(
  Variable1 = ID,
  Variable2 = MeanT
)
IDbon<- lm(MeanT ~ ID, data = dfbon)
summary(IDbon)

#sonmez 2013 Table2

# Create a dataframe with incubation duration and total temperature
sonmezd <- data.frame(
  IDson = c(50, 47, 48, 47, 45, 51, 48, 52, 54, 58),
  MeanTson = c(31.9, 31.5, 31.4, 30.9, 28.8, 32.0, 32.0, 31.5, 32.9, 28.3),
  Hatching_successson <- c(73, 81.9, 85.9, 80.4, 25.2, 72.2, 99.1, 15.8, 90.2, 87.6))

IDsonm<- lm(MeanTson ~ IDson, data = sonmezd)
summary(IDsonm)

HSson <- lm(MeanTson ~ Hatching_successson, data = sonmezd)
summary(HSson)

plot(sonmezd$IDson, sonmezd$MeanTson,
     xlab = "IDson",
     ylab = "MeanTson",
     main = "Relationship between MeanTson and IDson",
     pch = 16, col = "blue")

# Add the regression line
abline(IDsonm, col = "red")

plot(sonmezd$Hatching_successson, sonmezd$MeanTson,
     xlab = "Hatching_successson",
     ylab = "MeanTson",
     main = "Relationship between MeanTson and IDson",
     pch = 16, col = "blue")

# Add the regression line
abline(IDsonm, col = "red")

## García–Grajales 2019##

breed_2014_2015SJ <- data.frame(
  WiP = c(30.3, 30.7, 30.9, 31.5, 31.3, 31.4),
  HS = c(40.7, 35.9, 37, 44.4, 52.9, 46.4)
)



breed_2015_2016SJ <- data.frame(
  WiP = c(29.5, 29.7, 30, 30.1, 29.9, 31.5, 31.6, 31.7, 31.6, 31.8),
  HS = c(83.8, 79.8, 79.7, 80, 72.2, 76.3, 73.8, 80, 71.9, 75.6)
)

breed_2016_2017SJ <- data.frame(
  WiP = c(29.7, 29.6, 29.9, 29.8, 30.1, 30, 31, 31.1, 31.5, 31.6),
  HS = c(73.2, 82.1, 80.6, 70.8, 81.2, 76.8, 79.5, 71.3, 77.9, 79)
)

breed_2016_2017SJ <- data.frame(
  WiP = c(29.7, 29.6, 29.9, 29.8, 30.1, 30, 31, 31.1, 31.5, 31.6),
  HS = c(73.2, 82.1, 80.6, 70.8, 81.2, 76.8, 79.5, 71.3, 77.9, 79)
)

breed_2015_2016P <- data.frame(
  WiP = c(30.2, 30.5, 30.4, 30.5, 31, 30.6, 30.7, 30.7, 30.6, 30.9),
  HS = c(82.9, 78.5, 77.9, 81.6, 81, 73.4, 77.7, 84, 84.9, 82.6)
)

breed_2016_2017P <- data.frame(
  WiP = c(29.9, 30, 30.4, 30.5, 30.4, 30.3, 30.7, 31, 30.8, 31.2),
  HS = c(85.7, 83.5, 80.8, 80.7, 86.5, 83.3, 71.2, 83.1, 80.2, 82))

## decided to keep years together and seperate locations#

# Merge SJ datasets
combined_SJ <- rbind(breed_2014_2015SJ, breed_2015_2016SJ, breed_2016_2017SJ)

# Merge P datasets
combined_P <- rbind(breed_2015_2016P, breed_2016_2017P)


SJHS <- lm(HS ~ WiP, data = combined_SJ)
summary(SJHS)

PHS <- lm(HS ~ WiP, data = combined_P)
summary(PHS)

# need to split ID too.

# Create the ID vector for SJ
IDSJ <- c(62, 58, 57, 56, 55, 54, 62, 61, 59, 58, 58, 56, 56, 55, 55, 56, 62, 61, 61, 60, 59, 58, 57, 56, 56, 55)

# Make sure the length of IDSJ matches the number of rows in combined_SJ
if (length(IDSJ) == nrow(combined_SJ)) {
  combined_SJ$ID <- IDSJ
} else {
  stop("The length of IDSJ does not match the number of rows in combined_SJ")
}

# Create the ID vector for P
IDP <- c(59, 56, 58, 57, 52, 56, 56, 55, 55, 54, 59, 57, 59, 60, 58, 59, 53, 52, 54, 52)
# Make sure the length of IDSJ matches the number of rows in combined_SJ
if (length(IDP) == nrow(combined_P)) {
  combined_P$ID <- IDP
} else {
  stop("The length of IDSJ does not match the number of rows in combined_SJ")
}

SJID <- lm(ID ~ WiP, data = combined_SJ)
summary(SJID)

PID <- lm(ID ~ WiP, data = combined_P)
summary(PID)

#Oz et al 2004#

Oz_data <- data.frame(
  MT = c(28.8, 29.4, 30.7, 30.5),
  HS = c(96.2, 97.5, 90, 93.4))
Ozhs <- lm(HS ~ MT, data = Oz_data)
summary(Ozhs)

plot(Oz_data$MT, Oz_data$HS)

#ozdemir et al 2011#
Ozdemir_data <- data.frame(
  MT = c(28.95, 32.13, 29.15, 30.21, 29.78, 29.23, 30.22, 30.99, 30.33, 29.47, 30.88, 30.93, 30.93, 29.81, 29.16, 28.96, 29.70, 29.14, 28),
  HS = c(48.33, 19.12, 74.67, 59.18, 14.77, 26.25, 58.44, 72.95, 38.82, 24.14, 91, 96, 90, 98, 85.71, 94.94, 93.26, 98.68, 64.71))
Ozdhs <- lm(HS ~ MT, data = Ozdemir_data)
summary(Ozdhs)

plot(Ozdemir_data$MT, Ozdemir_data$HS)

#Simoes 2014#
Simoes_data <- data.frame(
  MT = c(31.19, 32.35, 32.53, 32.58, 32.28, 31.82, 31.47, 30.86, 30.12),
  HS = c(94.73, 12.4, 61.41, 47.72, 53.7, 77.97, 23.3, 69.69, 26.01))
simoeshs <- lm(HS ~ MT, data = Simoes_data)
summary(simoeshs)

plot(Simoes_data$MT, Simoes_data$HS)

install.packages("pacman")
pacman::p_load(devtools, tidyverse, metafor, patchwork, R.rsp, emmeans)

devtools::install_github("daniel1noble/orchaRd", force = TRUE)
library(orchaRd)