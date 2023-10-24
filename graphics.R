### Making graphics - only those that don't require the output from models ###


# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(readr)
library(ggthemes)
library(RColorBrewer)
library(ggstream)



#load data
data <- read_csv("Macro Ecology Data.csv")
data$Long <- as.numeric(data$Long)
data$Lat <- as.numeric(data$Lat)
data$Mean <- as.numeric(data$Mean)

## Before I make any graphics, I need to set colour palettes for each of my
## catagorical variables as the same catagpries will be used to make many graphs
## so they need to be consistant. 

## Subject first
## Make a nice figure using ggarrange 
scale_fill_brewer(palette="Set3", direction = -1)

# Set up a common color vector for all plots
subject_colors <- c("Breeding ecology"="#8AABDF", "Climate change" = "#E57970", "Disease" = "#6177A2", "General ecology" = "#F4AFDD", "Nest site selection" = "#629A61", "Offspring phenotype" = "#62C8D8",
"Other"= "#C9EF97", "Sex ratio" = "#9B7FC7", "Species management" = "#7FC784", "Temperature manipulation" = "#EBD073", "Thermoregulation" = "#E5A270")




#####################################
######## Make some graphics. ########
#####################################


## Lets try a map
#Load world map
world <- map_data("world") %>% filter(! long > 180)

## plot the map 
### All data points with mean temp data, showing taxa grouped into major taxonomic group (6 groups)
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "grey", fill = "grey"
  ) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  coord_map("moll") +
  geom_point(data = data, alpha = 0.7, size = 4,
             aes(Long, Lat, colour = Mean, shape = Group)) +
  scale_shape_manual(values = c(8, 3, 16, 17, 18, 15)) + 
  scale_colour_viridis(option = "plasma", na.value = NA) +
  labs(colour = "", shape = "Group") +
  theme(legend.position = "right")  # Force the legend to be at the top

### A map of just the reptiles
##Make a new dataframe as I cant seem to pass the NAs created from the other taxa 
reptiles <- data %>% filter(Group2 %in% c("Sea Turtle", "Freshwater Turtle", "Crocodilian", "Lizard/snake"))

ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "grey", fill = "grey") +
  theme(axis.title.x = element_text(size = 12),  # Adjust the size as needed
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(y = "Latitude (degrees)", x = "") +
  coord_map("moll") +
  geom_point(
    data = reptiles, alpha = 0.7, size = 4,
    aes(Long, Lat, colour = Mean, shape = Group2)) +
  scale_shape_manual(values = c(16, 17, 18, 15)) +
  scale_colour_viridis(option = "plasma", na.value = NA, guide = guide_colorbar(title = "Temperature (°C)")) +
  labs(colour = "", shape = "Group") +
  theme(legend.position = "right")  


##A map with all data, but grouping all reptiles together (so, 4 major groups)
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "grey", fill = "grey") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y = "Latitude (degrees)", x = "") +
  coord_map("moll") +
  geom_point(
    data = data, alpha = 0.7, size = 4,
    aes(Long, Lat, colour = Mean, shape = Major.group)) +
  scale_shape_manual(values = c(18,16,17,15)) +
  scale_colour_viridis(option = "plasma", na.value = NA, guide = guide_colorbar(title = "Temperature (°C)")) +
  labs(colour = "", shape = "Group") +
  theme(legend.position = "right") 

## Vert/Invert

ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "grey", fill = "grey") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  coord_map("moll") +
  geom_point(
    data = data, alpha = 0.7, size = 4,
    aes(Long, Lat, colour = Mean, shape = Back)) +
  scale_shape_manual(values = c(18,16)) +
  scale_colour_viridis(option = "plasma", na.value = NA)

## Water/Land comparison
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "grey", fill = "grey") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y = "Latitude (degrees)", x = "") +
  coord_map("moll") +
  geom_point(
    data = data, alpha = 0.7, size = 4,
    aes(Long, Lat, colour = Mean, shape = Water)) +
  scale_shape_manual(values = c(18,16)) +
  scale_colour_viridis(option = "plasma", na.value = NA, guide = guide_colorbar(title = "Temperature (°C)")) +
  labs(colour = "", shape = "Group") +
  theme(legend.position = "right") 

### Lets have a look at sea turtles only 
SeaTurts <- data %>% filter(Group2 %in% c("Sea Turtle"))

ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "grey", fill = "grey") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  coord_map("moll") +
  geom_point(
    data = SeaTurts, alpha = 0.7, size = 4,
    aes(Long, Lat, colour = Mean, shape = Species)) +
  scale_shape_manual(values = c(8,3,18,16,17,15)) +
  scale_colour_viridis(option = "plasma", na.value = NA)




#scatterplots
### nest temp x lat
## No grouping, non-linear fit
ggplot(data, aes(x=abs(Lat), y=Mean))+
  geom_point()+geom_smooth(formula=y~x+x^2)+ 
  theme_bw() +
  scale_colour_brewer(palette="Dark2")+
  ylim(5, 41)

## No grouping, linear fit
ggplot(data, aes(x=abs(Lat), y=Mean))+
  geom_point()+ 
  theme_bw() +
  scale_colour_brewer(palette="Dark2")+
  ylim(5, 41)

## Major taxanomic group
data %>%
  ggplot(aes(x = abs(Lat), y = Mean, color = Major.group)) +
  geom_point() +
  labs(x="Latitude", y="Mean temperature", title= "") +
  theme_bw()

# Set a threshold for the minimum number of observations in a group
threshold <- 10

filtered_data <- SeaTurts %>%
  group_by(Name) %>%
  filter(n() >= threshold) %>%
  ungroup()

ggplot(data = filtered_data, aes(x = abs(Lat), y = Mean, color = Name)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Latitude", y = "Mean temperature", title = "") +
  ylim(25, 35)+
  theme_bw()

##hmm🤯

##scatterplot with trendlines
Majorscat <- ggplot(data, aes(x = abs(Lat), y = Mean, col = Major.group)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(x = "", y = "") +
  theme_bw() +
  scale_colour_brewer(palette = "Spectral", direction = 1) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "lines"),
        axis.title.y = element_text(size = 10))  # Adjust the size as needed
Majorscat



Majorden <- data %>% ggplot(aes(x = abs(Lat), fill = Major.group, colour = Major.group)) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  scale_fill_brewer(palette = "Spectral", direction = 1) +
  scale_colour_brewer(palette = "Spectral", direction = 1) +
  labs(x = NULL, y = "", fill = NULL, color = NULL) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(plot.margin = unit(c(0, 0, -0.5, 0), "lines"))

# Create the combined plot with common legend
Scat1 <- ggarrange(Majorden, Majorscat,
                   common.legend = TRUE, legend = "none", align = "v",
                   ncol = 1, nrow = 2, heights = c(1, 3), hjust = -0.5, vjust = 1)
Scat1






##scatterplot with trendlines, removing the two smaller groups
ggplot(filter(data, Group!="Amphibian"&Group!="Fish"), aes(x=abs(Lat), y=Mean, col=Group))+geom_point()+geom_smooth(method="lm")+
  theme_bw()

##Land/water scatter
ggplot(data, aes(x=abs(Lat), y=Mean, col=Water))+geom_point()+geom_smooth(method="lm")+
  theme_bw()


## Reptiles only
##scatterplot

data$Group2 <- factor(data$Group2, levels =c("Sea Turtle", "Freshwater Turtle", "Crocodilian", "Lizard/snake"))
reptiledata<- data %>% 
  filter(!is.na(Group2))

reptilescat <- ggplot(reptiledata, aes(x=abs(Lat), y=Mean, col=Group2))+
  geom_point()+geom_smooth(method="lm")+ 
  labs(x="", y="") +
  theme_bw() +
  scale_color_brewer(palette = "PiYG")+
  theme(plot.margin = unit(c(0, 0, 0, 0), "lines"))
reptilescat

## Density plot
repdensity <- reptiledata %>% ggplot(aes(x=abs(Lat), fill=Group2, colour=Group2)) +
  geom_density(alpha=0.4) +
  theme_bw() +
  scale_color_brewer(palette = "PiYG") +
  scale_fill_brewer(palette = "PiYG") +
  labs(x = NULL, y = "", fill = NULL, color = NULL) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())+
  theme(plot.margin = unit(c(0, 0, -0.5, 0), "lines"))
repdensity

repdensity$data$Group2 <- str_wrap(repdensity$data$Group2, width = 10)  # Adjust the width as needed


Scat2 <- ggarrange(repdensity, reptilescat,
          common.legend = TRUE, legend = "left", align = "v",
          ncol = 1, nrow = 2, heights = c(1, 3), hjust = -0.5, vjust = 1)
Scat2
## Invert only

## 
Invert <- data %>% filter(Group %in% c("4. Invertebrate")) 

## Just want to take a look at the inverts compared to everything else
invertscatall <- ggplot(data, aes(x = abs(Lat), y = Mean, col = Major.group)) +
  geom_point() +
  theme_bw() +
  scale_colour_manual(values = c("Invertebrate" = "red", "Other Levels" = "blue")) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "lines"))

invertscatall
## Inverts have some of the highest nest temps, nothing above 35 for verts except a lone sea turtle.
# Interesting.

invertscat <- ggplot(Invert, aes(x=abs(Lat), y=Mean))+
  geom_point()+ 
  theme_bw() +
  scale_colour_brewer(palette="Dark2")+
  theme(plot.margin = unit(c(0, 0, 0, 0), "lines"))
invertscat

invertdensity <- Invert %>% ggplot(aes(x=abs(Lat), fill=Group3, colour=Group3)) +
  geom_density(alpha=0.4) +
  theme_bw() +
  scale_colour_brewer(palette="Dark2") +
  scale_fill_brewer(palette="Dark2") +
  labs(x = NULL) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())+
  theme(plot.margin = unit(c(0, 0, -0.5, 0), "lines"))
invertdensity

ggarrange(invertdensity, invertscat,
          common.legend = TRUE, legend = "none", align = "v",
          ncol = 1, nrow = 2, heights = c(1, 3), hjust = -0.5, vjust = 1)


## Scatter/density for the water/land
waterscat <- ggplot(data, aes(x=abs(Lat), y=Mean, col=Water))+
  geom_point()+geom_smooth(method="lm")+   
  labs(x="Absolute Latitude (degrees)", y="") +
  theme_bw() +
  scale_colour_brewer(palette="Dark2", direction= -1)+
  theme(plot.margin = unit(c(0, 0, 0, 0), "lines"))
waterscat

waterdensity <- data %>% ggplot(aes(x=abs(Lat), fill=Water, colour=Water)) +
  geom_density(alpha=0.4) +
  theme_bw() +
  scale_colour_brewer(palette="Dark2", direction= -1) +
  scale_fill_brewer(palette="Dark2", direction= -1) +
  labs(x = NULL, y = "", fill = NULL, color = NULL) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())+
  theme(plot.margin = unit(c(0, 0, -0.5, 0), "lines"))
waterdensity

Scat3 <- ggarrange(waterdensity, waterscat,
          common.legend = TRUE, legend = "none", align = "v",
          ncol = 1, nrow = 2, heights = c(1, 3), hjust = -0.5, vjust = 1)
Scat3

water_tbl <- data %>% group_by(Water)
samplewater <- water_tbl %>% summarise(n = n())

###########################################################################
############ SD graphics ##################################################

#filter the data so we are only using the lines with SD values
SDdata <- data %>% 
  filter(!is.na(Among_SD))


##SD scatter - No groups 

SDdata %>%
  ggplot(aes(x = abs(Lat), y = Among_SD)) +
  geom_point() + geom_smooth(method="lm")+ 
  labs(x="Latitude (absolute)", y="Among nest SD (Celcius)") +
  theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
  theme_bw()

## SD generally increases with latitude, model shows this is significant. 

## SD scatterplots major groups

## SD scatterplots major groups
SDscat <- SDdata %>%
  ggplot(aes(x = abs(Lat), y = Among_SD, color = Major.group)) +
  geom_point() +  geom_smooth(method="lm")+ 
  scale_colour_brewer(palette = "Spectral", direction = 1) +
  labs(x="", y="")+
  xlim(0, 50) +  # Set y-axis limit to 0 to 50
  theme(plot.margin = unit(c(0, 0, 0, 0), "lines")) +
  theme_bw()
SDscat
SDscat1<- SDscat + theme(axis.title.y = element_text(size = 9))

#density plot 
SDdensity <- SDdata %>% ggplot(aes(x=abs(Lat), fill=Major.group, colour=Major.group)) +
  geom_density(alpha=0.4) +
  theme_bw() +
  scale_fill_brewer(palette="Spectral", direction = 1) +
  scale_colour_brewer(palette="Spectral", direction = 1) +
  labs(x = NULL, y="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())+
  xlim(0, 50) +
  theme(plot.margin = unit(c(0, 0, -0.5, 0), "lines"))
SDdensity

Scat4 <- ggarrange(SDdensity, SDscat1,
                   common.legend = TRUE, legend = "none", align = "v",
                   ncol = 1, nrow = 2, heights = c(1, 3), hjust = -0.5, vjust = 1)
Scat4





## Looks pretty messy with the trend lines in, model shows groups are not significant
SDdata$Group2 <- factor(SDdata$Group2, levels =c("Sea Turtle", "Freshwater Turtle", "Crocodilian", "Lizard/snake"))
SDrepdata <- data %>% 
  filter(!is.na(Group2))

SDrepscat <- SDrepdata %>%
  ggplot(aes(x = abs(Lat), y = Among_SD, color = Group2)) +
  geom_point() +
  labs(x="", y="") +
  xlim(0, 50) +
  geom_point() + geom_smooth(method = "lm") +
  scale_color_brewer(palette = "PiYG") +
  theme_bw()
SDrepscat

## Very strange, Crocs and both turtles have very straight lines across latitude,
# however lizards/snakes increase SD with latitude. 

#density plot 
SDrepdensity <- SDrepdata %>% 
  ggplot(aes(x = abs(Lat), fill = Group2, colour = Group2)) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  scale_fill_brewer(palette = "PiYG") +
  scale_color_brewer(palette = "PiYG") +
  labs(x = NULL, y = "") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  xlim(0, 50) +
  theme(plot.margin = unit(c(0, 0, -0.5, 0), "lines"))
SDrepdensity

Scat5 <- ggarrange(SDrepdensity, SDrepscat,
          common.legend = TRUE, legend = "none", align = "v",
          ncol = 1, nrow = 2, heights = c(1, 3), hjust = -0.5, vjust = 1)
Scat5


## Scatter/density for the water/land
waterscatSD <- ggplot(SDdata, aes(x=abs(Lat), y=Among_SD, col=Water))+
  geom_point()+geom_smooth(method="lm")+   
  labs(x="Absolute Latitude (degrees)", y="") +
  theme_bw() +
  scale_colour_brewer(palette="Dark2", direction= -1)+
  theme(plot.margin = unit(c(0, 0, 0, 0), "lines"))
waterscatSD

waterdensitySD <- SDdata %>% ggplot(aes(x=abs(Lat), fill=Water, colour=Water)) +
  geom_density(alpha=0.4) +
  theme_bw() +
  scale_colour_brewer(palette="Dark2", direction= -1) +
  scale_fill_brewer(palette="Dark2", direction= -1) +
  labs(x = NULL, y="") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())+
  theme(plot.margin = unit(c(0, 0, -0.5, 0), "lines"))
waterdensitySD

Scat6 <- ggarrange(waterdensitySD, waterscatSD,
          common.legend = TRUE, legend = "none", align = "v",
          ncol = 1, nrow = 2, heights = c(1, 3), hjust = -0.5, vjust = 1)
Scat6

#Combine all scatterplots to make a single figure
Big_figure <- ggarrange(
  Scat1, Scat4, Scat2, Scat5, Scat3, Scat6,
  common.legend = FALSE, legend = "left",
  ncol = 2, nrow = 3,
  widths = c(1, 1), heights = c(1, 1, 1),
  hjust = 0.5, vjust = 0.5
)

Big_figure

###########################################################################################
###### Stream plot #######
#############################################################################################
# Group by count using dplyr

##First I need to remove all the duplicate lines for each paper
udata<- data %>% distinct(study_ID, .keep_all = TRUE)

year_tbl <- udata %>% group_by(Group, year)
by_group <- year_tbl %>% summarise(n = n())

by_group$Group <- factor(by_group$Group, levels =c("6. Turtle", "5. Other Reptile", "1. Crocodilian", "4. Invertebrate", "2. Amphibian", "3. Fish"))
udata$Group <- factor(udata$Group, levels =c("6. Turtle", "5. Other Reptile", "1. Crocodilian", "4. Invertebrate", "2. Amphibian", "3. Fish"))

# Rename the levels of the 'Group' factor to remove the numbers at the start
by_group$Group <- recode(by_group$Group,
                         "6. Turtle" = "Turtle",
                         "1. Crocodilian" = "Crocodilian",
                         "2. Amphibian" = "Amphibian",
                         "3. Fish" = "Fish",
                         "4. Invertebrate" = "Invertebrate",
                         "5. Other Reptile" = "Other Reptile")


# Create the ggplot with geom_stream and set the stacking order using the group aesthetic
ggplot(by_group, aes(x = year, y = n, fill = Group, group = Group)) +
  geom_stream(extra_span = 0.4, bw = 2, type = "ridge") +
  scale_fill_brewer(palette="Spectral", direction = -1) +
  labs(y = "Number of studies") +
  labs(x = "Publication year") +
  theme_bw()

## ok its perfect.



###########################################################################################
###### Pie graphs for analysing taxanomic make up #######
#############################################################################################

# Rename the levels of the 'Group' factor to remove the numbers at the start
udata$Group <- recode(udata$Group,
                         "6. Turtle" = "Turtle",
                         "1. Crocodilian" = "Crocodilian",
                         "2. Amphibian" = "Amphibian",
                         "3. Fish" = "Fish",
                         "4. Invertebrate" = "Invertebrate",
                         "5. Other Reptile" = "Other Reptile")

# Rename the levels of the 'Group' factor to remove the numbers at the start
udata$Group3 <- recode(udata$Group3,
                      "6. Sea Turtle" = "Sea Turtle",
                      "1. Crocodilian" = "Crocodilian",
                      "2. Lizard" = "Lizard",
                      "3. Snake" = "Snake",
                      "4. Tortoise" = "Tortoise",
                      "5. Freshwater Turtle" = "Freshwater Turtle")

udata$Group3 <- factor(udata$Group3, levels =c("6. Sea Turtle", "5. Freshwater Turtle", "1. Crocodilian", "4. Tortoise", "2. Lizard", "3. Snake"))


# Rename the levels of the 'Group' factor to remove the numbers at the start
udata$Name <- recode(udata$Name,
                       "6. Flatback" = "Flatback",
                       "1. Loggerhead" = "Loggerhead",
                       "2. Green" = "Green",
                       "3. Leatherback" = "Leatherback",
                       "4. Hawksbill" = "Hawksbill",
                       "5. Olive Ridley" = "Olive Ridley")

udata$Name <- factor(udata$Name, levels =c("Loggerhead", "Green", "Olive Ridley", "Leatherback", "Flatback", "Hawksbill"))






# Group by count using dplyr
group_tbl <- udata %>% group_by(Major.group) %>% 
  summarise(total_count=n(),
            .groups = 'drop')

group_tbl2 <- udata %>% group_by(Group3) %>% 
  summarise(total_count=n(),
            .groups = 'drop')

### This is count by major taxanomic group
# Convert tibble to df
df2 <- group_tbl %>% as.data.frame()


## pie chart
# Basic piechart
bigpie <- ggplot(df2, aes(x="", y=total_count, fill=Major.group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette="Spectral", direction = 1) +
  labs(title = "Ectotherms") +
  guides(fill=guide_legend(title="")) +
  theme_void()
bigpie

## Create an invertebrate only pie chart
Invert <- udata %>% filter(Group %in% c("4. Invertebrate")) 

Invert.counts <- Invert %>% group_by(Group3) %>% 
  summarise(total_count=n(),
            .groups = 'drop')

invertpie <- ggplot(Invert.counts, aes(x="", y=total_count, fill=Group3)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette="RdGy", direction = 1) +
  labs(title = "Invertebrates") +
  guides(fill=guide_legend(title="")) +
  theme_void() 
invertpie

##Create a reptiles only pie chart
reptile <- udata %>% filter(Major.group %in% c("Reptile"))

reptile.counts <- reptile %>% group_by(Group3) %>% 
  summarise(total_count=n(),
            .groups = 'drop')

reptilepie <- ggplot(reptile.counts, aes(x="", y=total_count, fill=Group3)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  labs(title = "Reptiles") +
  scale_fill_brewer(palette="PiYG") +
  guides(fill=guide_legend(title="")) +
  theme_void() 
reptilepie

ggarrange(invertpie, reptilepie,
          labels = c("A", "B"),
          common.legend = FALSE, legend = "right",
          ncol = 2, nrow = 1)

## Sea turtle only pie
SeaTurtleu <- udata %>% filter(Group2 %in% c("Sea Turtle"))

turtle.counts <- SeaTurtleu %>% group_by(Name) %>% 
  summarise(total_count=n(),
            .groups = 'drop')

#reverse the colour palette so I get the greens and no very light colours



turtlepie <- ggplot(turtle.counts, aes(x="", y=total_count, fill=Name)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette=("BrBG"), direction = -1) +
  labs(title = "Sea turtles") +
  guides(fill=guide_legend(title="")) +
  theme_void()
turtlepie

## Arrange for a single figure
taxarrange <- ggarrange(
  bigpie, labels = c("A"),
  ggarrange(invertpie, reptilepie, turtlepie, labels = c("B", "C", "D"),  ncol = 3),
  ncol = 1,
  heights = c(2, 1),   # Adjust the heights as needed
  legend = "right")   # Place common legend at the bottom
taxarrange


######################## Phenotype figure ############################################

##Check how many studies measured phenotype
phenotype.y <- udata %>% filter(phenotype %in% c("y"))
phenotype.n <- udata %>% filter(phenotype %in% c("n"))


##check how many studies measured phenotype
phenotype.counts <- udata %>% group_by(phenotype) %>% 
  summarise(total_count=n(),
            .groups = 'drop')

## might be interesting to see the taxanomic make up of studies that measured phenotypes and those that didnt
## not sure the best way to show this, I will make two pie charts for now.

phenotypey.tax <- phenotype.y %>% group_by(Group) %>% 
  summarise(total_count=n(),
            .groups = 'drop')


##Measured phenotypes - broad taxonomic groups

PY <- ggplot(phenotypey.tax, aes(x="", y=total_count, fill=Group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  guides(fill=guide_legend(title="Animal group")) +
  labs(title = "Taxonomic make-up of studies that measured phenotype") +
  scale_fill_brewer(palette="Spectral", direction = -1) +
  theme(plot.title = element_text(hjust = 1)) +
  theme_void() 
PY


##Didn't measure phenotypes - broad taxonomic groups
phenotypen.tax <- phenotype.n %>% group_by(Group) %>% 
  summarise(total_count=n(),
            .groups = 'drop')


##Measured phenotypes - broad taxonomic groups

PN <- ggplot(phenotypen.tax, aes(x="", y=total_count, fill=Group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  guides(fill=guide_legend(title="Animal group")) +
  labs(title = "Taxonomic make-up of studies that DID NOT measure phenotype") +
  scale_fill_brewer(palette="Spectral", direction = -1) +
  theme(plot.title = element_text(hjust = 1)) +
  theme_void() 
PN


ggarrange(PY, PN,
          labels = c("A", "B"),
          common.legend = TRUE, legend = "right",
          ncol = 2, nrow = 1)




#######################################################################################
## Study topics ###########################################################################
#####################################################################################
  
  
  topic_counts <- udata %>% group_by(topic) %>% 
    summarise(total_count=n(),
              .groups = 'drop')


  subjectpie <- ggplot(topic_counts, aes(x="", y=total_count, fill=topic)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    labs(title = "Study Topic - Ectotherms") +
    guides(fill=guide_legend(title="Topics")) +
    theme_void()
  subjectpie

  ## The above probably has too many topics, so I am going to merge some of them
  ## so it is easier to visualise as a pie chart
  
  merged_topics_data <- udata %>%
    mutate(
      merged_topic = case_when(
        topic %in% c("Viviparity", "Predator/prey relationship") ~ "Other",
        topic %in% c("Thermal tolerance", "Thermal regulation", "Nest thermo-regulation", "Nest environment") ~ "Thermoregulation",
        topic %in% c("Offspring phenotype", "Embryonic development") ~ "Offspring phenotype",
        TRUE ~ as.character(topic)
      )
    )
  topic_counts <- merged_topics_data %>% group_by(merged_topic) %>% 
    summarise(total_count=n(),
              .groups = 'drop')
  
  
  
  subjectpie <- ggplot(topic_counts, aes(x="", y=total_count, fill=merged_topic)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    labs(title = "") +
    guides(fill=guide_legend(title="Topics")) +
    theme_void()
  subjectpie
  

  ## Look at sea turtles
  sturtle_topic_counts <- merged_topics_data %>%
    filter(Group2 == "Sea Turtle") %>%
    group_by(merged_topic) %>%
    summarise(total_count=n(), .groups = 'drop')

  subjectSTpie <- ggplot(sturtle_topic_counts, aes(x="", y=total_count, fill=merged_topic)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    labs(title = "Sea Turtles") +
    guides(fill=guide_legend(title="Topics")) +
    theme_void()
  subjectSTpie
  
  ## Look at invertebrates
  invert_topic_counts <- merged_topics_data %>%
    filter(Major.group == "Invertebrate") %>%
    group_by(merged_topic) %>%
    summarise(topic_count = n(), .groups = 'drop')
  
  subjectIpie <- ggplot(invert_topic_counts, aes(x="", y=topic_count, fill=merged_topic)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    labs(title = "Invertebrates") +
    guides(fill=guide_legend(title="Topics")) +
    theme_void()
  subjectIpie
  
  ## Lizards, snakes and freshwater turtles
  
  # Combine specified levels of "group2" into "Other Reptile"
  # Filter data for the specified levels of "group2"
  filtered_dataOR <- merged_topics_data %>%
    filter(Group2 %in% c("Lizard/snake", "Freshwater Turtle"))
  
  # Get the group counts for each "topic"
  group_countsOR <- filtered_dataOR %>%
    group_by(Group2, merged_topic) %>%
    summarise(topic_count = n(), .groups = 'drop') %>%
    group_by(merged_topic) %>%
    summarise(merged_topic_count = sum(topic_count), .groups = 'drop')
  
  
  # Create a pie chart for the combined group
  pie_chartOR <- ggplot(group_countsOR, aes(x = "", y = merged_topic_count, fill = merged_topic)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    labs(title = "Lizards, snakes & freshwater turtles") +
    guides(fill = guide_legend(title = "Topics")) +
    theme_void()
  pie_chartOR
  

  subjectIpie <- subjectIpie + scale_fill_manual(values = subject_colors)
  pie_chartOR <- pie_chartOR + scale_fill_manual(values = subject_colors)
  subjectSTpie <- subjectSTpie + scale_fill_manual(values = subject_colors)
  subjectpie<- subjectpie + scale_fill_manual(values = subject_colors)

  subjectIpie <- subjectIpie +
    theme(legend.position = "none")
  
  pie_chartOR <- pie_chartOR +
    theme(legend.position = "none")
  
  
  subjectSTpie <- subjectSTpie +
    theme(legend.position = "none")
  
  # Arrange the pie charts using ggarrange with a common legend
  arranged_pie_charts <- ggarrange(
    subjectpie, labels = c("A"),
    ggarrange(subjectIpie, pie_chartOR, subjectSTpie, labels = c("B", "C", "D"),
               ncol = 3),
    ncol = 1,
    heights = c(2, 1),  
    legend = "right")   
  arranged_pie_charts

  
  ## Fish pie
  filtered_dataF <- udata %>%
    filter(Group %in% c("3. Fish"))
  
  group_countsF <- filtered_dataF %>%
    group_by(Group, topic) %>%
    summarise(topic_count = n(), .groups = 'drop')
  
  pie_chartF <- ggplot(group_countsF, aes(x = "", y = topic_count, fill = topic)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    labs(title = "Topic distribution for Fish") +
    guides(fill = guide_legend(title = "Topics")) +
    theme_void()
  pie_chartF
  
  ########################################################################################
  ################### Another streamplot for the topics #################################
  ########################################################################################
  
  sub_tbl <- merged_topics_data %>% group_by(merged_topic, year)
  sub_group <- sub_tbl %>% summarise(n = n())
  

  
  # Define the levels of merged_topic to keep
  selected_levels <- c(
    "Sex ratio",
    "Thermoregulation",
    "Nest site selection",
    "Offspring phenotype",
    "Breeding ecology",
    "Species management",
    "Climate change"
  )
  
  # Filter the data frame to only include the selected levels
  filtered_sub_group <- sub_group %>%
    filter(merged_topic %in% selected_levels)
  
  #reorder the topics so the stream plot visualises the data more accurately
  filtered_sub_group$merged_topic <- factor(filtered_sub_group$merged_topic, levels =c("Climate change", "Sex ratio", "Offspring phenotype", "Species management", "Nest site selection", "Breeding ecology", "Thermoregulation"))
  
  
  # Create the plot using the filtered data
  ggplot(filtered_sub_group, aes(x = year, y = n, fill = merged_topic, group = merged_topic)) +
    geom_stream(extra_span = 0.4, bw = 2, type = "ridge") +
    labs(y = "Number of studies") +
    labs(x = "Publication year") +
    guides(fill = guide_legend(title = "Topics")) +
    scale_fill_manual(values = subject_colors) +
    theme_bw()
  

### Not using any of the below at the moment ##
###############################################
  
  ##Violin plots
  data %>%
    ggplot( aes(x=Mean, y=Major.group, fill=Major.group, colour=Major.group)) +
    geom_violin(alpha= 0.8) +
    coord_flip() +
    labs(x="Mean temperature", y="", title="Mean temp grouped by major taxon") +
    theme_bw() 
  
  ##Violin group by backbone
  data %>%
    ggplot( aes(x=Mean, y=Back, fill=Back, colour=Back)) +
    geom_violin(alpha= 0.8) +
    coord_flip() +
    labs(x="Mean temperature", y="", title="Mean temp grouped by invert/vert") +
    theme_bw() +
    theme(legend.position = "none")
  
  ##Violin group by water/land
  data %>%
    ggplot( aes(x=Mean, y=Water, fill=Water, colour=Water)) +
    geom_violin(alpha= 0.8) +
    coord_flip() +
    labs(x="Mean temperature", y="", title="Mean temp grouped by Water/Land") +
    theme_bw() +
    theme(legend.position = "none")
  
  ##Violin reptiles only
  reptiles %>%
    ggplot( aes(x=Mean, y=Group2, fill=Group2, colour=Group2)) +
    geom_violin(alpha= 0.8) +
    coord_flip() +
    labs(x="Mean temperature", y="", title="Mean temp x Reptiles") +
    theme_bw() +
    theme(legend.position = "none")
  
  ##Violin inverts only
  Invert %>%
    ggplot( aes(x=Mean, y=Group3, fill=Group3, colour=Group3)) +
    geom_violin(alpha= 0.8) +
    coord_flip() +
    labs(x="Mean temperature", y="", title="Mean temp x Invert") +
    theme_bw() +
    theme(legend.position = "none")
  
  
  
  
##Lets try an alluvial plot

library(ggalluvial)

ggplot(as.data.frame(stream),
       aes(y = Frequency, axis1 = Frequency, axis2 = Frequency1, axis3 = Frequency2)) +
  geom_alluvium(aes(fill = Taxon), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Natural", "Phenotype"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("example")

##plot works but its not showing the data very well either. 





