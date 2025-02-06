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
library(forcats) 
library(export)
library(Cairo)


#load data
data <- read_csv("Macro Ecology Data.csv")
data$Long <- as.numeric(data$Long)
data$Lat <- as.numeric(data$Lat)
data$Mean <- as.numeric(data$Mean)

## Before I make any graphics, I need to set colour palettes for each of my
## catagorical variables as the same catagpries will be used to make many graphs
## so they need to be consistant. 

## Subject first


# Set up a common color vector for all plots: subject
subject_colors <- c( "Climate change" = "#42C269", "Threatened species management" = "#A942C2", "General ecology" = "#6942C2",  "Thermoregulation" = "#c2425b","Developmental ecology" = "#42C2A9", "Evolutionary transitions" = "#3e81b8")


# Major taxa
mtaxa_colors <- c("Reptile" = "#385E9A", "Arthropod"="#D82E0A", "Fish"="#E1AD09", "Amphibian"="#479821")

# Reptiles
reptile_colors <- c("Sea Turtle" = "#365a9e", "Lizard"="#772944", "Crocodilian"= "#369e7a", "Freshwater Turtle"="#b66883", "Tortoise" = "#1b4f3d", "Snake" = "#34425b", "Lizard/snake"="#772944")


# water/land
water_colors <- c("Water" = "#6A6EBD", "Land"="#B9824E")

## For bigger graphs
all_colors <- c("Sea Turtle" = "#365a9e", "Lizard"="#772944", "Crocodilian"= "#369e7a", "Freshwater Turtle"="#b66883", "Tortoise" = "#1b4f3d", "Snake" = "#34425b", "Lizard/snake"="#772944","Reptile" = "#385E9A", "Arthropod"="#D82E0A", "Fish"="#E1AD09", "Amphibian"="#479821", "Turtle" = "#365a9e", "Other Reptile" = "#772944" )



# Reorder the Group variable (Example: putting "B" first, followed by "A" and "C")
data$Major.group <- fct_relevel(data$Major.group, "Arthropod", "Fish", "Amphibian", "Reptile")


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
x <- ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "grey", fill = "grey") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "none") +
  labs(y = "Latitude (degrees)", x = "") +
  coord_map("moll") +
  geom_point(
    data = data, alpha = 0.7, size = 4,
    aes(Long, Lat, colour = Mean, shape = Major.group)) +
  scale_shape_manual(values = c(18,16,17,15)) +
  scale_colour_viridis(option = "plasma", na.value = NA, guide = "none") +
  labs(colour = "", shape = "Group")


filen <- "naturalnestsMA/majormap"

# PDF Export 
graph2vector(x = x, file = paste0(filen, ".pdf"), width = 8, height = 6)



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



###########################################################################################
###### Stream plot #######
#############################################################################################
# Group by count using dplyr

##First I need to remove all the duplicate lines for each paper
udata<- data %>% distinct(study_ID, .keep_all = TRUE)



year_tbl <- udata %>% group_by(Group, year)
by_group <- year_tbl %>% summarise(n = n())

by_group$Group <- factor(by_group$Group, levels =c("6. Turtle", "1. Crocodilian","5. Other Reptile","2. Amphibian","3. Fish", "4. Invertebrate" ))
udata$Group <- factor(udata$Group, levels =c("6. Turtle", "1. Crocodilian","5. Other Reptile","2. Amphibian","3. Fish", "4. Invertebrate" ))

# Rename the levels of the 'Group' factor to remove the numbers at the start
by_group$Group <- recode(by_group$Group,
                         "6. Turtle" = "Turtle",
                         "1. Crocodilian" = "Crocodilian",
                         "2. Amphibian" = "Amphibian",
                         "3. Fish" = "Fish",
                         "4. Invertebrate" = "Arthropod",
                         "5. Other Reptile" = "Lizard/snake")

# Create the ggplot with geom_stream and set the stacking order using the group aesthetic
stream <- ggplot(by_group, aes(x = year, y = n, fill = Group, group = Group)) +
  geom_stream(extra_span = 0.4, bw = 0.7, type = "ridge") +
  scale_fill_manual(values = all_colors) +
  labs(y = "Number of studies", x = "Publication year") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),  
        axis.title.y = element_text(size = 15),
        legend.text = element_text(size = 12),  
        legend.title = element_text(size = 14) 
  )
stream
## ok its perfect.

ggsave("streamplot.tiff",
       plot=stream,
       scale=1,
       width=3800, height=2500, units=c("px"),
       dpi=300)


###########################################################################################
###### Pie graphs for analysing taxanomic make up #######
#############################################################################################

# Rename the levels of the 'Group' factor to remove the numbers at the start
udata$Group <- recode(udata$Group,
                         "6. Turtle" = "Turtle",
                         "1. Crocodilian" = "Crocodilian",
                         "2. Amphibian" = "Amphibian",
                         "3. Fish" = "Fish",
                         "4. Invertebrate" = "Arthropod",
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

group_tbl2 <- udata %>% 
  group_by(Group3) %>% 
  summarise(
    total_count = n(),
    unique_species_count = n_distinct(Species),
    .groups = 'drop'
  )

group_tbl <- group_tbl %>%
  mutate(percentage = (total_count / sum(total_count)) * 100,
         label = paste0(round(percentage, 1), "%"))

### This is count by major taxanomic group
# Convert tibble to df
df2 <- group_tbl %>% as.data.frame()


## pie chart
# Basic piechart
bigpie <- ggplot(df2, aes(x="", y=total_count, fill=Major.group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  scale_fill_manual(values = mtaxa_colors) +
  labs(title = "") +
  guides(fill=guide_legend(title="")) +
  theme_void() +
  geom_text(aes(label=label),                           
            position=position_stack(vjust=0.5),       
            color="black",                               
            size=3)
bigpie


ggsave("bigtaxpie.tiff",
       plot=bigpie,
       scale=1,
       width=3800, height=2500, units=c("px"),
       dpi=300)

## Create an arthropod only pie chart
Invert <- udata %>% filter(Group %in% c("4. Invertebrate")) 

Invert.counts <- Invert %>% group_by(Group3) %>% 
  summarise(total_count=n(),
            .groups = 'drop')

Invert.counts <- Invert.counts %>%
  mutate(percentage = (total_count / sum(total_count)) * 100,
         label = paste0(round(percentage, 1), "%"))


invertpie <- ggplot(Invert.counts, aes(x="", y=total_count, fill=Group3)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette= "Reds", direction = 1) +
  labs(title = "") +
  guides(fill=guide_legend(title="")) +
  theme_void() +                                        
  geom_text(aes(label=label),                           
            position=position_stack(vjust=0.5),       
            color="black",                               
            size=3)
invertpie

ggsave("invertpie.tiff",
       plot=invertpie,
       scale=1,
       width=3800, height=2500, units=c("px"),
       dpi=300)

##Create a reptiles only pie chart
reptile <- udata %>% filter(Taxa %in% c("Reptile"))

reptile.count <- reptile %>% group_by(Group3) %>% 
  summarise(total_count=n(),
            .groups = 'drop')

reptile.count <- reptile.count %>%
  mutate(percentage = (total_count / sum(total_count)) * 100,
         label = paste0(round(percentage, 1), "%"))


### Reptile pies ###
reptilepie <- ggplot(reptile.count, aes(x="", y=total_count, fill=Group3)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  labs(title = "") +
  scale_fill_manual(values = all_colors) +
  guides(fill=guide_legend(title="")) +
  theme_void() +
  geom_text(aes(label=label),                           
            position=position_stack(vjust=0.5),       
            color="black",                               
            size=3)
reptilepie

ggsave("reptilepie.tiff",
       plot=reptilepie,
       scale=1,
       width=3800, height=2500, units=c("px"),
       dpi=300)

## Amphibian pie


amphib <- udata %>% filter(Major.group %in% c("Amphibian"))

amphib.count <- amphib %>% group_by(Group3) %>% 
  summarise(total_count=n(),
            .groups = 'drop')



amphibianpie <- ggplot(amphib.count, aes(x="", y=total_count, fill=Group3)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  scale_fill_manual(values =("#479821")) +
  labs(title ="") +
  theme_void() 
amphibianpie

ggsave("amphibpie.tiff",
       plot=amphibianpie,
       scale=1,
       width=3800, height=2500, units=c("px"),
       dpi=300)

## fish pie

fishd <- udata %>% filter(Major.group %in% c("Fish"))

fishp.count <- fishd %>% group_by(Group3) %>% 
  summarise(total_count=n(),
            .groups = 'drop')

fishp.count <- fishp.count %>%
  mutate(percentage = (total_count / sum(total_count)) * 100,
         label = paste0(round(percentage, 1), "%"))

# Custom yellow shades
yellow_palette <- colorRampPalette(c("#c19a11", "#edc02e","#f8e7ac" ))(5)

fishnpie <- ggplot(fishp.count, aes(x="", y=total_count, fill=Group3)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  scale_fill_manual(values=yellow_palette) +
  labs(title ="") +
  guides(fill=guide_legend(title="")) +
  theme_void() +
  geom_text(aes(label=label),                           
            position=position_stack(vjust=0.5),       
            color="black",                               
            size=3)
fishnpie

ggsave("fishpie.tiff",
       plot=fishnpie,
       scale=1,
       width=3800, height=2500, units=c("px"),
       dpi=300)


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
  labs(title ="") +
  guides(fill=guide_legend(title="")) +
  theme_void() +
  theme(
    plot.title = element_text(size = 20), legend.text = element_text(size = 14),    
  )
turtlepie


## Arrange for a single figure
taxarrange <- ggarrange(
  bigpie,
  ggarrange(invertpie, reptilepie, turtlepie,  ncol = 3),
  ncol = 1,
  heights = c(2, 1),   # Adjust the heights as needed
  legend = "right")   # Place common legend at the bottom
taxarrange

ggsave("Fig4hires.tiff",
       plot=taxarrange,
       scale=1,
       width=3800, height=2500, units=c("px"),
       dpi=300)

######################## Phenotype figure ############################################
## Change the levels so the pie charts look nicer
udata$Group <- factor(udata$Group, levels =c("Turtle", "Arthropod", "Other Reptile", "Crocodilian", "Fish", "Amphibian"))


##Check how many studies measured phenotype
phenotype.y <- udata %>% filter(phenotype %in% c("y"))
phenotype.n <- udata %>% filter(phenotype %in% c("n"))


##check how many studies measured phenotype
phenotype.counts <- udata %>% group_by(phenotype) %>% 
  summarise(total_count=n(),
            .groups = 'drop')

## might be interesting to see the taxanomic make up of studies that measured phenotypes and those that didnt
## not sure the best way to show this, I will make two pie charts for now.

phenotypey.tax <- phenotype.y %>% group_by(Major.group) %>% 
  summarise(total_count=n(),
            .groups = 'drop')



##Measured phenotypes - broad taxonomic groups

PY <- ggplot(phenotypey.tax, aes(x="", y=total_count, fill=Major.group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  guides(fill=guide_legend(title="Group")) +
  labs(title = "A") +
  scale_fill_manual(values = all_colors) +
  theme(plot.title = element_text(hjust = 1)) +
  theme_void() +
  guides(fill=guide_legend(title="")) +
  theme(
    legend.text = element_text(size = 16),     # Increase legend text size
    legend.title = element_text(size = 18), plot.title = element_text(size = 20))
PY


##Didn't measure phenotypes - broad taxonomic groups
phenotypen.taxn <- phenotype.n %>% group_by(Major.group) %>% 
  summarise(total_count=n(),
            .groups = 'drop')


##Measured phenotypes - broad taxonomic groups

PN <- ggplot(phenotypen.taxn, aes(x="", y=total_count, fill=Major.group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  guides(fill=guide_legend(title="Group")) +
  labs(title = "B") +
  scale_fill_manual(values = all_colors) +
  theme(plot.title = element_text(hjust = 1)) +
  theme_void() +
  guides(fill=guide_legend(title="")) +
  theme(
    legend.text = element_text(size = 16),     # Increase legend text size
    legend.title = element_text(size = 18), plot.title = element_text(size = 20)     # Increase legend title size
  )
PN



ppie <- ggarrange(PY, PN,
          labels = c(),
          common.legend = TRUE, legend = "right",
          ncol = 2, nrow = 1)

filen <- "naturalnestsMA/phenotypepies"

# PDF Export 
graph2vector(x = ppie, file = paste0(filen, ".pdf"), width = 5, height = 8)



#######################################################################################
## Study topics ###########################################################################
#####################################################################################

# Rename some topics
udata$topic2 <- recode(udata$topic2,
                      "Species management" = "Threatened species management",
                      "Evolution" = "Evolutionary transitions")
  
topic_counts <- udata %>% group_by(topic2) %>% 
    summarise(total_count=n(),
              .groups = 'drop')

#look at sub-categories
topic_counts2 <- udata %>%
  group_by(topic2, topic) %>%
  summarise(count = n(), .groups = "drop")

# Subject pies

# Calculate percentages and add them to the data
topic_counts <- topic_counts %>%
  mutate(percentage = (total_count / sum(total_count)) * 100,
         label = paste0(round(percentage, 1), "%"))

# Create the pie chart with percentage labels
subjectpie <- ggplot(topic_counts, aes(x="", y=total_count, fill=topic2)) +
  geom_bar(stat="identity", width=1, color="white") +   
  coord_polar("y", start=0) +                           
  labs(title = "") +                                    
  guides(fill=guide_legend(title="Topics")) +           
  theme_void()+                                        
  geom_text(aes(label=label),                           
            position=position_stack(vjust=0.5),       
            color="black",                               
            size=3)  
subjectpie

  ## Arthropods
  invert_topic_counts <- udata %>%
    filter(Major.group == "Arthropod") %>%
    group_by(topic2) %>%
    summarise(topic_count = n(), .groups = 'drop')
  
  # Calculate percentages and add them to the data
  invert_topic_counts <- invert_topic_counts %>%
    mutate(percentage = (topic_count / sum(topic_count)) * 100,
           label = paste0(round(percentage, 1), "%"))
  
  subjectIpie <- ggplot(invert_topic_counts, aes(x="", y=topic_count, fill=topic2)) +
    geom_bar(stat="identity", width=1, color="white") +    
    coord_polar("y", start=0) +                            
    labs(title = "") +                                     
    guides(fill=guide_legend(title="Topics")) +            
    theme_void() +                                        
    geom_text(aes(label=label),                           
              position=position_stack(vjust=0.5),       
              color="black",                               
              size=3)     
  subjectIpie
  
  ## all reptiles
  Reptile_topic_counts <- udata %>%
    filter(Major.group == "Reptile") %>%
    group_by(topic2) %>%
    summarise(topic_count = n(), .groups = 'drop')
  
  # Create a pie chart for the combined group
  # Calculate percentages and add them to the data
  Reptile_topic_counts <- Reptile_topic_counts %>%
    mutate(percentage = (topic_count / sum(topic_count)) * 100,
           label = paste0(round(percentage, 1), "%"))
  
  # Create the pie chart with percentage labels
  pie_chartR <- ggplot(Reptile_topic_counts, aes(x = "", y = topic_count, fill = topic2)) +
    geom_bar(stat = "identity", width = 1, color = "white") +   
    coord_polar("y", start = 0) +                               
    labs(title = "") +                                           
    guides(fill = guide_legend(title = "Topics")) +              
    theme_void()
  pie_chartR
  
  ## Fish pie
  filtered_dataF <- udata %>%
    filter(Major.group %in% c("Fish"))
  
  group_countsF <- filtered_dataF %>%
    group_by(Major.group, topic2) %>%
    summarise(topic_count = n(), .groups = 'drop')
  
  
  # Calculate percentages and add them to the data
  group_countsF <- group_countsF %>%
    mutate(percentage = (topic_count / sum(topic_count)) * 100,
           label = paste0(round(percentage, 1), "%"))
  
  # Create the pie chart with percentage labels
  pie_chartF <- ggplot(group_countsF, aes(x = "", y = topic_count, fill = topic2)) +
    geom_bar(stat = "identity", width = 1, color = "white") +    
    coord_polar("y", start = 0) +                                
    labs(title = "") +                                           
    guides(fill = guide_legend(title = "Topics")) +              
    theme_void()
  pie_chartF
  
  #amphibian pie
  
  filtered_dataA <- udata %>%
    filter(Major.group %in% c("Amphibian"))
  
  group_countsA <- filtered_dataA %>%
    group_by(Major.group, topic2) %>%
    summarise(topic_count = n(), .groups = 'drop')
  
  # Calculate percentages and add them to the data
  group_countsA <- group_countsA %>%
    mutate(percentage = (topic_count / sum(topic_count)) * 100,
           label = paste0(round(percentage, 1), "%"))
  
  # Create the pie chart with percentage labels
  pie_chartA <- ggplot(group_countsA, aes(x = "", y = topic_count, fill = topic2)) +
    geom_bar(stat = "identity", width = 1, color = "white") +   
    coord_polar("y", start = 0) +                               
    labs(title = "") +                                          
    guides(fill = guide_legend(title = "")) +                   
    theme_void() +                                        
    geom_text(aes(label=label),                           
              position=position_stack(vjust=0.5),       
              color="black",                               
              size=3) 
  pie_chartA
  
  subjectpie<- subjectpie + scale_fill_manual(values = subject_colors)
  subjectIpie <- subjectIpie + scale_fill_manual(values = subject_colors)
  pie_chartR<-  pie_chartR + scale_fill_manual(values = subject_colors)
  pie_chartF<- pie_chartF + scale_fill_manual(values = subject_colors)
  pie_chartA<- pie_chartA + scale_fill_manual(values = subject_colors)

  pie_chartR <-  pie_chartR +
    theme(legend.position = "none")
  
  subjectIpie <- subjectIpie +
    theme(legend.position = "none")
  
  pie_chartF <- pie_chartF +
    theme(legend.position = "none")
  pie_chartA <- pie_chartA +
    theme(legend.position = "none")
  
  subjectpie  <- subjectpie +
    theme(legend.position = "none")
  
  # Arrange the pie charts using ggarrange with a common legend
  arranged_pie_charts <- ggarrange(
    subjectpie, labels = c(),
    ggarrange(subjectIpie, pie_chartF, pie_chartA, pie_chartR, ncol = 4), ncol = 1,
    heights = c(2, 1),  
    legend = "none")   
  arranged_pie_charts

  ggsave("subjpiehires.tiff",
         plot=arranged_pie_charts,
         scale=1,
         width=3800, height=2500, units=c("px"),
         dpi=300)
  
  
  ## Not being used right now
  ## Look at sea turtles
  sturtle_topic_counts <- udata %>%
    filter(Group2 == "Sea Turtle") %>%
    group_by(topic2) %>%
    summarise(total_count=n(), .groups = 'drop')
  
  subjectSTpie <- ggplot(sturtle_topic_counts, aes(x="", y=total_count, fill=topic2)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    labs(title = "") +
    guides(fill=guide_legend(title="Topics")) +
    theme_void()
  subjectSTpie
  

  
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
  
  
  ########################################## Scatterplots #################################################
  ### MEAN##
  
  #### Old code that was lost ####
  ## Models are in the Macroecology script ##

  data2 <- mutate(data2, abs_lat = abs(Lat)) # Added absolute latitude column
  
  ## Predictions for "All taxa, major grouping"
  
  data_amphibian <- data.frame(Taxa = "Amphibian",
                               abs_lat = seq(min(data2$abs_lat[data2$Taxa == "Amphibian"]), max(data2$abs_lat[data2$Taxa == "Amphibian"]), length = 10))
  data_reptile <- data.frame(Taxa = "Reptile",
                             abs_lat = seq(min(data2$abs_lat[data2$Taxa == "Reptile"]), max(data2$abs_lat[data2$Taxa == "Reptile"]), length = 10))
  data_invert  <- data.frame(Taxa = "Arthropod",
                             abs_lat = seq(min(data2$abs_lat[data2$Taxa == "Arthropod"]), max(data2$abs_lat[data2$Taxa == "Arthropod"]), length = 10))
  data_fish <- data.frame(Taxa = "Fish",
                          abs_lat = seq(min(data2$abs_lat[data2$Taxa == "Fish"]), max(data2$abs_lat[data2$Taxa == "Fish"]), length = 10))
  new_data <- rbind(data_amphibian,
                    data_reptile,
                    data_invert,
                    data_fish) # Create dataframe with the right latitudinal range for each taxon
  est <- emmeans(mod, specs = "abs_lat", by  = "Taxa", data = data2,
                 at = list(abs_lat = new_data$abs_lat)) # Predict values based on the latitudinal range in new_data
  est.data <- as.data.frame(est)
  est.data <- est.data %>%
    inner_join(new_data, by = c("Taxa", "abs_lat")) # Only keep the predictions for the latitudinal range of each taxon


  # Major taxa

  Majorscat <- ggplot(est.data, aes(x = abs_lat, y = emmean, col = Taxa, fill = Taxa)) +
    geom_ribbon(aes(ymin = lower.HPD, ymax = upper.HPD), alpha = 0.2, col = NA) +
    geom_point(data = data2, aes(x = abs_lat, y = Mean), size = 3, alpha = 0.6) +
    geom_line(data = est.data, aes(group = Taxa), size = 2, linewidth = 2) +
    labs(x = "", y = "") +
    scale_color_manual(values = mtaxa_colors) +
    scale_fill_manual(values = mtaxa_colors) +
    theme_bw() +
    theme(
      plot.margin = unit(c(0, 0, 0, 0), "lines"),
      axis.title.y = element_text(size = 10)
    )
  Majorscat
  
  # histogram plot
  Majorden <- data2 %>% ggplot(aes(x = abs_lat, fill = Taxa, colour = Taxa)) +
    geom_histogram(alpha = 0.4, bw=1.5) +
    theme_bw() +
    scale_color_manual(values = mtaxa_colors) +
    scale_fill_manual(values = mtaxa_colors) +
    labs(x = NULL, y = "", fill = NULL, color = NULL) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    theme(plot.margin = unit(c(0, 0, -0.5, 0), "lines"))
  Majorden
  # Create the combined plot with common legend
  Scat1 <- ggarrange(Majorden, Majorscat,
                     common.legend = TRUE, legend = FALSE, align = "v",
                     ncol = 1, nrow = 2, heights = c(2, 4), hjust = -0.5, vjust = 1)
  Scat1
  
  
## Reptiles only predictions and graphs ##
  
  ## Predictions for "Reptiles"
  
  data_st <- data.frame(Group2 = "Sea Turtle",
                               abs_lat = seq(min(reptiles$abs_lat[reptiles$Group2 == "Sea Turtle"]), max(reptiles$abs_lat[reptiles$Group2 == "Sea Turtle"]), length = 10))
  data_croc <- data.frame(Group2 = "Crocodilian",
                             abs_lat = seq(min(reptiles$abs_lat[reptiles$Group2 == "Crocodilian"]), max(reptiles$abs_lat[reptiles$Group2 == "Crocodilian"]), length = 10))
  data_FWT  <- data.frame(Group2 = "Freshwater Turtle",
                             abs_lat = seq(min(reptiles$abs_lat[reptiles$Group2 == "Freshwater Turtle"]), max(reptiles$abs_lat[reptiles$Group2 == "Freshwater Turtle"]), length = 10))
  data_LS <- data.frame(Group2 = "Lizard/snake",
                          abs_lat = seq(min(reptiles$abs_lat[reptiles$Group2 == "Lizard/snake"]), max(reptiles$abs_lat[reptiles$Group2 == "Lizard/snake"]), length = 10))
  new_dataR <- rbind(data_st,
                    data_croc,
                    data_FWT,
                    data_LS) # Create dataframe with the right latitudinal range for each taxon
  estR <- emmeans(mod2, specs = "abs_lat", by  = "Group2", data = reptiles,
                 at = list(abs_lat = new_dataR$abs_lat)) # Predict values based on the latitudinal range in new_data
  est.dataR <- as.data.frame(estR)
  est.dataR <- est.dataR %>%
    inner_join(new_dataR, by = c("Group2", "abs_lat")) # Only keep the predictions for the latitudinal range of each taxon
  
  ## plot these predictions 
  
  reptilescat <- ggplot(est.dataR, aes(x = abs_lat, y = emmean, col = Group2, fill = Group2)) +
    geom_ribbon(aes(ymin = lower.HPD, ymax = upper.HPD), alpha = 0.2, col = NA) +
    geom_point(data = reptiles, aes(x = abs_lat, y = Mean), size = 3, alpha = 0.6) +
    geom_line(data = est.dataR, aes(group = Group2), size = 2, linewidth = 2) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 40))+
    scale_color_manual(values = reptile_colors) +
    scale_fill_manual(values = reptile_colors) +
    theme_bw() +
    theme(
      plot.margin = unit(c(0, 0, 0, 0), "lines"),
      axis.title.y = element_text(size = 10)
    )
  reptilescat
  
  # histogram plot
  reptileh <- reptiles %>% ggplot(aes(x = abs_lat, fill = Group2, colour = Group2)) +
    geom_histogram(alpha = 0.4, bw=1.5) +
    theme_bw() +
    scale_color_manual(values = reptile_colors) +
    scale_fill_manual(values = reptile_colors) +
    labs(x = NULL, y = "", fill = NULL, color = NULL) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    theme(plot.margin = unit(c(0, 0, -0.5, 0), "lines"))
  reptileh
  
  # Create the combined plot with common legend
  Scat2 <- ggarrange(reptileh, reptilescat,
                     common.legend = TRUE, legend = FALSE, align = "v",
                     ncol = 1, nrow = 2, heights = c(2, 4), hjust = -0.5, vjust = 1)
  Scat2
  
  
  ## Predictions for "Water/land"
  
  data_water <- data.frame(Water = "Water",
                               abs_lat = seq(min(data2$abs_lat[data2$Water == "Water"]), max(data2$abs_lat[data2$Water == "Water"]), length = 10))
  data_land <- data.frame(Water = "Land",
                             abs_lat = seq(min(data2$abs_lat[data2$Water == "Land"]), max(data2$abs_lat[data2$Water == "Land"]), length = 10))

  new_dataW <- rbind(data_water,
                    data_land) # Create dataframe with the right latitudinal range for each taxon
  estW <- emmeans(mod3, specs = "abs_lat", by  = "Water", data = data2,
                 at = list(abs_lat = new_dataW$abs_lat)) # Predict values based on the latitudinal range in new_data
  est.dataW <- as.data.frame(estW)
  est.dataW <- est.dataW %>%
    inner_join(new_dataW, by = c("Water", "abs_lat")) # Only keep the predictions for the latitudinal range of each taxon
  
  
  # Water/land scatterplot
  
  Waterscat <- ggplot(est.dataW, aes(x = abs_lat, y = emmean, col = Water, fill = Water)) +
    geom_ribbon(aes(ymin = lower.HPD, ymax = upper.HPD), alpha = 0.2, col = NA) +
    geom_point(data = data2, aes(x = abs_lat, y = Mean), size = 3, alpha = 0.6) +
    geom_line(data = est.dataW, aes(group = Water), size = 2, linewidth = 2) +
    labs(x = "", y = "") +
    scale_y_continuous(limits = c(0, 40))+
    scale_color_manual(values = water_colors) +
    scale_fill_manual(values = water_colors) +
    theme_bw() +
    theme(
      plot.margin = unit(c(0, 0, 0, 0), "lines"),
      axis.title.y = element_text(size = 10)
    )
  Waterscat
  
  # histogram plot
  Whist <- data2 %>% ggplot(aes(x = abs_lat, fill = Water, colour = Water)) +
    geom_histogram(alpha = 0.4, bw=1.5) +
    theme_bw() +
    scale_color_manual(values = water_colors) +
    scale_fill_manual(values = water_colors) +
    labs(x = NULL, y = "", fill = NULL, color = NULL) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    theme(plot.margin = unit(c(0, 0, -0.5, 0), "lines"))
  Whist
  # Create the combined plot with common legend
  Scat3 <- ggarrange(Whist, Waterscat,
                     common.legend = TRUE, legend = FALSE, align = "v",
                     ncol = 1, nrow = 2, heights = c(2, 4), hjust = -0.5, vjust = 1)
  Scat3
  
  
  meanscats<- ggarrange(Scat1, Scat2, Scat3,
            common.legend = FALSE, legend = FALSE, align = "v",
            ncol = 1, nrow = 3, hjust = -0.5, vjust = 1)
 meanscats 
  
  filen <- "naturalnestsMA/meanscats"
  
  # PDF Export 
  graph2vector(x = meanscats, file = paste0(filen, ".pdf"), width = 5, height = 8)
  
###################### Standard deviation prediction plots ##########################
  ##########################################################################
  
  SD_data$Taxa <- recode(SD_data$Taxa,
                           "Invertebrate" = "Arthropod",
                           )
  
  ## Predictions for "All taxa, major grouping"
  
  data_amphibian <- data.frame(Taxa = "Amphibian",
                               abs_lat = seq(min(SD_data$abs_lat[SD_data$Taxa == "Amphibian"]), max(SD_data$abs_lat[SD_data$Taxa == "Amphibian"]), length = 10))
  data_reptile <- data.frame(Taxa = "Reptile",
                             abs_lat = seq(min(SD_data$abs_lat[SD_data$Taxa == "Reptile"]), max(SD_data$abs_lat[SD_data$Taxa == "Reptile"]), length = 10))
  data_invert  <- data.frame(Taxa = "Arthropod",
                             abs_lat = seq(min(SD_data$abs_lat[SD_data$Taxa == "Arthropod"]), max(SD_data$abs_lat[SD_data$Taxa == "Arthropod"]), length = 10))
  data_fish <- data.frame(Taxa = "Fish",
                          abs_lat = seq(min(SD_data$abs_lat[SD_data$Taxa == "Fish"]), max(SD_data$abs_lat[SD_data$Taxa == "Fish"]), length = 10))
  new_dataSD <- rbind(data_amphibian,
                    data_reptile,
                    data_invert,
                    data_fish) # Create dataframe with the right latitudinal range for each taxon
  estSD <- emmeans(modSD, specs = "abs_lat", by  = "Taxa", data = SD_data,
                 at = list(abs_lat = new_dataSD$abs_lat)) # Predict values based on the latitudinal range in new_data
  est.dataSD <- as.data.frame(estSD)
  est.dataSD <- est.dataSD %>%
    inner_join(new_dataSD, by = c("Taxa", "abs_lat")) # Only keep the predictions for the latitudinal range of each taxon
  
  # Back-transform the predictions from log scale to original scale
  est.dataSD <- est.dataSD %>%
    mutate(emmean = exp(emmean),
           lower.HPD = exp(lower.HPD),
           upper.HPD = exp(upper.HPD))
  
  # Major taxa
  
  Majorscat <- ggplot(est.dataSD, aes(x = abs_lat, y = emmean, col = Taxa, fill = Taxa)) +
    geom_ribbon(aes(ymin = lower.HPD, ymax = upper.HPD), alpha = 0.2, col = NA) +
    geom_point(data = data2, aes(x = abs_lat, y = Among_SD), size = 3, alpha = 0.6) +
    geom_line(data = est.dataSD, aes(group = Taxa), size = 2, linewidth = 2) +
    labs(x = "", y = "") +
    scale_x_continuous(limits = c(0, 63)) +
    scale_y_continuous(limits = c(0, 10))+
    scale_color_manual(values = mtaxa_colors) +
    scale_fill_manual(values = mtaxa_colors) +
    theme_bw() +
    theme(
      plot.margin = unit(c(0, 0, 0, 0), "lines"),
      axis.title.y = element_text(size = 10)
    )
  Majorscat
  
  # histogram plot
  Majorden <- SD_data %>% ggplot(aes(x = abs_lat, fill = Taxa, colour = Taxa)) +
    geom_histogram(alpha = 0.4, bw=1.5) +
    theme_bw() +
    scale_color_manual(values = mtaxa_colors) +
    scale_fill_manual(values = mtaxa_colors) +
    labs(x = NULL, y = "", fill = NULL, color = NULL) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    theme(plot.margin = unit(c(0, 0, -0.5, 0), "lines"))
  Majorden
  # Create the combined plot with common legend
  Scat1 <- ggarrange(Majorden, Majorscat,
                     common.legend = TRUE, legend = FALSE, align = "v",
                     ncol = 1, nrow = 2, heights = c(2, 4), hjust = -0.5, vjust = 1)
  Scat1
  
  
  ## Predictions for "Reptiles"
  
  data_st <- data.frame(Group2 = "Sea Turtle",
                        abs_lat = seq(min(SDrepdata$abs_lat[SDrepdata$Group2 == "Sea Turtle"]), max(SDrepdata$abs_lat[SDrepdata$Group2 == "Sea Turtle"]), length = 10))
  data_croc <- data.frame(Group2 = "Crocodilian",
                          abs_lat = seq(min(SDrepdata$abs_lat[SDrepdata$Group2 == "Crocodilian"]), max(SDrepdata$abs_lat[SDrepdata$Group2 == "Crocodilian"]), length = 10))
  data_FWT  <- data.frame(Group2 = "Freshwater Turtle",
                          abs_lat = seq(min(SDrepdata$abs_lat[SDrepdata$Group2 == "Freshwater Turtle"]), max(SDrepdata$abs_lat[SDrepdata$Group2 == "Freshwater Turtle"]), length = 10))
  data_LS <- data.frame(Group2 = "Lizard/snake",
                        abs_lat = seq(min(SDrepdata$abs_lat[SDrepdata$Group2 == "Lizard/snake"]), max(SDrepdata$abs_lat[SDrepdata$Group2 == "Lizard/snake"]), length = 10))
  new_dataRSD <- rbind(data_st,
                     data_croc,
                     data_FWT,
                     data_LS) # Create dataframe with the right latitudinal range for each taxon
  estRSD <- emmeans(mod2SD, specs = "abs_lat", by  = "Group2", data = SDrepdata,
                  at = list(abs_lat = new_dataRSD$abs_lat)) # Predict values based on the latitudinal range in new_data
  est.dataRSD <- as.data.frame(estRSD)
  est.dataRSD <- est.dataRSD %>%
    inner_join(new_dataRSD, by = c("Group2", "abs_lat")) # Only keep the predictions for the latitudinal range of each taxon
  
  est.dataRSD <- est.dataRSD %>%
    mutate(emmean = exp(emmean),
           lower.HPD = exp(lower.HPD),
           upper.HPD = exp(upper.HPD))
  
  ## plot these predictions 
  
  reptilescat <- ggplot(est.dataRSD, aes(x = abs_lat, y = emmean, col = Group2, fill = Group2)) +
    geom_ribbon(aes(ymin = lower.HPD, ymax = upper.HPD), alpha = 0.2, col = NA) +
    geom_point(data = SDrepdata, aes(x = abs_lat, y = Among_SD), size = 3, alpha = 0.6) +
    geom_line(data = est.dataRSD, aes(group = Group2), size = 2, linewidth = 2) +
    labs(x = "", y = "") +
    scale_x_continuous(limits = c(0, 63)) +
    scale_y_continuous(limits = c(0, 10))+
    scale_color_manual(values = reptile_colors) +
    scale_fill_manual(values = reptile_colors) +
    theme_bw() +
    theme(
      plot.margin = unit(c(0, 0, 0, 0), "lines"),
      axis.title.y = element_text(size = 10)
    )
  reptilescat
  
  # histogram plot
  reptileh <- SDrepdata %>% ggplot(aes(x = abs_lat, fill = Group2, colour = Group2)) +
    geom_histogram(alpha = 0.4, bw=1.5) +
    theme_bw() +
    scale_color_manual(values = reptile_colors) +
    scale_fill_manual(values = reptile_colors) +
    labs(x = NULL, y = "", fill = NULL, color = NULL) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    theme(plot.margin = unit(c(0, 0, -0.5, 0), "lines"))
  reptileh
  
  # Create the combined plot with common legend
  Scat2 <- ggarrange(reptileh, reptilescat,
                     common.legend = TRUE, legend = FALSE, align = "v",
                     ncol = 1, nrow = 2, heights = c(2, 4), hjust = -0.5, vjust = 1)
  Scat2
  
  
  
  ## Predictions for "Water/land"
  
  data_water <- data.frame(Water = "Water",
                           abs_lat = seq(min(SD_data$abs_lat[SD_data$Water == "Water"]), max(SD_data$abs_lat[SD_data$Water == "Water"]), length = 10))
  data_land <- data.frame(Water = "Land",
                          abs_lat = seq(min(SD_data$abs_lat[SD_data$Water == "Land"]), max(SD_data$abs_lat[SD_data$Water == "Land"]), length = 10))
  
  new_dataWSD <- rbind(data_water,
                     data_land) # Create dataframe with the right latitudinal range for each taxon
  estWSD <- emmeans(mod3SD, specs = "abs_lat", by  = "Water", data = SD_data,
                  at = list(abs_lat = new_dataWSD$abs_lat)) # Predict values based on the latitudinal range in new_data
  est.dataWSD <- as.data.frame(estWSD)
  est.dataWSD <- est.dataWSD %>%
    inner_join(new_dataWSD, by = c("Water", "abs_lat")) # Only keep the predictions for the latitudinal range of each taxon
  
  est.dataWSD <- est.dataWSD %>%
    mutate(emmean = exp(emmean),
           lower.HPD = exp(lower.HPD),
           upper.HPD = exp(upper.HPD))
  
  # Water/land scatterplot
  
  Waterscat <- ggplot(est.dataWSD, aes(x = abs_lat, y = emmean, col = Water, fill = Water)) +
    geom_ribbon(aes(ymin = lower.HPD, ymax = upper.HPD), alpha = 0.2, col = NA) +
    geom_point(data = SD_data, aes(x = abs_lat, y = Among_SD), size = 3, alpha = 0.6) +
    geom_line(data = est.dataWSD, aes(group = Water), size = 2, linewidth = 2) +
    labs(x = "", y = "") +
    scale_x_continuous(limits = c(0, 63)) +
    scale_y_continuous(limits = c(0, 10))+
    scale_color_manual(values = water_colors) +
    scale_fill_manual(values = water_colors) +
    theme_bw() +
    theme(
      plot.margin = unit(c(0, 0, 0, 0), "lines"),
      axis.title.y = element_text(size = 10)
    )
  Waterscat
  
  # histogram plot
  Whist <- SD_data %>% ggplot(aes(x = abs_lat, fill = Water, colour = Water)) +
    geom_histogram(alpha = 0.4, bw=1.5) +
    theme_bw() +
    scale_color_manual(values = water_colors) +
    scale_fill_manual(values = water_colors) +
    labs(x = NULL, y = "", fill = NULL, color = NULL) +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
    theme(plot.margin = unit(c(0, 0, -0.5, 0), "lines"))
  Whist
  # Create the combined plot with common legend
  Scat3 <- ggarrange(Whist, Waterscat,
                     common.legend = TRUE, legend = FALSE, align = "v",
                     ncol = 1, nrow = 2, heights = c(2, 4), hjust = -0.5, vjust = 1)
  Scat3
  
  
SDscats <- ggarrange(Scat1, Scat2, Scat3,
            common.legend = FALSE, legend = FALSE, align = "v",
            ncol = 1, nrow = 3, hjust = -0.5, vjust = 1)
  
  
  filen <- "naturalnestsMA/SDscats"
  
  # PDF Export 
  graph2vector(x = SDscats, file = paste0(filen, ".pdf"), width = 5, height = 8)
  
  
  
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





