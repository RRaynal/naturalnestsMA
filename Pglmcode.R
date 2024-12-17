


#################################################################
## Try a PGLMM - a linear mixed effect model that includes a phylogeny
#################################################################

library(phyr)
library(ape)

data$study_ID <- as.character(data$study_ID)
data$sp_ <- data$Animal
data$Mean <- as.numeric(data$Mean)

# First run an intercept model 

pglmmint <- phyr::pglmm(Mean ~ (1 | sp_) + (1 | study_ID) + (1 | Animal), 
                        data = data, 
                        cov_ranef = list(sp = tl2$tip.label),
                        family = "gaussian")
summary(pglmmint)

#logLik    AIC    BIC 
#-951   1912   1926 

pglmmlat <- phyr::pglmm(Mean ~ abs(Lat) + (1 | sp_) + (1 | study_ID) + (1 | Animal), 
                        data = data, 
                        cov_ranef = list(sp = tl2$tip.label),
                        family = "gaussian")
summary(pglmmlat)

#logLik    AIC    BIC 
#-948.5 1908.9 1925.9 



#Change the reference level for Major.group
data$Major.group <- relevel(data$Major.group, ref = "Reptile")

pglmmgroup <- phyr::pglmm(Mean ~ Major.group-1 + (1 | sp_) + (1 | Animal) + (1 | study_ID), 
                          data = data, 
                          cov_ranef = list(sp = tl2$tip.label),
                          family = "gaussian")
summary(pglmmgroup)

#logLik    AIC    BIC 
#-928.8 1873.7 1896.3 

pglmmgroup2 <- phyr::pglmm(Mean ~ abs(Lat) + Major.group-1 + (1 | sp_) + (1 | Animal) + (1 | study_ID), 
                           data = data, 
                           cov_ranef = list(sp = tl2$tip.label),
                           family = "gaussian")
summary(pglmmgroup2)

#logLik    AIC    BIC 
#-926.4 1870.9 1896.3 

pglmmgroup2 <- phyr::pglmm(Mean ~ abs(Lat)*Major.group + (1 | sp_) + (1 | Animal) + (1 | study_ID), 
                           data = data, 
                           cov_ranef = list(sp = tl2$tip.label),
                           family = "gaussian")
summary(pglmmgroup2)

#logLik    AIC    BIC 
#-910.1 1844.2 1878.1 

pglmmwater <- phyr::pglmm(Mean ~ Water-1 + (1 | sp_) + (1 | Animal) + (1 | study_ID), 
                          data = data, 
                          cov_ranef = list(sp = tl2$tip.label),
                          family = "gaussian")
summary(pglmmwater)

#logLik    AIC    BIC 
#-935.7 1883.3 1900.3

pglmm4 <- phyr::pglmm(Mean ~ abs(Lat)*Water + (1 | sp_) + (1 | Animal) + (1 | study_ID), 
                      data = data, 
                      cov_ranef = list(sp = tl2$tip.label),
                      family = "gaussian")
summary(pglmm4)

##logLik    AIC    BIC 
##-928.3 1872.6 1895.2 




## These models only use the reptile data so they can't be compared to the above 
## which use all the data

reptiledata <- data %>% filter(Major.group %in% c("Reptile"))
reptiledata$Group2 <- relevel(reptiledata$Group2, ref = "Sea Turtle")

pglmmrepint <- phyr::pglmm(Mean ~ (1 | sp_) + (1 | Animal) + (1 | study_ID), 
                           data = reptiledata, 
                           cov_ranef = list(sp = tl2$tip.label),
                           family = "gaussian")
summary(pglmmrepint)

#logLik    AIC    BIC 
#-637.4 1284.9 1297.8 

pglmmreplat <- phyr::pglmm(Mean ~ abs(Lat) + (1 | sp_) + (1 | Animal) + (1 | study_ID), 
                           data = reptiledata, 
                           cov_ranef = list(sp = tl2$tip.label),
                           family = "gaussian")
summary(pglmmreplat)

#logLik    AIC    BIC 
#-635.4 1282.9 1298.4 

pglmmrep <- phyr::pglmm(Mean ~ Group2-1 + (1 | sp_) + (1 | Animal) + (1 | study_ID), 
                        data = reptiledata, 
                        cov_ranef = list(sp = tl2$tip.label),
                        family = "gaussian")
summary(pglmmrep)

#logLik    AIC    BIC 
#-606.8 1229.6 1250.2 


pglmmrep <- phyr::pglmm(Mean ~ abs(Lat)*Group2 + (1 | sp_) + (1 | Animal) + (1 | study_ID), 
                        data = reptiledata, 
                        cov_ranef = list(sp = tl2$tip.label),
                        family = "gaussian")
summary(pglmmrep)

#logLik    AIC    BIC 
#-597.4 1218.8 1249.7 

pglmmrep2 <- phyr::pglmm(Mean ~ abs(Lat) + Group2-1 + (1 | sp_) + (1 | Animal) + (1 | study_ID), 
                         data = reptiledata, 
                         cov_ranef = list(sp = tl2$tip.label),
                         family = "gaussian")
summary(pglmmrep2)

#logLik    AIC    BIC 
#-604.7 1227.3 1250.6 

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

SD_data <- data %>% filter(!is.na(Among_SD))
SD_data<-as.data.frame(SD_data)

SD_data$Among_SD_log <- log(SD_data$Among_SD)
SD_data <- SD_data %>%
  filter(!is.na(Among_SD_log), !is.infinite(Among_SD_log))


#Intercept only
pglmmSDint <- phyr::pglmm(Among_SD_log ~ (1 | sp_) + (1 | study_ID) + (1 | Animal), 
                          data = SD_data, 
                          cov_ranef = list(sp = tl2$tip.label),
                          family = "gaussian") 
summary(pglmmSDint)

# Get the residuals from your pglmm model
residuals <- residuals(pglmmSDint)

# Create a scatterplot of residuals vs. fitted values
plot(fitted(pglmmSDint), residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values",
     pch = 16, col = "blue")
abline(h = 0, col = "red", lty = 2)

##pglmm Check the full model preferred by AIC model selection

pglmmSD <- phyr::pglmm(Among_SD_log ~ abs(Lat)*Taxa + (1 | sp_) + (1 | study_ID) + (1 | Animal), 
                       data = SD_data, 
                       cov_ranef = list(sp = tl2$tip.label),
                       family = "gaussian")
summary(pglmmSD)


## Fan shaped residuals, so I will need to log SD.

data$Among_SD_log <- log(data$Among_SD)
data <- data %>%
  filter(!is.infinite(Among_SD_log))
data <- data %>%
  filter(Among_SD != 0)

SD_data <- data %>% filter(!is.na(Among_SD_log))

# check the model residuals now

pglmmSD <- phyr::pglmm(Among_SD_log ~ (1 | Species) + (1 | study_ID) + (1 | Animal), 
                       data = SD_data, 
                       cov_ranef = list(tl2_brlen),
                       family = "gaussian") 
summary(pglmmSD)

# Get the residuals from your pglmm model
residuals <- residuals(pglmmSDint)

# Create a scatterplot of residuals vs. fitted values
plot(fitted(pglmmSDint), residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values",
     pch = 16, col = "blue")
abline(h = 0, col = "red", lty = 2)

## This looks better, residuals are scattered around the 


plotSD <- ggplot(SD_data, aes(x = abs(Lat), y = Among_SD_log, col = Major.group)) +
  geom_point() + geom_smooth(method = "lm") +
  labs(x = "", y = "") +
  theme_bw() +
  scale_colour_brewer(palette = "Spectral", direction = 1) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "lines"),
        axis.title.y = element_text(size = 10))  # Adjust the size as needed
plotSD

# AIC Selection models:

#logLik    AIC    BIC 
#-265.4  542.8  557.1 

# INTERESTING - phylogeny is not absorbing most of the variance, variance across the random effects
# is quite low. In addition, there is a very significant relationship between standard deviation among nests
# and absolute latitude. As latitude increases, SD increases.

pglmmSDgr <- phyr::pglmm(Among_SD ~ Major.group-1 + (1 | sp_) + (1 | study_ID) + (1 | Animal), 
                         data = data, 
                         cov_ranef = list(sp = tl2$tip.label),
                         family = "gaussian")
summary(pglmmSDgr)

#logLik    AIC    BIC 
#-267.2  550.3  569.5

pglmmSD2 <- phyr::pglmm(Among_SD ~ abs(Lat)+Major.group-1 + (1 | sp_) + (1 | study_ID) + (1 | Animal), 
                        data = data, 
                        cov_ranef = list(sp = tl2$tip.label),
                        family = "gaussian")
summary(pglmmSD2)

#logLik    AIC    BIC 
#-260.9  539.8  561.4 


# Add in major taxanomic group to take a look if there are any patterns there
pglmmSDinteract <- phyr::pglmm(Among_SD ~ abs(Lat)*Major.group + (1 | sp_) + (1 | study_ID) + (1 | Animal), 
                               data = data, 
                               cov_ranef = list(sp = tl2$tip.label),
                               family = "gaussian")
summary(pglmmSDinteract)
residuals(pglmmSDinteract)

#logLik    AIC    BIC 
#-256.2  536.5  565.2 

# However, when you add in the taxanomic groups, latitude is no longer a significant predictor, 
# now nothing is significant.


#AIC selection prefers the interaction model

reptileSDdata <- SD_data %>% filter(Major.group %in% c("Reptile"))

## Reptiles only
pglmmSDrepint <- phyr::pglmm(Among_SD_log ~ (1 | sp_) + (1 | study_ID) + (1 | Animal), 
                             data = reptileSDdata, 
                             cov_ranef = list(sp = tl2$tip.label),
                             family = "gaussian")
summary(pglmmSDrepint)

#logLik    AIC    BIC 
#-199.0  407.9  419.2 




pglmmSDreplat <- phyr::pglmm(Among_SD_log ~ abs(Lat) + (1 | sp_) + (1 | study_ID)+ (1 | Animal), 
                             data = reptileSDdata, 
                             cov_ranef = list(sp = tl2$tip.label),
                             family = "gaussian")
summary(pglmmSDreplat)

#logLik    AIC    BIC 
#-195.1  402.1  415.7 


pglmmSDrepgr <- phyr::pglmm(Among_SD_log ~ Group2-1 + (1 | sp_) + (1 | study_ID)+ (1 | Animal), 
                            data = reptiledata, 
                            cov_ranef = list(sp = tl2$tip.label),
                            family = "gaussian")
summary(pglmmSDrepgr)

## Same as above, no significance and no difference between reptile groups

#logLik    AIC    BIC 
#-189.5  395.0  412.9

pglmmSDrepgr2 <- phyr::pglmm(Among_SD_log ~ abs(Lat)+Group2-1 + (1 | sp_) + (1 | study_ID)+ (1 | Animal), 
                             data = reptiledata, 
                             cov_ranef = list(sp = tl2$tip.label),
                             family = "gaussian")
summary(pglmmSDrepgr2)

#logLik    AIC    BIC 
#-186.5  391.1  411.2

pglmmSDrepgrint <- phyr::pglmm(Among_SD_log ~ abs(Lat)*Group2 + (1 | sp_) + (1 | study_ID)+ (1 | Animal), 
                               data = reptiledata, 
                               cov_ranef = list(sp = tl2$tip.label),
                               family = "gaussian")
summary(pglmmSDrepgrint)

#logLik    AIC    BIC 
#-183.1  390.2  417.0 

### Grouped by laying eggs in the water or on land 
pglmmSDwater <- phyr::pglmm(Among_SD ~ Water + (1 | sp_) + (1 | study_ID)+ (1 | Animal), 
                            data = data, 
                            cov_ranef = list(sp = tl2$tip.label),
                            family = "gaussian")
summary(pglmmSDwater)

#logLik    AIC    BIC 
#-271.4  554.8  569.1

pglmmSDlatwat <- phyr::pglmm(Among_SD ~ abs(Lat)+Water-1 + (1 | sp_) + (1 | study_ID)+ (1 | Animal), 
                             data = data, 
                             cov_ranef = list(sp = tl2$tip.label),
                             family = "gaussian")
summary(pglmmSDlatwat)

#logLik    AIC    BIC 
#-264.4  542.7  559.5 


## latitude is significant, interaction is close

pglmmSD4 <- phyr::pglmm(Among_SD ~ abs(Lat)*Water + (1 | sp_) + (1 | study_ID)+ (1 | Animal), 
                        data = data, 
                        cov_ranef = list(sp = tl2$tip.label),
                        family = "gaussian")
summary(pglmmSD4)

#logLik    AIC    BIC 
#-261.8  539.6  558.7 

## Latitude seems to be the most important determinant of among nest SD






