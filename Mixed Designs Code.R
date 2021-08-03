###################
###################
# Clear workspace #
###################
###################

# Clearing the workspace removes all current loaded data, functions and so on

rm(list = ls())

###################
###################
#### Libraries ####
###################
###################

library(easypackages) # enables the libraries function
suppressPackageStartupMessages(
  libraries("reshape",
            "ez", # for ezANOVA
            "nlme",
            "multcomp", # for posthoc tests
            "WRS2", # for robust repeated measures ANOVA
            "ggplot2",
            "pastecs" # for stat.desc()
  ))

#############
# Functions #
#############

# function to calculate r contrast
rcontrast <- function(t, df){
  r <- sqrt(t^2/(t^2 + df))
  print(paste("r = ", r))
}

#############
#############
# Load Data #
#############
#############

setwd("C:/R Portfolio/Mixed Designs")

dateData <- read.delim("LooksOrPersonality.dat", header = T)

###################
###################
# Data Formatting #
###################
###################

# R handles repeated measures designs in long format data, so melt() is used #

speedData <- melt(dateData, id = c("participant","gender"), 
                  measured = c("att_high", "av_high", "ug_high", "att_some", "av_some", "ug_some", "att_none", "av_none", "ug_none"))

# Rename columns #

names(speedData) <- c("participant", "gender", "groups", "dateRating")

# Create personality variable #

speedData$personality <- gl(3, 60, 
                            labels = c("Charismatic", "Average", "Dullard"))

# create looks variable #

speedData$looks <- gl(3,20, 180, labels = c("Attractive", "Average", "Ugly"))

# Data edited and ordered by participant #

speedData <- speedData[order(speedData$participant),]
head(speedData)

####################
####################
# Data Exploration #
####################
####################

dateBoxplot <- ggplot(speedData, aes(looks, dateRating, colour = personality))
dateBoxplot + geom_boxplot() + labs(x = "Attractiveness", 
                                    y = "Mean Rating of Date", 
                                    colour = "Charisma") + 
  facet_wrap( ~ gender)
imageFile <- paste(imageDirectory, "14 Speed Date Boxplot.png", sep = "/")
ggsave(file = imageFile)


looksBar <- ggplot(speedData, aes(looks, dateRating))
looksBar + stat_summary(fun = mean, geom = "bar", 
                        fill = "White", colour = "Black") + 
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + 
  labs(x = "Attractiveness", y = "Mean Rating of Date") 
imageFile <- paste(imageDirectory,"14 Speed Date Looks.png", sep = "/")
ggsave(file = imageFile)


charismaBar <- ggplot(speedData, aes(personality, dateRating))
charismaBar + stat_summary(fun = mean, geom = "bar", fill = "White", 
                           colour = "Black") + 
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + 
  labs(x = "Charisma", y = "Mean Rating of Date") 
imageFile <- paste(imageDirectory, "14 Speed Date Charisma.png", sep = "/")
ggsave(file = imageFile) 

genderBar <- ggplot(speedData, aes(gender, dateRating))
genderBar + stat_summary(fun = mean, geom = "bar", 
                         fill = "White", 
                         colour = "Black") + 
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + 
  labs(x = "Gender", y = "Mean Rating of Date") 
imageFile <- paste(imageDirectory, "14 Speed Date Gender.png", sep = "/")
ggsave(file = imageFile)

genderLooks <- ggplot(speedData, 
                      aes(looks, dateRating, colour = gender))
genderLooks + stat_summary(fun = mean, geom = "point") + 
  stat_summary(fun = mean, geom = "line", aes(group = gender)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(x = "Attractiveness", y = "Mean Rating of Date", colour = "Gender") + scale_y_continuous(limits = c(0, 100)) 
imageFile <- paste(imageDirectory,"14 looks * gender.png", sep = "/")
ggsave(file = imageFile)

genderCharisma <- ggplot(speedData, aes(personality, dateRating, colour = gender))
genderCharisma + stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun = mean, geom = "line", aes(group= gender)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(x = "Charisma", y = "Mean Rating of Date", colour = "Gender") + scale_y_continuous(limits = c(0,100)) 
imageFile <- paste(imageDirectory, "14 personality * gender.png", sep = "/")
ggsave(file = imageFile)

looksCharisma <- ggplot(speedData, aes(looks, dateRating, colour = personality))
looksCharisma + stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun = mean, geom = "line", 
               aes(group = personality)) + 
  stat_summary(fun.data = mean_cl_boot, 
               geom = "errorbar", width = 0.2) + 
  labs(x = "Attractiveness", y = "Mean Rating of Date", colour = "Charisma") + scale_y_continuous(limits = c(0,100)) 
imageFile <- paste(imageDirectory,"14 personality * looks.png",sep="/")
ggsave(file = imageFile)

looksCharismaGender <- ggplot(speedData, aes(looks, dateRating, colour = personality))
looksCharismaGender + stat_summary(fun = mean, geom = "point") + stat_summary(fun = mean, geom = "line", aes(group= personality)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Attractiveness", y = "Mean Rating of Date", colour = "Charisma") + scale_y_continuous(limits = c(0,100)) + facet_wrap(~ gender)
imageFile <- paste(imageDirectory,"14 three way interaction.png",sep="/")
ggsave(file = imageFile)

options(digits = 3)
by(speedData$dateRating, speedData$looks, stat.desc, basic = FALSE)
by(speedData$dateRating, speedData$personality, stat.desc, basic = FALSE)
by(speedData$dateRating, speedData$gender, stat.desc, basic = FALSE)
by(speedData$dateRating, list(speedData$looks, speedData$gender), stat.desc, basic = FALSE)
by(speedData$dateRating, list(speedData$personality, speedData$gender), stat.desc, basic = FALSE)
by(speedData$dateRating, list(speedData$looks, speedData$personality), stat.desc, basic = FALSE)
by(speedData$dateRating, list(speedData$looks, speedData$personality, speedData$gender), stat.desc, basic = FALSE)
options(digits = 7)

###############
###############
# Mixed Anova #
###############
###############

# Contrasts need to be set first to enable comparisons to be made

# Personality #

# here, we can compared average and charasmatic with dullard and then charismatic with average

SomevsNone <- c(1, 1, -2)
HivsAv <- c(1, -1, 0)
contrasts(speedData$personality) <- cbind(SomevsNone, HivsAv)

# Looks #

# we can compare attractive and average to ugly and then attractive to average.

AttractivevsUgly <- c(1, 1, -2)
AttractvsAv <- c(1, -1, 0)
contrasts(speedData$looks) <- cbind(AttractivevsUgly, AttractvsAv)

options(digits = 3)
speedModel <- ezANOVA(data = speedData, 
                      dv = .(dateRating), 
                      wid = .(participant),  
                      between = .(gender), 
                      within = .(looks, personality), 
                      type = 3, # type 3 sum of squares
                      detailed = T)
speedModel

# If we look at the Mauchly Test results, we see that none violate the assumption of sphericity. 
# In the anova table results, gender has a none significant effect, so if we ignore attractiveness and charisma levels in the data, male and female participants did not differ in the ratings they gave.
# The significant effect of looks means that if we ignore whether the date was charismatic, and whether the rating was from a man or woman, the attractiveness of a person significantly affected the ratings they received.
# The looks * gender interaction is also significant, which means that although the ratings were affected by whether the date was attractive, average or ugly, the way in which ratings were affected by attractiveness was different in male and female raters.
# Personality is also important which means that if we ignore whether the date was attractive, and whether the rating was from a man or woman, then the charisma significantly affected the rating they received
# The personality * gender interaction is also significant, which means that charisma effect differed in male and female raters
# The significant interaction between looks and personality means that if the gender of the rater is ignored, the profile of ratings across different levels of attractiveness differed for highly charismatic dates, charismatic dates and dullards. It also means that profile of ratings across different levels of charisma was different for attractive, average and ugly dates.
# The looks * personality * gender interaction is also significant which means that looks * personality interaction was significantly different in male and female participants.

##########################
##########################
# Mixed Designs as a GLM #
##########################
##########################

# Setting Contrasts #

# Looks variable #

# There are three conditions, namely attractive, average and ugly, so it makes sense to compare attractive and ugly conditions to average as this represents the norm.

AttractivevsAv <- c(1, 0, 0)
UglyvsAv <- c(0, 0, 1)
contrasts(speedData$looks) <- cbind(AttractivevsAv, UglyvsAv)
speedData$looks

# Personality Variable #

HighvsAv <- c(1, 0, 0)
DullvsAv <- c(0, 0, 1)
contrasts(speedData$personality) <- cbind(HighvsAv, DullvsAv)
speedData$personality

# Gender Variable #

# There is no need to set one, as there are only two levels, male and female, so a explicit contrast is not needed.

######################
######################
# Building the Model #
######################
######################

# A baseline model is needed with just the intercept and no predictors.
# In such models, sphericity does not apply

baseline <- lme(dateRating ~ 1, 
                random = ~ 1|participant/looks/personality, 
                data = speedData, 
                method = "ML")

# Add all other predictors

speedDateModel <- update(baseline, .~. + looks + personality + gender + 
                           looks:gender + looks:personality + 
                           looks:personality:gender)


# compare these models

anova(baseline, speedDateModel)

summary(speedDateModel)

##########################
##########################
# Calculate Effect Sizes #
##########################
##########################

rcontrast(-1.20802, 108)
rcontrast(3.85315, 108)
rcontrast(-7.53968, 108)
rcontrast(-0.97891, 108)

#####################################
#####################################
# Robust Analysis for Mixed Designs #
#####################################
#####################################

# There isnt a function to analyse a three-way mixed design like the previous example, so we use a two way design instead
#  sppba, sppbb, and sppbi for the between-subjects effect, the within-subjects
# effect, and the interaction effect, respectively

bwtrim(dateRating ~ gender*personality, id = participant, data = speedData, tr = .2)
sppba(dateRating ~ gender*personality, participant, data = speedData, est = "mom", nboot = 500)
sppbb(dateRating ~ looks*personality, participant, data = speedData, est = "mom", nboot = 500)
sppbi(dateRating ~ looks*personality, participant, data = speedData, est = "mom", nboot = 500)
