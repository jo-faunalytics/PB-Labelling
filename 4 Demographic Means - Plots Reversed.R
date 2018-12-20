#Set working directory
setwd("C:/Users/Jo/Documents/Sync/Faunalytics/A - PB Labelling/Stage 3/Data")

#Load library
library("psych")
library("Hmisc")
library("ggthemes")
library("ggplot")
library("tidyverse")

#Load data
load('PB Labelling Data - long cleaned.Rdata')


#REMOVE VEG*NS
ldata <- ldata[ldata$vegn == "non-vegn", ] #keeps only non-vegns
str(ldata)


#########################################
#CONSUMPTION STATUS

#Create summary table
ldtab6 <- group_by(ldata, label, alt.cons) %>% 
  summarise(avg = mean(label.score), 
            N = length(label.score), 
            sd = sd(label.score), 
            se = sd/sqrt(N),
            ymin = avg - 1.96*se, #confidence interval lower
            ymax = avg + 1.96*se) #confidence interval upper
ldtab6

ldtab6$alt.cons <- factor(ldtab6$alt.cons, labels = c("Does not eat alternatives", "Eats alternatives"))
ldtab6$label <- factor(ldtab6$label, labels = c("Feel-Good", "Vegan", "Clean", "Kojo", "Dir. Protein", "Planet Fr.", "Zero Chol.",
                                                "Plant-Based"))
ldtab6

plot6 <- ggplot(data = ldtab6, aes(x = alt.cons, y = avg, fill = label)) + 
  geom_bar(aes(y = avg), stat = "identity", position = position_dodge(), width = .75) + 
  geom_errorbar(aes(x = alt.cons, ymin = ymin, ymax = ymax), position = position_dodge(), width = .75) + #adds error bars
  ggtitle("Mean Ratings by Alternative Consumption Status") +
  labs(x = "", y = "Mean", fill = "") +
  theme_pander() + scale_colour_pander() +
  theme(legend.position = "right")
plot6

png('Ratings by Consumption Status - rev.png', width = 640)
plot6
dev.off()


#########################################
#REGION

#Create summary table
ldtab7 <- group_by(ldata[!is.na(ldata$Region),], label, Region) %>% 
  summarise(avg = mean(label.score), 
            N = length(label.score), 
            sd = sd(label.score), 
            se = sd/sqrt(N),
            ymin = avg - 1.96*se, #confidence interval lower
            ymax = avg + 1.96*se) #confidence interval upper
ldtab7

ldtab7$label <- factor(ldtab7$label, labels = c("Feel-Good", "Vegan", "Clean", "Kojo", "Dir. Protein", "Planet Fr.", "Zero Chol.",
                                                "Plant-Based"))
ldtab7

plot7 <- ggplot(data = ldtab7, aes(x = Region, y = avg, fill = label)) + 
  geom_bar(aes(y = avg), stat = "identity", position = position_dodge(), width = .8) + 
  geom_errorbar(aes(x = Region, ymin = ymin, ymax = ymax), position = position_dodge(), width = .8) + #adds error bars
  ggtitle("Mean Ratings by Region") +
  labs(x = "", y = "Mean", fill = "") +
  theme_pander() + scale_colour_pander() +
  theme(legend.position = "right")
plot7

png('Ratings by Region - rev.png', width = 640)
plot7
dev.off()


#########################################
#ETHNICITY

#Create summary table
ldtab8 <- group_by(ldata, label, eth.race2) %>% 
  summarise(avg = mean(label.score), 
            N = length(label.score), 
            sd = sd(label.score), 
            se = sd/sqrt(N),
            ymin = avg - 1.96*se, #confidence interval lower
            ymax = avg + 1.96*se) #confidence interval upper
ldtab8

ldtab8$label <- factor(ldtab8$label, labels = c("Feel-Good", "Vegan", "Clean", "Kojo", "Dir. Protein", "Planet Fr.", "Zero Chol.",
                                                "Plant-Based"))
ldtab8

plot8 <- ggplot(data = ldtab8, aes(x = eth.race2, y = avg, fill = label)) + 
  geom_bar(aes(y = avg), stat = "identity", position = position_dodge(), width = .8) + 
  geom_errorbar(aes(x = eth.race2, ymin = ymin, ymax = ymax), position = position_dodge(), width = .8) + #adds error bars
  ggtitle("Mean Ratings by Race/Ethnicity") +
  labs(x = "", y = "Mean", fill = "") +
  theme_pander() + scale_colour_pander() +
  theme(legend.position = "right")
plot8

png('Ratings by Race-Ethnicity - rev.png', width = 640)
plot8
dev.off()

#########################################
#INCOME

#Create summary table
ldtab9 <- group_by(ldata, label, income) %>% 
  summarise(avg = mean(label.score), 
            N = length(label.score), 
            sd = sd(label.score), 
            se = sd/sqrt(N),
            ymin = avg - 1.96*se, #confidence interval lower
            ymax = avg + 1.96*se) #confidence interval upper
ldtab9

ldtab9$label <- factor(ldtab9$label, labels = c("Feel-Good", "Vegan", "Clean", "Kojo", "Dir. Protein", "Planet Fr.", "Zero Chol.",
                                                "Plant-Based"))
ldtab9

plot9 <- ggplot(data = ldtab9, aes(x = income, y = avg, fill = label)) + 
  geom_bar(aes(y = avg), stat = "identity", position = position_dodge(), width = .8) + 
  geom_errorbar(aes(x = income, ymin = ymin, ymax = ymax), position = position_dodge(), width = .8) + #adds error bars
  ggtitle("Mean Ratings by Income") +
  labs(x = "", y = "Mean", fill = "") +
  theme_pander() + scale_colour_pander() +
  theme(legend.position = "right")
plot9

png('Ratings by Income - rev.png', width = 640)
plot9
dev.off()

#########################################
#AGE
str(ldata)

#Create summary table
ldtab10 <- group_by(ldata, label, AgeGroup) %>% 
  summarise(avg = mean(label.score), 
            N = length(label.score), 
            sd = sd(label.score), 
            se = sd/sqrt(N),
            ymin = avg - 1.96*se, #confidence interval lower
            ymax = avg + 1.96*se) #confidence interval upper
ldtab10

ldtab10$label <- factor(ldtab10$label, labels = c("Feel-Good", "Vegan", "Clean", "Kojo", "Dir. Protein", "Planet Fr.", "Zero Chol.",
                                                "Plant-Based"))
ldtab10

plot10 <- ggplot(data = ldtab10, aes(x = AgeGroup, y = avg, fill = label)) + 
  geom_bar(aes(y = avg), stat = "identity", position = position_dodge(), width = .8) + 
  geom_errorbar(aes(x = AgeGroup, ymin = ymin, ymax = ymax), position = position_dodge(), width = .8) + #adds error bars
  ggtitle("Mean Ratings by Age Group") +
  labs(x = "", y = "Mean", fill = "") +
  theme_pander() + scale_colour_pander() +
  theme(legend.position = "right")
plot10

png('Ratings by Age Group - rev.png', width = 640)
plot10
dev.off()

#########################################
#GENDER

#Create summary table
ldtab11 <- group_by(ldata, label, gender1) %>% 
  summarise(avg = mean(label.score), 
            N = length(label.score), 
            sd = sd(label.score), 
            se = sd/sqrt(N),
            ymin = avg - 1.96*se, #confidence interval lower
            ymax = avg + 1.96*se) #confidence interval upper
ldtab11

ldtab11$label <- factor(ldtab11$label, labels = c("Feel-Good", "Vegan", "Clean", "Kojo", "Dir. Protein", "Planet Fr.", "Zero Chol.",
                                                  "Plant-Based"))
ldtab11

plot11 <- ggplot(filter(ldtab11, gender1 != "Other"), aes(x = gender1, y = avg, fill = label)) + 
  geom_bar(aes(y = avg), stat = "identity", position = position_dodge(), width = .8) + 
  geom_errorbar(aes(x = gender1, ymin = ymin, ymax = ymax), position = position_dodge(), width = .8) + #adds error bars
  ggtitle("Mean Ratings by Gender") +
  labs(x = "", y = "Mean", fill = "") +
  theme_pander() + scale_colour_pander() +
  theme(legend.position = "right")
plot11

png('Ratings by Gender - rev.png', width = 640)
plot11
dev.off()

#########################################
#AGE X GENDER SIMPLIFIED

describe(ldata$AgeGroup)
describe(ldata$gender1)

ELSE <- TRUE #temp variable that just provides the last case for the case_when
ldata <- ldata %>%
  mutate(., GenderAgeGroup = with(., case_when(  #dots refer back to 'data' before the '%>%'
    (gender1 == "Woman" & (AgeGroup == "18-24" | AgeGroup == "25-34")) ~ "Woman 18-34", 
    (gender1 == "Woman" & (AgeGroup == "35-44" | AgeGroup == "45-54")) ~ "Woman 35-54", 
    (gender1 == "Woman" & (AgeGroup == "55-64" | AgeGroup == "65+")) ~ "Woman 55+",
    (gender1 == "Man" & (AgeGroup == "18-24" | AgeGroup == "25-34")) ~ "Man 18-34", 
    (gender1 == "Man" & (AgeGroup == "35-44" | AgeGroup == "45-54")) ~ "Man 35-54", 
    (gender1 == "Man" & (AgeGroup == "55-64" | AgeGroup == "65+")) ~ "Man 55+",
    ELSE ~ "Other"))) 
remove(ELSE)  #delete the dummy variable
describe(ldata$GenderAgeGroup)  #check categories

#Create summary table
ldtab12 <- group_by(filter(ldata, GenderAgeGroup != "Other"), label, GenderAgeGroup) %>% 
  summarise(avg = mean(label.score), 
            N = length(label.score), 
            sd = sd(label.score), 
            se = sd/sqrt(N),
            ymin = avg - 1.96*se, #confidence interval lower
            ymax = avg + 1.96*se) #confidence interval upper
ldtab12

ldtab12$label <- factor(ldtab12$label, labels = c("Feel-Good", "Vegan", "Clean", "Kojo", "Dir. Protein", "Planet Fr.", "Zero Chol.",
                                                  "Plant-Based"))
ldtab12

plot12 <- ggplot(ldtab12, aes(x = GenderAgeGroup, y = avg, fill = label)) + 
  geom_bar(aes(y = avg), stat = "identity", position = position_dodge(), width = .8) + 
  geom_errorbar(aes(x = GenderAgeGroup, ymin = ymin, ymax = ymax), position = position_dodge(), width = .8) + #adds error bars
  ggtitle("Mean Ratings by Age and Gender") +
  labs(x = "", y = "Mean", fill = "") +
  theme_pander() + scale_colour_pander() +
  theme(legend.position = "right")
plot12

png('Ratings by Age and Gender - rev.png', width = 720)
plot12
dev.off()

