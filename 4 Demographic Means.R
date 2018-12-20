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

plot6 <- ggplot(data = ldtab6, aes(x = label, y = avg, fill = alt.cons)) + 
  geom_bar(aes(y = avg), stat = "identity", position = position_dodge(), width = .75) + 
  geom_errorbar(aes(x = label, ymin = ymin, ymax = ymax), position = position_dodge(), width = .75) + #adds error bars
  ggtitle("Mean Ratings by Alternative Consumption Status") +
  labs(x = "Label", y = "Mean", fill = "") +
  theme_pander() + scale_colour_pander() +
  theme(legend.position = "top")
plot6

png('Ratings by Consumption Status.png', width = 640)
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

plot7 <- ggplot(data = ldtab7, aes(x = label, y = avg, fill = Region)) + 
  geom_bar(aes(y = avg), stat = "identity", position = position_dodge(), width = .8) + 
  geom_errorbar(aes(x = label, ymin = ymin, ymax = ymax), position = position_dodge(), width = .8) + #adds error bars
  ggtitle("Mean Ratings by Region") +
  labs(x = "Label", y = "Mean", fill = "") +
  theme_pander() + scale_colour_pander() +
  theme(legend.position = "top")
plot7

png('Ratings by Region.png', width = 640)
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

plot8 <- ggplot(data = ldtab8, aes(x = label, y = avg, fill = eth.race2)) + 
  geom_bar(aes(y = avg), stat = "identity", position = position_dodge(), width = .8) + 
  geom_errorbar(aes(x = label, ymin = ymin, ymax = ymax), position = position_dodge(), width = .8) + #adds error bars
  ggtitle("Mean Ratings by Race/Ethnicity") +
  labs(x = "Label", y = "Mean", fill = "") +
  theme_pander() + scale_colour_pander() +
  theme(legend.position = "top")
plot8

png('Ratings by Race-Ethnicity.png', width = 640)
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

plot9 <- ggplot(data = ldtab9, aes(x = label, y = avg, fill = income)) + 
  geom_bar(aes(y = avg), stat = "identity", position = position_dodge(), width = .8) + 
  geom_errorbar(aes(x = label, ymin = ymin, ymax = ymax), position = position_dodge(), width = .8) + #adds error bars
  ggtitle("Mean Ratings by Income") +
  labs(x = "Label", y = "Mean", fill = "") +
  theme_pander() + scale_colour_pander() +
  theme(legend.position = "top")
plot9

png('Ratings by Income.png', width = 640)
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

plot10 <- ggplot(data = ldtab10, aes(x = label, y = avg, fill = AgeGroup)) + 
  geom_bar(aes(y = avg), stat = "identity", position = position_dodge(), width = .8) + 
  geom_errorbar(aes(x = label, ymin = ymin, ymax = ymax), position = position_dodge(), width = .8) + #adds error bars
  ggtitle("Mean Ratings by Age Group") +
  labs(x = "Label", y = "Mean", fill = "") +
  theme_pander() + scale_colour_pander() +
  theme(legend.position = "top")
plot10

png('Ratings by Age Group.png', width = 640)
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

plot11 <- ggplot(filter(ldtab11, gender1 != "Other"), aes(x = label, y = avg, fill = gender1)) + 
  geom_bar(aes(y = avg), stat = "identity", position = position_dodge(), width = .8) + 
  geom_errorbar(aes(x = label, ymin = ymin, ymax = ymax), position = position_dodge(), width = .8) + #adds error bars
  ggtitle("Mean Ratings by Gender") +
  labs(x = "Label", y = "Mean", fill = "") +
  theme_pander() + scale_colour_pander() +
  theme(legend.position = "top")
plot11

png('Ratings by Gender.png', width = 640)
plot11
dev.off()


#########################################

###QUESTION 4: Overall, how are the eight labels perceived in terms of how good they are for health, the environment, 
#animal welfare, and taste?

#Create summary tables for rating variables by label
##Health
ldtab2 <- ddply(ldata, "label", summarise,
                N = length(health),
                mean = mean(health),
                sd = sd(health),
                se = sd/sqrt(N),
                ymin = mean - 1.96*se,
                ymax = mean + 1.96*se)
ldtab2$label <- factor(ldtab2$label, labels = c("Vegan", "Plant-Based", "Feel-Good", "Zero Chol.", "Kojo", "Dir. Protein",
                                                "Clean", "Planet Fr."))
ldtab2$label <- factor(ldtab2$label, levels = ldtab2$label[order(ldtab2$mean*-1)]) #order highest to lowest on animal-friendly

plot2 <- ggplot(data = ldtab2, aes(label, mean)) + 
  geom_bar(stat = "identity") + #for discrete X, continuous Y
  coord_cartesian(ylim = c(3.5,4.5)) + #changes y axis without actually removing unseen data points
  geom_errorbar(aes(x = label, ymin = ymin, ymax = ymax, width = .3)) + #adds error bars
  ggtitle("Mean Health Rating by Label") +
  xlab("Label") + 
  ylab ("Mean") +
  theme_pander() + scale_colour_pander()
plot2

png('Health by Label.png', width = 640)
plot2
dev.off()

##Animal-Friendliness

ldtab3 <- ddply(ldata, "label", summarise,
                N = length(animals),
                mean = mean(animals),
                sd = sd(animals),
                se = sd/sqrt(N),
                ymin = mean - 1.96*se,
                ymax = mean + 1.96*se)
ldtab3$label <- factor(ldtab3$label, labels = c("Vegan", "Plant-Based", "Feel-Good", "Zero Chol.", "Kojo", "Dir. Protein",
                                                "Clean", "Planet Fr."))
ldtab3$label <- factor(ldtab3$label, levels = ldtab3$label[order(ldtab3$mean*-1)]) 

plot3 <- ggplot(data = ldtab3, aes(label, mean)) + 
  geom_bar(stat = "identity") + #for discrete X, continuous Y
  coord_cartesian(ylim = c(3.5,4.5)) + #changes y axis without actually removing unseen data points
  geom_errorbar(aes(x = label, ymin = ymin, ymax = ymax, width = .3)) + #adds error bars
  ggtitle("Mean Animal-Friendliness Rating by Label") +
  xlab("Label") + 
  ylab ("Mean") +
  theme_pander() + scale_colour_pander()
plot3

png('Animal-Friendliness by Label.png', width = 640)
plot3
dev.off()

##Environment

ldtab4 <- ddply(ldata, "label", summarise,
                N = length(env),
                mean = mean(env),
                sd = sd(env),
                se = sd/sqrt(N),
                ymin = mean - 1.96*se,
                ymax = mean + 1.96*se)
ldtab4$label <- factor(ldtab4$label, labels = c("Vegan", "Plant-Based", "Feel-Good", "Zero Chol.", "Kojo", "Dir. Protein",
                                                "Clean", "Planet Fr."))
ldtab4$label <- factor(ldtab4$label, levels = ldtab4$label[order(ldtab4$mean*-1)]) 

plot4 <- ggplot(data = ldtab4, aes(label, mean)) + 
  geom_bar(stat = "identity") + #for discrete X, continuous Y
  coord_cartesian(ylim = c(3.5,4.5)) + #changes y axis without actually removing unseen data points
  geom_errorbar(aes(x = label, ymin = ymin, ymax = ymax, width = .3)) + #adds error bars
  ggtitle("Mean Environmental Rating by Label") +
  xlab("Label") + 
  ylab ("Mean") +
  theme_pander() + scale_colour_pander()
plot4

png('Environmental Rating by Label.png', width = 640)
plot4
dev.off()

##Taste

ldtab5 <- ddply(ldata, "label", summarise,
                N = length(taste),
                mean = mean(taste),
                sd = sd(taste),
                se = sd/sqrt(N),
                ymin = mean - 1.96*se,
                ymax = mean + 1.96*se)
ldtab5$label <- factor(ldtab5$label, labels = c("Vegan", "Plant-Based", "Feel-Good", "Zero Chol.", "Kojo", "Dir. Protein",
                                                "Clean", "Planet Fr."))

plot5 <- ggplot(data = ldtab5, aes(label, mean)) + 
  geom_bar(stat = "identity") + #for discrete X, continuous Y
  coord_cartesian(ylim = c(3,4)) + #changes y axis without actually removing unseen data points
  geom_errorbar(aes(x = label, ymin = ymin, ymax = ymax, width = .3)) + #adds error bars
  ggtitle("Mean Taste Rating by Label") +
  xlab("Label") + 
  ylab ("Mean") +
  theme_pander() + scale_colour_pander()
plot5

png('Taste by Label.png', width = 640)
plot5
dev.off()


#########################################

#AGE X GENDER

tab2a <- filter(ldata, label == "vegan" & ldata$gender1 != "Other") %>% 
  group_by(AgeGroup, gender1) %>% 
  summarise(avg = mean(label.score), 
            N = length(label.score), 
            sd = sd(label.score), 
            se = sd/sqrt(N),
            ymin = avg - 1.96*se, #confidence interval lower
            ymax = avg + 1.96*se) #confidence interval upper
tab2a

#Plot means by category: Age x Gender
bar2a <- ggplot(data = tab2a, aes(x = AgeGroup, y = avg, fill = gender1)) + 
  geom_bar(aes(y = avg), stat = "identity", position = "dodge") + 
  geom_errorbar(aes(x = AgeGroup, ymin = ymin, ymax = ymax), position = position_dodge()) + #adds error bars
  ggtitle("Mean Vegan Ratings by Age and Gender") +
  labs (x = "Age", y = "Mean", fill="Gender", group = "Gender") + 
  theme_pander() + scale_colour_pander()
bar2a

png('Mean Vegan Ratings by Age and Gender.png', width = 640)
bar2a
dev.off()

tab2b <- filter(ldata, label == "pb" & ldata$gender1 != "Other") %>% 
  group_by(AgeGroup, gender1) %>% 
  summarise(avg = mean(label.score), 
            N = length(label.score),
            sd = sd(label.score), 
            se = sd/sqrt(N),
            ymin = avg - 1.96*se, #confidence interval lower
            ymax = avg + 1.96*se) #confidence interval upper
tab2b

bar2b <- ggplot(data = tab2b, aes(x = AgeGroup, y = avg, fill = gender1)) + 
  geom_bar(aes(y = avg), stat = "identity", position = "dodge") + 
  geom_errorbar(aes(x = AgeGroup, ymin = ymin, ymax = ymax), position = position_dodge()) + #adds error bars
  ggtitle("Mean Plant-Based Ratings by Age and Gender") +
  labs (x = "Age", y = "Mean", fill="Gender") + 
  theme_pander() + scale_colour_pander()
bar2b

png('Mean Plant-Based Ratings by Age and Gender.png', width = 640)
bar2b
dev.off()

tab2c <- filter(ldata, label == "dp" & ldata$gender1 != "Other") %>% 
  group_by(AgeGroup, gender1) %>% 
  summarise(avg = mean(label.score), 
            N = length(label.score),
            sd = sd(label.score), 
            se = sd/sqrt(N),
            ymin = avg - 1.96*se, #confidence interval lower
            ymax = avg + 1.96*se) #confidence interval upper
tab2c

bar2c <- ggplot(data = tab2c, aes(x = AgeGroup, y = avg, fill = gender1)) + 
  geom_bar(aes(y = avg), stat = "identity", position = "dodge") + 
  geom_errorbar(aes(x = AgeGroup, ymin = ymin, ymax = ymax), position = position_dodge()) + #adds error bars
  ggtitle("Mean Direct Protein Ratings by Age and Gender") +
  labs (x = "Age", y = "Mean", fill="Gender") + 
  theme_pander() + scale_colour_pander()
bar2c

png('Mean Direct Protein Ratings by Age and Gender.png', width = 640)
bar2c
dev.off()