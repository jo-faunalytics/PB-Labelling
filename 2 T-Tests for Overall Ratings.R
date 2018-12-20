#Set working directory
setwd("C:/Users/Jo/Documents/Sync/Faunalytics/A - PB Labelling/Stage 3/Data")

#Load library
library("psych")
library("Hmisc")
library("ggthemes")
library("broom")
library("tidyverse")

#Load data
load('PB Labelling Data - long cleaned.Rdata')


#REMOVE VEG*NS
ldata <- ldata[ldata$vegn == "non-vegn", ] #keeps only non-vegns
str(ldata)

#########################################

###QUESTION 1: What are U.S. meat consumers' relative preferences for the labels?

#Create summary table for label.score by label
ldtab1 <- ldata %>% 
  group_by(label) %>% 
  summarise(mean = mean(label.score), 
            N = length(label.score), 
            sd = sd(label.score), 
            se = sd/sqrt(N),
            ymin = mean - 1.96*se, #confidence interval lower
            ymax = mean + 1.96*se) #confidence interval upper
ldtab1$label <- factor(ldtab1$label, labels = c("Feel-Good", "Vegan", "Clean", "Kojo", "Dir. Protein", "Planet Fr.", "Zero Chol.",
                                                "Plant-Based"))
ldtab1


#PAIRED T-TESTS

#Sort by group then ID. If you are using long-format data with a grouping variable, the first row with 
#group=1 is paired with the first row with group=2. It is important to make sure that the data is sorted
#and there are not missing observations; otherwise the pairing can be thrown off.
ldata <- ldata[order(ldata$label, ldata$ResponseId), ]

#H1a (note that direction of effects is not meaningful b/c it's determined by order in file)
tidy(t.test(label.score ~ label, ldata[ldata$label == "vegan" | ldata$label == "pb", ], paired=TRUE))
tidy(t.test(label.score ~ label, ldata[ldata$label == "vegan" | ldata$label == "fg", ], paired=TRUE))
tidy(t.test(label.score ~ label, ldata[ldata$label == "vegan" | ldata$label == "zc", ], paired=TRUE))
tidy(t.test(label.score ~ label, ldata[ldata$label == "vegan" | ldata$label == "kojo", ], paired=TRUE))
tidy(t.test(label.score ~ label, ldata[ldata$label == "vegan" | ldata$label == "dp", ], paired=TRUE))
tidy(t.test(label.score ~ label, ldata[ldata$label == "vegan" | ldata$label == "clean", ], paired=TRUE))
tidy(t.test(label.score ~ label, ldata[ldata$label == "vegan" | ldata$label == "pf", ], paired=TRUE))

#H2a
tidy(t.test(label.score ~ label, ldata[ldata$label == "pb" | ldata$label == "vegan", ], paired=TRUE))
tidy(t.test(label.score ~ label, ldata[ldata$label == "pb" | ldata$label == "fg", ], paired=TRUE))
tidy(t.test(label.score ~ label, ldata[ldata$label == "pb" | ldata$label == "zc", ], paired=TRUE))
tidy(t.test(label.score ~ label, ldata[ldata$label == "pb" | ldata$label == "kojo", ], paired=TRUE))
tidy(t.test(label.score ~ label, ldata[ldata$label == "pb" | ldata$label == "dp", ], paired=TRUE))
tidy(t.test(label.score ~ label, ldata[ldata$label == "pb" | ldata$label == "clean", ], paired=TRUE))
tidy(t.test(label.score ~ label, ldata[ldata$label == "pb" | ldata$label == "pf", ], paired=TRUE))

#Follow-up on adjacent labels
tidy(t.test(label.score ~ label, ldata[ldata$label == "fg" | ldata$label == "vegan", ], paired=TRUE))
tidy(t.test(label.score ~ label, ldata[ldata$label == "vegan" | ldata$label == "clean", ], paired=TRUE))
tidy(t.test(label.score ~ label, ldata[ldata$label == "clean" | ldata$label == "kojo", ], paired=TRUE))
tidy(t.test(label.score ~ label, ldata[ldata$label == "kojo" | ldata$label == "dp", ], paired=TRUE))
tidy(t.test(label.score ~ label, ldata[ldata$label == "dp" | ldata$label == "pf", ], paired=TRUE))
tidy(t.test(label.score ~ label, ldata[ldata$label == "pf" | ldata$label == "zc", ], paired=TRUE))
tidy(t.test(label.score ~ label, ldata[ldata$label == "zc" | ldata$label == "pb", ], paired=TRUE))


###PLOT

#x = label, y = label.score
plot1 <- ggplot(data = ldtab1, aes(label, mean)) + 
  geom_bar(stat = "identity") + #for discrete X, continuous Y
  geom_errorbar(aes(x = label, ymin = ymin, ymax = ymax, width = .3)) + #adds error bars
  ggtitle("Overall Means by Label") +
  xlab("Label") + 
  ylab ("Mean") +
  theme_pander() + scale_colour_pander()
plot1

png('Label Means.png', width = 640)
plot1
dev.off()
  

