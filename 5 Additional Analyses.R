#Set working directory
setwd("C:/Users/Jo/Documents/Sync/Faunalytics/A - PB Labelling/Stage 3/Data")

#Load library
library("psych")
library("Hmisc")
library("ggthemes")
library("ggplot2")
library("tidyverse")
library("broom")
library("stargazer")

#Load data
load('PB Labelling Data - wide cleaned.Rdata')

#REMOVE VEG*NS
data <- data[data$vegn == "non-vegn", ] #keeps only non-vegns
str(data)


###Who prefers vegan over plant-based?
vegan.pb.freq <- count(data, vegan.pb.rec)
vegan.pb.freq$pct <- vegan.pb.freq$n/sum(vegan.pb.freq$n)
vegan.pb.freq$vegan.pref <- ifelse(vegan.pb.freq$vegan.pb.rec < 0, 1, 0)
vegan.pb.freq

data$vegan.pref <- ifelse(data$vegan.pb.rec < 0, 1, 0)
chi1 <- chiSquare(data$vegan.pref ~ data$eth.race2)
pairwise.prop.test(chi1)

pie1 <- ggplot(vegan.pb.freq, aes(x="", y=pct, fill=vegan.pb.rec)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar ("y", start = 0)
pie1
