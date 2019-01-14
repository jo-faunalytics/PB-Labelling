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
library("Cairo")

#Load data
load('PB Labelling Data - wide cleaned.Rdata')

#REMOVE VEG*NS
data <- data[data$vegn == "non-vegn", ] #keeps only non-vegns
str(data)


###Who prefers vegan over plant-based?
#Create new dataframe
vegan.pb.freq <- count(data, vegan.pb.rec)
vegan.pb.freq$pct <- vegan.pb.freq$n/sum(vegan.pb.freq$n)
vegan.pb.freq$vegan.pref <- ifelse(vegan.pb.freq$vegan.pb.rec < 0, 1, 0)
vegan.pb.freq$vegan.pb.rec <- factor(vegan.pb.freq$vegan.pb.rec, labels = c("Vegan sounds much better than plant-based", 
                                                                 "Vegan sounds a bit better than plant-based",
                                                                 "They sound about equal",
                                                                 "Plant-based sounds a bit better than vegan",
                                                                 "Plant-based sounds much better than vegan"))
vegan.pb.freq


# Create a basic bar
pie = ggplot(vegan.pb.freq, aes(x="", y=pct, fill=as.factor(vegan.pb.rec))) + geom_bar(stat="identity", width=1)
pie

# Convert to pie (polar coordinates) and add labels
pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(pct*100), "%")), position = position_stack(vjust = 0.5))
pie

# Add color scale (hex colors)
pie = pie + scale_fill_manual(values=c("#0099ff", "#80ccff", "#c73333", "#a3f5be", "#009933")) 
pie

# Remove axis labels
pie = pie + labs(x = NULL, y = NULL, fill = NULL)
pie

# Tidy up the theme
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank())
pie

Cairo(width = 640, height = 480, file = "Pie Chart - Vegan vs PB.png", type = "png", bg = "white")
pie
dev.off()
