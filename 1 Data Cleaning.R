#Set working directory
setwd("C:/Users/Jo/Documents/Sync/Faunalytics/A - PB Labelling/Stage 3/Data")

#Load library
library("psych")
library("tidyverse")

#Load data
data <- read.csv("PB Labelling Data - deduped.csv", header=TRUE, stringsAsFactors=FALSE)

#Delete unnecessary variables from Qualtrics dataset
names(data) #show all column names, note ones to remove
data <- data[, -c(3:5, 7:8, 10:20, 22, 149, 150, 152, 178, 179, 181)] #remove unnecessary columns
names(data) #check

#EXCLUSIONS
data <- filter(data, is.na(Exclude)) #keeps only those without an Exclude flag (from panel company going over quota)

#Attention checks
#get frequencies of pass and fail
describe(as.factor(data$check1))
describe(as.factor(data$check2))

data <- filter(data, !is.na(Q119_Page.Submit)) #keeps only those who completed the survey

#Location check
table(as.factor(data$state)) #no one outside US

#Speeder check
median(data$Duration..in.seconds.)
speed.limit <- median(data$Duration..in.seconds.)/3
data$speeder <- ifelse(data$Duration..in.seconds. < speed.limit, 1, 0)
describe(as.factor(data$speeder)) #only four and we didn't pre-register it, so not excluding
remove(speed.limit)

#Create CSV file for panel company -- just GIDs and demographics
names(data) #show all column names, note ones to remove
toluna.data <- data[, -c(1:155, 158, 160:161, 165)] #remove unnecessary columns
names(toluna.data) #check
write.csv(toluna.data, file='PB Labelling Data - for Toluna.csv', row.names=FALSE)
remove(toluna.data)

#De-identify data
data$GID <- NULL

#####################################################
#MAJOR RECODING#

#PAIRWISE COMPARISONS: Reverse coding
attach(data)
data$rvegan.pb2 <- 6 - vegan.pb2
data$rvegan.fg2 <- 6 - vegan.fg2
data$rvegan.zc2 <- 6 - vegan.zc2
data$rvegan.kojo2 <- 6 - vegan.kojo2
data$rvegan.dp2 <- 6 - vegan.dp2
data$rvegan.clean2 <- 6 - vegan.clean2
data$rvegan.pf2 <- 6 - vegan.pf2

data$rpb.fg2 <- 6 - pb.fg2
data$rpb.zc2 <- 6 - pb.zc2
data$rpb.kojo2 <- 6 - pb.kojo2
data$rpb.dp2 <- 6 - pb.dp2
data$rpb.clean2 <- 6 - pb.clean2
data$rpb.pf2 <- 6 - pb.pf2

data$rfg.zc2 <- 6 - fg.zc2
data$rfg.kojo2 <- 6 - fg.kojo2
data$rfg.dp2 <- 6 - fg.dp2
data$rfg.clean2 <- 6 - fg.clean2
data$rfg.pf2 <- 6 - fg.pf2

data$rzc.kojo2 <- 6 - zc.kojo2
data$rzc.dp2 <- 6 - zc.dp2
data$rzc.clean2 <- 6 - zc.clean2
data$rzc.pf2 <- 6 - zc.pf2

data$rkojo.dp2 <- 6 - kojo.dp2
data$rkojo.clean2 <- 6 - kojo.clean2
data$rkojo.pf2 <- 6 - kojo.pf2

data$rdp.clean2 <- 6 - dp.clean2
data$rdp.pf2 <- 6 - dp.pf2

data$rclean.pf2 <- 6 - clean.pf2


#PAIRWISE COMPARISONS: Check reverse-coding
cor(subset(data, select=c(vegan.pb2, rvegan.pb2)), use="pairwise.complete.obs")
cor(subset(data, select=c(vegan.dp2, rvegan.dp2)), use="pairwise.complete.obs")
cor(subset(data, select=c(vegan.kojo2, rvegan.kojo2)), use="pairwise.complete.obs")
cor(subset(data, select=c(vegan.fg2, rvegan.fg2)), use="pairwise.complete.obs")
cor(subset(data, select=c(vegan.zc2, rvegan.zc2)), use="pairwise.complete.obs")
cor(subset(data, select=c(vegan.clean2, rvegan.clean2)), use="pairwise.complete.obs")
cor(subset(data, select=c(vegan.pf2,rvegan.pf2)), use="pairwise.complete.obs")
cor(subset(data, select=c(pb.fg2, rpb.fg2)), use="pairwise.complete.obs")
cor(subset(data, select=c(pb.zc2, rpb.zc2)), use="pairwise.complete.obs")
cor(subset(data, select=c(pb.dp2, rpb.dp2)), use="pairwise.complete.obs")
cor(subset(data, select=c(pb.kojo2, rpb.kojo2)), use="pairwise.complete.obs")
cor(subset(data, select=c(pb.clean2, rpb.clean2)), use="pairwise.complete.obs")
cor(subset(data, select=c(pb.pf2, rpb.pf2)), use="pairwise.complete.obs")
cor(subset(data, select=c(fg.zc2, rfg.zc2)), use="pairwise.complete.obs")
cor(subset(data, select=c(fg.kojo2, rfg.kojo2)), use="pairwise.complete.obs")
cor(subset(data, select=c(fg.dp2, rfg.dp2)), use="pairwise.complete.obs")
cor(subset(data, select=c(fg.clean2, rfg.clean2)), use="pairwise.complete.obs")
cor(subset(data, select=c(fg.pf2, rfg.pf2)), use="pairwise.complete.obs")
cor(subset(data, select=c(zc.kojo2, rzc.kojo2)), use="pairwise.complete.obs")
cor(subset(data, select=c(zc.dp2, rzc.dp2)), use="pairwise.complete.obs")
cor(subset(data, select=c(zc.clean2, rzc.clean2)), use="pairwise.complete.obs")
cor(subset(data, select=c(zc.pf2, rzc.pf2)), use="pairwise.complete.obs")
cor(subset(data, select=c(kojo.dp2, rkojo.dp2)), use="pairwise.complete.obs")
cor(subset(data, select=c(kojo.pf2, rkojo.pf2)), use="pairwise.complete.obs")
cor(subset(data, select=c(kojo.clean2, rkojo.clean2)), use="pairwise.complete.obs")
cor(subset(data, select=c(dp.clean2, rdp.clean2)), use="pairwise.complete.obs")
cor(subset(data, select=c(dp.pf2, rdp.pf2)), use="pairwise.complete.obs")
cor(subset(data, select=c(clean.pf2, rclean.pf2)), use="pairwise.complete.obs")


#PAIRWISE COMPARISONS: Combine left/right variables
attach(data)
data$vegan.pb <- ifelse(!is.na(vegan.pb1),vegan.pb1,rvegan.pb2)
data$vegan.dp <- ifelse(!is.na(vegan.dp1),vegan.dp1,rvegan.dp2)
data$vegan.kojo <- ifelse(!is.na(vegan.kojo1),vegan.kojo1,rvegan.kojo2)
data$vegan.fg <- ifelse(!is.na(vegan.fg1),vegan.fg1,rvegan.fg2)
data$vegan.zc <- ifelse(!is.na(vegan.zc1),vegan.zc1,rvegan.zc2)
data$vegan.clean <- ifelse(!is.na(vegan.clean1),vegan.clean1,rvegan.clean2)
data$vegan.pf <- ifelse(!is.na(vegan.pf1),vegan.pf1,rvegan.pf2)
data$pb.fg <- ifelse(!is.na(pb.fg1),pb.fg1,rpb.fg2)
data$pb.zc <- ifelse(!is.na(pb.zc1),pb.zc1,rpb.zc2)
data$pb.dp <- ifelse(!is.na(pb.dp1),pb.dp1,rpb.dp2)
data$pb.kojo <- ifelse(!is.na(pb.kojo1),pb.kojo1,rpb.kojo2)
data$pb.clean <- ifelse(!is.na(pb.clean1),pb.clean1,rpb.clean2)
data$pb.pf <- ifelse(!is.na(pb.pf1),pb.pf1,rpb.pf2)
data$fg.zc <- ifelse(!is.na(fg.zc1),fg.zc1,rfg.zc2)
data$fg.kojo <- ifelse(!is.na(fg.kojo1),fg.kojo1,rfg.kojo2)
data$fg.dp <- ifelse(!is.na(fg.dp1),fg.dp1,rfg.dp2)
data$fg.clean <- ifelse(!is.na(fg.clean1),fg.clean1,rfg.clean2)
data$fg.pf <- ifelse(!is.na(fg.pf1),fg.pf1,rfg.pf2)
data$zc.kojo <- ifelse(!is.na(zc.kojo1),zc.kojo1,rzc.kojo2)
data$zc.dp <- ifelse(!is.na(zc.dp1),zc.dp1,rzc.dp2)
data$zc.clean <- ifelse(!is.na(zc.clean1),zc.clean1,rzc.clean2)
data$zc.pf <- ifelse(!is.na(zc.pf1),zc.pf1,rzc.pf2)
data$kojo.dp <- ifelse(!is.na(kojo.dp1),kojo.dp1,rkojo.dp2)
data$kojo.pf <- ifelse(!is.na(kojo.pf1),kojo.pf1,rkojo.pf2)
data$kojo.clean <- ifelse(!is.na(kojo.clean1),kojo.clean1,rkojo.clean2)
data$dp.clean <- ifelse(!is.na(dp.clean1),dp.clean1,rdp.clean2)
data$dp.pf <- ifelse(!is.na(dp.pf1),dp.pf1,rdp.pf2)
data$clean.pf <- ifelse(!is.na(clean.pf1),clean.pf1,rclean.pf2)

#PAIRWISE COMPARISONS: Transform to -2 to +2 scale
attach(data)
data$vegan.pb.rec <- vegan.pb -3
data$vegan.dp.rec <- vegan.dp-3
data$vegan.kojo.rec <- vegan.kojo-3
data$vegan.fg.rec <- vegan.fg-3
data$vegan.zc.rec <- vegan.zc-3
data$vegan.clean.rec <- vegan.clean-3
data$vegan.pf.rec <- vegan.pf-3
data$pb.fg.rec <- pb.fg-3
data$pb.zc.rec <- pb.zc-3
data$pb.dp.rec <- pb.dp-3
data$pb.kojo.rec <- pb.kojo-3
data$pb.clean.rec <- pb.clean-3
data$pb.pf.rec <- pb.pf-3
data$fg.zc.rec <- fg.zc-3
data$fg.kojo.rec <- fg.kojo-3
data$fg.dp.rec <- fg.dp-3
data$fg.clean.rec <- fg.clean-3
data$fg.pf.rec <- fg.pf-3
data$zc.kojo.rec <- zc.kojo-3
data$zc.dp.rec <- zc.dp-3
data$zc.clean.rec <- zc.clean-3
data$zc.pf.rec <- zc.pf-3
data$kojo.dp.rec <- kojo.dp-3
data$kojo.pf.rec <- kojo.pf-3
data$kojo.clean.rec <- kojo.clean-3
data$dp.clean.rec <- dp.clean-3
data$dp.pf.rec <- dp.pf-3
data$clean.pf.rec <- clean.pf-3


#PAIRWISE COMPARISONS: calculate participants' scores for each label
attach(data)
data$vegan <- (vegan.pb.rec + vegan.dp.rec + vegan.kojo.rec + vegan.fg.rec + vegan.zc.rec + 
                 vegan.clean.rec + vegan.pf.rec) * -1
data$pb <- (pb.fg.rec + pb.zc.rec + pb.dp.rec + pb.kojo.rec +  pb.clean.rec +  pb.pf.rec) * -1 + vegan.pb.rec
data$fg <- (fg.zc.rec + fg.kojo.rec + fg.dp.rec + fg.clean.rec + fg.pf.rec) * -1 + vegan.fg.rec + pb.fg.rec
data$zc <- (zc.kojo.rec + zc.dp.rec + zc.clean.rec + zc.pf.rec) * -1 + vegan.zc.rec + pb.zc.rec + fg.zc.rec
data$kojo <- (kojo.dp.rec + kojo.pf.rec + kojo.clean.rec) * -1 + vegan.kojo.rec + pb.kojo.rec + fg.kojo.rec + zc.kojo.rec
data$dp <- (dp.clean.rec + dp.pf.rec) * -1 + vegan.dp.rec + pb.dp.rec + fg.dp.rec + zc.dp.rec + kojo.dp.rec
data$clean <- clean.pf.rec * -1 + vegan.clean.rec + pb.clean.rec + fg.clean.rec + zc.clean.rec + kojo.clean.rec + dp.clean.rec
data$pf <- vegan.pf.rec + pb.pf.rec + fg.pf.rec + zc.pf.rec + kojo.pf.rec + dp.pf.rec + clean.pf.rec

str(data)
describe(data$vegan)
describe(data$pb)
describe(data$fg)
describe(data$zc)
describe(data$kojo)
describe(data$dp)
describe(data$clean)
describe(data$pf)

hist(data$vegan)
hist(data$pb)
hist(data$fg)
hist(data$zc)
hist(data$kojo)
hist(data$dp)
hist(data$clean)
hist(data$pf)

attach(data)
mean(c(vegan, pb, fg, zc, kojo, dp, clean, pf)) #checks that these are coded right -- they should have a grand mean of 0

#RATINGS: Reverse coding for ratings with reversed scales
attach(data)
data$rclean1b <- 6 - clean1b
data$rdp1b <- 6 - dp1b
data$rpf1b <- 6 - pf1b
data$rpb1b <- 6 - pb1b
data$rvegan1b <- 6 - vegan1b
data$rzc1b <- 6 - zc1b
data$rkojo1b <- 6 - kojo1b
data$rfg1b <- 6 - fg1b
data$rclean2b <- 6 - clean2b
data$rdp2b <- 6 - dp2b
data$rpf2b <- 6 - pf2b
data$rpb2b <- 6 - pb2b
data$rvegan2b <- 6 - vegan2b
data$rzc2b <- 6 - zc2b
data$rkojo2b <- 6 - kojo2b
data$rfg2b <- 6 - fg2b
data$rclean3b <- 6 - clean3b
data$rdp3b <- 6 - dp3b
data$rpf3b <- 6 - pf3b
data$rpb3b <- 6 - pb3b
data$rvegan3b <- 6 - vegan3b
data$rzc3b <- 6 - zc3b
data$rkojo3b <- 6 - kojo3b
data$rfg3b <- 6 - fg3b
data$rclean4b <- 6 - clean4b
data$rdp4b <- 6 - dp4b
data$rpf4b <- 6 - pf4b
data$rpb4b <- 6 - pb4b
data$rvegan4b <- 6 - vegan4b
data$rzc4b <- 6 - zc4b
data$rkojo4b <- 6 - kojo4b
data$rfg4b <- 6 - fg4b


#RATINGS: combine ratings from scales with opposite directions
attach(data)
data$clean.health <- ifelse(!is.na(clean1),clean1,rclean1b)
data$clean.animals <- ifelse(!is.na(clean2),clean2,rclean2b)
data$clean.taste <- ifelse(!is.na(clean3),clean3,rclean3b)
data$clean.env <- ifelse(!is.na(clean4),clean4,rclean4b)
data$dp.health <- ifelse(!is.na(dp1),dp1,rdp1b)
data$dp.animals <- ifelse(!is.na(dp2),dp2,rdp2b)
data$dp.taste <- ifelse(!is.na(dp3),dp3,rdp3b)
data$dp.env <- ifelse(!is.na(dp4),dp4,rdp4b)
data$pf.health <- ifelse(!is.na(pf1),pf1,rpf1b)
data$pf.animals <- ifelse(!is.na(pf2),pf2,rpf2b)
data$pf.taste <- ifelse(!is.na(pf3),pf3,rpf3b)
data$pf.env <- ifelse(!is.na(pf4),pf4,rpf4b)
data$pb.health <- ifelse(!is.na(pb1),pb1,rpb1b)
data$pb.animals <- ifelse(!is.na(pb2),pb2,rpb2b)
data$pb.taste <- ifelse(!is.na(pb3),pb3,rpb3b)
data$pb.env <- ifelse(!is.na(pb4),pb4,rpb4b)
data$vegan.health <- ifelse(!is.na(vegan1),vegan1,rvegan1b)
data$vegan.animals <- ifelse(!is.na(vegan2),vegan2,rvegan2b)
data$vegan.taste <- ifelse(!is.na(vegan3),vegan3,rvegan3b)
data$vegan.env <- ifelse(!is.na(vegan4),vegan4,rvegan4b)
data$zc.health <- ifelse(!is.na(zc1),zc1,rzc1b)
data$zc.animals <- ifelse(!is.na(zc2),zc2,rzc2b)
data$zc.taste <- ifelse(!is.na(zc3),zc3,rzc3b)
data$zc.env <- ifelse(!is.na(zc4),zc4,rzc4b)
data$kojo.health <- ifelse(!is.na(kojo1),kojo1,rkojo1b)
data$kojo.animals <- ifelse(!is.na(kojo2),kojo2,rkojo2b)
data$kojo.taste <- ifelse(!is.na(kojo3),kojo3,rkojo3b)
data$kojo.env <- ifelse(!is.na(kojo4),kojo4,rkojo4b)
data$fg.health <- ifelse(!is.na(fb1),fb1,rfg1b)
data$fg.animals <- ifelse(!is.na(fb2),fb2,rfg2b)
data$fg.taste <- ifelse(!is.na(fb3),fb3,rfg3b)
data$fg.env <- ifelse(!is.na(fb4),fb4,rfg4b)

#Delete unnecessary variables from Qualtrics dataset
names(data) #show all column names, note ones to remove
data <- data[, -c(5:61, 63:130, 132, 158:160, 162, 164:220, 257:288)] #remove unnecessary columns
names(data) #check


###################################
#DEMOGRAPHICS

data$gender1 <- factor(gender1, levels = c(2, 1, 3), labels = c("Woman", "Man", "Other"))
describe(data$gender1) #show gender frequencies

describe(data$race)
data$race <- factor(data$race, levels = c(2:8), labels = c("White", "Black", "Native", "Asian", "Pacific Islander", 
                                                           "Multi-Racial", "Other"))
levels(data$race)
as.numeric(data$race) #note that it has now converted the levels to 1:7 instead of 2:8

table(data$race) #show race frequencies

data$hispanic <- factor(hispanic, levels = c(1, 2), labels = c("Hispanic/Latino","Non-Hispanic/Latino"))
describe(data$hispanic) #show hispanic frequencies

data$eth.race <- ifelse(data$hispanic == "Hispanic/Latino", 8, data$race) #combines race and ethnicity variables
data$eth.race <- factor(data$eth.race, levels = c(1:8), labels = c("White", "Black", "Native", "Asian", "Pacific Islander", 
                                                                   "Multi-Racial", "Other", "Hispanic"))
describe(data$eth.race) #show frequencies for race/ethnicity

data$eth.race2 <- factor(ifelse(data$eth.race %in% c("White", "Black", "Hispanic", "Other"), data$eth.race, 7))
data$eth.race2 <- factor(data$eth.race2, levels = c(1, 2, 8, 7), labels = c("White", "Black", "Hispanic", "Other"))
describe(data$eth.race2)

data$income <-factor(data$income, levels = c(1:5), labels = c("Less than $25,000", "$25,000 - $49,999",
                                                              "$50,000 - $74,999", "$75,000 - $99,999", "$100,000+"), ordered = TRUE)
describe(data$income) #show income frequencies

str(data)
data$AgeGroup <- factor(data$AgeGroup, labels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"))
describe(data$AgeGroup) #show age group frequencies

table(as.factor(data$state))
data$Region <- ifelse(data$state == 40, NA, data$Region) #remove Puerto Rico from Region groupings
data$Region <- factor(data$Region)
describe(data$Region)

data$ffq_16 <- factor(data$ffq_16, levels = c(1:5), labels = c("Never", "Less than 1 time per week", 
                                                               "1-3 times per week", "4-6 times per week", 
                                                               "1 or more times per day"), ordered = TRUE) #meat frequency
data$ffq_17 <- factor(data$ffq_17, levels = c(1:5), labels = c("Never", "Less than 1 time per week", 
                                                               "1-3 times per week", "4-6 times per week", 
                                                               "1 or more times per day"), ordered = TRUE) #fish frequency
data$ffq_18 <- factor(data$ffq_18, levels = c(1:5), labels = c("Never", "Less than 1 time per week", 
                                                               "1-3 times per week", "4-6 times per week", 
                                                               "1 or more times per day"), ordered = TRUE) #dairy/egg frequency
data$ffq_19 <- factor(data$ffq_19, levels = c(1:5), labels = c("Never", "Less than 1 time per week", 
                                                               "1-3 times per week", "4-6 times per week", 
                                                               "1 or more times per day"), ordered = TRUE) #meat alts frequency
data$ffq_20 <- factor(data$ffq_20, levels = c(1:5), labels = c("Never", "Less than 1 time per week", 
                                                               "1-3 times per week", "4-6 times per week", 
                                                               "1 or more times per day"), ordered = TRUE) #dairy/egg alts frequency
describe(data$ffq_16)
describe(data$ffq_17)
describe(data$ffq_18)
describe(data$ffq_19)
describe(data$ffq_20)

data$alt.cons <- factor(ifelse(data$ffq_19 == "Never" & data$ffq_20 == "Never", 0, 1), levels = c(0, 1), labels = c("Non-alt-consumer",
                                                                                                                    "Alt consumer"))
describe(data$alt.cons)

data$vegn <- ifelse(data$ffq_16 == "Never" & data$ffq_17 == "Never", 1, 0) #create vegan/vegetarian variable (1 = yes) if never eat meat/fish
data$vegn <- factor(data$vegn, levels = c(0,1), labels = c("non-vegn", "vegn"))
describe(data$vegn)

str(data)


###################################
#CREATE LONG-FORMAT DATASET

ldata <- gather(data, key = label, value = label.score, vegan:pf, factor_key = TRUE) #turn wide data long by adding label as a new
                                                                                      #variable and assigning labels as levels to it
print(levels(ldata$label))
#put labels in order of overall score (spoilers!)
ldata$label = factor(ldata$label, levels (ldata$label)[c(3,1,7,5,6,8,4,2)]) 
print(levels(ldata$label))

ldata$health <- NA
ldata$animals <- NA
ldata$taste <- NA
ldata$env <- NA

ldata$health <- ifelse(ldata$label == "vegan", ldata$vegan.health,
                           ifelse(ldata$label == "pb", ldata$pb.health,
                                  ifelse(ldata$label == "fg", ldata$fg.health,
                                         ifelse(ldata$label == "zc", ldata$zc.health,
                                                ifelse(ldata$label == "kojo", ldata$kojo.health,
                                                       ifelse(ldata$label == "dp", ldata$dp.health,
                                                              ifelse(ldata$label == "clean", ldata$clean.health,
                                                                     ifelse(ldata$label == "pf", ldata$pf.health, 0))))))))

ldata$animals <- ifelse(ldata$label == "vegan", ldata$vegan.animals,
                       ifelse(ldata$label == "pb", ldata$pb.animals,
                              ifelse(ldata$label == "fg", ldata$fg.animals,
                                     ifelse(ldata$label == "zc", ldata$zc.animals,
                                            ifelse(ldata$label == "kojo", ldata$kojo.animals,
                                                   ifelse(ldata$label == "dp", ldata$dp.animals,
                                                          ifelse(ldata$label == "clean", ldata$clean.animals,
                                                                 ifelse(ldata$label == "pf", ldata$pf.animals, 0))))))))

ldata$env <- ifelse(ldata$label == "vegan", ldata$vegan.env,
                       ifelse(ldata$label == "pb", ldata$pb.env,
                              ifelse(ldata$label == "fg", ldata$fg.env,
                                     ifelse(ldata$label == "zc", ldata$zc.env,
                                            ifelse(ldata$label == "kojo", ldata$kojo.env,
                                                   ifelse(ldata$label == "dp", ldata$dp.env,
                                                          ifelse(ldata$label == "clean", ldata$clean.env,
                                                                 ifelse(ldata$label == "pf", ldata$pf.env, 0))))))))

ldata$taste <- ifelse(ldata$label == "vegan", ldata$vegan.taste,
                       ifelse(ldata$label == "pb", ldata$pb.taste,
                              ifelse(ldata$label == "fg", ldata$fg.taste,
                                     ifelse(ldata$label == "zc", ldata$zc.taste,
                                            ifelse(ldata$label == "kojo", ldata$kojo.taste,
                                                   ifelse(ldata$label == "dp", ldata$dp.taste,
                                                          ifelse(ldata$label == "clean", ldata$clean.taste,
                                                                 ifelse(ldata$label == "pf", ldata$pf.taste, 0))))))))

#check variables were combined correctly 
vegan.only <- ldata[ldata$label == "vegan", ]
describeBy(vegan.only$taste, group = vegan.only$vegan.taste)
pb.only <- ldata[ldata$label == "pb", ]
describeBy(pb.only$taste, group = pb.only$pb.taste)
fg.only <- ldata[ldata$label == "fg", ]
describeBy(fg.only$taste, group = fg.only$fg.taste)
zc.only <- ldata[ldata$label == "zc", ]
describeBy(zc.only$taste, group = zc.only$zc.taste)
kojo.only <- ldata[ldata$label == "kojo", ]
describeBy(kojo.only$taste, group = kojo.only$kojo.taste)
dp.only <- ldata[ldata$label == "dp", ]
describeBy(dp.only$taste, group = dp.only$dp.taste)
clean.only <- ldata[ldata$label == "clean", ]
describeBy(clean.only$taste, group = clean.only$clean.taste)
pf.only <- ldata[ldata$label == "pf", ]
describeBy(pf.only$taste, group = pf.only$pf.taste)
remove(vegan.only, pb.only, fg.only, zc.only, kojo.only, dp.only, clean.only, pf.only)

#Delete original multiple rating variables
names(ldata) #show all column names, note ones to remove
ldata <- ldata[, -c(34:93)] #remove unnecessary columns
names(ldata) #check



########################################
#SAVING

#data
#Should be at n = 1431, 104 variables
save(data, file='PB Labelling Data - wide cleaned.Rdata')
write.csv(data, file='PB Labelling Data - wide cleaned.csv', row.names=FALSE)

#ldata
#Should be at n = 11,448 and 42 variables
save(ldata, file='PB Labelling Data - long cleaned.Rdata')
write.csv(ldata, file='PB Labelling Data - long cleaned.csv', row.names=FALSE)

