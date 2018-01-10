#ETG Analyses for Lab RSA Poster

setwd("C:/Users/sbuja/Documents/Conferences/RSA 2018/")

#required libraries
library(SpPack)
library(multilevel) #MLM analysis
library(plyr) #data wrangling
library(dplyr) #data wrangling 2
library(tidyr) #data wrangling 3
library(xlsx) #package to import xls files directly
library(grid) #for multiplot
library(psych) #for alpha (cronbach's alpha function)
library(GGally) #ggpairs plots
library(gmodels) #CrossTable Function
library(Hmisc) #rcorr
library(stats)
library(pscl) # for mcfadden R2 in logistic regression
library(caret) #for crossvalidation methods
library(ROCR) #For crossvalidation AUC curve
library(scales) #for percent axis
library(Deducer)


#Simulating Data----
ATLFB <- read.csv("ATLFB.Merge.csv", header=T, na.strings=c("NA"))
SpDesc(ATLFB)

#TLFB Scoring
ATLFB$Drink.Tot <- rowSums(subset(ATLFB, select=c("ATLFB_01", "ATLFB_02", "ATLFB_03", "ATLFB_04", "ATLFB_05", "ATLFB_06", "ATLFB_07", "ATLFB_08", "ATLFB_09", "ATLFB_10",
                                                              "ATLFB_11", "ATLFB_12", "ATLFB_13", "ATLFB_14", "ATLFB_15", "ATLFB_16", "ATLFB_17", "ATLFB_18", "ATLFB_19", "ATLFB_20",
                                                              "ATLFB_21", "ATLFB_22", "ATLFB_23", "ATLFB_24", "ATLFB_25", "ATLFB_26", "ATLFB_27", "ATLFB_28", "ATLFB_29", "ATLFB_30")))
ATLFB$Drink.Days <- rowSums(subset(ATLFB, select=c("ATLFB_01", "ATLFB_02", "ATLFB_03", "ATLFB_04", "ATLFB_05", "ATLFB_06", "ATLFB_07", "ATLFB_08", "ATLFB_09", "ATLFB_10",
                                                               "ATLFB_11", "ATLFB_12", "ATLFB_13", "ATLFB_14", "ATLFB_15", "ATLFB_16", "ATLFB_17", "ATLFB_18", "ATLFB_19", "ATLFB_20",
                                                               "ATLFB_21", "ATLFB_22", "ATLFB_23", "ATLFB_24", "ATLFB_25", "ATLFB_26", "ATLFB_27", "ATLFB_28", "ATLFB_29", "ATLFB_30"))>0)
ATLFB$DPD <- ATLFB$Drink.Tot/30
ATLFB$DPW <- ATLFB$DPD * 7
ATLFB$DPDD <- ATLFB$Drink.Tot/ATLFB$Drink.Days

#TLFB Summary Statistics and Figures
SpDesc(ATLFB[c("Drink.Tot", "Drink.Days", "DPD", "DPW", "DPDD")])

SpHist(data=ATLFB, variable="Drink.Tot", bins=10, save=F)
SpHist(data=ATLFB, variable="Drink.Days", bins=10, save=F)
SpHist(data=ATLFB, variable="DPD", bins=10, save=F)
SpHist(data=ATLFB, variable="DPDD", bins=10, save=F)
SpHist(data=ATLFB, variable="DPW", bins=10, save=F)

#simulate other data
ETG.Merge <- ATLFB
ETG.Merge$AUDIT <- round(SimCorX(ETG.Merge$DPW, ymean=8, ysd=4, rho=.7)$y)
ETG.Merge$Female <- SimCorX(ETG.Merge$AUDIT, ymean=0, ysd=1, rho=-.5)$y
ETG.Merge$Female <- ifelse(ETG.Merge$Female>=0,1,0)

table(ETG.Merge$Female)

ETG.Merge$ETG <- SimCorX(rowSums(ETG.Merge[c("ATLFB_01", "ATLFB_02", "ATLFB_03", "ATLFB_04", "ATLFB_05")]), ymean=0, ysd=1, rho=0.7)$y
ETG.Merge$ETG <- ifelse(ETG.Merge$ETG>=0,1,0)

SpDesc(ETG.Merge)

ETG.Merge %>% group_by(Female) %>% summarise(mDPW=mean(DPW), sdDPW=sd(DPW),
                                             mDPDD=mean(DPDD), sdDPDD=sd(DPDD),
                                             mAUDIT=mean(AUDIT), sdAUDIT=sd(AUDIT),
                                             mETG=mean(ETG))

ETG.Merge %>% group_by(ETG) %>% summarise(mDPW=mean(DPW), sdDPW=sd(DPW),
                                             mDPDD=mean(DPDD), sdDPDD=sd(DPDD),
                                             mAUDIT=mean(AUDIT), sdAUDIT=sd(AUDIT))

write.csv(ETG.Merge, "ETG Sample Data.csv", row.names=F)









#Importing Data----
#SRSA Data
# SRSA.ETGData <- Beh[c("Subject", "Female", "Drug_ETG_Beh",
#                       "ATLFB_01", "ATLFB_02", "ATLFB_03", "ATLFB_04", "ATLFB_05", "ATLFB_06", "ATLFB_07", "ATLFB_08", "ATLFB_09", "ATLFB_10",
#                       "ATLFB_11", "ATLFB_12", "ATLFB_13", "ATLFB_14", "ATLFB_15", "ATLFB_16", "ATLFB_17", "ATLFB_18", "ATLFB_19", "ATLFB_20",
#                       "ATLFB_21", "ATLFB_22", "ATLFB_23", "ATLFB_24", "ATLFB_25", "ATLFB_26", "ATLFB_27", "ATLFB_28", "ATLFB_29", "ATLFB_30")]
# SpDesc(SRSA.ETGData)
# SRSA.ETGData <- na.exclude(SRSA.ETGData)
# SpDesc(SRSA.ETGData)
# write.csv(SRSA.ETGData, "SRSA.ETGData.csv", row.names=F)

SRSA.ETGData <- read.csv("SRSA.ETGData.csv")
SpDesc(SRSA.ETGData)

VARNTX.ETGData <- read.csv("ETG TLFB DATA.csv")
SpDesc(VARNTX.ETGData)

# ETGData <- SRSA.ETGData
ETGData <- rbind(SRSA.ETGData, VARNTX.ETGData)
SpDesc(ETGData)
View(ETGData)

# ETGData <- read.csv("ETG Sample Data.csv", header=T, na.strings=c("NA"))

#Scoring data for any drinking in the past 1, 3, 5, and 7 days
ETGData$Past1Drank <- ifelse(rowSums(ETGData[c("ATLFB_01")]>0),1,0)
#View(ETGData[c("ATLFB_01", "Past1Drank")])
ETGData$Past3Drank <- ifelse(rowSums(ETGData[c("ATLFB_01", "ATLFB_02", "ATLFB_03")]>0),1,0)
#View(ETGData[c("ATLFB_01", "ATLFB_02", "ATLFB_03", "Past3Drank")])
ETGData$Past5Drank <- ifelse(rowSums(ETGData[c("ATLFB_01", "ATLFB_02", "ATLFB_03", "ATLFB_04", "ATLFB_05")]>0),1,0)
#View(ETGData[c("ATLFB_01", "ATLFB_02", "ATLFB_03", "ATLFB_04", "ATLFB_05", "Past5Drank")])
ETGData$Past7Drank <- ifelse(rowSums(ETGData[c("ATLFB_01", "ATLFB_02", "ATLFB_03", "ATLFB_04", "ATLFB_05", "ATLFB_06", "ATLFB_07")]>0),1,0)
#View(ETGData[c("ATLFB_01", "ATLFB_02", "ATLFB_03", "ATLFB_04", "ATLFB_05", "ATLFB_06", "ATLFB_07", "Past7Drank")])

#Analyses for Past1Drank
CrossTable(ETGData$ETG, ETGData$Past1Drank)
chisq.test(table(ETGData$ETG, ETGData$Past1Drank))
#sensitivity = True Detected Positives / (Total Positive Events)
table(ETGData$ETG, ETGData$Past1Drank)[2,2] / sum(table(ETGData$ETG, ETGData$Past1Drank)[,2])
#Specificity = True Detected Negatives / (Total Negative Events)
table(ETGData$ETG, ETGData$Past1Drank)[1,1] / sum(table(ETGData$ETG, ETGData$Past1Drank)[,1])

#Analyses for Past3Drank
CrossTable(ETGData$ETG, ETGData$Past3Drank)
chisq.test(table(ETGData$ETG, ETGData$Past3Drank))
#sensitivity = True Detected Positives / (Total Positive Events)
table(ETGData$ETG, ETGData$Past3Drank)[2,2] / sum(table(ETGData$ETG, ETGData$Past3Drank)[,2])
#Specificity = True Detected Negatives / (Total Negative Events)
table(ETGData$ETG, ETGData$Past3Drank)[1,1] / sum(table(ETGData$ETG, ETGData$Past3Drank)[,1])

#Analyses for Past5Drank
CrossTable(ETGData$ETG, ETGData$Past5Drank)
chisq.test(table(ETGData$ETG, ETGData$Past5Drank))
#sensitivity = True Detected Positives / (Total Positive Events)
table(ETGData$ETG, ETGData$Past5Drank)[2,2] / sum(table(ETGData$ETG, ETGData$Past5Drank)[,2])
#Specificity = True Detected Negatives / (Total Negative Events)
table(ETGData$ETG, ETGData$Past5Drank)[1,1] / sum(table(ETGData$ETG, ETGData$Past5Drank)[,1])

#Analyses for Past7Drank
CrossTable(ETGData$ETG, ETGData$Past7Drank)
chisq.test(table(ETGData$ETG, ETGData$Past7Drank))
#sensitivity = True Detected Positives / (Total Positive Events)
table(ETGData$ETG, ETGData$Past7Drank)[2,2] / sum(table(ETGData$ETG, ETGData$Past7Drank)[,2])
#Specificity = True Detected Negatives / (Total Negative Events)
table(ETGData$ETG, ETGData$Past7Drank)[1,1] / sum(table(ETGData$ETG, ETGData$Past7Drank)[,1])


#Scoring data for binge drinking in the past 1, 3, 5, and 7 days
ETGData$ATLFB_01Binge <- ifelse((ETGData$Female + ETGData$ATLFB_01)>=5,1,0)
ETGData$ATLFB_02Binge <- ifelse((ETGData$Female + ETGData$ATLFB_02)>=5,1,0)
ETGData$ATLFB_03Binge <- ifelse((ETGData$Female + ETGData$ATLFB_03)>=5,1,0)
ETGData$ATLFB_04Binge <- ifelse((ETGData$Female + ETGData$ATLFB_04)>=5,1,0)
ETGData$ATLFB_05Binge <- ifelse((ETGData$Female + ETGData$ATLFB_05)>=5,1,0)
ETGData$ATLFB_06Binge <- ifelse((ETGData$Female + ETGData$ATLFB_06)>=5,1,0)
ETGData$ATLFB_07Binge <- ifelse((ETGData$Female + ETGData$ATLFB_07)>=5,1,0)

#compute any binging in the past 1, 3, 5, and 7 days
ETGData$Past1Binge <- ifelse(rowSums(ETGData[c("ATLFB_01Binge")]>0),1,0)
#View(ETGData[c("Female", "ATLFB_01", "Past1Binge")])
ETGData$Past3Binge <- ifelse(rowSums(ETGData[c("ATLFB_01Binge", "ATLFB_02Binge", "ATLFB_03Binge")]>0),1,0)
#View(ETGData[c("Female", "ATLFB_01", "ATLFB_02", "ATLFB_03", "Past3Binge")])
ETGData$Past5Binge <- ifelse(rowSums(ETGData[c("ATLFB_01Binge", "ATLFB_02Binge", "ATLFB_03Binge", "ATLFB_04Binge", "ATLFB_05Binge")]>0),1,0)
#View(ETGData[c("Female", "ATLFB_01", "ATLFB_02", "ATLFB_03", "ATLFB_04", "ATLFB_05", "Past5Binge")])
ETGData$Past7Binge <- ifelse(rowSums(ETGData[c("ATLFB_01Binge", "ATLFB_02Binge", "ATLFB_03Binge", "ATLFB_04Binge", "ATLFB_05Binge", "ATLFB_06Binge", "ATLFB_07Binge")]>0),1,0)
#View(ETGData[c("Female", "ATLFB_01", "ATLFB_02", "ATLFB_03", "ATLFB_04", "ATLFB_05", "ATLFB_06", "ATLFB_07", "Past7Binge")])

#Analyses for Past1Binge
CrossTable(ETGData$ETG, ETGData$Past1Binge)
chisq.test(table(ETGData$ETG, ETGData$Past1Binge))
#sensitivity = True Detected Positives / (Total Positive Events)
table(ETGData$ETG, ETGData$Past1Binge)[2,2] / sum(table(ETGData$ETG, ETGData$Past1Binge)[,2])
#Specificity = True Detected Negatives / (Total Negative Events)
table(ETGData$ETG, ETGData$Past1Binge)[1,1] / sum(table(ETGData$ETG, ETGData$Past1Binge)[,1])

#Analyses for Past3Binge
CrossTable(ETGData$ETG, ETGData$Past3Binge)
chisq.test(table(ETGData$ETG, ETGData$Past3Binge))
#sensitivity = True Detected Positives / (Total Positive Events)
table(ETGData$ETG, ETGData$Past3Binge)[2,2] / sum(table(ETGData$ETG, ETGData$Past3Binge)[,2])
#Specificity = True Detected Negatives / (Total Negative Events)
table(ETGData$ETG, ETGData$Past3Binge)[1,1] / sum(table(ETGData$ETG, ETGData$Past3Binge)[,1])

#Analyses for Past5Binge
CrossTable(ETGData$ETG, ETGData$Past5Binge)
chisq.test(table(ETGData$ETG, ETGData$Past5Binge))
#sensitivity = True Detected Positives / (Total Positive Events)
table(ETGData$ETG, ETGData$Past5Binge)[2,2] / sum(table(ETGData$ETG, ETGData$Past5Binge)[,2])
#Specificity = True Detected Negatives / (Total Negative Events)
table(ETGData$ETG, ETGData$Past5Binge)[1,1] / sum(table(ETGData$ETG, ETGData$Past5Binge)[,1])

#Analyses for Past7Binge
CrossTable(ETGData$ETG, ETGData$Past7Binge)
chisq.test(table(ETGData$ETG, ETGData$Past7Binge))
#sensitivity = True Detected Positives / (Total Positive Events)
table(ETGData$ETG, ETGData$Past7Binge)[2,2] / sum(table(ETGData$ETG, ETGData$Past7Binge)[,2])
#Specificity = True Detected Negatives / (Total Negative Events)
table(ETGData$ETG, ETGData$Past7Binge)[1,1] / sum(table(ETGData$ETG, ETGData$Past7Binge)[,1])





#Continuous data predictors----
ETGData$Past3Drinks <- rowSums(ETGData[c("ATLFB_01", "ATLFB_03", "ATLFB_03")])
ETGData$Past5Drinks <- rowSums(ETGData[c("ATLFB_01", "ATLFB_03", "ATLFB_03", "ATLFB_04", "ATLFB_05")])
ETGData$Past7Drinks <- rowSums(ETGData[c("ATLFB_01", "ATLFB_03", "ATLFB_03", "ATLFB_04", "ATLFB_05", "ATLFB_06", "ATLFB_07")])

SpHist(ETGData, variable="Past3Drinks")
SpHist(ETGData, variable="Past5Drinks")
SpHist(ETGData, variable="Past7Drinks")
ETGData$logPast3Drink <- log(ETGData$Past3Drink+1)
SpHist(ETGData, variable="logPast3Drink")

Model.ETG.Past3 <- glm(ETG ~ logPast3Drink,
                       data=ETGData, family=binomial(link="logit"))
summary(Model.ETG.Past3)
exp(cbind(OR = coef(Model.ETG.Past3), confint(Model.ETG.Past3)))
pR2(Model.ETG.Past3)
rocplot(Model.ETG.Past3) + SpTheme()


Model.ETG.Past5 <- glm(ETG ~ Past5Drinks,
                       data=ETGData, family=binomial(link="logit"))
summary(Model.ETG.Past5)
exp(cbind(OR = coef(Model.ETG.Past5), confint(Model.ETG.Past5)))
pR2(Model.ETG.Past5)
rocplot(Model.ETG.Past5) + SpTheme()

Model.ETG.Past7 <- glm(ETG ~ Past7Drinks,
                       data=ETGData, family=binomial(link="logit"))
summary(Model.ETG.Past7)
exp(cbind(OR = coef(Model.ETG.Past7), confint(Model.ETG.Past7)))
pR2(Model.ETG.Past7)
rocplot(Model.ETG.Past7) + SpTheme()





#Sample Descriptors
ETGData$Drink.Tot <- rowSums(subset(ETGData, select=c("ATLFB_01", "ATLFB_02", "ATLFB_03", "ATLFB_04", "ATLFB_05", "ATLFB_06", "ATLFB_07", "ATLFB_08", "ATLFB_09", "ATLFB_10",
                                                    "ATLFB_11", "ATLFB_12", "ATLFB_13", "ATLFB_14", "ATLFB_15", "ATLFB_16", "ATLFB_17", "ATLFB_18", "ATLFB_19", "ATLFB_20",
                                                    "ATLFB_21", "ATLFB_22", "ATLFB_23", "ATLFB_24", "ATLFB_25", "ATLFB_26", "ATLFB_27", "ATLFB_28", "ATLFB_29", "ATLFB_30")))
ETGData$Drink.Days <- rowSums(subset(ETGData, select=c("ATLFB_01", "ATLFB_02", "ATLFB_03", "ATLFB_04", "ATLFB_05", "ATLFB_06", "ATLFB_07", "ATLFB_08", "ATLFB_09", "ATLFB_10",
                                                     "ATLFB_11", "ATLFB_12", "ATLFB_13", "ATLFB_14", "ATLFB_15", "ATLFB_16", "ATLFB_17", "ATLFB_18", "ATLFB_19", "ATLFB_20",
                                                     "ATLFB_21", "ATLFB_22", "ATLFB_23", "ATLFB_24", "ATLFB_25", "ATLFB_26", "ATLFB_27", "ATLFB_28", "ATLFB_29", "ATLFB_30"))>0)
ETGData$DPD <- ETGData$Drink.Tot/30
ETGData$DPW <- ETGData$DPD * 7
ETGData$DPDD <- ETGData$Drink.Tot/ETGData$Drink.Days

SpDesc(ETGData[c("Drink.Tot", "Drink.Days", "DPD", "DPW", "DPDD")])

#Compute Binge Variables
ETGData$ATLFB_01F <- ETGData$ATLFB_01 + ETGData$Female
ETGData$ATLFB_02F <- ETGData$ATLFB_02 + ETGData$Female
ETGData$ATLFB_03F <- ETGData$ATLFB_03 + ETGData$Female
ETGData$ATLFB_04F <- ETGData$ATLFB_04 + ETGData$Female
ETGData$ATLFB_05F <- ETGData$ATLFB_05 + ETGData$Female
ETGData$ATLFB_06F <- ETGData$ATLFB_06 + ETGData$Female
ETGData$ATLFB_07F <- ETGData$ATLFB_07 + ETGData$Female
ETGData$ATLFB_08F <- ETGData$ATLFB_08 + ETGData$Female
ETGData$ATLFB_09F <- ETGData$ATLFB_09 + ETGData$Female
ETGData$ATLFB_10F <- ETGData$ATLFB_10 + ETGData$Female
ETGData$ATLFB_11F <- ETGData$ATLFB_11 + ETGData$Female
ETGData$ATLFB_12F <- ETGData$ATLFB_12 + ETGData$Female
ETGData$ATLFB_13F <- ETGData$ATLFB_13 + ETGData$Female
ETGData$ATLFB_14F <- ETGData$ATLFB_14 + ETGData$Female
ETGData$ATLFB_15F <- ETGData$ATLFB_15 + ETGData$Female
ETGData$ATLFB_16F <- ETGData$ATLFB_16 + ETGData$Female
ETGData$ATLFB_17F <- ETGData$ATLFB_17 + ETGData$Female
ETGData$ATLFB_18F <- ETGData$ATLFB_18 + ETGData$Female
ETGData$ATLFB_19F <- ETGData$ATLFB_19 + ETGData$Female
ETGData$ATLFB_20F <- ETGData$ATLFB_20 + ETGData$Female
ETGData$ATLFB_21F <- ETGData$ATLFB_21 + ETGData$Female
ETGData$ATLFB_22F <- ETGData$ATLFB_22 + ETGData$Female
ETGData$ATLFB_23F <- ETGData$ATLFB_23 + ETGData$Female
ETGData$ATLFB_24F <- ETGData$ATLFB_24 + ETGData$Female
ETGData$ATLFB_25F <- ETGData$ATLFB_25 + ETGData$Female
ETGData$ATLFB_26F <- ETGData$ATLFB_26 + ETGData$Female
ETGData$ATLFB_27F <- ETGData$ATLFB_27 + ETGData$Female
ETGData$ATLFB_28F <- ETGData$ATLFB_28 + ETGData$Female
ETGData$ATLFB_29F <- ETGData$ATLFB_29 + ETGData$Female
ETGData$ATLFB_30F <- ETGData$ATLFB_30 + ETGData$Female

ETGData$Binge.Days <- rowSums(subset(ETGData, select=c("ATLFB_01F", "ATLFB_02F", "ATLFB_03F", "ATLFB_04F", "ATLFB_05F", "ATLFB_06F", "ATLFB_07F", "ATLFB_08F", "ATLFB_09F", "ATLFB_10F",
                                                     "ATLFB_11F", "ATLFB_12F", "ATLFB_13F", "ATLFB_14F", "ATLFB_15F", "ATLFB_16F", "ATLFB_17F", "ATLFB_18F", "ATLFB_19F", "ATLFB_20F",
                                                     "ATLFB_21F", "ATLFB_22F", "ATLFB_23F", "ATLFB_24F", "ATLFB_25F", "ATLFB_26F", "ATLFB_27F", "ATLFB_28F", "ATLFB_29F", "ATLFB_30F"))>=5)
SpDesc(ETGData$Binge.Days)
SpHist(ETGData, variable="Binge.Days")

ETGData$Binge.Per <- ETGData$Binge.Days / ETGData$Drink.Days
SpDesc(ETGData$Binge.Per)
SpHist(ETGData, var="Binge.Per")




