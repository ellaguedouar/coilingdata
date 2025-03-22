#Start####
pacman::p_load(pacman, tidyverse, lmPerm, tidyr, dplyr, ggplot2, car, ggpattern)

LI = read.csv ("Data/LIdata.csv") #Snake data by ID
info = read.csv ("Data/Snake_info.csv") #includes sex
snake = read.csv ("Data/data 4.15.24.csv") #Raw data - individual records

#Clean Data frames
LI$logLI <- log(LI$LI)
df = left_join(LI, info, by = "ID" )
df_raw = left_join(snake, info, by = "ID" )
df_raw = left_join(df_raw, LI, by = "ID" )
df <- df %>% select(-Site.y)
df <- df %>% rename(Site = Site.x)
df_raw <- df_raw %>% rename(Temperature = Temp.C.)


#Sex####
#I used a two-way analysis of variance (ANOVA) to compare LIs based on the sex of an individual
shapiro.test(df$LI) #normally dist.
SexAOV <- aov(LI ~ Sex, data = df)
summary(SexAOV)

#average LI differs between males and females.

#Using binomial test test here to look within sexes for a preference, C = successes
Female <- df_raw[df_raw$Sex =="F", ]
table(Female$Handedness)
binom.test(58, nrow(Female))

Male <- df_raw[df_raw$Sex =="M", ]
table(Male$Handedness)
binom.test(48, nrow(Male))

#A lack of significance here suggests that, even if there is a slight difference between sexes (as shown by ANOVA), the proportions within each sex are not strongly biased toward one direction.

#Temperature####
#create categories for temperature
cut_number(df_raw$Temperature, 3)
#[18.2,28.3], (28.3,31] (31,37.5]

df_raw <- df_raw %>%
  mutate(TempCat = case_when(
   Temperature >=c(18.2 - 28.3) ~ "Low",
    Temperature >=c(28.3 - 31) ~ "Med",
    Temperature >=c(31 - 37.5) ~ "High",
    TRUE ~ "" # If temp is missing leave blank
  ))
df_raw$TempCat <- as.factor(df_raw$TempCat)

df_raw$Handedness <- factor(df_raw$Handedness, levels = c("A", "C"))
model <- glm(Handedness ~ TempCat, data = df_raw, family = binomial)
summary(model)

aovTempCat<-aov(Handedness ~ TempCat + Error(ID), data = df_raw) #not working, error: model is singular
summary(aovTempCat)

anova2 <- lm(df3$LI.x ~ TempCat, data = df3) #doesnt account for repeated measures
anova(anova2)
#Df  Sum Sq   Mean Sq F value Pr(>F)
#TempCat     1 0.00191 0.0019051  0.2342  0.629
#Residuals 200 1.62688 0.0081344   

library(nlme)
df3 <- df3[complete.cases(df3$LI), ]#remove rows with NA in LI
df3tempNullModel <- lme(LI ~ 1, random = ~ 1| ID, data = df3)
summary(df3tempNullModel) #lower AIC in null model
df3RBModel <- lme(LI ~ TempCat, random = ~ 1| ID, data = df3)
summary(df3RBModel)

