#start####
pacman::p_load(pacman, tidyverse, lmPerm, tidyr, dplyr, ggplot2, car, ggpattern)


LI = read.csv ("Data/LIdata.csv")
info = read.csv ("Data/Snake_info.csv")
snake = read.csv ("Data/data 4.15.24.csv")
#transform

LI$logLI <- log(LI$LI)
df = left_join(LI, info, by = "ID" )

#Sex####
#I used a two-way analysis of variance (ANOVA) to compare LIs based on the sex of an individual

shapiro.test(df$LI) #normally dist.
SnakeAOV <- aov(LI ~ Sex, data = df)
summary(SnakeAOV)
plot(SnakeAOV)

#               Df   Sum Sq     Mean Sq   F value Pr(>F)  
#Sex             1    0.05565   0.05565   5.473    0.0475 *
#  Residuals     8    0.08135   0.01017                
#Signficant...

plot(SnakeAOV)

boxplot(LI ~ Sex, data
= df) 

#THIS DOES NOT ACCOUNT FOR INDIVIDUALS

#accounting for individual snakes:
SnakeRepeated <- aov(LI ~ Sex + Error(ID), data = df) #this dont matter they only appear once
summary(SnakeRepeated) #sig p - value

#two way accounting for indv.
SnakeRepeated2 <- aov(LI ~ Sex*Site.x + Error(ID), data = df)
summary(SnakeRepeated2) #no sig p value

#binom test, C = successes (df2 is created below)
Female <- df2[df2$Sex =="F", ]
table(Female$Handedness)
binom.test(58, nrow(Female))

Male <- df2[df2$Sex =="M", ]
table(Male$Handedness)
binom.test(48, nrow(Male))

#neither sexes had a significant coil preference

#Seasonal ####
#used a paired t test to examine LIs for seasonal differences. Seasons were classified as summer (June, July–August) and autumn (September–November). 
snake = read_csv ("Data/data 4.15.24.csv")

snake <- df3 %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y"),
         Season = case_when(
           month(Date) %in% c(6, 7, 8) ~ "Summer",
           month(Date) %in% c(9, 10, 11) ~ "Autumn",
           month(Date) %in% c(12, 1, 2) ~ "Winter",
           month(Date) %in% c(3, 4, 5) ~ "Spring",
           TRUE ~ "Unknown" # If month doesn't match any of the above
         ))
seasontable <- table(df3$Season, df3$Handedness)
chisq.test(seasontable)


#X-squared = 3.2658, df = 3, p-value = 0.3524

anovaseason <- aov(LI ~ Season + Error(ID), data = df3) #0.386
summary(anovaseason)

#By site:  (though it shouldnt matter)
df2 = left_join(snake, info, by = "ID" )

siteRB <- df2 [df2$Site %in% c("RB"), ]
seasontableRB <- table(siteRB$Season, siteRB$Handedness)
chisq.test(seasontableRB)
#X-squared = 2.9331, df = 3, p-value = 0.4021

siteFGCU <- df2 [df2$Site %in% c("FGCU"), ]
seasontableFGCU <- table(siteFGCU$Season, siteFGCU$Handedness)
chisq.test(seasontableFGCU)
#X-squared = 6.149, df = 3, p-value = 0.1046

#Repeated measures ANOVAs were used to test for LI differences due to temperature effects.

#Temperature####
#Temperature observations were placed into three groups: 18–22C, 23–27C and 28–33C. 
df3 = left_join(snake, LI, by = "ID" )


#create categories for temperature
cut_number(df3$`Temp(C)`, 3)
#[18.2,28.3], (28.3,31] (31,37.5]

df3 <- df3 %>%
  mutate(TempCat = case_when(
    `Temp(C)` >=c(18.2 - 28.3) ~ "Low",
    `Temp(C)` >=c(28.3 - 31) ~ "Med",
    `Temp(C)` >=c(31 - 37.5) ~ "High",
    TRUE ~ "" # If temp is missing leave blank
         ))
df3$TempCat <- as.factor(df3$TempCat)

df2 <- df2 %>%
  mutate(TempCat = case_when(
    `Temp(C)` >=c(18.2 - 28.3) ~ "Low",
    `Temp(C)` >=c(28.3 - 31) ~ "Med",
    `Temp(C)` >=c(31 - 37.5) ~ "High",
    TRUE ~ "" # If temp is missing leave blank
  ))
df2$TempCat <- as.factor(df2$TempCat)

#need column called visit
df3 <- df3 %>% 
  group_by(ID) %>%
 mutate(Visit = row_number())
#sum(is.na(df2$Handedness))


aovTempCat<-aov(LI ~ TempCat + Error(ID), data = df3) #not working, error: model is singular
summary(aovTempCat)
#Df Sum Sq Mean Sq F value Pr(>F)
#TempCat    1 0.1129  0.1129    0.58  0.468
#Residuals  8 1.5574  0.1947               

#Error: ID:TempCat
#Df Sum Sq Mean Sq
#TempCat  1 0.1747  0.1747

#Error: Within
#Df Sum Sq Mean Sq F value Pr(>F)
#Residuals 217  54.87  0.2529     

aovTempCat2<-aov(LI ~ TempCat +  
                  Error(ID), data = df3)
summary(aovTempCat2)
#           Df Sum Sq Mean Sq F value Pr(>F)
#TempCat    1 0.0736 0.07357   0.378  0.556

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

#time of day,  didnt vary that much. Temp is a fine comparison

####Regression (cont. var of temp)####
library(lme4)
df3$Handedness <- as.factor(df3$Handedness)

# Create dummy variables for Handedness
df3 <- within(df3, {
  HandNum <- ifelse(Handedness == "A", 1, 0)
  HandNum <- ifelse(Handedness == "C", 1, 0)
})

#Just not using categories here
Tempmodel <- lmer(HandNum ~ TempCat + (1 | Visit), data = df3)
summary(Tempmodel)

print(Tempmodel)

#Humidity####

#create categories for temperature
cut_number(df3$Hmidity, 3)
#[36.8,67, (67,78.7], (78.7,99.9]

df3 <- df3 %>%
  mutate(HCat = case_when(
    Hmidity >=c(36.8 - 67) ~ "Low",
    Hmidity >=c(67 - 78.7) ~ "Med",
    Hmidity >=c(78.7 - 99.9) ~ "High",
    TRUE ~ "" # If value is missing leave blank
  ))
df3$Hcat <- as.factor(df3$HCat)


aovHCat<-aov(LI ~ HCat + Error(ID), data = df3) #not working, error: model is singular
summary(aovHCat)
#Error: ID
#           Df Sum Sq Mean Sq F value Pr(>F)
#HCat       1 0.0775  0.0775    0.35  0.556
#Residuals  7 1.5513  0.2216               

#Error: ID:HCat
#       Df    Sum Sq   Mean Sq
#HCat  1 1.693e-31 1.693e-31

#Error: Within
#           Df    Sum Sq   Mean Sq F value Pr(>F)
#Residuals 192 2.928e-28 1.525e-30   


aovHCat2<-aov(LI ~ HCat + Error(ID), data = df3) #model is singular again
summary(aovHCat2)
#           Df Sum Sq Mean Sq F value Pr(>F)
#HCat       1 0.0775  0.0775    0.35  0.556
#Residuals  7 1.5513  0.2216     

Hanova2 <- lm(LI ~ Hcat, data = df3) #doesnt account for repeated measures
anova(Hanova2)
#Df  Sum Sq   Mean Sq F value Pr(>F)
#Hcat        1 0.00191 0.0019051  0.2342  0.629
#Residuals 200 1.62688 0.0081344  

df3HNullModel <- lme(LI ~ 1, random = ~ 1| ID, data = df3)
summary(df3HNullModel) #lower AIC in null model, again
df3RBModelH <- lme(LI ~ HCat, random = ~ 1| ID, data = df3)
summary(df3RBModelH)

intx <- aovp (LI ~ TempCat * HCat + (1|Visit), data = df3)
summary(intx)
#nothing

intx <- aov (LI ~ TempCat * HCat + (1|Visit), data = df3)
summary(intx)
#same nothing

#Location / Population ####

#chi sq
Sitetable <- table(df3$Site.x, df3$Handedness)
chisq.test(Sitetable)


#anova (permutated bc of unequal sample sizes)
permSite <- aov(df3$LI.x ~ df3$Site.x + Error(ID), data = df3)
summary(permSite)

siteNullModel <- lmp(LI.x ~ 1, random = ~ 1| ID, data = df3)
summary(siteNullModel) 
siteModel <- lmp(LI.x ~ Site.x, random = ~ 1| ID, data = df3)
summary(siteModel)
siteModel2 <- lmp(LI.x ~ Site.x, data = df3)
summary(siteModel2)#0.001091 = r2

#by site binom tests
#C = successes
FGCU <- df3[df3$Site.x =="FGCU", ]
RB <- df3[df3$Site.x =="RB", ]

table(FGCU$Handedness)
binom.test(81, n =nrow(FGCU)) #p-value = 0.3644
table(RB$Handedness)
binom.test(25, n = nrow(RB)) #p-value = 0.78

#Individual####

IDtable <- table(df3$ID, df3$Handedness)
chisq.test(IDtable) #i dont think this is right? But im really not sure what Roth did in a chi sq

#try this here
chisq.test(LI$LI, p = c(0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5))


#binom test (obs successes,  expect) - binomial data = handedness
#do this BY the individual
#successes = clockwise coils (C)

FGCU11 <- df3[df3$ID == 11, ]
FGCU12 <- df3[df3$ID == 12, ]
FGCU15 <- df3[df3$ID == 15, ]
FGCU16 <- df3[df3$ID == 16, ]
FGCU17 <- df3[df3$ID == 17, ]
FGCU18 <- df3[df3$ID == 18, ]
RB1 <- df3[df3$ID == 'RB1', ]
RB3 <- df3[df3$ID == 'RB3', ]
RB5 <- df3[df3$ID == "RB5", ]
RB6 <- df3[df3$ID == "RB6", ]

####with Power analyses####
install.packages("pwr")
library(pwr)

table(FGCU11$Handedness)
binom.test(14, nrow(FGCU11))
effect_size <- sqrt(nrow(FGCU11)) * abs(0.4117647 - 0.05)
power <- pbinom(qbinom(0.05, nrow(FGCU11), 0.05, lower.tail = FALSE), nrow(FGCU11), 0.4117647)
sample_size <- nrow(FGCU11)
observed_proportion <- 14 / sample_size # = 0.4117647
# Null hypothesis proportion (what you were testing against, likely 0.5)
null_proportion <- 0.5
# Calculate power
power_result <- pwr.p.test(h = ES.h(observed_proportion, null_proportion), 
                           n = sample_size, 
                           sig.level = 0.05, 
                           alternative = "two.sided")

power_result$power #= 0.1787136

table(FGCU12$Handedness)
binom.test(7, nrow(FGCU12))
effect_size <- sqrt(nrow(FGCU12)) * abs(0.35 - 0.05)
power <- pbinom(qbinom(0.05, nrow(FGCU12), 0.05, lower.tail = FALSE), nrow(FGCU12), 0.35)
sample_size <- nrow(FGCU12)
observed_proportion <- 7 / sample_size
null_proportion <- 0.5
power_result <- pwr.p.test(h = ES.h(observed_proportion, null_proportion), 
                           n = sample_size, 
                           sig.level = 0.05, 
                           alternative = "two.sided")
power_result$power #= 0.2755871

table(FGCU15$Handedness)
binom.test(25, nrow(FGCU15))
effect_size <- sqrt(nrow(FGCU15)) * abs(0.4807692 - 0.05)
power <- pbinom(qbinom(0.05, nrow(FGCU15), 0.05, lower.tail = FALSE), nrow(FGCU15), 0.4807692)
sample_size <- nrow(FGCU15)
observed_proportion <- 25 / sample_size
null_proportion <- 0.5
power_result <- pwr.p.test(h = ES.h(observed_proportion, null_proportion), 
                           n = sample_size, 
                           sig.level = 0.05, 
                           alternative = "two.sided")
power_result$power #0.05886223

table(FGCU16$Handedness)
binom.test(6, nrow(FGCU16))
effect_size <- sqrt(nrow(FGCU16)) * abs(0.4 - 0.05)
power <- pbinom(qbinom(0.05, nrow(FGCU16), 0.05, lower.tail = FALSE), nrow(FGCU16), 0.4)
sample_size <- nrow(FGCU16)
observed_proportion <- 6 / sample_size
null_proportion <- 0.5
power_result <- pwr.p.test(h = ES.h(observed_proportion, null_proportion), 
                           n = sample_size, 
                           sig.level = 0.05, 
                           alternative = "two.sided")
power_result$power #0.1220523

table(FGCU17$Handedness)
binom.test(22, nrow(FGCU17))
effect_size <- sqrt(nrow(FGCU17)) * abs(0.5116279 - 0.05)
power <- pbinom(qbinom(0.05, nrow(FGCU17), 0.05, lower.tail = FALSE), nrow(FGCU17), 0.5116279)
sample_size <- nrow(FGCU17)
observed_proportion <- 22 / sample_size
null_proportion <- 0.5
power_result <- pwr.p.test(h = ES.h(observed_proportion, null_proportion), 
                           n = sample_size, 
                           sig.level = 0.05, 
                           alternative = "two.sided")
power_result$power #0.05266875

table(FGCU18$Handedness)
binom.test(7, nrow(FGCU18))
effect_size <- sqrt(nrow(FGCU18)) * abs(0.6363636 - 0.05)
power <- pbinom(qbinom(0.05, nrow(FGCU18), 0.05, lower.tail = FALSE), nrow(FGCU18), 0.6363636)

sample_size <- nrow(FGCU18)
observed_proportion <- 7 / sample_size
null_proportion <- 0.5
power_result <- pwr.p.test(h = ES.h(observed_proportion, null_proportion), 
                           n = sample_size, 
                           sig.level = 0.05, 
                           alternative = "two.sided")
power_result$power #0.1502965

table(RB1$Handedness)
binom.test(5, nrow(RB1))
effect_size <- sqrt(nrow(RB1)) * abs(0.625 - 0.05)
power <- pbinom(qbinom(0.05, nrow(RB1), 0.05, lower.tail = FALSE), nrow(RB1), 0.625)

sample_size <- nrow(RB1)
observed_proportion <- 5 / sample_size
null_proportion <- 0.5
power_result <- pwr.p.test(h = ES.h(observed_proportion, null_proportion), 
                           n = sample_size, 
                           sig.level = 0.05, 
                           alternative = "two.sided")
power_result$power #0.1102554

table(RB3$Handedness)
binom.test(12, nrow(RB3))
b <- binom.test(12, nrow(RB3))
b$p.value/0.5
sample_size <- nrow(RB3)
observed_proportion <- 12 / sample_size
null_proportion <- 0.5
power_result <- pwr.p.test(h = ES.h(observed_proportion, null_proportion), 
                           n = sample_size, 
                           sig.level = 0.05, 
                           alternative = "two.sided")
power_result$power # 0.06783891

table(RB5$Handedness)
binom.test(3, nrow(RB5))#LOW SAMPLE SIZE - need to check assumptions
b <- binom.test(3, nrow(RB5))
b$p.value/0.5

sample_size <- nrow(RB5)
observed_proportion <- 3 / sample_size
null_proportion <- 0.5
power_result <- pwr.p.test(h = ES.h(observed_proportion, null_proportion), 
                           n = sample_size, 
                           sig.level = 0.05, 
                           alternative = "two.sided")
power_result$power #0.3466423

table(RB6$Handedness)
binom.test(5, nrow(RB6))
b <- binom.test(5, nrow(RB6))
b$p.value/0.5
#1.453125 = RR (effect size)

sample_size <- nrow(RB6)
observed_proportion <- 5 / sample_size
null_proportion <- 0.5
power_result <- pwr.p.test(h = ES.h(observed_proportion, null_proportion), 
                           n = sample_size, 
                           sig.level = 0.05, 
                           alternative = "two.sided")
power_result$power #0.1102554

#none are significant
#"probability of success" is = to the laterality index 

#Relative risk (effect size) #####
#prob of success / 0.5 (or whatever odds we're using)
#this means treatment/procedure has a relative risk of 0.42 for the outcome being clockwise coiled 

#RR = 1 means that exposure does not affect the outcome
#RR < 1 means that the risk of the outcome is decreased by the exposure, which is a "protective factor"
#RR > 1 means that the risk of the outcome is increased by the exposure, which is a "risk factor"

#SVL####
morphos = read.csv("Data/morphos.csv")
SVL = left_join(LI, morphos, by = "ID" )
plot(data = SVL, LI~SVL)
SVLreg <- lm(LI~SVL, data=SVL) 
abline(SVLreg, lwd=2)
summary(SVLreg) #Not signficant (p = 0.7541)

#Descriptive Stats####
mean(table(snake$ID))
sd(table(snake$ID))

#Figures####
boxplot(LI ~ Sex, data = df) 

library(ggplot2)

library(ggplot2)

library(ggplot2)

ggplot(df, aes(x = Sex, y = LI, fill = Sex)) +
  geom_boxplot() +
  scale_fill_manual(
    values = c("dodgerblue4", "mediumaquamarine"),
    labels = c("Female", "Male")  # Custom legend labels
  ) +
  scale_y_continuous(
    limits = c(0, 1),  # Set y-axis range
    breaks = seq(0, 1, 0.1)  # Define intervals at 0.1
  ) +
  geom_hline(
    yintercept = 0.5,  # Horizontal line at 0.5
    linetype = "dashed",  # Dashed line style
    color = "black",
    size = 0.5
  ) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",  # Position the legend below the chart
    legend.title = element_blank()  # Remove legend title
  ) +
  labs(
    x = "Sex",
    y = "Laterality Index"
  )


boxplot(LI ~ Sex*Site.x, data = df)
boxplot (LI ~ Site.x, data = df)
boxplot(LI ~ ID, data = SVL)+
        abline(a = 0.5, b = 0)
df$zeroed <- df$LI - 0.5
barplot(zeroed ~ ID, data = SVL, angle = 45)
ggplot(SVL, aes(ID, zeroed)) +
  geom_bar(stat = "identity") +
  coord_cartesian(xlim = c(-max(abs(SVL$zeroed)), max(abs(SVL$zeroed)))) +
  theme(axis.text.x = element_text(hjust = 0.5))+ 
  coord_flip()# center x-axis labels
  
df$population <- paste(df$Sex, df$Site.x, sep = " ")


ggplot(df, aes(ID, zeroed, fill = factor(Sex))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(-0.25, 0.25)) +
  coord_flip() +
  theme_bw() +  # Use black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(hjust = 0.5)) + 
  scale_fill_manual(values = c("F" = "gray70", "M" = "gray10")) +
  geom_hline(yintercept = 0,size = 0.2) 
  

library(ggplot2)
library(viridis) #colors
library(forcats) #fct_reorder

df$ID <- fct_reorder(df$ID, df$population)

custom_y_breaks <- c(-0.5,-0.4,-0.3,-0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5)  # Define custom breaks
custom_y_labels <- c("0.00","0.10","0.20","0.30", "0.40", "0.50", "0.60", "0.70", "0.80", "0.90", "1.00") 
legend_labels <- c("FGCU Females", "RB Females", "FGCU Males", "RB Males")



ggplot(df, aes(ID, zeroed, fill = factor(population))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(
    limits = c(-0.5,0.5), 
    breaks = custom_y_breaks, 
    labels = custom_y_labels  
  ) +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y= element_blank(), 
    axis.ticks.y = element_blank(),
    legend.position = "bottom", 
    legend.title = element_text(size = 10, family = "Times New Roman"), 
    legend.text = element_text(size = 8, family = "Times New Roman"),
    plot.margin = margin(10,10, 10, 10),
    legend.key = element_rect(fill = "white", colour = "black"),
    axis.text.x = element_text(family = "Times New Roman"),
    axis.title.x = element_text(family = "Times New Roman"),
    axis.title.y = element_text(family = "Times New Roman"),
    legend.key.size = unit(0.5, "lines"),
    plot.title = element_text(size = 12, hjust = 0.5, family = "Times New Roman"),  # Center the title and set font
    plot.title.position = "plot") +
  scale_fill_viridis_d(option = "mako", name = "Population:",
                       labels = legend_labels,
                       guide = guide_legend(nrow = 1, byrow = TRUE)) + #can change to 2 rows
  geom_hline(yintercept = 0, size = 0.2) +
  labs(
    title = "Coil Preference of Snake Populations", 
    x = "Snake",     
    y = "Laterality Index")


#ROTH DATA####
"roth" <- read.csv("Data/RothData.csv")
head(roth)

#sex
femaleR <- roth[roth$Sex =="F", ]
maleR <- roth[roth$Sex =="M", ]

binom.test(sum(femaleR$CW), n =sum(femaleR$TOTAL)) #p-value = 0.0002
  #Females prefer CW
binom.test(sum(maleR$CW), n =sum(maleR$TOTAL)) #p-value = 0.97

#age
juv <- roth[roth$Age =="Juv", ]
ad <- roth[roth$Age =="Adult", ]

binom.test(sum(juv$CW), n =sum(juv$TOTAL)) #p-value = not sig
binom.test(sum(ad$CW), n =sum(ad$TOTAL)) #p-value = p < 0.001
  #adults prefer CW

sum(nrow(df$CW))
   
