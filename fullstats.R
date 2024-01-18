#only use this file if you know whats going on here. go to PropMatchCommented.R if you have ANY questions here
# install rtools and plyr for descriptive stats
library(MatchIt)
library(ggplot2)
library(plyr)

df <- read.csv(file = '10yrthadata.csv', fileEncoding = 'UTF-8-BOM') 

newdata = read.csv ("10yrthadata.csv", header = TRUE, fileEncoding="UTF-8-BOM") 
headers = names(newdata)

set.seed(1) 

# create dummy variables
df$sex_male <- ifelse(df$sex == "Male", 1, 0)
df$MAKO_True <- ifelse(df$MAKO == "Yes", 1, 0)
df$Side_L <- ifelse(df$Side == "L", 1, 0)
df$Approach_Posterior <- ifelse(df$Approach == "Posterior", 1, 0)

#match data
m.nnc <- matchit(Condition ~ BMI + sex_male + MAKO_True + Approach_Posterior + Side_L+ age.at.sx, data = df, method = "nearest", caliper = 0.5, ratio = 2)
summary(m.nnc)
match.data <- match.data(m.nnc)

#shapiro wilk test to test for normality

# perform wilcox test to test if any factors matched are confounding with applied condition
wilcox.test(formula = BMI~Condition, data = match.data, alternative = 'two.sided', paired=FALSE, var.equal=TRUE)# 0.5539
wilcox.test(formula = sex_male ~ Condition, data = match.data, alternative = 'two.sided', paired=FALSE, var.equal=TRUE) # 0.7048
wilcox.test(formula = MAKO_True ~Condition, data = match.data, alternative = 'two.sided', paired=FALSE, var.equal=TRUE) #0.263
wilcox.test(formula = Approach_Posterior~Condition, data = match.data, alternative = 'two.sided', paired=FALSE, var.equal=TRUE) #0.4842
wilcox.test(formula = Side_L~Condition, data = match.data, alternative = 'two.sided', paired=FALSE, var.equal=TRUE) #0.7734
wilcox.test(formula = age.at.sx~Condition, data = match.data, alternative = 'two.sided', paired=FALSE, var.equal=TRUE)

write.csv(match.data, file = 'Matched.csv')
#plot(m.nnc, type = 'jitter')

#plot(m.nnc, type = 'hist')

########################################### stats tiem
df <- read.csv(file = 'matchstats.csv', fileEncoding = 'UTF-8-BOM')

df_PA <- df[df$Condition == 1, ]
df_MC <- df[df$Condition == 0, ]

#check normality using shapiro wilk test, depending on that test for differences in population

#sink(statout.txt)

##### descriptive stats #####

#sudo
"""
waga baga bobo

run descriptive statistics for each variable
  mean
  SD
  frequency if necessary
  chi square OR t/wilcox to test variables to see significant difference

"""

### frequency
# no of hips
nrow(df_PA)
nrow(df_MC)
# sex
m <-cbind(table(df_PA$sex), table(df_MC$sex))
colnames(m) = c("PA", "MC")
m
chisq.test(m) #remove yates correction
# side
m <-cbind(table(df_PA$Side), table(df_MC$Side))
colnames(m) = c("PA", "MC")
m
chisq.test(m)
# approach
m <-cbind(table(df_PA$Approach), table(df_MC$Approach))
colnames(m) = c("PA", "MC")
m
chisq.test(m)
# MAKO
m <-cbind(table(df_PA$MAKO), table(df_MC$MAKO))
colnames(m) = c("PA", "MC")
m
chisq.test(m)

### continuous
# Age at sx
mean(df_PA$age.at.sx)
sd(df_PA$age.at.sx)
mean(df_MC$age.at.sx)
sd(df_MC$age.at.sx)

if ((shapiro.test(df_PA$age.at.sx)$p.value < 0.05) & (shapiro.test(df_MC$age.at.sx)$p.value < 0.05)) {
  t.test(df_PA$age.at.sx, df_MC$age.at.sx, paired = FALSE)
} else {
  wilcox.test(df_PA$age.at.sx, df_MC$age.at.sx, paired = FALSE)
}


# BMI
mean(df_PA$BMI)
sd(df_PA$BMI)
mean(df_MC$BMI)
sd(df_MC$BMI)

if ((shapiro.test(df_PA$BMI)$p.value < 0.05) & (shapiro.test(df_MC$BMI)$p.value < 0.05)) {
  t.test(df_PA$BMI, df_MC$BMI, paired = FALSE)
} else {
  wilcox.test(df_PA$BMI, df_MC$BMI, paired = FALSE)
}
# follow up time
mean(df_PA$FU.Time)
sd(df_PA$FU.Time)
mean(df_MC$FU.Time)
sd(df_MC$FU.Time)

if ((shapiro.test(df_PA$FU.Time)$p.value < 0.05) & (shapiro.test(df_MC$FU.Time)$p.value < 0.05)) {
  t.test(df_PA$FU.Time, df_MC$FU.Time, paired = FALSE)
} else {
  wilcox.test(df_PA$FU.Time, df_MC$FU.Time, paired = FALSE)
}

##### PROs #####

### continuous
# mHHS
mean(df_PA$mHHS)
sd(df_PA$mHHS)
mean(df_MC$mHHS)
sd(df_MC$mHHS)

if ((shapiro.test(df_PA$mHHS)$p.value < 0.05) & (shapiro.test(df_MC$mHHS)$p.value < 0.05)) {
  t.test(df_PA$mHHS, df_MC$mHHS, paired = FALSE)
} else {
  wilcox.test(df_PA$mHHS, df_MC$mHHS, paired = FALSE)
}

# HHS
mean(df_PA$HHS)
sd(df_PA$HHS)
mean(df_MC$HHS)
sd(df_MC$HHS)

if ((shapiro.test(df_PA$HHS)$p.value < 0.05) & (shapiro.test(df_MC$HHS)$p.value < 0.05)) {
  t.test(df_PA$HHS, df_MC$HHS, paired = FALSE)
} else {
  wilcox.test(df_PA$HHS, df_MC$HHS, paired = FALSE)
}

# FJS
mean(df_PA$FJS)
sd(df_PA$FJS)
mean(df_MC$FJS)
sd(df_MC$FJS)

if ((shapiro.test(df_PA$FJS)$p.value < 0.05) & (shapiro.test(df_MC$FJS)$p.value < 0.05)) {
  t.test(df_PA$FJS, df_MC$FJS, paired = FALSE)
} else {
  wilcox.test(df_PA$FJS, df_MC$FJS, paired = FALSE)
}

# VAS
mean(df_PA$VAS)
sd(df_PA$VAS)
mean(df_MC$VAS)
sd(df_MC$VAS)

if ((shapiro.test(df_PA$VAS)$p.value < 0.05) & (shapiro.test(df_MC$VAS)$p.value < 0.05)) {
  t.test(df_PA$VAS, df_MC$VAS, paired = FALSE)
} else {
  wilcox.test(df_PA$VAS, df_MC$VAS, paired = FALSE)
}
# Satisfaction
mean(df_PA$Satisfaction)
sd(df_PA$Satisfaction)
mean(df_MC$Satisfaction)
sd(df_MC$Satisfaction)

if ((shapiro.test(df_PA$Satisfaction)$p.value < 0.05) & (shapiro.test(df_MC$Satisfaction)$p.value < 0.05)) {
  t.test(df_PA$Satisfaction, df_MC$Satisfaction, paired = FALSE)
} else {
  wilcox.test(df_PA$Satisfaction, df_MC$Satisfaction, paired = FALSE)
}
# VR12-Mental
mean(df_PA$vr12mental)
sd(df_PA$vr12mental)
mean(df_MC$vr12mental)
sd(df_MC$vr12mental)

if ((shapiro.test(df_PA$vr12mental)$p.value < 0.05) & (shapiro.test(df_MC$vr12mental)$p.value < 0.05)) {
  t.test(df_PA$vr12mental, df_MC$vr12mental, paired = FALSE)
} else {
  wilcox.test(df_PA$vr12mental, df_MC$vr12mental, paired = FALSE)
}
# VR12-Physical
mean(df_PA$vr12physical)
sd(df_PA$vr12physical)
mean(df_MC$vr12physical)
sd(df_MC$vr12physical)

if ((shapiro.test(df_PA$vr12physical)$p.value < 0.05) & (shapiro.test(df_MC$vr12physical)$p.value < 0.05)) {
  t.test(df_PA$vr12physical, df_MC$vr12physical, paired = FALSE)
} else {
  wilcox.test(df_PA$vr12physical, df_MC$vr12physical, paired = FALSE)
}
# SF12-Mental
mean(df_PA$sf12mental)
sd(df_PA$sf12mental)
mean(df_MC$sf12mental)
sd(df_MC$sf12mental)

if ((shapiro.test(df_PA$sf12mental)$p.value < 0.05) & (shapiro.test(df_MC$sf12mental)$p.value < 0.05)) {
  t.test(df_PA$sf12mental, df_MC$sf12mental, paired = FALSE)
} else {
  wilcox.test(df_PA$sf12mental, df_MC$sf12mental, paired = FALSE)
}
# SF12-Physical
mean(df_PA$sf12physical)
sd(df_PA$sf12physical)
mean(df_MC$sf12physical)
sd(df_MC$sf12physical)

if ((shapiro.test(df_PA$sf12physical)$p.value < 0.05) & (shapiro.test(df_MC$sf12physical)$p.value < 0.05)) {
  t.test(df_PA$sf12physical, df_MC$sf12physical, paired = FALSE)
} else {
  wilcox.test(df_PA$sf12physical, df_MC$sf12physical, paired = FALSE)
}
# HOOS-JR
mean(df_PA$HOOSJR)
sd(df_PA$HOOSJR)
mean(df_MC$HOOSJR)
sd(df_MC$HOOSJR)

if ((shapiro.test(df_PA$HOOSJR)$p.value < 0.05) & (shapiro.test(df_MC$HOOSJR)$p.value < 0.05)) {
  t.test(df_PA$HOOSJR, df_MC$HOOSJR, paired = FALSE)
} else {
  wilcox.test(df_PA$HOOSJR, df_MC$HOOSJR, paired = FALSE)
}
