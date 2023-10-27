#only use this file if you know whats going on here. go to PropMatchCommented.R if you have ANY questions here
library(MatchIt)
library(ggplot2)
library(fastDummies)

#figure out header print ???? wai

df <- read.csv(file = '5yrdatafull.csv', fileEncoding = 'UTF-8-BOM') 

newdata = read.csv ("5yrdatafull.csv", header = TRUE, fileEncoding="UTF-8-BOM") 

print(names(newdata))

dummies <- c('Type.of.Surgery','Side','Sex','Labral.Treatment')

df_dummy <- dummy_cols(df, select_columns = dummies, remove_first_dummy = TRUE, remove_selected_columns = TRUE)

names(df_dummy)[names(df_dummy) == "Type.of.Surgery_Scope & PAO"] <- 'PAO'
names(df_dummy)[names(df_dummy) == "Labral.Treatment_Base Repair"] <- 'labtreatbase'
names(df_dummy)[names(df_dummy) == "Labral.Treatment_Debridement"] <- 'labtreatdeb'
names(df_dummy)[names(df_dummy) == "Labral.Treatment_Combined Simple and Base Repair"] <- 'labtreatsab'
names(df_dummy)[names(df_dummy) == "Labral.Treatment_No"] <- 'labtreatno'
names(df_dummy)[names(df_dummy) == "Labral.Treatment_Reconstruction"] <- 'labtreatrecon'
names(df_dummy)[names(df_dummy) == "Labral.Treatment_Resection"] <- 'labtreatresec'
names(df_dummy)[names(df_dummy) == "Labral.Treatment_Simple Repair"] <- 'labtreatsimp'

df_dummy <- df_dummy[!(is.na(df_dummy$BMI)), ]

names(df_dummy)

set.seed(1) 

m.nnc <- matchit(PAO ~ BMI + Side_R + Age.at.Sx + Sex_Male + labtreatbase + labtreatdeb + labtreatsab + labtreatno + labtreatrecon + labtreatresec, data = df_dummy, method = "nearest", caliper = 0.1, ratio = 2)

summary(m.nnc)
match.data <- match.data(m.nnc)

wilcox.test(formula = BMI~PAO, data = match.data, alternative = 'two.sided', paired=FALSE, var.equal=TRUE)
wilcox.test(formula = Side_R~PAO, data = match.data, alternative = 'two.sided', paired=FALSE, var.equal=TRUE)
wilcox.test(formula = Age.at.Sx~PAO, data = match.data, alternative = 'two.sided', paired=FALSE, var.equal=TRUE)
wilcox.test(formula = Sex_Male~PAO, data = match.data, alternative = 'two.sided', paired=FALSE, var.equal=TRUE)
wilcox.test(formula = labtreatbase~PAO, data = match.data, alternative = 'two.sided', paired=FALSE, var.equal=TRUE)
wilcox.test(formula = labtreatdeb~PAO, data = match.data, alternative = 'two.sided', paired=FALSE, var.equal=TRUE)
wilcox.test(formula = labtreatsab~PAO, data = match.data, alternative = 'two.sided', paired=FALSE, var.equal=TRUE)
wilcox.test(formula = labtreatno~PAO, data = match.data, alternative = 'two.sided', paired=FALSE, var.equal=TRUE)
wilcox.test(formula = labtreatrecon~PAO, data = match.data, alternative = 'two.sided', paired=FALSE, var.equal=TRUE)
wilcox.test(formula = labtreatresec~PAO, data = match.data, alternative = 'two.sided', paired=FALSE, var.equal=TRUE)

plot(m.nnc, type = 'jitter')

plot(m.nnc, type = 'hist')

write.csv(match.data, file = 'Matched.csv')

