library(MatchIt)
library(ggplot2)
#run these two lines code above to add the necessary libraries

df <- read.csv(file.choose())
#After you run this line of code, a file explorer window will appear which will allow you to select the file you wish to match. Make sure the chosen file is a csv.



newdata = read.csv(file.choose(), header = TRUE, fileEncoding="UTF-8-BOM")
#the same window will appear, just select the same file.

names(newdata)
#this will display all headers in csv file.

set.seed(1)

m.nnc <- matchit(GM.Treatment ~ BMI + Gender + Side + Age.at.Surgery + Approach, data=df, method = "nearest", caliper = 0.5, ratio = 1)
#"GM.Treatment" is your independent variable and should be changed to what your group is. Matching cannot occur if your group has not been converted to binary

summary(m.nnc)
#This line will display the match but the numbers are unimportant.

match.data <- match.data(m.nnc)
#

wilcox.test(formula = BMI~GM.Treatment, data = match.data, alternative = 'two.sided', paired=FALSE, car.equal=TRUE)
wilcox.test(formula = Gender~GM.Treatment, data = match.data, alternative = 'two.sided', paired=FALSE, car.equal=TRUE)
wilcox.test(formula = Side~GM.Treatment, data = match.data, alternative = 'two.sided', paired=FALSE, car.equal=TRUE)
wilcox.test(formula = Age.at.Surgery~GM.Treatment, data = match.data, alternative = 'two.sided', paired=FALSE, car.equal=TRUE)
wilcox.test(formula = Approach~GM.Treatment, data = match.data, alternative = 'two.sided', paired=FALSE, car.equal=TRUE)


plot(m.nnc, type = 'jitter')

plot(m.nnc, type = 'hist')

match.data <- match.data(m.nnc)

print(getwd())

setwd("C:/Users/teja.pidatala/Desktop")

write.csv(match.data, file = 'glutemedmatch.csv')

