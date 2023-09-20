library(MatchIt)
library(ggplot2)
df <- read.csv(file.choose())

newdata = read.csv(file.choose(), header = TRUE, fileEncoding="UTF-8-BOM")

names(newdata)

set.seed(1)

m.nnc <- matchit(GM.Treatment ~ BMI + Gender + Side + Age.at.Surgery + Approach, data=df, method = "nearest", caliper = 0.5, ratio = 1)

summary(m.nnc)

match.data <- match.data(m.nnc)

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

