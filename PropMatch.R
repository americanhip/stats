#only use this file if you know whats going on here. go to PropMatchCommented.R if you have ANY questions here
library(MatchIt)
library(ggplot2)

df <- read.csv(file = '10y40.csv', fileEncoding = 'UTF-8-BOM') 

newdata = read.csv ("10y40.csv", header = TRUE, fileEncoding="UTF-8-BOM") 
names(newdata)

set.seed(1) 

m.nnc <- matchit(Group ~ BMI + Sex + Tonnis + LCEA + Capsular.Treatment, data = df, method = "nearest", caliper = 0.1, ratio = 2)

summary(m.nnc)
match.data <- match.data(m.nnc)

wilcox.test(formula = BMI~Independent Variable Column Title, data = match.data, alternative = 'two.sided', paired=FALSE, var.equal=TRUE) # perform wilcox test

plot(m.nnc, type = 'jitter')

plot(m.nnc, type = 'hist')

match.data <- match.data(m.nnc)
write.csv(match.data, file = 'Matched.csv')
