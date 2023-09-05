# load in libraries beforehand
library(MatchIt)
library(ggplot2)

# Change the name of the csv to your file's name
df <- read.csv(file = '10y40.csv', fileEncoding = 'UTF-8-BOM') # reads in csv into dataframe

newdata = read.csv ("10y40.csv", header = TRUE, fileEncoding="UTF-8-BOM") 
names(newdata) # reads in csv to see column headers

set.seed(1) #seeding randomizer

# compute propensity score
# Group should be changed to independent variable column title
# other parts should be changed to dependent variables
m.nnc <- matchit(Group ~ BMI + Sex + Tonnis + LCEA + Capsular.Treatment, data = df, method = "nearest", caliper = 0.1, ratio = 2)

summary(m.nnc) #print output of propensity scores
match.data <- match.data(m.nnc)
wilcox.test(formula = BMI~Independant Variable Column Title, data = match.data, alternative = 'two.sided', paired=FALSE, var.equal=TRUE) # perform wilcox test

#Jitter plot (remember to press escape) and histogram
plot(m.nnc, type = 'jitter')

plot(m.nnc, type = 'hist')

#Print CSV of matched groups
match.data <- match.data(m.nnc)
write.csv(match.data, file = 'Matched.csv')
