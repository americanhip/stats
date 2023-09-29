#run these two lines code below to load the necessary libraries. 
library(MatchIt)
library(ggplot2)

#if you don't have these libraries, delete the pound sign in front of the lines to uncomment the below lines and run them to install the libraries. you can also copy paste them into the terminal. select any mirror -- we've been using the USA (IA)mirror. and rerun the code from the beginning
#install.packages("MatchIt")
#install.packages("ggplot2")

#After you run this line of code, a file explorer window will appear which will allow you to select the file you wish to match. Make sure the chosen file is a csv.
df <- read.csv(file.choose())



#the same window will appear, just select the same file.
newdata = read.csv(file.choose(), header = TRUE, fileEncoding="UTF-8-BOM")

#this will display all headers in csv file.
names(newdata)

#this initializes a randomizer. dont worry about LOL
set.seed(1)

#"GM.Treatment" is your independent variable and should be changed to what your group is. Matching cannot occur if your group has not been converted to binary
m.nnc <- matchit(GM.Treatment ~ BMI + Gender + Side + Age.at.Surgery + Approach, data=df, method = "nearest", caliper = 0.5, ratio = 1)


summary(m.nnc)
#This line will display the match but the numbers are unimportant.

match.data <- match.data(m.nnc)

#the below performs a wilcox test on each factor and your independent variable
wilcox.test(formula = BMI~GM.Treatment, data = match.data, alternative = 'two.sided', paired=FALSE, car.equal=TRUE)
wilcox.test(formula = Gender~GM.Treatment, data = match.data, alternative = 'two.sided', paired=FALSE, car.equal=TRUE)
wilcox.test(formula = Side~GM.Treatment, data = match.data, alternative = 'two.sided', paired=FALSE, car.equal=TRUE)
wilcox.test(formula = Age.at.Surgery~GM.Treatment, data = match.data, alternative = 'two.sided', paired=FALSE, car.equal=TRUE)
wilcox.test(formula = Approach~GM.Treatment, data = match.data, alternative = 'two.sided', paired=FALSE, car.equal=TRUE)

#create jitter plot
plot(m.nnc, type = 'jitter')

#create histogram plot
plot(m.nnc, type = 'hist')

#map your results to a variable you can access
match.data <- match.data(m.nnc)

#prints your current workspace directory
print(getwd())

#the below changes your workspace directory to where you're saving your results csv
#change "teja.pidatala" below to firstname.lastname to save it to your desktop
#if you'd like it saved elsewhere, change Desktop to the path of your folder, like Documents or Downloads
#for example "C:\Users\Drashti.Sikligar\Documents\10 yr pao"
setwd("C:/Users/teja.pidatala/Desktop")

#the bottom prints your results to a csv file you can open
write.csv(match.data, file = 'glutemedmatch.csv')