library(survival)
library(ggplot2)

df <- read.csv(file = '5yrdataTHA.csv', fileEncoding = 'UTF-8-BOM')

km <- with(df, Surv(FU.time, Event))

head(km, 28)

km_fit <- survfit(Surv(FU.time, Event) ~ 1, data = df)

plot(km_fit, xlab = 'months', main = 'kaplan meyer plot')
