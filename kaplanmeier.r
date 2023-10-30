#Kaplan Meier curve creator
#dataframe should be follow up time and events
#install packages you don't have by copy pasting install packages line into the console

#install.packages('ggsurvfit', 'survival', 'ggplot2')

library(survival)
library(ggplot2)
library(ggsurvfit)

df <- read.csv(file = '5yrdataTHA.csv', fileEncoding = 'UTF-8-BOM')

km <- with(df, Surv(FU.time, Event))

head(km, 28)

km_fit <- survfit(Surv(FU.time, Event) ~ 1, data = df)
survfit2(Surv(FU.time, Event) ~ 1, data = df) %>% 
  ggsurvfit() +
  labs(
    x = "Months",
    y = "Overall survival probability"
    title = 'Time to THA on PAO patients'
  ) + 
  add_confidence_interval()

#plot(km_fit, xlab = 'months', ylab = 'survival probability', main = 'kaplan meyer plot')
