#Kaplan Meier curve creator
#Dataframe should be follow up time and events, as well as any treatments
#install packages you don't have by copy pasting install packages line into the console

#install.packages('ggsurvfit', 'survival', 'ggplot2')

library(survival)
library(ggplot2)
library(ggsurvfit)

#change filename
df <- read.csv(file = 'filename.csv', fileEncoding = 'UTF-8-BOM')

#change FU.time to header of column for follow up time , event to header of column of whether event has happened or not
km <- with(df, Surv(FU.time, Event))

head(km, 28)

#change FU.time to header of column for follow up time , event to header of column of whether event has happened or not, change 1 to header of column of treatment if applicable. Keep 1 if only working with one population
km_fit <- survfit(Surv(FU.time, Event) ~ 1, data = df)

#Keep months -- that's how the database is formatted for dates
#Change title to title of your plot
survfit2(Surv(FU.time, Event) ~ 1, data = df) %>% 
  ggsurvfit() +
  labs(
    x = "Months",
    y = "Overall survival probability"
    title = 'title of plot'
  ) + 
  add_confidence_interval()

#performs a chi squared test to see if there's a difference between your populations if you need it below
#diff = survdiff(Surv(FU.time, Endpoint) ~ 1, data = df)
#pchisq(diff$chisq, length(diff$n)-1, lower.tail = FALSE)