source('statfunc.R')
df <- read.csv('data.csv')
df$Study.Group <- factor(df$Study.Group, levels = c(1, 0), ordered = TRUE)
levs <- c('mHHS','NAHS','HOS-SSS', 'VAS','Satisfaction')

df_IJ <- df[df$Study.Group == 1, ]
df_MC <- df[df$Study.Group == 0, ]

# read excels#####
dput(names(df))
'c("THA.UID", "SCOPE.UID", "MRN", "MAKO", "MAKO.Numeric", "Age.at.Sx",
"BMI", "Gender", "Gender.Numeric", "Study.Group", "distance",
"weights", "subclass", "DOS", "Side", "Approach", "MAKO2", "DOS3",
"Type", "FU.Date", "Time.from.Sx", "mHHS", "HHS", "FJS", "VAS",
"Satisfaction", "HOOS.JR", "Endpoint", "DOR", "Column1", "Chart.Review.Notes",
"Complication", "Complication.Review", "X2y.mHHS", "X2y.HHS",
"X2y.FJS", "X2y.VAS", "X2y.Satisfaction", "X2y.HOOS.JR")'
df_full <- read_excel('03.22.2024 THAFronthsheet.xlsx', sheet = "Data")
names(df_full) <- df_full[1,]
df_full <- df_full[-1, ]

#df$THA.UID <-  match(df2$Activity, df1$Activity)
df$Study.Group <- factor(df$Study.Group, levels = c(1, 0), ordered = TRUE)

# Tables #####
## demographics #####

myVars = c("Gender","Side","Approach", "MAKO","Age.at.Sx","BMI","Time.from.Sx")
catVars = c("Gender","Side","Approach", "MAKO")
CreateTableOne(vars = myVars, strata = 'Study.Group',factorVars = catVars, data = df)
mean(df_scope$`Time to Endpoint`, na.rm = TRUE)
sd(df_scope$`Time to Endpoint`, na.rm = TRUE)

## PROs #####
df$dmhhs <- with(df, (X10y.mHHS-Pre.mHHS))
df$dnahs <- with(df, (Pre.NAHS-Pre.NAHS))
df$dhos <- with(df, (X10y.HOS.SSS-Pre.HOS.SSS))
df$dvas <- with(df, (X10y.VAS-Pre.VAS))
df_pro <- df[df$Endpoint == "", ]
myVars = c("Pre.mHHS", "X10y.mHHS", "Pre.NAHS",
           "Latest", "X10y.IHOT", "Pre.HOS.SSS", "X10y.HOS.SSS", "Pre.VAS",
           "X10y.VAS", "X10y.SF.M", "X10y.SF.P", "X10y.VR.M", "X10y.VR.P",
           "X10y.Satisfaction", "dmhhs", "dhos", "dnahs","dvas")
CreateTableOne(data = df,vars = myVars, strata = 'Labral.Treatment' )
#use below if THA
# myVars = c("mHHS", "HHS", "FJS", "VAS", "Satisfaction","HOOS.JR")
# CreateTableOne(vars = myVars, strata = 'Study.Group',data = df_pro)

## Complications #####
myVars = c("Complication.Table")
#df_temp <- df[df$Complication.Table != 'numbness ', ]
CreateTableOne(vars = myVars, factorVars = myVars, strata = 'Study.Group',data = df)

## Revisions #####
df_rev <- df[df$Endpoint == 'Revision', ]
df_rev$Endpoint[df_rev$Endpoint] <- 'Revision'
table(df_rev$Study.Group, df_rev$Endpoint)

myVars = c("Non.Endpoint.1", "Time.to.NE","Endpoint","Time.to.End","Complication")
CreateTableOne(vars = myVars, factorVars = c("Non.Endpoint.1","Endpoint","Complication"), strata = 'Study.Group',data = df_rev)

# Durability #####
df <- read.csv('data.csv')
df$Study.Group <- factor(df$Study.Group, levels = c(1, 0), ordered = TRUE)

headers <- dput(names(df))
df<-rename( df,
            X2y_mHHS = X2y.mHHS,
            X5y_mHHS = mHHS,
            X5y_NAHS = NAHS,
            X5y_HOS.SSS = HOS.SSS,
            X5y_VAS = VAS,
            X5y_Satisfaction = Satisfaction,
            X2y_NAHS = X2y.NAHS,
            X2y_HOS.SSS = X2y.HOS.SSS,
            X2y_VAS = X2y.VAS,
            X2y_Satisfaction = X2y.Satisfaction,
)
dput(names(df))
df <- select(df, c("MRN","Study.Group","X5y_mHHS", "X5y_HHS", "X5y_FJS",
                   "X5y_VAS", "X5y_Satisfaction", "X5y_HOOS.JR", "X2y_mHHS", "X2y_HHS", "X2y_FJS", "X2y_VAS", "X2y_Satisfaction", "X2y_HOOS.JR"))
df_PA <- df[df$Study.Group == 1, ]
df_MC <- df[df$Study.Group == 0, ]

##df_PA #####
df_PA <- subset(df_PA, select=-c(Study.Group))
df_pivot <- pivot_longer(df_PA, cols = !MRN, names_to = c('Time', 'PRO'), values_to = 'Value', names_pattern = "X(.*)_(.*)")
df_pivot

df_pivot$Time <- factor(df_pivot$Time,
                        levels = c('2y','5y'))
df_pivot$PRO <- factor(df_pivot$PRO,
                       levels = c('mHHS','HHS','FJS', 'VAS','Satisfaction'))

for(i in c('mHHS','HHS','FJS', 'VAS','Satisfaction')){
  print(i)
  df_temp <- df_pivot[df_pivot$PRO == i, ]
  res.aov <- aov(Value ~ Time, data = df_temp)
  print(summary(res.aov))
}

title <- c()
value <- c()

for(i in c('mHHS','HHS','FJS', 'VAS','Satisfaction')){
  df_temp <- df_pivot[df_pivot$PRO == i, ]
  for(j in c('2y','5y')){
    df_temp2 <- df_temp[df_temp$Time == j, ]
    m <- mean(df_temp2$Value, na.rm = TRUE)
    header <- paste(i,j,sep="_")
    title <- append(title, header)
    value <- append(value, m)
  }
}
vals <- as.data.frame(rbind(title, value))
names(vals) <- as.character(unlist(vals[1,]))
vals <- vals[-1,]
vals
df_means <- pivot_longer(vals, cols = 1:10, names_to = c('PRO','Time'), values_to = 'Value', names_pattern = "(.*)_(.*)")
df_means['Condition'] = 'THA-PA'
df_means$Value <- as.numeric(df_means$Value)

##df_MC#####
df_MC <- subset(df_MC, select=-c(Study.Group))
df_pivot <- pivot_longer(df_MC, cols = !MRN, names_to = c('Time', 'PRO'), values_to = 'Value', names_pattern = "X(.*)_(.*)")
df_pivot

df_pivot$Time <- factor(df_pivot$Time,
                        levels = c('2y','5y'))
df_pivot$PRO <- factor(df_pivot$PRO,
                       levels = c('mHHS','HHS','FJS', 'VAS','Satisfaction'))
levs <- c('mHHS','HHS','FJS', 'VAS','Satisfaction')
for(i in levs){
  print(i)
  df_temp <- df_pivot[df_pivot$PRO == i, ]
  res.aov <- aov(Value ~ Time, data = df_temp)
  print(summary(res.aov))
}
title <- c()
value <- c()

for(i in c('mHHS','HHS','FJS', 'VAS','Satisfaction')){
  df_temp <- df_pivot[df_pivot$PRO == i, ]
  for(j in c('2y','5y')){
    df_temp2 <- df_temp[df_temp$Time == j, ]
    m <- mean(df_temp2$Value, na.rm = TRUE)
    header <- paste(i,j,sep="_")
    title <- append(title, header)
    value <- append(value, m)
  }
}
vals <- as.data.frame(rbind(title, value))
names(vals) <- as.character(unlist(vals[1,]))
vals <- vals[-1,]
vals
df_means2 <- pivot_longer(vals, cols = 1:10, names_to = c('PRO','Time'), values_to = 'Value', names_pattern = "(.*)_(.*)")
df_means2['Condition'] = 'THA-N-PA'
df_means2$Value <- as.numeric(df_means2$Value)
## plotsssss #######
df_plot <- rbind(df_means, df_means2)
names(df_plot)
#df_plot[df_plot$Condition == 1, ] <- 'Reconstruction'
#df_plot[df_plot$Condition == 0, ] <- 'Repair'
df_plot$Condition <- factor(df_plot$Condition, levels = c('THA-PA','THA-N-PA'), ordered = TRUE)
df_plot$PRO <- factor(df_plot$PRO, levels = c('mHHS','HHS','FJS', 'VAS','Satisfaction'), ordered = TRUE)
df_plot$Time <- factor(df_plot$Time, levels = c('2y','5y'), ordered = TRUE)

colors <- setNames(c("darkorange1", "dodgerblue2"), c('2y','5y'))

par(mfrow = c(1,4))
plot <- ggplot(data = df_plot[df_plot$PRO == 'mHHS', ], mapping = aes(x = Condition, y = Value, fill = Time)) +
  geom_bar(stat="identity", position = "dodge", )+ #+
  labs(
    x = 'mHHS'
  )+
  scale_fill_manual(values=colors)

plot2 <- ggplot(data = df_plot[df_plot$PRO == 'HHS', ], mapping = aes(x = Condition, y = Value, fill = Time)) +
  geom_bar(stat="identity", position = "dodge", )+ #+
  labs(
    x = 'HHS'
  )+
  scale_fill_manual(values=colors)
plot3 <- ggplot(data = df_plot[df_plot$PRO == 'FJS', ], mapping = aes(x = Condition, y = Value, fill = Time)) +
  geom_bar(stat="identity", position = "dodge", )+ #+
  labs(
    x = 'FJS'
  )+
  scale_fill_manual(values=colors)
plot4 <- ggplot(data = df_plot[df_plot$PRO == 'VAS', ], mapping = aes(x = Condition, y = Value, fill = Time)) +
  geom_bar(stat="identity", position = "dodge", )+ #+
  labs(
    x = 'VAS'
  )+
  scale_fill_manual(values=colors)
plot5 <- ggplot(data = df_plot[df_plot$PRO == 'Satisfaction', ], mapping = aes(x = Condition, y = Value, fill = Time)) +
  geom_bar(stat="identity", position = "dodge", )+ #+
  labs(
    x = 'Satisfaction'
  )+
  scale_fill_manual(values=colors)

(plot+plot2+plot3)/(plot4+plot5) + plot_layout(guides = 'collect')& theme(legend.position = 'right')


#PASS#####
df <- read.csv('data.csv')
df_eligible <- df[df$Endpoint == "", ]
df_IJ <- df_eligible[df_eligible$Study.Group == 1, ]
df_MC <- df_eligible[df_eligible$Study.Group == 0, ]
PROlist <- c(93,92.2,76.7)
studylevs <- c(1,0)
PASSlist <- c('HHS','FJS','HOOS.JR')
x<-pass(df_IJ, df_MC, PASSlist, PROlist, studylevs)

#MCID#####
df <- read.csv('data.csv')
df_eligible <- df[df$Endpoint == "", ]
df_IJ <- df_eligible[df_eligible$Study.Group == 1, ]
df_MC <- df_eligible[df_eligible$Study.Group == 0, ]
x <- MCID(df_IJ,df_MC, PROlist = c("mHHS","NAHS","HOS.SSS"),studylevs = c(1,0),time_prefix = "X10y.")

# kaplan meier #####
df <- read.csv(file = 'data.csv', fileEncoding = 'UTF-8-BOM')
names(df)
df$Study.Group[df$Study.Group == 1] <- 'Recon'
df$Study.Group[df$Study.Group == 0] <- 'Repair'

#change FU.time to header of column for follow up time , event to header of column of whether event has happened or not
df$Endpoint[df$Endpoint == "THA"] <- 1
df$Endpoint[df$Endpoint == "Resurfacing"] <- 1
df$Endpoint[df$Endpoint == ""] <- 0
df$Time.to.End <- as.numeric(df$Time.to.End)
df$Endpoint <- as.numeric(df$Endpoint)
km <- with(df, Surv(df$Time.to.End, df$Endpoint))


#head(km, 28)

#change FU.time to header of column for follow up time , event to header of column of whether event has happened or not, change 1 to header of column of treatment if applicable. Keep 1 if only working with one population
km_fit <- survfit(Surv(FU.time, Endpoint) ~ Study.Group, df)

#Keep months -- that's how the database is formatted for dates
#Change title to title of your plot

table(df$Study.Group, df$Endpoint)
plt <- survfit2(Surv(FU.time, Endpoint) ~ Study.Group, data = df) %>%
  ggsurvfit() +
  labs(
    x = "Months",
    y = "Overall survival probability"
    #title = 'Long-term Survival Analysis of THA Patients with a Previous Arthroscopy'
  ) +
  add_confidence_interval()+
  theme_gray()+
  add_pvalue(
    caption = "Log-rank {p.value}",
    location = c("caption")
  ) +
  theme(plot.margin = margin(1,1,0,0, unit = "cm"),
        legend.position = "bottom")+
  coord_cartesian(ylim = c(0.95, 1.01))
plt
#performs a chi squared test to see if there's a difference between your populations if you need it below
diff = survdiff(Surv(FU.time, Endpoint) ~ Condition, data = df)
pchisq(diff$chisq, length(diff$n)-1, lower.tail = FALSE)
