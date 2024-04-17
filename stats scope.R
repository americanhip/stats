source('statfunc.R')
df <- read.csv('Matched.csv')


#data####
df$Diabetic. <- factor(df$Diabetic., levels = c(1, 0), ordered = TRUE)
levs <- c('mHHS','NAHS','HOS-SSS', 'VAS','Satisfaction')

df_IJ <- df[df$Diabetic. == 1, ]
df_MC <- df[df$Diabetic. == 0, ]

# read excels#####
dput(names(df))
'c("X.1", "UID", "MR..", "Side", "WC", "Age.at.Sx", "Female",
  "Sex", "BMI", "Seldes.Tear.Type", "ALAD.Grade", "A.Outerbridge",
  "FH.Outerbridge", "LT.Percentile", "Villar.Class", "Troch.Bursitis",
  "Loose.Bodies", "Labral.Treatment", "Reconstruction", "Repair",
  "Debridement", "Capsular.Treatment", "Release", "Repair.1", "Acetabuloplasty",
  "Femoroplasty", "X", "A.Microfracture", "FH.Microfracture", "LT.Treatment",
  "Iliopsoas.Release", "Troch.Bursectomy", "GM.Repair", "Notchplasty",
  "FU.time", "Pre.mHHS", "X.2y.mHHS", "Pre.NAHS", "X.2y.NAHS",
  "Pre.IHOT", "X2y.IHOT", "Pre.HOS.SSS", "X.2y.HOS.SSS", "Pre.VAS",
  "X.2y.VAS", "X.2y.Satisfaction", "Tonnis.Grade..Pre.op.", "Lateral.CEA..Pre.op.",
  "Anterior.CEA..Pre.op.", "Acetabular.Inclination..Pre.op.", "Alpha.Angle..Pre.op.",
  "Tonnis.Grade..Post.op.", "Lateral.CEA...Post.op.", "Anterior.CEA..Post.op.",
  "Acetabular.Inclination..Post.op.", "Alpha.Angle..Post.op.",
  "Complications", "Non.Endpoint.1", "Time.to.NE.1.Sx", "Endpoint",
  "Time.to.End", "Diabetic.", "Type1", "Typ2", "distance", "weights",
  "subclass")'

# df_full <- read_excel('03.22.2024 THAFronthsheet.xlsx', sheet = "Data")
# names(df_full) <- df_full[1,]
# df_full <- df_full[-1, ]

#df$THA.UID <-  match(df2$Activity, df1$Activity)
#df$Study.Group <- factor(df$Study.Group, levels = c(1, 0), ordered = TRUE)

# Tables #####
## demographics #####
myVars = c("Side", "WC", "Age.at.Sx", "Sex", "BMI","FU.time")
catVars = c("Side", "WC","Sex")
CreateTableOne(vars = myVars, strata = 'Diabetic.',factorVars = catVars, data = df)

mean(df_scope$`Time to Endpoint`, na.rm = TRUE)
sd(df_scope$`Time to Endpoint`, na.rm = TRUE)

## radiographic findings #####
myVars = c("Tonnis.Grade..Pre.op.", "Lateral.CEA..Pre.op.",
"Anterior.CEA..Pre.op.", "Acetabular.Inclination..Pre.op.", "Alpha.Angle..Pre.op.",
"Tonnis.Grade..Post.op.", "Lateral.CEA...Post.op.", "Anterior.CEA..Post.op.",
"Acetabular.Inclination..Post.op.", "Alpha.Angle..Post.op.")
catVars = c("Tonnis.Grade..Pre.op.","Tonnis.Grade..Post.op.")
CreateTableOne(vars = myVars, factorVars = catVars, strata = 'Diabetic.', data = df)

## intraop findings #####
myVars = c("Seldes.Tear.Type", "ALAD.Grade", "A.Outerbridge", "FH.Outerbridge")
CreateTableOne(data = df,vars = myVars, factorVars = myVars, strata = 'Diabetic.')

## intraop procedures #####
myVars = c("Labral.Treatment","Acetabuloplasty", "Femoroplasty", "LT.Treatment", "Capsular.Treatment", "A.Microfracture", "FH.Microfracture", "Iliopsoas.Release","Loose.Bodies","Notchplasty")
df$LT.Treatment[df$LT.Treatment == ""] <- "None"
df$A.Microfracture[df$A.Microfracture == ""] <- "No"
df$FH.Microfracture[df$FH.Microfracture == ""] <- "No"
df$Iliopsoas.Release[df$Iliopsoas.Release == ""] <- "No"
df$Notchplasty[df$Notchplasty == ""] <- "No"
CreateTableOne(data = df,vars = myVars, factorVars = myVars, strata = 'Diabetic.')


## PROs #####
df$dmhhs <- with(df, (X.2y.mHHS-Pre.mHHS))
df$dnahs <- with(df, (X.2y.NAHS-Pre.NAHS))
df$dhos <- with(df, (X.2y.HOS.SSS-Pre.HOS.SSS))
df$dvas <- with(df, (X.2y.VAS-Pre.VAS))
df$dihot <- with(df, (X2y.IHOT - Pre.IHOT))
df_pro <- df[df$Endpoint == "", ]
df_pro <- df_pro[df_pro$Non.Endpoint.1 == "", ]
myVars = c("Pre.mHHS", "X.2y.mHHS", "dmhhs","Pre.NAHS", "X.2y.NAHS",
           "dnahs", "Pre.IHOT", "X2y.IHOT", "dihot", "Pre.HOS.SSS", "X.2y.HOS.SSS","dhos", "Pre.VAS",
           "X.2y.VAS", "dvas", "X.2y.Satisfaction")
CreateTableOne(data = df_pro,vars = myVars, strata = 'Diabetic.')
#use below if THA
# myVars = c("mHHS", "HHS", "FJS", "VAS", "Satisfaction","HOOS.JR")
# CreateTableOne(vars = myVars, strata = 'Study.Group',data = df_pro)

## Complications #####
myVars = c("Complication.Rate","Complications")
#df_temp <- df[df$Complication.Table != 'numbness ', ]
CreateTableOne(vars = myVars, factorVars = myVars, strata = 'Diabetic.',data = df)

## Revisions #####

myVars = c("Non.Endpoint.1", "Time.to.NE.1.Sx","Endpoint","Time.to.End","Complications")
CreateTableOne(vars = myVars, factorVars = c("Non.Endpoint.1","Endpoint","Complications"), strata = 'Diabetic.',data = df)

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
plot <- ggplot(data = df_mhhs, mapping = aes(x=Time, y=Mean, color = "mHHS")) +
  geom_point(aes(shape = Group, color = "mHHS")) +
  geom_line(data = df_mhhs, aes(group=Group))+
  #geom_line(aes(x=Time, y=Mean, group = Group)) +
  geom_point(data = df_nahs, mapping = aes(x=Time, y=Mean, shape = Group, color = "NAHS")) +
  geom_line(data = df_nahs, aes(group=Group, color = "NAHS"))+
  geom_point(data = df_hos, mapping = aes(x=Time, y=Mean, shape = Group, color = "HOS-SSS")) +
  geom_line(data = df_hos, aes(group=Group, color = "HOS-SSS"))+
  geom_point(data = df_vas, mapping = aes(x=Time, y=Mean, color = "VAS")) +
  geom_line(data = df_vas, aes(group=Group, color = "VAS"))+
  geom_point(data = df_sat, mapping = aes(x=Time, y=Mean, shape = Group, color = "Satisfaction")) +
  geom_line(data = df_sat, aes(group=Group, color = "Satisfaction"))+
  scale_y_break(c(9.5,60))

(plot+plot2+plot3)/(plot4+plot5) + plot_layout(guides = 'collect')& theme(legend.position = 'right')


#PASS#####
df <- read.csv('Matched.csv')
df_eligible <- df[df$Endpoint == "", ]
df_IJ <- df_eligible[df_eligible$Study.Group == 1, ]
df_MC <- df_ligible[df_eligible$Study.Group == 0, ]
PASSlist <- c(83.25,85.6,76.4,72.2)
studylevs <- c('Diabetic','Non-diabetic')
PROlist <- c("mHHS","NAHS","HOS.SSS","IHOT")
x2<-pass(df_IJ, df_MC, PASSlist, PROlist, studylevs, time_prefix = 'X.2y.')

#MCID#####
df <- read.csv('Matched.csv')
df$dmHHS <- with(df, (X.2y.mHHS-Pre.mHHS))
df$dNAHS <- with(df, (X.2y.NAHS-Pre.NAHS))
df$dHOS.SSS <- with(df, (X.2y.HOS.SSS-Pre.HOS.SSS))
df$dVAS <- with(df, (X.2y.VAS-Pre.VAS))
df$dIHOT <- with(df, (X2y.IHOT - Pre.IHOT))
df$X.2y.IHOT <- df$X2y.IHOT
df_eligible <- df[df$Endpoint == "", ]
df_IJ <- df_eligible[df_eligible$Diabetic. == 1, ]
df_MC <- df_eligible[df_eligible$Diabetic. == 0, ]
df_IJ$Diabetic.[df_IJ$Diabetic. == 1] <- 'Diabetic'
df_MC$Diabetic.[df_MC$Diabetic. == 0] <- 'Non-diabetic'
x <- MCID(df_IJ,df_MC, PROlist = c("mHHS","NAHS","HOS.SSS","VAS","IHOT"),studylevs = c('Diabetic','Non-diabetic'),time_prefix = "X.2y.")
x + labs(
  x = 'PRO',
  y = 'Percentage of hips meeting MCID'
)

# MOI #####
df <- read.csv('Matched.csv')
df_eligible <- df[df$Endpoint == "", ]
df_IJ <- df_eligible[df_eligible$Diabetic. == 1, ]
df_MC <- df_eligible[df_eligible$Diabetic. == 0, ]
MOIlist <- c('70','80','90')
PROlist <- c("mHHS","NAHS","HOS.SSS")
x<-MOI(df_IJ, df_MC, MOIlist, PROlist, studylevs, time_prefix = "X.2y.")

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
