source('statfunc.R')
library(plyr)
library(dplyr)#dyplyr MUST come after plyr to avoid masking
library(tableone)
library(ggplot2)
library(tidyverse)

df <- read.csv("Matched.csv")
names(df)
df<-rename( df,
             X2y_mHHS = X.2y.mHHS,
             X5y_mHHS = X5y.MHHS,
             X10y_mHHS = X10y.mHHS,
             X2y_NAHS = X.2y.NAHS,
             X5y_NAHS = X5y.NAHS,
             X10y_NAHS = X10y.NAHS,
             X2y_HOS = X.2y.HOS.SSS,
             X5y_HOS = X5y.HOS.SSS,
             X10y_HOS = X10y.HOS.SSS,
             X2y_VAS = X.2y.VAS,
             X5y_VAS = X5y.VAS,
             X10y_VAS = X10y.VAS,
             X2y_Satisfaction =X.2y.Satisfaction,
             X5y_Satisfaction = X5y.Satisfaction,
             X10y_Satisfaction= X10y.Satisfaction
)
dput(names(df))
df <- select(df, c("MR..","Study","X5y_mHHS", "X10y_mHHS", "X2y_mHHS",
                   "X2y_NAHS", "X5y_NAHS", "X10y_NAHS",
                   "X2y_HOS", "X5y_HOS", "X10y_HOS","X2y_VAS", "X5y_VAS",
                   "X10y_VAS", "X2y_Satisfaction", "X5y_Satisfaction", "X10y_Satisfaction"))
df_PA <- df[df$Study == 1, ]
df_MC <- df[df$Study == 0, ]

#df_PA #####
df_pivot <- pivot_longer(df_PA, cols = !MR.., names_to = c('Time', 'PRO'), values_to = 'Value', names_pattern = "X(.*)_(.*)")
df_pivot

df_pivot$Time <- factor(df_pivot$Time,
                        levels = c('2y','5y','10y'))
df_pivot$PRO <- factor(df_pivot$PRO,
                       levels = c('mHHS','NAHS','HOS', 'VAS','Satisfaction'))

for(i in c('mHHS','NAHS','HOS', 'VAS','Satisfaction')){
  print(i)
  df_temp <- df_pivot[df_pivot$PRO == i, ]
  res.aov <- aov(Value ~ Time, data = df_temp)
  print(summary(res.aov))
}

title <- c()
value <- c()

for(i in c('mHHS','NAHS','HOS', 'VAS','Satisfaction')){
  df_temp <- df_pivot[df_pivot$PRO == i, ]
  for(j in c('2y','5y','10y')){
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
df_means <- pivot_longer(vals, cols = 1:15, names_to = c('PRO','Time'), values_to = 'Value', names_pattern = "(.*)_(.*)")
df_means['Condition'] = 'Reconstruction'
df_means$Value <- as.numeric(df_means$Value)

##df_MC#####

df_pivot <- pivot_longer(df_MC, cols = !MR.., names_to = c('Time', 'PRO'), values_to = 'Value', names_pattern = "X(.*)_(.*)")
df_pivot

df_pivot$Time <- factor(df_pivot$Time,
                        levels = c('2y','5y','10y'))
df_pivot$PRO <- factor(df_pivot$PRO,
                       levels = c('mHHS','NAHS','HOS', 'VAS','Satisfaction'))
levs <- c('mHHS','NAHS','HOS', 'VAS','Satisfaction')
for(i in levs){
  print(i)
  df_temp <- df_pivot[df_pivot$PRO == i, ]
  res.aov <- aov(Value ~ Time, data = df_temp)
  print(summary(res.aov))
}
title <- c()
value <- c()

for(i in c('mHHS','NAHS','HOS', 'VAS','Satisfaction')){
  df_temp <- df_pivot[df_pivot$PRO == i, ]
  for(j in c('2y','5y','10y')){
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
df_means2 <- pivot_longer(vals, cols = 1:15, names_to = c('PRO','Time'), values_to = 'Value', names_pattern = "(.*)_(.*)")
df_means2['Condition'] = 'Repair'
df_means2$Value <- as.numeric(df_means2$Value)
## plotsssss #######
df_plot <- rbind(df_means, df_means2)
names(df_plot)
#df_plot[df_plot$Condition == 1, ] <- 'Reconstruction'
#df_plot[df_plot$Condition == 0, ] <- 'Repair'
df_plot$Condition <- factor(df_plot$Condition, levels = c('Reconstruction','Repair'), ordered = TRUE)
df_plot$PRO <- factor(df_plot$PRO, levels = c('mHHS','NAHS','HOS','VAS','Satisfaction'), ordered = TRUE)
df_plot$Time <- factor(df_plot$Time, levels = c('2y','5y','10y'), ordered = TRUE)

colors <- setNames(c("darkorange1", "dodgerblue2", "yellowgreen"), c('2y','5y','10y'))

par(mfrow = c(1,4))
plot <- ggplot(data = df_plot[df_plot$PRO == 'mHHS', ], mapping = aes(x = Condition, y = Value, fill = Time)) +
  geom_bar(stat="identity", position = "dodge", )+ #+
  labs(
    x = 'mHHS'
  )+
  scale_fill_manual(values=colors)

plot2 <- ggplot(data = df_plot[df_plot$PRO == 'NAHS', ], mapping = aes(x = Condition, y = Value, fill = Time)) +
  geom_bar(stat="identity", position = "dodge", )+ #+
  labs(
    x = 'NAHS'
  )+
  scale_fill_manual(values=colors)
plot3 <- ggplot(data = df_plot[df_plot$PRO == 'HOS', ], mapping = aes(x = Condition, y = Value, fill = Time)) +
  geom_bar(stat="identity", position = "dodge", )+ #+
  labs(
    x = 'HOS-SSS'
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

#scale_x_discrete(limits = c('OB', 'MC'))

library(patchwork)
(plot+plot2+plot3)/(plot4+plot5) + plot_layout(guides = 'collect')& theme(legend.position = 'right')

###Durability#####
"
sudo
read in data
names
rename columns
select data we want
get means and create table with those
plot
run anovas on data
"
# MCID pass SCB #######
df <- read.csv('Matched.csv')
df <- df[df$Endpoint == "", ]
df <- df[df$Non.Endpoint.1 == "", ]



df$dmhhs <- with(df, (X10y.mHHS-Pre.mHHS))
df$dnahs <- with(df, (X10y.NAHS-Pre.NAHS))
df$dhos <- with(df, (X10y.HOS.SSS-Pre.HOS.SSS))
df$dvas <- with(df, (X10y.VAS-Pre.VAS))


df<-rename( df,
            Pre_mHHS = Pre.mHHS,
            X10y_mHHS = X10y.mHHS,
            Pre_NAHS = Pre.NAHS,
            X10y_NAHS = X10y.NAHS,
            Pre_HOS = Pre.HOS.SSS,
            X10y_HOS = X10y.HOS.SSS,
            Pre_VAS = Pre.VAS,
            X10y_VAS = X10y.VAS,
)
dput(names(df))
df <- select(df, c("MR..","Condition",))
df_PA <- df[df$Condition == 1, ]
df_MC <- df[df$Condition == 0, ]

"
sudo
df_pivot: mcid
"

#df_PA #####
df_pivot <- pivot_longer(df_PA, cols = !MR.., names_to = c('Time', 'PRO'), values_to = 'Value', names_pattern = "X(.*)_(.*)")
df_pivot

df_pivot$Time <- factor(df_pivot$Time,
                        levels = c('2y','5y','10y'))
df_pivot$PRO <- factor(df_pivot$PRO,
                       levels = c('mHHS','NAHS','HOS', 'VAS','Satisfaction'))

for(i in c('mHHS','NAHS','HOS', 'VAS','Satisfaction')){
  print(i)
  df_temp <- df_pivot[df_pivot$PRO == i, ]
  res.aov <- aov(Value ~ Time, data = df_temp)
  print(summary(res.aov))
}

title <- c()
value <- c()

for(i in c('mHHS','NAHS','HOS', 'VAS','Satisfaction')){
  df_temp <- df_pivot[df_pivot$PRO == i, ]
  for(j in c('2y','5y','10y')){
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
df_means <- pivot_longer(vals, cols = 1:15, names_to = c('PRO','Time'), values_to = 'Value', names_pattern = "(.*)_(.*)")
df_means['Condition'] = 'Reconstruction'
df_means$Value <- as.numeric(df_means$Value)

##df_MC#####

df_pivot <- pivot_longer(df_MC, cols = !MR.., names_to = c('Time', 'PRO'), values_to = 'Value', names_pattern = "X(.*)_(.*)")
df_pivot

df_pivot$Time <- factor(df_pivot$Time,
                        levels = c('2y','5y','10y'))
df_pivot$PRO <- factor(df_pivot$PRO,
                       levels = c('mHHS','NAHS','HOS', 'VAS','Satisfaction'))
levs <- c('mHHS','NAHS','HOS', 'VAS','Satisfaction')
for(i in levs){
  print(i)
  df_temp <- df_pivot[df_pivot$PRO == i, ]
  res.aov <- aov(Value ~ Time, data = df_temp)
  print(summary(res.aov))
}
title <- c()
value <- c()

for(i in c('mHHS','NAHS','HOS', 'VAS','Satisfaction')){
  df_temp <- df_pivot[df_pivot$PRO == i, ]
  for(j in c('2y','5y','10y')){
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
df_means2 <- pivot_longer(vals, cols = 1:15, names_to = c('PRO','Time'), values_to = 'Value', names_pattern = "(.*)_(.*)")
df_means2['Condition'] = 'Repair'
df_means2$Value <- as.numeric(df_means2$Value)
## plotsssss #######
df_plot <- rbind(df_means, df_means2)
names(df_plot)
#df_plot[df_plot$Condition == 1, ] <- 'Reconstruction'
#df_plot[df_plot$Condition == 0, ] <- 'Repair'
df_plot$Condition <- factor(df_plot$Condition, levels = c('Reconstruction','Repair'), ordered = TRUE)
df_plot$PRO <- factor(df_plot$PRO, levels = c('mHHS','NAHS','HOS','VAS','Satisfaction'), ordered = TRUE)
df_plot$Time <- factor(df_plot$Time, levels = c('2y','5y','10y'), ordered = TRUE)

colors <- setNames(c("darkorange1", "dodgerblue2", "yellowgreen"), c('2y','5y','10y'))

par(mfrow = c(1,4))
plot <- ggplot(data = df_plot[df_plot$PRO == 'mHHS', ], mapping = aes(x = Condition, y = Value, fill = Time)) +
  geom_bar(stat="identity", position = "dodge", )+ #+
  labs(
    x = 'mHHS'
  )+
  scale_fill_manual(values=colors)

plot2 <- ggplot(data = df_plot[df_plot$PRO == 'NAHS', ], mapping = aes(x = Condition, y = Value, fill = Time)) +
  geom_bar(stat="identity", position = "dodge", )+ #+
  labs(
    x = 'NAHS'
  )+
  scale_fill_manual(values=colors)
plot3 <- ggplot(data = df_plot[df_plot$PRO == 'HOS', ], mapping = aes(x = Condition, y = Value, fill = Time)) +
  geom_bar(stat="identity", position = "dodge", )+ #+
  labs(
    x = 'HOS-SSS'
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

#scale_x_discrete(limits = c('OB', 'MC'))

library(patchwork)
(plot+plot2+plot3)/(plot4+plot5) + plot_layout(guides = 'collect')& theme(legend.position = 'right')
