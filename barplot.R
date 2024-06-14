
df_plot <- read.csv('mcidplot.csv')
names(df_plot)
df_plot$Group <- factor(df_plot$Group, levels = c('Reconstruction', 'Repair'), ordered = TRUE)
df_plot$PRO <- factor(df_plot$PRO, levels = c('mHHS','NAHS','HOS-SSS'), ordered = TRUE)
df_mcid <- df_plot[df_plot$Measure == 'MCID', ]
#x <- cont_stat(df_IJ$X2ymhhs, df_IJ$X10y.mHHS, '2y mhhs', '5y mhhs', FALSE )
df_pass <- df_plot[df_plot$Measure == 'PASS', ]
df_scb <- df_plot[df_plot$Measure == 'SCB', ]


colors <- setNames(c("darkorange1", "dodgerblue2"), c('Reconstruction','Repair'))

par(mfrow = c(1,4))
plot <- ggplot(data = df_mcid, mapping = aes(x = PRO, y = Percentage, fill = Group)) +
  geom_bar(stat="identity", position = "dodge", )+ #+
  labs(
   x = 'MCID'
  )+
  scale_fill_manual(values=colors)
  #scale_x_discrete(limits = c('OB', 'MC'))

plot2 <- ggplot(data = df_pass, mapping = aes(x = PRO, y = Percentage, fill = Group)) +
  geom_bar(stat="identity", position = "dodge", )+ #+
  labs(
    x = 'PASS'
  )+
  scale_fill_manual(values=colors)

plot3 <- ggplot(data = df_scb, mapping = aes(x = PRO, y = Percentage, fill = Group)) +
  geom_bar(stat="identity", position = "dodge", )+ #+
  labs(
    x = 'SCB'
  )+
  scale_fill_manual(values=colors)

#scale_x_discrete(limits = c('OB', 'MC'))

library(patchwork)
plot+plot2+plot3 + plot_layout(guides = 'collect')& theme(legend.position = 'right')


########
df_plot <- read.csv('mcid_plot.csv')
names(df_plot)
df_plot<-df_plot[df_plot$Figure == 3, ]
df_plot$Group <- factor(df_plot$Group, levels = c('OB','MC'), ordered = TRUE)
df_plot$PRO <- factor(df_plot$PRO, levels = c('mHHS','NAHS','HOS-SSS'), ordered = TRUE)
df_mcid <- df_plot[df_plot$Measure == 'MCID', ]
#x <- cont_stat(df_IJ$X2ymhhs, df_IJ$X10y.mHHS, '2y mhhs', '5y mhhs', FALSE )
df_pass <- df_plot[df_plot$Measure == 'PASS', ]


colors <- setNames(c("darkorange1", "dodgerblue2"), c('OB','MC'))

par(mfrow = c(1,4))
plot <- ggplot(data = df_mcid, mapping = aes(x = PRO, y = Percent, fill = Group)) +
  geom_bar(stat="identity", position = "dodge", )+ #+
  labs(
    x = 'MCID'
  )+
  scale_fill_manual(values=colors)
#scale_x_discrete(limits = c('OB', 'MC'))

plot2 <- ggplot(data = df_pass, mapping = aes(x = PRO, y = Percent, fill = Group)) +
  geom_bar(stat="identity", position = "dodge", )+ #+
  labs(
    x = 'PASS'
  )+
  scale_fill_manual(values=colors)

#scale_x_discrete(limits = c('OB', 'MC'))

library(patchwork)
plot/plot2 + plot_layout(guides = 'collect')& theme(legend.position = 'right')
