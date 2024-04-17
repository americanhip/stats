#read in libraries
library(plyr)
library(dplyr)
library(tableone)
library(ggplot2)
library(patchwork)
library(tidyverse)
library(readxl)
library(ggpubr)
library(survival)
library(ggplot2)
library(ggsurvfit)
library(ggbreak)

#FUNCTIONS
#cat_stat(df1col, df2col, col1head = 'col1', col2head = 'col2', fish.exact = FALSE)
#cont_stat(dfcol1, dfcol2, colhead1 = 'col1', colhead2 = 'col2', pair = FALSE)
#PASS(df_IJ, df_MC, PASSlist, PROlist, studylevs, time_prefix='')
#MCID(df_IJ, df_MC, PROlist, studylevs, time_prefix = "")
#MOI(df_IJ, df_MC, MOIlist, PROlist, studylevs, time_prefix = "")
'
todo
matching fcn?
run all outputs into a .txt or csv?
'
#run categorical statistics
cat_stat <- function(df1col, df2col, col1head = 'col1', col2head = 'col2', fish.exact = FALSE){
  m <- cbind(table(df1col), table(df2col))
  colnames(m) = c(col1head, col2head)
  mtest <- chisq.test(m, correct = fish.exact)
  output <- list(m, mtest)
  return(output)
}

#out <- cat_stat(df_PA$sex, df_MC$sex, "PA", "MC", FALSE)

#run continuous statistics
#r starts at 1
cont_stat <- function(dfcol1, dfcol2, colhead1 = 'col1', colhead2 = 'col2', pair = FALSE) {
  out_data <- data.frame(mean(dfcol1, na.rm = TRUE), sd(dfcol1, na.rm = TRUE))
  out_data <- rbind(out_data, list(mean(dfcol2, na.rm = TRUE), sd(dfcol2, na.rm = TRUE)))
  rownames(out_data) = c(colhead1, colhead2)
  colnames(out_data) = c('Mean', 'SD')
  normality <- ((shapiro.test(dfcol1)$p.value < 0.05) & (shapiro.test(dfcol2)$p.value < 0.05))
  if(pair == FALSE){
    eqvar = var.test(dfcol1, dfcol2, alternative = "two.sided")$p.value > 0.05
  } else {
    eqvar = TRUE
  }

  if (normality == TRUE) {
    if (pair == TRUE){
      test <- c('paired t test')
      pval <- t.test(dfcol1, dfcol2, paired = TRUE)$p.value
      #paired t-test
    }else{
      if(eqvar == TRUE){
        test <- c('unpaired t for equal')
        pval <- t.test(dfcol1, dfcol2, paired = FALSE, var.equal = TRUE, alternative = "two.sided")$p.value
        #unpaired t for equal
      } else{
        test <- c('unpaired t for unequal')
        pval <- t.test(dfcol1, dfcol2, paired = FALSE, var.equal = FALSE, alternative = "two.sided" )$p.value
        #unpaired t for unequal
      }
    }

  }else{
    if(pair == TRUE){
      test <- c(' wilcoxon signed rank')
      pval <- wilcox.test(dfcol1, dfcol2, paired = TRUE)$p.value
      #wilcoxon
    } else{
      if(eqvar == TRUE){
        test <- c('mann whitney u test')
        pval <- wilcox.test(dfcol1, dfcol2, paired = FALSE)$p.value
        #mann whitney U test
      } else{
        test <- c('welch test')
        pval <- t.test(dfcol1, dfcol2, paired = FALSE, alternative = "two.sided")$p.value
        #welch test
      }
    }
  }
out <- list(out_data, test, pval)
return(out)
  #output mean, sd, test results
}

#calculate whether each person meets PASS and creates plot based on it. need to add significance manually!!
pass <- function(df_IJ, df_MC, PASSlist, PROlist, studylevs = c(1,0), time_prefix=''){

  #PASSHHS <- 93
  #PASSFJS <- 92.2
  #PASSHOOS.JR <- 76.7

  PRO <- c()
  Group <- c()
  values <- c()

  index <- 0

  for (i in PROlist){
    column <- paste('pass',i, sep = "")
    check <- paste(time_prefix,i, sep = "")
    print(check)
    #passval <- paste('PASS',i,sep = "")
    index <- index+1
    df_IJ[[column]] <- with(df_IJ, 0)
    df_MC[[column]] <- with(df_MC, 0)
    df_IJ[[column]] <- with(df_IJ, ifelse(df_IJ[[check]] >= PASSlist[index], 1, 0))
    df_MC[[column]] <- with(df_MC, ifelse(df_MC[[check]] >= PASSlist[index], 1, 0))
    fish <- (sum(df_IJ[[column]], na.rm = TRUE)<5&sum(df_MC[[column]], na.rm = TRUE)<5)
    passstats <- cat_stat(df_IJ[[column]], df_MC[[column]], 'IJ', 'MC', fish.exact=fish)
    m <- passstats[[1]]
    passperc <- ((m['1','IJ'])/((m['1','IJ'])+(m['0','IJ']))) * 100
    passpercMC <- ((m['1','MC'])/((m['1','MC'])+(m['0','MC']))) * 100
    print(i)
    print('p value')
    print(passstats[[2]])
    values <- append(values, eval(passperc))
    values <- append(values, eval(passpercMC))
    for (j in studylevs){
      PRO <- append(PRO, i)
      Group <- append(Group, j)
    }
  }

  df_plot <<- rbind(PRO, Group)
  df_plot <<- as.data.frame(t(rbind(df_plot, values)))
  df_plot$values <<- as.numeric(df_plot$values)

  df_plot$Group <- factor(df_plot$Group, levels = studylevs, ordered = TRUE)
  df_plot$PRO <- factor(df_plot$PRO, levels = PROlist, ordered = TRUE)

  colors <- setNames(c("darkorange1", "dodgerblue2"), studylevs)
  plot <- ggplot(data = df_plot, mapping = aes(x = PRO, y = values, fill = Group)) +
    geom_bar(stat="identity", position = "dodge")+ #+
    labs(
      x = 'PRO'
    )+
    scale_fill_manual(values=colors)
  return(plot)
}
#pass(df_IJ, df_MC, PASSlist, PROlist, studylevs, 'X10y.')

#calculate MCID and whether each person meets MCID and creates plot based on it. need to add significance manually!!
MCID <- function(df_IJ, df_MC, PROlist, studylevs = c(1,0), time_prefix = ""){
  PRO <- c()
  Group <- c()
  values <- c()
  for(i in PROlist){
    precol <- paste('Pre.',i, sep = "") #pre
    postcol <- paste('d',i,sep = "") #change
    checkcol <- paste(time_prefix,i,sep = "") #post time to check. confusing I KNOW
    val <- paste('mcid',i,sep = "")
    print(val)
    df_IJ[[postcol]] <- with(df_IJ, (df_IJ[[checkcol]]-df_IJ[[precol]]))
    df_MC[[postcol]] <- with(df_MC, (df_MC[[checkcol]]-df_MC[[precol]]))
    MCIDvalIJ <- 0.5 * (sd(df_IJ[[precol]], na.rm = TRUE))
    MCIDvalMC <- 0.5 * (sd(df_MC[[precol]], na.rm = TRUE))
    df_IJ[[val]] <- with(df_IJ, 0)
    df_MC[[val]] <- with(df_MC, 0)
    df_IJ[[val]] <- with(df_IJ, ifelse(abs(df_IJ[[postcol]]) >= eval(MCIDvalIJ), 1, 0))
    df_MC[[val]] <- with(df_MC, ifelse(abs(df_MC[[postcol]]) >= eval(MCIDvalMC), 1, 0))
    fish <- (sum(df_IJ[[val]], na.rm = TRUE)<5&sum(df_MC[[val]], na.rm = TRUE)<5)
    mcidstats <- cat_stat(df_IJ[[val]], df_MC[[val]], 'IJ', 'MC', FALSE)
    print(i)
    print('p value')
    print(mcidstats[[2]])
    m <- mcidstats[[1]]
    passperc <- ((m['1','IJ'])/((m['1','IJ'])+(m['0','IJ']))) * 100
    passpercMC <- ((m['1','MC'])/((m['1','MC'])+(m['0','MC']))) * 100

    values <- append(values, eval(passperc))
    values <- append(values, eval(passpercMC))
    for (j in studylevs){
      PRO <- append(PRO, i)
      Group <- append(Group, j)
    }
  }
  df_plot <<- rbind(PRO, Group)
  df_plot <<- as.data.frame(t(rbind(df_plot, values)))
  df_plot$values <<- as.numeric(df_plot$values)
  df_plot$Group <- factor(df_plot$Group, levels = studylevs, ordered = TRUE)
  df_plot$PRO <- factor(df_plot$PRO, levels = PROlist, ordered = TRUE)

  colors <- setNames(c("darkorange1", "dodgerblue2"), studylevs)
  plot <- ggplot(data = df_plot, mapping = aes(x = PRO, y = values, fill = Group)) +
    geom_bar(stat="identity", position = "dodge")+ #+
    labs(
      x = 'PRO'
    )+
    scale_fill_manual(values=colors)
  return(plot)
}
#x <- MCID(df_IJ,df_MC, PROlist = c("mHHS","NAHS","HOS.SSS"),studylevs = c(1,0),time_prefix = "X10y.")
#x

#calculate whether each person meets MOI and creates plot based on it. need to add significance manually!!
MOI <- function(df_IJ, df_MC, MOIlist, PROlist, studylevs = c(1,0), time_prefix = "") {
  PRO <- c()
  Group <- c()
  values <- c()

  index <- 0

  for (i in PROlist){
    column <- paste('moi',i, sep = "")#new column
    check <- paste('moicheck',i,sept = "") #column to store values in
    postcol <- paste(time_prefix,i, sep = "") #post value
    precol <- paste('Pre.',i, sep = "") #pre value
    #passval <- paste('PASS',i,sep = "")
    index <- index+1
    df_IJ[[column]] <- with(df_IJ, 0)
    df_MC[[column]] <- with(df_MC, 0)
    df_IJ[[check]] <- with(df_IJ, 0)
    df_MC[[check]] <- with(df_MC, 0)
    df_IJ[[check]] <- with(df_IJ, (100*(df_IJ[[postcol]]-df_IJ[[precol]]))/(100-(df_IJ[[precol]])))
    df_MC[[check]] <- with(df_MC, (100*(df_MC[[postcol]]-df_MC[[precol]]))/(100-(df_MC[[precol]])))
    df_IJ[[column]] <- with(df_IJ, ifelse(df_IJ[[check]] >= MOIlist[index], 1, 0))
    df_MC[[column]] <- with(df_MC, ifelse(df_MC[[check]] >= MOIlist[index], 1, 0))
    fish <- (sum(df_IJ[[column]], na.rm = TRUE)<5&sum(df_MC[[column]], na.rm = TRUE)<5)
    "MOI
  100*(postop-preop)/(100-preop)"
    passstats <- cat_stat(df_IJ[[column]], df_MC[[column]], 'IJ', 'MC', fish.exact=fish)
    m <- passstats[[1]]
    passperc <- ((m['1','IJ'])/((m['1','IJ'])+(m['0','IJ']))) * 100
    passpercMC <- ((m['1','MC'])/((m['1','MC'])+(m['0','MC']))) * 100
    print(i)
    print('p value')
    print(passstats[[2]])
    values <- append(values, eval(passperc))
    values <- append(values, eval(passpercMC))
    for (j in studylevs){
      PRO <- append(PRO, i)
      Group <- append(Group, j)
    }
  }

  df_plot <<- rbind(PRO, Group)
  df_plot <<- as.data.frame(t(rbind(df_plot, values)))
  df_plot$values <<- as.numeric(df_plot$values)

  df_plot$Group <- factor(df_plot$Group, levels = studylevs, ordered = TRUE)
  df_plot$PRO <- factor(df_plot$PRO, levels = PROlist, ordered = TRUE)

  colors <- setNames(c("darkorange1", "dodgerblue2"), studylevs)
  plot <- ggplot(data = df_plot, mapping = aes(x = PRO, y = values, fill = Group)) +
    geom_bar(stat="identity", position = "dodge")+ #+
    labs(
      x = 'PRO',
      y = 'Percent of Patients Meeting MOI'
    )+
    scale_fill_manual(values=colors)
  return(plot)

}
#MOIlist <- c('70','80','90')
#PROlist <- c("mHHS","NAHS","HOS.SSS")
#studylevs <- c('Non-diabetic','Diabetic')
#x<-MOI(df_IJ, df_MC, MOIlist, PROlist, studylevs, time_prefix = "X.2y.")
# df <- read.csv('Matched.csv')
# df_eligible <- df[df$Endpoint == "", ]
# df_IJ <- df_eligible[df_eligible$Obese == 1, ]
# df_MC <- df_eligible[df_eligible$Obese == 0, ]
# x <- MCID(df_IJ,df_MC, PROlist = c("mHHS","NAHS","HOS.SSS"),studylevs = c(1,0),time_prefix = "X10y.")
