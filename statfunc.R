#this is the file i use to define all the functions and libraries I use so you only have to load this once and you should be good unless you change this file
library(plyr) #data tools
library(dplyr) #data tools
library(tableone) #creating tables -- i use these but you dont have to
library(ggplot2) #plotting
library(patchwork) #plot manipulation tools
library(tidyverse) #data tools
library(readxl) #reading in excel -- i don't use this but you can
library(ggpubr) #plot tools
library(survival) #survival curve fitter
library(ggsurvfit) #survival plot maker

# categorical statistics -- input two columns you want to compare frequencies in, names of columns if desired, and whether the test should be a Fisher Exact test
cat_stat <- function(df1col, df2col, col1head = 'col1', col2head = 'col2', fish.exact = FALSE){
  m <- cbind(table(df1col), table(df2col)) #create table with both columns
  colnames(m) = c(col1head, col2head) #set names of columns to output
  mtest <- chisq.test(m, correct = fish.exact) #do chi squared test
  output <- list(m, mtest) #create output
  return(output)
}
#example:
#out <- cat_stat(df_PA$sex, df_MC$sex, "PA", "MC", FALSE)

#r starts at 1
#continuous statistics -- perform a test on two columns to compare continuous data. input two columns, names of columns if desired, and whether the data should be paired
cont_stat <- function(dfcol1, dfcol2, colhead1 = 'col1', colhead2 = 'col2', pair = FALSE) {
  out_data <- data.frame(mean(dfcol1, na.rm = TRUE), sd(dfcol1, na.rm = TRUE))
  out_data <- rbind(out_data, list(mean(dfcol2, na.rm = TRUE), sd(dfcol2, na.rm = TRUE))) #create dataframe with means and standard deviations of each column
  rownames(out_data) = c(colhead1, colhead2) #add row names
  colnames(out_data) = c('Mean', 'SD') #add names of values
  normality <- ((shapiro.test(dfcol1)$p.value < 0.05) & (shapiro.test(dfcol2)$p.value < 0.05)) #test whether both columns are normal
  if(pair == FALSE){
    eqvar = var.test(dfcol1, dfcol2, alternative = "two.sided")$p.value > 0.05 #if not paired, test whether both columns have equal variance
  } else {
    eqvar = TRUE #if paired, variance is assumed to be true
  }
#below: perform tests according to variance, normality, and whether they are paired
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
out <- list(out_data, test, pval) #return output
return(out)
  #output mean, sd, test results
}

#Patient Acceptable Symptom State function: check whether a PRO at a given time prefix (assigned in your headers) meets the PASS threshold
#input: two data frames, df_IJ and df_MC, PASSlist: your list of pass thresholds, PROlist: your list of what corresponding PROs those are, studylevs: what levels your study cohort is determined by (i.e. (1,0) or (yes,no), and time_prefix: the prefix for PROs on your datapull)
pass <- function(df_IJ, df_MC, PASSlist, PROlist, studylevs, time_prefix=''){

  #PASSHHS <- 93
  #PASSFJS <- 92.2
  #PASSHOOS.JR <- 76.7

  #setup
  PRO <- c()
  Group <- c()
  values <- c()

  index <- 0

  for (i in PROlist){
    column <- paste('pass',i, sep = "")
    check <- paste(time_prefix,i, sep = "") #create header for what column to check
    print(check)
    #passval <- paste('PASS',i,sep = "")
    index <- index+1
    df_IJ[[column]] <- with(df_IJ, 0) #create column to check pass for PRO, input 1 or 0
    df_MC[[column]] <- with(df_MC, 0)
    df_IJ[[column]] <- with(df_IJ, ifelse(df_IJ[[check]] >= PASSlist[index], 1, 0)) #put into passPRO if timed column passes PASS threshold
    df_MC[[column]] <- with(df_MC, ifelse(df_MC[[check]] >= PASSlist[index], 1, 0))
    fish <- (sum(df_IJ[[column]], na.rm = TRUE)<5&sum(df_MC[[column]], na.rm = TRUE)<5) #check if a fisher exact test needs to be completed
    passstats <- cat_stat(df_IJ[[column]], df_MC[[column]], 'IJ', 'MC', fish.exact=fish) #perform categorical statistics on how many meet pass on either end
    m <- passstats[[1]]
    passperc <- ((m['1','IJ'])/((m['1','IJ'])+(m['0','IJ']))) * 100 #get percentage that meet pass in study group
    passpercMC <- ((m['1','MC'])/((m['1','MC'])+(m['0','MC']))) * 100 #get percentage that meet pass in control group
    print(i)
    print('p value')
    print(passstats[[2]]) #print the p value
    values <- append(values, eval(passperc))
    values <- append(values, eval(passpercMC))
    for (j in studylevs){
      PRO <- append(PRO, i)
      Group <- append(Group, j)
    }
  }
#create dataframe to create plot
  df_plot <<- rbind(PRO, Group)
  df_plot <<- as.data.frame(t(rbind(df_plot, values)))
  df_plot$values <<- as.numeric(df_plot$values)
#factor so that study group shows up first
  df_plot$Group <- factor(df_plot$Group, levels = studylevs, ordered = TRUE)
  df_plot$PRO <- factor(df_plot$PRO, levels = PROlist, ordered = TRUE)
  colors <- setNames(c("darkorange1", "dodgerblue2"), studylevs) #set colors
  #plot below
  plot <- ggplot(data = df_plot, mapping = aes(x = PRO, y = values, fill = Group)) +
    geom_bar(stat="identity", position = "dodge")+ #+
    labs(
      x = 'PRO'
    )+
    scale_fill_manual(values=colors)
  return(plot)
}
#pass(df_IJ, df_MC, PASSlist, PROlist, studylevs, 'X10y.')

#Minimal Clinically Important Difference: this is checking whether the improvement from preoperative to postoperative is clinically significant. The threshold for this is determined by taking the standard deviation of the preop PRO and splitting it in half.
#inputs: df_IJ and df_MC: input dataframes, study and control respectively, PROlist: list of PROs you want to look at, studylevs: levels for your study group, i.e. c(1,0) or c('yes', 'no'), time_prefix: the prefix for the time point you want to look at
MCID <- function(df_IJ, df_MC, PROlist, studylevs, time_prefix = ""){
  PRO <- c()
  Group <- c()
  values <- c()
  for(i in PROlist){
    precol <- paste('Pre.',i, sep = "")
    postcol <- paste('d',i,sep = "")
    checkcol <- paste(time_prefix,i,sep = "")
    val <- paste('mcid',i,sep = "")
    print(val)
    df_IJ[[postcol]] <- with(df_IJ, (df_IJ[[checkcol]]-df_IJ[[precol]])) #get differences
    df_MC[[postcol]] <- with(df_MC, (df_MC[[checkcol]]-df_MC[[precol]])) #get differences
    MCIDvalIJ <- 0.5 * (sd(df_IJ[[precol]], na.rm = TRUE)) #calculate MCID
    MCIDvalMC <- 0.5 * (sd(df_MC[[precol]], na.rm = TRUE)) #calculate MCID
    df_IJ[[val]] <- with(df_IJ, 0)
    df_MC[[val]] <- with(df_MC, 0)
    df_IJ[[val]] <- with(df_IJ, ifelse(df_IJ[[postcol]] >= eval(MCIDvalIJ), 1, 0)) #check if difference is greater or equal to MCID
    df_MC[[val]] <- with(df_MC, ifelse(df_MC[[postcol]] >= eval(MCIDvalMC), 1, 0)) #check if difference is greater or equal to MCID
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
x <- MCID(df_IJ,df_MC, PROlist = c("mHHS","NAHS","HOS.SSS"),studylevs = c(1,0),time_prefix = "X10y.")

# df <- read.csv('Matched.csv')
# df_eligible <- df[df$Endpoint == "", ]
# df_IJ <- df_eligible[df_eligible$Obese == 1, ]
# df_MC <- df_eligible[df_eligible$Obese == 0, ]
# x <- MCID(df_IJ,df_MC, PROlist = c("mHHS","NAHS","HOS.SSS"),studylevs = c(1,0),time_prefix = "X10y.")
