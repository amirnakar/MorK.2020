#TIME LOG:
# Agreed on 120s per hour
#8/4/20   : 15m            [0.25h]
#6/8/20   :  10:20 - 11:20 [1h]
#11/8/20  : 8:00 - 10:00   [2h]
#11/8/20  : 13:00 - 14:00  [1h]
#1/9/20   : 08:30 - 11:30  [2.5h]

#Total time = 0.25+1+2+1+2.5 = 6.75h (charged as 6h)

#1/10/20 :  11:30 - 13:30  [2]
#2/10/20 : 9:00 - 10:00    [1]
#6/10/20 : 9:00 - 11:30    [2]
#7/10/20 : 9:30 - 10:30    [1]
#7/10/20 : 14:00 - 15:30   [1.5]

#Total time = 2+1+2+1+1.5 = 7.5h
# Total overall time: 7+6h = 13h * 120s per = 1560s


#### Load the data ####
library(openxlsx)
library(reshape2)
library(ggplot2)
library(Hmisc) 
library(dplyr)
library(car)
library(caret)
library(readxl)
library(ggsignif)
library(tidyr)
library(vegan)
library(purrr)
library(egg)


#old path = "C:/Users/di58lag/Documents/scratchboard/Scratchboard/MorK/questioner_time_measurments.xlsx"

# Reads the data
data = read.csv("F:/MorK/data.csv")

# Quality Control:
data
dim(data)
head(data)
colnames(data)

numerical <- unlist(lapply(data, is.numeric))
datan = data[,numerical]
summary(data[,numerical])

data.for.normality = data[,6:33]
#### Comparison of Averages ####
# First you must check normality: 


# NORMALITY: Shapiro test
# From the output, the p-value > 0.05 implies that the distribution
#of the data are not significantly different from a normal distribution.
#In other words, we can assume the normality.

Normality.Table = as.data.frame(
  do.call(rbind, lapply(data.for.normality[,numerical], function(x) shapiro.test(x)[c("p.value")] # Calculates Shapiro test for each variable, and attaches the pvalue to a new table
    ))) 
Normality.Table$Normal = Normality.Table$p.value < 0.05                             # This will show you which rows have pvalue<0.05

#And now the same for logs:
logstable = log10(data[,numerical])                                                 # Calculates the log of the data
Logs.Normality.Table = as.data.frame(
  do.call(rbind, lapply(logstable, function(x) shapiro.test(x)[c("p.value")])))     # Calculates Shapiro test for each variable, and attaches the pvalue to a new table
Logs.Normality.Table$Normal = Logs.Normality.Table$p.value < 0.05                   # This will show you which rows have pvalue<0.05

Normality.Table.Combined = cbind(Normality.Table, Logs.Normality.Table)             # Combines the regular and Log shapiro test results
colnames(Normality.Table.Combined)[3:4] = c("LOG: p.value", "LOG: Normal")          # renames the columns

colnames(data)

data.for.normality %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free", ncol = 3) +
  geom_histogram()

data.for.normality %>%
  keep(is.numeric) %>% 
  gather() %>% 
  # you have to specify the sample you want to use
  ggplot(aes(sample = value)) + 
  facet_wrap(~ key, scales = "free", ncol = 3) +
  stat_qq() + 
  stat_qq_line()


ggplot(data, aes(sample = Appointments1..NEW..Secs.)) + stat_qq() + stat_qq_line()


# This implies all distributions are non-normal
# Which makes sense given that you have ~50 human samples involved

# Significance testing: 
# We will use the wilcoxon Ranked Sum Test as a non-parametric test:
colnames(data)

#### Function explained ####
# This function will work as follows: 
# Input: two variable names from your data set
# Output: Boxplot with Wilcoxon Rank Sum Test statistic printed on it


# Notes: 
# 1. You can ignore the warnings: 
  # No id variables; using all as measure variables
  # Removed 2 rows containing non-finite values
# It's just the way it handles NA lines. Which you have

# 2. Box plots: 
# The middle line is the median
# 
# The lower and upper hinges correspond to the first and third quartiles
#  
# The whiskers extend from the hinge to the furthest value but no further than 1.5 * IQR from the hinge 
# (where IQR is the inter-quartile range, or distance between the first and third quartiles). 

# Outliers: Points beyond the whisker limit


                              wilcox.plus.boxes <- function(var1, var2) {  # Defines the function requiring two parameters
data.for.function = data[,c(var1,var2)]      # creates a dataframe from your data which is only the two parameters you chose
colnames(data.for.function) = c(var1,var2)   # Gives the relevant columns names (for later)

varname = gsub("\\..*","",var1)
levs    = c("New","Old")

result = wilcox.test(                        # Calculates the wilcoxon test between the two variables
  data.for.function[,1],
  data.for.function[,2]
  ) 

mean.diff = mean(data.for.function[,1], na.rm = T)-mean(data.for.function[,2],na.rm = T)

melted = melt(data.for.function)             # Transforms the data for ggplot

plot = ggplot(                               # This is a very long function to create the plot
  melted, aes(x=variable,y=value,fill=variable)) + # Defines the releant data
  geom_boxplot()+                                 # Defines the plot type as Box Plot
  theme(panel.background = element_rect(fill="white")) + # Makes it visually clean
  labs(x = varname,y="Time [Mins]") +                  # Adds X, Y axis labels
  theme(panel.grid.major = element_line(colour = "gray")) + # adds gray lines on the Y axis
  theme(panel.grid.minor = element_line(colour = "gray")) + # does the same
  guides(fill=FALSE) +                                      # Removes the legend (not needed for this)
  labs(                                                     # Startes to create the complicated titles
    title = paste("Comparison of", varname, sep = " "), # Main title: Comparison between the two variables
    
    
    
    
    subtitle = paste(                                                # Subtitle: gives the different statistics pasted together
      "Result of the Wilcoxon Rank Sum Test: \ntest statistic = ", 
      result$statistic, "\n p-value = ", 
      formatC(result$p.value, format = "g", digits = 2), 
      "\n mean difference = ", round(mean.diff, digits = 1), "minutes", sep = " " )
      ) + 
  scale_x_discrete(breaks = c(var1,var2), labels=c("Old","New")) +
  
  geom_signif(comparisons = list(c(var1, var2)),             # Creates a significance star when the pvalue<0.05
  test="wilcox.test", map_signif_level =  c("*"=0.05))
  

plot                                                         # Saves the final plot 

  }

names = colnames(data) # This is for my own reference to know which variables need to be tabulated

# Calculations:
# Each Q is a question from your list
Q1 = wilcox.plus.boxes(names[33],names[32]) #SUM
Q2 = wilcox.plus.boxes(names[19],names[18]) # Appointments1
Q3 = wilcox.plus.boxes(names[27],names[26]) # Mirshm
Q4 = wilcox.plus.boxes(names[29],names[28]) # Payments
Q5 = wilcox.plus.boxes(names[31],names[30]) # Update
Q6 = wilcox.plus.boxes(names[21],names[20]) # Appointments 2
Q7 = wilcox.plus.boxes(names[25],names[24]) # Medical File
Q8 = wilcox.plus.boxes(names[23],names[22]) # Blood Test

FIG3 = ggarrange(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8, ncol = 4)


#### Correlations ####

# This we must break down into 3 "categories":
# 1. Correlation between numericals 
# 2. Correlation between a binary category (like gender) and a numerical
# 3. Correlation between a categorical and a numerical

####1. Correlation between numericals ####

#### Function explained ####
# This function will work as follows: 
# Input: two variable names from your data set
# Output: scatterplot with correlation coefficents and statistic printed on it
#
# This will calculate the correlation between two chosen variables and plot it


correlation.with.plot = function(var1, var2, cor.met){  # Defines the function requiring two parameters. cor.met needs to be either "pearson" or "spearman"
  data.for.function = data[,c(var1,var2)]      # creates a dataframe from your data which is only the two parameters you chose
  colnames(data.for.function) = c(var1,var2)   # Gives the relevant columns names (for later)

  
cor = cor.test(data[,var1], data[,var2], method = cor.met)       # tests the correlation between the two variables
                                               # This calculates all the values we need: r and p-val

plot = ggplot(                               # This is a very long function to create the plot
  data.for.function, aes(x=data.for.function[,1],y=data.for.function[,2])) +                # Defines the relevant data
  geom_point()  +                            # Defines that we will create a scatter plot
  theme(panel.background = element_rect(fill="white")) +    # Makes it visually clean
  labs(x = var1,y=var2) +                                   # Adds X, Y axis labels
  theme(panel.grid.major = element_line(colour = "gray")) + # adds gray lines on the Y axis
  theme(panel.grid.minor = element_line(colour = "gray")) + # does the same
  guides(fill=FALSE) +                                      # Removes the legend (not needed for this)
  labs(                                                     # Starts to create the complicated titles
    title = paste("Comparison between\n",var1, "&", var2, sep = " "), # Main title: Comparison between the two variables
    subtitle = paste(                                       # Subtitle: gives the different statistics pasted together
      "Correlation statistics using", cor.met, "method:
      correlation coefficent (r) = ",   round(cor$estimate, digits = 2), 
      "\n R^2 = ", round(cor$estimate^2, digits = 2), 
      "\n p-value = ", 
      formatC(cor$p.value, format = "g", digits = 2))) +
  geom_smooth(method="lm", se = F)                             # Adds a linear correlation line to the figure
  
}

# Now we can ask the questions
names # This is again just for my reference


Q10 = correlation.with.plot(names[6], names[32], cor.met = "pearson")  # hours of use VS. SUM.OLD
Q11 = correlation.with.plot(names[6], names[33], cor.met = "pearson")  # hours of use VS. SUM.NEW
Q12 = correlation.with.plot(names[9], names[32], cor.met = "spearman")  # easy VS. SUM.OLD
Q13 = correlation.with.plot(names[9], names[33], cor.met = "spearman")  # easy VS. SUM.NEW
Q14 = correlation.with.plot(names[15], names[32], cor.met = "spearman") # n.menu2 VS. SUM.OLD
Q15 = correlation.with.plot(names[14], names[33], cor.met = "spearman") # n.menu1 VS. SUM.NEW
Q16 = correlation.with.plot(names[3], names[11], cor.met = "pearson")  # age VS. minutes

ggarrange(Q10,Q11,Q12,Q13,Q14,Q15, 
          ncol = 2, labels = c("n=50", "n=50", "n=25", "n=25", "n=50", "n=50"))

 # 2. Correlation between a binary category (like gender) and a numerical
# For questions with regards to 2 categories, like gender, it makes no sense to calculate a 
# CORRELATION (as there is no 1.3 male)
# Therefor we need to test whether there is a difference in averages like we did earlier

# As we already showed no variable behaves "noramlly", we must still use the non-parametric test: Wilcoxon Ranked Sum Test
# This function is a variation of the earlier 2 functions for this purpose.
# Input: Two variables, the first one is BINARY and the second NUMERICAL.
# Output: a boxplot with test statistics printed on it

wilcox.categorical.data <- function(var1, var2) {          # Defines the function requiring two parameters
  data.for.function = data[,c(var1,var2)]                  # creates a dataframe from your data which is only the two parameters you chose
  data.for.function[,1] = as.factor(data.for.function[,1]) # Defines the BINARY column as CATEGORICAL
  colnames(data.for.function) = c(var1,var2)               # Gives the relevant columns names (for later)
 
  result = wilcox.test(                        # Calculates the wilcoxon test between the categories based on the numerical values
    data.for.function[data.for.function==1,2],
    data.for.function[data.for.function==0,2]
  ) 
  melted = data.for.function[complete.cases(data.for.function),]   # Removes any missing data from the dataset
  plot = ggplot(                               # This is a very long function to create the plot
    melted, aes(x=melted[,1],y=melted[,2],fill=melted[,1])) + # Defines the relevant data
    geom_boxplot()+                                           # Defines the plot type as Box Plot
    theme(panel.background = element_rect(fill="white")) +    # Makes it visually clean
    labs(x = colnames(melted)[1],y="Time [Mins]") +           # Adds X, Y axis labels
    theme(panel.grid.major = element_line(colour = "gray")) + # adds gray lines on the Y axis
    theme(panel.grid.minor = element_line(colour = "gray")) + # does the same
    guides(fill=FALSE)      +                                 # Removes the legend (not needed for this)
    labs(                                                     # Startes to create the complicated titles
      title = paste("Comparison between", var1, "&", var2, sep = " "), # Main title: Comparison between the two variables
      subtitle = paste(                                                # Subtitle: gives the different statistics pasted together
        "Result of the Wilcoxon Rank Sum Test: \ntest statistic = ",   
        result$statistic, "\n p-value = ", 
        formatC(result$p.value, format = "g", digits = 2))) + 
        geom_signif(comparisons = list(c(var1, var2)),             # Creates a significance star when the pvalue<0.05
                test="wilcox.test", map_signif_level =  c("*"=0.05))
  
  plot                                                         # Saves the final plot 
}


# Now we can ask the questions:
#2-way catergorical:
Q9  =  wilcox.categorical.data(names[7], names[18]) # application VS. Appointments 1
Q17 =  wilcox.categorical.data(names[2], names[32]) # Gender VS. SUM.OLD 
Q18 =  wilcox.categorical.data(names[2], names[33]) # Gender VS. SUM.NEW

# Multi-way categorical
# For testing any relations between the language and the ease of use,
# there is a need for a more complex analysis.
# Here, we are trying to see if there is any difference between 
# different languages with regards to ease of use.
# Again, we will use box plots:

data.for.function = data[,c("lang","easy")]                    # creates a dataframe from your data which is only the two parameters you chose
data.for.function[,1] = as.factor(data.for.function[,1])       # Defines the language as a categorical value
colnames(data.for.function) = c("lang","easy")                 # Gives the relevant columns names (for later)
melted = data.for.function[complete.cases(data.for.function),] # Removes any missing data lines

plot = ggplot(                               # This is a function to create the plot
  melted, aes(x=lang ,y=easy,color=lang)) + # Defines the relevant data
  geom_point()+                            # Defines the plot type as Box Plot
  theme(panel.background = element_rect(fill="white"))  # Makes it visually clean

plot
# As there are so few cases per language, it makes no sense to calculate anything.

# Saving the plots

save.as.jpeg = function(Qn){
name = deparse(substitute(Qn))
  ggsave(filename = paste(name, ".jpg"),
         plot = Qn, 
         device = "jpeg", 
         path = "F:/MorK/graphs", 
         width = 3.5, 
         height = 5, 
         units = "in", 
         dpi = 500)
}

save.as.jpeg(Q1)
save.as.jpeg(Q2)
save.as.jpeg(Q3)
save.as.jpeg(Q4)
save.as.jpeg(Q5)
save.as.jpeg(Q6)
save.as.jpeg(Q7)
save.as.jpeg(Q8)
save.as.jpeg(Q9)
save.as.jpeg(Q10)
save.as.jpeg(Q11)
save.as.jpeg(Q12)
save.as.jpeg(Q13)
save.as.jpeg(Q14)
save.as.jpeg(Q15)
save.as.jpeg(Q16)
save.as.jpeg(Q17)
save.as.jpeg(Q18)

#### 2-way ANOVA on non-normal data ####

#Parameters needed from original data: Age [3], font 2 [13], Sum.new [33], Sum.old [32]
data.manova = data[,c(3,13,33,32)]                  #makes a new dataframe with only the relevant columns
data.manova.filtered = data.manova %>% drop_na() #removes any lines with missing data (NAs)
colnames(data.manova.filtered) = c("age", "font", "sum.new", "sum.old")  #renames the colomns for easy writing downstream

res.aov.Q1 <- adonis(sum.new ~ age * font, data = data.manova.filtered, permutations = 1000) # Calculates the 2-way ANOVA with 1,000 permutations
res.aov.Q2 <- adonis(sum.old ~ age * font, data = data.manova.filtered, permutations = 1000) # Calculates the 2-way ANOVA with 1,000 permutations