op <- options(scipen=999)
# Disable scientific notation on axis
# https://stackoverflow.com/questions/27688754/bar-chart-legend-position-avoiding-operlap-in-r

##############################################################################################
# Changing directory
##############################################################################################

setwd('C:\\Users\\paddy\\Documents\\00-OSAP\\5 R\\Working Directory')
getwd()

##############################################################################################
# Importing Data
##############################################################################################

hh <- read.csv("Project Sample Data_household.csv")

##############################################################################################
# Understanding Data
##############################################################################################

# converting Null to Na
hh[hh==''] <- NA 

# review dataset's shape
dim(hh)
colnames(hh)
str(hh)
summary(hh)
head(hh)
tail(hh,10)

##############################################################################################
# Cleaning Data
##############################################################################################

# make a copy
hh0 <- hh
hh0

# 1)Handling duplicate data
duplicated(hh)
sum(duplicated((hh)))
# make a copy
hh1 <- hh

# 2)Handling Missing Values
colSums(is.na(hh))
# make a copy
hh2 <- hh

# 3)Dropping non-meaningful or unnecessary column
hh$language <- NULL
colnames(hh)
# make a copy
hh3 <- hh

##############################################################################################
# Processing Data
##############################################################################################

# Q1 - Add a new column of 'income_total' which is the sum of 'income_husband' and 'income_wife'.
#      Then segment to 'income_group' by 'income_total'.
#------------------------------------------------------------------------------

# 1.1: Add the 1st column - income_total

hh$income_total <- hh$income_husband + hh$income_wife
View(hh)
summary(hh$income_total)

# 1.2: Add the 2nd column - income_group

hh$income_group = ifelse(hh$income_total < 50000, "Low Income",
                  ifelse(hh$income_total < 300000, "Middle Class",
                  ifelse(hh$income_total >= 300000, "High Income", "")))
View(hh)
str(hh)
# Q2 - What is the distribution of the variable 'income_group'?
#-----------------------------------------------------------------------------

# 2.1: list the distribution
tbl <- aggregate(hh$income_group,list(hh$income_group),length)
tbl

# 2.2: 3D Pie Chart
install.packages('plotrix')
library(plotrix)
count <- table(hh$income_group)
pct <- round(count/sum(count)*100)
lbls <- c("High Income", "Low Income", "Middle Class")
lbls <- paste(lbls, pct) # add pct to label
lbls <- paste(lbls, "%", sep = "") # add % to pct
pie <- pie3D(count, 
             explode=0.2,
             main = "Pie Chart of Income Group")
pie3D.labels(pie, labels = lbls)

# 2.3: Simple Bar Plot

counts <- table(hh$income_group)
counts
barplot(counts, 
        main = "Simple Bar Plot: Income Group",
        xlab = "income_group",
        ylab = "Frequency",
        col = 'black',
        horiz = FALSE)

# 2.4: Histogram of income_total

hist(hh$income_total, 
     main = "Histogram of Total Income", 
     col = "black")

# Q3 - Is there any relation between communication mode and target(income_group)?
#---------------------------------------------------------------------------------

#bivariate analysis for mode Vs. income_group
#bivariate analysis for categorical vs. categorical:for visualization:stacked barchart or grouped bar chart, ...
#                                                   for summarization: Contingency table(two-way table)
#                                                   for test of independence: chi-square test

# 3.1 - Visualization: Stacked Bar Plot 

tbl <- table(hh$mode,hh$income_group)
tbl
counts <- tbl[1:3,1:3]
counts
barplot(counts, 
        main = "Communication Mode Vs. Income Group",
        xlab = "Income Group", 
        col = c("black","red", "yellow"),
        legend = rownames(counts),
        args.legend = list(x ='top', bty='n', inset=c(0,0)))
# Legend position: https://stackoverflow.com/questions/27688754/bar-chart-legend-position-avoiding-operlap-in-r

# 3.2 - Summarization: Contingency Table 

add <- addmargins(xtabs(~ mode + income_group,data=hh))
add
add[1:4,1:4]
proportions(xtabs(~ mode + income_group,data=hh))[1:3,1:3]

# 3.3 - Indipendency: Chi-square Test

# 3.3.1 Problem:

# Test the hypothesis whether the communication mode is independent of the income group  at .05 significance level.
# Null hypothesis:  Communication Mode is independent of Income Group

# 3.3.2 Solution:

# p-value
library(MASS)
tbl <- table(hh$mode,hh$income_group)
tbl
chisq.test(tbl) # the p-value < 2.2e-16

# Mosaic Plots
library(vcd)
library(grid)
mosaic(structable(hh$income_group ~ hh$mode))
# structable: https://stackoverflow.com/questions/14547162/missing-value-where-true-false-needed-error-vcdmosaic

# Association Plots
assoc(hh$income_group ~ hh$mode, shade=TRUE)

# 3.3.3 Conclusion:

# As the p-value 2.2e-16 is less than the .05 significance level, we reject the null hypothesis that
# Communication Mode is independent of the Income_Group and conclude that 
# in our data, the 'mode' and the 'income_group' are statistically significantly associated (p-value = 0)

# Q4.what is Bedrooms distribution, how to handle the outlier if any.
#-------------------------------------------------------------------------------------------------------

# 4.1 summary
summary(hh$bedrooms) # 10 rooms seems too much

# 4.2 histogram
hist(hh$bedrooms, 
     breaks = 8, 
     main = "bedrooms",
     col = "blue",
     xlab = "bedrooms",
     ylab = "Frequency")

# 4.3 Boxplot of Bedrooms by internet
boxplot(bedrooms ~ internet,
        data = hh, 
        main = "Boxplot of Bedrooms by internet",
        xlab = "internet", 
        ylab = "bedrooms",
        col = "blue")

# 4.4 pattern of outlier
bed_out <- hh[which(hh["bedrooms"]==10),]
bed_out$bedrooms # total 72 obs cross all types of ownership, income_group, built years...
summary(bed_out["bedrooms"])
nrow(bed_out)

# 4.5 prove the bedroom numbers = 10 are just scaled up by 10. 
count <- 0
for (val in bed_out$bedrooms){
  if (val%%10 !=0) {count = count+1}
}
count # count = 0 means all the bedrooms equal to 10 are scaled up by 10 

# 4.6 amend outlier by deviding by 10
hh$bedrooms <- ifelse(hh$bedrooms == 10, hh$bedrooms/10, hh$bedrooms)
summary(hh["bedrooms"]) # Max reduced to 5.
# copy file
hh4 <- hh
View(hh4)

# Q5. Is there any relationship between Bedrooms and Internet(Yes/No)?
# ------------------------------------------------------------------------

#Continouse Vs. Categorical  : For summaraization: group by categorical column an aggregate for numerical column
#                              For visualization: Grouped box plot,...
#                              For test of independence :1) if categorical column has only two levels :t-test
#                                                        2) if categorical column has more than two levels: ANOVA

# 5.1: Summary grouped by Internet(Yes/No)
agg1 <- aggregate(bedrooms ~ internet, hh , mean)
agg1

# 5.2: Visualization by qplot
library(ggplot2)
qplot(internet, 
      bedrooms, 
      data = hh, 
      geom="boxplot", 
      fill = internet)

# 5.3: Changing histogram plot fill colors by internet and usinging semi-transparent fill
p <- ggplot(hh,aes(x=bedrooms, fill=internet, color=internet)) +
      geom_histogram(position="identity", bins=15, alpha=0.5)
# bins: https://stackoverflow.com/questions/34774120/set-number-of-bins-for-histogram-directly-in-ggplot
p

# 5.4: Add mean lines
library(plyr)
mu <- ddply(hh, "internet", summarise, grp.mean=mean(bedrooms,na.rm=T))
head(mu)
p <- p + geom_vline(data=mu, aes(xintercept=grp.mean, color=internet),linetype="solid")
p

# 5.5: Add density
p <- ggplot(hh, aes(x=bedrooms, fill=internet, color=internet)) +
      geom_histogram(aes(y=..density..),bins=15, position="identity", alpha=0.5)+
      geom_density(alpha=0.5)
p

# 5.6: Add mean lines and Change the legend position
p + geom_vline(data=mu, aes(xintercept=grp.mean, color=internet),linetype="dashed")+ 
    theme(legend.position="top")+
    labs(title="Bedrooms histogram plot", x="Bedrooms", y = "Density")

# 5.7 t-test
# Yes: House has internet, No: House has no internet
# Null Hypothesis: µYes = µNo (the means of both populations are equal)
# Alternate Hypothesis: µYes <> µNo (the means of both populations are not equal)
t.test(bedrooms ~ internet, data=hh )
# Conclusion: p-value is less than 0.05, so the mean values between uYes and uNo are not equal.

# Q6 - What is the distribution of ownership?
#-----------------------------------------------------------------------------

# 6.1: list the distribution
tbl <- aggregate(hh$own,list(hh$own),length)
tbl

# 6.2: Pie Chart

count <- table(hh$own)
count
freq1 <- c(count[1], count[2], count[3], count[4])
lbls <- c("Occupy", "Own", "Mortgage", "Rent")
pct <- round(freq1/sum(freq1)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(freq1, 
    labels = lbls, 
    col = rainbow(length(lbls)),
    main = "Pie Chart of Ownership")

# 6.3: Simple Bar Plot

counts <- table(hh$own)
counts
barplot(counts, 
        main = "Simple Bar Plot: Ownership",
        xlab = "Ownership",
        ylab = "Frequency",
        col = 'black',
        horiz = FALSE)

# Q7 - What is the distribution of built year?
#-----------------------------------------------------------------------------

# 7.1: list the distribution
tbl <- aggregate(hh$decade_built,list(hh$decade_built),length)
tbl

# 7.2: Pie Chart

count <- table(hh$decade_built)
pct <- round(count/sum(count)*100)
lbls <- hh$decade_built
lbls <- paste(lbls, pct) # add pct to label
lbls <- paste(lbls, "%", sep = "") # add % to pct
pie(count, 
    labels = lbls,
    col = rainbow(length(pct)),
    main = "Pie Chart of Built-decade")

# 7.3: Simple Bar Plot

counts <- table(hh$decade_built)
counts
barplot(counts, 
        main = "Simple Bar Plot: Built Decade",
        xlab = "Ownership",
        ylab = "Frequency",
        col = 'black',
        horiz = FALSE)

# 7.4: Histogram 

hist(hh$decade_built, 
     main = "Histogram of Built Decade", 
     col = "black")

# Q8 - Are there any relationships between the husband's age and his income?
#-----------------------------------------------------------------------------

# create a scatter plot of a data set
plot(x = hh$age_husband , y = hh$income_husband, type = 'p', col="red")

# Answer: In this data set there's no evidence proves there's a relation between 
# husband's age and his income.

plot(x = hh$bedrooms , y = hh$number_children, type = 'p', col="red") # yes
plot(x = hh$age_husband , y = hh$age_wife, type = 'p', col="red") # yes
plot(x = hh$gas , y = hh$electricity, type = 'p', col="red") # no
plot(x = hh$bedrooms , y = hh$decade_built, type = 'p', col="red")# yes
plot(x = hh$age_husband , y = hh$number_children, type = 'p', col="red")# yes


# Q9. Is there any relationship between wife's age and Internet availability(Yes/No)?
# ------------------------------------------------------------------------

#Continouse Vs. Categorical  : For summaraization: group by categorical column an aggregate for numerical column
#                              For visualization: Grouped box plot,...
#                              For test of independence :1) if categorical column has only two levels :t-test
#                                                        2) if categorical column has more than two levels: ANOVA

# 9.1: Summary grouped by Internet(Yes/No)
agg1 <- aggregate(age_wife ~ internet, hh , mean)
agg1

# 9.2: Visualization by qplot
library(ggplot2)
qplot(internet, 
      age_wife, 
      data = hh, 
      geom="boxplot", 
      fill = internet)
library(ggplot2)
# Conclusion: the yonger the age, the more internet access

# 9.3: Changing histogram plot fill colors by internet and usinging semi-transparent fill
p <- ggplot(hh,aes(x=age_wife, fill=internet, color=internet)) +
  geom_histogram(position="identity", bins=15, alpha=0.5)
# bins: https://stackoverflow.com/questions/34774120/set-number-of-bins-for-histogram-directly-in-ggplot
p

# 9.4: Add mean lines
library(plyr)
mu <- ddply(hh, "internet", summarise, grp.mean=mean(age_wife,na.rm=T))
head(mu)
p <- p + geom_vline(data=mu, aes(xintercept=grp.mean, color=internet),linetype="solid")
p

# 9.5: Add density
p <- ggplot(hh, aes(x=age_wife, fill=internet, color=internet)) +
  geom_histogram(aes(y=..density..),bins=15, position="identity", alpha=0.5)+
  geom_density(alpha=0.5)
p

# 9.6: Add mean lines and Change the legend position
p + geom_vline(data=mu, aes(xintercept=grp.mean, color=internet),linetype="dashed")+ 
  theme(legend.position="top")+
  labs(title="Wife's Age histogram plot", x="Age_Wife", y = "Density")

# 9.7 t-test
# Yes: House has internet, No: House has no internet
# Null Hypothesis: µYes = µNo (the means of both populations are equal)
# Alternate Hypothesis: µYes <> µNo (the means of both populations are not equal)
t.test(age_wife ~ internet, data=hh )
# Conclusion: p-value is less than 0.05, so there is association between wife's age and internet at 5% significant level

# Q10 - Is there any relation between ownership and target(income_group)?
#---------------------------------------------------------------------------------

#bivariate analysis for mode Vs. income_group
#bivariate analysis for categorical vs. categorical:for visualization:stacked barchart or grouped bar chart, ...
#                                                   for summarization: Contingency table(two-way table)
#                                                   for test of independence: chi-square test

# 10.1 - Visualization: Stacked Bar Plot 

tbl <- table(hh$own,hh$income_group)
tbl
counts <- tbl[1:4,1:3]
counts
barplot(counts, 
        main = "Ownership Vs. Income Group",
        xlab = "Income Group", 
        col = c("black","red", "yellow", "green"),
        legend = rownames(counts),
        args.legend = list(x ='topleft', bty='n', inset=c(0,-0.1)))
# Legend position: https://stackoverflow.com/questions/27688754/bar-chart-legend-position-avoiding-operlap-in-r

# 10.2 - Summarization: Contingency Table 

add <- addmargins(xtabs(~ own + income_group,data=hh))
add
add[1:5,1:4]
proportions(xtabs(~ own + income_group,data=hh))[1:4,1:3]

# 10.3 - Indipendency: Chi-square Test

# 10.3.1 Problem:

# Test the hypothesis whether the ownership is independent of the income group  at .05 significance level.
# Null hypothesis:  Ownership is independent of Income Group

# 10.3.2 Solution:

# p-value
library(MASS)
tbl <- table(hh$own,hh$income_group)
tbl
chisq.test(tbl) # the p-value < 2.2e-16

# Mosaic Plots
library(vcd)
library(grid)
mosaic(structable(hh$income_group ~ hh$own))
# structable: https://stackoverflow.com/questions/14547162/missing-value-where-true-false-needed-error-vcdmosaic

# Association Plots
assoc(hh$income_group ~ hh$own, shade=TRUE)

# 10.3.3 Conclusion:

# As the p-value 2.2e-16 is less than the .05 significance level, we reject the null hypothesis that
# ownership is independent of the Income_Group and conclude that 
# in our data, the 'own' and the 'income_group' are statistically significantly associated (p-value = 0)
