#Multivariate Analysis
#Telecom-Churn Data Analysis
#Author - Jeet Sanghavi

#------------------------------------------------------------------------------------
##Importing libraries
#------------------------------------------------------------------------------------
install.packages("Hotelling")
library(tidyverse)
library(MASS)
library(DMwR)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(pROC)
library(ggcorrplot)
library(lattice)
library(sm)
library(Hmisc)
library(asbio)
library(MVA)
library(Hotelling)
#------------------------------------------------------------------------------------
##Importing Data and inital analyses
#------------------------------------------------------------------------------------

#Importing csv file from a location on the local computer
tchurn<- read.csv("E:/Jeet D/Masters/Github/Telecom Churn Data Analysis/Dataset-Telco-Customer-Churn.csv")
tchurn <- as.data.frame(tchurn)
View(tchurn)

#Dimension of the dataset
dim(tchurn)

#Grabbing the first 5 rows of the dataset to get overview of the dataset
head(tchurn)

#Obtaining more insight about the kid of data stored in each column
#Total charges and monthly charges are numerical variables
#Tenure and SeniorCitizen are stored as numerical which need tobe converted to categorical
#variables
summary(tchurn)
glimpse(tchurn)

#------------------------------------------------------------------------------------
##Data cleaning and formatting
#------------------------------------------------------------------------------------

#Converting SeniorCitizen variable into a factor variable
tchurn$SeniorCitizen <- as.factor(ifelse(tchurn$SeniorCitizen==0,'Yes','No'))

#Converting tenure values into ranges of 12 months

tchurn <- mutate(tchurn,tenure_range = tenure)
cut(tchurn$tenure_range,6,labels = c('0-1 Years','1-2 Years','2-3 Years','4-5 Years','5-6 Years','6-7 Years'))
tchurn$tenure_range <- cut(tchurn$tenure_range,6,labels = c('0-1 Years','1-2 Years','2-3 Years','4-5 Years','5-6 Years','6-7 Years'))

#Replacing 'No Internet Service' value in Streaming Movies, Online Security, Device Prtection,
#Tech Support and Streaming TV with No'
tchurn$StreamingTV[tchurn$StreamingTV=='No internet service'] <- 'No'
tchurn$StreamingMovies[tchurn$StreamingMovies=='No internet service'] <- 'No'
tchurn$OnlineSecurity[tchurn$OnlineSecurity=='No internet service'] <- 'No'
tchurn$OnlineBackup[tchurn$OnlineBackup=='No internet service'] <- 'No'
tchurn$DeviceProtection[tchurn$DeviceProtection=='No internet service'] <- 'No'
tchurn$TechSupport[tchurn$TechSupport=='No internet service'] <- 'No'

#Deleting the unused levels from the factor variables
tchurn$StreamingMovies <- factor(tchurn$StreamingMovies)
tchurn$StreamingTV <- factor(tchurn$StreamingTV)
tchurn$OnlineSecurity <- factor(tchurn$OnlineSecurity)
tchurn$OnlineBackup <- factor(tchurn$OnlineBackup)
tchurn$DeviceProtection <- factor(tchurn$DeviceProtection)
tchurn$TechSupport <- factor(tchurn$TechSupport)

#Calculating the number of null values in each of the columns
nullvalues <- colSums(is.na(tchurn))
nullvalues <- (nullvalues/nrow(tchurn))*100
nullvalues

#Removing the rows containing null values as there are just 11 rows out of 7043 rows
#in total which is 0.15% and hence we can afford dropping those
tchurn <- tchurn[complete.cases(tchurn), ]

#------------------------------------------------------------------------------------
##Exploratory Data Analysis
#------------------------------------------------------------------------------------

#Checking for distributions in numerical columns
#The qqplotd show a few extreme outliers which break the assumption of 95% confidence
#normal distribution
par(mfrow = c(1,2))
hist(tchurn$TotalCharges,xlab='',main = 'Histogram of TotalCharges',freq = FALSE)
lines(density(tchurn$TotalCharges,na.rm = T))
rug(jitter(tchurn$TotalCharges))
qqPlot(tchurn$TotalCharges,main='Normal QQ plot of TotalCharges')
par(mfrow=c(1,1))

par(mfrow = c(1,2))
hist(tchurn$MonthlyCharges,xlab='',main = 'Histogram of MonthlyCharges',freq = FALSE)
lines(density(tchurn$MonthlyCharges,na.rm = T))
rug(jitter(tchurn$MonthlyCharges))
qqPlot(tchurn$MonthlyCharges,main='Normal QQ plot of MonthlyCharges')
par(mfrow=c(1,1))

#Boxplot distributions for our numeric columns
#The dashed line shows the mean and the dark center line shows the median
#Difference between these two lines depict the deviation from the central limit theorem
boxplot(tchurn$TotalCharges, ylab = "TotalCharges")
rug(jitter(tchurn$TotalCharges), side = 2)
abline(h = mean(tchurn$TotalCharges, na.rm = T), lty = 2)

boxplot(tchurn$MonthlyCharges, ylab = "MonthlyCharges",outline = TRUE)
rug(jitter(tchurn$MonthlyCharges), side = 2)
abline(h = mean(tchurn$MonthlyCharges, na.rm = T), lty = 2)

#Plotting the TotalCharges and Monthl Charges with 3 lines for mean, median and mean+std
plot(tchurn$TotalCharges, xlab = "")
abline(h = mean(tchurn$TotalCharges, na.rm = T), lty = 1)
abline(h = mean(tchurn$TotalCharges, na.rm = T) + sd(tchurn$TotalCharges, na.rm = T),lty = 2)
abline(h = median(tchurn$TotalCharges, na.rm = T), lty = 3)
identify(tchurn$TotalCharges)

#Plotting the TotalCharges and Monthl Charges with 3 lines for mean, median and mean+std
plot(tchurn$MonthlyCharges, xlab = "")
abline(h = mean(tchurn$MonthlyCharges, na.rm = T), lty = 1)
abline(h = mean(tchurn$MonthCharges, na.rm = T) + sd(tchurn$MonthlyCharges, na.rm = T),lty = 2)
abline(h = median(tchurn$MonthlyCharges, na.rm = T), lty = 3)
identify(tchurn$MonthlyCharges)

#Scatterplot to understand the relationship between monthly and yearly charges
#This shows us that both of them are highly corelated which is kind of obvious
#For other categorical varibles, we compare only on one numeric column which is TotalCharges
#Checking for outliers using bvplot
plot(tchurn$TotalCharges~tchurn$MonthlyCharges,data=tchurn,xlab="MonthlyCharges",ylab="TotalCharges")
with(tchurn,text(tchurn$MonthlyCharges,tchurn$TotalCharges,cex=0.6,labels=abbreviate(row.names(tchurn))))
x<-tchurn[,c(19,20)]
bvbox(x,ylab = "TotalCharges",xlab="MonthlyCharges")
identify(tchurn$MonthlyCharges)

#Chi Plot for inspecting the independence
chi.plot(tchurn_final$TotalCharges,tchurn_final$MonthlyCharges)

#Plotting joint boxplots for various categories wrt numerical column TotalCharges
bwplot(tchurn$tenure_range ~ tchurn$TotalCharges, data=tchurn, ylab='Tenure',xlab='TotalCharges')
bwplot(tchurn$gender ~ tchurn$TotalCharges, data=tchurn, ylab='Gender',xlab='TotalCharges')
bwplot(tchurn$SeniorCitizen ~ tchurn$TotalCharges, data=tchurn, ylab='SeniorCitizen',xlab='TotalCharges')
bwplot(tchurn$Partner ~ tchurn$TotalCharges, data=tchurn, ylab='Partner',xlab='TotalCharges')
bwplot(tchurn$Dependents ~ tchurn$TotalCharges, data=tchurn, ylab='Dependents',xlab='TotalCharges')
bwplot(tchurn$Contract ~ tchurn$TotalCharges, data=tchurn, ylab='Contract',xlab='TotalCharges')

#Plotting stripplots for various categories wrt numerical column TotalCharges
bwplot(tchurn$tenure_range ~ tchurn$TotalCharges, data=tchurn,panel=panel.bpplot, 
       probs=seq(.01,.49,by=.01), datadensity=TRUE, ylab='Tenure',xlab='TotalCharges')
bwplot(tchurn$gender ~ tchurn$TotalCharges, data=tchurn,panel=panel.bpplot, 
       probs=seq(.01,.49,by=.01), datadensity=TRUE, ylab='gender',xlab='TotalCharges')
bwplot(tchurn$SeniorCitizen ~ tchurn$TotalCharges, data=tchurn,panel=panel.bpplot, 
       probs=seq(.01,.49,by=.01), datadensity=TRUE, ylab='SeniorCitizen',xlab='TotalCharges')
bwplot(tchurn$Partner ~ tchurn$TotalCharges, data=tchurn,panel=panel.bpplot, 
       probs=seq(.01,.49,by=.01), datadensity=TRUE, ylab='Partner',xlab='TotalCharges')
bwplot(tchurn$Dependents ~ tchurn$TotalCharges, data=tchurn,panel=panel.bpplot, 
       probs=seq(.01,.49,by=.01), datadensity=TRUE, ylab='Dependents',xlab='TotalCharges')
bwplot(tchurn$Contract ~ tchurn$TotalCharges, data=tchurn,panel=panel.bpplot, 
       probs=seq(.01,.49,by=.01), datadensity=TRUE, ylab='Contract',xlab='TotalCharges')

#------------------------------------------------------------------------------------
##Creating Dummy Variables
#------------------------------------------------------------------------------------

#Converting double/int columns to numeric
numeric_col <- c("tenure","MonthlyCharges", "TotalCharges")
tchurn[numeric_col] <- sapply(tchurn[numeric_col], as.numeric)

#Segregating the numeric columns from categorical columns and storing them as a seperate dataframe
tchurn_int <- tchurn[,c("tenure","MonthlyCharges", "TotalCharges")]
tchurn_int <- data.frame(scale(tchurn_int))

#Creating dummy variables for the categorical data
tchurn_cat <- tchurn[,-c(1,6,19,20)]
dummy<- data.frame(sapply(tchurn_cat,function(x) data.frame(model.matrix(~x-1,data =tchurn_cat))[,-1]))
head(dummy)

#Combining the dummy and the numeric columns to form the final dataset
tchurn_final <- cbind(tchurn_int,dummy)
head(tchurn_final)

#------------------------------------------------------------------------------------
##Matrix Plots, Covariance and Corelations Plots
#------------------------------------------------------------------------------------
#Next 4 lines were used to solve the error "Figure margins too large"
par("mar")
par(mar=c(1,1,1,1))
graphics.off()
dev.off()

#ScatterPlot matrix
pairs(tchurn_final[,1:3],pch=".",cex=1.5)

#CorrelationMatrix
cormatrix <- round(cor(tchurn_final),4)
#Heatmap for correlation matrix
#Negative correlations are shown in blue and positive in red
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(cormatrix, col=col, symm=TRUE)

#Covariance Matrix
covmatrix <- round(cov(tchurn_final),4)
#Heatmap for covariance matrix
#Negative correlations are shown in blue and positive in red
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(covmatrix, col=col, symm=TRUE)

#------------------------------------------------------------------------------------
##Test of Significance
#------------------------------------------------------------------------------------
#T-Test
#Null Hypothesis - The two means are equal
#Alternate Hypothesis - Difference in the two means is not zero
#pvalue >= 0.05, accept null hypothesis
#Or else accept the alternate hypothesis

#Univariate mean comparison using t test

#Totalcharges and Churn
with(data=tchurn,t.test(tchurn$TotalCharges[tchurn$Churn=="Yes"],tchurn$TotalCharges[tchurn$Churn=="No"],var.equal=TRUE))

#MonthlyCharges and Churn
with(data=tchurn,t.test(tchurn$MonthlyCharges[tchurn$Churn=="Yes"],tchurn$MonthlyCharges[tchurn$Churn=="No"],var.equal=TRUE))

#Totalcharges and Churn
with(data=tchurn,t.test(tchurn$TotalCharges[tchurn$gender=="Male"],tchurn$TotalCharges[tchurn$gender=="Female"],var.equal=TRUE))

#MonthlyCharges and Churn
with(data=tchurn,t.test(tchurn$MonthlyCharges[tchurn$gender=="Male"],tchurn$MonthlyCharges[tchurn$gender=="Female"],var.equal=TRUE))

#Multivariate mean comparison using Hotelling t test

#Charges and gender
t2testgender <- hotelling.test(tchurn$TotalCharges + tchurn$MonthlyCharges ~ tchurn$gender, data=tchurn)
cat("T2 statistic =",t2testgender$stat[[1]],"\n")
print(t2testgender)

#Charges and Churn
t2testchurn <- hotelling.test(tchurn$TotalCharges + tchurn$MonthlyCharges ~ tchurn$Churn, data=tchurn)
cat("T2 statistic =",t2testchurn$stat[[1]],"\n")
print(t2testchurn)

#F Test
#Null Hypothesis - The two samples have same variance
#Alternate Hypothesis - Difference in the variance of two samples
#pvalue >= 0.05, accept null hypothesis
#Or else accept the alternate hypothesis

#The numerical columns we have do not have a normal distribution. Therefore we skip the F test

