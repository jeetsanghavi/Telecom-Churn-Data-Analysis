#Group Project
#Telecom Churn Data Analysis
#Author - Jeet Sanghavi

#AIM
#---------------------------------------------------------------------------------------
# Predict the behaviour of customers in order to devise strategies to retain them
#---------------------------------------------------------------------------------------

##-----------------------------------------------------------
##Extracting and Viewing the Data
##-----------------------------------------------------------

#Extracting the data from the dataset stored on the computer
data = read.csv("E:/Jeet D/Masters/Github/Telecom Churn Data Analysis/WA_Fn-UseC_-Telco-Customer-Churn.csv")

#Viewing the data
View(data)

#Compactly displaying the structure of the dataframe giving us 
#information about the attributes
str(data)
