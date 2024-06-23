
title: 'K-Means Cluster Analysis'

  

#install packages
#install.packages("tidyverse")
#install.packages("cluster")
#install.packages("fpc")

#load libraries
library(tidyverse)
library(cluster)
library(fpc)

#setting my working directory
setwd("C:/Users/muhir/Downloads/Data Science for Businesses")

#read in datafile
employeedf<-read.csv("employees.csv")
View(employeedf)

#Create a data frame with only the variables needed for the analysis
variabledf<-employeedf[c('Age', 'MonthlyIncome', 'PercentSalaryHike', 'YearsAtCompany')]
View(variabledf)

#normalize each variable
variabledfn<-scale(variabledf)
View(variabledfn)

#set random number seed in order to replicate the analysis
set.seed(42)

#create a function to calculate total within-cluster sum of squared deviations 
#to use in elbow plot
wss<-function(k){kmeans(variabledfn, k, nstart=10)} $tot.withinss

#range of k values for elbow plot
k_values<- 1:10

# run the function to create the range of values for the elbow plot
wss_values<-map_dbl(k_values, wss)

#create a new data frame containing both k_values and wss_values
elbowdf<- data.frame(k_values, wss_values)

#graph the elbow plot
ggplot(elbowdf, mapping = aes(x = k_values, y = wss_values)) +
  geom_line() + geom_point()

#run k-means clustering with 4 clusters (k=4) and 1000 random restarts
k4<-kmeans(variabledfn, 4, nstart=1000)

#display the structure of the 4-means clustering object
str(k4)

#display information on 4-means clustering
k4

#display cluster statistics
cluster.stats(dist(variabledfn, method="euclidean"), k4$cluster)

#combining each observation's cluster assignment with unscaled data frame
variabledfk4<-cbind(variabledf, clusterID=k4$cluster)
View(variabledfk4)

#write data frame to CSV file to analyze in Excel
write.csv(variabledfk4, "magazine_kmeans_4clusters.csv")

#calculate variable averages for all non-normalized observations
summarize_all(variabledf, mean)

#Calculate variable averages for each cluster
variabledfk4 %>%
  group_by(clusterID) %>%
  summarize_all(mean)


