---
title: 'Hierarchical Cluster Analysis'
---

#install packages
#install.packages("tidyverse")
#install.packages("cluster")
#install.packages("fpc")
#install.packages("factoextra")
#install.packages("janitor")

#load libraries
library(tidyverse)
library(cluster)
library(fpc)
library(factoextra)
library(janitor)

#setting my working directory
setwd("C:/Users/muhir/Downloads/Data Science for Businesses")

#read in datafile
employeedf<-read.csv("employees.csv")
View(employeedf)

#Create a data frame with only the variables needed for the analysis
hdf<-employeedf[c('Age', 'MonthlyIncome', 'PercentSalaryHike', 'YearsAtCompany')]
View(hdf)

hdfn <- scale(hdf)
view(hdfn)

#calculate distance between each pair of observations using the dist function 
#and euclidean distance
match_dist<-dist(hdfn, method="euclidean")

# Perform hierarchical clustering using Ward.D method
hc_ward <- hclust(match_dist, method = "ward.D")
hc_ward

#plot the dendrogram
plot(hc_ward)

#Create 4 clusters using the cutree function
hc_ward_4<-cutree(hc_ward, k=4)

#display vector of cluster assignments for each observation
hc_ward_4

#link cluster assignments to original data frame
hcl4df<-cbind(hdf, clusterID=hc_ward_4)
view(hcl4df)

#write data frame to CSV file to analyze in Excel
write.csv(hcl4df, "magazine_hier4_clusters.csv")

#display number of observations in each cluster
hcl4df %>%
  group_by(clusterID) %>%
  summarize(n())

#calculate variable averages for all non-normalized observations
summarize_all(hdf, mean)

#Variable averages for each cluster
hcl4df %>%
  group_by(clusterID) %>%
  summarize_all(mean)

