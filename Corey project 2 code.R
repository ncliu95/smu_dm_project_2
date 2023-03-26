library(ggplot2)
library(class)
library(caret)
library(cluster)
library(factoextra)

# Load the cases dataset
cases <- read.csv("/Users/Corey/Documents/normalized-data-kmeans-basic.csv")

#Scale Data for KNN/K means
cases_scaled <- scale(cases)

#set RNG seed.  number was picked at random.
set.seed(150)

# Split the data into training and testing sets
train_index <- sample(1:nrow(cases_scaled), round(0.7*nrow(cases_scaled)))
train_data <- cases_scaled[train_index, 2:9]
train_labels <- cases_scaled[train_index, 1]
test_data <- cases_scaled[-train_index, 2:9]


# Fit the k-nearest neighbor model
knn_model <- knn(train_data, test_data, train_labels)
kmeans_result <- kmeans(knn_model, centers = 3)
kmeans_model <- kmeans(cases_scaled, centers = 3, nstart=20)

#Discover/remove outliers (OPTION)
centers <- kmeans_result$centers[kmeans_model$cluster, ] 
distances <- sqrt(rowSums((cases_scaled - centers)^2))
outliers <- order(distances, decreasing=T)[1:9]
print(outliers)
#to do: plot separate outliers or re-plot and remove.  

#Plot using the cusplot library 
clusplot(cases_scaled, kmeans_model$cluster,  color=TRUE, shade=TRUE, labels=2, lines=0)







