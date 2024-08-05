
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)
library(tidyr)


customer_data <- read.csv("Shopping Mall Customer Segmentation Data .csv")


# Age distribution
ggplot(customer_data, aes(x = Age)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black") + 
  ggtitle("Distribution of Age")

# Gender distribution
ggplot(customer_data, aes(x = Gender)) + 
  geom_bar(fill = "pink", color = "black") + 
  ggtitle("Distribution of Gender")

# Scatter plot of Spending Score vs. Annual Income
ggplot(customer_data, aes(x = Annual.Income, y = Spending.Score)) + 
  geom_point(aes(color = Gender)) + 
  ggtitle("Spending Score vs Annual Income")

# Select relevant features for clustering
selected_data = customer_data %>% select(Age, Annual.Income, Spending.Score)

# Standardize the data
scaled_data = scale(selected_data)

# Check for NA/NaN/Inf values in the scaled data
sum(is.na(scaled_data))
sum(is.nan(scaled_data))
sum(is.infinite(scaled_data))

# Remove rows with NA/NaN/Inf values
cleaned_data <- scaled_data[complete.cases(scaled_data), ]

# Elbow method
fviz_nbclust(cleaned_data, kmeans, method = "wss") + 
  ggtitle("Elbow Method")

# Silhouette method
fviz_nbclust(cleaned_data, kmeans, method = "silhouette") + 
  ggtitle("Silhouette Method")

# Apply K-Means clustering to the cleaned data
set.seed(123)
kmeans_result <- kmeans(cleaned_data, centers = 5, nstart = 25)

# Add cluster assignments to the original dataset (matching rows only)
customer_data <- customer_data[complete.cases(scaled_data), ]
customer_data$Cluster <- as.factor(kmeans_result$cluster)

# Scatter plot of clusters
ggplot(customer_data, aes(x = Annual.Income, y = Spending.Score, color = Cluster)) + 
  geom_point() + 
  ggtitle("Customer Segments")

# Summary statistics by cluster
customer_data %>% 
  group_by(Cluster) %>% 
  summarise(
    Age = mean(Age),
    AnnualIncome = mean(Annual.Income),
    SpendingScore = mean(Spending.Score)
  )
