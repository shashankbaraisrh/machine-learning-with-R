# K Means Clustering Exercises


# 1. Use read.csv to open both data sets and set them as df1 and df2. Pay attention to what the separator (sep) is.

df1 <- read.csv("C:/Users/User/Desktop/shashankbaraicollege/R/exercise2/winequality-white.csv", sep = ';')
df2 <- read.csv("C:/Users/User/Desktop/shashankbaraicollege/R/exercise2/winequality-red.csv", sep = ';')
df1
df2

# 2. Now add a label column to both df1 and df2 indicating a label 'red' or 'white'.

df1$label <- 'red'
df2$label <- 'white'

# 3. Check the head of df1 and df2

head(df1)
head(df2)

# 4. Combine df1 and df2 into a single data frame called wine.

wine <- rbind(df1, df2)
head(wine)
str(wine)



# 10. Grab the wine data without the column 'label' and call it clus.data, and check the head of clus.data

clus.data <- wine[, !names(wine) %in% "label"]
head(clus.data)

# 11. Call the kmeans function on clus.data and assign the results to wine.cluster.

wine.cluster <- kmeans(clus.data, centers = 3)
wine.cluster

# 12. Print out the wine.cluster Cluster Means and explore the information.

cluster_means <- wine.cluster$centers
print(cluster_means)

# Perform PCA
pca_result <- prcomp(clus.data, scale. = TRUE)

# Get PCA scores
pca_scores <- as.data.frame(pca_result$x[,1:2])

# Add cluster labels
pca_scores$cluster <- as.factor(wine.cluster$cluster)

# Plot
library(ggplot2)
ggplot(pca_scores, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering of Wine Data (PCA Visualization)")


# 13. Use the table() function to compare your cluster results to the real results. Which is easier to correctly group, red or white wines?

comparison_table <- table(wine$label, wine.cluster$cluster)
print(comparison_table)

# white wines are easier to correctly group compared to red wines.








# 5. Create a Histogram of residual sugar from the wine data. Color by red and white wines.
head(wine)
ggplot(data=wine, aes(x=residual.sugar, fill=label))+geom_histogram(binwidth = 6,position = 'identity')+theme_minimal()












install.packages("ggplot2")

library(ggplot2)
#position = 'identity' ensures that each bar represents a single observation.It positions the bars directly according to their values
ggplot(wine, aes(x = residual.sugar, fill = label)) +
  geom_histogram(position = 'identity', alpha = 0.7, bins = 30) +
  labs(title = 'Histogram of Residual Sugar by Red and White Wines', x = 'Residual Sugar', y = 'Frequency') +
  scale_fill_manual(values = c('red' = 'red', 'white' = 'lightyellow')) +
  theme_minimal()

# 6. Create a Histogram of citric.acid from the wine data. Color by red and white wines.

ggplot(wine, aes(x = citric.acid, fill = label)) +
  geom_histogram(position = 'identity', alpha = 0.7, bins = 30) +
  labs(title = 'Histogram of Citric Acid by Red and White Wines', x = 'Citric Acid', y = 'Frequency') +
  scale_fill_manual(values = c('red' = 'red', 'white' = 'lightyellow')) +
  theme_minimal()

# 7. Create a Histogram of alcohol from the wine data. Color by red and white wines.

ggplot(wine, aes(x = alcohol, fill = label)) +
  geom_histogram(position = 'identity', alpha = 0.7, bins = 30) +
  labs(title = 'Histogram of Alcohol by Red and White Wines', x = 'Alcohol', y = 'Frequency') +
  scale_fill_manual(values = c('red' = 'red', 'white' = 'lightyellow')) +
  theme_minimal()


# 8. Create a scatterplot of residual.sugar versus citric.acid, color by red and white wine.
ggplot(data=wine, aes(x=residual.sugar, y=citric.acid, color=label))+geom_point()











ggplot(wine, aes(x = citric.acid, y = residual.sugar, color = label)) +
  geom_point() +
  labs(title = 'Scatterplot of Residual Sugar vs. Citric Acid', x = 'Citric Acid', y = 'Residual Sugar') +
  scale_color_manual(values = c('red' = 'red', 'white' = 'lightyellow')) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = 'grey90'))

# 9. Create a scatterplot of volatile.acidity versus residual.sugar, color by red and white wine.

ggplot(wine, aes(x = residual.sugar, y = volatile.acidity, color = label)) +
  geom_point() +
  labs(title = 'Scatterplot of Residual Sugar vs. Volatile Acidity', x = 'Residual Sugar', y = 'Volatile Acidity') +
  scale_color_manual(values = c('red' = 'red', 'white' = 'lightyellow')) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = 'grey90'))


