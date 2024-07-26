#Name :Shashank Barai
#Matricluation No: 11038167

library(factoextra)
# Load the dataset using read.delim('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/vltava-spe.txt', row.names = 1)

url <- "https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/vltava-spe.txt"
df <- read.delim(url, row.names = 1)

# Checking the structure of the dataset
str(df)
summary(df)


#Ensuring if all columns in dataset are numeric 
df_numeric <- df[, sapply(df, is.numeric)]

#Checking if there any nul values
is.na(df_numeric)


# Now we scale the df which is must for clustering algorithm
df_scaled <- scale(df_numeric)


#Now calculating distance matrix using the scaled data

dist_matrix <- dist(df_scaled, method = "euclidean")


#Implementing hierarchical clustering using the complete-linkage method

heirch_clust <- hclust(dist_matrix, method = "complete")

#Plotting the dendrogram to visualize the clustering structure

plot(heirch_clust)

# checking the clusters for understanding 
clusters <- cutree(heirch_clust, k = 4)
clusters  #I found that majority of points belong to cluster 1

#Plotting dendrogram with cluster borders and k =4
rect.hclust(heirch_clust, k=4, border=2:5)

#or using library(factoextra)
fviz_dend(as.dendrogram(heirch_clust), k = 4, k_colors = c("blue", "green", "red", "purple"), rect = TRUE)

