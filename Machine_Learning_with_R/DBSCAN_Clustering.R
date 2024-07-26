#Name:Shashank Barai
#Matriculation No : 11038167

# Installing necessary packages
install.packages("fpc")
lbrary("fpc")

library(dbscan)
library(factoextra)

# Loading the multishapes dataset and extracts the first two columns of it
data("multishapes", package = "factoextra")
#df1<-multishapes
#head(df1) just to check the whole data frame
df <- multishapes[, 1:2]# extracting only first two numeric coulmns which we will use for clustering


# Using kNNdistplot try to find the best eps value. 
kNNdistplot(df, k=3)
eps=0.15# we choose this value because from this point there is rapid increase in density i.e the elbow point
abline(h=0.15, lty=2)# used dash line to pass through e=0.15 for better visual




#. Implementing DBSCAN clustering with the parameter eps=0.15 we found and setting MinPts to 5.
library("fpc")# used for clustering and its validation
set.seed(123)
db_scan <- fpc::dbscan(df, eps = 0.15, MinPts = 5)
db_scan


# Plotting DBSCAN results using plot function
plot(db_scan, df, main = "DBSCAN", frame = FALSE)


# Plotting clusters using fviz_cluster function
library("factoextra")
fviz_cluster(db_scan, df, stand = FALSE, frame = FALSE, geom = "point")
