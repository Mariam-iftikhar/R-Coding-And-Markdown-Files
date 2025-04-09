install.packages("readr")
library(readr)
library(data.table)

data <- read_csv("C:/Users/maria/OneDrive/Desktop/Framingham_Training.csv")

#For the following exercises, work with the Framingham_training and Framingham_test data sets. Use only the Sex and Age fields. Standardize Age.
#18. Run k‐means clustering on the Framingham_training data set, requesting k = 2 clusters.


X <- subset(data, select = c("Sex", "Age"))
X
Xs <-  as.data.frame(scale(X))
Xs
colnames(Xs) <- c("Sex_z", "Age_z")
kmeans01<- kmeans(Xs, centers = 2)
cluster <- as.factor(kmeans01$cluster)
Cluster1 <- Xs[ which(cluster == 1), ]
Cluster2 <- Xs[ which(cluster == 2), ]

#19. Construct a table of statistics summarizing your clusters. Describe what these two clusters consist of.
summary(Cluster1)
summary(Cluster2)




