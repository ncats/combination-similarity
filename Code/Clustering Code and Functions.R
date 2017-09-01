library(dendextend)

# function that takes in a dissimilarity matrix and returns the dendrogram object
matrix_to_dendro <- function(dissim_matrix) {
  # make the dissimiarity matrix into a distance matrix it will recognize 
  dist <- as.dist(dissim_matrix)
  # do the clustering of the dissimilarity matrix
  clust <- hclust(dist)
  # make the clustering into the dendrogram
  dend <- as.dendrogram(clust)
  return(dend)
}

# steps for assay 6410
assay6410 <- get.data.blocks(6410)
assay6410_RMSE_response_matrices <- RMSE_list(assay6410)
assay6410_syrjala_response_matrices <- dist.syrjala(assay6410)
hclust(assay6410_syrjala_response_matrices)

# make it a distance matrix by using the as.dist() function 
dist <- as.dist(assay6410_syrjala_response_matrices, diag=TRUE)
hclust(dist)


# 1. make the distance matrix an actual distance matrix (recognizable in r)
assay1764_RMSE_response_d <- as.dist(assay1764_RMSE_response_matrices)
# 2. clustering of the distance matrix 
assay1764_rmse_clust <- hclust(assay1764_RMSE_response_d)
# view the dendrogram of the clustering
# plot(assay1764_clust)
# 3. save the clustering as a dendrogram (list with attributes)
assay1764_rmse_dend <- as.dendrogram(assay1764_clust)
# 4. view the dendrogram
assay1764_rmse_dend %>% plot()


# repeat for other malaria assays
assay1763_RMSE_response_d <- as.dist(assay1763_RMSE_response_matrices)
assay1763_rmse_clust <- hclust(assay1763_RMSE_response_d)
assay1763_rmse_dend <- as.dendrogram(assay1763_clust)

assay1761_RMSE_response_d <- as.dist(assay1761_RMSE_response_matrices)
assay1761_rmse_clust <- hclust(assay1761_RMSE_response_d)
assay1761_rmse_dend <- as.dendrogram(assay1761_clust)

# image of all three dendrograms RMSE
par(mfrow = c(1,3))
assay1764_dend %>% 
  plot(., main="Assay 1764 RMSE Combination Responses Dendrogram")
assay1763_dend %>% 
  plot(., main="Assay 1763 RMSE Combination Responses Dendrogram")
assay1761_dend %>% 
  plot(., main="Assay 1761 RMSE Combination Responses Dendrogram")

heatmap(assay1764_RMSE_response_matrices)
heatmap(assay1763_RMSE_response_matrices)
heatmap(assay1761_RMSE_response_matrices)


# comparing the three malaria assay tanglegrams- comparing each one
tanglegram(assay1764_rmse_dend, assay1763_rmse_dend, 
           lwd=1.5,
           main= paste("RMSE Malaria Assays 1764 and 1763 Entanglement=", round(entanglement(assay1764_dend, assay1763_dend), 2)))
tanglegram(assay1764_rmse_dend, assay1761_rmse_dend, 
           lwd=1.5,
           main= paste("RMSE Malaria Assays 1764 and 1761 entanglement=", round(entanglement(assay1764_dend, assay1761_dend), 2)))
tanglegram(assay1763_rmse_dend, assay1761_rmse_dend, 
           lwd=1.5,
           main= paste("RMSE Malaria Assays 1763 and 1761 entanglement=", round(entanglement(assay1763_dend, assay1761_dend), 2)))
# comparing assay 1764 dendrogram to itself (entanglement value of 0)
tanglegram(assay1764_rmse_dend, assay1764_rmse_dend, 
           lwd=0.2,
           common_subtrees_color_lines = FALSE,
           main= paste("RMSE Malaria Assay 1764 (with self) Entanglement=", round(entanglement(assay1764_dend, assay1764_dend), 2)))


# image of all three dendrograms Syrjala
assay1764_syrjala_response_d <- as.dist(assay1764_syrjala_response_matrices)
assay1764_syrjala_clust <- hclust(assay1764_syrjala_response_d)
assay1764_syrjala_dend <- as.dendrogram(assay1764_syrjala_clust)

assay1763_syrjala_response_d <- as.dist(assay1763_syrjala_response_matrices)
assay1763_syrjala_clust <- hclust(assay1763_syrjala_response_d)
assay1763_syrjala_dend <- as.dendrogram(assay1763_syrjala_clust)

assay1761_syrjala_response_d <- as.dist(assay1761_syrjala_response_matrices)
assay1761_syrjala_clust <- hclust(assay1761_syrjala_response_d)
assay1761_syrjala_dend <- as.dendrogram(assay1761_syrjala_clust)

par(mfrow = c(1,3))
assay1764_syrjala_dend %>% 
  plot(., main="Assay 1764 Syrjala Combination Responses Dendrogram")
assay1763_syrjala_dend %>% 
  plot(., main="Assay 1763 Syrjala Combination Responses Dendrogram")
assay1761_syrjala_dend %>% 
  plot(., main="Assay 1761 Syrjala Combination Responses Dendrogram")


#par(mfrow = c(1,3))
#image(assay1764_syrjala_response_matrices)
#image(assay1763_syrjala_response_matrices)
#image(assay1761_syrjala_response_matrices)


heatmap(assay1764_syrjala_response_matrices,
        main= "Clustering of Assay 1764 Syrjala for Combination Responses")
heatmap(assay1763_syrjala_response_matrices,
        main= "Clustering of Assay 1763 Syrjala for Combination Responses")
heatmap(assay1761_syrjala_response_matrices,
        main= "Clustering of Assay 1761 Syrjala for Combination Responses")


# comparing the three malaria assay syrjala tanglegrams- comparing each one
tanglegram(assay1764_syrjala_dend, assay1763_syrjala_dend, 
           lwd=1.5,
           main= paste("Syrjala Malaria Assays 1764 and 1763 Entanglement=", round(entanglement(assay1764_syrjala_dend, assay1763_syrjala_dend), 2)))
tanglegram(assay1764_syrjala_dend, assay1761_syrjala_dend, 
           lwd=1.5,
           main= paste("Syrjala Malaria Assays 1764 and 1761 Entanglement=", round(entanglement(assay1764_syrjala_dend, assay1761_syrjala_dend), 2)))
tanglegram(assay1763_syrjala_dend, assay1761_syrjala_dend, 
           lwd=1.5,
           main= paste("Syrjala Malaria Assays 1763 and 1761 Entanglement=", round(entanglement(assay1763_syrjala_dend, assay1761_syrjala_dend), 2)))
# comparing assay 1764 dendrogram to itself (entanglement value of 0)
#tanglegram(assay1764_dend, assay1764_dend, 
#           lwd=0.2,
#           common_subtrees_color_lines = FALSE,
#           main= paste("RMSE Malaria Assay 1764 (with self) Entanglement=", round(entanglement(assay1764_dend, assay1764_dend), 2)))
tanglegram(assay1761_syrjala_dend, assay1763_syrjala_dend, 
           lwd=1.5,
           main= paste("Syrjala Malaria Assays 1761 and 1763 Entanglement=", round(entanglement(assay1761_syrjala_dend, assay1763_syrjala_dend), 2)))


# Comparing RMSE to Syrjala
tanglegram(assay1764_rmse_dend, assay1764_syrjala_dend, 
           lwd=1.5,
           main= paste("Malaria Assay 1764 RMSE to Syrjala Entanglement=", round(entanglement(assay1764_rmse_dend, assay1764_syrjala_dend), 2)))
tanglegram(assay1763_rmse_dend, assay1763_syrjala_dend, 
           lwd=1.5,
           main= paste("Malaria Assay 1763 RMSE to Syrjala Entanglement=", round(entanglement(assay1763_rmse_dend, assay1763_syrjala_dend), 2)))
tanglegram(assay1761_rmse_dend, assay1761_syrjala_dend, 
           lwd=1.5,
           main= paste("Malaria Assay 1761 RMSE to Syrjala Entanglement=", round(entanglement(assay1761_rmse_dend, assay1761_syrjala_dend), 2)))



# A. Visualization of all of the data
# through Multidimensional Scaling in R of Malaria Assays Syrjala Test
fit <- cmdscale(assay1764_RMSE_response_matrices, eig=TRUE, k=2) # k is the number of dim
fit # view results
# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric	MDS- Assay 1764 RMSE Response Matrices")


# MultiDimensional Scaling for Malaria Assays
assay1764_syrjala_mds <- cmdscale(assay1764_syrjala_response_matrices, eig=TRUE, k=2) # k is the number of dim
assay1763_syrjala_mds <- cmdscale(assay1763_syrjala_response_matrices, eig=TRUE, k=2) # k is the number of dim
assay1761_syrjala_mds <- cmdscale(assay1761_syrjala_response_matrices, eig=TRUE, k=2) # k is the number of dim


# Saving the points to dataframes so they can be plotted
assay1764_syrjala_mds_points <- as.data.frame(assay1764_syrjala_mds$points)
assay1763_syrjala_mds_points <- as.data.frame(assay1763_syrjala_mds$points)
assay1761_syrjala_mds_points <- as.data.frame(assay1761_syrjala_mds$points)


# Plotting the results of multidimensional scaling to see the MDS for the Malaria Assays
#fit # view results
# plot solution 
#x <- assay1764_syrjala_mds$points[,1]
#y <- assay1764_syrjala_mds$points[,2]
par(mfrow = c(1,3))
plot(assay1764_syrjala_mds$points[,1], assay1764_syrjala_mds$points[,2], xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric	MDS- Assay 1764 Syrjala Response Matrices")
plot(assay1763_syrjala_mds$points[,1], assay1763_syrjala_mds$points[,2], xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric	MDS- Assay 1763 Syrjala Response Matrices")
plot(assay1761_syrjala_mds$points[,1], assay1761_syrjala_mds$points[,2], xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric	MDS- Assay 1761 Syrjala Response Matrices")


# B. Determining the appropriate number of clusters to use for a dissim matrix

# one way: silhouette width- can find this using the cluster.stats() function

# cluster quality metrics- is 5 groups a good number?
assay1764_cluster_quality <- cluster.stats(assay1764_syrjala_response_matrices, assay1764_groups)
assay1763_cluster_quality <- cluster.stats(assay1763_syrjala_response_matrices, assay1763_groups)
assay1761_cluster_quality <- cluster.stats(assay1761_syrjala_response_matrices, assay1761_groups)

# function that returns the avg silhouette width for each number of clusters (k)
# returns a single number for each number of clusters 
avg_clust_widths <- function(dissim_matrix, k.max=15) {
  
  # create an empty matrix for the data to go
  avgS_widths <- matrix(nrow=k.max-1, ncol=2)
  
  # make sure the matrix is in the correct distance matrix format
  dist_matrix <- dist(dissim_matrix)
  # cluster the distance matrix
  matrix_clustering <- hclust(dist_matrix)
  
  # for loop that makes the following occur for each number of clusters 
  # from 2 to the max number of clusters
  for (i in 2:k.max) {
    
    # adds the number of clusters (2, 3, ..., k.max) to the left column
    avgS_widths[i-1,1] <- i
    
    # groups the clustering into the k number of clusters (2, 3, ..., k.max)
    clusters <- cutree(matrix_clustering, k=i)
    # determine the cluster quality metrics for the grouping of the specific number of clusters
    quality <- cluster.stats(dissim_matrix, clusters)
    
    # extract the average silhouette width and save it to the right column of the matrix
    avgS_widths[i-1,2] <- quality$avg.silwidth[1]
  }
  
  # return the matrix with all of the values 
  return(avgS_widths)
}


# function that returns a list all of the sil widths for each of the number of clusters
# returns all of the widths for each cluster, returns several numbers for each number of clusters 
clus_widths <- function(dissim_matrix, k.max=15) {
  
  # create an empty list for all of the data to go in 
  sil_widths <- list()
  
  # make sure the matrix is in the correct distance matrix format
  dist_matrix <- dist(dissim_matrix)
  # cluster the distance matrix
  matrix_clustering <- hclust(dist_matrix)
  
  # for loop that makes the following occur for each number of clusters 
  # from 2 to the max number of clusters
  for (i in 2:k.max) {

    # groups the clustering into the k number of clusters (2, 3, ..., k.max)
    clusters <- cutree(matrix_clustering, k=i)
    # determine the cluster quality metrics for the grouping of the specific number of clusters
    quality <- cluster.stats(dissim_matrix, clusters)
    
    # extract the avg sil widths for each of the clusters and 
    # save it to the correct item in the list
    sil_widths[[i-1]] <- quality$clus.avg.silwidths
  }
  
  # return the list with all the data in it 
  return(sil_widths)
}


# function that does the previous differently so you can make histograms in the plot
clus_widths_hist <- function(dissim_matrix, k.max=15) {
  
  # making an empty matrix with the correct number of rows (depending on k.max) and 2 columns
  # ((k.max^2)+k.max)/2)-1 makes the number of number of rows have (two 2's, three 3's, ..., k.max k.max's)
  sil_widths <- matrix(nrow=(((k.max^2)+k.max)/2)-1, ncol=2)
  
  # make sure the matrix is in the correct distance matrix format
  dist_matrix <- dist(dissim_matrix)
  # cluster the distance matrix
  matrix_clustering <- hclust(dist_matrix)
  
  # making an index for where the for loop should start
  start_index <- 1
  
  for (i in 2:k.max) {
    
    # fill in the left column with the number of clusters
    for (j in 1 : i) {
      sil_widths[j+start_index-1] <- i
    } 
    
    # groups the clustering into the k number of clusters (2, 3, ..., k.max)
    clusters <- cutree(matrix_clustering, k=i)
    # determine the cluster quality metrics for the grouping of the specific number of clusters
    quality <- cluster.stats(dissim_matrix, clusters)
    # creates a vector with all of the avg cluster widths
    avg_clust_widths <- as.vector(quality$clus.avg.silwidths)
    
    # fill in the right column with all of the values for each of the clusters (length of vector = number of clusters)
    for (m in 1:length(avg_clust_widths)) {
      sil_widths[m+start_index-1,2] <- avg_clust_widths[m]
    }
    
    #update the starting index at the end of the for loop
    start_index <- start_index + i
  }
  
  # return the matrix with all the data as a data frame
  return(as.data.frame(sil_widths))
}


# making boxplots for the malaria assays to see the distribution of avg cluster widths for each number of clusters
assay1764_clus_widths <- clus_widths_hist(assay1764_syrjala_response_matrices)
assay1763_clus_widths <- clus_widths_hist(assay1763_syrjala_response_matrices)
assay1761_clus_widths <- clus_widths_hist(assay1761_syrjala_response_matrices)

p1 <- ggplot(assay1764_clus_widths, aes(factor(assay1764_clus_widths$V1), assay1764_clus_widths$V2)) + 
  geom_boxplot() +
  labs(
    x = "Number of Clusters",
    y = "Average Silhouette Widths",
    title = "Assay 1764 Cluster Quality Metrics"
  )
p2 <- ggplot(assay1763_clus_widths, aes(factor(assay1763_clus_widths$V1), assay1763_clus_widths$V2)) + 
  geom_boxplot() +
  labs(
    x = "Number of Clusters",
    y = "Average Silhouette Widths",
    title = "Assay 1763 Cluster Quality Metrics"
  )
p3 <- ggplot(assay1761_clus_widths, aes(factor(assay1761_clus_widths$V1), assay1761_clus_widths$V2)) + 
  geom_boxplot() +
  labs(
    x = "Number of Clusters",
    y = "Average Silhouette Widths",
    title = "Assay 1761 Cluster Quality Metrics"
  )
grid.arrange(ncol=3, p1, p2, p3)
# (decided that 1764 should have 4, 1763 should have 3. and 1761 should have 3)


# making boxplots for the additional assays to see the distribution of avg cluster widths for each number of clusters
assay6503_clus_widths <- clus_widths_hist(assay6503_syrjala_response_matrices)
assay7021_clus_widths <- clus_widths_hist(assay7021_syrjala_response_matrices)

p4 <- ggplot(assay6503_clus_widths, aes(factor(assay6503_clus_widths$V1), assay6503_clus_widths$V2)) + 
  geom_boxplot() +
  labs(
    x = "Number of Clusters",
    y = "Average Silhouette Widths",
    title = "Assay 6503 Cluster Quality Metrics"
  )
p5 <- ggplot(assay7021_clus_widths, aes(factor(assay7021_clus_widths$V1), assay7021_clus_widths$V2)) + 
  geom_boxplot() +
  labs(
    x = "Number of Clusters",
    y = "Average Silhouette Widths",
    title = "Assay 7021 Cluster Quality Metrics"
  )
grid.arrange(ncol=2, p4, p5)


# initial method: shows less information than boxplots- but still interesting
# making scatterplots with the average of the average sil widths for each number of clusters

par(mfrow=c(1,3))
assay1764_avg_sil <- number_clusters(assay1764_syrjala_response_matrices)
assay1763_avg_sil <- number_clusters(assay1763_syrjala_response_matrices)
assay1761_avg_sil <- number_clusters(assay1761_syrjala_response_matrices)

plot(assay1764_avg_sil, 
     xlab="Number of Clusters", ylab="Average Silhouette Width",
     main="Assay 1764 Cluster Quality Metrics")
plot(assay1763_avg_sil, 
     xlab="Number of Clusters", ylab="Average Silhouette Width",
     main="Assay 1763 Cluster Quality Metrics")
plot(assay1761_avg_sil, 
     xlab="Number of Clusters", ylab="Average Silhouette Width",
     main="Assay 1761 Cluster Quality Metrics")

# working with assays 6503 and 7021
assay6503 <- get.data.blocks(6503)
assay6503_good <- good_indices(assay6503)
assay6503_sub <- good_subset(assay6503, assay6503_good)
assay6503_syrjala_response_matrices <- dist.syrjala(assay6503_sub)
assay6503_avg_sil <- number_clusters(assay6503_syrjala_response_matrices) # changed the name of the function

assay7021 <- get.data.blocks(7021)
assay7021_good <- good_indices(assay7021)
assay7021_sub <- good_subset(assay7021, assay7021_good)
assay7021_syrjala_response_matrices <- dist.syrjala(assay7021_sub)
assay7021_avg_sil <- number_clusters(assay7021_syrjala_response_matrices)

par(mfrow=c(1,2))
plot(assay6503_avg_sil, 
     xlab="Number of Clusters", ylab="Average Silhouette Width",
     main="Assay 6503 Cluster Quality Metrics")
plot(assay7021_avg_sil, 
     xlab="Number of Clusters", ylab="Average Silhouette Width",
     main="Assay 7021 Cluster Quality Metrics")



# Another way: elbow method- looking for the "bend"- 
# code found online, don't completely understand
par(mfrow = c(1,2))
k.max <- 15 # Maximal number of clusters
# code 1 for which number of clusters to use
wss <- (nrow(assay1764_syrjala_mds_points)-1)*sum(apply(assay1764_syrjala_mds_points,2,var))
for (i in 2:k.max) wss[i] <- sum(kmeans(assay1764_syrjala_mds_points,
                                        centers=i)$withinss)
plot(1:k.max, wss, 
     type="b", pch = 19,
     main= "Method A for Number of Clusters- Assay 1764",
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# code 2 for number of clusters to use
set.seed(123)
# Compute and plot wss for k = 2 to k = 15
wss2 <- sapply(1:k.max, 
               function(k){kmeans(assay1764_syrjala_mds_points, k, nstart=10)$tot.withinss})
plot(1:k.max, wss2,
     type="b", pch = 19,
     main= "Method B for Number of Clusters- Assay 1764",
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

par(mfrow = c(1,2))
k.max <- 15 # Maximal number of clusters
# code 1 for which number of clusters to use
wss <- (nrow(assay1763_syrjala_mds_points)-1)*sum(apply(assay1763_syrjala_mds_points,2,var))
for (i in 2:k.max) wss[i] <- sum(kmeans(assay1763_syrjala_mds_points,
                                        centers=i)$withinss)
plot(1:k.max, wss, 
     type="b", pch = 19,
     main= "Method A for Number of Clusters- Assay 1763",
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# code 2 for number of clusters to use
set.seed(123)
# Compute and plot wss for k = 2 to k = 15
wss2 <- sapply(1:k.max, 
               function(k){kmeans(assay1763_syrjala_mds_points, k, nstart=10)$tot.withinss})
plot(1:k.max, wss2,
     type="b", pch = 19,
     main= "Method B for Number of Clusters- Assay 1763",
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

par(mfrow = c(2,3))
k.max <- 15 # Maximal number of clusters
# code 1 for which number of clusters to use
wss1764a <- (nrow(assay1764_syrjala_mds_points)-1)*sum(apply(assay1764_syrjala_mds_points,2,var))
for (i in 2:k.max) wss1764a[i] <- sum(kmeans(assay1764_syrjala_mds_points,
                                             centers=i)$withinss)
plot(1:k.max, wss1764a, 
     type="b", pch = 19,
     main= "Method A for Number of Clusters- Assay 1764",
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# code 1 for which number of clusters to use
wss1763a <- (nrow(assay1763_syrjala_mds_points)-1)*sum(apply(assay1763_syrjala_mds_points,2,var))
for (i in 2:k.max) wss1763a[i] <- sum(kmeans(assay1763_syrjala_mds_points,
                                             centers=i)$withinss)
plot(1:k.max, wss1763a, 
     type="b", pch = 19,
     main= "Method A for Number of Clusters- Assay 1763",
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# code 1 for which number of clusters to use
wss1761a <- (nrow(assay1761_syrjala_mds_points)-1)*sum(apply(assay1761_syrjala_mds_points,2,var))
for (i in 2:k.max) wss1761a[i] <- sum(kmeans(assay1761_syrjala_mds_points,
                                             centers=i)$withinss)
plot(1:k.max, wss1761a, 
     type="b", pch = 19,
     main= "Method A for Number of Clusters- Assay 1761",
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# code 2 for number of clusters to use
set.seed(123)
# Compute and plot wss for k = 2 to k = 15
wss1764b <- sapply(1:k.max, 
                   function(k){kmeans(assay1764_syrjala_mds_points, k, nstart=10)$tot.withinss})
plot(1:k.max, wss1764b,
     type="b", pch = 19,
     main= "Method B for Number of Clusters- Assay 1764",
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
# code 2 for number of clusters to use
set.seed(123)
# Compute and plot wss for k = 2 to k = 15
wss1763b <- sapply(1:k.max, 
                   function(k){kmeans(assay1763_syrjala_mds_points, k, nstart=10)$tot.withinss})
plot(1:k.max, wss1763b,
     type="b", pch = 19,
     main= "Method B for Number of Clusters- Assay 1763",
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
# code 2 for number of clusters to use
set.seed(123)
# Compute and plot wss for k = 2 to k = 15
wss1761b <- sapply(1:k.max, 
                   function(k){kmeans(assay1761_syrjala_mds_points, k, nstart=10)$tot.withinss})
plot(1:k.max, wss1761b,
     type="b", pch = 19,
     main= "Method B for Number of Clusters- Assay 1761",
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")





# C. Visualization of the data with the number of groups (groups of clustering)

# Way 1 Visualization: Dendrograms

# Now that you've determined 4,3,and 3 clusters, cut the dendrograms into the respective numbers
par(mfrow = c(1,3))

assay1764_syrjala_dend %>% plot(., main="Assay 1764 Syrjala Combination Responses Dendrogram- 4")
# use cutree function to split the dendrogram into groups (k number of groups, k = 4 as decided from before)
assay1764_groups4 <- cutree(assay1764_syrjala_clust, k=4)
# view the groupings on the dendrogram 
rect.hclust(assay1764_syrjala_clust, k=4, border="red")

assay1763_syrjala_dend %>% plot(., main="Assay 1763 Syrjala Combination Responses Dendrogram- 3")
# use cutree function to split the dendrogram into groups (k number of groups, k = 3 as decided from before)
assay1763_groups3 <- cutree(assay1763_syrjala_clust, k=3)
# view the groupings on the dendrogram 
rect.hclust(assay1763_syrjala_clust, k=3, border="red")

assay1761_syrjala_dend %>% plot(., main="Assay 1761 Syrjala Combination Responses Dendrogram- 3")
# use cutree function to split the dendrogram into groups (k number of groups, k = 3 as decided from before)
assay1761_groups3 <- cutree(assay1761_syrjala_clust, k=3)
# view the groupings on the dendrogram 
rect.hclust(assay1761_syrjala_clust, k=3, border="red")


# Way 2 Visualization: MDS with color coordaination of points
assay1764_syrjala_mds_points$groups4 <- assay1764_groups4
assay1763_syrjala_mds_points$groups3 <- assay1763_groups3
assay1761_syrjala_mds_points$groups3 <- assay1761_groups3


# the adjacent number of clusters (+/- 1) clusters
assay1764_groups3 <- cutree(assay1764_syrjala_clust, k=3)
assay1764_groups5 <- cutree(assay1764_syrjala_clust, k=5)
assay1764_syrjala_mds_points$groups3 <- assay1764_groups3
assay1764_syrjala_mds_points$groups5 <- assay1764_groups5

assay1763_groups2 <- cutree(assay1763_syrjala_clust, k=2)
assay1763_groups4 <- cutree(assay1763_syrjala_clust, k=4)
assay1763_syrjala_mds_points$groups2 <- assay1763_groups2
assay1763_syrjala_mds_points$groups4 <- assay1763_groups4

assay1761_groups2 <- cutree(assay1761_syrjala_clust, k=2)
assay1761_groups4 <- cutree(assay1761_syrjala_clust, k=4)
assay1761_syrjala_mds_points$groups2 <- assay1761_groups2
assay1761_syrjala_mds_points$groups4 <- assay1761_groups4


# Viewing all of the malaria assays with their optimal(subjective) number of clusters (4,3,3)
p6 <- ggplot(data=assay1764_syrjala_mds_points, aes(x=V1, y=V2)) +
  geom_point(aes(color=factor(groups))) + 
  labs(title="Assay 1764 MDS With Four Clusters")
p7 <- ggplot(data=assay1763_syrjala_mds_points, aes(x=V1, y=V2)) +
  geom_point(aes(color=factor(groups))) + 
  labs(title="Assay 1763 MDS With Three Clusters")
p8 <- ggplot(data=assay1761_syrjala_mds_points, aes(x=V1, y=V2)) +
  geom_point(aes(color=factor(groups))) + 
  labs(title="Assay 1761 MDS With Three Clusters")
grid.arrange(ncol=3, p6, p7, p8)
#ggsave("MalariaAssays_syrjala_mds_5clustes",plot=grid.arrange(ncol=3, p6, p7, p8), width=10, height=8, units=inches)

# + scale_color_gradientn(colours = rainbow(5))

#ggplot(data=assay1764_syrjala_mds_points, aes(x=V1, y=V2, color=groups)) +
#  geom_point() +
#  scale_color_gradient(low="blue", high="red")


# Viewing each of the malaria assays with their adjacent number of clusters 
p17 <- ggplot(data=assay1764_syrjala_mds_points, aes(x=V1, y=V2)) +
  geom_point(aes(color=factor(groups3))) + 
  labs(title="Assay 1764 MDS With Three Clusters")
p18 <- ggplot(data=assay1764_syrjala_mds_points, aes(x=V1, y=V2)) +
  geom_point(aes(color=factor(groups4))) + 
  labs(title="Assay 1764 MDS With Four Clusters")
p19 <- ggplot(data=assay1764_syrjala_mds_points, aes(x=V1, y=V2)) +
  geom_point(aes(color=factor(groups5))) + 
  labs(title="Assay 1764 MDS With Five Clusters")
grid.arrange(ncol=3, p17, p18, p19)

p20 <- ggplot(data=assay1763_syrjala_mds_points, aes(x=V1, y=V2)) +
  geom_point(aes(color=factor(groups2))) + 
  labs(title="Assay 1763 MDS With Two Clusters")
p21 <- ggplot(data=assay1763_syrjala_mds_points, aes(x=V1, y=V2)) +
  geom_point(aes(color=factor(groups3))) + 
  labs(title="Assay 1763 MDS With Three Clusters")
p22 <- ggplot(data=assay1763_syrjala_mds_points, aes(x=V1, y=V2)) +
  geom_point(aes(color=factor(groups4))) + 
  labs(title="Assay 1763 MDS With Four Clusters")
grid.arrange(ncol=3, p20, p21, p22)

p23 <- ggplot(data=assay1761_syrjala_mds_points, aes(x=V1, y=V2)) +
  geom_point(aes(color=factor(groups2))) + 
  labs(title="Assay 1761 MDS With Two Clusters")
p24 <- ggplot(data=assay1761_syrjala_mds_points, aes(x=V1, y=V2)) +
  geom_point(aes(color=factor(groups3))) + 
  labs(title="Assay 1761 MDS With Three Clusters")
p25 <- ggplot(data=assay1761_syrjala_mds_points, aes(x=V1, y=V2)) +
  geom_point(aes(color=factor(groups4))) + 
  labs(title="Assay 1761 MDS With Four Clusters")
grid.arrange(ncol=3, p23, p24, p25)


# repeating for assay 6503
assay6503_syrjala_mds <- cmdscale(assay6503_syrjala_response_matrices, eig=TRUE, k=2) # k is the number of dim
assay6503_syrjala_mds_points <- as.data.frame(assay6503_syrjala_mds$points)

assay6503_syrjala_response_d <- as.dist(assay6503_syrjala_response_matrices)
assay6503_clust <- hclust(assay6503_syrjala_response_d)

# looking how different numbers of clusters (2-7) looks different for the same MDS of an assay 
assay6503_2groups <- cutree(assay6503_clust, k=2)
assay6503_3groups <- cutree(assay6503_clust, k=3)
assay6503_4groups <- cutree(assay6503_clust, k=4)
assay6503_5groups <- cutree(assay6503_clust, k=5)
assay6503_6groups <- cutree(assay6503_clust, k=6)
assay6503_7groups <- cutree(assay6503_clust, k=7)

assay6503_syrjala_mds_points$groups2 <- assay6503_2groups
assay6503_syrjala_mds_points$groups3 <- assay6503_3groups
assay6503_syrjala_mds_points$groups4 <- assay6503_4groups
assay6503_syrjala_mds_points$groups5 <- assay6503_5groups
assay6503_syrjala_mds_points$groups6 <- assay6503_6groups
assay6503_syrjala_mds_points$groups7 <- assay6503_7groups


p9 <- ggplot(data=assay6503_syrjala_mds_points, aes(x=V1, y=V2)) +
  geom_point(aes(color=factor(groups2))) + 
  labs(title="Assay 6503 MDS Two Clusters; AvgSilWid=0.863")
p10 <- ggplot(data=assay6503_syrjala_mds_points, aes(x=V1, y=V2)) +
  geom_point(aes(color=factor(groups3))) + 
  labs(title="Assay 6503 MDS Three Clusters; AvgSilWid=0.833")
p11 <- ggplot(data=assay6503_syrjala_mds_points, aes(x=V1, y=V2)) +
  geom_point(aes(color=factor(groups4))) + 
  labs(title="Assay 6503 MDS Four Clusters; AvgSilWid=0.826")
p12 <- ggplot(data=assay6503_syrjala_mds_points, aes(x=V1, y=V2)) +
  geom_point(aes(color=factor(groups5))) + 
  labs(title="Assay 6503 MDS Five Clusters; AvgSilWid=0.815")
p13 <- ggplot(data=assay6503_syrjala_mds_points, aes(x=V1, y=V2)) +
  geom_point(aes(color=factor(groups6))) + 
  labs(title="Assay 6503 MDS Six Clusters; AvgSilWid=0.646")
p14 <- ggplot(data=assay6503_syrjala_mds_points, aes(x=V1, y=V2)) +
  geom_point(aes(color=factor(groups7))) + 
  labs(title="Assay 6503 MDS Seven Clusters; AvgSilWid=0.649")
grid.arrange(ncol=3, p9, p10, p11, p12, p13, p14)




# extra code
fviz_cluster(list(data = assay1764_RMSE_response_d, cluster = assay1764_groups))

assay1763_RMSE_response_d <- as.dist(assay1763_RMSE_response_matrices)
assay1763_clust <- hclust(assay1763_RMSE_response_d)
plot(assay1763_clust)