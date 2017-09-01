## To Run Overnight

assay6503 <- get.data.blocks(6503)
assay7021 <- get.data.blocks(7021)

assay6503_good <- good_indices(assay6503)
assay7021_good <- good_indices(assay7021)

assay6503_sub <- good_subset(assay6503, assay6503_good)
assay7021_sub <- good_subset(assay7021, assay7021_good)

# run from here overnight
assay6503_syrjala_response_matrices <- dist.syrjala(assay6503_sub)
assay7021_syrjala_response_matrices <- dist.syrjala(assay7021_sub)

assay6503_syrjala_response_d <- as.dist(assay6503_syrjala_response_matrices)
assay7021_syrjala_response_d <- as.dist(assay7021_syrjala_response_matrices)

assay6503_syrjala_clust <- hclust(assay6503_syrjala_response_d)
assay7021_syrjala_clust <- hclust(assay7021_syrjala_response_d)

assay6503_syrjala_dend <- as.dendrogram(assay6503_syrjala_clust)
assay7021_syrjala_dend <- as.dendrogram(assay7021_syrjala_clust)


par(mfrow = c(1,2))

assay6503_syrjala_dend %>% plot(., main="Assay 6503 Syrjala Combination Responses Dendrogram- 5")
# use cutree function to split the dendrogram into groups (k number of groups, k = 5 as decided from before)
assay6503_5groups <- cutree(assay6503_syrjala_clust, k=5)
# view the groupings on the dendrogram 
rect.hclust(assay6503_syrjala_clust, k=5, border="red")

assay7021_syrjala_dend %>% plot(., main="Assay 7021 Syrjala Combination Responses Dendrogram- 4")
# use cutree function to split the dendrogram into groups (k number of groups, k = 4 as decided from before)
assay7021_4groups <- cutree(assay7021_syrjala_clust, k=4)
# view the groupings on the dendrogram 
rect.hclust(assay7021_syrjala_clust, k=4, border="red")


assay6503_syrjala_mds <- cmdscale(assay6503_syrjala_response_matrices, eig=TRUE, k=2) # k is the number of dim
assay6503_syrjala_mds_points <- as.data.frame(assay6503_syrjala_mds$points)

assay7021_syrjala_mds <- cmdscale(assay7021_syrjala_response_matrices, eig=TRUE, k=2) # k is the number of dim
assay7021_syrjala_mds_points <- as.data.frame(assay7021_syrjala_mds$points)

#assay6503_5groups <- cutree(assay6503_syrjala_clust, k=5)
#assay7021_4groups <- cutree(assay7021_syrjala_clust, k=4)
                            
assay6503_syrjala_mds_points$groups5 <- assay6503_5groups
assay7021_syrjala_mds_points$groups4 <- assay7021_4groups


p15 <- ggplot(data=assay6503_syrjala_mds_points, aes(x=V1, y=V2)) +
  geom_point(aes(color=factor(groups5))) + 
  labs(title="Assay 6503 MDS Five Clusters")
p16 <- ggplot(data=assay7021_syrjala_mds_points, aes(x=V1, y=V2)) +
  geom_point(aes(color=factor(groups4))) + 
  labs(title="Assay 7021 MDS Four Clusters")
grid.arrange(ncol=2, p15, p16)




