# Working with assay 265

assay265 <- get.data.blocks(265)
RMSE_matrix <- RMSE_list(assay265)
euclidean_matrix <- euclidean_list(assay265)
ks_matrix <- ks_list(assay265)
syrjala_matrix <- dist.syrjala(assay265)




par(mfrow=c(2,2))

image(x=1:20, y=1:20, RMSE_matrix, 
      main="Assay 265 RMSE", 
      xlab="Block Number", ylab="Block Number")
image(x=1:20, y=1:20, euclidean_matrix,
      main="Assay 265 Euclidean Distance", 
      xlab="Block Number", ylab="Block Number")
image(x=1:20, y=1:20, ks_matrix, 
      main="Assay 265 KS Test", 
      xlab="Block Number", ylab="Block Number")
image(x=1:20, y=1:20, syrjala_matrix, 
      main="Assay 265 Syrjala Distance", 
      xlab="Block Number", ylab="Block Number")




heatmap(RMSE_matrix, main="Assay 265 RMSE Clustering")
heatmap(euclidean_matrix, main="Assay 265 Euclidean Clustering")
heatmap(ks_matrix, main="Assay 265 KS Test Clustering")
heatmap(syrjala_matrix, main="Assay 265 Syrjala Test Clustering")






assay265_response_matrices2 <- compute.similarity(assay265, "rmse", digits=0)
assay265_bliss_matrices2 <- delta.bliss.similarity(assay265, method="RMSE")

plot(assay265_bliss_matrices2 ~ assay265_response_matrices2)
junklm <- lm(as.vector(assay265_bliss_matrices2) ~ as.vector(assay265_response_matrices2))
plot(junklm)

par(mfrow=c(1,2))
plot(assay265_bliss_matrices2 ~ assay265_response_matrices2)
plot(junklm)


