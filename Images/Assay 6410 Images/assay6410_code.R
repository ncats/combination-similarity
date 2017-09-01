# Working with assay 6410

assay6410 <- get.data.blocks(6410)





# RMSE
assay6410_rmse_similarity_response_matrices <- RMSE_list(assay6410)
assay6410_rmse_similarity_bliss_matrices <- delta_bliss_similarity(assay6410, method="rmse")
## cleaning- replacing the rows of 0's to rows of NA's
assay6410_rmse_similarity_bliss_matrices[c(11,23),] <- NA
assay6410_rmse_similarity_bliss_matrices[,c(11,23)] <- NA

# Euclidean Distance
assay6410_euclidean_similarity_response_matrices <- euclidean_list(assay6410)
assay6410_euclidean_similarity_bliss_matrices <- delta_bliss_similarity(assay6410, method="euclidean")
## cleaning- replacing the rows of 0's to rows of NA's
assay6410_euclidean_similarity_bliss_matrices[c(11,23),] <- NA
assay6410_euclidean_similarity_bliss_matrices[,c(11,23)] <- NA

# KS Test
assay6410_ks_similarity_response_matrices <- ks_list(assay6410)
assay6410_ks_similarity_bliss_matrices <- delta_bliss_similarity(assay6410, method="ks")
## cleaning
## similarity of delta bliss matrices
assay6410_ks_similarity_bliss_matrices[c(11,23),] <- NA
assay6410_ks_similarity_bliss_matrices[,c(11,23)] <- NA

# Syrjala Test
assay6410_syrjala_similarity_response_matrices <- dist.syrjala(assay6410)
assay6410_syrjala_similarity_bliss_matrices <- delta_bliss_similarity(assay6410, method="syrjala")
## cleaning- replacing the rows of 0's to rows of NA's
## similarity of response matrices
assay6410_syrjala_similarity_response_matrices[c(11,23),] <- NA
assay6410_syrjala_similarity_response_matrices[,c(11,23)] <- NA
## similarity of delta bliss matrices
assay6410_syrjala_similarity_bliss_matrices[c(11,23),] <- NA
assay6410_syrjala_similarity_bliss_matrices[,c(11,23)] <- NA




pdf("Assay6410ComparingSimilaritMetrics.pdf", width=8, height=16)
par(mfrow=c(4,2))
# RMSE
assay6410_rmse_similarity_response_matrices %>% 
  image(x=1:24, y=1:24, 
        main="Assay 6410 RMSE Response Matrices", 
        xlab="Block Number", ylab="Block Number")
assay6410_rmse_similarity_bliss_matrices %>% 
  image(x=1:24, y=1:24, 
        main="Assay 6410 RMSE Delta Bliss Matrices", 
        xlab="Block Number", ylab="Block Number")
# Euclidean Distance
assay6410_euclidean_similarity_response_matrices %>% 
  image(x=1:24, y=1:24, 
        main="Assay 6410 Euclidean Response Matrices", 
        xlab="Block Number", ylab="Block Number")
assay6410_euclidean_similarity_bliss_matrices %>% 
  image(x=1:24, y=1:24, 
        main="Assay 6410 Euclidean Delta Bliss Matrices", 
        xlab="Block Number", ylab="Block Number")
# KS Test
assay6410_ks_similarity_response_matrices %>% 
  image(x=1:24, y=1:24, 
        main="Assay 6410 KS Response Matrices", 
        xlab="Block Number", ylab="Block Number")
assay6410_ks_similarity_bliss_matrices %>% 
  image(x=1:24, y=1:24, 
        main="Assay 6410 KS Delta Bliss Matrices", 
        xlab="Block Number", ylab="Block Number")
# Syrjala Test
assay6410_syrjala_similarity_response_matrices %>%
  image(x=1:24, y=1:24, 
        main="Assay 6410 Syrjala Response Matrices", 
        xlab="Block Number", ylab="Block Number")
assay6410_syrjala_similarity_bliss_matrices %>%
  image(x=1:24, y=1:24, 
        main="Assay 6410 Syrjala Delta Bliss Matrices", 
        xlab="Block Number", ylab="Block Number")
dev.off()





# Clustering of RMSE and Euclidean for Assay 6410 NOTE: clustering does not work with NA values... what to do?

heatmap(assay6410_rmse_similarity_response_matrices, 
        main="Assay 6410 RMSE Response Matrices Clustering")
heatmap(assay6410_rmse_similarity_bliss_matrices, 
        main="Assay 6410 RMSE Delta Bliss Clustering")
heatmap(assay6410_euclidean_similarity_response_matrices, 
        main="Assay 6410 Euclidean Response Matrices Clustering")
heatmap(assay6410_euclidean_similarity_bliss_matrices, 
        main="Assay 6410 Euclidean Delta Bliss Matrices Clustering")
heatmap(assay6410_syrjala_similarity_response_matrices, 
        main="Assay 6410 Syrjala Response Matrices Clustering")
heatmap(assay6410_syrjala_similarity_bliss_matrices, 
        main="Assay 6410 Syrjala Delta Bliss Matrices Clustering")


# Seeing if there is a correlation between the comparison of the response matrices and the comparison of the delta bliss matrices




pdf("Assay6410CorelationResponseAndBliss.pdf", width = 13, height=8)
par(mfrow=c(2,2))

# Using RMSE 
# visualizing it in a plot with the lm() as a line
plot( as.vector(assay6410_rmse_similarity_bliss_matrices) ~ as.vector(assay6410_rmse_similarity_response_matrices), 
      pch=16, cex=0.8, 
      main="Comparing Using RMSE",
      xlab= "RMSE Similairty of Reponse Matrices", 
      ylab="RMSE Similarity of Delta Bliss Matrices")
# making the linear model of the similarity matrices and the bliss matrices
assay6410_rmse_lm <- lm(as.vector(assay6410_rmse_similarity_bliss_matrices) ~ as.vector(assay6410_rmse_similarity_response_matrices))
# adding it to the graphic
assay6410_rmse_lm %>% abline(., col="red")

# Using Euclidean Distance
# visualizing it in a plot with the lm() as a line
plot( as.vector(assay6410_euclidean_similarity_bliss_matrices) ~ as.vector(assay6410_euclidean_similarity_response_matrices), 
      pch=16, cex=0.8, 
      main="Comparing Using Euclidean",
      xlab= "Euclidean Similairty of Reponse Matrices", 
      ylab="Euclidean Similarity of Delta Bliss Matrices")
# making the linear model
assay6410_euclidean_lm <- lm(as.vector(assay6410_euclidean_similarity_bliss_matrices) ~ as.vector(assay6410_euclidean_similarity_response_matrices))
assay6410_euclidean_lm %>% abline(., col="blue")

# Using KS
# visualizing it in a plot with the lm() as a line
plot( as.vector(assay6410_ks_similarity_bliss_matrices) ~ as.vector(assay6410_ks_similarity_response_matrices), 
      pch=16, cex=0.8, 
      main="Comparing Using KS Test",
      xlab= "KS Similarity of Reponse Matrices", 
      ylab="KS Similarity of Delta Bliss Matrices")
# making the linear model
assay6410_ks_lm <- lm(as.vector(assay6410_ks_similarity_bliss_matrices) ~ as.vector(assay6410_ks_similarity_response_matrices))
assay6410_ks_lm %>% abline(., col="green")

# Using Syrjala
# visualizing it in a plot with the lm() as a line
plot( as.vector(assay6410_syrjala_similarity_bliss_matrices) ~ as.vector(assay6410_syrjala_similarity_response_matrices), 
      pch=16, cex=0.8, 
      main="Comparing Using Syrjala",
      xlab= "Syrjala Similarity of Reponse Matrices", 
      ylab="Syrjala Similarity of Delta Bliss Matrices")
# making the linear model
assay6410_syrjala_lm <- lm(as.vector(assay6410_syrjala_similarity_bliss_matrices) ~ as.vector(assay6410_syrjala_similarity_response_matrices))
assay6410_syrjala_lm %>% abline(., col="magenta")

dev.off()

# thoughts: the row at the bottom is where there was a significant difference between two response matrices, but there was not a significant distance between the delta bliss matrices











# find the r squared value for the lm()
assay6410_rmse_lm_summary <- summary(assay6410_rmse_lm)
assay6410_rmse_lm_summary$r.squared

# find the r squared value for the lm()
assay6410_euclidean_lm_summary <- summary(assay6410_euclidean_lm)
assay6410_euclidean_lm_summary$r.squared

# find the r squared value for the lm()
assay6410_ks_lm_summary <- summary(assay6410_ks_lm)
assay6410_ks_lm_summary$r.squared

# find the r squared value for the lm()
assay6410_syrjala_lm_summary <- summary(assay6410_syrjala_lm)
assay6410_syrjala_lm_summary$r.squared



# agent distances 
assay6410_agents <- agent_distance_list(assay6410_sub2)

