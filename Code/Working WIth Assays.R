---
title: "RMSE Functions"
author: "Sarita Lee"
date: "June 22, 2017"
output: html_document
---
  
  ```{r}
# 1. run this 
.libPaths(c('/ncats/apps/hpc/R-locallib'))

library(ncgcdb)
set.db.credentials(list(user='r_admin', password='r0leadm1n'))

# 2. restart R
# 3. library(ncgcmatrix)
# 4. then can work with the package and matrix data
```



# Extra Code fom Lu
```{r}
library(mgcv)
attach(eg)
eg <- gamSim(2,n=500,scale=.1)

op <- par(mfrow=c(2,2),mar=c(4,4,1,1))

contour(truth$x,truth$z,truth$f) ## contour truth
b4 <- gam(y~s(x,z),data=data) ## fit model
fit1 <- matrix(predict.gam(b4,pr,se=FALSE),40,40)
contour(truth$x,truth$z,fit1)   ## contour fit
persp(truth$x,truth$z,truth$f)    ## persp truth
vis.gam(b4)                     ## persp fit
```




# Working with Assays 1764, 1763, and 1761
## Updated 7/24/2017
```{r}
assay1764 <- get.data.blocks(1764)
assay1763 <- get.data.blocks(1763)
assay1761 <- get.data.blocks(1761)
```


## Forming the subsets and delta bliss list
## Updated 7/26/2017
```{r}
# "good" matrices from all three malaria assays individually
assay1764_good <- good_indices(assay1764)
assay1763_good <- good_indices(assay1763)
assay1761_good <- good_indices(assay1761)

# figuring out which blocks are "good" across all of the lines
malaria_good <- assay1764_good == TRUE & assay1763_good == TRUE & assay1761_good == TRUE 

# making the subset of which blocks are good across all three of the assays
assay1764_sub <- good_subset(assay1764, malaria_good)
assay1763_sub <- good_subset(assay1763, malaria_good)
assay1761_sub <- good_subset(assay1761, malaria_good)

## delta bliss
assay1764_bliss_all <- list_delta_bliss(assay1764)
assay1763_bliss_all <- list_delta_bliss(assay1763)
assay1761_bliss_all <- list_delta_bliss(assay1761)

# subsets of delta bliss matrices- only the blocks that are good across all three assays
assay1764_bliss_sub <- good_subset(assay1764_bliss_all, malaria_good, bliss=TRUE) # was previously wrong- fixed 7/26/17
assay1763_bliss_sub <- good_subset(assay1763_bliss_all, malaria_good, bliss=TRUE) # was previously wrong- fixed 7/26/17
assay1761_bliss_sub <- good_subset(assay1761_bliss_all, malaria_good, bliss=TRUE) # was previously wrong- fixed 7/26/17
```


## Comparing Simlarity of Response Matrices- RMSE
## Updated 7/24/2017
```{r}
assay1764_RMSE_response_matrices <- RMSE_list(assay1764_sub)
assay1763_RMSE_response_matrices <- RMSE_list(assay1763_sub)
assay1761_RMSE_response_matrices <- RMSE_list(assay1761_sub)
```

## Comparing Simlarity of Delta Bliss Matrices- RMSE 
## Updated 7/26/2017
```{r}
assay1764_RMSE_bliss_matrices <- RMSE_list(assay1764_bliss_sub)
assay1763_RMSE_bliss_matrices <- RMSE_list(assay1763_bliss_sub)
assay1761_RMSE_bliss_matrices <- RMSE_list(assay1761_bliss_sub)
```


## Updated 7/26/2017
```{r}
#assay1764_syrjala_response_matrices %>% heatmap(.)
#assay1763_syrjala_response_matrices %>% heatmap(.)
#assay1761_syrjala_response_matrices %>% heatmap(.)

pdf("MalariaAssayComparingResponseMatricesRMSE_sub.pdf", width=13.5, height = 9)
par(mfrow=c(2,3))
# Comparing Response Matrices
assay1764_RMSE_response_matrices %>% 
  image(main="Assay 1764 Subset Similarity Response Matrices- RMSE")
assay1763_RMSE_response_matrices %>% 
  image(main="Assay 1763 Subset Similarity Response Matrices- RMSE")
assay1761_RMSE_response_matrices %>% 
  image(main="Assay 1761 Subset Similarity Response Matrices- RMSE")
# Comparing Synergy (Bliss) Matrices
assay1764_RMSE_bliss_matrices %>% 
  image(main="Assay 1764 Subset Similarity Delta Bliss Matrices- RMSE")
assay1763_RMSE_bliss_matrices %>% 
  image(main="Assay 1763 Subset Similarity Delta Bliss Matrices- RMSE")
assay1761_RMSE_bliss_matrices %>% 
  image(main="Assay 1761 Subset Similarity Delta Bliss Matrices- RMSE")
dev.off()
```


## Syrjala Distances of Response Matrices (of the subset)
## Updated 7/26/2017
```{r}
assay1764_syrjala_response_matrices <- dist.syrjala(assay1764_sub)
assay1763_syrjala_response_matrices <- dist.syrjala(assay1763_sub)
assay1761_syrjala_response_matrices <- dist.syrjala(assay1761_sub)

```

## Syrjala Distances of Delta Bliss Matrices (of the subset)
## Updated 7/26/2017
```{r}
## Comparing Simlarity of Delta Bliss Matrices- Syrjala

assay1764_syrjala_bliss_matrices <- dist.syrjala(assay1764_bliss_sub, model="bliss")
assay1763_syrjala_bliss_matrices <- dist.syrjala(assay1763_bliss_sub, model="bliss") # was previously incorrect
assay1761_syrjala_bliss_matrices <- dist.syrjala(assay1761_bliss_sub, model="bliss") # was previously incorrect

# Comparing Synergy (Bliss) Matrices
#assay1764_syrjala_bliss_matrices %>% 
#  image(., main="Assay 1764 Comparing Similarity Bliss Matrices")
#assay1763_syrjala_bliss_matrices %>% 
#  image(., main="Assay 1764 Comparing Similarity Bliss Matrices")
#assay1761_syrjala_bliss_matrices %>% 
#  image(., main="Assay 1764 Comparing Similarity Bliss Matrices")

#assay1764_syrjala_response_matrices %>% heatmap(.)
#assay1763_syrjala_response_matrices %>% heatmap(.)
#assay1761_syrjala_response_matrices %>% heatmap(.)
```

## Heatmaps of Syrjala Distance between Response Matrices and Delta Bliss Matrices (of the subsets)
## Updated 7/26/2017
```{r}
pdf("MalariaAssayComparingResponseMatricesSyrjala_sub.pdf", width=13.5, height = 9)
par(mfrow=c(2,3))
# Comparing Response Matrices
assay1764_syrjala_response_matrices %>% 
  image(main="Assay 1764 Subset Similarity Response Matrices- Syrjala")
assay1763_syrjala_response_matrices %>% 
  image(main="Assay 1763 Subset Similarity Response Matrices- Syrjala")
assay1761_syrjala_response_matrices %>% 
  image(main="Assay 1761 Subset Similarity Response Matrices- Syrjala")
# Comparing Synergy (Bliss) Matrices
assay1764_syrjala_bliss_matrices %>% 
  image(main="Assay 1764 Subset Similarity Delta Bliss Matrices- Syrjala")
assay1763_syrjala_bliss_matrices %>% 
  image(main="Assay 1763 Subset Similarity Delta Bliss Matrices- Syrjala")
assay1761_syrjala_bliss_matrices %>% 
  image(main="Assay 1761 Subset Similarity Delta Bliss Matrices- Syrjala")
dev.off()
```

## Correlation between Syrjala Distance of Response Matrices and Syrjala Distance of Delta Bliss matrices (of the subset)
## Updated 7/26/2017
```{r}
pdf("MalariaAssayCorrelationResponseAndBliss_sub.pdf", width=14, height=5)
par(mfrow=c(1,3))
plot( as.vector(assay1764_syrjala_bliss_matrices) ~ as.vector(assay1764_syrjala_response_matrices), 
      pch=16, cex=0.05,
      main="Assay 1764 Subset Response and Bliss",
      xlab="Syrjala Test- Reponse Matrices", ylab="Syrjala Test- Delta Bliss Matrices" )
# make a linear model
assay1764_syrjala_lm <- lm(as.vector(assay1764_syrjala_bliss_matrices) ~ as.vector(assay1764_syrjala_response_matrices))
# adding it to the graphic
assay1764_syrjala_lm %>% abline(., col="red")

plot( as.vector(assay1763_syrjala_bliss_matrices) ~ as.vector(assay1763_syrjala_response_matrices), 
      pc=16, cex=0.05,
      main="Assay 1763 Subset Response and Bliss",
      xlab="Syrjala Test- Reponse Matrices", ylab="Syrjala Test- Delta Bliss Matrices" )
# make a linear model
assay1763_syrjala_lm <- lm(as.vector(assay1763_syrjala_bliss_matrices) ~ as.vector(assay1763_syrjala_response_matrices))
# adding it to the graphic
assay1763_syrjala_lm %>% abline(., col="red")

plot( as.vector(assay1761_syrjala_bliss_matrices) ~ as.vector(assay1761_syrjala_response_matrices), 
      pch=16, cex=0.05,
      main="Assay 1761 Subset Response and Bliss",
      xlab="Syrjala Test- Reponse Matrices", ylab="Syrjala Test- Delta Bliss Matrices" )
# make a linear model
assay1761_syrjala_lm <- lm(as.vector(assay1761_syrjala_bliss_matrices) ~ as.vector(assay1761_syrjala_response_matrices))
# adding it to the graphic
assay1761_syrjala_lm %>% abline(., col="red")
dev.off()
```


## Looking at a single scatterplot in more detail
```{r}
pdf("MalariaAssay1764Large_sub.pdf", width=8, height=8)
plot( as.vector(assay1764_syrjala_bliss_matrices) ~ as.vector(assay1764_syrjala_response_matrices), 
      pch=16, cex=0.15,
      main="Assay 1764 Response and Bliss",
      xlab="Syrjala Test- Reponse Matrices", ylab="Syrjala Test- Delta Bliss Matrices" )
dev.off()


pdf("MalariaAssay1761Large_sub.pdf", width=8, height=8)
plot( as.vector(assay1761_syrjala_bliss_matrices) ~ as.vector(assay1761_syrjala_response_matrices), 
      pch=16, cex=0.1,
      main="Assay 1761 Response and Bliss",
      xlab="Syrjala Test- Reponse Matrices", ylab="Syrjala Test- Delta Bliss Matrices" )
dev.off()
```



## using ks.test to compare the three assays
```{r}
# make a list of the subset syrjala response assays (458 x 558)
malaria_response_dissim <- list(assay1764_syrjala_response_matrices, assay1763_syrjala_response_matrices, assay1761_syrjala_response_matrices)


malaria_ks_syrjala <- ks_list(malaria_response_dissim)



image(x=0:3, y=0:3,
      z=malaria_ks_syrjala,
      main="Dissimilarity of Dissimilarity Using Syrjala",
      xlab="Malaria Assays 1764, 1763, 1761",
      ylab="Malaria Assays 1764, 1763, 1761")
```





# Agent Distances (needs to be fixed)

assay1764_query <- get_unique_agent_query(assay1764_sub)
assay1763_query <- get_unique_agent_query(assay1763_sub)
assay1761_query <- get_unique_agent_query(assay1761_sub)


assay1764_agent_distance <- agent_distance_list(assay1764_sub, assay1764_query)
assay1763_agent_distance <- agent_distance_list(assay1763_sub, assay1763_query)
assay1761_agent_distance <- agent_distance_list(assay1761_sub, assay1761_query)




# Heatmap of Comparing Agent Distances

pdf("MalariaAssayAgentDistance.pdf", width=13.5, height=4.5)
par(mfrow=c(1,3))
assay1764_agent_distance %>% image(main="Assay 1764 Agent Similarity")
assay1763_agent_distance %>% image(main="Assay 1763 Agent Similarity")
assay1761_agent_distance %>% image(main="Assay 1761 Agent Similarity")
dev.off()


# Correlation between Agent Distances and Response Dissimilarities

pdf("MalariaAssayCorrelationAgentAndResponse_sub.pdf", width=14, height=5)
par(mfrow=c(1,3))
plot( as.vector(assay1764_syrjala_response_matrices) ~ as.vector(assay1764_agent_distance), 
      pch=16, cex=0.05,
      main="Assay 1764 Subset Agent Dissimilarity and Response Dissimilarity",
      xlab="Distance of Agents", ylab="Syrjala Test- Response Matrices" )
# make a linear model
assay1764_agent_lm2 <- lm(as.vector(assay1764_syrjala_response_matrices) ~ as.vector(assay1764_agent_distance))
# adding it to the graphic
assay1764_agent_lm2 %>% abline(., col="red")

plot( as.vector(assay1763_syrjala_response_matrices) ~ as.vector(assay1763_agent_distance), 
      pch=16, cex=0.05,
      main="Assay 1763 Subset Agent Dissimilarity and Response Dissimilarity",
      xlab="Distance of Agents", ylab="Syrjala Test- Response Matrices" )
# make a linear model
assay1763_agent_lm2 <- lm(as.vector(assay1763_syrjala_response_matrices) ~ as.vector(assay1763_agent_distance))
# adding it to the graphic
assay1763_agent_lm2 %>% abline(., col="red")

plot( as.vector(assay1761_syrjala_response_matrices) ~ as.vector(assay1761_agent_distance), 
      pch=16, cex=0.05,
      main="Assay 1761 Subset Agent Dissimilarity and Response Dissimilarity",
      xlab="Distance of Agents", ylab="Syrjala Test- Response Matrices" )
# make a linear model
assay1761_agent_lm2 <- lm(as.vector(assay1761_syrjala_response_matrices) ~ as.vector(assay1761_agent_distance))
# adding it to the graphic
assay1761_agent_lm2 %>% abline(., col="red")
dev.off()




# Correlation between Agent Distances and Synergy Dstances

pdf("MalariaAssayCorrelationAgentAndBliss_sub.pdf", width=14, height=5)
par(mfrow=c(1,3))
plot( as.vector(assay1764_syrjala_bliss_matrices) ~ as.vector(assay1764_agent_distance), 
      pch=16, cex=0.05,
      main="Assay 1764 Subset Agent Difference and Bliss",
      xlab="Distance of Agents", ylab="Syrjala Test- Delta Bliss Matrices" )
# make a linear model
assay1764_agent_lm <- lm(as.vector(assay1764_syrjala_bliss_matrices) ~ as.vector(assay1764_agent_distance))
# adding it to the graphic
assay1764_agent_lm %>% abline(., col="red")

plot( as.vector(assay1763_syrjala_bliss_matrices) ~ as.vector(assay1763_agent_distance), 
      pch=16, cex=0.05,
      main="Assay 1763 Subset Single Agent and Bliss",
      xlab="Distance of Agents", ylab="Syrjala Test- Delta Bliss Matrices" )
# make a linear model
assay1763_agent_lm <- lm(as.vector(assay1763_syrjala_bliss_matrices) ~ as.vector(assay1763_agent_distance))
# adding it to the graphic
assay1763_agent_lm %>% abline(., col="red")

plot( as.vector(assay1761_syrjala_bliss_matrices) ~ as.vector(assay1761_agent_distance), 
      pch=16, cex=0.05,
      main="Assay 1761 Subset Single Agent and Bliss",
      xlab="Distance of Agents", ylab="Syrjala Test- Delta Bliss Matrices" )
# make a linear model
assay1761_agent_lm <- lm(as.vector(assay1761_syrjala_bliss_matrices) ~ as.vector(assay1761_agent_distance))
# adding it to the graphic
assay1761_agent_lm %>% abline(., col="red")
dev.off()







# Discordant Pairs
```{r}
assay6410_rmse_discoordinant <- discoordinant_pairs_for(assay6410_rmse_similarity_response_matrices, assay6410_rmse_similarity_bliss_matrices)

assay6410_euclidean_discoordinant <- discoordinant_pairs_for(assay6410_euclidean_similarity_response_matrices, assay6410_euclidean_similarity_bliss_matrices)

assay6410_ks_discoordinant <- discoordinant_pairs_for(assay6410_ks_similarity_response_matrices, assay6410_ks_similarity_bliss_matrices)

assay6410_syrjala_discoordinant <- discoordinant_pairs_for(assay6410_syrjala_similarity_response_matrices, assay6410_syrjala_similarity_bliss_matrices)
```

```{r}
pdf("Assay6410DiscoordinantPairs_b.pdf", width=8, height=8)

par(mfrow=c(2,2))
#Assay 6410 Discoordinant Pairs from RMSE 
assay6410_rmse_discoordinant %>% 
  image(main="Assay 6410 Discoordinant Pairs from RMSE",
        x=1:24, y=1:24, 
        xlab="Block Number", ylab="Block Number")

#Assay 6410 Discoordinant Pairs from Euclidean
assay6410_euclidean_discoordinant %>% 
  image(main="Assay 6410 Discoordinant Pairs from Euclidean",
        x=1:24, y=1:24, 
        xlab="Block Number", ylab="Block Number")

#Assay 6410 Discoordinant Pairs from KS 
assay6410_ks_discoordinant %>% 
  image(main="Assay 6410 Discoordinant Pairs from KS",
        x=1:24, y=1:24, 
        xlab="Block Number", ylab="Block Number")

#Assay 6410 Discoordinant Pairs from Syrjala 
assay6410_syrjala_discoordinant %>% 
  image(main="Assay 6410 Discoordinant Pairs from Syrjala",
        x=1:24, y=1:24, 
        xlab="Block Number", ylab="Block Number")

dev.off()

```





# Dissimilarity of dose response
assay1764[[1]] %>% as.matrix()
assay1764_sub[[1]] %>% as.matrix()

assay1764_sub_last <- good_subset(assay1764, malaria_good, keep_last = TRUE)
#assay1764_sub_last[[1]] %>% as.matrix()

# comparing doses without jitter
assay1764_dose <- dose_list(assay1764_sub_last)

# comparing doses with default jitter
assay1764_dose_jitter <- dose_list_jitter(assay1764_sub_last)





## Figuring out Correlation between structural similarity (agent) vs dose response similarity
assay1764_dose_vector <- dose_vector(assay1764_sub_last)
assay1764_dose_vector_inverse <- 1 - assay1764_dose_vector

assay1764_agent_vector <- agent_vector(assay1764_sub, assay1764_query)
assay1764_agent_vector_inverse <- 1 - assay1764_agent_vector






`






# Malaria Assays Discoordinant Pairs 
```{r}
assay1764_syrjala_discoordinant <- discoordinant_pairs_for(assay1764_syrjala_response_matrices, assay1764_syrjala_bliss_matrices)

assay1763_syrjala_discoordinant <- discoordinant_pairs_for(assay1763_syrjala_response_matrices, assay1763_syrjala_bliss_matrices)

assay1761_syrjala_discoordinant <- discoordinant_pairs_for(assay1761_syrjala_response_matrices, assay1761_syrjala_bliss_matrices)
```

```{r}
pdf("MalariaAssayDiscoordinantPairs.pdf", width=13.5, height=4.5)

par(mfrow=c(1,3))
# Assay 1764 Discoordinant Pairs from Syrjala 
assay1764_syrjala_discoordinant %>% 
  image(main="Assay 1764 Discoordinant Pairs from Syrjala",
        x=1:nrow(assay1764_syrjala_response_matrices), y=1:nrow(assay1764_syrjala_response_matrices), 
        xlab="Block Number", ylab="Block Number")

# Assay 1763 Discoordinant Pairs from Syrjala
assay1763_syrjala_discoordinant %>% 
  image(main="Assay 1763 Discoordinant Pairs from Syrjala",
        x=1:nrow(assay1763_syrjala_response_matrices), y=1:nrow(assay1763_syrjala_response_matrices), 
        xlab="Block Number", ylab="Block Number")

# Assay 1761 Discoordinant Pairs from Syrjala 
assay1761_syrjala_discoordinant %>% 
  image(main="Assay 1761 Discoordinant Pairs from Syrjala",
        x=1:nrow(assay1761_syrjala_response_matrices), y=1:nrow(assay1761_syrjala_response_matrices), 
        xlab="Block Number", ylab="Block Number")
dev.off()
```






