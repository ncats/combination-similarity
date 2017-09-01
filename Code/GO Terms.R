# use the get_unique_agent_query() function from previous Structure Functions work 

# all of the steps
# function that takes in a list of blocks and returns the 
# gene id's for the blocks in the specific cluster (NA's removed)
cluster_gene_ids <- function(list_blocks, groups, clust_number){
  # 1. subset the list of assay blocks to only those in the nth cluster
  list_cluster <- list_blocks[groups==clust_number]
  # 2. query data of the chemicals in the nth cluster
  cluster_query <- get_unique_agent_query(list_cluster)
  # 3. getting the gene_id's associated with the 
  cluster_go_data <- merge(x= cluster_query, y=sample.info, by="SAMPLE_ID", all.x = TRUE)
  cluster_vector_narm <- as.numeric(as.vector(cluster_go_data$GENE_ID
                                              [!is.na(cluster_go_data$GENE_ID)]))
  return(cluster_vector_narm)
}


# function that removes the na's from any vector
background_gene_ids <- function(assay_query){
  assay_go_data <- merge(x= assay_query, y=sample.info, by="SAMPLE_ID", all.x = TRUE)
  assay_vector_narm <- as.numeric(as.vector(assay_go_data$GENE_ID
                                              [!is.na(assay_go_data$GENE_ID)]))
  return(assay_vector_narm)
}



assay1764_background <- background_gene_ids(assay1764_query)


assay6503_query <- get_unique_agent_query(assay6503_sub)
assay6503_background <- background_gene_ids(assay6503_query)

assay6503_clust1 <- cluster_gene_ids(assay6503_sub, assay6503_5groups, 1)
assay6503_clust2 <- cluster_gene_ids(assay6503_sub, assay6503_5groups, 2)
assay6503_clust3 <- cluster_gene_ids(assay6503_sub, assay6503_5groups, 3)
assay6503_clust4 <- cluster_gene_ids(assay6503_sub, assay6503_5groups, 4)
assay6503_clust5 <- cluster_gene_ids(assay6503_sub, assay6503_5groups, 5)

assay6503_clust4_info <- assay6503_sub[assay6503_5groups==4]
assay6503_clust4_query <- get_unique_agent_query(assay6503_clust4_info)
assay6503_clust4_sampleids <- assay6503_clust4_query$SAMPLE_ID
assay6503_clust4_samplenames <- assay6503_clust4_query$SAMPLE_NAME


assay6503_clust5_info <- assay6503_sub[assay6503_5groups==5]
assay6503_clust5_query <- get_unique_agent_query(assay6503_clust5_info)
assay6503_clust5_sampleids <- assay6503_clust5_query$SAMPLE_ID
assay6503_clust5_samplenames <- assay6503_clust5_query$SAMPLE_NAME
assay6503_clust5_genesymbols <- assay6503_clust5_query$GENE_SYMBOL


assay7021_query <- get_unique_agent_query(assay7021_sub)
assay7021_background <- background_gene_ids(assay7021_query)

assay7021_clust1 <- cluster_gene_ids(assay7021_sub, assay7021_4groups, 1)
assay7021_clust2 <- cluster_gene_ids(assay7021_sub, assay7021_4groups, 2)
assay7021_clust3 <- cluster_gene_ids(assay7021_sub, assay7021_4groups, 3)
assay7021_clust4 <- cluster_gene_ids(assay7021_sub, assay7021_4groups, 4)


# use a left merge to get the sample.info data added to the query data 
assay1764_background_go_data <- merge(x= assay1764_query, y= sample.info, by="SAMPLE_ID", all.x = TRUE)
assay1764_background_go_data2 <- assay1764_go_data[,c(1,14)]
assay1764_background_vector <- as.vector(assay1764_background_go_data$GENE_ID)

# saves as vector, removes the NA's, 
assay1764_background_vector_narm <- as.numeric(as.vector(
  assay1764_background_go_data$GENE_ID[!is.na(assay1764_background_go_data$GENE_ID)]))

# 1. subset the list of assay blocks to only those in the first cluster
assay1764_cluster1 <- assay1764_sub[assay1764_groups4==1]
# 2. query data of the chemicals in the specific cluster
assay1764_cluster1_query <- get_unique_agent_query(assay1764_cluster1)
# 3. getting the gene_id's
assay1764_cluster1_go_data <- merge(x= assay1764_cluster1_query, y= sample.info, by="SAMPLE_ID", all.x = TRUE)
assay1764_cluster1_vector <- as.vector(assay1764_cluster1_go_data$GENE_ID)
assay1764_cluster1_vector_narm <- as.numeric(as.vector(
  assay1764_cluster1_go_data$GENE_ID[!is.na(assay1764_cluster1_go_data$GENE_ID)]))


# 1. subset the list of assay blocks to only those in the first cluster
assay1764_cluster2 <- assay1764_sub[assay1764_groups4==2]
# 2. query data of the chemicals in the specific cluster
assay1764_cluster2_query <- get_unique_agent_query(assay1764_cluster2)
# 3. getting the gene_id's
assay1764_cluster2_go_data <- merge(x= assay1764_cluster2_query, y= sample.info, by="SAMPLE_ID", all.x = TRUE)
assay1764_cluster2_vector <- as.vector(assay1764_cluster2_go_data$GENE_ID)
assay1764_cluster2_vector_narm <- as.numeric(as.vector(
  assay1764_cluster2_go_data$GENE_ID[!is.na(assay1764_cluster2_go_data$GENE_ID)]))


# 1. subset the list of assay blocks to only those in the first cluster
assay1764_cluster3 <- assay1764_sub[assay1764_groups4==3]
# 2. query data of the chemicals in the specific cluster
assay1764_cluster3_query <- get_unique_agent_query(assay1764_cluster3)
# 3. getting the gene_id's
assay1764_cluster3_go_data <- merge(x= assay1764_cluster3_query, y= sample.info, by="SAMPLE_ID", all.x = TRUE)
assay1764_cluster3_vector <- as.vector(assay1764_cluster3_go_data$GENE_ID)
assay1764_cluster3_vector_narm <- as.numeric(as.vector(
  assay1764_cluster3_go_data$GENE_ID[!is.na(assay1764_cluster3_go_data$GENE_ID)]))

assay1764_clust4 <- cluster_gene_ids(assay1764_sub, assay1764_groups4, 4)


library("ALL") #error
library("hgu95av2.db") #error
library("GO.db")
library("annotate")
library("genefilter")
library("GOstats")
library("RColorBrewer")
library("xtable")
library("Rgraphviz") #error


# function that returns the dataframe with the info from the test in it
gostats_test <- function(cluster_ids, background_ids, p_value=0.05){
  params <- new("GOHyperGParams",
                    geneIds=cluster_ids,
                    universeGeneIds=background_ids,
                    annotation="org.Hs.eg.db",
                    ontology="MF",
                    pvalueCutoff=p_value,
                    testDirection="over")
  hgtest <- hyperGTest(params)
  hgtest_sum <- as.data.frame(summary(hgtest))
  return (hgtest_sum)
  }


params1764 <- new("GOHyperGParams",
                  geneIds=assay1764_cluster1_vector_narm,
                  universeGeneIds=assay1764_background_vector_narm,
                  annotation="org.Hs.eg.db",
                  ontology="MF",
                  pvalueCutoff=0.05,
                  testDirection="over")
hg1764 <- hyperGTest(params1764)
hg1764


params1764_2 <- new("GOHyperGParams",
                  geneIds=assay1764_cluster2_vector_narm,
                  universeGeneIds=assay1764_background_vector_narm,
                  annotation="org.Hs.eg.db",
                  ontology="MF",
                  pvalueCutoff=0.05,
                  testDirection="over")
hg1764_2 <- hyperGTest(params1764_2)
hg1764_2

params1764_3 <- new("GOHyperGParams",
                    geneIds=assay1764_cluster3_vector_narm,
                    universeGeneIds=assay1764_background_vector_narm,
                    annotation="org.Hs.eg.db",
                    ontology="MF",
                    pvalueCutoff=0.05,
                    testDirection="over")
hg1764_3 <- hyperGTest(params1764_3)
hg1764_3

params1764_4 <- new("GOHyperGParams",
                    geneIds=assay1764_clust4,
                    universeGeneIds=assay1764_background_vector_narm,
                    annotation="org.Hs.eg.db",
                    ontology="MF",
                    pvalueCutoff=0.05,
                    testDirection="over")
hg1764_4 <- hyperGTest(params1764_4)
hg1764_4_sum <- hg1764_4 %>% summary(.) %>% as.data.frame(.)


params6503_1 <- new("GOHyperGParams",
                    geneIds=assay6503_clust1,
                    universeGeneIds=assay6503_background,
                    annotation="org.Hs.eg.db",
                    ontology="MF",
                    pvalueCutoff=0.05,
                    testDirection="over")
hg6503_1 <- hyperGTest(params6503_1)
hg6503_1
hg6503_1_sum <- summary(hg6503_1) %>% as.data.frame(.)
hg6503_1_sub <- hg6503_1_sum[c("GOMFID","Pvalue","Term")]
hg6503_1_sub


params6503_2 <- new("GOHyperGParams",
                    geneIds=assay6503_clust2,
                    universeGeneIds=assay6503_background,
                    annotation="org.Hs.eg.db",
                    ontology="MF",
                    pvalueCutoff=0.05,
                    testDirection="over")
hg6503_2 <- hyperGTest(params6503_2)
hg6503_2
summary(hg6503_2)
hg6503_2_sum <- summary(hg6503_2) %>% as.data.frame(.)
hg6503_2_sub <- hg6503_2_sum[c("GOMFID","Pvalue","Term")]
hg6503_2_sub



params6503_3 <- new("GOHyperGParams",
                    geneIds=assay6503_clust3,
                    universeGeneIds=assay6503_background,
                    annotation="org.Hs.eg.db",
                    ontology="MF",
                    pvalueCutoff=0.05,
                    testDirection="over")
hg6503_3 <- hyperGTest(params6503_3)
hg6503_3
summary(hg6503_3)
hg6503_3_sum <- summary(hg6503_3) %>% as.data.frame(.)
hg6503_3_sub <- hg6503_3_sum[c("GOMFID","Pvalue","Term")]
hg6503_3_sub



params6503_4 <- new("GOHyperGParams",
                    geneIds=assay6503_clust4,
                    universeGeneIds=assay6503_background,
                    annotation="org.Hs.eg.db",
                    ontology="MF",
                    pvalueCutoff=0.05,
                    testDirection="over")
hg6503_4 <- hyperGTest(params6503_4)
hg6503_4
summary(hg6503_4)
hg6503_4_sum <- summary(hg6503_4) %>% as.data.frame(.)
hg6503_4_sub <- hg6503_4_sum[c("GOMFID","Pvalue","Term")]
hg6503_4_sub

params6503_5 <- new("GOHyperGParams",
                    geneIds=assay6503_clust5,
                    universeGeneIds=assay6503_background,
                    annotation="org.Hs.eg.db",
                    ontology="MF",
                    pvalueCutoff=0.05,
                    testDirection="over")
hg6503_5 <- hyperGTest(params6503_5)
hg6503_5
summary(hg6503_5)
hg6503_5_sum <- summary(hg6503_5) %>% as.data.frame(.)
hg6503_5_sub <- hg6503_5_sum[c("GOMFID","Pvalue","Term")]
hg6503_5_sub



hg7021_1_sum <- gostats_test(assay7021_clust1, assay7021_background)
hg7021_2_sum <- gostats_test(assay7021_clust2, assay7021_background)
hg7021_3_sum <- gostats_test(assay7021_clust3, assay7021_background)
hg7021_4_sum <- gostats_test(assay7021_clust4, assay7021_background)

hg7021_1_sum
hg7021_2_sum
hg7021_3_sum
hg7021_4_sum


hg1764_1_sumb <- gostats_test(assay1764_cluster1_vector_narm, assay1764_background_vector_narm)
hg1764_2_sumb <- gostats_test(assay1764_cluster2_vector_narm, assay1764_background_vector_narm)
hg1764_3_sumb <- gostats_test(assay1764_cluster3_vector_narm, assay1764_background_vector_narm)
hg1764_4_sumb <- gostats_test(assay1764_clust4, assay1764_background_vector_narm)


hg7021_1_sub <- hg7021_1_sum[c("GOMFID","Pvalue","Term")]
hg7021_2_sub <- hg7021_2_sum[c("GOMFID","Pvalue","Term")]
hg7021_3_sub <- hg7021_3_sum[c("GOMFID","Pvalue","Term")]
hg7021_4_sub <- hg7021_4_sum[c("GOMFID","Pvalue","Term")]

hg7021_1_sub[1:5,]
hg7021_2_sub[1:5,]
hg7021_3_sub[1:5,]
hg7021_4_sub[1:5,]


# function that returns a list with all of the blocks separated into their clusters
list_clusters <- function(list_blocks, syrjala_response_matrices, number_clusters){
  
  syrjala_response_d <- as.dist(syrjala_response_matrices)
  syrjala_clust <- hclust(syrjala_response_d)
  clustering_numbers <- cutree(syrjala_clust, k=number_clusters)
  
  list_all_clusters <- lapply(unique(clustering_numbers), 
                          function(x) list_blocks[ which(clustering_numbers == x) ])
  return (list_all_clusters)
}


# function that performs the hyper geom test for all of the clusters
hyperGTest_list_seperate <- function(list_blocks, list_all_clusters){
  
  # initiate an empty list where all of the results from the hyper g test will go 
  hyper_results <- list()
  
  # getthe background for the whole assay
  query <- get_unique_agent_query(list_blocks)
  background <- background_gene_ids(query)

  for (i in 1:length(list_all_clusters)){
    
    cluster_query <- get_unique_agent_query(list_all_clusters[[i]])
    # getting the gene_id's associated with the 
    cluster_go_data <- merge(x= cluster_query, y=sample.info, by="SAMPLE_ID", all.x = TRUE)
    cluster_vector_narm <- as.numeric(as.vector(cluster_go_data$GENE_ID
                                                [!is.na(cluster_go_data$GENE_ID)]))
    
    # setting up params for hyper g test
    params <- new("GOHyperGParams",
                        geneIds=cluster_vector_narm,
                        universeGeneIds=background,
                        annotation="org.Hs.eg.db",
                        ontology="MF",
                        pvalueCutoff=0.05,
                        testDirection="over")
    
    # getting results of the hyper g test
    hg <- hyperGTest(params)
    hg_sum <- as.data.frame(summary(hg))
    hg_sub <- hg_sum[c("GOMFID","Pvalue","Term")]
    
    # save results to the list of results 
    hyper_results[[i]] <- hg_sub[1:5,] # hg7021_1_sub[1:5,]

  }
  return(hyper_results)
}


# combine both into one
hyperGTest_list <- function(list_blocks, syrjala_response_matrices, number_clusters){
  
  # seperates all the blocks into their clusters
  syrjala_response_d <- as.dist(syrjala_response_matrices)
  syrjala_clust <- hclust(syrjala_response_d)
  clustering_numbers <- cutree(syrjala_clust, k=number_clusters)
  
  list_all_clusters <- lapply(unique(clustering_numbers), 
                              function(x) list_blocks[ which(clustering_numbers == x) ])
  
  # initiate an empty list where all of the results from the hyper g test will go 
  hyper_results <- list()
  
  # getthe background for the whole assay
  query <- get_unique_agent_query(list_blocks)
  background <- background_gene_ids(query)
  
  for (i in 1:number_clusters){
    
    cluster_query <- get_unique_agent_query(list_all_clusters[[i]])
    # getting the gene_id's associated with the 
    cluster_go_data <- merge(x= cluster_query, y=sample.info, by="SAMPLE_ID", all.x = TRUE)
    cluster_vector_narm <- as.numeric(as.vector(cluster_go_data$GENE_ID
                                                [!is.na(cluster_go_data$GENE_ID)]))
    
    # setting up params for hyper g test
    params <- new("GOHyperGParams",
                  geneIds=cluster_vector_narm,
                  universeGeneIds=background,
                  annotation="org.Hs.eg.db",
                  ontology="MF",
                  pvalueCutoff=0.05,
                  testDirection="over")
    
    # getting results of the hyper g test
    hg <- hyperGTest(params)
    hg_sum <- as.data.frame(summary(hg))
    hg_sub <- hg_sum[c("GOMFID","Pvalue","Term")]
    
    # save results to the list of results 
    hyper_results[[i]] <- hg_sub # hg7021_1_sub[1:5,]
    
  }
  return(hyper_results)
}

assay6503_clustering12 <- hyperGTest_list(assay6503_sub, assay6503_syrjala_response_matrices,12)
assay6503_clustering5 <- hyperGTest_list(assay6503_sub, assay6503_syrjala_response_matrices,5)


# function that compares GO terms of one cluster o GO terms of all other clusters
goid1 <- assay6503_clustering12[[1]]$GOMFID
goid2 <- assay6503_clustering12[[2]]$GOMFID


mgoSim1 <- mgoSim(goid1, goid2, ont="MF", measure="Wang", combine="BMA")
mgoSim1


# original method: wang and bma
similarity_goterms_wang_bma <- function(list_goterms){
  
  #comparing one cluster of go terms to all other clusters of go terms
  similarity <- lapply(list_goterms, function(x) 
    lapply(list_goterms, function(y) 
      mgoSim(x$GOMFID, y$GOMFID, ont="MF", measure="Wang", combine="BMA")))
  
  #Turn the results- all of the calculated RMSE values-  into a matrix
  ## Makes a list of matrices
  list_matrices <- lapply(similarity, function(x) as.matrix(x))
  ## Turns that list into a matrix
  matrix <- matrix(unlist(list_matrices), nrow=length(list_matrices))
  
  # make into a dissimilarity matrix
  dissimilarity_matrix <- 1-matrix
  
  return(dissimilarity_matrix)
}

# trying another method: wang and rcmax
similarity_goterms_wang_rcmax <- function(list_goterms){
  
  #comparing one cluster of go terms to all other clusters of go terms
  similarity <- lapply(list_goterms, function(x) 
    lapply(list_goterms, function(y) 
      mgoSim(x$GOMFID, y$GOMFID, ont="MF", measure="Wang", combine="rcmax")))
  
  #Turn the results- all of the calculated RMSE values-  into a matrix
  ## Makes a list of matrices
  list_matrices <- lapply(similarity, function(x) as.matrix(x))
  ## Turns that list into a matrix
  matrix <- matrix(unlist(list_matrices), nrow=length(list_matrices))
  
  # make into a dissimilarity matrix
  dissimilarity_matrix <- 1-matrix
  
  return(dissimilarity_matrix)
}

# mathod 1
# getting a 12 x 12 matrix for how similar the GO terms are
assay6503_clustering12_similarity_wbma <- similarity_goterms_wang_bma(assay6503_clustering12)
assay6503_clustering12_similarity_wbma
# heatmap of the matrix
image(main="Assay 6503 Cluster GO Term Dissimilarity", 
      x=1:12, xlab="Cluster Number",
      y=1:12, ylab="Cluster Number",
      z=assay6503_clustering12_similarity_wbma)

# method 2
assay6503_clustering12_similarity_wrcmax <- similarity_goterms_wang_rcmax(assay6503_clustering12)
# heatmap of the matrix
image(main="Assay 6503 Cluster GO Term Dissimilarity", 
      x=1:12, xlab="Cluster Number",
      y=1:12, ylab="Cluster Number",
      z=assay6503_clustering12_similarity_wrcmax)

# making the matrix a lower triangular matrix with diag=FLASE
assay6503_clustering12_similarity_wbma[upper.tri(assay6503_clustering12_similarity_wbma, diag=TRUE)] <- NA
# making a histogram of the lower tri matix to see the distribution of dissimialrity values 
assay6503_clustering12_similarity_wbma %>% 
  truehist(., main= "Assay 6503 Dissimilarity of 12 Clusters",
           xlab="Cluster Dissimilarity" , xlim=range(0:1), h=0.05,
           ylab="Relative Frequency Density")
abline(v= mean(assay6503_clustering12_similarity_wbma, na.rm=TRUE), 
       col="red" )
abline(v= median(assay6503_clustering12_similarity_wbma, na.rm=TRUE),
       col="black")


assay6503_clustering5_similarity_wbma <- similarity_goterms_wang_bma(assay6503_clustering5)
# heatmap for 5 clusters
assay6503_clustering5_similarity
image(main="Assay 6503 Cluster GO Term Dissimilarity", 
      x=1:5, xlab="Cluster Number",
      y=1:5, ylab="Cluster Number",
      z=assay6503_clustering5_similarity)
assay6503_clustering5_similarity_wbma[upper.tri(assay6503_clustering5_similarity_wbma, diag=TRUE)] <- NA
assay6503_clustering5_similarity_wbma %>% 
  truehist(., main= "Assay 6503 Dissimilarity of 5 Clusters",
           xlab="Cluster Dissimilarity" , xlim=range(0:1), h=0.05,
           ylab="Relative Frequency Density")
abline(v= mean(assay6503_clustering5_similarity_wbma, na.rm=TRUE),
       col="red")
abline(v= median(assay6503_clustering5_similarity_wbma, na.rm=TRUE),
       col="black")


# trying out ggplot for heatmaps
assay6503_clustering12_ggplot <- assay6503_clustering12_similarity %>%
  tbl_df() %>%
#  rownames_to_column('Var1') %>% 
#  gather(Var2, value, -Var1) %>%
#  mutate(
#    Var1=factor(Var1, levels = 1:12),
#    Var2=factor(gsub("V","",Var2), levels=1:12)
#  )
#ggplot(assay6503_clustering12_ggplot, aes(Var1, Var2)) +
#  geom_tile(aes(fill = value)) + 
#  geom_text(aes(label = round(value, 1))) +
#  scale_fill_gradient(low = "white", high = "red") 



# looking at the 5 clusters- determined from avg sil width 
assay6503_clustering5_similarity <- similarity_goterms(assay6503_clustering5)


