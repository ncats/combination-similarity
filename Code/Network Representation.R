library(igraph)

# learning more about the igraph package-
# ultimate goal: make a network representation of the clusters 
g1 <- graph( edges = c(1,2,2,3,3,1), n=10, directed=FALSE)
plot(g1)

eg <- make_empty_graph(40)
plot(eg, vertex.size=10, vertex.label=NA)

fg <- make_full_graph(40)
plot(fg, vertex.size=10, vertex.label=NA)


assay6503_syrjala_response_d

assay6503_clust <- hclust(assay6503_syrjala_response_d)
as.dendrogram(assay6503_clust) %>% plot(.)

dendPlot(assay6503_clust, mode="hclust")

# https://stackoverflow.com/questions/11462901/cluster-presentation-dendrogram-alternative-in-r