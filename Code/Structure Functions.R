## Function that Extracts the X Agent and Y Agents given a list of blocks 

library(ncgchts)

# want: eventually a function that displays a matrix (heatmap) 
# with similarity of the corresponding x and y agents


get_unique_agent_query <- function(list_blocks) {
  # list of x agent
  x_agent <- lapply(list_blocks, function(i) sid(x.agent(i)))
  # list of y agent
  y_agent <- lapply(list_blocks, function(i) sid(y.agent(i)))
  
  # unlist the lists of x and y agent, put the two together in a vector of all of the agent
  # take just the unique agent in the vector
  unique_agent <- unique(c(unlist(x_agent), unlist(y_agent)))
  
  # pass a vector of SID's through query.ncgc.sample() to get a 
  # dataframe with all of the information about the compounds
  # return the data frame
  return( query.ncgc.sample(unique_agent) )
}



## step 1: distance between agents in comparing two blocks
agent_distance_two_blocks <- function(block1, block2, unique_agent_query){
  # extract the Sample ID's of the x and y agents for both of the blocks
  ## x agents
  x_1 <- sid(x.agent(block1))
  x_2 <- sid(x.agent(block2))
  ## y agent
  y_1 <- sid(y.agent(block1))
  y_2 <- sid(y.agent(block2))
  
  # match that to the query which has already been run
  ## x agents
  sx_1 <- subset(unique_agent_query, SAMPLE_ID==x_1)$SMILES_ISO
  sx_2 <- subset(unique_agent_query, SAMPLE_ID==x_2)$SMILES_ISO
  ## y agents
  sy_1 <- subset(unique_agent_query, SAMPLE_ID==y_1)$SMILES_ISO
  sy_2 <- subset(unique_agent_query, SAMPLE_ID==y_2)$SMILES_ISO
  
  # get chemical fingerprint
  ## x agents
  x_1_fp <- get.fingerprint( parse.smiles( sx_1 )[[1]] , type="circular")
  x_2_fp <- get.fingerprint( parse.smiles( sx_2 )[[1]] , type="circular")
  ## y agents
  y_1_fp <- get.fingerprint( parse.smiles( sy_1 )[[1]] , type="circular")
  y_2_fp <- get.fingerprint( parse.smiles( sy_2 )[[1]] , type="circular")
   
  # measure of distance between x and y agents
  x_distance <- 1 - distance(x_1_fp, x_2_fp)
  y_distance <- 1 - distance(y_1_fp, y_2_fp)
  
  # take an average of those distances
  return ( (x_distance+y_distance) / 2 )
}



## step 2: distance between agents for all blocks in an assay
agent_distance_list <- function(list_blocks, unique_agent_query){
  
  # calculate the distance between x and y agents for all pairs in the assay (list of blocks)
  list_agent_distance <-  lapply(list_blocks, 
                                 function(x) sapply(list_blocks, function(y) agent_distance_two_blocks(x,y, unique_agent_query = unique_agent_query)))
  
  list_matrices <- lapply(list_agent_distance, function(x) as.matrix(x))
  ## Turns that list into a matrix
  matrix(unlist(list_matrices), nrow=length(list_matrices))
}





agent_distance_block <- function(block, unique_agent_query) {
  # extract the Sample ID's of the x and y agents for both of the blocks
  ## x agent
  x <- sid(x.agent(block))
  ## y agent
  y <- sid(y.agent(block))
  
  # match that to the query which has already been run
  ## x agent
  sx <- subset(unique_agent_query, SAMPLE_ID==x)$SMILES_ISO
  ## y agent
  sy <- subset(unique_agent_query, SAMPLE_ID==y)$SMILES_ISO
  
  # get chemical fingerprint
  ## x agent
  x_fp <- get.fingerprint( parse.smiles( sx )[[1]] , type="circular")
  ## y agent
  y_fp <- get.fingerprint( parse.smiles( sy )[[1]] , type="circular")
  
  # measure of distance between x and y agent
  distance <- 1 - distance(x_fp, y_fp)
  
  return(distance)
}


agent_vector <-function(list_blocks, unique_agent_query){
  sapply(list_blocks, function(x) agent_distance_block(x, unique_agent_query = unique_agent_query))
}





