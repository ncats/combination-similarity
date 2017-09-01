# Subset Functions

# 1. Function that checks a block object- inputs a block object and returns TRUE if the block is "good"
good_block <- function(block_object){
  if ( grepl("good", block_object@mqc$class, ignore.case = TRUE) ) {return (TRUE)}
  else { return(FALSE) }
}




# 2. Function that makes a boolean list- list of TRUE/FALSE with TRUE indicating good
good_indices <- function(list_blocks){
  # Boolean list that returns TRUE if the block is good and FALSE if anything else (medium or bad)
  return ( as.vector( sapply(list_blocks, function(x) good_block(x)) ) )
}

## NOTE: it may be important to make another boolean list for the blocks are "good" thoroughout several assays (their corresponding boolean lists)
## for example (checking 3 assays): malaria_good <- assay1764_bool == TRUE & assay1763_bool == TRUE & assay1761_bool == TRUE 




# 3. Function that takes a list of blocks and a boolean list and returns the subsets with the last row and last column removed
good_subset <- function(list_blocks, list_bool, keep_last=FALSE){ 
  
  # the subset- only the blocks with the corresponding TRUE index
  subset <-  list_blocks[list_bool == TRUE]
  
  # if the incoming list is a list of blocks for response matrices
  if(keep_last==FALSE){
    # try a for loop:
    for (i in 1:length(subset)) {
      subset[[i]]@response <- subset[[i]]@response[-nrow(as.matrix(subset[[i]])), -ncol(as.matrix(subset[[i]]))] 
    }
    }
  
  # if working with the delta bliss matrices, the last row and last column have already been taken out
  # (so no else function)

  return(subset)
}





# assay1764_bool <- as.vector( sapply(assay1764, function(x) good_block(x)) )
# assay1763_bool <- as.vector( sapply(assay1763, function(x) good_block(x)) )
# assay1761_bool <- as.vector( sapply(assay1761, function(x) good_block(x)) )
# Creating an index for where all of the combinations are good across all three of the assays
# malaria_good <- assay1764_bool == TRUE & assay1763_bool == TRUE & assay1761_bool == TRUE 

# assay1764$`1.1.1`@response

# this works:
# subset$`1.1.1`@response <- subset$`1.1.1`@response[-nrow(as.matrix(subset$`1.1.1`)), -ncol(as.matrix(subset$`1.1.1`))]

# corresponding lapply:
# lapply(subset, function(x) x@response[-nrow(x@response),-ncol(x@response)])
## doesn't work: lapply(subset, function(x) as.matrix(x)[-nrow(as.matrix(subset$x)),-ncol(as.matrix(subset$x))] )


# getting rid of the last row and the last column in each block
# return ( lapply(subset, function(x) x@response[-nrow(x@response),-ncol(x@response)]) )
# NOTE: doesn't return the original list with just the response edited as intended