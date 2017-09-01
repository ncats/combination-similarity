# Last Updated: 7/24/17 fixed function to work on any list of bocks
# Euclidean
## Code for Calculating Euclidean Distance for Two Matrices

euclidean_two_matrices <- function(matrix1, matrix2){
  # Make sure both things passed in are either matrices or vectors
  matrix1 <- as.matrix(matrix1) 
  matrix2 <- as.matrix(matrix2)
  
  # Make sure both matrices have the same dimension
  if (nrow(matrix1) != nrow(matrix2) | ncol(matrix1) != ncol(matrix2)) {
    warning("Matrices are not of the same dimension")
  }
  
  # Calculate Euclidean Distance
  ## take the difference of each element in the matrix
  ## square it, 
  ## sum all elements, 
  ## take the square root of the average
  sqrt( sum( (as.vector(matrix1)-as.vector(matrix2))^2, na.rm=TRUE ) ) 
}

# (NOTE: I would have done this in dplyr, but for some reason the package won't load on R??)





## Code for Calculating and Visualizing all of the Euclidean Distances for all blocks given a list of blocks

euclidean_list <- function(list_blocks){
  
  # Calculate Euclidean Distance for each block compared to all of the other blocks in the assay
  list_euclidean <- lapply(list_blocks, 
                           function(x) sapply(list_blocks, function(y) euclidean_two_matrices(x,y)))
  
  #Turn the results- all of the calculated Euclidean Distances-  into a matrix
  ## Makes a list of matrices
  list_matrices <- lapply(list_euclidean, function(x) as.matrix(x))
  ## Turns that list into a matrix
  matrix(unlist(list_matrices), nrow=length(list_matrices))
  
  # Ways to check this is working correctly: 
  ## (a) all diagonal entries of resulting matrix MUST be 0 (no error when comparing a matrix to itself)
  ## (b) matrix = symmertric = transpose of itself (magnitude of euclidean distance computation for first block compared to second block same in magnitude as euclidean distance computation for second block compared to first block)
  
}

# (NOTE: I feel as though there is a way to do this without creating so many additional objects, but not sure how to streamline)






## Code for Calculating Euclidean Distance on Two Objects (only Good blocks from the assay)

#euclidean_two_objects <- function(object1, object2){
  
#  # RMSE calculation only performed if both block objects are "good" for mQC
#  if(grepl("good", object1@mqc$class, ignore.case = TRUE) && 
#     grepl("good", object2@mqc$class, ignore.case = TRUE) ) {
    
#    # RMSE is done on the matrices from the block objects 
#    return ( euclidean_two_matrices( as.matrix(object1) , as.matrix(object2) ) )
#  }
  
#  # If at least one of block objects are not classfiied as "good", function returns NA 
#  else{
#    # warning("One or both of the objects is/are not good")
#    return ( NA )
#  }
#}