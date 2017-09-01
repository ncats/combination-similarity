# Last Updated: 7/24/17 fixed function to work on any list of bocks
# RMSE
## Code for Calculating RMSE Value for Two Matrices

RMSE_two_matrices <- function(matrix1, matrix2){
  # Make sure both things passed in are either matrices or vectors
  matrix1 <- as.matrix(matrix1) 
  matrix2 <- as.matrix(matrix2)
  
  # Make sure both matrices have the same dimension
  if (nrow(matrix1) != nrow(matrix2) | ncol(matrix1) != ncol(matrix2)) {
    warning("Matrices are not of the same dimension")
  }
  
  # Perform RMSE
  ## take the difference of each element in the matrix
  ## unwrap(??) the resulting matrix into a vector,
  ## square it, 
  ## sum all elements, 
  ## take the average, 
  ## take the square root of the average
  (sum ( (as.vector(matrix1)-as.vector(matrix2))^2, na.rm=TRUE) / (nrow(matrix1)*ncol(matrix1)) ) ^ 1/2 
}




## DONE Code for Calculating and Visualizing all of the RMSE values for all blocks given a list of blocks

RMSE_list <- function(list_blocks){
  
  # Calculate RMSE for each block compared to all of the other blocks in the assay
  list_RMSE <- lapply(list_blocks, 
                      function(x) sapply(list_blocks, function(y) RMSE_two_matrices(x,y)))
  
  #Turn the results- all of the calculated RMSE values-  into a matrix
  ## Makes a list of matrices
  list_matrices <- lapply(list_RMSE, function(x) as.matrix(x))
  ## Turns that list into a matrix
  matrix(unlist(list_matrices), nrow=length(list_matrices))
  
  # Ways to check this is working correctly: 
  ## (a) all diagonal entries of resulting matrix MUST be 0 (no error when comparing a matrix to itself)
  ## (b) matrix = transpose of itself (magnitude of RMSE computation for first block compared to second block same in magnitude RMSE computation for second block compared to first block)
  
}

# (NOTE: I feel as though there is a way to do this without creating so many additional objects, but not sure how to streamline)
# (NOTE: is it bad style to call a library within a function?)







## DON'T USE AS OF 7/21/17 Code for Calculating RMSE on Two Objects (only Good blocks from the assay)

#RMSE_two_objects <- function(object1, object2){
#  
#  # RMSE calculation only performed if both block objects are "good" for mQC
#  if(grepl("good", object1@mqc$class, ignore.case = TRUE) && 
#     grepl("good", object2@mqc$class, ignore.case = TRUE) ) {
#    
#    # RMSE is done on the matrices from the block objects 
#    return ( RMSE_two_matrices( as.matrix(object1) , as.matrix(object2) ) )
#  }
#  
#  # If at least one of block objects are not classfiied as "good", function returns NA 
#  else{
#    # warning("One or both of the objects is/are not good")
#    return ( NA )
#  }
#}

