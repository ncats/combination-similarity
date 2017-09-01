# KS TEST FUNCTIONS

## Code for Performing KS Test on Two Matrices

#NOTE: there is a function KS.test() in the package "stats" that does this
#ks.test()


## Code for Performing KS Test on Two Objects (only Good blocks from the assay)

ks_two_matrices <- function(matrix1, matrix2){
    # ks test is done on the matrices from the block objects 
    return ( ks.test( as.vector(as.matrix(matrix1)) , as.vector(as.matrix(matrix2)) ) )
}



## Code for determining D based on p values, etc.

# The default p value is set to 0.05, but the user can change it as they wish
get_ks_test_result <- function(ks_test_result, p=0.05) {
  # first check if the value is NA
  ## if the value is not NA (if there is a KS test result)
  if(is.na(ks_test_result)==FALSE){
    # extracting the computed p value from the ks test result and comparing it to the designated p value
    ## reject the null hypothesis that there is no difference between the matrices
    if(ks_test_result$p.value < p){ return (ks_test_result$statistic) }
    else{ return (0) }
  }
  ## if the value is NA, then the result should return an NA as well
  else { return (NA) }
}



## Code for Calculating and Visualizing all of the KS Tests for all blocks given a list of blocks

ks_list <- function(list_blocks){
  
  # Perform KS test for each block compared to all of the other blocks in the assay
  list_ks_test <- lapply(list_blocks, 
                         function(x) lapply(list_blocks, 
                                            function(y) ks_two_matrices(x,y)))
  
  # For one of the elements in a list this works:
  # as.matrix(lapply(list_ks_test[[1]], function(x) get_ks_test_result(x)))
  
  # Check p value: if significant, D=D. if not significant, set D=0 (done in function "significance")
  list_ks_values <- lapply(list_ks_test, 
                           function(x) lapply(x, 
                                              function(y) get_ks_test_result(y)))
  
  
  #Turn the results- all of the result of ks test-  into a matrix
  ## Makes a list of matrices
  list_matrices <- lapply(list_ks_values, function(x) as.matrix(x))
  ## Turns that list into a matrix
  matrix(unlist(list_matrices), nrow=length(list_matrices))
  
  # Ways to check this is working correctly: 
  ## (a) all diagonal entries of resulting matrix MUST be 0 (no error when comparing a matrix to itself)
  ## (b) matrix = symmertric = transpose of itself (magnitude of euclidean distance computation for first block compared to second block same in magnitude as euclidean distance computation for second block compared to first block)
  
}

# (NOTE: I feel as though there is a way to do this without creating so many additional objects, but not sure how to streamline)