# Last Updated: 7/24/17 fixed function to return a list of delta bliss matrices... then the other functions (existing 4) can be used on the list of delta bliss matrices
# Function that returns the delta bliss matrix for each block in an assay

list_delta_bliss <- function(list_blocks) {
  # creating a list of the delta bliss matrices 
  with_na <- lapply(list_blocks, function(x) bliss.model(as.matrix(x))$delta)
  
  # getting rid of the last row and the last column
  lapply(with_na, function(y) y[-nrow(y),-ncol(y)])
}plot()




# 7/24/17 Don't need this function anymore-- use the original functions (RMSE_list, euclidean_list, ks_list, dist.syrjala)
# Function that, given a list of block objects, computes similarity between the delta bliss matrices for all blocks in an assay using the method the user wants (RMSE, euclidean, ks, or syrjala)

# delta_bliss_similarity <- function(list_blocks, method){
#  
#  
#  ## Syrjala
#  if (grepl("syrjala", method, ignore.case=TRUE)){
#    return( dist.syrjala(list_blocks, model="bliss") )
#  }
#  
#  # compute similarity between all of the resulting delta bliss matrices
#  ## RMSE
#  if(grepl("RMSE", method, ignore.case=TRUE)){
#    # get a list of the delta bliss matrices from a list of block objects (only if Good matrices)
#    list_delta_bliss <- lapply(list_blocks, function(x) get_delta_bliss(x))
#    
#    list_similarity_bliss <- lapply(list_delta_bliss, 
#                                    function(x) sapply(list_delta_bliss, function(y) RMSE_two_matrices(x,y)))
#  }
#  
#  ## Euclidean
#  else if (grepl("euclidean", method, ignore.case=TRUE)){
#    # get a list of the delta bliss matrices from a list of block objects (only if Good matrices)
#    list_delta_bliss <- lapply(list_blocks, function(x) get_delta_bliss(x))
#    
#    list_similarity_bliss <- lapply(list_delta_bliss, 
#                                    function(x) lapply(list_delta_bliss, function(y) euclidean_two_matrices(x,y)))  
#  }
#  
#  ## KS
#  else if (grepl("ks", method, ignore.case=TRUE)){
#    # get a list of the delta bliss matrices from a list of block objects (all matrices and the ones that arent "good" will be taken out later)
#    list_delta_bliss <- lapply(list_blocks, function(x) get_delta_bliss(x, check_mqc=FALSE))
#    
#    # Perform KS test for each block compared to all of the other blocks in the assay
#    list_similarity_bliss_middle <- lapply(list_delta_bliss, 
#                                           function(x) lapply(list_delta_bliss, function(y) ( ks.test( as.vector(as.matrix(x)) , as.vector(as.matrix(y)) ) )))
#    # Check p value: if significant, D=D. if not significant, set D=0 (done in function "significance")
#    list_similarity_bliss <- lapply(list_similarity_bliss_middle, 
#                                    function(x) lapply(x, 
#                                                       function(y) get_ks_test_result(y)))
#    # NOTE: still need to change the "medium" or "bad" ones to NA
# }
#  
#  
#  #Turn the results- all of the calculated similarity values-  into a matrix (doesn't need to happen for Syrjala becasue the function does it itself)
#  ## Makes a list of matrices
#  list_bliss_matrices <- lapply(list_similarity_bliss, function(x) as.matrix(x))
#  ## Turns that list into a matrix
#  return ( matrix(unlist(list_bliss_matrices), nrow=length(list_bliss_matrices)) ) 
#}



