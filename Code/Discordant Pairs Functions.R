## Response Matrices are Similar, Synergy surfaces are not (& vice versa) (one function for comparing corresponding entries)

discordant_pairs_entries <- function(entry1, entry2){
  
  if ( is.na(entry1) | is.na(entry2) ) { return(NA) } 
  
  else{
    #Matrix 1 Similar, Matrix 2 Not Similar
    if (entry1 == 0 && entry2 != 0) { return(TRUE) }
    #Matrix 1 Not Similar, Matrix 2 Similar
    else if (entry1 != 0 && entry2==0) { return(TRUE) }
    # Not a discoordinate pair
    else{ return(FALSE) }
  }
  
}




discordant_pairs_for <- function(matrix1, matrix2, display_matrix=FALSE){
  
  if ( nrow(matrix1) == nrow(matrix2) && ncol(matrix1) == ncol(matrix2)){
    
    discordant_matrix <- c()
    
    for(i in 1:length(matrix1)){
      discordant_matrix[i] <- discordant_pairs_entries( as.vector(matrix1)[i], as.vector(matrix2)[i] )
      #return(discordant_matrix)
    } # end of for loop
    
    return (matrix(discordant_matrix, nrow=nrow(matrix1), ncol=ncol(matrix2)))
  } #end of if statement
  
  else{ warning("cannot find discordinant pairs- dimensions are not the same") }
  
} # end of function body



## for a matrix?

discordant_pairs <- function(matrix1, matrix2, display_matrix=FALSE){
  
  # apply the comparison function to all of the corresponding entries in the matrices
  discoordinate_matrix <- matrix( sapply(matrix1, function(x) discordant_pairs_entries(x, matrix2)) ,
                                  nrow=nrow(matrix1), ncol=ncol(matrix1) )
  
  if(display_matrix==TRUE) { return(discoordinate_matrix) }
  else{ return( which(discoordinate_matrix==TRUE, arr.ind=TRUE) ) }
}



## (one function to apply that to the entire matrix)

discordant_pairs <- function(matrix1, matrix2){
  # check that matrices are the same size (cannot compare if different dimensions)
  ## if the dimensions are the same...
  if ( nrow(matrix1) == nrow(matrix2) && 
       ncol(matrix1) == ncol(matrix2)){
    # compare each entry of the matrix directly with the corresponding matrix of the other entry 
    lapply( matrix1, 
            function(x) lapply(x, 
                               function(y) discordant_pairs_entries(x,y)))
  }
  ## if the dimensions are not the same size...
  else{ warning("cannot find discordinant pairs- dimensions are not of the same size") }
  
}

