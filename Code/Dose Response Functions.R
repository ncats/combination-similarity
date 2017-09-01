# WITHOUT JITTER
# step 1: write function that gets the dose response comparing last row to last row and last column to last column

dose_two_blocks <- function(block1, block2){
  # vectors
  ## last rows of the blocks- x agents
  x1 <- as.matrix(block1)[nrow(as.matrix(block1)), ]
  x2 <- as.matrix(block2)[nrow(as.matrix(block2)), ]
  ## last columns of blocks- y agents
  y1 <- as.matrix(block1)[ ,ncol(as.matrix(block1))]
  y2 <- as.matrix(block2)[ ,ncol(as.matrix(block2))]
  
  # ccc for agents
  ## ccc for x agents
  x.ccc <- epi.ccc(x1,x2)$rho.c$est
  ## ccc for y agents
  y.ccc <- epi.ccc(y1,y2)$rho.c$est
  
  # average of the ccc calculations for the x and y agents
  return ( (x.ccc + y.ccc)/2 )
}

# NOTE: need the full matrices for this becasue you need the last rows and the last columns


dose_list <- function(list_blocks){
  # calculate the ccc average for all blocks compared to all other blocks
  list_dose <-  lapply(list_blocks, 
                                 function(x) sapply(list_blocks, function(y) dose_two_blocks(x,y)))
  
  list_matrices <- lapply(list_dose, function(x) as.matrix(x))
  ## Turns that list into a matrix
  matrix(unlist(list_matrices), nrow=length(list_matrices))
}

# NOTE: will return NA when there is no response


## WITH JITTER
# step 1: write function that gets the dose response comparing last row to last row and last column to last column

dose_two_blocks_jitter <- function(block1, block2){
  # vectors
  ## last rows of the blocks- x agents
  x1 <- as.matrix(block1)[nrow(as.matrix(block1)), ]
  x2 <- as.matrix(block2)[nrow(as.matrix(block2)), ]
  ## last columns of blocks- y agents
  y1 <- as.matrix(block1)[ ,ncol(as.matrix(block1))]
  y2 <- as.matrix(block2)[ ,ncol(as.matrix(block2))]
  
  # add jitter to the vectors
  ## x vectors
  x1_j <- jitter(x1)
  x2_j <- jitter(x2)
  ## y vectors
  y1_j <- jitter(y1)
  y2_j <- jitter(y2)
  
  # ccc for x agents
  x.ccc <- epi.ccc(x1_j,x2_j)$rho.c$est
  
  # ccc for y agents
  y.ccc <- epi.ccc(y1_j,y2_j)$rho.c$est
  
  
  # average of the ccc calculations for the x and y agents
  return ( (x.ccc + y.ccc)/2 )
  
}

# NOTE: need the full matrices for this becasue you need the last rows and the last columns


dose_list_jitter <- function(list_blocks){
  # calculate the ccc average for all blocks compared to all other blocks
  list_dose <-  lapply(list_blocks, 
                       function(x) sapply(list_blocks, function(y) dose_two_blocks_jitter(x,y)))
  
  list_matrices <- lapply(list_dose, function(x) as.matrix(x))
  ## Turns that list into a matrix
  matrix(unlist(list_matrices), nrow=length(list_matrices))
}

# NOTE: will return NA when there is no response



# compares the x agent dose response to the y agent dose response
dose_block <- function(block){
  x <- as.matrix(block)[nrow(as.matrix(block)), ]
  y <- as.matrix(block)[ ,ncol(as.matrix(block))]
  
  ## add jitter
  x_j <- jitter(x)
  y_j <- jitter(y)
  
  ## ccc for x and y agents
  ccc <- epi.ccc(x_j,y_j)$rho.c$est
  
  return (ccc)
}


dose_vector <-function(list_blocks){
  sapply(list_blocks, function(x) dose_block(x))
}






z <- as.matrix(assay6410_sub_last[[3]])

# last rows of the blocks- x agents
x1 <- as.matrix(assay6410_sub_last[[3]])[nrow(as.matrix(assay6410_sub_last[[3]])), ]
x2 <- as.matrix(assay6410_sub_last[[3]])[nrow(as.matrix(assay6410_sub_last[[3]])), ]

# last columns of blocks- y agents
y1 <- as.matrix(assay6410_sub_last[[3]])[ ,ncol(as.matrix(assay6410_sub_last[[3]]))]
y2 <- as.matrix(assay6410_sub_last[[3]])[ ,ncol(as.matrix(assay6410_sub_last[[3]]))]

# ccc for x agents
x.ccc <- epi.ccc(x1,x2)
# ccc for y agents
y.ccc <- epi.ccc(y1,y2)