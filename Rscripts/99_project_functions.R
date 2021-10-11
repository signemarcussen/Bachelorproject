# Define project functions ------------------------------------------------

#######################################################################
## Make numerical representation of peptides/proteins using BLOSUM-X ##
## encoding.                                                         ##
#######################################################################

blosum_encoding <- function(x, m) {
      X_enc <- lapply(strsplit(x, split = ""),
                      function(x_i) { return( c(m[,x_i]) ) })
      X_enc <- do.call(rbind, X_enc)
      rownames(X_enc) <- x
      colnames(X_enc) <- rep(colnames(m), nchar(x[1]))
      return(X_enc)
}



pep_encode = function(pep){
   
   # Check input vector
   pep_check(pep = pep)
   
   # Set encoding matrix
   bl62_prob = BLOSUM62_PROB
   
   # Then we convert the vector of peptides to a matrix
   # with dimensions 'm x n' = 'n_peps x length_peps'
   p_mat = pep %>% pep_mat
   
   # Assign meaningful variable names to dimensions
   n_peps = length(pep)    # i'th row
   l_peps = nchar(pep[1])  # j'th column
   l_enc  = ncol(bl62_prob) # k'th slice
   
   # Finally we define our output tensor as a 3d array
   # with dimensions n_peps x l_peps x l_enc (l_enc = 20)
   o_tensor = array(data = NA, dim = c(n_peps,l_peps, l_enc))
   for( i in 1:n_peps ){
      pep_i_residues = p_mat[i,]
      pep_img = bl62_prob[pep_i_residues,]
      o_tensor[i,,]  = pep_img
   }
   return(o_tensor)
}

pep_encode_Signe = function(pep, matrix){
   
   # Check input vector
   pep_check(pep = pep)
   
   # Set encoding matrix
   bl62_prob = matrix
   
   # Then we convert the vector of peptides to a matrix
   # with dimensions 'm x n' = 'n_peps x length_peps'
   p_mat = pep %>% pep_mat
   
   # Assign meaningful variable names to dimensions
   n_peps = length(pep)    # i'th row
   l_peps = nchar(pep[1])  # j'th column
   l_enc  = ncol(bl62_prob) # k'th slice
   
   # Finally we define our output tensor as a 3d array
   # with dimensions n_peps x l_peps x l_enc (l_enc = 20)
   o_tensor = array(data = NA, dim = c(n_peps,l_peps, l_enc))
   for( i in 1:n_peps ){
      pep_i_residues = p_mat[i,]
      pep_img = bl62_prob[pep_i_residues,]
      o_tensor[i,,]  = pep_img
   }
   return(o_tensor)
}
