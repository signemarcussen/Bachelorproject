# Define project functions ------------------------------------------------

#######################################################################
## Make numerical representation of peptides/proteins using BLOSUM-X ##
## encoding.                                                         ##
#######################################################################

blosum_encoding <- function(peptide, blosum_matrix) {
   
   # Check input vector
   pep_check(pep = peptide)
   
   # Then we convert the vector of peptides to a matrix
   # with dimensions 'm x n' = 'n_peps x length_peps'
   p_mat = peptide %>% pep_mat
   
   # Assign meaningful variable names to dimensions
   n_peps = length(peptide)    # i'th row
   l_peps = nchar(peptide[1])  # j'th column
   l_enc  = ncol(blosum_matrix) # k'th slice
   
   # Finally we define our output tensor as a 3d array
   # with dimensions n_peps x l_peps x l_enc (l_enc = 20)
   o_tensor = array(data = NA, dim = c(n_peps,l_peps, l_enc))
   for (i in 1:n_peps) {
      pep_i_residues = p_mat[i,]
      pep_img = blosum_matrix[pep_i_residues,]
      print(pep_img)
      o_tensor[i,,]  = pep_img
   }
   return(o_tensor)
}

#######################################################################
## Make numerical representation of peptides/proteins using          ##
## One-hot encoding.                                                 ##
#######################################################################


one_hot_encoding <- function(peptide, amino) {
   
   # Define encoding matrix
   m = nchar(amino)
   n = nchar(amino)
   enc = diag(1, m, n)

   dimnames(enc) = c(strsplit(amino, ""),
                     strsplit(amino, ""))
   
   # Encode the sequence 
   x_enc = enc[unlist(strsplit(peptide, "")),]
   
   # Defining output tensor as a 3d array
   n_peps = length(peptide)
   l_peps = nchar(peptide[1])
   l_enc  = nchar(amino)
   
   o_tensor = x_enc %>% 
      array(., dim = c(n_peps*l_peps, l_enc))
   
   return(o_tensor)
   
}
