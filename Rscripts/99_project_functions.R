# Define project functions ------------------------------------------------

#######################################################################
## Make numerical representation of peptides/proteins using BLOSUM-X ##
## encoding.                                                         ##
#######################################################################

# # Signe
# pep <- data_complete %>%
#       select(Peptide)
# 
# blosum_encoding <- function(peptides, blosum_matrix) {
#       lapply(str_split(pep, split = ""))
# }


## Leons: Der er noget der ikke virker i line 23..
blosum_encoding <- function(x, m) {
      X_enc <- lapply(strsplit(x, split = ""),
                      function(x_i) { return( c(m[,x_i]) ) })
      X_enc <- do.call(rbind, X_enc)
      rownames(X_enc) <- x
      colnames(X_enc) <- rep(colnames(m), nchar(x[1]))
      return(X_enc)
}
