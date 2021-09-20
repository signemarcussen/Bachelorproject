# Define project functions ------------------------------------------------

#######################################################################
## Make numerical representation of peptides/proteins using BLOSUM-X ##
## encoding.                                                         ##
#######################################################################

# x <- peptides
# m <- blosum62

blosum_encoding <- function(x, m) {
      X_enc <- lapply(strsplit(x, split = ""),
                      function(x_i) { return( c(m[,x_i]) ) })
      X_enc <- do.call(rbind, X_enc)
      colnames(X_enc) <- x
      rownames(X_enc) <- rep(colnames(m), nchar(x[1]))
      return(X_enc)
}
