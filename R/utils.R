#Utility functions

#fast expand.grid
expand.grid.jc <- function(seq1,seq2) {
  cbind(Var1 = rep.int(seq1, length(seq2)),
        Var2 = rep.int(seq2, rep.int(length(seq1),length(seq2))))
}

#find index of combination from row and column indices
combo_indexer = function(m,N){
  i_ = m[,1]
  j_ = m[,2]
  (i_-1) * N - (i_^2 + i_)/2 + j_
}
