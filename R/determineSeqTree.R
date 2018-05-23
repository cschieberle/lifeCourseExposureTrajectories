library(TraMineR)

#' Determines the sequence tree.
#' 
#' @param data.rshp.subset 
#' @param dist.method See \code{\link[TraMineR]{seqdist}} for details.
#' @param dist.indel See \code{\link[TraMineR]{seqdist}} for details.
#' 
#' @export
#
determineSeqTree <- function(data.rshp.subset, dist.method = "OM", dist.indel = 1) {
  data.seq <- seqdef(data.rshp.subset, 8:11, alphabet = data.alphabet[1:11], states = data.scodes[1:11], labels = data.labels[1:11], xtstep = 1)
  submat <- seqsubm(data.seq, method = "TRATE")
  dist <- seqdist(data.seq, method = dist.method, indel = dist.indel, sm = submat)
  
  system.time( 
    st <- seqtree(data.seq ~ SEX + AGE.AT.FRST.INTRVW, data = data.rshp.subset, R = 1000, diss = dist, pval = 0.05)
  )
  
  return(st)
}

