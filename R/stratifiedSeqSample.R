library(WeightedCluster)

#' Creates a stratified sample of sequences.
#' 
#' Whenever there are too many sequences, difficulties may arise when the distance matrix is being calculated.
#' The problem is described in more detail:
#' http://stackoverflow.com/questions/15929936/problem-with-big-data-during-computation-of-sequence-distances-using-tramine
#' 
#' Here, a stratified sub-sampling approach is follows. This ensures that sequence distribution is maintained. 
#' 
#' @param data.rshp 
#' @param factor The relative sample size (defaults to 0.1, i.e. 10%).
#' @export
#
stratifiedSeqSample <- function(data.rshp, factor = 0.1) {
  ac <- wcAggregateCases(data.rshp[, 8:11])

  data.rshp.subset <- NULL
  for (i in 1:length(ac$aggIndex)) {
    
    idx <- ac$aggIndex[i]
    weight <- ac$aggWeights[i]
    
    disagg.idx.list <- which(ac$disaggIndex == i)
    
    #seq.sample.size <- max(1, ceiling(log(weight)), replace = F)
    seq.sample.size <- round(factor * weight)
    
    if (seq.sample.size > 0) {
      data.rshp.i.sample <- data.rshp[ disagg.idx.list[ sample(x = seq(1, length(disagg.idx.list)), size = seq.sample.size, replace = F) ], ]
      
      data.rshp.subset <- rbind(data.rshp.subset, data.rshp.i.sample)
    }
  }
  
  return(data.rshp.subset)
}


