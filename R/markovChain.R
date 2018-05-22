library(igraph)

#' Determines the matrix of transition rates from the sequence data and creates a Markov chain.
#'
#' @param data.seq data.seq
#' @param data.scodes data.scodes
#' @param brewer.pal.name Name of the RColorBrewer palette to use. TraMineR uses the following: If number of states <= 8 then 'Accent'; if between 8 and 12, then 'Set3'; if greater than 12 you have to specify your own palette.
#' @export
#
asMarkovChain <- function(data.seq, data.scodes, brewer.pal.name = NULL) {
  transition.rate <- seqtrate(data.seq)
  
  net <- graph.adjacency(
    transition.rate, 
    weighted=TRUE, 
    diag=TRUE
  ) 

  V(net)$name <- gsub(" |[[:punct:]]", "", V(net)$name)
  
  if (is.null(brewer.pal.name)) {
    if (length(data.scodes) <= 8) {
      V(net)$color <- RColorBrewer::brewer.pal(length(data.scodes), "Accent")
    } else if (length(data.scodes) > 8 & length(data.scodes) <= 12) {
      V(net)$color <- RColorBrewer::brewer.pal(length(data.scodes), "Set3")
    } else {
      V(net)$color <- c(RColorBrewer::brewer.pal(12, "Set3"), RColorBrewer::brewer.pal(length(data.scodes) - 12, "Set1"))
    }
  }
  
  
  return(net)
}


#' Plots the Markov chain determined from the transition matrix.
#'
#' @param net Markov chain derived from calling asMarkovChain function.
#' @param edge.weight.mult Multiply the original edge weight by this factor.
#' @param arrow.size Size of the arrow tips.
#' @seealso \code{\link{asMarkovChain}}
#' @export
#
plotMarkovChain <- function(net, edge.weight.mult = 5, arrow.size = 0.3) {
  plot.igraph(
    net,
    vertex.label = V(net)$name,
    #layout = layout.fruchterman.reingold, 
    vertex.size=20,
    vertex.label.color = "black",
    vertex.label.family = "sans",
    edge.color = "black",
    edge.width = E(net)$weight * edge.weight.mult, 
    edge.arrow.size = arrow.size
  )
}