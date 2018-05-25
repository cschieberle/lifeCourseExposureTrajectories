library(parallel)

#' Default configuration for this package.
#'
#' @export
#
defaultConfig <- function(path = ".", sample.size = 1000) {
  config <- new.env()
  
  config[["PATH"]] <- path

  config[["NUM_MASTER_CORES"]] <- 2
  config[["NUM_CORES"]] <- parallel::detectCores() - config[["NUM_MASTER_CORES"]] - 1
  config[["CLUSTER_TYPE"]] <- "PSOCK"
  
  config[["SAMPLE_SIZE"]] <- sample.size
  config[["NUM_SIM"]] <- 100
  
  return(config)
}