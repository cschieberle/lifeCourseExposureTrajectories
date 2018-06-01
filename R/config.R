library(parallel)

#' Default configuration for this package.
#'
#' @param path Base path for input and output.
#' @param subfolder.output Output subfolder to store intermediate and result files.
#' @param write.output Whether to write output files. If true, this may slow down execution time.
#' @param subfolder.exposure Subfolder of the base path which contains exposure data (again subfolder for each stressor).
#' @param employment.mapping Employment mapping file within the exposure subfolder.
#' @param num.sim Number of trajectory simulations to be conducted.
#' @param sample.size Sample size of exposure data.
#' @export
#
defaultConfig <- function(path = ".", subfolder.output = NULL, write.output = T, subfolder.exposure = NULL, employment.mapping = NULL, num.sim = 100, sample.size = 1000) {
  config <- new.env()
  
  config[["PATH"]] <- path
  
  if (!is.null(subfolder.output)) {
    config[["PATH_OUTPUT"]] <- paste0(path, '/', subfolder.output)
  } else {
    if (is.null(config[["PATH_OUTPUT"]])) {
      config[["PATH_OUTPUT"]] <- paste0(path, '/output')
    }
  }
  
  # Create output folder for cluster log file; otherwise makeCluster with output file given may hang ..
  dir.create(file.path(config[["PATH_OUTPUT"]]), showWarnings = FALSE)
  
  config[["WRITE_OUTPUT"]] <- write.output
  
  if (!is.null(subfolder.exposure)) {
    config[["PATH_EXPOSURE"]] <- paste0(path, '/', subfolder.exposure)
  } else {
    if (is.null(config[["PATH_EXPOSURE"]])) {
      config[["PATH_EXPOSURE"]] <- paste0(path, '/data')
    }
  }
  
  if (!is.null(employment.mapping)) {
    config[["EMPLOYMENT_MAPPING"]] <- paste0(config[["PATH_EXPOSURE"]], '/', employment.mapping)
  } else {
    if (is.null(config[["EMPLOYMENT_MAPPING"]])) {
      config[["EMPLOYMENT_MAPPING"]] <- paste0(config[["PATH_EXPOSURE"]], '/employment_mapping.xlsx')
    }
  }
  
  config[["NUM_MASTER_CORES"]] <- 2
  config[["NUM_CORES"]] <- parallel::detectCores() - config[["NUM_MASTER_CORES"]] - 1
  config[["CLUSTER_TYPE"]] <- "PSOCK"
  
  config[["SAMPLE_SIZE"]] <- sample.size
  config[["NUM_SIM"]] <- num.sim
  
  config[["CSV_SEP"]] <- ','
  config[["CSV_DEC"]] <- '.'
  
  config[["CLUSTER_OUTFILE"]] <- ""
  
  return(config)
}