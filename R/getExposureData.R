library(readxl)

#' Reads individual-specific daily exposure data from a given path.
#' 
#' It is assumed that there is a subfolder for each individual stressor.
#' Furthermore, it is assumed that one separate file per individual is given.
#'
#' @param indiv.id The identifier of an individual.
#' @param stressors List of stressors to be assessed.
#' @export
#
getExposureData <- function(indiv.id, stressors) {
  exposure.data.path <- config[["PATH_EXPOSURE"]]
  employment.mapping.file <- config[["EMPLOYMENT_MAPPING"]] 
  
  message(paste0("Reading daily exposure data -- subject ID: ", indiv.id, " // path: ", exposure.data.path))
  exposure.all <- NULL
  
  for (stressor in stressors) {
    message(stressor)
    daily.exposure <- read.csv(paste0(exposure.data.path, "/", stressor, "/", stressor, "_exposure_sample_", indiv.id, ".csv"))
    daily.exposure$STRESSOR <- stressor
    
    exposure.all <- rbind(exposure.all, daily.exposure)
  }

  emp.map <- readxl::read_excel(employment.mapping.file)
  emp.map <- emp.map[, c("EMP.scode", "empstat", "emp", "student", "retired")]
  
  # merge exposure data using MTUS 'empstat' nomenclature and employment type (or economic status) from EU-SILC named here 'EMP.scode'
  exposure.all <- merge(
    x = exposure.all,
    y = emp.map,
    by = c("empstat", "emp", "student", "retired"),
    sort = T
  )
  # move 'EMP.scode" to be the first column
  exposure.all <- exposure.all[c("STRESSOR", "EMP.scode", setdiff(names(exposure.all), list("STRESSOR", "EMP.scode")))]
  
  return(exposure.all)
}
