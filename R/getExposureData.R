library(readxl)

#' Reads individual-specific daily exposure data from a given path.
#' 
#' It is assumed that there is a subfolder for each individual stressor.
#' Furthermore, it is assumed that one separate file per individual is given.
#'
#' @param INDIV_SUBJID
#' @param stressors
#' @param PATH
#' @export
#
getExposureData <- function(INDIV_SUBJID, stressors, PATH) {
  message(paste0("Reading daily exposure data -- subject ID: ", INDIV_SUBJID, " // path: ", PATH))
  exposure.all <- NULL
  
  for (stressor in stressors) {
    message(stressor)
    daily.exposure <- read.csv(paste0(PATH, "\\ITR sample exposure_for Cara\\", stressor, "\\", stressor, "_exposure_sample_", INDIV_SUBJID, ".csv"))
    daily.exposure$STRESSOR <- stressor
    
    exposure.all <- rbind(exposure.all, daily.exposure)
  }

  # exposure.Chromium <- read.csv(paste0(PATH, "\\ITR sample exposure_for Cara\\Food intake\\Chromium\\Chromium_exposure_sample_", INDIV_SUBJID, ".csv"))
  # exposure.Chromium$STRESSOR <- "Chromium"
  # names(exposure.Chromium)
  # exposure.Chromium$income <- NA
  # exposure.Chromium$empstat <- -1000
  # exposure.Chromium$occup <- NA
  # exposure.Chromium$type <- NA
  # exposure.Chromium$comment <- NA
  # exposure.Chromium$retired <- -1000
  # exposure.Chromium$emp <- -1000
  # exposure.Chromium$student <- -1000
 
  emp.map <- readxl::read_excel(paste0(PATH, "\\LET\\employment_mapping.xlsx"), sheet = "employment_mapping")
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
  
  #sort(unique(exposure.all[ exposure.all$sample_ID == INDIV_SUBJID, ]$age))
  return( exposure.all )
}
