
#' Determine cross-sectional education data in terms of highest attained educational level and 
#' current education type.
#'
#' @param cs.data.all Cross-sectional education data including at least these variables: 
#' ECON.STATUS.CURR.SELFDEF, CURRENT.EDU.TYPE.LABEL, HIGHEST.ATTAINED.EDU.LABEL, ESTIMATED.AGE, SEX, and TOT.DISP.HH.INCOME.
#' @param country.filter Only return data related to the given countries. By default all data are returned.
#' @export
#
getCrossSectEdu <- function(cs.data.all, country.filter = NULL) {
  if (!is.null(country.filter)) {
    cs.data.all <- cs.data.all[ cs.data.all$COUNTRY %in% country.filter, ]    
  }
  
  cs.data.all <- cs.data.all[, c("ECON.STATUS.CURR.SELFDEF", "CURRENT.EDU.TYPE.LABEL", "HIGHEST.ATTAINED.EDU.LABEL", "ESTIMATED.AGE", "SEX", "TOT.DISP.HH.INCOME")]

  cs.data.all$EDULEVEL <- -1
  cs.data.all[ cs.data.all$HIGHEST.ATTAINED.EDU.LABEL %in% c("NONE", "PRE-PRIMARY", "PRIMARY"), ]$EDULEVEL <- 0
  cs.data.all[ cs.data.all$CURRENT.EDU.TYPE.LABEL %in% c("NONE", "PRE-PRIMARY", "PRIMARY"), ]$EDULEVEL <- 0
  
  cs.data.all[ cs.data.all$HIGHEST.ATTAINED.EDU.LABEL %in% c("LWR-SECONDARY", "UPR-SECONDARY"), ]$EDULEVEL <- 1
  cs.data.all[ cs.data.all$CURRENT.EDU.TYPE.LABEL %in% c("LWR-SECONDARY", "UPR-SECONDARY"), ]$EDULEVEL <- 1
  
  cs.data.all[ cs.data.all$HIGHEST.ATTAINED.EDU.LABEL %in% c("TERTIARY"), ]$EDULEVEL <- 2
  cs.data.all[ cs.data.all$CURRENT.EDU.TYPE.LABEL %in% c("TERTIARY"), ]$EDULEVEL <- 2
  
  cs.data.all$HIGHEST.ATTAINED.EDU.LABEL <- NULL
  
  cs.data.all <- cs.data.all[ !is.na(cs.data.all$ECON.STATUS.CURR.SELFDEF), ]
  
  cs.data.all <- droplevels(cs.data.all)
  
  return(cs.data.all)
}