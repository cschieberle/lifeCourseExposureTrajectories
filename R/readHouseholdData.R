#' Reads household data from cross-sectional EU-SILC data
#'
#' @param H.files.cs List of H-files.
#' @export
#
readHouseholdData <- function(H.files.cs) {
  cs.hh.data.all <- NULL
  for (H.file.cs in H.files.cs) {
    message("Loading H-file ", H.file.cs, appendLF=T)
    
    #cs.hh.data <- read.csv(H.file.cs)
	  library(data.table)
	  cs.hh.data <- fread(H.file.cs, header = T, sep = ',')
	
    cs.hh.data <- subset(cs.hh.data, cs.hh.data$HY020_F == 1)
    
    cs.hh.data.all <- rbind(cs.hh.data.all, cs.hh.data[,c("HB010", "HB020", "HB030", "HY010", "HY020")])
    rm(cs.hh.data)
  }
  cs.hh.data.all <- cs.hh.data.all[ !duplicated(cs.hh.data.all), ]
  names(cs.hh.data.all) <- c("YEAR.SURVEY", "COUNTRY", "HHID", "TOT.GROSS.HH.INCOME", "TOT.DISP.HH.INCOME")

  #cs.hh.data.all$YEAR.SURVEY <- factor(cs.hh.data.all$YEAR.SURVEY)
  #cs.hh.data.all$COUNTRY <- factor(cs.hh.data.all$COUNTRY)

  return(cs.hh.data.all)  
}

#' Reads household data from cross-sectional EU-SILC data
#'
#' @param H.files.cs List of H-files.
#' @export
#
readHouseholdData2 <- function(H.files.cs) {
  cs.hh.data.all <- NULL
  
  for (H.file.cs in H.files.cs) {
    message("Loading H-file ", H.file.cs, appendLF=T)
    
    cs.hh.data <- read.csv(H.file.cs)

    # remove data where missing
    #cs.hh.data <- subset(cs.hh.data, cs.hh.data$HY010_F == 1)
    # remove data where missing
    #cs.hh.data <- subset(cs.hh.data, cs.hh.data$HY020_F == 1)
    # remove data where missing
    #cs.hh.data <- subset(cs.hh.data, cs.hh.data$HH010_F == 1)
    # remove data where missing
    #cs.hh.data <- subset(cs.hh.data, cs.hh.data$HH070_F == 1)
    
    if (nrow(cs.hh.data) > 0) {
      if (!("HH020" %in% names(cs.hh.data))) {
        cs.hh.data$HH020 <- NA
      }
      if (!("HH021" %in% names(cs.hh.data))) {
        cs.hh.data$HH021 <- NA
      }
    }
    
    cs.hh.data.all <- rbind(cs.hh.data.all, cs.hh.data[,c("HB010", "HB020", "HB030", "HY010", "HY020", "HH010", "HH070", "HH020", "HH021")])

    rm(cs.hh.data)
  }
  cs.hh.data.all <- cs.hh.data.all[ !duplicated(cs.hh.data.all), ]
  names(cs.hh.data.all) <- c("YEAR.SURVEY", "COUNTRY", "HHID", "TOT.GROSS.HH.INCOME", "TOT.DISP.HH.INCOME", "DWELLING.TYPE", "TOT.HOUSING.COST", "TENURE.STATUS.HH020", "TENURE.STATUS.HH021")
  return(cs.hh.data.all)  
}

