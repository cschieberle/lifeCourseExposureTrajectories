#' Reads individual data from cross-sectional EU-SILC data (only age 16 and 17)
#'
#' @param P.files.cs List of P-files (should match R-files list)
#' @param P.files.cs List of R-files (should match P-files list)
#' @param cs.hh.data.all Optional household data that will be linked to the individual (household income etc.)
#' @export
#' @seealso \code{\link{readHouseholdData}}
#
readIndividualData <- function(P.files.cs, R.files.cs, cs.hh.data.all = NULL) {
  cs.data.all <- NULL
  files.cs <- data.frame(P.file = P.files.cs, R.file = R.files.cs)
  
  for (i in 1:nrow(files.cs)) {
    
    R.file.cs <- as.character(files.cs[i,]$R.file)
    message("Loading R-file ", R.file.cs, appendLF=T)

    #R.cs.data <- read.csv(R.file.cs)
	  library(data.table)
	  R.cs.data <- fread(R.file.cs, header = T, sep = ',', select=c("RB010", "RB020", "RB030", "RB080", "RB080_F", "RB090", "RB090_F", "RB220", "RB230", "RL010", "RL020", "RL030", "RL040", "RL050", "RL060"))

    # remove data where data on year of birth is missing
    R.cs.data <- subset(R.cs.data, R.cs.data$RB080_F == 1)
    # remove data where data on sex is missing
    R.cs.data <- subset(R.cs.data, R.cs.data$RB090_F == 1)
    
    R.cs.data <- R.cs.data[,c("RB010", "RB020", "RB030", "RB080", "RB090", "RB220", "RB230", "RL010", "RL020", "RL030", "RL040", "RL050", "RL060")]
    names(R.cs.data) <- c("YEAR.SURVEY", "COUNTRY", "PERSID", "YOB", "SEX", "FATHER.ID", "MOTHER.ID", "EDUHRS.PRE.SCHOOL", "EDUHRS.COMPULS", "CHLDCARE.CNTR.BASED", "CHLDCARE.DAYCARE", "CHLDCARE.PROFESSIONAL", "CHLDCARE.RELATIVES")
    
    R.cs.data$ESTIMATED.AGE <- R.cs.data$YEAR.SURVEY - R.cs.data$YOB
    R.cs.data$YOB <- NULL
    
    P.file.cs <- as.character(files.cs[i,]$P.file)
    message("Loading P-file ", P.file.cs, appendLF=T)
    
    #P.cs.data <- read.csv(P.file.cs) 
	  P.cs.data <- fread(P.file.cs, header = T, sep = ',', select=c("PB010", "PB020", "PB030", "PB140", "PB140_F", "PB150", "PB150_F", "PL031", "PL031_F", "PE020", "PE020_F", "PE040"))
		
    # remove data where data on year of birth is missing
    P.cs.data <- subset(P.cs.data, P.cs.data$PB140_F == 1)
    # remove data where data on sex is missing
    P.cs.data <- subset(P.cs.data, P.cs.data$PB150_F == 1)
    # select only those who provided type of education (also if they might not even be in education currently, i.e. PE020 == -2)
    P.cs.data <- subset(P.cs.data, P.cs.data$PE020_F != -1)
    # remove data where data on self-defined current economic status is missing
    P.cs.data <- subset(P.cs.data, P.cs.data$PL031_F == 1)
    
    # estimate age 
    P.cs.data$ESTIMATED.AGE <- as.integer(P.cs.data$PB010) - as.integer(P.cs.data$PB140)

    ########
    # P.cs.data <- subset(P.cs.data, P.cs.data$ESTIMATED.AGE >= 16 & P.cs.data$ESTIMATED.AGE < 18)
    ########
    
    #summary(P.cs.data$ESTIMATED.AGE)
    #summary(P.cs.data$PE020)
    #summary(P.cs.data$PL031)
    P.cs.data <- P.cs.data[,c("PB010", "PB020", "PB030", "PB150", "ESTIMATED.AGE", "PL031", "PE020", "PE040")]
    names(P.cs.data) <- c("YEAR.SURVEY", "COUNTRY", "PERSID", "SEX", "ESTIMATED.AGE", "ECON.STATUS.CURR.SELFDEF", "CURRENT.EDU.TYPE", "HIGHEST.ATTAINED.EDU")
    
    # P.cs.data$YEAR.SURVEY <- as.numeric(P.cs.data$YEAR.SURVEY)
    # P.cs.data$PERSID <- as.numeric(P.cs.data$PERSID)
    # P.cs.data$COUNTRY <- as.character(P.cs.data$COUNTRY)
    # 
    # R.cs.data$YEAR.SURVEY <- as.numeric(R.cs.data$YEAR.SURVEY)
    # R.cs.data$PERSID <- as.numeric(R.cs.data$PERSID)
    # R.cs.data$COUNTRY <- as.character(R.cs.data$COUNTRY)
    
    cs.data <- merge(
      P.cs.data,
      R.cs.data,
      by = c("YEAR.SURVEY", "COUNTRY", "PERSID"),
      all = T,
      suffixes = c(".P", ".R")
    )
    rm(P.cs.data)
    rm(R.cs.data)
    
    # determine age for individuals of age >= 16 (from P-file) and other houshold members like children of age < 16 (from R-file)
    cs.data$ESTIMATED.AGE <- cs.data$ESTIMATED.AGE.P
    if (nrow(cs.data[ is.na(cs.data$ESTIMATED.AGE), ]) > 0) {
      cs.data[ is.na(cs.data$ESTIMATED.AGE), ]$ESTIMATED.AGE <- cs.data[ is.na(cs.data$ESTIMATED.AGE), ]$ESTIMATED.AGE.R
    }
    cs.data$ESTIMATED.AGE.P <- NULL
    cs.data$ESTIMATED.AGE.R <- NULL
    
    # determine sex from P- or R-file
    cs.data$SEX <- cs.data$SEX.P
    if (nrow(cs.data[ is.na(cs.data$SEX), ]) > 0) {
      cs.data[ is.na(cs.data$SEX), ]$SEX <- cs.data[ is.na(cs.data$SEX), ]$SEX.R
    }
    cs.data$SEX.P <- NULL
    cs.data$SEX.R <- NULL
    
    cs.data.all <- rbind(cs.data.all, cs.data)
    rm(cs.data)
  }
  cs.data.all <- cs.data.all[ !duplicated(cs.data.all), ]
  
  cs.data.all$HHID <- as.integer(floor(cs.data.all$PERSID / 100))
  
  # join with household data if available
  if (!is.null(cs.hh.data.all)) {
    # cs.data.all$YEAR.SURVEY <- as.integer(cs.data.all$YEAR.SURVEY)
    # cs.data.all$HHID <- as.integer(cs.data.all$HHID)
    # cs.data.all$COUNTRY <- as.character(cs.data.all$COUNTRY)
    # 
    # cs.hh.data.all$YEAR.SURVEY <- as.integer(cs.hh.data.all$YEAR.SURVEY)
    # cs.hh.data.all$HHID <- as.integer(cs.hh.data.all$HHID)
    # cs.hh.data.all$COUNTRY <- as.character(cs.hh.data.all$COUNTRY)

    cs.data.all <- merge(
      x = cs.data.all,
      y = cs.hh.data.all,
      by = c("YEAR.SURVEY", "COUNTRY", "HHID")
    )
  }

  # cs.data.all$YEAR <- factor(cs.data.all$YEAR)
  # cs.data.all$COUNTRY <- factor(cs.data.all$COUNTRY)
  #cs.data.all$HHID <- factor(cs.data.all$HHID)
  cs.data.all$ECON.STATUS.CURR.SELFDEF <- factor(cs.data.all$ECON.STATUS.CURR.SELFDEF)
  
  # set current education type to a default value (e.g. -2 as in EU-SILC flag variable) if it is not given 
  # that is people that are currently not in education
  cs.data.all$CURRENT.EDU.TYPE[is.na(cs.data.all$CURRENT.EDU.TYPE)] <- -2
  cs.data.all$HIGHEST.ATTAINED.EDU[is.na(cs.data.all$HIGHEST.ATTAINED.EDU)] <- -2
  
  # replace current level of education with last attained if currently not in education or odd data (i.e. current < already attained)
  #cs.data.all[ cs.data.all$HIGHEST.ATTAINED.EDU > cs.data.all$CURRENT.EDU.TYPE, ]$CURRENT.EDU.TYPE <- 
  #  cs.data.all[ cs.data.all$HIGHEST.ATTAINED.EDU > cs.data.all$CURRENT.EDU.TYPE, ]$HIGHEST.ATTAINED.EDU

  # PE020: ISCED level currently attended
  #
  # 0 pre-primary education
  # 1 primary education
  # 2 lower secondary education
  # 3 (upper) secondary education
  # 4 post-secondary non tertiary education
  # 5 first stage of tertiary education (not leading directly to an advanced research qualification)
  # 6 second stage of tertiary education (leading to an advanced research qualification)
  #
  cs.data.all$CURRENT.EDU.TYPE.LABEL[ cs.data.all$CURRENT.EDU.TYPE %in% c(-2) ] <- "NONE"
  cs.data.all$CURRENT.EDU.TYPE.LABEL[ cs.data.all$CURRENT.EDU.TYPE %in% c(0) ] <- "PRE-PRIMARY"
  cs.data.all$CURRENT.EDU.TYPE.LABEL[ cs.data.all$CURRENT.EDU.TYPE %in% c(1) ] <- "PRIMARY"
  #cs.data.all$CURRENT.EDU.TYPE.LABEL[ cs.data.all$CURRENT.EDU.TYPE %in% c(2) ] <- "SECONDARY"
  cs.data.all$CURRENT.EDU.TYPE.LABEL[ cs.data.all$CURRENT.EDU.TYPE %in% c(2) ] <- "LWR-SECONDARY"
  #cs.data.all$CURRENT.EDU.TYPE.LABEL[ cs.data.all$CURRENT.EDU.TYPE %in% c(3,4) ] <- "SECONDARY"
  cs.data.all$CURRENT.EDU.TYPE.LABEL[ cs.data.all$CURRENT.EDU.TYPE %in% c(3,4) ] <- "UPR-SECONDARY"
  cs.data.all$CURRENT.EDU.TYPE.LABEL[ cs.data.all$CURRENT.EDU.TYPE %in% c(5,6) ] <- "TERTIARY"
  
  cs.data.all$HIGHEST.ATTAINED.EDU.LABEL[ cs.data.all$HIGHEST.ATTAINED.EDU %in% c(-2) ] <- "NONE"
  cs.data.all$HIGHEST.ATTAINED.EDU.LABEL[ cs.data.all$HIGHEST.ATTAINED.EDU %in% c(0) ] <- "PRE-PRIMARY"
  cs.data.all$HIGHEST.ATTAINED.EDU.LABEL[ cs.data.all$HIGHEST.ATTAINED.EDU %in% c(1) ] <- "PRIMARY"
  #cs.data.all$HIGHEST.ATTAINED.EDU.LABEL[ cs.data.all$HIGHEST.ATTAINED.EDU %in% c(2) ] <- "SECONDARY"
  cs.data.all$HIGHEST.ATTAINED.EDU.LABEL[ cs.data.all$HIGHEST.ATTAINED.EDU %in% c(2) ] <- "LWR-SECONDARY"
  #cs.data.all$HIGHEST.ATTAINED.EDU.LABEL[ cs.data.all$HIGHEST.ATTAINED.EDU %in% c(3,4) ] <- "SECONDARY"
  cs.data.all$HIGHEST.ATTAINED.EDU.LABEL[ cs.data.all$HIGHEST.ATTAINED.EDU %in% c(3,4) ] <- "UPR-SECONDARY"
  cs.data.all$HIGHEST.ATTAINED.EDU.LABEL[ cs.data.all$HIGHEST.ATTAINED.EDU %in% c(5,6) ] <- "TERTIARY"
  
  cs.data.all$CURRENT.EDU.TYPE <- factor(cs.data.all$CURRENT.EDU.TYPE)
  cs.data.all$HIGHEST.ATTAINED.EDU <- factor(cs.data.all$HIGHEST.ATTAINED.EDU)

  return(cs.data.all)
}

#' Reads individual data from cross-sectional EU-SILC data (only age 16 and 17)
#'
#' @param P.files.cs List of P-files
#' @param cs.hh.data.all Optional household data that will be linked to the individual (household income etc.)
#' @export
#' @seealso \code{\link{readHouseholdData}}
#
readIndividualData2 <- function(P.files.cs, cs.hh.data.all = NULL) {
  cs.data.all <- NULL
  
  stop("Deprecated function.")
  
  for (P.file.cs in P.files.cs) {
    message("Loading P-file ", P.file.cs, appendLF=T)
    
    cs.data <- read.csv(P.file.cs) 
    # remove data where data on year of birth is missing
    #cs.data <- subset(cs.data, cs.data$PB140_F == 1)
    # remove data where data on sex is missing
    #cs.data <- subset(cs.data, cs.data$PB150_F == 1)

    cs.data.all <- rbind(cs.data.all, cs.data[,c("PB010", "PB020", "PB030", "PB040", "PB140", "PB150", "PB180", "PB190")])
    rm(cs.data)
  }
  cs.data.all <- cs.data.all[ !duplicated(cs.data.all), ]
  names(cs.data.all) <- c("YEAR.SURVEY", "COUNTRY", "PERSID", "PERS.CS.WEIGHT", "YOB", "SEX", "SPOUSE.PARTNER.ID", "MARITAL.STATUS")
  cs.data.all$HHID <- floor(cs.data.all$PERSID / 100)
  
  # join with household data if available
  if (!is.null(cs.hh.data.all)) {
    cs.data.all <- merge(
      x = cs.data.all,
      y = cs.hh.data.all,
      by = c("YEAR.SURVEY", "COUNTRY", "HHID")
    )
  }
  
  return(cs.data.all)
}
