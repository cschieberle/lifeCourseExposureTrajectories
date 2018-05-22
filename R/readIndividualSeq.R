#' Reads individual 4-year sequences from longitudinal EU-SILC data
#'
#' @param P.files.lon List of P-files.
#' @param lon.hh.data.all Longitudinal household data.
#' @export
#
readIndividualSeq <- function(P.files.lon, lon.hh.data.all) {
  data.all <- NULL
  
  for (P.file in P.files.lon) {
    
    message("Loading P-file ", P.file, appendLF=T)
    
    #data <- read.csv(P.file)
	  library(data.table)
	  data <- fread(P.file, header = T, sep = ',')

    # remove data where data on year of birth is missing
    data <- subset(data, data$PB140_F == 1)
    # remove data where data on sex is missing
    data <- subset(data, data$PB150_F == 1)
    # remove data where data on year of the interview is missing (necessary for sequence calculation)
    data <- subset(data, data$PB110_F == 1)
    # remove data where data on marital status is missing
    data <- subset(data, data$PB190_F == 1)
    
    # remove data where data on self-defined economic status is missing
    if ("PL031" %in% names(data)) {
      data <- subset(data, data$PL031_F == 1)
    } else {
      data <- subset(data, data$PL030_F == 1)
      data <- subset(data, data$PL040_F == 1)
    }

    # version before 2009 do not contain self-defined economic status in PL031 but contain PL030 which does not
    # distinguish between self-employed and employed work. The distinction is given however in PL040.
    # The values 1-2 correspond to values 1-4 in newer versions.
    # The values 3-9 correspong to values 5-11 in newer versions.
    if (!("PL031" %in% names(data))) {
      data$PL031 <- apply(data[, c("PL030", "PL040")], 1, function(x) {
        if (x[1] > 2) {
          return(x[1] + 2)
        } else {
          if (x[2] == 3) {
            return(x[1])
          } else {
            return(x[1] + 2)
          }
        }
      })
    }

    data.1 <- data[ , c("PB020", "PB030" ,"PB140", "PB150", "PB190", "PB110", "PL031") ]
    names(data.1) <- c("CNTRY", "PERSID", "BIRTH.YEAR", "SEX", "MARITAL.STATUS", "INTRVW.YEAR", "ECON.STATUS.CURR.SELFDEF") 
    rm(data)
    
    data.1 <- subset(data.1, !is.na(data.1$BIRTH.YEAR))
    data.1 <- subset(data.1, !is.na(data.1$INTRVW.YEAR))
    data.1 <- subset(data.1, !is.na(data.1$ECON.STATUS.CURR.SELFDEF))
    data.1 <- subset(data.1, !is.na(data.1$MARITAL.STATUS))
    
    data.all <- rbind(data.all, data.1, fill=T)
    rownames(data.all) <- NULL
    data.all <- data.all[!duplicated(data.all), ]
    rm(data.1)
  }
  
  message("P-files loaded. Merging interviews.", appendLF=T)
  
  data.intrvw.year <-aggregate(data.all$INTRVW.YEAR, by=list(data.all$CNTRY, data.all$PERSID, data.all$BIRTH.YEAR, data.all$SEX), FUN=min, na.rm=TRUE)
  names(data.intrvw.year) <- c("CNTRY", "PERSID", "BIRTH.YEAR", "SEX", "MIN.INTRVW.YEAR")

  data.marital.status <-aggregate(data.all$MARITAL.STATUS, by=list(data.all$CNTRY, data.all$PERSID, data.all$BIRTH.YEAR, data.all$SEX), FUN=min, na.rm=TRUE)
  names(data.marital.status) <- c("CNTRY", "PERSID", "BIRTH.YEAR", "SEX", "MIN.MARITAL.STATUS")
  
  data.all <- merge(
    data.all, 
    data.intrvw.year, 
    by=c("CNTRY", "PERSID", "BIRTH.YEAR", "SEX")
  )
  rm(data.intrvw.year)

  data.all <- merge(
    data.all, 
    data.marital.status, 
    by=c("CNTRY", "PERSID", "BIRTH.YEAR", "SEX")
  )
  rm(data.marital.status)
  
  data.all$INTRVW.IDX <- data.all$INTRVW.YEAR - data.all$MIN.INTRVW.YEAR
  data.all$AGE.AT.FRST.INTRVW <- (data.all$INTRVW.YEAR - data.all$INTRVW.IDX) -  data.all$BIRTH.YEAR
  data.all$YEAR.AT.FRST.INTRVW <- (data.all$INTRVW.YEAR - data.all$INTRVW.IDX)
  data.all$MARITAL.STATUS <- data.all$MIN.MARITAL.STATUS
  
  data.all$BIRTH.YEAR <- NULL
  data.all$INTRVW.YEAR <- NULL
  data.all$MIN.INTRVW.YEAR <- NULL
  data.all$MIN.MARITAL.STATUS <- NULL

  # remove duplicates 
  rownames(data.all) <- NULL
  data.all <- data.all[ !duplicated(data.all), ]
  
  # only consider first 4 years of longitudinal study (IDX 0,...,3); some have IDX 0,...,4
  data.all <- data.all[ data.all$INTRVW.IDX < 4, ]
  
  # Affects less than 0.2% of the results. Better take the "newest" than the "maximum" values, maybe..
  data.all <- aggregate(. ~ CNTRY+PERSID+SEX+MARITAL.STATUS+INTRVW.IDX+AGE.AT.FRST.INTRVW+YEAR.AT.FRST.INTRVW, data = data.all, FUN = max)
  
  # reshape data  
  message("Reshaping data into sequences.", appendLF=T)
  
  data.rshp <- NULL
  countries.list <- intersect(unique(data.all$CNTRY), unique(lon.hh.data.all$COUNTRY))
  for (country.id in countries.list) {
    
    data.temp <- data.frame(data.all[ which(data.all$CNTRY == country.id), ]) 
    data.all <- data.frame(data.all[ -which(data.all$CNTRY == country.id), ])

    # remove duplicates
    #rownames(data.temp) <- NULL
    #data.temp <- data.temp[ !duplicated(data.temp), ]
    #rownames(data.all) <- NULL
    #data.all <- data.all[ !duplicated(data.all), ]

    message(paste("Reshaping for ", country.id))
    
    message(paste(Cstack_info(), "/"))
    
    #data.rshp.temp <- reshape(
    #  data.temp, 
     # idvar=c("CNTRY", "PERSID", "AGE.AT.FRST.INTRVW", "SEX", "YEAR.AT.FRST.INTRVW", "MARITAL.STATUS"), 
      #timevar="INTRVW.IDX", 
     # direction="wide"
    #)
    if (!is.null(data.temp)) {
      if (nrow(data.temp) > 0) {
        #data.temp$row <- 1:nrow(data.temp)
        
        data.rshp.temp <- tidyr::spread(data.temp, key=INTRVW.IDX, value=ECON.STATUS.CURR.SELFDEF, sep = ".")
        colnames(data.rshp.temp) <- gsub("INTRVW.IDX.", "ECON.STATUS.CURR.SELFDEF.", colnames(data.rshp.temp))
    
        message(paste(Cstack_info(), "/"))
    
        if (!is.null(data.rshp.temp)) {
          if (nrow(data.rshp.temp) > 0) {
            #data.rshp <- data.table::rbindlist(list(data.rshp, data.rshp.temp), use.names=T, fill=T)
            #data.rshp.temp$row <- NULL
            data.rshp <- rbind(data.rshp, data.rshp.temp)
          }
        }
      }
    }
  }
  
  rm(data.all)
  data.rshp$HHID <- floor(data.rshp$PERSID / 100)  
  #names(data.rshp)
  
  # propely order sequences
  col.ord.names <- c(names(data.rshp)[1:7], sort(names(data.rshp)[8:11]))
  col.reorder <- NULL
  for(name in col.ord.names) {
    col.reorder <- c(col.reorder, which(names(data.rshp) == name))
  }
  data.rshp <- data.rshp[ col.reorder ]

  data.rshp <- subset(data.rshp, !is.na(data.rshp$ECON.STATUS.CURR.SELFDEF.0))
  data.rshp <- subset(data.rshp, !is.na(data.rshp$ECON.STATUS.CURR.SELFDEF.1))
  data.rshp <- subset(data.rshp, !is.na(data.rshp$ECON.STATUS.CURR.SELFDEF.2))
  data.rshp <- subset(data.rshp, !is.na(data.rshp$ECON.STATUS.CURR.SELFDEF.3))
  
  result <- merge(
    x = data.rshp,
    y = lon.hh.data.all[, c("COUNTRY", "HHID", "YEAR.SURVEY", "TOT.DISP.HH.INCOME")],
    by.x = c("CNTRY", "HHID", "YEAR.AT.FRST.INTRVW"),
    by.y = c("COUNTRY", "HHID", "YEAR.SURVEY")
  )
  
  return(result)
}

# PL031: Self-defined current economic status
#
# 1 Employee working full-time
# 2 Employee working part-time
# 3 Self-employed working full-time (including family worker)
# 4 Self-employed working part-time (including family worker)
# 5 Unemployed
# 6 Pupil, student, further training, unpaid work experience
# 7 In retirement or in early retirement or has given up business
# 8 Permanently disabled or/and unfit to work
# 9 In compulsory military community or service
# 10 Fulfilling domestic tasks and care responsibilities
# 11 Other inactive person
#
