library(randomForest) 
library(caret)

ALPHABET_OFFSET <- 18

#' Creates the adolescence model from individual cross-secional data based current economic status and educational level.
#'
#' @param cs.data.all Cross-sectional individual data.
#' @export
#
adolescModel <- function(cs.data.all) {
  cs.data.for.rf <- cs.data.all[,c("COUNTRY", "SEX", "ECON.STATUS.CURR.SELFDEF", "TOT.DISP.HH.INCOME", "CURRENT.EDU.TYPE.LABEL")]
  
  cs.data.for.rf$TOT.DISP.HH.INCOME <-  round(cs.data.for.rf$TOT.DISP.HH.INCOME / 20000) * 20000
  cs.data.for.rf <- cs.data.for.rf[ !duplicated(cs.data.for.rf), ]
  
  #inTrain <- createDataPartition(y = as.factor(cs.data.for.rf$CURRENT.EDU.TYPE.LABEL), p=0.9, list=FALSE)
  #data.trainset <- cs.data.for.rf[inTrain,]
  #data.testset <- cs.data.for.rf[-inTrain,]
  
  names(cs.data.for.rf)
  
  cs.data.for.rf <- cs.data.for.rf[ !is.na(cs.data.for.rf$SEX), ]
  cs.data.for.rf <- cs.data.for.rf[ !is.na(cs.data.for.rf$ECON.STATUS.CURR.SELFDEF), ]
  cs.data.for.rf <- cs.data.for.rf[ !is.na(cs.data.for.rf$TOT.DISP.HH.INCOME), ]
  cs.data.for.rf <- cs.data.for.rf[ !is.na(cs.data.for.rf$CURRENT.EDU.TYPE.LABEL), ]

  typeof(cs.data.for.rf$ECON.STATUS.CURR.SELFDEF)
  
  # fit the randomforest model
  model <- randomForest(
    as.factor(CURRENT.EDU.TYPE.LABEL) ~ SEX + TOT.DISP.HH.INCOME + ECON.STATUS.CURR.SELFDEF,
    data = cs.data.for.rf,
    na.action = na.omit,
    importance = TRUE,
    keep.forest = TRUE
  )
  
  return(model)
}

#' Predicts school type.
#'
#' @param sex sex
#' @param activity activity
#' @param income income
#' @param model Adolescence model 
#' @seealso \code{\link{adolescModel}}
#' @export
#
predictSchoolTypeProb <- function(sex, activity, income, model) {
  school.type.prob <- data.frame(
    predict(
      model, 
      type = "prob", 
      newdata = data.frame(
        SEX = sex, 
        ECON.STATUS.CURR.SELFDEF = activity, 
        TOT.DISP.HH.INCOME = income)
   )
  )
  return(school.type.prob)
}

#' Labels of adolesc model.
#'
#' @export
#
adolescModelLabels <- function() {
  return(
    c(
      "Infancy", 
      "Pre-primary education (ISCED 0)",
      "Primary education or first stage of basic education (ISCED 1)",
      "Lower-secondary or second stage of basic education (ISCED 2)",
      "Upper-secondary education (ISCED 3)",
      "Post-secondary non-tertiary education (ISCED 4)",
      "Not in education / cancelled school"
    )
  )
}

#' Codes of adolesc model.
#'
#' @export
#
adolescModelCodes <- function() {
  return(c("INFT", "ISCED0", "ISCED1", "ISCED2", "ISCED3", "ISCED4", "NOEDU"))
}

#' Alphabet of adolesc model.
#'
#' @export
#
adolescModelAlphabet <- function() {
  return(ALPHABET_OFFSET + seq(1, length(adolescModelCodes())))
}

#' Predicts school type.
#'
#' @param schoolTypeAtAge16 School type probabilities at age 16
#' @param n Sample zize.
#' @seealso \code{\link{adolescModel}} \code{\link{predictSchoolTypeProb}}
#' @export
#
adolescSeq <- function(schoolTypeAtAge16, n) {
  adolesc.seq <- NULL
  for (i in seq(1,n)) {
    prob <- i / n
    
    INFANT.years <- sample(c(1,2,2,2,3), size=1)
    PRIMARY.start <- sample(c(5,6,6,6,7), size=1)
    PRE.PRIMARY.years <- PRIMARY.start - INFANT.years - 1
    PRIMARY.years <- sample(c(4,4,4,5), size=1)
    
    codes <- adolescModelCodes()
    
    seq <- NULL
    
    if (prob <= sum(schoolTypeAtAge16[ schoolTypeAtAge16$Var1 %in% c("NONE", "PRIMARY", "LWR-SECONDARY", "UPR-SECONDARY", "TERTIARY"), ]$Freq)) {
      seq <- c(
        rep(which(codes == "INFT") + ALPHABET_OFFSET, INFANT.years), 
        rep(which(codes == "ISCED0") + ALPHABET_OFFSET, PRE.PRIMARY.years), 
        rep(which(codes == "ISCED1") + ALPHABET_OFFSET, PRIMARY.years), 
        rep(which(codes == "ISCED2") + ALPHABET_OFFSET, 4), 
        rep(which(codes == "ISCED3") + ALPHABET_OFFSET, 15 - (INFANT.years + PRE.PRIMARY.years + PRIMARY.years + 4))
      )
    }
    if (prob <= sum(schoolTypeAtAge16[ schoolTypeAtAge16$Var1 %in% c("NONE", "PRIMARY", "LWR-SECONDARY"), ]$Freq)) {
      seq <- c(
        rep(which(codes == "INFT") + ALPHABET_OFFSET, INFANT.years), 
        rep(which(codes == "ISCED0") + ALPHABET_OFFSET, PRE.PRIMARY.years), 
        rep(which(codes == "ISCED1") + ALPHABET_OFFSET, PRIMARY.years), 
        rep(which(codes == "ISCED2") + ALPHABET_OFFSET, 15 - (INFANT.years + PRE.PRIMARY.years + PRIMARY.years))
      )
    }
    if (prob <= sum(schoolTypeAtAge16[ schoolTypeAtAge16$Var1 %in% c("NONE", "PRIMARY"), ]$Freq)) {
      PRIMARY.years <- PRIMARY.years + sample(c(0,0,0,1), size=1)
      seq <- c(
        rep(which(codes == "INFT") + ALPHABET_OFFSET, INFANT.years), 
        rep(which(codes == "ISCED0") + ALPHABET_OFFSET, PRE.PRIMARY.years), 
        rep(which(codes == "ISCED1") + ALPHABET_OFFSET, PRIMARY.years), 
        rep(which(codes == "NOEDU") + ALPHABET_OFFSET, 15 - (INFANT.years + PRE.PRIMARY.years + PRIMARY.years))
      )
    }
    
    adolesc.seq <- rbind(adolesc.seq, seq)
  }
  

  return(adolesc.seq)
}
