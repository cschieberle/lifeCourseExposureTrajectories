
#' Simulation of the lifecourse trajectory of the given individual.
#'
#' @param sim.id
#' @param INDIV_AGE
#' @param INDIV_SEX
#' @param INDIV_ACT
#' @param INDIV_EDU
#' @export
#
par_lifeTrajSim <- function(sim.id, INDIV_AGE, INDIV_SEX, INDIV_ACT, INDIV_EDU) {
  NUM_SIM <- config[["NUM_SIM"]]
  
  life.traj <- simulate(
    df,
    st,
    indiv.age <- INDIV_AGE,
    indiv.sex <- INDIV_SEX,
    indiv.activity <- INDIV_ACT
  )
  life.traj$node.id <- NULL
  life.traj$num.seq <- NULL
  rownames(life.traj) <- NULL
  
  adolesc.age <- min(life.traj$age)
  sex <- as.integer(life.traj[ life.traj$age == adolesc.age, ]$sex)
  activity <- as.integer(life.traj[ life.traj$age == adolesc.age, ]$activity.0)
  activity.label <- data.scodes[ activity ]
  
  temp <- subset(edu.data, (ESTIMATED.AGE >= adolesc.age-1 & ESTIMATED.AGE <= adolesc.age) & SEX == sex)
  if (nrow(temp) < 1) {
    temp <- subset(edu.data, (ESTIMATED.AGE >= adolesc.age & ESTIMATED.AGE <= adolesc.age + 1) & SEX == sex)
  }
  schoolTypeAtAge16 <- as.data.frame(table(temp$CURRENT.EDU.TYPE.LABEL)/nrow(temp))
  
  adolesc.seq <- adolescSeq(schoolTypeAtAge16, n=NUM_SIM)
  adolesc.seq <- adolesc.seq[ sample(seq(1, nrow(adolesc.seq)), 1), ]
  for (j in seq(4,0)) {
    life.traj <- rbind(
      c(1 + (j * 3), sex, adolesc.seq[1 + (j * 3)], adolesc.seq[2 + (j * 3)], adolesc.seq[3 + (j * 3)]),
      life.traj
    )
  }
  
  rownames(life.traj) <- NULL
  life.traj$sex <- NULL
  
  sim.id.df <- rep(sim.id, nrow(life.traj)) 
  sim.results <- cbind(data.frame(sim.id.df), life.traj)
  sim.results.rshp <- reshape(sim.results, direction="wide", timevar="age", idvar="sim.id.df")
  names(sim.results.rshp)[2:ncol(sim.results.rshp)] <- paste0("Y", seq(1, ncol(sim.results.rshp)-1))
  return( sim.results.rshp )
}


#' Runs the trajectory simulation in parallel.
#'
#' @param INDIV_SUBJID
#' @param INDIV_AGE
#' @param INDIV_SEX
#' @param INDIV_EDU
#' @param INDIV_ACT
#' @export
#
determineFullTraj <- function(INDIV_SUBJID, INDIV_AGE, INDIV_SEX, INDIV_EDU, INDIV_ACT) {
  NUM_SIM <- config[["NUM_SIM"]]
  
  sim.results.rshp <- mclapply(
    seq(1:NUM_SIM),
    par_lifeTrajSim, INDIV_AGE = INDIV_AGE, INDIV_SEX = INDIV_SEX, INDIV_ACT = INDIV_ACT, INDIV_EDU = INDIV_EDU
  )
  sim.results.rshp <- do.call(rbind.data.frame, sim.results.rshp)
  
  sim.results.grpd <- NULL
  for (age.export in seq(from=81, to=1)) {
    life.traj.data <- as.data.frame(table(sim.results.rshp[ , age.export + 1 ]))
    names(life.traj.data) <- c("EMP.type" ,"EMP.probability")
    life.traj.data$EMP.probability <- life.traj.data$EMP.probability / NUM_SIM
    sim.results.grpd <- rbind(
      sim.results.grpd,
      cbind(
        INDIV_SUBJID,
        age.export,
        life.traj.data
      )
    )
  }
  names(sim.results.grpd)[1] <- "USUBJID"
  names(sim.results.grpd)[2] <- "AGE"
  
  emp.type.label <- cbind(as.data.frame(data.alphabet), as.data.frame(data.scodes), as.data.frame(data.labels))
  names(emp.type.label) <- c("EMP.alphabet", "EMP.scode", "EMP.label")
  
  sim.results.grpd <- merge(
    sim.results.grpd,
    emp.type.label,
    by.x = "EMP.type",
    by.y = "EMP.alphabet"
  )
  
  # reformat
  sim.results.grpd <- sim.results.grpd[ c("AGE", "EMP.probability", "EMP.scode") ]
  sim.results.grpd <- sim.results.grpd[ with(sim.results.grpd, order(-AGE, -EMP.probability)), ]
  
  sim.results.grpd <- cbind(
    data.frame(INDIV_SUBJID = rep(INDIV_SUBJID, nrow(sim.results.grpd))),
    sim.results.grpd
  )
  
  return( sim.results.grpd )
}
