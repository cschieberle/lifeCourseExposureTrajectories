#' @export
data.alphabet <- c(
  seq(1, 11), 
  adolescModelAlphabet()
)

#' @export
data.labels <- c(
  "Employee working full-time",
  "Employee working part-time",
  "Self-employed working full-time (including family worker)",
  "Self-employed working part-time (including family worker)",
  "Unemployed",
  "Pupil, student, further training, unpaid work experience",
  "In retirement or in early retirement or has given up business",
  "Permanently disabled or/and unfit to work",
  "In compulsory military community or service",
  "Fulfilling domestic tasks and care responsibilities",
  "Other inactive person",
  adolescModelLabels()
)

#' @export
data.scodes <- c(
  "EWFT",  "EWPT",  "SEFT",  "SEPT",  "UNEM",  "STUD",  "RETD",  "UNFT",  "CMCS",  "DOME",  "INAC", 
  adolescModelCodes()
)


#' Simulates life trajectories
#'
#' @param df df
#' @param st st
#' @param indiv.age indiv.age
#' @param indiv.sex indiv.sex
#' @param indiv.activity indiv.activity
#' @param AGE.STEP AGE.STEP
#' @export
#
simulate <- function(df, st, indiv.age, indiv.sex, indiv.activity, AGE.STEP = 3) {
  
  traj <- data.frame()
  
  message(paste("simulation: [age = ", indiv.age, ", sex = ", indiv.sex, ", activity = ", indiv.activity, "]" ))
  
  # store original information and first
  # (i) perform simulation of RETROSPECTIVE (i.e. past) life course
  orig.age <- indiv.age
  orig.activity <- indiv.activity
  
  while (indiv.age <= 82) {
    node.id <- getNodeId(df, indiv.age, indiv.sex)
    node <- findNodeById(st$root, node.id)
    
    # pick only the sequences that are in the node, and...
    potential.seq <- data.frame(data.seq[ node$info$ind, ])
    # ..pick only the ones that START with the current main activity
    potential.seq <- subset(potential.seq, potential.seq$ECON.STATUS.CURR.SELFDEF.0 == indiv.activity)
    
    act.seq.in.node <- NULL
    # if at least one valid sequence exists:
    nrow.potential.seq <- nrow(potential.seq)
    if (nrow.potential.seq >= 1) {
      # very important to reset row.names as we like to sample only from the subset!
      row.names(potential.seq) <- seq(1:nrow(potential.seq))
      
      sample.seq.idx <- sample.int(nrow.potential.seq, 1)
      act.seq.in.node <- potential.seq[ sample.seq.idx, ]
    } else {
      # if no sequence starting with the given activity exists, 
      # just picky any sequence within the node
      potential.seq <- data.frame(data.seq[ node$info$ind, ])
      row.names(potential.seq) <- seq(1:nrow(potential.seq))
      nrow.potential.seq <- nrow(potential.seq)
      
      sample.seq.idx <- sample.int(nrow.potential.seq, 1)
      act.seq.in.node <- potential.seq[ sample.seq.idx, ]
    }
    
    traj <- rbind(
      traj, 
      c(
        indiv.age, 
        indiv.sex, 
        node.id, 
        nrow(potential.seq), 
        act.seq.in.node$ECON.STATUS.CURR.SELFDEF.0, 
        act.seq.in.node$ECON.STATUS.CURR.SELFDEF.1, 
        act.seq.in.node$ECON.STATUS.CURR.SELFDEF.2 
      )
    )
    #message(
    #  paste(
    #    "age:", indiv.age,
    #    ", sex: ", indiv.sex,
    #    " [ node.id:", node.id, ", numseq: ", nrow(potential.seq), "]: ",
    #    act.seq.in.node$ECON.STATUS.CURR.SELFDEF.0, 
    #    act.seq.in.node$ECON.STATUS.CURR.SELFDEF.1,
    #    act.seq.in.node$ECON.STATUS.CURR.SELFDEF.2,
    #    "(", act.seq.in.node$ECON.STATUS.CURR.SELFDEF.3, ")"
    #  )
    #)
    indiv.age <- indiv.age + AGE.STEP
    
    # set activity to the LAST activity in the 4-year sequence (as we continue the PROSPECTIVE analysis)
    indiv.activity <- act.seq.in.node$ECON.STATUS.CURR.SELFDEF.3
  }
  
  # restore original information and secondly
  # (ii) perform simulation of PROSPECTIVE (i.e. future) life course
  indiv.age <- orig.age
  indiv.activity <- orig.activity
  
  while ((indiv.age - AGE.STEP) >= 16) {
    indiv.age <- indiv.age - AGE.STEP
    
    node.id <- getNodeId(df, indiv.age, indiv.sex)
    node <- findNodeById(st$root, node.id)
    
    # pick only the sequences that are in the node, and...
    potential.seq <- data.frame(data.seq[ node$info$ind, ])
    # ..pick only the ones that END with the current main activity
    potential.seq <- subset(potential.seq, potential.seq$ECON.STATUS.CURR.SELFDEF.3 == indiv.activity)
    
    act.seq.in.node <- NULL
    # if at least one valid sequence exists:
    nrow.potential.seq <- nrow(potential.seq)
    if (nrow.potential.seq >= 1) {
      # very important to reset row.names as we like to sample only from the subset!
      row.names(potential.seq) <- seq(1:nrow(potential.seq))
      
      sample.seq.idx <- sample.int(nrow.potential.seq, 1)
      act.seq.in.node <- potential.seq[ sample.seq.idx, ]
    } else {
      # if no sequence starting with the given activity exists, 
      # just picky any sequence within the node
      potential.seq <- data.frame(data.seq[ node$info$ind, ])
      row.names(potential.seq) <- seq(1:nrow(potential.seq))
      nrow.potential.seq <- nrow(potential.seq)
      
      sample.seq.idx <- sample.int(nrow.potential.seq, 1)
      act.seq.in.node <- potential.seq[ sample.seq.idx, ]
    }
    
    traj <- rbind(
      traj, 
      c(
        indiv.age, 
        indiv.sex, 
        node.id, 
        nrow(potential.seq), 
        act.seq.in.node$ECON.STATUS.CURR.SELFDEF.1, 
        act.seq.in.node$ECON.STATUS.CURR.SELFDEF.2, 
        act.seq.in.node$ECON.STATUS.CURR.SELFDEF.3
      )
    )
    
    #message(
    #  paste(
    #    "age:", indiv.age,
    #    ", sex: ", indiv.sex,
    #    " [ node.id:", node.id, ", numseq: ", nrow(potential.seq), "]: ",
    #    "(", act.seq.in.node$ECON.STATUS.CURR.SELFDEF.0, ")", 
    #    act.seq.in.node$ECON.STATUS.CURR.SELFDEF.1, 
    #    act.seq.in.node$ECON.STATUS.CURR.SELFDEF.2,
    #    act.seq.in.node$ECON.STATUS.CURR.SELFDEF.3
    #  )
    #)
    # set activity to the FIRST activity in the 4-year sequence (as we continue the RETROSPECTIVE analysis)
    if (indiv.age <= 16) {
      indiv.activity <- act.seq.in.node$ECON.STATUS.CURR.SELFDEF.1
    } else {
      indiv.activity <- act.seq.in.node$ECON.STATUS.CURR.SELFDEF.0
    }
  }
  
  names(traj) <- c("age", "sex", "node.id", "num.seq", "activity.0", "activity.1", "activity.2")
  traj <- traj[order(traj$age),]
  return(traj)
}
