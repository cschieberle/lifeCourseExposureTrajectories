library(plyr)
library(triangle)
library(parallel)

#' Prallel estimation of life-long exposure of the given individual.
#'
#' @param i Index of individual.
#' @export
#
par_lifeCourseExposure <- function(i) {
  path.out <- config[["PATH_OUTPUT"]]
  if (config[["WRITE_OUTPUT"]]) {
    dir.create(file.path(path.out), showWarnings = FALSE)
  }
  
  
  message(individuals$id[i])
  
  cl <- parallel::makeCluster(config[["NUM_CORES"]], config[["CLUSTER_TYPE"]])

  clusterEvalQ(cl, library(lifeCourseExposureTrajectories))
  clusterEvalQ(cl, library(readxl))
  clusterEvalQ(cl, library(parallel))
  
  clusterExport(cl, "config")
  clusterExport(cl, "st")
  clusterExport(cl, "data.seq")
  clusterExport(cl, "individuals")

  #sim.results <- NULL
  
  INDIV_SUBJID <- individuals$id[i]
  INDIV_AGE <- individuals$age[i]
  INDIV_SEX <- ifelse(as.character(individuals$sex[i]) == "M", 1, 2)
  
  INDIV_EDU <- individuals$edulevel[i]
  
  econ.stat <- subset(
    edu.data, 
    (ESTIMATED.AGE >= INDIV_AGE-1 & ESTIMATED.AGE <= INDIV_AGE+1) & EDULEVEL == INDIV_EDU & SEX == INDIV_SEX
  )$ECON.STATUS.CURR.SELFDEF
  INDIV_ACT <- data.scodes[ sample(econ.stat, 1) ]
  
  sim.results.grpd <- lifeCourseExposureTrajectories::determineFullTraj(INDIV_SUBJID, INDIV_AGE, INDIV_SEX, INDIV_EDU, INDIV_ACT)
  
  stopifnot( length(unique(sim.results.grpd$INDIV_SUBJID)) == 1 )
  
  if (config[["WRITE_OUTPUT"]]) {
    write.csv(sim.results.grpd, file = paste0(path.out, "/", INDIV_SUBJID, "-lifetraj.csv"), row.names=F)
  }
  message("Daily exposures ...")
  
  exposure.all <- lifeCourseExposureTrajectories::getExposureData(
    INDIV_SUBJID, 
    stressors = config[["stressors"]]
  )
  stopifnot( length(unique(exposure.all$sample_ID)) == 1 )
  
  # impute based on 'surrounding' age for same individual if data for specific age is missing
  
  sim.lifetraj <- sim.results.grpd
  
  parallel.gapfill.exposure <- function(stressor, INDIV_SUBJID, INDIV_AGE) {
    exposure.add <- data.frame()
    
    for (t in c(1:nrow(sim.lifetraj))) {
      
      sim.age <- sim.lifetraj[ t, ]$AGE
      
      if (sim.age <= INDIV_AGE) {
        sim.emp.scode  <- sim.lifetraj[ t, ]$EMP.scode
        
        exposure.subset <- data.frame()
        age.diff <- 0
        while (nrow(exposure.subset) <= 0 & age.diff < 10) {
          exposure.subset <- subset(
            exposure.all, 
            sample_ID == INDIV_SUBJID & 
              (age >= sim.age - age.diff & age <= sim.age + age.diff) &
              EMP.scode == sim.emp.scode &
              STRESSOR == stressor
          )
          if (age.diff > 0 & nrow(exposure.subset) > 0) {
            exposure.subset$type <- 3
            exposure.subset$comment <- paste0("based on different age (original age=", exposure.subset$age[1], ")")
            exposure.subset$age <- sim.age
            exposure.add <- rbind(exposure.subset, exposure.add) 
          }
          age.diff <- age.diff + 1
        }
      }
      
    }
    return( exposure.add )
  }
  
  exposure.temp <- parLapply(
    cl,
    unique(exposure.all$STRESSOR), 
    parallel.gapfill.exposure, INDIV_SUBJID=INDIV_SUBJID, INDIV_AGE=INDIV_AGE
  )
  t <- do.call(rbind.data.frame, exposure.temp)
  exposure.all <- rbind(exposure.all, t)
  stopifnot( length(unique(exposure.all$sample_ID)) == 1 )
  
  sim.exposure.all <- merge(
    x = sim.results.grpd,
    y = exposure.all,
    by.x = c("INDIV_SUBJID", "AGE", "EMP.scode"),
    by.y = c("sample_ID", "age", "EMP.scode")
  )
  stopifnot( length(unique(sim.exposure.all$INDIV_SUBJID)) == 1 )

  # determine relative weight of exposure estimate per (age, EMP.scode)-combination based on
  # the number of original diaries that were used for estimation
  #
  # equal weight could be reached by replacing the 'aggregate' statement below by
  #   y = count(sim.exposure.PM25, vars = c("INDIV_SUBJID", "AGE", "EMP.scode"))
  #
  sim.exposure.all <- merge(
    x = sim.exposure.all,
    y = aggregate(count ~ INDIV_SUBJID + STRESSOR + AGE + EMP.scode, data=sim.exposure.all[,c("INDIV_SUBJID", "STRESSOR", "AGE", "EMP.scode", "count")], sum),
    by = c("INDIV_SUBJID", "STRESSOR", "AGE", "EMP.scode"),
    suffixes = c(".per.emp", ".all")
  )
  sim.exposure.all$EMP.map.prob <- sim.exposure.all$count.per.emp / sim.exposure.all$count.all
  
  # determine total probability per row ...
  sim.exposure.all$total.prob <- sim.exposure.all$EMP.probability * sim.exposure.all$EMP.map.prob
  
  if (config[["WRITE_OUTPUT"]]) {
    write.csv(sim.exposure.all, file = paste0(path.out, "/", INDIV_SUBJID, "-exposure-map.csv"), row.names=F)
  }
  
  # ... and sample accordingly
  d <- unique(sim.exposure.all[ c("INDIV_SUBJID", "STRESSOR", "AGE") ])
  d <- d[ with(d, order(STRESSOR, -AGE)), ]
  rownames(d) <- NULL
  
  d$EXP_2.5PCT <- -1
  d$EXP_25PCT <- -1
  d$EXP_MEDIAN <- -1
  d$EXP_75PCT <- -1
  d$EXP_97.5PCT <- -1
  d$EXP_MEAN <- -1
  d$EXP_SD <- -1
  
  sample.exp.all <- data.frame(INDIV_SUBJID = NULL, STRESSOR = NULL, value = NULL, age = NULL)
  
  parallel.sample.exposure <- function(j) {
    subjid   <- d[ j, ]$INDIV_SUBJID
    stressor <- d[ j, ]$STRESSOR
    age      <- d[ j, ]$AGE
    
    sim.exposure.subset <- subset(sim.exposure.all, INDIV_SUBJID == subjid & STRESSOR == stressor & AGE == age)
    
    sample.rownames <- sample(
      size = config[["SAMPLE_SIZE"]],
      x = rownames(sim.exposure.subset),
      prob = sim.exposure.subset$total.prob,
      replace = T
    )
    
    sample.exp <- data.frame(value = NULL)
    parallel.sample.exposure.inner <- function(r) {
      return(
        data.frame(
          triangle::rtriangle(
            n = config[["SAMPLE_SIZE"]],
            a = min(sim.exposure.subset[ r, ]$X2.5._percentile, sim.exposure.subset[ r, ]$mean),
            b = max(sim.exposure.subset[ r, ]$mean, sim.exposure.subset[ r, ]$X97.5._percentile),
            c = median(c(sim.exposure.subset[ r, ]$X2.5._percentile, sim.exposure.subset[ r, ]$mean, sim.exposure.subset[ r, ]$X97.5._percentile))
          )
        )
      )
    }
    sample.exp.temp <- mclapply(
      sample.rownames,
      FUN=parallel.sample.exposure.inner
    )
    sample.exp <- do.call(rbind.data.frame, sample.exp.temp)
    names(sample.exp) <- c("value")
    
    d[ j, c("EXP_2.5PCT", "EXP_25PCT", "EXP_MEDIAN", "EXP_75PCT", "EXP_97.5PCT", "EXP_MEAN", "EXP_SD") ] <- c(quantile(sample.exp$value, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)), mean(sample.exp$value), sd(sample.exp$value))
    
    return(
      data.frame(
        INDIV_SUBJID = subjid, 
        STRESSOR = stressor, 
        value = sample.exp, 
        age = age
      )
    )
  }
  
  clusterEvalQ(cl, library(triangle))
  clusterEvalQ(cl, library(parallel))

  sample.exp.t <- parLapply(
    cl,
    c(1:nrow(d)),
    parallel.sample.exposure
  )
  t <- do.call(rbind.data.frame, sample.exp.t)
  sample.exp.all <- rbind(sample.exp.all, t)
  
  if (config[["WRITE_OUTPUT"]]) {
    write.csv(sample.exp.all, file = paste0(path.out, "/", INDIV_SUBJID, "-exposure-samples.csv"), row.names=F)
  }
  
  # name critical life stages ...
  sample.exp.all$CRITICAL_LIFE_STAGE <- ""
  
  if (nrow(sample.exp.all[ sample.exp.all$age <= 3, ]) > 0) {
    sample.exp.all[ sample.exp.all$age <= 3, ]$CRITICAL_LIFE_STAGE <- "Infancy (1-3)"
  }
  
  if (nrow(sample.exp.all[ sample.exp.all$age > 3 & sample.exp.all$age <= 11, ]) > 0) {
    sample.exp.all[ sample.exp.all$age > 3 & sample.exp.all$age <= 11, ]$CRITICAL_LIFE_STAGE <- "Childhood (4-11)"
  }
  
  if (nrow(sample.exp.all[ sample.exp.all$age > 11 & sample.exp.all$age <= 17, ]) > 0) {
    sample.exp.all[ sample.exp.all$age > 11 & sample.exp.all$age <= 17, ]$CRITICAL_LIFE_STAGE <- "Adolescence (12-17)"
  }
  
  if (nrow(sample.exp.all[ sample.exp.all$age > 17 & sample.exp.all$age <= 39, ]) > 0) {
    sample.exp.all[ sample.exp.all$age > 17 & sample.exp.all$age <= 39, ]$CRITICAL_LIFE_STAGE <- "Adulthood before 40 (18-39)"
  }
  
  if (nrow(sample.exp.all[ sample.exp.all$age > 39 & sample.exp.all$age <= 64, ]) > 0) {
    sample.exp.all[ sample.exp.all$age > 39 & sample.exp.all$age <= 64, ]$CRITICAL_LIFE_STAGE <- "Adulthood before 65 (40-64)"
  }
  
  if (nrow(sample.exp.all[ sample.exp.all$age > 64, ]) > 0) {
    sample.exp.all[ sample.exp.all$age > 64, ]$CRITICAL_LIFE_STAGE <- "Adulthood 65 and older (>=65)"
  }
  
  stopCluster(cl)
  
  # ... and generate aggregate stats
  sample.exp.stats <- aggregate(value ~ INDIV_SUBJID + STRESSOR + CRITICAL_LIFE_STAGE, data=sample.exp.all, FUN=quantile, probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
  sample.exp.stats <- cbind(sample.exp.stats[, 1:3 ], data.frame(sample.exp.stats$value))
  
  sample.exp.mean <- aggregate(value ~ INDIV_SUBJID + STRESSOR + CRITICAL_LIFE_STAGE, data=sample.exp.all, FUN=mean)
  names(sample.exp.mean$value) <- "value.MEAN"
  
  sample.exp.sd <- aggregate(value ~ INDIV_SUBJID + STRESSOR + CRITICAL_LIFE_STAGE, data=sample.exp.all, FUN=sd)
  names(sample.exp.sd$value) <- "value.SD"
  
  sample.exp.stats <- merge(
    sample.exp.stats,
    sample.exp.mean,
    by = c("INDIV_SUBJID", "STRESSOR", "CRITICAL_LIFE_STAGE")
  )
  sample.exp.stats <- merge(
    sample.exp.stats,
    sample.exp.sd,
    by = c("INDIV_SUBJID", "STRESSOR", "CRITICAL_LIFE_STAGE")
  )
  
  names(sample.exp.stats) <- c("INDIV_SUBJID", "STRESSOR", "CRITICAL_LIFE_STAGE", "value.2.5PCT", "value.25PCT", "value.50PCT", "value.75PCT", "value.97.5PCT", "value.MEAN", "value.SD")
  if (config[["WRITE_OUTPUT"]]) {
    write.csv(sample.exp.stats, file = paste0(path.out, "/", INDIV_SUBJID, "-exposure-stats.csv"), row.names=F)
  }
  
  return( sample.exp.stats )
}