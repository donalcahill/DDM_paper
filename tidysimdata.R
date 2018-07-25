tidysimdata <- function(simData,empData,mdl=1) {
  
  if (mdl==1) {
    
    # tidying sim results
    simData$subj_idx <- as.numeric(gsub('wfpt.','',simData$node))
    simData <- simData[,c("X","X.1","rt_sampled","subj_idx")]
    names(simData)[1:3] <- c("simNum","idx","rt") 
    simData$response <- +(simData$rt > 0)
    simData$rt <- abs(simData$rt)
    
    # trial idxs per subj
    pptTrialIdx <- dlply(empData,.(subj_idx),function(x) return(x$trial))
    pptDesWorldIdx <- dlply(empData,.(subj_idx),function(x) return(x$desWorld))
    
    # sanity check
    #cbind(ddply(simData,.(subj_idx),nrow),
    #      ddply(empData,.(subj_idx),nrow))
    
    # adding trial numbers and outcome
    simData <- ddply(simData,.(subj_idx),here(transform),
                      trial = pptTrialIdx[[as.character(subj_idx[1])]],
                      desWorld=pptDesWorldIdx[[as.character(subj_idx[1])]])
    
    # merge empirical and sim_data
    empData$simNum <- "empirical"
    simData$idx <- NULL
    simData$simNum<- factor(simData$simNum)
    return(simData <- rbind(empData,simData))
    
  } else if (mdl==4) {
    
    # model 4 as whichWorld regressor
    # tidying sim results
    simData$subj_idx <- as.numeric(gsub('wfpt.','',simData$node))
    simData <- simData[,c("X","X.1","rt_sampled","subj_idx")]
    names(simData)[1:3] <- c("simNum","idx","rt") 
    simData$response <- +(simData$rt > 0)
    simData$rt <- abs(simData$rt)
    
    # trial idxs per subj
    pptTrialIdx <- dlply(empData,.(subj_idx),function(x) return(x$trial))
    pptWhichWorldIdx <- dlply(empData,.(subj_idx),function(x) return(x$whichWorld))
    
    # sanity check
    #cbind(ddply(simData,.(subj_idx),nrow),
    #      ddply(empData,.(subj_idx),nrow))
    
    # adding trial numbers and outcome
    simData <- ddply(simData,.(subj_idx),here(transform),
                     trial = pptTrialIdx[[as.character(subj_idx[1])]],
                     whichWorld=pptWhichWorldIdx[[as.character(subj_idx[1])]])
    
    # merge empirical and sim_data
    empData$simNum <- "empirical"
    simData$idx <- NULL
    simData$simNum<- factor(simData$simNum)
    return(simData <- rbind(empData,simData))
    
    
    # this is a lot simpler to tidy since it is not a regression model and the simulation output from hddm is much cleaner.
    
    # names(simData)[2] <- "simNum"
    # simData <- simData[,c("subj_idx","trial","desWorld","response_sampled","rt_sampled","simNum")]
    # names(simData)[names(simData)=="rt_sampled"] <- "rt"
    # names(simData)[names(simData)=="response_sampled"] <- "response"
    # simData$rt <- abs(simData$rt)
    # empData$simNum <- "empirical"
    # simData$simNum<- factor(simData$simNum)
    # return(simData <- rbind(empData,simData))
    
  }
}




