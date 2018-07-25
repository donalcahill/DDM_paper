sumsimdata <- function(simData,quantileCuts=c(0.1,0.3,0.5,0.7,0.9),desOrWhich ="des") {
  
  if(desOrWhich=="des") {
    
    # quantiles
    quantiles <- ddply(simData,.(desWorld,response,simNum),function(x) {
      quantile(x$rt,probs=quantileCuts)
    })
    quantiles <- melt(quantiles,id.vars=c("desWorld","response","simNum"),variable.name="quantile",value.name="rt")
    
    # prop resp. (NB. this is resp code, 1=des, 0=undes)
    propResp <- ddply(simData,.(desWorld,simNum),summarise,propResp = mean(response==1))
    
    simsum <- merge(quantiles,propResp,by=c("desWorld","simNum")) # should leave propResp x resp x RT
    simsum[simsum$response==0,"propResp"] <- 1 - simsum[simsum$response==0,"propResp"]
    
    # before the above transformation:
    # Wundes Rundes: ~0.3
    # Wundes Rdes: ~0.3
    # Wdes Rundes: ~0.7
    # Wdes Rdes: ~0.7
    
    # we want, acc code -> so switch on undes resp
    # Wundes Rundes: ~0.7
    # Wundes Rdes: ~0.3
    # Wdes Rundes: ~0.3
    # Wdes Rdes: ~0.7
    
    # if we wanted resp code -> switch on non-matches.
    # Wundes Rundes: ~0.3
    # Wundes Rdes: ~0.7
    # Wdes Rundes: ~0.3
    # Wdes Rdes: ~0.7
    
    # for ppc plot
    simsum$desWorld <- factor(simsum$desWorld,labels=c("Undesirable Factory","Desirable Factory"))
    simsum$empOrSim <- factor(ifelse(simsum$simNum=="empirical","empirical","simulation"))
    
  } else {
    
    # quantiles
    quantiles <- ddply(simData,.(whichWorld,response,simNum),function(x) {
      quantile(x$rt,probs=quantileCuts)
    })
    quantiles <- melt(quantiles,id.vars=c("whichWorld","response","simNum"),variable.name="quantile",value.name="rt")
    
    # prop resp. (NB. this is resp code, 1=des, 0=undes)
    propResp <- ddply(simData,.(whichWorld,simNum),summarise,propResp = mean(response==1))
    
    simsum <- merge(quantiles,propResp,by=c("whichWorld","simNum")) # should leave propResp x resp x RT
    simsum[simsum$response==0,"propResp"] <- 1 - simsum[simsum$response==0,"propResp"]
    
    # for ppc plot
    simsum$whichWorld <- factor(simsum$whichWorld,labels=c("Telephone Factory","Television Factory"))
    simsum$empOrSim <- factor(ifelse(simsum$simNum=="empirical","empirical","simulation"))
    
    }
  
  return(simsum)
  
}

