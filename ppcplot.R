ppcplot <- function(plotData,mdl=1,attemptText="a",desOrWhich="des") {
  
  if (desOrWhich=="des") {
    
    ppc.p <- ggplot(plotData,aes(x=propResp,y=rt,group=quantile)) + geom_point(aes(color=empOrSim,shape=empOrSim,size=empOrSim)) +
      facet_grid(. ~ desWorld) + xlim(c(0,1)) + theme_bw() + labs(x="prop. correct response",y="RT (secs)") +
      scale_color_manual(values=c("black","grey")) +
      scale_shape_manual(values=c(4,20)) + 
      scale_size_manual(values=c(5,0.25)) + 
      theme(legend.title = element_blank())
    
  } else {
    
    ppc.p <- ggplot(plotData,aes(x=propResp,y=rt,group=quantile)) + geom_point(aes(color=empOrSim,shape=empOrSim,size=empOrSim)) +
      facet_grid(. ~ whichWorld) + xlim(c(0,1)) + theme_bw() + labs(x="prop. correct response",y="RT (secs)") +
      scale_color_manual(values=c("black","grey")) +
      scale_shape_manual(values=c(4,20)) + 
      scale_size_manual(values=c(5,0.25))
    
  }
  
  
  cairo_ps(paste0("ppcPlot_m0",num2str(mdl,0),attemptText,".eps"),family = "Palatino");print(ppc.p);dev.off()
  return(ppc.p)
  
}



