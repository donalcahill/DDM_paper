outputPostDensityPlots <- function(dataIn,expTag,mdl=1,attemptText="a") {
  
  uppYLim <- if(expTag=="mainExp") c(80,50) else c(50,25)
  
  z.p <- ggplot(dataIn,aes(x=z)) + 
    geom_vline(xintercept=0.5,linetype="dashed",size=1,color="red") +
    geom_line(stat="density",color="black",size=1) +
    theme(text=element_text(family="Palatino")) + 
    xlim(c(0.49,0.55)) +
    #scale_y_continuous(expand = c(0,0,0,10)) +
    ylim(c(0,uppYLim[1])) +
    labs(x="Starting point (z)",y="Posterior Probability")
  
  vTraces <- melt(dataIn[,c("v1","v0")],variable.name = "param")
  v.p <- ggplot(vTraces,aes(x=value,color=param)) + 
    geom_line(stat="density",size=1) +
    theme(text=element_text(family="Palatino"),
          legend.position = c(1,1),
          legend.justification = c(1,1)) + 
    ylim(c(0,uppYLim[2])) +
    labs(x="Drift Rate",y="Posterior Probability") +
    scale_color_manual(values=c("forestgreen","firebrick"),
                       name="Factory",
                       labels=c("Desirable","Undesirable"))
  
  bothPlots <- plot_grid(z.p,v.p,labels=c("A","B"))
  save_plot(paste0("paramDensityPlots_",expTag,"_m0",num2str(mdl,0),attemptText,".pdf"),bothPlots, device=cairo_pdf,ncol=2,base_aspect_ratio = 0.75)
  
  return(densityPlots <- list(z.p=z.p,v.p=v.p,bothPlots=bothPlots))  
  
}
