outputParaPlots <- function(datatable,mdl=1,attemptText="a") {
  
  # axis text formatting
  axis_text_format <- theme(axis.title.x = element_text(family = "Palatino",size = 15,vjust=-0.5),
                            axis.title.y = element_text(family = "Palatino",size = 15,vjust=1),
                            axis.text.x = element_text(family = "Palatino",size=15),
                            panel.border = element_rect(color="black"))
  
  if(mdl==1) {
    
    pms.grp <- subset(datatable, param %in% c("a","t","z","v_C(desWorld)[0]","v_C(desWorld)[1]"))
    pms.grp$param <- gsub("_C(desWorld)","",pms.grp$param,fixed=TRUE)
    pms.grp$param <- gsub("0","undes",pms.grp$param,fixed=TRUE)
    pms.grp$param <- gsub("1","des",pms.grp$param,fixed=TRUE)
    pms.grp$param <- factor(pms.grp$param)
    pms.grp[pms.grp$param=="v[undes]",c(2,4,5,6,7,8)] <- -pms.grp[pms.grp$param=="v[undes]",c(2,4,5,6,7,8)]
    pms.grp[,2:ncol(pms.grp)] <- numcolwise(abs)(pms.grp) # turning neg drift rates into positive.
    
    ### param plots
    
    # starting point
    z.p <- ggplot(pms.grp[pms.grp$param=="z",],aes(x=param,y=mean-0.5)) + 
      geom_bar(stat="identity",position=position_dodge(),color="black",fill="coral",size=0.75,width=0.4) +
      geom_errorbar(aes(ymin=mean-std-0.5,ymax=mean+std-0.5),
                    width=.2,size=0.75) +
      theme_bw() +
      theme(aspect.ratio=2) +
      axis_text_format +
      labs(x="",y="starting point (z)") +
      scale_y_continuous(labels=function(x){x+0.5},limits=c(-0.05,0.15)) +
      scale_x_discrete(breaks=NULL)
    
    # drift rate
    v.p <- ggplot(pms.grp[grep("v",pms.grp$param),], aes(x=param,y=mean)) +
      geom_bar(stat="identity", position=position_dodge(), color = "black",fill="cyan") +
      geom_errorbar(aes(ymin=mean-std, ymax=mean+std),
                    width=.2,position=position_dodge(0.9)) +
      theme_bw() + ylim(c(0,0.4)) +
      axis_text_format +
      scale_x_discrete(labels=c("Des.","Undes.")) +
      labs(x="Factory",y="drift rate (v)") +
      coord_fixed(16)
    
    # would also like posterior density plots.
    
  } else {
    # not currently plotting m04 plots
  }
  cairo_ps(paste0("zPlot_m0",num2str(mdl,0),attemptText,".eps"),family = "Palatino");print(z.p);dev.off()
  cairo_ps(paste0("vPlot_m0",num2str(mdl,0),attemptText,".eps"),family = "Palatino");print(v.p);dev.off()
  
  return(paramPlots <- list(z.p=z.p,v.p=v.p))
}

