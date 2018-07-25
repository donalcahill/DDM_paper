# plotting HDDM results for DDM paper.

rm(list=ls())
setwd("")
source("supplementary_functions.R")
source("amalgamateParamTraces.R")
source("outputPostDensityPlots.R")
source("tidysimdata.R")
source("sumsimdata.R")
source("ppcplot.R")

# pilotExp
pilotExp_empData <- read.csv("pilotExp_forHDDM_desCode.csv",header=TRUE)
pilotExp_simData <- read.csv("pilotExp_m01a_ppc.csv",header=TRUE)

pilotExp_paramData <- amalgamateParamTraces("pilotExp_m01a")
pilotExp_paramPlots <- outputPostDensityPlots(pilotExp_paramData,"pilotExp")

pilotExp_simData <- tidysimdata(pilotExp_simData,pilotExp_empData)
pilotExp_simData_sum <- sumsimdata(pilotExp_simData)
pilotExp_ppcPlot <- ppcplot(pilotExp_simData_sum)

pilotExp_allPlots <- plot_grid(pilotExp_paramPlots$bothPlots,pilotExp_ppcPlot,labels=c("","C"),ncol=1) #,rel_widths = c(1,1.3),scale=0.9)
save_plot("pilotExp_allDDMPlots.pdf",pilotExp_allPlots, device=cairo_pdf,ncol=1,nrow=2,base_aspect_ratio = 2)


# e13c
mainExp_empData <- read.csv("mainExp_forHDDM_desCode.csv",header=TRUE)
mainExp_simData <- read.csv("mainExp_m01a_ppc.csv",header=TRUE)

mainExp_paramData <- amalgamateParamTraces("mainExp_m01a")
mainExp_paramPlots <- outputPostDensityPlots(mainExp_paramData,"mainExp")

mainExp_simData <- tidysimdata(mainExp_simData,mainExp_empData)
mainExp_simData_sum <- sumsimdata(mainExp_simData)
mainExp_ppcPlot <- ppcplot(mainExp_simData_sum)

mainExp_allPlots <- plot_grid(mainExp_paramPlots$bothPlots,mainExp_ppcPlot,labels=c("","C"),ncol=1) #,rel_widths = c(1,1.3),scale=0.9)
save_plot("mainExp_allDDMPlots.pdf",mainExp_allPlots, device=cairo_pdf,ncol=1,nrow=2,base_aspect_ratio = 2)


# redoing for Tali, July 2018

pilotExp_simData_sum$responseF <- factor(pilotExp_simData_sum$response,labels=c("chose undesirable","chose desirable"))
ggplot(pilotExp_simData_sum,aes(x=propResp,y=rt)) + geom_point(aes(color=responseF,alpha=empOrSim,shape=empOrSim,size=empOrSim)) +
  facet_grid(. ~ desWorld) + xlim(c(0,1)) + theme_bw() + labs(x="proportion response",y="RT (secs)",color="Response",shape="",size="",alpha="") +
  scale_color_manual(values=c("firebrick","forestgreen")) +
  scale_alpha_manual(values=c(1,0.1)) +
  scale_shape_manual(values=c(4,20)) + 
  scale_size_manual(values=c(5,0.25))


mainExp_simData_sum$responseF <- factor(mainExp_simData_sum$response,labels=c("chose undesirable","chose desirable"))
ggplot(mainExp_simData_sum,aes(x=propResp,y=rt)) + geom_point(aes(color=responseF,alpha=empOrSim,shape=empOrSim,size=empOrSim)) +
  facet_grid(. ~ desWorld) + xlim(c(0,1)) + theme_bw() + labs(x="proportion response",y="RT (secs)",color="Response",shape="",size="",alpha="") +
  scale_color_manual(values=c("firebrick","forestgreen")) +
  scale_alpha_manual(values=c(1,0.1)) +
  scale_shape_manual(values=c(4,20)) + 
  scale_size_manual(values=c(5,0.25))
# note the des RT is faster in undesWorld here (where it is not summarized by ppt first), but is faster in desWorld when you first summarize ppts, and then summarize those values (e.g. mean of ppt medians)

# now annotating quantiles (we could keep crosses for precision, and separately annotate quantiles but it would be messy)
pilotExp_ppcPlot <- ggplot(pilotExp_simData_sum,aes(x=propResp,y=rt,color=responseF)) + 
  geom_text(data=pilotExp_simData_sum[pilotExp_simData_sum$empOrSim=="empirical",],aes(label=quantile),size=3,show.legend = FALSE) +
  geom_point(data=pilotExp_simData_sum[pilotExp_simData_sum$empOrSim=="simulation",],alpha=0.1,size=0.25,shape=20) +
  facet_grid(. ~ desWorld) + xlim(c(0,1)) + 
  theme_bw() + 
  labs(x="proportion response",y="RT (secs)",color="Response") +
  scale_color_manual(values=c("firebrick","forestgreen")) +
  guides(color = guide_legend(override.aes = list(size = 3, shape = 20, alpha = 1)))

pilotExp_allPlots <- plot_grid(pilotExp_paramPlots$bothPlots,pilotExp_ppcPlot,labels=c("","C"),ncol=1) #,rel_widths = c(1,1.3),scale=0.9)
save_plot("pilotExp_allDDMPlots.pdf",pilotExp_allPlots, device=cairo_pdf,ncol=1,nrow=2,base_aspect_ratio = 2)


mainExp_ppcPlot <- ggplot(mainExp_simData_sum,aes(x=propResp,y=rt,color=responseF)) + 
  geom_text(data=mainExp_simData_sum[mainExp_simData_sum$empOrSim=="empirical",],aes(label=quantile),size=3,show.legend = FALSE) +
  geom_point(data=mainExp_simData_sum[mainExp_simData_sum$empOrSim=="simulation",],alpha=0.1,size=0.25,shape=20) +
  facet_grid(. ~ desWorld) + xlim(c(0,1)) + 
  theme_bw() + 
  labs(x="proportion response",y="RT (secs)",color="Response") +
  scale_color_manual(values=c("firebrick","forestgreen")) +
  guides(color = guide_legend(override.aes = list(size = 3, shape = 20, alpha = 1)))

mainExp_allPlots <- plot_grid(mainExp_paramPlots$bothPlots,mainExp_ppcPlot,labels=c("","C"),ncol=1) #,rel_widths = c(1,1.3),scale=0.9)
save_plot("mainExp_allDDMPlots.pdf",mainExp_allPlots, device=cairo_pdf,ncol=1,nrow=2,base_aspect_ratio = 2)

# now doing annotation with crosses
pilotExp_simData_sum$responseF <- factor(pilotExp_simData_sum$response,labels=c("chose undesirable","chose desirable"))
quantile_annotation <- data.frame(propResp=0.5,rt=c(1.6,2.8,4.23,6.2,10.25),label=c("10%","30%","50%","70%","90%"))
pilotExp_ppcPlot <- ggplot(pilotExp_simData_sum,aes(x=propResp,y=rt)) + geom_point(aes(color=responseF,alpha=empOrSim,shape=empOrSim,size=empOrSim)) +
  facet_grid(. ~ desWorld) + xlim(c(0,1)) + theme_bw() + labs(x="proportion response",y="RT (secs)",color="Response",shape="",size="",alpha="") +
  scale_color_manual(values=c("firebrick","forestgreen")) +
  scale_alpha_manual(values=c(1,0.1)) +
  scale_shape_manual(values=c(4,20)) + 
  scale_size_manual(values=c(5,0.25)) +
  geom_text(aes(label=label),data=quantile_annotation) +
  guides(shape = guide_legend(override.aes = list(size = 1, alpha = 1)))
pilotExp_allPlots <- plot_grid(pilotExp_paramPlots$bothPlots,mainExp_ppcPlot,labels=c("","C"),ncol=1) #,rel_widths = c(1,1.3),scale=0.9)
save_plot("pilotExp_allDDMPlots.pdf",pilotExp_allPlots, device=cairo_pdf,ncol=1,nrow=2,base_aspect_ratio = 2)

mainExp_simData_sum$responseF <- factor(mainExp_simData_sum$response,labels=c("chose undesirable","chose desirable"))
quantile_annotation <- data.frame(propResp=0.5,rt=c(2,3.35,4.9,7.25,12.5),label=c("10%","30%","50%","70%","90%"))
mainExp_ppcPlot <- ggplot(mainExp_simData_sum,aes(x=propResp,y=rt)) + geom_point(aes(color=responseF,alpha=empOrSim,shape=empOrSim,size=empOrSim)) +
  facet_grid(. ~ desWorld) + xlim(c(0,1)) + theme_bw() + labs(x="proportion response",y="RT (secs)",color="Response",shape="",size="",alpha="") +
  scale_color_manual(values=c("firebrick","forestgreen")) +
  scale_alpha_manual(values=c(1,0.1)) +
  scale_shape_manual(values=c(4,20)) + 
  scale_size_manual(values=c(5,0.25)) +
  geom_text(aes(label=label),data=quantile_annotation) +
  guides(shape = guide_legend(override.aes = list(size = 1, alpha = 1)))
mainExp_allPlots <- plot_grid(mainExp_paramPlots$bothPlots,mainExp_ppcPlot,labels=c("","C"),ncol=1) #,rel_widths = c(1,1.3),scale=0.9)
save_plot("mainExp_allDDMPlots.pdf",mainExp_allPlots, device=cairo_pdf,ncol=1,nrow=2,base_aspect_ratio = 2)

