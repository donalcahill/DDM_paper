rm(list=ls())

baseDir <- ""
setwd(baseDir)
source("supplementary_functions.R")
load("mainExp_processedData.RData")

# generate data summaries
data.s <- ddply(data,.(ID),summarise,
                p.chooseDes = mean(chooseDes),
                p.chooseDes.m = mean(chooseDes.m),                
                minRT = min(RT.raw),
                medRT = median(RT.raw),
                maxRT = max(RT.raw),
                numTrials = length(RT.raw))
j.s.byWd <- ddply(data,.(ID,desWorldF),summarise,
                  p.chooseDes.m = mean(chooseDes.m))
RT.s.byCh <- ddply(data,.(ID,chooseDesF),summarise,
                   mRTl = mean(RT.log),
                   mRTr = mean(RT.raw),
                   mSeq = mean(seqLength),
                   mEvid = mean(evidForChoice),
                   mdRTr = median(RT.raw),
                   mDiff = mean(diffForChoice))


# Judgment bias analyses --------------------------------------------------

### all analyses here reported in paper

# a1) simple average judgment
j.a1.s <- summarySE(data.s,measurevar="p.chooseDes")
j.a1 <- t.test(data.s$p.chooseDes,mu=0.5)

# a2) now by desWorld v undesWorld
j.a2.s <- summarySEwithin(j.s.byWd,measurevar="p.chooseDes.m",withinvars="desWorldF",idvar="ID")
j.a2 <- t.test(p.chooseDes.m ~ desWorldF,j.s.byWd,paired=TRUE)

# a3) hierarchical model of desWorld
j.a3 <- glmer(chooseDes ~ desWorldC + (1 + desWorldC|ID), data=data, family="binomial")

# a4) judgment by RT quantile
data$RTq5n <- as.numeric(data$RTq5)
j.a4 <- glmer(chooseDes ~ RTq5n + (1+RTq5n|ID), data=data, family="binomial")


# RT and sequence length analyses -----------------------------------------

# summarizing by participant
rt.a1.sa <- summarySEwithin(RT.s.byCh,measurevar="mRTl",withinvars="chooseDesF",idvar="ID")
rt.a1.sb <- summarySEwithin(RT.s.byCh,measurevar="mRTr",withinvars="chooseDesF",idvar="ID")
rt.a1.sc <- summarySEwithin(RT.s.byCh,measurevar="mSeq",withinvars="chooseDesF",idvar="ID")
rt.a1.sd <- summarySEwithin(RT.s.byCh,measurevar="mEvid",withinvars="chooseDesF",idvar="ID")
rt.a1.se <- summarySEwithin(RT.s.byCh,measurevar="mdRTr",withinvars="chooseDesF",idvar="ID") ###
rt.a1.sf <- summarySEwithin(RT.s.byCh,measurevar="mDiff",withinvars="chooseDesF",idvar="ID") ###

# t-tests of participant summaries
rt.a1a <- t.test(mRTl ~ chooseDesF,RT.s.byCh,paired=TRUE)
rt.a1b <- t.test(mRTr ~ chooseDesF,RT.s.byCh,paired=TRUE)
rt.a1c <- t.test(mSeq ~ chooseDesF,RT.s.byCh,paired=TRUE)
rt.a1d <- t.test(mEvid ~ chooseDesF,RT.s.byCh,paired=TRUE)
rt.a1e <- t.test(mdRTr ~ chooseDesF,RT.s.byCh,paired=TRUE)
rt.a1f <- t.test(mDiff ~ chooseDesF,RT.s.byCh,paired=TRUE)

# mixed models
rt.a2a <- lmer(RT.log ~ chooseDesF + (1 + chooseDesF|ID), data=data) ###
rt.a2b <- lmer(RT.raw ~ chooseDesF + (1 + chooseDesF|ID), data=data) 
rt.a2c <- lmer(seqLength ~ chooseDesF + (1 + chooseDesF|ID), data=data) 
rt.a2d <- glmer(cbind(itemsForChoiceCount,seqLength-itemsForChoiceCount) ~ chooseDesF + (1 + chooseDesF|ID), data=data,family="binomial") ###
rt.a2f <- lmer(diffForChoice ~ chooseDesF + (1 + chooseDesF|ID), data=data) ###

# plots for paper -----------------------------------------------------

# Fig 2A 
# plotting as percentage for Tali
p.1 <- ggplot(j.a2.s, aes(x=desWorldF,y=p.chooseDes.m*100,fill=desWorldF)) +
  geom_bar(stat="identity",width=1) +
  geom_errorbar(aes(ymin=(p.chooseDes.m-se)*100, ymax=(p.chooseDes.m+se)*100),
                width=0,position=position_dodge(1),size=1.2) +
  ylim(c(0,100)) +
  scale_fill_manual(values=c("firebrick","forestgreen"),
                    name="Judgment",
                    labels=c("Undes.","Des.")) +
  theme(text=element_text(family="Palatino")) +
  labs(x="Factory",y="Percent judgment") +
  geom_hline(yintercept=50,color="white",linetype="dashed") +
  scale_x_discrete(expand=c(0,1.5),labels=c("Undes.","Des.")) +
  geom_text(x=1.5,y=90,label="***",size=6)

# Fig 2B
p.3 <- ggplot(rt.a1.se,aes(x=chooseDesF,y=mdRTr,color=chooseDesF)) +
  geom_pointrange(aes(ymin=mdRTr-se, ymax=mdRTr+se),size=1) + #,shape=21,fill="white") + ,position=position_dodge(0.2)
  theme(text=element_text(family="Palatino")) +
  scale_color_manual(values=c("firebrick","forestgreen"),
                     guide=FALSE) +
  labs(x="Judgment",y="median RT") +
  ylim(c(5200,6200)) +
  scale_x_discrete(expand=c(0,1.2),labels=c("Undes.","Des."))

# Fig 2C # plotting as % for Tali
data$seqq5 <- binTrials(data$seqLength,5) # using number items seen for visualizing quantiles
levels(data$seqq5) <- c("2-5","6-8","9-12","13-18","19+")
jByQ2seq <- summarySEwithin(data,measurevar = "chooseDes",withinvars = c("seqq5"),idvar="ID")
p.4 <- ggplot(jByQ2seq,aes(x=seqq5,y=chooseDes*100)) +
  geom_hline(yintercept=50,color="black",linetype="dashed") +
  geom_pointrange(aes(ymin=(chooseDes-se)*100,ymax=(chooseDes+se)*100),shape=21,fill="white") +
  theme(text=element_text(family="Palatino")) +
  labs(x="# items seen",y="Percent desirable judgments") + ylim(c(45,65))

# Fig 2D
p.2 <- ggplot(rt.a1.sf, aes(x=chooseDesF,y=mDiff,fill=chooseDesF)) + 
  geom_bar(stat="identity",width=1) +
  geom_errorbar(aes(ymin=mDiff-se, ymax=mDiff+se),
                width=0,position=position_dodge(1),size=1.2) +
  ylim(c(0,4)) +
  scale_fill_manual(values=c("firebrick","forestgreen"),
                    guide=FALSE) +
  theme(text=element_text(family="Palatino")) +
  labs(x="Judgment",y="Evidence strength (#items pro\u2013contra)") +
  #geom_hline(yintercept=0.5,color="white",linetype="dashed") +
  scale_x_discrete(expand=c(0,1.7),labels=c("Undes.","Des.")) +
  geom_text(x=1.5,y=4,label="***",size=6)

plot_output <- plot_grid(p.2,p.1,p.4,p.3,labels=c("A","B","C","D"),ncol=2,rel_widths = c(1,1.3),scale=0.9)
save_plot("plots_for_paper1.pdf",plot_output, device=cairo_pdf,ncol=2,nrow=2)

# eof ---------------------------------------------------------------------





