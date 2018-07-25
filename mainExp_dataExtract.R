rm(list=ls())

baseDir <- ""
setwd(baseDir)
source("supplementary_functions.R")
numTrials <- 80
timePerItem <- 550


# load and parse data -----------------------------------------------------

file.in.data <- "mainExp_rawData.csv"
toBeExtracted.data <- read.csv(file.in.data,header=FALSE,stringsAsFactors=FALSE)

numPpts <- nrow(toBeExtracted.data)
ID <- factor(1:numPpts)

# the code below simply extracts all the numbers following a particular letter in the string that we get from Qualtrix and reshapes it into a ppt x trial matrix
whatIsDes           <- matrix(unlist(str_extract_all(toBeExtracted.data[ID,],perl("(?<=A)(.{2})"))),ncol = numTrials,byrow = TRUE) # what factory is desirable
desWorld            <- matrix(as.numeric(unlist(str_extract_all(toBeExtracted.data[ID,],perl("(?<=B)[-0-9]+")))),ncol = numTrials,byrow = TRUE) # desirable world: 0 - no, 1 - yes
outcomeDesSeq       <- matrix(unlist(str_extract_all(toBeExtracted.data[ID,],perl("(?<=C)[-0-9]+"))),ncol = numTrials,byrow = TRUE) # no - 0, 1 - yes
chooseDes           <- matrix(as.numeric(unlist(str_extract_all(toBeExtracted.data[ID,],perl("(?<=D)[-0-9]+")))),ncol = numTrials,byrow = TRUE) # no - 0, 1 - yes
whichWorld          <- matrix(as.numeric(unlist(str_extract_all(toBeExtracted.data[ID,],perl("(?<=E)[-0-9]+")))),ncol = numTrials,byrow = TRUE) # ph (0) or tv (1)
chooseTV            <- matrix(as.numeric(unlist(str_extract_all(toBeExtracted.data[ID,],perl("(?<=G)[-0-9]+")))),ncol = numTrials,byrow = TRUE) # 0 - chose phone, 1 - chose TV
RT.1                <- matrix(as.numeric(unlist(str_extract_all(toBeExtracted.data[ID,],perl("(?<=K)[-0-9]+")))),ncol = numTrials,byrow = TRUE) #
RT.2                <- matrix(as.numeric(unlist(str_extract_all(toBeExtracted.data[ID,],perl("(?<=L)[-0-9]+")))),ncol = numTrials,byrow = TRUE) # 
seqLength           <- matrix(as.numeric(unlist(str_extract_all(toBeExtracted.data[ID,],perl("(?<=M)[-0-9]+")))),ncol = numTrials,byrow = TRUE) # 
keyCode             <- matrix(as.numeric(unlist(str_extract_all(toBeExtracted.data[ID,],perl("(?<=N)[-0-9]+")))),ncol = numTrials,byrow = TRUE) # 
correct             <- matrix(as.numeric(unlist(str_extract_all(toBeExtracted.data[ID,],perl("(?<=O)[-0-9]+")))),ncol = numTrials,byrow = TRUE) # 
desCount            <- matrix(as.numeric(unlist(str_extract_all(toBeExtracted.data[ID,],perl("(?<=P)[-0-9]+")))),ncol = numTrials,byrow = TRUE) # 
desProp             <- matrix(as.numeric(unlist(str_extract_all(toBeExtracted.data[ID,],perl("(?<=Q)[-0-9]+")))),ncol = numTrials,byrow = TRUE) # 
aniFlagCount        <- matrix(as.numeric(unlist(str_extract_all(toBeExtracted.data[ID,],perl("(?<=R)[-0-9]+")))),ncol = numTrials,byrow = TRUE) # 

data <- data.frame(ID,chooseDes)
data <- melt(data,id.vars = "ID",variable.name = "trial",value.name = "chooseDes")
data$whichWorld  <- as.vector(whichWorld)
data$chooseTV <- as.vector(chooseTV)
data$desWorld <- as.vector(desWorld)
data$whatIsDes <- as.vector(whatIsDes)
data$seqLength <- as.vector(seqLength)
data$desCount <- as.vector(desCount)
data$desProp <- data$desCount / data$seqLength
data$RT.lev <- as.vector(RT.1)
data$RT.raw <- as.vector(RT.2)
data$RT.log <- as.vector(log(RT.2))
data$aniFlagCount <- as.vector(aniFlagCount)
data$outcomeDesSeq <- as.vector(outcomeDesSeq)
data$correct <- as.vector(correct)


# data transformation -----------------------------------------------------

# evidence and accuracy
data$desPropC <- data$desProp - 0.5
data$evidForChoice <- data$desProp
data[data$chooseDes==0,"evidForChoice"] <- 1 - data[data$chooseDes==0,"evidForChoice"]
data$undesCount <- data$seqLength-data$desCount
data$desDiff <- data$desCount - data$undesCount
data$diffForChoice <- ifelse(data$chooseDes==0,-data$desDiff,data$desDiff)
data$itemsForChoiceCount <- ifelse(data$chooseDes==0,data$undesCount,data$desCount)
data$chooseDes.m <- +(data$chooseDes == data$desWorld) # "accuracy" coding
data$desWorldC <- (data$desWorld*2)-1
data$chooseDesC <- (data$chooseDes*2)-1

# for some plots
data$chooseDesF <- factor(data$chooseDes,labels=c("chose undesirable","chose desirable"))
data$desWorldF <- factor(data$desWorld,labels=c("undesirable fac.","desirable fac."))

# RT quantiles
data$RTq5 <- binTrials(data$RT.raw,5)


# removing bad trials -----------------------------------------------------

# sanity RT filter
sum(data$RT.raw<=0)
data <- subset(data,RT.raw>0)

# dubious timing
data$timingFlag <- data$RT.raw > data$seqLength * timePerItem
data$timingRelative <- data$RT.raw - ((data$seqLength-1) * timePerItem)
data <- subset(data,!timingFlag)

# sequence length == 1
seq1CountperPpt <- ddply(data,.(ID),summarise,seq1Count=sum(seqLength==1))
seqIDFlags <- seq1CountperPpt[seq1CountperPpt$seq1Count>40,"ID"]
data <- data[!(data$ID %in% seqIDFlags) & data$seqLength>1,]


# save data ---------------------------------------------------------------

save(data,file = "mainExp_processedData.RData")


# for HDDM ----------------------------------------------------------------

# resp coding, desCoded
write.csv(data.frame(subj_idx=data$ID,
                     trial=data$trial,
                     desWorld=data$desWorld,
                     response=data$chooseDes,
                     rt=data$RT.raw/1000),"mainExp_forHDDM_desCode.csv",row.names=FALSE)

# resp coding, facCoded
write.csv(data.frame(subj_idx=data$ID,
                     trial=data$trial,
                     whichWorld=data$whichWorld,
                     response=data$chooseTV,
                     rt=data$RT.raw/1000),"mainExp_forHDDM_facCode.csv",row.names=FALSE)



