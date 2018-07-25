amalgamateParamTraces <- function(modName) {
  dOut <- data.frame(read.csv(paste0(modName,"_z.csv"),header=FALSE),
                     read.csv(paste0(modName,"_v1.csv"),header=FALSE),
                     read.csv(paste0(modName,"_v0.csv"),header=FALSE))
  names(dOut) <- c("z","v1","v0")
  dOut$v0 <- -dOut$v0
  return(dOut)
}