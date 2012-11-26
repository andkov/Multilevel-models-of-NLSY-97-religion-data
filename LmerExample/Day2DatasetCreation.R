rm(list=ls(all=TRUE))  #Clears variables
pathOutput <- file.path(getwd(), "LmerExample", "Day2MlmExampleData.csv")

groupSize <- 50 #How many people in each group.
timePoints <- 1:9 #The time points observed for each person.
timeCount <- length(timePoints) #The number of time points observed for each person.

groupIntercepts <- c(0, 5) 
groupSlopes <- c(1.5, 1)
groupCount <- length(groupSlopes)

sdSubjectIntercept <- 1
sdTimeIntercept <- .2
sdSubjectSlope <- .3

totalSubjectCount <- groupCount * groupSize
totalPointCount <- totalSubjectCount * timeCount
ds <- data.frame(
  GroupID=rep(seq_len(groupCount), each=groupSize*timeCount),
  SubjectID=rep(seq_len(totalSubjectCount), each=timeCount),
  TimePoint=rep(timePoints, times=totalSubjectCount),
  Score=NA
)

for( subjectID in unique(ds$SubjectID) ) {
  groupID <- ds[ds$SubjectID==subjectID, "GroupID"]
  groupIntercept <- groupIntercepts[groupID][1]
  groupSlope <- groupSlopes[groupID][1]
  subjectIntercept <- rnorm(n=1, mean=groupIntercept, sd=sdSubjectIntercept)
  subjectSlope <- rnorm(n=1, mean=groupSlope, sd=sdSubjectSlope)
  
  for( timePoint in timePoints ) {
    eTimeIntercept <- rnorm(n=1, sd=sdTimeIntercept)
    score <- subjectIntercept + subjectSlope * timePoint + eTimeIntercept
    ds[ds$SubjectID==subjectID & ds$TimePoint==timePoint, "Score"] <- score 
  }
}
write.csv(x=ds, file=pathOutput, row.names=F)


