rm(list=ls(all=TRUE)) #Clear all the variables from previous runs
require(lme4)

pathDirectory <- file.path(getwd(), "Data")
pathInData <- file.path(pathDirectory, "Long.csv") #The name of the file to write to.

ds <- read.csv(pathInData, stringsAsFactors=FALSE)
ds$id <- factor(ds$id)
ds$byearF <- factor(ds$byear) #Create a factor variable (for lmer), but retain the numerical variable if you need it later (for graphing).
summary(ds)
head(ds)

m0 <- lm(attendence ~ time, data=ds)
summary(m0)

m1 <- lmer(attendence ~ time + (1 | id), data=ds) #All subjects have the same slope, but different varying intercepts
summary(m1)
#ranef(m1) Look at the random effects (one intercept for each subject)
fixef(m1)

m2 <- lmer(attendence ~ time + (1 + time | id), data=ds) #All subjects have varying intercepts and slopes (for 'time').
summary(m2)
#ranef(m2) Look at the random effects (one intercept and one slope for each subject).  Notice there's an extra column, compared to ranef(m1).
fixef(m2)

m3 <- lmer(attendence ~ time + (1 + time | id) + (1 | byearF), data=ds)
summary(m3)
#ranef(m3) Look at the random effects (two for each subjec and one for each cohort).  Notice there's an extra table, compared to ranef(m2).
fixef(m3)

m4 <- lmer(attendence ~ time + (1 + time | id) + (1 + time | byearF), data=ds)
summary(m4)
#ranef(m4) Look at the random effects (two for each subjec and two for each cohort).  Notice there's an extra column in the second table, compared to ranef(m3).
fixef(m4)

#For example of graphing, look at the 'Day2' files in this directory.  I used it when I taught 
#   a few lectures in Rodgers' EDA class.  It shows how to pull out the coefficients to graph lines.

#Of course, attendance should be blindly treated as a continuous/interval variables, as I did.  
#   This is more to provide an example of lmer with a dataset you're familiar with.