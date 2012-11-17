rm(list=ls(all=TRUE)) #Clear all the variables from previous runs
# require(ggplot2)
# require(plyr)
# require(reshape2)
# require(lme4) #Load the library necessary for multilevel models
# require(colorspace) #Load the library necessary for creating tightly-controlled palettes.

#Load NLSY97 Religion data
relatt<- read.csv("C:/Users/Serious/Documents/GitHub/NLSY-97_Religiosity/databank/current/notmissing/relatt.csv", stringsAsFactors=FALSE)
wrelatt<- read.csv("C:/Users/Serious/Documents/GitHub/NLSY-97_Religiosity/databank/current/wide/relatt_wide_nomiss.csv", stringsAsFactors=FALSE)


str(relatt)

# wrelatt$age<-((wrelatt$byear-1980)*12)+wrelatt$bmonth

relatt$byearc <- as.factor(relatt$byear-1980)       # Centered at 1980
relatt$AgeMonc <- as.integer(relatt$AgeMon-(16*12)) # Centered at 16  = months since turning 16 

relatt$AgeYearc<-((relatt$AgeMon/12)-16)
relatt$age<-as.integer(relatt$AgeMon/12)
relatt$agec<-(relatt$age-16)            # Centered at 16 = years since turning 16


qplot(AgeYearc,attend,data=relatt,geom="jitter")
qplot(age,attend,data=relatt,geom="jitter")

qplot(AgeYearc,data=relatt,geom="histogram",binwidth=.1)
qplot(age,data=relatt,geom="histogram", binwidth=.1)

qplot(agemon_2000, data=wrelatt, geom="histogram", binwidth=1 )