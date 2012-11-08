rm(list=ls(all=TRUE)) #Clear all the variables from previous runs
require(ggplot2)
require(plyr)
require(reshape2)

install.packages("ggplot2")
library(ggplot2)

pathDirectory <- file.path("C:/Users/Serious/Documents/GitHub")
pathBank <- file.path(pathDirectory, "NLSY-97_Religiosity/databank")
pathInData <- file.path(pathBank, "temp_NLSY97_Religion_10242012/relatt.csv")

pathDirectory
pathBank
pathInData

#ds <- read.csv (pathInData,stringsAsFactors=FALSE) # combined dataframe
ds <- read.csv ("C:/Users/Serious/Documents/GitHub/NLSY-97_Religiosity/databank/temp_NLSY97_Religion_24102012/relatt.csv",stringsAsFactors=FALSE)
##############################################################################################################

ds$byear <- as.factor(ds$byear) #Converts cohort to a factor or integer
dsWide$byear <- as.factor(dsWide$byear)


head(ds)
str(ds)
summary(ds)



qplot(attend,AgeMon, data=ds)
qplot(attend,AgeMon, data=ds,geom="jitter")
qplot(AgeMon,attend, data=ds,geom="jitter")
qplot(attend,AgeMon, data=ds,geom="jitter",color=byear)
qplot(AgeMon,attend, data=ds,geom="jitter",color=byear)

qplot(time,attend, data=ds,geom="jitter",color=byear)

qplot(AgeMon, data = ds,geom="histogram", binwidth =12 )

qplot(attend_2000,agemon_2000, data=dsWide,geom=c("line","jitter"),color=byear)
qplot(attend_2000, data=dsWide,geom="density", color=byear, ylim=c(0,.3) )
qplot(byear, data=dsWide, geom="histogram", fill=attend_2000)

qplot(attend_2010,data = dsWide, facets = byear ~ .,geom = "histogram", 
      binwidth = 1,xlim=c(1,8),ylim=c(0,1500) )

qplot(attend,data = ds, facets = byear~.,geom = "histogram", 
      binwidth = 1,xlim=c(1,8),ylim=c(0,1500) )

qplot(attend, data = ds,geom="histogram",binwidth =1, xlim=c("1,8"))

summary(dsWide$attend_2010)