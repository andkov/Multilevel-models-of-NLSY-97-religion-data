rm(list=ls(all=TRUE)) #Clear all the variables from previous runs
require(ggplot2)
require(plyr)
require(reshape2)

AgeMon<- read.csv("C:/Users/Serious/Documents/GitHub/NLSY-97_Religiosity/databank/temp_NLSY97_Religion_24102012/agemon.csv", stringsAsFactors=FALSE)
attend<- read.csv("C:/Users/Serious/Documents/GitHub/NLSY-97_Religiosity/databank/temp_NLSY97_Religion_24102012/attend.csv", stringsAsFactors=FALSE)

relatt <- merge(attend,AgeMon, by=c("id","byear","time"))
relatt <- relatt[order(relatt$id, relatt$byear),]

pathOutData <- file.path (pathOut,"relatt.csv") # combined dataframe
write.csv (relatt, pathOutData, row.names=FALSE)

require(lme4)

str(relatt)
relatt$AgeMon <- as.factor(relatt$value)     #Convert to a number.
relatt$byear <- as.factor(relatt$byear)
str(relatt)

m0 <- lm(attend ~ AgeMon, data=relatt)
summary(m0)

m0 <- lmer(attend ~ AgeMon, data=relatt)
summary(m0) 

coef(m4) #Visually inspect all the model's coefficients
co <- fixef(m0) #Extract the coefficients for the fixed effects.
print(co) #Print the fixed effects to the console
names(co) #Inspect the names of the four coefficients.

#Plot the summary for Group1 (which is just the intercept plus the slope*time)
curve(co["(Intercept)"] + co["byear"]*x , add=T, col=colorGroupDark[1], lwd=4)

m4 <- lmer(attend ~ AgeMon + (1 + AgeMon | id) + (1 + AgeMon | byear), data=relatt)
summary(m4)
curve(co["(Intercept)"] + co["AgeMon"]*x , add=T, col=colorGroupDark[1], lwd=4)