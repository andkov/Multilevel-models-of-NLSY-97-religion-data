rm(list=ls(all=TRUE)) #Clear all the variables from previous runs
require(ggplot2)
require(plyr)
require(reshape2)
require(lme4) #Load the library necessary for multilevel models
require(colorspace) #Load the library necessary for creating tightly-controlled palettes.


relattm<- read.csv("C:/Users/Serious/Documents/GitHub/NLSY-97_Religiosity/databank/temp_NLSY97_Religion_24102012/backup_withmissing/relatt.csv", stringsAsFactors=FALSE)
relatt<- read.csv("C:/Users/Serious/Documents/GitHub/NLSY-97_Religiosity/databank/temp_NLSY97_Religion_24102012/backup_withoutmissing/relatt.csv", stringsAsFactors=FALSE)


relattm <- relattm[order(relattm$id, relattm$byear),]
relatt <- relatt[order(relatt$id, relatt$byear),]
#byearc$relattm<-byear$relattm-1980)

str(relatt)
relattm$AgeMon <- as.integer(relattm$AgeMon)     #Convert to a number.
relattm$byear <- as.factor(relattm$byear)       #Create a factor variable (for lmer), but retain the numerical variable if you need it later (for graphing).
relatt$AgeMon <- as.integer(relatt$AgeMon)     
relatt$byear <- as.factor(relatt$byear)
str(relattm)


m00<- lm(attend~ 1,data=relatt )
summary(m00)

m0 <- lmer(attend ~ 1 + (1 | id), data=relatt) # Null, random intercept model, no predictors
summary(m0)
coef(m0)

m1 <- lmer(attend ~ AgeMon + (1 | id), data=relatt)# Random intercept, fixed slope
summary(m1)
coef(m1)

m2 <- lmer(attend ~ AgeMon + (1 + AgeMon | id), data=relatt)# Random intercept, random slopes
summary(m2)
coef(m2)



ranef(m2) #Look at the random effects (one intercept for each subject)
fixef(m2) #Look at the fixted effects ( beta_0j)

co <- fixef(m2) #Extract the coefficients for the fixed effects.
print(co) #Print the fixed effects to the console
names(co) #Inspect the names of the four coefficients.
#Plot the summary for Group1 (which is just the intercept plus the slope*time)
curve(co["(Intercept)"] + co["byear"]*x , add=T, col=colorGroupDark[1], lwd=4)


#Run and compare competing models.  I liked m7 for this.
# m0 <- lm(Score ~ 1 + TimePoint, data=ds)
# summary(m0)
# m1 <- lm(Score ~ 1 + GroupF + TimePoint, data=ds)
# summary(m1)
# m2 <- lm(Score ~ 1 + GroupF + TimePoint + GroupF*TimePoint, data=ds)
# summary(m2)
# m3 <- lmer(Score ~ 1 + GroupF + TimePoint + (1 | SubjectF), data=ds)
# summary(m3)
# m4 <- lmer(Score ~ 1 + GroupF + TimePoint + (1 + TimePoint | SubjectF), data=ds)
# summary(m4)
# m5 <- lmer(Score ~ 1 + TimePoint + (1 | SubjectF) + (1 + TimePoint | GroupF), data=ds)
# summary(m5)
# m6 <- lmer(Score ~ 1 + TimePoint +  (1 + TimePoint | GroupF), data=ds)
# summary(m6)










m4 <- lmer(attend ~ AgeMon + (1 + AgeMon | id) + (1 + AgeMon | byear), data=relatt)
summary(m4)
curve(co["(Intercept)"] + co["AgeMon"]*x , add=T, col=colorGroupDark[1], lwd=4)