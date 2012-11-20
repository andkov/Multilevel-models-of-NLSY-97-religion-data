rm(list=ls(all=TRUE)) #Clear all the variables from previous runs
require(ggplot2)
require(plyr)
require(reshape2)
require(lme4) #Load the library necessary for multilevel models
require(colorspace) #Load the library necessary for creating tightly-controlled palettes.
require(Hmisc)

pathDataDir<-file.path(getwd())

pathInLong <- file.path (pathDataDir, "attend_long_nomiss_modeling.csv") # melted by AgeMon

dsattendlong<- read.csv(pathInLong, stringsAsFactors=FALSE)

str(dsattendlong)

# The names of the models correspond to the buttons in ISE_NLSY97_Religion.pptx
# Explicitely requesting the model estimates:
# summary(m0) # Variances and st.devs of random effect + fixed effect
# fixef(m0) # Fixed effects
# ranef(m0) # Random effects
# coef(m0)  # Fixed and Random effects
# 
# 
# (m00<- lm(attend~ 1,data=dsattendlong))
#  
# (m0 <- lmer(attend ~ 1 +              (1 | id),dsattendlong,REML=0)) # Null, random intercept model with no predictors
# 
# (m1 <- lmer(attend ~  timec +         (1 | id),dsattendlong,REML=0))# Random intercept, fixed slope
#  
# (m2 <- lmer(attend ~  timec +         (1 + timec | id), dsattendlong,REML=0))# Random intercept, random slopes
# 
# (m3a <- lmer(attend ~ timec + timec2+(1 + timec         |id), dsattendlong,REML=0)) # fixed quadratic time
# 
# (m3b <- lmer(attend ~ timec + timec2+(1 + timec + timec2| id), dsattendlong,REML=0)) # random quadratic time
# 


# (m10 <- lmer(attend ~ timec + timec2 + timec3+ (1 + timec + timec2+ timec3| id), dsattendlong,REML=0)) 


dssmall<-subset(dsattendlong,dsattendlong$id<=200)
str(dssmall)
(m6s <- lmer(attend ~ 1 + byearc + timec + byearc*timec + (1 + timec | id), dssmall,REML=0)) 



# describe(dsattendlong)

