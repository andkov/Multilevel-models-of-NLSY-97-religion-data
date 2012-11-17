rm(list=ls(all=TRUE)) #Clear all the variables from previous runs
require(ggplot2)
require(plyr)
require(reshape2)
require(lme4) #Load the library necessary for multilevel models
require(colorspace) #Load the library necessary for creating tightly-controlled palettes.
pathGitHub<-file.path("C:/Users/Serious/Documents/GitHub")
pathData <- file.path(pathGitHub,"NLSY-97_Religiosity/databank/temp_NLSY97_Religion_24102012") #Location of the data file.

relatt<- read.csv("C:/Users/Serious/Documents/GitHub/NLSY-97_Religiosity/databank/current/notmissing/relatt.csv", stringsAsFactors=FALSE)
wrelatt<- read.csv("C:/Users/Serious/Documents/GitHub/NLSY-97_Religiosity/databank/current/wide/relatt_wide_nomiss.csv", stringsAsFactors=FALSE)

# relattm <- relattm[order(relattm$id, relattm$byear),]
relatt <- relatt[order(relatt$id, relatt$byear),]
#byearc$relattm<-byear$relattm-1980)

relatt$byearc <- as.factor(relatt$byear-1980)       # Centered at 1980
relatt$AgeMonc <- as.integer(relatt$AgeMon-(16*12)) # Centered at 16  = months since turning 16 
relatt$AgeYearc<-((relatt$AgeMon/12)-16)            # Centered at 16 = years since turning 16
relatt$AgeMonc2<-(relatt$AgeMonc^2)                 # Quadratid term for Months
relatt$AgeYearc2<-(relatt$AgeYearc^2)               # Quadratid term for Years
relatt$AgeMonc3<-(relatt$AgeMonc^3)
relatt$AgeYearc3<-(relatt$AgeYearc^3)

str(relattm) 
str(relatt)



# The names of the models correspond to the buttons in ISE_NLSY97_Religion.pptx
# Explicitely requesting the model estimates:
# summary(m0) # Variances and st.devs of random effect + fixed effect
# fixef(m0) # Fixed effects
# ranef(m0) # Random effects
# coef(m0)  # Fixed and Random effects


# (m00<- lm(attend~ 1,data=relatt))
# summary(m00)
 
(m0 <- lmer(attend ~ 1 + (1 | id),relatt,REML=0)) # Null, random intercept model with no predictors
 
(m1 <- lmer(attend ~ AgeMon + (1 | id),relatt,REML=0))# Random intercept, fixed slope
 
(m2 <- lmer(attend ~ AgeMon + (1 + AgeMon | id), relatt,REML=0))# Random intercept, random slopes

(m3 <- lmer(attend ~ AgeMon + AgeMon2 + (1 + AgeMon | id), relatt,REML=0)) # fixed level-2 predictor


m0
m1
m2
m3

# ranef(m2) #Look at the random effects (one intercept for each subject)
# fixef(m2) #Look at the fixted effects 


###
# Draw the graph
###

#For color information see http://cran.r-project.org/web/packages/colorspace/vignettes/hcl-colors.pdf
# and http://statmath.wu.ac.at/~zeileis/papers/Zeileis+Hornik+Murrell-2009.pdf

#pchGroup <- sort(unique(relatt$byear)) #Define the shapes used for each group. In this case, it's just a 1 and 2.
pchGroup <- c("1980","1981")
colorSubject <- rainbow_hcl(n=length(unique(relatt$id))) #Define colors for each subject
colorGroup <- rainbow_hcl(n=length(unique(relatt$byear))) #Define colors for each group
colorGroupAlpha <- adjustcolor(colorGroup, alpha.f=.5) #Define translucent colors for each group
colorGroupDark <- rainbow_hcl(n=length(unique(relatt$byear)), c=50, l=60) #Define slightly darker colors for each group (usd for their mean slope).
xRange <- range(relatt$AgeMon, na.rm=TRUE) #To be used for the x-axis limits (and remove and missing/NA values).
yRange <- range(relatt$attend, na.rm=TRUE) #To be used for the x-axis limits

#Define three shades of gray to be used by the graph's non-data elements
controlDark <- gray(.4)
controlMedium <- gray(.6)
controlLight <- gray(.8)

#Start plotting
#oldPar <- par(mar=c(5, 4, 4, 2), mgp=c(3,1,0))
oldPar <- par(mar=c(2.1, 2, 3, .5), tcl=0, mgp=c(1,0,0))
#plot(NA, xlim=xRange, ylim=yRange, main="MLM Practice", xlab="Time", ylab="Score")
plot(NA, xlim=xRange, ylim=yRange, main="MLM Practice", ann=F, xaxt="n", yaxt="n", bty="n") #plot and empty square
axis(side=1, col.axis=controlMedium, cex.axis=.8) #Draw the x-axis with slightly smaller numbers (80% of the original size)
axis(side=2, col.axis=controlMedium, cex.axis=.8) #Draw the y-axis with slightly smaller numbers
mtext("Age in months", side=1, line=1, col=controlDark) #Label the x-axis
mtext("Religious involvement", side=2, line=1, col=controlDark) #Label the y-axis
box(col=controlLight) #Draw a light framing box.
#points(x=ds$TimePoint, y=ds$Score)
#points(x=ds$TimePoint, y=ds$Score, col=colorSubject[ds$SubjectID])
#boxRadius <- .1

boxRadius <- .05 #Define half of the box's width
boxOffset <- c(-.05,.05 ) #Shift Group1 to the left, shift Group2 to the right

for( byear in unique(relatt$byear) ) { #Iterate through each group in the dataset.
  for( AgeMon in sort(unique(relatt$AgeMon)) ) { #Iterate through each timepoint in the dataset.
    dsSlice <- subset(relatt, byear==byear & AgeMon==AgeMon) #Select data only from a specific group's time point.
    if( nrow(dsSlice) > 0 ) { #If these points exist, the continue to draw the box
      uh <- quantile(dsSlice$Score, .75) #Calculate the upper hinge.
      #median <- - quantile(dsSlice$Score, .5)
      lh <- quantile(dsSlice$Score, .25) #Calculate the lower hinge.
      
      xLeft <- AgeMon - boxRadius + boxOffset[byear] #Calculate the box's left boundary location.
      xRight <- AgeMon + boxRadius + boxOffset[byear]#Calculate the box's right boundary location.
      #rect(ybottom=lh, ytop=uh, xleft=xLeft, xright=xRight)
      #rect(ybottom=lh, ytop=uh, xleft=xLeft, xright=xRight, col=colorGroup[byear]) 
      rect(ybottom=lh, ytop=uh, xleft=xLeft, xright=xRight, col=colorGroup[byear], border=NA) #Draw the box with an invisible border.
      #rect(ybottom=lh, ytop=uh, xleft=xLeft, xright=xRight, border=colorGroup[groupID], col=NA)
      
      #TODO: add a bar for median
    }
  }
}
rm(dsSlice)

#This loop iterates over each subject and 
for( SubjectID in unique(relatt$id) ) { 
  dsSlice <- subset(relatt, SubjectID==id) #Select only that subject's data
  xs <- dsSlice$AgeMon #Should we jitter?
  ys <- dsSlice$attend
  #lines(x=xs, y=ys)
  #lines(x=xs, y=ys, col=colorGroup[dsSlice$byear]  )
  lines(x=xs, y=ys, col=colorGroupAlpha[dsSlice$byear]) #Draw their line, with their group's color
}
rm(dsSlice) #Remove the variable from memory


summary(m2)
co <- fixef(m2) #Extract the coefficients for the fixed effects.
print(co) #Print the fixed effects to the console
names(co) #Inspect the names of the four coefficients.
#Plot the summary for Group1 (which is just the intercept plus the slope*time)
colorGroupDark<-c("red")
curve(co["(Intercept)"] + co["AgeMon"]*x , add=T, col=colorGroupDark[1], lwd=4)
# curve - shortcut for the folling code
# x<-150:400
# a<-co["(Intercept)"]
# b<-co["AgeMon"]
# y<-a+b*x
# lines(x,y,col="red")




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






