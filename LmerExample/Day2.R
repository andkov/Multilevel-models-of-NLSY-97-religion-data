rm(list=ls(all=TRUE))  #Clears variables
#install.packages(c("lme4", "colorspace", "NlsyLinks")) #Install only once for each version of R.
require(lme4) #Load the library necessary for multilevel models
require(colorspace) #Load the library necessary for creating tightly-controlled palettes.
pathInput <- file.path(getwd(), "LmerExample", "Day2MlmExampleData.csv") #Location of the data file.

ds <- read.csv(file=pathInput, stringsAsFactors=F, colClasses=c("integer", "integer", "integer", "numeric")) #Read the datafile into a "data.frame" object.
ds$GroupF <- factor(ds$GroupID) #Create a 'factor' for Group (which is necessary for the multilevel model)
ds$SubjectF <- factor(ds$SubjectID)#Create a 'factor' for Subject (which is necessary for the multilevel model)

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

#For m7, the L2/fixed coeffients are for the (a) intercept, (b) group's offset, (c) time's slope and (d) their interaction.
# The L1/random coefficients (ie, those specific to the subject) are intercept and slope.
m7 <- lmer(Score ~ 1 + GroupF + TimePoint + GroupF*TimePoint + (1 + TimePoint | SubjectF), data=ds)
summary(m7) 

###
# Draw the graph
###

#For color information see http://cran.r-project.org/web/packages/colorspace/vignettes/hcl-colors.pdf
# and http://statmath.wu.ac.at/~zeileis/papers/Zeileis+Hornik+Murrell-2009.pdf

pchGroup <- sort(unique(ds$GroupID)) #Define the shapes used for each group. In this case, it's just a 1 and 2.
colorSubject <- rainbow_hcl(n=length(unique(ds$SubjectID))) #Define colors for each subject
colorGroup <- rainbow_hcl(n=length(unique(ds$GroupID))) #Define colors for each group
colorGroupAlpha <- adjustcolor(colorGroup, alpha.f=.5) #Define translucent colors for each group
colorGroupDark <- rainbow_hcl(n=length(unique(ds$GroupID)), c=50, l=60) #Define slightly darker colors for each group (usd for their mean slope).
xRange <- range(ds$TimePoint, na.rm=TRUE) #To be used for the x-axis limits (and remove and missing/NA values).
yRange <- range(ds$Score, na.rm=TRUE) #To be used for the x-axis limits

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
mtext("Time", side=1, line=0, col=controlDark) #Label the x-axis
mtext("Score", side=2, line=1, col=controlDark) #Label the y-axis
box(col=controlLight) #Draw a light framing box.
#points(x=ds$TimePoint, y=ds$Score)
#points(x=ds$TimePoint, y=ds$Score, col=colorSubject[ds$SubjectID])

#boxRadius <- .1
boxRadius <- .05 #Define half of the box's width
boxOffset <- c(-boxRadius, boxRadius) #Shift Group1 to the left, shift Group2 to the right
for( groupID in unique(ds$GroupID) ) { #Iterate through each group in the dataset.
  for( timePoint in sort(unique(ds$TimePoint)) ) { #Iterate through each timepoint in the dataset.
    dsSlice <- subset(ds, GroupID==groupID & TimePoint==timePoint) #Select data only from a specific group's time point.
    if( nrow(dsSlice) > 0 ) { #If these points exist, the continue to draw the box
      uh <- quantile(dsSlice$Score, .75) #Calculate the upper hinge.
      #median <- - quantile(dsSlice$Score, .5)
      lh <- quantile(dsSlice$Score, .25) #Calculate the lower hinge.
      
      xLeft <- timePoint - boxRadius + boxOffset[groupID] #Calculate the box's left boundary location.
      xRight <- timePoint + boxRadius + boxOffset[groupID]#Calculate the box's right boundary location.
      #rect(ybottom=lh, ytop=uh, xleft=xLeft, xright=xRight)
      #rect(ybottom=lh, ytop=uh, xleft=xLeft, xright=xRight, col=colorGroup[groupID]) 
      rect(ybottom=lh, ytop=uh, xleft=xLeft, xright=xRight, col=colorGroup[groupID], border=NA) #Draw the box with an invisible border.
      #rect(ybottom=lh, ytop=uh, xleft=xLeft, xright=xRight, border=colorGroup[groupID], col=NA)
      
      #TODO: add a bar for median
    }
  }
}
rm(dsSlice)

#This loop iterates over each subject and 
for( subjectID in unique(ds$SubjectID) ) { 
  dsSlice <- subset(ds, SubjectID==subjectID) #Select only that subject's data
  xs <- dsSlice$TimePoint #Should we jitter?
  ys <- dsSlice$Score
  #lines(x=xs, y=ys)
  #lines(x=xs, y=ys, col=colorGroup[dsSlice$GroupID]  )
  lines(x=xs, y=ys, col=colorGroupAlpha[dsSlice$GroupID]) #Draw their line, with their group's color
}
rm(dsSlice) #Remove the variable from memory

coef(m7) #Visually inspect all the model's coefficients
co <- fixef(m7) #Extract the coefficients for the fixed effects.
print(co) #Print the fixed effects to the console
names(co) #Inspect the names of the four coefficients.

#Plot the summary for Group1 (which is just the intercept plus the slope*time)
curve(co["(Intercept)"]  + co["TimePoint"]*x , add=T, col=colorGroupDark[1], lwd=4)
#Plot the summary for Group2 (which now includes the Group 2 offsets for intercept & slope)
curve(co["(Intercept)"] + co["GroupF2"] + co["TimePoint"]*x + co["GroupF2:TimePoint"]*x, add=T, col=colorGroupDark[2], lwd=4)
  
#Annotate the clinical threshold of 13.
threshold <- 13
abline(h=threshold, col="blue")
text(x=1, y=threshold, labels="Clinical\nThreshold", pos=4, col="blue")

par(oldPar) #Reset the graphical parameters