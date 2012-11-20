#install.packages(c("lme4", "colorspace", "NlsyLinks")) #Install only once for each version of R.
require(lme4) #Load the library necessary for multilevel models
require(colorspace) #Load the library necessary for creating tightly-controlled palettes.
pathInput <- file.path(getwd(), "LmerExample", "Day2MlmExampleData.csv") #Location of the data file.

ds <- dssmall
# ds$GroupF <- factor(ds$GroupID) #Create a 'factor' for Group (which is necessary for the multilevel model)
# ds$SubjectF <- factor(ds$SubjectID)#Create a 'factor' for Subject (which is necessary for the multilevel model)


summary(m6s)
fixef(m6s) # defines the line for reference - given all predictors are 0
(coef<-coef(m6s)) # individual trajectories of subjects
co <- fixef(m6s) #Extract the coefficients for the fixed effects.
print(co) #Print the fixed effects to the console
names(co) #Inspect the names of the four coefficients.



### Draw the graph
###
#For color information see http://cran.r-project.org/web/packages/colorspace/vignettes/hcl-colors.pdf
# and http://statmath.wu.ac.at/~zeileis/papers/Zeileis+Hornik+Murrell-2009.pdf
str(ds)
ds$byearci<-ds$byearc+1 # for 
pchGroup <- 1 # sort(unique(ds$byearc)) #Define the shapes used for each group. In this case, it's just a 1 and 2.
colorSubject <- rainbow_hcl(n=length(unique(ds$id))) #Define colors for each subject
colorGroup <- rainbow_hcl(n=length(unique(ds$byearci))) #Define colors for each group
colorGroupAlpha <- adjustcolor(colorGroup, alpha.f=.5) #Define translucent colors for each group
colorGroupDark <- rainbow_hcl(n=length(unique(ds$byearci)), c=50, l=60) #Define slightly darker colors for each group (usd for their mean slope).

xRange <- range(ds$time, na.rm=TRUE) #To be used for the x-axis limits (and remove and missing/NA values).
yRange <- range(ds$attend, na.rm=TRUE) #To be used for the x-axis limits

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
mtext("time", side=1, line=1, col=controlDark) #Label the x-axis
mtext("attend", side=2, line=1, col=controlDark) #Label the y-axis
box(col=controlLight) #Draw a light framing box.
# points(x=ds$TimePoint, y=ds$Score)
#points(x=ds$TimePoint, y=ds$Score, col=colorSubject[ds$SubjectID])

#boxRadius <- .1
boxRadius <- .02 #Define half of the box's width - This case has five (5) groups
boxOffset <- c(-boxRadius*2,-boxRadius,boxRadius, boxRadius*2) #Shift Group1 to the left, shift Group2 to the right
for( groupID in unique(ds$byearci) ) { #Iterate through each group in the dataset.
  for( timePoint in sort(unique(ds$timec)) ) { #Iterate through each timepoint in the dataset.
    dsSlice <- subset(ds, byearci==groupID & timec==timePoint) #Select data only from a specific group's time point.
    if( nrow(dsSlice) > 0 ) { #If these points exist, the continue to draw the box
      uh <- quantile(dsSlice$attend, .75) #Calculate the upper hinge.
      #median <- - quantile(dsSlice$Score, .5)
      lh <- quantile(dsSlice$attend, .25) #Calculate the lower hinge.
      
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
rm(dsSlice) # Removes the variable from memory

#This loop iterates over each subject and 
for( i in unique(ds$id) ) { 
  dsSlice <- subset(ds, id==i) #Select only that subject's data
  xs <- dsSlice$time #Should we jitter?
  ys <- dsSlice$attend
  #lines(x=xs, y=ys)
  lines(x=xs, y=ys, col=colorGroup[dsSlice$byearci]  )
  #lines(x=xs, y=ys, col=colorGroupAlpha[dsSlice$byearci]) #Draw their line, with their group's color
}
rm(dsSlice) #Remove the variable from memory

summary(m6s)
fixef(m6s) # defines the line for reference - given all predictors are 0
coef(m6s) # individual trajectories of subjects
co <- fixef(m6s) #Extract the coefficients for the fixed effects.
print(co) #Print the fixed effects to the console
names(co) #Inspect the names of the four coefficients.
print (co)

#Plot the summary for Group1 (which is just the intercept plus the slope*time)
curve(co["(Intercept)"]  + co["timec"]*x , add=T, col=colorGroupDark[1], lwd=4)
#Plot the summary for Group2 (which now includes the Group 2 offsets for intercept & slope)
curve(co["(Intercept)"] + co["byearc"]*x + co["timec"]*x + co["byearc:timec"]*x, add=T, col=colorGroupDark[2], lwd=4)

#Annotate the clinical threshold of 13.
threshold <- 3.44
abline(h=threshold, col="blue")
text(x=1, y=threshold, labels="Clinical\nThreshold", pos=4, col="blue")

par(oldPar) #Reset the graphical parameters