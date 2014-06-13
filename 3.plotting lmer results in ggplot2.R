# rm(list=ls(all=TRUE))
library(ggplot2)
library(plyr)
library(lme4)

pathOutData <- file.path("./Images/predicted.trajectories") # where to put images
rm(dsp)


modnum<-"m9.png"   # model name and also the file number for the export, goes to title
model<-m9          # assing the model to portray in the graph

predict(m9) #Gives you yHat; as far as we can tell, it works the same as fitted(model).
model@pp$X #Gives you the predictor values
model@flist #Give you the ID values.  Be careful if you're using more than one level.  It may not generalize the way you expect.
attr(model@frame,which="intercept", exact=F)
# Basic critiria
LL<-logLik(model)
dev<-deviance(model)
AIC <- AIC(model) 
BIC <- BIC(model) 

# class(model) #lmerMod
# lme4:::residuals.merMod(model) #Residual for each observervation
lme4:::summary.merMod(model)
lme4:::summary.merMod
# 
# dsG <- data.frame ( # Don't ask me what 'G' stands for
#   Response = model@resp$y
#   Residual = lme4:::residuals.merMod(model) #Residual for each observervation
# )
# 
# model@resp

model@devcomp #Fit stats

names(model@pp)

dsG <- model@frame # Don't ask me what 'G' stands for.  Name it something better
dsG$Residual = lme4:::residuals.merMod(model) #Residual for each observervation
head(dsG)

sd(lme4:::residuals.merMod(model) )

lme4:::ranef.merMod(model)
ranef(model)$id[, "timec"] #The coefficient (of the specific 'timec' random effect), where each row is an individual (not an observation)
# summary(model)
# fixef(model)
# ranef(model)
# co<-coef(model)
# print(co)

# str(m9@call)
# str(m9@theta)
# str(m9@beta)


# dsPredict <- data.frame(
#   model@flist,  #The grouping factors for the random effects
#   timec=model@X[, 3], #The values of the time points(which varies, depending how the model equation is specified). 
#                       #model            m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,10,m11,m12,
#                       #model@X value     1  2  2  2  2  2  2  2  2  3  2   2   2
#   YHat=model@eta #Predicted response, given the fixed and random effectss .
# )

# #dsPredict above would error. Try this instead.
dsPredict<-data.frame(id=model@frame$id,
                      timec=model@frame$timec,
                      YHat=fitted(model))
#dsPredict$timec <-c(0:10)  # add this for m0 to plot no slopes
str(dsPredict$timec) # to check what variable was taken by model@X
dsp <- plyr::join(x=ds, y=dsPredict, by=c("id", "timec")) #Probably overkill

dsp$byearf<-as.factor(ds$byear)

str(dsp)

(coefs <- fixef(model)) #Extract the coefficients for the fixed effects.
#create conditional prediction lines for each of the birth year cohorts.
# This is where the bottom part goes from "the list of models.R"
dsp$YPar<-(
  (coefs["(Intercept)"])         +(coefs["agec"]*dsp$agec)
  +(coefs["timec"]*dsp$timec)    +(coefs["timec:agec"]*dsp$agec*dsp$timec)
  +(coefs["timec2"]*dsp$timec2)  +(coefs["timec2:agec"]*dsp$agec*dsp$timec2)
  +(coefs["timec3"]*dsp$timec3)  
)
str(dsp$YPar)# visually inspect YPar - should be numeric values, not NA

bgColour<-gray(.95)   # background color
indLineSz<-.08        # individual line size
indLineAl<-.06        # individual line alpha

str(dsPredict$timec) # inspect the appropriateness of model@X[, 2] assignment. lines should be 0:10

title<-file.path("Predicted Trajectories",modnum)
g0 <- ggplot(dsp, aes(x=timec, y=attend, group=id))
g1<-g0 +xlab("timec.Years past since 2000")+ylab("Worship Attendance")+ggtitle(title)
g2<-g1 +theme(text = element_text(size =25), panel.background=element_rect(fill=bgColour,colour=NA))
g3<-g2 +theme(panel.grid = element_line(linetype = 1,size=rel(3)))
g4<-g3 + geom_line(aes(x=timec,y=YHat),colour="red",alpha=indLineAl,size=indLineSz)  # predicted
g5<-g4 + scale_x_continuous(breaks=seq(0,10, 1),limits=c(0,10)) + scale_y_continuous(breaks=seq(0, 8, 1),limits=c(.5,8.5)) 
g6<-g5 +theme(legend.position=c(.95,.90),legend.direction="vertical")
g7<-g6 +theme(legend.background = element_rect(fill=NA))
g<-g7 +theme(legend.text = element_text(size = 15),legend.title.align =(-3.3))
finalplot<-g + geom_smooth(aes(x=timec, y=YPar,group=byearf,colour=byear),fill=NA, linetype=1,size=1.5,alpha=1)
#finalplot<-g  + geom_smooth(aes(x=timec, y=YPar,group=1),fill=NA, linetype=1,size=1.5,alpha=1) # code for m0

# pathFileOut<-file.path(pathOutData,modnum)
# png(filename = pathFileOut,
#     width =912, height =960 , units = "px")  # the resolution should be decided based on where to use the graphs
# plot(finalplot)
# dev.off()

