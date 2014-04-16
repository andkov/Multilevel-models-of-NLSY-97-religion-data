install.packages('ggplot2')
install.packages('plyr')
install.packages('reshape2')
install.packages('lme4')
install.packages('colorspace')
install.packages('Hmisc')
install.packages('lattice')

rm(list=ls(all=TRUE)) #Clear all the variables from previous runs
require(ggplot2)
require(plyr)
require(reshape2)
require(lme4) #Load the library necessary for multilevel models
require(colorspace) #Load the library necessary for creating tightly-controlled palettes.
require(Hmisc)
require(lattice)



             
pathDataDir<-file.path (getwd())                                            #GitHub repository locaton
pathInLong <- file.path (pathDataDir,"attend_long_nomiss_modeling.csv")     # reading in the data
                       
dsattendlong<- read.csv(pathInLong, stringsAsFactors=FALSE)
str(dsattendlong)
ds<-dsattendlong
ds$agec<-ds$age2000c                                                        #renaming for simpler usage

# The names of the models correspond to the buttons in ISE_NLSY97_Religion.pptx
# Explicitely requesting the model estimates:
# summary(m0) # Variances and st.devs of random effect + fixed effect
# fixef(m0) # Fixed effects
# ranef(m0) # Random effects
# coef(m0)  # Fixed and Random effects
 

# ds<-subset(ds,(ds$byear==1980 |ds$byear==1982 |ds$byear== 1984),) # keeps size manageble, select chosen cohorts

ds<-subset(ds,(ds$id<=200),) # keeps size manageble


# Incert a model from "the list of models.R" file here:

(m5 <-lmer (attend ~ 
              1  + timec + timec2 + timec3            
            + (1 + timec + timec2 | id),
            data = ds, REML=0))



