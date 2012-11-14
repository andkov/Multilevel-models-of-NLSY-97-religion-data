rm(list=ls(all=TRUE)) #Clear all the variables from previous runs
require(ggplot2)
require(plyr)
require(reshape2)
require(lme4) #Load the library necessary for multilevel models
require(colorspace) #Load the library necessary for creating tightly-controlled palettes.

#Load NLSY97 Religion data
relatt<- read.csv("C:/Users/Serious/Documents/GitHub/NLSY-97_Religiosity/databank/current/notmissing/relatt.csv", stringsAsFactors=FALSE)
wrelatt<- read.csv("C:/Users/Serious/Documents/GitHub/NLSY-97_Religiosity/databank/current/wide/relatt_wide_nomiss.csv", stringsAsFactors=FALSE)


str(wrelatt)

