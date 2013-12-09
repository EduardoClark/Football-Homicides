###################################
### Author:Eduardo Clark
### Project: Homicides and Fútbol
### Date: September 2013
### For mediotiempo.com
###################################

### Run all for project
source("src/loadLibraries.R") #Load Libraries

#Get and clean data
source("src/GameDates.R") ## Get Game Dates from 2007-2011
source("src/CleanMatches.R") ## Clean Game Dates and create complete DF of Game Dates 
source("src/HomicideData.R") ## Load Homicide Data and merge with Game Dates

#Game Day effect estimation
source("src/EffectsEstimation.R") #Some estimation scripts and more data cleaning
source("src/NegativeBinomial.R")  #Estimation results from the negative binomial and ZI models

#Summary Statistics for tables
source("src/SummaryStatistics.R") ##Latex Text outputs to latex-plots

