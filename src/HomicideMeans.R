###################################
### Author:Eduardo Clark
### Project: Homicides and Fútbol
### Date: September 2013
### For mediotiempo.com
###################################

##Get means for nonfootball day and football day for each city

Cities <- unique(Panel$Ciudad)
Cities <- Cities[-17]
Panel$Homicidios <- ifelse(is.na(Panel$Homicidios)==TRUE, 0, Panel$Homicidios)

HomicideMeans <- data.frame( HomMeanNo=NA, HomMeanSi=NA,City=NA)
for (i in 1:length(Cities)){
  tmp1 <- subset(Panel, Panel$Ciudad==Cities[i])
  tmp2 <- ldply(tapply(tmp1$Homicidios, tmp1$Partido, mean))
  tmp2 <- data.frame(t(tmp2))
  tmp2$City <- Cities[[i]]
  tmp2 <- tmp2[-1,]
  colnames(tmp2) <- c("HomMeanNo", "HomMeanSi", "City")
  HomicideMeans <- rbind(HomicideMeans, tmp2)
  remove(list=ls(pattern="tmp"))  
}

