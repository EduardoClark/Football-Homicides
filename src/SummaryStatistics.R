###################################
### Author:Eduardo Clark
### Project: Homicides and Fútbol
### Date: September 2013
### For mediotiempo.com
###################################

##DataSet Summary Statistics

#Total City/Days by weekday
GamesSt1 <- ldply(table(Panel1$Dia))
colnames(GamesSt1) <- c("Category", "Observations")
tmp <- data.frame(Category="Total Days", Observations=29234) 
GamesSt1 <- rbind(tmp, GamesSt1);remove(tmp)
GamesSt1$Observations <- format(GamesSt1$Observations, big.mark=",")
tmp1 <- subset(Panel1, Panel1$Partido=="Si")
tmp2 <- subset(Panel1, Panel1$Partido=="No")
GamesSt2 <- ldply(table(tmp1$Dia))
colnames(GamesSt2) <- c("Category", "Game")
tmp <- data.frame(Category="Total Days", Game=2366)
GamesSt2 <- rbind(tmp, GamesSt2);remove(tmp)
GamesSt2$Game <- format(GamesSt2$Game, big.mark=",")
GamesSt1 <- cbind(GamesSt1, GamesSt2$Game)
colnames(GamesSt1) <- c("Day", "Total Observations", "Game(Yes)")
GamesSt2 <- ldply(table(tmp2$Dia))
colnames(GamesSt2) <- c("Category", "Game")
tmp <- data.frame(Category="Total Days", Game=26868)
GamesSt2 <- rbind(tmp, GamesSt2);remove(tmp)
GamesSt2$Game <- format(GamesSt2$Game, big.mark=",")
GamesSt1 <- cbind(GamesSt1, GamesSt2$Game)
colnames(GamesSt1) <- c("Day", "Total Observations", "Game(Yes)", "Game(No)")
T1 <- xtable(GamesSt1, label="Days-City Observations") ##Save table for latex
remove(tmp1, tmp2, GamesSt1, GamesSt2)


###Means by day
MeanDay <- data.frame(GameYes=NA, GameNo=NA, Day=NA, pValue=NA, AllDays=NA, HomeGame=NA, AwayGame=NA)
tmp <- as.data.frame(t(ldply(tapply(Panel1$Homicidios, Panel1$Partido, mean))))
Test1 <- t.test( subset(Panel1, Panel1$PartidoCasa=="Si")[[3]], 
                 subset(Panel1, Panel1$PartidoVisita=="Si")[[3]])
tmp <- tmp[2,]
tmp$V3 <- "All Days"
tmp$V4 <- round(Test1$p.value,5)
tmp$V5 <- mean(Panel1$Homicidios)
tmp3 <- subset(Panel1, Panel1$PartidoCasa=="Si")
tmp$V6 <- mean(tmp3$Homicidios)
tmp4 <- subset(Panel1, Panel1$PartidoVisita=="Si")
tmp$V7 <- mean(tmp4$Homicidios)
colnames(tmp) <- c("GameYes", "GameNo", "Day", "pValue", "AllDays", "HomeGame", "AwayGame")
MeanDay <- rbind(MeanDay, tmp)
Days <- as.character(unique(Panel1$Dia))
for (i in 1:7){
  tmp1 <- subset(Panel1, Panel1$Dia==Days[i])
  tmp2 <- t.test( subset(Panel1, Panel1$PartidoCasa=="Si")[[3]], 
                   subset(Panel1, Panel1$PartidoVisita=="Si")[[3]])
  tmp2 <- t.test( subset(tmp1, Panel1$PartidoVisita=="Si")[[3]], 
                   subset(tmp1, Panel1$Partido=="No")[[3]])
  tmp <- as.data.frame(t(ldply(tapply(tmp1$Homicidios, tmp1$Partido, mean))))
  tmp <- tmp[2,]
  tmp$V3 <- Days[i]
  tmp$V4 <- round(tmp2$p.value,5)
  tmp$V5 <- mean(tmp1$Homicidios)
  tmp3 <- subset(tmp1, tmp1$PartidoCasa=="Si")
  tmp$V6 <- mean(tmp3$Homicidios)
  tmp4 <- subset(tmp1, tmp1$PartidoVisita=="Si")
  tmp$V7 <- mean(tmp4$Homicidios)
  colnames(tmp) <- c("GameYes", "GameNo", "Day", "pValue", "AllDays","HomeGame", "AwayGame")
  MeanDay <- rbind(MeanDay, tmp)
  remove(list=ls(pattern="tmp"))
}
remove(i, Days)
MeanDay <- MeanDay[-1,]
MeanDay <- subset(MeanDay, select=c(3,5,1,2,6,7,4))
MeanDay$GameYes <- round(as.numeric(MeanDay$GameYes), 2)
MeanDay$GameNo <- round(as.numeric(MeanDay$GameNo), 2)
Days <- as.character(unique(Panel1$Dia))
Days <- data.frame(Days=Days, Corre=c(4,5,6,7,8,2,3))
MeanDay <- merge(MeanDay, Days,by=1, all=TRUE)
MeanDay[1,8] <- 1
MeanDay <- arrange(MeanDay, MeanDay$Corre)
MeanDay$Corre <- NULL
T2 <- xtable(MeanDay)
remove(Test1, Days)


###Games per City
tmp2 <- subset(Panel1, Panel1$Partido=="Si")
tmp3 <- ldply(table(tmp2$Ciudad))
tmp2 <- subset(Panel1, Panel1$PartidoCasa=="Si")
tmp4 <- ldply(table(tmp2$Ciudad))
tmp2 <- subset(Panel1, Panel1$PartidoVisita=="Si")
tmp5 <- ldply(table(tmp2$Ciudad))
colnames(tmp3) <- c("Ciudad", "TotalGameDays")
colnames(tmp4) <- c("Ciudad", "HomeGameDays")
colnames(tmp5) <- c("Ciudad", "AwayGameDays")
GamesCity <- merge(tmp3, tmp4, by=1)
GamesCity <- merge(GamesCity, tmp5, by=1)
remove(list=ls(pattern="tmp"))
GamesCity <- arrange(GamesCity, desc(GamesCity$TotalGameDays))
T4 <- xtable(GamesCity)

remove(GamesCity, MeanDay)

write(print(T1), file="latex-plots/T1")
write(print(T2), file="latex-plots/T2")
write(print(T4), file="latex-plots/T3")
remove(T1,T2,T4)







