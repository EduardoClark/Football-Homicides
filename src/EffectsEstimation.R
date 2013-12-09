###################################
### Author:Eduardo Clark
### Project: Homicides and Fútbol
### Date: September 2013
### For mediotiempo.com
###################################

##First effect estimation script

#Adjust data to fit our need of LE
Panel1 <- subset(Panel, select=c(1:3,12,13,14, 11))
Panel1 <- unique(Panel1)
Panel1$PartidoCasa <- Panel1$Partido
Panel1$Partido <- ifelse(Panel1$PartidoCasa=="Si" | Panel1$PartidoVisita=="Si", "Si", "No")
Panel1$Anio <- substr(x=Panel1$Fecha, 0,4)
Panel1$Month <- substr(Panel1$Fecha, 6,7)
Panel1$HomLog <- log(Panel1$Homicidios)
Panel1$MonthYearCity <- paste(Panel1$Month, Panel1$Anio, Panel1$Ciudad, sep="-")
MonthAverage <- ldply(tapply(Panel1$Homicidios, Panel1$MonthYearCity, mean))
colnames(MonthAverage) <- c("MonthYearCity", "MonthAverage")
Panel1 <- merge(Panel1, MonthAverage, by="MonthYearCity")
remove(MonthAverage)
Panel1 <- subset(Panel1, is.na(Panel1$Fecha)==FALSE)
Panel1 <- arrange(Panel1, Panel1$Fecha)
Panel1$MonthYearCity <- NULL
Panel1$HomDev <- Panel1$Homicidios / Panel1$MonthAverage
Panel1$Gana <- ifelse(is.na(Panel1$Gana)==TRUE, "Nada", Panel1$Gana)
Panel1$Gana <- as.factor(Panel1$Gana)
Panel1$Gana <- factor(Panel1$Gana, levels = c("Nada", "Casa", "Empate", "Visita"))
Panel1$Ganador <- "Nada"
Panel1$Ganador <- ifelse(Panel1$Gana=="Casa" & Panel1$PartidoCasa=="Si", "Casa", Panel1$Ganador)
Panel1$Ganador <- ifelse(Panel1$Gana=="Empate", "Empate", Panel1$Ganador)
Panel1$Ganador <- ifelse(Panel1$Gana=="Visita" & Panel1$PartidoCasa=="Si", "Visita", Panel1$Ganador)
Panel1$Ganador <- factor(Panel1$Ganador, levels = c("Nada", "Casa", "Empate", "Visita"))
Panel1 <- arrange(Panel1, Panel1$Ciudad)
HomTmp <- subset(Panel1, select=3)
HomTmp <- HomTmp[-29234,]
Panel1$PreDayHom <- c(NA, HomTmp)
HomTmp <- subset(Panel1, select=3)
HomTmp <- HomTmp[-1,]
Panel1$AftDayHom <- c( HomTmp,NA)
Panel1$PreDayHom <- ifelse(Panel1$Fecha=="2007-01-02", NA, Panel1$PreDayHom)
Panel1$AftDayHom <- ifelse(Panel1$Fecha=="2011-12-31", NA, Panel1$AftDayHom)
remove(HomTmp)


##First round of effect estimation and assesment of fit, better model fits at NegativeBinomial

#z <- plm(HomDev ~ as.factor(Partido) + as.factor(Anio) + as.factor(Month) , data=Panel1, index=c("Ciudad", "Fecha"), effect="individual" ,  model="within")
#z1 <- plm(HomDev ~ as.factor(Partido) , data=Panel1, index=c("Ciudad", "Fecha"), effect="individual" ,  model="random")
#z3 <- plm(HomLog ~ as.factor(Partido) + as.factor(Anio) + as.factor(Month) , data=Panel1, index=c("Ciudad", "Fecha"), effect="individual" ,  model="within")
#pFtest(z,z1)

### Mexico City
#Mex <- subset(Panel1 , Panel1$Ciudad=="Valle de México")
#z3 <- lm(HomDev ~ as.factor(Partido) + as.factor(Anio) + as.factor(Month) , data= Mex)

### Monterrey
#Mty <- subset(Panel1 , Panel1$Ciudad=="Monterrey")
#z4 <- lm(HomDev ~ as.factor(Partido) + as.factor(Anio) + as.factor(Month) , data= Mty)

### Guadalajara
#GDL <- subset(Panel1 , Panel1$Ciudad=="Guadalajara")
#z5 <- lm(HomDev ~ as.factor(Partido) + as.factor(Anio) + as.factor(Month) , data= GDL)

### Juarez
#Jua <- subset(Panel1 , Panel1$Ciudad=="Juárez")
#z6 <- lm(HomDev ~ as.factor(Partido) + as.factor(Anio) + as.factor(Month) , data= Jua)

### La Laguna
#Tor <- subset(Panel1 , Panel1$Ciudad=="La Laguna")
#z7 <- lm(HomDev ~ as.factor(Partido) + as.factor(Anio) + as.factor(Month) , data= Tor)


### Morelia
#Mor <- subset(Panel1 , Panel1$Ciudad=="Morelia")
#z8 <- lm(HomDev ~ as.factor(Partido) + as.factor(Anio) + as.factor(Month) , data= Mor)

### Queretaro
#QRT <- subset(Panel1 , Panel1$Ciudad=="Querétaro")
#z9 <- lm(HomDev ~ as.factor(Partido) + as.factor(Anio) + as.factor(Month) , data= QRT)

### San Luis Potosí

#SLP <- subset(Panel1 , Panel1$Ciudad=="San Luis Potosí")
#z10 <- lm(HomDev ~ as.factor(Partido) + as.factor(Anio) + as.factor(Month) , data= SLP)

### Tijuana
#TJ <- subset(Panel1 , Panel1$Ciudad=="Tijuana")
#z11 <- lm(HomDev ~ as.factor(Partido) + as.factor(Anio) + as.factor(Month) , data= TJ)

#CSD <- pcdtest(z, test = c("lm"))
#SC <- pbgtest(z)

#Panel.set <- plm.data(Panel1, index = c("Ciudad", "Fecha"))
#DF <- adf.test(  Panel.set$Homicidios  , k=2)
#bptest(HomDev ~ as.factor(Partido) + as.factor(Anio) + factor(Ciudad) + as.factor(Month), data = Panel1, studentize =F)
#coeftest(z, vcovHC(z, method="arellano"))


