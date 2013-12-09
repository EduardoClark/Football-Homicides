###################################
### Author:Eduardo Clark
### Project: Homicides and Fútbol
### Date: September 2013
### For mediotiempo.com
###################################

### Read Homicides in Mexico Data
Homicides <- read.csv("data/Homicidios.csv")
Homicides$day_occur <- as.Date(Homicides$date_occur)

### Get number of homicides by date for each team 
Places <- sort(as.character(unique(Homicides$metro_area)))
Places <- gsub(" ", "_", Places)
Places <- gsub(",", "", Places)
p <- c("Valle_de_México", "Cancún", "Guadalajara", "Valle_de_México", "Guadalajara", "Guadalajara","Juárez","Tuxtla_Gutiérrez", "Monterrey", "Morelia", "Aguascalientes", "Pachuca", "Puebla", "Valle_de_México", "Querétaro", "San_Luis_Potosí", "La_Laguna", "Monterrey", "Tijuana", "Toluca", "Veracruz")
p <- gsub("_", " ", p)
Subsetter <- function(Place){
  tmp <- subset(Homicides, Homicides$metro_area==Place)
  tmp <- ldply(table(tmp$date_occur))
  colnames(tmp) <- c("Fecha", "Homicidios")
  tmp$Ciudad <- gsub("_"," ",Place)
  return(tmp)
}
Tabulados <- ldply(lapply(p, Subsetter))
remove(Subsetter, Places)
Tabulados$Fecha <- as.Date(Tabulados$Fecha)
Tabulados <- subset(Tabulados, as.Date(Tabulados$Fecha) > "2007-01-01")
Equipos <- data.frame(Equipo=sort(unique(Complete$Local)), Ciudad=p); remove(p)
Tabulados <- merge(Tabulados, Equipos, by="Ciudad")

### Include City in both Homicides per Day + Game Dates
Complete <- merge(Complete, Equipos, by.x="Local", by.y=1, all.x=TRUE, all.y=FALSE)
Equipos1 <- Equipos
colnames(Equipos1) <- c("Visitante", "CiudadVisita")
Complete1 <- merge(Complete, Equipos1, by="Visitante", all.x=TRUE, all.y=FALSE)
#Tabulados <- merge(Tabulados, Equipos, by.x="Equipo", by.y=1, all.x=TRUE, all.y=FALSE)


### Merge with game dates and create final dataframe for analisis
Panel <- merge(Tabulados, Complete, by.x=c("Fecha", "Ciudad"), by.y=c("Fecha", "Ciudad"), all=TRUE)
Panel$Equipo <- NULL
Panel <- unique(Panel)
Panel$Partido <- ifelse(is.na(Panel$GolesVisita)==FALSE, "Si", "No")
Panel$Dia <- wday(Panel$Fecha, label=TRUE)


### Add visiting games
Complete1 <- subset(Complete1, select=c(11,3))
Complete1$PartidoVisita <- "Si"
Complete1$Merger <- paste(Complete1$CiudadVisita,Complete1$Fecha, sep="-")
Panel$Merger <- paste(Panel$Ciudad, Panel$Fecha, sep="-")
Complete1$Fecha <- NULL
Panel <- merge(Panel, Complete1, by="Merger", all.x=TRUE, all.y=FALSE)
Panel$Merger <- NULL
Panel <- unique(Panel)
Panel$PartidoVisita <- ifelse(is.na(Panel$PartidoVisita)==TRUE, "No", "Si")
Panel$CiudadVisita <- NULL
Panel$Homicidios <- ifelse(is.na(Panel$Homicidios)==TRUE, 0 ,Panel$Homicidios)

##Remove Redundants
remove(Complete1, Equipos, Equipos1, Homicides, Tabulados)



#Panel1 <- subset(Panel, select=c(1,3,4,12,13))
#Panel1 <- unique(Panel1)
#Panel1$Index <- paste(Panel1$Fecha, Panel1$Ciudad, sep="-")
#Double <- ldply(table(Panel1$Index))
#Panel1 <- merge(Panel1, Double, by.x="Index", by.y=".id")
#Panel1$Partido <- ifelse(Panel1$V1>1, "Si", Panel1$Partido)
#Panel1 <- unique(Panel1)
#Panel1$lHom <- log(Panel1$Homicidios)
#Panel1$Month <- substr(Panel1$Fecha, 6,7)
#colnames(Seleccion1) <- c("Fecha", "Seleccion", "ResultadoMexico")
#Panel1 <- merge(Panel1, Seleccion1, by="Fecha", all=TRUE)
#Panel1$Seleccion <- ifelse(is.na(Panel1$Seleccion)==TRUE, "NoJuega", Panel1$Seleccion)
#Panel1$ResultadoMexico <- ifelse(is.na(Panel1$ResultadoMexico)==TRUE, "NoJuega", Panel1$ResultadoMexico)
#Panel1$Anio <- substr(Panel1$Fecha,0,4)

### Write analisis df as a csv
write.csv(Panel, "out-data/PanelComplete.csv")
