###################################
### Author:Eduardo Clark
### Project: Homicides and Fútbol
### Date: September 2013
### For mediotiempo.com
###################################

#### Clean Match Data and create final dataframe with all matches

### Merge those with long month name
Partial <- rbind(F2009, F2010, F2011)
Splitter <- regexpr("\\[", Partial$Fecha)
Splitter <- ifelse(Splitter==-1, 1000, Splitter)
Partial$Fecha <- substr(Partial$Fecha, 0, Splitter)
remove(Splitter)
Partial$Fecha <- tolower(Partial$Fecha)
Months <- data.frame(Long=c("enero", "febrero", "marzo", "abril", "mayo", "junio",
                            "julio", "agosto", "septiembre", "octubre",
                            "noviembre", "diciembre"), Number=c(1:12))
Months$Short <- substr(Months$Long, 0,3)
Months$Long <- as.character(Months$Long)
for (i in 1:12){
  tmp <- Months$Long[i]
  tmp1 <- Months$Number[i]
  Partial$Fecha <- gsub(tmp, tmp1, Partial$Fecha)
  remove(tmp, tmp1)
}
remove(i)
Partial1 <- rbind(F2007, F2008)
Partial1$Fecha <- tolower(Partial1$Fecha)
for (i in 1:12){
  tmp <- Months$Short[i]
  tmp1 <- Months$Number[i]
  Partial1$Fecha <- gsub(tmp, tmp1, Partial1$Fecha)
  remove(tmp, tmp1)
}
remove(i)

###Remove Redundants
Complete <- rbind(Partial1, Partial)
remove(list=ls(pattern="F"), Months, Partial, Partial1)


###As.Date
Complete$Fecha <- as.Date(x=Complete$Fecha, format="%d %m %Y")
Complete$Marcador <- as.character(Complete$Marcador)
Complete$GolesCasa <- as.numeric(substr(Complete$Marcador, 0,1))
Complete$GolesVisita <- as.numeric(substr(Complete$Marcador, 4,5))
Complete$Gana <- ifelse(Complete$GolesCasa > Complete$GolesVisita, "Casa", "Empate")
Complete$Gana <- ifelse(Complete$GolesCasa < Complete$GolesVisita, "Visita", Complete$Gana)
##Correct some team names
Complete$Local <- gsub("Estudiantes Tecos", "Tecos", Complete$Local)
Complete$Local <- gsub("Tecos", "Estudiantes", Complete$Local)
Complete$Local <- gsub("Querétaro FC", "Querétaro" , Complete$Local)
Complete$Local <- gsub("Monarcas Morelia", "Morelia" , Complete$Local)
Complete$Local <- gsub("Jaguares de Chiapas", "Jaguares" , Complete$Local)
Complete$Local <- gsub("Puebla FC", "Puebla" , Complete$Local)
Complete$Local <- gsub("Santos Laguna","Santos"  , Complete$Local)
Complete$Local <- gsub("Tigres de la UANL","Tigres" , Complete$Local)
Complete$Local <- gsub("Tigres UANL","Tigres" , Complete$Local)
Complete$Local <- gsub("UNAM","Pumas" , Complete$Local)
Complete$Local <- gsub("Toluca FC","Toluca" , Complete$Local)
Complete$Visitante <- gsub("Estudiantes Tecos", "Tecos", Complete$Visitante)
Complete$Visitante <- gsub("Tecos", "Estudiantes", Complete$Visitante)
Complete$Visitante <- gsub("Querétaro FC", "Querétaro" , Complete$Visitante)
Complete$Visitante <- gsub("Monarcas Morelia", "Morelia" , Complete$Visitante)
Complete$Visitante <- gsub("Jaguares de Chiapas", "Jaguares" , Complete$Visitante)
Complete$Visitante <- gsub("Puebla FC", "Puebla" , Complete$Visitante)
Complete$Visitante <- gsub("Santos Laguna","Santos"  , Complete$Visitante)
Complete$Visitante <- gsub("Tigres de la UANL","Tigres" , Complete$Visitante)
Complete$Visitante <- gsub("Tigres UANL","Tigres" , Complete$Visitante)
Complete$Visitante <- gsub("UNAM","Pumas" , Complete$Visitante)
Complete$Visitante <- gsub("Deportivo Toluca","Toluca" , Complete$Visitante)
Complete$Visitante <- gsub("San luis","San Luis" , Complete$Visitante)

### Write Final Dataframe to csv

write.csv(Complete, "out-data/Matches2007_2011.csv", row.names=FALSE)
