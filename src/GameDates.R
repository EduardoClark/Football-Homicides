###################################
### Author:Eduardo Clark
### Project: Homicides and Fútbol
### Date: September 2013
### For mediotiempo.com
###################################

######### Get Game Dates

##Apertura 2007
Jornadas <- "http://www.terra.com.mx/futbol_mexicano/articulo/366302/Calendario+++Apertura+2007.htm"
Jornadas <- c(Jornadas, paste("http://www.terra.com.mx/futbol_mexicano/articulo/366302/Calendario+++Apertura+2007.htm&paginaid=", 1:21, sep=""))
Extractor <- function(URL){
  frame <- ldply(readHTMLTable(htmlParse(URL, encoding="UTF"))[9])
}
Info <- ldply(lapply(Jornadas, Extractor))
Info$.id <- NULL
Info <- subset(Info, select=1:6)
colnames(Info) <- c("Fecha", "Hora", "Local", "Marcador", "Visitante", "Estadio")
Info$Fecha <- as.character(Info$Fecha)
for (i in 1:nrow(Info)){
  tmp1 <- Info[i,1]
  Info [i,1] <- ifelse(tmp1=="",  Info[i-1,1], tmp1)
  remove(tmp1)
}
remove(i, Extractor, Jornadas)
Info$Fecha <- paste(Info$Fecha, "2007", sep=" ")
Info <- subset(Info, is.na(Info$Local)==FALSE)
Info <- subset(Info, Info$Local!="")
Info$Local <- as.character(Info$Local)
Info$Visitante <- as.character(Info$Visitante)
Info$Local <- gsub("\\*", "", Info$Local)
Info$Visitante <- gsub("\\*", "", Info$Visitante)
Info$Visitante <- gsub("\\+", "", Info$Visitante)
Info$Fecha <- gsub("\\.", "", Info$Fecha)
Info$Fecha <- gsub("\\-", " ", Info$Fecha)


###Clausura 2007
URL <- "http://www.esmas.com/sef/futbol/tor27/jor27-jornadas.html"
HTMLDOC <- readHTMLTable(htmlParse(URL, encoding="UTF"))
C2007 <- ldply(HTMLDOC[6])
for (i in 7:22){
  tmp <- ldply(HTMLDOC[i])
  C2007 <- rbind(C2007, tmp)
  remove(tmp)
}
remove(i, HTMLDOC, URL)
C2007$.id <- NULL
colnames(C2007) <- c("Fecha", "Hora", "Local", "Marcador", "Visitante", "Estadio")
C2007$Fecha <- substr(C2007$Fecha, 3, 1000)
C2007$Fecha <- gsub("-", " ", C2007$Fecha)
F2007 <- rbind(C2007, Info)
F2007$Fecha <- paste(F2007$Fecha, "2007", sep=" ")
remove(C2007, Info)

#### Clasura 2008
URL <- "http://www.esmas.com/sef/futbol/tor31/jor31-jornadas.html"
HTMLDOC <- readHTMLTable(htmlParse(URL, encoding="UTF"))
C2007 <- ldply(HTMLDOC[6])
for (i in 7:22){
  tmp <- ldply(HTMLDOC[i])
  C2007 <- rbind(C2007, tmp)
  remove(tmp)
}
remove(i, HTMLDOC, URL)
C2007$.id <- NULL
colnames(C2007) <- c("Fecha", "Hora", "Local", "Marcador", "Visitante", "Estadio")
C2007$Fecha <- substr(C2007$Fecha, 3, 1000)
C2007$Fecha <- gsub("-", " ", C2007$Fecha)

### Apertura 2008
URL <- "http://www.esmas.com/futbol/estadisticas/apertura-2008/jorapertura-2008-jornadas.html"
HTMLDOC <- readHTMLTable(htmlParse(URL, encoding="UTF"))
A2008 <- ldply(HTMLDOC[6])
for (i in 7:22){
  tmp <- ldply(HTMLDOC[i])
  A2008 <- rbind(A2008, tmp)
  remove(tmp)
}
remove(i, HTMLDOC, URL)
A2008$.id <- NULL
colnames(A2008) <- c("Fecha", "Hora", "Local", "Marcador", "Visitante", "Estadio")
A2008$Fecha <- substr(A2008$Fecha, 3, 1000)
A2008$Fecha <- gsub("-", " ", A2008$Fecha)
F2008 <- rbind(C2007,A2008)
F2008$Fecha <- paste(F2008$Fecha, "2008", sep=" ")
remove(A2008, C2007)

##Clausura 2009
URL <- "http://es.wikipedia.org/wiki/Torneo_Clausura_2009_%28M%C3%A9xico%29#Jornada_1"
HTMLDOC <- readHTMLTable(htmlParse(URL, encoding="UTF-8"))
jor1 <- function(URL){ldply(HTMLDOC[URL])}
C2009 <- ldply(lapply(11:27, jor1))
remove(jor1, HTMLDOC, URL)
C2009$.id <- NULL
C2009$Fecha <- gsub(" de", "", C2009$Fecha)
C2009$Partido <- gsub(":", " - ", C2009$Partido)
C2009 <- subset(C2009, select=)

### Apertura 2009
URL <- "http://es.wikipedia.org/wiki/Torneo_Apertura_2009_%28M%C3%A9xico%29"
HTMLDOC <- readHTMLTable(htmlParse(URL, encoding="UTF-8"))
jor1 <- function(URL){ldply(HTMLDOC[URL])}
A2009 <- ldply(lapply(18:34, jor1))
remove(jor1, HTMLDOC, URL)
A2009$.id <- NULL
A2009$Fecha <- gsub(" de", "", A2009$Fecha)
A2009$Partido <- gsub(":", " - ", A2009$Partido)
F2009 <- rbind(C2009, A2009)
remove(A2009, C2009)
colnames(F2009) <- c("Fecha", "Estadio", "Local", "Marcador", "Visitante")
F2009$Hora <- "NA"
F2009$Fecha <- paste(F2009$Fecha, "2009", sep=" ")
F2009 <- subset(F2009, select=c(1:5,20))


###Clausura 2010
URL <- "http://es.wikipedia.org/wiki/Torneo_Clausura_2010_%28M%C3%A9xico%29"
HTMLDOC <- readHTMLTable(htmlParse(URL, encoding="UTF-8"))
jor1 <- function(URL){ldply(HTMLDOC[URL])}
C2010 <- ldply(lapply(14:30, jor1))
remove(jor1, HTMLDOC, URL)
C2010$.id <- NULL
C2010$Fecha <- gsub(" de", "", C2010$Fecha)
C2010$Partido <- gsub(":", " - ", C2010$Partido)

###Apertura 2010
URL <- "http://es.wikipedia.org/wiki/Torneo_Apertura_2010_%28M%C3%A9xico%29"
HTMLDOC <- readHTMLTable(htmlParse(URL, encoding="UTF-8"))
jor1 <- function(URL){ldply(HTMLDOC[URL])}
A2010 <- ldply(lapply(19:35, jor1))
remove(jor1, HTMLDOC, URL)
A2010$.id <- NULL
A2010$Fecha <- gsub(" de", "", A2010$Fecha)
A2010$Partido <- gsub(":", " - ", A2010$Partido)
Splitter <- regexpr(":", A2010$Fecha)
A2010$Fecha <- substr(A2010$Fecha, 0, Splitter-3)
F2010 <- rbind(C2010, A2010)
remove(A2010, C2010, Splitter)
colnames(F2010) <- c("Fecha", "Estadio", "Local", "Marcador", "Visitante")
F2010$Hora <- "NA"
F2010$Fecha <- paste(F2010$Fecha, "2010", sep=" ")
F2010 <- subset(F2010, select=c(1:5,20))

#### Clausura 2011
URL <- "http://es.wikipedia.org/wiki/Torneo_Clausura_2011_%28M%C3%A9xico%29"
HTMLDOC <- readHTMLTable(htmlParse(URL, encoding="UTF-8"))
jor1 <- function(URL){ldply(HTMLDOC[URL])}
C2011 <- ldply(lapply(19:35, jor1))
remove(jor1, HTMLDOC, URL)
C2011$.id <- NULL
C2011$Fecha <- gsub(" de", "", C2011$Fecha)
C2011$Partido <- gsub(":", " - ", C2011$Partido)
Splitter <- regexpr(":", C2011$Fecha)
C2011$Hora <- substr(C2011$Fecha, Splitter-2, Splitter+2)
C2011$Fecha <- substr(C2011$Fecha, 0, Splitter-4)
colnames(C2011) <- c("Fecha", "Estadio", "Local", "Marcador", "Visitante", "Hora")
remove(Splitter)


###Apertura 2011
URL <- "http://es.wikipedia.org/wiki/Torneo_Apertura_2011_%28M%C3%A9xico%29"
HTMLDOC <- readHTMLTable(htmlParse(URL, encoding="UTF-8"))
jor1 <- function(URL){ldply(HTMLDOC[URL])}
A2011 <- ldply(lapply(20:36, jor1))
remove(jor1, HTMLDOC, URL)
A2011$.id <- NULL
A2011 <- subset(A2011, A2011$V1!="Local")
A2011$V7 <- NULL
A2011$V6 <- substr(A2011$V6, 0,5)
A2011$V5 <- gsub(" de", "", A2011$V5)
colnames(A2011) <- c("Local", "Marcador", "Visitante", "Estadio", "Fecha", "Hora")
A2011 <- subset(A2011, select=1:6)
C2011 <- subset(C2011, select=1:6)
F2011 <- rbind(C2011, A2011)
remove(A2011, C2011)
F2011$Fecha <- paste(F2011$Fecha, "2011", sep=" ")





