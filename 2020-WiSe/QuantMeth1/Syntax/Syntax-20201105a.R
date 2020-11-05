

############# 1. Vorbereitung

# Wichtig!
# Vor jeder Analyse _zwei_ Sachen beachten:
# 1. Pakete laden:

# jedes Mal, wenn Sie R öffnen, müssen Sie diese Befehle hier ausführen,
# um die Pakete zu starten
library(dplyr)
library(ggplot2)
library(nycflights13)  # hier wohnt der Datensatz "flights"
#library(tidyverse)


# Bei Fehler "es gibt kein Paket namens xyz" 
# --> Du musst das Paket xyz noch installieren!


# Paket installieren <--> Paket starten !!!!!!

# 2. Daten laden

data(flights)  # lädt den Datensatz "flights" in R.

# die sind hier jetzt schon automatisch geladen!
# da das Paket mit den Daten auch schon geladen ist.



############# 2. Jetzt geht's richtig los.




alaska_flights <- flights %>% 
  filter(carrier == "AS")




# Auf Deutsch

# Hey R! Nimm die Tabelle "flights",  UND-DANN ...
# filter, so dass in Spalte "carrier" so dass nur noch der Wert "AS" übrig bleibt.


# %>% -- Shortcut: Strg-Shift-M, Der "UND-DANN-Befehl"


flights_BTV_SEA <- flights %>%  # Hey R, nimm die Tabelle flights, UND-DANN
  filter(dest == "BTV" | dest == "SEA")
  # filter mir: alle Flüge mit Ziel BTV oder SEA


flights %>%  
  filter(! (dest == "BTV" | dest == "SEA"))
  # Alle Flüge, die NICHT nach BTV oder SEA gehen
  



not_BTV_SEA <- flights %>% 
  filter(!(dest == "BTV" | dest == "SEA"))

# synonym:

not_BTV_SEA2 <- 
flights %>% 
  filter( dest != "BTV" & dest != "SEA" )




View(not_BTV_SEA)







