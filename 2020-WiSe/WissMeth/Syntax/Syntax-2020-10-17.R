
1+1

library(mosaic)  # Strg-Enter führt den Befehl aus 


data("Births78")  #synonym: 
data(Births78)

help("Births78")

#View(Births78)


# meine.analyse(mein.y ~ mein.x, data = Meine.Daten)  NUR PSEUDOCODE

gf_point(births ~ date, 
         data = Births78)  # S. S. Skript S. 88

# Leerzeichen fast überall wurscht, AUSNAHME:
# innerhalb eines Operators verboten. "me an()", "gf_    point()"

alter = 42
alter <- 42+3.14159267



# ~ MAC: Option-N (Tilde)



gf_point(births ~ date, 
         data = Births78, color = ~ wday)  # S. Skript S. 92



# Data Handling, "Datenjudo"


# `filter` filtert Zeilen (Beobachtungen)
Births78 %>% # UND-DANN-Befehl: Shortcut *Strg-Shift-M*
  filter(date == "1978-07-04")


# Übung: Filtern Sie den Juli!

Births78 %>% 
  filter(month == 7)

# ANFÜHRUNGSSTRICHE: 
# Text, d.h. auch Dinge, die R nicht als Variable/Objekt kennt
# Keine Anführungsstriche bei: R-Befehlen, Variablen/Objekte, die definiert sind
# bei Zahlen KEINE Anführungsstriche


geburten_juli <- Births78 %>%
  filter(month == 7)


# 7  ---- "7"


Births78 %>%  # UND-DANN-Befehl
  filter(month == 7) %>% 
  select(births)

# Diese Befehle erwarten Tabellen als Eingabe und geben
# Tabellen zurück!


Births78 %>%
  filter(date == "1978-07-04") %>% 
  select(births)


# Übung
# Nimm Births78 UND DANN ("PFEIFE")
# UND DANN filtere den Juli
# UND DANN filtere den Dienstag
# UND DANN wähle die Geburtenspalte aus


Births78 %>% 
  filter(month == 7) %>% 
  filter(wday == "Tue") %>% 
  select(births)

# identisch:
Births78 %>% 
  dplyr::filter(month == 7, wday == "Tue") %>% 
  select(births)



# Kap. 5 -- EDA -----------------------------------------------------------



# Kapitel 5


d_test <- read.csv2(file = "Anwendungsdaten.csv")  # mit Semikolon als Trennzeichen




d_train <- read.csv2(file = "data/Trainingsdaten.csv")  

download.file("https://goo.gl/whKjnl", 
              destfile = "tips.csv")

tips <- read.csv2("tips.csv")


mosaicplot(day ~ smoker, data = tips, color = TRUE)


?mosaicplot




