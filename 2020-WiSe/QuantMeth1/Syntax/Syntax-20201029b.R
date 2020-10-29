

studis <- c("Anna", "Berta", "Carla", "Dora")

bestanden <- c(TRUE, FALSE, TRUE, TRUE)

bestanden2 <- c("ja", "nein", "ja", "ja")


klausur <- data.frame(
  name = studis,
  bestanden = bestanden
)


studis

studis[2]
klausur[4,2] # Zeile, Spalte

str(studis)
str(klausur)
