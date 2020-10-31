

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


studis[c(1,2)]

studis[c(1,2,1,1,1,1,1)]


bestanden3 <- c("Studi1" =  "ja",
                "Studi2" = "nein",
                "Studi3" =  "ja",
                "Studi4" = "ja")

bestanden3

bestanden3[1]
bestanden3["Studi1"]

bestanden3[c(TRUE, FALSE, TRUE, FALSE)]

bestanden3 == "ja"


bestanden3[c(TRUE, FALSE, TRUE, TRUE)]
bestanden3[bestanden3 == "ja"]



test <- factor(c("#", "ä", "a", "A", "1", "!"))
str(test)
levels(test)



x <- C("Anna", "Berta" "Carla")
x <- c("Anna", "Berta", "Carla")
