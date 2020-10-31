
klausurerfolg <- c(1.1, 2.2, 3.3)

studis <- c("Anna", "Berta", "Carla")


meine_tabelle <- data.frame(ke = klausurerfolg,
                            studis = studis)


str(meine_tabelle)



# Variablen auslesen

studis
ke

klausurerfolg[1]
klausurerfolg[c(1,3)]

meine_tabelle[3,1]


klausurerfolg[c(1,3,1,2,99)]

klausurerfolg[c(1,3)]
klausurerfolg[c(TRUE, FALSE, TRUE)]
klausurerfolg[c(TRUE, TRUE, TRUE, FALSE)]

klausurerfolg > 3.0

klausurerfolg[klausurerfolg > 3.0]


noten <- c("Anna" = 1.1, "Berta" = 2.2, "Charlie" = 3.3)

noten["Charlie"]




antwort <- c("a", "b", "c")

str(antwort)


x <- C("Anna", "Dora" "Charlie")


