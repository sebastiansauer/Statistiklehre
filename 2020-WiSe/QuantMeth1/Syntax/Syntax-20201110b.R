

library(gapminder)  # falls noch nicht geladen
library(tidyverse)  # falls noch nicht geladen
data("gapminder")  # falls noch nicht geladen


gapminder %>% # "Pfeife" "UND-DANN"
  ggplot() +
  aes(x = country, y = lifeExp) +
  geom_boxplot()


# synomym:

# A)
gapminder %>% # "Pfeife" "UND-DANN"
  ggplot(mapping = aes(x = country, y = lifeExp) )+
  geom_boxplot()



# B)
gapminder %>% 
  ggplot() +
  aes(x = continent, y = lifeExp) +
  geom_boxplot()



# D)
gapminder %>% 
  ggplot() +
  aes(x = continent, y = lifeExp, fill = continent) +
  geom_boxplot()


# synonym:

ggplot(data = gapminder) +
  aes(x = continent, y = lifeExp, fill = continent) +
  geom_boxplot()

#gg: Grammar of graphics (Leland Wilson)




# C)


C) Erstellen Sie einen Boxplort für jedes *Kontinent* im Datensatz, 
um die Verteilung der Lebenserwartung zu visualisieren. 
Dieses Mal sollen Sie aber vorab den Datensatz zusammenfassen, 
so dass ein (zeilen-)reduzierter Datensatz entsteht, 
der für jeden Kontinent eine Zeile umfasst. Wie sinnvoll ist dieses Vorgehen?


gapminder %>% 
  group_by(continent) %>% 
  summarise(lifeExp_continent = median(lifeExp)) %>% 
  ggplot() +
  aes(x = continent, y = lifeExp_continent) +
  geom_boxplot()


# Facetten


gapminder %>% # "Pfeife" "UND-DANN"
  ggplot(mapping = aes(x = country, y = lifeExp) )+
  geom_boxplot() +
  facet_wrap( ~ continent)
