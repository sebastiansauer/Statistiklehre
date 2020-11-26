

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





A) Erstellen Sie ein Streudiagramm, 
das den Zusammenhang von Bruttosozialprodukt und Lebenserwartung widerspiegelt.


gapminder %>%  # Strg-Shift-M
  ggplot() +
  aes(x = gdpPercap, y = lifeExp) +
  geom_point()



B) Logarithmieren Sie die Variable zum 
Bruttosozialprodukt und erstellen 
Sie auf dieser Basis das Diagramm erneut.


log(100, base = 10)
log(1000, base = 10)




B) 


gapminder %>%  # Strg-Shift-M
  ggplot() +
  aes(x = log(gdpPercap, base = 10), y = lifeExp) +
  geom_point()

# synonym:


gapminder %>% 
  mutate(gdp_log = log(gdpPercap, base = 10)) %>% 
  ggplot() +
  aes(x = gdpPercap, y = lifeExp) +
  geom_point() 



D) Die Punkte im Streudiagramm sind stark überlagert. 
Wie kann man diese "Overplotting" verringern?
  
  
gapminder %>% 
  mutate(gdp_log = log(gdpPercap, base = 10)) %>% 
  ggplot() +
  aes(x = gdp_log, y = lifeExp) +
  geom_point(alpha = .5)  



E) Färben Sie die Punkte entsprechend nach ihrem Kontinent!
  
  
  gapminder %>% 
  mutate(gdp_log = log(gdpPercap, base = 10)) %>% 
  ggplot() +
  aes(x = gdp_log, y = lifeExp, color = continent) +
  geom_point(alpha = .5)    





x <- c(0.64 , 0.01, -1.33, -0.81,  0.22)
mean(x)




noten <- c(1.1, 2.2, 2.5, 2.7, 3.0, 3.3, 3.5, 3.7, 4.3)

noten >= 3.3

noten[c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE,  TRUE,  TRUE)]

noten[noten >= 3.3]





x <-0
y <- 0

x <- x+y

y = x

x <- x+1

x <- sqrt(x)
x
