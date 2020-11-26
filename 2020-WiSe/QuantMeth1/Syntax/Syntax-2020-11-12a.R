# D) Die Punkte im Streudiagramm sind stark überlagert.
# Wie kann man diese "Overplotting" verringern?

library(tidyverse)
library(gapminder)

data(""gapminder"")

gapminder %>% 
  mutate(gdpPercap_log = log(gdpPercap, base = 10)) %>% 
  ggplot() +
  aes(x = gdpPercap_log, y = lifeExp) +
  geom_point()


gapminder %>% 
  mutate(gdpPercap_log = log(gdpPercap, base = 10)) %>% 
  ggplot() +
  aes(x = gdpPercap_log, y = lifeExp) +
  geom_point(alpha = .2) +
  geom_hex()  # bin2d oder density2d



gapminder %>% 
  mutate(gdpPercap_log = log(gdpPercap, base = 10)) %>% 
  ggplot() +
  aes(x = gdpPercap_log, y = lifeExp) +
  geom_density2d()


gapminder %>% 
  mutate(gdpPercap_log = log(gdpPercap, base = 10)) %>% 
  ggplot() +
  aes(x = gdpPercap_log, y = lifeExp) +
  geom_density_2d_filled()




#B) Teilen Sie das Histogramm in mehrere Facetten auf, 
# entsprechend der Kontinente.




ggplot(data = gapminder, mapping = aes(x = lifeExp)) +
  geom_histogram() +
  facet_wrap( ~ continent)
