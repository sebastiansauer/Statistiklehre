####### Vorbereitung


# 1. Schritt: Pakete starten (jedes Mal, wenn Sie RStudio öffnen!)

library(dplyr)
library(ggplot2)
library(nycflights13)


# Fehler: Es gibt kein Paket "xyz"-- Sie müssen es erst noch installieren.


# 2. Schritt: Daten importieren/öffnen


data("flights")  # Strg-Shift-Enter





####### Los geht's


flights %>%  # "Hey R, nimm die Tabelle 'flights' UND-DANN"
  filter(dest == "PDX") "filter mir die Flüge mit destination PDX"


portland_flights <- flights %>% 
  filter(dest == "PDX")



flights %>%  # "Hey R, nimm die Tabelle 'flights' UND DANN..."
  filter(origin == "JFK" & (dest == "BTV" | dest == "SEA") & month >= 10)
View(btv_sea_flights_fall)


btv_sea_flights_fall <- flights %>% 
  filter(origin == "JFK" & (dest == "BTV" | dest == "SEA") & month >= 10)
View(btv_sea_flights_fall)




data("weather")


summary_temp <- weather %>% 
  summarize(mean = mean(temp, na.rm = TRUE), 
            std_dev = sd(temp, na.rm = TRUE))


summary_temp



summary_temp2 <- weather %>% 
  summarize(mean = mean(temp, na.rm = TRUE))


summary_temp3 <- weather %>% 
  summarize(mean = mean(temp, na.rm = TRUE), 
            std_dev = sd(temp, na.rm = TRUE),
            med = median(temp, na.rm = TRUE),
            max_temp = max(temp, na.rm = TRUE))



 
  weather %>%  # Hey R, nimm die Tabelle 'Wetter' UND DANN
  group_by(month) %>%  # Gruppiere nach 'Monat' UNDD DANN
  summarize(mean = mean(temp, na.rm = TRUE),  # Fasse zum MW zusammen ...
            std_dev = sd(temp, na.rm = TRUE)) -> summary_monthly_temp  
  # zum Schluss kipp alles in ne Tabelle mit namen 'summary_monthly_temp'.


summary_monthly_temp




flights <- flights %>% 
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours
  )


