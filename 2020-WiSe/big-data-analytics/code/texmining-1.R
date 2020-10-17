

## ----libs-textmining, message=FALSE, error = FALSE----------------------------
library(tidyverse)  # Datenjudo
library(stringr)  # Textverarbeitung
library(tidytext)  # Textmining
library(lsa)  # Stopwörter
library(SnowballC)  # Wörter trunkieren
library(wordcloud)  # Wordcloud anzeigen


## ----libs-textmining-hidden, echo = FALSE-------------------------------------
library(knitr)


## -----------------------------------------------------------------------------
text <- c("Wir haben die Frauen zu Bett gebracht,",
          "als die Männer in Frankreich standen.",
          "Wir hatten uns das viel schöner gedacht.",
          "Wir waren nur Konfirmanden.")


## -----------------------------------------------------------------------------
text_df <- data_frame(Zeile = 1:4,
                      text = text)


## ----echo = FALSE-------------------------------------------------------------
knitr::kable(text_df)


## ----eval = FALSE-------------------------------------------------------------
## text <- read_lines("Brecht.txt")




## -----------------------------------------------------------------------------
text_df %>%
  unnest_tokens(output = wort, input = text) -> tidytext_df

head(tidytext_df)


## -----------------------------------------------------------------------------
tidytext_df %>%
  filter(str_detect(wort, "[a-z]")) -> tidytext_df_lowercase


## -----------------------------------------------------------------------------
osf_link <- paste0("https://osf.io/b35r7/?action=download")
afd <- read_csv(osf_link)


## ----eval = FALSE-------------------------------------------------------------
## afd_pfad <- "data/afd_programm.pdf"
## afd_raw <- pdf_text(afd_pfad)
## afd <- data_frame(Zeile = 1:96,
##                   afd_raw)


## -----------------------------------------------------------------------------
afd %>%
  unnest_tokens(output = token, input = content) %>%
  dplyr::filter(str_detect(token, "[a-z]")) -> afd_long


## -----------------------------------------------------------------------------
afd_long %>%
  na.omit() %>%  # fehlende Werte löschen
  count(token, sort = TRUE)


## -----------------------------------------------------------------------------
data(stopwords_de, package = "lsa")

stopwords_de <- data_frame(word = stopwords_de)

# Für das Joinen werden gleiche Spaltennamen benötigt
stopwords_de <- stopwords_de %>%
  rename(token = word)

afd_long %>%
  anti_join(stopwords_de) -> afd_no_stop


## -----------------------------------------------------------------------------
afd_no_stop %>%
  count(token, sort = TRUE) -> afd_count


## ----echo = FALSE-------------------------------------------------------------
afd_count %>%
  top_n(10) %>%
  knitr::kable(caption = "Die häufigsten Wörter")


## -----------------------------------------------------------------------------
afd_no_stop %>%
  mutate(token_stem = wordStem(.$token, language = "de")) %>%
  count(token_stem, sort = TRUE) -> afd_count_stemmed

afd_count_stemmed %>%
  top_n(10) %>%
  knitr::kable(caption = "Die häufigsten Wörter - mit 'stemming'")


## ----show-wordcloud-FALSE, fig.cap = "Eine Wordwolke zum AfD-Parteiprogramm", eval = FALSE----
## wordcloud(words = afd_count_stemmed$token_stem,
##           freq = afd_count_stemmed$n,
##           max.words = 100,
##           scale = c(2,.5),
##           colors=brewer.pal(6, "Dark2"))


## ----show-wordcloud, fig.cap = "Eine Wordwolke zum AfD-Parteiprogramm", echo = FALSE----
knitr::include_graphics("/images/textmining/wordcloud1.png")


## -----------------------------------------------------------------------------
afd_count_stemmed %>%
  top_n(30) %>%
  ggplot() +
  aes(x = reorder(token_stem, n), y = n) +
  geom_col() +
  labs(title = "mit Trunkierung") +
  coord_flip() -> p1

afd_count %>%
  top_n(30) %>%
  ggplot() +
  aes(x = reorder(token, n), y = n) +
  geom_col() +
  labs(title = "ohne Trunkierung") +
  coord_flip() -> p2


## ----p-word-freq, echo = FALSE, fig.cap = "Worthäufigkeiten im AfD-Parteiprogramm"----
gridExtra::grid.arrange(p1, p2, ncol = 2)


## Richtig oder Falsch!?

##
## 1. Unter einem Token versteht man die größte Analyseeinheit in einem Text.

## 1. In einem tidytext Dataframe steht jedes Wort in einer (eigenen) Zeile.

## 1. Eine hinreichende Bedingung für einen tidytext Dataframe ist es, dass in jeder Zeile ein Wort steht (beziehen Sie sich auf den tidytext Dataframe wie in diesem Kapitel erörtert).

## 1. Gibt es 'Stop-Wörter' in einem Dataframe, dessen Text analysiert wird, so kommt es - per definitionem - zu einem Stop.

## 1. Mit dem Befehl `unnest_tokens` kann man einen tidytext Dataframe erstellen.

## 1. Balkendiagramme sind sinnvolle und auch häufige Diagrammtypen, um die häufigsten Wörter (oder auch Tokens) in einem Corpus darzustellen.

## 1. In einem 'tidy text Dataframe' steht in jeder Zeile ein Wort (token) *aber nicht* die Häufigkeit des Worts im Dokument.

## 1. Unter 'Stemming' versteht man (bei der Textanalyse), die Etymologie eines Wort (Herkunft) zu erkunden.

##
