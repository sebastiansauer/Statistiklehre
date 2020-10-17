
library(mosaic)




## ---- eval=FALSE------------------------------
## install.packages("mosaic")


## ---- eval = FALSE----------------------------
## analysiere( y  # ggfs. abhängige Variable
##             ~  x # unabhängige Variable(n)
##             | z, # ggfs. bedingende (gruppierende) Variable(n)
##             Optionen, # ggfs. weitere Optionen
##             data = meine_daten ) # Datensatz



## ---- message=TRUE----------------------------
library(mosaic)


## ---------------------------------------------
data(Births78)


## ---- eval=FALSE------------------------------
## ?Births78


## ---- eval=FALSE------------------------------
## View(Births78)


## ---- eval = FALSE----------------------------
## meine.analyse(mein.y ~ mein.x, data = Meine.Daten)


## ----R_gfpoint, fig.align="center", out.width="60%"----
gf_point(births ~ date, data = Births78)


## ----R_gfpoint-ex, echo=FALSE, fig.align="right", out.width="20%"----


## ----eval = FALSE-----------------------------
## inspect(Births78)


## ----echo = FALSE-----------------------------
mosaic::inspect(Births78)


## ----eval=FALSE-------------------------------
##
## gf_point(births ~ date, color = ~ mein.z, data = Births78)
##


## ---- fig.align="center", out.width="70%"-----
gf_point(births ~ date, color = ~ wday, data = Births78)


## ----births78-filter, eval=FALSE--------------
## Births78 %>%
##   filter(date == "1978-07-04")


## ----ref.label="births78-filter", echo=FALSE----


## ---------------------------------------------
Births78 %>%
  filter(date == "1978-07-04") %>%
  select(births)


## ---------------------------------------------
mean(births ~ 1, data = Births78)


## ---------------------------------------------
mean( ~ births, data = Births78)


## ---------------------------------------------
mean(births ~ wday, data = Births78)


## ----eval=FALSE, message=TRUE, warning=FALSE, error=FALSE----
## mean(births ~ 1, data = Births78)


## ----str-birth78, eval=FALSE------------------
## str(Births78)


## ----ref.label="str-birth78", echo=FALSE------




