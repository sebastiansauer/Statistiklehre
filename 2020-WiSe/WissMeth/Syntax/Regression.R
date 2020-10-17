

library(mosaic)



## ----resample-shuffle-setup, echo=FALSE-------
set.seed(2009)
x <- 1:15
y <- 1:15
df <- tibble(x=x, y=y)

my_point <- function(formula, data, caption="", jitter=FALSE) {
  gf_point(gformula=formula,
           color = ~y,
           show.legend = FALSE,
           caption=caption,
           xlab = "x",
           ylab = "y",
           size= 2.5,
           stroke= 1,
           data=data) %>%
    gf_refine(scale_y_discrete(limits=y)) %>%
    gf_refine(scale_x_discrete(limits=x))
}


## ----fig-low-high-rsquared, echo = FALSE, fig.align="center", out.width="60%", fig.asp = 0.5----
df1 <- tibble(
  x = rnorm(100),
  y = x + rnorm(100, mean = 0, sd = .47)
)

lm1 <- lm(y ~ x, data = df1)

df1 <- df1 %>%
  mutate(pred = predict(lm1))

df1_r2 <- rsquared(lm1) %>% round(2)

p1 <- df1 %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_lm() +
  geom_segment(aes(x = x,
                   xend = x,
                   y = y,
                   yend = pred),
               color = "grey60") +
  labs(title = bquote(R^2 == .(df1_r2)))+
  theme(axis.text=element_blank())



df2 <- tibble(
  x = rnorm(100),
  y = x + rnorm(100, mean = 0, sd = 2)
)

lm2 <- lm(y ~ x, data = df2)

df2 <- df2 %>%
  mutate(pred = predict(lm2))

df2_r2 <- rsquared(lm2) %>% round(2)

p2 <- df2 %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_lm() +
  geom_segment(aes(x = x,
                   xend = x,
                   y = y,
                   yend = pred),
               color = "grey60") +
  labs(title = bquote(R^2 == .(df2_r2))) +
  theme(axis.text=element_blank())


gridExtra::grid.arrange(p1, p2, nrow = 1)

rm(df1)
rm(df2)
rm(df1_r2)
rm(df2_r2)
rm(p1)
rm(p2)



## ----echo=FALSE, out.width = "20%", fig.align="right"----
knitr::include_graphics(file.path(pathToImages, "maschine.jpg"), error=FALSE)


## ----showDownloadCSV, eval=FALSE, message=FALSE, cache=FALSE----
## download.file("https://goo.gl/whKjnl", destfile = "tips.csv")
## tips <- read.csv2("tips.csv")
## # Alternativ - heruntergeladene Datei einlesen:
## # tips <- read.csv2(file.choose())
##
## library(mosaic) # Paket laden


## ---- fig.align="center", out.width="66%"-----
gf_point(tip ~ total_bill, data = tips)


## ---- echo=FALSE, fig.align="right", out.width="20%"----
gf_point(tip ~ total_bill, data = tips)


## ----erzeuge-erglm1---------------------------
# Speichere Ergebnis der Regression lm() in "erglm1"
erglm1 <- lm(tip ~ # abhängige Variable
             total_bill, # unabhängige Variable(n)
             data = tips) # Datensatz

erglm1


## ----plotte-erglm1, fig.align="center", out.width="66%"----
plotModel(erglm1)


## ---- echo=FALSE, fig.align="center", out.width="80%"----
intercept <- coef(erglm1)[1]
slope <- coef(erglm1)[2]

best_fit_plot <- ggplot(data = tips, mapping = aes(x = total_bill, y = tip)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  annotate("point", x = 38.01, y = 3, color = "blue", size = 3) +
  annotate("segment", x = 38.01, xend = 38.01, yend = 3, y = intercept + slope * 38.01,
           color = "blue", arrow = arrow(length = unit(0.03, "npc"))) +
  annotate("point", x = 9.60, y = 4, color = "blue", size = 3) +
  annotate("segment", x = 9.60, xend = 9.60, yend = 4, y = intercept + slope * 9.60,
           color = "blue", arrow = arrow(length = unit(0.03, "npc")))
best_fit_plot


## ----summary-erglm1---------------------------
# Zeige Zusammenfassung von "erglm1"
summary(erglm1)


## ---------------------------------------------
# Datensatz mit neuer Beobachtung
x0 <- data.frame(total_bill = 10)

# Prognose
predict(erglm1, # Modell
        newdata = x0)


## ----echo=FALSE, out.width = "60%", fig.align="center", cache=FALSE----
# Lizenzworkaround:
extern_image_include("https://www.causeweb.org/cause/sites/default/files/caption_contest/2019/Caption-Contest_05-2019.jpg", "cartoon0519.jpg", pathToImages)


## ---- echo=FALSE, fig.align="center", out.width="65%"----
data("anscombe")
p1 <- plotModel(lm(y1~x1, data = anscombe))
p2 <- plotModel(lm(y2~x2, data = anscombe))
p3 <- plotModel(lm(y3~x3, data = anscombe))
p4 <- plotModel(lm(y4~x4, data = anscombe))
gridExtra::grid.arrange(p1,p2,p3,p4)




## ----resample-use-print-original, echo=FALSE, out.width="80%"----
my_point(y ~ x,
         data=df, caption="original" )


## ----resample-use-print-original-cap, echo=TRUE, eval=FALSE----
## gf_point(y ~ x, data=df)


## ----resample-use-print-resample-1, echo=FALSE, out.width="80%"----
my_point(y ~ jitter(x), data=resample(df), caption="resample" )

## ----resample-use-print-resample-1-cap, echo=TRUE, eval=FALSE----
## gf_point(y ~ x, data=resample(df))


## ----resample-use-print-resample-2, echo=FALSE, out.width="80%"----
my_point(y ~ jitter(x), data=resample(df), caption="resample" )

## ----resample-use-print-resample-2-cap, echo=TRUE, eval=FALSE----
## gf_point(y ~ x, data=resample(df))


## ----tips-resample-orig, echo=TRUE, eval=FALSE----
## # Original
## tips %>% head()


## ----ref.label="tips-resample-orig", eval=TRUE, echo=FALSE----


## ----tips-resample-resample, echo=TRUE, eval=FALSE----
## # Reproduzierbarkeit
## set.seed(1896)
## # Resample
## tips %>% head() %>% resample()


## ----ref.label="tips-resample-resample", eval=TRUE, echo=FALSE----


## ----simuboot, echo=FALSE, fig.align="center", out.width="80%"----
set.seed(1896)

mytips <- tips %>%
  select(tip, total_bill) %>%
  sample_n(40)

boottips <- do(12) * resample(mytips)

gf_point(tip ~ total_bill | .index, data = boottips) %>%
  gf_lm()


## ---- echo=FALSE, fig.align="right", out.width="20%"----
gf_point(tip ~ total_bill | .index, data = boottips) %>%
  gf_lm()


## ----resampling-im-modell-original, eval=FALSE, echo=FALSE----
## # Original
## mosaic::do(1) * lm(tip ~ total_bill, data = tips)


## ----resampling-im-modell-original-no-eval, eval=FALSE, echo=TRUE----
## # Original
## do(1) * lm(tip ~ total_bill, data = tips)


## ----ref.label="resampling-im-modell-original", echo=FALSE, eval=TRUE----


## ----resampling-im-modell-resample, eval=FALSE, echo=FALSE----
## # Reproduzierbarkeit
## set.seed(1896)
## # Resample
## mosaic::do(3) * lm(tip ~ total_bill, data = resample(tips))


## ----resampling-im-modell-resample-no-eval, eval=FALSE, echo=TRUE----
## # Reproduzierbarkeit
## set.seed(1896)
## # Resample
## do(3) * lm(tip ~ total_bill, data = resample(tips))


## ----ref.label="resampling-im-modell-resample", echo=FALSE, eval=TRUE----


## ----do-resample-lm-many-times-no-eval, eval = FALSE----
## # Reproduzierbarkeit
## set.seed(1896)
## Bootvtlg <- do(10000) *
##   lm(tip ~ total_bill, data = resample(tips))


## ----do-resample-lm-many-times, echo = FALSE----
# Reproduzierbarkeit
set.seed(1896)
Bootvtlg <- mosaic::do(10000) *
  lm(tip ~ total_bill, data = resample(tips))


## ----  fig.align="center", out.width="60%"----
gf_histogram( ~ total_bill, data = Bootvtlg)


## ---------------------------------------------
sd( ~ total_bill, data = Bootvtlg)


## ---------------------------------------------
quantile( ~ total_bill, data = Bootvtlg,
          probs = c(0.025, 0.975))


## ----sample-use-print-original, echo=FALSE, out.width="80%"----
my_point(y ~ x,          data=df, caption="original" )

## ----sample-use-print-original-cap, echo=TRUE, eval=FALSE----
## gf_point(y ~ x, data=df)


## ----sample-use-print-shuffle-1, echo=FALSE, out.width="80%"----
my_point(y ~ shuffle(x), data=df, caption="shuffled" )

## ----sample-use-print-shuffle-1-cap, echo=TRUE, eval=FALSE----
## gf_point(y ~ shuffle(x), data=df)


## ----sample-use-print-shuffle-2, echo=FALSE, out.width="80%"----
my_point(y ~ shuffle(x), data=df, caption="shuffled" )

## ----sample-use-print-shuffle-2-cap, echo=TRUE, eval=FALSE----
## gf_point(y ~ shuffle(x), data=df)


## ----tips-shuffle-orig, echo=TRUE, eval=FALSE----
## # Original
## tips %>% head()


## ----ref.label="tips-shuffle-orig", eval=TRUE, echo=FALSE----


## ----tips-shuffle-shuffle, echo=TRUE, eval=FALSE----
## # Reproduzierbarkeit
## set.seed(1896)
## # Shuffle
## tips %>% head() %>% shuffle()


## ----ref.label="tips-shuffle-shuffle", eval=TRUE, echo=FALSE----


## ----simushuffle, echo=FALSE, fig.align="center", out.width="80%"----
shuffletips <- do(12) * mytips
shuffletips <- shuffletips %>%
  group_by(.index) %>%
  mutate(total_bill = shuffle(total_bill)) %>%
  ungroup() %>%
  select(-.row)

orgtips <- mytips %>%
  mutate(.index = 8)

shuffletips[shuffletips$.index==8, ] <- orgtips

gf_point(tip ~ total_bill | .index, data = shuffletips) %>%
  gf_lm()


## ---- echo=FALSE, fig.align="right", out.width="20%"----
gf_point(tip ~ total_bill | .index, data = shuffletips) %>%
  gf_lm()


## ----shuffling-im-modell-original-no-eval, eval=FALSE, echo=TRUE----
## # Original
## do(1) * lm(tip ~ total_bill, data = tips)


## ----shuffling-im-modell-original, eval=FALSE, echo=FALSE----
## # Original
## mosaic::do(1) * lm(tip ~ total_bill, data = tips)


## ----ref.label="shuffling-im-modell-original", echo=FALSE, eval=TRUE----


## ----shuffling-im-modell-resample-no-eval, eval=FALSE, echo=TRUE----
## # Reproduzierbarkeit
## set.seed(1896)
## # Resample
## do(3) * lm(tip ~ shuffle(total_bill), data = tips)


## ----shuffling-im-modell-resample, eval=FALSE, echo=FALSE----
## # Reproduzierbarkeit
## set.seed(1896)
## # Resample
## mosaic::do(3) * lm(tip ~ shuffle(total_bill), data = tips)


## ----ref.label="shuffling-im-modell-resample", echo=FALSE, eval=TRUE----


## ----do-shuffle-lm-many-times-no-eval, eval = FALSE----
## set.seed(1896) # Reproduzierbarkeit
## Nullvtlg <- do(10000) *
##   lm(tip ~ shuffle(total_bill), data = tips)


## ----do-shuffle-lm-many-times, echo = FALSE----
set.seed(1896) # Reproduzierbarkeit
Nullvtlg <- mosaic::do(10000) *
  lm(tip ~ shuffle(total_bill), data = tips)


## ----  fig.align="center", out.width="60%"----
gf_histogram( ~ total_bill, data = Nullvtlg)


## ---- echo=FALSE, fig.align="right", out.width="20%"----
gf_histogram( ~ total_bill, data = Nullvtlg) %>%
  gf_vline(xintercept= ~coef(lm(tip ~ total_bill, data = tips))[2])


## ---------------------------------------------
coef(erglm1) # Koeffizienten
coef(erglm1)[2] # Steigung
abs(coef(erglm1))[2] # Absolutbetrag der Steigung
effektdach <- abs(coef(erglm1))[2] # Zuweisung
effektdach


## ---------------------------------------------
Nullvtlg <- Nullvtlg %>%
  mutate(effekt0 = abs(total_bill))


## ---------------------------------------------
prop( ~ (effekt0 >= effektdach), data = Nullvtlg)


## ----echo=FALSE, out.width = "50%", fig.align="center", cache=FALSE----
set.seed(1896)
x <- runif(50, -3, 3)
ya <- scale(5-x+rnorm(50))
yb <- scale(x^2+0.1*rnorm(50))
yc <- scale(exp(x)+0.1*rnorm(50))
yd <- scale(1/(1+exp(-x))+0.01*rnorm(50))
pa <- gf_point(ya~x, title="A")
pb <- gf_point(yb~x, title="B")
pc <- gf_point(yc~x, title="C")
pd <- gf_point(yd~x, title="D")
gridExtra::grid.arrange(pa,pb,pc,pd)


## ----do-tips2, echo=FALSE, fig.align="center", out.width="70%"----
tipsModified <- data.frame(tips)
tipsModified[1, "total_bill"] <- 1000
tipsModified[1, "tip"] <- 0
plotModel(lm(tip ~ total_bill, data = tipsModified))




## ----  fig.align="center", out.width="70%"----
gf_histogram( ~ resid(erglm1))


## ----  fig.align="center", out.width="70%"----
gf_qq( ~ resid(erglm1)) %>% gf_qqline()


## ---- echo=FALSE, , fig.align="right", out.width="20%"----
p1 <- gf_dhistogram( ~ resid(erglm1)) %>% gf_fitdistr(dist = "dnorm")
p2 <- gf_qq( ~ resid(erglm1)) %>% gf_qqline()
gridExtra::grid.arrange(p1,p2, ncol=2)


## ----echo=FALSE, out.width = "35%", fig.align="center", cache=FALSE----
set.seed(1896)
x <- runif(50, -3, 3)
ya <- scale(5-x+rnorm(50))
yb <- scale(x^2+0.1*rnorm(50))
yc <- scale(exp(x)+0.1*rnorm(50))
yd <- scale(1/(1+exp(-x))+0.01*rnorm(50))
la <- lm(ya~x)
lb <- lm(yb~x)
lc <- lm(yc~x)
ld <- lm(yd~x)
pa <- gf_point(resid(la)~fitted(la), title = "A") %>%
  gf_labs(x="Angepasste Werte", y="Residuum")
pb <- gf_point(resid(lb)~fitted(lb), title = "B") %>%
  gf_labs(x="Angepasste Werte", y="Residuum")
pc <- gf_point(resid(lc)~fitted(lc), title = "C") %>%
  gf_labs(x="Angepasste Werte", y="Residuum")
pd <- gf_point(resid(ld)~fitted(ld), title = "D") %>%
  gf_labs(x="Angepasste Werte", y="Residuum")
gridExtra::grid.arrange(pa,pb,pc,pd)


## ----  fig.align="center", out.width="50%"----
gf_point(resid(erglm1) ~ fitted(erglm1))


## ---- echo=FALSE, fig.align="right", out.width="20%"----
gf_point(resid(erglm1) ~ fitted(erglm1))


## ---------------------------------------------
predict(erglm1, # Modell
        # Neue Beobachtung mit x=1000:
        newdata = data.frame(total_bill = 1000),
        # Prognoseintervall:
        interval  = "prediction")


## ----mean-tip-smoker, echo=TRUE, eval=FALSE----
## mean(tip ~ smoker, data = tips)


## ----ref.label="mean-tip-smoker", eval=TRUE, echo=FALSE----


## ----diffmean-tip-smoker, echo=TRUE, eval=FALSE----
## diffmean(tip ~ smoker, data = tips)

## ---- fig.align="center", out.width="40%"-----


## ----ref.label="diffmean-tip-smoker", eval=TRUE, echo=FALSE----


## ----gf-point-tip-smoker, echo=TRUE, eval=FALSE, fig.asp = 0.5----
## gf_point(tip ~ smoker, data = tips,
##          position = "jitter",
##          width = 0.1, height = 0)


## ----ref.label="gf-point-tip-smoker", eval=TRUE, echo=FALSE, out.width = "65%", fig.align="center"----


## ---- echo=FALSE------------------------------
contrasts(as.factor(tips$smoker)) %>% knitr::kable()


## ---- echo=FALSE------------------------------
contrasts(as.factor(tips$day)) %>% knitr::kable()


## ----tips-erglm2-summary, echo=TRUE, eval=FALSE----
## erglm2 <- lm(tip ~ smoker, data = tips)
## summary(erglm2)


## ----ref.label="tips-erglm2-summary", eval = TRUE, echo = FALSE----


## ----lm-boot-no-eval, out.width = "40%", fig.align="center", eval = FALSE----
## set.seed(1896)
## Bootvtlg <- do(10000)* lm(tip ~ smoker, data = resample(tips))
## gf_histogram( ~ smokerYes, data = Bootvtlg) %>%
##   gf_vline(xintercept = ~0)
## quantile( ~ smokerYes, probs = c(0.025, 0.975), data = Bootvtlg)


## ----lm-boot, out.width = "40%", fig.align="center", echo = FALSE----
set.seed(1896)
Bootvtlg <- mosaic::do(10000)* lm(tip ~ smoker, data = resample(tips))
gf_histogram( ~ smokerYes, data = Bootvtlg) %>%
  gf_vline(xintercept = ~0)
quantile( ~ smokerYes, probs = c(0.025, 0.975), data = Bootvtlg)


## ----lm-smoker-shuffle-no-eval, out.width = "40%", fig.align="center", eval=FALSE----
## set.seed(1896)
## Nullvtlg <- do(10000) * lm(tip ~ shuffle(smoker), data = tips)
## dachbeta_smokerYes <- coef(lm(tip ~ smoker, data = tips))[2]
## gf_histogram( ~ smokerYes, data = Nullvtlg) %>%
##   gf_vline(xintercept = ~dachbeta_smokerYes)
## quantile( ~ smokerYes, probs = c(0.025, 0.975), data = Nullvtlg)


## ----lm-smoker-shuffle, out.width = "40%", fig.align="center", echo=FALSE----
set.seed(1896)
Nullvtlg <- mosaic::do(10000) * lm(tip ~ shuffle(smoker), data = tips)
dachbeta_smokerYes <- coef(lm(tip ~ smoker, data = tips))[2]
gf_histogram( ~ smokerYes, data = Nullvtlg) %>%
  gf_vline(xintercept = ~dachbeta_smokerYes)
quantile( ~ smokerYes, probs = c(0.025, 0.975), data = Nullvtlg)


## ---------------------------------------------
lm(total_bill ~ day, data = tips)


## ----prop-smoker-time, eval=FALSE, echo=TRUE----
## prop(smoker ~ time,
##      success = "Yes", data = tips)


## ----ref.label="prop-smoker-time", eval=TRUE, echo=FALSE----


## ----diffprop-smoker-time, eval=FALSE, echo=TRUE----
## diffprop(smoker ~ time,
##          success = "Yes", data = tips)


## ----ref.label="diffprop-smoker-time", eval=TRUE, echo=FALSE----


## ----lm-smoker-time, eval=FALSE, echo=TRUE----
## lm( (smoker=="Yes") ~ time,
##     data = tips) %>% coef()


## ----ref.label="lm-smoker-time", eval=TRUE, echo=FALSE----


## ---------------------------------------------
mean(tip ~ 1, data = tips)
lm(tip ~ 1, data = tips)


## ----tips-erglm3-summary, echo=TRUE, eval=FALSE----
## erglm3 <- lm(tip ~ # abbhängige Variable
##                total_bill + smoker, # unabhängige Variablen
##              data = tips) # Datensatz
## summary(erglm3)


## ----ref.label="tips-erglm3-summary", eval = TRUE, echo = FALSE----


## ----MMR, out.width = "80%", fig.align="center"----
plotModel(erglm3, col = viridis(2, alpha = 0.6)) + scale_color_viridis_d()


## ----do-mult-regr-no-eval, eval = FALSE-------
## set.seed(1896) # Reproduzierbarkeit
## Bootvtlg <- do(10000) * lm(tip ~ total_bill + smoker,
##                            data = resample(tips))
## confint(Bootvtlg)


## ----do-mult-regr, echo = FALSE---------------
set.seed(1896) # Reproduzierbarkeit
Bootvtlg <- mosaic::do(10000) * lm(tip ~ total_bill + smoker,
                           data = resample(tips))
confint(Bootvtlg)


## ---------------------------------------------
set.seed(1896)
Nullvtlg <- do(10000) *  lm(shuffle(tip) ~ total_bill + smoker,
                           data = tips)


## ---- out.width = "40%", fig.align="center"----
gf_histogram( ~ F, data = Nullvtlg)


## ---- echo=FALSE,  out.width = "50%", fig.align="center"----
set.seed(1896)
# Anzahl Beobachtungen im Datensatz
size.s <- 10
# Anzahl Simulationen
size.sim <- 5

# Stichropbe
tips.s <- tips %>%
  head(n=size.s)

# Vektor initiieren
r.ss <- NULL

# Random Walk R^2_
for(j in 1:size.sim)
  {
  # Vektor R^2 bereitstellen
  r.s <- numeric(size.s)
  # Nullmodell rechnen
  r.s[1] <- lm(tips.s$tip ~ 1)  %>% rsquared()
  # Initiales "Modell":
  x <- rnorm(size.s)

  # Schleife über Terme im Modell
  for (i in 2:size.s)
    {
    r.s[i] <- lm(tips.s$tip ~ x) %>% rsquared()
    # Variable Hinzufügen
    x <- cbind(x, rnorm(size.s))
  }

  r.ss <- c(r.ss, r.s)
}

ranwalk.sim <- data.frame(parameter = rep(1:size.s, size.sim), rsquared = r.ss, simulation= rep(1:size.sim, each = size.s))
lm.mod <- lm(tip ~ total_bill , data = tips.s)

tips.s.ext <- tips.s %>%
  mutate(tip0=mean(tip),
         tipd=fitted(lm.mod),
         e=resid(lm.mod))

# Definiere Viridis-Farben
#my_colors <- viridis(2, alpha = 1, begin = 0, end = 1, option = "D")


gf_lm(tip ~ total_bill , data = tips.s.ext) %>%
  gf_hline(yintercept = ~ mean(~tip, data = tips.s.ext)) %>%
  gf_segment(tipd+tip0 ~ total_bill + total_bill, color = "#440154FF", size = 2) %>%
  gf_segment(tipd+tip ~  total_bill + total_bill, color =  "#FDE725FF", size = 2) %>%
  gf_labs(title = "Modellierte Abweichung zum Mittelwert (violett) \nund Abweichung zum modellierten Wert (gelb)") %>%
  gf_point(tipd ~ total_bill, color = "grey80", size = 3) %>%
  gf_point(color = "grey60", size = 3)


## ---- echo=FALSE,  out.width = "40%", fig.align="center"----
gf_line(rsquared ~ parameter, group = ~simulation, data = ranwalk.sim, alpha = 0.2) %>%
  gf_segment(0 + rsquared(lm.mod) ~ 1 + 2, color = "blue", arrow = arrow()) %>%
  gf_segment(rsquared(lm.mod) + 1 ~ 2 + size.s , color = "red", arrow = arrow()) %>%
  gf_labs(x="Anzahl Modellparameter (Variablen)", y = bquote(R^2))  %>%
  gf_labs(title = "F: Steigung Modell (blau) / \n  Steigung Residuen (rot)") +
  gf_refine(scale_x_continuous(breaks = c(1:10)))


## ---------------------------------------------
# URL der Daten
urlco2 <- "ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt"

# Datei herunterladen
CO2 <- read.table(file=url(urlco2))

# Daten vorverarbeiten
CO2 <- CO2 %>%
  rename(co2 = V4) %>% # co2 Werte in 4. Spalte
  mutate(zeit = V1 + (V2-1)/12) %>% # zeit = jahr + (monat-1)/12
  mutate(monat = factor(V2)) %>% # Saisonkomponente
  select(zeit, monat, co2)


## ---- out.width = "80%", fig.align="center"----
gf_line(co2 ~ zeit, data = CO2)


## ----echo=FALSE, out.width = "20%", fig.align="right"----
gf_line(co2 ~ zeit, data = CO2)


## ---------------------------------------------
lm(co2 ~ zeit + monat, data = CO2)


## ----monatseffeke, eval = FALSE, echo = FALSE----
## lm(co2 ~ zeit + monat, data = CO2) %>% coef()


## ----ref.label="monatseffeke", eval = TRUE, echo = TRUE----


## ---- echo=FALSE, out.width = "35%", fig.align="center"----
lmco2 <- lm(co2 ~ zeit + monat, data = CO2)
gf_line(resid(lmco2) ~ lmco2$model$zeit) %>%
        gf_labs(x="zeit", y="residuum")


## ---------------------------------------------
erglm4 <- lm(tip ~ total_bill + smoker + total_bill:smoker, data=tips)


## ----plot-wechselwirkung, echo=FALSE, eval = TRUE, out.width = "95%", fig.align="center"----
plotModel(erglm4, col = viridis(2)) +
  scale_color_viridis_d()


## ---- echo=FALSE, fig.align="right", out.width="20%"----
plotModel(erglm4, col = viridis(2)) + scale_color_viridis_d()


## ---------------------------------------------
summary(erglm4)


## ---- echo=FALSE, fig.align="center", out.width="30%"----
plotModel(erglm4, col = viridis(2)) %>%
  gf_vline(xintercept = ~15) + scale_color_viridis_d()


## ----echo=FALSE, out.width = "60%", fig.align="center"----
knitr::include_graphics(file.path(pathToImages, "MSE.png"), error=FALSE)


## ----echo=FALSE, out.width = "20%", fig.align="right"----
knitr::include_graphics(file.path(pathToImages, "maschine.jpg"), error=FALSE)


## ----echo=FALSE, out.width = "50%", fig.align="center", cache=FALSE----
# Lizenzworkaround:
extern_image_include("https://www.causeweb.org/cause/sites/default/files/caption_contest/2019/Caption-Contest_11-2019.jpg", "cartoon1119.jpg", pathToImages)


## ---- include=FALSE---------------------------
erglmUeb <- lm(total_bill ~ size + time, data = tips)



