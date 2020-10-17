
library(mosaic)
library(gridExtra)
library(viridis)
library(latex2exp)

tips <- assertData("tips.csv", "https://goo.gl/whKjnl")




## ---- message = FALSE, eval = FALSE-----------
## # Herunterladen
## download.file("https://goo.gl/whKjnl", destfile = "tips.csv")
## # Einlesen in R
## tips <- read.csv2("tips.csv")
##
## # Alternativ - heruntergeladene Datei einlesen
## #   und das Verzeichnis auswählen:
## # tips <- read.csv2(file.choose())


## ----str-tips, eval=FALSE---------------------
## str(tips)


## ----ref.label="str-tips", echo=FALSE---------


## ----mosaic, message=FALSE--------------------
# Ggfs. einmalig vorab installieren
# install.packages("mosaic")

# Pakete müssen bei jedem Start von R bzw. RStudio geladen werden
# Paket mosaic laden
library(mosaic)



## ----echo = FALSE, out.width = "70%", fig.asp = 0.5, fig.align = "center"----
plot1 <- gf_bar(~ sex, data = tips, xlab = "Kategoriale Variable: sex")   # kategoriale Variable
plot2 <- gf_bar(~ size, data = tips, xlab = "Metrisch diskrete Variable: size")  # metrisch diskrete Variable
# Gebe die Plots nebeneinander aus
grid.arrange(plot1, plot2, nrow = 1)


## ----EDA_bargraph, fig.align="center", out.width="50%"----
gf_bar( ~ sex, # (unabhängige) Variable, die analysiert wird
          data = tips) # Datensatz


## ----EDA_bargraph, echo=FALSE, fig.align="right", out.width="20%"----
gf_bar( ~ sex, # (unabhängige) Variable, die analysiert wird
          data = tips) # Datensatz


## ---------------------------------------------
prop( ~ sex,  # (unabhängige) Variable, die analysiert wird
      data = tips) # Datensatz


## ---------------------------------------------
prop( ~ sex,  # (unabhängige) Variable, die analysiert wird
      success = "Female", # wovon den Anteil bestimmen
      data = tips) # Datensatz


## ----tally-beispiel1--------------------------
tally( ~ sex, # Variable, die analysiert wird
       data = tips) # Datensatz


## ----tally-beispiel2--------------------------
tally( ~ sex, # Variable, die analysiert wird
       format = "proportion", # Option: Anteile
       data = tips) # Datensatz


## ----EDA_bargraph-group, fig.align="center", out.width="60%"----
gf_bar( ~ sex # Variable, die analysiert wird
        | time,  # Variable, nach der bedingt wird
        data = tips) # Datensatz


## ----EDA_bargraph-group, echo=FALSE, fig.align="right", out.width="20%"----
gf_bar( ~ sex # Variable, die analysiert wird
        | time,  # Variable, nach der bedingt wird
        data = tips) # Datensatz


## ----tally_group, echo=TRUE-------------------
tally( ~ sex # Variable, die analysiert wird
       | time,  # Variable, nach der bedingt wird
       data = tips) # Datensatz



## ---- echo=TRUE-------------------------------
tally( ~ sex # Variable, die analysiert wird
       | time,  # Variable, nach der bedingt wird
       format = "proportion", # Option: Anteile
       data = tips) # Datensatz


## ---------------------------------------------
tally( ~ smoker | day, format = "proportion", data = tips)

tally( ~ day | smoker, format = "proportion", data = tips)


## ---- eval=FALSE, cache=FALSE-----------------
## tally( ~ x data = daten)


## ---- eval=FALSE, cache=FALSE-----------------
## Tally( ~ x, data = daten)


## ----EDA_Titanic, echo=FALSE, out.width = "65%", fig.align="center", cache=FALSE----
data("Titanic")
mosaicplot(~ Class+Survived,
           color = viridis(2, alpha = 0.6),
           main = "Überleben auf der Titanic",
           cex = 1.5,
           data = Titanic)


## ----EDA_Titanic, echo=FALSE, fig.align="right", out.width="20%", cache=FALSE----
data("Titanic")
mosaicplot(~ Class+Survived,
           color = viridis(2, alpha = 0.6),
           main = "Überleben auf der Titanic",
           cex = 1.5,
           data = Titanic)


## ----eval=FALSE, echo=TRUE,fig.align='center', out.width="60%"----
## mosaicplot(day ~ smoker, data = tips)

## ----evaL=TRUE, echo=FALSE,fig.align='center', out.width="60%"----
mosaicplot(day ~ smoker, data = tips, color = viridis(2, alpha = 0.6))


## ----out.width="20%", fig.asp=2---------------
gf_dotplot( ~ size, data = tips)


## ----out.width="30%", fig.asp=1.5-------------
gf_dotplot( ~ total_bill, data = tips, binwidth = 2) %>%
  gf_theme(scale_y_continuous(NULL, breaks = NULL))


## ----EDA_hist, out.width = "50%", fig.align = "center"----
gf_histogram( ~ total_bill, data = tips, binwidth = 10, center = 5)


## ----EDA_hist, echo=FALSE, fig.align="right", out.width="20%"----
gf_histogram( ~ total_bill, data = tips, binwidth = 10, center = 5)


## ----histogrambins, fig.align="center", out.width="80%", echo=FALSE----
gh2  <- gf_histogram(~ total_bill, data = tips, bins = 2, title ="bins = 2")
gh10 <- gf_histogram(~ total_bill, data = tips, bins = 10, title ="bins = 10")
gh25 <- gf_histogram(~ total_bill, data = tips, bins = 25, title ="bins = 25")
gh50 <- gf_histogram(~ total_bill, data = tips, bins = 50, title ="bins = 50")
grid.arrange(gh2, gh10, gh25, gh50)


## ----histogram, fig.align="center", out.width="60%"----
gf_histogram( ~ total_bill, # Variable, die analysiert wird
              binwidth = 5, # Breite einer Säule entspricht 5$
              data = tips)  # Datensatz


## ----histogram, echo=FALSE, fig.align="right", out.width="20%"----
gf_histogram( ~ total_bill, # Variable, die analysiert wird
              binwidth = 5, # Breite einer Säule entspricht 5$
              data = tips)  # Datensatz


## ----histogramlog, fig.align="center", out.width="50%"----
gf_histogram( ~ sqrt(total_bill), # Quadratwurzel der Variable
              bins = 9,           # Anzahl Säulen
              data = tips)        # Datensatz







## ----pdata------------------------------------
pdata( ~ total_bill, q = 10, data = tips)




## ----qdata------------------------------------
qdata( ~ total_bill, p = 0.9, data = tips)


## ----echo=FALSE, out.width = "60%", fig.align="center"----
set.seed(1896)
xsym <- rnorm(1000)
xls <- 1 - rchisq(1000, 2)
xrs <- rchisq(1000, 2)

hls <- gf_histogram( ~ xls,
                     xlab = NULL,
                     title = "A",
                     fill = viridis(1, alpha = 0.6),
                     color = "darkgrey") %>%
         gf_theme(axis.text.x = element_blank())

hsym <- gf_histogram( ~ xsym,
                      xlab = NULL,
                      title = "B",
                      fill = viridis(1, alpha = 0.6),
                      color = "darkgrey") %>%
          gf_theme(axis.text.x = element_blank())

hrs <- gf_histogram( ~ xrs,
                     xlab = NULL,
                     title = "C",
                     fill = viridis(1, alpha = 0.6),
                     color = "darkgrey") %>%
         gf_theme(axis.text.x = element_blank())



## ----echo=FALSE, out.width = "45%", fig.align="center"----
hls <- gf_histogram( ~ xls,
                     xlab = NULL,
                     title = "A",
                     fill = viridis(1, alpha = 0.6),
                     color = "darkgrey") %>%
      gf_theme(axis.text.x = element_blank()) %>%
      gf_vline(xintercept = ~median(xls),
               color = viridis(1, alpha = 0.6),
               linetype = "dashed") %>%
      gf_vline(xintercept = ~mean(xls),
               color = "red")

hsym <- gf_histogram( ~ xsym,
                      xlab = NULL,
                      title = "B",
                      fill = viridis(1, alpha = 0.6),
                      color = "darkgrey") %>%
        gf_theme(axis.text.x = element_blank()) %>%
        gf_vline(xintercept = ~median(xsym),
                 color = viridis(1, alpha = 0.6),
                 linetype = "dashed") %>%
        gf_vline(xintercept = ~mean(xsym),
                 color = "red")

hrs <- gf_histogram( ~ xrs,
                     xlab = NULL,
                     title = "C",
                     fill = viridis(1, alpha = 0.6),
                     color = "darkgrey") %>%
       gf_theme(axis.text.x = element_blank()) %>%
       gf_vline(xintercept = ~median(xrs),
                color = viridis(1, alpha = 0.6),
                linetype = "dashed") %>%
       gf_vline(xintercept = ~mean(xrs),
                color = "red")

grid.arrange(hls, hsym, hrs, nrow = 3)

rm(xsym)
rm(xls)
rm(xrs)


## ---------------------------------------------
mean( ~ total_bill, data = tips)


## ----setup_mean_model, echo=FALSE-------------
my.model.mean.f <- mean( ~ total_bill, data = tips)
my.model.mean.sumofsq <- nrow(tips) * var( ~ total_bill, data = tips)


## ----echo=FALSE, out.width = "20%", fig.align="right"----
knitr::include_graphics(file.path(pathToImages, "maschine.jpg"), error=FALSE)


## ----fig-abw-balken-mw, echo = FALSE, out.width="60%"----

d <- tibble::tribble(
  ~id, ~note, ~note_avg, ~delta, ~note2, ~note_avg2,
   1L,     2,     2.325, -0.325,  2.325,          2,
   2L,   2.7,     2.325,  0.375,    2.7,      2.325,
   3L,   3.1,     2.325,  0.775,    3.1,      2.325,
   4L,   1.5,     2.325, -0.825,  2.325,        1.5
  )


d <- d %>%
  mutate(delta_abs = abs(delta),
         pos = ifelse(delta > 0, "positiv", "negativ"),
         delta_sq = delta^2)



d %>%
  ggplot(aes(x = id,
             y = note)) +
  geom_hline(yintercept = mean(d$note),
             linetype = "dashed") +
  geom_segment(aes(y = mean(d$note),
                   yend = note,
                   x = id,
                   xend = id,
                   linetype = pos)
               ) +
  geom_point(size = 5) +
  labs(linetype = "Richtung der Abweichung") +
  theme(legend.position =  c(1, 1),
        legend.justification = c(1, 1)) +
  annotate(geom = "label",
           x = 0,
           hjust = 0,
           y = mean(d$note),
           label = paste0("MW = ", round(mean(d$note), 2))) +
  scale_y_continuous(limits = c(1, 4)) +
  labs(x = "",
       y = "") +
  scale_x_continuous(breaks = 1:4) -> p_mean_deltas

p_mean_deltas



## ----delta-plot, echo = FALSE, fig.width = 9, out.width = "7cm", fig.asp = 0.8----

d %>%
  ggplot(aes(x = id, y = note)) +
  geom_hline(yintercept = mean(d$note), linetype = "dashed") +
  geom_segment(aes(y = mean(d$note),
                   yend = note,
                   x = id,
                   xend = id,
                   linetype = pos)) +
    annotate(geom = "label",
           x = 0,
           hjust = 0,
           y = mean(d$note),
           label = paste0("MW = ", round(mean(d$note), 2))) +
  geom_rect(aes(ymin = note_avg2,
                ymax = note2,
                xmin = id,
                xmax = id+delta_abs),
            fill = viridis(1, alpha = 0.6),
            alpha = .5) +
  geom_text(aes(label=round(d$delta_sq,3)),
            hjust = "left",
            nudge_x = 0.05,
            vjust = ifelse(d$pos == "positiv", "top", "bottom"),
            nudge_y = ifelse(d$pos == "positiv", -0.05, 0.05),
            color = viridis(1, alpha=0.8),
            size = 6) +
  geom_point(size = 5) +
    labs(linetype = "Richtung der Abweichung",
         x = "",
         y = "") +
  theme(legend.position =  c(1, 1),
        legend.justification = c(1, 1)) +
  scale_y_continuous(limits = c(1, 5)) +
  scale_x_continuous(breaks = 1:4) -> p_mean_deltas_sq

p_mean_deltas_sq


## ----echo=FALSE, out.width = "50%", fig.align="center"----
gabi <- c(5, 25, 2, 3, 30, 15, 5, 20, 35, 10)
klaus <- c(13, 15, 14, 14, 16, 15, 16, 15, 16, 16)
kaufdaten <- data.frame(Zeitpunkte = c(cumsum(gabi), cumsum(klaus)),
                        Personen = c(rep("Gabi", length(gabi)), rep("Klaus", length(klaus))))
gf_point(Personen ~ Zeitpunkte,
         data = kaufdaten,
         size = 3,
         color = viridis(1, alpha = 0.6))

rm(gabi)
rm(klaus)
rm(kaufdaten)


## ----echo=FALSE, out.width = "60%", fig.align="center", message=FALSE----
set.seed(1896)
xs1 <- rnorm(1000)
xs2 <- rnorm(1000, sd = 2)
xs3 <- runif(1000, -6, 6)

hs1 <- gf_histogram( ~ xs1,
                     xlab = NULL,
                     title = "A",
                     fill = viridis(1, alpha = 0.6),
                     color = "darkgrey") %>%
       gf_lims(x=c(-6,6))

hs2 <- gf_histogram( ~ xs2,
                     xlab = NULL,
                     title = "B",
                     fill = viridis(1, alpha = 0.6),
                     color = "darkgrey") %>%
       gf_lims(x=c(-6,6))

hs3 <- gf_histogram( ~ xs3,
                     xlab = NULL,
                     title = "C",
                     fill = viridis(1, alpha = 0.6),
                     color = "darkgrey") %>%
       gf_lims(x=c(-6,6))

grid.arrange(hs1, hs2, hs3, nrow = 3)

rm(xs1)
rm(xs2)
rm(xs3)


## ----Trinkgelddaten_inspect_command, eval=FALSE----
## inspect(tips)


## ----Trinkgelddaten_inspect_output, echo=FALSE, R.options=list(width=90)----
(insp <- mosaic::inspect(tips))


## ----favstats---------------------------------
favstats( ~ total_bill, # Variable, die analysiert wird
          data = tips)  # Datensatz


## ----favstats---------------------------------
favstats( ~ total_bill, # Variable, die analysiert wird
          data = tips)  # Datensatz


## ----out.width = "55%", fig.align="center"----
gf_boxplot(tip ~ 1, data = tips)


## ---- echo=FALSE, out.width = "60%", fig.align="center", fig.asp=0.6----
set.seed(1896)
chi <- data.frame(x = rchisq(100, 3))
gf_boxploth(1 ~ x, data = chi,
            ylab = element_blank()) %>%
  gf_theme(axis.ticks.y = element_blank(),
           axis.text.y = element_blank())

(stats <- round(favstats(~x, data = chi), 2))


## ----violin-box, fig.align="center", out.width="60%", echo=FALSE----
p1 <- gf_boxplot(tip ~ 1, data = tips) %>% gf_rug()
p2 <- gf_violin(tip ~ 1, data = tips) %>% gf_rug()
gridExtra::grid.arrange(p1,p2, nrow = 1)


## ----histogram_group, fig.align="center", out.width="60%"----
 gf_histogram( ~ total_bill # Variable, die analysiert wird
               | sex,       # Variable, nach der gruppiert wird
               bins = 9,    # Anzahl Säulen
               data = tips) # Datensatz


## ----boxplot-group, fig.align="center", out.width="50%"----
gf_boxplot(total_bill ~ # abhängige Variable
          sex, # unabhängige Variable
          data = tips) # Datensatz


## ----boxplot-group, echo=FALSE, fig.align="right", out.width="20%"----
gf_boxplot(total_bill ~ # abhängige Variable
          sex, # unabhängige Variable
          data = tips) # Datensatz


## ---------------------------------------------
favstats(total_bill ~ sex, data = tips)


## ----fig.width = 6, out.width="6cm", fig.asp = 0.6----
gf_point(total_bill ~ sex, stat = "summary", size = 5, data = tips)



## ---------------------------------------------
favstats(total_bill ~ 1, data = tips)


## ---- include=FALSE---------------------------
f1 <- favstats(total_bill ~ 1, data = tips)


## ---------------------------------------------
favstats(total_bill ~ size, data = tips)


## ---- include=FALSE---------------------------
f2 <- favstats(total_bill ~ size, data = tips)


## ----setup_groupmean_model, echo=FALSE--------
my.model.groupmean.f <- mean(total_bill ~ size, data = tips)
my.model.groupmean.sumofsq <- sum((favstats(total_bill ~ size, data = tips)$n - 1) * var(total_bill ~ size, data = tips))

my.model.groupmean.text <- "7.242  & : x_{size} =  1\\\\\n 16.448  & : x_{size} =  2\\\\\n 23.278  & : x_{size} =  3\\\\\n 28.614  & : x_{size} =  4\\\\\n 30.068  & : x_{size} =  5\\\\\n 34.83  & : x_{size} =  6\n"


## ----out.width = "65%", fig.align="center", fig.asp=0.5, echo=FALSE----

tips2 <- tips %>%
  filter(size %in% c(1, 6)) %>%
  mutate(ID = 1:nrow(.),
         total_bill_resid = total_bill - mean(total_bill),
         total_bill_resid_quad = total_bill_resid^2) %>%
  group_by(size) %>%
  mutate(total_bill_mean_group = mean(total_bill)) %>%
  ungroup()


tips_sum <- tips2 %>%
  group_by(size) %>%
  summarise(total_bill = mean(total_bill)) %>%
  ungroup()


p1 <- tips2 %>%
  ggplot() +
  geom_hline(aes(yintercept = mean(total_bill)), size = 3) +
  geom_segment(aes(x = ID,
                   xend = ID,
                   y = total_bill,
                   yend = mean(total_bill)
                   ),
               size = 1,
               color = "grey60") +
  geom_point(aes(x = ID, y = total_bill),
             size = 7) +
  labs(title = "total_bill ~ 1",
       x = "",
       y = "",
       caption = "Relativ viel Residualstreuung")



p2 <- ggplot(tips2) +
  geom_hline(data = tips_sum,
             aes(yintercept = total_bill,
                 color = ordered(size)),
             size = 3) +
  geom_segment(aes(x = ID,
                   xend = ID,
                   y = total_bill,
                   yend = total_bill_mean_group),
               color = "grey80",
               size = 1) +
  geom_point(data = tips2,
             aes(x = ID,
                 y = total_bill,
                 color = ordered(size)),
             size = 7) +
  theme(legend.position = "none") +
  labs(title = "total_bill ~ size",
       x = "",
       y = "",
       caption = "Relativ wenig Residualstreuung") +
  scale_color_viridis_d(alpha = 0.6) +
  geom_label(data = tips_sum,
             aes(y = total_bill),
             x = 1, label = paste0("size: ", tips_sum$size), hjust = 0)

grid.arrange(p1, p2, nrow = 1)


## ----out.width = "65%", fig.align="center", fig.asp=0.5, echo=FALSE----
plot1 <- gf_point(tip ~ total_bill, data = tips)   # zwei stetige Variablen
plot2 <- gf_point(tip ~ jitter(size), data = tips) # eine stetige, eine diskrete
grid.arrange(plot1, plot2, nrow = 1)


## ----rho-delta-rect, echo = FALSE, out.width="60%"----
# Zeichengröße für die Rechteckflächen
cov.value.font.size <- 2.5
# Datensatz auf die Tischgrößen 1 und 6 reduzieren.
tips %>% filter(size==1 | size == 5) -> df

# Mittelwerte berechnen und speichern
y.mean <- mean(~ tip, data = df)
x.mean <- mean(~ total_bill, data = df)

# Streudiagramm mit Covarianz-Anteilen zeichnen:
cov.value <- round(c((df$tip - y.mean) * (df$total_bill - x.mean)), 2)
cov.value.pos.x <- c((df$total_bill + x.mean)/2)
cov.value.pos.y <- c((df$tip + y.mean)/2)
gf_point(tip ~ total_bill, data = df) %>%
    gf_rect(tip + y.mean ~ total_bill + x.mean,
            fill = c(rep("green", 6), "red", rep("green",2)),
            color = c(rep("darkgreen", 6), "red", rep("darkgreen",2)),
            alpha = 0.20) %>%
    gf_hline(yintercept = ~ y.mean, linetype = "dotted") %>%
    gf_vline(xintercept = ~ x.mean, linetype = "dotted") %>%
    gf_text(cov.value.pos.y ~ cov.value.pos.x,
            size = cov.value.font.size,
            color = c(rep("darkgreen", 6), "red", rep("darkgreen",2)),
            label = cov.value)


## ----Visualisierung_Korrelation, fig.align="center", echo=FALSE, out.width="80%"----
#### Quelle http://moderndive.com/scripts/06-regression.R
## Leichte Anpassungen durch N. Markgraf
# library(mosaic)
library(mvtnorm)
set.seed(2009)

correlation <- c(-0.999999, -0.90, -0.75, -0.30, 0.00, 0.30, 0.75, 0.90, 0.999999)
eps <- 0.00001
n_sim <- 100

values <- NULL
for (i in 1:length(correlation)) {
    rho <- correlation[i]
    rs <- rho * sqrt(50)
    sigma <- matrix(c(5, rs, rs, 10), 2, 2)
    sim <- rmvnorm(
        n = n_sim,
        mean = c(20,40),
        sigma = sigma
    )
    r <- cor(sim[,1],sim[,2])
    new_err <- NULL
    new_r <- NULL
    err <- abs(rho - r)
    for (j in 1:1000) {
        sim_t <- rmvnorm(
            n = n_sim,
            mean = c(20,40),
            sigma = sigma
        )
        new_r <- cor(sim_t[,1], sim_t[,2])
        new_err <- abs(rho - new_r)
        if (new_err < err) {
            # cat(paste("want:",rho,"- got:",r,"- err:",err,"- new:",new_r,"new-err:",new_err,"\n"))
            sim <- sim_t
            r <- new_r
            err <- new_err
        }
        if (new_err < eps) {
            break
        }
    }
    sim %>%
        as.data.frame() %>%
        mutate(correlation = round(rho,2)) %>%
        mutate(reel_correlation = round(cor(V1,V2),2)) %>%
        mutate(reel_correlation_2 = round(r,2)) %>%
        mutate(cor_err = err) -> sim

    values <- bind_rows(values, sim)
}

ggplot(data = values, mapping = aes(V1, V2)) +
    geom_point() +
    # stat_ellipse(level=0.999, type="norm", color="darkgreen", linetype="dotted", alpha=0.25) +
    # geom_lm(level=0.999) +
    facet_wrap(~ correlation, ncol = 3)  +
    labs(x = "", y = "") +
    # coord_fixed(ratio=25/40) + # (30-5)/(60-20)
    theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
    )
#unique(values$correlation)
#unique(values$reel_correlation)
#unique(values$reel_correlation_2)
#unique(values$cor_err)

detach("package:mvtnorm", unload=TRUE)
#detach("package:bindrcpp", unload=TRUE)


## ----echo=FALSE, out.width = "40%", fig.align="center"----
set.seed(1896)
x <- seq(-2, 2, by = 0.1)
y <- x^2
gf_point(y ~ x)
rm(x)
rm(y)


## ---- fig.align="center", echo=FALSE, out.width="80%"----
data(anscombe)
x <- c(anscombe$x1, anscombe$x2, anscombe$x3, anscombe$x4)
y <- c(anscombe$y1, anscombe$y2, anscombe$y3, anscombe$y4)
z <- factor(rep(1:4, each = 11))
ans <- data.frame(x = x, y = y, z = z)
gf_point(y ~ x | z, data = ans, main = "Anscombe Daten" )


## ---- echo=FALSE------------------------------
real_sd <- function(x) {
#    sqrt(real_var(x))
    sd(x)
}

real_var <- function(x) {
    var(x)*(length(x) - 1)/length(x)
}



## ----xyplot, fig.align="center", out.width="60%"----
gf_point( tip # Variable auf y-Achse
        ~ total_bill, # Variable auf x-Achse
          data = tips) # Datensatz


## ----xyplot, echo=FALSE, fig.align="right", out.width="20%"----
gf_point( tip # Variable auf y-Achse
        ~ total_bill, # Variable auf x-Achse
          data = tips) # Datensatz


## ----cor--------------------------------------
cor(tip ~ total_bill, # Variablen
     data = tips)  # Datensatz


## ---------------------------------------------
tips <- tips %>%
  mutate(rel_tip = tip/total_bill)


## ----xyplot2, fig.align="center", out.width="35%"----
gf_point(rel_tip ~ total_bill, data = tips)


## ----out.width="60%", fig.align="center", fig.asp=0.5----
gf_point(tip ~ total_bill | sex, data = tips)


## ---------------------------------------------
tips %>%
  group_by(sex) %>%
  summarise(cor(tip~total_bill, data=.))


## ----xyplot2, echo=FALSE, fig.align="right", out.width="20%"----
gf_point(rel_tip ~ total_bill, data = tips)



## ---- echo=FALSE, fig.align="center", out.width="40%"----
gf_dist("binom", size = 8, prob = 0.5) %>%
  gf_labs(x = "Anzahl Kopf", y = "Relative Häufigkeit")


## ---------------------------------------------
set.seed(1896)
Sarah_Raet <- do(1000) * rflip(n = 8)
prop( ~ heads, success = 8, data = Sarah_Raet)





