
library(mosaic)


## ----smile, echo=FALSE, fig.align="center", fig.width = 12, out.width="12cm"----
set.seed(1896)
# LaFrance, M., & Hecht, M. A., "Why smiles generate leniency", Personality and Social Psychology Bulletin, 21, 1995, 207-214.

nachsichtigkeit <- c(7, 3, 6, 4.5, 3.5, 4, 3, 3, 3.5, 4.5, 7, 5, 5, 7.5, 2.5, 5, 5.5, 5.5,
                     5, 4, 5, 6.5, 6.5, 7, 3.5, 5, 3.5, 9, 2.5, 8.5, 3.5, 4.5, 3.5, 4.5,
                     2, 4, 4, 3, 6, 4.5, 2, 6, 3, 3, 4.5, 8, 4, 5, 3.5, 4.5, 6.5, 3.5,
                     4.5, 4.5, 2.5, 2.5, 4.5, 2.5, 6, 6, 2, 4, 5.5, 4, 2.5, 2.5, 3, 6.5)
gesicht <- factor(rep(c("lächeln", "neutral"), each =34), levels = c("neutral", "lächeln"))
Laecheln <- data.frame(gesicht, nachsichtigkeit)


Laecheln.shuffle <- do(12) * Laecheln
Laecheln.shuffle <- Laecheln.shuffle %>%
  group_by(.index) %>%
  mutate(gesicht = shuffle(gesicht)) %>%
  ungroup() %>%
  select(-.row)

Laecheln.org  <- Laecheln  %>%
  mutate(.index = 3)

Laecheln.shuffle[Laecheln.shuffle$.index==3, ] <- Laecheln.org


gf_jitter(nachsichtigkeit ~ gesicht,
          color = ~ gesicht, data = Laecheln.shuffle, width=0.1, height = 0.05, alpha = 0.5) %>%
  gf_point(nachsichtigkeit ~ gesicht, color = ~ gesicht, data = Laecheln.shuffle,
           group = ~ gesicht, stat="summary", size = 4) +
  facet_wrap(~ .index, ncol = 4) +
  scale_color_viridis_d()
  #ggthemes::scale_color_colorblind()


## ---- echo=FALSE, fig.align="right", out.width="20%"----
gf_jitter(nachsichtigkeit ~ gesicht,
          color = ~ gesicht, data = Laecheln.shuffle, width=0.1, height = 0.05, alpha = 0.5) %>%
  gf_point(nachsichtigkeit ~ gesicht, color = ~ gesicht, data = Laecheln.shuffle,
           group = ~ gesicht, stat="summary", size = 4) +
  facet_wrap(~ .index, ncol = 4) +
  ggthemes::scale_color_colorblind()


## ----smiledot, echo=FALSE, fig.align="right", out.width="40%"----
set.seed(1896)
Nullvtlg <- do(100)*diffmean(nachsichtigkeit ~ shuffle(gesicht), data = Laecheln)
gf_dotplot( ~ diffmean, data = Nullvtlg, binwidth = 0.05) %>%
  gf_vline(xintercept = ~diffmean(nachsichtigkeit ~ gesicht , data = Laecheln)) %>%
  gf_lims(x=c(-1.2,1.2)) %>%
  gf_labs(y="Simulationen", x="Differenz Mittelwerte")


## ----muenz-simu, echo = FALSE, out.width = "50%"----
set.seed(1896) # Reproduzierbarkeit
muenzverteilung <- do(100) * rflip(n = 10)
muenzverteilung <- muenzverteilung %>%
  mutate(heads = factor(heads, levels = 0:10))
#prop(~heads >= 8, data = muenzverteilung)
gf_props(~heads, data = muenzverteilung, title = "9 der 100 Stichproben hatten 8-mal oder mehr Kopf") %>%
  gf_refine(scale_x_discrete(drop = FALSE))


## ---- echo=FALSE, fig.align="right", out.width="20%"----
gf_bar(~heads, data = muenzverteilung, title = "9 der 100 Stichproben hatten 8-mal oder mehr Kopf") %>%
  gf_refine(scale_x_discrete(drop=FALSE))


## ---- eval = FALSE----------------------------
## # Paket laden, ggf. vorher
## # einmalig installieren:
## # install.packages("mosaic")
## library(mosaic)
## set.seed(1896) # Reproduzierbarkeit
## muenzverteilung <- do(100) *
##                    rflip(n = 10)



## ---- echo=FALSE, fig.align="center", out.width="30%"----
plot(c(1,2,3), y=c(1,sqrt(5),1),
     col=c("darkgreen", "red", "red"),
     pch=c(3,4,4), cex=8, lwd=8, ann=FALSE, axes=FALSE, xlim = c(0,4), ylim = c(0,3), asp = 1)



## ---------------------------------------------
population <- rep(factor(c("f","r")), c(220000, 120000))
prop( ~ population, success = "r")


## ---- eval=FALSE------------------------------
## prop( ~ sample(population, size = 34), success = "r")
## prop( ~ sample(population, size = 34), success = "r")


## ---- echo=FALSE, cache=0---------------------
set.seed(1904)
prop( ~ sample(population, size = 34), success = "r")
prop( ~ sample(population, size = 34), success = "r")


## ---------------------------------------------
set.seed(1896) # Reproduzierbarkeit
Stiprovtlg <- do(10000)* prop( ~ sample(population, size = 34),
                               success = "r")


## ---------------------------------------------
sd( ~ prop_r, data = Stiprovtlg)


## ---- fig.align="center", out.width="60%"-----
gf_bar( ~ prop_r, data = Stiprovtlg)


## ---- echo=FALSE, fig.align="right", out.width="20%"----
gf_bar( ~ prop_r, data = Stiprovtlg)


## ---- eval=FALSE------------------------------
## stipro <- rep(factor(c("f","r")), c(22, 12))
## stipro

## ---- echo=FALSE------------------------------
stipro <- rep(factor(c("f","r")), c(22, 12))
names(stipro) <- 1:length(stipro)
stipro


## ---- eval=FALSE------------------------------
## resample(stipro)


## ---- echo=FALSE------------------------------
set.seed(1896)
rstipro <- resample(stipro)
rstipro <- stipro[sort(as.numeric(names(rstipro)))]
rstipro


## ---------------------------------------------
set.seed(1896) # Reproduzierbarkeit
do(3)* prop( ~ resample(stipro), success = "r")



## ---------------------------------------------
set.seed(1896)
Bootvtlg <- do(10000)* prop( ~ resample(stipro),
                             success = "r")


## ---- fig.align="center", out.width="60%"-----
gf_bar( ~ prop_r, data = Bootvtlg)


## ---- echo=FALSE, fig.align="center", out.width="60%"----
p1 <- gf_bar( ~ prop_r, data = Stiprovtlg, title = "Stichprobenverteilung")
p2 <- gf_bar( ~ prop_r, data = Bootvtlg, title ="Bootstrap-Verteilung")


Stichprobenverteilung <- Stiprovtlg$prop_r
Bootstrapverteilung <- Bootvtlg$prop_r
values <- c(Stichprobenverteilung, Bootstrapverteilung)
types <- c(rep("Stichprobenvtlg", length(Stichprobenverteilung)), rep("Bootstrapvtlg", length(Bootstrapverteilung)))
df <- data.frame(values, types)



gridExtra::grid.arrange(gridExtra::grid.arrange(p1,p2, ncol=2))


## ---------------------------------------------
quantile( ~ prop_r, data = Bootvtlg, probs = c(0.025, 0.975))


## ---------------------------------------------
quantile( ~ prop_r, data = Bootvtlg, probs = c(0.025, 0.975))


## ---- echo=FALSE, , fig.align="center", out.width="80%"----
set.seed(1896) # Reproduzierbarkeit
CIsim(n=10, samples=100)


## ----echo=FALSE, out.width = "20%", fig.align="right"----
knitr::include_graphics(file.path(pathToImages, "maschine.jpg"), error=FALSE)


## ---- eval=FALSE------------------------------
## rflip(n = 34, prob = 1/3)

## ---- echo=FALSE------------------------------
set.seed(1896)
# 34-facher Münzwurf mit Erfolgsw.keit 1/3
rflip(n = 34, prob = 1/3)


## ----simu-Nullvtlg-DT, fig.align="center", out.width="40%"----
set.seed(1896)
Nullvtlg <- do(10000) * rflip(n = 34, prob = 1/3)


## ---- eval=FALSE, fig.align="center", out.width="80%"----
## gf_bar( ~ heads, data = Nullvtlg )

## ----barplotHeadNullvtlg, echo=FALSE, eval=TRUE, fig.align="center", out.width="80%"----
Nullvtlg %>% mutate(heads = sprintf("%2s", 1:34)[heads]) -> Nullvtlg_c
gf_bar( ~ heads, data = Nullvtlg_c )


## ----ref.label="barplotHeadNullvtlg", echo=FALSE, fig.align="right", out.width="20%"----


## ----ref.label="barplotHeadNullvtlg", echo=FALSE, fig.align="right", out.width="20%"----


## ----echo=FALSE, out.width = "70%", fig.align="center"----
knitr::include_graphics(file.path(pathToImages,"OneTest.png"), error=FALSE)


## ----echo=FALSE, out.width = "20%", fig.align="right"----
knitr::include_graphics(file.path(pathToImages, "maschine.jpg"), error=FALSE)


## ----echo=FALSE, out.width = "40%", fig.align="center", cache=FALSE----
# Lizenzworkaround:
extern_image_include("https://www.causeweb.org/cause/sites/default/files/caption_contest/2016/Caption-Contest_11-2016.jpg", "cartoon1116.jpg", pathToImages)


## ----eval = FALSE, echo = TRUE----------------
## gf_bar( ~ heads,
##         data = Nullvtlg)


## ----muenze-null-graph, out.width = "95%", fig.align="center", echo = FALSE----
gf_bar( ~ heads,
        data = Nullvtlg,
        fill = ~ ordered(heads >= 12)) %>%
  gf_labs(fill = "Mind. 12 Treffer") %>%
  gf_theme(legend.position = c(0.95,0.95), legend.justification = c(1,1))


## ----ref.label="muenze-null-prop", eval = FALSE, echo = TRUE----
## NA


## ----muenze-null-prop, out.width = "90%", fig.align="center", echo = FALSE----
prop( ~ heads >= 12,
      data = Nullvtlg)


## ---- echo=FALSE, out.width = "60%", fig.align="center"----
set.seed(1896)
pNull <- do(10000)*binom.test(~sample(population, 34), success = "r", p=1/3, alternative = "greater") %>% pval()
gf_bar(~cut(pNull$p.value, seq(0,1, by=0.05))) %>%
  gf_labs(x="p-Werte", y="Anzahl Stichproben", title="p-Wert Dreieckstest", subtitle=" mit simulierten Stichproben") %>%
  gf_refine(theme(axis.text.x = element_text(angle=45, hjust=1)))


## ----echo=FALSE, out.width = "40%", fig.align="center", cache=FALSE----
# Lizenzworkaround:
extern_image_include("https://www.causeweb.org/cause/sites/default/files/caption_contest/2017/Caption-Contest_02-2017.jpg", "cartoon0217.jpg", pathToImages)


## ----echo=FALSE, out.width = "80%", fig.align="center"----
p1 <- gf_dist("norm", mean=0, geom = "area", fill= ~(x>qnorm(0.95)), alpha = 0.5,
        title=expression(paste("Fehler 1. Art: Verteilung falls ", mu==0)),
        subtitle=expression(H[0]:mu<=0 )) %>%
  gf_refine(guides(fill=guide_legend(title=expression(paste(alpha,"-Fehler"))))) %>%
  gf_vline(xintercept = ~qnorm(0.95)) %>%
  gf_labs(x=expression(bar(x))) %>%
  gf_dist("norm", mean=2, alpha = 0.5) %>%
  gf_refine(scale_fill_viridis_d()) %>%
 gf_refine(annotate("label",x=1.65, y=0.45, label="Kritischer Wert"),
            scale_y_continuous(limits = c(0, 0.5)))


p2 <- gf_dist("norm", mean=2, geom = "area", fill= ~(x<qnorm(0.95)), alpha = 0.5,
        title=expression(paste("Fehler 2. Art: Verteilung falls ", mu==2)),
        subtitle=expression(H[0]:mu<=0 )) %>%
  gf_refine(guides(fill=guide_legend(title=expression(paste(beta,"-Fehler"))))) %>%
  gf_vline(xintercept = ~qnorm(0.95)) %>%
  gf_labs(x=expression(bar(x))) %>%
  gf_dist("norm", mean=0, alpha = 0.5) %>%
  gf_refine(scale_fill_viridis_d()) %>%
  gf_refine(annotate("label",x=1.65, y=0.45, label="Kritischer Wert"),
            scale_y_continuous(limits = c(0, 0.5)))

gridExtra::grid.arrange(p1,p2, nrow=2)



## ---------------------------------------------
quantile( ~ prop_r, data = Bootvtlg, probs = c(0.025, 0.975))


## ---- eval=FALSE------------------------------
## # Einmalige Installation
## install.packages("lsr")
##
## # Paket laden
## library(lsr)


## ----load-library-lsr, echo=FALSE-------------
library(lsr)


## ----print-cohens-d---------------------------
cohensD(total_bill ~ smoker, data=tips)


## ----plot-cohens-d, echo=FALSE, out.width="80%", fig.align="center"----
set.seed(1896)
n <- 1000
x <- scale(rnorm(n))
x02 <- x+0.2
x05 <- x+0.5
x08 <- x+0.8
x11 <- x+1.1

cohendata <- data.frame(x=c(x, x02, x, x05, x, x08, x, x11),
                   Gruppe=rep(c(rep(c("A","B"), each=n)), 4),
                   d=c(rep("d=0.2", 2*n), rep("d=0.5", 2*n), rep("d=0.8", 2*n) , rep("d=1.1", 2*n)))

gf_fitdistr(gformula=~x|d, color=~ordered(Gruppe), data=cohendata, size = 2) %>%
    gf_labs(y="f(x)", x="x", color = "Gruppe")

rm(n,x,x02,x05,x08,x11,cohendata)


## ----plot-p-value-n-d, echo=FALSE, out.width="80%", fig.align="center"----
set.seed(1896)
n <- c(30, 100)
d <- c(0, 0.2, 0.5, 0.8)
pvalue <- matrix(nrow=10000*8, ncol = 4)
zaehler <- 1
for(i in 1:10000)
  for(j in n)
    for(k in d)
    {
      x0 <- rnorm(j)
      x1 <- rnorm(j, mean=k)
      pvalue[zaehler,1] <- j
      pvalue[zaehler,2] <- k
      pvalue[zaehler,3] <- t.test(x0,x1)$p.value
      zaehler <- zaehler+1
    }
pvalsim <- data.frame(n=paste0("n=",pvalue[,1]), d=paste0("d=",pvalue[,2]), pvalue=pvalue[,3])

gf_histogram(~pvalue, data = pvalsim, bins=20,
             breaks=seq(0, 1, by=0.025), fill=~ordered(pvalue<0.05)) %>%
  gf_refine(scale_y_log10()) %>%
  gf_vline(xintercept = ~0.05) %>%
  gf_facet_grid(n~d) %>%
  gf_theme(legend.position="bottom") %>%
  gf_labs(title="Ergebnis der Simulation mit logarithmischer y-Achse",
          fill = "p-Wert < 0.05") %>%
  gf_refine(scale_x_continuous(name = "p-Wert", breaks = c(0, .5, 1)))





