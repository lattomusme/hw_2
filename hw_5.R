---
title: "Spatial Modeling Assignment"
author: "Morgan Lattomus"
date: "2/22/22"
output:
  html_document: default
  pdf_document: default
  word_document: default
---

```{r setup}
library(vegan)
data(BCI)
## UTM Coordinates (in metres)
BCI_xy = data.frame(x = rep(seq(625754, 626654, by=100), each=5), 
                    y = rep(seq(1011569,  1011969, by=100), len=50))
?BCI
```

1.Examine if there s evidence of spatial dependence in a rare and a common species in the BCI tree dataset.

```{r figuring out if they are dependent}
plot(BCI_xy)
sr <- rowSums(BCI > 0)
hist(sr)
plot(BCI_xy, cex = sr/max(sr))

sr_dist <- dist(sr)
xy_dist <- dist(BCI_xy)
max_dist <- max(xy_dist) / 2

abu <- colSums(BCI)
quantile(log10(abu))
plot(density(log10(abu)))
```

```{r separating species}
rare_sp <- BCI[ , 65]
comm_sp <- BCI [ , 58]

par(mfrow=c(1,2))
plot(BCI_xy, cex = rare_sp / max(rare_sp))
plot(BCI_xy, cex = comm_sp / max(comm_sp))
```

```{r getting the very spotty graph thingy}
geod <- dist(BCI_xy)
rared <- dist(rare_sp)
commd <- dist(comm_sp)

par(mfrow=c(1,2))
plot(geod, rared, main = 'rare species')
lines(lowess(geod,rared), lwd =2, col='red')
plot(geod, commd, main = 'common species')
lines(lowess(geod, commd), lwd =2, col ='red')
```

```{r mantel correlogram}
rare_corlog = mantel.correlog(rared, BCI_xy)
comm_corlog = mantel.correlog(commd, BCI_xy)
rare_corlog
comm_corlog
```

  The rare species (Erythrina costaricensis & Inga acuminata) have a lower geographical distribution, which is to be expected as they are the less commonly found group of species, whereas the common species chosen  (Sterculia apetala & Symphonia globulifera) have a greater geographical distribution. There is a slight correlation found in the rare species and their geographical location, but the common species is much more generalized in its geography. 
  
2. Build two generalized linear models to predict the abundance of the species Drypetes standleyi using the abundance of other tree species in the study site. Specifically examine the following species as predictor variables.
```{r loading predictor species}
sp_ids = c("Cordia.lasiocalyx", "Hirtella.triandra",
           "Picramnia.latifolia", "Quassia.amara",
           "Tabernaemontana.arborea", "Trattinnickia.aspera", 
           "Xylopia.macrantha")
```

```{r model 1}
library(nlme)
names(BCI)

pred_sp <- BCI[ , sp_ids]
mod_abu <- colSums(pred_sp)
resp_sp <- BCI[ , "Drypetes.standleyi"]

sp_a <- BCI$Cordia.lasiocalyx
sp_b <- BCI$Hirtella.triandra
sp_c <- BCI$Picramnia.latifolia
sp_d <- BCI$Quassia.amara
sp_e <- BCI$Tabernaemontana.arborea
sp_f <- BCI$Trattinnickia.aspera
sp_g <- BCI$Xylopia.macrantha

```
```{r one species as a predictor variable}
abu_lm <- gls(resp_sp ~ sp_a , data = BCI_xy)

plot(Variogram(abu_lm, form= ~ x + y))

```
  Based on the above graph, there appears to be some correlation between the two variables.


```{r using all species as predictor variables}
abus_lm <- gls(resp_sp ~ sp_a + sp_b + sp_c + sp_d + sp_e + sp_f + sp_g , data = BCI_xy)

plot(Variogram(abus_lm, form= ~ x + y))

```

```{r comparing using an anova}
anova(abus_lm)
```
  While the ANOVA test shows a significant difference between the predictor species and the response species, the variogram does not show this relationship. This is most likely because the variogram does factor in the spatial error, which might be the contributing factor to their significant relationship seen in the ANOVA. Once that is factored into the analysis, the significant relationship is no longer observed. 
