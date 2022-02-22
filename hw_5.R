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

abu <- colSums(BCI)
quantile(log10(abu))
plot(density(log10(abu)))
```

```{r}
which(BCI[ , 65])
which(BCI[ , 58])

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

  The rare species (Erythrina costaricensis & Inga acuminata) have a lower geographical distribution, which is to be expected as they are the less commonly found group of species, whereas the common species chosen  (Sterculia apetala & Symphonia globulifera) have a greater geographical distribution. There is a slight correlation found in the rare species 